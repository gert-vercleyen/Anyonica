(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2022-09-03 *)

Package["Anyonica`"]

(*
+---------------------------------------------------------------------------+
|                               General Case                                |
+---------------------------------------------------------------------------+
*)
PackageExport["HexagonEquations"]

HexagonEquations::usage =
  "HexagonEquations[ r ] returns the hexagon equations related to the fusion ring r.";
(*The \"Knowns\" can be set to a list of rules of variables that are already known, e.g. a solution to the pentagon equations.";*)

Options[HexagonEquations] =
  {
    "Knowns" -> {},
    "TrivialVacuumSymbols" -> True
  };

HexagonEquations[ ring_FusionRing?FusionRingQ, opts:OptionsPattern[] ] :=
  If[
    Mult[ring] === 1,
    HexagonEquationsWithoutMultiplicity[ ring, opts ],
    HexagonEquationsWithMultiplicity[ ring, opts ]
  ];

(*
+---------------------------------------------------------------------------+
|                          Multiplicity Free Case                           |
+---------------------------------------------------------------------------+
*)

Options[HexagonEquationsWithoutMultiplicity] =
  Options[HexagonEquations];

HexagonEquationsWithoutMultiplicity[ ring_, OptionsPattern[] ] :=
  Module[{ a, b, c, d, e, g, sR, sF, rank, knowns, rSymbols,fSymbols, matchingLabels },
    rSymbols =
      R @@@ NZSC[ring];
    fSymbols =
      FSymbols[ring];
    rank =
      Rank[ring];
    knowns =
      Dispatch @
      Join[
        If[
          OptionValue["TrivialVacuumSymbols"],
          (* THEN *)
          Thread[
            Cases[ Join[ rSymbols, fSymbols ], $VacuumRPattern | $VacuumFPattern ] -> 1
          ],
          (* ELSE *)
          {}
        ],
        Normal @ OptionValue["Knowns"]
      ];

    (* construct a sparse array of R symbols *)
    sR =
      SparseArray[
        Thread[ NZSC[ring] -> rSymbols ]/.knowns,
        { rank, rank, rank }
      ];

    sF =
      SparseArray @
      ReplaceAll[
        Normal @ SparseFTensor[ ring ],
        knowns
      ];

    matchingLabels[ { a_, c_, b_, d_, e_} ] :=
      Cases[ List @@@ fSymbols, { c, a, b, d, e, _ } ][[;;,6]];
    
    DeleteDuplicates @
    DeleteCases[True] @
    Flatten[
      Reap[
        Do[ (* Loop over valid F-symbols and construct equations *)
          { a, c, b, d, e, g } = List @@ ff;
          Sow[
            {
              sR[[ c, a, e ]] sF[[a,c,b,d,e,g]] sR[[ c, b, g ]]
              ==
              Sum[
                sF[[ c, a, b, d, e, f ]] sR[[ c, f, d ]] sF[[ a, b, c, d, f, g ]],
                { f, matchingLabels[ { a, c, b, d, e } ] }
              ],
              
              sR[[ a, c, e ]]^(-1) sF[[a,c,b,d,e,g]] sR[[ b, c, g ]]^(-1)
              ==
              Sum[
                sF[[ c, a, b, d, e, f ]] sR[[ f, c, d ]]^(-1) sF[[ a, b, c, d, f, g ]],
                { f, matchingLabels[ { a, c, b, d, e } ] }
              ]
            }
          ],
          { ff, fSymbols }
        ]
      ][[2]]
    ]
  ];

(*
+---------------------------------------------------------------------------+
|                             Multiplicity Case                             |
+---------------------------------------------------------------------------+
*)

Options[ HexagonEquationsWithMultiplicity ] =
  Options[HexagonEquations];
HexagonEquationsWithMultiplicity[ ring_, OptionsPattern[] ] :=
  Module[ {
    fGroupedLabels, replaceKnowns, matchingLabels, Rt, Ft,
    DS, TP, ID, a, b, c, d, e, g, eLabels, fLabels, gLabels, op,
    SwapOperator, swap, mt
    },
    mt =
      MT[ring];
    
    fGroupedLabels =
      GroupBy[(* Group right 2-vertex trees by equal values of a,b,c,d *)
        RightOrderedFusionTrees[ ring, 2 ],
        #[[{1,2,3,5}]]&
      ];
    
    (* Substitutes known values in F or R tensors *)
    replaceKnowns[ tensors_ ] :=
      ( SparseArray[ Normal[#]/.Dispatch[ OptionValue["Knowns"] ] ] & ) /@
      tensors;
    
    (* Searches for intermediate fusion labels s where the two patterns are vertex labels, each containing 1 s *)
    matchingLabels[ pattern1_, pattern2_, s_ ] :=
      With[{
        non0Ns = NZSC[ring],
        n1 = Flatten @ FirstPosition[ pattern1, s ],
        n2 = Flatten @ FirstPosition[ pattern2, s ]
        },
        Flatten @
        Intersection[
          Cases[ non0Ns, ReplaceAll[ pattern1, s->Blank[] ] ][[;;,n1]],
          Cases[ non0Ns, ReplaceAll[ pattern2, s->Blank[] ] ][[;;,n2]]
        ]
      ];

    SwapOperator[ { a_, b_, e_ }, { e_, c_, d_ } ] :=
      ArrayFlatten[
        SparseArray[
          Flatten @
          Table[
            { i, j, j, i } -> 1,
            { i, mt[[a,b,e]] },
            { j, mt[[e,c,d]] }
          ]
        ]
      ];
    
    Rt =
      replaceKnowns @
      RTensors[ring];
    Ft =
      replaceKnowns @
      FTensors[ring];
    DS =
      MatrixDirectSum;
    TP =
      KroneckerProduct;
    ID =
      IdentityMatrix[ mt[[##]] ]&;
      
    DeleteCases[True] @
    Flatten @
    Reap[
      Do[ (* Loop over valid a,b,c,d labels *)
        { a, b, c, d } =
          treeLabels;
        eLabels =
          matchingLabels[ { a, b, e }, { e, c, d }, e ];
        fLabels =
          fGroupedLabels[ treeLabels ][[;;,4]];
        gLabels =
          matchingLabels[ { c, a, g }, { g, b, d }, g ];
        
        op[1,1] =
          DS @ Table[ TP[ Rt[ { c, a, g } ], ID[ g, b, d ] ], { g, gLabels } ];
        
        op[1,2] =
          Ft[ { a, c, b, d } ];
        
        op[1,3] =
          DS @ Table[ TP[ ID[ a, f, d ], Rt[ { c, b, f } ] ], { f, fLabels } ];
        
        op[2,1] =
          Ft[ { c, a, b, d } ];
        
        swap =
          DS @ Table[ SwapOperator[ { a, b, e }, { e, c, d } ], { e, eLabels } ];
        
        op[2,2] =
          DS @ Table[ TP[ ID[ a, b, e ], Rt[ { c, e, d } ] ], { e, eLabels } ];
        
        (* DS @ Table[ ID[ a, b, e ] ~ TP ~ Rt[ { c, e, d } ], { e, eLabels } ]; *)
        op[2,3] =
          Ft[ { a, b, c, d } ];
        
        Sow @
        {
          ThreadMatrixEquality[
            op[1,1].op[1,2].op[1,3] == op[2,1].swap.op[2,2].op[2,3]
          ],
          ThreadMatrixEquality[
            Inverse[op[1,1]].op[1,2].Inverse[op[1,3]] == op[2,1].swap.Inverse[op[2,2]].op[2,3]
          ]
        };
        , { treeLabels, Keys[ fGroupedLabels ]  }
      ]
    ][[2,1]]
  
  ];