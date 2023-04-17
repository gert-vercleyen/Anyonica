(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2022-02-18 *)


Package["Anyonica`"]

Options[ PentagonEquations ] =
  Options[ PentagonTower ];

PentagonEquations[ ring_FusionRing?FusionRingQ, opts:OptionsPattern[] ] :=
  With[{ tower = PentagonTower[ ring, opts ] },
    Join[
      tower["Bin"] // Values,
      tower["Sum"] // Values
    ] // Flatten
  ];

Options[ PentagonTower ] =
  {
    "TrivialVacuumSymbols" -> True,
    "Knowns" -> {}
  };

PentagonTower[ ring_FusionRing?FusionRingQ, opts:OptionsPattern[] ] :=
  Module[{ a,b,c,d,e,p,q,r,s,n, matches, eqn, dim, patt, pentEqns, lFInd, sF, dimF, trivVacQ, knowns},
    trivVacQ =
      OptionValue["TrivialVacuumSymbols"];
    knowns =
      OptionValue["Knowns"];
    dimF =
      DimF @ FMatrices[ring];
    n =
      Rank[ring];
    lFInd =
      List @@@ FSymbols[ring];
    
    sF =
      SparseArray @
      ReplaceAll[
        Normal[SparseFTensor[ring]],
          Join[
            knowns,
            If[
              trivVacQ,
              Thread[ { F[ 1, __ ] | F[ _, 1, __ ] | F[ _, _, 1, __ ] } -> 1 ],
              {}
            ]
          ]
        ];
    
    pentEqns =
    Reap[
      (* Collect equations of the form Non0LHS == RHS *)
      Do[
        { p, c, d, e, q, r } = label;
        matches = Cases[ lFInd, { a_, b_, r, e, p, s_ } ];
        Do[
          { a, b, s } = label2[[ { 1, 2, 6 } ]];
          eqn =
            (
              sF[[p,c,d,e,q,r]] sF[[a,b,r,e,p,s]] ==
              Sum[ sF[[b,c,d,s,x,r]] sF[[a,b,c,q,p,x]] sF[[a,x,d,e,q,s]], {x,n} ]
            );
          
          If[ (* Equation is not trivial *)
            !TrueQ[eqn],
            (* THEN Set dim equal to max size of F-matrix *)
            dim =
              Max[
                dimF /@
                GetVars[ {eqn}, F, "LevelSpec" -> 4 ]
              ];
            If[
              eqn[[2,0]] === Plus,
              Sow[ eqn, patt[2][dim]],
              Sow[ eqn, patt[1][dim]]
            ]
          ],
          { label2, matches }
        ],
        { label, lFInd }
      ],
      Flatten @
      Table[ patt[i][j], {i,2}, {j,n} ]
    ][[2]];
    
    <|
      "Bin" -> Association @@ Table[ i -> Flatten @ pentEqns[[i]], {i,n} ],
      "Sum" -> Association @@ Table[ i -> Flatten @ pentEqns[[i+n]], {i,n} ]
    |>
    
  ];


BinomialEquationsFromTower[ tower_Association ] :=
  tower["Bin"] //
  Values //
  Flatten;

SumEquationsFromTower[ tower_Association ] :=
  tower["Sum"] //
  Values //
  Flatten;

MonSumEquationsFromTower[ tower_Association ] :=
  Map[
    Flatten,
    {
      tower["Bin"] // Values,
      tower["Sum"] // Values
    }
  ];

PentagonEquationsFromTower[ tower_Association ] :=
  Join @@ MonSumEquationsFromTower[ tower ];


DimF[regMats_] :=
  Module[ { matToRules },
    matToRules[ mat_ ] :=
      With[{ n = Length[mat] },
        Flatten @ Map[ # -> n &, mat, {2} ]
      ];
    Association @@
    Flatten[
      matToRules /@ regMats
    ]
  ];


(*
+---------------------------------------------------------------------------+
|                             Multiplicity Case                             |
+---------------------------------------------------------------------------+
*)

Options[PentagonEquationsWithMultiplicity] = { "Knowns" -> {} };
PentagonEquationsWithMultiplicity[ ring_FusionRing?FusionRingQ, OptionsPattern[] ] :=
  Module[
    { fgGroupedLabels, gGroupedLabels, mt, a, b, c, d, e, l, k, h, replaceKnowns,
      matchingLabels, dim, fLabels, gLabels, hLabels, kLabels, lLabels },
    mt =
      MT[ring];
    
    fgGroupedLabels =
      GroupBy[(* Group left 3-vertex trees by equal values of a,b,c,d,e *)
        LeftOrderedFusionTrees[ ring, 3 ],
        #[[{1,2,3,4,7}]]&
      ];
  
    gGroupedLabels =
      GroupBy[ (#[[5]]&) -> (#[[6]]&) ] /@
      fgGroupedLabels;

    (* Substitutes known values in F or R tensors *)
    replaceKnowns[ expr_ ] :=
      expr/.Dispatch[ OptionValue["Knowns"] ];

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
    
    dim[ a_, b_, c_ ] :=
      mt[[a,b,c]];

    DeleteCases[True] @
    Flatten @
    Reap[
      Do[ (* Loop over valid a,b,c,d labels *)
        { a, b, c, d, e } =
          treeLabels;
        fLabels =
          DeleteDuplicates @
          fgGroupedLabels[ treeLabels ][[;;,5]];
        gLabels =
          gGroupedLabels[ treeLabels ];
        Do[
          lLabels =
            matchingLabels[ { f, l, e }, { c, d, l }, l ];
          
          kLabels[ l_ ] :=
            matchingLabels[ { a, k, e }, { b, l, k }, k ];
          
          hLabels[ k_ ] :=
            Intersection[
              matchingLabels[ { a, h, g }, { b, c, h }, h ],
              matchingLabels[ { b, c, h }, { h, d, k }, h ]
            ];
          
          Sow @
          replaceKnowns @
          Table[
            Sum[
              F[ f, c, d, e, { g, bet, gam }, { l, zet, eps } ] *
              F[ a, b, l, e, { f, alp, zet }, { k, the, eta } ]
              , { zet, dim[ f, l, e ] }
            ]
            ==
            Sum[
              F[ a, b, c, g, { f, alp, bet }, { h, kap, iot } ] *
              F[ a, h, d, e, { g, kap, gam }, { k, the, lam } ] *
              F[ b, c, d, k, { h, iot, lam }, { l, eta, eps } ]
              , { h, hLabels[ k ] }
              , { iot, dim[ b, c, h ] }
              , { kap, dim[ a, h, g ] }
              , { lam, dim[ h, d, k ] }
            ]
            , { l, lLabels }, { k, kLabels[ l ] }
            , { alp, dim[ a, b, f ] }, { bet, dim[ f, c, g ] }, { gam, dim[ g, d, e ] }
            , { eps, dim[ c, d, l ] }
            , { eta, dim[ b, l, k ] }, { the, dim[ a, k, e ] }
          ]
          , { f, fLabels }, { g, gLabels[f] }
        ]
        , { treeLabels, Keys[ fgGroupedLabels ]  }
      ]
    ][[2,1]]
  ];