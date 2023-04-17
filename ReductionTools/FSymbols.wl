(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2022-02-25 *)

(* Check for proper format of solutions *)
ProperPentagonSolutionQ[ sol_ ] :=
  MatchQ[ sol, { Repeated[ F[__] -> _ ] } ];

PPSQ[ sol_ ] :=
  ProperPentagonSolutionQ[ sol ];

ProperListOfPentagonSolutionsQ[ soln_ ] :=
  MatchQ[ soln, { Repeated[ _?PPSQ ] } ];

PLOPSQ[ soln_ ] :=
  ProperListOfPentagonSolutionsQ[ soln ];

$VacuumFPattern =
  F[ 1, __ ] | F[ _, 1, __ ] | F[ _, _, 1, __ ];

(* Create a list of F-symbols *)
FSymbols[ ring_FusionRing?FusionRingQ ] :=
  If[
    Mult[ring] == 1,
    (* THEN *)
    Module[{ a, b, c, d, e, r, non0Ns, multTab, compatibleNs },
      r = Rank[ring];
      non0Ns = NZSC[ring];
      multTab = MT[ring];
      Sort @
      Reap[
        Do[
          {a,b,e} = label1;
          compatibleNs = Cases[ non0Ns, {e,_,_} ];
          Do[
            {c,d} = label2[[{2,3}]];
            Do[
              If[
                multTab[[b,c,f]] multTab[[a,f,d]] =!= 0,
                Sow @ F[a,b,c,d,e,f]
              ],
              {f,r}
            ],
            { label2, compatibleNs }
          ],
          { label1, non0Ns }
        ]
      ][[2,1]]
    ],
    (* ELSE *)
    Module[{a, b, c, d, e, r, non0Ns, multTab, compatibleNs, mult1, mult2, mult3, mult4},
      r = Rank[ring];
      non0Ns = NZSC[ring];
      multTab = MT[ring];
      Sort@
      Flatten @
      Reap[
        Do[
          { a, b, e } = label1;
          mult1 = multTab[[ a, b, e ]];
          compatibleNs = Cases[ non0Ns, { e, _, _ } ];
          Do[
            { c, d } = label2[[{ 2, 3 }]];
            mult2 = multTab[[ e, c, d ]];
            Do[
              mult3 = multTab[[ b, c, f ]];
              mult4 = multTab[[ a, f, d ]];
              If[
                mult3 mult4 != 0,
                Sow@
                Table[
                  F[a, b, c, d, { e, i, j }, { f, k, l } ],
                  { i, mult1 }, { j, mult2 }, { k, mult3 }, { l, mult4 }
                ]
              ],
              { f, r }
            ],
            { label2, compatibleNs }
          ],
          { label1, non0Ns }
        ]
      ][[ 2, 1 ]]
    ]
  ];

(* Create a symbolic sparse F tensor *)
SparseFTensor[ ring_FusionRing?FusionRingQ ] :=
  SparseArray[
    Map[ ( List @@ # ) -> # &, FSymbols @ ring ],
    Table[ Rank[ring], 6 ]
  ];

(* Create symbolic F matrices *)
FMatrices[ ring_FusionRing?FusionRingQ ] :=
  Module[{ sparseF, r, newMat, mats },
    r = Rank[ring];
    sparseF = SparseFTensor[ring];
    mats =
    Reap[
      Do[
        If[
          ( newMat =
          Normal[sparseF][[a,b,c,d,;;,;;]] //
          RemoveZeroColumns //
          RemoveZeroRows
          )
          =!= {{}} && newMat =!= {{1}},
          Sow[ newMat ]
        ],
        { a, r }, { b, r }, { c, r }, { d, r }
      ]
    ][[2]];
    If[
      mats == {},
      {},
      Flatten[ mats, 1 ]
    ]
  ];

(* Calculate the inverse F-symbols *)
InverseFSymbols[ ring_, s_, FSymbols_ ] :=
  With[
    { rF = FMatrices[ring] },
    With[
      { inverseLabels = Flatten[ rF/.F[x__, e_, f_] :> s[x, f, e] ] },
      Thread[
        Rule[
          inverseLabels,
          Flatten[ Transpose @* Inverse /@ (rF /. Dispatch[FSymbols])
          ]
        ]
      ]
    ]
  ];

FTensors[ ring_ ] :=
  Module[
    { gTrees, a, b, c, d, mt, eLabels, fLabels, non0Ns, r, m1, m2, m3, m4 },
    gTrees =
      GroupBy[
        LeftOrderedFusionTrees[ ring, 2 ],
        #[[{1, 2, 3, 5}]] &
      ];
    mt =
      MT[ring];
    non0Ns =
      NZSC[ring];
    r =
      Rank[ring];
    Association @@
    Reap[
      Do[
        { a, b, c, d } =(*EchoLabel["Key"] @*)
          key;
        
        eLabels = (*EchoLabel["eLabels"]@*)
          gTrees[key][[;; , 4]];
        
        fLabels = (*EchoLabel["fLabels"]@*)
          Intersection[
            Cases[ non0Ns, { a, _, d } ][[;; , 2]],
            Cases[ non0Ns, { b, c, _ } ][[;; , 3]]
          ];
        
        m1[e_] := mt[[a, b, e]]; m2[e_] := mt[[e, c, d]];
        m3[f_] := mt[[a, f, d]]; m4[f_] := mt[[b, c, f]];
        
        Sow[
          { a, b, c, d } ->
          ArrayFlatten @
          Table[
            ArrayReshape[
              SparseArray[
                Flatten @
                Table[
                  { alp, bet, gam, del } -> F[ a, b, c, d, {e, alp, bet}, {f, gam, del} ],
                  { alp, m1[e] }, { bet, m2[e] },
                  { gam, m3[f] }, { del, m4[f] }
                ],
                {  m1[e], m2[e], m3[f], m4[f] }
              ],
              { m1[e]*m2[e], m3[f]*m4[f] }
            ],
            { e, eLabels },
            { f, fLabels }
          ]
        ]
        , { key, Keys[gTrees] }
      ]
    ][[2, 1]]
  ]
