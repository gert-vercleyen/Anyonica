(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2022-02-25 *)


Package["Anyonica`"]

PackageExport["\[ScriptCapitalF]"]

Unprotect[ \[ScriptCapitalF] ];

ClearAll[ \[ScriptCapitalF] ];

SetAttributes[ \[ScriptCapitalF], NHoldAll ];

Protect[ \[ScriptCapitalF] ];

\[ScriptCapitalF]::usage =
  "Formal symbol that represents an F-symbol.";


PackageScope["F"]

F =
  \[ScriptCapitalF];


PackageScope["ProperPentagonSolutionQ"]

ProperPentagonSolutionQ::usage =
  "Returns True if sol is a list of rules of the pattern \[ScriptCapitalF][__] -> _";

(* Check for proper format of solutions *)
ProperPentagonSolutionQ[ sol_ ] :=
  MatchQ[ sol, { Repeated[ F[__] -> _ ] } ];


PackageScope["PPSQ"]

PPSQ::usage =
  "Shorthand for ProperPentagonSolutionQ";

PPSQ[ sol_ ] :=
  ProperPentagonSolutionQ[ sol ];


PackageScope["ProperListOfPentagonSolutionsQ"]

ProperListOfPentagonSolutionsQ::usage =
  "Returns True if soln is a list of proper pentagon solutions.";

ProperListOfPentagonSolutionsQ[ soln_ ] :=
  TrueQ @
  MatchQ[ soln, { Repeated[ _?PPSQ ] } ];


PackageScope["PLOPSQ"]

PLOPSQ::usage =
  "Shorthand for ProperListOfPentagonSolutionsQ";

PLOPSQ[ soln_ ] :=
  ProperListOfPentagonSolutionsQ[ soln ];


PackageExport["$VacuumFPattern"]

$VacuumFPattern::usage =
  "General pattern of vacuum F-symbols";

$VacuumFPattern =
  F[ 1, __ ] | F[ _, 1, __ ] | F[ _, _, 1, __ ];


PackageExport["FSymbols"]

FSymbols::usage =
  "FSymbols[r] returns a list of well-defined F-symbols of the fusion ring r.";

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


PackageScope["SparseFTensor"]

SparseFTensor::usage  =
  "Returns a symbolic sparse F tensor.";

SparseFTensor::notmultfree =
  "`1` should have multiplicity 1.";

SparseFTensor[ ring_FusionRing?FusionRingQ ] :=
  If[
    Mult[ring] != 1
    ,
    Message[ SparseFTensor::notmultfree, ring ];
    Abort[]
    ,
    SparseArray[
      Map[ ( List @@ # ) -> # &, FSymbols @ ring ],
      Table[ Rank[ring], 6 ]
    ]
  ];


PackageExport["FMatrices"]

FMatrices::usage =
  "FMatrices[ring] returns a list of symbolic F-matrices.";

FMatrices::notmultfree =
  "`1` should have multiplicity 1. Use FTensors for higher multiplicities.";

(* Create symbolic F matrices *)
FMatrices[ ring_FusionRing?FusionRingQ ] :=
  If[
    Mult[ ring ] != 1
    ,
    Message[ FMatrices::notmultfree, ring ];
    Abort[]
    ,
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
    ]
  ];

RemoveZeroRows[ mat_?MatrixQ ] :=
  With[{ newMat = DeleteCases[ { 0 .. } ] @ mat },
    If[
      newMat === {},
      {{}},
      newMat
    ]
  ];

RemoveZeroColumns[ mat_?MatrixQ ] :=
  Module[{ r },
    If[
      mat === {{}},
      {{}},
      r = mat // Transpose // RemoveZeroRows;
      If[
        r === {{}},
        {{}},
        Transpose[r]
      ]
    ]
  ];


PackageExport["InverseFSymbols"]

InverseFSymbols::usage =
  "InverseFSymbols[ring,s,fSymbols] returns a list of the inverse F-symbols of the fSymbols of " <>
  "the Fusion Ring ring expressed via the symbol s";

InverseFSymbols::notasymbol =
  "`1` should have Head Symbol.";

InverseFSymbols::wrongsolutionformat =
  "`1` should be a proper solution to the pentagon equations.";

InverseFSymbols::notmultfree =
  "Function does not yet support rings with multiplicity.";

(*They are of the form symbol[...] -> val.*)
(* Calculate the inverse F-symbols *)
InverseFSymbols[ ring_FusionRing, s_, FSymbols_ ] :=
  Which[
    !SymbolQ[s]
    ,
    Message[ InverseFSymbols::notasymbol, s ];
    Abort[]
    ,
    !PPSQ[FSymbols]
    ,
    Message[ InverseFSymbols::wrongsolutionformat, FSymbols ];
    Abort[]
    ,
    Mult[ring] != 1
    ,
    Message[ InverseFSymbols::notmultfree ];
    Abort[]
    ,
    True
    ,
      With[
        { rF = FMatrices[ring] },
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



PackageExport["FTensors"]

FTensors::usage =
  "FTensors[ ring ] returns an association that maps four-tuples {a,b,c,d} to the matrix form of F[a,b,c,d] where "<>
  "the triples of upper-and lower indices have been flattened to single indices.";

FTensors[ ring_FusionRing ] :=
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
  ];

PackageExport["TetrahedralEquivalences"]

TetrahedralEquivalences::usage =
  "TetrahedralEquivalences[r] returns a list of rules that maps each F-symbol of the fusion ring to a "<>
  "representative that is equivalent up to tetrahedral symmetry.\n"<>
  "TetrahedralEquivalences[r,l] returns a list of rules that maps each F-symbol in the list l to a" <>
  "representative that is equivalent up to tetrahedral symmetry.";

TetrahedralEquivalences[ r_FusionRing, l_List ] :=
  With[{ classes = Gather[ l , TEQ[ CC[r] ] ] },
    Join @@
    Table[
      DeleteCases[ HoldPattern[ f_ -> f_ ] ] @
      Thread[ c -> First @ c ] ,
      { c, classes }
    ]
  ];

TetrahedralEquivalences[r_FusionRing] :=
  TetrahedralEquivalences[r, FSymbols[r]];

TEQ[d_][ F[i1_, j1_, k1_, l1_, m1_, n1_], F[i2_, j2_, k2_, l2_, m2_, n2_] ] :=
 Or[
  	And[ i1 == l2, j1 == k2, k1 == j2, l1 == i2, m1 == d[m2], n1 == n2 ],
  	And[ i1 == j2, j1 == i2, k1 == l2, l1 == k2, m1 == m2, n1 == d[n2] ],
  	And[ i1 == i2, j1 == m2, k1 == d[k2], l1 == n2, m1 == j2, n1 == l2 ]
 ];

(*Create the orbit of a set of symmetry generators on the indices*)
AllForms[transforms_List][arg_] :=
  FixedPoint[ NextForms[transforms], Flatten @ { arg } ];

NextForms[transforms_List][currforms_] :=
  Union @@ Prepend[currforms] @ Table[ t /@ currforms, { t, transforms } ];

NextForms[t_][currforms_] :=
  NextForms[{t}][currforms];