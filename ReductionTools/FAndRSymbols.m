(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gert *)
(* :Date: 2023-10-17 *)
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

F = \[ScriptCapitalF];

PackageExport["FilterFRules"]

FilterFRules = FilterRules[ #, F[__] ]&;


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

(* From: https://journals.aps.org/prb/pdf/10.1103/PhysRevB.102.115154 *)

PackageExport["TetrahedralSymmetries"]

TetrahedralSymmetries::usage =
"TetrahedralSymmetries[r] returns a list of rules that maps each F-symbol of the fusion ring to a "<>
"representative that is equal via a tetrahedral symmetry.\n"<>
"TetrahedralSymmetries[r,l] returns a list of rules that maps each F-symbol in the list l to a" <>
"representative that is equal via a tetrahedral symmetry.";

Options[TetrahedralSymmetries] :=
{
  "PreEqualCheck" -> RootReduce
};

TetrahedralSymmetries[ r_FusionRing, opts:OptionsPattern[] ] :=
TetrahedralSymmetries[ r, FSymbols @ r, opts ];

TetrahedralSymmetries[ r_FusionRing, l_List, opts:OptionsPattern[] ] :=
Cases[
  Sort[
    FixRule[ OptionValue["PreEqualCheck"] ] /@
    DeleteCases[ a_ -> a_ ] @
    BuildSymmetries[ Flatten[ TSOrbit[r] /@ l ], { }, OptionValue["PreEqualCheck"] ]
  ],
  ( a_ -> b_ ) /; NumericQ[b] || MemberQ[ l, b ] (* Want symmetry transforms to stay in our list *)
];

TSOrbit[ ring_FusionRing ][ symb:F[j_,k_,l_,i_,m_,n_] ] :=
With[ { d = CC[ring], qd = FrobeniusPerronDimensions[ring][[#]]& },
  DeleteDuplicates[
    (
      DeleteCases[
        {
          symb -> F[ k, j, d[i], d[l], m, d[n] ],
          symb -> F[ d[i], l, k, d[j], d[m], n ],
          symb -> F[ d[m], k, d[n], d[i], d[j], d[l] ] Sqrt[ qd[m] qd[n] / ( qd[j] qd[l] ) ]
        }/.$VacuumFPattern -> 1,
        1 -> 1
      ]/.( 1 -> a_?NumericQ ) :> Throw[ 1 -> a ])/. ( 1 -> a_ ) :> FixLHS[ a -> 1 ]
  ]
];

BuildSymmetries[ {}, sym_, check_  ] :=
sym;

BuildSymmetries[ rules_, sym_, check_ ] :=
With[{ firstRule = First @ rules, restRules = Rest @ rules },
  BuildSymmetries[
    DeleteDuplicates[
      FixRule[check] /@ DeleteCases[ restRules /. firstRule, a_ -> a_ ]
    ],
    Append[firstRule] @ ( sym /. firstRule ),
    check
  ]
];

FixLHS[ a_ -> b_ ] :=
With[{ f = First @ GetVariables[ { a }, F ] }, f -> b / (a/f) ];

FixRule[ check_ ][ a_ -> b_ ] :=
Switch[ NumericQ /@ { a, b },
  { True, True }
  ,
  If[ check[ a - b ] != 0, Throw[ a -> b ], a -> b ]
  ,
  { True, False }
  ,
  FixLHS[ b -> a ]
  ,
  _
  ,
  FixLHS[ a -> b ]
];

PackageExport["ProjectiveTetrahedralSymmetries"]

ProjectiveTetrahedralSymmetries::usage =
"ProjectiveTetrahedralSymmetries[r] returns a list of rules that maps each F-symbol of the fusion ring to a "<>
"representative that is equal via a projective tetrahedral symmetry.\n"<>
"ProjectiveTetrahedralSymmetries[r,l] returns a list of rules that maps each F-symbol in the list l to a" <>
"representative that is equal via a projective tetrahedral symmetry.";

ProjectiveTetrahedralSymmetries :=
ProjectRHS @* TetrahedralSymmetries;

SetAttributes[ ProjectRHS, Listable ];
ProjectRHS[ a_ -> b_?NumericQ ] :=
a -> 1;
ProjectRHS[ a_ -> b_ ] :=
With[ { f = First @ GetVariables[ b, F ] }, a -> f ];

PackageExport["TransparentFSymbols"]

TransparentFSymbols::usage =
"TransparentFSymbols[ ring ] returns a list of rules that maps F-symbols of ring, that have a group element as "<>
"an outer upper index, to 1.";

TransparentFSymbols[ ring_FusionRing ] :=
Module[{ d, groupQ },
  d =
  CC[ring];

  groupQ[i_] :=
  Total[ MT[ring][[ i, d[i] ]] ] == 1;

  Thread[ Cases[ FSymbols @ ring, F[ a_, _, c_, __ ] /; groupQ[a] || groupQ[c] ] -> 1 ]
];
PackageExport["\[ScriptCapitalR]"]

Unprotect[ \[ScriptCapitalR] ];

ClearAll[ \[ScriptCapitalR] ];

SetAttributes[ \[ScriptCapitalR], NHoldAll ];

Protect[ \[ScriptCapitalR] ];

\[ScriptCapitalR]::usage =
"Formal symbol that represents an R-symbol.";


PackageScope["R"]

R =
\[ScriptCapitalR];

PackageExport["FilterRRules"]

FilterRRules =
FilterRules[ #, R[__] ]&;

PackageScope["ProperHexagonSolutionQ"]

ProperHexagonSolutionQ::usage =
"Returns True if sol is a list of rules of the pattern \[ScriptCapitalR][__] -> _";

ProperHexagonSolutionQ[ sol_ ] :=
MatchQ[ sol, { Repeated[ R[__] -> _ ] } ];


PackageScope["PHSQ"]

PHSQ::usage =
"Shorthand for ProperHexagonSolutionQ";

PHSQ[ sol_ ] :=
ProperHexagonSolutionQ[ sol ];


PackageScope["ProperListOfHexagonSolutionsQ"]

ProperListOfHexagonSolutionsQ::usage =
"Returns True if soln is a list of proper pentagon solutions.";

ProperListOfHexagonSolutionsQ[ soln_ ] :=
MatchQ[ soln, { Repeated[ _?PHSQ ] } ];

PackageScope["PLOHSQ"]

PLOHSQ::usage =
"Shorthand for ProperListOfHexagonSolutionsQ";

PLOHSQ[ soln_ ] :=
ProperListOfHexagonSolutionsQ[ soln ];


PackageExport["$VacuumRPattern"]

$VacuumRPattern::usage =
"General pattern of vacuum R-symbols";

$VacuumRPattern =
R[ 1, __ ] | R[ _, 1, __ ];


PackageExport["RSymbols"]

RSymbols::usage =
"RSymbols[ ring ] returns a list of the well-defined R-symbols (without values) of the Fusion Ring ring.";

(* Create a list of R-symbols *)
RSymbols[ ring_FusionRing?FusionRingQ ] :=
If[
  Mult[ring] === 1,
  (* THEN *)
  R @@@ NZSC[ring],
  (* ELSE *)
  Module[ { a, b, c, non0Ns, multTab, mult1, mult2 },
    non0Ns = NZSC[ring];
    multTab = MT[ring];
    Sort @
    Flatten @
    Reap[
      Do[
        { a, b, c } = label;
        mult1 = multTab[[ a, b, c ]];
        mult2 = multTab[[ b, a, c ]];
        Sow @
        Table[
          R[ a, b, c, i, j ],
          { i, mult1 }, { j, mult2 }
        ],
        { label, non0Ns }
      ]
    ][[ 2, 1 ]]
  ]
];

PackageExport["RTensors"]

RTensors::usage =
"RTensors[ ring ] returns an association that maps well defined {a,b,c} to the matrix form of R[a,b,c].";

RTensors[ ring_ ] :=
Module[{mt, a, b, c},
  mt = MT[ring][[##]] &;
  Association @@
  Table[
    { a, b, c } = n;
    n ->
    SparseArray[ Array[ R[a, b, c, #1, #2] & , { mt @@ n, mt @@ n } ] ],
    { n, NZSC[ring] }
  ]
];

PackageScope["SparseRTensor"]

(* Create a symbolic sparse R tensor *)
SparseRTensor[ ring_FusionRing?FusionRingQ ] :=
SparseArray[
  Map[ ( List @@ # ) -> # &, RSymbols @ ring ],
  Table[ Rank[ring], 3 ]
];
