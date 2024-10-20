(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-04-24 *)


Package["Anyonica`"]

(* PERMUTING ELEMENTS OF RINGS *)
PackageExport["PermutedRing"]

PermutedRing::usage =
  "PermutedRing[ring,\[Sigma]] returns a ring with multiplication table obtained by applying the permutation \[Sigma].";

PermutedRing::invalidpermutation =
  "Permutation vector `1` should either be of length `2` and contain all entries  in the range 1...`2` exactly once " <>
  "with a 1 on the first position or should contain all entries in the range 2...`2` exactly once";

PermutedRing[ r_FusionRing?FusionRingQ, perm_List ] :=
  (
  If[ perm == Range[ Rank @ r ], Return @ r ];
  Module[{ properPerm, newMultTab, PermuteModularData },

    properPerm =
      If[
        MemberQ[1] @ perm
        ,
        perm
        ,
        Prepend[1] @ perm
      ];

    If[
      Sort[properPerm] =!= Range[ Rank[r] ]
      ,
      Message[ PermutedRing::invalidpermutation, properPerm, Rank[r] ];
      Abort[]
    ];

    newMultTab =
      PermuteMultTab[ MT[r], properPerm ];

    PermuteModularData[ <|"SMatrix" -> mat_, "TwistFactors" -> twists_|> ] :=
      <|
         "SMatrix" -> mat[[properPerm, properPerm]],
         "TwistFactors" -> twists[[;; , properPerm]]
      |>;

    FusionRing @
    Sequence[
      "MultiplicationTable"         -> newMultTab,
      "Names"                       -> Names[r],
      "ElementsName"                -> ElementsName[r],
      "ElementNames"                -> ElementNames[r],
      "Barcode"                     -> Barcode @ r,
      "FormalParameters"            -> FC @ r,
      "DirectProductDecompositions" -> r["DirectProductDecompositions"],
      "SubFusionRings"              -> PermuteSubRingParticles[ r["SubFusionRings"], properPerm ],
      "QuantumDimensions"           -> FPDims[r][[ properPerm ]],
      "SMatrices"                   -> NormalizedSMatrices[r][[ ;;, properPerm, properPerm ]],
      "TwistFactors"                -> TwistFactors[r][[ ;;, properPerm ]],
      "ModularData"                 -> PermuteModularData /@ ModularData[r],
      "Characters"                  -> FusionRingCharacters[r][[;;,properPerm]]
    ]
  ]
  );


PermutedRing[ r_FusionRing?FusionRingQ, perm_Cycles ] :=
  PermutedRing[ r, PermutationList[ perm, Rank[r] ] ];

PackageExport["PR"]

PR::usage =
  "Shorthand for PermutedRing.";

PR =
  PermutedRing;


PackageScope["PermuteMultTab"]

PermuteMultTab[ table_, perm_ ] :=
  permuteMultTab[ table, perm ];

permuteMultTab =
  Compile[
    {{m,_Integer,3},{perm,_Integer,1}},
    Table[
      m[[ perm[[a]], perm[[b]], perm[[c]] ]],
      {a,Length[m]}, {b,Length[m]}, {c,Length[m]} ],
    {{permTab,_Integer,3}},
    CompilationOptions -> {"ExpressionOptimization"->True},
    "RuntimeOptions" -> "Speed"
  ];

PermuteSubRingParticles[subRings_, perm_List] :=
  Module[{permutationMap, code},
    permutationMap[l_] :=
      Sort[l /. (Reverse /@ Thread[Range[Length[perm]] -> perm])];

    { permutationMap[#1], code[ FormalCode[#2] ] } & @@@ ReleaseHold[subRings] /. code -> Hold[FRBC]
  ];

PackageExport["WhichPermutation"]

WhichPermutation::usage =
  "WhichPermutation[ring1,ring2] returns the permutation that transforms the multiplication table of ring1 into that" <>
  "of ring2.";

WhichPermutation[ ring1_FusionRing?FusionRingQ, ring2_FusionRing?FusionRingQ ] :=
  Module[{ vec },
    vec =
      PermutationVector[ MT[ring1], MT[ring2] ];

    If[
      vec === None,
      None,
      vec
    ]
  ];

PackageExport["WP"]

WP::usage =
  "Shorthand for WhichPermutation";

WP =
  WhichPermutation;

(* SORTING ELEMENTS OF RINGS *)
PackageExport["SortedRing"]

SortedRing::usage = 
  "SortedRing[ring] returns a fusion ring with its elements sorted."

SetAttributes[ SortedRing, Listable ];

Options[SortedRing] =
  { "SortBy" -> "FPDims" };

SortedRing[ r_FusionRing?FusionRingQ, OptionsPattern[] ] :=
  With[{
    permVec =
      Which[
        OptionValue["SortBy"] == "Selfdual-Conjugates"
        ,
        PermVecSDConj[r]
        ,
        OptionValue["SortBy"] == "Conjugates-Selfdual"
        ,
        PermVecSDConj[r]//Reverse
        ,
        True,
        PermVecQD[r]]
    },
    PermutedRing[ r, permVec ]
  ];

PackageExport["SR"]

SR::usage =
  "Shorthand for SortedRing.";

SR =
  SortedRing;


PackageScope["PermVecQD"]

Options[PermVecQD] =
  {"Order" -> "Increasing"};

PermVecQD[r_FusionRing?FusionRingQ, OptionsPattern[] ] :=
  With[{
    qds = FPDims[r],
    range = Range[ Rank[r] - 1] + 1
    },
    Prepend[
      If[ OptionValue["Order"] == "Increasing",
        Identity,
        Reverse
      ] @ SortBy[ range, N[ qds[[#]], 100 ]& ],
      1
    ]
  ];

PackageScope["PermVecSDConj"]

(* Based on anti-particles *)
Options[PermVecSDConj] = Options[PermVecQD];
PermVecSDConj[r_FusionRing?FusionRingQ, OptionsPattern[]] :=
  Module[{ apmat = AntiparticleMatrix[r], qds = FPDims[r], pairs, sdpos, nsdpos, qdSort},
    pairs =
      DeleteCases[ {1,1} ] @
      DeleteDuplicates[
        SortBy[ #, Function[ x, N[ qds[[x]], { Infinity, 16 } ] ] ]& /@
        Position[ apmat, 1 ]
      ];

    qdSort[ l_List ] :=
      SortBy[ l, qds[[#]]& ];

    If[ OptionValue["Order"] == "Increasing",
      Identity,
      Reverse
    ] @
    Prepend[1] @
    Flatten[
      Join[
        qdSort @ Cases[ pairs, { a_Integer, a_Integer } :> a ],
        qdSort /@ SortBy[ Cases[ pairs, {a_Integer,b_Integer}/; a != b], Max @ N[ qds[[#]], 100000 ] & ]
      ]
    ]
  ];


(* Combining, decomposing and comparing fusion rings *)

PackageExport["EquivalentFusionRings"]

EquivalentFusionRings::usage =
  "EquivalentFusionRings[ring] returns a list of all rings r for which there exists a permutation vector \[Sigma] "<>
  "with \[Sigma][[1]] == 1 and r equals PermutedRing[ring,\[Sigma]].";

SetAttributes[ EquivalentFusionRings, Listable ];

EquivalentFusionRings[ r_FusionRing?FusionRingQ ] :=
  With[{
    l =  Permutations[ Range[Rank[r] - 1 ] ] + 1 },
    Table[ PermutedRing[ r, Join[ {1}, \[Sigma] ] ], { \[Sigma], l } ]
  ];

(* We need to call this before unprotecting TensorProduct since otherwise we get an error.
I don't know why this happens though *)

Options[TensorProduct];

Unprotect[TensorProduct];

Options[TensorProduct] =
Join[
  Options[FusionCategory],
  Options[FusionRing],
  { "SimplifyBy" -> Identity }
];

FusionRing /: TensorProduct[ ring1:FusionRing[_], ring2:FusionRing[_], opts:OptionsPattern[] ] :=
  AddOptions[opts][FusionRing][
    "MultiplicationTable" ->  KroneckerProduct[ MT @ ring1, MT @ ring2 ]
  ];

FusionRing /: TensorProduct[ ring1:FusionRing[_], ring2:FusionRing[_], rings__, opts:OptionsPattern[] ] :=
  TensorProduct[ TensorProduct[ ring1, ring2, opts ] , rings, opts ];

(* For legacy reasons *)
DirectProduct =
  TensorProduct;

PackageExport["ReplaceByKnownRing"]

ReplaceByKnownRing::usage =
"Replace ring by the first known equivalent ring in FusionRingList.";

SetAttributes[ ReplaceByKnownRing, Listable ];

ReplaceByKnownRing[ ring_ ] :=
  Module[{knownRings, equivRing },
     knownRings = FRBC /@
      Cases[ Keys @ FRBCData, { Rank[ring], Mult[ring], NNSD[ring], _ } ];

    equivRing =
      FirstCase[ knownRings, r_/; EquivalentFusionRingsQ[ r, ring ] ];

    If[ MissingQ[equivRing], Return @ ring ];

    PermutedRing[ equivRing, WhichPermutation[ ring, equivRing ] ]

  ];

PackageExport["RBKR"]

RBKR::usage =
  "Shorthand for ReplaceByKnownRing.";

RBKR =
  ReplaceByKnownRing;
