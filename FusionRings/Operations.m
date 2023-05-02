(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-04-24 *)

(* TODO: make sure that permutations permute all data *)

Package["Anyonica`"]

(* PERMUTING ELEMENTS OF RINGS *)
PackageExport["PermutedRing"]

PermutedRing::usage =
  "PermutedRing[ring,\[Sigma]] returns a ring with multiplication table obtained by applying the permutation \[Sigma].";

PermutedRing::invalidpermutation =
  "Permutation vector `1` should either be of length `2` and contain all entries  in the range 1...`2` exactly once " <>
  "with a 1 on the first position or should contain all entries in the range 2...`2` exactly once";

PermutedRing[ r_FusionRing?FusionRingQ, \[Sigma]_List ] :=
  Module[{
    multTab = MultiplicationTable[r],
    m = Rank[r],
    hasUnit = MemberQ[ \[Sigma], 1 ],
    newMultTab
    },
    If[
      Not[ (hasUnit && Sort[\[Sigma]] == Range[m]) || Sort[\[Sigma]] == Range[m-1] + 1 ],
      Message[ PermutedRing::invalidpermutation, \[Sigma], m ],
      newMultTab =
      If[
        hasUnit,
        PermuteMultTab[ multTab, \[Sigma] ],
        PermuteMultTab[ multTab, Prepend[ \[Sigma], 1 ] ]
      ];

      FusionRing @ Sequence[
        "MultiplicationTable"         -> newMultTab,
        "Names"                       -> Names[r],
        "ElementNames"                -> ElementNames[r],
        "Barcode"                     -> Barcode @ r,
        "FormalParameters"            -> FC @ r,
        "DirectProductDecompositions" -> WhichDecompositions @ r,
        "SubFusionRings"              -> PermuteSubRingParticles[r["SubFusionRings"],\[Sigma]]
      ]
    ]
  ];

PackageScope["PermutedRing"]

PermutedRing[ r_FusionRing?FusionRingQ, \[Sigma]_Cycles ] :=
  PermutedRing[ r, PermutationList[ \[Sigma], Rank[r] ] ];


PackageScope["PermuteMultTab"]

PermuteMultTab[ table_, perm_ ] :=
  permuteMultTab[ table, perm ];

permuteMultTab =
  Compile[
    {{m,_Integer,3},{\[Sigma],_Integer,1}},
    Table[
      m[[ \[Sigma][[a]], \[Sigma][[b]], \[Sigma][[c]] ]],
      {a,Length[m]}, {b,Length[m]}, {c,Length[m]} ],
    {{permTab,_Integer,3}},
    CompilationOptions -> {"ExpressionOptimization"->True},
    "RuntimeOptions" -> "Speed"
  ];

PermuteSubRingParticles[ subRings_, \[Sigma]_List ] :=
  With[{ permutationMap = Reverse /@ Thread[ Range[ Length[ \[Sigma] ] ] -> \[Sigma] ] },
    ReplaceAll[
      subRings,
      {n__Integer} :> Sort[ ({n}/.permutationMap) ]
    ]
  ];

PackageExport["WhichPermutation"]

WhichPermutation::usage =
  "WhichPermutation[ring1,ring2] returns the permutation that transforms the multiplication table of ring1 into that" <>
  "of ring2.";

WhichPermutation[ ring1_FusionRing?FusionRingQ, ring2_FusionRing?FusionRingQ ] :=
  Module[
    { perms, mt1, mt2 },
    perms =
      FusionRingAutomorphisms[ ring1 ];
    mt1 =
      MT[ ring1 ];
    mt2 =
      MT[ ring2 ];
    FirstCase[
      perms,
      s_/;
      PermuteMultTab[ mt1, s ] == mt2
    ]
  ];


(* SORTING ELEMENTS OF RINGS *)
PackageExport["SortedRing"]

SetAttributes[ SortedRing, Listable ];

Options[SortedRing] =
  { "SortBy" -> "QuantumDimensions" };

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


PackageScope["PermVecQD"]

Options[PermVecQD] =
  {"Order" -> "Increasing"};

PermVecQD[r_FusionRing?FusionRingQ, OptionsPattern[] ] :=
  With[{
    qds = QuantumDimensions[r],
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
PermVecSDConj[r_FusionRing?FusionRingQ, OptionsPattern[]] := Module[{
  apmat = AntiparticleMatrix[r],
  qds = QuantumDimensions[r],
  pairs,
  sdpos,
  nsdpos,
  qdSort},
  pairs = DeleteCases[ DeleteDuplicates[ SortBy[ #, Function[ x, N[ qds[[x]] ] ] ]& /@ Position[ apmat, 1 ] ], {1,1} ];
  qdSort[ l_List ] := SortBy[ l, qds[[#]]& ];

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



(* Changing information of fusion rings.
   Everytime you want to change information about a FusionRing you basically create a new fusion ring
   with the desired info added. Upon creation it is checked whether the object is a valid fusion ring
   so altering information might be a costly operation.
*)

PackageExport["RenameElements"]

RenameElements::usage =
  "RenameElements[ring,elementnames] returns a ring with with elements named after elementnames.";
(*Here elementnames is only allowed to be a list of either String's, Integer's or Symbol's and must have a length equal to the rank of the ring.";*)

RenameElements[ r_FusionRing?FusionRingQ, list_ ] :=
  FusionRing @ Sequence[
    "MultiplicationTable" -> MultiplicationTable @ r,
    "ElementsName" -> ElementsName @ r,
    "ElementNames" -> list,
    "Names" -> Names @ r,
    "Barcode" -> Barcode @ r,
    "FormalParameters"  -> FC @ r,
    "DirectProductDecompositions" -> WhichDecompositions @ r,
    "SubFusionRings"  -> SubFusionRings @ r
  ];


PackageExport["AddName"]

AddName::usage =
  "AddName[ring,string] returns a ring where the name string is added to the list of possible names of ring.";

AddName[ r_FusionRing?FusionRingQ, s_String ] :=
  FusionRing @ Sequence[
    "MultiplicationTable" -> MultiplicationTable @ r,
    "ElementsName" -> ElementsName @ r,
    "ElementNames" -> ElementNames @ r,
    "Names" -> Append[ Names[r], s ],
    "Barcode"             -> Barcode @ r,
    "FormalParameters"    -> FC @ r,
    "DirectProductDecompositions" -> WhichDecompositions @ r,
    "SubFusionRings"      -> SubFusionRings @ r
  ];

AddName[ r_FusionRing?FusionRingQ, names_List ] :=
  FusionRing @ Sequence[
    "MultiplicationTable" -> MultiplicationTable @ r,
    "ElementsName" -> ElementsName @ r,
    "ElementNames" -> ElementNames @ r,
    "Names" -> Join[ Names[r], names ],
    "Barcode"             -> Barcode @ r,
    "FormalParameters"    -> FC @ r,
    "DirectProductDecompositions" -> WhichDecompositions @ r,
    "SubFusionRings"      -> SubFusionRings @ r
  ];

PackageExport["SetNames"]

SetNames::usage =
  "SetNames[ring,stringlist] returns a ring for which the names are now stringlist.";

SetNames[ r_FusionRing?FusionRingQ, names_List ] :=
  FusionRing @ Sequence[
    "MultiplicationTable" -> MultiplicationTable @ r,
    "ElementsName" -> ElementsName @ r,
    "ElementNames" -> ElementNames @ r,
    "Names" -> names,
    "Barcode"             -> Barcode @ r,
    "FormalParameters"    -> FC @ r,
    "DirectProductDecompositions" -> WhichDecompositions @ r,
    "SubFusionRings"      -> SubFusionRings @ r
  ];


(*Combining, decomposing and comparing fusion rings*)

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

PackageExport["DirectProduct"]

DirectProduct::usage =
  "DirectProduct[ring1,ring2] returns the direct ring product of ring1 and ring2.";

DirectProduct[ ring1_FusionRing?FusionRingQ, ring2_FusionRing?FusionRingQ ] :=
With[{
  k1 = Rank @ ring1,
  k2 = Rank @ ring2,
  tab1 = MultiplicationTable[ring1],
  tab2 = MultiplicationTable[ring2]
  },
  FusionRing @ Rule[ "MultiplicationTable",
    Flatten[ #, {{1,2},{3,4}} ]& @
    Table[
      Flatten @ Outer[ Times, tab1[[m1,n1]], tab2[[m2,n2]] ],
      {m1,k1},{m2,k2},{n1,k1},{n2,k2}
    ]
  ]
];

DirectProduct[ ring1_FusionRing?FusionRingQ, ring2_FusionRing?FusionRingQ, rings__ ] :=
  DirectProduct[ DirectProduct[ ring1, ring2 ] , rings ];


PackageExport["ReplaceByKnownRing"]

ReplaceByKnownRing::usage =
"Replace ring by the first known equivalent ring in FusionRingList.";

SetAttributes[ ReplaceByKnown, Listable ];

ReplaceByKnownRing[ ring_ ] :=
Module[{equivRing},
  With[{
    knownRings =
    RingsFromParams[ NSDNSD[ring], Mult[ring], NNZSC[ring] ]},
    equivRing =
    FirstCase[ knownRings, r_/;EquivalentFusionRingQ[ r, ring ] ];
    If[ Head[equivRing] =!= Missing,
      equivRing,
      ring
    ]
  ]
];


RingsFromParams[ nsdnsd_List, mult_Integer, nnzsc_Integer ] :=
Cases[
  FRL,
  ring_/;
  NNZSC[ring] == nnzsc &&
  NSDNSD[ring] == nsdnsd &&
  Mult[ring] == mult
];