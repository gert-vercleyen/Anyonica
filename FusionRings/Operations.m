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
  Module[{ properPerm, newMultTab, PermuteModularData },

    properPerm =
      If[
        MemberQ[ perm, 1 ]
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
      "QuantumDimensions"           -> QuantumDimensions[r][[ properPerm ]],
      "SMatrices"                   -> SMatrices[r][[ ;;, properPerm, properPerm ]],
      "TwistFactors"                -> TwistFactors[r][[ ;;, properPerm ]],
      "ModularData"                 -> PermuteModularData /@ ModularData[r],
      "Characters"                  -> FusionRingCharacters[r][[;;,properPerm]]
    ]
  ];


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

RenameElements[ r_FusionRing?FusionRingQ, elNames_ ] :=
  Module[ { r2 = r, multTab = MT[r] },
    Which[
      elNames =!= None && Length[ elNames ] != Length[ multTab ]
      ,
      Message[ FusionRing::elnameslength, Length[ elNames ], Length[ multTab ] ];
      Abort[]
      ,
      !( Equal @@ Head /@ elNames)
      ,
      Message[ FusionRing::elnamesdifferentheads, elNames ];
      Abort[]
      ,
      !(MemberQ[{String,Integer,Symbol}, Head @* First @ elNames])
      ,
      Message[ FusionRing::elnameswrongheads, elNames];
      Abort[]
    ];

    r2["ElementNames"] =
      elNames;
    r2
  ];


PackageExport["AddName"]

AddName::usage =
  "AddName[ring,string] returns a ring where the name string is added to the list of possible names of ring.";

AddName[ r_FusionRing?FusionRingQ, s_String ] :=
  AddName[ r, { s } ];

AddName[ r_FusionRing?FusionRingQ, names_List ] :=
  FusionRing @@
  FilterRules[
    Normal[First @ r] /. ("Names" -> l_List) :> "Names" -> Join[ l, names ],
    Options[FusionRing]
  ];

PackageExport["SetNames"]

SetNames::usage =
  "SetNames[ring,stringlist] returns a ring for which the names are now stringlist.";

SetNames[ r_FusionRing?FusionRingQ, names_List ] :=
  FusionRing @@
  FilterRules[
    Normal[First @ r] /. ("Names" -> l_List) :> "Names" -> names,
    Options[FusionRing]
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

PackageExport["DirectProduct"]

DirectProduct::usage =
  "DirectProduct[ring1,ring2] returns the direct ring product of ring1 and ring2.";

DirectProduct[ ring1_FusionRing?FusionRingQ, ring2_FusionRing?FusionRingQ ] :=
  With[
    {
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

SetAttributes[ ReplaceByKnownRing, Listable ];

ReplaceByKnownRing[ ring_ ] :=
  Module[{knownRings, equivRing },
     knownRings = FRBC /@
      Cases[ Keys @ FRBC, { Rank[ring], Mult[ring], NNSD[ring], _ } ];

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