(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-04-24 *)

Package["Anyonica`"]

(* The structure of the FusionRing objects and the means of storing and using information
   contained in FusionRing  objects has been greatly inspired by the following post on stackexchange
   https://mathematica.stackexchange.com/questions/213618/implement-abstract-algebraic-structure. *)

(* All the core data will be stored in an Association for convenience of
   access. We still want FusionRing to be the head of the object so
   we store the association inside the function FusionRing, which will
   only serve as a wrapper and never return a value *)

PackageExport["FusionRing"]

FusionRing::usage =
  "FusionRing[ \"MultiplicationTable\" -> multTab ] initializes a fusion ring based on the given multiplication " <>
  "table multTab.";
(* Extra options include\n(i) \"ElementsName\" -> s which gives all elements an indexed name with String or Symbol s, \n(ii) \"ElementNames\" -> elNames where elNames is a list of Strings, Integers or Symbols representing the names of the different elements (giving this option overwrites the \"ElementsName\" option, \n(iii) \"Names\" -> names where names is a list of String's representing the possible names of the fusion ring." *)

FusionRing::nomulttable =
  "No multiplication data is provided so no ring could be initialized.";

FusionRing::wrongdim =
  "Wrong dimensions: `1`. The dimensions of the multiplication table should be {m,m,m}, with m the amount of particles.";

FusionRing::notassociative =
  "Multiplication table does not correspond to associative multiplication.";

FusionRing::nounit =
  "The first element in the multiplication table is not a unit element.";

FusionRing::noinverse =
  "The multiplication table contains particles with no antiparticle.";

FusionRing::multipleinverse =
  "The multiplication table contains particles with multiple antiparticles.";

FusionRing::badcoeff =
  "The multiplication table contains arguments that are not positive integers.";

FusionRing::elnameslength =
  "Length of list of names `1` is not equal to amount of generators `2` of the fusion ring.";

FusionRing::elnamesdifferentheads =
  "The elements of list `1` should have equal Head's.";

FusionRing::elnameswrongheads =
  "The elements of list `1` should have Head Integer, String, or Symbol.";

FusionRing::badargnames =
  "OptionValue `1` of \"Names\" should be a String or a list of Strings.";

Options[ FusionRing ] =
  Thread[
    {
      "MultiplicationTable",
      "Names",
      "ElementsName",
      "ElementNames",
      "Barcode",
      "FormalParameters",
      "DirectProductDecompositions",
      "SubFusionRings",
      "QuantumDimensions",
      "SMatrices",
      "TwistFactors",
      "ModularData",
      "Characters"
    } -> Missing[]
  ];

FusionRing[ ops:OptionsPattern[] ] :=
  FusionRing[ InitializeFusionRing[ ops ] ];

(* Access elements of the fusion ring as if it were an association. *)
FusionRing[ r_ ][ k_ ] :=
  Lookup[ r, k ];

Options[ InitializeFusionRing ] =
  Options[ FusionRing ];

InitializeFusionRing[ ops:OptionsPattern[] ] :=
  With[{
    multTab =
      "MultiplicationTable" // OptionValue // Developer`ToPackedArray,
    names =
      Which[
        MissingQ[#], {},
        StringQ[#], {#},
        Head[#] === List && And @@ StringQ /@ #, #,
        True, Message[FusionRing::badargnames, # ]
      ]& @ OptionValue["Names"],
    elsName =
      If[ !MissingQ[#], #, None ]& @ ("ElementsName" // OptionValue)
    },
    {
      elNames = If[ !MissingQ[#], #, elementName/@ Range[Length[multTab]] ]& @ OptionValue["ElementNames"],
      dualvec = If[ Length[multTab] == 1, {1}, Join[ {1}, multTab[[2;;,2;;,1]] . ( Range[ Length[multTab] - 1 ] + 1 ) ] ]
    },
    Which[
      multTab === None
      ,
      Message[ FusionRing::nomulttable ]
      ,
      !ProperStructureConstantsQ[ multTab ]
      ,
      Message[ FusionRing::badcoeff ]
      ,
      !ProperDimensionsQ[ multTab ]
      ,
      Message[ FusionRing::wrongdim, Dimensions[ multTab ] ]
      ,
      !HasUnitQ[ multTab ]
      ,
      Message[ FusionRing::nounit ]
      ,
      !UniqueInverseQ[ multTab ]
      ,
      Message[ FusionRing::multipleinverse ]
      ,
      !AllInversesQ[ multTab ]
      ,
      Message[ FusionRing::noinverse ]
      ,
      !AssociativeQ[ multTab ]
      ,
      Message[ FusionRing::notassociative ]
      ,
      elNames =!= None && Length[ elNames ] != Length[ multTab ]
      ,
      Message[ FusionRing::elnameslength, Length[ elNames ], Length[ multTab ] ]
      ,
      !( Equal @@ Head /@ elNames)
      ,
      Message[ FusionRing::elnamesdifferentheads, elNames ]
      ,
      !(MemberQ[{String,Integer,Symbol}, Head @* First @ elNames])
      ,
      Message[ FusionRing::elnameswrongheads, elNames]
      ,
      True
      ,
      Association[
        "Barcode" -> OptionValue["Barcode"],
        "DirectProductDecompositions" -> OptionValue["DirectProductDecompositions"],
        "Dual" -> Association[ # -> dualvec[[#]]& /@ Range[Length[multTab]] ],
        "ElementNames" -> elNames,
        "ElementsName" -> elsName,
        "FormalParameters" -> OptionValue["FormalParameters"],
        "MultiplicationTable" -> multTab,
        "Names" -> names,
        "SubFusionRings" -> OptionValue["SubFusionRings"],
        "QuantumDimensions" -> OptionValue["QuantumDimensions"],
        "SMatrices" -> OptionValue["SMatrices"],
        "TwistFactors" -> OptionValue["TwistFactors"],
        "ModularData" -> OptionValue["ModularData"],
        "Characters" -> OptionValue["Characters"]
      ]
    ]
  ];

elementName[i_Integer] :=
  With[{
    replaceByUnicode =
      Thread[ Range[10] - 1 -> FromCharacterCode /@ (120811 + Range[10]) ]
    },
    StringJoin @@ (IntegerDigits[i] /. replaceByUnicode)
  ];

toString =
  Function[expr, ToString[expr, StandardForm], {HoldAll, Listable}];

ProperStructureConstantsQ[ multTable_ ] :=
  And @@ Map[ IntegerQ[#] && # >= 0 &, Flatten @ multTable ];

ProperDimensionsQ[ multTable_ ] :=
  With[{ d = Dimensions @ multTable },
    TrueQ[ Equal @@ d ] && TrueQ[ Length[ d ] == 3 ]
  ];

HasUnitQ[ multTable_ ] :=
  With[{ i = IdentityMatrix[ Length @ multTable ] },
    multTable[[ 1, All, All ]] === multTable[[ All, 1, All ]] === i
  ];

UniqueInverseQ[ multTable_ ] :=
  With[{apMatrix = multTable[[All,All,1]]},
    And @@
    Join[
      Map[ Count[ #, x_/; x > 0 ] < 2 &, apMatrix ],
      Map[ Count[ #, x_/; x > 0 ] < 2 &, Transpose @ apMatrix ]
    ]
  ];

AllInversesQ[ multTable_ ] :=
  With[ { apMatrix = multTable[[All,All,1]] },
    And @@
    Join[
      Map[ Count[ #, x_/; x > 0 ] > 0 &, apMatrix ],
      Map[ Count[ #, x_/; x > 0 ] > 0 &, Transpose @ apMatrix ]
    ]
  ];

AssociativityMatrixCompiled =
  Compile[
    {{multTable,_Integer,3}},
    Table[
      multTable[[a,b,1;;Length[multTable]]] . multTable[[1;;Length[multTable],c,d]]
      - multTable[[a,1;;Length[multTable],d]] . multTable[[b,c,1;;Length[multTable]]],
      {a,Length[multTable]},
      {b,Length[multTable]},
      {c,Length[multTable]},
      {d,Length[multTable]}
    ],
    {{zerosTable,_Integer,3}},
    CompilationOptions -> {"ExpressionOptimization"->True},
    "RuntimeOptions" -> "Speed"
  ];

AssociativeQ[ table_ ] :=
  With[ {
    truthTab = AssociativityMatrixCompiled[table]},
    Equal@@Flatten[truthTab] && truthTab[[1,1,1,1]]==0
  ];

(* FusionRingQ will be implemented such way that we don't always need to check whether
   the ring is a fusion ring or not. *)
PackageExport["FusionRingQ"]

FusionRingQ::usage =
  "FusionRingQ[ring] returns True if ring is a FusionRing and False otherwise.";

FusionRingQ[ ring_ ] :=
  Head[ring] === FusionRing;

(*
FusionRingQ[ r_FusionRing ] :=
  System`Private`HoldValidQ[ r ];

FusionRingQ[ _ ] :=
  False;

FusionRingQ[ s_Symbol ] :=
  And[
    Head[s] === FusionRing,
    FusionRingQ @ Evaluate @ s
  ];

ValidateFusionRing[ r_Association ] :=
  With[{ mtab = r["MultiplicationTable"] },
    TrueQ[
      And[
        ProperStructureConstantsQ @ #,
        ProperDimensionsQ @ #,
        HasUnitQ @ #,
        UniqueInverseQ @ #,
        AllInversesQ @ #,
        AssociativeQ @ #
      ]
    ]& @ mtab
  ];

FusionRing[ r_Association ]?NotFusionRingQ :=
  (System`Private`HoldSetValid[ FusionRing[ r ] ]) /; validateFusionRing[ r ];

SetAttributes[ NotFusionRingQ, HoldFirst ];

NotFusionRingQ[ r_ ] :=
  Not @ FusionRingQ[r];
*)

(* Constructors for some common fusion rings *)



(* PSU(2)_k *)
PackageExport["FusionRingPSU2k"]

FusionRingPSU2k::usage =
  "FusionRingPSU2k[k] returns the fusion ring \!\(\*
  StyleBox[\"PSU\",\nFontWeight->\"Bold\"]\)\!\(\*
  StyleBox[\"(\",\nFontWeight->\"Bold\"]\)\!\(\*
  StyleBox[\"2\",\nFontWeight->\"Bold\"]\)\!\(\*
  StyleBox[SubscriptBox[\")\", \"k\"],\nFontWeight->\"Bold\"]\)";

FusionRingPSU2k[ k_Integer ] :=
  FusionRing @ Sequence[
    Rule[ "MultiplicationTable", Table[
      If[ MemberQ[ rangePSU2k[ i, j, k ], l ],
        1,
        0
      ],
      { i, 0, k, 2 }, { j, 0, k, 2 }, { l, 0, k, 2 } ]
    ],
    Rule[ "Names" , {"PSU(2\!\(\*SubscriptBox[\()\), \("<>ToString[k]<>"\)]\)"}]
  ];

rangePSU2k[ i_Integer, j_Integer, k_Integer ] :=
  Table[ l, { l, Abs[ i - j ], Min[ i + j, 2k - i - j ] , 2 } ];


(* SU(2)_k *)
PackageExport["FusionRingSU2k"]

FusionRingSU2k::usage =
  "FusionRingSU2k[k] returns the fusion ring \!\(\*
  StyleBox[\"SU\",\nFontWeight->\"Bold\"]\)\!\(\*
  StyleBox[\"(\",\nFontWeight->\"Bold\"]\)\!\(\*
  StyleBox[\"2\",\nFontWeight->\"Bold\"]\)\!\(\*
  StyleBox[SubscriptBox[\")\", \"k\"],\nFontWeight->\"Bold\"]\)";

FusionRingSU2k[ k_Integer ] :=
  FusionRing @ Sequence[
  Rule[ "MultiplicationTable", Table[
    If[ MemberQ[ rangeSU2k[ i, j, k ], l ],
      1,
      0
    ],
    { i, 0, k }, { j, 0, k }, { l, 0, k } ]
  ],
  Rule[ "Names" , {"SU(2\!\(\*SubscriptBox[\()\), \("<>ToString[k]<>"\)]\)"} ]
];

rangeSU2k[ i_Integer, j_Integer, k_Integer ] :=
  rangePSU2k[ i, j, k ];


(* Fusion rings from groups *)


PackageExport["FusionRingFromGroup"]

Options[FusionRingFromGroup] := { "Names" -> {""}};

FusionRingFromGroup::usage =
  "FusionRingFromGroup[g],  returns a fusion ring whose multiplication matches that of the group g. \n"<>
  "FusionRingFromGroup[multTab] returns a fusion ring whose multiplication table matches the group multiplication"<>
  " table multTab.";
FusionRingFromGroup::notgroupmulttab =
  "The multiplication table `1` does not correspond to that of a group."

(*Its name will be set to that of the group if the group is a PermutationGroup, SymmetricGroup, AlternatingGroup, CyclicGroup, DihedralGroup or AbelianGroup, or will be the value given by the option \"Names\".*)
FusionRingFromGroup[ table_?MatrixQ, opts:OptionsPattern[] ] := With[ {
  n = Length[ table ]},
  If[ !GroupTableQ[table], Message[ FusionRingFromGroup::notgroupmulttab, table ]; Return @ $$Failed ];
  AddOptions[opts][FusionRing][
      "MultiplicationTable" ->
      Table[
        If[ k == table[[i,j]], 1 , 0]
        , { i, n }, { j, n }, { k, n }
      ]
  ]
];

FusionRingFromGroup[ group_, opts:OptionsPattern[] ] :=
  FusionRingFromGroup[
    GroupMultiplicationTable @ group,
    If[ OptionValue[ "Names" ] == { "" },
      "Names" -> { GroupName @ group },
      OptionValue[ "Names" ]
    ]
  ];

(* Code for assigning the correct name *)
GroupName[PermutationGroup[list_]] :=
  "\!\(\*SubscriptBox[\(G\), \(Perm\)]\)[" <>
  (ToString@StringReplace[ToString[#],{"{"->"(",","->"","}"->")"}]& /@
  Cases[list,Cycles[x_]:>x,\[Infinity]]) <>
  "]";

GroupName[SymmetricGroup[n_]]:=
  "\!\(\*SubscriptBox[\(S\), \("<>ToString[n]<>"\)]\)";

GroupName[AlternatingGroup[n_]]:=
  "\!\(\*SubscriptBox[\(A\), \("<>ToString[n]<>"\)]\)";

GroupName[DihedralGroup[n_]]:=
  "\!\(\*SubscriptBox[\(D\), \("<>ToString[n]<>"\)]\)";

GroupName[CyclicGroup[n_]]:=
  "\!\(\*SubscriptBox[\(\[DoubleStruckCapitalZ]\), \("<>ToString[n]<>"\)]\)";

GroupName[AbelianGroup[list_]]:=
  StringJoin @@
  Riffle[ Table["\!\(\*SubscriptBox[\(\[DoubleStruckCapitalZ]\), \("<>ToString[i]<>"\)]\)",{i,list}],"\[Cross]"];

GroupName[_] :=
  "";

(* Code for checking whether the multiplication table is that of a group *)
Group::invalidrangesmulttab =
  "Not every row and column in `1` contains numbers from 1 to `2`.";
Group::nouniqueinverse =
  "Not every element in `1` has a unique inverse.";
Group::nonassociativemulttab =
  "The multiplication defined by `1` is not associative.";
Group::noninvertibleelement =
  "Not every element is invertible.";
GroupTableQ[ tab_?MatrixQ ] :=
  With[{
    m = tab[[#1,#2]]&,
    n = tab // Length
    },
    Which[
      Not[ And @@ Map[ Sort[#] == Range[n]&, tab ~ Join ~ Transpose[ tab] ] ]
      ,
      Message[ Group::invalidrangesmulttab, tab, n]; False
      ,
      Not[ Times @@ Count[1] /@ tab == 1]
      ,
      Message[ Group::nouniqueinverse, tab ]; False
      ,
      Not[ And @@ Flatten @ Array[ m[#1,m[#2,#3]] == m[m[#1,#2],#3]&, {n,n,n} ] ]
      ,
      Message[ Group::nonassociativemulttab, tab ]; False
      ,
      Not[ tab[[;;,1]] == tab[[1,;;]] == Range[n] ]
      ,
      Message[ Group::noninveribleelement ]; False
      ,
      True
      ,
      True
    ]
  ];

(* Z_n *)
PackageExport["FusionRingZn"]

FusionRingZn::usage =
  "FusionRingZn[n] returns the group fusion ring \!\(\*SubscriptBox[\(\[DoubleStruckCapitalZ]\), \(n\)]\)";

FusionRingZn[ n_Integer ] :=
  FusionRingFromGroup[CyclicGroup[n]];

(* Rep(G) *)
PackageExport["FusionRingRepG"]

FusionRingRepG::usage =
  "FusionRingRep[G] returns the character ring of the finite group G.\n"<>
  "FusionRingRep[string] returns the character ring whose standard name is a string.\n"<>
  "FusionRingRep[{\"gname\",n}] returns the character ring of the finite group with name gname and parameter n.";

FusionRingRepG[ g_/; ListQ[g] || StringQ[g] ] :=
  Module[{ ct, rank, n },
    ct =
      FiniteGroupData[ g , "CharacterTable" ];

    If[ MissingQ[ct], Return[ct] ];

    rank =
      Length @ ct;

    FusionRing[
      "MultiplicationTable" ->
      Table[
        ToInteger @
        Solve[
          ct[[i]] ct[[j]] == Sum[ n[k] ct[[k]], { k, rank } ]
        ][[ 1, ;; , 2 ]],
        { i, rank },
        { j, rank }
      ]
    ]
  ];

FusionRingRepG[ g_ ] :=
  FusionRingRepG @ ToFGDName[ g ];

ToFGDName[g_[n_]] :=
  { ToString @ g, n };

CharacterTable[ g_ ] :=
  FiniteGroupData[ ToFGDName[g], "CharacterTable" ];

SetAttributes[ToInteger, Listable];
ToInteger[ x_ ] :=
  If[ IntegerQ[x], x, Floor[N[ x, { Infinity, 1 }]] ];

(* Haagerup-Izumi *)
PackageExport["FusionRingHI"]

FusionRingHI::usage =
  "FusionRingHI[g] returns the Haagerup-Izumi fusion ring associated to the built-in abelian group g." <>
  "FusionRingHI[multtable] returns the Haagerup-Izumi fusion ring associated to the group with multiplication" <>
  " table multTab.";

FusionRingHI::nonsymmulttab =
  "The multiplication table `1` must be symmetric.";

FusionRingHI::notgrouptable =
  "The multiplication table `1` must be a group multiplication table.";

Options[FusionRingHI] =
  {"Names" -> {} };

FusionRingHI[ tab_?MatrixQ, OptionsPattern[] ] :=
  With[
    {
    n = tab // Length,
    inv = tab // Position[ #, 1 ]& // Transpose // Last
    },
    Which[
      !GroupTableQ[ tab ], Message[ FusionRingHI::nongrouptable, tab ] ; Abort[]
      ,
      !SymmetricMatrixQ[ tab ], Message[ FusionRingHI::nonsymmulttab, tab ]
      ,
      True
      ,
      If[
        OptionValue["Names"] === {}
        ,
        FusionRing @ Sequence[
          Rule[ "MultiplicationTable", Table[
            Which[
              i <= n && j <= n, Table[ If[ k == tab[[ i, j ]], 1, 0 ], { k, 2n } ],
              i <= n && j >  n, Table[ If[ k == n + tab[[ i, j - n ]], 1, 0 ], { k, 2n } ],
              i > n  && j <= n, Table[ If[ k == n + tab[[ inv[[ j ]], i - n ]], 1, 0 ], { k, 2n } ],
              i > n  && j >  n, Table[ If[ k == tab[[ i - n, inv[[ j - n ]] ]] || k > n, 1, 0 ], { k, 2n } ]
            ],
            { i, 2n }, { j, 2n } ]
          ]
        ]
        ,
        FusionRing @ Sequence[
          Rule[ "MultiplicationTable", Table[
            Which[
              i <= n && j <= n, Table[ If[ k == tab[[ i, j ]], 1, 0 ], { k, 2n } ],
              i <= n && j >  n, Table[ If[ k == n + tab[[ i, j - n ]], 1, 0 ], { k, 2n } ],
              i > n  && j <= n, Table[ If[ k == n + tab[[ inv[[ j ]], i - n ]], 1, 0 ], { k, 2n } ],
              i > n  && j >  n, Table[ If[ k == tab[[ i - n, inv[[ j - n ]] ]] || k > n, 1, 0 ], { k, 2n } ]
            ],
            { i, 2n }, { j, 2n } ]
          ],
          Rule[
            "Names",
            OptionValue["Names"]
          ]
        ]
      ]
    ]
  ];

FusionRingHI[ group_, OptionsPattern[] ] :=
  FusionRingHI[
    GroupMultiplicationTable @ group,
    If[
      OptionValue["Names"] != {""}
      ,
      "Names" -> OptionValue["Names"]
      ,
      If[
        GroupName[group] != ""
        ,
        "Names" -> { "HI(" <> GroupName[group] <> ")" }
        ,
        "Names" -> { "" }
      ]
    ]
  ];


(* Tambara Yamagami *)
PackageExport["FusionRingTY"]

FusionRingTY::usage =
  "FusionRingTY[g] returns the Tambara-Yamagami fusion ring associated to the built-in abelian group g.\n"<>
  "FusionRingTY[multtable] returns the Tambara-Yamagami fusion ring associated to the group with multiplication " <>
  "table multtable.";

FusionRingTY::notgrouptable =
  "The multiplication table `1` must be a group multiplication table.";

Options[FusionRingTY] = {"Names" -> {} };

FusionRingTY[ tab_?MatrixQ, OptionsPattern[] ] :=
  With[
    {n = tab // Length },
    If[
      !GroupTableQ[ tab ], Message[ FusionRingTY::notgrouptable, tab ]; Return[],
      If[
        OptionValue["Names"] === {},
        (* THEN *)
        FusionRing @ Sequence[
          Rule[ "MultiplicationTable", Table[
            Which[
              i <= n && j <= n, Table[ If[ k == tab[[ i, j ]], 1, 0 ], { k, n + 1 } ],
              i <= n && j >  n, Table[ If[ k == n + 1, 1, 0 ],         { k, n + 1 } ],
              i >  n && j <= n, Table[ If[ k == n + 1, 1, 0 ],         { k, n + 1 } ],
              i >  n && j >  n, Table[ If[ k <= n, 1, 0 ],             { k, n + 1 } ]
            ],
            { i, n + 1 }, { j, n + 1 } ]
          ]
        ],
        (* ELSE *)
        FusionRing @ Sequence[
          Rule[ "MultiplicationTable", Table[
            Which[
              i <= n && j <= n, Table[ If[ k == tab[[ i, j ]], 1, 0 ], { k, n + 1 } ],
              i <= n && j >  n, Table[ If[ k == n + 1, 1, 0 ],         { k, n + 1 } ],
              i >  n && j <= n, Table[ If[ k == n + 1, 1, 0 ],         { k, n + 1 } ],
              i >  n && j >  n, Table[ If[ k <= n, 1, 0 ],             { k, n + 1 } ]
            ],
            { i, n + 1 }, { j, n + 1 } ]
          ],
          Rule[
            "Names",
            OptionValue["Names"]
          ]
        ]
      ]
    ]
  ];

FusionRingTY[ group_, OptionsPattern[] ] :=
  FusionRingTY[
    GroupMultiplicationTable @ group,
    If[ OptionValue["Names"] != {""},
      "Names" -> OptionValue["Names"],
      If[ GroupName[group] != "",
        "Names" -> { "TY(" <> GroupName[group] <> ")" },
        "Names" -> { "" }
      ]
    ]
  ];

(* Fusion Rules come directly from "On Metaplectic Modular Categories and Their Applications, Communications
    in Math Phys, M B Hastings, C Nayak, Zhenghan Wang", and from notes from Eddy Ardonne from 2010 *)

PackageExport["FusionRingSON2"]

FusionRingSON2::usage =
  "FusionRingSON2[m] returns the the fusion ring \!\(\*
  StyleBox[\"SO\",\nFontWeight->\"Bold\"]\)\!\(\*
  StyleBox[\"(\",\nFontWeight->\"Bold\"]\)\!\(\*
  StyleBox[\"N\",\nFontWeight->\"Bold\"]\)\!\(\*
  StyleBox[SubscriptBox[\")\", \"2\"],\nFontWeight->\"Bold\"]\), for m >= 4.";

FusionRingSON2::notimplemented =
  "This ring is only implemented for integer parameter > 3.";

(* TODO: implement modular data for metaplectic categories *)
FusionRingSON2[ m_ ] :=
(*If[
    m < 4,
    Message[ FusionRingSON2::notimplemented ],*)
    FusionRing[
      "MultiplicationTable" -> Which[ Mod[ m, 4 ] == 0, rulesdiv4[m/2], Mod[ m, 2 ] == 0, rulesdiv2[m/2], True, rulesodd[m] ],
      "Names" -> {"SO(" <> ToString[m] <> "\!\(\*SubscriptBox[\()\), \(2\)]\)", "Metaplectic(" <> ToString[m] <> ")"}
      (*]*)
  ];

rulesodd[m_] :=
  Module[ { r, rank, ar, mat1, matZ, matXe1, matXe2, matY },
    r =
      (m - 1) / 2;

    rank =
      (m + 7) / 2;

    ar[i_] := (* representation of elements via E_i arrays *)
      Normal @ SparseArray[ i -> 1, {rank}];

    mat1 =
      IdentityMatrix[rank];

    matZ =
      Table[
        Which[
          i == 1, ar[2],
          i == 2, ar[1],
          3 <= i <= 4, ar[ 3 + Mod[i, 2]],
          5 <= i, ar[i]
        ]
        , {i, rank}
      ];

    matXe1 =
      Table[
        Which[
          i == 1, ar[3],
          i == 2, ar[4],
          i == 3, ar[1] + Sum[ar[j], {j, 5, rank}],
          i == 4, ar[2] + Sum[ar[j], {j, 5, rank}],
          i >= 5, ar[3] + ar[4]
        ]
        , {i, rank}
      ];

    matXe2 =
      Table[
        Which[
          i == 1, ar[4],
          i == 2, ar[3],
          i == 3, ar[2] + Sum[ar[j], {j, 5, rank}],
          i == 4, ar[1] + Sum[ar[j], {j, 5, rank}],
          i >= 5, ar[3] + ar[4]
        ]
        , {i, rank}
      ];

    matY[j_] :=
      Table[
        Which[
          i == 1, ar[j + 4],
          i == 2, ar[j + 4],
          i == 3, ar[3] + ar[4],
          i == 4, ar[3] + ar[4],
          i >= 5 && i - 4 == j, ar[1] + ar[2] + ar[ Min[ 2 * j, m - 2 * j ] + 4 ],
          i >= 5 && i - 4 != j, ar[ Abs[i - 4 - j] + 4 ] + ar[ Min[ i - 4 + j, m - i - j + 4 ] + 4 ]
        ]
        , {i, rank}
      ];

    Transpose /@
    Join[
      { mat1, matZ, matXe1, matXe2 },
      matY /@ Range[r]
    ]
  ];

rulesdiv2[ p_ ] :=
  Module[
    {
      matId, mat\[CapitalTheta], mat\[CapitalPhi]1, mat\[CapitalPhi]2, mat\[Sigma]1, mat\[Sigma]2, mat\[Tau]1,
      mat\[Tau]2, mat\[Phi], rank, Id, \[CapitalTheta], \[CapitalPhi]1, \[CapitalPhi]2, \[Sigma]1, \[Sigma]2, \[Tau]1,
      \[Tau]2, \[Phi], sumEven\[Lambda]s, sumOdd\[Lambda]s
    },
    rank =
        p + 7;

    Evaluate[
      Join[
        { Id, \[CapitalTheta], \[CapitalPhi]1, \[CapitalPhi]2, \[Sigma]1, \[Sigma]2, \[Tau]1, \[Tau]2 },
        \[Phi] /@ Range[ rank - 8 ]
      ]
    ] = IdentityMatrix[ rank ];

    sumEven\[Lambda]s =
      Sum[ \[Phi][i], { i, 2, p - 1, 2 } ];

    sumOdd\[Lambda]s =
      Sum[ \[Phi][i], { i, 1, p - 1, 2 } ];

    matId =
      IdentityMatrix[ rank ];

    mat\[CapitalTheta] =
      Table[
        Which[
            i == 1, \[CapitalTheta], (* 1 *)
            i == 2, Id, (* \[CapitalTheta] *)
            i == 3, \[CapitalPhi]2, (* \[CapitalPhi]1 *)
            i == 4, \[CapitalPhi]1, (* \[CapitalPhi]2 *)
            i == 5, \[Tau]1, (* \[Sigma]1 *)
            i == 6, \[Tau]2, (* \[Sigma]2 *)
            i == 7, \[Sigma]1, (* \[Tau]1 *)
            i == 8, \[Sigma]2, (* \[Tau]2 *)
            i >= 9, \[Phi][i - 8]	 (* \[Phi]_\[Lambda]'s *)
          ],
        {i, rank}
      ];

    mat\[CapitalPhi]1 =
      Table[
        Which[
            i == 1, \[CapitalPhi]1, (* 1 *)
            i == 2, \[CapitalPhi]2, (* \[CapitalTheta] *)
            i == 3, \[CapitalTheta], (* \[CapitalPhi]1 *)
            i == 4, Id, (* \[CapitalPhi]2 *)
            i == 5, \[Sigma]2, (* \[Sigma]1 *)
            i == 6, \[Tau]1, (* \[Sigma]2 *)
            i == 7, \[Tau]2, (* \[Tau]1 *)
            i == 8, \[Sigma]1, (* \[Tau]2 *)
            i >= 9, \[Phi][p - (i - 8) ]	 (* \[Phi]_\[Lambda]'s *)
          ],
        {i, rank}
      ];

    mat\[CapitalPhi]2 =
      Table[
        Which[
            i == 1, \[CapitalPhi]2, (* 1 *)
            i == 2, \[CapitalPhi]1, (* \[CapitalTheta] *)
            i == 3, Id, (* \[CapitalPhi]1 *)
            i == 4, \[CapitalTheta], (* \[CapitalPhi]2 *)
            i == 5, \[Tau]2, (* \[Sigma]1 *)
            i == 6, \[Sigma]1, (* \[Sigma]2 *)
            i == 7, \[Sigma]2, (* \[Tau]1 *)
            i == 8, \[Tau]1, (* \[Tau]2 *)
            i >= 9, \[Phi][p - (i - 8)]	 (* \[Phi]_\[Lambda]'s *)
          ],
        {i, rank}
      ];

    mat\[Sigma]1 =
      Table[
        Which[
            i == 1, \[Sigma]1, (* 1 *)
            i == 2, \[Tau]1, (* \[CapitalTheta] *)
            i == 3, \[Sigma]2, (* \[CapitalPhi]1 *)
            i == 4, \[Tau]2, (* \[CapitalPhi]2 *)
            i == 5, \[CapitalPhi]2 + sumOdd\[Lambda]s, (* \[Sigma]1 *)
            i == 6, Id + sumEven\[Lambda]s, (* \[Sigma]2 *)
            i == 7, \[CapitalPhi]1 + sumOdd\[Lambda]s, (* \[Tau]1 *)
            i == 8, \[CapitalTheta]  + sumEven\[Lambda]s, (* \[Tau]2 *)
            i >= 9,
              If[ OddQ[i], \[Sigma]2 + \[Tau]2, \[Sigma]1 + \[Tau]1 ]	 (* \[Phi]_\[Lambda]'s *)
            ],
          {i, rank}
        ];

    mat\[Sigma]2 =
        Table[
          Which[
              i == 1, \[Sigma]2, (* 1 *)
              i == 2, \[Tau]2, (* \[CapitalTheta] *)
              i == 3, \[Tau]1, (* \[CapitalPhi]1 *)
              i == 4, \[Sigma]1, (* \[CapitalPhi]2 *)
              i == 5, Id + sumEven\[Lambda]s, (* \[Sigma]1 *)
              i == 6, \[CapitalPhi]1 + sumOdd\[Lambda]s, (* \[Sigma]2 *)
              i == 7, \[CapitalTheta]  + sumEven\[Lambda]s, (* \[Tau]1 *)
              i == 8, \[CapitalPhi]2 + sumOdd\[Lambda]s, (* \[Tau]2 *)
              i >= 9,
                If[ EvenQ[i], \[Sigma]2 + \[Tau]2, \[Sigma]1 + \[Tau]1 ]	 (* \[Phi]_\ \[Lambda]'s *)
            ],
          {i, rank}
        ];

    mat\[Tau]1 =
        Table[
          Which[
              i == 1, \[Tau]1, (* 1 *)
              i == 2, \[Sigma]1, (* \[CapitalTheta] *)
              i == 3, \[Tau]2, (* \[CapitalPhi]1 *)
              i == 4, \[Sigma]2, (* \[CapitalPhi]2 *)
              i == 5, \[CapitalPhi]1 + sumOdd\[Lambda]s, (* \[Sigma]1 *)
              i == 6, \[CapitalTheta] + sumEven\[Lambda]s, (* \[Sigma]2 *)
              i == 7, \[CapitalPhi]2 + sumOdd\[Lambda]s, (* \[Tau]1 *)
              i == 8, Id  + sumEven\[Lambda]s, (* \[Tau]2 *)
              i >= 9,
                If[ OddQ[i], \[Sigma]2 + \[Tau]2, \[Sigma]1 + \[Tau]1 ]	 (* \[Phi]_\\[Lambda]'s *)
            ],
          {i, rank}
        ];

    mat\[Tau]2 =
        Table[
          Which[
              i == 1, \[Tau]2, (* 1 *)
              i == 2, \[Sigma]2, (* \[CapitalTheta] *)
              i == 3, \[Sigma]1, (* \[CapitalPhi]1 *)
              i == 4, \[Tau]1, (* \[CapitalPhi]2 *)
              i == 5, \[CapitalTheta] + sumEven\[Lambda]s, (* \[Sigma]1 *)
              i == 6, \[CapitalPhi]2 + sumOdd\[Lambda]s, (* \[Sigma]2 *)
              i == 7, Id  + sumEven\[Lambda]s, (* \[Tau]1 *)
              i == 8, \[CapitalPhi]1 + sumOdd\[Lambda]s, (* \[Tau]2 *)
              i >= 9,
                If[ EvenQ[i], \[Sigma]2 + \[Tau]2, \[Sigma]1 + \[Tau]1 ]	 (* \[Phi]_\\[Lambda]'s *)
            ],
          {i, rank}
        ];

    mat\[Phi][j_] :=
        Table[
          Which[
              i == 1, \[Phi][j], (* 1 *)
              i == 2, \[Phi][j], (* \[CapitalTheta] *)
              i == 3, \[Phi][p - j], (* \[CapitalPhi]1 *)
              i == 4, \[Phi][p - j], (* \[CapitalPhi]2 *)
              i == 5, If[ OddQ[j], \[Sigma]2 + \[Tau]2, \[Sigma]1 + \[Tau]1 ], (* \[Sigma]1 *)
              i == 6, If[ EvenQ[j], \[Sigma]2 + \[Tau]2, \[Sigma]1 + \[Tau]1 ], (* \[Sigma]2 *)
              i == 7, If[ OddQ[j], \[Sigma]2 + \[Tau]2, \[Sigma]1 + \[Tau]1 ], (* \[Tau]1 *)
              i == 8, If[ EvenQ[j], \[Sigma]2 + \[Tau]2, \[Sigma]1 + \[Tau]1 ], (* \[Tau]2 *)
              i >= 9,
                With[{ii = i - 8 },
                  Which[
                    ii == j && (2 j < p),
                      Id + \[CapitalTheta] + \[Phi][ 2 j ],
                    ii == j && (2*j > p),
                      Id + \[CapitalTheta] + \[Phi][ 2 (p - j) ],
                    ii + j  < p,
                      \[Phi][ Abs[ ii - j ] ] + \[Phi][ ii + j  ],
                    ii + j > p,
                      \[Phi][ Abs[ ii - j ] ] + \[Phi][ 2 * p - ii - j ],
                    ii == p - j ,
                      \[CapitalPhi]1 + \[CapitalPhi]2 + \[Phi][ Abs[ p - 2 ii ] ]
                  ]	 (* \[Phi]_\[Lambda]'s *)
                ]
            ],
          {i, rank}
        ];

    Join[
      {  matId, mat\[CapitalTheta], mat\[CapitalPhi]1, mat\[CapitalPhi]2, mat\[Sigma]1, mat\[Sigma]2, mat\[Tau]1, mat\[Tau]2 },
       mat\[Phi] /@ Range[rank - 8]
    ]
  ];

rulesdiv4[ p_ ] :=
  Module[
    {
      matId, mat\[CapitalTheta], mat\[CapitalPhi]1, mat\[CapitalPhi]2, mat\[Sigma]1, mat\[Sigma]2, mat\[Tau]1,
      mat\[Tau]2, mat\[Phi], rank, Id, \[CapitalTheta], \[CapitalPhi]1, \[CapitalPhi]2, \[Sigma]1, \[Sigma]2, \[Tau]1,
      \[Tau]2, \[Phi], sumEven\[Lambda]s, sumOdd\[Lambda]s
    },
    rank =
        p + 7;

    Evaluate[
      Join[
        { Id, \[CapitalTheta], \[CapitalPhi]1, \[CapitalPhi]2, \[Sigma]1, \[Sigma]2, \[Tau]1, \[Tau]2 },
        \[Phi] /@ Range[ rank - 8 ]
      ]
    ] = IdentityMatrix[ rank ];

    sumEven\[Lambda]s =
        Sum[ \[Phi][i], { i, 2, p - 1, 2 } ];

    sumOdd\[Lambda]s =
        Sum[ \[Phi][i], { i, 1, p - 1, 2 } ];

    matId =
        IdentityMatrix[ rank ];

    mat\[CapitalTheta] =
        Table[
          Which[
              i == 1, \[CapitalTheta], (* 1 *)
              i == 2, Id, (* \[CapitalTheta] *)
              i == 3, \[CapitalPhi]2, (* \[CapitalPhi]1 *)
              i == 4, \[CapitalPhi]1, (* \[CapitalPhi]2 *)
              i == 5, \[Tau]1, (* \[Sigma]1 *)
              i == 6, \[Tau]2, (* \[Sigma]2 *)
              i == 7, \[Sigma]1, (* \[Tau]1 *)
              i == 8, \[Sigma]2, (* \[Tau]2 *)
              i >= 9, \[Phi][i - 8]	 (* \[Phi]_\[Lambda]'s *)
            ],
          {i, rank}
        ];

    mat\[CapitalPhi]1 =
        Table[
          Which[
              i == 1, \[CapitalPhi]1, (* 1 *)
              i == 2, \[CapitalPhi]2, (* \[CapitalTheta] *)
              i == 3, Id, (* \[CapitalPhi]1 *)
              i == 4, \[CapitalTheta], (* \[CapitalPhi]2 *)
              i == 5, \[Tau]1, (* \[Sigma]1 *)
              i == 6, \[Sigma]2, (* \[Sigma]2 *)
              i == 7, \[Sigma]1, (* \[Tau]1 *)
              i == 8, \[Tau]2, (* \[Tau]2 *)
              i >= 9, \[Phi][p - (i - 8) ]	 (* \[Phi]_\[Lambda]'s *)
            ],
          {i, rank}
        ];

    mat\[CapitalPhi]2 =
        Table[
          Which[
              i == 1, \[CapitalPhi]2, (* 1 *)
              i == 2, \[CapitalPhi]1, (* \[CapitalTheta] *)
              i == 3, \[CapitalTheta], (* \[CapitalPhi]1 *)
              i == 4, Id, (* \[CapitalPhi]2 *)
              i == 5, \[Sigma]1, (* \[Sigma]1 *)
              i == 6, \[Tau]2, (* \[Sigma]2 *)
              i == 7, \[Tau]1, (* \[Tau]1 *)
              i == 8, \[Sigma]2, (* \[Tau]2 *)
              i >= 9, \[Phi][p - (i - 8)]	 (* \[Phi]_\[Lambda]'s *)
            ],
          {i, rank}
        ];

    mat\[Sigma]1 =
        Table[
          Which[
              i == 1, \[Sigma]1, (* 1 *)
              i == 2, \[Tau]1, (* \[CapitalTheta] *)
              i == 3, \[Tau]1, (* \[CapitalPhi]1 *)
              i == 4, \[Sigma]1, (* \[CapitalPhi]2 *)
              i == 5,
    Id + \[CapitalPhi]2 + sumEven\[Lambda]s, (* \[Sigma]1 *)
              i == 6, sumOdd\[Lambda]s, (* \[Sigma]2 *)

    i == 7, \[CapitalTheta] + \[CapitalPhi]1 +
     sumEven\[Lambda]s, (* \[Tau]1 *)
              i == 8, sumOdd\[Lambda]s, (* \[Tau]2 *)
              i >= 9,
    If[ OddQ[
      i], \[Sigma]2 + \[Tau]2, \[Sigma]1 + \[Tau]1 ]	 (* \[Phi]_\\[Lambda]'s *)
            ],
          {i, rank}
        ];

    mat\[Sigma]2 =
        Table[
          Which[
              i == 1, \[Sigma]2, (* 1 *)
              i == 2, \[Tau]2, (* \[CapitalTheta] *)
              i == 3, \[Sigma]2, (* \[CapitalPhi]1 *)
              i == 4, \[Tau]2, (* \[CapitalPhi]2 *)
              i == 5, sumOdd\[Lambda]s, (* \[Sigma]1 *)
              i == 6,
    Id + \[CapitalPhi]1 + sumEven\[Lambda]s, (* \[Sigma]2 *)
              i == 7, sumOdd\[Lambda]s, (* \[Tau]1 *)

    i == 8, \[CapitalTheta] + \[CapitalPhi]2 +
     sumEven\[Lambda]s, (* \[Tau]2 *)
              i >= 9,
    If[ EvenQ[
      i], \[Sigma]2 + \[Tau]2, \[Sigma]1 + \[Tau]1 ]	 (* \[Phi]_\
\[Lambda]'s *)
            ],
          {i, rank}
        ];

    mat\[Tau]1 =
        Table[
          Which[
              i == 1, \[Tau]1, (* 1 *)
              i == 2, \[Sigma]1, (* \[CapitalTheta] *)
              i == 3, \[Sigma]1, (* \[CapitalPhi]1 *)
              i == 4, \[Tau]1, (* \[CapitalPhi]2 *)

    i == 5, \[CapitalTheta] + \[CapitalPhi]1 +
     sumEven\[Lambda]s, (* \[Sigma]1 *)
              i == 6, sumOdd\[Lambda]s, (* \[Sigma]2 *)
              i == 7,
    Id + \[CapitalPhi]2 + sumEven\[Lambda]s, (* \[Tau]1 *)
              i == 8, sumOdd\[Lambda]s, (* \[Tau]2 *)
              i >= 9,
    If[ OddQ[
      i], \[Sigma]2 + \[Tau]2, \[Sigma]1 + \[Tau]1 ]	 (* \[Phi]_\
\[Lambda]'s *)
            ],
          {i, rank}
        ];

    mat\[Tau]2 =
        Table[
          Which[
              i == 1, \[Tau]2, (* 1 *)
              i == 2, \[Sigma]2, (* \[CapitalTheta] *)
              i == 3, \[Tau]2, (* \[CapitalPhi]1 *)
              i == 4, \[Sigma]2, (* \[CapitalPhi]2 *)
              i == 5, sumOdd\[Lambda]s, (* \[Sigma]1 *)

    i == 6, \[CapitalTheta] + \[CapitalPhi]2 +
     sumEven\[Lambda]s, (* \[Sigma]2 *)
              i == 7, sumOdd\[Lambda]s, (* \[Tau]1 *)
              i == 8,
    Id + \[CapitalPhi]1 + sumEven\[Lambda]s, (* \[Tau]2 *)
              i >= 9,
    If[ EvenQ[
      i], \[Sigma]2 + \[Tau]2, \[Sigma]1 + \[Tau]1 ]	 (* \[Phi]_\
\[Lambda]'s *)
            ],
          {i, rank}
        ];

    mat\[Phi][j_] :=
        Table[
          Which[
              i == 1, \[Phi][j], (* 1 *)
              i == 2, \[Phi][j], (* \[CapitalTheta] *)
              i == 3, \[Phi][p - j], (* \[CapitalPhi]1 *)
              i == 4, \[Phi][p - j], (* \[CapitalPhi]2 *)
              i == 5,
    If[ OddQ[
      j], \[Sigma]2 + \[Tau]2, \[Sigma]1 + \[Tau]1 ], (* \[Sigma]1 *)
              i == 6,
    If[ EvenQ[
      j], \[Sigma]2 + \[Tau]2, \[Sigma]1 + \[Tau]1 ], (* \[Sigma]2 *)
              i == 7,
    If[ OddQ[
      j], \[Sigma]2 + \[Tau]2, \[Sigma]1 + \[Tau]1 ], (* \[Tau]1 *)
              i == 8,
    If[ EvenQ[
      j], \[Sigma]2 + \[Tau]2, \[Sigma]1 + \[Tau]1 ], (* \[Tau]2 *)
              i >= 9,
                With[{ii = i - 8 },
                Which[
                    ii == j && (2 j < p),
                      Id + \[CapitalTheta] + \[Phi][ 2 j ],
                    ii == j && 2 j == p,
                      Id + \[CapitalTheta] + \[CapitalPhi]1 + \[CapitalPhi]2,
                    ii == j && (2*j > p),
                      Id + \[CapitalTheta] + \[Phi][ 2 (p - j) ],
                    ii + j  < p,
                      \[Phi][ Abs[ ii - j ] ] + \[Phi][ ii + j  ],
                    ii + j > p,
                      \[Phi][ Abs[ ii - j ] ] + \[Phi][ 2 * p - ii - j ],
                    ii == p - j ,
                      \[CapitalPhi]1 + \[CapitalPhi]2 + \[Phi][
        Abs[ p - 2 ii ] ]
                  ]	 (* \[Phi]_\[Lambda]'s *)
                ]
            ],
          {i, rank}
        ];

    Join[
      {  matId, mat\[CapitalTheta], mat\[CapitalPhi]1, mat\[CapitalPhi]2, mat\[Sigma]1, mat\[Sigma]2, mat\[Tau]1, mat\[Tau]2 },
       mat\[Phi] /@ Range[rank - 8]
    ]
  ];



PackageExport["FusionRingMetaplectic"]

FusionRingMetaplectic::usage =
  "FusionRingMetaplectic[m] returns the metaplectic fusion ring with dimension 4m, where m is odd.";

FusionRingMetaplectic =
  FusionRingSON2;
