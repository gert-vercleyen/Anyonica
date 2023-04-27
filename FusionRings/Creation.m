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
( FusionRing[ r_ ]?FusionRingQ )[ k_ ] :=
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
      Message[ FusionRing::elnameslength, Length[ elNames ], Length[ multTab ] ],
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

SetAttributes[ FusionRingQ, HoldFirst ];

FusionRingQ[ r_FusionRing ] :=
  System`Private`HoldValidQ[ r ];

FusionRingQ[ _ ] :=
  False;

FusionRingQ[ s_Symbol ] :=
  And[
    Head[s] === FusionRing,
    FusionRingQ @ Evaluate @ s
  ];

validateFusionRing[ r_Association ] :=
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


(* Constructors for some common fusion rings *)

(* Z_n *)
PackageExport["FusionRingZn"]

FusionRingZn::usage =
"FusionRingZn[n] returns the group fusion ring \!\(\*SubscriptBox[\(\[DoubleStruckCapitalZ]\), \(n\)]\)";

FusionRingZn[ n_Integer ] :=
  FusionRing @ Sequence[
    Rule[ "MultiplicationTable",
      Table[
        If[ Mod[c-1,n] == Mod[a+b-2,n],
          1,
          0
        ],
        { a, n }, { b, n }, { c, n }
      ]
    ],
    Rule[ "Names", {"\!\(\*SubscriptBox[\(\[DoubleStruckCapitalZ]\), \("<>ToString[n]<>"\)]\)"} ]
  ];


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

(*Its name will be set to that of the group if the group is a PermutationGroup, SymmetricGroup, AlternatingGroup, CyclicGroup, DihedralGroup or AbelianGroup, or will be the value given by the option \"Names\".*)
FusionRingFromGroup[ table_?MatrixQ, OptionsPattern[] ] := With[ {
  n = Length[ table ]},
  If[ GroupTableQ[table],
    FusionRing @ Sequence[
      Rule[
        "MultiplicationTable",
        Table[
          If[ k == table[[i,j]], 1 , 0]
          , { i, n }, { j, n }, { k, n }]
      ],
      Rule[
        "Names",
        OptionValue["Names"]
      ]
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
  With[{
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
  With[{
    n = tab // Length },
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

