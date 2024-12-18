(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2022-04-17 *)

Package["Anyonica`"]

FusionCategories::usage =
"This package contains a list of fusion categories and tools for working with fusion categories. Evaluate ?FusionCategories`* for a list of all functions.";

(*
   All the core data will be stored in an Association for convenience of
   access. We still want FusionCategory to be the head of the object so
   we store the association inside the function FusionRing, which will
   only serve as a wrapper and never return a value
*)

PackageExport["FusionCategory"]

FusionCategory::usage =
  "FusionCategory[ \"GrothendieckRing\" -> r, \"FSymbols\" -> fSymbols, opts ] initializes a fusion category with fusion ring r and F-symbols fSymbols. Extra options include \"RSymbols\"-> rSymbols to initialize a braided fusion category, \"PreEqualCheck\" -> f to apply a function f to both sides of each pentagon equation before checking equality.";

FusionCategory::nofusionring =
  "No fusion ring is provided so no category could be initialized";
FusionCategory::nofsymbols =
  "No F-symbols were provided so no category could be initialized";
FusionCategory::wrongfsymbolsformat =
  "The format for the F-symbols should be a list of the form { \[ScriptCapitalF][indices__] -> value_, ... } or a list of the form { {indices__} -> value }";
FusionCategory::wrongringformat =
  "The fusion ring should be a fusion ring object from the FusionRings package.";
FusionCategory::wrongrsymbolsformat =
  "The format for the R-symbols should be a list of the form { \[ScriptCapitalR][__] ->_, ... }";
FusionCategory::invalidfsymbols =
  "The F-symbols are not a valid solution to the pentagon equations. This might be due Mathematica being unable to simplify certain expressions. If so, use the option \"PreEqualCheck\" to force simplification. Invalid equations:\n`1`";
FusionCategory::invalidrsymbols =
  "The R-symbols are not a valid solution to the hexagon equations. This might be due Mathematica being unable to simplify certain expressions. If so, use the option \"PreEqualCheck\" to force simplification. Invalid equations:\n`1`";
FusionCategory::invaliddata = 
  "The data provided does not correspond to a fusion category."

Options[ FusionCategory ] =
  {
    "FusionRing"              -> Missing[],
    "FSymbols"                -> Missing[],
    "RSymbols"                -> Missing[],
    "FormalParameters"        -> Missing[],
    "Unitary"                 -> Missing[],
    "PivotalStructure"        -> Missing[],
    "Twists"                  -> Missing[],
    "SMatrix"                 -> Missing[],
    "Modular"                 -> Missing[],
    "PreEqualCheck"           -> Identity,
    "ReplaceRingByKnownRing"  -> False,
    "SkipCheck"               -> False
  };

FusionCategory[ ops:OptionsPattern[] ] :=
  FusionCategory[ InitializeFusionCategory[ ops ] ];

Options[ InitializeFusionCategory ] :=
  Options[ FusionCategory ];

InitializeFusionCategory[ ops:OptionsPattern[] ] :=
  Module[
    { ring, fSymbols, rSymbols, preEqualCheck, skipCheck, pivStruct, twists, sMat, modular },
    ring =
      OptionValue[ "FusionRing" ];
    fSymbols =
      OptionValue[ "FSymbols" ];
    rSymbols =
      OptionValue[ "RSymbols" ];
    preEqualCheck =
      OptionValue[ "PreEqualCheck" ];
    skipCheck =
      OptionValue[ "SkipCheck" ];
    pivStruct =
      OptionValue[ "PivotalStructure" ];
    twists =
      OptionValue[ "Twists" ];
    sMat =
      OptionValue[ "SMatrix" ];
    modular =
      OptionValue[ "Modular" ];

    If[
      !skipCheck && !ValidInitalizationDataQ[ ring, fSymbols, rSymbols, preEqualCheck ],
      Message[ FusionCategory::invaliddata ]
    ];

    If[ 
      OptionValue["ReplaceRingByKnownRing"], 
      ring = Hold[FRBC][ FC @ ReplaceByKnownRing @ ring ]
    ];

    Association[
      "FusionRing"                -> ring,
      "FSymbols"                  -> fSymbols,
      "RSymbols"                  -> rSymbols,
      "PivotalStructure"          -> pivStruct,
      "Twists"                    -> twists,
      "SMatrix"                   -> sMat,
      "Modular"                   -> modular,
      "FormalParameters"          -> OptionValue["FormalParameters"],
      "Unitary"                   -> OptionValue["Unitary"],
      "Names"                     -> Missing[]
    ]
  ];

ValidInitalizationDataQ[ ring_, fsymbols_, rsymbols_, preEqualCheck_ ] :=
  Module[
    {vc},
    Which[
      MissingQ @ ring,
        Message[ FusionCategory::nofusionring ]; False,
      MissingQ @ fsymbols,
        Message[ FusionCategory::nofsymbols ]; False,
      !ProperFSymbolRulesQ[ fsymbols ],
        Message[ FusionCategory::wrongfsymbolsformat ]; False,
      !FusionRingQ[ ring ],
        Message[ FusionCategory::wrongringformat ]; False,
      !MissingQ[ rsymbols ] && !ProperRSymbolRulesQ[ rsymbols ],
        Message[ FusionCategory::wrongrsymbolsformat ]; False,
      !TrueQ[(vc = CheckPentagonEquations[ ring, fsymbols, "PreEqualCheck" -> preEqualCheck ])],
        Message[ FusionCategory::invalidfsymbols, vc[[2]] ]; False,
      !MissingQ[rsymbols] && 
      !TrueQ[
        vc = CheckHexagonEquations[ ring, fsymbols, rsymbols, "PreEqualCheck" -> preEqualCheck ]
      ],
        Message[ FusionCategory::invalidrsymbols, vc[[2]] ]; False,
      True,
        True
    ]
  ];

(* Replaced by the much faster CheckPentagonEquations
PentagonValidityConstraints[ ring_, fSymbols_, preEqualCheck_ ] :=
  With[{
    pEqns =
      Map[
        preEqualCheck,
        PentagonEquations[ring]/.Dispatch[fSymbols],
        { 2 }
      ]
    },
    pEqns //
    DeleteCases[True] //
    DeleteDuplicates
  ];
*)

Options[CheckPentagonEquations] = 
	{
		"PreEqualCheck" -> Identity
	};

CheckPentagonEquations[ ring_, fSymbols_, OptionsPattern[]  ] := 
  Module[ { fInd, p, c, d, e, q, r, a, b, s, n, matches, eqn, simplify,sF },
    n = Rank @ ring;
    fInd = List @@@ fSymbols[[;;,1]];
    sF = SparseArray[ Thread[ fInd -> Values @ fSymbols ], { n, n, n, n, n, n } ];
    simplify = OptionValue["PreEqualCheck"];
    Catch[
      Do[
        { p, c, d, e, q, r } = label;
        matches = Cases[ fInd, { a_, b_, r, e, p, s_ } ];
        Do[
          { a, b, s } = 
            label2[[ { 1, 2, 6 } ]];

          eqn = 
            simplify[ 
              sF[[p,c,d,e,q,r]] sF[[a,b,r,e,p,s]] ==
              Sum[ sF[[b,c,d,s,x,r]] sF[[a,b,c,q,p,x]] sF[[a,x,d,e,q,s]], {x,n} ] 
            ];

          If[ 
            !TrueQ[ eqn ], 
            Throw[ { False, F[p,c,d,e,q,r] F[a,b,r,e,p,s] ==
            Sum[ F[b,c,d,s,x,r] F[a,b,c,q,p,x] F[a,x,d,e,q,s], {x,n} ] } ]
          ],
          { label2, matches }
        ],
        { label, fInd }
      ];
      True
    ]
  ];

HexagonValidityConstraints[ ring_, fSymbols_, rSymbols_, preEqualCheck_ ] :=
  With[{
    hEqns =
      Map[
        preEqualCheck,
        HexagonEquations[ring]/.Dispatch[Join[ fSymbols, rSymbols ] ],
        { 2 }
      ]
    },
    hEqns //
    DeleteCases[True] //
    DeleteDuplicates
  ];

Options[CheckHexagonEquations] = 
  {
    "PreEqualCheck" -> Identity
  };

CheckHexagonEquations[ ring_, fSymbols_, rSymbols_, OptionsPattern[] ] := 
  Module[{ a, b, c, d, e, g, sR, sF, rank, matchingLabels, eqn1, eqn2, fLabels, simplify },
    simplify = 
      OptionValue["PreEqualCheck"];

    fLabels =
      List @@@ Keys[fSymbols];
    
    rank =
      Rank[ring];

    (* construct a sparse array of R symbols *)
    sR =
      SparseArray[
        Thread[ NZSC[ring] -> Values[rSymbols] ],
        { rank, rank, rank }
      ];

    sF =
      SparseArray[
        Thread[ fLabels -> Values[fSymbols] ],
        { rank, rank, rank, rank, rank, rank }
      ];

    matchingLabels[ { a_, c_, b_, d_, e_} ] :=
      Cases[ fLabels, { c, a, b, d, e, _ } ][[;;,6]];

    Catch[
      Do[ (* Loop over valid F-symbols and construct equations *)
        { a, c, b, d, e, g } = fl;
          
        eqn1 = 
          simplify[   
            sR[[ c, a, e ]] sF[[a,c,b,d,e,g]] sR[[ c, b, g ]]
            ==
            Sum[
              sF[[ c, a, b, d, e, f ]] sR[[ c, f, d ]] sF[[ a, b, c, d, f, g ]],
              { f, matchingLabels[ { a, c, b, d, e } ] }
            ]
          ];

        If[ 
          !TrueQ[ eqn1 ],
          EchoLabel["eqn1"]@eqn1;
          Throw @
          { 
            False, 
            R[ c, a, e ] F[a,c,b,d,e,g] R[ c, b, g ] ==
            Sum[
              F[ c, a, b, d, e, f ] R[ c, f, d ] F[ a, b, c, d, f, g ],
              { f, matchingLabels[ { a, c, b, d, e } ] }
            ]
          }
        ];

        eqn2 = 
          simplify[
            sR[[ a, c, e ]]^(-1) sF[[a,c,b,d,e,g]] sR[[ b, c, g ]]^(-1) ==
            Sum[
              sF[[ c, a, b, d, e, f ]] sR[[ f, c, d ]]^(-1) sF[[ a, b, c, d, f, g ]],
              { f, matchingLabels[ { a, c, b, d, e } ] }
            ]
          ];
        
        If[
          !TrueQ[ eqn2 ],
                    EchoLabel["eqn2"]@eqn2;
          Throw @ 
          {
            False,
            R[ a, c, e ]^(-1) F[a,c,b,d,e,g] R[ b, c, g ]^(-1) ==
            Sum[
              F[ c, a, b, d, e, f ] R[ f, c, d ]^(-1) F[ a, b, c, d, f, g ],
              { f, matchingLabels[ { a, c, b, d, e } ] }
            ]
          }
        ]  
        ,
        { fl, fLabels }
      ];
      True
    ]
  ];


ProperFSymbolRulesQ[ fSymbols_ ] :=
  PPSQ[ fSymbols ];

ProperLabelRulesQ[ fSymbols_ ] :=
  MatchQ[ fSymbols, { Repeated[ { a__ } -> b_ ] } /; Length[{a}] == 6 ];

ProperRSymbolRulesQ[ rSymbols_ ] :=
  PHSQ[ rSymbols ];

(*
+---------------------------------------------------------------------------+
|                         FUNCTIONS FOR FUSION RINGS                        |
+---------------------------------------------------------------------------+
*)

(*
  Here we overload certain functions for fusion rings to functions on fusion categories.
*)

FusionCategory /: FusionRing[ FusionCategory[data_] ] :=
  With[ { r = data["FusionRing"] },
    If[
      Head[r] === Hold[FRBC],
      ReleaseHold[r],
      r
    ]
  ];

(* List of functions to inherit *)
InheritedFCFunctionsFromFRList =
  {
    MultiplicationTable, MT,
    SymbolicMultiplicationTable, SMT,
    ElementNames,
    AntiparticleMatrix, AM,
    CommutativeQ, CQ,
    GroupRingQ,
    Multiplicity, Mult,
    MultiplicityFreeQ, MFQ,
    NonZeroStructureConstants, NZSC,
    NNonZeroStructureConstants, NNZSC,
    Rank,
    FrobeniusPerronDimensions, FPDims,
    FrobeniusPerronDimension, FPDim,
    NSelfDual, NSD,
    NNonSelfDual, NNSD,
    NSelfDualNonSelfDual, NSDNSD,
    ConjugateCharge, CC,
    FusionRingAutomorphisms, FRA,
    InjectionForm
    (*FusionProduct*)
  };

InheritedFCFunctionFromFRQ[ f_ ] :=
  MemberQ[f] @ InheritedFCFunctionsFromFRList;

(* If function from above list is applied to FusionCategory, redirect it to the fusion ring *)
FusionCategory /: f_[ cat:FusionCategory[ data_ ] ] :=
  f[ FusionRing[cat] ] /; InheritedFCFunctionFromFRQ[f];

FusionCategory /: f_[ cat1:FusionCategory[ data1_ ], cat2:FusionCategory[ data2_ ] ] :=
  f[ FusionRing[cat1], FusionRing[cat2] ] /; InheritedFCFunctionFromFRQ[f];

(* TODO: atm it only works for functions with 1 or 2 arguments: should work for n arguments *)

(* Standard getters *)

FusionCategory /: FormalCode[ FusionCategory[data_] ] :=
  data["FormalParameters"];
  
FusionCategory /: FC[ FusionCategory[data_] ] :=
  data["FormalParameters"];

PackageExport["FSymbols"]

FusionCategory /: FSymbols[ FusionCategory[data_] ] :=
  data["FSymbols"];

PackageExport["FMatrices"]

FMatrices[ cat_FusionCategory ] := 
  ReplaceAll[ FMatrices @ FusionRing @ cat, FSymbols @ cat ];

PackageExport["RSymbols"]

FusionCategory /: RSymbols[ FusionCategory[data_] ] :=
  data["RSymbols"];


PackageExport["UnitaryQ"]

UnitaryQ::usage =
  "UnitaryQ[fusionCat] returns True if it is known that the category has a unitary gauge.";

FusionCategory /: UnitaryQ[ FusionCategory[data_] ] :=
  data["Unitary"];


(* Import the FusionRingList *)
currentDirectory =
	Directory[];

importDirectory =
	Quiet[
		Check[ SetDirectory @ DirectoryName @ $InputFileName,    (* If not using notebook interface *)
		SetDirectory @ NotebookDirectory[]], SetDirectory::fstr   (* If using notebook interface *)
	];


PackageExport["FusionCategoryByCode"]

FusionCategoryByCode::usage =
	"FusionCategoryByCode[sevenTuple] returns the fusion category with formal code equal to sevenTuple."<>
  "\nFusionCategoryByCode[sixTuple] returns the multiplicity-free category with formal code equal to sixTuple.";

FusionRingByCode::notsixorseventuple = 
	"The input `1` should be a six or seventuple of natural numbers.";

PackageExport["FCBC"]

FCBC::usage =
	"Shorthand for FusionCategoryByCode.";

PackageScope["FCBCData"]

FCBCData =
	OptimizedImport[ "FusionCategoryAssociation", importDirectory ];

FusionCategoryByCode[ tuple_List ] :=
	Switch[ Length @ tuple, 
		7, FCBCData @ tuple,
		6, FCBCData @ Join[ { tuple[[1]] }, { 1 }, Rest @ tuple ],
		_, Message[ FusionCategoryByCode::notsixorseventuple, tuple ]
	];

FCBC = 
  FusionCategoryByCode;

PackageExport["FusionCategoryList"]

FusionCategoryList::usage =
	"FusionCategoryList is a list of all saved FusionRing objects.";


PackageExport["FCL"]

FCL::usage =
	"Shorthand for FusionCategoryList.";

FCL =
	FusionCategoryList =
    OptimizedImport[ "FusionCategoryList", importDirectory ];

SetDirectory @
	currentDirectory;

PackageExport["FusionCategories"]

FusionCategories::usage =
  "FusionCategories[ring] returns all stored fusion categories with ring as Grothendieck ring.";

FusionCategories[ ring_FusionRing ] :=
  Module[ { fc = FC @ ring },
    If[ MissingQ[fc], fc = FC @ ReplaceByKnownRing[ring] ];

    FCBC /@
    Select[ Keys @ FCBCData, #[[;;4]] == fc& ]
  ];

Format[ cat:FusionCategory[r_Association], StandardForm ] :=
  With[{ CFP = r["FormalParameters"], rn = Names @ FusionRing[cat] },
    Which[
      !MissingQ[CFP] && rn =!= {}
      ,
      "FC"[
        "\!\(\*SubsuperscriptBox[\(["<> First @ rn <>"]\),"<>
        " \("<> ToString[CFP[[-3]]]<>","<>ToString[CFP[[-2]]]<>"\)," <>
        "\( "<> ToString[CFP[[-1]]]<> " \)"<>"]\)"
      ]
      ,
      !MissingQ[CFP]
      ,
      "FC"[ Sequence @@ CFP ]
      ,
      True
      ,
      "FC"[ Sequence @@ FC @ FusionRing[cat], "_" ]
    ]
  ];


