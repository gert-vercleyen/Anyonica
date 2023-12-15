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

Options[ FusionCategory ] =
  {
    "FusionRing"        -> Missing[],
    "FSymbols"          -> Missing[],
    "RSymbols"          -> Missing[],
    "FormalParameters"  -> Missing[],
    "Unitary"           -> Missing[],
    "PivotalStructure"  -> Missing[],
    "Dimensions"        -> Missing[],
    "Twists"            -> Missing[],
    "SMatrix"           -> Missing[],
    "Modular"           -> Missing[],
    "PreEqualCheck"     -> Identity,
    "SimplifyBy"        -> Identity,
    "SkipCheck"         -> False
  };

FusionCategory[ ops:OptionsPattern[] ] :=
  FusionCategory[ InitializeFusionCategory[ ops ] ];

Options[ InitializeFusionCategory ] :=
  Options[ FusionCategory ];

InitializeFusionCategory[ ops:OptionsPattern[] ] :=
  Module[
    { ring, fSymbols, rSymbols, preEqualCheck, skipCheck, pivStruct, dims, twists, sMat, modular },
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
    dims =
      OptionValue[ "Dimensions" ];
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
      (vc = PentagonValidityConstraints[ ring, fsymbols, preEqualCheck ]) =!= {},
        Message[ FusionCategory::invalidfsymbols, vc ]; False,
      !MissingQ[rsymbols] && (vc = HexagonValidityConstraints[ ring, fsymbols, rsymbols, preEqualCheck ] ) =!= {},
        Message[ FusionCategory::invalidrsymbols, vc ]; False,
      True,
        True
    ]
  ];

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

(* List of functions to inherit *)
InheritedFCFunctionsFromFRList =
  {
    MultiplicationTable, MT,
    SymbolicMultiplicationTable, SMT,
    ElementsName,
    ElementNames,
    AntiparticleMatrix, AM,
    CommutativeQ, CQ,
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
    InjectionForm
    (*FusionProduct*)
  };

InheritedFCFunctionFromFRQ[ f_ ] :=
  MemberQ[f] @ InheritedFCFunctionsFromFRList;

(* If function from above list is applied to FusionCategory, redirect it to the fusion ring *)
FusionCategory /: f_[ FusionCategory[ data_ ] ] :=
  f[ data["FusionRing"] ] /; InheritedFCFunctionFromFRQ[f];

FusionCategory /: f_[ FusionCategory[ data1_ ], FusionCategory[ data2_ ] ] :=
  f[ data1["FusionRing"], data2["FusionRing"] ] /; InheritedFCFunctionFromFRQ[f];

(* TODO: atm it only works for functions with 1 or 2 arguments: should work for n arguments *)

(* Standard getters *)

FusionCategory /: FusionRing[ FusionCategory[data_] ] :=
  data["FusionRing"];

FusionCategory /: FormalCode[ FusionCategory[data_] ] :=
  data["FormalParameters"];


PackageExport["FSymbols"]

FusionCategory /: FSymbols[ FusionCategory[data_] ] :=
  data["FSymbols"];


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
		SetDirectory @ NotebookDirectory[]],SetDirectory::fstr   (* If using notebook interface *)
	];


PackageExport["FusionCategoryByCode"]

FusionCategoryByCode::usage =
	"FusionCategoryByCode[sixTuple] returns the fusion category with formal code equal to six-tuple.";


PackageExport["FCBC"]

FCBC::usage =
	"Shorthand for FusionCategoryByCode.";

FusionCategoryByCode =
	FCBC =
		OptimizedImport[ "FusionCategoryAssociation", importDirectory ];


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
    Select[ Keys @ FCBC, #[[;;4]] == fc& ]
  ];

Format[ cat:FusionCategory[r_Association], StandardForm ] :=
  With[ { CFP = r["FormalParameters"], rn = Names @ r["FusionRing"] },
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
      "FC"[ Sequence @@ FC @ r["FusionRing"], "_" ]
    ]
  ];
