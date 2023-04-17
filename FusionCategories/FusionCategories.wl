(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2022-04-17 *)

BeginPackage["FusionCategories`"];

(* Messages *)
Get[ FileNameJoin[{ DirectoryName[$InputFileName], "Messages.wl" }]  ];

(* Options *)
Options[ FusionCategory ] =
  {
    "FusionRing"    -> Missing[],
    "FSymbols"      -> Missing[],
    "RSymbols"      -> Missing[],
    "PreEqualCheck" -> Identity
  };


(* Begin private context *)

Begin["`Private`"];

(*
   All the core data will be stored in an Association for convenience of
   access. We still want FusionCategory to be the head of the object so
   we store the association inside the function FusionRing, which will
   only serve as a wrapper and never return a value
*)

FusionCategory[ ops:OptionsPattern[] ] :=
  FusionCategory[ InitializeFusionCategory[ ops ] ];

Options[ InitializeFusionCategory ] = Options[ FusionCategory ];
InitializeFusionCategory[ ops:OptionsPattern[] ] :=
  Module[
    { ring, fSymbols, rSymbols, preEqualCheck, braidedQ },
    ring =
      OptionValue[ "FusionRing" ];
    fSymbols =
      OptionValue[ "FSymbols" ];
    rSymbols =
      OptionValue[ "RSymbols" ];
    preEqualCheck =
      OptionValue[ "PreEqualCheck" ];
    braidedQ =
      rSymbols === {} || MissingQ[ rSymbols ];

    If[
      !ValidInitalizationDataQ[ ring, fSymbols, rSymbols, preEqualCheck ],
      Message[ FusionCategory::invaliddata ]
    ];

    Association[
      "FusionRing"                -> ring,
      "FSymbols"                  -> fSymbols,
      "RSymbols"                  -> rSymbols,
      "FrobeniusSchurIndicator"   -> Missing[],
      "UnitaryFsymbolsQ"          -> Missing[],
      "HasUnitaryGaugeQ"          -> Missing[],
      "SMatrix"                   -> Missing[],
      "Twistfactors"              -> Missing[],
      "Names"                     -> Missing[],
      "CentralCharge"             -> Missing[],
      "BraidedQ"                  -> braidedQ
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
      !ProperFSymbolRulesQ[ fsymbols ] || !ProperLabelRulesQ[ fsymbols ],
        Message[ FusionCategory::wrongfsymbolsformat ]; False,
      !FusionRingQ[ ring ],
        Message[ FusionCategory::wrongringformat ]; False,
      !MissingQ[ rsymbols ] && !ProperRSymbolRulesQ[ rsymbols ],
        Message[ FusionCategory::wrongrsymbolsformat ]; False,
      (vc = PentagonValidityConstraints[ ring, fsymbols, preEqualCheck ]) =!= {},
        Message[ FusionCategory::invalidfsymbols, vc ]; False,
      rsymbols =!= Missing[] && (vc = HexagonValidityConstraints[ ring, fsymbols, rsymbols, preEqualCheck ] ) =!= {},
        Message[ FusionCategory::invalidrsymbols, vc ]; False,
      True,
        True
    ]
  ];

PentagonValidityConstraints[ ring_, fSymbols_, preEqualCheck_] :=
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

HexagonValidityConstraints[ ring_, rSymbols_, preEqualCheck_ ] :=
  Missing["NotImplementedYet"];

ProperFSymbolRulesQ[ fSymbols_ ] :=
  ProperListOfPentagonSolutionsQ[ fSymbols ];

ProperLabelRulesQ[ fSymbols_ ] :=
  MatchQ[ fSymbols, { Repeated[ { a__ } -> b_ ] } /; Length[{a}] == 6 ];

ProperRSymbolRulesQ[ rSymbols_ ] :=
  ProperListOfPentagonSolutionsQ[ rSymbols ];

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
    QuantumDimensions, QD,
    TotalQuantumDimensionSquared, TQDS,
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