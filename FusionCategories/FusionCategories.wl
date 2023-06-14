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
      "CentralCharge"             -> Missing[]
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

(* Standard getters *)

FusionCategory /: FusionRing[ FusionCategory[data_] ] :=
  data["FusionRing"];

FusionCategory /: FSymbols[ FusionCategory[data_] ] :=
  data["FSymbols"];

FusionCategory /: RSymbols[ FusionCategory[data_] ] :=
  data["RSymbols"];


FusionCategory /: DirectProduct[ fc1: FusionCategory[ data1_ ], fc2: FusionCategory[ data2_ ] ] :=
  Module[{ r1, r2, r, fs1, fs2, fs, rs1, rs2, rs, tupleToSingle, sTupleToSingle },
    r1 = FusionRing[fc1]; r2 = FusionRing[fc2];
    fs1 = FSymbols[fc1]; fs2 = FSymbols[fc2];
    rs1 = RSymbols[fc1]; rs2 = RSymbols[fc2];

    r = DirectProduct[ r1, r2 ];

    tupleToSingle[a_,b_] :=
      ReplaceAll[
        { a, b },
        Thread[
          Flatten[ Table[ { i, j }, { i, Rank[r1] }, { j, Rank[r2] } ], 1 ] ->
          Range[ Rank[r1] * Rank[r2] ]
        ]
      ];

    sTupleToSingle[ s_[i1__], s_[i2__] ] :=
      s @@ MapThread[ tupleToSingle, { {i1}, {i2} } ];

    fs =
      Sort @
      Flatten @
      Table[
        sTupleToSingle[ f1[[1]], f2[[1]] ] -> f1[[2]] * f2[[2]],
        { F1, fs1 }, { F2, fs2 }
      ];

    If[
      rs1 === {} || rs2 === { }
      ,
      rs = {}
      ,
      Sort @
      Flatten @
      Table[
        sTupleToSingle[ r1[[1]], r2[[1]] ] -> r1[[2]] * r2[[2]],
        { R1, rs1 }, { R2, rs2 }
      ]
    ];

    FusionCategory[
      "FusionRing" -> r,
      "FSymbols"   -> fs,
      "RSymbols"   -> rs
    ]
  ];