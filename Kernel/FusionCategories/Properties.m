(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-09-14 *)

Package["Anyonica`"]

ChangeProperty[ ring_FusionCategory, list_ ] :=
  Module[ {opts},
    opts = (* All defining properties of previous fusion ring *)
    Normal @ First[ List @@ ring ];
    AddOptions[opts][FusionCategory][ Sequence @@ list ]
  ];


PackageExport["BraidedQ"]

BraidedQ::usage =
  "BraidedQ[fusionCat] returns True if the fusion category has a set of R-symbols.";

BraidedQ[ cat_FusionCategory ] :=
  !MissingQ[ RSymbols @ cat ];

FusionCategory /: UnitaryGaugeQ[ cat_FusionCategory, opts:OptionsPattern[] ] :=
  UnitaryGaugeQ[ FusionRing @ cat, FSymbols @ cat, opts ];


PackageExport["\[ScriptP]"]

Unprotect[ \[ScriptP] ];

ClearAll[ \[ScriptP] ];

SetAttributes[ \[ScriptP], NHoldAll ];

Protect[ \[ScriptP] ];

\[ScriptP]::usage =
  "Formal symbol that represents a pivotal coefficient.";


PackageExport["PivotalStructure"]

PivotalStructure::usage =
  "PivotalStructure[cat] returns the pivotal structure of the fusion category cat.";

PivotalStructure[ FusionCategory[data_]] :=
  data["PivotalStructure"];


PackageExport["AllPivotalStructures"]

AllPivotalStructures::usage =
  "AllPivotalStructures[fusionCat] returns a list of pivotal structures compatible with the " <>
  "the F-symbols of fusionCat.";


Options[AllPivotalStructures] :=
  {
    "SimplifyBy" -> Identity
  };

AllPivotalStructures[ cat_FusionCategory, opts:OptionsPattern[] ] :=
  Module[{ r, p, d, sF, eqns, rhs },
    r =
      Rank @ cat;
    d =
      CC[cat];
    p = 
      \[ScriptP];
    sF =
      SparseArray[
        MapAt[ List @@ #&, FSymbols[cat], {All,1} ],
        {r,r,r,r,r,r}
      ];

    rhs[a_,b_,c_] :=
      sF[[a,b,d[c],1,c,d[a]]] sF[[b,d[c],a,1,d[a],d[b]]] sF[[d[c],a,b,1,d[b],c]];

    eqns =
      TEL @
      OptionValue["SimplifyBy"][
        Cases[
          Tuples[ Range @ r, 3 ],
          { a_, b_, c_ } /;
          rhs[a,b,c] =!= 0 :>
					p[c] / (p[a] p[b] ) == rhs[a,b,c]
        ]/.p[1] -> 1
      ];

    Prepend[ p[1] -> 1 ] /@
    Solve[ eqns, Rest @ Array[ p, r ] ]
  ];

AllPivotalStructures[ ring_FusionRing, fSymbols_List, opts:OptionsPattern[] ] := 
  AddOptions[opts][AllPivotalStructures][ 
    FusionCategory[ "FusionRing" -> ring, "FSymbols" -> fSymbols, "SkipCheck" -> True ] 
  ];

PackageExport["\[ScriptT]"]

Unprotect[ \[ScriptT] ];

ClearAll[ \[ScriptT] ];

SetAttributes[ \[ScriptT], NHoldAll ];

Protect[ \[ScriptT] ];

\[ScriptT]::usage =
  "Formal symbol that represents a topological twist.";

PackageExport["Twists"]

Twists::usage =
  "Twists[cat] returns the topological twists of the fusion category cat.";

Twists[ FusionCategory[data_] ] :=
  data["Twists"];

PackageExport["\[ScriptD]"]

Unprotect[ \[ScriptD] ];

ClearAll[ \[ScriptD] ];

SetAttributes[ \[ScriptD], NHoldAll ];

Protect[ \[ScriptD] ];

\[ScriptD]::usage =
  "Formal symbol that represents a quantum dimension.";

PackageExport["QuantumDimensions"]

QuantumDimensions::usage =
  "QuantumDimensions[cat] returns a list of dimensions \!\(\*SuperscriptBox[\(T\), \(L\)]\)(a) of the simple objects "<>
  "of the fusion category cat";

QuantumDimensions[ cat_FusionCategory ] :=
  With[{ d = CC[cat] },
    Table[
      \[ScriptD][a] -> \[ScriptP][a] / F[a,d[a],a,a,1,1], { a, Rank @ cat }
    ]/.FSymbols[cat]/.PivotalStructure[cat]
  ];

PackageExport["RibbonQ"]

RibbonQ::usage =
  "RibbonQ[cat] returns True if cat is known to be a ribbon category.";

RibbonQ[ cat_FusionCategory ] :=
  SphericalQ[cat] && BraidedQ[cat];

PackageExport["SMatrix"]

SMatrix::usage =
  "SMatrix[cat] returns the S-matrix of the fusion category cat.";

SMatrix[ FusionCategory[data_] ] :=
  data["SMatrix"];

PackageExport["ModularQ"]

ModularQ::usage =
  "ModularQ[cat] returns True if the fusion category cat is modular.";

ModularQ[ FusionCategory[data_] ] :=
  If[ MissingQ[data["Modular"]], False, data["Modular"] ];

ModularData[ cat_FusionCategory ] :=
  If[
    ModularQ[ cat ],
    { SMatrix[cat], Twists[cat] },
    Missing["NonModularCategory"]
  ];

PackageExport["SphericalQ"]

SphericalQ::usage =
  "SphericalQ[cat] returns True if cat is a sperical fusion category.";

SphericalQ::nodims =
  "The category has no quantum dimensions so can not be checked for sphericality.";

SphericalQ[ cat_FusionCategory ] :=
  With[ { dims = QuantumDimensions[cat] },
    If[
      MissingQ[ dims ],
      Message[ SphericalQ::nodims ]; $Failed,
      And @@ Table[ RootReduce[ \[ScriptD][i] == \[ScriptD][i+1] /. dims ], { i, NSD[cat]+1, Rank[cat], 2 } ]
    ]
  ];

CheckFormalCode[c1_,c2_]:=
	With[{fc1 = FormalCode @ c1, fc2 = FormalCode @ c2 },
		If[ !MissingQ[fc1] && !MissingQ[fc2] && Most[fc1] =!= Most[fc2], Throw @ False ]
	];

CheckFusionRing[c1_,c2_] :=
	With[ { eq=EquivalentFusionRingsQ[FusionRing@c1,FusionRing@c2]},
		If[ !eq, Throw @ False ]
	];

FullInvariants[c1_]:=
	Module[ { zeroFs,invariants },
		zeroFs = Select[ FSymbols[c1], Last[#]===0& ][[;;,1]];
		(* Invariants of F and R symbols *)
		invariants =
			GaugeInvariants[
				FusionRing @ c1,
				"Zeros" -> zeroFs,
				"IncludeOnly" -> If[ !BraidedQ[c1], "FSymbols", "All" ]
			];

		(* Invariants from pivotal structure *)
		Join[
			invariants,
			Array[ \[ScriptD], Rank[c1] ]
		]
	];

PackageExport["EquivalentFusionCategoriesQ"]

Options[EquivalentFusionCategoriesQ] = { "PreEqualCheck" -> Identity };

EquivalentFusionCategoriesQ[ c1_FusionCategory, c2_FusionCategory, OptionsPattern[] ] :=
Catch[
	Module[{fra, invariants1, invariants2, qDims1, qDims2,fSymb1,fSymb2,rSymb1,rSymb2,rules1,rules2,test,eqQdims},
	CheckFormalCode[c1,c2];
	CheckFusionRing[c1,c2];

	qDims1 = QuantumDimensions[c1]; qDims2 = QuantumDimensions[c2];

  If[ PreEqualCheck[ Sort[qDims1] ] != PreEqualCheck[ Sort[ qDims2 ] ], Throw @ False ];

  fra = FRA @ FusionRing @ c1;

  eqQDims =
    Catch[
      Do[
        If[ PermuteSymbols[qDims1,a] == qDims2, Throw @ True ]
        , { a, fra }
      ]; Throw @ False
    ];

  If[ !eqQDims, Throw @ False ];

  test = OptionValue["PreEqualCheck"];
	invariants1 = FullInvariants[c1];
	invariants2 = FullInvariants[c2];


	fSymb1 = FSymbols[c1]; fSymb2 = FSymbols[c2];
	If[ BraidedQ[c1] && BraidedQ[c2],
		rSymb1 = RSymbols[c1]; rSymb2 = RSymbols[c2],
		rSymb1 = rSymb2 = {}
	];

	rules1 = Join[ fSymb1, rSymb1, qDims1 ]; rules2 = Dispatch[ Join[ fSymb2, rSymb2, qDims2 ] ];

	Do[
		If[
		ConsistentQ[ Thread[ (invariants1/.Dispatch[PermuteSymbols[rules1,a]]) == (invariants2/.rules2) ]/.{False->{False},True->{True}}, TrueQ @* test ],  True ]
		, {a,fra}
	];

	Throw @ False
	]
];

GaugeInvariants::nonbraidedcat = 
  "Gauge invariants for braided category requested but category is not braided.\n"<>
  " Use option \"IncludeOnly\" -> \"FSymbols\" for gauge invariants with only F-symbols.";

GaugeInvariants[ cat_FusionCategory, opts:OptionsPattern[] ] := 
  Module[
    { io, bq, zeros, gi },
    io = OptionValue["IncludeOnly"];

    If[ 
      io === "All" && !BraidedQ[cat], 
      Message[GaugeInvariants::nonbraidedcat]; Return @ $Failed  
    ];

    bq = BraidedQ[cat] && OptionValue["IncludeOnly"] =!= "FSymbols";

    zeros = Keys @ Select[ FSymbols @ cat,  #[[2]] === 0& ];

    gi = AddOptions[opts][GaugeInvariants][ FusionRing @ cat, "Zeros" -> zeros ];

    Thread[ gi -> ( gi/.Dispatch[ Join[ FSymbols @ cat, If[ bq, RSymbols @ cat, {} ] ] ] ) ]
  ];
