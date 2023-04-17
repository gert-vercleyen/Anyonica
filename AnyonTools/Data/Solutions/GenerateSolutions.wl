(* ::Package:: *)

(* ::Section:: *)
(*Initialization*)


Needs["FusionRings`"]
Check[
	Get[ "/home/gert/Projects/AnyonTools/Packages/PentaTools/PentaTools.wl" ],
	Get[ "/Users/gertvercleyen/Projects/AnyonTools/Packages/PentaTools/PentaTools.wl" ]
]


ClearAll[ProspectiveFusionCat];

Options[ProspectiveFusionCat] =
{
	"FSymbolValues" -> Missing[],
	"FTensor" -> Missing[],
	"HomPentEqns" -> Missing[],
	"NonTrivial0FSymbols" -> Missing[],
	"RegularFMatrices" -> Missing[],
	"RegularFSymbols" -> Missing[],
	"NonHomPentEqns" -> Missing[]
};

ProspectiveFusionCat[ ring_FusionRing?FusionRingQ, ops:OptionsPattern[] ] := Module[{
	nonTrivialFs, fSymb, fTens, gSpace, regFMat, pEqns, log, pentEqns, homEqns, sumEqns,hashoms,hassums },
	PrintTemporary[ "Prospective fusion category with "<>ToString[NNZSC[ring]]<>" non-trivial N symbols, "];

	nonTrivialFs = NonTrivialFs[ring];
	PrintTemporary[ "and ", ToString[ Length[nonTrivialFs]], " non-trivial F symbols"];

	regFMat =
	If[
		OptionValue[ "RegularFMatrices" ] =!= Missing[],
		OptionValue[ "RegularFMatrices" ],
		RegularFMatrices[ring]
	];

	fSymb = Association[
		"Values" ->
			If[
				OptionValue[ "FSymbolValues" ] =!= Missing[],
				OptionValue[ "FSymbolValues" ],
				MapThread[ Rule, {#, FixTrivialGauge[#]} ]& @ nonTrivialFs
			],
		"Regular" ->
			If[
				OptionValue[ "RegularFSymbols" ] =!= Missing[],
				OptionValue[ "RegularFSymbols" ],
				Cases[ regFMat, {{f_}} :> f ]
			],
		"NonTrivial0" ->
			OptionValue[ "NonTrivial0FSymbols" ]
	];

	fTens =
	If[
		OptionValue[ "FTensor" ] =!= Missing[],
		OptionValue[ "FTensor" ],
		SparseFTensor[ring]
	];


	{ hashoms, hassums } = (OptionValue[#] =!= Missing[])& /@ {"HomPentEqns","NonHomPentEqns"};
	pEqns =
	If[
		hashoms && hassums,
		<| "Hom" -> OptionValue["HomPentEqns"], "NonHom" -> OptionValue["NonHomPentEqns"] |>,
		PrintTemporary["Setting up pentagon equations"];
		PentagonTower[ring]
	];

	log = {
		(*
		{TitleStyle["Initialisation of the category"]},
		Report["Properties"]["Ring"][ring],
		Report["Properties"]["FSymbols"][nonTrivialFs],
		Report["Operation"]["FixingTrivialGauge"][],
		Report["Operation"]["Deleting0Columns"][regFMat]
		*)
	};

	Association[
		"FSymbols" -> fSymb,
		"FTensor" -> fTens,
		"RegularFMatrices" -> regFMat,
		"PentEqns" -> pEqns,
		"Ring" -> ring,
		"Log" -> log
	]
]


ClearAll[NonTrivialFs];
NonTrivialFs[ ring_FusionRing?FusionRingQ ] :=
NonTrivialFs[ ring ] =
Module[{
	a,b,c,d,e,f,
	r = Rank[ring],
	non0Ns = NZSC[ring],
	multTab = MT[ring],
	compatibleNs
	},
	Sort @
	Reap[
		Do[
			{a,b,e} = label1;
			compatibleNs = Cases[ non0Ns, {e,_,_} ];
			Do[
				{c,d} = label2[[{2,3}]];
				Do[
					If[
						multTab[[b,c,f]] multTab[[a,f,d]] != 0,
						Sow @ F[a,b,c,d,e,f]
					],
				{f,r}],
			{label2,compatibleNs}],
		{label1,non0Ns}]
	][[2,1]]
]


(* Set all F symbols in with a 1 as
	one of the first three indices, in expr, equal to 1  *)
ClearAll[ FixTrivialGauge ];
FixTrivialGauge = Identity;(*
FixTrivialGauge[ expr_ ] :=
	ReplaceAll[
		expr,
		( F[ a_, b_, c_, rest__ ] /; a == 1 || b == 1 || c == 1 ) -> 1
	];*)


SparseFTensor[ ring_FusionRing ] :=
With[{
	indices = List @@ # &,
	Fs = NonTrivialFs @ ring },
	SparseArray[
		Map[ indices[#] -> FixTrivialGauge[#] &, Fs ],
		Table[ Rank[ring], 6 ]
	]
]


RegularFMatrices[ ring_FusionRing ] := Module[{
	sparseF = SparseFTensor[ring],
	r = Rank[ring],
	newMat,
	mats},
	mats =
	Reap[
	Do[
		If[
			( newMat =
				RemoveZeroColumns @
				RemoveZeroRows @
				Normal[sparseF][[a,b,c,d,;;,;;]] )
			=!= {{}} && newMat =!={{1}},
			Sow[ newMat ]
			],
		{a,r},{b,r},{c,r},{d,r}]
	][[2]];
	If[
		mats == {},
		{},
		Flatten[ mats, 1 ]
	]
]

RemoveZeroRows[ mat_?MatrixQ ] :=
If[
	mat === {{}},
	{{}},
	Module[{
		newMat,
		l = Length[mat[[1]]]},
		newMat = DeleteCases[ Table[ 0, l ] ] @ mat;
		If[
			newMat === {},
			{{}},
			newMat
		]
	]
]
RemoveZeroColumns[ mat_?MatrixQ ] :=
If[
	mat === {{}},
	{{}},
	Transpose @ RemoveZeroRows @ Transpose @ mat
]


ClearAll[ PentagonTower ];
PentagonTower[ ring_ ] :=
PentagonTower[ring] =
Module[{
	a,b,c,d,e,p,q,r,s,x,
	matches, eqn, dim, patt, pentEqns,
	n = Rank[ring],
	lFInd = List @@@ NonTrivialFs[ring],
	sF = SparseFTensor[ring],
	dimF = DimF[RegularFMatrices[ring]]},
	pentEqns =
	Reap[
		(* Collect equations of the form Non0LHS \[Equal] RHS *)
		Do[ {p,c,d,e,q,r} = FInd;
			matches = Cases[ lFInd, {a_,b_,r,e,p,s_} ];
			Do[ {a,b,s} = label2[[{1,2,6}]];
				eqn =
					sF[[p,c,d,e,q,r]] sF[[a,b,r,e,p,s]] ==
					Sum[ sF[[b,c,d,s,x,r]] sF[[a,b,c,q,p,x]] sF[[a,x,d,e,q,s]], {x,n}];
				If[ !TrueQ[eqn],
					dim = Max[
						dimF /@
						GetVars[ {eqn}, F, "LevelSpec" -> 4 ]
					];
					If[ eqn[[2,0]] === Plus,
						Sow[ eqn, patt[2][dim]],
						Sow[ eqn, patt[1][dim]]
					]
				],
			{ label2, matches }],
		{ FInd, lFInd }];

		(* Collect equations of the form 0 \[Equal] Non0RHS *)
		Do[ {b,c,d,s,x,r} = List @@ label;
			matches = Cases[ lFInd, {u_,x,d,v_,w_,s}];
			Do[ {a,e,q}= label2[[{1,4,5}]];
				Do[
					If[ sF[[a,b,c,q,p,x]] != 0 && sF[[p,c,d,e,q,r]]*sF[[a,b,r,e,p,s]] == 0,
						eqn =
							0 == \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(y = 1\), \(n\)]\(sF[\([b, c, d, s, y, r]\)]*sF[\([a, b, c, q, p, y]\)]*sF[\([a, y, d, e, q, s]\)]\)\);
						If[
							!TrueQ[eqn],
							dim = Max[
								dimF /@
								GetVars[ {eqn}, F, "LevelSpec" -> 4 ]
							];
							If[
								eqn[[2,0]] === Plus,
								Sow[ eqn, patt[2][dim] ],
								Sow[ eqn, patt[1][dim] ]
							]
						]
					],
				{ p, n }],
			{ label2, matches }],
		{ label, lFInd }];,
		Flatten @ Table[ patt[i][j], {i,2}, {j,n} ]
	][[2]];
	<|
		"Hom" -> Association @@ Table[ i -> Flatten@pentEqns[[i]], {i,n} ],
		"NonHom" -> Association @@ Table[ i -> Flatten@pentEqns[[i+n]], {i,n} ]
	|>
]

DimF[ regMats_ ] := Module[ {
	matToRules},
	matToRules[ mat_ ] := With[{
		n = Length[mat]},
		Flatten @ Map[ # -> n &, mat, {2} ]
	];
	Association @@
	Flatten[
		matToRules /@ regMats
	]
]


(* ::Section:: *)
(*SolveMultiplicityFreePentagonEquations*)


ToStringJoin[ str__ ] := StringJoin @@ (ToString /@ {str});
fancyF = #/.F[a_,b_,c_,d_,e_,f_] :> Subsuperscript[ "F", ToStringJoin[d,e,f], ToStringJoin[a,b,c] ]&;
Options[SolveMultiplicityFreePentagonEquations] =
	{
		"FindZerosUsingSums" -> True,
		"GaugeDemands" -> None,
		"DeleteGaugeEquivalentSolutions" -> True,
		"ZeroValues" -> None,
		"NonSingular" -> False,
		"SimplifyIntermediateResultsBy" -> Identity
	};
SolveMultiplicityFreePentagonEquations[ ring_FusionRing?FusionRingQ, OptionsPattern[] ] := Module[ {
	monEqns, gaugeSymmetries, gtransf, FSymbols, restrictedSym, restrictedFSymbols, restrictedEqns, TrivialFPattern, monSolutions,
	sumSolutions, pentEqns, sumEqns, gb, invMats, restrictedInvMats, pentSoln, hexSoln, extraFixedFs, zeros, restrictedMonEqns,
	unionZeros, sharedVars, prospectiveCat, sharedMonomialSystem, remainingEquations, remainingSym, specificSym, specificFixedFs,
	solverInput, headerStyle = Style[ #, Bold, FontSize -> 14 ]&, g, dz
	},
	Print[
		Style["Setting up pentagon equations for ", Bold, Orange, FontSize->14 ],
		Style[ ring, Bold, Orange, FontSize->14 ]
	];
	prospectiveCat =
		ProspectiveFusionCat[ ring ];

	FSymbols =
		prospectiveCat["FSymbols"]["Values"][[;;,1]];

	monEqns  =
		prospectiveCat["PentEqns"]["Hom"] //
		Values //
		Flatten;

	sumEqns  =
		prospectiveCat["PentEqns"]["NonHom"] //
		Values //
		Flatten;

	pentEqns =
		Join[ monEqns, sumEqns ];

	invMats  =
		prospectiveCat["RegularFMatrices"];

	gtransf[ F[a_,b_,c_,d_,e_,f_] ] :=
		F[a,b,c,d,e,f] (g[a,b,e]g[e,c,d])/(g[a,f,d] g[b,c,f]);

	gaugeSymmetries =
		<| "Transforms" -> Table[ f -> gtransf[f], { f, FSymbols } ], "Symbols" -> {g} |>;

	Print[ "Attempting to solve a total of ", Length[pentEqns], " polynomial equations" ];
	Print[ "Amount of variables: ", Length[FSymbols] ];
	Print[ headerStyle[ "Setting vacuum F-symbols equal to 1" ] ];

	(* Set all F-symbols with a 1 on top equal to 1 and update matrices and gauge symmetries *)
	TrivialFPattern =
		F[ 1, __ ] | F[ _, 1, __ ] | F[ _, _, 1, __ ];

	(* Update gauges *)
	restrictedSym =
		RestrictMultiplicativeSymmetries[
			gaugeSymmetries,
			Cases[FSymbols, TrivialFPattern ],
			g
		];
	(* Remove trivial F's from list of variables *)
	restrictedFSymbols =
		DeleteCases[ FSymbols, TrivialFPattern ];

	(* Remove trivial F-matrices from list of invertible matrices *)
	restrictedInvMats =
		invMats //
		DeleteCases[ {{#}}& /@ TrivialFPattern ];

	(* Substitute the trivial Fs in the equations, remove trivial equalities and delete duplicate equations *)
	restrictedEqns =
		pentEqns/.Dispatch[ TrivialFPattern -> 1 ] //
		DeleteCases[True] //
		DeleteDuplicates;
	Print[ Length[restrictedFSymbols], " F-symbols remain" ];
	Print[ "After substitution of F's with a 1 on top there remain ", Length[restrictedEqns], " polynomial equations."];

	Print[ headerStyle @ "Determining which F-symbols could be non-trivially 0" ];
	(* Find Configurations of non-trivial 0-values *)
	zeros = If[
		OptionValue["NonSingular"],
		{{}},
		If[
		OptionValue["ZeroValues"] =!= None,
		OptionValue["ZeroValues"],
		Dispatch @
		If[ (* Want to use all equations for finding 0's (takes longer but eliminates more invalid sets up front ) *)
			OptionValue[ "FindZerosUsingSums" ],
			(* THEN *)
			Cases[ HoldPattern[ _ -> 0 ] ] /@
			FindZeroValues[
				restrictedEqns,
				restrictedFSymbols,
				"InvertibleMatrices" -> restrictedInvMats
			],
			(* ELSE: update monomial equations and only use these to find zeros *)
			restrictedMonEqns =
				monEqns/.Dispatch[ TrivialFPattern -> 1 ] //
				DeleteCases[True] //
				DeleteDuplicates;
			Cases[ HoldPattern[ _ -> 0 ] ] /@
			FindZeroValues[ restrictedMonEqns, restrictedFSymbols, "InvertibleMatrices" -> restrictedInvMats ]
		]
		]
	];
	Print[ Length[Normal[zeros]], " configuration(s) found." ];
	Print[ headerStyle @ "Fixing symmetry for F-symbols that are never 0" ];
	If[
		zeros == {},
		Return[{}]
	];
	(* Break Gauge Symmetry: first for all variables that are never 0, i.e.
		 that do not appear in any of the "zeros" from previous step *)
	(* Get all F-symbols that could be 0 for some configuration in zeros *)
	unionZeros =
    Union @@ Normal[zeros][[;;,;;,1]];

	(* Get all F-symbols that can never be 0 *)
  sharedVars =
    Complement[ restrictedFSymbols, unionZeros ];

  (* Fix the gauge for all the F symbols that can never be 0 *)
  { remainingSym, extraFixedFs } =
    BreakMultiplicativeSymmetry[
			restrictedSym,
			"GaugeDemands" -> OptionValue["GaugeDemands"],
			"Exclusions" -> unionZeros
	];

	(* Remove the newly fixed F-symbols from the list of variables *)
	restrictedFSymbols = (* EchoFunction[ "restrictedFSymbols", fancyF ] @ *)
		Complement[ restrictedFSymbols, extraFixedFs[[;;,1]] ];

	(* Substitute the values of the newly fixed F-symbols in the invertible matrices
		 and remove trivial 1D F-matrices *)
	restrictedInvMats = (* EchoFunction["restrictedInvMats", MatrixForm @* fancyF /@ #&][ *)
		restrictedInvMats/.extraFixedFs //
		DeleteCases[ {{n_?NumericQ}} /; n != 0 ](*]*); (* Dont need to remove bigger matrices since can't fix whole matrix using only gauges (PROVE!) *)
	(* Substitute the values of the newly fixed F-symbols in the equations,
		 remove trivial equations and delete duplicate equations *)
	restrictedEqns = (*EchoFunction[ "restrictedEqns", fancyF ][*)
		restrictedEqns/.extraFixedFs //
		DeleteCases[True] //
		DeleteDuplicates(*]*);

	Print[ "The value(s) of ", Length[extraFixedFs], " extra F-symbol(s) have been fixed."];
	Print[ "After fixing gauges for F-symbols that are never 0 there remain ", Length[restrictedEqns], " polynomial equations."];
	(* Try to fix extra gauges, if possible, for each of the 0-configurations. Also substitute 0 values
		and update equations, variables, and matrices *)
	Print[ headerStyle @ "Fixing remaining gauges per configuration of 0 F's" ];
	If[ (* All gauges are fixed *)
		remainingSym["Transforms"][[;;,1]] === remainingSym["Transforms"][[;;,2]],
		(* THEN *)
		Print["No gauge freedom left. Substituting zeros, and updating system."];
		solverInput = Reap[
		Do[
			dz = Dispatch[z];
			Sow[
				<|
					"eqns" -> ( restrictedEqns/.dz // DeleteCases[True] // DeleteDuplicates ),
					"vars" -> Complement[ restrictedFSymbols, z[[;;,1]] ],
					"sym" -> AddZerosToSymmetries[ remainingSym, dz ],
					"mats" -> ( restrictedInvMats/.dz ),
					"specificFs" -> {},
					"zeros" -> z
				|>
			],
			{z, Normal[zeros]}]
		][[2,1]],
		(* ELSE *)
		Print["Extra symbols can be fixed."];
		solverInput = Reap[
		Do[
			dz = Dispatch[z];
			{ specificSym, specificFixedFs } =
				BreakMultiplicativeSymmetry[
					AddZerosToSymmetries[ remainingSym, z ]
				];
			Sow[
				<|
					"eqns" -> (restrictedEqns/.dz/.specificFixedFs // DeleteCases[True] // DeleteDuplicates),
					"vars" -> Complement[ restrictedFSymbols, specificFixedFs[[;;,1]], z[[;;,1]] ],
					"sym" -> specificSym,
					"mats" -> (restrictedInvMats/.dz/.specificFixedFs // DeleteCases[ {{n_?NumericQ}} /; n != 0 ]),
					"specificFs" -> specificFixedFs,
					"zeros" -> z
				|>
			],
			{z, Normal[zeros]}]
		][[2,1]]
	];
	Print[
		"Equations will be solved for systems with the following properties:\n",
		Grid[
			Prepend[ { "#(Zeros)", "#(extra fixed F-symbols)", "#(Pentagon equations)", "#(variables)" }] @
			Table[ { Length @ input["zeros"], Length @ input["specificFs"] , Length @ input["eqns"], Length @ input["vars"] }, { input, solverInput } ],
			Frame -> All
		]
	];

	Print[ headerStyle @ "Solving the pentagon equations per configuration of 0 F's" ];
	Flatten[ #, 1 ]& @
	Table[
	pentSoln =
		If[
			OptionValue["DeleteGaugeEquivalentSolutions"],
			DeleteSymmetryEquivalentSolutions[ gaugeSymmetries, "SimplifyBy" -> ComplexExpand, "Numeric" -> True ],
			Identity
		][
		Sort[ Join[ Thread[ Cases[ FSymbols, TrivialFPattern ] -> 1 ], extraFixedFs, input["zeros"], input["specificFs"], # ] ]& /@
		SolvePolynomialSystem[
			input["eqns"],
			input["vars"],
			z,
			"Symmetries" -> input["sym"],
			"InvertibleMatrices" -> input["mats"],
			"NonSingular" -> True,
			"SimplifyIntermediateResultsBy" -> OptionValue["SimplifyIntermediateResultsBy"]
		]/.F->List
	],
	{input, solverInput}]
]


SimplifyQDs[ qds_ ] := With[{
	s = FullSimplify[ qds ]},
	Which[
		N[qds] != N[s],
			qds,
		MemberQ[ qds, d_ /; d < 1 ],
			qds,
		True,
			s
	]
]
UnitaryGaugeDemands[ring_] := With[{
	qds = SimplifyQDs @ QuantumDimensions[ring],
	dual = CC[ring]
	},
	Table[
		F[ a, dual[a], a, a, 1, 1 ] -> 1 / qds[[a]],
		{ a, Rank[ring] }
	]
]


Needs["FusionRings`"]
Get[ "~/Projects/AnyonTools/Packages/PentaTools/PentaTools.wl" ]
Block[{ codes, r, filename, timingfilename, t, sol},
	(* We exclude the PSU(2)_k for k > 5 and odd since these are somehow very hard to do *)
	codes = Complement[ FC /@ Cases[ FRL, r_/; Mult[r] === 1 && 1 < Rank[r] <= 6 ], { {4,1,0,6}, {5,1,0,10}, {6,1,0,18} } ];
	Do[
	r = FRBC[code];
	filename  = "~/Projects/AnyonTools/Data/Solutions/" <> ToString[code] <> ".mx";
	timingfilename = "~/Projects/AnyonTools/Data/Solutions/timing_" <> ToString[code] <> ".txt";
	(* Also try using different configurations *)
	Off[N::meprec];
	Off[SolveModZSpace::nonzerocoeff];
	{ t, sol } = TimeConstrained[
		Check[
			AbsoluteTiming[
				SolveMultiplicityFreePentagonEquations[ r ]
			],
			AbsoluteTiming[
				SolveMultiplicityFreePentagonEquations[ r, "SimplifyIntermediateResultsBy" -> ComplexExpand ]
			]
		],
		10000
	];
	Export[
		filename,
		sol
	];
	Export[
		timingfilename,
		t
	];
	ClearSystemCache[]
	,{ code, codes[[34;;39]] }
	]
]


Needs["FusionRings`"]
Get[ "~/Projects/AnyonTools/Packages/PentaTools/PentaTools.wl" ]
Block[{ codes, r, filename, timingfilename, t, sol},
	(* We exclude the PSU(2)_k for k > 5 and odd since these are somehow very hard to do *)
	codes = Complement[ FC /@ Cases[ FRL, r_/; Mult[r] === 1 && 1 < Rank[r] <= 6 ], { {4,1,0,6}, {5,1,0,10}, {6,1,0,18} } ];
	Do[
	r = FRBC[code];
	filename  = "~/Projects/AnyonTools/Data/Solutions/" <> ToString[code] <> ".mx";
	timingfilename = "~/Projects/AnyonTools/Data/Solutions/timing_" <> ToString[code] <> ".txt";
	(* Also try using different configurations *)
	Off[N::meprec];
	Off[SolveModZSpace::nonzerocoeff];
	{ t, sol } = TimeConstrained[
		Check[
			AbsoluteTiming[
				SolveMultiplicityFreePentagonEquations[ r ]
			],
			Check[
				AbsoluteTiming[
					SolveMultiplicityFreePentagonEquations[ r, "SimplifyIntermediateResultsBy" -> ComplexExpand ]
				],
				{ Missing["Warning Messages generated"], Missing["Warning Messages generated"] }
			]
		],
		10000
	];
	Export[
		filename,
		sol
	];
	Export[
		timingfilename,
		t
	];
	ClearSystemCache[]
	, { code, { { 6, 1, 0, 14 }, { 6, 1, 0, 16 } } }
	]
]


(* ::Section:: *)
(*Solving problematic cases*)


(* ::Text:: *)
(*Some cases returned no solutions where there should've been and others aborted due an error. Some are not solvable at all and some where skipped because they would take too much time. *)
(**)


(* ::Subsection:: *)
(*Trivial category*)


(* ::Text:: *)
(*The category for the trivial ring could not be found because the system was too trivial so we put it in manually.*)


(* Trivial ring *)
Export[
	"~/Projects/AnyonTools/Data/Solutions/{1, 1, 0, 1}.mx",
	{ { 1, 1, 1, 1, 1, 1 } -> 1 }
]


(* ::Subsection:: *)
(*Rings without categorification*)


(* Codes of rings with no categorifications *)
noCats = {
{ 5, 1, 0, 5 },
{ 5, 1, 0, 8 },
{ 5, 1, 0, 9 },
{ 5, 1, 2, 5 },
{ 6, 1, 0, 3 },
{ 6, 1, 0, 10 },
{ 6, 1, 0, 11 },
{ 6, 1, 0, 12 },
{ 6, 1, 0, 13 },
{ 6, 1, 0, 15 },
{ 6, 1, 0, 17 },
{ 6, 1, 0, 19 },
{ 6, 1, 0, 20 },
{ 6, 2, 0, 9 },
{ 6, 2, 0, 10 },
{ 6, 2, 0, 11 },
{ 6, 4, 0, 4 },
{ 6, 4, 0, 6 },
{ 6, 4, 0, 7 },
{ 6, 4, 0, 8 }
};
Do[
	Export[ "~/Projects/AnyonTools/Data/Solutions/"<>ToString[code]<>".mx",
	{}
	],
	{ code, noCats }
]


(* ::Subsection:: *)
(*Actual problematic rings*)


(* ::Text:: *)
(*The following rings were somehow not even given an attempt to categorification *)


Import[ "~/Projects/AnyonTools/Data/Solutions/"<>ToString[{ 6, 1, 0, 14 }]<>".mx"]
Import[ "~/Projects/AnyonTools/Data/Solutions/"<>ToString[{ 6, 1, 0, 16 }]<>".mx"]


(* ::Text:: *)
(*I do remember aborting these manually since finding zeros took too long. Here we go again without time constraints*)


Needs["FusionRings`"]
Get[ "~/Projects/AnyonTools/Packages/PentaTools/PentaTools.wl" ]
Block[{ codes, r, filename, timingfilename, t, sol},
	(* We exclude the PSU(2)_k for k > 5 and odd since these are somehow very hard to do *)
	codes = Complement[ FC /@ Cases[ FRL, r_/; Mult[r] === 1 && 1 < Rank[r] <= 6 ], { {4,1,0,6}, {5,1,0,10}, {6,1,0,18} } ];
	Do[
	r = FRBC[code];
	filename  = "~/Projects/AnyonTools/Data/Solutions/" <> ToString[code] <> ".mx";
	timingfilename = "~/Projects/AnyonTools/Data/Solutions/timing_" <> ToString[code] <> ".txt";
	(* Also try using different configurations *)
	Off[N::meprec];
	Off[SolveModZSpace::nonzerocoeff];
	{ t, sol } =
		Check[
			AbsoluteTiming[
				SolveMultiplicityFreePentagonEquations[ r ]
			],
			Check[
				AbsoluteTiming[
					SolveMultiplicityFreePentagonEquations[ r, "SimplifyIntermediateResultsBy" -> ComplexExpand ]
				],
				{ Missing["Warning Messages generated"], Missing["Warning Messages generated"] }
			]
		];
	Export[
		filename,
		sol
	];
	Export[
		timingfilename,
		t
	];
	ClearSystemCache[]
	, { code, { { 6, 1, 0, 14 }, { 6, 1, 0, 16 } } }
	]
]


(* ::Subsection:: *)
(*Rings of Subscript[PSU(2), k] type*)


psu2kCodes =
{
	{ 4, 1, 0, 6 },
	{ 5, 1, 0, 10 },
	{ 6, 1, 0, 18 }
};
