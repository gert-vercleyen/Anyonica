(* ::Package:: *)

(* ::Chapter:: *)
(*Multiplicity-Free Theories*)


(* ::Section:: *)
(*Initialization*)


<<AnyonTools`


(* Use stored solutions to the pentagon equations *)
StoredSolution[code_List] := 
	MapAt[
		Apply[\[ScriptCapitalF]],
		Import[ "~/Projects/AnyonTools/Data/Solutions/"<>ToString[code]<>".mx" ],
		{ All, All, 1 }
	]; 
StoredSolution[ring_FusionRing] :=
	StoredSolution[ FC[ring] ];


(* Useful for debugging *)
fancyF = \[ScriptCapitalF][a_,b_,c_,d_,e_,f_] :> Subsuperscript[ "\[ScriptCapitalF]", ToStringJoin[d,e,f], ToStringJoin[a,b,c]];
fancyR = R[a_,b_,c_] :> Subsuperscript[ "R",ToString[c], ToStringJoin[a,b]];
fancyP = P[a_,b_,c_,d_,e_] :> Subsuperscript[ "P",ToStringJoin[d,e], ToStringJoin[a,b,c]];
fancyQ = Q[a_,b_,c_,d_,e_] :> Subsuperscript[ "Q",ToStringJoin[d,e], ToStringJoin[a,b,c]];


(* ::Section:: *)
(*Solving Pentagon Equations*)


(* ::Subsection:: *)
(*Checking for unitarity*)


ClearAll[UnitaryGaugeQ];
Options[UnitaryGaugeQ] = { "SimplifyBy" -> Identity };
UnitaryGaugeQ[ ring_, FSymbols_, OptionsPattern[] ] := 
	And @@ 
	Map[
		UnitaryMatrixQ,
		OptionValue["SimplifyBy"][ FMatrices[ring]/.Dispatch[FSymbols] ]
	];


(* ::Input:: *)
(*ring = FRL[[9]];*)


(* ::Input:: *)
(*soln = SolveMultiplicityFreePentagonEquations[ring];*)


(* ::Input:: *)
(*UnitaryGaugeQ[ring,#]&/@soln*)


(* ::Input:: *)
(*FMatrices[ring]/.soln[[1]]*)


(* ::Input:: *)
(*unitaryFSol1 =*)
(*PrintLog[*)
(*	ToUnitaryGauge[ ring, soln[[2]], "Numeric"->True, "Accuracy"->16 ],*)
(*	"~/Tests/GaugeFixer/LOGS/"*)
(*];*)


(* ::Input:: *)
(*Block[*)
(*	{ sol = unFSol1[[1]]//FullSimplify, mats },*)
(*	mats =	FMatrices[ring]/.sol;*)
(*	Table[*)
(*		ConjugateTranspose[mat] . mat//N,*)
(*		{mat,mats}]*)
(*	]*)


(* ::Subsection::Closed:: *)
(*Exporting Solutions*)


ringString[ ring_FusionRing ] := 
	StringDrop[
		"FR" <> StringReplace[ ToString[ FC[ring] ], Except[DigitCharacter].. -> "_" ],
		-1
	];


(*i = 3;
ring = FRL[[i]];
(* Create directories to store the solutions and log files *)
dirName = "~/Projects/AnyonTools/Data/Solutions/" <> ringString[ring] <> "/";
logDirName = dirName <> "Logs/";
CreateDirectory[ dirName ]; CreateDirectory[ logDirName ];
(* Solve the pentagon equations and store the log files *)
soln = 
	PrintLog[
		SolveMultiplicityFreePentagonEquations[ 
			ring, 
			"DeleteGaugeEquivalentSolutions" -> False
		],
		logDirName
	];*)


(* Check validity of the solutions *)
PentagonEquations[ring]/.( Dispatch /@ soln )


(* Check unitarity of the solutions *)
UnitaryTab[ ring ] @ soln


(*  *)


Export[ dirName <> "FSymbols_" <> ringString[ring]<>".wdx", soln];


(* ::Subsection::Closed:: *)
(*Importing solutions*)


ImportSolution[ ring_FusionRing?FusionRingQ, dir_ ] := 
With[{
	soln = Import[ dir <> ringString[ ring ] <> ".wdx" ] },
	soln/.HoldPattern[ x_ -> y_ ] :> F@@x->y
];

ImportSolution[ ring_FusionRing?FusionRingQ ] := 
With[
	{ str = FileNameJoin[ { $UserBaseDirectory, "Applications", "AnyonTools", "Data", "Solutions" } ] <> "/" },
	ImportSolution[ring,str]
];

ImportSolution[ code:{ a_, b_, c_, d_ }, dir_String ] := 
	ImportSolution[ FRBC[code], dir ];

ImportSolution[ code:{ a_, b_, c_, d_ } ] := 
	ImportSolution[ FRBC[code] ];


(* ::Section::Closed:: *)
(*Hexagon Equations*)


MultFreeHexagonEquations[ ring_ ][ FSymb_List, R_ ] := Module[{
	a, b, c, d, e, f, g, Rs = R@@@NZSC[ring], rank = Rank[ring],sR },
	sR = SparseArray[ Thread[ NZSC[ring] -> Rs ], { rank, rank, rank } ];
	Reap[
		Do[ 
			{ a, c, b, d, e, g } = List @@ ff;
			Sow[ R[ c, a, e ] ff R[ c, b, g ] == Sum[ F[ c, a, b, d, e, f ] R[ c, f, d ] F[ a, b, c, d, f, g ], {f,rank} ]/.FSymb/.F[__] -> 0/.R[a_,b_,c_]:>sR[[a,b,c]] ];
			Sow[ R[ a, c, e ]^(-1) ff R[ b, c, g ]^(-1) == Sum[ F[ c, a, b, d, e, f ] R[ f, c, d ]^(-1) F[ a, b, c, d, f, g ], {f,rank} ]/.FSymb/.F[__] -> 0/.R[a_,b_,c_]:>sR[[a,b,c]] ],
		{ ff, FSymb[[;;,1]] }
		]
	][[2]]/.{{x_}} :> {x}
];

RPQSymbols[ ring_, R_, P_, Q_ ] := Module[{
	structConst = NZSC[ring],
	properTrees,
	Rs, Ps, Qs,
	a, b, c, d, e
	},
	properTrees = 
		Reap[
			Do[
				{ a, b, e } = n1;
				Sow /@ Table[ { a, b, n2[[2]], e, n2[[3]] }, { n2, Cases[ structConst, { e, _, _ } ] }],
				{ n1, structConst }]
		][[2,1]];

	{ R @@@ structConst, P @@@ properTrees, Q @@@ properTrees }
]

TrijunctionThreeParticleHexagonEquations[ ring_ ][ FSymb_List, R_Symbol, P_Symbol, Q_Symbol ] :=
	Module[{
		rank = Rank[ring],
		trivialSymbols =
			R[ 1, _, _ ] | R[ _, 1, _ ] |
			P[ 1, a_, _, a_, _ ] | P[ a_, 1, _, a_, _ ] |
			Q[ 1, a_, _, a_, _ ] | Q[ a_, 1, _, a_, _ ],
		reducePQs = 
			{ P[ a_, b_, 1, e_, d_ ] :> R[ a, b, e ], Q[ a_, b_, 1, e_, d_ ] :> R[ a, b, e ] },
		Rs, Ps, Qs,
		sR, sP, sQ,
		substituteKnowns,
		a, b, c, d, e, f, g
		},
		{ Rs, Ps, Qs } = 
			RPQSymbols[ ring, R, P, Q ];
		sR =
			SparseArray[ Thread[ (List @@@ Rs) -> Rs ], { rank, rank, rank } ];
		sP = 
			SparseArray[ Thread[ (List @@@ Ps) -> Ps ], { rank, rank, rank, rank, rank } ];
		sQ = 
			SparseArray[ Thread[ (List @@@ Qs) -> Qs ], { rank, rank, rank, rank, rank } ];
			
		substituteKnowns =
			ReplaceAll[ { R[a_,b_,c_] :> sR[[a,b,c]], P[a_,b_,c_,d_,e_] :> sP[[a,b,c,d,e]], Q[a_,b_,c_,d_,e_] :> sQ[[a,b,c,d,e]] } ] @*
			ReplaceAll[ F[__] -> 0 ] @* (* The sum on the RHS might introduce trivial 0 F-symbols which must be removed manually*)
			ReplaceAll[ reducePQs ] @* 
			ReplaceAll[ trivialSymbols -> 1 ] @*
			ReplaceAll[ FSymb ];
		DeleteCases[True] @ (
		Reap[
			Do[
				{ a, c, b, d, g, f } = List @@ ff;
				Sow @* substituteKnowns /@
				{
					P[ c, a, b, g, d ] ff R[ c, b, f ] == Sum[ F[ c, a, b, d, g, e ] R[ c, e, d ] F[ a, b, c, d, e, f ], { e, rank } ],
					Q[ a, c, b, g, d ]^(-1) ff R[ b, c, f ]^(-1) == Sum[ F[ c, a, b, d, g, e ] R[ e, c, d ]^(-1) F[ a, b, c, d, e, f ], { e, rank } ] 
				}, 
				{ ff, FSymb[[;;,1]] }
      ]
     ][[2]]/.{{x__}} :> {x}
    )
]


TrijunctionThreeParticleHexagonEquations[ ring_ ][ FSymb_List, R_Symbol ] :=
	Module[{ 
		rank, structConst, trivialSymbols, sR, substituteKnowns, matches, 
		a, b, c, d, e, f, g, x, zeroFs, zFs, nZFs },
		rank = 
			Rank[ring];
		structConst =
			NZSC[ring];
		trivialSymbols =
			R[ 1, _, _ ] | R[ _, 1, _ ];
		sR =
			SparseArray[ Thread[ structConst -> (R @@@ structConst) ], { rank, rank, rank } ];

		substituteKnowns =
			ReplaceAll[ R[a_,b_,c_] :> sR[[a,b,c]] ] @*
			ReplaceAll[ F[__] -> 0 ] @* (* The sum on the RHS might introduce trivial 0 F-symbols which must be removed manually*)
			ReplaceAll[ trivialSymbols -> 1 ] @*
			ReplaceAll[ FSymb ];
		
		zeroFs = 
			Select[ FSymb, #[[2]] === 0 & ][[;;,1]];

		Reap[
			Do[
				{ a, c, b, d, g, f } = List @@ ff;
				Which[
					a === 1 || c === 1,
						Sow @ substituteKnowns @
							{
								ff R[ c, b, f ] == Sum[ F[ c, a, b, d, g, e ] R[ c, e, d ] F[ a, b, c, d, e, f ], { e, rank } ],
								ff R[ b, c, f ]^(-1) == Sum[ F[ c, a, b, d, g, e ] R[ e, c, d ]^(-1) F[ a, b, c, d, e, f ], { e, rank } ] 
							}, 
					b === 1,
						Sow @ substituteKnowns @
							{
								R[ c, a, g ] ff R[ c, b, f ] == Sum[ F[ c, a, b, d, g, e ] R[ c, e, d ] F[ a, b, c, d, e, f ], { e, rank } ],
								R[ a, c, g ]^(-1) ff R[ b, c, f ]^(-1) == Sum[ F[ c, a, b, d, g, e ] R[ e, c, d ]^(-1) F[ a, b, c, d, e, f ], { e, rank } ] 
							}, 
					True, 
						matches =
							Cases[ FSymb[[;;,1]], F[ a, c, b, d, g, _ ] ];
						If[ (* At least one of the related F's is zero *)
							Intersection[ matches, zeroFs ] =!= {},
							(* THEN *)
							{ zFs, nZFs } 
								= GroupBy[ matches, MemberQ[zeroFs,#]&] /@ { True, False };
							Sow @* substituteKnowns @
							Table[ 
								x = FSymb[[6]];
								{
									0 == Sum[ F[ c, a, b, d, g, e ] R[ c, e, d ] F[ a, b, c, d, e, x ], { e, rank } ],
									0 == Sum[ F[ c, a, b, d, g, e ] R[ e, c, d ]^(-1) F[ a, b, c, d, e, x ], { e, rank } ]
								},
								{ FSymb, zFs }
							];
							Sow @* substituteKnowns @
							{
							Equal @@@
							Partition[
								Table[ 
									x = List @@ fSymb[[6]];
									( 1/(fSymb R[ c, b, x ]) ) Sum[ F[ c, a, b, d, g, e ] R[ c, e, d ] F[ a, b, c, d, e, x ], { e, rank } ]
									, { fSymb, nZFs }
								]
								, 2
								, 1 
							]
							, Equal @@@
							Partition[
								Table[ 
									x = fSymb[[6]];
									(R[ b, c, x ]/fSymb) Sum[ F[ c, a, b, d, g, e ] R[ e, c, d ]^(-1) F[ a, b, c, d, e, x ], { e, rank } ]
									, { fSymb, nZFs }
								]
							, 2
							, 1 
							]
							}, 
							(* ELSE *)
							Sow @* substituteKnowns @
							{
							Equal @@@
							Partition[
								Table[ 
									x = List @@ fSymb[[6]];
									( 1/(fSymb R[ c, b, x ]) ) Sum[ F[ c, a, b, d, g, e ] R[ c, e, d ] F[ a, b, c, d, e, x ], { e, rank } ]
									, { fSymb, matches }
								]
								, 2
								, 1 
							]
							, Equal @@@
							Partition[
								Table[ 
									x = fSymb[[6]];
									(R[ b, c, x ]/fSymb) Sum[ F[ c, a, b, d, g, e ] R[ e, c, d ]^(-1) F[ a, b, c, d, e, x ], { e, rank } ]
									, { fSymb, matches }
								]
								, 2
								, 1 
							]
							}
						]
					]
				,{ ff, FSymb[[;;,1]] }
      ]
    ][[2]]/.{{x__}} :> {x} //
    Flatten // 
    DeleteCases[True] //
    DeleteDuplicates
]


(* ::Section::Closed:: *)
(*Solving Multiplicity-Free Trijunction Hexagon Equations*)


(* This only works under the assumption that the solver works correctly *)
Options[ MemoSolvePentEqns ] = Options[ SolveMultiplicityFreePentagonEquations ];
MemoSolvePentEqns[ ring_FusionRing?FusionRingQ, opts:OptionsPattern[] ] := 
	MemoSolvePentEqns[ ring, opts ] = SolveMultiplicityFreePentagonEquations[ ring, opts ];

ReduceTrijunctionHexagonEquations[ ring_FusionRing?FusionRingQ, R_, P_, Q_ ] := Module[{
	pentSoln = If[ GroupQ[ring],
		MemoSolvePentEqns[ ring, "NonSingular"->True],
		MemoSolvePentEqns[ ring]
	],
	hexEqns,
	simplerEqns,
	simplerGaugeSym,
	simplerVars,
	revertVars,
	monSoln,
	gaugeSym,
	gTransf,
	RPQs,
	reducedRPQs,
	trivialRPQs,
	reducePQs
	},
	(* Initialize the variables for the system *)
	RPQs = 
		Flatten @ RPQSymbols[ ring, R, P, Q ];
		
	(* Define gauge transforms for the variables *)
	gTransf[ F[a_,b_,c_,d_,e_,f_] ] := 
		F[a,b,c,d,e,f] (g[a,b,e]g[e,c,d])/(g[a,f,d] g[b,c,f]);
	gTransf[ R[ a_, b_, c_ ] ] :=
		R[a,b,c] g[ a, b, c ] / g[ b, a, c ];
	gTransf[ P[a_, b_, c_, e_, d_ ] ] := 
		P[a, b, c, e, d ] g[ a, b, e ] / g[ b, a, e ];
	gTransf[ Q[a_, b_, c_, e_, d_ ] ] := 
		Q[a, b, c, e, d ]	g[ a, b, e ] / g[ b, a, e ];
	
	(* Define the pattern for trivial variables: Braiding with vacuum is trivial *)
	trivialRPQs =
			R[ 1, _, _ ] | R[ _, 1, _ ] |
			P[ 1, a_, _, a_, _ ] | P[ a_, 1, _, a_, _ ] |
			Q[ 1, a_, _, a_, _ ] | Q[ a_, 1, _, a_, _ ];
	
	(* Reduce the amount of variables: moving vacuum out of the way is a trivial action.  *)
	reducePQs = 
			{ P[ a_, b_, 1, e_, d_ ] :> R[ a, b, e ], Q[ a_, b_, 1, e_, d_ ] :> R[ a, b, e ] };
	reducedRPQs = 
		DeleteDuplicates[
			RPQs/.reducePQs
		];
	
	(* Determine remaining gauge symmetry given that all F-symbols are already fixed *)
	gaugeSym = 
		RestrictMultiplicativeSymmetries[ 
			<|
				"Transforms" -> 
						Table[ x -> gTransf[x], { x, Join[ pentSoln[[1,;;,1]], reducedRPQs ] } ],
				"Symbols" -> {g}
			|>,
			pentSoln[[1,;;,1]],
			g
		];
		
	(* Trivial R's P's and Q's are fixed as well *)
	gaugeSym = 
		RestrictMultiplicativeSymmetries[ 
			gaugeSym,
			Cases[ reducedRPQs, trivialRPQs ],
			g
		];
		
	Reap[
	(* For all solutions of the pentagon equations, solve the trijunction hexagon equations *)
	Do[ 
		(* Set up hexagon equations *)
		hexEqns = 
			TrijunctionThreeParticleHexagonEquations[ ring ][ sol, R, P, Q ];
		
		(* Simplify the variables to single indexed variables in "r" *)
		{ { simplerEqns, simplerGaugeSym }, simplerVars, revertVars } = EchoLabel["Renamed variables"] @ 
			SimplifyVariables[ { hexEqns, gaugeSym}, GetVars[ hexEqns, { R, P, Q } ], r ];
			
		(* Solve the binomial equations *)
		monSoln = EchoLabel["Binomial Solutions"] @
			SolveBinomialSystem[
				Select[ simplerEqns, BinomialEquationQ ],
				simplerVars, 
				z,
				"NonSingular" -> True, 
				"Symmetries" -> MapAt[ SortBy[ #, First ]&, simplerGaugeSym, {1} ]
			];
		If[
			monSoln =!= {},
			Sow[
			Table[
				<|
					"FSymbols" -> sol, 
					"System" -> 
							<|
								"RemainingEquations" -> ( simplerEqns/.monSol  // DeleteCases[True] // DeleteDuplicates),
								"BinomialSolutions" -> monSol
							|>
				|>,
				{ monSol, monSoln }
			]/.revertVars  
			]
		]
	,{ sol, pentSoln }]][[2,1]]
]


SolveTrijunctionHexagonEquations[ ring_FusionRing?FusionRingQ, R_, z_ ] := 
Module[{
	pentSoln, hexEqns, simplerEqns, simplerGaugeSym, simplerVars, revertVars, monSoln, gaugeSym,
	gTransf, Rs, trivialRs
	},
	
	(* If ring number is known: import solutions to the pentagon equations *)
	pentSoln =
		If[
			MissingQ @ FC[ring],
			SolveMultiplicityFreePentagonEquations[ ring ],
			ImportSolution @ FC @ ring
		];
	
	(* Initialize the variables for the system *)
	Rs = 
		R @@@ NZSC[ ring ];
		
	(* Define gauge transforms for the variables *)
	gTransf[ F[a_,b_,c_,d_,e_,f_] ] := 
		F[a,b,c,d,e,f] (g[a,b,e]g[e,c,d]) / (g[a,f,d] g[b,c,f]);
	gTransf[ R[ a_, b_, c_ ] ] :=
		R[a,b,c] g[ a, b, c ] / g[ b, a, c ];
	
	(* Define the pattern for trivial variables: Braiding with vacuum is trivial *)
	trivialRs =
			R[ 1, _, _ ] | R[ _, 1, _ ];
			
	(* Determine remaining gauge symmetry given that all F-symbols are already fixed *)
	gaugeSym = 
		RestrictMultiplicativeSymmetries[ 
			<|
				"Transforms" -> 
						Table[ x -> gTransf[x], { x, Join[ pentSoln[[1,;;,1]], Rs ] } ],
				"Symbols" -> {g}
			|>,
			pentSoln[[1,;;,1]],
			g
		];
		
	(* Trivial R's are fixed as well *)
	gaugeSym = 
		RestrictMultiplicativeSymmetries[ 
			gaugeSym,
			Rs,
			g
		];
	
	Map[ 
	Sort[ Join[ Thread[ Cases[ Rs, trivialRs ] -> 1 ], # ] ]&,
	Reap[
	(* For all solutions of the pentagon equations, solve the trijunction hexagon equations *)
	Do[ 
		(* Set up hexagon equations *)
		hexEqns =
			TrijunctionThreeParticleHexagonEquations[ ring ][ sol, R ];
			
		(* Solve the equations *)
		Sow[
			SolvePolynomialSystem[ 
				hexEqns, 
				Cases[ Rs, R[a_,b_,_]/; a =!= 1 && b =!= 1], 
				z,
				"NonSingular" -> True, 
				"Symmetries" -> MapAt[ SortBy[ #, First ]&, gaugeSym, {1} ]
			]
		]
	,{ sol, pentSoln }
	]][[2,1]],
	{2}
	]
]


(* ::Subsection:: *)
(*Testing*)


FRL[[9]]


Block[ 
	{ ring = FRL[[5]] },
	TrijunctionThreeParticleHexagonEquations[ ring ][ #, R ]& /@ ImportSolution[ ring ]
]


Block[ 
	{ ring = FRBC[{5,1,0,3}] },
	TrijunctionThreeParticleHexagonEquations[ ring ][ #, R ]& /@ ImportSolution[ ring ]
]


sol = Block[ { ring = FRL[[5]] }, 
	Print[ Names[ring][[1]] ];
	SolveTrijunctionHexagonEquations[ ring, R, z ]
]


FRL[[9]] // SMT // TableForm


sol = Block[ { ring = FRBC[{5,1,0,3}] }, 
	PrintLog[
	SolveTrijunctionHexagonEquations[ ring, R, z ]
	,"~/Tests/Logs/SU2_4/"
	]
]


Export[ "/Users/gertvercleyen/Projects/papers/Graph_Networks/MathematicaWork/Sol_3p_Tri_SU2_4.wdx", sol ];


Map[
	GetVars[ #, z ]&, 
	sol,
	{2}
]


sol = %;


Grid[Transpose@{TableForm[ #, TableDepth->2, TableDirections->Row]&/@sol}, Frame->All]


soln =
PrintLog[
	SolveTrijunctionHexagonEquations[ FRL[[9]], R, P, Q ],
	"~/Tests/Logs/SU2_3/"
] 


soln // FullSimplify


soltyz4[[1,1]]["System"]


SolveMultiplicityFreePentagonEquations[ FusionRingTY[ CyclicGroup[3] ] ]


ReduceTrijunctionHexagonEquations[ FusionRingTY[ CyclicGroup[3] ], R, P, Q ] // QuietEcho


soltyz3 = %;


Reduce[ 
	Join[ 
		soltyz3[[4,1]]["System"]["RemainingEquations"],
		Thread[ z/@Range[17] == 0 ]
	],
	z/@Range[17]
]


With[{vars = z /@ Range[17]},
Reduce[ soltyz3[[1,1]]["System"]["RemainingEquations"] ~ Join ~ Thread[vars !=0], vars ]
] 


fsym["Ising"][1] = Cases[ HoldPattern[ F[a_,b_,c_,__] -> _ ] /; a != 1 && b != 1 && c != 1 ] @ {F[1,1,1,1,1,1]->1,F[1,1,2,2,1,2]->1,F[1,1,3,3,1,3]->1,F[1,2,1,2,2,2]->1,F[1,2,2,1,2,1]->1,F[1,2,3,3,2,3]->1,F[1,3,1,3,3,3]->1,F[1,3,2,3,3,3]->1,F[1,3,3,1,3,1]->1,F[1,3,3,2,3,2]->1,F[2,1,1,2,2,1]->1,F[2,1,2,1,2,2]->1,F[2,1,3,3,2,3]->1,F[2,2,1,1,1,2]->1,F[2,2,2,2,1,1]->1,F[2,2,3,3,1,3]->1,F[2,3,1,3,3,3]->1,F[2,3,2,3,3,3]->-1,F[2,3,3,1,3,2]->1,F[2,3,3,2,3,1]->1,F[3,1,1,3,3,1]->1,F[3,1,2,3,3,2]->1,F[3,1,3,1,3,3]->1,F[3,1,3,2,3,3]->1,F[3,2,1,3,3,2]->1,F[3,2,2,3,3,1]->1,F[3,2,3,1,3,3]->-1,F[3,2,3,2,3,3]->1,F[3,3,1,1,1,3]->1,F[3,3,1,2,2,3]->1,F[3,3,2,1,2,3]->-1,F[3,3,2,2,1,3]->-1,F[3,3,3,3,1,1]->1/Sqrt[2],F[3,3,3,3,1,2]->-(1/Sqrt[2]),F[3,3,3,3,2,1]->1/Sqrt[2],F[3,3,3,3,2,2]->1/Sqrt[2]};
fsym["Ising"][2] = Cases[ HoldPattern[ F[a_,b_,c_,__] -> _ ] /; a != 1 && b != 1 && c != 1 ] @ Simplify[{F[1,1,1,1,1,1]->1,F[1,1,2,2,1,2]->1,F[1,1,3,3,1,3]->1,F[1,2,1,2,2,2]->1,F[1,2,2,1,2,1]->1,F[1,2,3,3,2,3]->1,F[1,3,1,3,3,3]->1,F[1,3,2,3,3,3]->1,F[1,3,3,1,3,1]->1,F[1,3,3,2,3,2]->1,F[2,1,1,2,2,1]->1,F[2,1,2,1,2,2]->1,F[2,1,3,3,2,3]->1,F[2,2,1,1,1,2]->1,F[2,2,2,2,1,1]->1,F[2,2,3,3,1,3]->1,F[2,3,1,3,3,3]->1,F[2,3,2,3,3,3]->-1,F[2,3,3,1,3,2]->1,F[2,3,3,2,3,1]->1,F[3,1,1,3,3,1]->1,F[3,1,2,3,3,2]->1,F[3,1,3,1,3,3]->1,F[3,1,3,2,3,3]->1,F[3,2,1,3,3,2]->1,F[3,2,2,3,3,1]->1,F[3,2,3,1,3,3]->-1,F[3,2,3,2,3,3]->1,F[3,3,1,1,1,3]->1,F[3,3,1,2,2,3]->1,F[3,3,2,1,2,3]->-1,F[3,3,2,2,1,3]->-1,F[3,3,3,3,1,1]->E^(2 I \[Pi] (1/2+(I Log[2])/(4 \[Pi]))),F[3,3,3,3,1,2]->-E^(2 I \[Pi] (1/2+(I Log[2])/(4 \[Pi]))),F[3,3,3,3,2,1]->E^(2 I \[Pi] (1/2+(I Log[2])/(4 \[Pi]))),F[3,3,3,3,2,2]->E^(2 I \[Pi] (1/2+(I Log[2])/(4 \[Pi])))}];


toTeX[ expr_ ] := expr // 
TeXForm // 
ToString // 
StringReplace[ "F("~~a_~~","~~b_~~","~~c_~~","~~d_~~","~~e_~~","~~f_~~")" :> "\\Fterm{"<>a<>"}{"<>b<>"}{"<>c<>"}{"<>d<>"}{"<>e<>"}{"<>f<>"}" ] //
StringReplace[ "R("~~a_~~","~~b_~~","~~c_~~")" :> "\\Rterm{"<>a<>"}{"<>b<>"}{"<>c<>"}" ] // 
StringReplace[ "P("~~a_~~","~~b_~~","~~c_~~","~~d_~~","~~e_~~")" :> "\\Pterm{"<>a<>"}{"<>b<>"}{"<>c<>"}{"<>d<>"}{"<>e<>"}" ] //
StringReplace[ "Q("~~a_~~","~~b_~~","~~c_~~","~~d_~~","~~e_~~")" :> "\\Qterm{"<>a<>"}{"<>b<>"}{"<>c<>"}{"<>d<>"}{"<>e<>"}" ] //
StringReplace[ "=" -> " = " ];


eqns["Ising"][1] = {P[2,2,2,1,2] R[2,2,1]==1,1/(Q[2,2,2,1,2] R[2,2,1])==1,P[2,2,3,1,3] R[2,3,3]==-R[2,3,3],1/(Q[2,2,3,1,3] R[3,2,3])==-(1/R[3,2,3]),-P[3,2,2,3,3] R[3,2,3]==1,-(1/(Q[2,3,2,3,3] R[2,3,3]))==1,P[3,2,3,3,1] R[3,3,2]==-R[3,3,1],1/(Q[2,3,3,3,1] R[3,3,2])==-(1/R[3,3,1]),P[3,2,3,3,2] R[3,3,1]==R[3,3,2],1/(Q[2,3,3,3,2] R[3,3,1])==1/R[3,3,2],P[2,3,2,3,3] R[2,2,1]==-R[2,3,3],1/(Q[3,2,2,3,3] R[2,2,1])==-(1/R[3,2,3]),-P[2,3,3,3,1] R[2,3,3]==-R[2,2,1],-(1/(Q[3,2,3,3,1] R[3,2,3]))==-(1/R[2,2,1]),P[2,3,3,3,2] R[2,3,3]==-1,1/(Q[3,2,3,3,2] R[3,2,3])==-1,-P[3,3,2,2,1] R[3,2,3]==R[3,3,1],-(1/(Q[3,3,2,2,1] R[2,3,3]))==1/R[3,3,1],-P[3,3,2,1,2] R[3,2,3]==-R[3,3,2],-(1/(Q[3,3,2,1,2] R[2,3,3]))==-(1/R[3,3,2]),(P[3,3,3,1,3] R[3,3,1])/Sqrt[2]==1/2-1/2 R[3,2,3],1/(Sqrt[2] Q[3,3,3,1,3] R[3,3,1])==1/2-1/(2 R[2,3,3]),-((P[3,3,3,1,3] R[3,3,2])/Sqrt[2])==-(1/2)-1/2 R[3,2,3],-(1/(Sqrt[2] Q[3,3,3,1,3] R[3,3,2]))==-(1/2)-1/(2 R[2,3,3]),(P[3,3,3,2,3] R[3,3,1])/Sqrt[2]==1/2+1/2 R[3,2,3],1/(Sqrt[2] Q[3,3,3,2,3] R[3,3,1])==1/2+1/(2 R[2,3,3]),(P[3,3,3,2,3] R[3,3,2])/Sqrt[2]==-(1/2)+1/2 R[3,2,3],1/(Sqrt[2] Q[3,3,3,2,3] R[3,3,2])==-(1/2)+1/(2 R[2,3,3])};
eqns["Ising"][2] = ComplexExpand @ {P[2,2,2,1,2] R[2,2,1]==1,1/(Q[2,2,2,1,2] R[2,2,1])==1,P[2,2,3,1,3] R[2,3,3]==-R[2,3,3],1/(Q[2,2,3,1,3] R[3,2,3])==-(1/R[3,2,3]),-P[3,2,2,3,3] R[3,2,3]==1,-(1/(Q[2,3,2,3,3] R[2,3,3]))==1,P[3,2,3,3,1] R[3,3,2]==-R[3,3,1],1/(Q[2,3,3,3,1] R[3,3,2])==-(1/R[3,3,1]),P[3,2,3,3,2] R[3,3,1]==R[3,3,2],1/(Q[2,3,3,3,2] R[3,3,1])==1/R[3,3,2],P[2,3,2,3,3] R[2,2,1]==-R[2,3,3],1/(Q[3,2,2,3,3] R[2,2,1])==-(1/R[3,2,3]),-P[2,3,3,3,1] R[2,3,3]==-R[2,2,1],-(1/(Q[3,2,3,3,1] R[3,2,3]))==-(1/R[2,2,1]),P[2,3,3,3,2] R[2,3,3]==-1,1/(Q[3,2,3,3,2] R[3,2,3])==-1,-P[3,3,2,2,1] R[3,2,3]==R[3,3,1],-(1/(Q[3,3,2,2,1] R[2,3,3]))==1/R[3,3,1],-P[3,3,2,1,2] R[3,2,3]==-R[3,3,2],-(1/(Q[3,3,2,1,2] R[2,3,3]))==-(1/R[3,3,2]),E^(2 I \[Pi] (1/2+(I Log[2])/(4 \[Pi]))) P[3,3,3,1,3] R[3,3,1]==E^(4 I \[Pi] (1/2+(I Log[2])/(4 \[Pi])))-E^(4 I \[Pi] (1/2+(I Log[2])/(4 \[Pi]))) R[3,2,3],E^(2 I \[Pi] (1/2+(I Log[2])/(4 \[Pi])))/(Q[3,3,3,1,3] R[3,3,1])==E^(4 I \[Pi] (1/2+(I Log[2])/(4 \[Pi])))-E^(4 I \[Pi] (1/2+(I Log[2])/(4 \[Pi])))/R[2,3,3],-E^(2 I \[Pi] (1/2+(I Log[2])/(4 \[Pi]))) P[3,3,3,1,3] R[3,3,2]==-E^(4 I \[Pi] (1/2+(I Log[2])/(4 \[Pi])))-E^(4 I \[Pi] (1/2+(I Log[2])/(4 \[Pi]))) R[3,2,3],-(E^(2 I \[Pi] (1/2+(I Log[2])/(4 \[Pi])))/(Q[3,3,3,1,3] R[3,3,2]))==-E^(4 I \[Pi] (1/2+(I Log[2])/(4 \[Pi])))-E^(4 I \[Pi] (1/2+(I Log[2])/(4 \[Pi])))/R[2,3,3],E^(2 I \[Pi] (1/2+(I Log[2])/(4 \[Pi]))) P[3,3,3,2,3] R[3,3,1]==E^(4 I \[Pi] (1/2+(I Log[2])/(4 \[Pi])))+E^(4 I \[Pi] (1/2+(I Log[2])/(4 \[Pi]))) R[3,2,3],E^(2 I \[Pi] (1/2+(I Log[2])/(4 \[Pi])))/(Q[3,3,3,2,3] R[3,3,1])==E^(4 I \[Pi] (1/2+(I Log[2])/(4 \[Pi])))+E^(4 I \[Pi] (1/2+(I Log[2])/(4 \[Pi])))/R[2,3,3],E^(2 I \[Pi] (1/2+(I Log[2])/(4 \[Pi]))) P[3,3,3,2,3] R[3,3,2]==-E^(4 I \[Pi] (1/2+(I Log[2])/(4 \[Pi])))+E^(4 I \[Pi] (1/2+(I Log[2])/(4 \[Pi]))) R[3,2,3],E^(2 I \[Pi] (1/2+(I Log[2])/(4 \[Pi])))/(Q[3,3,3,2,3] R[3,3,2])==-E^(4 I \[Pi] (1/2+(I Log[2])/(4 \[Pi])))+E^(4 I \[Pi] (1/2+(I Log[2])/(4 \[Pi])))/R[2,3,3]};


TableForm[ { eqns["Ising"][1], eqns["Ising"][2] } // Transpose ];
eqns["Ising"][1][[;;-9]] //toTeX


monsoln["Ising"] = {R[2,2,1]->-(z[4]/z[5]),R[2,3,3]->-(1/z[3]),R[3,2,3]->-(1/z[5]),R[3,3,1]->z[7]/z[3],R[3,3,2]->-(z[6]/z[3]),P[2,2,2,1,2]->-(z[5]/z[4]),P[2,2,3,1,3]->-1,P[2,3,2,3,3]->-(z[5]/(z[3] z[4])),P[2,3,3,3,1]->(z[3] z[4])/z[5],P[2,3,3,3,2]->z[3],P[3,2,2,3,3]->z[5],P[3,2,3,3,1]->z[7]/z[6],P[3,2,3,3,2]->-(z[6]/z[7]),P[3,3,2,1,2]->(z[5] z[6])/z[3],P[3,3,2,2,1]->(z[5] z[7])/z[3],P[3,3,3,1,3]->z[1],P[3,3,3,2,3]->z[2],Q[2,2,2,1,2]->-(z[5]/z[4]),Q[2,2,3,1,3]->-1,Q[2,3,2,3,3]->z[3],Q[2,3,3,3,1]->z[7]/z[6],Q[2,3,3,3,2]->-(z[6]/z[7]),Q[3,2,2,3,3]->-(1/z[4]),Q[3,2,3,3,1]->z[4],Q[3,2,3,3,2]->z[5],Q[3,3,2,1,2]->z[6],Q[3,3,2,2,1]->z[7],Q[3,3,3,1,3]->z[8],Q[3,3,3,2,3]->z[9]};
Equal @@@ monsoln["Ising"] // toTeX // StringReplace[ "z("~~n_~~")" :> "z_"<>n]


Equal @@ GroupBy[ fsym["Ising"][1], #[[2]]& ][1][[;;,1]] // toTeX


Equal @@ GroupBy[ fsym["Ising"][2], #[[2]]& ][-1][[;;,1]]// toTeX


conclusionsZn[n_] := ReduceTrijunctionHexagonEquations[ FusionRingZn[n], R, P, Q ];


monSolutions = Table[ 
	Join[ 
		t[[i,j,1]]["FSymbols"],
		t[[i,j,1]]["System"]["BinomialSolutions"]
	],	
	{ i, 7 },
	{j,i+1}
];


PEqualsQQ[ sol_ ] := Module[{
	Ps, Qs},
	Ps = SortBy[ Cases[ sol, HoldPattern[ P[__] -> _ ] ], First ];
	Qs = SortBy[ Cases[ sol, HoldPattern[ Q[__] -> _ ] ], First ];
	Thread[Ps[[;;,2]] == Qs[[;;,2]]]
]


Table[ PEqualsQQ[monSolutions[[i,j]]], { i, 4 }, {j,i+1} ] // FullSimplify


Solve[ PEqualsQQ[monSolutions[[3,1]]], z /@ Range[ 3^2] ]


nIndices =
Block[{$MaxExtraPrecision = 10000},
Table[ 
	If[ 
		TrueQ[And@@FullSimplify[t[[i,j]]["System"]["RemainingEquations"]]],
		Max[ Identity @@@ GetVars[ t[[i,j]]["System"]["BinomialSolutions"], z ] ],
		""
	],
	{i,7},
	{j,i+1}
]	
]


Block[ { s = t[[4,5]]},
	Join[ 
		s["FSymbols"],
		s["System"]["BinomialSolutions"]
	]/.fancyF/.fancyR/.fancyP/.fancyQ/.z[i_]:>Subscript[z, i]
]


ReduceTrijunctionHexagonEquations[ FRL[[15]], R, P, Q ]


solty = %;


solty[[1,1]]["System"]["RemainingEquations"]//Simplify


(* ::Section:: *)
(*Testing*)


(* ::Subsection::Closed:: *)
(*Finding 0 values*)


(* ::Input:: *)
(*(* Potts: FRBC[{4,1,2,2}] *)*)


Get[ "/home/gert/Projects/AnyonTools/Packages/PentaTools/PentaTools.wl" ]
ToStringJoin[ str__ ] := StringJoin @@ (ToString /@ {str});
Module[ {
	c = ProspectiveFusionCat[ FRBC[{5,1,0,6}] ],
	monEqns, gaugeSymmetries, gtransf, FSymbols, restrictedSym, restrictedFSymbols, restrictedEqns, TrivialFPattern, monSolutions,
	sumSolutions, pentEqns, sumEqns, gb, invMats, restrictedInvMats, pentSoln, hexSoln,extraFixedFs, 
	fancyF = F[a_,b_,c_,d_,e_,f_] :> Subsuperscript[ "F", ToStringJoin[d,e,f], ToStringJoin[a,b,c]],
	fancyR = R[a_,b_,c_] :> Subsuperscript[ "R",ToString[c], ToStringJoin[a,b]],
	fancyP = P[a_,b_,c_,d_,e_] :> Subsuperscript[ "P",ToStringJoin[d,e], ToStringJoin[a,b,c]],
	fancyQ = Q[a_,b_,c_,d_,e_] :> Subsuperscript[ "Q",ToStringJoin[d,e], ToStringJoin[a,b,c]],
	zeros
	},
	Print[c["Ring"]];
	FSymbols = c["FSymbols"]["Values"][[;;,1]];
	monEqns  = c["PentEqns"]["Hom"] // Values // Flatten; sumEqns  = c["PentEqns"]["NonHom"] // Values // Flatten; pentEqns = Join[ monEqns, sumEqns ];
	invMats  = c["RegularFMatrices"];


	zeros = Cases[ HoldPattern[x_ -> 0] ]/@
		FindZeroValues[ pentEqns, FSymbols, "InvertibleMatrices" -> invMats ](*;
	monEqns/.(Cases[HoldPattern[_\[Rule]0]] @ zeros[[1]])//DeleteCases[True]*)
]


Get[ "/home/gert/Projects/AnyonTools/Packages/PentaTools/PentaTools.wl" ]
ToStringJoin[ str__ ] := StringJoin @@ (ToString /@ {str});
Module[ {
	c ,
	monEqns, gaugeSymmetries, gtransf, FSymbols, restrictedSym, restrictedFSymbols, restrictedEqns, TrivialFPattern, monSolutions,
	sumSolutions, pentEqns, sumEqns, gb, invMats, restrictedInvMats, pentSoln, hexSoln,extraFixedFs, nonZeroVars, monomialLists, t, zeros,
	code,
	fancyF = F[a_,b_,c_,d_,e_,f_] :> Subsuperscript[ "F", ToStringJoin[d,e,f], ToStringJoin[a,b,c]],
	fancyR = R[a_,b_,c_] :> Subsuperscript[ "R",ToString[c], ToStringJoin[a,b]],
	fancyP = P[a_,b_,c_,d_,e_] :> Subsuperscript[ "P",ToStringJoin[d,e], ToStringJoin[a,b,c]],
	fancyQ = Q[a_,b_,c_,d_,e_] :> Subsuperscript[ "Q",ToStringJoin[d,e], ToStringJoin[a,b,c]]
	},
	
	Do[
	c = ProspectiveFusionCat[FRL[[i]] ];
	Print[c["Ring"]];
	code = FC[c["Ring"]];
	FSymb = c["FSymb"]["Values"][[;;,1]];
	monEqns  = c["PentEqns"]["Hom"] // Values // Flatten; sumEqns  = c["PentEqns"]["NonHom"] // Values // Flatten; pentEqns = Join[ monEqns, sumEqns ];
	invMats  = c["RegularFMatrices"];
	
	{ t, zeros } = 
	AbsoluteTiming[ 
		Cases[ HoldPattern[x_ -> 0] ]/@ 
		FindZeroValues[ pentEqns, FSymb, "InvertibleMatrices" -> invMats ] 
	];
	Export[ "/home/gert/Documents/Wolfram Mathematica/TestTimingsCorrectnessZeroFinder/New/"<>ToString[code]<>".wdx", { code, t, zeros } ];
	Print[ { code, t } ];
	,{i,40}];
]


Block[{r,s}, 
	r = Developer`ToPackedArray[ Range[1000] ];
	s = Position[r, x_?PrimeQ ];
	Delete[r,s] 
]


(* ::Subsection::Closed:: *)
(*Solving monomial equations, no simplifications*)


Get[ "/home/gert/Projects/AnyonTools/Packages/PentaTools/PentaTools.wl" ]
ToStringJoin[ str__ ] := StringJoin @@ (ToString /@ {str});
Module[ {
	c = ProspectiveFusionCat[ FRL[[5]] ],
	monEqns, gaugeSymmetries, gtransf, FSymbols, restrictedSym, restrictedFSymbols, restrictedEqns, TrivialFPattern, monSolutions,
	sumSolutions, pentEqns, sumEqns, gb, invMats, restrictedInvMats, pentSoln, hexSoln,extraFixedFs, 
	fancyF = F[a_,b_,c_,d_,e_,f_] :> Subsuperscript[ "F", ToStringJoin[d,e,f], ToStringJoin[a,b,c]],
	fancyR = R[a_,b_,c_] :> Subsuperscript[ "R",ToString[c], ToStringJoin[a,b]],
	fancyP = P[a_,b_,c_,d_,e_] :> Subsuperscript[ "P",ToStringJoin[d,e], ToStringJoin[a,b,c]],
	fancyQ = Q[a_,b_,c_,d_,e_] :> Subsuperscript[ "Q",ToStringJoin[d,e], ToStringJoin[a,b,c]]
	},
	Print[c["Ring"]];
	FSymbols = c["FSymbols"]["Values"][[;;,1]];
	monEqns  = c["PentEqns"]["Hom"] // Values // Flatten; sumEqns  = c["PentEqns"]["NonHom"] // Values // Flatten; pentEqns = Join[ monEqns, sumEqns ];
	invMats  = c["RegularFMatrices"];
	gtransf[ F[a_,b_,c_,d_,e_,f_] ] := F[a,b,c,d,e,f] (u[a,b,e]u[e,c,d])/(u[a,f,d] u[b,c,f]);
	gaugeSymmetries = <| "Transforms" -> Table[ f -> gtransf[f], { f, FSymbols } ], "Symbols" -> {u} |>;

	Export[
		"~/BinomialSolutionsRepS4.mx",
		SolveBinomialSystem[ monEqns, FSymbols, z, "InvertibleMatrices" -> invMats, "Symmetries" -> gaugeSymmetries ]
	]

	(* monEqns/.zeroVals[[11,2]] *)
	
	(*
	gtransf[ F[a_,b_,c_,d_,e_,f_] ] := F[a,b,c,d,e,f] (u[a,b,e]u[e,c,d])/(u[a,f,d] u[b,c,f]);
	gaugeSymmetries = <| "Transforms" -> Table[ f -> gtransf[f], { f, FSymbols } ], "Symbols" -> {u} |>;
	(*Print[ TableForm[ gaugeSymmetries["Transforms"] ]/.fancyF ];	*)

	pentSoln = 
		(*DeleteSymmetryEquivalentSolutions[ gaugeSymmetries, "SimplifyBy" \[Rule] ComplexExpand, "Numeric" \[Rule] True ] @*)
		SolvePolynomialSystem[ pentEqns, FSymbols, z, "Symmetries" -> gaugeSymmetries, "InvertibleMatrices" -> invMats ];
	Print[pentSoln//TableForm];
	*)
]


(* ::Subsection::Closed:: *)
(*Solving pentagon equations, no simplifications*)


Get[ "/home/gert/Projects/AnyonTools/Packages/PentaTools/PentaTools.wl" ]
ToStringJoin[ str__ ] := StringJoin @@ (ToString /@ {str});
Module[ {
	c = ProspectiveFusionCat[ FRBC[{5,1,0,6}] ],
	monEqns, gaugeSymmetries, gtransf, FSymbols, restrictedSym, restrictedFSymbols, restrictedEqns, TrivialFPattern, monSolutions,
	sumSolutions, pentEqns, sumEqns, gb, invMats, restrictedInvMats, pentSoln, hexSoln,extraFixedFs, 
	fancyF = F[a_,b_,c_,d_,e_,f_] :> Subsuperscript[ "F", ToStringJoin[d,e,f], ToStringJoin[a,b,c]],
	fancyR = R[a_,b_,c_] :> Subsuperscript[ "R",ToString[c], ToStringJoin[a,b]],
	fancyP = P[a_,b_,c_,d_,e_] :> Subsuperscript[ "P", ToStringJoin[d,e], ToStringJoin[a,b,c]],
	fancyQ = Q[a_,b_,c_,d_,e_] :> Subsuperscript[ "Q", ToStringJoin[d,e], ToStringJoin[a,b,c]]
	},
	Print[c["Ring"]];
	FSymbols = c["F"NonHom " -> OptionValue["NonHomPentEqns "]Symbols"]["Values"][[;;,1]];
	monEqns  = c["PentEqns"]["Hom"] // Values // Flatten; sumEqns  = c["PentEqns"]["NonHom"] // Values // Flatten; pentEqns = Join[ monEqns, sumEqns ];
	invMats  = c["RegularFMatrices"];
	gtransf[ F[a_,b_,c_,d_,e_,f_] ] := F[a,b,c,d,e,f] (u[a,b,e]u[e,c,d])/(u[a,f,d] u[b,c,f]);
	gaugeSymmetries = <| "Transforms" -> Table[ f -> gtransf[f], { f, FSymbols } ], "Symbols" -> {u} |>;
	(*Print[ TableForm[ gaugeSymmetries["Transforms"] ]/.fancyF ];	*)

	pentSoln = 
		(*DeleteSymmetryEquivalentSolutions[ gaugeSymmetries, "SimplifyBy" \[Rule] ComplexExpand, "Numeric" \[Rule] True ] @*)
		SolvePolynomialSystem[ pentEqns, FSymbols, z, "Symmetries" -> gaugeSymmetries, "InvertibleMatrices" -> invMats ];
	Print[pentSoln//TableForm];
	
]


(* ::Subsection::Closed:: *)
(*Solving pentagon equations, trivial symbols fixed*)


Get[ "/home/gert/Projects/AnyonTools/Packages/PentaTools/PentaTools.wl" ]
ToStringJoin[ str__ ] := StringJoin @@ (ToString /@ {str});
soln = Module[ {
	c = Echo @ ProspectiveFusionCat[ FRBC[{5,1,0,6}] ],
	monEqns, gaugeSymmetries, gtransf, FSymbols, restrictedSym, restrictedFSymbols, restrictedEqns, TrivialFPattern, monSolutions,
	sumSolutions, pentEqns, sumEqns, gb, invMats, restrictedInvMats, pentSoln, hexSoln,extraFixedFs, 
	fancyF = F[a_,b_,c_,d_,e_,f_] :> Subsuperscript[ "F", ToStringJoin[d,e,f], ToStringJoin[a,b,c]],
	fancyR = R[a_,b_,c_] :> Subsuperscript[ "R",ToString[c], ToStringJoin[a,b]],
	fancyP = P[a_,b_,c_,d_,e_] :> Subsuperscript[ "P",ToStringJoin[d,e], ToStringJoin[a,b,c]],
	fancyQ = Q[a_,b_,c_,d_,e_] :> Subsuperscript[ "Q",ToStringJoin[d,e], ToStringJoin[a,b,c]]
	},
	Print[c["Ring"]];
	FSymbols = c["FSymbols"]["Values"][[;;,1]];
	monEqns  = c["PentEqns"]["Hom"] // Values // Flatten; sumEqns  = c["PentEqns"]["NonHom"] // Values // Flatten; pentEqns = Join[ monEqns, sumEqns ];
	invMats  = c["RegularFMatrices"];
		
	gtransf[ F[a_,b_,c_,d_,e_,f_] ] := F[a,b,c,d,e,f] (u[a,b,e]u[e,c,d])/(u[a,f,d] u[b,c,f]);
	gaugeSymmetries = <| "Transforms" -> Table[ f -> gtransf[f], { f, FSymbols } ], "Symbols" -> {u} |>;

	TrivialFPattern = 
		F[ 1, __ ] | F[ _, 1, __ ] | F[ _, _, 1, __ ];
	restrictedSym = 
		RestrictMultiplicativeSymmetries[ 
			gaugeSymmetries,
			Cases[ FSymbols, TrivialFPattern ] ,
			z
		];
	restrictedFSymbols =
		DeleteCases[ FSymbols, TrivialFPattern ];
	restrictedInvMats = 
		DeleteCases[ invMats, {{F[ 1, __ ]}} | {{F[ _, 1, __ ]}} | {{F[ _, _, 1, __ ]}} ];
	restrictedEqns = 
		DeleteCases[True] @ 
		(monEqns/.TrivialFPattern -> 1);
	
	Export[
		"~/BinomialSolutionsRepS4.mx",
		SolveBinomialSystem[
			restrictedEqns, 
			restrictedFSymbols, 
			z, 
			"InvertibleMatrices" -> restrictedInvMats, 
			"Symmetries" -> restrictedSym 
		]
	]
	(*
	pentSoln = 
		(*DeleteSymmetryEquivalentSolutions[ restrictedSym, "SimplifyBy" -> ComplexExpand, "Numeric" -> True ] @ *)
		SolvePolynomialSystem[ 
			restrictedEqns, 
			restrictedFSymbols, 
			z, 
			"Symmetries" -> restrictedSym, 
			"InvertibleMatrices" -> restrictedInvMats,
			"FindZerosUsingSums" -> False
		]
		*)
	(*
	hexSoln =
		(* SolvePolynomialSystem[ #, GetVars[ #, R ], z, "NonSingular" \[Rule] True ]& /@*)Print @ 
		(
			( MultFreeHexagonEquations[ #, Rank[c["Ring"]], R ][[1]] )& /@ 
			pentSoln
		);
	*)
	(*
	Print[Simplify[TableForm[Flatten[hexSoln,1]]]/.fancyR];
	*)
]


m = Import["~/BinomialSolutionsRepS4.mx"];


m//Short


(* ::Subsection::Closed:: *)
(*Solving pentagon equations, all gauges fixed*)


Get[ "/home/gert/Projects/AnyonTools/Packages/PentaTools/PentaTools.wl" ]
ToStringJoin[ str__ ] := StringJoin @@ (ToString /@ {str});
Module[ {
	c = ProspectiveFusionCat[ FRBC[{5,1,0,6}] ],
	monEqns, gaugeSymmetries, gtransf, FSymbols, restrictedSym, restrictedFSymbols, restrictedEqns, TrivialFPattern, monSolutions,
	sumSolutions, pentEqns, sumEqns, gb, invMats, restrictedInvMats, pentSoln, hexSoln,extraFixedFs, 
	fancyF = F[a_,b_,c_,d_,e_,f_] :> Subsuperscript[ "F", ToStringJoin[d,e,f], ToStringJoin[a,b,c]],
	fancyR = R[a_,b_,c_] :> Subsuperscript[ "R",ToString[c], ToStringJoin[a,b]],
	fancyP = P[a_,b_,c_,d_,e_] :> Subsuperscript[ "P", ToStringJoin[d,e], ToStringJoin[a,b,c]],
	fancyQ = Q[a_,b_,c_,d_,e_] :> Subsuperscript[ "Q", ToStringJoin[d,e], ToStringJoin[a,b,c]]
	},
	Print[c["Ring"]];
	FSymbols = c["FSymbols"]["Values"][[;;,1]];
	monEqns  = c["PentEqns"]["Hom"] // Values // Flatten; sumEqns  = c["PentEqns"]["NonHom"] // Values // Flatten; pentEqns = Join[ monEqns, sumEqns ];
	invMats  = c["RegularFMatrices"];
		
	gtransf[ F[a_,b_,c_,d_,e_,f_] ] := F[a,b,c,d,e,f] (u[a,b,e]u[e,c,d])/(u[a,f,d] u[b,c,f]);
	gaugeSymmetries = <| "Transforms" -> Table[ f -> gtransf[f], { f, FSymbols } ], "Symbols" -> {u} |>;
	
	TrivialFPattern = 
		F[ 1, __ ] | F[ _, 1, __ ] | F[ _, _, 1, __ ];
	
	restrictedSym =
		RestrictMultiplicativeSymmetries[ 
			gaugeSymmetries,
			Cases[FSymbols, TrivialFPattern ],
			z
		];
		

	extraFixedFs = Module[ { nextF, rsym = restrictedSym  },
		Reap[
		While[ 
			Head[ nextF = FirstCase[ rsym["Transforms"], HoldPattern[ x_ -> x_ * y_ ] ] ] =!= Missing,
			rsym = 
				RestrictMultiplicativeSymmetries[ 
					rsym,
					{ Sow[ nextF[[1]] ] },
					z
				]
		]
		][[2,1]]
	];
	
	restrictedSym = 
		RestrictMultiplicativeSymmetries[ 
			restrictedSym,
			extraFixedFs ,
			z
		];
	restrictedFSymbols = 
		DeleteCases[ Complement[ FSymbols, extraFixedFs ], TrivialFPattern ];
	restrictedInvMats =
		DeleteCases[ {{n_?NumericQ}} /; n != 0 ] @
		DeleteCases[ {{#}}& /@ TrivialFPattern ] @
		( invMats/.Thread[ extraFixedFs -> 1 ] );
	restrictedEqns = 
		DeleteCases[True] @ (pentEqns/.{ TrivialFPattern -> 1 }/.Thread[ extraFixedFs -> 1 ]);
		
	pentSoln =
	DeleteSymmetryEquivalentSolutions[ gaugeSymmetries, "SimplifyBy" -> ComplexExpand, "Numeric" -> True ][ 
		Sort[ Join[ Thread[ Cases[ FSymbols, TrivialFPattern ] -> 1 ], Thread[ extraFixedFs -> 1 ], # ] ]& /@ 
		SolvePolynomialSystem[ 
			restrictedEqns, 
			restrictedFSymbols, 
			z, 
			"Symmetries" -> restrictedSym, 
			"InvertibleMatrices" -> restrictedInvMats,
			"FindZerosUsingSums" -> True
		]
	];
	Print[ TableForm[ pentSoln ]/.fancyF ];
	(*
	hexSoln = 
		Join[ #[[1]], SolvePolynomialSystem[ #[[2]], GetVars[#,{R,P,Q}], z, "NonSingular" -> True ][[1]] ]& /@ 
		(
			{#,(TrijunctionThreeParticleHexagonEquations[ #, Rank[c["Ring"]], R, P, Q ][[1]])}& /@ 
			pentSoln
		);
	
	Print[hexSoln/.fancyR/.fancyP/.fancyQ/.fancyF];
	*)
]


(* ::Subsection::Closed:: *)
(*PentagonSolver*)


monSolutionsRepS4 = Import["~/BinomialSolutionsRepS4.mx"];


c = ProspectiveFusionCat[FRBC[{5,1,0,6}]];


monSolnc = Dispatch[monSolutionsRepS4];


monRepS4 = c["PentEqns"]["Hom"]//Values//Flatten;
sumRepS4 = c["PentEqns"]["NonHom"]//Values//Flatten;
TrivialFPattern = F[ 1, __ ] | F[ _, 1, __ ] | F[ _, _, 1, __ ];
monAfterSubsRepS4 = DeleteCases[True] /@ (monRepS4/.monSolnc/.Dispatch[TrivialFPattern ->1]);
sumAfterSubsRepS4 = DeleteCases[True] /@ (sumRepS4/.monSolnc/.Dispatch[TrivialFPattern ->1]);


monAfterSubsRepS4[[1]]


(* ::Chapter:: *)
(*Theories With Multiplicity*)


<<AnyonTools`
<<Notation`


Format[F[a_,b_,c_,d_,{e_,i_,j_},{f_,k_,l_}]]:=
SubsuperscriptBox[
	RowBox[{"[",SubsuperscriptBox["F",d,RowBox[{a,",",b,",",c}]], "]"}],
	RowBox[{"(",e,",",i,",",j,")"}],
	RowBox[{"(",f,",",k,",",l,")"}]
]//DisplayForm

Format[G[a_,b_,c_][i_,j_]]:= 
	SubsuperscriptBox[
		RowBox[{"[",SubsuperscriptBox["G",c,RowBox[{a,",",b}]], "]"}],
		j,
		i
	]//DisplayForm;


fSymbolsMult[ ring_ ] :=
	Module[{ a, b, c, d, e, r, non0Ns, mt, compatibleNs, multQ, dims },
	r = Rank[ring]; non0Ns = NZSC[ring]; mt = MT[ring]; multQ = Mult[ring] > 1;
  Sort @
  Reap[
    Do[
      {a,b,e} = label1;
      compatibleNs = Cases[ non0Ns, {e,_,_} ];
      Do[
        {c,d} = label2[[{2,3}]];
        Do[
          If[
            mt[[b,c,f]] mt[[a,f,d]] =!= 0,
            If[
              !multQ,
              Sow @ F[a,b,c,d,e,f],
              dims = { mt[[a,b,e]], mt[[e,c,d]], mt[[b,c,f]], mt[[a,f,d]] }; 
              Map[
                Sow,
                Flatten @ Array[ F[a,b,c,d,{e,#1,#2},{f,#3,#4}]&, dims ]
              ]
            ]
          ],
          {f,r}
        ],
        { label2, compatibleNs }
      ],
      { label1, non0Ns }
    ]
  ][[2,1]]
];


ring = FirstCase[ FRL, r_/; Mult[r] >1 ];


fSymbolsMult[ ring ]





(* ::Section:: *)
(*Gauge transforms *)


<<Notation`


(* ::Input:: *)
(*Notation[NotationTemplateTag[\!\(\*SubsuperscriptBox[*)
(*FrameBox[*)
(*SubsuperscriptBox["\[ScriptCapitalF]", "d_", GridBox[{*)
(*{"a_", "b_", "c_"}*)
(*}]]], \((\*GridBox[{*)
(*{*)
(*Pattern[f, Blank[]], *)
(*Pattern[\[Gamma], Blank[]], *)
(*Pattern[\[Delta], Blank[]]}*)
(*}])\), \((\*GridBox[{*)
(*{*)
(*Pattern[e, Blank[]], *)
(*Pattern[\[Alpha], Blank[]], *)
(*Pattern[\[Beta], Blank[]]}*)
(*}])\)]\)] \[DoubleLongLeftArrow] NotationTemplateTag[F[a_,b_,c_,d_,{e_,\[Alpha]_,\[Beta]_},{f_,\[Gamma]_,\[Delta]_}]]]*)
(*Notation[NotationTemplateTag[\!\(\*SubsuperscriptBox[*)
(*FrameBox[*)
(*SubsuperscriptBox["G", "c_", GridBox[{*)
(*{"a_", "b_"}*)
(*}]]], \(\[Alpha]2_\), \(\[Alpha]1_\)]\)] \[DoubleLongLeftArrow] NotationTemplateTag[G[a_,b_,c_][\[Alpha]1_,\[Alpha]2_]]]*)
(*Notation[NotationTemplateTag[\!\(\*SubsuperscriptBox[*)
(*FrameBox[*)
(*SuperscriptBox[*)
(*RowBox[{"(", *)
(*SubsuperscriptBox["G", "c_", GridBox[{*)
(*{"a_", "b_"}*)
(*}]], ")"}], *)
(*RowBox[{"-", "1"}]]], \(\[Alpha]2_\), \(\[Alpha]1_\)]\)] \[DoubleLongLeftArrow] NotationTemplateTag[Ginv[a_,b_,c_][\[Alpha]1_,\[Alpha]2_]]]*)


(* ::Input:: *)
(*ring = FirstCase[FRL, r_/; Mult[r]>1&&Rank[r]>2];*)


(* ::Input:: *)
(*ring // MT*)


(* ::Input:: *)
(*fsymbols[ ring_ ] := *)
(*Module[ {mult, rank}, *)
(*If[ *)
(*mult == 1, *)
(*FSymbols[ring],*)
(*]*)
(*]*)
(**)
(*NZSC[ring]*)
(**)


(* ::Input:: *)
(*fsymbols[ring_FusionRing?FusionRingQ]:=*)
(*Module[{a,b,c,d,e,r,non0Ns,multTab,compatibleNs, mult1, mult2, mult3, mult4},*)
(*r=Rank[ring];*)
(*non0Ns=NZSC[ring];*)
(*multTab=MT[ring];*)
(*Sort@*)
(*Flatten @*)
(*Reap[*)
(*Do[*)
(*{a,b,e}=label1;*)
(*mult1 = multTab[[a,b,e]];*)
(*compatibleNs=Cases[ non0Ns,{e,_,_}];*)
(*Do[*)
(*{c,d}=label2[[{2,3}]];*)
(*mult2 = multTab[[e,c,d]];*)
(*Do[*)
(*mult3 = multTab[[b,c,f]];*)
(*mult4 = multTab[[a,f,d]];*)
(*If[*)
(*mult3 mult4 !=0,*)
(*Sow@*)
(*Table[*)
(*F[a,b,c,d,{e,\[Alpha],\[Beta]},{f,\[Gamma],\[Delta]}], *)
(*{\[Alpha],mult1},{\[Beta],mult2},{\[Gamma],mult3},{\[Delta],mult4}*)
(*]*)
(*],*)
(*{f,r}*)
(*],*)
(*{label2,compatibleNs}*)
(*],*)
(*{label1,non0Ns}*)
(*]*)
(*][[2,1]]*)
(*];*)
(**)


(* ::Input:: *)
(*gaugeTransform[ ring_ , G_ ][ F[a_,b_,c_,d_,{e_,\[Alpha]_,\[Beta]_},{f_,\[Gamma]_,\[Delta]_}] ] :=*)
(*Module[{mt, mult,L,R,FMat},*)
(*mt = *)
(*MT[ ring ];*)
(*mult =Echo@*)
(*{ mt[[a,b,e]], mt[[e,c,d]], mt[[a,f,d]], mt[[b,c,f]]};*)
(*L =*)
(*KroneckerProduct[ *)
(*Array[ G[a,b,e], {mult[[1]],mult[[1]]} ],*)
(*Array[ G[e,c,d], {mult[[2]],mult[[2]]} ]*)
(*];*)
(*R = *)
(*KroneckerProduct[*)
(*Array[ Ginv[a,f,d], {mult[[3]],mult[[3]]} ],*)
(*Array[ Ginv[b,c,f], {mult[[4]],mult[[4]]} ]*)
(*];*)
(*FMat = *)
(*ArrayReshape[*)
(*Array[*)
(*F[a,b,c,d,{e,#1,#2},{f,#3,#4}]&,*)
(*{mult[[1]],mult[[2]],mult[[3]],mult[[4]]}*)
(*],{mult[[1]]*mult[[2]],mult[[3]]*mult[[4]]}*)
(*];*)
(*(*HoldForm[l.ff.r]/.{l->MatrixForm[L],ff->MatrixForm[FMat],r->MatrixForm[R]}*)*)
(*ArrayReshape[*)
(*(L . FMat . R),*)
(*mult*)
(*]*)
(*]*)


(* ::Input:: *)
(*gaugeTransform[ ring,G ][fsymbols[ring][[-23]]]*)


(* ::Input:: *)
(*fsymbols[ring][[-23]]*)
(*Solve[ *)
(*Thread[{1,0,0,0}==Flatten[gaugeTransform[ ring,G ][fsymbols[ring][[-23]]]]],Cases[gaugeTransform[ ring,G ][fsymbols[ring][[-23]]],G[__][__]|Ginv[__][__],Infinity ]//DeleteDuplicates*)
(* ]*)


(* ::Input:: *)
(*Det[#]*Inverse[#]&@Array[G[a,b,c],{4,4}]//MatrixForm*)


(* ::Input:: *)
(*Thread[{1,0,0,0}==Flatten[gaugeTransform[ ring,G ][fsymbols[ring][[-23]]]]]*)


(* ::Input:: *)
(*Cases[gaugeTransform[ ring,G ][fsymbols[ring][[-23]]],G[__][__]|Ginv[__][__],Infinity ]//DeleteDuplicates*)


(* ::Input:: *)
(*gaugeTransform[ ring,G ][fsymbols[ring][[-23]]]*)


(* ::Input:: *)
(*mult = {2,2,1,1};*)
(*ArrayReshape[*)
(*Array[*)
(*F[a,b,c,d,{e,#1,#2},{f,#3,#4}]&,*)
(*{mult[[1]],mult[[2]],mult[[3]],mult[[4]]}*)
(*],{mult[[1]]*mult[[2]],mult[[3]]*mult[[4]]}*)
(*]*)
(**)
(**)


(* ::Input:: *)
(*L =*)
(*KroneckerProduct[ *)
(*Array[ G[a,b,e], {mult[[1]],mult[[1]]} ],*)
(*Array[ G[e,c,d], {mult[[2]],mult[[2]]} ]*)
(*];*)
(*R = *)
(*KroneckerProduct[*)
(*Array[ G[a,f,d], {mult[[3]],mult[[3]]} ],*)
(*Array[ G[b,c,f], {mult[[4]],mult[[4]]} ]*)
(*];*)


(* ::Input:: *)
(*L*)


(* ::Input:: *)
(*fsymbols[ring]*)
