(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-10-11 *)


BackTrack2Cocycles[
	G_FusionRing, (* grading group *)
	A_FusionRing?CommutativeQ, (* invertibles in trivial component *)
	action_,
	w_
	] :=
(
	If[ !GroupRingQ[G] || ! GroupRingQ[A], Print["Rings should be group rings"]; Abort[] ];
	Module[{GNRules, ANRules, GProd, RProd, GElements, RElements,
		cocycleEquations, leftAct, CompleteCocycle, newRule, trivialRules, unknowns
		},
		(* Basic check *)
		GNRules = Dispatch[ ( { #1, #2 } -> #3 )& @@@ NZSC @ G ];
		ANRules = Dispatch[ ( { #1, #2 } -> #3 )& @@@ NZSC @ A ];
		
		GProd[ i_Integer, j_Integer ] :=
			{ i, j } /. GNRules;
		RProd[ i_Integer, j_Integer ] :=
			{ i, j } /. ANRules;
		
		GElements = Range @ Rank @ G;
		RElements = Range @ Rank @ A;
		
		leftAct[ i_Integer ] :=
  		action[[i]];
		
		cocycleEquations =
			Flatten @
			Table[
				leftAct[ RProd[ w[ g2, g3 ], w[ g1, GProd[ g2, g3 ] ] ] ] ==
				RProd[ w[ GProd[ g1, g2 ], g3 ], w[ g1, g2 ] ],
				{ g1, GElements },
				{ g2, GElements },
				{ g3, GElements }
			];
		
		(* Backtrack on cocycles *)
		CompleteCocycle[ {}, {}, rules_ ] :=
			Sow[ rules ];
		
		CompleteCocycle[ eqns_, unknowns_, rules_ ] :=
		(
			If[ MemberQ[ False ] @  eqns, Return[{}] ];
			Module[ { newRule },
				Do[
					newRule = First[unknowns] -> g;
     
					CompleteCocycle[
						DeleteCases[True] @ ReplaceAll[ eqns, newRule ]
						,
						Rest @ unknowns
						,
						Append[ newRule ] @ rules
					]
					, { g, RElements }
				]
			]
		);
		
		Reap[
			Do[
				trivialRules = Table[ w[ 1, h ] -> g, { h, GElements } ];
				unknowns =
					Complement[
						Flatten @
						Table[
							w[ h1, h2 ],
							{ h1, GElements },
							{ h2, GElements }
						],
						trivialRules[[;;, 1]]
					];
				
				CompleteCocycle[
					DeleteCases[ cocycleEquations /. trivialRules, True ],
					unknowns,
					trivialRules
				]
				
				, { g, RElements }
			]
		
		][[2, 1]]
	]
);