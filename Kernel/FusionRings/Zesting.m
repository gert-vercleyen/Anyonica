(* ::Package:: *)

ZestedFusionRings[ R_ ] :=
	Module[ { actions, gradingMap, UG, invertibles, invRe, ple, w, cocycles, allCocycles, mt,zestedMultTabs, fpDims },
		
		(* For now we only look at universal grading group *)
		{ gradingMap, UG } = 
			EchoLabel["UniversalGrading"] @ 
			UniversalGrading @ R;
		
		(* Find all point-like elements with trivial grade *)
		fpDims = FPDims[R];
		
		ple = 
			EchoLabel["inv(R_e)"] @
			Keys @
			Cases[
				gradingMap, 
				arr_ /; Values[arr] == 1 && fpDims[[ Keys[arr] ]] == 1  
			];
		
		(* These elements form a group: invRe. This will be the G-Module *)
		invRe =
			FusionRing[ "MultiplicationTable" -> MT[R][[ ple, ple, ple ]] ];
		
		(* If invRe is non-commutative the procedure doesn't work. *)
		If[ !CommutativeQ[invRe], Return @ { R } ];
			
		actions = 
			EchoLabel["Actions"] @
			LinearGroupActions[ UG, invRe ]; (* Function defined below *)
			
		allCocycles = 
			EchoLabel["Cocycles"] @
			DeleteDuplicates[
				Join @@
				Table[ 
					NormalizedCocycles[ UG, invRe, a, 2 ]/.({i_,j_}->k_):>({i,j}->ple[[k]]),
					{ a, actions }
				]
			];
		
		mt = MultiplicationTable[R];
		
		zestedMultTabs = 
			EchoLabel["ZestedMultTabs"] @
			DeleteDuplicates @
			Table[
				zest2[ mt, gradingMap, w ], (* Function defined below *)
				{ w, allCocycles }
			];
			
		DeleteDuplicates[
			FusionRing[ "MultiplicationTable" -> # ]& /@
			zestedMultTabs,
			EquivalentFusionRingsQ
		]
		
	];


(* ::Subsection:: *)
(*Cocycles*)


(* Code to calculate n-cocycles from a group (seen as a fusion ring with group mult rules) 
   to an abelian group via an action. At the moment only n = 2,3 are supported. The cocycles are normalized in the sense that they map
   { 1, g } -> 1, { g, 1 } -> 1 *)
(* Args: 
  - G: grading group 
  - M: commutative group that is also a G-module. For zesting these are the invertibles in trivially graded component
  - action: action that makes M a G-module. This should be a function (or association) that maps integers to permutation vectors   
*)

NormalizedCocycles::usage =
	"NormalizedCocycles[ G, M, action, n ] returns a list of normalized n-cocycles for the G-module M where n can be 2 or 3.";
	
NormalizedCocycles::notagroup = 
	"`1` and `2` should be group rings.";
	
NormalizedCocycles::nonabeliangroup = 
	"`1` should be an abelian group ring.";

				
NormalizedCocycles[ G_FusionRing, M_FusionRing?CommutativeQ, action_, n_Integer ] :=
	Module[ { GNRules, MNRules, GProd, MProd, GElements, MElements, leftAct, 
		CompleteCocycle, trivialRules, unknowns, cocycles, w, updatedEquations },
		
		(* Basic check *)
		If[ !GroupRingQ[G] || !GroupRingQ[M], Message[NormalizedCocycles::notagroup, G, M ]; Abort[] ];
		
		(* Group elements are integers *)
		GElements = Range @ Rank @ G;
		MElements = Range @ Rank @ M;
		
		(* Define multiplication of group elements *)
		GNRules = Dispatch[ ( { #1, #2 } -> #3 )& @@@ NonZeroStructureConstants @ G ]; 
		MNRules = Dispatch[ ( { #1, #2 } -> #3 )& @@@ NonZeroStructureConstants @ M ];
		
		(* We need to hold elements that aren't integers. Otherwise we get array errors *)
		GProd[ i_Integer, j_Integer ] := { i, j }/.GNRules;
		MProd[ i_Integer, j_Integer ] := { i, j }/.MNRules;
		
		(* Define action via permutation *)
		leftAct[ g_Integer ][ m_Integer ] := action[g][[m]];
		
		(* Backtrack on cocycles *)
		CompleteCocycle[ {}, {}, rules_ ] := 
			Sow[ rules ];
			
		CompleteCocycle[ eqns_, unknowns_, rules_ ] := 
			Module[ { newRule },
				If[ MemberQ[ False ] @ eqns, Return @ {} ];
				Do[
					newRule = First[unknowns] -> g; 
					
					CompleteCocycle[ 
						DeleteCases[True] @ ReplaceAll[newRule] @ eqns
						,
						Rest @ unknowns
						,
						Append[ newRule ] @ rules
					]
					, 
					{ g, MElements }
				]
			];
			
		cocycles = 
			Reap[
				(* Cocycles are normalized *)
				trivialRules = 
					Switch[ n, 
						2, 
							Union[ 
								Table[ w[ g, 1 ] -> 1, { g, GElements } ], 
								Table[ w[ 1, g ] -> 1, { g, GElements } ] 
							],
						3, 
							Union @ 
							Flatten @ 
							Join[
								Table[ w[ g, h, 1 ] -> 1, { g, GElements }, { h, GElements } ], 
								Table[ w[ g, 1, h ] -> 1, { g, GElements }, { h, GElements } ], 
								Table[ w[ 1, g, h ] -> 1, { g, GElements }, { h, GElements } ]
							]
					];
				
				(* Equations contain no trivial cocycles  *)
				updatedEquations = EchoLabel["cocycleEqns"]@
					DeleteCases[True] @ 
					ReplaceAll[trivialRules] @ 
					cocycleEquations[ GProd, MProd, leftAct, GElements, w, n ];
				
				unknowns = 
					GetVariables[ updatedEquations, w ];
				
				CompleteCocycle[ 
					updatedEquations,
					unknowns,
					trivialRules
				]
			][[2]]/.w->List/.{{x__}}:>{x}
		
]

(* tM = product from M, tG = product from G *)
cocycleEquations[ tG_, tM_, leftAct_, GElements_, w_, n_ ] :=
	Switch[ n,
		2,  
		Flatten @ 
		Table[
		  (leftAct[ g1 ] @  w[ g2, g3 ]) ~ tM ~ w[ g1, g2**g3 ]  == 
			w[ g1**g2, g3 ] ~ tM ~ w[ g1, g2 ],
			{ g1, GElements },
			{ g2, GElements },
			{ g3, GElements }
		],
		3,
		Flatten @ 
		Table[
		  ( leftAct[ g1 ] @ w[ g2, g3, g4 ]) ~ tM ~ w[ g1, g2**g3, g4 ] ~ tM ~ w[ g1, g2, g3 ] == 
			w[ g1**g2, g3, g4 ] ~ tM ~ w[ g1, g2, g3**g4 ],
			{ g1, GElements },
			{ g2, GElements },
			{ g3, GElements },
			{ g4, GElements }
		]
	]/.NonCommutativeMultiply -> tG;



z2 = FusionRingFromGroup[ CyclicGroup[2] ];
z3 = FusionRingFromGroup[ CyclicGroup[3] ];
z4 = FusionRingFromGroup[ CyclicGroup[4] ];
z2t2 = FusionRingFromGroup[ AbelianGroup[ { 2, 2 } ] ];
z2t2t2 = FusionRingFromGroup[ AbelianGroup[ { 2, 2, 2 } ] ];


NormalizedCocycles[ z2, z2t2t2,<|1->{1,2,3,4,5,6,7,8},2->{1,2,4,3,6,5,7,8}|>, 3 ]


(* ::Subsection:: *)
(*Group Actions*)


(* Find all linear group actions on a an abelian group. G and H are group fusion rings *)

LinearGroupActions[ G_FusionRing?GroupRingQ, M_FusionRing?GroupRingQ ] := 
	Module[ { GProd, MProd, GEl, MEl, knowns, eqns, Gmt, Mmt, Actions, unknowns,a },
		If[ Rank[G] == 1, Return @ { <| 1 -> Range[ Rank[M] ] |> } ];
		
		If[ Rank[M] == 1, Return @ { Association[ Table[ i -> {1}, { i, Rank @ G } ]  ] } ];
	(*
		If[ 
			Rank[G] == 2, (* Looking at all involutions on Rank[M] elements: TOO MANY HERE: GROUP ACTION SHOULD BE LINEAR!! *)
			Return @
			With[ {id = Range @ Rank @ M },
				<| 1 -> id, 2 -> # |>& /@ Involutions[ Rank @ M ]
			]
		];
*)
		Gmt = groupMultTab @ G;
		Mmt = groupMultTab @ M;
	
		GProd[ i_Integer, j_Integer ] := Gmt[[ i, j ]];
    MProd[ i_Integer, j_Integer ] := Mmt[[ i, j ]];
    
    GEl = Range @ Rank @ G;
    MEl = Range @ Rank @ M;
	
		knowns = 
			Join[
				Table[ a[ 1, m ] -> m, { m, Rank @ M } ],
				Table[ a[ g, 1 ] -> 1, { g, 2, Rank @ G }]
			];
		
		unknowns = 
			Flatten @
			Table[ a[ i, j ] , { i, 2, Rank @ G }, { j, 2, Rank @ M } ];
			
		eqns = 
			DeleteCases[True] @ 
      ReplaceAll[knowns] @
			Flatten @ 
			Join[
				Table[ 
					a[ h, a[ g, m ] ] == a[ GProd[ h, g ], m ],
					{ g, 2, Rank @ G },
					{ h, 2, Rank @ G }, 
					{ m, 2, Rank @ M }
				], 
				Table[ 
					a[ g, MProd[ m, n ] ] == MProd[ a[ g, m ], a[ g, n ] ],
					{ g, 2, Rank @ G },
					{ m, 2, Rank @ M }, 
					{ n, 2, Rank @ M }
				]
			];
			
		Actions[ {}, {}, rules_, _ ] := 
      Sow[ rules ];
            
    Actions[ eqns_, unknowns_, rules_, possibleValues_ ] := 
      Module[{newRule,newEqns,var,varInd,g,m,newRules},
        If[ MemberQ[ False ] @ eqns, Return @ {} ];
        
        var = First @ unknowns;
        { g, m } = List @@ var;
                
           Do[
              newRule = First[unknowns] -> n ; 
              
              newRules = Append[ newRule ] @ rules;
                    
              Actions[ 
                DeleteCases[True][ eqns//.newRules ]
                ,
                Rest @ unknowns
                ,
                newRules
                , 
                MapAt[ DeleteCases[n], possibleValues, { g } ]
              ]
              , 
              { n, possibleValues[[g]] }
           ]
       ];
       
       Map[
         Last,
         GroupBy[ #, First -> Rest, SortBy[First] ],
         {2}
       ]&/@
       (
       Reap[
         Actions[ eqns, unknowns, knowns, ConstantArray[ Rest @ Range @ Rank @ M, Rank @ G ] ]
       ][[2,1]] /. ( a[ i_, j_ ] -> k_ ) :> {i,j,k}
       )  
	];
	
			
	
(* From https://mathematica.stackexchange.com/questions/173039/how-to-generate-all-involutive-permutations *)
PackedQ = Developer`PackedArrayQ;
ToPack = Developer`ToPackedArray;

Involutions[k_Integer] := 
Block[ { data, A, n, g, list },
	If[ k == 2, Return @ { { 1, 2 }, { 2, 1 } } ];
	list = Range @ k;
  n = Length[list];
  A = ToPack[
    UpperTriangularize[{Range[3, n]}[[ConstantArray[1, n - 1]]]]];
  A[[2 ;;]] += ToPack[LowerTriangularize[{Range[2, n - 1]}[[ConstantArray[1, n - 2]]]]];
  g[2, {{}}] = ToPack[{{{1, 2}}}];
  g[n_, data_] := With[{m = Length[data]},
    Join[
     Transpose[{{
        ConstantArray[1, {(n - 1) m}],
        Flatten[Table[ConstantArray[i, {m}], {i, 2, n}]]
        }},
      {2, 3, 1}
      ],
     ArrayReshape[
      A[[1 ;; (n - 1), Flatten[data]]], {(n - 1) m, 
      Quotient[n - 2, 2], 2}
      ], 
    2]
    ];

  data = {{}};
  Join @@ Join[
    {{list}},
    Table[
     data = g[ 2 i, data ];
     getPermutationLists[
      list,
      ArrayReshape[
       Subsets[Range[n], {2 i}][[All, Flatten[data]]],
       {Binomial[n, 2 i] Length[data], Sequence @@ Rest[Dimensions[data]]}
       ]
      ],
     {i, 1, Quotient[n, 2]}]
    ]
  ]
  
getPermutationLists = 
	Compile[{{ran, _Integer, 1}, {idx, _Integer, 2}},
		Block[{a = ran, i, j, k, x},
			Do[
				i = Compile`GetElement[idx, k, 1];
				j = Compile`GetElement[idx, k, 2];
				x = Compile`GetElement[a, i];
				a[[i]] = Compile`GetElement[a, j];
				a[[j]] = x,
				{k, 1, Length[idx]}
			];
			a
		],
		CompilationTarget -> "C",
		RuntimeAttributes -> {Listable},
		Parallelization -> True,
		RuntimeOptions -> "Speed"
	];	  


(* ::Subsection:: *)
(*Group Homomorphisms*)


(* Code to find all group homomorphisms. G and H are the multiplication tables *)
GroupHomomorphisms[ G_?MatrixQ, H_?MatrixQ ] := 
    Module[{ HProd, GProd, homEquations, CompleteHom, h },
        
        (* We need GProd and HProd to be unevaluated for non-integers
           otherwise we get array errors  *)
        GProd[ i_Integer, j_Integer ] := G[[ i, j ]];
        HProd[ i_Integer, j_Integer ] := H[[ i, j ]];
        
        homEquations =
            Flatten @ 
            Table[
                h[ GProd[ i, j ] ] == HProd[ h[ i ], h[ j ] ],
                { i, 2, Length @ G },
                { j, 2, Length @ G }
            ];  
        
        (* Backtracking function *)
        (* If no unknowns and no equations left: Sow the solution *)
        CompleteHom[ {}, {}, rules_ ] := 
            Sow[ rules ];
            
        (* Otherwise: If equations are false: return nothing. 
           If not false: for each possible value: set unknown equal 
           to that value, update equations, unknowns, and rules 
           and continue.  *)
        CompleteHom[ eqns_, unknowns_, rules_ ] := 
            Module[{newRule,newEqns},
                If[ MemberQ[ False ] @ eqns, Return @ {} ];
                
                Do[
                    newRule = First[unknowns] -> m; 
                    
                    CompleteHom[ 
                        DeleteCases[True] @ ReplaceAll[newRule] @ eqns
                        ,
                        Rest @ unknowns
                        ,
                        Append[ newRule ] @ rules
                    ]
                    , 
                    { m, Length @ H }
                ]
                
            ];
        
        Reap[
            CompleteHom[ 
                DeleteCases[True] @ ReplaceAll[{ h[ 1 ] -> 1 }] @ homEquations,
                Table[ h[i], { i, 2, Length[G] } ],
                { h[ 1 ] -> 1 }
            ]
        ][[2]] /. h -> Identity /. { { x__ } } :> { x }
    ];
	
		
(* Function that zests a multiplication table *)
zest[ mt_, grading_, cocycle_, ple_ ] := 
	Module[{ rk = Length[mt], deg, w },
	
			deg[ i_Integer ] := i/.grading;
			
			w[ i_, j_ ] := { i, j }/.cocycle;
			
			Table[ 
				(mt[[i,j]] . mt)[[ ple[[ w[ deg[i], deg[j] ] ]] ]],
				{ i, rk },
				{ j, rk }
			]
	];
	


(* ::Subsection:: *)
(*AllGradings*)


AllGradings[ ring_FusionRing ] :=
	Module[ { gradingMap, UG, subGroups, homs, epimorphismQ },
		{ gradingMap, UG } = 
			UniversalGrading @ ring;
		
		(* Construct all subgradings *)
		subGroups = 
			SubFusionRings @ ring; (* this should be of the UG *)
			
		epimorphismQ[ hom_, subGroup_ ] :=
			Union[Values[hom]] == Range @ Rank @ subGroup;
				
		homs = 
			GroupHomomorphisms[ 
					groupMultTab @ UG, 
					groupMultTab @ # 
			]& /@ subGroups[[;;,2]];
		
		
		DeleteDuplicates[
		Append[ { gradingMap, UG } ] @
			Reap[
			Table[
				If[ 
					epimorphismQ[ homs[[i, j]], subGroups[[i,2]] ],
					Sow @{ MapAt[ ReplaceAll[ homs[[i, j]] ], gradingMap, { All, 2 } ], subGroups[[i,2]]  }
				], 
				{ i, Length @ homs },
				{ j, Length @ homs[[i]] }
			]
			][[2,1]]
		]
	];
	
groupMultTab[ ring_?GroupRingQ ] := 
	Normal @ SparseArray[ {#1,#2}->#3&@@@NZSC[ring], { Rank @ ring, Rank @ ring} ]


(* ::Subsection:: *)
(*ZestedFusionRings2*)


ZestedFusionRings2[ R_ ] :=
	Module[ { actions, gradings, UG, invertibles, invRe, ple, w, cocycles, allCocycles, mt,zestedMultTabs, fpDims, subGroups },
		
		(* This time, we will also look at subgroups of the universal grading group *)
		gradings = 
			EchoLabel["AllGradings"] @ (* Should have a function AllGradings *)
			AllGradings @ R;
		
		(* Find all point-like elements with trivial grade *)
		fpDims = FPDims[R];
		
		ple = 
			EchoLabel["inv(R_e)"] @
			Keys[
				Cases[ 
					#, 
					arr_ /; Values[arr] == 1 && fpDims[[ Keys[arr] ]] == 1  
				]
			]& /@ gradings[[;;,1]];
		
		(* These elements form a group: invRe. This will be the G-Module *)
		(* If invRe is non-commutative the procedure doesn't work. *)
		invRe =
			Select[
				FusionRing[ "MultiplicationTable" -> MT[R][[ #, #, # ]] ]& /@
				ple,
				CommutativeQ
			];
		
		If[ Flatten[invRe] === {}, Return @ { R } ];
			
		actions = 
			EchoLabel["Actions"] @
			MapThread[ 
				LinearGroupActions,
				{ gradings[[;;,2]], invRe }
			]; 
			
		allCocycles = 
			EchoLabel["Cocycles"][
			DeleteCases[
			DeleteDuplicates /@
				Flatten[ 
				Table[ 
					With[ { grp = gradings[[i,2]], ind = gradings[[i,1]], module = invRe[[i]], action = actions[[i,j]] },
						Echo @ { grp, module, action };
						NormalizedCocycles[ grp, module, action, 2 ] (*//
						ReplaceAll[ ({i_,j_}->k_) :> ( { ind[[i]], ind[[j]] } -> ple[[k]] ) ]*) (* Have to map group elements to their spots in fusion ring *)
					], 
					{ i, Length @ gradings },
					{ j, Length @ actions[[i]] }
				], 
				1
				],
			{}
			] (* Check whether empty solution makes sense *)
			];
		
		mt = MultiplicationTable[R];
		
		zestedMultTabs = 
			DeleteDuplicates @
			EchoLabel["ZestedMultTabs"] @
			Flatten[
			Table[
				Echo[{ gradings[[i,1]], allCocycles[[i,j]]}];
				zest2[ mt, gradings[[i,1]], allCocycles[[i,j]] ], (* Function defined below *)
				{ i, Length @ allCocycles },
				{ j, Length @ allCocycles[[i]] }
			],
			1
			];
			
		DeleteDuplicates[
			FusionRing[ "MultiplicationTable" -> # ]& /@
			zestedMultTabs,
			EquivalentFusionRingsQ
		]
		
	];

zest2[ mt_, grading_, cocycle_ ] := 
	Module[
		{ r, deg, \[Lambda] }, 
		
		r = Length @ mt;
		
		deg[ a_ ] := a/.grading;
			
		\[Lambda][ a_, b_ ] := { a, b }/.cocycle;
	
		Normal @ 
		SparseArray[
			Flatten[
				Table[ 
					{ X, Y, Z } -> 
						Sum[ mt[[ \[Lambda][ deg[X], deg[Y] ], X, W ]] mt[[ W, Y, Z ]], { W, r } ]
					,
					{ X, r },
					{ Y, r }, 
					{ Z, r }
				], 
				2
			],
			{r, r, r}
		]
	]



(* ::Subsection:: *)
(*Testing*)


(* ::Input:: *)
(*ZestedFusionRings @ FRL[[8]]*)
(*ZestedFusionRings2 @ FRL[[8]]*)


(* ::Section:: *)
(*TRY TO USE NON-UNIVERSAL GRADING: MAYBE THEN WE GET PALCOUX'S EXAMPLES OF RANK 6*)


(* ::Input:: *)
(*zestings1 = *)
(*Quiet@*)
(*QuietEcho @ *)
(*Map[ *)
(*ReplaceByKnownRing ,*)
(*Monitor[*)
(*Table[   ZestedFusionRings[ FRL[[i]] ],{ i, 352 } ],*)
(*{ ProgressIndicator[i/352],i }*)
(*],*)
(*{2} *)
(*];*)


(* ::Input:: *)
(*zestingsNew = *)
(*Quiet@*)
(*QuietEcho @ *)
(*Map[ *)
(*ReplaceByKnownRing ,*)
(*Monitor[*)
(*Table[   ZestedFusionRingsNew[ FRL[[i]] ],{ i, 352 } ], *)
(*{ ProgressIndicator[i/352],i }*)
(*],*)
(*{2} *)
(*];*)


(* ::Input:: *)
(*nonTrivialZestings = *)
(*DeleteCases[*)
(*DeleteDuplicatesBy[Sort] @ *)
(*Cases[ zestings1, r_ /; Length[r] > 1 ],*)
(*l_List/;MemberQ[\!\(\**)
(*TagBox[*)
(*StyleBox[*)
(*RowBox[{"FusionRing", "[", "Null", "]"}],*)
(*ShowSpecialCharacters->False,*)
(*ShowStringCharacters->True,*)
(*NumberMarks->True],*)
(*FullForm]\)]@l*)
(*];*)


(* ::Input:: *)
(*GroupBy[ nonTrivialZestings, Rank @* First ][8]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*Needs[ "WikiTools`","~/Projects/anyonwiki.github.io/code/Mathematica/WikiTools.wl" ]*)


(* ::Input:: *)
(*fcToTexString[ {a_,b_,c_,d_} ]:=*)
(*	"$ \\text{FR}^{" <> ToString[a] <> "," <> ToString[b] <> "," <> ToString[c] <> "}_{" <> ToString[d] <> "} $";*)


(* ::Input:: *)
(*ToTeXRow=*)
(*FixLatex[StringReplace[*)
(*(StringDrop[#,-1]&)@*)
(*(StringDrop[#,1]&)@ToString[ *)
(*{ *)
(*ToString@Rank @First@#, *)
(*ToString@N@FPDim@First@#,*)
(*(*ToString[("$ "<>fcToTexString[#]<>" $"&)@*FC/@#]//StringReplace[", "->"|"],*)*)
(*((StringDrop[#,-1]&)@*)
(*(StringDrop[#,1]&)@ToString[wikiName/@#]//StringReplace[{", "->"|"}])*)
(*}],*)
(*{", "->" & ","|"->"\\leftrightarrow"}*)
(*]]<>" \\\\\n"&;*)
(**)
(*FixLatex[str_String] := *)
(*	str //*)
(*	StringReplace["\\unicode{22ca}" -> "\\rtimes "] //*)
(*	StringReplace[Shortest["\\text{$\\times $"~~x__~~"}"] :> "\\times \\text{"<>x<>"}" ] //*)
(*	StringReplace[Shortest["\\left.\\text{"~~x__~~"}"~~y__~~"\\right)"] :>"\\text{"<>x<>"} "<>y<>")" ] //*)
(*	StringReplace[Shortest["\\text{"~~x__~~"$\\times $"~~y__~~"}"] :> "\\text{"<>x<>"} \\times \\text{" <> y <> "}"] //*)
(*	StringReplace[Shortest["\\text{"~~P___~~"SU(2}"] :> "\\text{"<>P<>"SU}(2" ]//*)
(*	StringReplace[ "\\text{}" ->""] //*)
(*	StringReplace["Rep(}" -> "Rep}("] //*)
(*	StringReplace["TY(}" -> "TY}("] //*)
(*	StringReplace["HI(}" -> "HI}("] //*)
(*	StringReplace["Adj(}" -> "Adj}("];*)


(* ::Input:: *)
(*ToTeXRow/@nonTrivialZestings[[;;1]]*)


(* ::Input:: *)
(*StringJoin@@(ToTeXRow/@nonTrivialZestings)*)


(* ::Input:: *)
(*Length[zestings1]*)


(* ::Input:: *)
(*TableForm[Table[Flatten@{ i,zestings1[[i]]},{i,Length@ zestings1}]]*)


(* ::Input:: *)
(*TableForm[zestings]*)
