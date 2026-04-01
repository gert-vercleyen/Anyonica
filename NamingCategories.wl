(* ::Package:: *)

<<Anyonica`


integerUpperTriangularDecomp[ sparseMat_ ] := 
	Module[{ sortVal, permVec, u, H, m, n, cMatchQ, rMatchQ, ar, pivot, 
		rowIndices, x, p2, y, z, val, pVal, sparseID, swapMatrix,
		pivotIndices, newPivotIndex, sm, uInv, bezout, b
		},
		(* 
			 Reverse sorting the matrix lexicographically using a custom sort:
			 rows are sorted such that rows that start with a 0 end up below those
			 with a nonzero number and the ones with the lowest numbers are on top:
			 this way we increase the chance of having a pivot equal to 1 or -1 
		*)
		
		SetAttributes[ sortVal, Listable ];
		sortVal[ n_ ] := If[ n === 0, 0, 1/Abs[n] ];
				
		permVec = (* Create the permutation vector that applies the new order *)
			Sow @ ReverseSortBy[ Range @ First @ Dimensions @ sparseMat, sortVal[ Normal @ sparseMat[[#]]]& ];
		
		(* Create the sorted sparse matrix *)
		H = sparseMat[[permVec]];
		
		{ m, n } = Dimensions @ H;
		
		(* Create the transition matrix *)
		(*u = SparseArray[IdentityMatrix[ m ]][[InversePermutation[permVec]]];*)
		uInv = SparseArray[IdentityMatrix[ m ]][[permVec]]; 
		
		(* FUNCTIONS THAT WILL BE USEFUL *)
		
		cMatchQ[ k_Integer ][{i_,j_} -> r_] := (* Returns true if column of array rule matches k *)
			j === k;
		
		sparseID = (* m x m sparse identity matrix *)
			IdentityMatrix[ m, TargetStructure -> "Sparse" ];
		
		bezout[ i1_, i2_, { \[Sigma]_, \[Tau]_, \[Gamma]_, \[Alpha]_ } ] := 
			Block[
				{ id = sparseID },
				id[[i1,i1]] = \[Sigma];  id[[i1,i2]] = \[Tau];
				id[[i2,i1]] = -\[Gamma]; id[[i2,i2]] = \[Alpha];
				id 
			];
		(*
		bezout1Inv[ i1_, i2_, { \[Sigma]_, \[Tau]_, \[Gamma]_, \[Alpha]_ } ] := 
			Block[
				{ id = sparseID },
				id[[i1,i1]] = \[Alpha]; id[[i1,i2]] = -\[Tau];
				id[[i2,i1]] = \[Gamma]; id[[i2,i2]] = \[Sigma];
				id 
			];*)
		
		swapMatrix[ i_, j_ ] := (* Matrix that swaps row i with row j when acting on left *)
			Block[ { id = sparseID }, 
				id[[ i, i ]] = id[[ j, j ]] = 0;
				id[[ i, j ]] = id[[ j, i ]] = 1;
				id
			];
			
		(* START OF ALGORITHM *)
		Do[
			ar = (* Array rules that define sparse matrix. We remove anything that points to 0 *) 
				DeleteCases[ ArrayRules @ H, HoldPattern[ { _, _ } -> 0 ] ];
			
			p2 = (* pivot column index = first column with non-zero element *) 
				Min[ Last /@ Cases[ Keys @ ar, { p1, _ } ] ];
			
			If[ (* Multiple adjacent zeros in this row were created during last step *)
				p2 > p1
				, (* THEN: swap row with other row with highest index and column pivot index p1 *)
				pivotIndices = 
					Cases[ 
						First /@ Keys @ Select[ ar, cMatchQ[p1] ],
						p_ /; p > p1
					];
				If[ (* Whole column is 0 *)
					pivotIndices === {}
					, (* THEN: don't do anything => set iterator for next steps to empty list *)
					rowIndices = {}
					, (* ELSE swap current row with last of the rows that has nonzero pivot  *)
					sm = Sow @ swapMatrix[ p1, Max @ pivotIndices ];
					(*u = u . sm;*)
					uInv = sm . uInv;
					H = sm . H;
					ar = (* update array rules that define sparse matrix *) 
						DeleteCases[ ArrayRules @ H, HoldPattern[ { _, _ } -> 0 ] ];
							 (* update the column pivot position*)
					p2 = p1;
				]
			];

			rowIndices = (* Indices of nonzero el below pivot *) 
				Cases[ 
					First /@ Keys @ Select[  ar, cMatchQ[p2] ],
					ind_ /; ind > p1
				];
			
			If[ (* There are at least two nonzero elements in the same column *) 
				Length @ rowIndices > 0
				, 
				Do[
					{ pVal, val } = H[[ { p1, i }, p2 ]]; 

					{ x, y, z } = 
						extendedGCD[ pVal , val ];

					b = bezout[ p1, i, { y, z, val/x, pVal/x } ]; 					
					uInv = b . uInv;
					H = b . H;

					,
					{ i, rowIndices }
				]
			]
			,
			{ p1, m }
		];
		
		{ uInv, H }
	];
	
extendedGCD[ a_, b_ ] :=
	Block[ { x, y, u, v, aa, bb, q, r, m, n },
		{ x, y, u, v, aa, bb } = { 0, 1, 1, 0, a, b }; 
		While[ aa != 0,
			{ q, r } = QuotientRemainder[ bb, aa ];
			{ m, n } = { x - u q, y - v q };
			{ bb, aa, x, y, u, v } = { aa, r, u, v, m, n } 
		];
		{ bb, x, y }
	];


intNullSpace[ sparseMat_ ] :=
	Module[ { r, h, rank, sortOrder },
		{ r, h } = integerUpperTriangularDecomp @ Transpose @ sparseMat;
		rank = 
			Length @ 
			DeleteDuplicatesBy[Last] @ (* Tally might be faster *)
			Keys @ 
			DeleteCases[ _ -> 0 ] @ 
			ArrayRules[h];

		Normal @ r[[rank+1;;]]
	];


Options[gaugeMatrix] = 
  {
    "IncludeOnly" -> "All",
    "Zeros" -> {}
  };
gaugeMatrix[ ring_, OptionsPattern[] ] := 
	  With[{ io = OptionValue["IncludeOnly"] },

    If[ Rank[ring] == 1, Return @ If[ io =!= All, { IdentityMatrix[1], 1 }, { IdentityMatrix[2], 2 } ] ];

    Module[{ symbols, g, sym, m, monomial, powers, d, v, r, sortf, zeros, zeroPos, symbolsWithZeros },
      zeros =
        OptionValue["Zeros"];

      symbolsWithZeros =
        Switch[ io,
          "All",          Join[ FSymbols @ ring, RSymbols @ ring ],
          "FSymbols",     FSymbols @ ring,
          "FAndRSymbols", Join[ FSymbols @ring, RSymbols @ ring ],
          _,              Message[ GaugeSplitTransform::invalidoptionincludeonly, io ]; Abort[]
        ];

      zeroPos = 
        Flatten @ 
        Position[ symbolsWithZeros, x_ /; MemberQ[x] @ zeros, Heads -> False ];

      symbols =
        Complement[ symbolsWithZeros, zeros ]; 

      sym =
        GaugeSymmetries[ symbols, g ];

      monomial =
        PowerExpand[
          PowerDot[ symbols, Array[ m, Length @ symbols ] ] //
          ReplaceAll[ Dispatch @ sym["Transforms"] ] //
          ReplaceAll[ Dispatch @ Thread[symbols -> 1] ]
        ];

      powers =
        Expand @
        Cases[ monomial, Power[ g[__], p_. ] :> p ];
        
      SparseArray[ powerToRow[ m, Length @ symbols ] /@ powers ]
		]
];

powerToRow[ s_, n_ ][ pow_ ] :=
  Normal @ SparseArray[ Cases[ pow, i_. * s[j_] :> { j } -> i ], {n} ];
  
PowerDot[a_,b_] :=
	If[
		MatchQ[ a, { 1 .. } ],
		ConstantArray[ 1, Length[b] ],
		Inner[ Power, a, Transpose[b], Times ]
	];


FInvariants[ cat_FusionCategory ] := 
	Module[ { r, fvals, fs, zeroFs, gM, nullSpace, gaugeInvariants, automorphisms },
		r = FusionRing @ cat;
		fvals = FSymbols @ cat; 
		fs = FSymbols @ r;
		
		zeroFs = Keys @ Cases[ fvals, HoldPattern[ _ -> 0 ] ];
	
		gM = 
			gaugeMatrix[ 
				r, 
				"Zeros" -> zeroFs,
				"IncludeOnly" -> "FSymbols"
			];
			
		nullSpace = intNullSpace @ gM;
		
		gaugeInvariants = 
			Join[ 
				zeroFs, 
				PowerDot[ Complement[ fs, zeroFs ], nullSpace ] 
			];
		
		(* Create full invariant *)
		automorphisms = 
			PermutationCycles /@ 
			FusionRingAutomorphisms @ 
			FusionRing @ 
			cat;
		
		Table[ 
			ReplaceAll[ \[ScriptCapitalF][i__]:>PermutationReplace[ \[ScriptCapitalF][i], s ] ] @ 
			gaugeInvariants,
			{ s, automorphisms }
		]
	];

RInvariants[ cat_FusionCategory ] :=	
	Module[
		{ r, fvals, fs, rs, zeroFs, gM, nullSpace, gaugeInvariants, automorphisms },
		r = FusionRing @ cat;
		fvals = FSymbols @ cat;
		fs = FSymbols @ r;
		rs = RSymbols @ r;
		
		zeroFs = Keys @ Cases[ fvals, HoldPattern[ _ -> 0 ] ];
		
		gM = 
			gaugeMatrix[ 
				r,  
				"IncludeOnly" -> "FAndRSymbols",
				"Zeros" -> zeroFs
			];
			
		nullSpace = intNullSpace @ gM;
		
		gaugeInvariants = 
			Join[ zeroFs, PowerDot[ Join[ Complement[ fs, zeroFs], rs ], nullSpace ] ];	
				
		(* Create full invariant *)
		automorphisms = 
			PermutationCycles /@ 
			FusionRingAutomorphisms @ 
			FusionRing @ 
			cat;
		
		Table[ 
			ReplaceAll[ 
				{ 
					\[ScriptCapitalR][i__]:>PermutationReplace[ \[ScriptCapitalR][i], s ],  
					\[ScriptCapitalF][i__]:>PermutationReplace[ \[ScriptCapitalF][i], s ] 
				} 
			] @ 
			gaugeInvariants,
			{ s, automorphisms }
		]
	];
	
PInvariants[ cat_FusionCategory ] := 
	Module[{r,fvals,fs, rs, zeroFs, gM, nullSpace, gaugeInvariants, automorphisms},
		r = FusionRing @ cat;
		fvals = FSymbols @ cat; 
		fs = FSymbols @ r;
		rs = RSymbols @ r;
		
		zeroFs = Keys @ Cases[ fvals, HoldPattern[ _ -> 0 ] ];
	
		gM = 
			gaugeMatrix[ 
				r, 
				"Zeros" -> zeroFs,
				"IncludeOnly" -> "FAndRSymbols"
			];
			
		nullSpace = intNullSpace @ gM;
		
		gaugeInvariants = 
			Join[ 
				zeroFs, 
				PowerDot[ Join[ Complement[ fs, zeroFs ], rs ], nullSpace ],
				Array[ \[ScriptD], Rank @ cat ] (* These are the dimensions of the cat *)
			];
		
		
		(* Create full invariant *)
		automorphisms = 
			PermutationCycles /@ 
			FusionRingAutomorphisms @ 
			FusionRing @ 
			cat;
		
		Table[ 
			ReplaceAll[ 
				{ 
					\[ScriptD][i__]:>PermutationReplace[ \[ScriptD][i], s ],
					\[ScriptCapitalR][i__]:>PermutationReplace[ \[ScriptCapitalR][i], s ],
					\[ScriptCapitalF][i__]:>PermutationReplace[ \[ScriptCapitalF][i], s ] 
				}
			] @ 
			gaugeInvariants,
			{ s, automorphisms }
		]
	]
	  
SetAttributes[ numberWeight, Listable ]
numberWeight[z_] :=
	Block[ { $MaxExtraPrecision = Infinity, nz = InfN[ z, 2048 ] },
		{ InfN[-Abs[z], 2048], InfN[ If[ Arg[ nz ] < 0, 2 Pi + Arg[nz], Arg[nz] ], 2048 ] }
	];

(* Weight for group of cats with same F-symbols *)
FWeight[invariants_, cats_] := 
	{ 
		MemberQ[True] @ ( BraidedQ /@ cats ), 
		MemberQ[True] @ ( UnitaryQ /@ cats ), 
		MemberQ[True] @ ( SphericalQ /@ cats), 
		MemberQ[True] @ ( RibbonQ /@ cats ), 
		-Count[ cats, c_/; ModularQ[c] ],
		NumericalSort @ numberWeight[ invariants/.Dispatch[ FSymbols @ First @ cats ] ]
	}/.{ True -> -1, False -> 1 };

(* Weight for group of cats with same F-symbols and same R-symbols *)
RWeight[ invariants_, cats_ ] :=
	If[
		MemberQ[False] @ (BraidedQ /@ cats ),
		Infinity, 
		{
			MemberQ[True] @ ( SphericalQ /@ cats), 
			MemberQ[True] @ ( RibbonQ /@ cats ), 
	  	-Count[ cats, c_/; ModularQ[c] ],
			NumericalSort @ numberWeight[ invariants/.Dispatch[ RSymbols @ First @ cats ] ]
		}/.{ True -> -1, False -> 1 }
	];
	
(* Weight for cat based on pivotal symbols *)
PWeight[ invariants_, cat_ ] := 
	{
		SphericalQ[cat], 
		RibbonQ[cat], 
		ModularQ[cat],
		NumericalSort @ numberWeight[ invariants/.Dispatch[ QuantumDimensions @ cat ] ]
	}/.{ True -> -1, False -> 1 };


CatWithStandardRing[ cat_FusionCategory ] :=
	With[ { 
		perm = 
			WhichPermutationToStandardRep @ 
			FusionRing @ 
			cat 
		},
		PermutedFusionCategory[ cat, perm ]
	];

WhichPermutationToStandardRep[ r_FusionRing ] :=
WhichPermutationToStandardRep[ r ] =
    Module[{ sRing, qds,permutations, pmtaps},
      sRing =
        SortedRing[ r, "SortBy" -> "Selfdual-Conjugates" ];
	
      qds =
        Rest @ FPDims[ sRing ]; (* 1 should be left alone *)

      permutations =
        Prepend[1] @* Flatten /@
        Tuples[ Permutations /@ (GatherBy[ Range[ Rank[r] - 1 ], qds[[#]]& ] + 1) ];
						
			pmtaps = 
				DeleteDuplicatesBy[ (* Some permutations result in same mult tabs *)
				  { PermuteMultTab[ MT @ sRing, # ], # }& /@ permutations,
				  First
				];
				 
        Last @ First @ MaximalBy[ pmtaps, multTabCode[ #[[1]], Mult @ r ]& ]
    ];
    
PermuteMultTab =
  Compile[
    {{m,_Integer,3},{perm,_Integer,1}},
    Table[
      m[[ perm[[a]], perm[[b]], perm[[c]] ]],
      {a,Length[m]}, {b,Length[m]}, {c,Length[m]} ],
    {{permTab,_Integer,3}},
    "RuntimeOptions" -> "Speed"
  ];
  
multTabCode[ mat_List, mult_ ] :=
  FromDigits[ Flatten[ mat ], mult + 1 ];

multTabCode[ ring_FusionRing ] :=
  multTabCode[ MT @ ring, Mult @ ring ];


groupAndSortCats[ring_FusionRing] := 
	groupAndSortCats[ FusionCategories @ ring ];
groupAndSortCats[ cats_List ] :=
Block[ { stdCats, groupedBy0, firstGIs, gaugeInvariants, fi, ri, pi, weights, sotredByFs, catGITuples,
	groupedByF, ind, fSymb,grpdTuplesF,tplsFOrder,tplsROrder,tplPOrder,grpedTuplesFR,groupedAndSortedByF,groupedAndSortedByFR,groupedAndSortedByFRP,rSymb,tplPWeight },
	If[ cats === {}, Return @ Missing[] ];
	If[ Length @ cats === 1, Return @ { { cats } } ];
	
	(* Permute the cats such that their fusion ring has 
	   a multiplication matrix in standard form *)
	
	stdCats = CatWithStandardRing /@ cats;
	
	(* Group categories by pattern of 0 values for the F-symbols *)
	groupedBy0 = GatherBy[ stdCats, Position[ #,  _ -> 0, 1 ]& @* FSymbols ];
	
	(* Construct gauge invariants for each group of symbols. *)

	(* The invariants only depend on the pattern of 0's: we calculate
		 them only for the first cat per pattern*)
	firstGIs = 
		Comap[{ FInvariants, RInvariants, PInvariants }] @*
		First /@  
		groupedBy0;
	
	(* Create tuples of (1) position of cat in <cats>, (2) position of
		 corresponding gauge invariants in <firstGIs> *)
	ind = Flatten[ MapIndexed[ Reverse[#2]&, groupedBy0, {2} ], 1 ];
	catGITuples = 	
		Table[ 
			{ i, ind[[i,2]] }
			,{ i,Length @ cats }
		];
	
	(* First we group the positions of the cats via the FSymbols
	   and we sort them using the FWeights *)
	  
	(* Group Cats with same F-symbols together. These are the ones with
		 the same 5 th index for the formal code  *)
	fSymb[ tuple_ ] := FC[ stdCats[[ First @ tuple ]] ][[5]];
	grpdTuplesF =
		GatherBy[ catGITuples, fSymb ];
	
	(* Sort these groups of cats using the F-weight *)
	tplsFOrder[ tuples1_, tuples2_ ] :=
		Module[ { 
			invariants1 = firstGIs[[ tuples1[[1,2]], 1 ]], 
			cts1 = stdCats[[tuples1[[;;,1]]]],
			invariants2 = firstGIs[[ tuples2[[1,2]], 1 ]], 
			cts2 = stdCats[[tuples2[[;;,1]]]],
			order
			},
			order = 
				NumericalOrder[ 
					FWeight[ invariants1, cts1 ],
					FWeight[ invariants2, cts2 ]
				];
			If[ 
				order === 0,
				Throw["Accuracy not big enough to distinguish pent solutions"]; 
				Abort[],
				order
			]
		];
	
	groupedAndSortedByF = 
		Sort[ grpdTuplesF, tplsFOrder[#1,#2]& ];
	
	rSymb[ tuple_ ] := FC[ stdCats[[ First @ tuple ]] ][[6]]; 
	grpedTuplesFR = 
		Map[ GatherBy[ #, rSymb ]&, groupedAndSortedByF ];
	
	tplsROrder[ tuples1_, tuples2_ ] :=
		Module[ { 
			invariants1 = firstGIs[[ tuples1[[1,2]], 2 ]], 
			cts1 = stdCats[[tuples1[[;;,1]]]],
			invariants2 = firstGIs[[ tuples2[[1,2]], 2 ]], 
			cts2 = stdCats[[tuples2[[;;,1]]]],
			order
			},
	    
			order =  
				NumericalOrder[ 
					RWeight[ invariants1, cts1 ],
					RWeight[ invariants2, cts2 ]
				];
			If[ 
				order === 0,
				Throw["Accuracy not big enough to distinguish hex solutions"]; 
				Abort[],
				order
			]
		];
	
	groupedAndSortedByFR = 
		Map[ Sort[ #, tplsROrder ]&, grpedTuplesFR ];
		
	tplPWeight[ tuple_ ] := 
		With[ { invariants = firstGIs[[ tuple[[2]], 3 ]], cat = stdCats[[ tuple[[1]] ]] },
			PWeight[ invariants, cat ]
		];
		
	tplPOrder[ tuple1_, tuple2_ ] :=
		Module[ { 
			invariants1 = firstGIs[[ tuple1[[2]], 3 ]], 
			cts1 = stdCats[[tuple1[[1]]]],
			invariants2 = firstGIs[[ tuple2[[2]], 3 ]], 
			cts2 = stdCats[[tuple2[[1]]]],
			order
			},
			
			order = 
				NumericalOrder[ 
					PWeight[ invariants1, cts1 ],
					PWeight[ invariants2, cts2 ]
				];
			If[ 
				order === 0,
				Throw["Accuracy not big enough to distinguish piv solutions"]; 
				Abort[],
				order
			]
		];

	groupedAndSortedByFRP =  
		Map[
			Sort[ #, tplPOrder ]&,
			groupedAndSortedByFR,
			{2}
		];
	
	groupedAndSortedByFRP/.{ i_Integer, j_Integer } :> cats[[i]]
]


catRings = 
	Cases[ FRL, ring_/; Mult[ring] == 1 && Rank[ring] < 8 ];

fileNames = FileNames[ All, "/home/gert/Projects/SmallRankFusionCategories/Code/NewGroupingsCats/"];
Monitor[ 
QuietEcho@
Do[ 
	fn = "/home/gert/Projects/SmallRankFusionCategories/Code/NewGroupingsCats/NewGroupingRing"<>ToString[ii]<>".wdx";
	If[
		FreeQ[ fn ] @ fileNames,
		Export[ fn, groupAndSortCats[ FRL[[ii]] ] ] ]
	,
	{ ii, Length @ catRings }
],
	Column[{ ProgressIndicator[ii/Length[catRings]], FRL[[ii]], ii }]
]


groupedCats = 
Table[ 
	Import["/home/gert/Projects/SmallRankFusionCategories/Code/NewGroupingsCats/NewGroupingRing"<>ToString[i]<>".wdx"],
	{ i, Length @ catRings }
];
