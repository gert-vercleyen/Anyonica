Package["Anyonica`"]

PackageExport["SparseIntegerUpperTriangularDecomp"]

Options[ SparesIntegerUpperTriangularDecomp ] = 
  { "TransformationMatrices" -> "Inverse" }

SparseIntegerUpperTriangularDecomp[ sparseMat_ ] := 
	Block[{ sortVal, permVec, u, H, m, n, cMatchQ, rMatchQ, ar, pivot, 
		rowIndices, \[Beta], p2, \[Sigma], \[Tau], val, pVal, sparseID, swapMatrix,
		pivotIndices, newPivotIndex, sm, uInv, bezout, b, extendedGCD
		},

    Switch[ OptionValue["TransformationMatrices"],
      "Inverse",
        uQ = False; uInvQ = True,
      "Regular",
        uQ = True; uInvQ = False, 
      "All", 
        uQ = True; uInvQ = True,
      "None",
        uQ = False; uInvQ = False 
    ];

    (* Memoize the extendedGCD for extra performance *)
    extendedGCD[ a_, b_ ] :=
      extendedGCD[ a, b ] = 
      Block[ { x, y, u, v, aa, bb, q, r, m, n },
        { x, y, u, v, aa, bb } = { 0, 1, 1, 0, a, b }; 
        While[ aa != 0,
          { q, r } = QuotientRemainder[ bb, aa ];
          { m, n } = { x - u q, y - v q };
          { bb, aa, x, y, u, v } = { aa, r, u, v, m, n } 
        ];
        { bb, x, y }
      ];
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
		
		(* Possibly create the transition matrices *)
    If[ uQ, u = SparseArray[IdentityMatrix[ m ]][[ InversePermutation[permVec] ]] ];

    If[ uInvQ, uInv = SparseArray[IdentityMatrix[ m ]][[permVec]] ];
		
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
          If[ uQ, u = u . sm ];
					If[ uInvQ, uInv = sm . uInv ];
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

					{ \[Beta], \[Sigma], \[Tau] } = 
						extendedGCD[ pVal , val ];

					b = bezout[ p1, i, { \[Sigma], \[Tau], val/\[Beta], pVal/\[Beta] } ]; 					
          If[ uQ, u = u . b ];
					If[ uInvQ, uInv = b . uInv];
					H = b . H;

					,
					{ i, rowIndices }
				]
			]
			,
			{ p1, m }
		];

    Switch[ { uQ, uInvQ }, 
      { True, True }, { u, uInv, H },
      { True, False }, { u, H },
      { False, True }, { uInv, H },
      { False, False }, { H }
    ]
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