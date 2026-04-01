Package["Anyonica`"]

(* Some functions from mathematica are outdated or non-performant 
    Here we define improved versions by relying on Julia and the
    Oscar package
 *)

(* TODO: we need to add columns of 0's at the end of the matrix 
   since the way we create sparse matrices with OSCAR does 
	 not take those last columns into account *)


PackageExport["JuliaHNFWithTransform"]

JuliaHNFWithTransform::usage = 
  "JuliaHNFWithTransform[ sparseMat ] returns the Hermite normal form "<>
  "and its transformation matrix of the sparse array sparseMat by "<>
  "using the OSCAR package for Julia.\n"<>
  "JuliaHNFWithTransform[ ses, sparseMat ] uses the Julia session ses to "<>
  "compute the Hermite normal form and its transformation matrix of the"<>
  " sparse array sparseMat by using the OSCAR package for Julia."

JuliaHNFWithTransform[ sparseMat_SparseArray ] :=
	Module[{ session, result }, 
		session = StartExternalSession["Julia"];
		
		result = JuliaHNFWithTransform[ session, sparseMat ];
		
		DeleteObject[session];
		
		result
	];

JuliaHNFWithTransform[ session_, sparseMat_ ] := 
	Module[ 
		{ dims, arg, hnfWithTransform, u, h },
		
		ExternalEvaluate[ session, "using Oscar"];
		
		dims = Dimensions[ sparseMat ];
			
		arg =
			Transpose /@  
			Values @
				GroupBy[
					Join[ (* Append Identity to mat *)
						ReplaceAll[({i_,j_}->k_):>{i,j,k}] @
						Most[ ArrayRules @ sparseMat ],
						Table[ { i, i + dims[[2]], 1 }, { i, dims[[1]] } ]
					]
					,
					First -> Rest
				];

		hnfWithTransform  = 
			ExternalFunction[ 
				session,
				"
				function myhnf(arrayind)
					sm = sparse_matrix(ZZ)
					for i in 1:first(size(arrayind))
						push!( sm, sparse_row(ZZ, arrayind[i][1,:], arrayind[i][2,:]) )
					end

					hnf!( sm )

					v = []
					for i in 1:first(size(sm))
						for j in 1:last(size(sm))
							if sm[i,j] != 0
								push!(v,[ i, j, Int(sm[i,j]) ])
							end
						end
					end
					v
				end
				"
		  ];
		  
		{ u, h } = (* Set up array rules *)
			BinSplit[ 
				hnfWithTransform[arg], 
				#[[2]] > dims[[2]]& 
			];

		{ 
			SparseArray[ u/.{i_,j_,k_}:>({i,j-dims[[2]]}->k) ],(* shift u to the left *)
			SparseArray[ h/.{i_,j_,k_}:>({i,j}->k), { Max @ h[[;;,1]], dims[[2]] } ]
		} 
		
	];

PackageExport["JuliaHNF"]

JuliaHNF::usage = 
  "JuliaHNF[ sparseMat ] returns the Hermite normal form "<>
  "of the sparse array sparseMat by using the OSCAR package for Julia.\n"<>
  "JuliaHNF[ ses, sparseMat ] uses the Julia session ses to "<>
  "compute the Hermite normal form of the sparse array sparseMat by using"<>
  " the OSCAR package for Julia."

JuliaHNF[ sparseMat_SparseArray ] := 
	Module[{ session, result }, 
		session = StartExternalSession["Julia"];
		
		result = JuliaHNF[ session, sparseMat ];
		
		DeleteObject[session];
		
		result
	];

JuliaHNF[ session_, sparseMat_SparseArray ] := 
	Module[ 
		{ dims, arg, hnf, h, m  },
		
		ExternalEvaluate[session,"using Oscar"];
		
		arg = 
			Transpose /@
			Values @
			GroupBy[
				ReplaceAll[({i_,j_}->k_):>{i,j,k}] @
				Most @ 
				ArrayRules @ 
				sparseMat,
				First -> Rest
			];

		dims = Dimensions @ sparseMat;

		hnf  = 
			ExternalFunction[ 
				session,
				"
				function myhnf(arrayind)
					
					sm = sparse_matrix(ZZ)
					for i in 1:first(size(arrayind))
						push!( sm, sparse_row(ZZ, arrayind[i][1,:], arrayind[i][2,:]) )
					end
					
					sm = hnf(sm,truncate=true)

					v = []
					for i in 1:first(size(sm))
						for j in 1:last(size(sm))
							if sm[i,j] != 0
								push!(v,[ i, j, Int(sm[i,j]) ])
							end
						end
					end
					v
				end
				"
		  ];

		h = hnf[arg];
		m = Max @ h[[;;,1]];
		SparseArray[ h/.{i_,j_,k_} :> ({i,j}->k), { m, dims[[2]] } ]
	];