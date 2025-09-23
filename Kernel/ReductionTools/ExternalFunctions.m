(* Some functions from mathematica are outdated or non-performant 
    Here we define improved versions by relying on Julia and the
    Oscar package
 *)

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
		
		result = juliaHNFWithTransform[ session, sparseMat ];
		
		DeleteObject[session];
		
		result
	];

JuliaHNFWithTransform[ session_, sparseMat_SparseArray ] := 
	Module[ 
		{ dims, arg, hnfWithTransform },
		
		ExternalEvaluate[ session, "using Oscar"];
		
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

		hnfWithTransform  = 
			ExternalFunction[ 
				session,
				"
				function myhnf(arrayind)
					sm = sparse_matrix(ZZ)
					for i in 1:first(size(arrayind))
						push!( sm, sparse_row(ZZ, arrayind[i][1,:], arrayind[i][2,:]) )
					end
					map( Matrix, map.( Int, hnf_kb_with_transform( matrix(sm) ) ) )
				end
				"
		  ];
		  
		SparseArray /@ hnfWithTransform[arg]
	]

PackageExport["JuliaHNF"]

JuliaHNF::usage = 
  "JuliaHNF[ sparseMat ] returns the Hermite normal form "<>
  "of the sparse array sparseMat by using the OSCAR package for Julia.\n"<>
  "JuliaHNF[ ses, sparseMat ] uses the Julia session ses to "<>
  "compute the Hermite normal form of the sparse array sparseMat by using"<>
  " the OSCAR package for Julia."

JuliaHNF[ sparseMat_ ] := 
	Module[{ session, result }, 
		session = StartExternalSession["Julia"];
		
		result = juliaHNF[ session, sparseMat ];
		
		DeleteObject[session];
		
		result
	];

JuliaHNF[ session_, sparseMat_ ] := 
	Module[ 
		{ arg, hnf },
		
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

		hnf  = 
			ExternalFunction[ 
				session,
				"
				function myhnf(arrayind)
					sm = sparse_matrix(ZZ)
					for i in 1:first(size(arrayind))
						push!( sm, sparse_row(ZZ, arrayind[i][1,:], arrayind[i][2,:]) )
					end
					map( Int, Matrix(hnf(sm,truncate=true)) )
				end
				"
		  ];
		  
		SparseArray @ hnf[arg]
	];