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
				"IncludeOnly" -> If[ !BraidedQ[c1], { "FSymbols" }, { "FSymbols", "RSymbols" } ]
			];

		(* Invariants from pivotal structure *)
		Join[
			invariants,
			Array[ \[ScriptD], Rank[c1] ]
		]
	];

PackageExport["EquivalentFusionCategoriesQ"]

Options[EquivalentFusionCategoriesQ] = 
	{ 
		"PreEqualCheck" -> Identity,
		"UseFormalCode" -> True
	};

EquivalentFusionCategoriesQ[ c1_FusionCategory, c2_FusionCategory, OptionsPattern[] ] :=
Catch[
	Module[{fra, invariants1, invariants2, qDims1, qDims2,fSymb1,fSymb2,rSymb1,rSymb2,rules1,rules2,test,eqQdims},
	If[
		OptionValue["UseFormalCode"],
		CheckFormalCode[c1,c2];
	];
	CheckFusionRing[c1,c2];
	check = OptionValue["PreEqualCheck"];

	qDims1 = QuantumDimensions[c1]; qDims2 = QuantumDimensions[c2];

	If[ check[ Sort[qDims1] ] != check[ Sort[ qDims2 ] ], Throw @ False ];

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
	If[ 
		BraidedQ[c1] && BraidedQ[c2],
		rSymb1 = RSymbols[c1]; rSymb2 = RSymbols[c2],
		rSymb1 = rSymb2 = {}
	];

		rules1 = Join[ fSymb1, rSymb1, qDims1 ]; rules2 = Dispatch[ Join[ fSymb2, rSymb2, qDims2 ] ];

	Do[
		If[
			ConsistentQ[ 
				Thread[ 
					(invariants1/.Dispatch[PermuteSymbols[rules1,a]]) == (invariants2/.rules2) 
				]/.{ False -> {False}, True -> {True} }, 
				TrueQ @* test 
			],  
			Throw @ True 
		]
		, { a, fra }
	];

		Throw @ False
	]
];

GaugeInvariants::nonbraidedcat = 
  "Gauge invariants for braided category requested but category is not braided.\n"<>
  " Use option \"IncludeOnly\" -> \"FSymbols\" for gauge invariants with only F-symbols.";

GaugeInvariants[ cat_FusionCategory, opts:OptionsPattern[] ] := 
  Module[
    { io, zeros, gi, getSymbols, values },
    io = OptionValue["IncludeOnly"];

    If[ 
      (MemberQ["RSymbols"] @ io) && !BraidedQ[cat], 
      Message[GaugeInvariants::nonbraidedcat]; Return @ $Failed  
    ];

    zeros = Keys @ Select[ FSymbols @ cat, #[[2]] === 0& ];

    gi = AddOptions[opts][GaugeInvariants][ FusionRing @ cat, "Zeros" -> zeros ];

		getSymbols = 
			Comap[ 
				{ 
					If[ MemberQ["FSymbols"] @ io, FSymbols, Splice[{}] ], 
					If[ MemberQ["RSymbols"] @ io, RSymbols, Splice[{}] ], 
					If[ MemberQ["PSymbols"] @ io, PSymbols, Splice[{}] ] 
				}
			];  

		values = Dispatch[ Join @@ getSymbols @ cat ];	

    Thread[ gi -> ( gi/.values ) ]
  ];

PackageExport["FusionCategoryAutomorphisms"]

FusionCategoryAutomorphisms::usage =
  "FusionCategoryAutomorphisms[cat,u] returns the automorphisms of the fusion category cat in the symbol u.";

Options[FusionCategoryAutomorphisms] :=
	Union[
		Options[AutomorphismEquations], 
    Options[FusionRingAutomorphisms],
		Options[ReduceBinomialSystem],
		Options[SolveBinomialSystem],
    { "Permutations" -> Missing[] }
	];

FusionCategoryAutomorphisms[ cat_FusionCategory, u_, opts:OptionsPattern[] ] :=
	Module[
		{ ring, FRAuth, catAuth, g, eqnsPerPerm, vars, symmetries, solutions, 
		z, trivialSymbols, structConst, reducedSymmetries, autEqns, fixedSymbols, AddKnowns,
		reducedSystem }, 
		ring = FusionRing @ cat; 
		
		If[ Rank @ ring === 1, Return @ { {1}, { u[1,1,1] -> 1 } } ];

		FRAuth = 
			If[ 
				OptionValue["Permutations"] =!= Missing[],
				OptionValue["Permutations"],
				AddOptions[opts][FusionRingAutomorphisms][ ring ]
			];
		
		structConst = NZSC @ cat;

		vars = u @@@ structConst;
		
		symmetries = 
			<|
				"Transforms" -> ( u[##] -> ( g[#1] g[#2] / g[#3] ) u[##] & ) @@@ structConst, 
				"Symbols" -> {g}
			|>;
		
		trivialSymbols = Thread[ u @@@ Cases[ structConst, { 1, __ } | { _, 1, _ }] -> 1 ];
		
		(* Only symmetries that keep the trivial u symbols invariant are allowed *)
		reducedSymmetries = 
      RestrictMultiplicativeSymmetries[ symmetries, Keys @ trivialSymbols, g ];
    
    (* Break the symmetry *)
    fixedSymbols = Last @ BreakMultiplicativeSymmetry[ reducedSymmetries ];
			
		(* Function that adds trivial data and fixed symbols to solution *)
		AddKnowns[ sol_ ] := Sort @ Join[ trivialSymbols, fixedSymbols, sol ];
			
		solutions = 
      DeleteDuplicates[ #, GSEQ[ MultiplicativeGaugeMatrix[symmetries] ] ]& @*
      ReverseSortBy[Count[1] @* Values] /@ (* Prefer solutions with highest number of 1's *)
			Table[
				autEqns = 
          AddOptions[opts][AutomorphismEquations][ cat, perm, u ] //
          ReplaceAll[ Dispatch @ Join[ trivialSymbols, fixedSymbols ] ] //
          TEL;

        If[
          MemberQ[False] @ autEqns,
          { },

          reducedSystem = 
            ( # == 0 ) & /@
            AddOptions[opts][ReduceBinomialSystem][ autEqns, GetVariables[ autEqns, u ] ]["Polynomials"];
					
					Which[ 
						MemberQ[False] @ reducedSystem,
							{},
						reducedSystem === {},
							AddKnowns /@ { {} },
						True,
							AddKnowns /@ 
							AddOptions[opts][SolveBinomialSystem][ 
								reducedSystem, 
								GetVariables[ reducedSystem, u ], 
								z, 
								"NonSingular" -> True 
							]
					]
        ]
        ,	
				{ perm, FRAuth } 
			];
    
		Flatten[
			Table[ { FRAuth[[i]], # }& /@ solutions[[i]], { i, Length @ FRAuth }  ], 
			1
		]
	];

PackageExport["FCA"]

FCA::usage =
  "Shorthand for FusionCategoryAutomorphisms.";

FCA = FusionCategoryAutomorphisms;

	
Options[AutomorphismEquations] = 
	{ "Type" -> { "Braided", "Pivotal" } };

AutomorphismEquations[ cat_, perm_, g_, opts:OptionsPattern[] ] := 
	Module[{ring, permute, transform, symbols, nbq },
		nbq = !BraidedQ[cat] || FreeQ["Braided"] @ OptionValue["Type"];
		
		ring = FusionRing @ cat;
		
		permute = 
			If[
				perm === Range @ Rank @ cat,
				Identity,
				ReplaceAll[
					If[ nbq, Identity, Append[ R[a_,b_,c_] :> R[ perm[[a]], perm[[b]], perm[[c]] ] ] ] @ 
					{ 
						g[a_,b_,c_] :> g[ perm[[a]], perm[[b]], perm[[c]] ],
						F[a_,b_,c_,d_,e_,f_] :> F[ perm[[a]], perm[[b]], perm[[c]], perm[[d]], perm[[e]], perm[[f]] ]
					}
				]
			];
		
		transform = 
			ReplaceAll[ 
				If[ nbq, Identity, Append[ R[i__] :> permute @  GaugeTransform[g] @ R[i] ] ] @
				{ 
					g[i__] :> permute @ g[i], 
					F[i__] :> permute @ GaugeTransform[g] @ F[i]
				}
			];
			
		symbols = 
			Join[
				FSymbols @ ring,
				If[ nbq, {}, RSymbols @ ring ]
			];
			
		
		TrimEquationList[
			Thread[ symbols == transform @ symbols ]/.
			Dispatch[ Join[ FSymbols @ cat, If[ nbq, {}, RSymbols @ cat ] ] ]
		]
	]



cayleyTable[ autData_, u_ ] := 
	Module[{ groupedData, permMT, g, permute, autProduct, symmetries, prod, equivalentQ },
		(* Group autos by permutation *)
		(*
		groupedData = GroupBy[ autData, First ];
		
		permMT = 
			Transpose @ (* We take the left action as default *)
			With[ 
				{ perms = Keys @ groupedData }, 
				Table[ 
					First @ 
					FirstPosition[ perms, PermutationProduct[ p1, p2 ] ],
					{ p1, perms },
					{ p2, perms }
				]
			];
			*)
			
		permute[p_][ u[a_,b_,c_] ] := u[ p[[a]], p[[b]], p[[c]] ];
		
		autProduct[ a1_, a2_ ] :=
			Module[ { 
				ip = InversePermutation @ First @ a1,
				u1 = Last @ a1,
				u2 = Last @ a2,
				a, b, c
				},
				{
					PermutationProduct[ First @ a2, First @ a1 ], 
					Table[
						{ a, b, c } = labels;
						u[ a, b, c ] -> ( u[ a, b, c ] /. u1 ) * ( permute[ip][ u[ a, b, c ] ] /. u2 ),
						{ labels, List @@@ Keys @ u1 }
					]
				}
			];
		
		(* Define equivalence between two group elements: 
			 - permutations must match 
			 - gauge transforms must only be equivalent 
		*)
		symmetries =
			With[ { lb = List @@@ Keys @ autData[[1,2]] },
				<|
					"Transforms" -> ( (u[##] -> u[##] * (g[#1] g[#2]/g[#3]))& @@@ lb ),
					"Symbols" -> {g}
				|>
			];
		
		equivalentQ[ a1_, a2_ ] := 
			First[a1] == First[a2] && 
			GSEQ[symmetries][ Last @ a1, Last @ a2 ];
			
		Table[ 
			prod = autProduct[ a1, a2 ];
			First @ 
			FirstPosition[ autData, el_ /; equivalentQ[ el, prod ], None, {1}, Heads -> False ]
			,
			{ a1, autData }, 
			{ a2, autData }
		]
		
	]
