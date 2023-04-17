(* ::Package:: *)

Package["Anyonica`"]

Options[ SolveModZSpace ] = {
  "OrthogonalTo" -> None,
  "SimplifyBy"   -> Identity,
  "IntegerCheck" -> IntegerQ
};
SolveModZSpace[ mat_?MatrixQ, vec_List, opts:OptionsPattern[] ] :=
  Module[{ ZSpace, u, d, v, r, ld, newVec, modVec, simplify, gaugeMat, intCheck, procID, absTime, result },
    simplify =
      OptionValue["SimplifyBy"];
    gaugeMat =
      OptionValue["OrthogonalTo"];
    intCheck =
      OptionValue["IntegerCheck"];
    procID =
      ToString[Unique[]];
    
    printlog["SMZS:init", {procID,{mat,vec},{opts}}];
    
    { absTime, result } =
    AbsoluteTiming[
      If[
        mat === { {} } || vec === {},
        printlog["Gen:trivial_system", {procID}];
        Return[ { {}, {{}}, {{}} } ]
      ];

      { u, d, v } =
        SmithDecomposition[ mat ];

      printlog["SMZS:decomposition", {procID,{u,d,v}}];

      r =
        Length[
          ld = DeleteCases[0] @ Diagonal @ Normal @ d
        ];

      If[
        r == 0,
        Return[ { {}, {{}}, IdentityMatrix[ Length[vec] ] } ]
      ];

      ZSpace =
        v[[ ;;, ;;r ]] . DiagonalMatrix[ 1 / ld ];

      newVec =
        simplify[ u. vec ];

      modVec =
        Quiet[ (* ReMod often has trouble with deeply nested numbers *)
          Check[
            ReMod[ newVec, 1 ],
            newVec
          ]
        ];

      If[
        r < Length[ modVec ] && MemberQ[ modVec[[r+1;;]], x_ /; Not[ intCheck[x] ] ],
        printlog["SMZS:nonzero_coeff", {procID,modVec,r}];
        Return[ { {}, {{}}, {{}} } ]
      ];

      If[
        gaugeMat =!= None,
        { ZSpace . (newVec[[1;;r]]), ZSpace, IntOrthogonalSpace[ v[[ ;;, r+1;; ]], gaugeMat ] },
        { ZSpace . (newVec[[1;;r]]), ZSpace, v[[ ;;, r+1;; ]] }
      ]
    ];
    
    printlog["Gen:results", {procID, result, absTime }];
    
    result

  ];

SolveModZSpace[ { mat_?MatrixQ, vec_List }, opts:OptionsPattern[] ] :=
  SolveModZSpace[ mat, vec, opts ];

Options[ SolveModZ ] = Options[ SolveModZSpace ];
SolveModZ[ mat_?MatrixQ, vec_List, opts:OptionsPattern[] ] := Module[{
  v0, ZSpace, CSpace },
  { v0, ZSpace, CSpace } =
    SolveModZSpace[ mat, vec, opts ];
  If[
    ZSpace === {{}},
    { {{}}, CSpace },
    {
      (v0 + ZSpace . #)& /@ Tuples[ Range[ LCM @@ Denominator /@ ZSpace ] - 1 ],
      CSpace
    }
  ]
];

SolveModZ[ { mat_?MatrixQ, vec_List }, opts:OptionsPattern[] ] :=
  SolveModZ[ mat, vec, opts ];



SetAttributes[ ReMod, Listable ];
ReMod[ z_, i_ ] :=
  Mod[ Re[z], i ] + I * Im[z];


(* Finding subspace of column space mat1 orthogonal to column space of mat2 *)
IntOrthogonalSpace[ mat1_, mat2_ ] :=
  Which[
    Times @@ Dimensions[ mat1 ] == 0, Return[{{}}],
    Times @@ Dimensions[ mat2 ] == 0, Return[mat1],
    True, With[{
      HD = HermiteDecomposition[ Transpose[ mat1 ] .  mat2 ]
      },
      ( mat1 . Transpose[ HD[[1]] ] )[[ ;;, FirstZeroRowPos[ HD[[2]] ];; ]]
    ]
  ];
(* Returns space orthogonal to mat *)
IntOrthogonalSpace[ mat_ ] :=
  Transpose @ NullSpace @ Transpose @ mat;

FirstZeroRowPos[ mat_?MatrixQ ] := With[{
  nRows = Dimensions[mat][[1]],
  nCols = Dimensions[mat][[2]]
  },
  First @
  FirstPosition[
    mat,
    Table[ 0, nCols ],
    { nRows + 1 }
  ]
];

Options[SolveSemiExponentiatedSystem] =
{
  "OrthogonalTo" -> None,
  "UseDatabaseOfSmithDecompositions" -> False,
  "StoreDecompositions" -> False,
  "PreEqualCheck" -> Identity,
  "Hold" -> False
};

SolveSemiExponentiatedSystem[ mat_?MatrixQ, vec_List, param_, opts:OptionsPattern[] ] :=
Module[{
  ZSpace, u, d, v, r, ld, constVec, zVecs, CSpace, monomials, expRHS, NonOneCoeff, gaugeMat,
  preEqCheck, procID, hold, result, absTime, noc
  },
  gaugeMat =
    OptionValue["OrthogonalTo"];
  preEqCheck =
    OptionValue["PreEqualCheck"];
  hold =
    If[ OptionValue["Hold"], Hold, Identity ];
  procID =
    ToString[Unique[]];

  printlog["SSES:init", {procID,{mat,vec},{opts}}];

  { absTime, result } =
  AbsoluteTiming[
    If[
      mat === { {} } || vec === {},
      printlog["Gen:trivial_system", {procID}];
      Return[ {} ]
    ];

    { u, d, v } =
      If[
        OptionValue["UseDatabaseOfSmithDecompositions"],
        LoadData["SmithDecompositions"];
        AddOptions[opts][MemoizedSmithDecomposition][ mat ],
        SmithDecomposition[ mat ]
      ];

    printlog["SSES:decomposition", {procID,{u,d,v}}];

    r =
      Length[
        ld = DeleteCases[0] @ Diagonal @ Normal @ d
      ];
    
    If[
      r == 0,
      Return[{}]
    ];

    expRHS =
      Inner[ Power, vec, Transpose[ u ], Times ];

    ZSpace =
      v[[ ;;, ;;r ]] . DiagonalMatrix[ 1 / ld ];

    NonOneCoeff[ l_ ] :=
      FirstCase[ l, x_ /; preEqCheck[x] != 1 ];

    If[
      r < Length[ expRHS ] &&  !MissingQ[ noc =  NonOneCoeff[ expRHS[[r+1;;]] ] ],
      printlog["SSES:nonone_coeff", {procID,expRHS,r, preEqCheck @ noc}];
      Return[ {} ],
      constVec =
        Inner[ Power, vec, Transpose[ ZSpace.u[[;;r]] ], Times ]
    ];

    zVecs =
      hold[Map][
        Exp[ 2 Pi I # ]&,
        hold[Map][
          (ZSpace . #)&,
          hold[Tuples][ Range[ LCM @@ Denominator /@ ZSpace ] - 1 ]
        ],
        { 2 }
      ];

    CSpace =
      If[
        gaugeMat =!= None,
        IntOrthogonalSpace[ v[[ ;;, r+1;; ]], gaugeMat ],
        v[[ ;;, r+1;; ]]
      ];

    monomials =
      If[
        CSpace === {{}},
        ConstantArray[ 1, Dimensions[zVecs][[2]] ],
        With[ { parameters = param /@ Range[ Dimensions[CSpace][[2]] ] },
          Inner[ Power, parameters, #, Times ]& /@ CSpace
        ]
      ];
    
    If[
      OptionValue["Hold"],
      { constVec, zVecs, monomials },
      Table[
        constVec * zVec * monomials,
        { zVec, zVecs }
      ]
    ]
  ];
  printlog["Gen:results", {procID, result, absTime}];

  result
];

SolveSemiExponentiatedSystem[ { mat_?MatrixQ, vec_List }, param_, opts:OptionsPattern[] ] :=
  SolveSemiExponentiatedSystem[ mat, vec, param, opts ];

Options[FindInstanceSemiExponentiatedSystem] =
  Options[SolveSemiExponentiatedSystem];

FindInstanceSemiExponentiatedSystem[ { mat_?MatrixQ, vec_List }, param_, opts:OptionsPattern[] ] :=
Module[{
  ZSpace, u, d, v, r, ld, constVec, solution, zVecs, CSpace, monomials, expRHS, listOfOnesQ,
  gaugeMat = OptionValue["OrthogonalTo"],
  preEqCheck = OptionValue["PreEqualCheck"](*,
  procID = ToString[Unique[]]*)
  },
  (*printlog["SSES:init", {procID,{mat,vec},{opts}}];*)

  If[
    mat === { {} } || vec === {},
    (*
    printlog["Gen:trivial_system", {procID}];
    printlog["Gen:end", {procID}];*)
    Return[ None ]
  ];

  { u, d, v } =
  If[
    OptionValue["UseDatabaseOfSmithDecompositions"],
    LoadData["SmithDecompositions"];
    AddOptions[opts][MemoizedSmithDecomposition][ mat ],
    SmithDecomposition[ mat ]
  ];

  (* printlog["SSES:decomposition", {procID,{u,d,v}}]; *)

  r =
    Length[
      ld = DeleteCases[0] @ Diagonal @ Normal @ d
    ];

  If[
    r == 0,
    Return[{}]
  ];
  
  expRHS =
    Inner[ Power, vec, Transpose[ u ], Times ];

  ZSpace =
    v[[ ;;, ;;r ]] . DiagonalMatrix[ 1 / ld ];

  listOfOnesQ[ l_ ] :=
    DeleteCases[
      Map[
        preEqCheck,
        MapThread[ Equal, { l, ConstantArray[ 1, Length[l] ] } ],
        {2}
      ],
      True
    ] === {};

  If[
    r < Length[ expRHS ] &&  !listOfOnesQ[ expRHS[[r+1;;]] ] ,
    (* printlog["SSES:nonzero_coeff", {procID,expRHS,r}];
    printlog["Gen:end"];*)
    Return[ None ],
    constVec =
      Inner[ Power, vec, Transpose[ ZSpace.u[[;;r]] ], Times ]
  ];

  CSpace =
    If[
      gaugeMat =!= None,
      IntOrthogonalSpace[ v[[ ;;, r+1;; ]], gaugeMat ],
      v[[ ;;, r+1;; ]]
    ];

  monomials =
    If[
      CSpace === {{}},
      ConstantArray[ 1, Dimensions[zVecs][[2]] ],
      With[ { parameters = param /@ Range[ Dimensions[CSpace][[2]] ] },
        Inner[ Power, parameters, #, Times ]& /@ CSpace
      ]
    ];

  solution =
    constVec * monomials;

  (* printlog["Gen:solutions", {procID,solutions}]; *)
  (* printlog["Gen:end", {procID}]; *)

  solution

];

Options[ MonPolEqnsToSemiExponentiatedSystem ] =
  Join[
    Options[ MonEqnToFactorList ],
    { "Numeric" -> False }
  ];

MonPolEqnsToSemiExponentiatedSystem[ eqnList_, nVars_Integer, s_, opts:OptionsPattern[] ] :=
  If[
    nVars == 0, (* TODO: might want to throw warning here *)
    { {{}}, {} },
    With[{
      factorLists =
        Sort @
        DeleteDuplicates @
        DeleteCases[{{{0, 1}}, {{0, 1}}}] @ (* These correspond to trivial equations 0 == 0 *)
        ( AddOptions[opts][MonEqnToFactorList][ # ]& /@ eqnList )
      },
      If[
        factorLists === {},
        { { ConstantArray[ 0, nVars ] }, {0} },
        With[{
          coeffList1 = factorLists[[ ;;, 1, 1, 1 ]],
          expLists1  = factorLists[[ ;;, 1, 2;; ]],
          coeffList2 = factorLists[[ ;;, 2, 1, 1 ]],
          expLists2  = factorLists[[ ;;, 2, 2;; ]],
          nEqn = Length[ factorLists ]
          },
          {
            SparseArray[
              Join @@
              MapIndexed[
                FactorListToSparseInput[ #1, First[#2], s ]&,
                expLists1
              ],
              { nEqn, nVars }
            ] -
            SparseArray[
              Join @@
              MapIndexed[
                FactorListToSparseInput[ #1, First[#2], s ]&,
                expLists2
              ],
              { nEqn, nVars }
            ],
            -coeffList2 / coeffList1
          }
        ]
      ]
    ]
  ];

(* ONLY TAKES SYSTEMS WITH SINGLE INDEXED VARIABLES! so no vars of the form x[ i, j, ... ], only x[i],... *)
Options[ MonPolEqnsToMatSys ] =
  Options[ MonEqnToFactorList ];

MonPolEqnsToMatSys[ eqnList_, nVars_Integer, s_, opts:OptionsPattern[] ] :=
  If[
    nVars == 0, (* TODO: might want to throw warning here *)
    { {{}}, {} },
    With[{
      factorLists = (* EchoFunction["factorLists", Normal] @ *)
        DeleteDuplicates @
        DeleteCases[{{{0, 1}}, {{0, 1}}}] @ (* These correspond to trivial equations 0 == 0 *)
        ( AddOptions[opts][MonEqnToFactorList][ # ]& /@ eqnList )
      },
      If[
        factorLists === {},
        { { ConstantArray[ 0, nVars ] }, {0} },
        With[{
          coeffList1 = factorLists[[ ;;, 1, 1, 1 ]],
          expLists1  = factorLists[[ ;;, 1, 2;; ]],
          coeffList2 = factorLists[[ ;;, 2, 1, 1 ]],
          expLists2  = factorLists[[ ;;, 2, 2;; ]],
          nEqn = Length[ factorLists ]
        },
          {
            SparseArray[
              Join @@
              MapIndexed[
                FactorListToSparseInput[ #1, First[#2], s ]&,
                expLists1
              ],
              { nEqn, nVars }
            ] -
            SparseArray[
              Join @@
              MapIndexed[
                FactorListToSparseInput[ #1, First[#2], s ]&,
                expLists2
              ],
              { nEqn, nVars }
            ],
            ComplexExpand[ Log[ - coeffList2 / coeffList1 ] /( 2 Pi I ) ]
          }
        ]
      ]
    ]
  ];

Options[ MonEqnToFactorList ] = { "NonSingular" -> False };
MonEqnToFactorList[ eqn_?BinomialEquationQ, OptionsPattern[] ]  :=
  With[{
    properMonEqn = ToProperBinomialEquation[eqn],
    ns = OptionValue["NonSingular"],
    getCoeffs =
      GatherCoeffs @*
      FactorList
    },
    Which[
      (* Equation is trivially satisfied *)
      TrueQ[ properMonEqn ],
        { { { 0, 1 } }, { { 0, 1 } } },
      (* RHS of proper mon equation is 0 (i.e. LHS or RHS of original eqn is 0 ) *)
      properMonEqn[[2]] === 0 && ns,
        If[
          ComplexExpand[ properMonEqn[[2]] ] === 0,
          { { { 0, 1 } }, { { 0, 1 } } },
          Throw[ { {}, {} } , "ZeroVariableInNonSingularSystem"]
        ],
      (* All other cases *)
      True,
        getCoeffs /@
        { Together[ properMonEqn[[1]] ], - Together[ properMonEqn[[2]] ] }
    ]
  ];

(*
NFactorList[ monomial_ ] :=
  Module[ { coeffQ, mList },
    Which[
      Head[monomial] === Power,
        Return[ monomial /. Power[ i_, j_] :> { {1, 1}, { i, j } } ],
      MatchQ[ monomial, _[_Integer] ],
        Return[ { { 1, 1 }, { monomial, 1 } } ],
      NumericQ[ monomial ],
        Return[ { { monomial, 1 } } ],
      True,
        mList =
          monomial /. {
            Times -> List,
            Power[ i_, j_] :> { i, j },
            x_[i_] :> { x[i], 1 }
          };
        coeffQ =
          NumericQ[ monomial[[1]]];
        If[
          coeffQ ,
          Return[ Prepend[ { monomial[[1]], 1 } ] @ Rest[mList] ],
          Return[ Prepend[ { 1, 1 }] @ mList ]
        ]
    ]
  ];
*)

MonPolsToMat[ polList_, nVars_Integer, s_ ] := With[{
  factorLists = MonPolToFactorList /@ polList,
  nPol = Length[polList]
  },
    With[{
      expLists1 = factorLists[[;;,2;;,;;]]
    },
      SparseArray[
        Join @@
        MapIndexed[
          FactorListToSparseInput[ #1, First @ #2, s ]&,
          expLists1
        ],
        { nPol, nVars }
      ]
    ]
  ];

MonPolToFactorList[ pol_?MonomialQ]:=
  GatherCoeffs @ FactorList[ pol ];

GatherCoeffs[ l_List ] :=
  l;
GatherCoeffs[ { { a_?NumericQ, b_?NumericQ }, { c_?NumericQ, d_?NumericQ }, l2___List } ] :=
  GatherCoeffs[ { { a^b c^d, 1 }, l2 } ];

FactorListToSparseInput[ list_, i_, s_ ] :=
  ReplaceAll[ list, { s[j_], c_ } :> Rule[ { i, j } , c ] ];
