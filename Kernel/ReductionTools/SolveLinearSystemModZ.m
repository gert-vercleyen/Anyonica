(* ::Package:: *)

Package["Anyonica`"]

PackageExport["SolveModZSpace"]

SolveModZSpace::usage =
  "SolveModZSpace[mat,vec] returns a list { v, mZ, mC } where v is a vector " <>
  " and mZ, mC are matrices such that mat.(v + mZ.iVec1 + mC.cVec) == vec mod iVec2, for " <>
  "iVec1 and iVec2 arbitrary integer vectors and cVec an arbitrary complex vector.";

SolveModZSpace::nonzerocoeff =
  "The vector `1` contains non-zero entries at position(s) greater " <>
  "than `2`. Assuming system is unsolvable. This could be caused by non-simplified expressions that " <>
  " should be 0 or by the system being unsolvable.";

SolveModZSpace::nonintegermatrix =
  "`1` contains a non-integer entry.";

Options[ SolveModZSpace ] =
  {
    "OrthogonalTo" -> None,
    "SimplifyBy"   -> Identity,
    "IntegerCheck" -> IntegerQ
  };

SolveModZSpace[ mat_?MatrixQ, vec_List, opts:OptionsPattern[] ] :=
  If[
    !IntegerMatrixQ[mat]
    ,
    Message[ SolveModZSpace::nonintegermatrix, mat ];
    Abort[]
    ,
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
          { ZSpace . (newVec[[1;;r]]), ZSpace, IntegerOrthogonalSpace[ v[[ ;;, r+1;; ]], gaugeMat ] },
          { ZSpace . (newVec[[1;;r]]), ZSpace, v[[ ;;, r+1;; ]] }
        ]
      ];
      
      printlog["Gen:results", {procID, result, absTime }];
      
      result

    ]
  ];

SolveModZSpace[ { mat_?MatrixQ, vec_List }, opts:OptionsPattern[] ] :=
  SolveModZSpace[ mat, vec, opts ];

IntegerMatrixQ[ mat_ ] :=
  TrueQ[ MatrixQ[mat] && ( mat === {{}} || FreeQ[ mat, x_ /; !IntegerQ[x], {3} ] ) ];

IntegerMatrixQ[ mat_SparseArray ] :=
  TrueQ @ FreeQ[ ArrayRules[mat][[;;,2]], x_ /; !IntegerQ[x], {2} ];


PackageExport["SolveModZ"]

SolveModZ::usage =
  "SolveModZ[mat,vec] returns a couple { vecs, mC } where vecs " <>
  "is a list of constant vectors v, and mC is a matrix such that " <>
  "mat.(v + mC.cVec) == vec mod iVec, for iVec an arbitrary integer " <>
  "vector and cVec an arbitrary complex vector.";(* Solving pentagon equations *)

Options[ SolveModZ ] :=
  Options[ SolveModZSpace ];

SolveModZ[ mat_?MatrixQ, vec_List, opts:OptionsPattern[] ] :=
  Module[{v0, ZSpace, CSpace },
    
    { v0, ZSpace, CSpace } =
      SolveModZSpace[ mat, vec, opts ];
    
    If[
      ZSpace === {{}}
      ,
      { {{}}, CSpace }
      ,
      {
        (v0 + ZSpace . #)& /@ Tuples[ Range[ LCM @@ Denominator /@ ZSpace ] - 1 ],
        CSpace
      }
    ]
  ];

SolveModZ[ { mat_?MatrixQ, vec_List }, opts:OptionsPattern[] ] :=
  SolveModZ[ mat, vec, opts ];


FirstZeroRowPos[ mat_?MatrixQ ] :=
  With[{
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

SetAttributes[ ReMod, Listable ];

ReMod[ z_, i_ ] :=
  Mod[ Re[z], i ] + I * Im[z];


PackageExport["IntegerOrthogonalSpace"]

IntegerOrthogonalSpace::usage =
  "IntegerOrthogonalSpace[ mat ] returns an integer matrix whose column space " <>
  " is orthogonal to that of the integer matrix mat.\n" <>
  "IntegerOrthogonalSpace[ mat1, mat2 ] returns an integer matrix whose " <>
  "column space is a subspace of the integer matrix mat1 and orthogonal to the integer matrix mat2.";

IntegerOrthogonalSpace::nonintegermatrix =
  "`1` contains a non-integer entry.";

IntegerOrthogonalSpace[ mat1_, mat2_ ] :=
  Which[
    !IntegerMatrixQ[mat1]
    ,
    Message[ IntegerOrthogonalSpace::nonintegermatrix, mat1 ];
    Abort[]
    ,
    !IntegerMatrixQ[mat2]
    ,
    Message[ IntegerOrthogonalSpace::nonintegermatrix, mat2 ];
    Abort[]
    ,
    Times @@ Dimensions[ mat1 ] == 0
    ,
    {{}}
    ,
    Times @@ Dimensions[ mat2 ] == 0
    ,
    mat1
    ,
    True
    ,
      With[{ HD = HermiteDecomposition[ Transpose[ mat1 ] .  mat2 ] },
          ( mat1 . Transpose[ HD[[1]] ] )[[ ;;, FirstZeroRowPos[ HD[[2]] ];; ]]
      ]
  ];

IntegerOrthogonalSpace[ mat_ ] :=
  If[
    !IntegerMatrixQ[mat]
    ,
    Message[IntegerOrthogonalSpace::nonintegermatrix, mat ];
    Abort[]
    ,
    Transpose @ NullSpace @ Transpose @ mat
  ];


PackageScope["SolveSemiLinModZ"]

SolveSemiLinModZ::usage =
  "SolveSemiLinModz[ mat, vec, param ] solves a binomial system whose logarithm is given by mat but whose vector of numeric factors is left as is.";

SolveSemiLinModZ::nonintegermatrix =
  "`1` contains a non-integer element.";

Options[SolveSemiLinModZ] =
  {
    "OrthogonalTo" -> None,
    "UseDatabaseOfSmithDecompositions" -> False,
    "StoreDecompositions" -> False,
    "PreEqualCheck" -> Identity,
    "SimplifyIntermediateResultsBy" -> Identity,
    "Parallel" -> False (* TODO: setting Parallel to true creates a wrong structure of solutions. I can't reproduce the bug, even by coppying the whole code. Echo also doesn't show any strange results.*)
  };

SolveSemiLinModZ[ mat_?MatrixQ, vec_List, param_, opts:OptionsPattern[] ] :=
  Module[{
    ZSpace, u, d, v, r, ld, constVec, zVecs, CSpace, monomials, expRHS,gaugeMat,
    preEqCheck, procID, simplify, result, absTime, map, consistent
    },
    gaugeMat =
      OptionValue["OrthogonalTo"];
    preEqCheck =
      OptionValue["PreEqualCheck"];
    simplify =
      OptionValue["SimplifyIntermediateResultsBy"];
    (*If[ OptionValue["Parallel"], LaunchKernels[]; map = ParallelMap, *)map =  Map (*];*);
    
    procID =
      ToString[Unique[]];

    printlog["SSES:init", {procID,{mat,vec}, param,{opts}}];

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
        PowerDot[ vec, u ];

      consistent =
        AddOptions[opts][ConsistentQ][ r, expRHS, TrueQ[# == 1]& ];
      
      If[ !consistent, Return @ { } ];

(*      If[*)
(*        r < Length[ expRHS ] &&  !MissingQ[ noc =  NonOneCoeff[ expRHS[[r+1;;]] ] ],*)
(*        printlog["SSES:nonone_coeff", {procID,expRHS,r, preEqCheck @ noc}];*)
(*        Return[ {} ],*)
(*        constVec =*)
(*          map[*)
(*            simplify,*)
(*            PowerDot[ expRHS[[;;r]], ZSpace ]*)
(*          ]*)
(*      ];*)

      ZSpace =
        v[[ ;;, ;;r ]] . DiagonalMatrix[ 1 / ld ];

      constVec =
        map[
          simplify,
          PowerDot[ expRHS[[;;r]], ZSpace ]
        ];
      
      zVecs =
        Exp[ 2 Pi I * Map[ (ZSpace . #)&, Tuples[ Range[ LCM @@ Denominator /@ ZSpace ] - 1 ] ] ];

      CSpace =
        If[
          gaugeMat =!= None,
          IntegerOrthogonalSpace[ v[[ ;;, r+1;; ]], gaugeMat ],
          v[[ ;;, r+1;; ]]
        ];

      monomials =
        If[
          CSpace === {{}}
          ,
          ConstantArray[ 1, Dimensions[zVecs][[2]] ]
          ,
          With[ { parameters = param /@ Range[ Dimensions[CSpace][[2]] ] },
            PowerDot[ parameters, CSpace ]
          ]
        ];
      
      map[ (constVec * monomials) * #&, zVecs ]
    ];
    
    If[ OptionValue["Parallel"], CloseKernels[] ];
    
    printlog["Gen:results", {procID, result, absTime}];

    result
  ];

SolveSemiLinModZ[ { mat_?MatrixQ, vec_List }, param_, opts:OptionsPattern[] ] :=
  SolveSemiLinModZ[ mat, vec, param, opts ];

PackageExport["BinToSemiLin"]

BinToSemiLin::usage =
  "BinToSemiLin[eqnList,nVars,s] converts the binomial system eqnList with nVars variables labeled by s to " <>
  "the logarithm of eqnList and a vector of factors.";

BinToSemiLin::nonbineqns =
  "`1` is not a system of binomial polynomial equations.";

BinToSemiLin::wrongnumberofvars =
  "The number of variables `1` should be a positive integer";

Options[BinToSemiLin] :=
  {
    "SimplifyIntermediateResultsBy" -> Identity,
    "PreEqualCheck" -> Identity,
    "Parallel" -> False,
    "Numeric" -> False,
    "Accuracy" -> 64
  };

CheckBinToSemiLinArgs[ eqnList_, nVars_ ] :=
  Which[
    !BinomialSystemQ[eqnList]
    ,
    Message[ BinToSemiLin::nonbineqns, eqnList ]; Abort[]
    ,
    nVars <= 0
    ,
    Message[ BinToSemiLin::wrongnumberofvars, nVars ]; Abort[]
  ];


BinToSemiLin[ eqns_, vars_, x_, opts : OptionsPattern[] ] :=
(
  CheckBinToSemiLinArgs[ eqns, Length[vars] ];
  Module[ { map, properEqns, sa, betr, tpbe, simplify, probPos, numeric, accuracy, preEqCheck },
    
    If[ OptionValue["Parallel"], Quiet[ LaunchKernels[] ]; map = ParallelMap, map = Map ];
    
    simplify =
      OptionValue["SimplifyIntermediateResultsBy"];
    numeric =
      OptionValue["Numeric"];
    accuracy =
      OptionValue["Accuracy"];
    preEqCheck =
      OptionValue["PreEqualCheck"];
    
    If[
      simplify =!= Identity
      ,
      betr = BinEqnToRow[ #, vars, x, simplify ] &;
      tpbe = ToProperBinomialEquation[ #, "SimplifyBy" -> simplify] &;
      ,
      betr = BinEqnToRow[ #, vars, x ] &;
      tpbe = ToProperBinomialEquation
    ];
    
    (* Make sure the form of the equations is correct *)
    properEqns =
      map[
        tpbe,
        If[ numeric, Rationalize[ eqns, 10^-accuracy ], eqns ]
      ];
    
    (* Check for zeros and False *)
    probPos =
      FirstPosition[ properEqns, n_Equal /; preEqCheck[n] == 0, Missing[], 1 ];
    
    If[ ! MissingQ[probPos], Throw[ { "Zero", probPos, eqns[[probPos]], properEqns[[probPos]] }, "Inconsistent"] ];
    
    probPos =
      FirstPosition[ properEqns, False, Missing[], 1 ];
    
    If[ ! MissingQ[probPos], Throw[ { "False", probPos, eqns[[probPos]], properEqns[[probPos]] }, "Inconsistent"] ];
    
    sa =
      DeleteSparseDuplicates @ SparseArray @ map[ betr, DeleteCases[True] @  properEqns ] ;
    
    If[ OptionValue["Parallel"], CloseKernels[] ];
    
    If[
      numeric,
      { sa[[;; , ;; -2]], N[ Normal @ sa[[;; , -1]], { Infinity, accuracy } ] },
      { sa[[;; , ;; -2]], Normal @ sa[[;; , -1]] }
    ]
  ]
);

PackageScope["BinEqnToRow"]

BinEqnToRow[ eqn_, vars_, x_, simplify_ ] :=
  With[ {
    rhsMons1 = ConstMonSplit[ eqn[[1]], x ],
    rhsMons2 = ConstMonSplit[ eqn[[2]], x ]
    },
    MapAt[
      simplify,
      NormalizeAndCombine @
      {
        SparseArray[ CoefficientRules[ rhsMons1[[2]], vars ][[1, 1]] ] -
        SparseArray[ CoefficientRules[ rhsMons2[[2]], vars ][[1, 1]] ],
        rhsMons2[[1]]/rhsMons1[[1]]
      },
      {-1}
    ]
  ];

  BinEqnToRow[ eqn_, vars_, x_ ] :=
  With[ {
    rhsMons1 = ConstMonSplit[ eqn[[1]], x ],
    rhsMons2 = ConstMonSplit[ eqn[[2]], x ]
    },
    NormalizeAndCombine @
    {
      SparseArray[ CoefficientRules[ rhsMons1[[2]], vars ][[1, 1]] ] -
      SparseArray[ CoefficientRules[ rhsMons2[[2]], vars ][[1, 1]] ],
      rhsMons2[[1]]/rhsMons1[[1]]
    }
  ];

PackageScope["ConstMonSplit"]

ConstMonSplit[ x_?NumericQ, _ ] :=
  { x, 1 };

ConstMonSplit[ mon_, x_ ] :=
  With[ { const = mon /. x[_] -> 1 }, { const, mon/const } ];

(* normalize gets rid of a possible overall minus sign, and combines the row with RHS so that deleting duplicates becomes much faster *)

NormalizeAndCombine[ { row_, rhs_ } ] :=
  With[ 
    { firstNonZero = SelectFirst[ ArrayRules[ row ][[ ;; , 2 ]], # != 0 & ] },
    If[
      TrueQ[ firstNonZero < 0 ],
      Append[ 1/rhs ] @ (-row),
      Append[ rhs ] @ row
    ]
  ];

(*
BinToSemiLin[ eqnList_, nVars_Integer, s_, opts:OptionsPattern[] ] :=
(
  CheckBinToSemiLinArgs[ eqnList, nVars ];
  With[
    {
      factorLists =
      Union @
      DeleteCases[{{{0, 1}}, {{0, 1}}}] @ (* These correspond to trivial equations 0 == 0 *)
      ( BinEqnToFactorList[#,opts]& /@ eqnList )
    },

    If[ factorLists === {}, Return @ { { ConstantArray[ 0, nVars ] }, {0} } ];

    With[{
      coeffList1 = factorLists[[ ;;, 1, 1, 1 ]],
      expLists1  = factorLists[[ ;;, 1, 2 ;; ]],
      coeffList2 = factorLists[[ ;;, 2, 1, 1 ]],
      expLists2  = factorLists[[ ;;, 2, 2 ;; ]],
      nEqn       = Length[ factorLists ],
      fts        = FactorListToSparseInput
      },

      TrimSemiLin @
      {
        SparseArray[ Join @@ MapIndexed[ fts[ #1, First[#2], s ]&, expLists1 ], { nEqn, nVars } ] -
        SparseArray[ Join @@ MapIndexed[ fts[ #1, First[#2], s ]&, expLists2 ], { nEqn, nVars } ]
        ,
        -coeffList2 / coeffList1
      }
    ]
  ]
);

TrimSemiLin[ { mat_, rhs_ } ] :=
  Module[ { j, joined, reduced },
    j =
      Dimensions[mat][[2]] + 1;
    joined =
      SparseArray[
        Join[
          Most[ ArrayRules @ mat ],
          Table[ { i, j } -> rhs[[i]], { i, Length[rhs]} ]
        ]
      ];
    reduced =
      SparseArray @
      DeleteDuplicates[
        DeleteDuplicates[ joined ],
        (Most[#1] == -Most[#2] && Last[#1] == 1/Last[#2]) &
      ];

    { reduced[[;; , ;; -2]], Normal @ reduced[[;; , -1]] }
  ];
*)

PackageExport["BinToLin"]

BinToLin::usage =
  "BinToLin[eqnList,vars,s] converts the binomial system eqnList with variables vars labeled by s to " <>
  "a tuple {m,v} where m is the matrix and v the vector that describe the logarithm of the system.";
(*  " equations eqns in nvars variables named s[1],...,s[nvars], modulo 2 \*)
(*  Pi I.";*)
BinToLin::nonbineqns =
  "`1` is not a system of binomial polynomial equations.";

Options[BinToLin] :=
  Options[BinToSemiLin];

BinToLin[ eqns_, vars_, x_, opts:OptionsPattern[] ] :=
  With[{ semiLin = BinToSemiLin[eqns,vars,x,opts] },
    {
      semiLin[[1]],
      Map[
        OptionValue["SimplifyIntermediateResultsBy"][ Log[#] / ( 2 Pi I ) ]&,
        semiLin[[2]]
      ]
    }
  ];

(* ONLY TAKES SYSTEMS WITH SINGLE INDEXED VARIABLES! so no vars of the form x[ i, j, ... ], only x[i],... *)
(*
BinToLin[ eqnList_, nVars_Integer, s_, opts:OptionsPattern[] ] :=
  Which[
    !BinomialSystemQ[eqnList]
    ,
    Message[ BinToLin::nonbineqns, eqnList ];
    Abort[]
    ,
    nVars == 0
    ,
    { {{}}, {} }
    ,
    True
    ,
      With[{
        factorLists =
          DeleteDuplicates @
          DeleteCases[{{{0, 1}}, {{0, 1}}}] @ (* These correspond to trivial equations 0 == 0 *)
          ( BinEqnToFactorList[#,opts]& /@ eqnList )
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
*)

(*
BinPolsToMat[ polList_, nVars_Integer, s_ ] :=
  With[
    { factorLists = BinPolToFactorList /@ polList,  nPol = Length[polList] },
    { expLists1 = factorLists[[;;,2;;,;;]] },
    SparseArray[
      Join @@
      MapIndexed[
        FactorListToSparseInput[ #1, First @ #2, s ]&,
        expLists1
      ],
      { nPol, nVars }
    ]
  ];
*)


(*

Options[BinEqnToFactorList] :=
  Options[ToProperBinomialEquation];

BinEqnToFactorList[ eqn_, opts:OptionsPattern[] ]  :=
  With[{
    properBinEqn =  AddOptions[opts][ToProperBinomialEquation][eqn],
    getCoeffs = GatherCoeffs @* FactorList
    },
    Which[
      (* Equation is trivially satisfied *)
      TrueQ[ properBinEqn ]
      ,
      { { { 0, 1 } }, { { 0, 1 } } }
      ,
      (* RHS of proper bin equation is 0 (i.e. LHS or RHS of original eqn is 0 ) *)
      properBinEqn[[2]]  === 0 || properBinEqn[[1]] === 0
      ,
      
      Throw[ {eqn,properBinEqn}, "ZeroVariableInNonSingularSystem"]
      ,
      (* All other cases *)
      True,
      getCoeffs /@
      Sort @ { Together[ properBinEqn[[1]] ], - Together[ properBinEqn[[2]] ] }
    ]
  ];

BinPolToFactorList[ pol_]:=
  GatherCoeffs @ FactorList[ pol ];

GatherCoeffs[ { { a_?NumericQ, b_?NumericQ }, monList___ } ] :=
  { { a^b, 1 }, monList };

GatherCoeffs[ { { a_?NumericQ, b_?NumericQ }, { c_?NumericQ, d_?NumericQ }, l2___List } ] :=
  GatherCoeffs[ { { a^b c^d, 1 }, l2 } ];

FactorListToSparseInput[ list_, i_, s_ ] :=
  ReplaceAll[ list, { s[j_], c_ } :> Rule[ { i, j } , c ] ];
  
 *)

PackageExport["ReduceSemiLinModZ"]

Options[ReduceSemiLinModZ] := 
  Join[
    Options[ConsistentQ],
    {
      "UseDatabaseOfHermiteDecompositions" -> True,
      "StoreHermiteDecompositions" -> False
    }
  ];

ReduceSemiLinModZ[ mat_, rhs_, s_, opts:OptionsPattern[]  ] := 
  Module[{ uInv, upperTriang, leftHandSides, rightHandSides },
    
    { uInv, upperTriang  } =
      Normal @ 
      If[
        OptionValue["UseDatabaseOfHermiteDecompositions"]
        ,
        AddOptions[opts][MemoizedHermiteDecomposition][ mat ]
        ,
        HermiteDecomposition[mat];
      ];

    (* rank of system *)
    r = 
      Length[ upperTriang ] - Count[ upperTriang, { 0 .. } ];
    
    expRHS =
      PowerDot[ rhs, uInv ];

    (* New right hand side of system should be trivial for trivial LHS *)
    consistent =
      AddOptions[opts][ConsistentQ][ r, expRHS, TrueQ[# == 1]& ];
      
    If[ !consistent, Return @ { False } ];

    rightHandSides = 
      expRHS[[;;r]];

    leftHandSides = 
      PowerDot[ Array[ s, Last @ Dimensions[mat] ], upperTriang[[;;r]] ];

    Thread[ leftHandSides == rightHandSides ]

  ]