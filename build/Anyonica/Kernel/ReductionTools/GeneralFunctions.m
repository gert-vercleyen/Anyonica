(* ::Package:: *)

(*
+---------------------------------------------------------------------------+
|                                                                           |
|                       GENERICALLY USEFUL FUNCTIONS                        |
|                                                                           |
+---------------------------------------------------------------------------+
*)

Package["Anyonica`"]

PackageScope["ListOfEquationsQ"]

ListOfEquationsQ::usage =
  "Check whether an expression is a list of expressions with head Equal.";

ListOfEquationsQ[ expr_ ] :=
  TrueQ @ MatchQ[ expr, { (_Equal | True | False )... } ];


PackageExport["NIntegerQ"]

NIntegerQ::usage =
  "NIntegerQ[x,acc] numerically checks whether x is an integer with accuracy acc and infinite precision.\n" <>
  " NIntegerQ[x] is shorthand for NIntegerQ[x,$MachinePrecision]";

(* Numerically check whether a number is a real integer with desired accuracy *)
NIntegerQ[ x_, accuracy_ ] :=
  With[ { nx = N[ x, { Infinity, accuracy } ] },
    RealAbs[ Im[ nx ] ] == 0 && nx == Round[nx]
  ];

NIntegerQ[ x_ ] :=
  NIntegerQ[ x, $MachinePrecision ];


PackageExport["MonomialQ"]

MonomialQ::usage =
  "MonomialQ[rat] returns true if the rational function rat has monomial denominator and numerator.";

MonomialQ[ pol_ ] :=
  With[ {
    newPol = Cancel[Together[pol]] /. Power[ _, i_ ] /; i < 0 -> 1
    },
    Length[ MonomialList[ newPol ] ] === 1
  ];

PackageExport["BEQ"]

BEQ::usage =
  "Shorthand for BinomialEquationQ";

BEQ =
  BinomialEquationQ;

PackageExport["BinomialEquationQ"]

BinomialEquationQ::usage =
  "BinomialEquationQ[eqn] returns True if the equation is a Binomial equation.";

BinomialEquationQ::notequation =
  "The argument `1` is not an equation or True/False.";

  (* TODO: check whether it causes problems if False returns True *)
BinomialEquationQ[True] :=
  True;

BinomialEquationQ[ eqn_ ] :=
  If[
    Head[eqn] =!= Equal
    ,
    False
    ,
    With[{ newEqn = RemoveFractions[eqn] },
      Which[
        TrueQ[newEqn],
        Return[True],
        newEqn === False,
        Return[False]
      ];
    
      With[{ lhs = newEqn[[1]],  rhs = newEqn[[2]] },
        Which[
          lhs =!= 0 && rhs =!= 0,
            Length[ MonomialList[ lhs ] ] + Length[ MonomialList[ rhs ] ] <= 2,
          lhs === 0,
            Length[ MonomialList[ lhs ] ] <= 2,
          rhs === 0,
            Length[ MonomialList[ lhs ] ] <= 2
        ]
      ]
    ]
  ];


PackageExport["ToProperBinomialEquation"]

ToProperBinomialEquation::usage =
  "ToProperBinomialEquation[binEqn] returns an equivalent binomial "<>
  "equation of the form LHS == RHS where both RHS and LHS are " <>
  " non-zero.";

ToProperBinomialEquation::notbineqn =
  "Equation `1` is not a binomial equation.";

Options[ToProperBinomialEquation] =
  { "SimplifyBy" -> Identity };

SetAttributes[ ToProperBinomialEquation, Listable ];

ToProperBinomialEquation[ True, ___ ] :=
  True;

ToProperBinomialEquation[ eqn_, opts:OptionsPattern[] ] :=
  Which[
    !BinomialEquationQ[ eqn ]
    ,
    Message[ ToProperBinomialEquation::notbineqn, eqn ];
    Abort[]
    ,
    True
    ,
    With[{ pol = OptionValue["SimplifyBy"] @ ToPolynomial @ eqn },

      If[ pol === 0, Return @ True ] ;

      With[{ mList = MonomialList[ pol ] },
        If[
          Length[mList] === 1,
          mList[[1]] == 0,
          mList[[1]] == -mList[[2]]
        ]
      ]
    ]
  ];


PackageExport["ToStandardPolynomial"]


Options[ToStandardPolynomial] =
  { "SimplifyBy" -> Identity };

ToStandardPolynomial[ pol_, opts:OptionsPattern[] ] :=
  With[{ poly =  OptionValue["SimplifyBy"] @ RemoveFractions @ pol },
    If[
      poly === 0,
      0,
      Cancel[ poly/CoefficientRules[ poly ][[-1,2]] ]
    ]
  ];

RemoveFractions[ eqn_Equal ] :=
  Expand[ Numerator[ Together[ eqn[[1]] - eqn[[2]] ] ] ] == 0;

RemoveFractions[ True ] =
  True;

RemoveFractions[ False ] =
  False;

RemoveFractions[ pol_ ] :=
  Expand @* Numerator @* Together @ pol;

PackageExport["RemoveCommonFactors"]

RemoveCommonFactors[ poly_?NumericQ, _ ] :=
  poly;

RemoveCommonFactors[ poly_, s_ ] :=
  Module[{vars, cr, ce, d, commonVarNum, minExponents, commonFactor},
    vars =
      GetVariables[poly, s];
    cr =
      CoefficientRules[poly, vars];
    ce =
      cr[[;; , 1]];
    d =
      cr[[ -1 , 2]];
    commonVarNum =
      Flatten@Position[Times @@ ce, x_ /; x > 0];
    minExponents =
      Min /@ Transpose[ce][[commonVarNum]];
    commonFactor =
      PowerDot[ vars[[commonVarNum]], minExponents ];
    
    Expand @ Cancel[poly/( d * commonFactor)]
  ];

PackageExport["ToPolynomial"]

ToPolynomial::usage =
  "ToPolynomial[ equation ] converts a polynomial equation to a polynomial.";

ToPolynomial::notanequation =
  "The argument `1` must be an equation.";

SetAttributes[ ToPolynomial, Listable ];

ToPolynomial[ eqn_ ] :=
  If[
    Head[eqn] =!= Equal
    ,
    Message[ ToPolynomial::notanequation, eqn ];
    Abort[]
    ,
    RemoveFractions @
    ( Subtract @@ eqn )
  ];


PackageScope["ToReducedPolynomial"]

Options[ToReducedPolynomial] =
  {
    "SimplifyBy" -> Identity
  };

SetAttributes[ ToReducedPolynomial, Listable ]

ToReducedPolynomial[ eqn_, x_, OptionsPattern[] ] :=
  With[{ pol = OptionValue["SimplifyBy"] @ RemoveFractions @ (eqn[[1]]-eqn[[2]]) },
    RemoveCommonFactors[ pol, x ]
  ];

PackageExport["PolynomialDegree"]

PolynomialDegree::usage =
  "PolynomialDegree[ pol, vars ] returns the degree of the polynomial with all vars replaced by a single variable.\n" <>
  "PolynomialDegree[ pol, s ] equals PolynomialDegree[ pol, GetVariables[ pol, s ] ].";

PolynomialDegree[ n_?NumericQ, _ ] =
  0;

PolynomialDegree[ pol_, vars_ ] :=
  Max[
    Total /@
    (
      ArrayRules[ CoefficientList[ pol, vars ] ][[;; -2, 1]] - 1
    )
  ];

PolynomialDegree[ pol_, s_Symbol ] :=
  PolynomialDegree[ pol, GetVariables[ pol, s ] ];


PackageExport["BinomialSystemQ"]

BinomialSystemQ::usage =
  "BinomialSystem[ eqnList ] returns True if eqnList is a list of binomial equations.";

BinomialSystemQ[ eqns_ ] :=
  TrueQ @
  MatchQ[ eqns, { _?BinomialEquationQ ... } ];


PackageExport["BinSplit"]

BinSplit::usage =
  "BinSplit[l,f] splits list l in two lists for which f is resp True and False.\n"<>
  "BinSplit[f] is an operator form of BinSplit.";
  
BinSplit[ l_List, f_ ] :=
  Replace[
    GroupBy[
      l,
      TrueQ @* f
    ] /@ { True, False },
    Missing[___] -> {},
    1
  ];

BinSplit[f_][l_List] :=
  BinSplit[ l, f ];

PackageExport["BinomialSplit"]

BinomialSplit::usage =
  "BinomialSplit[eqnList] splits eqnList into a list of binomial equations and a list non-binomial " <>
  "equations.";

Options[ BinomialSplit ] =
  {
    "PreEqualCheck" -> Identity
  };

BinomialSplit[ eqnList_List, OptionsPattern[] ] :=
  BinSplit[ eqnList, BinomialEquationQ @* OptionValue["PreEqualCheck"] ];


PackageScope["SymbolQ"]

SymbolQ::usage =
  "Checks whether head of expression is Symbol.";

SymbolQ[expr_] :=
  Head[expr] === Symbol;


(* DeleteDuplicates destroys the sparsity of an array so an alternative is the following. It groups row numbers by
  same values of their rows and deletes duplicates that way. *)
PackageScope["DeleteSparseDuplicates"]

DeleteSparseDuplicates[ sparseArray_ ] :=
  With[ { positions = GatherBy[ Range @ Length @ sparseArray, sparseArray[[#]]& ][[ ;; , 1 ]] },
    sparseArray[[positions]]
  ];


PackageExport["GV"]

GV::usage =
  "Shorthand for GetVariables.";

GV =
  GetVariables;

PackageExport["GetVariables"]

GetVariables::usage =
  "GetVariables[expr,s] returns a sorted list of the variables s[__] in expr.\n"<>
  "GetVariables[expr,s,exclude] is like GetVariables[expr,s] except that it excludes variables from the list exclude";

GetVariables::invalidlevelspec =
  "`1` must be an integer or Infinity";

Options[GetVariables] =
  { "LevelSpec" -> Infinity };

GetVariables[ expression_, s_, OptionsPattern[] ] :=
  With[{ n = OptionValue["LevelSpec"] },
    Which[
      !IntegerQ[n] && n =!= Infinity,
      Message[GetVariables::invalidlevelspec,n];
      Abort[],
      True,
        Cases[ { expression }, s[__], n ] //
        DeleteDuplicates //
        Sort
    ]
  ];

GetVariables[ expression_, s_, excludedVars_List, OptionsPattern[] ] :=
  With[{
    n = OptionValue["LevelSpec"]},
    If[ !IntegerQ[n] && n =!= Infinity
      ,
      Message[GetVariables::invalidlevelspec,n];
      Abort[]
      ,
      Cases[ { expression }, s[i__]/; FreeQ[excludedVars, s[i] ], n ] //
      DeleteDuplicates //
      Sort
    ]
  ];

GetVariables[ expression_, symbols_List, opts:OptionsPattern[] ] :=
  Join @@ (GetVariables[ expression, # , opts]& /@ symbols );

GetVariables[ expression_, symbols_List, excludedVars_, opts:OptionsPattern[] ] :=
  Join @@ (GetVariables[ expression, #, excludedVars, opts ]& /@ symbols );


PackageExport["CountVariables"]

CountVariables::usage =
  "CountVariables[expr,s] returns the number of different variables s[__] that appear in expr."

Options[CountVariables] :=
  Options[GetVariables];

CountVariables[ data__ ] :=
  Length @ GetVariables[ data ];

PackageExport["SV"]

SV::usage =
  "Shorthand for SimplifyVariables.";

SV =
  SimplifyVariables;

PackageExport["SimplifyVariables"]

SimplifyVariables::usage =
  "SimplifyVariables[eqns,oldVars,s] replaces all variables, oldVars, in eqns " <>
  "by a single indexed variable labeled by s and and returns a " <>
  "triple of new eqns, new variables, and replacement rules to revert to old " <>
  "variables.";

SimplifyVariables::varswrongformat =
  "`1` needs to be a list of variables.";

SimplifyVariables::exprwrongformat =
  "`1` needs to be a list of expressions";

SimplifyVariables[ exprList_, oldVars_, s_ ] :=
  Which[
    !ListQ[exprList]
    ,
    Message[ SimplifyVariables::exprwrongformat, exprList ];
    Abort[]
    ,
    !ListQ[oldVars]
    ,
    Message[ SimplifyVariables::varswrongformat, oldVars ];
    Abort[]
    ,
    True
    ,
      With[
        { newVars = s /@ Range[ Length[ oldVars ] ] },
        { r = Thread[ oldVars -> newVars ] },
        printlog["SV:subs", { ToString[Unique[]], r } ];
        {
          exprList/.Dispatch[r],
          newVars,
          Dispatch[ Reverse /@ r ]
        }
      ]
  ];

(* Replace all equivalent variables by a representative of the equivalence class *)

PackageExport["ReplaceByRepresentatives"]

ReplaceByRepresentatives::usage =
  "ReplaceByRepresentatives[equivClasses, representatives, expr] returns expr" <>
  " with all occurrences e_i in expr replaced by their representatives" <>
  " according to the equivalence class that e_i belongs to.\n" <>
  "ReplaceByRepresentatives[equivClasses, representatives] is the operator form of" <>
  "ReplaceByRepresentatives";

ReplaceByRepresentatives::repnotinclass =
  "One or more classes in `1` does not contain an element from `2` at " <>
  "the same position as the class.";

ReplaceByRepresentatives[ equivClasses_List, reps_, expr_ ] :=
  If[
    And @@ MapThread[ MemberQ, { equivClasses, reps } ],
    With[{
      repRules =
        MapThread[
          Table[ expr -> #1, { expr, #2 } ]&,
          { reps, equivClasses }
        ] // Flatten
      },
      { ReplaceAll[ expr, Dispatch[ repRules ] ], Dispatch[ Reverse /@ repRules ] }
    ],
    Message[ ReplaceByRepresentatives::repnotinclass, equivClasses, reps ];
    Abort[]
  ];

PackageScope["Orbit"]

Orbit::usage =
  "Returns the list of equivalent elements to var (including var itself)"

Orbit[ mapToReps_ ][ var_ ] :=
  If[
    MemberQ[ mapToReps[[;;,2]], var ],
    With[ { asso = Association @@ mapToReps },
      Append[ var ] @
      Select[ Keys[asso], asso[#] === var & ]
    ],
    var
  ];

Orbit[ mapToReps_ ][ var_ -> val_ ] :=
  If[
    MemberQ[ mapToReps[[;;,2]], var ],
    With[ { asso = Association @@ mapToReps },
      Map[
        (# -> val)&,
        Append[ var ] @
        Select[ Keys[asso], asso[#] === var & ]
      ]
    ],
    var -> val
  ];


PackageScope["CheckSystemValidity"]

CheckSystemValidity[ eqns_List ] :=
  If[
    !ValidSystemQ[ eqns ],
    Throw[{False}],
    eqns
  ];

ValidSystemQ[ eqns_List ] :=
  FreeQ[ eqns, False ];


PackageExport["AddOptions"]

AddOptions::usage =
  "AddOptions[ opts ][ func ][ args ] returns func[ args, filteredOptions ], where filteredOptions is the"<>
  " list of those options of opts that are valid for func.";

AddOptions[ opts:OptionsPattern[] ][ head_ ][ args___ ] :=
  head[ args, Sequence @@ FilterRules[ {opts}, Options[head] ] ];

AddOptions[][ head_ ][ args___ ] :=
  head[ args ];


PackageExport["SimplifyUsingRoots"]

SimplifyUsingRoots::usage =
  "SimplifyUsingRoots[ expr_, acc_ ] attempts to simplify expr by approximating " <>
  "all non-integers in expr with accuracy acc and replacing them with exact roots of polynomials.";

SimplifyUsingRoots[ expr_, acc_Integer ] :=
  ReplaceAll[
    expr,
    x_?NumericQ /; ! IntegerQ[x] :> RootApproximant[ N[x, { Infinity, acc } ] ]
  ];

SimplifyUsingRoots[ expr_ ] :=
  SimplifyUsingRoots[ expr, 128 ];

SimplifyUsingRoots[ expr_, variables_List, acc_Integer ] :=
  Block[{ x, simplerExpr, newVars, revertVars},
    { simplerExpr, newVars, revertVars} =
      SimplifyVariables[ expr, variables, x ];

    SetAttributes[ x, NHoldAll ];

    ReplaceAll[
      N[ expr, { Infinity, acc } ],
      x_?NumericQ :> RootApproximant[ N[x, { Infinity, acc } ] ]
    ] /. revertVars
  ];

SimplifyUsingRoots[ expr_, variables_List ] :=
  SimplifyUsingRoots[ expr, variables, 128 ];


PackageScope["ValidSolutionQ"]

ValidSolutionQ::usage =
  "Check whether soln satisfies eqns after applying preEqCheck.";

ValidSolutionQ[ eqns_, preEqCheck_ ][ soln_ ] :=
  With[{
      filledInEqns =
        DeleteDuplicates @
        DeleteCases[True] @
        Expand[ Map[ preEqCheck, eqns/.Dispatch[soln], {2} ] ]
    },
    If[
      MemberQ[ filledInEqns, False ],
      False,
      If[
        filledInEqns =!= {},
        Message[ ValidSolutionQ::unresolvedequations, filledInEqns ]
      ];
      True
    ]
  ];

PackageScope["NotInvalidNonZeroSolutionQ"]

NotInvalidNonZeroSolutionQ::usage =
  "Checks whether a single solution is correct, given that no variables are allowed to be 0";

NotInvalidNonZeroSolutionQ[ {}, _ ][ _ ] :=
  True;

NotInvalidNonZeroSolutionQ[ eqns_, preEqCheck_ ][ soln_ ] :=
  With[
    {
      filledInEqns =
        Expand[ Map[ preEqCheck, eqns/.Dispatch[soln], {2} ] ]
    },
    FreeQ[
      filledInEqns,
      False | HoldPattern[ 0 == Times[__] ] | HoldPattern[ Times[__] == 0 ]
    ]
  ];


PackageExport["WithDimension"]

WithDimension::usage =
  "WithDimension[ matList, { min, max, step } ] returns all (m x n) matrices in mathList " <>
  " such that m  is an element of Range[ min, max, step ].\n" <>
  "WithDimension[ matList_, { min, max } ] returns all (m x n) matrices such that min <= m <= max.\n" <>
  "WithDimension[ matList_, { k_Integer } ] returns al (m x n) matrices such that m == k.";
(*   Set \"ColumnDimensions\" -> True to select matrices based on the number of columns.";*)

Options[WithDimension] =
  {
    "ColumnDimension" -> False
  };

WithDimension[ matList_, {min_Integer, max_Integer, step_Integer }, OptionsPattern[] ] :=
  With[ {
    r = Range[min,max,step],
    dim = If[OptionValue["ColumnDimension"],Last,First] @* Dimensions
    },
    Cases[
      matList,
      mat_?MatrixQ /; MemberQ[dim[mat]] @ r
    ]
  ];

WithDimension[ matList_, { min_, max_ }, OptionsPattern[] ] :=
  With[{
    dim = If[OptionValue["ColumnDimension"],Last,First] @* Dimensions
    },
    Cases[
      matList,
      mat_?MatrixQ /; min <= dim[mat] <= max
    ]
  ];
  
WithDimension[ matList_, { k_Integer }, OptionsPattern[] ] :=
  With[{
    dim = If[OptionValue["ColumnDimension"],Last,First] @* Dimensions
    },
    Cases[
      matList,
      mat_?MatrixQ /; dim[mat] == k
    ]
  ];

WithDimension[ matList_List, k_Integer, opts:OptionsPattern[] ] :=
  WithDimension[ matList, { k }, opts ];


PackageExport["WithMinimumDimension"]

WithMinimumDimension::usage =
  "WithMinimumDimension[ matList, k ] returns all (m x n) matrices in mathList with m >= k."

Options[WithMinimumDimension] :=
  Options[WithDimension];

WithMinimumDimension[ matList_List, k_Integer, opts:OptionsPattern[] ] :=
  WithDimension[ matList, { k, Infinity }, opts ];


PackageExport["WithMaximumDimension"]

WithMaximumDimension::usage =
  "WithMaximumDimension[ matList, k ] returns all (m x n) matrices in mathList with m <= k."

Options[WithMaximumDimension] :=
  Options[WithDimension];

WithMaximumDimension[ matList_List, k_Integer, opts:OptionsPattern[] ] :=
  WithDimension[ matList, { 0, k }, opts ];


PackageScope["MatrixDirectSum"]

MatrixDirectSum::usage =
  "MatrixDirectSum[ listOfMatrices ] returns the direct sum of the matrices in listOfMatrices as a sparse matrix.";

MatrixDirectSum::notlistofmatrices =
  "`1` should be a list of matrices.";

MatrixDirectSum[ listOfMatrices_ ] :=
  If[
    !ListOfMatricesQ[listOfMatrices]
    ,
    Message[ MatrixDirectSum::notlistofmatrices, listOfMatrices ];
    Abort[]
    ,
    With[
      { r = MapIndexed[#2[[1]] {1, 1} -> # &, DeleteCases[ listOfMatrices, {{}} ], 1]},
      SparseArray`SparseBlockMatrix[r]
    ]
  ];

ListOfMatricesQ[ l_ ] :=
  MatchQ[ l, { _?MatrixQ .. } ];


PackageScope["ThreadMatrixEquality"]

ThreadMatrixEquality::usage =
  "ThreadMatrixEquality[ mat1 == mat2 ] returns a list of equations between the individual matrix elements.";
(*  It is optimized for use with sparse matrices.";*)

ThreadMatrixEquality::dimensionsmismatch =
  "The dimensions of the two matrices do not coincide.";

ThreadMatrixEquality[ False ] =
  False;

ThreadMatrixEquality[ True ] =
  True;

ThreadMatrixEquality[ m1_?MatrixQ == m2_?MatrixQ ] :=
  Which[
    Dimensions[m1] != Dimensions[m2]
    ,
    Message[ ThreadMatrixEquality::dimensionsmismatch ];
    Abort[]
    ,
    Head[ m1 ] === Head[ m2 ] === SparseArray,
      Module[ { sparseAssociation, sa1, sa2, k1, k2 },
        sparseAssociation =
          Association @@ Sort @ ArrayRules @ # &;
        sa1 =
          sparseAssociation @ m1;
        sa2 =
          sparseAssociation @ m2;
        If[
          ( k1 = Most @Keys[ sa1 ] ) === ( k2 = Most @Keys[ sa2 ] ),
          Thread[ Values[sa1] == Values[sa2]  ],
          With[
            { inter = Intersection[ k1, k2 ] },
            With[
              { uk1 = Complement[ k1, inter ],
                uk2 = Complement[ k2, inter ] },
              Join[
                Thread[ (sa1 /@ inter) == (sa2 /@ inter) ],
                Thread[ (sa1 /@ uk1) == 0 ],
                Thread[ 0 == (sa2 /@ uk2) ]
              ]
            ]
          ]
        ]
      ],
    True,
    Flatten @
    MapThread[
      Equal,
      { m1, m2 },
      2
    ]
  ];


PackageScope["NextTuple"]

NextTuple::usage =
  "Generates the the next tuple following tp in the set of tuples generated by listOfRanges.";
(* Note: tuples are generated one by one *)

NextTuple[ listOfRanges_ ][ tp_]:=
  PositionPlus[ listOfRanges, tp, Length[listOfRanges] ];

PositionPlus[ listOfRanges_, currentTuple_, n_ ] :=
  With[{ l = Length @ listOfRanges[[n]] },
    If[
      n == 0,
      Return[ Missing["ReachedEnd"] ]
    ];
    Module[{ newTuple = currentTuple },
      If[ (* max val at range n reached *)
        currentTuple[[n]] == l - 1,
        (* THEN *)
        newTuple[[n]] = 0;
        PositionPlus[ listOfRanges, newTuple, n - 1 ],
        (* ELSE *)
        newTuple[[n]] += 1;
        newTuple
      ]
    ]
  ];


PackageExport["SolveUsingReduce"]

SolveUsingReduce::usage =
  "SolveUsingReduce[ eqns, vars ] solves the system of equations eqns in variables vars using Reduce.";

SolveUsingReduce[ eqns_, vars_, rest___ ] :=
  { ToRules @ Reduce[ eqns, vars, Backsubstitution-> True, rest ] };


PackageExport["PowerSumReduce"]

PowerSumReduce::usage =
  "PowerSumReduce[expr] simplifies Root expressions of power sum polynomials, in expr, to their polar form.";

(* Simplify Root expressions of power sum polynomials *)
PowerSumReduce[ expr_ ] :=
  ReplaceAll[
    expr,
    r_Root :> PowerSumReduce[r]
  ];

PowerSumReduce[ r : Root[ f_, __ ] ] :=
  With[{
    psr =
      FirstCase[
        PowerSumRoots[ f[x], x ],
        root_ /; N[ root - r, { Infinity, 1000 } ] == 0
      ]
    },
    If[
      MissingQ @ psr,
      r,
      psr
    ]
  ];

PowerSumRoots[ f_, x_ ] :=
  Module[
    { deg , lf, powers, gcd, u, v },
    deg =
      Exponent[ f, x ];
    lf =
      Quiet @
      Check[
        (List @@ f)/f[[1]],
        Return @ {}
      ];

    Quiet @
    Check[
      If[
        !MatchQ[ Rest @ lf, { _. * Power[ x, _.] .. } ],
        Return @ {}
      ],
      Return @ {}
    ];

    powers =
      Cases[ Rest @ lf, _. * Power[ x, i_. ] :> i ];

    gcd =
      GCD @@ powers;

    If[
      Not[ powers / gcd == Range[ deg / gcd ] ],
      Return @ {}
    ];

    If[
      Table[ (lf[[2]])^i, { i, 0, deg / gcd } ] === lf,
      (* THEN *)
      u =
        2 Pi I / ( deg + gcd );
      v =
        ( lf[[2]]/.x -> 1 )^( 1 / gcd );

      Flatten @
      Table[ Exp[ 2 Pi I k / gcd ] Exp[ l u ] / v, { l, deg / gcd }, { k, gcd } ],
      (* ELSE *)
      { }
    ]
  ];

PackageExport["QuietCheck"]

QuietCheck::usage =
  "QuietCheck[code,failExpr,msgs] performs code and silently returns failexpr if messages from msgs are generated.\n"<>
  "QuietCheck[code,failExpr] performs code and silently returns failexpr if any messages are generated.";

SetAttributes[ QuietCheck, HoldAll ];

QuietCheck[ code_, failExpr_, msgs_List ] :=
  Quiet[
    Check[
      code,
      failExpr,
      msgs
    ],
    msgs
  ];

QuietCheck[ code_, failExpr_ ] :=
  Quiet[ Check[ code, failExpr ] ];


PackageScope["ToNumericRootIsolation"]

ToNumericRootIsolation::usage =
  "Converts Root expressions with symbolic root isolation to those with numeric root isolation.";
(* Roots with Exact root isolation result in errors for RootReduce.
   Numeric isolation should be fine since the roots are still isolated
   exactly.
   
   Note that this operation might permute roots !!!
*)

ToNumericRootIsolation[ expr_ ] :=
  ReplaceAll[
    expr,
    Root[ f_, n_, 1 ] :> Root[ f, n, 0 ]
  ];

PackageExport["SafeRootReduce"]

SafeRootReduce::usage =
  "Applies RootReduce to argument but returns argument if messages are generated. " <>
  "(Needed in v13.2- due to bug in RootReduce)";

SafeRootReduce =
  Function[
    { polynomial },
    QuietCheck[
      RootReduce @ polynomial,
      polynomial
    ]
  ];

PackageExport["InfN"]

InfN::usage =
  "InfN[expr,acc] is shorthand for N[ expr, { Infinity, acc } ].\n"<>
  "InfN[acc] is an operator form of InfN.";

(* Approximate value with accuracy acc and infinite precision *)
InfN[ expr_, acc_ ] :=
  N[ expr, { Infinity, acc } ];
InfN[ acc_ ] :=
  InfN[ #, acc ]&;

PackageExport["PMap"]

PMap::usage =
  "PMap[ f, list ] maps f to list and displays a progress bar while doing so.";

PMap[ f_, set_ ] :=
  Monitor[
    Table[
      f[set[[i]]],
      { i, Length[set] }
    ],
    ProgressIndicator[ i/Length[set] ]
  ];

column[i_Integer] :=
  #[[;;,i]]&;

row[i_Integer] :=
  #[[i]]&;

(* Returns a list of indices of arg subsets such that the subsets provide a cover
 of set *)
SubsetCover[ subsets_ ] :=
  With[ { mMat = CompiledListsOfIndicesToSparseArray @ subsets },
    Module[ { sol, decisions },
     sol =
      First @
      LinearOptimization[
        (Length /@ subsets) . decisions
        ,
        {
          VectorGreaterEqual[ mMat . decisions, 1.],
          VectorLessEqual[ 0., decisions ],
          VectorLessEqual[decisions, 1. ]
        }
        ,
        Element[ decisions, Vectors[ Length @ subsets, Integers ] ]
      ];
      Pick[ Range[Length[subsets]], Values @ sol, 1]
    ]
  ];

CompiledListsOfIndicesToSparseArray[ indices_ ] :=
  SparseArray @
  With[ { sList = MembershipList[ PadRight[indices], Length /@ indices ] },
    Thread[ Partition[ sList, 2 ] -> 1 ]
  ];

(* Create a vector of indices that who, taken two by two, represent
   arrayrules for the membership matrix.
   The output
   { a, b, c, d, ... }
   should be interpreted as
   { { a, b } -> 1, { c, d } -> 1, ... }
  *)

MembershipList =
  Compile[
    {
      { indices, _Integer, 2 }, (* matrix of elements per subset, padded with 0's *)
      { nIndices, _Integer, 1 } (* number of elements per subset*)
    },
    Module[{ i, j, bg = Internal`Bag[Most[{0}]] }, (* We use Internal`Bag for compiled quick storage *)
      For[ i = 1, i <= Length[indices], i++,
        For[ j = 1, j <= nIndices[[i]], j++,
          Internal`StuffBag[ bg, indices[[i, j]] ];
          Internal`StuffBag[ bg, i ];
        ]
      ];
      Internal`BagPart[ bg, All ]
    ]
    ,
    { { output, _Integer, 1 } }, (* Hint to compiler about the output form *)
    CompilationTarget -> "C",
    RuntimeOptions -> "Speed" (* All elements are small integers so no chance of overflow *)
  ];

PackageExport["TrimEquationList"]

TrimEquationList::usage =
  "Same as DeleteDuplicates @* DeleteCases[True].";

TrimEquationList =
  DeleteDuplicates @*
  DeleteCases[True];

PackageExport["TEL"]

TEL::usage =
  "Shorthand for TrimEquationList";

TEL =
  TrimEquationList;

PackageExport["TrimPolynomialList"]

TrimPolynomialList::usage =
  "Same as DeleteDuplicates @* DeleteCases[0].";

TrimPolynomialList =
  DeleteDuplicates @*
  DeleteCases[0];

PackageExport["TPL"]

TPL::usage =
  "Shorthand for TrimPolynomialList";

TPL =
  TrimPolynomialList;



(* Update expressions one by one, throwing { False }, if any do not satisfy testf *)

PackageScope["UpdateAndCheck"]

Options[UpdateAndCheck] :=
  {
    "SimplifyIntermediateResultsBy" -> Identity,
    "PreEqualCheck" -> Identity
  };

UpdateAndCheck[ {}, __ ] =
  { };

UpdateAndCheck[ exprList_List, sol_, testf_, OptionsPattern[] ] :=
  Module[ { newExpr, preEqCheck, simplify },
    If[ exprList === {}, Return @ {} ];
    preEqCheck =
      OptionValue["PreEqualCheck"];
    simplify =
      OptionValue["SimplifyIntermediateResultsBy"];

    Catch[
      Reap[
        Do[
          newExpr =
            simplify /@ ReplaceAll[ e, sol ];

          If[
            testf[ preEqCheck @ newExpr ] === False,
            Throw[ { False } ],
            Sow[ newExpr ]
          ]
          ,
          { e, exprList }
        ]
      ][[2,1]]
    ]
  ];

PackageScope["PowerDot"]

PowerDot[ a_, b_ ] :=
  If[ 
    MatchQ[ a, { 1 .. } ],
    ConstantArray[1,Length[b]],
    Inner[ Power, a, Transpose @ b, Times ]
  ];

PackageScope["ConsistentQ"]

Options[ConsistentQ] :=
  {
    "PreEqualCheck" -> Identity
  };

ConsistentQ[ r_, rhs_, test_, opts:OptionsPattern[] ] :=
	Module[ { FirstFailure, firstFail, check, procID, t, problemQ },
		check =
			OptionValue["PreEqualCheck"];
		FirstFailure[ l_ ] :=
      FirstCase[ l, x_ /; !test[ check @ x ] ];
    procID =
      ToString @ Unique[];
      
    printlog["CQ:init", { procID, rhs, r, test, {opts} }];
    
    { t, problemQ } =
      AbsoluteTiming[ r < Length[ rhs ] &&  !MissingQ[ firstFail = FirstFailure[ rhs[[r+1;;]] ] ] ];
    
    If[ problemQ, printlog["CQ:failed", { procID, rhs, r, test, firstFail }] ];
    
    printlog["Gen:results", {procID, !problemQ, t } ];
    
    !problemQ
	];

ConsistentQ[ eqns_, test_, opts:OptionsPattern[] ] :=
  Module[ { FirstFailure, firstFail, check, procID, t, problemQ },
    check =
      OptionValue["PreEqualCheck"];
    FirstFailure[ l_ ] :=
      FirstCase[ l, x_ /; !test[ check @ x ] ];
    procID =
      ToString @ Unique[];
    
    printlog["CQ:init", { procID, eqns, test, {opts} }];
    
    { t, problemQ } =
      AbsoluteTiming[ !MissingQ[ firstFail = FirstFailure[ eqns ] ] ];
    
    If[ problemQ, printlog["CQ:failed", { procID, eqns, test, firstFail }] ];
    
    printlog["Gen:results", {procID, !problemQ, t } ];
    
    !problemQ
  ];

PackageExport["ToRationalPhase"]

ToRationalPhase::usage =
  "ToRationalPhase[x] approximates x by the form (-1)^q where q is a rational number.\n"<>
  "ToRationalPhase[x,dx] approximates x by the form (-1)^q where q deviates at most dx from its actual value.";
 

ToRationalPhase[ z_, dz_ ] :=
  With[ { nz = InfN[ z, 100 ] },
    Which[
      nz == 1
      ,
      1
      ,
      nz == -1
      ,
      -1
      ,
      nz == I
      ,
      I
      ,
      nz == -I
      ,
      -I
      ,
      True
      ,
      (-1)^(Rationalize[Log[nz]/(I Pi),dz])
    ]
  ];
  
ToRationalPhase[ z_ ] :=
	ToRationalPhase[ z, 0 ];
	
PackageExport["ReplaceByKnownValue"]

ReplaceByKnownValue::usage = 
"ReplaceByKnownValues[n,vals] checks the list vals for the expression that numerically matches the number n and, if found, returns it."<>
" If no such expression exists then the number n is returned";
		
Options[ReplaceByKnownValue] = { "Accuracy" -> 128 };
ReplaceByKnownValue[n_,values_,OptionsPattern[]]:=
Module[{ fc },
	fc = FirstCase[ values, x_/; InfN[ x-n==0, OptionValue["Accuracy"] ] ];
	If[ MissingQ[fc], n,fc ]
]

PackageExport["EchoIn"]

EchoIn::usage = 
  "EchoIn[ n, label, function ][ code ] aplies EchoFunction[ label, function ] to"<> 
  " code, about once every n times it is called.\n"<> 
  "EchoIn[ n, label ][ code ] equals EchoIn[ n, label, Identity ][ code ]";

PackageScope["$EchoCounter"]

$EchoCounter = CreateDataStructure["Counter", 0];

SetAttributes[EchoIn, HoldAllComplete];

EchoIn[ n_Integer, label_, function_][ code_ ] :=
  (
   $EchoCounter["Increment"];
   If[ 
    Mod[$EchoCounter["Get"], n] == 0
    ,
    EchoFunction[label, function][code]
    ,
    code
    ]
   );

EchoIn[ n_Integer, label_ ][code_] := 
  EchoIn[ n, label, Identity, code ];

PackageExport["EchoPerformance"]

EchoPerformance::usage = 
"EchoPerformance[expr] prints the absolute number "<>
"of seconds and memory in bytes used to evaluate expr and returns expr.\n" <>
"EchoPerformance[expr,label] prepends label to a printed expression."

EchoPerformance := GeneralUtilities`EchoPerformance;

PackageExport["FailOnMessage"]

FailOnMessage::usage = 
"FailOnMessage[code] stops evaluation of code if a message occurs and returns $Failed. Author:Richard Hennigan (Wolfram Research)."

Attributes[FailOnMessage] = {HoldAllComplete}
 
FailOnMessage[eval_] := FailOnMessage[eval, $Failed]
 
FailOnMessage[eval_, default_] := FailOnMessage[eval, default, None]
 
FailOnMessage[eval_, default_, quiet_] := 
 FailOnMessage[eval, default, quiet, None]
 
FailOnMessage[eval_, default_, quiet_, All] := 
 FailOnMessage[eval, default, quiet, _]
 
FailOnMessage[eval_, default_, quiet_, {listen___}] := 
 FailOnMessage[eval, default, quiet, Alternatives[listen]]
 
FailOnMessage[eval_, default_, quiet_, listen_] := 
 Module[{$tag}, 
  Catch[If[FreeQ[Internal`Handlers["Message"], _msgThrow], 
    Internal`HandlerBlock[{"Message", 
      msgThrow[$tag, default, listen, ##1] & }, Quiet[eval, quiet]], 
    eval], $tag]]
 
FailOnMessage[___] := $Failed

Attributes[msgThrow] = {HoldAllComplete}
 
msgThrow[tag_, default_, listen_, 
   Hold[m : Message[msg_, ___], False]] /; 
  MatchQ[Unevaluated[msg], listen] := ($failedMessage = 
   HoldComplete[msg]; Throw[Unevaluated[m; default], tag])
 
msgThrow[tag_, default_, _, 
  Hold[msg_, True]] := ($failedMessage = HoldComplete[msg]; 
  Throw[default, tag])