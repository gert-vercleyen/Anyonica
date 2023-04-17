(*
+---------------------------------------------------------------------------+
|                                                                           |
|                       GENERICALLY USEFUL FUNCTIONS                        |
|                                                                           |
+---------------------------------------------------------------------------+
*)

(* Check whether an expression is a list of equations *)
ListOfEquationsQ[ expr_ ] :=
  Head[expr] === List && MatchQ[ expr, { Repeated[_Equal|True|False ] } ];

(* Numerically check whether a number is a real integer with desired accuracy *)
NIntegerQ[ x_, accuracy_ ] :=
With[ { nx = N[ x, { Infinity, accuracy } ] },
  RealAbs[ Im[ nx ] ] == 0 && nx == Round[nx]
];

NIntegerQ[ x_ ] :=
  NIntegerQ[ x, $MachinePrecision ];

MonomialQ[ pol_ ] :=
  With[ {
    newPol = Cancel[Together[pol]] /. Power[ _, i_ ] /; i < 0 -> 1
    },
    Length[ MonomialList[ newPol ] ] === 1
  ];

BinomialEquationQ[True] :=
  True;
BinomialEquationQ[ eqn_Equal ] :=
  With[{
    newEqn = RemoveFractions[eqn]
    },
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
  ];

SetAttributes[ ToProperBinomialEquation, Listable ];
ToProperBinomialEquation[ True ] :=
  True;
ToProperBinomialEquation[ eqn_?BinomialEquationQ ] :=
  With[{
    noFracEqn = RemoveFractions @ eqn
    },
    If[
      TrueQ[ noFracEqn ],
      Return[ True ],
      With[{
        mList = MonomialList[ First @ noFracEqn ]
        },
        If[
          Length[mList] === 1,
          mList[[1]] == 0,
          mList[[1]] == -mList[[2]]
        ]
      ]
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

SetAttributes[ ToPolynomial, Listable ];
ToPolynomial[ eqn_Equal ] :=
  First @ RemoveFractions @ eqn;

PolynomialDegree[ n_?NumericQ, _ ] :=
  0;

PolynomialDegree[ pol_, vars_ ] :=
  Max[
    Total /@
    (
      ArrayRules[ CoefficientList[ pol, vars ] ][[;; -2, 1]] - 1
    )
  ];

PolynomialDegree[ pol_, s_Symbol ] :=
  PolynomialDegree[ pol, GetVars[ pol, s ] ];

BinomialSystemQ[ eqnList_List ] :=
  FirstCase[ eqnList, eqn_/; !MonomialEquationQ[ eqn ] -> False, True ];

(* Split list in two lists for which f is resp True and False *)
BinSplit[ l_List, f_ ] :=
  ReplaceAll[
    GroupBy[
      l,
      TrueQ @* f
    ] /@ { True, False },
    Missing[___] -> {}
  ];

Options[ BinomialSplit ] =
  {
    "PreEqualCheck" -> Identity
  };

BinomialSplit[ eqnList_List, OptionsPattern[] ] :=
  BinSplit[ eqnList, BinomialEquationQ @* OptionValue["PreEqualCheck"] ];

Options[GetVars] = {"LevelSpec" -> Infinity};
GetVars[ expression_, s_, OptionsPattern[] ] :=
  With[{ n = OptionValue["LevelSpec"] },
    Which[
      !IntegerQ[n] && n =!= Infinity,
        Message[GetVars::invalidlevelspec,n],
      True,
        Cases[ expression, s[__], n ] //
        DeleteDuplicates //
        Sort
    ]
  ];

GetVars[ expression_, s_, excludedVars_List, OptionsPattern[] ] :=
  With[{
    n = OptionValue["LevelSpec"]},
    If[ !IntegerQ[n] && n =!= Infinity,
      (* THEN *)
      Message[GetVars::invalidlevelspec,n],
      (* ELSE *)
      Cases[ expression, s[i__]/; FreeQ[excludedVars, s[i] ], n ] //
      DeleteDuplicates //
      Sort
    ]
  ];

GetVars[ expression_, symbols_List, opts:OptionsPattern[] ] :=
  Join @@ (GetVars[ expression, # , opts]& /@ symbols );

GetVars[ expression_, symbols_List, excludedVars_, opts:OptionsPattern[] ] :=
  Join @@ (GetVars[ expression, #, excludedVars, opts ]& /@ symbols );

Options[CountVars] =
 Options[GetVars];

CountVars[ data__ ] :=
  Length @ GetVars[ data ];

(* Replace all variables in eqns by single indexed vars in symbol s and return also the old list
 of variables together with a rule to revert the variables to their old forms*)
SimplifyVariables[ eqns_List, oldVars_List, s_ ] :=
  With[{ newVars = s /@ Range[ Length[ oldVars ] ] },
    With[{ r = Thread[ oldVars -> newVars ] },
      printlog["SV:subs", { ToString[Unique[]], r } ];
      {
        eqns/.Dispatch[r],
        newVars,
        Dispatch[ Reverse /@ r ]
      }
    ]
  ];

(* Replace all equivalent variables by a representative of the equivalence class *)
ReplaceByReps[ equivClasses_List, reps_, expr_ ] :=
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
    Message[ ReplaceByReps::repnotinclass, equivClasses, reps ]
  ];

(* Returns the list of equivalent elements to var (including var itself) *)
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

ValidSystemQ[ eqns_List ] :=
  !MemberQ[ eqns, False ];

CheckSystemValidity[ eqns_List ] :=
  If[
    !ValidSystemQ[ eqns ],
    Throw[{False}],
    eqns
  ];

(* ADDING OPTIONS TO FUNCTIONS WHERE NON-APPLICABLE OPTIONS ARE LEFT OUT *)
AddOptions[ opts:OptionsPattern[] ][ head_ ][ args___ ] :=
  head[ args, Sequence @@ FilterRules[ {opts}, Options[head] ] ];
AddOptions[][ head_ ][ args___ ] :=
  head[ args ];

(* REMOVING ZERO ROWS AND COLUMNS FROM MATRICES *)
RemoveZeroRows[ mat_?MatrixQ ] :=
  If[
    mat === {{}},
    {{}},
    With[{ newMat = DeleteCases[ Table[ 0, Length[mat[[1]]] ] ] @ mat },
      If[
        newMat === {},
        {{}},
        newMat
      ]
    ]
  ];

RemoveZeroColumns[ mat_?MatrixQ ] :=
  Module[{ r },
    If[
      mat === {{}},
      {{}},
      r = mat // Transpose // RemoveZeroRows;
      If[
        r === {{}},
        {{}},
        Transpose[r]
      ]
    ]
  ];


(* Simplify Exressions using RootApproximant *)
SimplifyUsingRoots[ expr_, acc_Integer ] :=
  ReplaceAll[
    expr,
    x_?NumericQ /; ! IntegerQ[x] :> RootApproximant[ N[x, { Infinity, acc } ] ]
  ];

SimplifyUsingRoots[ expr_ ] :=
  SimplifyUsingRoots[ expr, 128 ];

SimplifyUsingRoots[ expr_, variables_List, acc_Integer ] :=
  Block[{ x, simplerExpr, newVars, revertVars},
    {simplerExpr, newVars, revertVars} =
      SimplifyVariables[ expr, variables, x ];

    SetAttributes[x, NHoldAll];

    ReplaceAll[
      N[ expr, { Infinity, acc } ],
      x_?NumericQ :> RootApproximant[ N[x, { Infinity, acc } ] ]
    ] /. revertVars
  ];

SimplifyUsingRoots[ expr_, variables_List ] :=
  SimplifyUsingRoots[ expr, variables, 128 ];

(* Check whether a single solution is correct *)
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


(* Check whether a single solution is correct, given that no variables are allowed to be 0*)
NotInvalidNonZeroSolutionQ[ {}, _ ][ _ ] := True;
NotInvalidNonZeroSolutionQ[ eqns_, preEqCheck_ ][ soln_ ] :=
  With[
    {
      filledInEqns =
        Expand[ Map[ preEqCheck, eqns/.Dispatch[soln], {2} ] ]
    },
    (* TODO: If some equations are nested deeply might want to print a warning. *)
    FreeQ[
      filledInEqns,
      False | HoldPattern[ 0 == Times[__] ] | HoldPattern[ Times[__] == 0 ]
    ]
  ];

Options[WithDimension] = {"ColumnDimension" -> False};
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

WithMinimumDimension[ matList_List, k_Integer, opts:OptionsPattern[] ] :=
  WithDimension[ matList, { k, Infinity }, opts ];

WithMaximumDimension[ matList_List, k_Integer, opts:OptionsPattern[] ] :=
  WithDimension[ matList, { 0, k }, opts ];

MatrixDirectSum[ listOfMatrices_ ] :=
  With[
    { r = MapIndexed[#2[[1]] {1, 1} -> # &, DeleteCases[ listOfMatrices, {{}} ], 1]},
    SparseArray`SparseBlockMatrix[r]
  ];

ThreadMatrixEquality[ False ] =
  False;
ThreadMatrixEquality[ True ] =
  True;
ThreadMatrixEquality[ m1_?MatrixQ == m2_?MatrixQ ] :=
  Which[
    Dimensions[m1] != Dimensions[m2],
      Message[ ThreadMatrixEquality::dimensionsmismatch ];
      Return[ $Failed ],
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

(* Generate tuples from a list of ranges of the form
 { { 0, n1 }, { 0, n2 }, ..., { 0, nm } } one by one *)
NextTuple[ listOfRanges_ ][ tuple_]:=
  PositionPlus[ listOfRanges, tuple, Length[listOfRanges]];

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

SolveUsingReduce[ eqns_, vars_, rest___ ] :=
  With[{
    logicalExpression =
      LogicalExpand @
      EqualitiesToRules[
        Reduce[ eqns, vars, rest, Backsubstitution -> True ],
        vars
      ]
    },
    If[
      logicalExpression === False,
      Return @ { }
    ];

    Which[
      Head[ logicalExpression ] === Or, (* Multiple solutions, each with a single or multiple variables *)
        Map[
          If[ Head[#] === And, List @@ #, { # } ]& ,
          List @@ logicalExpression
        ],
      Head[ logicalExpression ] === And, (* Single solution in multiple variables *)
        { List @@ logicalExpression },
      True, (* Single solution in 1 variable *)
        logicalExpression
    ]
  ];

EqualitiesToRules[ expr_, vars_ ] :=
  ReplaceAll[
    expr,
    a_ == b_ :>
    Which[
      MemberQ[a] @ vars,
        a -> b,
      MemberQ[b] @ vars,
        b -> a,
      True,
        SortBy[ a -> b, ByteCount ]
    ]
  ];


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

PowerSumQ[ f_, x_ ] :=
  Module[
    { deg , lf, powers, gcd },
    deg =
    Exponent[ f, x ];
    lf =
    List @@ f;

    Quiet @
    Check[
      If[
        Not[
          lf[[1]] === 1 &&
          MatchQ[ Rest @ lf, { _. * Power[ x, _.] .. } ] ],
        Return @ False
      ],
      Return @ False
    ];

    powers =
      Cases[ Rest @ lf, _. * Power[ x, i_. ] :> i ];

    gcd =
      GCD @@ powers;

    If[
      Not[ powers / gcd == Range[ deg / gcd ] ],
      Return @ False
    ];

    TrueQ[ Sum[ (lf[[2]])^i, { i, 0, deg / gcd } ] === f ]
  ];

QuietCheck[ code_, failExpr_, messages_List ] :=
  Quiet[
    Check[
      code,
      failExpr,
      messages
    ],
    messages
  ];

QuietCheck[ code_, failExpr_ ] :=
  Quiet @ Check[ code, failExpr ];

(* Roots with Exact root isolation result in errors for RootReduce.
   Numeric isolation should be fine since the roots are still isolated
   exactly
*)
ToNumericRootIsolation[ expr_ ] :=
  ReplaceAll[
    expr,
    Root[ f_, n_, 1 ] :> Root[ f, n, 0 ]
  ];

SafeRootReduce =
  Function[
    { polynomial },
    QuietCheck @
    RootReduce @
    polynomial
  ];


(* Approximate value with accuracy acc and infinite precision *)
InfN[ expr_, acc_ ] :=
  N[ expr, { Infinity, acc } ];
InfN[ acc_ ] :=
  InfN[ #, acc ]&;