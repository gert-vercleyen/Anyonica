(* ::Package:: *)

Package["Anyonica`"]

(*
+---------------------------------------------------------------------------+
|                                                                           |
|                             FINDING 0-VALUES                              |
|                                                                           |
+---------------------------------------------------------------------------+
*)

PackageExport["FindZeroValues"]

FindZeroValues::usage =
  "Returns a list of all possible configurations of zero values of vars, consistent with the rational equations eqns";

FindZeroValues::wrongequationsformat =
  "`1` must be a list of equations/inequalities.";

FindZeroValues::wrongvariablesformat =
  "`1` must be a list of variables of the form _Symbol[i__Integer].";

Options[ FindZeroValues ] :=
  Join[
    {
      "FindZerosBy" -> "Logic",
      "InvertibleMatrices" -> {}
    },
    Options[SumEqnsToProp],
    Options[BooleanZeroValues]
  ];

CheckArgs[ eqns_, vars_ ] :=
  Which[
    !ListQ[eqns]
    ,
    Message[ FindZeroValues::wrongequationsformat, eqns ];
    Abort[]
    ,
    !ListQ[vars]
    ,
    Message[ FindZeroValues::wrongvariablesformat, vars ];
    Abort[]
    ,
    Length[vars] === 0
    ,
    Message[ FindZeroValues::emptylistofvariables ];
    Abort[]
  ];

FindZeroValues[ eqns_, vars_, opts:OptionsPattern[] ] :=
(
  CheckArgs[eqns, vars];
  Module[
    {
      regMats, simpleEqns, simpleVars, revertVars, simpleMats, b, procID,
      result, absTime, proposition, knowns, equivs, reducedVars, AddExtraInfo,
      preProp, binEqns, sumEqns, trivialVars
    },
    regMats =
      OptionValue["InvertibleMatrices"];

    procID =
      ToString[Unique[]];

    printlog["FZV:init", {procID, eqns, vars, {opts}}];

    { absTime, result } =
      Catch @
      AbsoluteTiming[

        If[
          MemberQ[False] @ PermanentConditions @ regMats,
          printlog["FZV:non_invertible_matrix", { procID, regMats }];
          Throw @ { }
        ];

        trivialVars =
          Cases[ regMats, {{a_}} /; a =!= 0 :> a ];

        regMats =
          regMats ~ WithMinimumDimension ~ 2;

        If[
          Length[trivialVars] == Length[vars]
          ,
          printlog["FZV:all_vars_trivial", { procID } ];
          Throw @ { { } }
        ];

        { { simpleEqns, simpleMats }, simpleVars, revertVars } =
          SimplifyVariables[
            {
              eqns/.Dispatch[ Thread[ trivialVars -> 1 ] ],
              regMats
            },
            Complement[ vars, trivialVars ],
            b
          ];

        { binEqns, sumEqns } =
          BinomialSplit @
          simpleEqns;

        (* Convert the equations to a proposition *)
        preProp =
          And[
            EqnsToProp[ binEqns ],
            AddOptions[opts][SumEqnsToProp][ sumEqns, b ],
            MatsToProposition[ simpleMats ]
          ];

        printlog[ "FZV:preProp", { procID, preProp } ];

        (* Reduce the proposition using logic *)
        { proposition, knowns, equivs } =
          ReduceViaLogic[ preProp, b ];

        reducedVars =
          Complement[ simpleVars, First /@ knowns, First /@ equivs ];

        printlog[ "FZV:reduced_system", { procID, proposition, knowns, equivs, reducedVars } ];

        If[ (* If all variables are known *)
          Length[ reducedVars ] === 0
          , (* THEN: select all vars that point to False, replace False by 0 and return solution *)
          Return[ { Select[ knowns, Not @* Last ] /. revertVars /. False -> 0 } ]
        ];

        AddExtraInfo[ sol_ ] :=
          SortBy[First] @
          Select[ !Last[#] || Last[#] == 0 & ] @
          Join[ sol, knowns, equivs/.Dispatch[sol] ] /. revertVars /. False -> 0;

        Map[
          AddExtraInfo,
          Switch[ OptionValue["FindZerosBy"],
            "Logic",  AddOptions[opts][BooleanZeroValues],
            "Reduce", ZeroValuesFromReduce,
            "CCode",  CCodeZeroValues
          ] @@
          { proposition, reducedVars }
        ]

      ];

      printlog["FZV:solutions", {procID,result}];

      printlog["Gen:results", {procID, result, absTime}];

      result
  ]
);

Options[SumEqnsToProp] =
  {
    "SumSubsetFunction" -> "SolutionCount",
    "SumSubsetParameter" -> "Default"
  };

SumEqnsToProp[ { }, x_, opts:OptionsPattern ] :=
  True;

SumEqnsToProp[ eqns_, x_, opts:OptionsPattern[] ] :=
  With[
    {
      func = OptionValue["SumSubsetFunction"],
      par = OptionValue["SumSubsetParameter"],
      reducedEqns = DeleteCases[True] @ ReduceMonomials @ eqns
    },
    EqnsToProp @
    Switch[ func,
      "SolutionCount"
      ,
      SolutionCountEqns[ reducedEqns, x, par ]
      (*,
      "CoverAllVariables"
      ,*)
      ,
      "TermCount"
      ,
      TermCountEqns[ reducedEqns, par ]
    ]
  ];

SolutionCountEqns[ eqns_, x_, par_ ] :=
  With[{ p = If[ par === "Default", .2, par ] },
    MinimalBy[ eqns, CountSumSol[x], Ceiling[ Length[eqns] * p ] ]
  ];

CountSumSol[x_][ eqn_ ] :=
  With[
    {
    prop =
      NotOnlyOne @@
      Map[
        IntToBool,
        ( List @@ eqn ) /. Plus -> Sequence /. Times -> And
      ],
    vars =
      GetVariables[ eqn, x ]
    },
    SatisfiabilityCount[ prop, vars ]
  ];

TermCountEqns[ eqns_, par_ ] :=
  With[
    { p = If[ par === "Default", 3, par  ] },
    Select[
      eqns,
      nTerms[#] <= p &
    ]
  ];

nTerms[ sumEqn_ ] :=
  Length[ Flatten[( List @@ sumEqn )/.Plus -> List ] ];

(*
FindZeroValues[ eqns_, vars_, opts:OptionsPattern[] ] :=
(
  CheckArgs[eqns, vars];
  If[
    OptionValue["FindZerosBy"] =!= "Logic"
    ,
    Module[
      {
        regMats, simpleEqns, simpleVars, revertVars, simpleMats, s, procID,
        reducedEqns, result, absTime, proposition, knowns, equivs, reducedVars,
        AddExtraInfo, solutions
      },
      regMats =
        OptionValue["InvertibleMatrices"];

      procID =
        ToString[Unique[]];

      printlog["FZV:init", {procID, eqns, vars, {opts}}];

      { absTime, result } =
      AbsoluteTiming[

        { { simpleEqns, simpleMats }, simpleVars, revertVars } =
          SimplifyVariables[ { eqns, regMats }, vars, s ];

        (* Convert and reduce the equations using logic *)
        { proposition, knowns, equivs } =
          MapAt[
            BooleanConvert[ #, "CNF" ]&,
            ReduceViaLogic[
              EqnsToProp[simpleEqns] && MatsToProposition[simpleMats],
              s
            ],
            {1}
          ];

        reducedEqns =
          PropToEqns @ proposition;

        reducedVars =
          Complement[ simpleVars, knowns[[;;,1]], equivs[[;;,1]] ];

        If[ (* If all variables are known *)
          Length[ reducedVars ] === 0
          , (* THEN: select all vars that point to False, replace False by 0 and return solution *)
          Return[ { Select[ equivs, Not @* Extract[2] ] /. False -> 0 } ]
        ];

        AddExtraInfo[ sol_ ] :=
          SortBy[First] @
          Select[ #[[2]] === 0& ] @
          Join[
            sol,
            knowns,
            equivs /. Dispatch[sol] /. False -> 0
          ];

        solutions =
          If[
            OptionValue["FindZerosBy"] === "CCode"
            ,
            SolveDiophantineSystem[ reducedEqns, reducedVars, { 0, 1 } ]
            ,
            SolveUsingReduce[ Join[ reducedEqns, Thread[ 1 >= reducedVars >= 0 ] ], reducedVars, Integers ]
          ];

        Map[ AddExtraInfo, solutions ] /. revertVars
      ];

      printlog["FZV:solutions", {procID,result}];

      printlog["Gen:results", {procID, result, absTime}];

      result
    ]
    ,
    AddOptions[opts][BooleanZeroValues][ eqns, vars ]
  ]
);
*)

PropToEqns[ prop_And ] :=
  Map[
    Simplify,
    ( List @@ BooleanConvert[ prop, "CNF" ] ) /. HoldPattern[Or[x__]] :> ( Plus[x] >= 1 ) /. Not[a_] :> (1 - a)/.Equivalent->Equal
  ];

PropToEqns[ prop_Or ] :=
  Map[
    Simplify,
    { BooleanConvert[ prop, "CNF" ] } /. HoldPattern[Or[x__]] :> ( Plus[x] >= 1 ) /. Not[a_] :> (1 - a)/.Equivalent->Equal
  ];

PropToEqns[ True ] :=
  { True };

PropToEqns[ False ] :=
  { False };
(*
PackageExport["BinEqnsToProposition"]

BinEqnsToProposition[ eqns_ ] :=
  And @@ (
    Equivalent @@@
    Map[
      IntToBool,
      Join @@@ MonomialList[ List @@@ DeleteCases[True] @ ReduceMonomials @ eqns ] /.
      Times -> And,
      {2}
    ]
  );
*)

(*PackageExport["SumEqnsToProposition"] *)

(*SumEqnsToProposition[ eqns_ ] :=*)
(*  And @@ *)(* Eqns with powers or 1 don't provide info but sumEqnToProps does provide a proposition which it shouldn't do *)
(*  DeleteCases[ 1 == _ | _ == 1 | (eq_/;MemberQ[ eq, HoldPattern[Power[__] ] ] ) ] @*)
(*  sumEqnToProp[eqns];*)

PackageExport["EqnsToProp"]

EqnsToProp[ eqns_ ] :=
  And @@
  EqnToProp[
    DeleteCases[True] @
    ReduceMonomials @
    eqns
  ];

SetAttributes[EqnToProp,Listable];

EqnToProp[eqn_] :=
  If[
    MemberQ[ HoldPattern[ Plus[__] ] ] @ eqn
    ,
    NotOnlyOne @@
    Map[
      IntToBool,
      ( List @@ eqn )/.Plus -> Sequence/.Times -> And
    ]
    ,
    Map[
      IntToBool,
      Equivalent[ First @ eqn, Last @ eqn ]/.Times -> And
    ]
  ];

(* Returns True if exactly one of the arguments is True *)
OnlyOne[ args__ ] :=
 	Or @@
  Table[
  	And @@ MapAt[ Not, Not /@ { args }, { i } ], { i, Length @ {args} }
  ];

NotOnlyOne[ args__ ] :=
  BooleanMinimize @ Not @ OnlyOne[args] ;

IntToBool[1] :=
  True;

IntToBool[0] :=
  False;

IntToBool[x_] :=
  x;

PackageExport["BooleanZeroValues"]

Options[BooleanZeroValues] :=
  Options[SatisfiabilityInstances];

BooleanZeroValues[ prop_, remainingVars_, opts:OptionsPattern[] ] :=
  Module[
    { instances, procID, result, time },

    printlog["BZV:init",{procID,prop,remainingVars,{opts}}];

    { time, result } =
      AbsoluteTiming[
        instances =
          Catch[
            AddOptions[opts][SatisfiabilityInstances][
              prop,
              remainingVars,
              All
            ]
          ];

        If[
          !MatchQ[ instances, { { __?BooleanQ }.. } ]
          ,
          Abort[]
        ];

        Thread[ remainingVars -> # ]& /@
        instances

      ];
    result

  ];

ZeroValuesFromReduce[ prop_, reducedVars_ ] :=
  SolveUsingReduce[
    Join[
      PropToEqns @ prop,
      Thread[ 0 <= reducedVars <= 1 ]
    ],
    reducedVars,
    Integers
  ];

CCodeZeroValues[ prop_, reducedVars_ ] :=
  SolveDiophantineSystem[ PropToEqns @ prop, reducedVars, { 0, 1 } ];

(* Assumes single indexed vars x[i] *)
PackageExport["ReduceViaLogic"]

ReduceViaLogic[ proposition_, x_ ] :=
  Module[
    { UpdateKnowns, UpdateEquivalences, SimplifySystem, simplify1, simplify2, simplify3, simplify4, findEquiv },

    UpdateKnowns[ { prop_, knowns_, equivs_ } ] :=
      Switch[ Head[prop],
        And
        ,
        Module[{ newKnowns1, newKnowns2, newKnowns, newEquivs },
          newKnowns1 =
            ReplaceAll[
              Cases[ prop, x[_] | Not[x[_]] ],
              { x[i_] :> ( x[i] -> True ), Not[ x[i_] ] :> ( x[i] -> False ) }
            ];

          { newKnowns2, newEquivs } =
            BinSplit[ equivs/.Dispatch[newKnowns1], BooleanQ @* Extract[2] ];

          newKnowns =
            Join[ newKnowns1, newKnowns2 ];

          {
            prop/.Dispatch[ newKnowns ],
            Join[ knowns, newKnowns ],
            newEquivs
          }
        ]
        ,
        Or | Symbol,
        { prop, knowns, equivs }
        ,
        x
        ,
        { True, Append[prop->True] @ knowns, equivs/.prop->True }
      ];

    findEquiv[ prop_ ] :=
      FirstCase[
        prop,
        ( Equivalent[ x[i_], y__ ] | Equivalent[ y__, x[i_] ] ) /; FreeQ[ y, x[i] ] :> ( x[i] -> y )
      ];

    UpdateEquivalences[ { prop_, knowns_, equivs_ } ] :=
      Module[ { newEquivs, newProp, eq },
        newProp =
          prop;
        newEquivs =
          equivs;

        While[
          !MissingQ[ eq = findEquiv @ newProp ]
          ,
          newEquivs =
            Join[ newEquivs/.eq, {eq} ];

          newProp =
            newProp/.eq
        ];

        { newProp, knowns, newEquivs }
      ];


    SimplifySystem[ { prop_, knowns_, equivs_ } ] :=
      If[
        TrueQ @ prop
        ,
        { prop, knowns, equivs }
        ,
        With[ { newProp = prop/.{ And -> Union @* And, Or -> Union @* Or } },
          {
            If[
              Length[newProp] === 1,
              newProp,
              DeleteDuplicatesBy[Sort] @ newProp
            ],
            knowns,
            equivs
          }
        ]
      ];

    simplify1 =
      FixedPoint[ SimplifySystem @* UpdateEquivalences @* UpdateKnowns, # ]&;

    simplify2 = (* Converting to CNF often gives extra info about variables *)
      FixedPoint[ UpdateKnowns, { BooleanConvert[ #[[1]], "CNF" ], #[[2]], #[[3]] } ]&;

    simplify3 = (* Converting back to "DNF" can reduce the number of variables as well *)
      QuietCheck[
        With[
          { dnfForm = BooleanConvert[ #[[1]], "DNF" ] },
          { removeRedundantVars = Dispatch[ Thread[ (Intersection @@ (dnfForm/.{ And|Or->List })) -> True ] ] },
          {
            dnfForm /. removeRedundantVars,
            #[[2]],
            #[[3]] /. removeRedundantVars
          }
        ],
        #
      ]&;

    simplify4 = (* Often some equivalences remain but UpdateEquivalences doesn't work on "DNF"/"CNF" forms so we revert *)
      QuietCheck[
        simplify1 @
        {
          ReplaceRepeated[
            BooleanConvert[#[[1]],"CNF"],
            And[p1___, (! a_ || b_), (a_ || ! b_), p2___] :> And[ p1, Equivalent[ a, b ], p2 ]
          ],
          #[[2]],
          #[[3]]
        }
        ,
        #
      ]&;

      MapAt[
        BooleanMinimize
        ,
        { proposition, { }, { } } //
        simplify1 //
        simplify2 //
        simplify3 //
        simplify4
        ,
        { 1 }
      ]
  ];

PackageExport["MatsToProposition"]

MatsToProposition[ matList_ ] :=
  With[{ perms = ReduceMonomials @* Permanent /@ matList },
    And @@ ReplaceAll[ perms, { Times -> And, Plus -> Or } ]
  ];


ReduceMonomials[ expr_ ] :=
  expr //
  ReplaceAll[ Power[ a_, b_Integer ] :> a ] //
  ReplaceAll[ Times[ a_Integer, b__ ] :> Times[b] ];


PermanentConditions[ mats_List ] :=
  Map[
    ( Expand[Permanent[#]] != 0 )&,
    mats
  ];

(*
PropositionToCNFFile[ prop_, x_ ] :=
  With[{
    orProps = List @@ BooleanConvert[ prop, "CNF" ],
    nVars = CountVariables[ prop, x ]
    },
    StringJoin[
      "p "<>ToString[nVars] <> " " <> ToString[ Length @ orProps ] <>"\n",
      Sequence @@ Map[ OrToString[x], orProps ]
    ]
  ];


OrToString[ x_][x[i_]] :=
  ToString[i] <> " 0";

OrToString[ x_][Not[x[i_]]] :=
  ToString[-i] <> " 0";

OrToString[ x_ ][ prop_ ] :=
  StringJoin[ #, " 0\n" ]& @
  StringRiffle @
  ReplaceAll[ prop, { Or -> List, x[i_] :> ToString[i], Not[x[i_]] :> ToString[-i] } ];
*)