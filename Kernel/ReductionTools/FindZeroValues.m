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
  "FindZeroValues[eqns,vars] returns a list of all possible configurations of zero values of vars, consistent with " <>
  "polynomial equations eqns";

FindZeroValues::wrongequationsformat =
  "`1` must be a list of equations/inequalities.";

FindZeroValues::wrongvariablesformat =
  "`1` must be a list of variables of the form _Symbol[i__Integer].";

Options[ FindZeroValues ] :=
  Join[
    {
      "FindZerosBy" -> "Logic",
      "Constraint" -> True,
      "Equivalences" -> {}
    },
    Options[SumEqnsToProp],
    Options[BooleanZeroValues],
    Options[ReduceViaLogic]
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
      simpleEqns, simpleVars, revertVars, simpleConstraint, b, procID,
      result, absTime, proposition, knowns, equivs, reducedVars, AddExtraInfo,
      preProp, binEqns, sumEqns, trivialVars, eRules, reducedMats, reducedEqns,
      simpleERules, remainingVars, FilterSolutions, reducedConstraint, trivialVarsFromEquivalences,
      properEquivalences, allsatSolutions
    },
    constraints =
      OptionValue["Constraint"];

    eRules =
      OptionValue["Equivalences"];

    procID =
      ToString[Unique[]];

    printlog["FZV:init", {procID, eqns, vars, {opts}}];

    { absTime, result } =
      AbsoluteTiming[
      Catch[
        (* Write al variables as b[1], ..., b[n] *)
        { { simpleEqns, simpleConstraint, simpleERules }, simpleVars, revertVars } =
          SimplifyVariables[ { eqns, constraint, eRules }, vars, b ];

        (* Apply tetrahedral equivalences *)

        (* First, split the tetrahedral equivalences in (1) rules that demand an F-symbol to be non
           zero and (2) rules that express F-symbols in terms of other F-symbols *)
        { trivialVarsFromEquivalences, properEquivalences } =
          BinSplit[ simpleERules, #[[2]] === 1& ];

        (* Use equivalences to update the equations, variables and constraint *)
        reducedConstraint =
          simpleConstraint/.Dispatch[simpleERules];

        If[
          reducedConstraint === False,
          printlog["FZV:constraint_not_met", { procID, reducedMats }];
          Throw @ { }
        ];

        reducedVars =
          simpleVars/.Dispatch[properEquivalences];

        reducedEqns =
          TEL /@ ( simpleEqns/.Dispatch[properEquivalences] );

        { binEqns, sumEqns } =
          BinomialSplit @
          reducedEqns;

        (* Convert the equations to a proposition and replace
          b[i] by i since it takes less space *)
        preProp =
          And[
            EqnsToProp @ binEqns ,
            AddOptions[opts][SumEqnsToProp][ sumEqns, b ],
            reducedConstraint
          ]/.b[i_]:>i;

        printlog[ "FZV:preProp", { procID, preProp } ];

        (* Reduce the proposition using logic and revert vars to b[i] *)
        { proposition, knowns, equivs } =
          ReduceViaLogic[ preProp ]/.i_Integer :> b[i];

        remainingVars =
          Complement[ reducedVars, First /@ knowns, First /@ equivs ];

        printlog[ "FZV:reduced_system", { procID, proposition, knowns, equivs, remainingVars } ];

        If[ (* If all variables are known *)
          Length[ remainingVars ] === 0
          , (* THEN: select all vars that point to False, replace False by 0 and return solution *)
          Throw[
            {
              SortBy[First] @
              ReplaceAll[
                Select[ Join[ knowns, simpleERules/.knowns ], Not @* Last ],
                revertVars
              ]/.False -> 0
            }
          ]
        ];

        (* Check compatibility of solutions with all other equations *)
        FilterSolutions[ soln_ ] :=
          Select[
            soln,
            FreeQ[ 
              sumEqns/.Dispatch[#], 
              False  | 
              0 == HoldPattern[Times[__]] | 
              HoldPattern[Times[__]] == 0 |
              0 == Power[ a_, b_. ] |
              Power[ a_, b_. ] == 0
            ]&
          ];

        AddExtraInfo[ sol_ ] :=
          SortBy[First] @
          Select[ Last[#] == 0 & ] @
          (
            Join[
              sol,
              knowns,
              properEquivalences/.Dispatch[Join[knowns,equivs]]/.Dispatch[sol],
              equivs/.Dispatch[sol]
            ]/.False -> 0
          );


        allsatSolutions = 
          Map[
            AddExtraInfo,
            Switch[ OptionValue["FindZerosBy"],
              "Logic", AddOptions[opts][BooleanZeroValues],
              "Reduce", ZeroValuesFromReduce,
              "CCode",  CCodeZeroValues
            ] @@
            { proposition, remainingVars }
          ];

        printlog["FZV:allsat_solutions", { procID, allsatSolutions } ];

        ReplaceAll[ 
          FilterSolutions @ allsatSolutions,
          revertVars
        ]
      ]
      ];

      printlog["FZV:solutions", {procID,result}];

      printlog["Gen:results", {procID, result, absTime}];

      result
  ]
);

PackageExport["FZV"]

FZV::usage =
  "Shorthand for FindZeroValues.";

FZV =
  FindZeroValues;

Options[SumEqnsToProp] =
  {
    "SumSubsetFunction" -> "LeafCount",
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
      "LeafCount"
      ,
      LeafCountEqns[ reducedEqns, par ]
      ,
      "SolutionCount"
      ,
      SolutionCountEqns[ reducedEqns, x, par ]
      ,
      "TermCount"
      ,
      TermCountEqns[ reducedEqns, par ]
    ]
  ];

LeafCountEqns[ eqns_, par_ ] :=
  With[ { p = If[ par === "Default", .3, par ] },
    MinimalBy[ eqns, LeafCount, Ceiling[ Length[eqns] * p ] ]
  ];

SolutionCountEqns[ eqns_, x_, par_ ] :=
  With[{ p = If[ par === "Default", .3, par ] },
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

PackageScope["EqnsToProp"]

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

PackageScope["BooleanZeroValues"]

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

Options[ReduceViaLogic] = 
  {
    "LevelOfSimplification" -> 1
  };

ReduceViaLogic[ proposition_, OptionsPattern[] ] :=
  Module[
    { UpdateKnowns, UpdateEquivalences, SimplifySystem, simplify1, simplify2, simplify3, simplify4,
      TransferKnowns, FindEquivs, RepeatedUpdateEquivalences, lSymp, combinedSimplification 
    },

    (* TODO: implement error message if lSymp > 4 *)
    lSymp =
      Min[ 4, OptionValue["LevelOfSimplification"] ];

    (* Transfer known values from equivs to knowns *)
    TransferKnowns[ { prop_, knowns_, equivs_ } ] :=
      Block[ { newKnowns, newEquivs },
        { newKnowns, newEquivs } =
          BinSplit[ equivs, BooleanQ @* Last ];

        { prop, Join[ knowns, newKnowns ], newEquivs }
      ];

    UpdateKnowns2[ { prop_And, knowns_, equivs_ } ] :=
      TransferKnowns @ 
      With[
        {
          newKnowns =
          DeleteDuplicates @
          ReplaceAll[ { i_Integer :> (i -> True), Not[i_Integer] :> (i -> False) } ] @
          Cases[ prop, _Integer | Not[_Integer] ]
        },
        {
          prop /. Dispatch[newKnowns], 
          Join[ knowns, newKnowns ], 
          equivs /. Dispatch[newKnowns]
        }
      ]; 
    UpdateKnowns2[ { prop_Symbol, knowns_, equivs_ } ] :=
      TransferKnowns @ 
      { prop, knowns, equivs };
    UpdateKnowns2[ { prop_Or, knowns_, equivs_ } ] :=
      TransferKnowns @ 
      { prop, knowns, equivs };  
    UpdateKnowns2[ { prop_Integer, knowns_, equivs_ } ] := 
      TransferKnowns @ 
      { 
        True, 
        Append[ prop -> True ] @ knowns, 
        equivs /. prop -> True
      };

    (* Find disjunct equivalences *)
    FindEquivs[prop_] :=
      DeleteDuplicates[
        Cases[
          prop,
          (Equivalent[i_Integer, y_] | Equivalent[y_, i_Integer]) /; FreeQ[y, i] :> i -> y
        ],
        IntersectingQ[ Cases[ #1, _Integer, Infinity ], Cases[ #2, _Integer, Infinity ] ] &
      ];

    UpdateEquivalences[ { prop_, knowns_, equivs_ } ] :=
      With[ { newEquivs = FindEquivs @  prop },
        { 
          prop/.Dispatch[newEquivs], 
          knowns, 
          Join[ equivs/.Dispatch[newEquivs], newEquivs ] 
        }
      ];

    RepeatedUpdateEquivalences =
      FixedPoint[ UpdateEquivalences, # ]&;

    (* Get rid of duplicate statements at the first level *)
    SimplifySystem[ { prop_, knowns_, equivs_ } ] :=
      If[
        TrueQ @ prop
        ,
        { prop, knowns, equivs }
        ,
        With[ { newProp = prop/.{ And -> Union @* And, Or -> Union @* Or } },
          {
            If[
              Length[newProp] <= 1,
              newProp,
              Quiet[
                DeleteDuplicatesBy[Sort] @ newProp,
                Sort::normal
              ]
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

    (* Often some equivalences remain but UpdateEquivalences doesn't work on "DNF"/"CNF" forms so we revert *)
    simplify4 = 
      QuietCheck[
        simplify1 @
        {
          ReplaceRepeated[
            BooleanConvert[ #[[1]],"CNF"],
            And[ p1___, (! a_ || b_), (a_ || ! b_), p2___ ] :> And[ p1, Equivalent[ a, b ], p2 ]
          ],
          #[[2]],
          #[[3]]
        }
        ,
        #
      ]&;

      combinedSimplify = 
        Composition @@ 
        { simplify4, simplify3, simplify2, simplify1 }[[-lSymp;;]];

      MapAt[
        BooleanMinimize
        ,
        combinedSimplify @ { proposition, { }, { } } 
        ,
        { 1 }
      ]
  ];
