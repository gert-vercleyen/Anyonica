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
      "InvertibleMatrices" -> {},
      "Equivalences" -> {}
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
      preProp, binEqns, sumEqns, trivialVars, eRules, reducedMats, reducedEqns,
      simpleERules, remainingVars, FilterSolutions
    },
    regMats =
      OptionValue["InvertibleMatrices"];

    eRules =
      OptionValue["Equivalences"];

    procID =
      ToString[Unique[]];

    printlog["FZV:init", {procID, eqns, vars, {opts}}];

    { absTime, result } =
      AbsoluteTiming[
      Catch[
        { { simpleEqns, simpleMats, simpleERules }, simpleVars, revertVars } =
          SimplifyVariables[
            { eqns, regMats, eRules },
            vars,
            b
          ];

        (* We can use a reduced set of variables by using tetrahedral equivalences *)
        reducedMats =
          simpleMats/.Dispatch[simpleERules];

        trivialVars =
          Cases[ reducedMats, {{a_}} /; a =!= 0 :> a ];

        reducedMats =
          reducedMats ~ WithMinimumDimension ~ 2;

        If[
          MemberQ[False] @ PermanentConditions @ reducedMats,
          printlog["FZV:non_invertible_matrix", { procID, reducedMats }];
          Throw @ { }
        ];

        reducedVars =
          Union[ Complement[ simpleVars/.Dispatch[simpleERules], trivialVars ] ];

        If[
          Length[trivialVars] == Length[reducedVars]
          ,
          printlog["FZV:all_vars_trivial", { procID } ];
          Throw @ { { } }
        ];

        reducedEqns =
          DeleteDuplicates @*
          DeleteCases[True] /@
          ( simpleEqns/.Dispatch[simpleERules] );

        { binEqns, sumEqns } =
          BinomialSplit @
          reducedEqns;

        (* Convert the equations to a proposition *)
        preProp =
          And[
            EqnsToProp[ binEqns ],
            AddOptions[opts][SumEqnsToProp][ sumEqns, b ],
            MatsToProposition[ reducedMats ]
          ]/.Thread[trivialVars->True]/.b[i_]:>i;

        printlog[ "FZV:preProp", { procID, preProp } ];

        (* Reduce the proposition using logic *)
        { proposition, knowns, equivs } =
          ReduceViaLogic[ preProp ]/.i_Integer :> b[i];

        remainingVars =
          Complement[ reducedVars, First /@ knowns, First /@ equivs ];

        printlog[ "FZV:reduced_system", { procID, proposition, knowns, equivs, reducedVars } ];

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

        FilterSolutions[ soln_ ] :=
          Select[
            soln,
            FreeQ[ sumEqns/.Dispatch[#], False  | 0 == HoldPattern[Times[__]] | HoldPattern[Times[__]] == 0 ]&
          ];

        AddExtraInfo[ sol_ ] :=
          With[ { knownsFromEquivs = Select[ equivs/.Dispatch[sol], BooleanQ @* Last ] },
            SortBy[First] @
            Select[ Last[#] == 0 & ] @
            ReplaceAll[
              Join[
                sol,
                knowns,
                simpleERules/.knowns/.Dispatch[sol]/.knownsFromEquivs,
                equivs/.Dispatch[sol]
              ]/.False -> 0,
              revertVars
            ]
          ];

        Map[
          AddExtraInfo,
          FilterSolutions[
            Switch[ OptionValue["FindZerosBy"],
              "Logic",  AddOptions[opts][BooleanZeroValues],
              "Reduce", ZeroValuesFromReduce,
              "CCode",  CCodeZeroValues
            ] @@
            { proposition, remainingVars }
          ]
        ]
      ]
      ];

      printlog["FZV:solutions", {procID,result}];

      printlog["Gen:results", {procID, result, absTime}];

      result
  ]
);


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

ReduceViaLogic[ proposition_ ] :=
  Module[
    { UpdateKnowns, UpdateEquivalences, SimplifySystem, simplify1, simplify2, simplify3, simplify4, findEquiv,
      TransferKnowns, FindEquivs, RepeatedUpdateEquivalences
    },

    (* Transfer known values from equivs to knowns *)
    TransferKnowns[ { prop_, knowns_, equivs_ } ] :=
      Module[ { newKnowns, newEquivs },
        { newKnowns, newEquivs } =
          BinSplit[ equivs, BooleanQ @* Last ];

        { prop, Join[ knowns, newKnowns ], newEquivs }
      ];

    UpdateKnowns[ { prop_, knowns_, equivs_ } ] :=
      TransferKnowns @
      Switch[ Head[prop],
        And
        ,
        With[{
          newKnowns =
            DeleteDuplicates @
            ReplaceAll[
              Cases[ prop, _Integer | Not[_Integer] ],
              { i_Integer :> ( i -> True ), Not[ i_Integer ] :> ( i -> False ) }
            ]
          },

          {
            prop/.Dispatch[newKnowns],
            Join[ knowns, newKnowns ],
            equivs/.Dispatch[newKnowns]
          }
        ]
        ,
        Or | Symbol
        ,
        { prop, knowns, equivs }
        ,
        Integer
        ,
        { True, Append[ prop -> True ] @ knowns, equivs/.prop -> True }
      ];

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
        { prop/.Dispatch[newEquivs], knowns, Join[ equivs/.Dispatch[newEquivs], newEquivs ] }
      ];

    RepeatedUpdateEquivalences =
      FixedPoint[ UpdateEquivalences, # ]&;

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

    simplify4 = (* Often some equivalences remain but UpdateEquivalences doesn't work on "DNF"/"CNF" forms so we revert *)
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