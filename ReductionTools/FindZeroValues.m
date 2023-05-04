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
      "Method" -> "CCode"
    },
    Options[BooleanZeroValues]
  ];

CheckArgs[ eqns_, vars_ ][ code_ ] :=
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
    True
    ,
    code
  ];

FindZeroValues[ eqns_, vars_, opts:OptionsPattern[] ] :=
  CheckArgs[eqns,vars] @
  If[
    OptionValue["Method"] === "CCode"
    ,
    Module[
      {
      regMats, simpleEqns, simpleVars, revertVars, simpleMats, s, procID,
      reducedEqns, result, absTime, proposition, knowns, equivs, reducedVars,AddEquivalences
      },

      procID =
        ToString[Unique[]];

      printlog["FZV:init", {procID,eqns,vars,{opts}}];

      { absTime, result } =
      AbsoluteTiming[
        regMats =
          OptionValue["InvertibleMatrices"];

        { { simpleEqns, simpleMats }, simpleVars, revertVars } =
          SimplifyVariables[ { eqns, regMats }, vars, s ];

        (* Convert and reduce the equations using logic *)
        { proposition, knowns, equivs } =
          ReduceViaLogic[
            BinEqnsToProposition[ simpleEqns ] && MatsToProposition[simpleMats],
            s
          ];

        reducedEqns =
          PropositionToEqns @ proposition;

        reducedVars =
          GetVariables[ reducedEqns, s ];

        If[ (* If all variables are known *)
          Length[ reducedVars ] === 0
          , (* THEN: select all vars that point to False, replace False by 0 and return solution *)
          Return[ { Select[ equivs, Not @* Extract[2] ]/.False -> 0 } ]
        ];

        AddEquivalences[ sol_ ] :=
          SortBy[First] @
          Select[ #[[2]] === 0& ] @
          Join[
            sol,
            equivs/.Dispatch[sol]/.False -> 0
          ];

        Map[
          AddEquivalences,
          SolveDiophantineSystem[ reducedEqns, reducedVars, { 0, 1 } ]
        ]/.revertVars
      ];

(*      printlog["FZV:solutions", {procID,result}];*)

      printlog["Gen:results", {procID,result,absTime}];

      result
    ]
    ,
    BooleanZeroValues[ eqns, vars, opts ]
  ];

PackageExport["BooleanZeroValues"]

BooleanZeroValues::usage =
  "BooleanZeroValues[binEqns,vars] tries to find admissible sets of 0 values for the variables vars using logic.";

BooleanZeroValues::mallocfailure =
  "From SatfisfiabilityInstances: Not enough free memory to perform computation.";

Options[BooleanZeroValues] :=
  Join[
    {
      "InvertibleMatrices" -> { }
    },
    Options[SatisfiabilityInstances]
  ];

PropositionToEqns[ prop_ ] :=
  FixInvertibilityCondition /@
  ReplaceAll[
    List @@ prop,
    {
      And -> Times,
      Or -> Plus,
      Equivalent -> Equal
    }
  ];

(* The invertibility of a matrix has a different form than a binomial constraint *)
FixInvertibilityCondition[ x_Equal ] :=
  x;

FixInvertibilityCondition[ x_ ] :=
  x != 0;

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

IntToBool[1] :=
  True;

IntToBool[0] :=
  False;

IntToBool[x_] :=
  x;

BooleanZeroValues[ eqns_, vars_, opts:OptionsPattern[] ] :=
  Module[
    { regMats, trueVars, newEqns, newRegMats, newVars, revertVars, x, knowns, equivs, remainingVars,
      AddEquivalences, proposition, instances
    },

    regMats =
      Normal @ OptionValue["InvertibleMatrices"];

    If[
      MemberQ[ PermanentConditions @ regMats, False ] || vars === {},
      Return[ { } ]
    ];

    trueVars =
      Cases[ regMats, {{a_}} /; a =!= 0 :> a ];

    regMats =
      regMats ~ WithMinimumDimension ~ 2;

    If[
      Length[trueVars] == Length[vars],
      Return[ { { } } ]
    ];

    { { newEqns, newRegMats }, newVars, revertVars } =
      SimplifyVariables[
        {
          eqns/.Dispatch[ Thread[ trueVars -> 1 ] ],
          regMats ~ WithMinimumDimension ~ 2
        },
        vars,
        x
      ];

    { proposition, knowns, equivs } =
      ReduceViaLogic[
        BinEqnsToProposition[newEqns] && MatsToProposition[newRegMats],
        x
      ];

    remainingVars =
      GetVariables[ proposition, x ];

    instances =
      Thread[ remainingVars -> # ]& /@
      Catch[
        AddOptions[opts][SatisfiabilityInstances][ proposition, remainingVars, All ],
        Message[BooleanZeroValues::mallocfailure];
        Abort[]
      ];

    (* Add the variables from the equivalences that are False *)
    AddEquivalences[ sol_ ] :=
      SortBy[First] @
      Join[
        sol,
        Select[ equivs/.Dispatch[sol], Not @* Extract[2] ]
      ];

    Map[ AddEquivalences, instances ]/. revertVars /. False -> 0
  ];

(* Assumes single indexed vars x[i] *)
ReduceViaLogic[ proposition_, x_ ] :=
  Module[{ UpdateKnowns, UpdateEquivalences, SimplifySystem, s, k, e },

    UpdateKnowns[ { sys_, knowns_, equivs_ } ] :=
      With[{ newKnowns = Cases[ sys, x[i_] :> ( x[i] -> True ) ] },
        {
          sys/.Dispatch[newKnowns],
          Join[ knowns, newKnowns ],
          equivs/.Dispatch[newKnowns]
        }
      ];

    UpdateEquivalences[ { system_, knowns_, equivs_ } ] :=
      Module[ { findEquiv, newEquivs, newSystem, eq },
        findEquiv =
          FirstCase[
            #,
            ( Equivalent[ x[i_], y__ ] | Equivalent[ y__, x[i_] ] ) /; FreeQ[ y, x[i] ] :> ( x[i] -> y )
          ]&;

        newSystem =
          system;

        newEquivs =
          equivs/.Dispatch[knowns];

        While[
          !MissingQ[ eq = findEquiv @ newSystem ]
          ,
          newEquivs =
            Join[ newEquivs/.eq, {eq} ];

          newSystem =
            newSystem/.eq
        ];

        { newSystem, knowns, newEquivs }
      ];

    SimplifySystem[ { system_, knowns_, equivs_ } ] :=
      {
        DeleteDuplicatesBy[
          system /. { And -> Union @* And, Or -> Union @* Or },
          Sort
        ],
        knowns,
        equivs
      };

    { s, k, e } =
      FixedPoint[
        SimplifySystem @* UpdateEquivalences @* UpdateKnowns,
        { proposition, { }, { } }
      ];

    {
      s,
      k ~ Join ~ Select[ e, TrueQ @* Extract[2] ],
      Select[ e, Not @* TrueQ @* Extract[2] ]
    }

  ];


ReduceMonomials[ expr_ ] :=
  expr //
  ReplaceAll[ Power[ a_, b_Integer ] :> a ] //
  ReplaceAll[ Times[ a_Integer, b__ ] :> Times[b] ];

MatsToProposition[ matList_ ] :=
  With[{ perms = ReduceMonomials @* Permanent /@ matList },
    And @@ ReplaceAll[ perms, { Times -> And, Plus -> Or } ]
  ];

PermanentConditions[ mats_List ] :=
  Map[
    ( Expand[Permanent[#]] != 0 )&,
    mats
  ];