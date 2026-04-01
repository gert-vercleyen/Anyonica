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
      "InvertibleMatrices" -> {},
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
      regMats, simpleEqns, simpleVars, revertVars, simpleMats, b, procID,
      result, absTime, proposition, knowns, equivs, reducedVars, AddExtraInfo,
      preProp, binEqns, sumEqns, trivialVars, eRules, reducedMats, reducedEqns,
      simpleERules, remainingVars, FilterSolutions, trivialVarsFromMats, trivialVarsFromEquivalences,
      properEquivalences, allsatSolutions
    },
    regMats = OptionValue["InvertibleMatrices"];
    eRules  = OptionValue["Equivalences"];

    procID = ToString @ Unique[];

    printlog["FZV:init", {procID, eqns, vars, {opts}}];

    { absTime, result } =
      AbsoluteTiming[
      Catch[
        { { simpleEqns, simpleMats, simpleERules }, simpleVars, revertVars } =
          SimplifyVariables[ { eqns, regMats, eRules }, vars, b ];

        (* We can use a reduced set of variables by using (tetrahedral) equivalences *)
        (* First we split the tetrahedral equivalences in (1) rules that demand an F-symbol to be non
           zero and (2) rules that express F-symbols in terms of other F-symbols *)

        { trivialVarsFromEquivalences, properEquivalences } = 
          BinSplit[ simpleERules, Last[#] === 1& ];

        reducedMats = simpleMats/.Dispatch[simpleERules];

        trivialVars = 
          Union @ 
          Join[
            Cases[ reducedMats, {{a_}} /; a =!= 1 :> a ],
            Keys @ trivialVarsFromEquivalences
          ];

        replaceTrivialVars = ReplaceAll[ Dispatch @ Thread[ trivialVars -> 1 ] ];

        reducedMats = 
          WithMinimumDimension[ reducedMats, 2 ] // 
          replaceTrivialVars;

        reducedERules = MapAt[ replaceTrivialVars, simpleERules, {All,2} ];

        If[
          MemberQ[False] @ PermanentConditions @ reducedMats,
          printlog["FZV:non_invertible_matrix", { procID, reducedMats }];
          Throw @ { }
        ];

        reducedVars = 
          DeleteCases[1] @ Union @ Values @ reducedERules;

        If[ 
          Length[reducedVars] === 0
          ,
          printlog["FZV:all_vars_trivial", { procID } ];
          Throw @ { { } }
        ];

        If[ 
          OptionValue["FindZerosBy"] == "JuliaPicoSAT"
          ,
          Print["Not implemented yet."]; Abort[];
        ];

        { binEqns, sumEqns } =  BinomialSplit @ simpleEqns;

        dEquiv = Dispatch @ properEquivalences;
        (* Convert the equations to a proposition *)
        preProp = EchoFunction["PreProp length", Length] @  
          And[
            EqnsToProp[ ReplaceTrivialVars[ binEqns/.dEquiv] ],
            AddOptions[opts][SumEqnsToProp][ sumEqns, properEquivalences, trivialVars, b ],
            MatsToProposition[b] @ reducedMats/.dEquiv
          ]/.b[i_]:>i;

        If[ preProp =!= True, preProp = DeleteDuplicates[preProp] ];

        printlog[ "FZV:preProp", { procID, preProp } ];

        (* Reduce the proposition using logic *)
        { proposition, knowns, equivs } = 
          AddOptions[opts][ReduceViaLogic][ preProp ]/.i_Integer :> b[i];

        remainingVars =
          Complement[ reducedVars, First /@ knowns, First /@ equivs ];

        printlog[ "FZV:reduced_system", { procID, proposition, knowns, equivs, remainingVars } ];

        AddExtraInfo[ sol_ ] :=
          SortBy[First] @
          With[
            { rules = 
              Dispatch @ 
                Join[ 
                  sol,
                  knowns,
                  MapAt[ ReplaceAll[Dispatch @ sol], equivs, { All, 2 } ]  
                ]
            }, 
            Select[ Not @* Last ] @ 
            MapAt[ 
              ReplaceAll[rules], 
              Join[ 
                knowns,
                properEquivalences
              ], 
              { All, 2 } 
            ]
          ]/.False -> 0;

        If[ (* If all variables are known *)
          Length[ remainingVars ] === 0
          , (* THEN: solution is trivial *) 
          Throw @ AddExtraInfo[{}]
        ];

        FilterSolutions[ soln_ ] :=
          Select[
            soln,
            FreeQ[ 
              sumEqns/.Dispatch[#], 
              False  | 
              0 == HoldPattern[Times[__]] | 
              HoldPattern[Times[__]] == 0 |
              0 == Power[ a_, b_ ] |
              Power[ a_, b_ ] == 0
            ]&
          ];


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

      EchoLabel["zerovalsresult"] @ result
  ]
);

MatrixOfOnesQ[ m_ ] := MatchQ[ m, { { 1 .. } .. } ];

PackageExport["FZV"]

FZV::usage =
  "Shorthand for FindZeroValues.";

FZV = FindZeroValues;

Options[SumEqnsToProp] =
  {
    "SumSubsetFunction" -> "LeafCount",
    "SumSubsetParameter" -> "Default"
  };

SumEqnsToProp[ { }, equivs_, trivVars_,  x_, opts:OptionsPattern ] := True;

SumEqnsToProp[ eqns_, equivs_, trivVars_, x_, opts:OptionsPattern[] ] :=
  With[
    {
      func = OptionValue["SumSubsetFunction"],
      par  = OptionValue["SumSubsetParameter"],
      simplifications = 
        Dispatch[ Join[ Thread[ trivVars -> True ], equivs/. Thread[ trivVars -> True ] ] ],
      reducedEqns  = DeleteCases[True] @ ReduceMonomials @ eqns
    },
    EqnsToProp[
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
      ],
      simplifications
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

PropToEqns[ True ] := { True };

PropToEqns[ False ] := { False };

PackageScope["EqnsToProp"]

EqnsToProp[ eqns_ ] :=
  And @@
  EqnToProp[
    DeleteCases[True] @
    ReduceMonomials @
    eqns
  ];


EqnsToProp[ eqns_, simplifications_ ] :=
  And @@
  EqnToProp[
    DeleteCases[True] @
    ReduceMonomials @
    eqns
  ]/.simplifications;

SetAttributes[EqnToProp,Listable];

EqnToProp[eqn_] :=
  BooleanConvert[
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
    ],
    "CNF"
  ];

(* Returns True if exactly one of the arguments is True *)
OnlyOne[ args__ ] :=
 	Or @@
  Table[
  	And @@ MapAt[ Not, Not /@ { args }, { i } ], { i, Length @ {args} }
  ];

NotOnlyOne[ args__ ] := BooleanMinimize @ Not @ OnlyOne[args] ;

IntToBool[1]  := True;
IntToBool[0]  := False;
IntToBool[x_] := x;

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
    "LevelOfSimplification" -> 2
  };

ReduceViaLogic[ proposition_, OptionsPattern[] ] :=
  Module[
    { UpdateKnowns, UpdateEquivalences, SimplifySystem, simplify1, simplify2, simplify3, simplify4, findEquiv,
      TransferKnowns, FindEquivs, RepeatedUpdateEquivalences, lSymp, combinedSimplification 
    },

    (* TODO: implement error message if lSymp > 4 *)
    lSymp =
      Min[ 4, OptionValue["LevelOfSimplification"] ];

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


MatsToProposition[b_][ matList_ ] :=
  And @@ ( MatProp[b] /@ matList );

MatProp[ b_ ][ mat_ ] :=
  With[ { p = Permanent @ mat }, 
    If[ 
      TrueQ[ (p /.b[_] -> 0 ) > 0 ],
      True,
      BooleanConvert[ ReduceMonomials @ p /. { Times -> And, Plus -> Or } , "CNF" ] 
    ]
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

PicoSATSolutions[ prop_ ] := 
  Module[ 
    { currentdir, scriptdir, dir }, 
    currentdir = Directory[];

    scriptdir =
      Quiet[
        Check[ SetDirectory @ DirectoryName @ $InputFileName,    (* If not using notebook interface *)
        SetDirectory @ NotebookDirectory[]],SetDirectory::fstr   (* If using notebook interface *)
      ];
    scriptfn = FileNameJoin @ { scriptdir, "JuilaPicoSAT.jl" };

    dir    = CreateDirectory[]; 
    datafn = FileNameJoin @ { dir, "proposition.csv" };
    solfn  = FileNameJoin @ { dir, "solutions.csv" };
    (* Export Proposition *) 
    PicoSATExportProp[ datafn , prop ];

    Run @ StringJoin[ "julia -e \"include(\\\"", scriptfn, "\\\") -- ", datafn ];

    soln = Transpose @ Import @ solfn;

  ];


PicoSATExportProp[ fn_, prop_ ] :=
	Module[
		{ cscmat },
		cscmat =
			SparsetoCSC @
			ListPropToSparse @ 
			( Flatten /@ ( List/@( List @@ prop )/.{ Or -> List, Not -> (-1 * #&) } ) );
			
		Export[ fn, cscmat, "CSV" ]
	];


PicoSATExportScript[ dir_ ] :=
	Module[
		{ cscmat },
		cscmat =
			SparsetoCSC @
			ListPropToSparse @ 
			( Flatten /@ ( List/@( List @@ prop )/.{ Or -> List, Not -> (-1 * #&) } ) );
			
		Export[ fn, cscmat, "CSV" ]
	];


(* Convert list of lists to sparsearray *)
ListPropToSparse[ lp_List ] := 
  Reap[ 
    Do[
      Sow[ { i, j } -> lp[[i,j]] ]
      ,
      { i, Length @ lp },
      { j, Length @ lp[[i]] }
    ]
  ][[2,1]];

(* Convert sparse array to CSC format *)
SparsetoCSC[ m_SparseArray ] := 
	With[{ rules = Most @ ArrayRules @ m },
		Transpose[ Flatten /@ (List @@@ rules) ]
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
