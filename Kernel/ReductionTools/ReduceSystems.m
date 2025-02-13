(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gert *)
(* :Date: 2023-10-17 *)

Package["Anyonica`"]

(* This file contains functions that reduce systems of polynomials. Originally these could take either the form of a system of
   polynomial equations, or a list of polynomials but we will try to convert all functions to work with lists of polynomials.
   It is easy to convert between the two.

   Every function that reduces a system should have a standard output in the form of
   <|
      "Polynomials" -> list of polynomials after reduction,
      "Assumptions" -> logical expression that expresses assumptions on the variables,
      "Values"      -> list of rules mapping the original unknowns to expressions in terms of unknowns appearing in
                       the list of polynomials after reduction, or numbers if all polynomials have been solved for                 
      "Symbol"      -> the symbol used for the variables
   |>

   We should also use this as a standard input. 
   We can set up the polynomials with singly-indexed variables when we create such a system and 
   keep the old variables as keys of "Values"

   We call this the Standard System Data Structure (SSDS)

   Every function that reduces a system should also have 2 standard inputs for the system: either in SSDS form or just
   as a list of polynomials.


*)

(*
+---------------------------------------------------------------------------+
|                        REDUCTION OF ANY SYSTEM                            |
+---------------------------------------------------------------------------+
*)


Options[ReduceSystemRecursively] := 
  {
    "SortRulesBy" -> None
  };

(* 
    oracle: a function that returns True when an equation/polynomial can be used 
    to obtain info about the system
    toRules: a function that maps an equation/polynomial to a list of replacement rules 
   *)
ReduceSystemRecursively[ oracle_, toRules_ , system_] :=
  Module[ 
    { reduce  },

    SetAttributes[ reduce, Listable ];

    reduce[sys_] := 
      ReduceSystems[oracle,toRules,ruleWeight][sys];
   
    FixedPoint[ reduce, { system } ]
  ];
(*
ReduceSystems[ oracle_, toRules_, OptionsPattern[] ][ system_ ]
  Module[
    { rulesLists, sort,  },
    sort = 
      If[ OptionValue["SortRulesBy"] =!= None, SortBy[OptionValue["SortRulesBy"]], Identity ];

    (* Obtain all info using the oracle and construct lists of rules from this info *)
    (* The rules will be sorted by a weightfunction *)

    rulesLists = 
      Map[  
        toRules,
        Select[
          SystemPolynomials[system],
          TrueQ @* oracle
        ]
      ];

    (* Sort rules within lists of rules by complexity and remove clashing rules *)
      RemoveCircularRules[ SystemSymbol[system] ] @*
      sort @ 



  ]
*)


(* Here follows an attempt at an abstract function that reduces systems *)


(*  Remove circular rules from a list of rules
    ASSUMPTIONS: the variables must be indexed by a single integer!
 *)

(* The method removes too many rules though *)
RemoveCircularRules[ x_ ][ rules_ ] :=
  Module[{rulesLists, clashingQ},
   rulesLists = 
    Developer`ToPackedArray[
     Flatten @* Join /@
       Map[
        GetVariables[#,x] &,
        List @@@ rules,
        {2}
        ] /. x[i_] :> i
     ];
   
   clashingQ[i1_, i2_] :=
    ClashingRulesQ[rulesLists[[i1]], rulesLists[[i2]]];
   
   rules[[
     DeleteDuplicates[
      Range @ Length @ rulesLists,
      clashingQ
      ]
     ]]
   ];

ClashingRulesQ[ l1_, l2_ ] :=
  Or[
    First[l1] === First[l2], 
    MemberQ[ First @ l1] @ Rest[l2], 
    MemberQ[ First @ l2] @ Rest[l1]
  ];

(* Apply a set of rules to itself until all rules are as advanced as possible. 
  E.g. { x[1] -> x[2], x[2] -> x[3] } should become { x[1] -> x[3], x[2] -> x[3] }
 *)





(*
+---------------------------------------------------------------------------+
|                      REDUCTION OF BINOMIAL SYSTEM                         |
+---------------------------------------------------------------------------+
*)


(* Sometimes the smith decomposition is too heavy for a computer to carry out.
   If this is the case one can reduce the binomial system first using
   other methods 
 *)

(* TODO: convert functions to work with polynomials instead of equations *)
PackageExport["ReduceBinomialSystem"]

ReduceBinomialSystem::usage =
"ReduceBinomialSystem[binomials,vars] returns an association containing a reduced set of polynomials, a "<>
"boolean expression encoding the assumptions on the variables, and assignments of variables in terms of " <>
"those that appear in the reduced polynomials.";

ReduceBinomialSystem::wrongvarsformat =
"`1` should be a list of variables";

ReduceBinomialSystem::wrongeqnsformat =
"`1` should be a list of binomial equations";

Options[ReduceBinomialSystem] :=
  Join[
    Options[ ReduceBinSysHermite ],
    {
      "SimplifyIntermediateResultsBy" -> Identity,
      "Method" -> "HermiteDecomposition"
    }
  ];


argCheck[ binEqns_, vars_ ] :=
  Which[
    !BinomialSystemQ[binEqns]
    ,
    Message[ ReduceBinomialSystem::wrongeqnsformat, binEqns ]; Abort[]
    ,
    Head[vars] =!= List
    ,
    Message[ ReduceBinomialSystem::wrongvarsformat, vars ]; Abort[]
  ];

ReduceBinomialSystem[ binomialEqns_, variables_, opts:OptionsPattern[] ] :=
(
  argCheck[ binomialEqns, variables ];
  Module[
    { eqns, vars, revertVars, x, procID, result, t },

    procID = 
      ToString @ Unique[];

    printlog["RBS:init", { procID, binomialEqns, variables, {opts} } ];
    
    { t, result } = 
      AbsoluteTiming[
        { eqns, vars, revertVars } =
          SimplifyVariables[ ToProperBinomialEquation @ binomialEqns, variables, x ];

        If[
          OptionValue["Method"] === "HermiteDecomposition",
          AddOptions[opts][ReduceBinSysHermite][ eqns, vars ]/.revertVars ,
          Catch @
          FixedPoint[ UpdateEquivalences[x], { NormalForm @ ToProperBinomialEquation @ eqns, { } } ] /. revertVars
        ]
      ];

    printlog[ "Gen:results", { procID, result, t  } ];

    result

  ]
);

Options[ReduceBinSysHermite] := 
  Join[
    Options[ ReduceSemiLinModZ ],
    { 
      "MaxEquationsPerPartition" -> 1024 
    }
  ];

ReduceBinSysHermite[ equations_, variables_, opts : OptionsPattern[] ] := 
  Module[ { nSubsys, s, procID, subsysSize, rhs, rhs1Pos, rhsNon1Pos, 
    subMat1, reducedMat1, matSubsys2, mat2, rhs2, reducedMat2, reducedrhs2, rhsFinal, 
    u, h, mat, t, result, trm1, trm2, combinedMat }, 

    procID = ToString @ Unique[];

    { t, result } = 
    AbsoluteTiming[
      subsysSize = OptionValue["MaxEquationsPerPartition"];

      (* Quite arbitrary, this resulted in doable partition sizes on my computer*)
      nSubsys = Quotient[ Length @ equations, subsysSize ] + 1;
      
      printlog["RBSVHD:init", { procID, Length @ equations, subsysSize } ];

      (*Transform the system to a matrix equation. This step is expensive 
        so best to do it only once*)
      s = Head @ First @ variables;

      { mat, rhs } =  
          Catch @ Normal @ AddOptions[opts][BinToSemiLin][ equations, variables, s ];
      
      If[ Head @ mat === String, Return @ { False } ];

      If[ 
        TrueQ[ subsysSize >= Length[equations] ],
        { u, h } = HermiteDecomposition[ mat ];
        Return @ 
        <| 
          "Polynomials" ->
            TPL @ Thread[ PowerDot[ variables, h ] - PowerDot[ rhs, u ] ] // TPL,
          "Assumptions" -> True,
          "Values" -> Thread[ variables -> variables ]
        |>
      ];
          
      (* ================================================= *)
      (* Reduction of subsystem of eqns who's RHS equals 1 *)
      (* ================================================= *)

      rhs1Pos = Flatten @ Position[ rhs, 1 ];

      (* Sorting rows lexicographically=> groups eqns with equal vars together *)
      subMat1 = ReverseSortBy[ mat[[rhs1Pos]], Abs ]; 

      printlog["RBSVHD:toric", { procID, Length @ subMat1 }];

      If[ 
        subMat1 === {{}}
        ,
        { trm1, reducedMat1 } = AbsoluteTiming[ {{}} ]
        ,
        { trm1, reducedMat1 } =
          AbsoluteTiming @  
          Catch @
          FixedPoint[
            partitionAndReduce[ #, subsysSize, procID ] &, 
            subMat1, 
            5 (*Nest at most 5 times*)
          ]
      ];

      printlog["RBSVHD:toricresults", { procID, trm1, Length @ reducedMat1 }];

      (* ========================================================= *)
      (* Reduction of subsystem of eqns who's RHS does not equal 1 *)
      (* ========================================================= *)
      
      rhsNon1Pos = Complement[ Range @ Length @ rhs, rhs1Pos ];
      
      (* Now the RHS matters => need to combine mat with RHS when sorting *) 
      matSubsys2 = 
        ReverseSortBy[
          AppendRHS[ mat[[rhsNon1Pos]], rhs[[rhsNon1Pos]] ],
          Abs
        ];
      
      printlog["RBSVHD:nontoric", { procID, Length @ matSubsys2 }];

      If[ 
        matSubsys2 === {{}}
        ,
        { trm2, { reducedMat2, reducedrhs2 } } =
          AbsoluteTiming[ { {}, {} } ]
        ,
        { trm2, { reducedMat2, reducedrhs2 } } =
          AbsoluteTiming[
            mat2 = matSubsys2[[ ;;, ;;-2 ]];
            rhs2 = matSubsys2[[ ;;, -1 ]];
            Catch @
            FixedPoint[ 
              partitionAndReduce[ #, subsysSize, procID ]&, 
              { mat2, rhs2 }, 
              5 
            ] 
          ]
      ];

      (* Check validity solution *)

      If[
        reducedMat2 === Missing["InconsistentSystem"],
        Throw @ <| "Polynomials" -> {1}, "Assumptions" -> False, "Values" -> {} |>
      ];
      
      printlog["RBSVHD:nontoricresults", { procID, trm2, Length @ reducedMat2 } ];

      combinedMat = reducedMat1 ~ Join ~ reducedMat2 ;

      printlog["RBSVHD:intermediatereduction", { procID, combinedMat } ];
      (* Final Hermite Decomposition of combination two reduced systems *)
      { u, h } = HermiteDecomposition @ combinedMat;
      
      (* Need to add the 1's from the RHS of the first system and act on RHS with u *)
      rhsFinal = 
        PowerDot[
          PadLeft[ reducedrhs2, Length[reducedMat1] + Length[reducedMat2], 1 ], 
          u 
        ];
      
      <| 
        "Polynomials" ->
          TPL @ Thread[ PowerDot[ variables, h ] - rhsFinal ],
        "Assumptions" -> True,
        "Values" -> Thread[ variables -> variables ]
      |>
    ];

    printlog["RBSVHD:reduction",{procID,t,result}];

    result
  ];

Options[ partitionAndReduce ] = 
  { "PreEqualCheck" -> Identity };

(* Partition matrix, compute hermite decompositions and combine them for case where 
  all elements of the RHS equal 1 *)
partitionAndReduce[ mat_?MatrixQ, subsysSize_Integer, procID_ ] := 
  Module[{ t, result, partitionPos, reduceMat }, 
    (* Check for empty matrix *)
    If[ ByteCount[ mat < 1024 ] && Flatten[mat] === {}, Return @ {{}} ];

    printlog["PAR:init", {procID,Length @ mat,subsysSize} ];

    partitionPos = Partition[ Range @ Length @ mat, UpTo[subsysSize] ];

    reduceMat[ m_ ] := 
      DeleteCases[ { 0 .. } ] @  (* Delete zero rows *)
      Last @ (* Only care about reduced matrix *)
      MemoizedHermiteDecomposition[ m, "StoreHermiteDecompositions" -> True ];

    { t, result } = 
      AbsoluteTiming @
      ReverseSortBy[
        DeleteDuplicates @
        Flatten[ (*This Flatten operation will riffle the lists of equations.*)
          Map[ 
            reduceMat, 
            Table[ mat[[pos]], { pos, partitionPos } ]
          ], 
          { 2, 1 }
        ],
        Abs
      ];

    printlog["PAR:reduction",{procID,t, Length @ result}];

    result
  ];

(* Partition matrix, compute hermite decompositions and combine them for case where 
  none of the elements of the RHS equal 1 *)
partitionAndReduce[ { mat_?MatrixQ, rhs_?VectorQ }, subsysSize_Integer, procID_] := 
  Module[{ t, result, partitionPos, decomps, newMat, newRHS, newSys },  

    (* Check for empty system *)
    If[ ByteCount[ rhs < 1024 ] && Flatten[rhs] === {}, Return @ { {{}}, { } } ];

    printlog["PAR:init", {procID,Length @ mat,subsysSize} ];
  
    {t, result} =
    AbsoluteTiming[
      partitionPos = Partition[ Range @ Length @ mat, UpTo[subsysSize] ];

      decomps = 
        Map[ 
          MemoizedHermiteDecomposition[ #, "StoreHermiteDecompositions" -> True ] &, 
          Table[ mat[[pos]], { pos, partitionPos } ]
        ];

      newMat = Join @@ decomps[[;; , 2]];

      newRHS = 
        Join @@ 
        MapThread[ 
          PowerDot, 
          { 
            Table[ rhs[[pos]], { pos, partitionPos } ], 
            decomps[[;; , 1]] 
          }
        ];
      
      newSys = 
        DeleteCases[ { 0 .., 1 } ] @
        ReverseSortBy[
          AppendRHS[ newMat, newRHS ],
          Abs
        ];
      
      (* Faster to use reverse indices because problems appear at the bottom *)
      If[ (* There is a zero row with non-one rhs *)
        MemberQ[ 
            Reverse @ Range @ Length @ newSys,  
            i_ /; 
            MatchQ[ newSys[[i]], { 0 .., x_ } /; TrueQ[ PreEqualCheck[x] != 1 ] ]
        ],
        printlog["Gen:has_False", { procID, newSys  } ];
        Throw @ { Missing["InconsistentSystem"], {}  }
      ];

      { newSys[[;; , ;; -2]], newSys[[;; , -1]] }
    ];

    printlog["PAR:reduction",{procID,t, Length @ result}];

    result
  ];

AppendRHS[ mat_, rhs_ ] := ArrayFlatten[ { { mat, Transpose @ { rhs } } } ];

AppendRHS[ {{}}, _ ] := {{}};

AppendRHS[ _, {} ] := {{}}


FindEquivalences[ eqns_, x_ ] :=
  With[{ eqnVarPairs = { #, LinearVars[#,x], GetVariables[#,x] }& /@ eqns },
    DeleteDuplicates[
      DeleteCases[ eqnVarPairs, { _, { }, _ } ],
      IntersectingQ[  Last @ #1, Last @ #2 ]&
    ]
  ];

LinearVars[ eqn_, x_ ] :=
  With[{ powerVars = Cases[ eqn, Power[ a_, b_ ] /; Abs[b] > 1 :> a, Infinity ] },
    GetVariables[ eqn, x, powerVars ]
  ];

EquivalencesToRules[ equivs_, x_ ] :=
  EquivToRule[x] /@ equivs;

EquivToRule[x_][ { eqn_, linvars_, vars_ } ] :=
  With[{ var = First @ linvars },
    Solve[ eqn, var ][[1,1]]
  ];

UpdateEquivalences[x_][ { eqns_, equivs_ }] :=
  With[ { equivRules =  EquivToRule[x] /@ FindEquivalences[ eqns, x ] },
    EchoIn[ 500, "NewKnowns", Last ] @
    {
      TEL @
      NormalForm @
      ToProperBinomialEquation @  (* This should not be necessary when only working with binomial equations, still the computer takes ages when I leave this function out*)
      ReplaceAll[ eqns, Dispatch[equivRules] ],
      Join[ equivs/.Dispatch[equivRules], equivRules ]
    }
  ];

SetAttributes[NormalForm,Listable];
NormalForm[True] = 
  True;
NormalForm[False] = 
  False;
NormalForm[ eqn_ ] :=
  eqn[[2]] / eqn[[1]] == 1;


(*
+---------------------------------------------------------------------------+
|                 REDUCTION OF GENERAL SYSTEM VIA BINOMIALS                 |
+---------------------------------------------------------------------------+
*)

PackageExport["ReduceByBinomials"]

ReduceByBinomials::usage =
"ReduceByBinomials[sumEqns,binomialEqns,vars,s] recursively reduces sumEqns using solutions to binomialEqns"<>
" and expresses vars in terms of new variables, labeled by s, that appear in the reduced sumEqns.";

ReduceByBinomials::notlistofequations =
"`1` is not a list of equations.";

ReduceByBinomials::notbinomialsystem =
"`1` is not a list of binomial equations.";

ReduceByBinomials::notlistofvars =
"`1` is not a list of variables.";

Options[ ReduceByBinomials ] :=
  Join[
    Options[ SolveBinomialSystem ],
    { "MaxNumberOfBinomialEquations" -> Infinity }
  ];

CheckArgs[ sumEqns_, binomialEqns_, vars_ ] :=
  Which[
    !ListOfEquationsQ[sumEqns]
    ,
    Message[ReduceByBinomials::notlistofequations, sumEqns ];
    Abort[]
    ,
    !BinomialSystemQ[binomialEqns]
    ,
    Message[ ReduceByBinomials::notbinomialsystem, binomialEqns ];
    Abort[]
    ,
    !ListQ[vars]
    ,
    Message[ ReduceByBinomials::notlistofvars, vars ];
    Abort[]
  ];

ReduceByBinomials[ sumEqns_, binomialEqns_, vars_, s_, opts:OptionsPattern[] ] :=
(
  CheckArgs[ sumEqns, binomialEqns, vars ];
  Module[{
    SolveRepeatedly, firstSystems, constraints, invertibleMatrices, polynomialConstraints, simplify,
    preEqCheck, procID, absTime, result, maxNBin, newBinEqns, newNonBinEqns
    },
    invertibleMatrices =
      OptionValue["InvertibleMatrices"];
    polynomialConstraints =
      OptionValue["PolynomialConstraints"];
    simplify =
      OptionValue["SimplifyIntermediateResultsBy"];
    preEqCheck =
      OptionValue["PreEqualCheck"];
    maxNBin = 
      Min[ Length[binomialEqns], OptionValue["MaxNumberOfBinomialEquations"] ];
    procID =
      ToString[Unique[]];

    printlog["RBM:init", {procID, sumEqns, binomialEqns, vars, s, {opts}}];

    { absTime, result } =
    AbsoluteTiming[
      constraints =
        Join[
          polynomialConstraints,
          DeterminantConditions @ invertibleMatrices
        ];

      SolveRepeatedly[ { {}, nonBinEqns_ }, _, _, prevSols_, _  ] :=
        Sow[ { nonBinEqns, prevSols } ];

      SolveRepeatedly[ { binEqns_, nonBinEqns_ }, variables_, s[i_], prevSols_, constr_ ] :=
        Module[ { updatedSystems },
          updatedSystems =
            AddOptions[opts][SolveAndUpdate][
              binEqns, nonBinEqns, constr, variables, s[i],
              "NonSingular"   -> True,
              "Symmetries"    -> None  (* Symmetries are exhausted by first call. If not set to None, the original symmetries are used and an error occurs  *)
            ];

          If[ updatedSystems === {}, Return @ {} ];

          Do[
            SolveRepeatedly[
              BinSplit[ sys["Equations"], BinomialEquationQ @* preEqCheck ] // MoveEquations[maxNBin],
              GetVariables[ Normal @ sys["Equations"], s[i] ] ,
              s[i+1],
              Normal @ prevSols /. sys["Solution"],
              sys["Constraints"]
            ]
            ,
            { sys, updatedSystems }
          ]
        ];

      (* Possibly only use a maximum number of the binomial Equations per reduction step *)

      { newBinEqns, newNonBinEqns } = 
        MoveEquations[maxNBin][ { binomialEqns, sumEqns } ];

      firstSystems =
        AddOptions[opts][SolveAndUpdate][
          newBinEqns,
          newNonBinEqns,
          { polynomialConstraints, invertibleMatrices },
          vars,
          s[1]
        ];

      If[ firstSystems === {}, Return @ {} ];

      Reap[
        Do[
          SolveRepeatedly[
            BinSplit[ sys["Equations"], BinomialEquationQ @* preEqCheck ] // MoveEquations[maxNBin],
            GetVariables[ Normal @ sys["Solution"], s[1] ],
            s[2],
            Normal @ sys["Solution"],
            sys["Constraints"]
          ]
          ,
          { sys, firstSystems }
        ];
      ][[2]] /. ( s[_][i_] :> s[i] ) /. ( {x_List} :> x )
    ];

    printlog["RBM:solutions", {procID, result}];
    printlog["Gen:results", {procID, result, absTime}];

    result
  ]
);

(* Move n eqns from l1 to l2 *)
MoveEquations[n_][ { l1_, l2_ } ] := 
  With[
    { split = TakeDrop[ l1, Min[ n, Length @ l1 ] ] },
    { First @ split, Join[ Last @ split, l2 ] }
  ];

PackageExport["RBB"]

RBB::usage =
  "Shorthand for ReduceByBinomials.";

RBB =
  ReduceByBinomials;


Options[SolveAndUpdate] :=
  Options[ReduceByBinomials];

(* Solves binEqns, updates sumEqns and constraints and checks for validity.
  A list of triples { sumEqns_i, sol_i, constr_i } is returned
*)
SolveAndUpdate[ binEqns_, sumEqns_, constraints_, vars_, s_, opts:OptionsPattern[] ] :=
Module[{ soln, solve, preEqCheck, simplify, simplifySolutions, procID, absTime, result, eqnSolConstr, notInvalidPos },
  simplify =
    OptionValue["SimplifyIntermediateResultsBy"];
  simplifySolutions =
  Function[
    solutions,
    If[
      solutions =!= {},
      MapAt[ simplify, solutions, { All, All, 2 } ],
      {}
    ]
  ];
  preEqCheck =
  OptionValue["PreEqualCheck"];
  solve =
  If[ OptionValue["NonSingular"], SNSBS, SBS ];
  procID =
  ToString[ Unique[] ];

  printlog[ "SAU:init", {procID, binEqns,sumEqns,vars,s,{opts}} ];

  { absTime, result } =
  AbsoluteTiming[
    (* Note that we simplify solutions first and then simplify sumEqns again. This is to reduce memory pressure *)
    soln =
    Map[
      Dispatch,
      simplifySolutions @ AddOptions[opts][solve][ binEqns, vars, s ]
    ];

    eqnSolConstr =
    Table[
      {
        TEL @ AddOptions[opts][UpdateAndCheck][ sumEqns, sol, Identity ],
        sol,
        AddOptions[opts][UpdateConstraints][ constraints, sol ]
      },
      { sol, soln }
    ];

    printlog["SAU:updated_sys", {procID, eqnSolConstr[[;;,1]], eqnSolConstr[[;;,3]] } ];

    notInvalidPos =
    Flatten @
    Position[
      eqnSolConstr,
      { e_, sl_, c_ } /;
      NotInvalidNonZeroSolutionQ[e,preEqCheck][sl] && c =!= {False},
      {1}
    ];

    If[
      Length[notInvalidPos] != Length[soln],
      printlog["SAU:invalid_positions", {procID, Complement[ Range @ Length @ soln, notInvalidPos ] } ]
    ];

    Table[
      <|
        "Equations"   -> eqnSolConstr[[i,1]],
        "Solution"    -> eqnSolConstr[[i,2]],
        "Constraints" -> eqnSolConstr[[i,3]]
      |>,
      { i, notInvalidPos }
    ]

  ];

  printlog["SAU:remainingsol", {procID,soln,result}];
  printlog["Gen:results", {procID,result,absTime}];

  result
];


Options[UpdateConstraints] =
{
  "SimplifyIntermediateResultsBy" -> Identity,
  "PreEqualCheck" -> Identity
};

UpdateConstraints[ { polConstr_, invertibleMatrices_ }, solution_, opts:OptionsPattern[] ] :=
Module[ { newPolConstr, newInvMats, simplify, preEqCheck },
  simplify =
  OptionValue["SimplifyIntermediateResultsBy"];
  preEqCheck =
  OptionValue["PreEqualCheck"];

  newPolConstr =
  AddOptions[opts][UpdateAndCheck][ polConstr, solution, Identity ];

  If[ newPolConstr === { False }, Return @ { False } ];

  newInvMats =
  AddOptions[opts][UpdateAndCheck][ invertibleMatrices, solution, Det[#] =!= 0& ];

  If[ newInvMats === { False }, Return @ { False } ];

  {
    Select[ Not @* TrueQ @* preEqCheck ] @ newPolConstr,
    DeleteCases[ mat_/; TrueQ[ Det[mat] != 0 ] ] @ newInvMats
  }

];

UpdateInvertibleMatrices[ {}, _ ] :=
{};

UpdateInvertibleMatrices[ mats_List, soln_ ] :=
( mats /. Dispatch[soln] ) //
DeleteCases[ mat_/; TrueQ[Det[mat] != 0] ];

Options[ SimplestLinearRule ] =
{ "LinearReductionWeight" -> RationalWeight };

SimplestLinearRule[ pols_, s_, opts:OptionsPattern[] ] :=
With[
  {
    cases =
    Cases[
      FindLinearRule[ #, s ]& /@ pols,
      r_/; Head[ r ] =!= Missing
    ],
    weightfn =
    OptionValue["LinearReductionWeight"]
  },

  If[
    cases === {},
    Return @ Missing[]
  ];

  MinimalBy[
    cases,
    weightfn[ (#[[2]])["Numerator"], (#[[2]])["Denominator"] ]&,
    1
  ][[1]]
];

RationalWeight[ num_, denom_ ] :=
Which[
  MonomialQ[ num ] && MonomialQ[ denom ],
  { 0, LeafCount @ Cancel[num/denom] },
  MonomialQ[ denom ],
  { 1, LeafCount @ Cancel[num/denom] },
  True,
  { 2, LeafCount @ denom }
];


(*
+---------------------------------------------------------------------------+
|                     REDUCTION VIA LINEAR POLYNOMIALS                      |
+---------------------------------------------------------------------------+
*)


PackageExport["ReduceByLinearity"]

ReduceByLinearity::usage =
"ReduceByLinearity[polList,s] reduces polList by using polynomials in polList that are linear in one of the "<>
"variables labeled by s.";

Options[ReduceByLinearity] :=
Join[
  {
    "SimplifyIntermediateResultsBy" -> Identity,
    "ReduceByLinearityMaxMemoryFactor" -> .9,
    "Parallel" -> False
  },
  Options[SimplestLinearRule]
];

(*ReduceByLinearity[ { } , _, ___ ] :=
  <| "Polynomials" -> {}, "Assumptions" -> True, "Rules" -> { } |>;*)


ReduceByLinearity[ polList_List, s_, opts:OptionsPattern[] ] :=
Module[
  {
    ToPol, vars, RCF, InvalidPolSystem, UpdateSystem, RecursiveReduce, time, result, procID, id, RatRule,
    ReduceSystem, AddNonZeroPols, simplify, rootReduce, MonQ, PolRest, parallelQ, map, maxMemFactor
  },
  maxMemFactor =
  OptionValue["ReduceByLinearityMaxMemoryFactor"];
  simplify =
  OptionValue["SimplifyIntermediateResultsBy"];
  parallelQ =
  OptionValue["Parallel"];
  procID =
  ToString @ Unique[];
  id :=
  ToString @ Unique[];
  vars =
  GetVariables[ polList, s ];
  RCF =
  RemoveCommonFactors[ #, s ]&;

  If[
    parallelQ
    ,
    Quiet[LaunchKernels[]];
    map =
    ParallelMap
    ,
    map =
    Map
  ];

  rootReduce = (* TODO: might want to give user the option to turn this off *)
  If[
    MemberQ[ polList, _Root, Infinity ],
    SafeRootReduce,
    Identity
  ];

  (* We need to store the numerator and denominator of rational functions separately because
     because Mathematica automatically cancels common factors.
     RatRule converts the target of the linear rule to an actual rational function. *)
  RatRule[ a_ -> b_Association ] :=
  a -> Cancel[ b["Numerator"] / b["Denominator"] ];
  RatRule[ Missing[] ] :=
  Missing[];

  ToPol =
  RCF @* rootReduce @* simplify @* Expand @* Numerator @* Cancel @* Together;

  MonQ[ pol_ ] :=
  Length[ MonomialList[ pol ] ] === 1;

  PolRest[ pol_, ps_ ] :=
  RCF @ PolynomialReduce[ pol, ps, vars ][[2]];

  (* Make sure all Kernels know the definitions of the functions to be mapped *)
  If[ parallelQ, DistributeDefinitions[ RCF, simplify, ToPol, PolRest, GetVariables ] ];

  (* It is assumed that none of the variables are 0 *)
  InvalidPolSystem[ pols_, nonZeroPols_, rules_  ] :=
  With[
    {
      polProblem = (* note that all zeros are assumed to be removed from pols *)
      FirstCase[ pols, p_ /;  MonQ[p] || MemberQ[p] @ nonZeroPols ],
      nonZeroPolProblem =
      MemberQ[0] @ nonZeroPols,
      ruleProblem =
      MemberQ[0] @ rules[[;;, 2]]
    },
    Which[
      !MissingQ[ polProblem ]
      ,
      printlog["RBL:pol_problem", { id , polProblem, pols } ];
      True
      ,
      nonZeroPolProblem
      ,
      printlog["RBL:non_zero_pol_problem"];
      True
      ,
      ruleProblem
      ,
      printlog["RBL:rule_problem", { id, rules } ];
      True
      ,
      (* ELSE: no problem *)
      True
      ,
      False
    ]
  ];

  UpdateSystem[ pols_, nonZeroPols_, knownRules_, rule_ ] :=
  MemoryConstrained[
    {
      TrimPolynomialList @ map[ ToPol, pols /. rule ],
      nonZeroPols /. rule,
      Append[rule] @ Expand[ knownRules/.rule ]
    }
    ,
    Floor[ MemoryAvailable[] * maxMemFactor ]
    ,
    Sow[ { pols, nonZeroPols, knownRules } ];
    $Aborted
  ];

  ReduceSystem[ pols_, nonZeroPols_, rules_, pol_ ] :=
  MemoryConstrained[
    {
      TrimPolynomialList @ map[ RCF[PolRest[ #, pol ]]&, pols ],
      nonZeroPols,
      rules
    }
    ,
    Floor[ MemoryAvailable[] * maxMemFactor ]
    ,
    Sow[ { pols, nonZeroPols, rules } ];
    $Aborted
  ];

  (* Add denominators appearing in the rules to the set of nonzero pols *)
  AddNonZeroPols[ pols_, nonZeroPols_, rules_ ] :=
  With[
    {
      denominators =
      Denominator @* Cancel @* Together /@
      rules[[;;,2]]
    },
    <|
      "Polynomials" ->  DeleteDuplicates @ map[ Expand, pols ],
      "Assumptions" ->
      LogicalExpand @
      Reduce[
        Thread[
          Map[ RCF, Join[ nonZeroPols, denominators ] ] != 0
        ],
        Backsubstitution -> True
      ],
      "Rules" -> SortBy[First] @ rules
    |>
  ];

  RecursiveReduce[$Aborted] =
  $Aborted;

  RecursiveReduce[
    ps_,    (* Polynomials:                        *)
    nzps_,  (* Non-zero polynomials: initially { } *)
    rs_     (* Substitution rules:   initially { } *)
  ] :=
  Catch[

    If[ InvalidPolSystem[ ps, nzps, rs ], Throw @ Null ];

    With[
      { lRule = AddOptions[opts][SimplestLinearRule][ ps, s ] },
      { denom = If[ MissingQ @ lRule, Missing[], RCF @ lRule[[2]]["Denominator"] ] },

      printlog["RBL:simplest_rule", { id, lRule, s } ];

      If[ (* No linear rules in system *)
        MissingQ @ lRule
        ,
        printlog["RBL:no_rules_left", { id } ];
        Sow[ { ps, nzps, rs } ];
        Throw @ Null
      ];

      If[ (* Denominator in rule isn't zero *)
        MonQ @  PolRest[ denom, nzps ]
        ,
        printlog[ "RBL:nonzero_denominator", { id, lRule } ];
        RecursiveReduce @@
        UpdateSystem[ ps, nzps, rs, RatRule @ lRule ];
        Throw @ Null
      ];

      (* Denominator in rule contains sum. *)

      (* ASSUME DENOMINATOR != 0 *)
      printlog[ "RBL:assuming_nonzero_denominator", { id, lRule } ];
      RecursiveReduce @@
      UpdateSystem[ ps, Append[ denom ] @  nzps, rs, RatRule @ lRule ];

      (* ASSUME DENOMINATOR == 0 *)
      printlog[ "RBL:reduction", { id, denom, s } ];
      RecursiveReduce @@ ReduceSystem[ ps, nzps, rs, denom ]
    ]
  ];

  printlog["RBL:init", { procID, polList, s, { opts } } ];

  { time, result } =
  AbsoluteTiming @
  Block[ { $RecursionLimit = Infinity },
    AddNonZeroPols @@@
    DeleteCases[
      Reap[ RecursiveReduce[ RCF /@ polList, { }, { } ] ][[2]] /. {{x__}} :> {x},
      { { 1 }, { }, { } }
    ]
  ];

  printlog["Gen:results", { procID, result, time } ];

  If[ parallelQ, CloseKernels[] ];

  result

];

PackageExport["RBL"]

RBL::usage =
"Shorthand for ReduceByLinearity.";

RBL =
ReduceByLinearity;


FindLinearRule[ pol_, s_ ] :=
Module[{ nonLinVars, linVars, var },
  nonLinVars =
  Cases[ { pol }, Power[ s[i__], _ ] :> s[i], { 1, 5 } ] // DeleteDuplicates;

  linVars =
  Tally @
  Cases[
    pol,
    s[i__] /; FreeQ[s[i]] @ nonLinVars,
    Infinity
  ];

  If[
    linVars === {},
    Return @ Missing[],
    var =
    MinimalBy[ linVars, Last, 1 ][[1,1]];
    Collect[ pol, var ] /. a_. * var + b_ : 0 :> var -> <| "Denominator" -> a, "Numerator" -> -b |>
  ]
];


(*

  EXPERIMENTAL FUNCTION BELOW

*)

PackageScope["ReduceByLinearity2"]

ReduceByLinearity2::usage =
"For testing.";

Options[ SimplestPol ] =
{ "WeightFunction" -> PolWeight };

Options[ReduceByLinearity2] :=
Join[
  {
    "SimplifyIntermediateResultsBy" -> Identity,
    "MaxMemory" -> None,
    "Parallel" -> False
  },
  Options[SimplestLinearRule]
];

ReduceByLinearity2[ polList_List, s_, opts:OptionsPattern[] ] :=
Module[
  {
    vars, RCF, InvalidPolSystem, UpdateSystem, RecursiveReduce, time, result, procID, id,
    ReduceSystem, simplify, rootReduce, maxMemory, MonQ, PolRest,
    MoldPol, ToAssociation, PolMod, map, parallelQ
  },
  simplify =
  OptionValue["SimplifyIntermediateResultsBy"];
  maxMemory =
  OptionValue["MaxMemory"];
  parallelQ =
  OptionValue["Parallel"];

  If[
    parallelQ
    ,
    Quiet[LaunchKernels[]];
    map = ParallelMap
    ,
    map = Map
  ];

  procID =
  ToString @ Unique[];
  id :=
  ToString @ Unique[];
  vars =
  GetVariables[ polList, s ];
  RCF =
  RemoveCommonFactors[ #, s ]&;
  MoldPol =
  RCF @* RootReduce @* simplify @* Expand;
  MonQ[ pol_ ] :=
  Length[ MonomialList[ pol ] ] === 1;
  PolRest[ pol_, ps_ ] :=
  PolynomialReduce[ pol, ps, vars ][[2]];

  PolMod :=
  Function[
    { p1, p2, var },
    With[{ gb = GroebnerBasis[ { p1, p2 }, GetVariables[ { p1, p2 }, s ], { var }  ] },
      If[ gb === {}, 0, MoldPol @ gb[[1]] ]
    ]
  ];

  If[ parallelQ, DistributeDefinitions[ GetVariables, MoldPol, PolMod ] ];

  (* It is assumed that none of the variables are 0 *)
  InvalidPolSystem[ pols_, nonZeroPols_, linPols_  ] :=
  With[
    {
      polProblem =
      MemberQ[ pols, p_ /; MonQ[p] || MemberQ[p] @ nonZeroPols ],
      nonZeroPolProblem =
      MemberQ[0] @ nonZeroPols
    },
    Which[
      polProblem,
      printlog["RBL:pol_problem", { id , pols } ];
      True,
      nonZeroPolProblem,
      printlog["RBL:non_zero_pol_problem"];
      True,
      (* ELSE: no problem *)
      True,
      False
    ]
  ];

  UpdateSystem[ pols_, nonZeroPols_, linPols_, linPol_ ] :=
  With[{ pol = linPol["Var"] * linPol["Factor"] + linPol["Term"] },
    MemoryConstrained[
      {
        DeleteDuplicates @
        DeleteCases[0] @
        map[
          PolMod[ #, pol, linPol["Var"] ]&,
          pols
        ],
        Map[
          PolMod[ #, pol, linPol["Var"] ]&,
          nonZeroPols
        ],
        Append[ linPol ] @ linPols
      },
      maxMemory,
      (* When maxMemory is exceeded during calculation,
         sow non-updated system and stop further operations on this branch *)
      printlog["RBL:memory_overflow", { procID } ];
      Sow[ { pols, nonZeroPols, linPols } ];
      { { 1 }, { }, { } }
    ]
  ];

  ReduceSystem[ pols_, nonZeroPols_, linPols_, denom_ ] :=
  MemoryConstrained[
    {
      map[ MoldPol[ PolRest[ #, denom ] ]&, pols ],
      nonZeroPols,
      linPols
    },
    maxMemory,
    printlog["RBL:memory_overflow", { procID }];
    Sow[ { pols, nonZeroPols, linPols } ];
    { { 1 }, { }, { } }
  ];

  (* Add denominators appearing in the rules to the set of nonzero polList *)
  ToAssociation[ pols_, nonZeroPols_, linPols_ ] :=
  <|
    "Polynomials" -> DeleteDuplicates @ Map[ MoldPol, pols ],
    "Assumptions"-> LogicalExpand @
    Reduce[ Thread[  nonZeroPols != 0 ], Backsubstitution -> True ],
    "LinearPolynomials" -> linPols
  |>;

  RecursiveReduce[
    ps_,    (* Polynomials:          initially RCF /@ pols *)
    nzps_,  (* Non-zero polynomials: initially { } *)
    lps_    (* Linear polynomials:   initially { } *)
  ] :=
  If[
    InvalidPolSystem[ ps, nzps, lps ],
    (* THEN *)
    Return @ Null,
    (* ELSE *)

    With[ { linPol = AddOptions[opts][SimplestPol][ ps, s ] },

      If[ (* No linear rules in system *)
        MissingQ @ linPol,
        (* THEN *)
        Sow[ { ps, nzps, lps } ];
        Return @ Null
      ];

      If[ (* Denominator in rule isn't zero *)
        MonQ @  PolRest[ linPol["Factor"], nzps ],
        (* THEN *)
        RecursiveReduce @@
        UpdateSystem[ ps, nzps, lps, linPol ];
        Return @ Null
      ];

      (* ASSUME DENOMINATOR != 0 *)
      RecursiveReduce @@
      UpdateSystem[ ps, Append[ linPol["Factor"] ] @  nzps, lps, linPol ];

      (* ASSUME DENOMINATOR == 0 *)
      RecursiveReduce @@
      ReduceSystem[ ps, nzps, lps, RCF @ linPol["Factor"] ]
    ]
  ];


  printlog["RBL:init", { procID, polList, s, { opts } } ];

  { time, result } =
  AbsoluteTiming[
    ToAssociation @@@
    DeleteCases[
      Reap[ RecursiveReduce[ RCF /@ polList, { }, { } ] ][[2]] /. {{x__}} :> {x},
      { { 1 }, { }, { } }
    ]
  ];

  printlog["Gen:results", { procID, result, time } ];

  If[ parallelQ, CloseKernels[ ] ];

  result

];

SimplestPol[ pols_, s_, opts:OptionsPattern[] ] :=
With[
  {
    polsInfo =
    Select[
      LinearVarInfo[ s ] /@ pols,
      Not @* MissingQ
    ],
    weightfn =
    OptionValue["WeightFunction"]
  },

  If[
    polsInfo === {},
    Return @ Missing[]
  ];

  MinimalBy[
    polsInfo,
    weightfn @@ { #["Factor"], #["Term"] }&,
    1
  ][[1]]
];

PolWeight[ f_, t_ ] :=
If[
  Length[ MonomialList[f] ] == 1,
  0,
  LeafCount[ { f, t } ]
];

LinearVarInfo[ s_ ][ pol_ ] :=
Module[{ nonLinVars, linVars, var },
  nonLinVars =
  Cases[ { pol }, Power[ s[i__], _ ] :> s[i], { 1, 5 } ] //
  DeleteDuplicates;

  linVars =
  Tally @
  Cases[
    pol,
    s[i__] /; FreeQ[s[i]] @ nonLinVars,
    Infinity
  ];

  If[
    linVars === {},
    Return @ Missing[],
    var =
    MinimalBy[ linVars, Last, 1 ][[1,1]];
    ReplaceAll[
      Collect[ pol, var ],
      a_. * var + b_ : 0 :> <| "Var" -> var, "Factor" -> a, "Term" -> b |>
    ]
  ]
];


(*
+---------------------------------------------------------------------------+
|                        REDUCING VIA TRIVIALITIES                          |
+---------------------------------------------------------------------------+
*)

PackageExport["ReduceTrivialities"]

ReduceTrivialities::usage =
"ReduceTrivialities[eqnList,vars] reduces eqnList recursively by using trivial equalities in eqnList.";

Options[ReduceTrivialities] =
{
  "SimplifyBy" -> Identity,
  "Parameters" -> {}
};

ReduceTrivialities[ eqnsList_?ListOfEquationsQ, vars_, OptionsPattern[] ] :=
Module[ { newEqns, newVars, revertVars, s, newSystem, simplify },
  simplify =
  OptionValue["SimplifyBy"];
  { newEqns, newVars, revertVars } =
    SimplifyVariables[ eqnsList, vars, s ];

  newSystem =
    UpdateSystemViaTrivialities[ s, simplify ][ {}, {}, newEqns ];

  Normal[newSystem]/.revertVars
];

(* Update system to find all trivial information *)
UpdateSystemViaTrivialities[ s_Symbol, simplify_ ][
  knownVars_List,
  mapToReps_List,
  eqnsList_?ListOfEquationsQ
] :=
With[{
  newSystem =
  Catch[
    FixedPoint[ MoldEquationsViaTrivialities[s,simplify], { knownVars, mapToReps, eqnsList } ]
  ]
},
  If[
    newSystem === {False},
    { {}, {}, {False} },
    newSystem
  ]
];

MoldEquationsViaTrivialities[ s_Symbol, simplify_ ][ { knownVars_List, mapToReps_List, {} } ] :=
  { knownVars, mapToReps, {} };
MoldEquationsViaTrivialities[ s_Symbol, simplify_ ][
  { knownVars_List, mapToReps_, eqnsList_?ListOfEquationsQ }
] :=
  With[{
    newVarsAndEqns =
      FixedPoint[ UpdateVariablesAndSystem[s,simplify], {  knownVars, eqnsList } ]
    },
    With[{
      newEquivClassesAndEqns =
        FixedPoint[ UpdateEquivalenceClasses[s,simplify], {  mapToReps, newVarsAndEqns[[2]] } ]
      },
      {
        newVarsAndEqns[[1]],
        newEquivClassesAndEqns[[1]],
        DeleteDuplicates @
        Map[
          Sort,
          newEquivClassesAndEqns[[2]]
        ]
      }
    ]
  ];

UpdateVariablesAndSystem[ s_Symbol, simplify_ ][ { knownVars_List, {} } ] :=
  { knownVars, {} };
UpdateVariablesAndSystem[ s_Symbol, simplify_ ][ { knownVars_List, eqnsList_?ListOfEquationsQ } ] :=
  With[{
    newKnownVars =
    UpdateKnownVars[ knownVars, eqnsList, s ] //
    simplify
  },
  {
    newKnownVars,
    ReplaceAll[ eqnsList, Dispatch[newKnownVars] ] //
    simplify //
    DeleteCases[True] //
    CheckSystemValidity
  }
];

UpdateKnownVars[ knownVars_List, eqnsList_?ListOfEquationsQ , s_Symbol ] :=
Join[
  knownVars,
  Cases[ eqnsList, s[i__] == x_?NumericQ | x_?NumericQ == s[i__] :> ( s[i] -> x ) ]
];

(* Replace all equivalent F symbols by representatives and update eqnsList *)
UpdateEquivalenceClasses[ s_Symbol, simplify_ ][ { mapToReps_, {} } ] :=
{ mapToReps, {} };
UpdateEquivalenceClasses[ s_Symbol, simplify_ ][ { mapToReps_, eqnsList_?ListOfEquationsQ } ] :=
Module[{
  newReps =
  Dispatch @
  MapToReps @
  CreateEquivalenceClasses[s][ eqnsList ]
},
  {
    Dispatch @ Join[ Normal @ mapToReps /. newReps, Normal @ newReps ],
    ( eqnsList /. newReps ) //
    simplify //
    DeleteCases[True] //
    CheckSystemValidity
  }
];

(* To use all equations of the form s[i__] == s[j__] we construct equivalence classes *)
CreateEquivalenceClasses[ s_Symbol ][ eqnsList_?ListOfEquationsQ ] :=
ConnectedComponents @
Graph @
Cases[ eqnsList, s[a__] == s[b__] :> UndirectedEdge[ s[a], s[b] ] ];

MapToReps[ equivClasses_List ] :=
Flatten[
  Thread[ Most[#] -> Last[#] ]& /@
  equivClasses
];
