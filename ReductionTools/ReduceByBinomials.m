(* ::Package:: *)

Package["Anyonica`"]

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
  Options[ SolveBinomialSystem ];

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
    preEqCheck, procID, absTime, result
    },
    invertibleMatrices =
      OptionValue["InvertibleMatrices"];
    polynomialConstraints =
      OptionValue["PolynomialConstraints"];
    simplify =
      OptionValue["SimplifyIntermediateResultsBy"];
    preEqCheck =
      OptionValue["PreEqualCheck"];
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
              "Symmetries"    -> None
              (* Symmetries are exhausted by first call. If not set to None, the original symmetries are used and an error occurs  *)
            ];

          If[ updatedSystems === {}, Return @ {} ];

          Do[
            SolveRepeatedly[
              BinSplit[ sys["Equations"], BinomialEquationQ @* preEqCheck ],
              GetVariables[ Normal @ sys["Equations"], s[i] ] ,
              s[i+1],
              Normal @ prevSols /. sys["Solution"],
              sys["Constraints"]
            ]
            ,
            { sys, updatedSystems }
          ]
        ];

      firstSystems =
        AddOptions[opts][SolveAndUpdate][
          binomialEqns,
          sumEqns,
          { polynomialConstraints, invertibleMatrices },
          vars,
          s[1]
        ];

      If[ firstSystems === {}, Return @ {} ];

      Reap[
        Do[
          SolveRepeatedly[
            BinSplit[ sys["Equations"], BinomialEquationQ @* preEqCheck ],
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
