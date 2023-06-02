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
  Options[ SolvePolynomialSystem ];

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
    SolveRepeatedly, firstSoln, constraints, invertibleMatrices, polynomialConstraints, simplify,
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

      SolveRepeatedly[ { binEqns_, nonBinEqns_ }, variables_, s[i_], prevSols_, { pConstr_, invMats_ } ] :=
      Module[ { validSoln },

        If[ pConstr === { False } || invMats === { False }, Return @ {} ];

        validSoln =
          SolveAndCheck[
            binEqns,
            variables,
            s[i],
            "PolynomialConstraints" -> pConstr,
            "InvertibleMatrices" -> invMats,
            "NonSingular" -> True,
            "SumEquations" -> nonBinEqns,
            "PreEqualCheck" -> preEqCheck
          ];

        If[ validSoln === {}, Return @ {} ];

        MapThread[
          SolveRepeatedly[
            UpdateSystem[ nonBinEqns, #1, simplify, preEqCheck ],
            GetVariables[ Normal[#1], s[i] ] ,
            s[i + 1],
            Normal[prevSols] /. #1,
            #2
          ]&,
          {
            Dispatch /@ validSoln,
            UpdateConstraints[ { pConstr, invMats }, #, simplify, preEqCheck ]& /@
            validSoln
          }
        ]
      ];


      firstSoln =
        AddOptions[opts][SolveAndCheck][ binomialEqns, vars, s[1] ];

      If[ firstSoln === {}, Return @ {} ];

      Reap[
        MapThread[
          SolveRepeatedly[
            UpdateSystem[ sumEqns, #1, simplify, preEqCheck ],
            GetVariables[ Normal @ #1, s[1] ],
            s[2],
            Normal @ #1,
            #2
          ]&,
          {
            Dispatch /@ firstSoln,
            UpdateConstraints[ { polynomialConstraints, invertibleMatrices }, #, simplify, preEqCheck ]& /@
            firstSoln
          }
        ]
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


Options[SolveAndCheck] :=
  Join[ 
    Options[ReduceByBinomials], 
    { "SumEquations" -> {} } 
  ];

SolveAndCheck[ binEqns_, vars_, s_, opts:OptionsPattern[] ] :=
  Module[{ soln, simplify, sumEqns, preEqCheck, procID, absTime, result },
    simplify =
      Function[
        solutions,
        If[
          solutions =!= {},
          MapAt[
            OptionValue["SimplifyIntermediateResultsBy"],
            solutions,
            { All, All, 2 }
          ],
          {}
        ]
      ];
    sumEqns =
      OptionValue["SumEquations"];
    preEqCheck =
      OptionValue["PreEqualCheck"];
    procID =
      ToString[ Unique[] ];
    
    printlog[ "SAC:init", {procID, binEqns,vars,s,{opts}} ];

    { absTime, result } =
      AbsoluteTiming[
        Select[
          soln =
            simplify @
            AddOptions[opts][
              If[
                OptionValue["NonSingular"],
                SolveNonSingularBinomialSystem,
                SolveBinomialSystem
              ]
              ][ binEqns, vars, s ],
          NotInvalidNonZeroSolutionQ[ sumEqns, preEqCheck ]
        ]
      ];
    
    printlog["SAC:remainingsol", {procID,soln,result}];
    printlog["Gen:results", {procID,result,absTime}];
  
    result
  ];

UpdateSystem[ eqns_, solution_, simplify_, preEqCheck_ ] :=
  With[{
    updatedSystem =
      Map[
        simplify,
        eqns/.solution,
        {2}
      ] //
      Select[ Not[ TrueQ[ preEqCheck[ # ] ] ]& ] //
      DeleteDuplicates
    },
    BinSplit[ updatedSystem, BinomialEquationQ @* preEqCheck ]
  ];

UpdateConstraints[ { polConstr_, invertibleMatrices_ }, solution_, simplify_, preEqCheck_ ] :=
  Module[ { newPolConstr, newInvMats },
    newPolConstr =
      Select[
        Map[
          simplify,
          polConstr/.solution,
          {2}
        ],
        Not[TrueQ[preEqCheck[#]]]&
      ];

    If[
      MemberQ[False] @ newPolConstr,
      Return[ { { False }, {} } ]
    ];

    newInvMats =
      DeleteDuplicatesBy[
        Map[
          simplify,
          invertibleMatrices/.solution,
          {2}
        ] // DeleteCases[ mat_/; TrueQ[ Det[mat] != 0 ] ],
        Abs @* Det
      ];
    
    If[
      MemberQ[ newInvMats, mat_/; simplify[Det[mat]] == 0 ],
      Return[ { {}, { False } } ]
    ];

    { newPolConstr, newInvMats }
  ];

UpdateInvertibleMatrices[ {}, _ ] :=
  {};

UpdateInvertibleMatrices[ mats_List, soln_ ] :=
  DeleteDuplicates[
    ( mats /. Dispatch[soln] ) //
    DeleteCases[ mat_/; TrueQ[Det[mat] != 0] ],
    Abs[Det[#1]] === Abs[Det[#2]]&
  ];
