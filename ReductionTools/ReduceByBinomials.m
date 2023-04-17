(* ::Package:: *)

Package["Anyonica`"]

Options[ ReduceByBinomials ] = Options[ SolvePolynomialSystem ];
ReduceByBinomials[ sumEqns_List, binomialEqns_List, vars_List, symbol_, opts:OptionsPattern[] ] :=
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
    
    printlog["RBM:init", {procID,sumEqns,binomialEqns,vars,symbol,{opts}}];

    { absTime, result } =
    AbsoluteTiming[
      constraints =
        Join[
          polynomialConstraints,
          DeterminantConditions @ invertibleMatrices
        ];

      SolveRepeatedly[ { {}, nonMonEqns_ }, _, _, prevSols_, _  ] :=
        Sow[ { nonMonEqns, prevSols } ];
      SolveRepeatedly[
        { monEqns_, nonMonEqns_ },
        variables_,
        symbol[i_],
        prevSols_,
        { pConstraints_, invertibleMatrices_ }
      ] :=
        Module[{ validSoln },
          If[
            Not[ pConstraints === { False } || invertibleMatrices === { False } ],
            
            validSoln =
              SolveAndCheck[
                monEqns,
                variables,
                symbol[i],
                "PolynomialConstraints" -> pConstraints,
                "InvertibleMatrices" -> invertibleMatrices,
                "NonSingular" -> True,
                "SumEquations" -> nonMonEqns,
                "PreEqualCheck" -> preEqCheck
              ];
            
            If[
              validSoln =!= {},
              
              MapThread[
                SolveRepeatedly[
                  UpdateSystem[ nonMonEqns, #1, simplify, preEqCheck ],
                  GetVars[ Normal[#1], symbol[i] ] ,
                  symbol[i+1],
                  Normal[prevSols]/.#1,
                  #2
                ]&,
                {
                  Dispatch /@ validSoln,
                  UpdateConstraints[ { pConstraints, invertibleMatrices }, #, simplify, preEqCheck ]& /@
                  validSoln
                }
              ]
            ]
          ]
        ];

      firstSoln =
        AddOptions[opts][SolveAndCheck][ binomialEqns, vars, symbol[1] ];
      
      If[
        firstSoln === {},
        {},
        Reap[
          MapThread[
            SolveRepeatedly[
              UpdateSystem[ sumEqns, #1, simplify, preEqCheck ],
              GetVars[ Normal @ #1, symbol[1] ],
              symbol[2],
              Normal @ #1,
              #2
            ]&,
            {
              Dispatch /@ firstSoln,
              UpdateConstraints[ { polynomialConstraints, invertibleMatrices }, #, simplify, preEqCheck ]& /@
              firstSoln
            }
          ]
        ][[2]]/.( symbol[_][i_] :> symbol[i] )/.( {x_List} :> x )
      ]
    ];

    printlog["RBM:solutions", {procID, result}];
    printlog["Gen:results", {procID, result, absTime}];

    result
  ];

Options[SolveAndCheck] = Join[ Options[ReduceByBinomials], { "SumEquations" -> {} } ];
SolveAndCheck[ monEqns_, vars_, symbol_, opts:OptionsPattern[] ] :=
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
    
    printlog[ "SAC:init", {procID, monEqns,vars,symbol,{opts}} ];

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
            ][ monEqns, vars, symbol ],
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
