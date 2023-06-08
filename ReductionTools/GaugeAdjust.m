(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2022-09-09 *)

Package["Anyonica`"]

(*
+---------------------------------------------------------------------------+
|                          Multiplicity-Free Case                           |
+---------------------------------------------------------------------------+
*)

PackageExport["UnitaryGaugeQ"]

UnitaryGaugeQ::usage =
  "UnitaryGaugeQ[ring,fSymbols] returns True if the F-symbols fSymbols have unitary F-matrices.";
  (*Here ring is the fusion ring for which the F-symbols are a solution to the pentagon equations. Options are (a) \"Numeric\": set to true to do a numerical check with a standard tolerance of 64, (b) \"Tolerance\" to set the Tolerance used for the internal call to UnitaryMatrixQ (see the documentation centre for more info), (c) \"SimplifyBy\": a function applied to symbolic matrices during the checks for unitarity. This option only works if the \"Numeric\" option is False";*)

UnitaryGaugeQ::notmultfree =
  "Function does not yet support rings with multiplicity.";

UnitaryGaugeQ::wrongsolformat =
  "`1` should be a proper solution to the pentagon equations.";

Options[UnitaryGaugeQ] =
  {
    "SimplifyBy" -> Identity,
    "Numeric" -> False,
    "Tolerance" -> 10^-32,
    "Accuracy" -> 64
  };

UnitaryGaugeQ[ ring_, fSymbols_, OptionsPattern[] ] :=
  Which[
    Mult[ ring ] != 1
    ,
    Message[ UnitaryGaugeQ::notmultfree ];
    Abort[]
    ,
    !PPSQ[ fSymbols ]
    ,
    Message[ UnitaryGaugeQ::wrongsolformat, fSymbols ];
    Abort[]
    ,
    True
    ,
      With[
        {
        simplify  = OptionValue["SimplifyBy"],
        tolerance = OptionValue["Tolerance"],
        numQ      = OptionValue["Numeric"],
        acc       = OptionValue["Accuracy"],
        fMats     = FMatrices[ring]/.Dispatch[fSymbols]
        },
        Catch[
          If[
            numQ,
            Do[
              If[
                !UnitaryMatrixQ[ N[ mat, { Infinity, acc } ], Tolerance -> tolerance ],
                Throw[False]
              ],
              { mat, fMats }
            ],
            Do[
              If[
                !UnitaryMatrixQ[ mat, SameTest -> (Equal[ simplify[#1], simplify[#2] ]&) ],
                Throw[False]
              ],
              { mat, fMats }
            ]
          ];
          True
        ]
      ]
  ];

PackageExport["UGQ"]

UGQ::usage = 
  "Shorthand for UnitaryGaugeQ.";

UGQ = 
  UnitaryGaugeQ;


PackageExport["ToUnitaryGauge"]

ToUnitaryGauge::usage =
  "ToUnitaryGauge[ring,fSymbols] attempts to find a gauge in which the F-matrices corresponding to fSymbols are unitary.";

ToUnitaryGauge::wrongsolformat =
  "`1` should be a proper solution to the pentagon equations.";

(*Here ring is the fusion ring and FSymbols is a solution to the pentagon equations. Options are (a) \"Numeric\": set to true to use numerical recipes. The standard accuracy and tolerance are 64, (b) \"Tolerance\" to set the Tolerance used for the internal call to UnitaryMatrixQ (see the documentation centre for more info), (c) \"SimplifyBy\": a function to simplify intermediate expressions during computation and during the checks for unitarity, (d) \"Use2DConstraints\": set to False if you don't want to use explicit optimization via 2D constraints, (e) all options from SolveSemiLinModZ which are used for the internal call to this function."; *)

(* TODO: add to possible issues: numeric overrides preequalcheck*)

Options[ToUnitaryGauge] :=
  Join[
    {
      "Numeric" -> False,
      "Accuracy" -> 64,
      "Precision" -> Infinity,
      "Tolerance" -> 10^(-16),
      "SimplifyBy" -> Identity,
      "Use2DConstraints" -> True,
      "ReturnGaugeTransform" -> False,
      "GaugeDemands" -> {},
      "PreserveTrivialValues" -> True
    },
    Options[SolveSemiLinModZ]
  ];

ToUnitaryGauge[ ring_FusionRing, FSymb_, opts:OptionsPattern[] ] :=
  If[
    !PPSQ[FSymb]
    ,
    Message[ ToUnitaryGauge::wrongsolformat, FSymb ];
    Abort[]
    ,
    Module[
      {
      z, g, g2, vars, gaugeSymmetries, use2DQ, useDataBaseQ, preEqCheck,
      transforms, unitaryQ, newFs, constraints2D, binomialConstraints, sumConstraints,
      newBinomialConstraints, newSumConstraints, newVars, revertVars, numericQ, simplify, acc, binomialMat,
      trivialGaugeSpace,rankBinomialMat,diagonalElements,expRHS,ZSpace,CSpace,rhsVec,NonOneCoeff,noc,constVec,
      currentTuple,zVec,monomials,ZTuples,binomialSolution,reducedSumConstraints,gauge,instance,
      newFSolution,simplifiedFs,mU,mD,mV,u, procID, positiveQ, realQ, returnTransformQ,
      result, time, gaugeDemands, vacuumConstraints, fSymbols
      },
      z =
        Unique["z"];
      numericQ =
        OptionValue["Numeric"];
      simplify =
        OptionValue["SimplifyBy"];
      acc =
        OptionValue["Accuracy"];
      use2DQ =
        OptionValue["Use2DConstraints"];
      useDataBaseQ =
        OptionValue["UseDatabaseOfSmithDecompositions"];
      preEqCheck =
        If[
          numericQ,
          InfN[acc],
          OptionValue["PreEqualCheck"]
        ];
      returnTransformQ =
        OptionValue["ReturnGaugeTransform"];
      gaugeDemands =
        Equal @@@
        OptionValue["GaugeDemands"];
      
      fSymbols =
        FSymbols[ring];
      
      procID =
        ToString @ Unique[];
      
      printlog[ "TUG:init", {procID,ring,FSymb, {opts} }];
      
      { time, result } =
      Normal @
      AbsoluteTiming[
        
        gaugeSymmetries =
        GaugeSymmetries[ fSymbols, g ];
        
        transforms =
        gaugeSymmetries["Transforms"];
        
        vacuumConstraints =
        If[
          OptionValue["PreserveTrivialValues"],
          (* THEN *)
          Thread[
            ReplaceAll[
              Cases[ fSymbols, $VacuumFPattern ]/.transforms,
              F[__] -> 1
            ] == 1
          ],
          (* ELSE *)
          {}
        ];
        
        newFs =
          MapAt[
            If[ numericQ, InfN[ 2 * acc ], simplify ], (* 2 * acc since will lose some acc during calculations *)
            FSymb,
            { All, 2 }
          ];
        
        Catch[
          (* Could replace this with properGaugeQ *)
          unitaryQ[ symb_ ] :=
          AddOptions[opts][UnitaryGaugeQ][ ring, symb ];
          
          If[ (* Already have unitary gauge *)
            unitaryQ @ newFs
            ,
            printlog[ "TUG:already_unitary", {procID} ];
            Throw @
            If[
              returnTransformQ,
              { FSymb, Thread[ GetVariables[ transforms, g ] -> 1 ] },
              FSymb
            ]
          ];
          
          (*
            Set up the constraints
          *)
          
          (* Construct the 2D gauge constraints *)
          constraints2D =
          If[
            use2DQ,
            Constraints2D[ ring, FMatrices[ring], newFs, g ],
            {}
          ];
          
          (* Get the binomial equations from the set of gauge constraints *)
          { binomialConstraints, sumConstraints } =
          BinomialSplit[
            Join[
              vacuumConstraints,
              UnitaryGaugeConstraints[ ring, g, newFs ],
              constraints2D,
              gaugeDemands
            ],
            "PreEqualCheck" -> preEqCheck
          ];
          
          printlog[ "TUG:constraints", {procID,binomialConstraints,sumConstraints}];
          
          (* Relabel the variables to single indexed variables *)
          { { newBinomialConstraints, newSumConstraints }, newVars, revertVars } =
          SimplifyVariables[
            {  binomialConstraints, sumConstraints },
            g @@@ NZSC[ring],
            u
          ];
          
          (*
            Create the space of solutions to the binomial equations
          *)
          
          { binomialMat, rhsVec } =
            Catch[
              AddOptions[opts][BinToSemiLin][
                Rationalize[ newBinomialConstraints, 10^(-2*acc) ],
                NNZSC[ring],
                u
              ],
              "ZeroVariableInNonSingularSystem"
            ];

          If[
            { binomialMat, rhsVec } === { {}, {} }
            ,
            printlog[ "TUG:zero_variable", {procID,newBinomialConstraints} ];
            printlog[ "Gen:failed" {procID } ];
            Throw @
            If[
              returnTransformQ,
              { $Failed, {} },
              $Failed
            ]
          ];
          
          trivialGaugeSpace =
          TrivialGaugeMatrix[fSymbols];
          
          { mU, mD, mV } =
          If[
            useDataBaseQ,
            AddOptions[opts][MemoizedSmithDecomposition][ binomialMat ],
            SmithDecomposition @ binomialMat
          ];
          
          printlog["TUG:decomposition", {procID,{mU,mD,mV}}];
          
          rankBinomialMat =
            Length[
              diagonalElements = DeleteCases[0] @ Diagonal @ Normal @ mD
            ];
          
          expRHS =
            Inner[ Power, rhsVec, Transpose[ mU ], Times ];
          
          ZSpace =
            mV[[ ;;, ;; rankBinomialMat ]] . DiagonalMatrix[ 1 / diagonalElements ];
          
          NonOneCoeff[ l_ ] :=
            FirstCase[ l, x_ /; preEqCheck[x] != 1 ];
          
          If[
            rankBinomialMat < Length[ expRHS ] &&
            Head[ noc = NonOneCoeff @ expRHS[[ rankBinomialMat + 1;; ]] ] =!= Missing,
            (* THEN *)
            printlog["TUG:nonone_coeff", {procID,expRHS,rankBinomialMat,preEqCheck @ noc }];
            printlog["Gen:failed", {procID}];
            Throw @
            If[
              returnTransformQ,
              { $Failed, {} },
              $Failed
            ],
            (* ELSE *)
            constVec =
            Inner[ Power, rhsVec, Transpose[ ZSpace . mU[[;;rankBinomialMat]] ], Times ]
          ];
          
          CSpace =
            IntegerOrthogonalSpace[ mV[[ ;;, rankBinomialMat+1;; ]], trivialGaugeSpace ];
          
          (* The discrete set of solutions to these equations can be too big to handle.
            Since we only need 1 solution that works, we will construct them one by one *)
          
          ZTuples =
          Range[ LCM @@ Denominator /@ ZSpace ] - 1;
          
          currentTuple =
          ConstantArray[ 0, Length[ ZTuples ] ];
          
          (*
            Loop over all individual solutions of the binomial equations
          *)
          
          While[
            currentTuple =!= Missing["ReachedEnd"],
            
            zVec =
            Exp[ 2 Pi I ( ZSpace . currentTuple ) ];
            
            monomials =
              If[
                CSpace === {{}},
                (* THEN *)
                ConstantArray[ 1, Length[zVec] ],
                (* ELSE *)
                vars =
                z /@ Range[ Dimensions[ CSpace ][[2]] ];
                Inner[ Power, vars, #, Times ]& /@ CSpace
              ];
            
            binomialSolution =
              If[
                numericQ,
                Thread[ newVars -> InfN[ constVec * zVec, 2 * acc ]* monomials ],
                Thread[ newVars -> constVec * zVec * monomials ]
              ];
            
            reducedSumConstraints =
              DeleteCases[True] @
              DeleteDuplicates @
              If[
                numericQ,
                Rationalize[ #, 10^(-2*acc) ]&,
                simplify
              ][
                newSumConstraints/.binomialSolution
              ];
            
            printlog[ "TUG:parametrization", {procID,binomialSolution,reducedSumConstraints}];
            
            (* Set the gauge *)
            
            If[ (* No vars left *)
              vars === {},
              (* THEN: CHECK VALIDITY SOLUTION *)
              realQ =
                If[
                  numericQ,
                  And @@ Thread[ Im[ InfN[ binomialSolution[[;;,2]], acc ] ] == 0 ],
                  And @@ Thread[ Im[ binomialSolution[[;;,2]] ] == 0 ]
                ];
              
              positiveQ = (* Need to use Re because even when the complex part is smaller than 10^-acc, its still complex *)
                And @@ Thread[ Re[ binomialSolution[[;;,2]] ] > 0 ];
              
              gauge =
                If[ (* Solution is both real, positive, and satisfies the sum eqns *)
                  TrueQ[ realQ && positiveQ ],
                  (* THEN *)
                  Dispatch[binomialSolution/.revertVars],
                  (* ELSE *)
                  {}
                ];
              
              printlog[ "TUG:no_vars_conclusion", {procID,binomialSolution,realQ,positiveQ}];
              ,
              (* ELSE FIND SOLUTION REMAINING EQUATIONS*)
              
              instance = (* Have to use Rationalize since FindInstance is bugged for non-exact numbers  *)
                FindInstance[
                  And @@
                  Join[
                    Rationalize[ reducedSumConstraints,  10^(-2*acc) ],
                    Thread[ binomialSolution[[;;,2]] > 0 ]
                  ],
                  vars
                ];
              
              If[
                instance =!= {},
                gauge =
                If[
                  numericQ,
                  Dispatch @
                  MapAt[
                    InfN[acc],
                    binomialSolution/.Dispatch[ First @ instance ]/.revertVars,
                    { All, 2 }
                  ],
                  Dispatch[binomialSolution/.Dispatch[ First @ instance ]/.revertVars]
                ];
                ,
                gauge =
                {};
              ];
              printlog[ "TUG:vars_conclusion", {procID,binomialSolution,gauge}];
            ];
            
            If[
              (* Valid gauge found *)
              gauge =!= {},
              (* THEN *)
              newFSolution =
              MapAt[
                ReplaceAll[newFs],
                transforms/.gauge,
                { All, 2 }
              ];
              (* Try to simplify the F-symbols before performing a final check *)
              If[
                numericQ,
                (* THEN *)
                If[
                  Not[ unitaryQ @ newFSolution ],
                  printlog[ "TUG:sol_not_unitary", {procID} ];
                ];
                Throw @
                If[
                  returnTransformQ,
                  { newFSolution, gauge },
                  newFSolution
                ],
                (* ELSE *)
                simplifiedFs =
                simplify @ newFSolution;
                If[
                  Not[ unitaryQ @ simplifiedFs ],
                  printlog[ "TUG:sol_not_unitary", {procID} ];
                ];
                Throw @
                If[
                  returnTransformQ,
                  { newFSolution, gauge },
                  newFSolution
                ]
              ],
              currentTuple =
              NextTuple[ZTuples] @ currentTuple;
            ]
          ];
          Print["No unitary solution found"];
          If[
            returnTransformQ,
            Throw[ { $Failed, { } } ],
            Throw[ $Failed ]
          ];
        
        ]
      ];
      
      printlog["Gen:results", { procID, result, time } ];
      
      result/.g->List
    
    ]
  ];

PackageExport["TUG"]

TUG::usage =
  "Shorthand for ToUnitaryGauge.";

TUG =
  ToUnitaryGauge;


(* FUNCTIONS USED IN ToUnitaryGauge *)

(* Useful matrices for gauge transforms *)
mL[g_Symbol][ symFMat_ ] :=
  With[{
    a = symFMat[[1,1,1]], b = symFMat[[1,1,2]], c = symFMat[[1,1,3]], d = symFMat[[1,1,4]],
    eLabels = Diagonal[symFMat][[;;,5]]
    },
    DiagonalMatrix @
    Table[
      g[a,b,e] g[e,c,d],
      { e, eLabels }
    ]
  ];

mR[g_Symbol][ symFMat_ ] :=
  With[{
    a = symFMat[[1,1,1]], b = symFMat[[1,1,2]], c = symFMat[[1,1,3]], d = symFMat[[1,1,4]],
    fLabels = Diagonal[symFMat][[;;,6]]
    },
    DiagonalMatrix @
    Table[
      g[a,f,d] g[b,c,f],
      { f, fLabels }
    ]
  ];

mLNormSquared[r_Symbol][ symFMat_ ] :=
  With[{
    a = symFMat[[1,1,1]], b = symFMat[[1,1,2]], c = symFMat[[1,1,3]], d = symFMat[[1,1,4]],
    eLabels = Diagonal[symFMat][[;;,5]]
    },
    DiagonalMatrix @
    Table[
      ( r[a,b,e] r[e,c,d] )^2,
      { e, eLabels }
    ]
  ];

mRNormSquared[r_Symbol][ symFMat_ ] :=
  With[{
    a = symFMat[[1,1,1]], b = symFMat[[1,1,2]], c = symFMat[[1,1,3]], d = symFMat[[1,1,4]],
    fLabels = Diagonal[symFMat][[;;,6]]
    },
    DiagonalMatrix @
    Table[
      ( r[a,f,d] r[b,c,f] )^2,
      { f, fLabels }
    ]
  ];


(* Create unitary gauge constraints on the symbols r where r is the square of the norm of the gauge symbol  *)
UnitaryGaugeConstraints[r_Symbol][ symFMats_List ] :=
  With[{
    L = mLNormSquared[r],
    R = mRNormSquared[r],
    dagger = ConjugateTranspose
    },
    Flatten @
    Table[
      MapThread[
        Equal,
        { dagger[FMat].L[FMat].FMat, R[FMat] },
        2
      ],
      { FMat, symFMats }
    ]
  ];

UnitaryGaugeConstraints[ symFMats_List, r_Symbol, FSymbols_ ] :=
  DeleteCases[True] @ ( UnitaryGaugeConstraints[r][symFMats]/.Dispatch[FSymbols] );

UnitaryGaugeConstraints[ ring_FusionRing, args__ ] :=
  UnitaryGaugeConstraints[ FMatrices[ring], args ];

Constraints2D[ ring_FusionRing, symFMats_List, FSymb_, r_Symbol ] :=
  Module[{zeroFLabels, nzsc , properSymFMats, sqrtNormFSymbols, a, b, c, d, e1, e2, f1, f2 },
    nzsc =
      NZSC[ring];

    (* These only make sense if none of the F-symbols are 0 *)
    zeroFLabels =
      Cases[ FSymb, HoldPattern[ f_ -> 0 ] :> f[[;;4]] ];

    properSymFMats =
      Cases[
        symFMats,
        mat_/;
        Dimensions[mat][[1]] == 2 &&
        FreeQ[ zeroFLabels, mat[[1,1,;;4]] ]
      ];

    sqrtNormFSymbols =
      Dispatch @
      MapAt[
        Sqrt @* Norm,
        FSymb,
        { All, 2 }
      ];

    DeleteCases[True] @
    Flatten @
    Reap[
      Do[
        { a, b, c, d } = List @@ mat[[1,1,{1,2,3,4}]];
        { e1, e2 } = List @@ mat[[;;,1,5]];
        { f1, f2 } = List @@ mat[[1,;;,6]];
        Sow @
        ReplaceAll[
          {
            r[ a, b, e1 ] r[ e1, c, d ] mat[[1,1]] mat[[1,2]] == r[ a, b, e2 ] r[ e2, c, d ] mat[[2,2]] mat[[2,1]],
            r[ a, f1, d ] r[ b, c, f1 ] mat[[2,2]] mat[[1,2]] == r[ a, f2, d ] r[ b, c, f2 ] mat[[1,1]] mat[[2,1]]
          },
          sqrtNormFSymbols
        ],
        { mat, properSymFMats }
      ]
    ][[2]]
  ];


PackageExport["SymmetricGaugeQ"]

SymmetricGaugeQ::usage =
  "SymmetricGaugeQ[ring,fSymb] returns True if the F-matrices corresponding to fSymb are symmetric.";

SymmetricGaugeQ::notmultfree =
  "Function does not yet support rings with multiplicity.";

SymmetricGaugeQ::wrongsolformat =
  "`1` should be a proper solution to the pentagon equations.";

Options[SymmetricGaugeQ] =
  {
    "SimplifyBy" -> Identity,
    "Numeric" -> False,
    "Accuracy" -> 64
  };

SymmetricGaugeQ[ ring_, symb_, OptionsPattern[] ] :=
  Which[
    Mult[ring] != 1
    ,
    Message[ SymmetricGaugeQ::notmultfree ];
    Abort[]
    ,
    !PPSQ[symb]
    ,
    Message[ SymmetricGaugeQ::wrongsolformat, symb ];
    Abort[]
    ,
    True
    ,
      With[
        {
          fMats = (FMatrices[ring] ~ WithMinimumDimension ~ 2)/.symb,
          simplify = OptionValue["SimplifyBy"],
          numQ = OptionValue["Numeric"],
          acc = OptionValue["Accuracy"]
        },
        With[
          {
          check =
          Evaluate @
          If[
            numQ,
            TrueQ[ N[ #, { Infinity, acc } ] == 0 ]&,
            TrueQ[ simplify[ # ] == 0 ]&
          ]
        },
          Catch[
            Do[
              Map[
                If[
                  Not[ check[ # ] ],
                  Throw[ False ]
                ]&,
                mat - Transpose[mat],
                { 2 }
              ]
              , { mat, fMats }
            ];
            True
          ]
        ]
      ]
  ];

PackageExport["SGQ"]

SGQ::usage =
  "Shorthand for SymmetricGaugeQ.";

SGQ =
  SymmetricGaugeQ;


PackageExport["ToSymmetricGauge"]

ToSymmetricGauge::usage =
  "ToSymmetricGauge[ring,FSymb] tries to find a gauge for which the F-matrices are symmetric.";

ToSymmetricGauge::notmultfree =
  "Function does not yet support rings with multiplicity.";

ToSymmetricGauge::wrongsolformat =
  "`1` should be a proper solution to the pentagon equations.";

Options[ToSymmetricGauge] =
  {
    "Numeric" -> False,
    "Accuracy" -> 64,
    "SimplifyBy" -> Identity,
    "ReturnGaugeTransform" -> False,
    "UseDatabaseOfSmithDecompositions" -> True,
    "PreEqualCheck" -> Identity,
    "UnitaryGaugeTransform" -> False,
    "GaugeDemands" -> {},
    "PreserveTrivialValues" -> True
  };


ToSymmetricGauge[ ring_, FSymb_, opts:OptionsPattern[] ] :=
  Which[
    Mult[ring] != 1
    ,
    Message[ ToSymmetricGauge::notmultfree ];
    Abort[]
    ,
    !PPSQ[FSymb]
    ,
    Message[ ToSymmetricGauge::wrongsolformat, FSymb ];
    Abort[]
    ,
    True
    ,
      Module[
        { gaugeSymmetries, transforms, g, u, newFs, constraints, newVars, newConstraints, revertVars,
        unitaryGTQ, acc, numericQ, simplify, symGaugeQ, binomialMat, rhsVec, mU, mD, mV, rankBinomialMat, expRHS,
        NonOneCoeff,noc, diagonalElements,ZSpace,constVec, useDataBaseQ, preEqCheck, CheckSolution,newFSolution,
        time, result, procID, returnTransformQ, gaugeDemands, gauge, vacuumConstraints
        },
        acc =
          OptionValue["Accuracy"];
        numericQ =
          OptionValue["Numeric"];
        simplify =
          OptionValue["SimplifyBy"];
        useDataBaseQ =
          OptionValue["UseDatabaseOfSmithDecompositions"];
        preEqCheck =
          If[
            numericQ,
            InfN[acc],
            OptionValue["PreEqualCheck"]
          ];
        unitaryGTQ =
          OptionValue["UnitaryGaugeTransform"];
        returnTransformQ =
          OptionValue["ReturnGaugeTransform"];
        gaugeDemands =
          Equal @@@
          OptionValue["GaugeDemands"];
        
        procID =
          ToString[Unique[]];
        
        printlog["TSG:init", { procID, ring, FSymb, {opts} } ];
        
        { time, result } = Normal @
        AbsoluteTiming[
          
          gaugeSymmetries =
            GaugeSymmetries[ FSymbols[ring], g ];

          transforms =
            gaugeSymmetries["Transforms"];

          vacuumConstraints =
            If[
              OptionValue["PreserveTrivialValues"],
              (* THEN *)
              Thread[
                ReplaceAll[
                  Cases[ FSymbols[ring], $VacuumFPattern ]/.transforms,
                  F[__] -> 1
                ] == 1
              ],
              (* ELSE *)
              {}
            ];
          
          newFs =
            MapAt[
              If[ numericQ, InfN[2*acc], simplify ],
              FSymb,
              { All, 2 }
            ];
          
          Catch[
            symGaugeQ[ symb_ ] :=
              AddOptions[opts][SymmetricGaugeQ][ ring, symb ];
            
            If[ (* Already in symmetric gauge *)
              symGaugeQ @ newFs,
              printlog["TSG:already_symmetric", {procID}];
              Throw @
              If[
                returnTransformQ,
                { FSymb, Thread[ GetVariables[ transforms, g ] -> 1 ] },
                FSymb
              ];
            ];
            
            (*
              Set up the constraints
            *)
            
            constraints =
              Join[
                vacuumConstraints,
                SymmetricGaugeConstraints[g][ring]/.Dispatch[newFs],
                gaugeDemands
              ];
            
            printlog[ "TSG:constraints", { procID, constraints } ];
            
            { newConstraints, newVars, revertVars } =
              SimplifyVariables[
                constraints,
                g @@@ NZSC[ring],
                u
              ];
            
            { binomialMat, rhsVec } =
              AddOptions[opts][BinToSemiLin][
                Rationalize[ newConstraints, 10^(-2*acc) ],
                NNZSC[ring],
                u
              ];
            
            { mU, mD, mV } =
              If[
                useDataBaseQ,
                AddOptions[opts][MemoizedSmithDecomposition][ binomialMat ],
                SmithDecomposition @ binomialMat
              ];
            
            printlog["TSG:decomposition",{procID,{mU,mD,mV}}];
            
            rankBinomialMat =
              Length[
                diagonalElements = DeleteCases[0] @ Diagonal @ Normal @ mD
              ];
            
            expRHS =
              Inner[ Power, rhsVec, Transpose[ mU ], Times ];

            NonOneCoeff[ l_ ] :=
              FirstCase[ l, x_ /; preEqCheck[x] != 1 ];

            If[
              (* RHS and LHS are incompatible *)
              rankBinomialMat < Length[ expRHS ] &&
              Head[ noc = NonOneCoeff  @ expRHS[[ rankBinomialMat + 1;; ]] ] =!= Missing,
              (* THEN *)
              printlog["TSG:nonone_coeff", { procID, expRHS, rankBinomialMat, preEqCheck @ noc } ];
              Throw[$Failed],
              (* ELSE *)
              ZSpace =
                mV[[ ;;, ;; rankBinomialMat ]] . DiagonalMatrix[ 1 / diagonalElements ];
              constVec =
                Inner[ Power, rhsVec, Transpose[ ZSpace . mU[[;;rankBinomialMat]] ], Times ]
            ];
            
            CheckSolution[ sol_ ] :=
              If[
                Not @ AddOptions[opts][SymmetricGaugeQ][ ring, sol ],
                printlog["TSG:sol_not_symmetric", {procID} ];
              ];
            
            gauge =
              If[
                numericQ,
                Thread[ newVars -> InfN[ constVec, acc ] ],
                Thread[ newVars -> constVec ]
              ]/.revertVars;

            newFSolution =
              ApplyGaugeTransform[ FSymb, gauge, g ];
            
            CheckSolution[ newFSolution ];
            
            If[ (* Don't have further demands on gauges *)
              !unitaryGTQ,
              
              (* THEN *)
              Throw @
              If[
                returnTransformQ,
                { newFSolution, gauge },
                newFSolution
              ],
              
              (* ELSE *)
              If[ (* const vec is not vector of phases *)
                Not[ And @@ Thread[ preEqCheck[ simplify[ Abs[expRHS] ] ] == 1 ] ],
                printlog["TSG:non_unitary_transform", {procID} ]
              ];
             
              Throw @
              If[
                returnTransformQ,
                { newFSolution, gauge },
                newFSolution
              ];
            ];
          ] (* END CATCH *)
        
        ];
        
        printlog[ "Gen:results", { procID, result, time }];
        
        result/.g -> List

      ]
  ];


PackageExport["TSG"]

TSG::usage =
  "Shorthand for ToSymmetricGauge.";

TSG =
  ToSymmetricGauge;


(* Try to find a gauge where the F symbols are symmetric matrices *)
(* Putting solutions into a symmetric gauge *)
SymmetricGaugeConstraints[g_][ ring_FusionRing ] :=
  With[
    {
    transformedFMats =
    ( mL[g][#].#.mR[g][#]& ) /@
    ( FMatrices[ring] ~ WithMinimumDimension ~ 2 )
    },
    DeleteCases[True] @
    Flatten @
    Table[
      MapThread[
        Equal,
        { mat, Transpose[mat] },
        2
      ],
      { mat, transformedFMats }
    ]
  ];


PackageExport["WhichGaugeTransform"]

WhichGaugeTransform::usage =
  "WhichGaugeTransform[ ring, sol1, sol2, s ] returns a gauge transform, in the variable s, that transforms "<>
  "sol1 into sol2";
(*  , where sol1 and sol2 are solutions to the pentagon and/or hexagon equations for the Fusion Ring ring. Here g is the variable that will be used as a gauge variable."*)

WhichGaugeTransform::wrongsolformat =
  "`1` and `2` should be proper solutions to the pentagon equations.";

WhichGaugeTransform::notmultfree =
  "Function does not yet support rings with multiplicity.";

Options[WhichGaugeTransform] :=
  {
    "OnlyMatchAbsoluteValues" -> False,
    "Numeric" -> False,
    "Accuracy" -> 64,
    "UseDatabaseOfSmithDecompositions" -> True,
    "PreEqualCheck" -> Identity,
    "SimplifyBy" -> Identity
  };

WhichGaugeTransform[ ring_, sol1_, sol2_, g_, opts:OptionsPattern[] ] :=
  Which[
    Mult[ring] != 1
    ,
    Message[ WhichGaugeTransform::notmultfree ];
    Abort[]
    ,
    !PPSQ[sol1] || !PPSQ[sol2]
    ,
    Message[ WhichGaugeTransform::wrongsolformat, sol1, sol2 ];
    Abort[]
    ,
    True
    ,
      Module[
        { gaugeSymmetries, transforms, u, constraints, newVars, newConstraints, revertVars,
        acc, numericQ, simplify, binomialMat, rhsVec, mU, mD, mV, rankBinomialMat, expRHS, listOfOnesQ, newSol1, newSol2,
        diagonalElements, ZSpace, constVec, useDataBaseQ, preEqCheck, nGaugeVars, nonZeroFs,  trivialSpace, CSpace,
        monomials, time, result, procID, onlyAbsQ, values1, values2, vars, t, z, normSquaredExtraVars, absSol, a, b
        },
      
        acc =
          OptionValue["Accuracy"];
        numericQ =
          OptionValue["Numeric"];
        simplify =
          OptionValue["SimplifyBy"];
        useDataBaseQ =
          OptionValue["UseDatabaseOfSmithDecompositions"];
        preEqCheck =
          OptionValue["PreEqualCheck"];
        onlyAbsQ =
          OptionValue["OnlyMatchAbsoluteValues"];
        procID =
          ToString[Unique[]];
        
        { newSol1, newSol2 } =
        If[ numericQ,
          N[ { sol1, sol2 } ],
          Map[ simplify, { sol1, sol2 }, { 2 } ]
        ];
        
        { values1, values2 } =
        { newSol1, newSol2 }[[ ;;, ;;, 2 ]];
        
        nGaugeVars =
        NNZSC[ring];
        
        (* printlog["TSG:init", { procID, ring, FSymb, {opts} } ]; *)
        
        { time, result } = Normal @
        AbsoluteTiming[
          
          nonZeroFs =
          ( DeleteCases[ _ -> 0 ] @ newSol1 )[[ ;;, 1 ]];
          
          gaugeSymmetries =
          GaugeSymmetries[
            nonZeroFs,
            g
          ];
          
          transforms =
          gaugeSymmetries["Transforms"];
          
          Catch[
            
            If[ (* Solutions are the same *)
              TrivialGaugeSymmetryEquivalentQ[
                "Numeric" -> numericQ,
                "Accuracy" -> acc,
                "SimplifyBy" -> simplify
              ][ values1, values2 ],
              (* Then *)
              printlog["WGT:same_solution", { procID, sol1, sol2 } ];
              Throw @
              Thread[ ( g @@@ NZSC[ring] ) -> 1  ]
            ];
            
            If[ (* Different zero values for F-symbols *)
              Unequal @@@
              Map[
                Position[0],
                { values1, values2 }
              ],
              (* THEN *)
              printlog["WGT:different_zero_positions", { procID, sol1, sol2 } ];
              Throw[{}],
              (* ELSE *)
              values1 =
              DeleteCases[0] @ values1;
              values2 =
              DeleteCases[0] @ values2;
            ];
            
            constraints =
            DeleteCases[True] @
            Thread[
              values2 ==
              ReplaceAll[
                GaugeTransform[ If[ onlyAbsQ, { g, t }, g ] ][ nonZeroFs ],
                sol1
              ]
            ];
            
            If[
              constraints === False,
              Throw @ { }
            ];
            
            vars =
            GetVariables[ constraints, { g, t } ];
            
            { newConstraints, newVars, revertVars } =
            SimplifyVariables[ constraints, vars , u ];
            
            { binomialMat, rhsVec } =
            AddOptions[opts][BinToSemiLin][
              newConstraints,
              Length[vars],
              u
            ];
            
            { mU, mD, mV } =
            If[
              useDataBaseQ,
              AddOptions[opts][MemoizedSmithDecomposition][ binomialMat ],
              SmithDecomposition @ binomialMat
            ];
            
            printlog["WGT:decomposition",{procID,{mU,mD,mV}}];
            
            rankBinomialMat =
            Length[
              diagonalElements = DeleteCases[0] @ Diagonal @ Normal @ mD
            ];
            
            expRHS =
            Inner[ Power, rhsVec, Transpose[ mU ], Times ];
            
            listOfOnesQ[ l_ ] :=
            And @@
            Thread[ preEqCheck[l] == 1 ];
            
            If[
              (* RHS and LHS are incompatible *)
              rankBinomialMat < Length[ expRHS ] &&
              Not[ TrueQ[ listOfOnesQ[ expRHS[[ rankBinomialMat + 1;; ]] ] ] ],
              
              (* THEN *)
              printlog["WGT:nonone_coeff", { procID, expRHS, rankBinomialMat } ];
              Throw @ { },
              
              (* ELSE *)
              ZSpace =
              mV[[ ;;, ;; rankBinomialMat ]] . DiagonalMatrix[ 1 / diagonalElements ];
              constVec =
              Inner[ Power, rhsVec, Transpose[ ZSpace . mU[[;;rankBinomialMat]] ], Times ]
            ];
            
            trivialSpace =
            With[{
              ntVars = Length[vars] - NNZSC[ring]
            },
              MatrixDirectSum[
                {
                  TrivialGaugeMatrix @ nonZeroFs,
                  If[
                    onlyAbsQ,
                    ConstantArray[ 0, { ntVars, ntVars } ],
                    {{}}
                  ]
                }
              ]
            ];
            
            CSpace =
            IntegerOrthogonalSpace[ mV[[ ;;, rankBinomialMat+1;; ]], trivialSpace ];
            
            monomials =
            If[
              CSpace === {{}},
              ConstantArray[ 1, Length[constVec] ],
              With[ { parameters = z /@ Range[ Dimensions[CSpace][[2]] ] },
                Inner[ Power, parameters, #, Times ]& /@ CSpace
              ]
            ];
            
            If[ (* Any solution goes *)
              !onlyAbsQ,
              
              (* THEN *)
              Throw[
                Thread[ newVars -> constVec ]/.revertVars
              ],
              
              (* ELSE *)
              (* Only want abs vals of solutions to be the same *)
              If[ (* All extra vars are already phases *)
                listOfOnesQ[
                  simplify @* Abs /@ constVec[[ nGaugeVars + 1 ;;  ]]
                ],
                
                (* THEN *)
                Throw[
                  Thread[ newVars -> constVec ][[;;nGaugeVars]]/.revertVars
                ],
                
                (* ELSE: add monomials and try to find vals of monomials s.t. extra vars become phases *)
                
                If[ (* No freedom left *)
                  CSpace === {{}},
                  Throw @ {}
                ];
                
                (* Calculate squared norm of extra vars *)
                normSquaredExtraVars =
                ( ComplexExpand[ Norm /@ constVec ]^2 * ( monomials/. z[i_] :> a[i]^2 + b[i]^2 ) )[[ nGaugeVars + 1 ;; ]];
                
                (* Try to find solution with norm 1 *)
                absSol =
                FindInstance[
                  Thread[ normSquaredExtraVars == 1 ],
                  GetVariables[ normSquaredExtraVars, { a, b } ],
                  Reals
                ];
                
                If[
                  (* No solution found *)
                  absSol === {},
                  
                  (* THEN *)
                  Throw @ {},
                  
                  (* ELSE *)
                  Throw[
                    Thread[
                      newVars -> constVec * monomials/. z[i_] :> a[i] + I b[i] /. absSol /. revertVars
                    ][[ ;; nGaugeVars ]]
                  ]
                ]
              ]
            ];
          ]
        ];
        
        printlog[ "Gen:results", { procID, result, time }];
        
        result
      ]
  ];

PackageExport["WGT"]

WGT::usage = 
  "Shorthand for WhichGaugeTransform.";

WGT = 
  WhichGaugeTransform;


(*
+---------------------------------------------------------------------------+
|                             Multiplicity Case                             |
+---------------------------------------------------------------------------+
*)

PackageExport["FixVacuumRSymbols"]

FixVacuumRSymbols::usage =
  "Returns a list of rules that send vacuum R-symbols to identity operators";

(* If we assume the category can be braided and the R-matrices are unitary,
 then we can fix the gauge by fixing these R-matrices *)
Options[ FixVacuumRSymbols ] =
  {
    "RSymbols" -> None
  };

FixVacuumRSymbols[ ring_, OptionsPattern[] ] :=
  Module[ { OVRSymb, RSymb, ToRule },
    OVRSymb =
      OptionValue["RSymbols"];
    RSymb =
      If[
        OVRSymb === None,
        RSymbols[ring],
        OVRSymb
      ];
    
    ToRule[ r : R[ a_, b_, c_, i_, j_ ] ] :=
      Which[
        a == 1 || b == 1,
          r -> 1,
        a < b,
          r -> If[ i == j, 1, 0 ],
        a >= b,
          r -> If[ i == j, r, 0 ]
      ];

    ToRule /@
    RSymb
  ];


PackageExport["FixVacuumFSymbols"]

FixVacuumFSymbols::usage =
  "Returns a list of rules that send vacuum F-symbols to identity operators";

FixVacuumFSymbols[ ring_ ] :=
  With[ { fs = FSymbols @ ring },
    If[
      Mult[ring] === 1
      ,
      Thread[
        Cases[
          fs,
          F[ 1, __ ] | F[ _, 1, __ ] | F[ _, _, 1, __ ]
        ] -> 1
      ]
      ,
      Join[
        Cases[ fs, x:F[ 1, __, { _, i_, j_ }, { _, k_, l_ } ]        :> ( x -> KroneckerDelta[ j, l ] ) ],
        Cases[ fs, x:F[ _, 1, __, { _, i_, j_ }, { _, k_, l_ } ]     :> ( x -> KroneckerDelta[ j, k ] ) ],
        Cases[ fs, x:F[ _, _, 1, __, { _, i_, j_ }, { _, k_, l_ } ]  :> ( x -> KroneckerDelta[ i, k ] ) ]
      ]
    ]
  ];
