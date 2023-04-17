(* ::Package:: *)

(*
+---------------------------------------------------------------------------+
|                                                                           |
|                       SOLVING DIOPHANTINE EQUATIONS                       |
|                                                                           |
+---------------------------------------------------------------------------+
*)

(* TODO: ADD ERROR MESSAGE FOR WHEN eqns is not a list of equations *)

Options[SolveDiophantineEquations] = { "FileNames" -> {}, "WeighedBy" -> Null, "OnlyCCode" -> False };
SolveDiophantineEquations[ eqnsList_List, vars_List, ranges_?ProperRangesQ, opts:OptionsPattern[] ] :=
  Module[{
    fileNames = OptionValue["FileNames"],
    w = OptionValue["WeighedBy"],
    eqns, newEqns, newVars, revertVars, CString,tower,sortedVars, freeVariables, freeSolutions,
    newRanges, absTime, result,
    sym = Unique["x"], procID = ToString[Unique[]]
    },

    printlog["SDE:init", {procID,eqnsList,vars,ranges,{opts}}];
    
    { absTime, result } =
    AbsoluteTiming[
      If[
        vars === {},
        printlog["Gen:no_vars", { procID, eqnsList }];
        printlog["Gen:end"];
        Return[{}]
      ];
  
      eqns =
        DeleteCases[True] @
        eqnsList;
  
      If[
        MemberQ[False] @ eqns,
        printlog["Gen:has_False", { procID, eqns }];
        printlog["Gen:end"];
        Return[{}]
      ];
  
      { { newEqns, newRanges }, newVars, revertVars } =
        SimplifyVariables[ { eqns, ranges } , vars, sym  ];
  
      freeVariables =
        Complement[ newVars, GetVars[ newEqns, sym ] ];
  
      freeSolutions =
        Thread[ freeVariables -> #  ]& /@
        Tuples[ freeVariables/.newRanges ];
  
      If[
        newEqns === {},
        printlog["Gen:trivialSystem", {procID}];
        printlog["Gen:end"];
        Return[ freeSolutions/.revertVars ]
      ];
  
      tower =
        If[
          w =!= Null,
          (* THEN *)
          TowerOfExpressions[ newEqns, sym, "LevelSpec" -> 5, "WeighedBy" -> w ],
          (* ELSE *)
          TowerOfExpressions[ newEqns, sym, "LevelSpec" -> 5, "WeighedBy" -> SearchSpaceSize[newRanges] ]
        ];
  
      CString =
        BackTrackCCode[ tower, sym, newRanges ];

      If[
        fileNames === {},
        fileNames = { "source.c", "executable", "results.dat" },
        If[
          !MatchQ[ fileNames, {  _String .. } ],
          Message[SolveDiophantineEquations::wrongfilenameformat,fileNames]
        ]
      ];
  
      sortedVars =
        Flatten @
        tower[[;;,1]];
  
      If[
        OptionValue["OnlyCCode"],
        (* THEN *)
        <|
          "Code" -> CString,
          "Variables" -> newVars,
          "RevertVariables" -> revertVars
        |>,
        (* ELSE *)
        With[ { Csolutions = EvaluateExternally[ CString, sortedVars, fileNames ] },
          Flatten[#,1]& @
          Table[ Join[ sol, freeSol ], { sol, Csolutions }, { freeSol, freeSolutions } ]
        ]/.revertVars
      ]
    ];
    
    printlog["Gen:solutions", {procID,result}];
    
    printlog["Gen:results", {procID,result,absTime}];

    Remove[sym];

    result
  ];

ProperRangesQ[ l_ ] :=
  And[
    Head[ l ] == List,
    And @@ Map[ MatchQ[ #, a_ -> { i_Integer, j_Integer }  ]&, l ]
  ];

SolveDiophantineEquations[ eqnsList_List, vars_List, { rMin_, rMax_ }, opts:OptionsPattern[] ] :=
  SolveDiophantineEquations[ eqnsList, vars, (# -> { rMin, rMax } )& /@ vars, opts ];

SearchSpaceSize[ ranges_ ] :=
  With[{ varsToSize = Dispatch @ MapAt[ Apply[ #2 - #1 + 1 & ], ranges, { All, 2 } ] },
    Times @@ ReplaceAll[ #1, varsToSize  ]&
  ];

BackTrackCCode[ tower_, s_ , ranges_List?ProperRangesQ ] :=
  Module[{ varsList, statementList },
    { varsList, statementList } = Transpose @ DeleteCases[ tower, { {}, _ } ];
    With[{
      nVars = Length @ Flatten @ varsList,
      varsString = ToString @ ReplaceBrackets[ #, s ]& @ Flatten @ varsList,
      maxVal = MaxVal[ statementList, s, ranges ] },
      StringJoin[
        "#include<stdio.h>\n\n",
        "// Variables: ", varsString ,"\n\n",
        "int main(){\n",
        StringJoin @@
        MapThread[
          StringJoin,
          {
            VarsListToForString[ s, ranges, maxVal ] /@ varsList,
            StatementListToIfString[ s ] /@ statementList
          }
        ],
        "printf(\"", StringJoin @@ ConstantArray[ "%i ", nVars - 1 ] , "%i\\n\",",
        StringTake[ varsString, {2,-2} ], ");\n",
        "}"
      ]
    ]
  ];

ReplaceBrackets[ expr_, s_ ] :=
  expr/.s[i_] :> SymbolName[s] <> "_" <> ToString[i];

MaxVal[ eqns_List, s_, ranges_ ] :=
  With[{ filteredEqns = Select[ EqIneqQ ] @ Flatten @ eqns }, (* eqns could contain logical statements *)
    With[{
      worstCaseExpressions = List @@@ Flatten[filteredEqns] /.i_Integer :> Abs[i],
      maxVals = MapAt[ Max[ Abs[ #[[1]] ], Abs[ #[[2]] ] ]&, ranges, { All, 2 } ]
    },
      Max[ worstCaseExpressions/.maxVals ]
    ]
  ];

VarsListToForString[ s_, ranges_, maxVal_ ][ vars_List ] :=
  With[{
    stringList = ReplaceBrackets[ vars, s ]},
    With[{
      type = Which[
        maxVal < 2^7, "char ",
        maxVal < 2^15, "int ",
        maxVal < 2^31, "long int ",
        True, Throw[ "rangeOutOfBounds" ]
      ] },
      StringJoin @@
      MapThread[
        StringJoin[
          "for( ", type, #1, " = ", ToString[ #2[[1]] ], "; ",
          #1, " < ", ToString[ #2[[2]] + 1 ], "; ",
          #1, "++ )\n"
        ]&,
        { stringList, vars/.Dispatch[ranges] }
      ]
    ]
  ];

StatementToString[ s_Symbol ][ expr_ ] :=
  With[ {
    h = Head[ expr ]},
    Which[
      EqIneqQ[ expr ], StringJoin[ " ( ", EqnToString[s][ expr ], " ) " ],
      h === Not,  StringJoin[ " !( ", StatementToString[s][Identity@@expr], " ) "],
      h === And, StringJoin[" ( " , StringJoin @@ Riffle[ StatementToString[s] /@ ( List @@ expr ), " && "] , " ) " ],
      h === Or, StringJoin[" ( " , StringJoin @@ Riffle[ StatementToString[s] /@ ( List @@ expr ), " || "] , " ) " ],
      h === Integer, ToString[ expr ],
      h === True, "1",
      h === Symbol, ToString[ expr ],
      h === False, Throw[ "Expression is false by default!"]
    ]
  ];

(* Convert a list of statements to an if clause in C *)
StatementListToIfString[ s_Symbol ][ sys_List ] :=
  With[{
    stringList = StatementToString[s] /@ sys },
    "if( " <> StringJoin @@ Riffle[ stringList, " &&\n" ] <> " )\n"
  ];

EqnToString[ s_ ][ eqn_?EqIneqQ ] := With[{
  lhs = PolToString[ eqn[[1]], s ],
  rhs = PolToString[ eqn[[2]], s ],
  str = Which[
    Head[eqn] === Equal, " == ",
    Head[eqn] === Unequal, " != ",
    Head[eqn] === LessEqual,  " <= ",
    Head[eqn] === GreaterEqual, " >= ",
    Head[eqn] === Less, " < ",
    Head[eqn] === Greater, " > "
  ]},
  lhs <> str <> rhs
];

EqIneqQ[ x_ ] := With[{
  h = Head[x]},
  Or[
    h === Equal,
    h === Unequal,
    h === LessEqual,
    h === GreaterEqual,
    h === Less,
    h === Greater
  ]
];

PolToString[ i_Integer, s_ ] :=
  ToString[ i ];
PolToString[ pol_, s_ ] :=
  StringJoin @@
  ToString /@ (
    pol //
    Expand //
    ReplacePlus //
    ReplaceTimes //
    ReplacePower //
    ReplaceBrackets[ #, s ]& //
    {#}& //
    Flatten
);

ReplacePlus[ expr_ ] :=
  expr/.Plus[a_,b__]  :> Riffle[{a,b}, " + "];
ReplaceTimes[ expr_ ] :=
  expr/.Times[a_,b__] :> Riffle[{a,b}, " * "];
ReplacePower[ expr_ ] :=
  expr/.Power[a_,b__] :> Riffle[ConstantArray[a,b], " * " ];

(*
+---------------------------------------------------------------------------+
|                                                                           |
|                     EVALUATING THE C CODE EXTERNALLY                      |
|                                                                           |
+---------------------------------------------------------------------------+
*)

EvaluateExternally[ sourceCode_String, vars_, { source_, exec_, data_ } ] :=
  With[{ dir = CreateDirectory[], procID = ToString @ Unique[] },
    
    printlog["EE:directory", { procID, dir }];
    
    ExportCCode[ sourceCode, dir, "FileName" -> source ];
    
    If[
      CompileCCode[ dir, source, "FileName" -> exec ] != 0,
      Print["CompilationError."]
    ];
    
    If[
      RunCCode[ dir, exec, "FileName" -> data ] != 0,
      Print["ExecutionError"]
    ];
    
    ImportResultsCCode[ vars, dir, data ]
  ];

Options[ExportCCode] = {"FileName" -> "source.c"};
ExportCCode[ str_String, dir_String, OptionsPattern[] ] :=
  With[{
    path = FileNameJoin[ { dir, OptionValue[ "FileName" ] } ] },
    Export[ path, str, "Text" ]
  ];

Options[CompileCCode] = {"FileName" -> "executable" };
CompileCCode[ dir_String, sourceName_String, OptionsPattern[] ] :=
  Module[{
    source = FileNameJoin[ { dir, sourceName } ],
    output = FileNameJoin[ { dir, OptionValue["FileName"] } ]},
    Run @
    StringJoin[
      "gcc ", source, " -o ", output, " -Ofast"
    ]
  ];

Options[RunCCode] = {"FileName" -> "results.dat"};
RunCCode[ dir_String, execName_String, OptionsPattern[] ] :=
  Module[{
    pathOfExec = FileNameJoin[ { dir, execName } ],
    pathOfOutput = FileNameJoin[ { dir, OptionValue["FileName"] } ] },
    Run @
    StringJoin[
      pathOfExec, " >> ", pathOfOutput
    ]
  ];

ImportResultsCCode[ vars_, dir_String, file_String ] :=
  Map[
    Thread[ vars -> # ]&,
    Import[ FileNameJoin[ { dir, file } ] ]
  ];