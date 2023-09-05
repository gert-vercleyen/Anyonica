(* ::Package:: *)


Package["Anyonica`"]

(*
+---------------------------------------------------------------------------+
|                                                                           |
|                       LOGGING INTERMEDIATE RESULTS                        |
|                                                                           |
+---------------------------------------------------------------------------+
*)

(* We add a printlog symbol with attribute HoldAllComplete. When we want to print
   to a log this one will be replaced with an actual function by using Block *)
PackageScope["printlog"]

Unprotect[printlog];

SetAttributes[ printlog, HoldAllComplete ];

Protect[printlog];

printlog::usage =
"Inert wrapper that will be replaced by PrintLog if user wants to print log files.";


PackageExport["PrintLog"]

PrintLog::usage =
  "PrintLog[code] exports information of intermediate results to a notebook with clickable hyperlinks.";

PrintLog::cantcreatedirectory =
  "Directory `1` could not be found and could not be created.";

Options[PrintLog] =
  {
    "Directory" -> HomeDirectory[],
    "FileName" -> "ISODateTime"
  };

SetAttributes[ PrintLog, HoldAllComplete ];

PrintLog[ code_ , opts:OptionsPattern[] ] :=
  With[{ dir = OptionValue["Directory"]},
    If[ (* Not an existing directory & can't create directory *)
      !DirectoryQ[ Evaluate @ dir ] && (Quiet[ CreateDirectory[dir] ] === $Failed)
      ,
      Message[ PrintLog::cantcreatedirectory, dir ];
      Abort[]
      ,
      Block[{ fileName, ovfn = OptionValue["FileName"], result },
        fileName =
          If[
            ovfn === "ISODateTime",
            StringReplace[
              FileNameJoin @ { dir, "WLLOG_" <> DateString["ISODateTime"] <> ".nb" },
              ":" -> "-"
            ],
            FileNameJoin[ { dir, ovfn } ]
          ];
        
        Block[ { nbo },
          Export[
            fileName,
            TextCell["Log created at " <> DateString[], "Chapter"]
          ];
          
          nbo =
            NotebookOpen[fileName];

          SetOptions[ nbo, CellGrouping -> Manual ];
          
          Block[ {
            printlog =
              Block[ {
                Internal`$ContextMarks = False
                },
                MyNotebookPrint[ dir, fileName, nbo ][##]
              ]&
            },
            result =
              code;

            NotebookPut[ ApplyCellGrouping @ NotebookGet @ nbo, nbo ];

            NotebookSave[ nbo ];

            result
          ]
        ]
      ]
    ]
  ];

ApplyCellGrouping[ notebook_Notebook ] :=
  ReplaceRepeated[
    notebook,
    {
      a__,
      Longest[
        PatternSequence[
          p1 : Cell[__, CellTags -> {x_, "Start"}, ___ ],
          seq___,
          p2 : Cell[__, CellTags -> {x_, "End"}, ___ ]
        ]
      ],
      b__
    } :>
    { a, CellGroupData[ { p1, seq, p2} ], b }
  ] //
  ReplaceAll[
    Notebook[ { firstCell_Cell, a_Cell, b__, c_Cell }, y__ ] :>
    Notebook[ { firstCell, CellGroupData[ { a, b, c } ] }, y ]
  ];

AddCell[ fileName_, nbo_, cell_ ] :=
  Module[ {},
    SelectionMove[ nbo, After, Notebook ];
    NotebookWrite[
      nbo,
      cell
    ];
    SelectionMove[ nbo, After, Notebook ];

    NotebookSave[ nbo ];
  ];

stringID[ id_ ] :=
  StringDrop[ ToString[id], 1 ];

dataFileName[ id_, dir_, name_ ] :=
  FileNameJoin[{ dir, name <> "_" <> stringID[id]<>".nb"}];

safeExport[ name_, data_ ] :=
  If[
    ByteCount[data] < 10^6
    ,
    Export[ name, data ]
    ,
    Block[{ Internal`$ContextMarks = False },
      Module[ { cd = Compress[data] },
        Export[ name, Hold[Uncompress][cd] ]
      ]
    ]
  ];

inputStyle[ string_ ] :=
  StyleBox[ string, "Input"];

textStyle[ string_ ] :=
  StyleBox[ string, "Text"];

hyperlinkBox[ label_, link_ ] :=
  StyleBox[
    Cell[
      BoxData[
        TemplateBox[
          {
            label,
            { link, None},
            link
          },
          "HyperlinkDefault"
        ]
      ],
      "Input"
    ],
    "Input"
  ];

(* Returns a list of hyperlink boxes, riffled with comma's*)
argumentsHyperlink[ argLinkList_ ] :=
  Riffle[
    hyperlinkBox @@@ argLinkList,
    inputStyle[", "]
  ];

(* Returns a list of boxes that format options
   and exports values of options if they can't
   be presented in a compact way.*)
optionsHyperlink[ id_, dir_, {} ] :=
  {};
optionsHyperlink[ id_, dir_, optionList_ ] :=
  Module[{hl, maxStringLength = 30},
    hl[  opt_ -> val_  ] :=
      With[{
        optString = ToString[ opt ],
        valString = ToString @ InputForm @ val
        },
        {
          inputStyle[ optString <> " -> " ],
          If[ (* Long option argument *)
            StringLength[ valString ] <= maxStringLength,
            (* THEN: use value itself *)
            inputStyle[ valString ],
            (* ELSE: use hyperlink *)
            With[
              { fn = dataFileName[ id, dir, "Option_" <> optString ] },
              safeExport[ fn, val ];
              hyperlinkBox[ optString, fn ]
            ]
          ]
        }
      ];
    Prepend[inputStyle[", "]] @
    Riffle[
      hl /@ optionList,
      inputStyle[", "]
    ]
  ];

startCell[ id_, dir_, funName_, argLinkList_, optionList_ ] :=
  Cell[
    TextData[
      Join[
        {
          inputStyle[
            StringJoin[
              "┌ Start "<>stringID[id]<>"\n",
              funName,
              "[ "
            ]
          ]
        },
        argumentsHyperlink[ argLinkList ],
        optionsHyperlink[ id, dir, optionList ],
        { inputStyle[" ]"]}
      ]
    ],
    "Text",
    CellTags -> { id, "Start" }
  ];

endCell[ id_, label_, link_, time_ ] :=
  Cell[
    TextData[{
      hyperlinkBox[ label, link ],
      inputStyle[ "\nElapsed time: " <> ToString[time]<>"\n" ],
      inputStyle[ "└ End " <> stringID[id] ]
    }],
    "Text",
    CellTags -> { id, "End" }
  ];

failedCell[ id_ ] :=
  Cell[
    TextData[{
      inputStyle["Process " <> ToString[id] <> " failed"]
    }],
    "Text",
    CellTags -> { id, "Warning" },
    Background -> LightRed
  ];

warningCell[ id_, content_ ] :=
  Cell[
    content,
    "Text",
    CellTags -> { id, "Warning" },
    Background -> LightRed
  ];

(* Generic *)

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "Gen:no_vars", { id_, equations_,___} ] :=
  Module[
    { fn },
    fn = dataFileName[ id, dir, "Equations" ];
    safeExport[ fn, equations ];
    
    AddCell[
      fileName,
      nbo,
      warningCell[
        id,
        TextData[
          {
            inputStyle["Warning! "],
            hyperlinkBox[ "Equations", fn ],
            inputStyle[" contain no variables. This can be a cause of errors. Check the options of the solve function used to get rid of unresolved numeric equalities."]
          }
        ]
      ]
    ]
  ];


MyNotebookPrint[ dir_, fileName_, nbo_ ]["Gen:trivial_system", {id_,__} ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      "Trivial system of equations.",
      "Text",
      CellTags -> { id, "Info"}
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ]["Gen:has_False", { id_, data_ } ] :=
  Module[
    { fn },
    fn = dataFileName[ id, dir, "InconsistentSystem" ];
    safeExport[ fn, data ];
    
    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[ "At least one of the " ],
          hyperlinkBox[ "equations", fn ],
          inputStyle[ " is False." ]
        }],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "Gen:failed", { id_ } ] :=
  AddCell[
    fileName,
    nbo,
    failedCell[id]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "Gen:results", { id_, results_, time_ } ] :=
  Module[{fn},
    fn = dataFileName[ id, dir, "Results" ];
    safeExport[ fn, results ];
    AddCell[
      fileName,
      nbo,
      endCell[ id, "Results", fn, time ]
    ]
  ];

(* SolveDiophantineSystem *)
MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SDE:init", { id_, eqns_, vars_, ranges_, optionList_ } ] :=
  Module[ { fn1, fn2, fn3 },
    fn1 = dataFileName[ id, dir, "Equations" ];
    fn2 = dataFileName[ id, dir, "Variables" ];
    fn3 = dataFileName[ id, dir, "Ranges" ];
    safeExport[ fn1, eqns ];
    safeExport[ fn2, vars ];
    safeExport[ fn3, ranges ];
    
    AddCell[
      fileName,
      nbo,
      startCell[
        id,
        dir,
        "SolveDiophantineSystem",
        {
          { "Equations",  fn1 },
          { "Variables",  fn2 },
          { "Ranges",     fn3 }
        },
        optionList
      ]
    ];
    
    AddCell[
     fileName,
     nbo,
     Cell[
       StringJoin[
         "Started solving ",
         ToString[Length[eqns]],
         " equations in ",
         ToString[Length[vars]],
         " variables."
       ],
       "Text",
       CellTags -> {id,"Info"}
     ]
   ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SDE:solutions", { id_, solutions_, ___ } ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      "There are " <> ToString[Length[solutions]]<>" solution(s) to the system of Diophantine equations in the given range.",
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SDE:everything_free", _ ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      "All solutions to the equations are free.",
      "Text",
      CellTags -> {"Info"}
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "EE:directory", {id_, dir_} ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      "C files are stored in directory: "<> dir,
      "Text",
      CellTags -> { "Info" }
    ]
  ];

(* FindZeroValues *)
MyNotebookPrint[ dir_, fileName_, nbo_ ][ "FZV:init", { id_, constraints_, variables_, optionList_ } ] :=
 Module[{ fn1, fn2 },
   fn1 = dataFileName[ id, dir, "Constraints" ];
   fn2 = dataFileName[ id, dir, "Variables" ];
   safeExport[ fn1, constraints ];
   safeExport[ fn2, variables ];
   
   AddCell[
     fileName,
     nbo,
     startCell[
       id,
       dir,
       "FindZeroValues",
       {
         { "Constraints", fn1 },
         { "Variables",   fn2 }
       },
       optionList
     ]
   ]
 ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "FZV:solutions", { id_, soln_, ___ } ] :=
  Module[{fn},
    fn = dataFileName[ id, dir, "Solutions" ];
    safeExport[ fn, soln ];
    
    AddCell[
      fileName,
      nbo,
      Cell[
        ToString[Length[soln]]<> " configuration(s) of 0 values found.",
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "FZV:non_invertible_matrix", { id_, mats_ } ] :=
  Module[{ fn },
    fn = dataFileName[ id, dir, "InvertibleMatrices"];
    safeExport[ fn, mats ];

    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[ "The list of " ],
          hyperlinkBox[ "Invertible Matrices", fn ],
          inputStyle[" contains a non-invertible matrix. Assuming no solutions."]
        }],
        "Text",
        CellTags -> {"Info"}
      ]
    ]
  ];


MyNotebookPrint[ dir_, fileName_, nbo_ ][ "FZV:all_vars_trivial", { id_ } ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      "From the invertible matrices it is found that all variables are non-zero.",
      "Text",
      CellTags -> {"Info"}
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "FZV:preProp", { id_, prop_ } ] :=
  Module[{ fn },
    fn = dataFileName[ id, dir, "NonReducedProposition"];
    safeExport[ fn, prop ];

    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[ "Constructed the " ],
          hyperlinkBox[ "Proposition", fn ],
          inputStyle[" to be satisfied."]
        }],
        "Text",
        CellTags -> {"Info"}
      ]
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "FZV:reduced_system", { id_, prop_, knowns_, equivs_, remainingVars_ } ] :=
  Module[{ fn1, fn2 },
    fn1 = dataFileName[ id, dir, "ReducedSystem"];
    safeExport[ fn1, { prop, knowns, equivs} ];
    fn2 = dataFileName[ id, dir, "RemainingVariables"];
    safeExport[ fn2, remainingVars ];

    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[ "A reduced " ],
          hyperlinkBox[ "System", fn1 ],
          inputStyle[" will be solved for the following "],
          hyperlinkBox[ "Variables", fn2 ],
          inputStyle[".\n(note that the variables from 1D matrices have already been filtered out)"]
        }],
        "Text",
        CellTags -> {"Info"}
      ]
    ]
  ];

(* BreakMultiplicativeSymmetry *)
MyNotebookPrint[ dir_, fileName_, nbo_ ][ "BMS:init", { id_, symmetries_, optionList_ } ] :=
  Module[{ fn },
    fn = dataFileName[ id, dir, "Symmetries"];
    safeExport[ fn, symmetries ];
    
    AddCell[
      fileName,
      nbo,
      startCell[
        id,
        dir,
        "BreakMultiplicativeSymmetry",
        { { "Symmetries", fn } },
        optionList
      ]
    ];
    AddCell[
      fileName,
      nbo,
      Cell[
        "Started breaking multiplicative symmetry.",
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "BMS:fixed_vars", { id_, vars_ } ] :=
  Module[{fn},
    fn = dataFileName[ id, dir, "FixedVars"];
    safeExport[ fn, vars ];
    
    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[ ToString[Length[vars]] <> " " ],
          hyperlinkBox[ "Variable(s)", fn ],
          inputStyle[" fixed."]
        }],
        "Text",
        CellTags -> {"Info"}
      ]
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "BMS:unfixed_demanded_vars", { id_, vars_ } ] :=
  Module[{fn},
    fn = dataFileName[ id, dir, "UnfixedDemandedVars" ];
    safeExport[ fn, vars ];
    
    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[ "Not enough gauge freedom to fix all demanded variables. Some "],
          hyperlinkBox["Variable(s)", fn],
          inputStyle[" could not be fixed up front."]
        }],
        "Text",
        CellTags -> { "Info" }
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "BZV:solutions", { id_, soln_ } ] :=
  MyNotebookPrint[ dir, fileName, nbo ][ "FZV:solutions", { id, soln } ];


(*SolveBinomialSystem*)
MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SMS:init", { id_, eqns_ ,vars_ ,param_, optionList_} ] :=
  Module[{fn1, fn2, fn3},
    fn1 = dataFileName[ id, dir, "Equations" ];
    safeExport[ fn1, eqns ];
    fn2 = dataFileName[ id, dir, "Variables" ];
    safeExport[ fn2, vars ];
    fn3 = dataFileName[ id, dir, "Param" ];
    safeExport[ fn3, param ];
    
    AddCell[
      fileName,
      nbo,
      startCell[
        id,
        dir,
        "SolveBinomialSystem",
        { { "Equations", fn1 }, { "Variables", fn2 }, { "Param", fn3 } },
        optionList
      ]
    ];
    
    AddCell[
      fileName,
      nbo,
      Cell[
        "Solving system of " <> ToString[Length[eqns]]<>" equations in "<>ToString[Length[vars]]<>" variables",
        "Text",
        CellTags -> {"Info"}
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SMS:no_zero_vals", { id_, ___} ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      "No valid combination of 0 values found.",
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SMS:specific_systems", {id_,___} ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      "Started solving remaining equations per set of 0 values",
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SMS:solutions", {id_,solutions_} ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      ToString[Length[solutions]] <> " solution(s) found.",
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];

(*SolveNonSingularBinomialSystem*)
MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SNSBS:init", { id_, eqns_, vars_, param_, optionList_ } ] :=
  Module[{ fn1, fn2, fn3 },
    fn1 = dataFileName[ id, dir, "Equations" ];
    safeExport[ fn1, eqns ];
    fn2 = dataFileName[ id, dir, "Variables" ];
    safeExport[ fn2, vars ];
    fn3 = dataFileName[ id, dir, "Param" ];
    safeExport[ fn3, param ];
    
    AddCell[
      fileName,
      nbo,
      startCell[
        id,
        dir,
        "SolveNonSingularBinomialSystem",
        { { "Equations", fn1 }, { "Variables", fn2 }, { "Param", fn3 } },
        optionList
      ]
    ];
    
    AddCell[
      fileName,
      nbo,
      Cell[
        "Started solving "<>ToString[Length[eqns]]<>" binomial equations in "<> ToString[Length[vars]] <>" variables.",
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SNSBS:has_false_or_zero", {id_,eqns_,___} ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      "System contains either False, or equality of form 0 == Times[vars__] or Times[vars__] == 0, where vars only contains non-zero variables: ",
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SNSBS:has_false_or_zero_2", { id_, eqns_, { message_String, pos_, preSimpl_, postSimpl_ }  } ] :=
  Module[{ fn1 },
    fn1 = dataFileName[ id, dir, "Eqns" ];
    safeExport[ fn1, eqns ];
    
    warningCell[
      TextData[
        inputStyle["System of "],
        hyperlinkBox["Equations", fn1 ],
        inputStyle[" contains "],
        If[
          message == "Zero",
          inputStyle["a nonzero variable that has to equal zero "],
          inputStyle[" False "]
        ],
        inputStyle["at position "<> pos<>"."],
        inputStyle[" Equation before simplification: "<> ToString[InputForm[preSimpl]] <> "\n" ],
        inputStyle[" Equation after simplification: "<> ToString[InputForm[postSimpl]] <> "\n" ],
        inputStyle["This could be caused by the fact that Mathematica doesn't always simplify "<>
        "all expressions by default. If this is the case, using the options \"SimplifyIntermediateResultsBy\" and/or "<>
        "\"PreEqualCheck\" can be of use."
        ]
      ]
    
    ]
  ];


MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SNSBS:no_solutions_log_mon", { id_, ___} ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      "No solutions to the logarithm of the binomial equations.",
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SNSBS:solutions", {id_,solutions_,___} ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      ToString[Length[solutions]] <> " solution(s) found.",
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];
MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SNSBS:constraints_not_satisfied", { id_, preSolutions_, constraints_, invalidPos_ } ] :=
  Module[{ fn1, fn2, fn3 },
    fn1 = dataFileName[ id, dir, "PreSolutions" ];
    safeExport[ fn1, preSolutions ];
    fn2 = dataFileName[ id, dir, "Constraints" ];
    safeExport[ fn2, constraints ];
    
    
    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          hyperlinkBox["Solutions",fn1],
          inputStyle[" at positions "<>ToString[invalidPos]<>" do not satisfy the "],
          hyperlinkBox["constraints", fn2 ]
          }
        ],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

(*SolveModZSpace*)
MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SMZS:init", { id_, system_, optionList_ } ] :=
  Module[{fn},
    fn = dataFileName[ id, dir, "System" ];
    safeExport[ fn, system ];
    
    AddCell[
      fileName,
      nbo,
      startCell[
        id,
        dir,
        "SolveModZSpace",
        { { "System", fn } },
        optionList
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SMZS:nonzero_coeff", { id_, vector_, rank_ }  ] :=
  Module[{fn},
    fn = dataFileName[ id, dir, "RHSVector" ];
    safeExport[ fn, vector ];
    
    AddCell[
      fileName,
      nbo,
      warningCell[
        id,
        TextData[{
          inputStyle["Warning! "],
          hyperlinkBox[ "Vector", fn],
          inputStyle[" contains non-integer entry at position greater than the rank (="<>ToString[rank]<>") of the system. Assuming system has no solutions. This could be because Mathematica doesn't simplify some expressions by default. In this case use the options \"SimplifyBy\" and/or \"IntegerCheck\" to resolve the issue."]
        }]
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SMZS:decomposition", { id_, decomp_ } ] :=
  Module[{fn},
    fn = dataFileName[ id, dir, "SmithDecomposition" ];
    safeExport[ fn, decomp ];
    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle["Computed "],
          hyperlinkBox["Smith Decomposition", fn ],
          inputStyle["."]
        }],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SMZS:solutions", { id_, solutions_ } ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      ToString[Length[solutions]] <> " solution(s) found.",
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];

(*SolveSemiLinModZ*)
MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SSES:init", { id_, system_, param_, optionList_, ___ } ] :=
  Module[{fn, fn2},
    fn = dataFileName[ id, dir, "System" ];
    safeExport[ fn, system ];
    fn2 = dataFileName[ id, dir, "Param" ];
    safeExport[ fn2, param ];
    
     AddCell[
       fileName,
       nbo,
       startCell[
         id,
         dir,
         "SolveSemiLinModZ",
         { { "System", fn }, { "Param", fn2 } },
         optionList
       ]
     ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SSES:decomposition", data_ ] :=
  MyNotebookPrint[ dir, fileName, nbo ][ "SMZS:decomposition", data ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SSES:nonone_coeff", { id_, vector_, rank_, noc_ }  ] :=
  Module[{fn},
    fn = dataFileName[ id, dir, "RHSVector" ];
    safeExport[ fn, vector ];
    
    AddCell[
      fileName,
      nbo,
      warningCell[
        id,
        TextData[{
          inputStyle["Warning! "],
          hyperlinkBox[ "Vector", fn],
          inputStyle[
            StringJoin[
              " contains value(s) { ",
              ToString @ InputForm[noc],
              ", ... } different from 1 at position greater than the rank (="<>ToString[rank]<>") of the system. Assuming system has no solutions. This could be because Mathematica fails to simplify some expressions. In this case use the options \"SimplifyBy\" and/or \"IntegerCheck\" to resolve the issue."
            ]
          ]
        }]
      ]
    ];
  ];

(*SolvePolynomialSystem*)
MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SPS:init", { id_, eqns_,vars_,param_,optionList_} ] :=
  Module[{ fn1, fn2, fn3 },
    fn1 = dataFileName[ id, dir, "Equations" ];
    safeExport[ fn1, eqns ];
    fn2 = dataFileName[ id, dir, "Variables" ];
    safeExport[ fn2, vars ];
    fn3 = dataFileName[ id, dir, "Param" ];
    safeExport[ fn3, param ];
    
    AddCell[
      fileName,
      nbo,
      startCell[
        id,
        dir,
        "SolvePolynomialSystem",
        { { "Equations", fn1 }, { "Variables", fn2 }, { "Param", fn3 } },
        optionList
      ]
    ];
    
    AddCell[
      fileName,
      nbo,
      Cell[
        StringJoin[
          "Started solving system of ",
          ToString[Length[eqns]],
          " polynomial equations in ",
          ToString[Length[vars]],
          " variables."
        ],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SPS:solutions", { id_, solutions_, ___ } ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      ToString[Length[solutions]] <> " solution(s) found to system of polynomial equations.",
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];

(*ReduceByBinomials*)
MyNotebookPrint[ dir_, fileName_, nbo_ ][ "RBM:init", { id_, sumEqns_,monEqns_,vars_,symbol_,optionList_ } ] :=
  Module[{ fn1, fn2, fn3, fn4 },
    fn1 = dataFileName[ id, dir, "SumEquations" ];
    safeExport[ fn1, sumEqns ];
    fn2 = dataFileName[ id, dir, "BinomialEquations" ];
    safeExport[ fn2, monEqns ];
    fn3 = dataFileName[ id, dir, "Variables" ];
    safeExport[ fn3, vars ];
    fn4 = dataFileName[ id, dir, "Symbol" ];
    safeExport[ fn4, symbol ];
    
    AddCell[
      fileName,
      nbo,
      startCell[
        id,
        dir,
        "ReduceByBinomials",
        { { "SumEquations", fn1 }, {"BinomialEquations", fn2 }, {"Variables", fn3 }, { "Symbol", fn4 } },
        optionList
      ]
    ];
    
    AddCell[
      fileName,
      nbo,
      Cell[
        StringJoin[
          "Started reducing system of ",
          ToString[Length[sumEqns]],
          " equations containing sums by using ",
          ToString[Length[monEqns]],
          " binomial equations. Total number of variables: ",
          ToString[Length[vars]],
          "."
        ],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "RBM:solutions", { id_, solutions_, ___ } ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      ToString[Length[solutions]] <> " solution(s) found.",
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];

(*SolveAndCheck*)
MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SAU:init", { id_, binomialEqns_,sumEqns_,vars_,symbol_,optionList_, ___ } ] :=
  Module[{fn1,fn2,fn3,fn4},
    fn1 = dataFileName[ id, dir, "BinEqns" ];
    safeExport[ fn1, binomialEqns ];
    fn2 = dataFileName[ id, dir, "Variables" ]; 
    safeExport[ fn2, vars ];
    fn3 = dataFileName[ id, dir, "Symbol" ];
    safeExport[ fn3, symbol ];
    fn4 = dataFileName[ id, dir, "NonBinEqns" ];
    safeExport[ fn4, sumEqns ];
    
    AddCell[
      fileName,
      nbo,
      startCell[
        id,
        dir,
        "SolveAndUpdate",
        { { "BinEqns", fn1 }, { "NonBinEqns", fn4 }, { "Variables", fn2 }, { "Symbol", fn3 } },
        optionList
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SAU:updated_sys", { id_, sumEqns_, constr_ } ] :=
  Module[{fn1,fn2},
    fn1 = dataFileName[ id, dir, "NewSumEqns" ];
    safeExport[ fn1, sumEqns ];
    fn2 = dataFileName[ id, dir, "NewConstraints" ];
    safeExport[ fn2, constr ];

    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[ "Updated the "],
          hyperlinkBox[ "non-binomial equations", fn1 ],
          inputStyle[ " and the "],
          hyperlinkBox[ "constraints", fn2 ]
        }],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SAU:invalid_positions", { id_, positions_,  ___ } ] :=
Module[{},
  AddCell[
    fileName,
    nbo,
    Cell[
      TextData[{
        inputStyle[
          StringJoin[
            "Solutions with positions ",
            ToString[positions],
            " are rejected because the updated equations contain either False, 0 == monomial, monomial == 0, ",
            " or because some of the constraints are not satisfied."
          ]
        ]
      }],
      "Text",
      CellTags -> { id, "Info" }
    ]
  ]
];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SAU:remainingsol", { id_, soln_, validSoln_,  ___ } ] :=
  Module[{fn1,fn2},
    fn1 = dataFileName[ id, dir, "Solutions" ];
    safeExport[ fn1, soln ];
    fn2 = dataFileName[ id, dir, "RemainingSolutions" ];
    safeExport[ fn2, validSoln ];
    
    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[
            StringJoin[
              "There are ",
              ToString[Length[soln]],
              " "
            ]
          ],
          hyperlinkBox[ "Solution(s)", fn1 ],
          inputStyle[
            StringJoin[
              " to the binomial equations. ",
              ToString[Length[validSoln]],
              " "
            ]
          ],
          hyperlinkBox[ "Solution(s)", fn2 ],
          inputStyle[" of these are compatible with the equations containing sums."]
        }],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

(* ToUnitaryGauge *)
MyNotebookPrint[ dir_, fileName_, nbo_ ][ "TUG:init", { id_, ring_, FSymb_, optionList_,___ } ] :=
  Module[{fn1, fn2},
    fn1 = dataFileName[ id, dir, "Ring" ];
    safeExport[ fn1, ring ];
    fn2 = dataFileName[ id, dir, "FSymbols" ];
    safeExport[ fn2, FSymb ];

    AddCell[
      fileName,
      nbo,
      startCell[
        id,
        dir,
        "ToUnitaryGauge",
        { { "Ring", fn1 }, { "FSymbols", fn2 } },
        optionList
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "TUG:already_unitary", { id_} ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      "The FSymbols are already in a unitary gauge",
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "TUG:constraints", { id_, binomialConstr_, sumConstr_, ___ } ] :=
  Module[{fn1,fn2},
    fn1 = dataFileName[ id, dir, "BinomialConstraints" ];
    safeExport[ fn1, binomialConstr ];
    fn2 = dataFileName[ id, dir, "SumConstraints" ];
    safeExport[ fn2, sumConstr ];

    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[
            StringJoin[
              "The demands for a unitary gauge is a system of ",
              ToString[Length[binomialConstr]],
              " "
            ]
          ],
          hyperlinkBox[ "Binomial Equations", fn1 ],
          inputStyle[
            StringJoin[
              " and ",
              ToString[Length[sumConstr]],
              " "
            ]
          ],
          hyperlinkBox[ "Equations with sums", fn2 ],
          inputStyle[", where the variables are radii of complex numbers."]
        }],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "TUG:decomposition", data_ ] :=
  MyNotebookPrint[ dir, fileName, nbo ][ "SMZS:decomposition", data ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "TUG:nonone_coeff", data_ ] :=
  MyNotebookPrint[ dir, fileName, nbo ][ "SSES:nonone_coeff", data ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "TUG:parametrization", { id_, parametrization_, rsc_, ___ } ] :=
  Module[{fn1,fn2},
    fn1 = dataFileName[ id, dir, "Parametrization" ];
    safeExport[ fn1, parametrization ];
    fn2 = dataFileName[ id, dir, "ReducedSumConstraints" ];
    safeExport[ fn2, rsc ];

    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[ "Used the "],
          hyperlinkBox[ "Solution To The Binomial Equations", fn1 ],
          inputStyle[" to reduce the "],
          hyperlinkBox[ "Equations With Sums", fn2 ]
        }],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "TUG:zero_variable", { id_, binEqns_ } ] :=
  Module[{fn1},
    fn1 = dataFileName[ id, dir, "inconsistent_system" ];
    safeExport[ fn1, binEqns ];

    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[ "The "],
          hyperlinkBox[ "system of binomial equations", fn1 ],
          inputStyle[" implies that a gauge variable is zero. Assuming no solutions."]
        }],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "TUG:no_vars_conclusion", { id_, sol_, sumQ_, realQ_, posQ_ } ] :=
  Module[{fn},
    fn = dataFileName[ id, dir, "Parametrization" ];
    safeExport[ fn, sol ];

    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle["There are no variables left to solve for. The tests whether the "],
          hyperlinkBox[ "Solution", fn ],
          inputStyle[
            StringJoin[
              "\n(1) satisfies the equations with sums,",
              "\n(2) is positive, and ",
              "\n(3) is real \nreturned \n(1) ",
              ToString[sumQ],
              ",\n(2) ",
              ToString[realQ],
              ",\n(3) ",
              ToString[posQ],
              ".\n",
              "The solution will therefore be ",
              If[ sumQ && realQ && posQ, "accepted", "rejected"]
            ]
          ]
        }],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "TUG:vars_conclusion", { id_, sol_, gauge_, ___ } ] :=
  Module[{fn1,fn2},
    fn1 = dataFileName[ id, dir, "Parametrization" ];
    safeExport[ fn1, sol ];
    fn2 = dataFileName[ id, dir, "Gauge" ];
    safeExport[ fn2, gauge ];

    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle["The "],
          hyperlinkBox[ "Solution", fn1 ],
          inputStyle[
            StringJoin[
              " to the binomial equations does",
              If[ gauge === {}, " not",""],
              " admit a "
              ]
            ],
          If[
            gauge === {},
            inputStyle["unitary gauge"],
            hyperlinkBox[ "Unitary Gauge", fn2 ]
          ]
        }],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "TUG:sol_not_unitary", { id_, ___ } ] :=
  AddCell[
    fileName,
    nbo,
    warningCell[
      id,
      "After performing the gauge transform, UnitaryGaugeQ still returns False. Best to double check the result manually. If the symbols are indeed not in a unitary gauge, try adjusting the options for ToUnitaryGauge."
    ]
  ];


(* ToSymmetricGauge *)
MyNotebookPrint[ dir_, fileName_, nbo_ ][ "TSG:init", { id_, ring_, FSymb_, optionList_,___ } ] :=
Module[{fn1, fn2},
  fn1 = dataFileName[ id, dir, "Ring" ];
  safeExport[ fn1, ring ];
  fn2 = dataFileName[ id, dir, "FSymbols" ];
  safeExport[ fn2, FSymb ];
  
  AddCell[
    fileName,
    nbo,
    startCell[
      id,
      dir,
      "ToSymmetricGauge",
      { { "Ring", fn1 }, { "FSymbols", fn2 } },
      optionList
    ]
  ];
];


MyNotebookPrint[ dir_, fileName_, nbo_ ][ "TSG:already_symmetric", { id_} ] :=
AddCell[
  fileName,
  nbo,
  Cell[
    "The F-symbols are already in a symmetric gauge",
    "Text",
    CellTags -> { id, "Info" }
  ]
];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "TSG:constraints", { id_, symmetricConstr_, ___ } ] :=
Module[{fn1},
  fn1 = dataFileName[ id, dir, "SymmetricConstraints" ];
  safeExport[ fn1, symmetricConstr ];
  
  AddCell[
    fileName,
    nbo,
    Cell[
      TextData[{
        inputStyle[
          StringJoin[
            "The demands for a symmetric gauge is a system of ",
            ToString[Length[symmetricConstr]],
            " "
          ]
        ],
        hyperlinkBox[ "Binomial Equations", fn1 ]
      }],
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];
];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "TSG:decomposition", data_ ] :=
  MyNotebookPrint[ dir, fileName, nbo ][ "SMZS:decomposition", data ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "TSG:nonone_coeff", data_ ] :=
  MyNotebookPrint[ dir, fileName, nbo ][ "SSES:nonone_coeff", data ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "TSG:sol_not_symmetric", { id_, ___ } ] :=
  AddCell[
    fileName,
    nbo,
    warningCell[
      id,
      "After performing the gauge transform, SymmetricGaugeQ still returns False. Best to double check the result manually. If the symbols are indeed not in a symmetric gauge, try adjusting the options for ToSymmetricGauge."
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "TSG:non_unitary_transform", { id_ } ] :=
  AddCell[
    fileName,
    nbo,
    warningCell[
      id,
      "The transformation to make the solution symmetric might put it in a non-unitary gauge. Best to check unitarity of results."
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "TSG:off_diagonal_zero", { id_ } ] :=
  AddCell[
    fileName,
    nbo,
    warningCell[
      id,
      "There are F-matrices with off-diagonal zero elements that do not appear symmetrically. Impossible to fix symmetric gauge."
    ]
  ];
(* SimplifyVariables *)
MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SV:subs", { id_, subs_, ___ } ] :=
  Module[{fn},
    fn = dataFileName[ id, dir, "Substitution" ];
    safeExport[ fn, subs ];
    
    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          hyperlinkBox[ "Substituting", fn ],
          inputStyle[" variables."]
          }
        ],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ]
  ];

(* SolveMultiplicityFreePentagonEquations *)
MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SMFPE:init", { id_, ring_, optionList_ } ] :=
  Module[ { fn1 },
    fn1 = dataFileName[ id, dir, "Ring" ];
    safeExport[ fn1, ring ];
    
    AddCell[
      fileName,
      nbo,
      startCell[
        id,
        dir,
        "SolveMultiplicityFreePentagonEquations",
        { { "Ring", fn1 } },
        optionList
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SMFPE:solutions", { id_, solutions_, ___ } ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      ToString[Length[solutions]] <> " solution(s) found.",
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SMFPE:solving_systems", { id_ } ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      "Started search for roots of Groebner bases.",
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];
(* MultiplicityFreePentagonGroebnerSystems *)

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "MFPGS:init", { id_, ring_, var_, optionList_ } ] :=
  Module[ { fn1, fn2 },
    fn1 = dataFileName[ id, dir, "Ring" ];
    safeExport[ fn1, ring ];
    fn2 = dataFileName[ id, dir, "Var" ];
    safeExport[ fn2, var ];
    
    AddCell[
      fileName,
      nbo,
      startCell[
        id,
        dir,
        "MultiplicityFreePentagonGroebnerSystems",
        { { "Ring", fn1 }, { "Var", fn2 } },
        optionList
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "MFPGS:systems", { id_, systems_, ___ } ] :=
  Module[{fn},
    fn = dataFileName[ id, dir, "Systems" ];
    safeExport[ fn, systems ];
    
    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[
          {
            inputStyle["Prepared "],
            hyperlinkBox[ "Systems", fn ],
            inputStyle[" for Groebner basis calculations"]
          }
        ],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "MFPGS:quicksolve", { id_, systems_ } ] :=
  Module[{fn},
    fn = dataFileName[ id, dir, "quicksolve" ];
    safeExport[ fn, systems ];

    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[
          {
            inputStyle["Reduced "],
            hyperlinkBox[ "Systems", fn ],
            inputStyle[" by solving those with only 1 variable."]
          }
        ],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

(* MultiplicityFreeHexagonGroebnerSystems *)
MyNotebookPrint[ dir_, fileName_, nbo_ ][ "MFHGS:init", { id_, ring_, var_, optionList_ } ] :=
  Module[ { fn1, fn2 },
    fn1 = dataFileName[ id, dir, "Ring" ];
    safeExport[ fn1, ring ];
    fn2 = dataFileName[ id, dir, "Var" ];
    safeExport[ fn2, var ];

    AddCell[
      fileName,
      nbo,
      startCell[
        id,
        dir,
        "MultiplicityFreeHexagonGroebnerSystems",
        { { "Ring", fn1 }, { "Var", fn2 } },
        optionList
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "MFHGS:systems", { id_, systems_, x___ } ] :=
  MyNotebookPrint[ dir, fileName, nbo ][ "MFPGS:systems", { id, systems, x } ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "MFHGS:quicksolve", { id_, systems_ } ] :=
  MyNotebookPrint[ dir, fileName, nbo ][ "MFPGS:quicksolve", { id, systems } ];


(* PreparePentagonSolverInput *)
MyNotebookPrint[ dir_, fileName_, nbo_ ][ "PPSI:init", { id_, ring_, optionList_ } ] :=
  Module[ { fn1 },
    fn1 = dataFileName[ id, dir, "Ring" ];
    safeExport[ fn1, ring ];
    
    AddCell[
      fileName,
      nbo,
      startCell[
        id,
        dir,
        "PreparePentagonSolverInput",
        { { "Ring", fn1 } },
        optionList
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "PPSI:original_system", { id_, pentEqns_, vars_, ___ } ] :=
  Module[{fn1,fn2},
    fn1 = dataFileName[ id, dir, "PentagonEquations" ];
    safeExport[ fn1, pentEqns ];
    fn2 = dataFileName[ id, dir, "Variables" ];
    safeExport[ fn2, vars ];
    
    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle["Dealing with a total of " <> ToString[Length[pentEqns]] <> " " ],
          hyperlinkBox[ "pentagon equations", fn1 ],
          inputStyle[" in " <> ToString[Length[vars]]<> " "],
          hyperlinkBox[ "variables", fn2 ]
        }],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "PPSI:zero_Fs", { id_ } ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      "Determining which F-symbols could be non-trivially 0",
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "PPSI:zero_Fs_results", { id_, zeros_, ___ } ] :=
  Module[{fn},
    fn = dataFileName[ id, dir, "ZeroFSymbols" ];
    safeExport[ fn, zeros ];
    
    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[ ToString[Length[zeros]] <>" " ],
          hyperlinkBox["configurations", fn ],
          inputStyle[ " found"]
        }],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "PPSI:fixing_gauge", { id_, ___ } ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      "Using gauge symmetries to fix F-symbols that can't be 0",
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "PHSI:init", { id_, ring_, optionList_ } ] :=
  Module[ { fn1 },
    fn1 = dataFileName[ id, dir, "Ring" ];
    safeExport[ fn1, ring ];

    AddCell[
      fileName,
      nbo,
      startCell[
        id,
        dir,
        "PrepareHexagonSolverInput",
        { { "Ring", fn1 } },
        optionList
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "PPSI:fixed_fs", { id_, fixedFs_, ___ } ] :=
  Module[{fn},
    fn = dataFileName[ id, dir, "FixedFs" ];
    safeExport[ fn, fixedFs ];

    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[ "The " ],
          hyperlinkBox[ "value(s)", fn ],
          inputStyle[ "of " <> ToString[Length[fixedFs]]<> " F-symbols have been fixed." ]
        }],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "PPSI:fixing_extra_gauges", { id_, ___ } ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      "Fixing remaining gauges per configuration of 0 F's",
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "PPSI:no_gauge_freedom_left", { id_, ___ } ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      "No gauge freedom left. Substituting zeros, and updating system.",
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];


MyNotebookPrint[ dir_, fileName_, nbo_ ][ "PPSI:gauge_freedom_left", { id_, ___ } ] :=
AddCell[
  fileName,
  nbo,
  Cell[
    "Extra symbols can be fixed.",
    "Text",
    CellTags -> { id, "Info" }
  ]
];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "PPSI:restricting_gauges", { id_ } ] :=
AddCell[
  fileName,
  nbo,
  Cell[
    "Restricting gauge symmetries to to take account of trivial F-symbols.",
    "Text",
    CellTags -> { id, "info" }
  ]
];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "PHSI:knowns", { id_, knowns_, ___ } ] :=
  Module[{fn},
    fn = dataFileName[ id, dir, "Knowns" ];
    safeExport[ fn, knowns ];

    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[ "Assuming that some " ],
          hyperlinkBox[ "value(s)", fn ],
          inputStyle[ " are known."  ]
        }],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "PHSI:symmetries", { id_, symmetries_, ___ } ] :=
  Module[{fn},
    fn = dataFileName[ id, dir, "Symmetries" ];
    safeExport[ fn, symmetries ];

    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[ "Generated reduced set of "],
          hyperlinkBox[ "symmetries", fn ],
          inputStyle[ " using the assumption that the known values are fixed." ]
        }],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];


MyNotebookPrint[ dir_, fileName_, nbo_ ][ "RBL:init", { id_, pols_, var_, optionList_ } ] :=
  Module[ { fn1, fn2 },
    fn1 = dataFileName[ id, dir, "Polynomials" ];
    safeExport[ fn1, pols ];
    fn2 = dataFileName[ id, dir, "Var" ];
    safeExport[ fn2, var ];

    AddCell[
      fileName,
      nbo,
      startCell[
        id,
        dir,
        "ReduceByLinearity",
        { { "Polynomials", fn1 } , { "Var", fn2 } },
        optionList
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "RBL:pol_problem", { id_, pol_, pols_ } ] :=
  Module[{fn1},
    fn1 = dataFileName[ id, dir, "Polynomials" ];
    safeExport[ fn1, pols ];

    fn2 = dataFileName[ id, dir, "PolProblem" ];
    safeExport[ fn2, pol ];

    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[ "The " ],
          hyperlinkBox[ "polynomial system", fn1 ],
          inputStyle[ " contains a non-zero "],
          hyperlinkBox["polynomial", fn2 ],
          inputStyle[ " and therefore it has no roots." ]
        }],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "RBL:rule_problem", { id_, rules_ } ] :=
  Module[{fn1},
    fn1 = dataFileName[ id, dir, "Rules" ];
    safeExport[ fn1, rules ];

    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[ "The set of " ],
          hyperlinkBox[ "variables", fn1 ],
          inputStyle[ " contains a variable with value 0 which is not allowed by assumption."]
        }],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "RBL:simplest_rule", { id_, rule_, s_ } ] :=
  Module[ { NiceForm },
    NiceForm[ a_ -> b_ ] :=
      With[{ sString = ToString[s] },
        StringReplace[
          ToString @ InputForm[ a -> b["Numerator"]/b["Denominator"] ],
          { "*" -> " ", sString -> StringReplace[ sString, x_ ~~ "$" ~~ __ :> x ] }
        ]
      ];
    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[ "Finished search for the simplest rule: " ],
          inputStyle[ NiceForm @ rule ]
        }],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "RBL:memory_overflow", { id_ } ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      TextData[{
        inputStyle[ "More memory than the given bound was required to update the system. Sowing non-updated system. \nNote: this is not an error. The system returned is only not fully reduced." ]
      }],
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "RBL:no_rules_left", { id_ } ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      TextData[{
        inputStyle[ "No rules left. Sowing data." ]
      }],
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];


MyNotebookPrint[ dir_, fileName_, nbo_ ][ "RBL:nonzero_denominator", { id_, rule_ } ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      TextData[{
        inputStyle[ "The variable has a non-zero denominator.\nThis is either trivially true or follows from previous assumptions." ]
      }],
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "RBL:assuming_nonzero_denominator", { id_, rule_ } ] :=
  Module[{fn1},
    fn1 = dataFileName[ id, dir, "SimplestRule" ];
    safeExport[ fn1, rule ];

    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[ "The variable has a non-monomial denominator.\n-> Started splitting cases." ]
        }],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];

    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[ "Assuming denominator is non-zero." ]
        }],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_][ "RBL:reduction", { id_, denom_, s_ } ] :=
  Module[{NiceForm},
    NiceForm[expr_] :=
      With[{ sString = ToString[s] },
        StringReplace[
          ToString @ InputForm @ expr,
          { "*" -> " ", sString -> StringReplace[ sString, x_ ~~ "$" ~~ __ :> x ] }
        ]
      ];

    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[ "Reducing system modulo " ],
          inputStyle[ NiceForm @ denom ],
          inputStyle["."]
        }],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ]
  ];



(*MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SMFPE:systems", { id_, solverInput_, ___ } ] :=*)
(*  Module[{fn},*)
(*    fn = dataFileName[ id, dir, "SolverInput" ];*)
(*    safeExport[ fn, solverInput ];*)
(*    *)
(*    AddCell[*)
(*      fileName,*)
(*      nbo,*)
(*      Cell[*)
(*        TextData[{*)
(*          inputStyle["Equations will be solved for " <> ToString[Length[solverInput]] <> "" ],*)
(*          hyperlinkBox[ "systems", fn ]*)
(*        }],*)
(*        "Text",*)
(*        CellTags -> { id, "Info" }*)
(*      ]*)
(*    ];*)
(*  ];*)

(*MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SMFPE:validity_check", { id_, validityCheck_, ___ } ] :=*)
(*  Module[{fn},*)
(*    fn = dataFileName[ id, dir, "ValidityCheck" ];*)
(*    safeExport[ fn, validityCheck ];*)
(*    *)
(*    AddCell[*)
(*      fileName,*)
(*      nbo,*)
(*      warningCell[*)
(*        id,*)
(*        "Not all solutions could be verified. Double check solutions " <>*)
(*        ToString[ Position[ validityCheck, False ] // Flatten ]*)
(*      ]*)
(*    ];*)
(*  ];*)


MyNotebookPrint[ dir_, fileName_, nbo_ ][ "SMFHE:init", { id_, ring_, optionList___ } ] :=
  Module[{fn},
    fn = dataFileName[ id, dir, "Ring" ];
    safeExport[ fn, ring ];
    
    AddCell[
      fileName,
      nbo,
      startCell[
        id,
        dir,
        "SolveMultiplicityFreeHexagonEquations",
        { { "Ring", fn } },
        optionList
      ]
    ];
  ];

(* RestrictMultiplicativeSymmetries *)
MyNotebookPrint[ dir_, fileName_, nbo_ ][ "RMS:init", { id_, sym_, vars_, symbol_, optsList_ } ] :=
  Module[{fn1, fn2, fn3 },
    fn1 = dataFileName[ id, dir, "Symmetries" ];
    safeExport[ fn1, sym ];
    fn2 = dataFileName[ id, dir, "Variables" ];
    safeExport[ fn2, vars ];
    fn3 = dataFileName[ id, dir, "Symbol" ];
    safeExport[ fn3, symbol ];
    
    AddCell[
      fileName,
      nbo,
      startCell[
        id,
        dir,
        "RestrictMultiplicativeSymmetries",
        { { "Symmetries", fn1 }, { "Vars", fn2 }, { "Symbol", fn3 } },
        optsList
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "RMS:trivial_gauges", { id_ } ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      "All gauge transforms are trivial. Removing variables from list of symmetries.",
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];

(* DeleteEquivalentSolutions *)
MyNotebookPrint[ dir_, fileName_, nbo_ ][ "DSES:init", { id_, soln_, ring_, sym_, optionList_ } ] :=
  Module[{fn1, fn2, fn3 },
    fn1 = dataFileName[ id, dir, "Solutions" ];
    safeExport[ fn1, soln ];
    fn2 = dataFileName[ id, dir, "Ring" ];
    safeExport[ fn2, ring ];
    fn3 = dataFileName[ id, dir, "Symmetries" ];
    safeExport[ fn3, sym ];

    AddCell[
      fileName,
      nbo,
      startCell[
        id,
        dir,
        "DeleteEquivalentSolutions",
        { { "Solutions", fn1 }, { "Ring", fn2 }, { "Symmetries", fn3 } },
        optionList
      ]
    ];
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "DSES:trivial_gauge_transform", { id_ } ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      "The gauge transform is trivial. Removing duplicates by automorphisms of the fusion ring.",
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "DSES:groups", { id_, groups_ } ] :=
  Module[{ fn },
    fn = dataFileName[ id, dir, "Groups" ];
    safeExport[ fn, groups ];
    AddCell[
      fileName,
      nbo,
      Cell[
        TextData[{
          inputStyle[ "The solutions are divided in " <> ToString @ Length[groups] <> " " ],
          hyperlinkBox[ "group(s)", fn ],
          inputStyle[ " based on the zero values of the F-symbols." ]
        }],
        "Text",
        CellTags -> { id, "Info" }
      ]
    ];
  ];

(* GaugeSymmetryEquivalentQ *)
MyNotebookPrint[ dir_, fileName_, nbo_ ][ "GSEQ:init", { id_, mat_, sol1_, sol2_, optionList_ } ] :=
Module[{fn1, fn2, fn3},
  fn1 = dataFileName[ id, dir, "HermiteDecompOfGaugeMatrix" ];
  safeExport[ fn1, mat ];
  fn2 = dataFileName[ id, dir, "Solution1" ];
  safeExport[ fn2, sol1 ];
  fn3 = dataFileName[ id, dir, "Solution2" ];
  safeExport[ fn3, sol2 ];

  AddCell[
    fileName,
    nbo,
    startCell[
      id,
      dir,
      "GaugeSymmetryEquivalentQ",
      { { "HermiteDecompOfGaugeMatrix", fn1 }, { "Solution1", fn2 }, { "Solution2", fn3 } },
      optionList
    ]
  ];
];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "GSEQ:unitary_different_abs", { id_ } ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      "The solutions are not unitarily equivalent since the absolute values of some symbols differ.",
      "Text",
      CellTags -> { id, "Info" }
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "GSEQ:nonone_coeff", data_ ] :=
  MyNotebookPrint[ dir, fileName, nbo ][ "SSES:nonone_coeff", data ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "MZV:using_database", { procID_ } ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      "Using database of zero values",
      "Text",
      CellTags -> { procID, "Info" }
    ]
  ];

MyNotebookPrint[ dir_, fileName_, nbo_ ][ "MZV:entry_not_found", { procID_ } ] :=
  AddCell[
    fileName,
    nbo,
    Cell[
      "Entry not found in database. Calculating zero values and adding result.",
      "Text",
      CellTags -> { procID, "Info" }
    ]
  ];
