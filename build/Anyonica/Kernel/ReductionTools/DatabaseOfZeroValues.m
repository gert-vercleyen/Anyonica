(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-04-27 *)

Package["Anyonica`"]

$ZeroValuesLoaded = False;

$OptimizedFileName =
  FileNameJoin[ { $ReductionToolsInstallDirectory, "DatabaseOfZeroValues.mx" } ];

$ZeroDataFileName =
  FileNameJoin[ { $ReductionToolsInstallDirectory, "DatabaseOfZeroValues.wdx" } ];

$DevelopZeroDataFileName =
  FileNameJoin[ { $ReductionToolsDevelopDirectory,"DatabaseOfZeroValues.mx"} ];

LoadData["ZeroValues"] :=
  Module[ { files },
    files =
      FileNames[ All, $ReductionToolsInstallDirectory ];

    If[ TrueQ @ $ZeroValuesLoaded, Return[] ];

    If[ (* Optimized version of database exists *)
      MemberQ[ $OptimizedFileName ] @ files
      ,
      $ZeroValues =
        Import[ $OptimizedFileName, "MX" ];

      $ZeroValuesLoaded =
        True;

      Return[]
    ];

    If[
      (* Non-optimized version of database exists *)
      TrueQ @ MemberQ[ $ZeroDataFileName ] @ files
      ,
      Export[ $OptimizedFileName, $ZeroValues = Import[ $ZeroDataFileName , "WDX"], "MX" ];

      If[ (* Developer has same project structure as me *)
        FileExistsQ[ $ReductionToolsDevelopDirectory ]
        ,
        Export[ $DevelopZeroDataFileName, $ZeroValues ]
      ];

      $ZeroValuesLoaded =
        True
    ]
  ];

StoreZeroValues[ multTab_, values_ ] :=
(
  $ZeroValues[multTab] = values;
  Export[ $OptimizedFileName, $ZeroValues ]
);


PackageExport["MemoizedZeroValues"]

MemoizedZeroValues::usage =
  "MemoizedZeroValues[multTab,{binEqns,sumEqns}] checks whether the configurations of F-symbols that are zero of "<>
  "the ring with multiplication table multTab are stored in a database and if so," <>
  " returns them. If they are not, it calculates them.";

Options[MemoizedZeroValues] :=
  Join[
    Options[FindZeroValues],
    { "StoreDecompositions" -> True }
  ];

MemoizedZeroValues[ multTab_, { binEqns_, sumEqns_ }, fSymbols_, opts:OptionsPattern[] ] :=
  Module[{ procID = ToString @ Unique[] },

    printlog["MZV:using_database", {procID} ];

    If[
      !$ZeroValuesLoaded,
      LoadData["ZeroValues"]
    ];

    If[
      (* Want to store decomposition*)
      OptionValue["StoreDecompositions"]
      ,
      Module[ { zeroValues },
        If[ (* multTab is not known *)
          KeyFreeQ[multTab] @ $ZeroValues
          ,
          printlog["MZV:entry_not_found", { procID } ];
          StoreZeroValues[
            multTab,
            zeroValues =
              AddOptions[opts][FindZeroValues][
                binEqns,
                fSymbols
              ]
          ];
          zeroValues
          ,
          (* ELSE: use entry from database *)
          $ZeroValues[multTab]
        ]
      ]
      ,
      (* ELSE *)
      If[
        KeyFreeQ[multTab] @ $ZeroValues
        ,
        Select[ ValidZerosQ[sumEqns] ] @
        AddOptions[opts][FindZeroValues][
          binEqns,
          fSymbols
        ]
        ,
        $ZeroValues[multTab]
      ]
    ]
  ];
