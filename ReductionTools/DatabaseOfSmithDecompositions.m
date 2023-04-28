(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2022-09-05 *)

Package["Anyonica`"]

$SmithDecompositionsLoaded = False;


PackageScope["$ReductionToolsInstallDirectory"]

$ReductionToolsInstallDirectory =
  DirectoryName[$InputFileName];

$OptimizedSmithDataFileName =
  FileNameJoin[ { $ReductionToolsInstallDirectory, "DatabaseOfSmithDecompositions.mx" } ];

$SmithDataFileName =
  FileNameJoin[ { $ReductionToolsInstallDirectory, "DatabaseOfSmithDecompositions.wdx" } ];

LoadData["SmithDecompositions"] :=
  Module[ {files },
    files =
      FileNames[ All, $ReductionToolsInstallDirectory ];
    If[
      !$SmithDecompositionsLoaded,
      If[ (* Optimized version of database exists *)
        MemberQ[ $OptimizedSmithDataFileName ] @ files,
        (* THEN *)
        $SmithDecompositions =
          Import[ $OptimizedSmithDataFileName, "MX" ];
        $SmithDecompositionsLoaded =
          True,
        (* ELSE *)
        If[
          (* Non-optimized version of database exists *)
          MemberQ[ $SmithDataFileName ] @ files,
          (* THEN *)
          Export[ $OptimizedSmithDataFileName, $SmithDecompositions = Import[ $SmithDataFileName , "WDX"], "MX" ];
          $SmithDecompositionsLoaded = True,
          (* ELSE *)
          Print["Neither "<> $OptimizedSmithDataFileName <> ", nor "<> $SmithDataFileName <> " found."]
        ]
      ]
    ]
  ];

StoreDecomposition[ mat_, decomp_ ] :=
  (
    $SmithDecompositions[mat] = decomp;
    Export[ $OptimizedSmithDataFileName, $SmithDecompositions ]
  );


PackageExport["MemoizedSmithDecomposition"]

MemoizedSmithDecomposition::usage =
  "MemoizedSmithDecomposition[ mat ] checks whether the SmithDecomposition of mat is stored in a database and if so," <>
  " loads it. If it is not, it calculates it.";

Options[MemoizedSmithDecomposition] =
  { "StoreDecompositions" -> False };

MemoizedSmithDecomposition[ mat_, OptionsPattern[] ] :=
  (
    If[
      !$SmithDecompositionsLoaded,
      LoadData["SmithDecompositions"]
    ];
    If[
      (* Want to store decomposition*)
      OptionValue["StoreDecompositions"],
      (* THEN *)
      Module[ { decomp },
        If[ (* mat is not known *)
          KeyFreeQ[mat] @ $SmithDecompositions,
          (* THEN: calc, store, and return SmithDecomposition *)
          StoreDecomposition[ mat, decomp = SparseArray /@ SmithDecomposition[mat] ];
          decomp,
          (* ELSE: load SmithDecomposition *)
          $SmithDecompositions[mat]
        ]
      ],
      (* ELSE *)
      If[
        KeyFreeQ[mat] @ $SmithDecompositions,
        SmithDecomposition[mat],
        $SmithDecompositions[mat]
      ]
    ]
  );