(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2022-09-05 *)

$SmithDecompositionsLoaded = False;
$OptimizedSmithDataFileName = FileNameJoin[ { $PentaToolsInstallDirectory, "DatabaseOfSmithDecompositions.mx" } ];
$SmithDataFileName = FileNameJoin[ { $PentaToolsInstallDirectory, "DatabaseOfSmithDecompositions.wdx" } ];

LoadData["SmithDecompositions"] =
  Module[ { PTID, files, optimizedDataFileName, dataFileName },
    PTID = $PentaToolsInstallDirectory;
    files = FileNames[All,PTID];
    If[
      !$SmithDecompositionsLoaded,
      If[ (* Optimized version of database exists *)
        MemberQ[ $OptimizedSmithDataFileName ] @ files,
        (* THEN *)
        $SmithDecompositions = Import[ $OptimizedSmithDataFileName, "MX" ];
        $SmithDecompositionsLoaded = True,
        (* ELSE *)
        If[
          (* Non-optimized version of database exists *)
          MemberQ[ $SmithDataFileName ] @ files,
          (* THEN *)
          Export[ $OptimizedSmithDataFileName, $SmithDecompositions = Import[ $SmithDataFileName , "WDX"], "MX" ];
          $SmithDecompositionsLoaded = True,
          (* ELSE *)
          Print["Neither "<> optimizedDataFileName <> ", nor "<> dataFileName <> " found."]
        ]
      ]
    ]
  ];

StoreDecomposition[ mat_, decomp_ ] :=
  (
    $SmithDecompositions[mat] = decomp;
    Export[ $OptimizedSmithDataFileName, $SmithDecompositions ]
  );

Options[MemoizedSmithDecomposition] = { "StoreDecompositions" -> False };
MemoizedSmithDecomposition[ mat_?MatrixQ, OptionsPattern[] ] :=
(
  If[
    !$SmithDecompositionsLoaded,
    LoadData["SmithDecompositions"]
  ];
  If[
    (* Want to store decomposition*)
    OptionValue[ "StoreDecompositions" ],
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

