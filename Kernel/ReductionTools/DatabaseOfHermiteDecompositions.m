Package["Anyonica`"]

$HermiteDecompositionsLoaded = False;


$OptimizedHermiteDataFileName =
  FileNameJoin[ { $ReductionToolsInstallDirectory, "DatabaseOfHermiteDecompositions.mx" } ];

$HermiteDataFileName =
  FileNameJoin[ { $ReductionToolsInstallDirectory, "DatabaseOfHermiteDecompositions.wdx" } ];

$DevelopHermiteDataFileName =
  FileNameJoin[ {$ReductionToolsDevelopDirectory,"DatabaseOfHermitedecompositions.mx"} ];

LoadData["HermiteDecompositions"] :=
  Module[ {files},
    If[ $HermiteDecompositionsLoaded, Return[] ];

    files =
      FileNames[ All, $ReductionToolsInstallDirectory ];

    If[ (* Optimized version of database exists *)
      MemberQ[ $OptimizedHermiteDataFileName ] @ files
      ,
      $HermiteDecompositions =
        Import[ $OptimizedHermiteDataFileName, "MX" ];

      $HermiteDecompositionsLoaded =
        True;

      Return[]
    ];

    If[ (* Non-optimized version of database exists *)
      MemberQ[ $HermiteDataFileName ] @ files
      ,
      Export[ $OptimizedHermiteDataFileName, $HermiteDecompositions = Import[ $HermiteDataFileName , "WDX"], "MX" ];

      If[ (* Developer has same project structure as me *)
        FileExistsQ[ $ReductionToolsDevelopDirectory ]
        ,
        Export[ $DevelopHermiteDataFileName, $HermiteDecompositions ]
      ];

      $HermiteDecompositionsLoaded =
        True
    ]
  ];

StoreHermiteDecomposition[ mat_, decomp_ ] :=
  (
    $HermiteDecompositions[mat] = decomp;
    Export[ $OptimizedHermiteDataFileName, $HermiteDecompositions ]
  );


PackageExport["MemoizedHermiteDecomposition"]

MemoizedHermiteDecomposition::usage =
  "MemoizedHermiteDecomposition[ mat ] checks whether the HermiteDecomposition of mat is stored in a database and if so," <>
  " loads it. If it is not, it calculates it.";

Options[MemoizedHermiteDecomposition] =
  { "StoreHermiteDecompositions" -> False };

MemoizedHermiteDecomposition[ mat_, OptionsPattern[] ] :=
  (
    If[
      !$HermiteDecompositionsLoaded,
      LoadData["HermiteDecompositions"]
    ];
    If[
      (* Want to store decomposition*)
      OptionValue["StoreHermiteDecompositions"],
      (* THEN *)
      Module[ { decomp },
        If[ (* mat is not known *)
          KeyFreeQ[mat] @ $HermiteDecompositions,
          (* THEN: calc, store, and return HermiteDecomposition *)
          StoreHermiteDecomposition[ mat, decomp = SparseArray /@ HermiteDecomposition[mat] ];
          decomp,
          (* ELSE: load HermiteDecomposition *)
          $HermiteDecompositions[mat]
        ]
      ],
      (* ELSE *)
      If[
        KeyFreeQ[mat] @ $HermiteDecompositions,
        HermiteDecomposition[mat],
        $HermiteDecompositions[mat]
      ]
    ]
  );