(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-09-14 *)

Package["Anyonica`"]


FusionCategory /: ApplyGaugeTransform[ cat_FusionCategory, gaugeVals_, s_ ] :=
  With[{ opts = Normal @ First @ ( List @@ cat ) },
    AddOptions[opts][FusionCategory][
      "FSymbols" -> ApplyGaugeTransform[ FSymbols @ cat, gaugeVals, s ],
      "RSymbols" -> ApplyGaugeTransform[ RSymbols @ cat, gaugeVals, s ],
      "SkipCheck" -> True
    ]
  ];


PackageExport["DirectProduct"]

Options[DirectProduct] :=
  Join[
    Options[FusionCategory],
    { "SimplifyBy" -> Identity }
  ];

FusionCategory /: DirectProduct[ fc1: FusionCategory[ data1_ ], fc2: FusionCategory[ data2_ ], opts:OptionsPattern[] ] :=
  Module[{ r1, r2, r, fs1, fs2, fs, rs1, rs2, rs, tupleToSingle, sTupleToSingle, simplify },
    simplify =
      OptionValue["SimplifyBy"];
    
    r1 = FusionRing[fc1]; r2 = FusionRing[fc2];
    fs1 = FSymbols[fc1];  fs2 = FSymbols[fc2];
    rs1 = RSymbols[fc1];  rs2 = RSymbols[fc2];
    
    r = DirectProduct[ r1, r2 ];
    
    tupleToSingle[a_,b_] :=
      ReplaceAll[
        { a, b },
        Thread[
          Flatten[ Table[ { i, j }, { i, Rank[r1] }, { j, Rank[r2] } ], 1 ] ->
          Range[ Rank[r1] * Rank[r2] ]
        ]
      ];
    
    sTupleToSingle[ s_[i1__], s_[i2__] ] :=
      s @@ MapThread[ tupleToSingle, { {i1}, {i2} } ];
    
    fs =
      simplify @
      Sort @
      Flatten @
      Table[
        sTupleToSingle[ F1[[1]], F2[[1]] ] -> F1[[2]] * F2[[2]],
        { F1, fs1 }, { F2, fs2 }
      ];
    
    If[
      rs1 === {} || rs2 === { }
      ,
      rs = {}
      ,
      rs =
        simplify @
        Sort @
        Flatten @
        Table[
          sTupleToSingle[ R1[[1]], R2[[1]] ] -> R1[[2]] * R2[[2]],
          { R1, rs1 }, { R2, rs2 }
        ]
    ];
    
    AddOptions[opts][FusionCategory][
      "FusionRing" -> r,
      "FSymbols"   -> fs,
      "RSymbols"   -> rs
    ]
  ];
  