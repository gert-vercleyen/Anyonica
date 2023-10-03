(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-09-14 *)

Package["Anyonica`"]

ChangeProperty[ ring_FusionCategory, list_ ] :=
  Module[ {opts},
    opts = (* All defining properties of previous fusion ring *)
    Normal @ First[ List @@ ring ];
    AddOptions[opts][FusionCategory][ Sequence @@ list ]

  ];


PackageExport["BraidedQ"]

BraidedQ::usage =
  "BraidedQ[fusionCat] returns True if the fusion category has a set of R-symbols.";
  
BraidedQ[ cat_FusionCategory ] :=
  !MissingQ[ RSymbols @ cat ];

FusionCategory /: UnitaryGaugeQ[ cat_FusionCategory, opts:OptionsPattern[] ] :=
  UnitaryGaugeQ[ FusionRing @ cat, FSymbols @ cat, opts ];

PackageExport["TopologicalSpins"]

TopologicalSpins::usage =
  "TopologicalSpins[cat] returns a list of topological spins of the braided category cat.";

TopologicalSpins::nonbraidedcat =
  "The topological spins are only defined for a braided category.";

TopologicalSpins[ cat_FusionCategory ] :=
(
  If[ !BraidedQ[cat], Message[TopologicalSpins::nonbraidedcat]; Abort[] ];
  Module[{ sR, qd, dMat },
    sR =
      SparseArray[
        Rest[#] -> R @@ # & /@ Cases[ NZSC @ cat, { a_, a_, c_ } ] /. Dispatch[ RSymbols @ cat ]
      ];
    
    qd =
      QD @ FusionRing @ cat;
    
    dMat =
      Table[ qd[[c]] / qd[[a]], { c, Rank @ cat }, { a, Rank @ cat } ];
    
    Tr[ sR.dMat ]
    
  ]
)


