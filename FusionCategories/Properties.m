(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-09-14 *)

Package["Anyonica`"]

PackageExport["BraidedQ"]

BraidedQ::usage =
  "BraidedQ[fusionCat] returns True if the fusion category has a set of R-symbols.";
  
BraidedQ[ fc_FusionCategory ] :=
  !MissingQ[ fc["RSymbols"] ];

FusionCategory /: UnitaryGaugeQ[ cat_FusionCategory, opts:OptionsPattern[] ] :=
  UnitaryGaugeQ[ FusionRing @ cat, FSymbols @ cat, opts ];


