(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2022-04-07 *)

(* The following code generates a list of affine Lie algebras. These can be used to test the code from alatc *)

types = {"a", "b", "c", "d", "e", "f", "g"};
affineData =
<|
  "a" -> <|"Ranks" -> Range[1, 3], "Levels" -> Range[1, 4],
    "RootFactors" -> {1}|>,
  "b" -> <|"Ranks" -> Range[3, 5], "Levels" -> Range[1, 4],
    "RootFactors" -> {1}|>,
  "c" -> <|"Ranks" -> Range[2, 4], "Levels" -> Range[1, 4],
    "RootFactors" -> {1}|>,
  "d" -> <|"Ranks" -> Range[4, 6], "Levels" -> Range[1, 4],
    "RootFactors" -> {1}|>,
  "e" -> <|"Ranks" -> Range[6, 8], "Levels" -> Range[1, 4],
    "RootFactors" -> {1}|>,
  "f" -> <|"Ranks" -> {4}, "Levels" -> Range[1, 4],
    "RootFactors" -> {1}|>,
  "g" -> <|"Ranks" -> {2}, "Levels" -> Range[1, 4],
    "RootFactors" -> {1}|>
|>;
TestCases =
Flatten@
Table[
  <|"Type" -> t, "Rank" -> r, "Level" -> k, "RootFactor" -> rf|>,
  { t, types },
  { r, affineData[t]["Ranks"] },
  { k , affineData[t]["Levels"] },
  { rf, affineData[t]["RootFactors"]}
]