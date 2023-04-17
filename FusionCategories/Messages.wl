(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2022-04-17 *)

(*
+---------------------------------------------------------------------------+
|                                                                           |
|                            FUSION CATEGORIES                              |
|                                                                           |
+---------------------------------------------------------------------------+

+---------------------------------------------------------------------------+
|                                 GENERAL                                   |
+---------------------------------------------------------------------------+

+---------------------------------------------------------------------------+
|                              Usage Messages                               |
+---------------------------------------------------------------------------+
*)
FusionCategories::usage =
"This package contains a list of fusion categories and tools for working with fusion categories. Evaluate ?FusionCategories`* for a list of all functions.";
FusionCategory::usage =
"FusionCategory[ \"GrothendieckRing\" -> r, \"FSymbols\" -> fSymbols, opts ] initializes a fusion category with fusion ring r and F-symbols fSymbols. Extra options include \"RSymbols\"-> rSymbols to initialize a braided fusion category, \"PreEqualCheck\" -> f to apply a function f to both sides of each pentagon equation before checking equality.";

(*
+---------------------------------------------------------------------------+
|                              Error Messages                               |
+---------------------------------------------------------------------------+
*)

FusionCategory::nofusionring =
"No fusion ring is provided so no category could be initialized";
FusionCategory::nofsymbols =
"No F-symbols were provided so no category could be initialized";
FusionCategory::wrongfsymbolsformat =
"The format for the F-symbols should be a list of the form { \[ScriptCapitalF][indices__] -> value_, ... } or a list of the form { {indices__} -> value }";
FusionCategory::wrongringformat =
"The fusion ring should be a fusion ring object from the FusionRings package.";
FusionCategory::wrongrsymbolsformat =
"The format for the R-symbols should be a list of the form { \[ScriptCapitalR][__] ->_, ... }";
FusionCategory::invalidfsymbols =
"The F-symbols are not a valid solution to the pentagon equations. This might be due Mathematica being unable to simplify certain expressions. If so, use the option \"PreEqualCheck\" to force simplification. Invalid equations:\n`1`";
FusionCategory::invalidrsymbols =
"The R-symbols are not a valid solution to the hexagon equations. This might be due Mathematica being unable to simplify certain expressions. If so, use the option \"PreEqualCheck\" to force simplification. Invalid equations:\n`1`";

(*
+---------------------------------------------------------------------------+
|                      PROPERTIES OF FUSION CATEGORIES                      |
+---------------------------------------------------------------------------+

+---------------------------------------------------------------------------+
|                              Usage Messages                               |
+---------------------------------------------------------------------------+
*)

FusionCategoryQ::usage =
"FusionCategoryQ[ fusionCat ] returns true if the fusionCat is a valid FusionCategory and False otherwise.";
GrothendieckRing::usage =
"GrothendieckRing[ fusionCat ] returns the Grothendieck ring (a.k.a. the fusion ring) associated to fusionCat.";
