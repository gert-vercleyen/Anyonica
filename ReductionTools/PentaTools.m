(* ::Package:: *)

(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Sep 30, 2020 *)


BeginPackage["PentaTools`"];

Unprotect["PentaTools`*"];
ClearAll["PentaTools`*"];

Unprotect["PentaTools`Private`*"];
ClearAll["PentaTools`Private`*"];

(* Load the file with usage messages *)
Get[ FileNameJoin[{ DirectoryName[$InputFileName], "Messages.wl" }] ];

(* FusionRings functionality will be needed *)
Needs["FusionRings`"];

(* \[ScriptCapitalF] and \[ScriptCapitalR] will be used as a header for
   F-and R-symbols so they need to be protected *)
Unprotect[ \[ScriptCapitalF] ];
ClearAll[ \[ScriptCapitalF] ];
SetAttributes[ \[ScriptCapitalF], NHoldAll ];
Protect[ \[ScriptCapitalF] ];

Unprotect[ \[ScriptCapitalR] ];
ClearAll[ \[ScriptCapitalR] ];
SetAttributes[ \[ScriptCapitalR], NHoldAll ];
Protect[ \[ScriptCapitalR] ];

Begin["`Private`"];

$PentaToolsInstallDirectory =
	DirectoryName[$InputFileName];

(* We load all functionality from separate files *)
loadfile[ name_String ] :=
	With[
		{ PTID = $PentaToolsInstallDirectory },
		{ file = FileNameJoin[{ PTID, name }], files = FileNames[All,PTID] },
		If[
			FreeQ[file] @ files,
			Print[ "File "<>name<>" could not be found in "<> PTID ]
		];
		Get[ file ];
	];

(* Use shorthand for \[ScriptCapitalF] and \[ScriptCapitalR] *)
F =
	\[ScriptCapitalF];
R =
	\[ScriptCapitalR];

(* Logging results to a directory *)
loadfile[ "Logging.wl" ];

(* Generically useful functions *)
loadfile[ "GeneralFunctions.wl" ];

(* Using precalculated Smith Decompositions *)
loadfile[ "DatabaseOfSmithDecompositions.wl" ];

(* Creating towers of expressions *)
loadfile[ "TowersOfExpressions.wl" ];

(* Solving Diophantine equations *)
loadfile[ "SolveDiophantineSystem.wl" ];

(* Finding zero values*)
loadfile[ "FindZeroValues.wl" ];

(* Dealing with gauge symmetries *)
loadfile[ "Symmetries.wl" ];

(* Solving non-singular binomial equations *)
loadfile[ "SolveNonSingularBinomialSystem.wl" ];

(* Solving binomial equations *)
loadfile[ "SolveBinomialSystem.wl" ];

(* Solving linear systems modulo integers *)
loadfile[ "SolveLinearSystemModZ.wl" ];

(* Solving generic polynomial equations *)
loadfile[ "SolvePolynomialSystem.wl"];

(* Reducing a system of equations by trivial equations *)
loadfile[ "ReduceTrivialities.wl" ];

(* Reducing a system of polynomial equations by using binomial equations *)
loadfile[ "ReduceByBinomials.wl" ];

(* Reducing a system of equations using linearity *)
loadfile[ "ReduceByLinearity.wl" ];

(* Splitting systems of equations into independent subsystems *)
loadfile[ "IndependentSubsystems.wl" ];

(* Dealing with F Symbols *)
loadfile[ "FSymbols.wl" ];

(* Setting up pentagon equations *)
loadfile[ "PentagonEquations.wl" ];

(* Dealing with R Symbols *)
loadfile[ "RSymbols.wl" ];

(* Setting up hexagon equations *)
loadfile[ "HexagonEquations.wl" ];

(* Preparing pentagon equations to be solved *)
loadfile[ "PrepareSolverInput.wl" ];

(* Calculating Groebner Bases of pentagon and hexagon equations *)
loadfile[ "GroebnerSystems.wl" ];

(* Solving pentagon and hexagon equations *)
loadfile[ "SolveGroebnerSystem.wl" ];

(* Adjusting the gauge for solutions *)
loadfile[ "GaugeAdjust.wl" ];

(* Checking criteria for categorifiability *)
loadfile[ "CategorifiabilityCriteria.wl" ];

End[];
Protect["PentaTools`*"];
EndPackage[]
