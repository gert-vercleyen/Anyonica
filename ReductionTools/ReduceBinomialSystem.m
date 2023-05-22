(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-05-15 *)

Package["Anyonica`"]

(* Sometimes the smith decomposition is too heavy for a computer to carry out.
   If this is the case one can reduce the binomial system first using
   some symbolic manipulation defined in this file
 *)

PackageExport["ReduceBinomialSystem"]

ReduceBinomialSystem::usage =
  "ReduceBinomialSystem[binEqns,vars] returns a tuple of reduced equations, and "<>
  "assignments of variables in terms of those that appear in the reduced equations.";

(*  *)
ReduceBinomialSystem[ binomialEqns_, variables_ ] :=
  Module[
    { eqns, vars, revertVars, x },
    { eqns, vars, revertVars } =
      SimplifyVariables[ binomialEqns, variables, x ];

    FixedPoint[ UpdateEquivalences[x], { eqns, { } } ]/.revertVars

  ];

(* TODO: beware of 1 ==... and x[]^2 ==... *)

FindEquivalences[ eqns_, x_ ] :=
  With[{ eqnVarPairs = { #, GetVariables[#,x] }& /@ eqns },
    DeleteDuplicates[
      eqnVarPairs,
      IntersectingQ[  Last @ #1, Last @ #2 ]&
    ][[ ;;, 1 ]]
  ];

EquivalencesToRules[ equivs_, x_ ] :=
  EquivToRule[x] /@ equivs;

EquivToRule[x_][ equiv_ ] :=
  With[{ var = FirstCase[ x[_] ] @ First @ equiv },
    var -> equiv[[2]]/(equiv[[1]]/.var->1)
  ];

UpdateEquivalences[x_][ { eqns_, equivs_ }] :=
  With[ { equivRules = EquivToRule[x] /@ FindEquivalences[ eqns, x ] },
    {
      DeleteDuplicates @
      DeleteCases[True] @
      ReplaceAll[ eqns, Dispatch[equivRules] ],
      Join[ equivs/.Dispatch[equivRules], equivRules ]
    }
  ];