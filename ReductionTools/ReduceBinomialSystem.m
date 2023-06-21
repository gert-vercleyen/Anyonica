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

ReduceBinomialSystem::wrongvarsformat =
  "`1` should be a list of variables";

ReduceBinomialSystem::wrongeqnsformat =
"`1` should be a list of binomial equations";

argCheck[ binEqns_, vars_ ] :=
  Which[
    !BinomialSystemQ[binEqns]
    ,
    Message[ ReduceBinomialSystem::wrongeqnsformat, binEqns ]; Abort[]
    ,
    Head[vars] =!= List
    ,
    Message[ ReduceBinomialSystem::wrongvarsformat, vars ]; Abort[]
  ];

ReduceBinomialSystem[ binomialEqns_, variables_ ] :=
(
  argCheck[ binomialEqns, variables ];
  Module[
    { eqns, vars, revertVars, canEqns, x },
    { eqns, vars, revertVars } =
      SimplifyVariables[ binomialEqns, variables, x ];

    Catch @
    FixedPoint[ UpdateEquivalences[x], { NormalForm @ ToProperBinomialEquation @ eqns, { } } ] /. revertVars
  ]
);

FindEquivalences[ eqns_, x_ ] :=
  With[{ eqnVarPairs = { #, LinearVars[#,x], GetVariables[#,x] }& /@ eqns },
    DeleteDuplicates[
      DeleteCases[ eqnVarPairs, { _, { }, _ } ],
      IntersectingQ[  Last @ #1, Last @ #2 ]&
    ]
  ];

LinearVars[ eqn_, x_ ] :=
  With[{ powerVars = Cases[ eqn, Power[ a_, b_ ] /; Abs[b] > 1 :> a, Infinity ] },
    GetVariables[ eqn, x, powerVars ]
  ];

EquivalencesToRules[ equivs_, x_ ] :=
  EquivToRule[x] /@ equivs;

EquivToRule[x_][ { eqn_, linvars_, vars_ } ] :=
  With[{ var = First @ linvars },
    Solve[ eqn, var ][[1,1]]
  ];

UpdateEquivalences[x_][ { eqns_, equivs_ }] := EchoFunction["updatedsys",#/.x->P`z&] @
  With[ { equivRules = EchoFunction["rules",#/.x->P`z&][ EquivToRule[x] /@ FindEquivalences[ eqns, x ] ] },
    If[ MemberQ[0] @ equivRules[[;;,2]], Throw[{{False},{}}] ];
    {
      TEL @
      NormalForm @
      ToProperBinomialEquation @
      ReplaceAll[ eqns, Dispatch[equivRules] ],
      Join[ equivs/.Dispatch[equivRules], equivRules ]
    }
  ];

SetAttributes[NormalForm,Listable];
NormalForm[True] = True;
NormalForm[False] = False;
NormalForm[ eqn_ ] :=
  eqn[[2]] / eqn[[1]] == 1;