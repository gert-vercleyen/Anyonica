(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2022-09-03 *)

Package["Anyonica`"]


PackageExport["\[ScriptCapitalR]"]

Unprotect[ \[ScriptCapitalR] ];

ClearAll[ \[ScriptCapitalR] ];

SetAttributes[ \[ScriptCapitalR], NHoldAll ];

Protect[ \[ScriptCapitalR] ];

\[ScriptCapitalR]::usage =
  "Formal symbol that represents an R-symbol.";


PackageScope["R"]

R =
  \[ScriptCapitalR];

PackageExport["FilterRRules"]

FilterRRules =
  FilterRules[ #, R[__] ]&;

PackageScope["ProperHexagonSolutionQ"]

ProperHexagonSolutionQ::usage =
  "Returns True if sol is a list of rules of the pattern \[ScriptCapitalR][__] -> _";

ProperHexagonSolutionQ[ sol_ ] :=
  MatchQ[ sol, { Repeated[ R[__] -> _ ] } ];


PackageScope["PHSQ"]

PHSQ::usage =
  "Shorthand for ProperHexagonSolutionQ";

PHSQ[ sol_ ] :=
  ProperHexagonSolutionQ[ sol ];


PackageScope["ProperListOfHexagonSolutionsQ"]

ProperListOfHexagonSolutionsQ::usage =
  "Returns True if soln is a list of proper pentagon solutions.";

ProperListOfHexagonSolutionsQ[ soln_ ] :=
  MatchQ[ soln, { Repeated[ _?PHSQ ] } ];

PackageScope["PLOHSQ"]

PLOHSQ::usage =
  "Shorthand for ProperListOfHexagonSolutionsQ";

PLOHSQ[ soln_ ] :=
  ProperListOfHexagonSolutionsQ[ soln ];


PackageExport["$VacuumRPattern"]

$VacuumRPattern::usage =
  "General pattern of vacuum R-symbols";

$VacuumRPattern =
  R[ 1, __ ] | R[ _, 1, __ ];


PackageExport["RSymbols"]

RSymbols::usage =
  "RSymbols[ ring ] returns a list of the well-defined R-symbols (without values) of the Fusion Ring ring.";

(* Create a list of R-symbols *)
RSymbols[ ring_FusionRing?FusionRingQ ] :=
  If[
    Mult[ring] === 1,
    (* THEN *)
    R @@@ NZSC[ring],
    (* ELSE *)
    Module[ { a, b, c, non0Ns, multTab, mult1, mult2 },
      non0Ns = NZSC[ring];
      multTab = MT[ring];
      Sort @
      Flatten @
      Reap[
        Do[
          { a, b, c } = label;
          mult1 = multTab[[ a, b, c ]];
          mult2 = multTab[[ b, a, c ]];
          Sow @
          Table[
            R[ a, b, c, i, j ],
            { i, mult1 }, { j, mult2 }
          ],
          { label, non0Ns }
        ]
      ][[ 2, 1 ]]
    ]
  ];

PackageExport["RTensors"]

RTensors::usage =
  "RTensors[ ring ] returns an association that maps well defined {a,b,c} to the matrix form of R[a,b,c].";

RTensors[ ring_ ] :=
  Module[{mt, a, b, c},
    mt = MT[ring][[##]] &;
    Association @@
    Table[
      { a, b, c } = n;
      n ->
      SparseArray[ Array[ R[a, b, c, #1, #2] & , { mt @@ n, mt @@ n } ] ],
      { n, NZSC[ring] }
    ]
  ];

PackageScope["SparseRTensor"]

(* Create a symbolic sparse R tensor *)
SparseRTensor[ ring_FusionRing?FusionRingQ ] :=
  SparseArray[
    Map[ ( List @@ # ) -> # &, RSymbols @ ring ],
    Table[ Rank[ring], 3 ]
  ];