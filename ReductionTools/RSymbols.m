(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2022-09-03 *)

Package["Anyonica`"]

ProperHexagonSolutionQ[ sol_ ] :=
  MatchQ[ sol, { Repeated[ R[__] -> _ ] } ];

PHSQ[ sol_ ] :=
  ProperHexagonSolutionQ[ sol ];

ProperListOfHexagonSolutionsQ[ soln_ ] :=
  MatchQ[ soln, { Repeated[ _?PHSQ ] } ];

PLOHSQ[ soln_ ] :=
  ProperListOfHexagonSolutionsQ[ soln ];

$VacuumRPattern =
  R[ 1, __ ] | R[ _, 1, __ ];

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

RTensors[ ring_] :=
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

(* Create a symbolic sparse R tensor *)
SparseRTensor[ ring_FusionRing?FusionRingQ ] :=
SparseArray[
  Map[ ( List @@ # ) -> # &, RSymbols @ ring ],
  Table[ Rank[ring], 3 ]
];