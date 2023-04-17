(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-04-03 *)

Package["Anyonica`"]

IndependentSubsystems[ eqns_, s_ ] :=
  IndepSubsystems[ eqns, GetVars[#,s]& ];

IndependentSubsystems[ eqns_, s_, vars_ ] :=
  IndepSubsystems[ eqns, Union[ GetVars[ #, s ], vars ]& ];

(* Finds independent subsystems using func to get variables from eqns *)
IndepSubsystems[ eqns_, getVars_ ] :=
  Module[{ vars, varEqns, SowEdges, subSystemsVars, subSystemsEqns, varLists },
    vars =
      getVars @ eqns;
    varEqns =
      Association @
      Thread[ Map[ getVars, eqns ] -> eqns ];
    varLists =
      Keys[varEqns];

    SowEdges =
      Function[
        { var, varList },
        If[
          MemberQ[var] @ varList,
          (* THEN *)
          Sow[ UndirectedEdge[ var, # ] ]& /@
          varList
        ]
      ];

    subSystemsVars =
      Map[
        Sort,
        ConnectedComponents @
        Graph @
        Reap[
          Table[
            SowEdges[ var, varList ],
            { var, vars },
            { varList, varLists }
          ];
        ][[2,1]]
      ];

    subSystemsEqns =
      GroupBy[
        eqns,
        Function[
          eqn,
          IntersectingQ[ getVars @ eqn, # ]& /@
          subSystemsVars
        ]
      ];

    Reverse /@
    MapAt[
      subSystemsVars[[First @ FirstPosition[True] @ # ]] &,
      Normal[subSystemsEqns]/.Rule -> List,
      { All, 1 }
    ]

  ]
