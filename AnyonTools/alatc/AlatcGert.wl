(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2022-03-10 *)

(* TODO: this might be much faster if we build a sparse tensor and then contracted it.
   Note that, e.g., m1 always appears together with d1, and likewise for other couples -> could take
   these indices together
*)

constructfsymbols[ FSym_ ] := With[{},
  Table[
    { f1 , f2, f3, f4, f5, f6 }
    N[
      Chop[
        Sum[ (* TODO: what is the 7'nth index??? *)
          qcg[ flist[[i, 1]], {m1, d1}, flist[[i, 2]], {m2, d2}, flist[[i, 5]], {m12, d12}, flist[[i, 7, 1]] ] * qcg[ flist[[i, 5]], {m12, d12}, flist[[i, 3]], {m3, d3}, flist[[i, 4]], {flist[[i, 4]], 1}, flist[[i, 7, 2]] ] *
          qcg[ flist[[i, 2]], {m2, d2}, flist[[i, 3]], {m3, d3}, flist[[i, 6]], {m23, d23}, flist[[i, 7, 3]] ] * qcg[ flist[[i, 1]], {m1, d1}, flist[[i, 6]], {m23, d23}, flist[[i, 4]], {flist[[i, 4]], 1}, flist[[i, 7, 4]] ],
          { m1, weights[flist[[i, 1]]] },
          { d1, wdim[ flist[[i, 1]], m1] },
          { m2, weights[flist[[i, 2]]]},
          { d2, wdim[flist[[i, 2]], m2]},
          { m12, Cases[weights[flist[[i, 5]]], x_ /; (m1 + m2) == x]},
          { d12, wdim[flist[[i, 5]], m12]},
          { m3, Cases[weights[flist[[i, 3]]], x_ /; (m12 + x) == flist[[i, 4]]]},
          { d3, wdim[flist[[i, 3]], m3]},
          { m23, Cases[weights[flist[[i, 6]]], x_ /; (m2 + m3) == x]},
          { d23, wdim[flist[[i, 6]], m23]}
        ] ,
        10^(-20)],
      precision
    ];
    , { f, FSym }
  ];

  Do[
    fmat[Sequence @@ fm] =
    Flatten[
      Table[
        fsym[fm[[1]], fm[[2]], fm[[3]], fm[[4]], e, f, {v1, v2, v3, v4}],
        {e, Cases[fusion[fm[[1]], fm[[2]]], x_ /; MemberQ[fusion[x, fm[[3]]], fm[[4]]]]},
        {f, Cases[fusion[fm[[2]], fm[[3]]], x_ /; MemberQ[fusion[fm[[1]], x], fm[[4]]]]},
        {v1, nv[fm[[1]], fm[[2]], e]},
        {v2, nv[e, fm[[3]], fm[[4]]]},
        {v3, nv[fm[[2]], fm[[3]], f]},
        {v4, nv[fm[[1]], f, fm[[4]]]}
      ]
      , {{1, 3, 4}, {2, 5, 6}}];

    fmatdim[Sequence @@ fm] = Length[fmat[Sequence @@ fm]];
    , {fm, fmatlist}];

  fmatdimtab = Table[fmatdim[Sequence @@ fm], {fm, fmatlist}];

  If[pentagontobechecked,
    numofpentagonequations = Sum[
      nv[a, b, f]*nv[f, c, g]*nv[g, d, e]* nv[c, d, gp]*nv[b, gp, fp]*nv[a, fp, e]
      , {a, irreps}, {b, irreps}, {c, irreps}, {d, irreps}
      , {f, fusion[a, b]}, {g, fusion[f, c]}, {e, fusion[g, d]}
      , {gp, Cases[fusion[c, d], x_ /; MemberQ[fusion[f, x], e]]}
      , {fp, Cases[fusion[b, gp], x_ /; MemberQ[fusion[a, x], e]]}];
  ];

];

sumIndices[ flist_, weights_, wdims_ ] :=
  Module[
    {
      f1, f2, f3, f4, f5, f6,
      we,
      wd1, wd2, wd3, wd4, wd5, wd6,
      m1, d1, m2, d2, m12, d12, m3, d3, m23, d23
    },
    { f1, f2, f3, f4, f5, f6 } = flist;
    we = weights[[ flist ]];
    Reap[
      Do[
        wd1 = wdims[[ f1, m1 ]];
        wd2 = wdims[[ f2, m2 ]];
        Do[
          m12vals = ConstantArray[ m1 + m2, Count[we[[5]], m1 + m2 ] ];
          Do[
            d12 = wdims[[ f5, m12 ]];
            m3vals = ConstantArray[ f4 - m12, Count[ f3, f4 - m12 ] ];
            Do[

              { m3, m3vals }
            ]
            ,
            { m12, m12vals }
          ],
          { d1, wd1 },
          { d2, wd2 }
        ],
        { m2, we[[2]] },
        { m1, we[[1]] }
      ]
        we1 =
        { m1, weights[flist[[i, 1]]] },
        { d1, wdim[ flist[[i, 1]], m1] },
        { m2, weights[flist[[i, 2]]]},
        { d2, wdim[flist[[i, 2]], m2]},
        { m12, Cases[weights[flist[[i, 5]]], x_ /; (m1 + m2) == x]},
        { d12, wdim[flist[[i, 5]], m12]},
        { m3, Cases[weights[flist[[i, 3]]], x_ /; (m12 + x) == flist[[i, 4]]]},
        { d3, wdim[flist[[i, 3]], m3]},
        { m23, Cases[weights[flist[[i, 6]]], x_ /; (m2 + m3) == x]},
        { d23, wdim[flist[[i, 6]], m23]}
      ]
    ]


  ]