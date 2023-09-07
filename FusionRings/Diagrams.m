(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-04-24 *)

Package["Anyonica`"]

PackageExport["LeftTreeDiagram"]

LeftTreeDiagram::usage =
  "LeftTreeDiagram[labels] returns a graphics object of a left ordered fusion tree labeled by labels. "<>
  "Here the convention is that the horizontal labels (read from left to right) are the first l + 1 "<>
  "labels (where l is the number of vertices) and the diagonal labels (read from top to bottom) are the last l labels.";

LeftTreeDiagram::wrongnumberoflabels =
  "The number of labels should be of the form 2 n + 1, where n is an integer greater than 0.";

LeftTreeDiagram[ labels_ ] :=
  Module[ { level, hPoints, vPoints, hLine, vLines, insets, vLabelOffset, hLabelOffset, labelCoordinates },
    level =
    ( Length[labels] -1 )/2;

    If[
      !IntegerQ[level],
      Message[ LeftTreeDiagram::wrongnumberoflabels ];
      Return[$Failed]
    ];

    hPoints =
    RotationTransform[ -Pi/4 ] /@
    Join[
      Table[
        { i, 0 },
        { i, 1, level }
      ],
      { { level + Sqrt[2]/2 , -Sqrt[2]/2 } }
    ];

    vPoints =
    RotationTransform[ -Pi/4 ] /@
    Table[
      { i, i },
      { i, 0, level }
    ];

    hLine =
    Line[ { vPoints[[1]], hPoints[[-2]], hPoints[[-1]] } ];

    vLines =
    Line[ Transpose[ { hPoints[[;;-2]], vPoints[[2;;]] } ] ];

    vLabelOffset =
    ConstantArray[ { 0, .25 }, level + 1 ];

    hLabelOffset =
    ConstantArray[ { Sqrt[2]/8 , -3Sqrt[2]/8 }, level - 1  ];

    labelCoordinates =
    Join[
      vPoints + vLabelOffset,
      hPoints[[;;-3]] + hLabelOffset,
      { hPoints[[-1]] + { 0, -.25 } }
    ];

    insets =
    MapThread[
      Inset,
      {
        labels,
        labelCoordinates
      }
    ];

    Graphics[
      {
        Point[ hPoints ~ Join ~ vPoints ],
        hLine,
        vLines,
        Sequence @@ insets
      }
    ]
  ];


PackageExport["RightTreeDiagram"]

RightTreeDiagram::usage =
  "RightTreeDiagram[labels] returns a graphics object of a right ordered fusion tree labeled by labels. "<>
  "Here the convention is that the horizontal labels (read from left to right) are the first l + 1 labels "<>
  "(where l is the number of vertices) and the diagonal labels (read from top to bottom) are the last l labels.";

RightTreeDiagram::wrongnumberoflabels =
  "The number of labels should be of the form 2 n + 1, where n is an integer greater than 0.";

RightTreeDiagram[ labels_ ] :=
  Module[ { level, hPoints, vPoints, hLine, vLines, insets, vLabelOffset, hLabelOffset, labelCoordinates },
    level =
    (Length[ labels ] - 1 )/2;

    If[
      !IntegerQ[level],
      Message[ RightTreeDiagram::wrongnumberoflabels ];
      Return[$Failed]
    ];

    hPoints =
    ReflectionTransform[ { 1, 0 } ] /@
    RotationTransform[ -Pi/4 ] /@
    Join[
      Table[
        { i, 0 },
        { i, 1, level }
      ],
      { { level + Sqrt[2]/2 , -Sqrt[2]/2 } }
    ];

    vPoints =
    ReflectionTransform[ { 1, 0 } ] /@
    RotationTransform[ -Pi/4 ] /@
    Table[
      { i, i },
      { i, 0, level }
    ];

    hLine =
    Line[ { vPoints[[1]], hPoints[[-2]], hPoints[[-1]] } ];

    vLines =
    Line[ Transpose[ { hPoints[[;;-2]], vPoints[[2;;]] } ] ];

    vLabelOffset =
    ConstantArray[ { 0, .25 }, level + 1 ];

    hLabelOffset =
    ConstantArray[ { -Sqrt[2]/8 , -3Sqrt[2]/8 }, level - 1  ];

    labelCoordinates =
    Join[
      Reverse[ vPoints + vLabelOffset],
      hPoints[[;;-3]] + hLabelOffset,
      { hPoints[[-1]] + { 0, -.25 } }
    ];

    insets =
    MapThread[
      Inset,
      {
        labels,
        labelCoordinates
      }
    ];

    Graphics[
      {
        Point[ hPoints ~ Join ~ vPoints ],
        hLine,
        vLines,
        Sequence @@ insets
      },
      ImageSize->Tiny
    ]
  ];


PackageExport["LeftOrderedFusionTrees"]

LeftOrderedFusionTrees::usage =
  "LeftOrderedFusionTrees[ ring, n ] returns lists of labels that form valid left-ordered fusion trees "<>
  "with n vertices of the Fusion Ring ring. The labels are ordered such that the first n + 1 labels denote the "<>
  "horizontal labels of the fusion tree (input anyons) and the last n labels denote the diagonal labels "<>
  "(fusion outcomes) from top to bottom.";

LeftOrderedFusionTrees[ ring_FusionRing, level_ ] :=
  coupleLeftTree /@
  allChains[ validTwoLevelLeftTreeQ, NZSC[ ring ], level ];


PackageExport["RightOrderedFusionTrees"]

RightOrderedFusionTrees::usage =
  "RightOrderedFusionTrees[ ring, n ] returns lists of labels that form valid right-ordered fusion trees with n "<>
  "vertices of the Fusion Ring ring. The labels are ordered such that the first n + 1 labels denote the horizontal "<>
  "labels of the fusion tree (input anyons) and the last n labels denote the diagonal labels (fusion outcomes) from "<>
  "top to bottom.";

RightOrderedFusionTrees[ ring_FusionRing?FusionRingQ, level_ ] :=
  coupleRightTree /@
  allChains[ validTwoLevelRightTreeQ, NZSC[ ring ], level ];


(* allChains builds a list of all possible chains of length <level> \
of objects <obj> where each is a list of matching objects, { o[1], ... \
o[n] }, (where matchq[ o[i], o[i+1] ] returns True.) *)

(* Find all left ordered fusion trees *)
allChains[ matchq_ , obj_, level_] :=
  Block[{$RecursionLimit = Max[ level + 1, $RecursionLimit ] },
    Flatten[
      Reap[
        sowChains[ matchq, obj, level, List /@ obj ];
      ][[2]],
      2] /. {{x__}} :> {x}
  ];

(* Basic recursive building block of allChains *)
(* If at level 1, throw current chains on a stack *)
sowChains[ matchq_, obj_, 1, currentChains_ ] :=
  Sow[ currentChains ];

sowChains[ matchq_ , obj_, level_Integer, currentChains_ ] :=
  Do[
    sowChains[
      matchq,
      obj,
      level - 1,
      Append[ chain, # ] & /@ Select[ obj, matchq[ Last[chain], # ] & ]
    ]
    ,{ chain, currentChains }
  ];

validTwoLevelLeftTreeQ[ ind1_, ind2_] :=
  ind1[[3]] === ind2[[1]];

validTwoLevelRightTreeQ[ ind1_, ind2_ ] :=
  ind1[[3]] === ind2[[2]];

coupleLeftTree[l_] :=
  Join[
    { l[[1,1]] },
    l[[;;,2]],
    l[[;;,3]]
  ];

coupleRightTree[l_] :=
  Join[
    Reverse @ l[[;;,1]],
    { l[[1,2]] },
    l[[;;,3]]
  ];





