(* ::Package:: *)

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





(*Returns a list of labels for the external edges of the left handed basis trees for a given Hilbert space.  The vacuum charge appears at the beginning of the list.
*)
(*Inputs:
cat - which fusion category 
nanyons - how many total "leaves" there are in the anyon fusion diagram, should be more than 2
anyontype - which anyon of the theory the leaves correspond to (should belong to {1,...,rank(fcat)})
Outputs:
edgeLabels - a list of combinations of internal edge labels corresponding to the left handed basis trees of the Hilbert space
 *)

PackageExport["LeftHandedBasis"]

LeftHandedBasis::usage = 
"LeftHandedBasis[ cat, nAnyons, a] returns a list of lists of internal edge labels corresponding to the possible left handed basis trees for a given number nAnyons of total anyons each of type a from modular fusion category cat.  Each sublist is ordered from bottom to top and assumes vacuum 
total charge, so that each of the sublists will have 1 as the first element.";

LeftHandedBasis::categorynotmodular =
  "The fusion category `1` is not modular";
LeftHandedBasis::anyontypeoutofbounds =
  "anyontype `1` must be between 1 and  `2`  for this category.";
LeftHandedBasis::anyontypeabelian =
  "The anyontype `1` is abelian, only non-abelian anyons can be used for computation";
LeftHandedBasis::incorrectnumberofanyons =
  "nAnyons `1` is not sufficient, need 3 or more anyons for a qubit";

LeftHandedBasis[cat_FusionCategory,nAnyons_, a_]:=
  Module[{basisList, ring},
  (*Check validity of various inputs*)
    If[
      !ModularQ[cat],
       Message[ LeftHandedBasis::categorynotmodular, cat];
       Return @ $Failed
    ];

    If[
      Not[ 1 <= a <= Rank[cat] ], 
      Message[ LeftHandedBasis::anyontypeoutofbounds, a, Rank[cat]]; 
      Return @ $Failed
    ];

    If[(*number of anyons check*)
      nAnyons <=2, 
      Message[ LeftHandedBasis::incorrectnumberofanyons, nAnyons]; 
      Return @ $Failed
    ];

    If[
      QuantumDimensions[cat][[a]][[2]] == 1, 
      Message[ LeftHandedBasis::anyontypeabelian, a]; 
      Return @ $Failed
    ];

    ring = FusionRing @ cat;

    (*Only use nonzero structure constants with anyontype as 2nd fusion label*)
    validStructConst = 
      Select[ #[[2]] == a &] @ NZSC @ ring;

    (*top to bottom basis labels*)
    Reverse @* 
    (#[[ nAnyons + 1;; 2 nAnyons - 1 ]]&) /@
    Select[
      coupleLeftTree/@
      allChains[
        validTwoLevelLeftTreeQ,
        validStructConst,
        nAnyons - 1
      ],
      (*Select trees with all leaves anyontype and total charge is vacuum*)
      First[#] == a && Last[#] == 1 &
    ]
  ];
