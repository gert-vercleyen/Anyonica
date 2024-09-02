(* Mathematica Source File *)

(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2022-04-10 *)

(* ::Section:: *)
(* Begin Package *)

BeginPackage["AffineLieAlgebras`"];

(* ::Subsection::Closed:: *)
(* Messages *)

(* Usage Messages *)

AffineLieAlgebra::usage =
"AffineLieAlgebra[ \"Type\" -> t, \"Rank\" -> r, \"Level\" -> k, \"RootFactor\" -> rf ] returns an AffineLieAlgebra datastructure which represents an affine Lie algebra of type t, rank r, level k, and rootfactor rf.";

WeightsOfIrrep::usage = "ForTesting";
WeightSpaceDimensions::usage = "ForTesting";

(* Error messages *)
AffineLieAlgebra::missingdata =
"Missing data to initialize affine Lie algebra.";

AffineLieAlgebra::invaliddata = (* TODO: would be nicer i*)
"The data provided does not correspond to a valid affine Lie algebra.";

AffineLieAlgebra::typerankmismatch =
"The type and the rank of the affine Lie algebra are not compatible.";

AffineLieAlgebra::rootfactormismatch =
"The rootfactor is incompatible with the type, rank, and level of the affine Lie algebra.";

AffineLieAlgebra::nonintegerlevel =
"The level of the affine Lie algebra should be an integer.";

AffineLieAlgebra::negativelevel =
"The level of the affine Lie algebra should be a positive number";

(* ::Subsection::Closed:: *)
(* Options & Packages *)

(* Initialization of affine Lie algebras *)
Options[AffineLieAlgebra] =
  {
    "Type"        -> Missing[],
    "Rank"        -> Missing[],
    "Level"       -> Missing[],
    "RootFactor"  -> Missing[]
  };

(* ::Section:: *)
(* Begin `Private` Context *)

Begin["`Private`"];

(* ::Subsection:: *)
(* Function definitions *)

(* ::Subsubsection:: *)
(* Intialization of affine Lie algebras *)
AffineLieAlgebra[ ops:OptionsPattern[] ] :=
  AffineLieAlgebra[ InitializeAffineLieAlgebra[ ops ] ];

Options[InitializeAffineLieAlgebra] = Options[AffineLieAlgebra];
InitializeAffineLieAlgebra[ ops:OptionsPattern[] ] :=
  Module[ { t, r, k, rf, cartanMat, qfm, uniform, irreps, roots },
    t  = OptionValue["Type"];
    r  = OptionValue["Rank"];
    k  = OptionValue["Level"];
    rf = OptionValue["RootFactor"];

    If[
      MissingDataQ[ t, r, k, rf ],
      Message[ AffineLieAlgebra::missingdata ];
      Return[$Failed]
    ];

    If[
      !ValidDataQ[ t, r, k, rf ],
      Message[ AffineLieAlgebra::invaliddata ];
      Return[$Failed]
    ];

    uniform =
      !NonUniformALAQ[ t, r, k, rf ];

    cartanMat =
      InitializeCartanMatrix[ t, r ];

    qfm =
      QuadraticForm[ t, r , cartanMat ];

    irreps =
      If[
        uniform,
        IrrepsUniform[ t, r, k, qfm ],
        IrrepsNonUniform[ t, r, k, qfm ]
      ];

    roots =
      ALARoots[ t, r, cartanMat ];

    Association[
      "Type" -> t,
      "Rank" -> r,
      "Level" -> k,
      "RootFactor" -> rf,
      "CartanMatrix" -> cartanMat,
      "Irreps" -> irreps,
      "Roots" -> roots,
      "Uniform" -> !NonUniformALAQ[ t, r, k, rf ],
      "CentralCharge" -> Missing[],
      "QuadraticForm" -> qfm
    ]
  ];


MissingDataQ[ t_, r_, k_, rf_ ] :=
  MemberQ[ { t, r, k, rf }, Missing[] ];

ValidDataQ[ t_, r_, k_, rf_ ] :=
  Which[
    !ProperRangeQ[ t, r ],
      Message[ AffineLieAlgebra::typerankmismatch ],
    !IntegerQ[k],
      Message[ AffineLieAlgebra::nonintegerlevel ],
    k < 0,
      Message[ AffineLieAlgebra::negativelevel ],
    !ProperRootFactorQ[ t, r, k, rf ],
      Message[ AffineLieAlgebra::rootfactormismatch ],
    True,
      True
  ];

ProperRangeQ[ type_, rank_ ] :=
  IntegerQ[rank] &&
  Or[
    type == "a" && rank > 0,
    type == "b" && rank > 2,
    type == "c" && rank > 1,
    type == "d" && rank > 3,
    type == "e" && 5 < rank < 9,
    type == "f" && rank == 4,
    type == "g" && rank == 2
  ];

NonUniformALAQ[ t_, r_, k_, rf_ ] :=
  MemberQ[rf] @
  PossibleRootFactors[ t, r, k ]["NonUniform"];

ProperRootFactorQ[ type_, rank_, level_, rf_ ] :=
  MemberQ[rf] @
  (
    Join @@
    Values[ PossibleRootFactors[ type, rank, level ] ]
  );

(* For now I will assume that all data will be availlable at the start *)
PossibleRootFactors[ type_, rank_, level_ ] :=
  Association[
    "Uniform"     -> PossibleRootFactorsUniform[ type, rank, level ],
    "NonUniform"  -> PossibleRootFactorsNonUniform[ type, rank, level ]
  ];

PossibleRootFactorsUniform[ type_, rank_, level_ ] :=
  With[
    {
      tMax  = TMaxValue[ type ],
      g     = DualCoxeter[ type, rank ]
    },
    Cases[
      Range[ tMax (level + g) ],
      x_ /; GCD[ x, tMax(level + g) ] == 1
    ]
  ];

PossibleRootFactorsNonUniform[ type_, rank_, level_ ] :=
  With[
    {
      tMax = TMaxValue[ type ],
      g    = DualCoxeter[ type, rank ]
    },
    If[
      PossiblyNonUniformQ[ type, rank, level],
      Cases[
        Range[ tMax (level + g) ],
        x_ /; Mod[ x, tMax ] == 0 && GCD[ x, level + g ] == 1
      ],
      {}
    ]
  ];

(* Length between short and long roots, often denoted by t in litterature *)
TMaxValue[ type_ ] :=
  Which[
    MemberQ[ type ] @ { "a", "d", "e" }, 1,
    MemberQ[ type ] @ { "b", "c", "f" }, 2,
    MemberQ[ type ] @ { "g" },           3
  ];

DualCoxeter[ type_, rank_ ] :=
  Piecewise[
    {
      { rank + 1,   type == "a" && rank >= 1 },
      { 2 rank - 1, type == "b" && rank >= 3 },
      { rank + 1,   type == "c" && rank >= 2 },
      { 2 rank - 2, type == "d" && rank >= 4 },
      { 12,         type == "e" && rank == 6 },
      { 18,         type == "e" && rank == 7 },
      { 30,         type == "e" && rank == 8 },
      { 9,          type == "f" && rank == 4 },
      { 4,          type == "g" && rank == 2 }
    }
  ];

PossiblyNonUniformQ[ type_, rank_, level_ ] :=
  Or[
    type == "b" && rank >= 3 && Mod[level, 2 ] == 0                 && level >= 1,
    type == "c" && rank >= 2 && Mod[level + rank, 2 ] == 0          && level >= rank - 1,
    type == "f" && rank == 4 && Mod[level, 2 ] == 0                 && level >= 3,
    type == "g" && rank == 2 && MemberQ[ Mod[level, 3], { 0, 1 } ]  && level >= 2
  ];

(* From here on we assume all data belongs to a valid ALA *)

InitializeCartanMatrix[ type_, rank_ ] :=
  Normal @
  Switch[ type,
    "a", SparseArray[ { {i_, i_} -> 2, {i_, j_} /; Abs[i - j] == 1 -> -1 }, { rank, rank } ],
    "b", SparseArray[ { {i_, i_} -> 2, {i_, j_} /; Abs[i - j] == 1 && j < rank -> -1, {rank - 1, rank} -> -2}, {rank, rank} ],
    "c", SparseArray[ { {i_, i_} -> 2, {i_, j_} /; Abs[i - j] == 1 && i < rank -> -1, {rank, rank - 1} -> -2}, {rank, rank} ],
    "d", SparseArray[ { {i_, i_} -> 2, {i_, j_} /; Abs[i - j] == 1 && i < rank && j < rank -> -1, {rank - 2, rank} -> -1, {rank, rank - 2} -> -1}, {rank, rank}],
    "e",
    Switch[ rank,
      6, SparseArray[ { {i_, i_} -> 2, {i_, j_} /; Abs[i - j] == 1 && i < rank && j < rank -> -1, {3, 6} -> -1, {6, 3} -> -1}, {rank, rank}],
      7, SparseArray[ { {i_, i_} -> 2, {i_, j_} /; Abs[i - j] == 1 && i < rank && j < rank -> -1, {3, 7} -> -1, {7, 3} -> -1}, {rank, rank}],
      8, SparseArray[ { {i_, i_} -> 2, {i_, j_} /; Abs[i - j] == 1 && i < rank && j < rank -> -1, {5, 8} -> -1, {8, 5} -> -1}, {rank, rank}]
    ],
    "f", SparseArray[ { {i_, i_} -> 2, {i_, j_} /; Abs[i - j] == 1 && {i, j} != {2, 3} -> -1, {2, 3} -> -2}, {rank, rank}],
    "g", {{2, -3}, {-1, 2}}
  ];

IrrepsUniform[ type_, rank_, level_, qfm_ ] :=
  Module[
    { vars, qfMat, thetaVector },
    vars =
      Array[ Unique["n"], rank ];
    thetaVector =
      LongThetaVector[ type, rank ];
    Solve[
      ( vars + 1 ).qfm.thetaVector < level + DualCoxeter[type, rank],
      vars,
      NonNegativeIntegers
    ][[All, All, 2]] // Sort
  ];

IrrepsNonUniform[ type_, rank_, level_, qfm_ ] :=
  Module[
    { vars, thetaVector },
    vars =
      Array[ Unique["n"], rank ];

    thetaVector =
      ShortThetaVector[ type, rank ];

    Sort @
    Solve[
      TMaxValue[ type ] ( vars + 1) . qfm . thetaVector < level + DualCoxeter[ type, rank ],
      vars,
      NonNegativeIntegers
    ][[ ;;, ;; , 2 ]]
  ];

LongThetaVector[ type_, rank_ ] :=
  Switch[ type,
    "a", If[ rank == 1, { 2 }, Join[ { 1 }, ConstantArray[ 0, rank - 2 ], { 1 } ] ],
    "b", Join[ { 0, 1 }, ConstantArray[ 0, rank - 2 ] ],
    "c", Join[ { 1 },    ConstantArray[ 0, rank - 1 ] ],
    "d", Join[ { 0, 1 }, ConstantArray[ 0, rank - 2 ] ],
    "e",
      Switch[ rank,
        6, { 0, 0, 0, 0, 0, 1 },
        7, { 1, 0, 0, 0, 0, 0, 0 },
        8, { 1, 0, 0, 0, 0, 0, 0, 0 }
      ],
    "f", { 1, 0, 0, 0 },
    "g", { 1, 0 }
  ];

ShortThetaVector[ type_, rank_ ] :=
  Switch[ type,
    "a", If[ rank == 1, { 2 }, Join[ { 1 }, ConstantArray[ 0, rank - 2 ], { 1 } ] ],
    "b", Join[ { 1 },    ConstantArray[ 0, rank - 1 ] ],
    "c", Join[ { 0, 1 }, ConstantArray[ 0, rank - 2 ] ],
    "d", Join[ { 0, 1 }, ConstantArray[ 0, rank - 2 ] ],
    "e",
      Switch[ rank,
        6, { 0, 0, 0, 0, 0, 1 },
        7, { 1, 0, 0, 0, 0, 0, 0 },
        8, { 1, 0, 0, 0, 0, 0, 0, 0 }
      ],
    "f", { 0, 0, 0, 1 },
    "g", { 0, 1 }
  ];

(* Quadratic Form matrix *)
QuadraticForm[ type_, rank_, cartanMat_ ] :=
  With[
    {
      ict   = Transpose[ Inverse[ cartanMat ] ],
      rlfs  = RootLengthFactors[ type, rank ]
    },
    ict / rlfs
  ];


RootLengthFactors[ type_, rank_ ] :=
  Which[
    MemberQ[type] @ { "a", "d", "e" }, ConstantArray[ 1, rank ],
    type == "b", ConstantArray[ 1, rank - 1 ] ~ Join ~ { 2 },
    type == "c", ConstantArray[ 2, rank - 1 ] ~ Join ~ { 1 },
    type == "f", { 1, 1, 2, 2 },
    type == "g", { 1, 3 }
  ];

InvertedRootLengthFactors[ type_, rank_ ] :=
  TMaxValue[type] / RootLengthFactors[ type, rank ];

ALARoots[ type_, rank_, cartanMat_ ] :=
  Module[{ invCartanMat, a, pos, roots, root, jRoot },
    invCartanMat =
      Inverse[cartanMat];
    a =
      invCartanMat . invCartanMat . Range[rank];

    pos = 1; roots = { LongThetaVector[ type, rank ] };
    While[ pos <= First[ Dimensions @ roots ],
      root = roots[[pos]];
      Do[
        jRoot = root - j cartanMat[[i]];
        If[
          FreeQ[ roots, jRoot ],
          AppendTo[ roots, jRoot ]
        ],
        { i, rank },
        { j, root[[i]] }
      ];
      pos++;
    ];

    SortBy[
      roots,
      # . a &
    ]
  ];

PositiveRoots[ roots_ ] :=
  With[ { nRoots = Length @ roots },
    roots[[ ;; (nRoots - 1)/2 ]]
  ];


WeightsOfIrrep[ rank_, cartanMat_, highestWeight_ ] :=
  Module[
    { weights, weight, jWeight, pos },
    weights = { highestWeight }; pos = 1;
    While[ pos <= First[ Dimensions @ weights ],
      weight = weights[[pos]];
      Do[
        jWeight = weight - j cartanMat[[i]];
        If[
          FreeQ[ weights, jWeight ],
          AppendTo[ weights, jWeight ]
        ]
        , { i, rank }
        , { j, weight[[i]] }
      ];
      pos++;
    ];

    (* Todo: ORDER IS NOT UNIQUELY FIXED THIS WAY: ask Eddy whether that could pose a problem *)
    SortBy[
      weights,
      - # . Inverse[cartanMat] . ConstantArray[ 1, rank ] &
    ]
  ];

WeightSpaceDimensions[ type_, rank_, cartanMat_, qfm_, irreps_, roots_ ] :=
  Module[
    { rho, v, innerProd, preFactor, weights, dimension,  nWeights, dimensions, nRoots },
    rho =
      ConstantArray[ 1, rank ];
    innerProd[ A_, B_ ] :=
      A.qfm.B;
    preFactor[ A_, B_ ] :=
      2/( innerProd[ A + rho, A + rho ] - innerProd[ B + rho, B + rho ] );
    v =
      Inverse[ cartanMat ] . rho;
    nRoots =
      First[ Dimensions @ roots ];

    (* Returns all valid weights w + j * root, with j = 1, 2, ... *)
    CompatibleWeights[ weights_, w_, root_ ] :=
      Most @
      NestWhileList[
        # + root &,
        w + root,
        MemberQ[#] @ weights &
      ];

    dimension[ weights_, hw_, w_ ] :=
      dimension[ weights, hw, w ] =
      preFactor[ hw_, w_ ] *
      Sum[
        innerProd[ jWeight, root ] dimension[ weights, hw_, jWeight ],
        { root, PositiveRoots[roots] },
        { jWeight, CompatibleWeights[ weights, w, root ] }
      ];

    (* Find out which weights are in the rep by acting with the lowering
       operator to the highest weight *)
    Table[
      weights =
        WeightsOfIrrep[ rank, cartanMat, hw ];
      nWeights =
        First[ Dimensions @ weights ];

      (*Todo: check whether weights are sorted from highest to lowest *)
      Prepend[1] @
      Table[ dimension[ weights, hw, w ], { w, Rest[ weights ] } ]
      ,{ hw, irreps }
    ]
  ];

(* TODO: make RaisingOperator work properly *)

RaisingOperator[ a_AffineLieAlgebra, irrep_, w_, i_Integer ][ root_ ] :=
  With[
    { rootPos = Position[ roots, root ] },
    If[ MissingQ @ rootPos, Message[ RaisingOperator::notaroot, root ]; Abort[] ];
    
    If[
      MemberQ[ w + i * CartanMatrix[a][[ rootPos ]]  ] @ Weights[ a, irrep ],
      w + i * CartanMatrix[a],
      0
    ]
    
  ];

RaisingOperator[ a_AffineLieAlgebra, irrep_, w_ ] :=
  RaisingOperator[ a, irrep, w, 1 ];

LoweringOperator[ a_AffineLieAlgebra, irrep_, w_, i_Integer ] :=
  RaisingOperator[ a, irrep, w, -i ];

LoweringOperator[ a_AffineLieAlgebra, irrep_, w_ ] :=
  RaisingOperator[ a, irrep, w, -1 ];

InnerProduct[ highestWeight_, operators_ ] :=
  Module[{
    leftmostLowerPos
  },
  
  ]


(* ::Subsubsection:: *)
(* Properties of affine Lie algebras *)

(* ::Subsubsection:: *)
(* Formatting *)

Format[ ala:AffineLieAlgebra[a_Association], StandardForm ] :=
"ALA"[
  a["Type"],
  a["Rank"],
  a["Level"],
    2 Pi I a["RootFactor"] / ( TMaxValue[a["Type"]]( a["Level"]+DualCoxeter[a["Type"],a["Rank"] ] ) )
  ];

(* End `Private` Context *)



End[];

EndPackage[]


