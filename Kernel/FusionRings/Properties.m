
Package["Anyonica`"]

(* Define getter functions for the fusion ring. Since system symbols
   are protected and we don't want to alter them unless strictly necessary
   we use TagSetDelayed ( /: lhs := rhs ) to pattern match FusionRing objects
   within expressions and replace them with our own definitions. *)

PackageExport["ChangeProperty"]

ChangeProperty::usage =
  "ChangeProperty[ ring, opt -> val ] changes the property of type opt to val.\n"<>
  "ChangeProperty[ ring, { opt1 -> val1, ..., optn -> valn } ] changes the properties of types opti to vali."<>
  "ChangeProperty works for both fusion rings and fusion categories";


ChangeProperty[ ring_FusionRing, list_List ] :=
  Module[ {opts},
    opts = (* All defining properties of previous fusion ring *)
      Normal @ First[ List @@ ring ];
    AddOptions[opts][FusionRing][ Sequence @@ list ]
  ];

ChangeProperty[ ring_, a_ -> b_ ] :=
  ChangeProperty[ ring, { a -> b } ];

PackageExport["MultiplicationTable"]

MultiplicationTable::usage =
  "MultiplicationTable[ring] returns a triple nested list l where l[[a,b,c]] is the structure constant N_{a,b}^c.";

SetAttributes[ MultiplicationTable, Listable ];

FusionRing /: MultiplicationTable[ r_FusionRing?FusionRingQ ] :=
  r["MultiplicationTable"];

PackageExport["MT"]
MT::usage =
  "Shorthand for MultiplicationTable.";

SetAttributes[ MT, Listable ];

FusionRing /: MT[ r_FusionRing?FusionRingQ ] :=
  MultiplicationTable[ r ];

PackageExport["SymbolicMultiplicationTable"]

SymbolicMultiplicationTable::usage =
  "SymbolicMultiplicationTable[ring] returns a table l where l[[a,b]] equals fusion.";

SetAttributes[ SymbolicMultiplicationTable, Listable ];

FusionRing /: SymbolicMultiplicationTable[ r_FusionRing?FusionRingQ ] :=
  Table[ FusionProduct[ r, {a,b} ], {a, Rank[r]}, {b,Rank[r]} ];

PackageExport["SMT"]

SMT::usage =
  "Shorthand for SymbolicMultiplicationTable.";

SetAttributes[ SMT, Listable ];

FusionRing /: SMT[ r_FusionRing?FusionRingQ ] :=
  SymbolicMultiplicationTable[ r ];


PackageExport["Names"]

Names::usage =
  "Names[ring] returns a list of possible names of the fusion ring.";
(*  " The possible names of a fusion ring can set upon initialization by adding the option \"Names\"->{...} or afterwards by issuing ring[\"Names\"] = {...}";*)

SetAttributes[ Names, Listable ];

FusionRing /: Names[ r_FusionRing?FusionRingQ ] :=
  r["Names"];

PackageExport["ElementNames"]

ElementNames::usage =
  "ElementNames[ring] returns a list of strings denoting the names of the current generators of the fusion ring.";
(*These will be used to label the elements of the ring during calculations and can also be used to access the elements via ring[[el]], where el can either be the name as a string or as a symbol. ElementNames can be given as an option \"ElementNames\" -> stringlist to FusionRing upon initialization or can be added later via ring[\"ElementNames\"] = stringlist. The format required is a list of strings with the same length as the amount of generators.";*)

SetAttributes[ ElementNames, Listable ];

FusionRing /: ElementNames[ r_FusionRing?FusionRingQ ] :=
  r["ElementNames"];

PackageExport["AntiparticleMatrix"]

AntiparticleMatrix::usage =
  "AntiparticleMatrix[ring] returns a matrix that maps each particle to its antiparticle.";

SetAttributes[ AntiparticleMatrix, Listable ];

FusionRing /: AntiparticleMatrix[ r_FusionRing?FusionRingQ ] :=
  MT[r][[All,All,1]];

PackageExport["AM"]

AM::usage =
  "Shorthand for AntiparticleMatrix.";

SetAttributes[ AM, Listable ];
FusionRing /: AM[ r_FusionRing?FusionRingQ ] :=
  AntiparticleMatrix[ r ];


PackageExport["CommutativeQ"]

CommutativeQ::usage =
  "CommutativeQ[ring] returns True if ring is commutative and False otherwise.";

SetAttributes[ CommutativeQ, Listable ];

FusionRing /: CommutativeQ[ r_FusionRing?FusionRingQ ] :=
  TrueQ[ Transpose[ MT[r] ] == MT[r] ];


PackageExport["CQ"]

CQ::usage =
  "Shorthand for CommutativeQ.";

SetAttributes[ CQ, Listable ];

FusionRing /: CQ[ r_FusionRing?FusionRingQ ] :=
  CommutativeQ[ r ];


PackageExport["Multiplicity"]

Multiplicity::usage =
  "Multiplicity[ring] returns the multiplicity of ring.";

SetAttributes[ Multiplicity, Listable ];

FusionRing /: Multiplicity[ r_FusionRing?FusionRingQ ] :=
  Max[ MultiplicationTable[ r ] ];


PackageExport["Mult"]

Mult::usage =
  "Shorthand for Multiplicity.";

SetAttributes[ Mult, Listable ];

FusionRing /: Mult[ r_FusionRing?FusionRingQ ] :=
  Multiplicity[ r ];


PackageExport["MultiplicityFreeQ"]

MultiplicityFreeQ::usage =
  "MultiplicityFreeQ[ring] returns True if ring has no structure constants bigger than 1.";

SetAttributes[ MultiplicityFreeQ, Listable ];

FusionRing /: MultiplicityFreeQ[ r_FusionRing?FusionRingQ ] :=
  Multiplicity[r] == 1;


PackageExport["MFQ"]

MFQ::usage =
  "Shorthand for MultiplicityFreeQ.";

SetAttributes[ MFQ, Listable ];

FusionRing /: MFQ[ r_FusionRing?FusionRingQ ] :=
  MultiplicityFreeQ[ r ];

PackageExport["NonZeroStructureConstants"]

NonZeroStructureConstants::usage =
  "NonZeroStructureConstants[ring] returns a list of triples of indices for which the structure constants are non-zero";

SetAttributes[ NonZeroStructureConstants, Listable ];

FusionRing /: NonZeroStructureConstants[ r_FusionRing?FusionRingQ ] := With[{
  range = Range[Rank[r]],
  mt = MT[r]},
  Complement[ Tuples[ {range,range,range} ], Position[ mt, 0 ] ]
];

PackageExport["NZSC"]

NZSC::usage =
  "Shorthand for NonZeroStructureConstants.";

SetAttributes[ NZSC, Listable ];

FusionRing /: NZSC[ r_FusionRing?FusionRingQ ] :=
  NonZeroStructureConstants[ r ];


PackageExport["NNonZeroStructureConstants"]

NNonZeroStructureConstants::usage =
  "NNonZeroStructureConstants[ring] returns the number of nonzero structure constants of ring.";

SetAttributes[ NNonZeroStructureConstants, Listable ];

FusionRing /: NNonZeroStructureConstants[ r_FusionRing?FusionRingQ ] :=
  Length @ NonZeroStructureConstants[ r ];


PackageExport["NNZSC"]

NNZSC::usage =
  "Shorthand for NNonZeroStructureConstants.";

SetAttributes[ NNZSC, Listable ];

FusionRing /: NNZSC[ r_FusionRing?FusionRingQ ] :=
  NNonZeroStructureConstants[ r ];


PackageExport["Rank"]

Rank::usage =
  "Rank[ring] returns the number of generators (particles) of the fusion ring.";

SetAttributes[ Rank, Listable ];

FusionRing /: Rank[ r_FusionRing?FusionRingQ ] :=
  Length @ MultiplicationTable[r];

PackageExport["FrobeniusPerronDimensions"]

FrobeniusPerronDimensions::usage =
  "FrobeniusPerronDimensions[Ring] returns the list of Frobenius-Perron dimensions of the ring.";

SetAttributes[ FrobeniusPerronDimensions, Listable ];

FusionRing /: FrobeniusPerronDimensions[ r_FusionRing?FusionRingQ ] :=
  If[
    !MissingQ[ r["QuantumDimensions"] ]
    ,
    r["QuantumDimensions"]
    ,
    With[{ tab = MultiplicationTable[r] },
      Last[ SortBy[ Eigenvalues[#], N @* Re ] ] & /@
      tab
    ]
  ];


PackageExport["FPDims"]

FPDims::usage =
  "FPDims is shorthand for FrobeniusPerronDimensions.";

SetAttributes[ FPDims, Listable ];

FusionRing /: FPDims[ r_FusionRing?FusionRingQ ] :=
  FrobeniusPerronDimensions[ r ];

PackageExport["FrobeniusPerronDimension"]


FrobeniusPerronDimension::usage =
  "FrobeniusPerronDimension[Ring] returns the Frobenius-Perron dimension of Ring";

SetAttributes[ FrobeniusPerronDimension, Listable ];
FusionRing /: FrobeniusPerronDimension[ r_FusionRing?FusionRingQ ] :=
  Total[ FPDims[r]^2 ];

PackageExport["FPDim"]

FPDim::usage =
  "FPDim is shorthand for FrobeniusPerronDimension.";

SetAttributes[ FPDim, Listable ];

FusionRing /: FPDim[ r_FusionRing?FusionRingQ ] :=
  FrobeniusPerronDimension[ r ];

PackageExport["NSelfDual"]

NSelfDual::usage =
  "NSelfDual[ring] returns the number of particles that are their own antiparticle. This includes the vacuum particle";

SetAttributes[ NSelfDual, Listable ];

FusionRing /: NSelfDual[ r_FusionRing?FusionRingQ ] :=
  AntiparticleMatrix[r] // Diagonal // Count[x_/; x != 0 ];

PackageExport["NSD"]

NSD::usage =
  "Shorthand for NSelfDual.";

SetAttributes[ NSD, Listable ];

FusionRing /: NSD[ r_FusionRing?FusionRingQ ] :=
  NSelfDual[ r ];


PackageExport["NNonSelfDual"]

NNonSelfDual::usage =
  "NNonSelfDual[ring] returns the number of particles that are not their own antiparticle";

SetAttributes[ NNonSelfDual, Listable ];

FusionRing /: NNonSelfDual[ r_FusionRing?FusionRingQ ] :=
  Rank[r] - NSelfDual[r];


PackageExport["NNSD"]

NNSD::usage =
  "Shorthand for NNonSelfDual.";

SetAttributes[ NNSD, Listable ];

FusionRing /: NNSD[ r_FusionRing?FusionRingQ ] :=
  NNonSelfDual[ r ];


PackageExport["NSelfDualNonSelfDual"]

NSelfDualNonSelfDual::usage =
  "NSelfDualNonSelfDual[ring] returns a tuple {sd,nsd} where sd is the number of particles that are their own " <>
  "antiparticle and nsd the number of particles that are not their own antiparticle";

SetAttributes[ NSelfDualNonSelfDual, Listable ];

FusionRing /: NSelfDualNonSelfDual[ r_FusionRing?FusionRingQ ] := With[{
  nsd = NSelfDual[r]},
  { nsd, Rank[r] - nsd }
];


PackageExport["NSDNSD"]

NSDNSD::usage =
  "Shorthand for NSelfDualNonSelfDual.";

SetAttributes[ NSDNSD, Listable ];

FusionRing /: NSDNSD[ r_FusionRing?FusionRingQ ] :=
  NSelfDualNonSelfDual[ r ];


PackageExport["GroupRingQ"]

GroupRingQ::usage =
  "GroupRingQ[ring] returns True if the multiplication table comes from a finite group.\n 
  GroupRingQ[category] returns True if the Grothendieck ring of the fusion category is a group ring.";

SetAttributes[ GroupRingQ, Listable ];

FusionRing /: GroupRingQ[ r_FusionRing?FusionRingQ ] :=
  Total[ Flatten[ MultiplicationTable[ r ] ] ] == Rank[ r ]^2;

PackageExport["GRQ"]

GRQ::usage =
  "Shorthand for GroupRingQ.";

FusionRing /: GRQ[ r_FusionRing?FusionRingQ ] :=
  GroupRingQ[r];

PackageExport["ConjugateCharge"]

ConjugateCharge::usage =
  "ConjugateCharge[ring] returns a function that maps an integer index to the dual index.";

FusionRing /: ConjugateCharge[ r_FusionRing?FusionRingQ ] :=
  r["Dual"];

FusionElement /: ConjugateCharge[ FusionElement[ r_FusionRing, i_ ] ] :=
  r[[CC[r][i]]];

PackageExport["CC"]

CC::usage =
  "Shorthand for ConjugateCharge";

FusionRing /: CC[ r_FusionRing?FusionRingQ ] :=
  ConjugateCharge[ r ];

FusionElement /: CC[ e_FusionElement ] :=
  ConjugateCharge[e];

PackageExport["FormalCode"]

FormalCode::usage =
  "FormalCode[ring] returns a 4-tuple that uniquely classifies the ring.";
(*  "The first 3 numbers are the Rank, Multiplicity, and Number of non-selfdual particles, while the 4th is the position in the list of rings with common first 3 numbers, sorted by amount of nonzero structure constants and Barcode.";*)

SetAttributes[ FormalCode, Listable ];

FusionRing /: FormalCode[ r_FusionRing?FusionRingQ ] :=
  r["FormalParameters"];

PackageExport["FC"]

SetAttributes[ FC, Listable ];

FC::usage =
  "Shorthand for FormalCode";

FusionRing /: FC[ r_FusionRing?FusionRingQ ] :=
  FormalCode[ r ];


(* If the ring does not have a barcode, either construct a new ring that does have one or never add the info to the
   ring in the first place.
 *)
PackageExport["Barcode"]

Barcode::usage =
  "Barcode[ring] returns a number associated to the set of rings obtained by permuting the elements of ring."<>
  "It equals the maximum of the numbers obtained flattening each multiplication table and interpreting the lists " <>
  "of integers as digits of an integer in base Multiplicity[ring] + 1. It is therefore an invariant under" <>
  "permutation of the elements of a ring and for rings with equal multiplicity it uniquely determines the " <>
  "structure of a ring.";

SetAttributes[ Barcode, Listable ];

FusionRing /: Barcode[ r_FusionRing?FusionRingQ ] :=
  Which[
    r["Barcode"] =!= Missing[]
    ,
    r["Barcode"],
    True
    ,
    Module[{ sRing, qds,permutations},
      sRing =
        FusionRing[
          "MultiplicationTable" -> PermuteMultTab[ MT[r], PermVecSDConj[r] ]
        ];

      qds =
        Rest @ FPDims[ sRing ]; (* 1 should be left alone *)

      permutations =
        Prepend[1] @* Flatten /@
        Tuples[ Permutations /@ (GatherBy[ Range[ Rank[r] - 1 ], qds[[#]]& ] + 1) ];

      Max[ multTabCode[ PermuteMultTab[ MT[sRing], # ], Mult[r] ]& /@ permutations ]
    ]
  ];

(* multTabCode assigns a number to a fusion ring based on the multiplication table. This
   number is unique per multiplication table but not invariant under permutations of the
   elements. The function barcode will then take the maximum of these numbers to make it
   independent of choice of basis.
  *)

multTabCode[ mat_List, mult_ ] :=
  FromDigits[ Flatten[ mat ], mult + 1 ];

multTabCode[ ring_FusionRing?FusionRingQ ] :=
  multTabCode[ MT @ ring, Mult @ ring ];


PackageExport["SubFusionRings"]

SubFusionRings::usage =
  "SubFusionRings[r] returns a list of tuples { s[i], ring[i] } where s[i] is a list of indices such that the " <>
  "restriction of the fusion ring r to the elements with those indices gives the fusion ring ring[i].";

SetAttributes[ SubFusionRings, Listable ];

SubFusionRings[ ring_FusionRing ] :=
  (
    If[
      ring["SubFusionRings"] =!= Missing[],
      Return @ ReleaseHold @ ring["SubFusionRings"]
    ];

    If[
      Rank[ring] === 1,
      Return @ {}
    ];

    Module[ { m, ToRing, multTabs },
      m =
        MT[ring];
      ToRing =
        FusionRing[ "MultiplicationTable" -> # ]&;
      multTabs =
        SubRingTables[m];
      {
        #["Subset"],
        ReplaceByKnownRing @ ToRing @ #["MultTab"]
      }& /@ multTabs
    ]
  );

SubRingTables[ multTab_ ] :=
  With[{
    IMQ = InternalMultiplicationQ[ multTab, # ]&,
    subsets = SubsetChoices[ multTab ]
    },
    <| "Subset"-> #, "MultTab"-> multTab[[#,#,#]]|>& /@
    Select[ subsets, IMQ ]
  ];

SubsetChoices[ multTab_ ] :=
  With[{
    apPairs =
      Sort /@ ( Position[ multTab[[2;;,2;;,1]], 1 ] + 1)/.{a_,a_}:>{a} },
    Prepend[ #, 1 ]& /@
    Join @@@
    Subsets[ DeleteDuplicates[ apPairs ] ][[2;;-2]]
  ];
  
PackageExport["SFR"]

SFR::usage = 
  "Shorthand for SubFusionRings";

PackageExport["InjectionForm"]

InjectionForm::usage =
  "InjectionForm[ ring, subring ] returns a fusion ring homomorphism as an association that maps each element of " <>
  "subring to a corresponding element of ring.";

InjectionForm[ ring_FusionRing?FusionRingQ, subring_FusionRing?FusionRingQ ] :=
  Module[{ mt, rs, mts, equivTable,	permutation, possiblePerms },
    mt =
    MT @ ring;
    rs =
    Rank @ subring;
    mts =
    MT @ subring;

    equivTable =
    SelectFirst[
      SubRingTables[ mt ],
      EquivalentMultiplicationTableQ[ mts, #["MultTab"] ]&
    ];

    If[
      MissingQ[equivTable],
      Return @ None
    ];

    possiblePerms =
    With[ { diagonalChannels = Rest[ Count[ x_/; x > 0 ] /@ Diagonal[ # ] ]& },
      Prepend[1] /@ (
        AllPermutations[
          diagonalChannels[ mts ],
          diagonalChannels[ equivTable["MultTab"] ]
        ] + 1
      )
    ];

    permutation =
    FirstCase[ possiblePerms, s_/; mts[[s,s,s]] == equivTable["MultTab"] ];

    Association @@
    Thread[
      Range[ rs ] -> equivTable["Subset"][[permutation]]
    ]
  ];


PackageExport["SubFusionRingQ"]

SubFusionRingQ::usage =
  "SubFusionRingQ[ ring, subRing ] returns True if subRing is a sub-ring of ring.";

SubFusionRingQ[ ring_FusionRing, subring_FusionRing ] :=
  With[ { subrings = SubFusionRings[ring][[;;,2]] },
    Catch[
    Do[
      If[
        EquivalentFusionRingsQ[ subring, r ],
        Throw @ True
      ],
      { r, subrings }
    ];
    False
    ]
  ];

PackageExport["EquivalentFusionRingsQ"]

EquivalentFusionRingsQ::usage =
  "EquivalentFusionRingQs[ring1,ring2] returns True if the elements of ring1 are a relabeling of the elements of ring2"<>
  " and False otherwise.";

EquivalentFusionRingsQ[ r1_FusionRing?FusionRingQ, r2_FusionRing?FusionRingQ ] :=
  With[{
    nFusionOutcomes1 = Map[ Count[ #, x_/; x > 0 ]&, MultiplicationTable[r1], {2}],
    nFusionOutcomes2 = Map[ Count[ #, x_/; x > 0 ]&, MultiplicationTable[r2], {2}]},
    With[{
      check = And[
        NNonZeroStructureConstants[r1] === NNonZeroStructureConstants[r2],
        NSelfDualNonSelfDual[r1] === NSelfDualNonSelfDual[r2],
        Sort[ Flatten[ nFusionOutcomes1 ] ] === Sort[ Flatten[ nFusionOutcomes2 ] ],
        Sort[ Diagonal[ nFusionOutcomes1 ] ] === Sort[ Diagonal[ nFusionOutcomes2 ] ],
        CommutativeQ[r1] == CommutativeQ[r2],
        Multiplicity[r1] == Multiplicity[r2]
      ]},
      If[
        !check,
        False,
        EquivalentMultiplicationTableQ[ MultiplicationTable[r1], MultiplicationTable[r2] ]
      ]
    ]
  ];

PackageExport["EFRQ"]

EFRQ::usage = 
  "Shorthand for EquivalentFusionRingsQ";

EFRQ = 
  EquivalentFusionRingsQ;

PackageScope["PermutationVector"]

PermutationVector[ tab1_, tab2_ ] :=
  Module[ { tabs, diagonalSums, n, prePerms, cyclePerms, newTabs, possiblePerms, p },
    tabs =
      { tab1, tab2 };

    diagonalSums =
      Rest[ Count[ #, x_/; x > 0 ]& /@ Diagonal[ # ] ]& /@ tabs;

    n =
      Length[ tab1 ];

    Catch[
      If[ Sort[ diagonalSums[[1]] ] =!= Sort[ diagonalSums[[2]] ], Throw @ None ];

      prePerms =
        Prepend[ (PermutationList[ FindPermutation[ Sort[#], # ], n - 1 ] + 1), 1 ]& /@
        diagonalSums;

      cyclePerms =
        PermutationCycles /@ prePerms;

      newTabs =
        MapThread[ PermuteMultTab, { tabs, prePerms } ];

      possiblePerms =
        PossiblePermutationVectors[ Sort @ First @  diagonalSums ];

      Do[
        p =
          Prepend[ possiblePerms[[i]] + 1, 1 ];

        If[
          PermuteMultTab[ newTabs[[1]], p ] === newTabs[[2]],
          Throw @
          PermutationList[
            PermutationProduct[
              InversePermutation @ cyclePerms[[2]],
              PermutationCycles[p],
              cyclePerms[[1]]
            ],
            n
          ]
        ],
        { i, Length[ possiblePerms ] }
      ];

      None
    ]
  ];

EquivalentMultiplicationTableQ[ tab1_, tab2_ ] :=
  PermutationVector[ tab1, tab2 ] =!= None;


PossiblePermutationVectors[ l_List ] :=
  AllPermutations[ l, l ];

AllPermutations[ l1_List, l2_List ] :=
  Select[ Tuples[ PositionIndex[l1] /@ l2 ], Apply[Unequal] ];



PackageExport["EFQ"]

EFQ::usage =
  "Shorthand for EquivalentFusionRingsQ.";

EFQ[ ring1_, ring2_ ] :=
  EquivalentFusionRingsQ[ ring1, ring2 ];


PackageExport["FusionRingAutomorphisms"]

FusionRingAutomorphisms::usage =
  "FusionRingAutomorphisms[ring] returns a list of permutation vectors that leaves the multiplication table "<>
  "of ring invariant.";

FusionRingAutomorphisms[ ring_FusionRing?FusionRingQ ] :=
  Module[{ mt, possiblePerms },
    mt =
      MultiplicationTable @ ring;
    possiblePerms =
      Prepend[1] /@ (
        PossiblePermutationVectors[
          Rest[ Count[ x_/; x > 0 ] /@ Diagonal[ mt ] ]
        ] + 1
      );
    Cases[ possiblePerms, perm_/; PermuteMultTab[ mt, perm ] === mt ]
  ];


PackageExport["FRA"]

FRA::usage =
  "Shorthand for FusionRingAutomorphisms.";

FRA[ ring_FusionRing ] :=
  FusionRingAutomorphisms[ring];


PackageExport["WhichDecompositions"]

WhichDecompositions::usage =
  "WhichDecompositions[ring] returns a list of lists of fusion rings whose tensor product is a fusion ring isomorphic to ring.";

WhichDecompositions[ r_FusionRing?FusionRingQ, list_List ] :=
If[
  r["DirectProductDecompositions"] =!= Missing[],
  ReleaseHold @ r["DirectProductDecompositions"],
  With[{
    candidates = CandidatesByTQDS[ r, list ] },
    DeleteDuplicates[
      Sort /@ Cases[ candidates, rings_/; EquivalentFusionRingsQ[ r, TensorProduct @@ rings ] ]
    ]
  ]
];

WhichDecompositions[ r_FusionRing?FusionRingQ ] :=
  WhichDecompositions[ r, FusionRingList ];

CandidatesByTQDS[ r_FusionRing?FusionRingQ, list_ ] :=
  Module[{
    tqds = FPDim[r],
    R = Rank[ r ],
    partitions,
    AllQDims,
    rings
    },
    If[
      PrimeQ[ R ]
      ,
      { }
      ,
      partitions =
        MultiplicativePartitions[ R ];
      rings =
        Table[ Cases[ list, ring_/; Rank[ring] == ran ] , { p, partitions }, { ran, p } ];
      AllQDims =
        Map[ FPDim, rings, {3} ];

      Table[
        { rings[[i]], BackTrackQDims[ AllQDims[[i]], tqds ] },
        { i, Length @ partitions }
      ] //
      Cases[ #, {rings_,{numbers_}} :> Table[ MapThread[ Part, { rings, ind } ], {ind, numbers} ] ]& //
      Flatten[ #, 1 ]&
    ]
  ];


BackTrackQDims[ quantumDims_, tqds_ ] := Block[{
  $RecursionLimit = Infinity},
  Reap[ internalBackTrackQDims[ quantumDims, Length[quantumDims], tqds, 1, {}, 1 ] ]// Last
];

internalBackTrackQDims[ qDims_, nRings_ , goal_, current_, ringNs_ , ringN_ ] :=
If[ ringN > nRings && Chop[N[goal - current ],10^-5] == 0,
  Sow[ ringNs ],
  If[ ringN <= nRings && Chop[N[goal - current],10^-5] > 0,
    Do[
      internalBackTrackQDims[ qDims, nRings, goal, current * qDims[[ringN,i]] , Append[ ringNs, i ], ringN + 1 ]
      ,{ i, Length[ qDims[[ringN]] ] }];
  ];
];

(* MultiplicativePartitions[int] returns list of integer tuples {i1,...,in} such
   that i1*...*in \[Equal] int. This is needed to find all possible ranks of rings whose
   direct product could equal the given ring *)

MultiplicativePartitions[x_] :=
Rest[
  DeleteDuplicates[
    Sort /@
    Map[ Times @@ # &,
      SetPartitions[
        Flatten[ ConstantArray @@@ FactorInteger[x] ]
      ],
      {2}
    ]
  ]
];

SetPartitions[ {} ] :=
{ {} };

SetPartitions[ s_List ] :=
Array[ KSetPartitions[ s ], Length[s] ] // Flatten[#,1]&;

KSetPartitions[ {} ][ 0 ] :=
{ {} };

KSetPartitions[ s_List ][ 0 ] :=
{};

KSetPartitions[ s_List ][ k_Integer ] :=
{} /; ( k > Length[s] );

KSetPartitions[ s_List ][ k_Integer ] :=
{ Map[ { # }&, s ] } /; (k === Length[s]);

KSetPartitions[ s_List ][ k_Integer ] :=
Block[ { $RecursionLimit = Infinity },
  Join[(* Put first element in subset of its own *)
    Prepend[ #, {First[s]} ] & /@ KSetPartitions[ Rest[s] ][ k-1 ],
    (* Put first el in one of the subsets  *)
    Table[ PrependAt[ #, First @ s, j ], {j,Length[#]}]& /@ KSetPartitions[ Rest[s] ][ k ] // Flatten[#,1]&
  ]
]/; ( k > 0 ) && ( k < Length[s] );

PrependAt[ listoflists_, element_, position_ ] :=
  Module[{ l = listoflists },
    PrependTo[ l[[position]], element ];
    l
  ];

PackageExport["WD"]

WD::usage =
  "Shorthand for WhichDecompositions.";

WD[ r__] :=
  WhichDecompositions[ r ];


(*Finding subrings*)
(*Check whether the multiplication is internal*)
InternalMultiplicationQ[ multTab_, particles_ ] :=
  With[{ comp = Complement[ Range[ Length[multTab] ], particles ] },
    MatchQ[ Flatten @ multTab[[ particles, particles, comp ]], { 0 .. } ]
  ];


PackageExport["AdjointFusionRing"]

AdjointFusionRing::usage =
  "AdjointFusionRing[ ring ] returns a couple { el, fr } where fr is the adjoint fusion sub-ring of ring "<>
  "and el corresponds to the elements of ring that form this sub-ring (and are ordered such that the first element "<>
  "of el corresponds to the first element of fr, and so on).";

AdjointFusionRing[ ring_FusionRing?FusionRingQ ] :=
  Module[ { d, mt, el, generatedEl },
    d =
      CC[ring];
    mt =
      MT[ring];
    el =
      Sort @
      DeleteDuplicates @
      Map[Last] @
      Select[ #[[1]] == d[ #[[2]] ] & ] @
      NZSC @
      ring;

    generatedEl =
      FixedPoint[
        Function[
          l,
          Union[ l, Flatten @ Table[FusionOutcomes[ring][i,j], {i,l}, {j,l} ] ]
        ],
        el
      ];

    If[
      Length[ generatedEl] === Rank[ring]
      ,
      { generatedEl, ring }
      ,
      {
        generatedEl,
        ReplaceByKnownRing @
        FusionRing[
          "MultiplicationTable" -> mt[[generatedEl,generatedEl,generatedEl]]
        ]
      }
    ]

  ];

PackageExport["AFR"]

AFR::usage =
 "Shorthand for AdjointFusionRing";

AFR =
  AdjointFusionRing;


PackageExport["UpperCentralSeries"]

UpperCentralSeries::usage =
  "UpperCentralSeries[ fusionRing ] returns a list of couples { c[1], ..., c[n] } where c[i] = { el[i], adj[i] } and subring [i] corresponds to the adjoint subring of adj[i-1] and el[i] to the elements of adj[i-1] that form this subring.";

UpperCentralSeries[ ring_FusionRing?FusionRingQ ] :=
  DeleteDuplicatesBy[ (* Remove trivial inclusions *)
    FixedPointList[
      AdjointFusionRing @* Last,
      { Range @ Rank @ ring, ring }
    ],
    Last
  ];

PackageExport["UCS"]

UCS::usage =
  "Shorthand for UpperCentralSeries.";

UCS =
  UpperCentralSeries;


PackageExport["NilpotentFusionRingQ"]

NilpotentFusionRingQ::usage =
  "NilpotentFusionRingQ[ fusionRing ] returns True if the fusion ring fusionRing is nilpotent and False otherwise.";

NilpotentFusionRingQ[ring_] :=
  TrueQ[ ( First @ Last @ UpperCentralSeries[ring] ) == {1} ];

PackageExport["NPFRQ"]

NPFRQ::usage =
  "Shorthand for NilpotentFusionRingQ.";

NPFRQ =
  NilpotentFusionRingQ;


PackageScope["FusionOutcomes"]

FusionOutcomes[ ring_ ][ i_, j_ ] :=
  Sort @
  Map[Last] @
  Cases[ { i, j, _ } ] @
  NZSC @
  ring;


PackageExport["AdjointIrreps"]

AdjointIrreps::usage =
  "AdjointIrreps[ fusionRing ] returns a partition of the elements of fusionRing, where each set is invariant " <>
  "under left-and right action of the adjoint subring. ";
(*  "The first element always corresponds to the adjoint subring itself.";*)

AdjointIrreps[ ring_ ] :=
  Module[ { adRing },
    adRing = AdjointFusionRing[ ring ];
    DeleteDuplicates[
      Table[
        FixedPoint[ CombinedAction[ { ring, adRing } ], { e } ],
        { e, Range @ Rank @ ring }
      ]
    ]
  ];

Action[ { ring_FusionRing?FusionRingQ, { subEl_List, subRing_FusionRing?FusionRingQ } }, elements_List  ] :=
  Table[
    FusionOutcomes[ring][ a, el ],
    { a, subEl },
    { el, elements }
  ] //
  Flatten //
  DeleteDuplicates //
  Sort;

Action[ elements_List, { ring_FusionRing?FusionRingQ, { subEl_List, subRing_FusionRing?FusionRingQ } } ] :=
  Table[
    FusionOutcomes[ring][ el, a ],
    { a, subEl },
    { el, elements }
  ] //
  Flatten //
  DeleteDuplicates //
  Sort;

CombinedAction[ pair:{ ring_FusionRing?FusionRingQ, { subEl_List, subRing_FusionRing?FusionRingQ } } ][ elements_List ] :=
  Action[ pair, elements ] ~ Union ~ Action[ elements, pair ];


PackageExport["UniversalGrading"]

UniversalGrading::usage =
  "UniversalGrading[ ring ] returns a couple { grading, groupRing } where grading is a list of rules of elements "<>
  "from ring to groupRing and groupRing is the universal group that grades the fusion ring.";

UniversalGrading[ ring_ ] :=
  Module[ { irreps, nIrreps, elements, grading, cond, mt },
    irreps =
      AdjointIrreps[ ring ];

    nIrreps =
      Length @ irreps;

    elements =
      Range @	nIrreps;

    grading =
      Sort @
      Flatten @
      MapThread[
        Thread[ #2 -> #1 ] &,
        { elements, irreps }
      ];

    cond[ l1_List, l2_List, l3_List ] :=
      SubsetQ[
        l3,
        Flatten @ Table[
          FusionOutcomes[ring][ i, j ],
          { i, l1 }, { j, l2 }
        ]
      ];

    mt =
      Table[
        If[
          cond[ irreps[[a]], irreps[[b]], irreps[[c]] ],
          1,
          0
        ],
        { a, nIrreps }, { b, nIrreps }, { c, nIrreps }
      ];

    {
      grading,
      ReplaceByKnownRing @
      FusionRing[
        "MultiplicationTable" -> mt
      ]
    }
  ];

PackageExport["UG"]

UG::usage = 
  "Shorthand for UniversalGrading."


PackageExport["AllGradings"]

AllGradings::usage = 
  "AllGradings[ r ] returns all possible gradings of the fusion ring r.";

PackageExport["FusionRingCommutator"]

FusionRingCommutator::usage =
  "FusionRingCommutator[ ring, subRing ] returns the commutator of subRing in ring.";

FusionRingCommutator[ ring_FusionRing, subRing_List ] :=
  Which[
    !CommutativeQ[ring]
    ,
    Message[ FusionRingCommutator::noncommutativering ]; None
    ,
    !SubFusionRingQ[ ring, subRing[[2]] ]
    ,
    Message[ FusionRingCommutator::notsubring, subRing, ring ]
    ,
    True
    ,
    Module[ { d, subRingEl, inSubRingQ, el },
      d = CC[ring]; subRingEl = subRing[[1]];
      inSubRingQ[ i_ ] :=
        SubsetQ[
          subRingEl,
          FusionOutcomes[ring][ i, d[i] ]
        ];
      el =
      Select[
        Range @ Rank @ ring,
        inSubRingQ
      ];
      {
        el,
        ReplaceByKnownRing @
        FusionRing[
          "MultiplicationTable" -> MT[ring][[el,el,el]]
        ]
      }
    ]
  ];


PackageExport["FusionRingCharacters"]

FusionRingCharacters::usage =
  "FusionRingCharacters[r], with r a commutative ring, returns a symbolic character table of r or a numeric one if "<>
  "no symbolic form was found.";

FusionRingCharacters[ ring_FusionRing?FusionRingQ ] :=
  ring["Characters"];


PackageExport["FRC"]

FRC::usage =
  "Shorthand for FusionRingCharacters";

FRC[ ring_FusionRing?FusionRingQ ] :=
  FusionRingCharacters[ ring ];


PackageExport["NormalizedSMatrices"]

NormalizedSMatrices::usage =
  "NormalizedSMatrices[ring] returns a list of possible S-matrices of the ring that are normalized to be unitary.";

SetAttributes[ NormalizedSMatrices, Listable];

NormalizedSMatrices[ ring_FusionRing?FusionRingQ ] :=
  ring["SMatrices"];


PackageExport["NSM"]

NSM::usage =
  "Shorthand for NormalizedSMatrices.";

NSM[ ring_FusionRing?FusionRingQ ] :=
  NormalizedSMatrices[ ring ];


PackageExport["TwistFactors"]

TwistFactors::usage =
  "TwistFactors[ring] returns a vector of rational numbers denoting the factors q in the "<>
  "expressions Exp[ 2 \[Pi] \[ImaginaryI] q ] that appear in the T-matrix of the ring.";

SetAttributes[ TwistFactors, Listable];

TwistFactors[ ring_FusionRing?FusionRingQ ] :=
  ring["TwistFactors"];


PackageExport["TF"]

TF[ ring_FusionRing?FusionRingQ ] :=
  TwistFactors[ ring ];

TF::usage =
  "Shorthand for TwistFactors.";

PackageExport["ModularData"]

ModularData::usage =
  "ModularData[ring] returns a list of associations <| \"SMatrix\" -> Si, \"TwistFactors\" -> Qi |>, where the "<>
  " Si are the S matrices of the ring and the Qi lists of twist factors for for which the corresponding T-matrix "<>
  "obeys (ST\!\(\*SuperscriptBox[\()\), \(3\)]\) == \!\(\*SuperscriptBox[\(\[Lambda]S\), \(2\)]\) with \[Lambda] a " <>
  "non-zero complex number. If there are no compatible T-matrices for any S-matrix an empty list is returned.";

SetAttributes[ ModularData, Listable ];

ModularData[ ring_FusionRing?FusionRingQ ] :=
  ring["ModularData"];


PackageExport["MD"]

MD::usage =
  "Shorthand for ModularData.";

MD[ ring_FusionRing?FusionRingQ ] :=
  ModularData[ ring ];
