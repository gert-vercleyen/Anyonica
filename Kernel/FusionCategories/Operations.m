(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-09-14 *)

Package["Anyonica`"]


FusionCategory /: ApplyGaugeTransform[ cat_FusionCategory, gaugeVals_, s_ ] :=
  With[{ opts = Normal @ First @ ( List @@ cat ), p = PivotalStructure @ cat, d = CC[cat] },
    AddOptions[opts][FusionCategory][
      "FSymbols" -> ApplyGaugeTransform[ FSymbols @ cat, gaugeVals, s ],
      "RSymbols" -> If[ BraidedQ[cat], ApplyGaugeTransform[ RSymbols @ cat, gaugeVals, s ], Missing["NonBraidedCategory"] ],
      "PivotalStructure" -> Table[ p[[a]] s[a,d[a],1]/s[d[a],a,1] /. gaugeVals, {a,Rank[cat]} ],
      "SkipCheck" -> True
    ]
  ];

(* We need to call this before unprotecting TensorProduct since otherwise we get an error.
I don't know why this happens though *)

Options[TensorProduct];

Unprotect[TensorProduct];

Options[TensorProduct] =
  Join[
    Options[FusionCategory],
    Options[FusionRing],
    { "SimplifyBy" -> Identity }
  ];

Protect[TensorProduct];

FusionCategory /: TensorProduct[ cat1: FusionCategory[_], cat2: FusionCategory[_], opts:OptionsPattern[] ] :=
  Module[ { sF1, sF2, sR1, sR2, r1, r2, fSymbols, rSymbols, simplify, twists, modQ, sMat },
    simplify =
      OptionValue["SimplifyBy"];

    r1 = Rank[cat1];
    r2 = Rank[cat2];

    sF1 = SparseArray[ FSymbols[cat1] /. F -> List, { r1, r1, r1, r1, r1, r1 } ];
    sF2 = SparseArray[ FSymbols[cat2] /. F -> List, { r2, r2, r2, r2, r2, r2 } ];
    fSymbols = simplify @ MapAt[ Apply[F], Most @ ArrayRules @ KroneckerProduct[ sF1, sF2 ], { All, 1 } ];

    If[
      BraidedQ[cat1] && BraidedQ[cat2]
      ,
      sR1      = SparseArray[ RSymbols[cat1] /. R -> List, { r1, r1, r1 } ];
      sR2      = SparseArray[ RSymbols[cat2] /. R -> List, { r2, r2, r2 } ];
      rSymbols = simplify @ MapAt[ Apply[R], Most @ ArrayRules @ KroneckerProduct[ sR1, sR2 ], { All, 1 } ];
      twists   = Flatten @ KroneckerProduct[Twists[cat1],Twists[cat2]];
      modQ     = TrueQ[ ModularQ[cat1] && ModularQ[cat2] ];
      sMat     = KroneckerProduct[ SMatrix[cat1], SMatrix[cat2] ]
      ,
      rSymbols = Missing["NonBraidedCategory"];
      twists   = Missing["NonBraidedCategory"];
      modQ     = False,
      sMat     = Missing["NonBraidedCategory"]
    ];

    pivStruct =
      Thread[
        Array[ \[ScriptP], r1 * r2 ] ->
        Flatten @
        KroneckerProduct[ Values @ PivotalStructure[cat1], Values @ PivotalStructure[cat2] ]
      ];

    AddOptions[opts][FusionCategory][
      "FusionRing" -> TensorProduct[ FusionRing[cat1], FusionRing[cat2] ],
      "FSymbols"   -> fSymbols,
      "RSymbols"   -> rSymbols,
      "Unitary"    -> And[ UnitaryQ @ cat1, UnitaryQ @ cat2 ],
      "PivotalStructure" -> pivStruct,
      "Twists"     -> twists,
      "Modular"    -> modQ,
      "SMatrix"    -> sMat,
      "SkipCheck"  -> True
    ]
  ];

PackageExport["PermutedFusionCategory"]

PermutedFusionCategory::usage =
  "PermutedFusionCategory[cat,perm] returns a fusion category whose objects are permuted by"<>
  "the permutation vector perm";

Options[PermutedFusionCategory] := 
  { 
    "SkipCheck" -> True,
    "PreEqualCheck" -> Identity
  }

(*TODO: include dims ... *)
PermutedFusionCategory[ cat:FusionCategory[data_], perm_, OptionsPattern[] ] :=
  Module[ { pVec, permuteSymbols, permutedRing, sMat },
    pVec =
      Permute[ Range @ Length @ perm, PermutationCycles @ perm ];

    permuteSymbols =
      Sort @ MapAt[ ReplaceAll[ i_Integer :> pVec[[i]] ] , #, { All, 1 } ]&;

    permutedRing = 
      PermutedRing[ FusionRing @ cat, perm ];

    sMat = 
      If[ ModularQ @ cat, SMatrix[cat][[ perm, perm ]], SMatrix @ cat ];
    
    twists = 
      If[ BraidedQ[cat] && SphericalQ[cat], permuteSymbols @ Twists @ cat, Twists @ cat ];   

    AddOptions[ Normal @ data ][FusionCategory][
      "FusionRing"        -> permutedRing,
      "FSymbols"          -> permuteSymbols @ FSymbols @ cat,
      "RSymbols"          -> If[ BraidedQ[cat], permuteSymbols @ RSymbols @ cat, RSymbols[cat] ],
      "PivotalStructure"  -> permuteSymbols @ PivotalStructure @ cat,
      "Twists"            -> twists,
      "SMatrix"           -> sMat,
      "FormalParameters"  -> FormalCode @ cat,
      "Unitary"           -> UnitaryQ @ cat,
      "Modular"           -> ModularQ @ cat,
      "SkipCheck"         -> OptionValue["SkipCheck"],
      "PreEqualCheck"     -> OptionValue["PreEqualCheck"]
    ]
  ];
