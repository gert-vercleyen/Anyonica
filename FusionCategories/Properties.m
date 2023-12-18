(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-09-14 *)

Package["Anyonica`"]

ChangeProperty[ ring_FusionCategory, list_ ] :=
  Module[ {opts},
    opts = (* All defining properties of previous fusion ring *)
    Normal @ First[ List @@ ring ];
    AddOptions[opts][FusionCategory][ Sequence @@ list ]

  ];


PackageExport["BraidedQ"]

BraidedQ::usage =
  "BraidedQ[fusionCat] returns True if the fusion category has a set of R-symbols.";
  
BraidedQ[ cat_FusionCategory ] :=
  !MissingQ[ RSymbols @ cat ];

FusionCategory /: UnitaryGaugeQ[ cat_FusionCategory, opts:OptionsPattern[] ] :=
  UnitaryGaugeQ[ FusionRing @ cat, FSymbols @ cat, opts ];


PackageExport["AllPivotalStructures"]

AllPivotalStructures::usage =
  "PivotalStructure[fusionCat] returns a list of pivotal structures compatible with the "<>
  "the F-symbols of fusionCat.";

Attributes[AllPivotalStructures] = { Listable };

Options[AllPivotalStructures] :=
  {
    "SimplifyBy" -> Identity
  };

AllPivotalStructures[ cat_FusionCategory, opts:OptionsPattern[] ] :=
  Module[{ r, e, d, sF, eqns, rhs },
    r =
      Rank @ cat;
    d =
      CC[cat];
    sF =
      SparseArray[
        MapAt[ List@@#&, FSymbols[cat], {All,1} ],
        {r,r,r,r,r,r}
      ];
      
    rhs[a_,b_,c_] :=
      sF[[a,b,d[c],1,c,d[a]]] sF[[b,d[c],a,1,d[a],d[b]]] sF[[d[c],a,b,1,d[b],c]];
   
    eqns =
      TEL @
      OptionValue["SimplifyBy"][
        Cases[
          Tuples[ Range @ r, 3 ],
          { a_, b_, c_ } /;
          rhs[a,b,c] =!= 0 :>
          e[a] e[b] / e[c]  == rhs[a,b,c]
        ]/.e[1] -> 1
      ];
      
    Prepend[1] /@
    Solve[ eqns, Rest @ Array[ e, r ] ][[;;,;;,2]]
  ];

PackageExport["PivotalStructure"]

PivotalStructure::usage = 
  "PivotalStructure[cat] returns the pivotal structure of the fusion category cat.";
 
PivotalStructure[ FusionCategory[data_]] := 
  data["PivotalStructure"];

PackageExport["Twists"]

Twists::usage = 
  "Twists[cat] returns the topological twists of the fusion category cat.";
 
Twists[ FusionCategory[data_] ] := 
  data["Twists"];

PackageExport["QuantumDimensions"]

QuantumDimensions::usage =
  "QuantumDimensions[cat] returns a list of dimensions \!\(\*SuperscriptBox[\(T\), \(L\)]\)(a) of the simple objects "<>
  "of the fusion category cat";
 
QuantumDimensions[ cat_FusionCategory ] :=
  With[{ d = CC[cat], p = PivotalStructure[cat] },
    Table[
      p[[a]] F[a,d[a],a,a,1,1], { a, Rank @ cat }
    ]/.FSymbols[cat]
  ];

PackageExport["SMatrix"]

SMatrix::usage =
  "SMatrix[cat] returns the S-matrix of the fusion category cat.";
 
SMatrix[ FusionCategory[data_] ] :=
  data["SMatrix"];

PackageExport["ModularQ"]

ModularQ::usage =
  "ModularQ[cat] returns True if the fusion category cat is modular.";
 
ModularQ[ FusionCategory[data_] ] :=
  data["Modular"];

ModularData[ cat_FusionCategory ] :=
  If[
    ModularQ[ cat ],
    { SMatrix[cat], Twists[cat] },
    Missing["NonModularCategory"]
  ];

PackageExport["SphericalQ"]

SphericalQ::usage =
  "SphericalQ[cat] returns True if cat is a sperical fusion category.";
 
SphericalQ::nodims =
  "The category has no quantum dimensions so can not be checked for sphericality.";

SphericalQ[ cat_FusionCategory ] :=
  With[ { dims = QuantumDimensions[cat] },
    If[
      MissingQ[ dims ],
      Message[ SphericalQ::nodims ]; $Failed,
      And @@ Table[ RootReduce[ dims[[i]] == dims[[i+1]] ], { i, NSD[cat]+1, Rank[cat], 2 } ]
    ]
  ];