(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-04-24 *)
Package["Anyonica`"]

PackageExport["FusionProduct"]

FusionProduct::usage =
  "FusionProduct[ring, {i,j}] returns a sum of FusionElements that equals the fusion product of elements i and j.";

FusionProduct[ r_FusionRing?FusionRingQ, { el1_, el2_ } ] :=
  With[ { tab = MultiplicationTable[r] },
    Sum[ ElementNames[r][[i]]tab[[ el1, el2, i ]], { i, Rank[r] } ]
  ];



PackageExport["FusionElement"]

FusionElement::usage =
  "FusionElement[ring, i] represents a symbolic form of the i'th generator of the fusion ring r. If no \"ElementsName\" option is set, FusionElement[ring, i] will be formated as \!\(\*SubscriptBox[\(\[Psi]\), \(i\)]\). If the \"ElementsName\" option is set to \"string\" then the FusionElement[ring,i] will be formatted as \!\(\*SubscriptBox[\(string\), \(i\)]\). If the option \"ElementNames\" is set then FusionElement[ring,i] will be formatted by the i'th entry of ring[\"ElementNames\"]";

FusionElement /: FrobeniusPerronDimension[ FusionElement[ r_FusionRing?FusionRingQ, el_ ] ] :=
  FrobeniusPerronDimension[ FusionElement[ r, el ] ] =
  (FrobeniusPerronDimension[ r ])[[ el ]];

FusionElement /: FPDim[  el_FusionElement  ] :=
  FrobeniusPerronDimension[ el ];

(*
  Call particles by invoking ring[[el]], where el is either Integer,
  or a Symbol or String corresponding to one of the strings defined
  in ring[ElementNames]
*)

FusionRing::elnotfound =
  "`1` is not a known name of an element";
FusionRing::eloutofbounds =
  "Particle number `1` does not belong to any particle in the ring";

FusionRing /: r_FusionRing?FusionRingQ[[el_Integer]] :=
If[ 0 < el <= Rank[r],
  FusionElement[ r, el ],
  Message[ FusionRing::eloutofbounds, el ]
];

FusionRing /: r_FusionRing?FusionRingQ[[el_String]] :=
With[{
  pos = Position[ElementNames[r], el ]},
  If[ Length[pos] == 0,
    Message[ FusionRing::elnotfound, el ],
    FusionElement[ r, pos[[1,1]] ]
  ]
];

FusionRing /: r_FusionRing?FusionRingQ[[el_Symbol]] :=
With[{
  pos = Position[ElementNames[r], ToString[el] ]},
  If[ Length[pos] == 0,
    Message[ FusionRing::elnotfound, el ],
    FusionElement[ r, pos[[1,1]] ]
  ]
];

(* Use CenterDot (esc . esc) notation for the fusion product between elements *)
FusionElement /:
CenterDot[
  FusionElement[ r_FusionRing?FusionRingQ, el1_ ],
  FusionElement[ r_FusionRing?FusionRingQ, el2_ ] ] := FusionProduct[ r, { el1, el2 } ];

(* Encode linearity of the multiplication *)

CenterDot[ a___, b_?IntegerQ * x_, c___ ] :=  b * CenterDot[ a, x, c ];
CenterDot[ a___, ( x_ + y_ ), c___ ] := Expand[ CenterDot[ a, x, c ] + CenterDot[ a, y, c ] ];
CenterDot[ x_ ] := x ;
CenterDot[ a_Integer, b_Integer ] := a * b;
CenterDot[ a_, b_, c__] := Expand[ CenterDot[ CenterDot[ a, b ], CenterDot[c] ] ];

