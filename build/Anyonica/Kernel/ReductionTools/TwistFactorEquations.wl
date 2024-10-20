(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-06-14 *)

Package["Anyonica`"]

(*TwistFactorEquations::usage =
    "TwistFactorEquations[ring,twistFactors] returns the equations that relate twist factors to R-symbols for a" <>
    "braided fusion category with ring as Grothendieck ring.";
*)

TwistFactorEquations[ ring_FusionRing, t_ ] :=
  Module[
    { t, d, a, b, c, rs },
    d =
      QD[ring][[#]]&;
    rs =
      RSymbols[ring];

    TEL[
      Join[
        Table[
          { a, b, c } =
            List @@ r;
          R[ a, b, c ] * R[ b, a, c ] == t[c] / ( t[a] t[b] ),
          { r, rs }
        ]
      ]/.$VacuumRPattern -> 1
    ]

  ];