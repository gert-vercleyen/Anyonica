(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-09-14 *)

Package["Anyonica`"]

FusionCategory /: ApplyGaugeTransform[ cat_FusionCategory, gaugeVals_, s_ ] :=
  With[{ opts = Normal @ First @ ( List @@ cat ) },
    AddOptions[opts][FusionCategory][
      "FSymbols" -> ApplyGaugeTransform[ FSymbols @ cat, gaugeVals, s ],
      "RSymbols" -> ApplyGaugeTransform[ RSymbols @ cat, gaugeVals, s ],
      "SkipCheck" -> True
    ]
  ];
  