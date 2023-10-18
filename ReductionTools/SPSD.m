(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gert *)
(* :Date: 2023-10-18 *)
(*
  Here we define constructors and accessors for the standard polynomial system datastructure
  
  <|
      "Polynomials" -> list of polynomials after reduction,
      "Assumptions" -> logical expression that expresses assumptions on the variables,
      "Values"      -> list of rules mapping the original unknowns to expressions in terms of unknowns appearing in
                       the list of polynomials after reduction, or numbers if all polynomials have been solved for,
      "Variables"   -> list of remaining unknowns
   |>
   
   We call this the Standard Polynomial System (SPS)

*)

ToSPS[ polynomials_, assumptions_, values_, variables_ ] :=
  SPS[
    Association @
    {
      "Polynomials" -> polynomials,
      "Assumptions" -> assumptions,
      "Values"      -> values,
      "Variables"   -> variables
    }
  ];

ToSPS[ polynomials_, values_, variables_ ] :=
  ToSPS[ polynomials, True, values, variables ];

ToSPS[ polynomials_, variables_ ] :=
  ToSPS[ polynomials, True, Thread[ variables -> variables ], variables ];


GetPolynomials[ SPS[ data_Association ] ] :=
  data["Polynomials"];

GetAssumptions[ SPS[ data_Association ] ] :=
  data["Assumptions"];

GetValues[ SPS[ data_Association ] ] :=
  data["Values"];

GetVariables[ SPS[ data_Association ] ] :=
  data["Variables"];

GetData[ SPS[ data_Association ] ] :=
  Values @ data;

Options[ ValidQ ] =
  {
    "PreEqualCheck" -> Identity
  };

ValidQ[ sys_SPS, OptionsPattern[] ] :=
  Catch[
    Module[ { pols, assump, vals, firstProblem, check, val },
      check = OptionValue["PreEqualCheck"];
      
      pols = GetPolynomials @ sys;
      assump = GetAssumptions @ sys;
      vals = GetValues @ sys;
    
      If[ assump/.vals === False, Throw @ False ];
      
      firstProblem =
        FirstCase[ pols, pol_ /; NumericQ @ ( val = check[pol] ) && val != 0 ];
      
      If[ !MissingQ[firstProblem], Throw @ False, Throw @ True ]
    ]
  ];

