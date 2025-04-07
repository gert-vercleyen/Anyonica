(*
  Here we define constructors and accessors for the standard polynomial system datastructure
  
  <|
      "Polynomials" -> list of polynomials in variables s[1], ..., s[n] ,
      "Assumptions" -> logical expression that expresses assumptions on the variables,
      "Values"      -> list of rules mapping the original unknowns to expressions in terms s[1], ..., s[n]
                       (or numbers if all polynomials have been solved for)
      "Variables"   -> list of remaining unknowns appearing in polynomials
      "Weights"     -> list of weights of the remaining unknowns. This allows to set prefered order in which they are solved for
   |>
   
   We call this the Standard Polynomial System (SPS)

*)
Unprotect[SPS];

ClearAll[SPS];


ToSPS::nonvalidvariables = 
  "The list of variables `1` should be of the form s[1], ..., s[n] where"<>
  " s is a unique symbol. You can use the function SimplifyVariables to "<>
  "transform your system to one in a single indexed symbol.";

ToSPS::nonpolynomialsystem = 
  "`1` should be a list of polynomials in the variables `2`.";

ToSPS::notlistofrules =
  "`1` should be a list of rules.";

ToSPS::variablesrulesmismatch = 
  "The variables appearing in the values of `1` should be equal to the variables `2` of the system.";


CheckArgumentsToSPS[ polynomials_, rules_, variables_, s_ ] :=
  Which[ 
    !MatchQ[ rules, { _Rule ... }],
    Message[ ToSPS::notlistofrules, rules ]; Abort[]
    ,
    variables =!= Array[ s, Length @ variables ], 
    Message[ ToSPS::nonvalidvariables, variables ]; Abort[]
    ,
    GetVariables[ Values @ rules, s ] =!= Sort[variables]
    ,
    Message[ ToSPS::variablesrulesmismatch, rules, variables ]; Abort[]
    ,
    !listOfPolsQ[ polynomials, s ], 
    Message[ ToSPS::nonpolynomialsystem, polynomials, Values @ rules ]; Abort[]
  ]

listOfPolsQ[ polynomials_, s_ ] :=
  With[{ 
      check = If[ !PolynomialQ[ #, GetVariables[ #, s ] ], Throw @ False ]& 
    },
    Catch[
      check /@ polynomials;
      True
    ]
  ];

Options[ ToSPS ] :=
  {
    "Weights" -> {}
  };

ToSPS[ polynomials_, assumption_, rules_, variables_, s_, opts:OptionsPattern[] ] :=
Module[{ weights = OptionValue["Weights"] },

  CheckArgumentsToSPS[ polynomials, rules, variables, s ];

  If[ weights === { }, weights = Thread[ variables -> 1 ] ];

  SPS[
    Association @
    {
      "Polynomials" -> polynomials,
      "Assumption"  -> assumption,
      "Rules"       -> rules,
      "Variables"   -> variables,
      "Weights"     -> weights,
      "Symbol"      -> s
    }
  ]
]


ToSPS[ polynomials_, assumption_, rules_ ] :=
  ToSPS[ polynomials, assumption, rules, Values @ rules ];

GetPolynomials[ SPS[ data_Association ] ] :=
  data["Polynomials"];

GetAssumptions[ SPS[ data_Association ] ] :=
  data["Assumption"];

GetRules[ SPS[ data_Association ] ] :=
  data["Rules"];

GetWeights[ SPS[data_Association] ] := 
  data["Weights"];

SPS /: GetVariables[ SPS[ data_Association ] ] :=
  data["Variables"];

GetData[ SPS[ data_Association ] ] :=
  Values @ data;

UpdateSystem::nonexistingvars = 
  "The new rules `1` for the system don't apply to any of the variables `2`.";

Options[UpdateSystem] = 
  { 
    "Weights" -> {},
    "SimplifyIntermediateResultsBy" -> Identity,
    "TrimPolynomialList" -> True
  };

UpdateSystem[ sys:SPS[ data_Association ], newRules_, newVars_, s_, opts:OptionsPattern[] ] :=
  Module[{ simplify, trim, vars, rules, dr, updatedRules },
    simplify      = OptionValue["SimplifyIntermediateResultsBy"];
    trim          = If[ OptionValue["TrimPolynomialList"], TPL, Identity ];
    vars          = GetVariables @ sys; 
    rules         = GetRules @ sys;
    dr            = Dispatch @ newRules;
    updatedRules  = Thread[ Keys @ rules -> simplify[ Values[rules]/.Dispatch[newRules] ] ];

    (* sanity check on the rules *)
    If[
      !SubsetQ[ vars, Keys @ newRules ],
      Message[UpdateSystem::nonexistingvars,newRules,vars]
    ];
    
    ToSPS[
      trim @ simplify[ GetPolynomials[sys]/.dr ],
      simplify[ GetAssumptions[sys]/.dr ],
      updatedRules,
      newVars,
      s,
      opts
    ]
  ];

(* This one might be too dangerous
UpdateSystem[ sys_SPS, newRules_, opts:OptionsPattern[] ] :=
  UpdateSystem[ 
    sys, 
    newRules, 
    Complement[ GetVariables[sys], Keys @ newRules ], 
    GetSymbol @ sys,
    opts
  ];
*)

Options[ ValidQ ] =
  {
    "PreEqualCheck" -> Identity
  };

ValidQ[ sys_SPS, OptionsPattern[] ] :=
  With[ { check = OptionValue["PreEqualCheck"], assump = GetAssumptions @ sys },
    Not[ BooleanMinimize[ check @ assump ] === False ]
  ];
