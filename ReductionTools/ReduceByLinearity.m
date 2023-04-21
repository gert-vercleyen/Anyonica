(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-03-09 *)


Package["Anyonica`"]


Options[ SimplestLinearRule ] =
  { "LinearReductionWeight" -> RationalWeight };

SimplestLinearRule[ pols_, s_, opts:OptionsPattern[] ] :=
  With[
    {
      cases =
      Cases[
        FindLinearRule[ #, s ]& /@ pols,
        r_/; Head[ r ] =!= Missing
      ],
      weightfn =
        OptionValue["LinearReductionWeight"]
    },
    
    If[
      cases === {},
      Return @ Missing[]
    ];
    
    MinimalBy[
      cases,
      weightfn[ (#[[2]])["Numerator"], (#[[2]])["Denominator"] ]&,
      1
    ][[1]]
  ];

RationalWeight[ num_, denom_ ] :=
  Which[
    MonomialQ[ num ] && MonomialQ[ denom ],
      { 0, LeafCount @ Cancel[num/denom] },
    MonomialQ[ denom ],
      { 1, LeafCount @ Cancel[num/denom] },
    True,
      { 2, LeafCount @ denom }
  ];


PackageExport["ReduceByLinearity"]

ReduceByLinearity::usage =
  "Reduces polList by using polynomials in polList that are linear in one of the variables labeled by s.";

Options[ReduceByLinearity] =
  Join[
    {
      "SimplifyIntermediateResultsBy" -> Identity,
      "Parallel" -> False
    },
    Options[SimplestLinearRule]
  ];

ReduceByLinearity[ polList_List, s_, opts:OptionsPattern[] ] :=
  Module[
    {
      ToPol, vars, RCF, InvalidPolSystem, UpdateSystem, RecursiveReduce, time, result, procID, id, RatRule,
      ReduceSystem, AddNonZeroPols, simplify, rootReduce, MonQ, PolRest, parallelQ, map
    },
    simplify =
      OptionValue["SimplifyIntermediateResultsBy"];
    parallelQ =
      OptionValue["Parallel"];
    procID =
      ToString @ Unique[];
    id :=
      ToString @ Unique[];
    vars =
      GetVariables[ polList, s ];
    RCF =
      RemoveCommonFactors[ #, s ]&;

    If[
      parallelQ
      ,
      Quiet[LaunchKernels[]];
      map =
        ParallelMap
      ,
      map =
        Map
    ];

    rootReduce =
      If[
        MemberQ[ polList, _Root, Infinity ],
        SafeRootReduce,
        Identity
      ];

    (* We need to store the numerator and denominator of rational functions separately because
       because Mathematica automatically cancels common factors.
       RatRule converts the target of the linear rule to an actual rational function. *)
    RatRule[ a_ -> b_Association ] :=
      a -> Cancel[ b["Numerator"] / b["Denominator"] ];
    RatRule[ Missing[] ] :=
      Missing[];

    ToPol =
      RCF @* rootReduce @* simplify @* Expand @* Numerator @* Cancel @* Together;

    MonQ[ pol_ ] :=
      Length[ MonomialList[ pol ] ] === 1;

    PolRest[ pol_, ps_ ] :=
      RCF @ PolynomialReduce[ pol, ps, vars ][[2]];

    (* Make sure all Kernels know the definitions of the functions to be mapped *)
    If[ parallelQ, DistributeDefinitions[ RCF, simplify, ToPol, PolRest, GetVariables ] ];

    (* It is assumed that none of the variables are 0 *)
    InvalidPolSystem[ pols_, nonZeroPols_, rules_  ] :=
      With[
        {
          polProblem =
            MemberQ[ pols, p_ /; MonQ[p] || MemberQ[p] @ nonZeroPols ],
          nonZeroPolProblem =
            MemberQ[0] @ nonZeroPols,
          ruleProblem =
            MemberQ[0] @ rules[[;;, 2]]
        },
        Which[
          polProblem,
            printlog["RBL:pol_problem", { id , pols } ];
            True,
          nonZeroPolProblem,
            printlog["RBL:non_zero_pol_problem"];
            True,
          ruleProblem,
            printlog["RBL:rule_problem", { id, rules } ];
            True,
          (* ELSE: no problem *)
          True,
            False
        ]
      ];

    UpdateSystem[ pols_, nonZeroPols_, knownRules_, rule_ ] :=
      {
        DeleteDuplicates @ DeleteCases[0] @ map[ ToPol, pols /. rule ],
        nonZeroPols /. rule,
        Append[rule] @ Expand[ knownRules/.rule ]
      };

    ReduceSystem[ pols_, nonZeroPols_, rules_, pol_ ] :=
      {
        map[ RCF[PolRest[ #, pol ]]&, pols ],
        nonZeroPols,
        rules
      };

    (* Add denominators appearing in the rules to the set of nonzero pols *)
    AddNonZeroPols[ pols_, nonZeroPols_, rules_ ] :=
      With[
        {
          denominators =
            Denominator @* Cancel @* Together /@
            rules[[;;,2]]
        },
        <|
          "Polynomials" ->  DeleteDuplicates @ Expand @ pols,
          "Assumptions"-> LogicalExpand @
            Reduce[ Thread[ Join[ nonZeroPols, denominators ] != 0 ], Backsubstitution -> True ],
          "Rules" -> SortBy[First] @ rules
        |>
      ];

    RecursiveReduce[
      ps_,    (* Polynomials:                        *)
      nzps_,  (* Non-zero polynomials: initially { } *)
      rs_     (* Substitution rules:   initially { } *)
    ] :=
      If[
        InvalidPolSystem[ ps, nzps, rs ]
        ,
        Return @ Null
        ,
        With[
          { lRule = AddOptions[opts][SimplestLinearRule][ ps, s ] },
          { denom = If[ MissingQ @ lRule, Missing[], RCF @ lRule[[2]]["Denominator"] ] },

          printlog["RBL:simplest_rule", { id, lRule, s } ];

          If[ (* No linear rules in system *)
            MissingQ @ lRule
            ,
            printlog["RBL:no_rules_left", { id } ];
            Sow[ { ps, nzps, rs } ];
            Return @ Null
          ];

          If[ (* Denominator in rule isn't zero *)
            MonQ @  PolRest[ denom, nzps ]
            ,
            printlog[ "RBL:nonzero_denominator", { id, lRule } ];
            RecursiveReduce @@
            UpdateSystem[ ps, nzps, rs, RatRule @ lRule ];
            Return @ Null
          ];

          (* Denominator in rule contains sum. *)

          (* ASSUME DENOMINATOR != 0 *)
          printlog[ "RBL:assuming_nonzero_denominator", { id, lRule } ];
          RecursiveReduce @@
          UpdateSystem[ ps, Append[ denom ] @  nzps, rs, RatRule @ lRule ];

          (* ASSUME DENOMINATOR == 0 *)
          printlog[ "RBL:reduction", { id, denom, s } ];
          RecursiveReduce @@ ReduceSystem[ ps, nzps, rs, denom ]
        ]
      ];

    printlog["RBL:init", { procID, polList, s, { opts } } ];

    { time, result } =
      AbsoluteTiming[
        AddNonZeroPols @@@
        DeleteCases[
          Reap[ RecursiveReduce[ RCF /@ polList, { }, { } ] ][[2]] /. {{x__}} :> {x},
          { { 1 }, { }, { } }
        ]
      ];

    printlog["Gen:results", { procID, result, time } ];

    If[ parallelQ, CloseKernels[] ];

    result

  ];


FindLinearRule[ pol_, s_ ] :=
  Module[{ nonLinVars, linVars, var },
    nonLinVars =
      Cases[ { pol }, Power[ s[i__], _ ] :> s[i], { 1, 5 } ] // DeleteDuplicates;

    linVars =
      Tally @
      Cases[
        pol,
        s[i__] /; FreeQ[s[i]] @ nonLinVars,
        Infinity
      ];

    If[
      linVars === {},
      Return @ Missing[],
      var =
        MinimalBy[ linVars, Last, 1 ][[1,1]];
      Collect[ pol, var ] /. a_. * var + b_ : 0 :> var -> <|  "Denominator" -> a, "Numerator" -> -b |>
    ]
  ];

RemoveCommonFactors[ poly_, s_ ] :=
  Module[ { vars, ce, commonVarNum, minExponents, commonFactor },
    vars =
      GetVariables[ poly, s ];
    ce =
      CoefficientRules[ poly, vars ][[;; , 1]];
    commonVarNum =
      Flatten @ Position[ Times @@ ce, x_ /; x > 0 ];
    minExponents =
      Min /@ Transpose[ce][[commonVarNum]];
    commonFactor =
      Inner[ Power, vars[[commonVarNum]], minExponents, Times ];
    Cancel[ poly / commonFactor ]
  ];

(*

  EXPERIMENTAL FUNCTION BELOW

*)

PackageExport["ReduceByLinearity2"]

ReduceByLinearity2::usage =
  "For testing.";

Options[ SimplestPol ] =
  { "WeightFunction" -> PolWeight };

Options[ReduceByLinearity2] =
  Join[
    {
      "SimplifyIntermediateResultsBy" -> Identity,
      "MaxMemory" -> None,
      "Parallel" -> False
    },
    Options[SimplestLinearRule]
  ];

ReduceByLinearity2[ polList_List, s_, opts:OptionsPattern[] ] :=
Module[
  {
    vars, RCF, InvalidPolSystem, UpdateSystem, RecursiveReduce, time, result, procID, id,
    ReduceSystem, simplify, rootReduce, maxMemory, MonQ, PolRest,
    MoldPol, ToAssociation, PolMod, map, parallelQ
  },
  simplify =
    OptionValue["SimplifyIntermediateResultsBy"];
  maxMemory =
    OptionValue["MaxMemory"];
  parallelQ =
    OptionValue["Parallel"];

  If[
    parallelQ
    ,
    Quiet[LaunchKernels[]];
    map = ParallelMap
    ,
    map = Map
  ];

  procID =
    ToString @ Unique[];
  id :=
    ToString @ Unique[];
  vars =
    GetVariables[ polList, s ];
  RCF =
    RemoveCommonFactors[ #, s ]&;
  rootReduce =
    If[
      MemberQ[ polList, _Root, Infinity ],
      SafeRootReduce,
      Identity
    ];
  MoldPol =
    RCF @* rootReduce @* simplify @* Expand;
  MonQ[ pol_ ] :=
    Length[ MonomialList[ pol ] ] === 1;
  PolRest[ pol_, ps_ ] :=
    PolynomialReduce[ pol, ps, vars ][[2]];

  PolMod :=
    Function[
      { p1, p2, var },
      With[{ gb = GroebnerBasis[ { p1, p2 }, GetVariables[ { p1, p2 }, s ], { var }  ] },
        If[ gb === {}, 0, MoldPol @ gb[[1]] ]
      ]
    ];

  If[ parallelQ, DistributeDefinitions[ GetVariables, MoldPol, PolMod ] ];

  (* It is assumed that none of the variables are 0 *)
  InvalidPolSystem[ pols_, nonZeroPols_, linPols_  ] :=
  With[
    {
      polProblem =
        MemberQ[ pols, p_ /; MonQ[p] || MemberQ[p] @ nonZeroPols ],
      nonZeroPolProblem =
        MemberQ[0] @ nonZeroPols
    },
    Which[
      polProblem,
        printlog["RBL:pol_problem", { id , pols } ];
        True,
      nonZeroPolProblem,
        printlog["RBL:non_zero_pol_problem"];
        True,
      (* ELSE: no problem *)
      True,
        False
    ]
  ];

  UpdateSystem[ pols_, nonZeroPols_, linPols_, linPol_ ] :=
    With[{ pol = linPol["Var"] * linPol["Factor"] + linPol["Term"] },
      MemoryConstrained[
        {
          DeleteDuplicates @
          DeleteCases[0] @
          map[
            PolMod[ #, pol, linPol["Var"] ]&,
            pols
          ],
          Map[
            PolMod[ #, pol, linPol["Var"] ]&,
            nonZeroPols
          ],
          Append[ linPol ] @ linPols
        },
        maxMemory,
        (* When maxMemory is exceeded during calculation,
           sow non-updated system and stop further operations on this branch *)
        printlog["RBL:memory_overflow", { procID } ];
        Sow[ { pols, nonZeroPols, linPols } ];
        { { 1 }, { }, { } }
      ]
    ];

  ReduceSystem[ pols_, nonZeroPols_, linPols_, denom_ ] :=
    MemoryConstrained[
      {
        map[ MoldPol[ PolRest[ #, denom ] ]&, pols ],
        nonZeroPols,
        linPols
      },
      maxMemory,
      printlog["RBL:memory_overflow", { procID }];
      Sow[ { pols, nonZeroPols, linPols } ];
      { { 1 }, { }, { } }
    ];

  (* Add denominators appearing in the rules to the set of nonzero polList *)
  ToAssociation[ pols_, nonZeroPols_, linPols_ ] :=
    <|
      "Polynomials" -> DeleteDuplicates @ Map[ MoldPol, pols ],
      "Assumptions"-> LogicalExpand @
        Reduce[ Thread[  nonZeroPols != 0 ], Backsubstitution -> True ],
      "LinearPolynomials" -> linPols
    |>;

  RecursiveReduce[
    ps_,    (* Polynomials:          initially RCF /@ pols *)
    nzps_,  (* Non-zero polynomials: initially { } *)
    lps_    (* Linear polynomials:   initially { } *)
  ] :=
    If[
      InvalidPolSystem[ ps, nzps, lps ],
      (* THEN *)
      Return @ Null,
      (* ELSE *)

      With[ { linPol = AddOptions[opts][SimplestPol][ ps, s ] },

        If[ (* No linear rules in system *)
          MissingQ @ linPol,
          (* THEN *)
          Sow[ { ps, nzps, lps } ];
          Return @ Null
        ];

        If[ (* Denominator in rule isn't zero *)
          MonQ @  PolRest[ linPol["Factor"], nzps ],
          (* THEN *)
          RecursiveReduce @@
          UpdateSystem[ ps, nzps, lps, linPol ];
          Return @ Null
        ];

        (* ASSUME DENOMINATOR != 0 *)
        RecursiveReduce @@
        UpdateSystem[ ps, Append[ linPol["Factor"] ] @  nzps, lps, linPol ];

        (* ASSUME DENOMINATOR == 0 *)
        RecursiveReduce @@
        ReduceSystem[ ps, nzps, lps, RCF @ linPol["Factor"] ]
      ]
    ];


  printlog["RBL:init", { procID, polList, s, { opts } } ];

  { time, result } =
    AbsoluteTiming[
      ToAssociation @@@
      DeleteCases[
        Reap[ RecursiveReduce[ RCF /@ polList, { }, { } ] ][[2]] /. {{x__}} :> {x},
        { { 1 }, { }, { } }
      ]
    ];

  printlog["Gen:results", { procID, result, time } ];

  If[ parallelQ, CloseKernels[ ] ];

  result

];

SimplestPol[ pols_, s_, opts:OptionsPattern[] ] :=
  With[
    {
      polsInfo =
        Select[
          LinearVarInfo[ s ] /@ pols,
          Not @* MissingQ
        ],
      weightfn =
        OptionValue["WeightFunction"]
    },

    If[
      polsInfo === {},
      Return @ Missing[]
    ];

    MinimalBy[
      polsInfo,
      weightfn @@ { #["Factor"], #["Term"] }&,
      1
    ][[1]]
  ];

PolWeight[ f_, t_ ] :=
  If[
    Length[ MonomialList[f] ] == 1,
    0,
    LeafCount[ { f, t } ]
  ];

LinearVarInfo[ s_ ][ pol_ ] :=
  Module[{ nonLinVars, linVars, var },
    nonLinVars =
      Cases[ { pol }, Power[ s[i__], _ ] :> s[i], { 1, 5 } ] //
      DeleteDuplicates;

    linVars =
      Tally @
      Cases[
        pol,
        s[i__] /; FreeQ[s[i]] @ nonLinVars,
        Infinity
      ];

    If[
      linVars === {},
      Return @ Missing[],
      var =
        MinimalBy[ linVars, Last, 1 ][[1,1]];
      ReplaceAll[
        Collect[ pol, var ],
        a_. * var + b_ : 0 :> <| "Var" -> var, "Factor" -> a, "Term" -> b |>
      ]
    ]
  ];