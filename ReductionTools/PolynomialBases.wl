Options[ PolynomialBases ] = Join[ Options[ SolvePolynomialSystem ], Options[GroebnerBasis ] ];
PolynomialBases[ eqns_List, vars_List, symbol_, opts:OptionsPattern[] ] := Module[{
  sym               = OptionValue["Symmetries"],
  polConstraints    = OptionValue["PolynomialConstraints"],
  invertibleMats    = OptionValue["InvertibleMatrices"],
  nonSingularQ      = OptionValue["NonSingular"],
  monomialOrder     = OptionValue[MonomialOrder],
  coefficientDomain = OptionValue[CoefficientDomain],
  method            = OptionValue[Method],
  modulus           = OptionValue[Modulus],
  s = Unique["x"],
  newEquations, newSymmetries, newPolConstraints, newInvertibleMats, newVars, revertVars,
  monomialEquations, sumEquations, sumEqnSolnPairs, solToGroebner, groebnerBases
  },

  { { newEquations, newSymmetries, newPolConstraints, newInvertibleMats }, newVars, revertVars } =
    SimplifyVariables[ { eqns, sym, polConstraints, invertibleMats }, vars, s ];

  { monomialEquations, sumEquations } =
    (
      DeleteCases[True] /@
      GroupBy[ newEquations, MonomialEquationQ ] /@
      { True, False }
    )/.Missing[__] -> {};

  sumEqnSolnPairs = Transpose @
    ReduceByMonomials[ sumEquations, monomialEquations , newVars, symbol,
      "Symmetries" -> newSymmetries,
      "PolynomialConstraints" -> newPolConstraints,
      "InvertibleMatrices" -> newInvertibleMats,
      "NonSingular" -> nonSingularQ
    ];

  solToGroebner[ monOrder_, coeffDomain_, meth_, mod_ ][ newSumEqns_, sol_ ] :=
    If[
      newSumEqns === {},
      {
        {},
        sol/.Dispatch[revertVars]
      },
      {
        GroebnerBasis[
          newSumEqns,
          GetVars[ newSumEqns , symbol ],
          MonomialOrder -> monOrder,
          CoefficientDomain -> coeffDomain,
          Method -> method,
          Modulus -> mod
        ],
        sol/.Dispatch[revertVars]
      }
    ];

  PrintTemporary["Constructing Groebner bases"];
  groebnerBases =
    MapThread[
      solToGroebner[ monomialOrder, coefficientDomain, method, modulus ],
      sumEqnSolnPairs
    ];

  Remove[s];

  groebnerBases
];
