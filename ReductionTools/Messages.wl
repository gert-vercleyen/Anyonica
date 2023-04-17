(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)

(*
+---------------------------------------------------------------------------+
|                                                                           |
|                                PentaTools                                 |
|                                                                           |
+---------------------------------------------------------------------------+
*)
(*
+---------------------------------------------------------------------------+
|                              Usage Messages                               |
+---------------------------------------------------------------------------+
*)
PentaTools::usage =
"Package that provides various functions and routines for dealing \
with polynomial equations, in particular pentagon equations";
GetVars::usage =
"GetVars[expr,s] returns a sorted list of the variables s[__] \
apparent in expr. GetVars[expr,s,excludedVars] returns a sorted list \
of the variables s[__] in expr, excluding those given by the list of \
excludedVars. Options include \"LevelSpec\" -> n to search only for \
variables up to level n.";
PrintLog::usage =
"PrintLog[\"Directory\", code_ ] prints information of intermediate \
results during evaluation of code.";
LeastVarsExpr::usage =
"LeastVarsExpr[exprList,s] returns the expression containing the \
least amount of different s variables. \
LeastVarsExpr[exprList,s,excludedVars] returns the expression from \
exprList with the least amount of s variables, not counting the \
variables in excludedVars. Options include \"LevelSpec\" -> n to take \
only variables up to level n into account";
TowerOfExpressions::usage =
"TowerOfExpressions[exprList,s] returns a list of couples \
containing variables and expressions in those variables such that the \
first couple contains polynomials with the least amount of unknowns, \
the second couple contains polynomials with the least amount of \
unknowns under the assumption that the unknowns of the first couple \
are known etc. Options include \"Symbol\" -> x to assume the \
variables are of the form x[__], and \"LevelSpec\" -> n to take only \
for variables up to level n into account.";
ReplaceByReps::usage =
"ReplaceByReps[equivClasses, representatives, expr]  returns expr \
with all occurences e_i in expr replaced by their representatives \
according to the equivalence class that e_i belongs to. \
ReplaceByReps[equivClasses, representatives] is the operator form of \
ReplaceByReps";
SimplifyVariables::usage =
"SimplifyVariables[eqns,oldvars,s] replaces all variables in eqns, \
oldVars, by a single indexed variable in the symbol s and and returns \
triple of new eqns, new vars, and replacement rules to revert to old \
variables.";
BinomialEquationQ::usage =
"BinomialEquationQ[eqn] returns true if the equation is a Binomial \
equation.";
ToProperBinomialEquation::usage =
"ToProperBinomialEquation[moneqn] returns an equivalent Binomial \
equation to eqn of the form LHS == RHS where both RHS and LHS are \
non-zero.";
MonomialQ::usage =
"MonomialQ[pol] returns true if the pol is a monomial.";
MonPolEqnsToMatSys::usage =
"MonPolEqnsToMatrixSystem[ eqns, nvars, s ] returns a couple of a \
matrix m and vector v of the logarithm of the system of linear \
equations eqns in nvars variables named s[1],...,s[nvars], modulo 2 \
Pi I.";
SolveModZSpace::usage =
"SolveModZSpace[mat,vec] returns a list { v, mZ, mC } where v is a \
vector and mZ, mC are matrices such that mat.(v + mZ.intVec + mC \
+contVec) == vec modulo integer vectors. Here intvec and contVec are \
vectors containing integer parameters and continuous parameters.";
SolveModZ::usgae =
"SolveModZ[mat,vec] returns a list of lists { v0s, mC } where v0s \
is a list of constant vectors such that every solution, mod Z, to the \
matrix system, mod Z, can be written as one of the vectors in v0s \
plus a complex linear combination of column vectors of mC.";
SolveBinomialSystem::usage =
"SolveBinomialEquations[eqnlist,s] solves the Binomial equations in \
eqnlist and returns a solution in terms of the free parameters \
s[i].";
IntOrthogonalSpace::usage =
"IntOrthogonalSpace[ mat ] returns an integer matrix whose column \
space is orthogonal to that of the integer matrix mat. \
IntOrthogonalSpace[ mat1, mat2 ] returns an integer matrix whose \
column space is a subspace of mat1 and orthogonal to mat2.";
BackTrackCCode::usage =
"BacktrackCCode[ t, r ] takes a tower t of couples of vars and eqns \
and produces C code to solve the equations with vars in range r";
SolveDiophantineEquations::usage =
"SolveDiophantineEquations[ eqns, vars, r ] solves the system of equations \
eqns in the variables vars in the range r using external C code";
RandomPolynomial::usage =
"RandomPolynomial[ symbol, maxPower, maxTerms, nVars, {cMin,cMax} ] \
returns a random polynomial in nVars variables with name \
symbol[1],...,symbol[nVars], with maximum maxTerms terms of degree at \
most maxDegree, and with integer coefficients in the range \
{cMin,cMax}.";
(* TODO: complete description for function with fewer arguments *)
SimplifyUsingRoots::usage =
"SimplifyUsingRoots[ expr_, acc_ ] attempts to simplify expr by approximating (with accuracy acc and infinite precision) all non-integers in expr and then repalcing them with exact roots of polynomials.";
SearchSpaceSize::usage =
"SearchSpaceSize[ ranges_ ] takes a list of ranges of the form { var1 -> { min1, max1 }, ... } and returns a function that takes a list of variables the form {vari,...} and returns the size of the search space corresponding to those variables.";
AddOptions::usage =
"AddOptions[ opts ][ func ][ args ] returns func[ args, filteredOptions ], where filteredOptions is the list of options of opts that are valid for func.";
PolynomialDegree::usage =
"PolynomialDegree[ pol, vars ] returns the degree of the polynomial with all vars replaced by a single variable. PolynomialDegree[ pol, s ] equals PolynomialDegree[ pol, GetVars[ pol, s ] ].";

SolvePolynomialSystem::usage =
"SolvePolynomialSystem[ eqnList, varList, symbol, options ] returns the solutions to the system of polynomial equations eqns in the variables vars. If there are continuous degrees of freedom in the solutions, these will be parametrized by an index variable: symbol[i]. Options are \"Symmetries\" -> None, \"NonSingular\" -> False, \"PolynomialConstraints\" -> {}, \"InvertibleMatrices\" -> {}, \"ZeroValues\" -> None, \"FindZerosUsingSums\" -> False, \"SimplifyIntermediateResultsBy\" -> Identity, \"PreEqualCheck\" -> Identity";
RestrictMultiplicativeSymmetries::usage =
"RestrictMultiplicativeSymmetries[ symmetries, vars, symbol, options ] returns the symmetries that are left after demanding that the variables vars are constant. The degrees of freedom of the new symmetries are parametrized by indexed variables: symbol[i]. The options are the same as those for the function SolveModZ and will be passed to an internal call to SolveModZ.";
DeleteSymmetryEquivalentSolutions::usage =
"DeleteSymmetryEquivalentSolutions[ symmetries, options ][ solList ] removes all redundant solutions from solList. Here redundant means that there exists another solution in solList which equivalent via a combination of a (possibly trivial) gauge transform and (possibly trivial) automorphism of the fusion ring. Possible options are (a) \"SimplifyBy\": function to simplify expressions whose values are checked, (b) \"Numeric\": Set to True to let the function check equality nummerically, (c) \"Accuracy\": Set the accuracy to use when checking values numerically (precision is always infinite).";
BinomialEquationQ::usage =
"BinomialEquationQ[ eqn ] returns True if eqn is of the form lhs == rhs, where lhs and rhs are monomials.";
BinomialSystemQ::usage =
"BinomialSystem[ eqnList ] returns True if eqnList is a list of binomial equations.";
ReduceByBinomials::usage =
"To be documented";
PolynomialBases::usage =
"To be documented";
FindZeroValues::usage =
"To be documented";
GaugeSymmetryEquivalentQ::usage =
"To be documented";
SymmetryEquivalentQ::usage =
"To be documented";
BreakMultiplicativeSymmetry::usage =
"To be documented";
AddZerosToSymmetries::usage =
"To be documented";
PentagonEquations::usage =
"PentagonEquations[ ring ] returns the pentagon equations related to the Fusion Ring ring where all trivial and duplicate equations have been removed.";
InverseFSymbols::usage =
"InverseFSymbols[ ring, symbol, FSymbols ] returns a list of the inverse F-symbols of the FSymbols of the Fusion Ring ring (with values). They are of the form symbol[...] -> val.";
FMatrices::usage =
"FMatrices[ ring_ ] returns a list of well defined F-matrices (without value) of the Fusion Ring ring.";
FSymbols::usage =
"FSymbols[ ring ] returns a list of well-defined F-symbols (without values) of the Fusion Ring ring.";
SolvePentagonEquations::usage =
"SolvePentagonEquations[ ring, options ] returns the solutions for the pentagon equations associated to the Fusion Ring ring. Options are \"FindZerosUsingSums\" -> True, \"GaugeDemands\" -> None,  \"DeleteGaugeEquivalentSolutions\" -> False,  \"ZeroValues\" -> None,  \"NonSingular\" -> False, \"SimplifyIntermediateResultsBy\" -> Identity, \"IntegerCheck\" -> IntegerQ, \"PreEqualCheck\" -> Identity";
UnitaryGaugeDemands::usage =
"UnitaryGaugeDemands[ring] returns the demands on values on the F-symbols that are needed when expressed in a unitary gauge.";
NIntegerQ::usage =
"NIntegerQ[x,acc] numerically checks whether x is an integer with accuracy acc and infinite precision. NIntegerQ[x] is shorthand for NIntegerQ[x,$MachinePrecision]";
ToUnitaryGauge::usage =
"ToUnitaryGauge[ ring, FSymbols, options ] attempts to find a gauge in which the F-matrices are unitary. Here ring is the fusion ring and FSymbols is a solution to the pentagon equations. Options are (a) \"Numeric\": set to true to use numerical recipes. The standard accuracy and tolerance are 64, (b) \"Tolerance\" to set the Tolerance used for the internal call to UnitaryMatrixQ (see the documentation centre for more info), (c) \"SimplifyBy\": a function to simplify intermediate expressions during computation and during the checks for unitarity, (d) \"Use2DConstraints\": set to False if you don't want to use explicit optimization via 2D constraints, (e) all options from SolveSemiExponentiatedSystem which are used for the internal call to this function.";
ToRealGauge::usage =
"ToRealGauge[ ring, FSymbols, options ] attempts to find a gauge in which the F-matrices are real. Here ring is the fusion ring and FSymbols is a solution to the pentagon equations. Options are (a) \"Numeric\": set to true to use numerical recipes. The standard accuracy is 64, (b) \"SimplifyBy\": a function to simplify intermediate expressions during computation and during the checks for unitarity";
GaugeTransform::usage =
"GaugeTransform[g][ s[ ind ] ] applies a gauge transformation with parameters labeled by g to the symbols s with indices ind. Possible values of s are: \[ScriptCapitalF], \[ScriptCapitalR] (not implemented yet), etc.";
UnitaryGaugeQ::usage =
"UnitaryGaugeQ[ ring, FSymbols, options ] returns true if the F-symbols FSymbols are in a gauge such that the F-matrices are unitary. Here ring is the fusion ring for which the F-symbols are a solution to the pentagon equations. Options are (a) \"Numeric\": set to true to do a numerical check with a standard tolerance of 64, (b) \"Tolerance\" to set the Tolerance used for the internal call to UnitaryMatrixQ (see the documentation centre for more info), (c) \"SimplifyBy\": a function applied to symbolic matrices during the checks for unitarity. This option only works if the \"Numeric\" option is False";
WithDimension::usage =
"WithDimension[ matList, { min, max, step } ] returns all (m x n) matrices in mathList with m an element of Range[ min, max, step ].\nWithDimension[ matList_, { min, max } ] returns all (m x n) matrices with min <= m <= max.\nWithDimension[ matList_, { k_Integer } ] returns al (m x n) matrices with m == k. Set \"ColumnDimensions\" -> True to select matrices based on the number of columns.";
RSymbols::usage =
"RSymbols[ ring ] returns a list of the well-defined R-symbols (without values) of the Fusion Ring ring.";
RTensors::usage =
"RTensors[ ring ] returns an association that maps well defined {a,b,c} to the matrix form of R[a,b,c].";
HexagonEquationsWithMultiplicity::usage =
"For testing";
FixUnitaryRSymbols::usage =
"For testing";
FixVacuumFSymbols::usage =
"For testing";
FTensors::usage =
"FTensors[ ring ] returns an association that maps four-tuples {a,b,c,d} to the matrix form of F[a,b,c,d]. Here the triples of upper-and lower indices have been flattened to single indices.";
MatrixDirectSum::usage =
"MatrixDirectSum[ listOfMatrices ] returns a sparse matrix corresponding to the direct sum of the matrices in listOfMatrices.";
ThreadMatrixEquality::usage =
"ThreadMatrixEquality[ mat1 == mat2 ] returns a list of equations between the individual matrix elements. It is optimized for use with sparse matrices.";
FirstOrderReduce::usage =
"FirstOrderReduce[system,s] returns a couple { newSystem, rules } where newSystem contains no first order terms, and rules is a list of rules that expresses old variables s[i__] (previsouly apparent in the system) in terms of current variables.";
PentagonEquationsWithMultiplicity::usage =
"For testing";
ReduceTrivialities::usage =
"TBD";
SolveSemiExponentiatedSystem::usage =
"For testing";
MemoizedSmithDecomposition::usage =
  "MemoizedSmithDecomposition[ mat ] checks whether the SmithDecomposition of mat is stored in a database and if so, loads it. If it is not stored in the database it calculates it. Set the option \"StoreDecompositions\" -> True to automatically store the decomposition when it was not yet available.";
ToSymmetricGauge::usage =
"TBD";
SymmetricGaugeQ::usage =
"TBD";
SolveHexagonEquations::usage =
  "SolveHexagonEquations[ ring_ ] solves the hexagon equations for the Fusion Ring ring. The option \"Knowns\" can be set to a list of rules of variables that are already known, e.g. a solution to the pentagon equations.";
ApplyGaugeTransform::usage =
  "ApplyGaugeTransform[ solution, symbol ] applies a formal gauge transformation, with gauge variables labeled by symbol, to all the values of the solution. ApplyGaugeTransform[ solution, gaugeVals, symbol ] applies the actual gauge transformation with values decided by gaugeVals to solution.";
WhichGaugeTransform::usage =
"WhichGaugeTransform[ ring, sol1, sol2, symbol ] returns a gauge transform that transforms sol1 into sol2, where sol1 and sol2 are solutions to the pentagon and/or hexagon equations for the Fusion Ring ring. Here g is the variable that will be used as a gauge variable.";
HexagonEquations::usage =
"HexagonEquations[ fusionRing ] returns the hexagon equations related to the fusion ring fusionRing. The \"Knowns\" can be set to a list of rules of variables that are already known, e.g. a solution to the pentagon equations.";
ZSCriterion::usage =
"ZSCriterion[ fusionRing ] returns True if the fusion ring fusionRing cannot be categorified due to the Zero Spectrum criterion. See arXiv:2203.06522v1 for more info.";
CSPCValue::usage =
"CSPCValue[ acc ][ fusionRing ] returns a number that, if lower than 0, indicates that the ring fusionRing does not have a unitary categorification. Here acc stands for the accuracy used to approximate the fusion ring characters. See arXiv:1910.12059v5 for more info.";
GaugeSymmetries::usage =
"GaugeSymmetries[ symbols, symbol ] returns the gauge transforms of the symbols, with gauge variables indexed by symbol.";
PentagonGroebnerSystems::usage =
"PentagonGroebnerSystems[ fusionRing, var ] calculates a Groebner basis for the pentagon equations of the ring fusionRing in the variable var. Options include all options for solving the pentagon equations and all options for finding groebner bases.";
ToPolynomial::usage =
"ToPolynomial[ equation ] converts a polynomial equation to a polynomial.";
ReduceByLinearity::usage =
"TBD";
HexagonGroebnerSystems::usage =
"HexagonGroebnerSystems[ fusionRing, var ] calculates a Groebner basis for the hexagon equations of the ring fusionRing in the variable var. Options include all options for solving the hexagon equations and all options for finding groebner bases.";
FastTowerOfExpressions::usage =
"For testing";
$VacuumFPattern::usage =
"General pattern of vacuum F-symbols";
$VacuumRPattern::usage =
"General pattern of vacuum R-symbols";
IndependentSubsystems::usage =
"IndependentSubsystems[ eqns, symbol ] returns lists of subsystems in variables s[i__] that are independent.\n IndependentSubsystems[ eqns, symbol, vars ] returns lists of subsystems in the variables vars.";
SolveUsingReduce::usage =
"Solves system of eqns using Reduce";
PowerSumReduce::usage =
"Simplifies Root expressions of power sum polynomials to their polar form.";
ParallelGroebnerBasis::usage =
"Calculates Groebner bases with different permutations of the variables on different kernels and returns the first result found.";
IncrementalGroebnerBasis::usage =
"Calculates Groebner by incrementally taking subsets of original system.";
PentagonTower::usage=
"Calculates a tower of pentagon equations based on the dimensions of the F-matrices.";
ReduceByLinearity2::usage =
"For testing.";
ToNumericRootIsolation::usage =
"Converts Root expressions with symbolic root isolation to those with numeric root isolation.";
(*
+---------------------------------------------------------------------------+
|                              Error Messages                               |
+---------------------------------------------------------------------------+
*)

GetVars::invalidlevelspec =
"`1` must be an integer or Infinity";
PrintLog::cantcreatedirectory =
"Directory `1` could not be found and could not be created.";
LeastVarsExpr::wrongformat =
"Argument `1` must be a List or Association of expressions.";
LeastVarsExpr::invalidlevelspec =
"`1` must be an integer or Infinity";
LeastVarsExpr::novarsinexpressions =
"Warning: `1` contains no variables in the symbol `2`";
LeastVarsExpr::invalidlevelspec =
"`1` must be an integer or Infinity";
TowerOfExpressions::wrongformat =
"Argument `1` must be a list of expressions.";
TowerOfExpressions::notasymbol =
"Argument `1` is not a symbol";
ReplaceByReps::repnotinclass =
"One or more classes in `1` does not contain an element from `2` at \
the same position as the class.";
SolveMonomialEquations::nonmoneqns =
"`1` is not a system of Monomial polynomial equations.";
SolveModZSpace::nonzerocoeff =
"The vector `1` contains non-zero entries at position(s) greater \
than `2`. This could be caused by non-simplified expressions that \
should be 0 or by the system being unsolvable.";
SolveDiophantineEquations::compilationerror =
"Error during compilation.";
SolveDiophantineEquations::executionerror =
"Error during execution.";
SolveDiophantineEquations::wrongfilenameformat =
"`1` must be a triple of strings.";
SolveNonSingularBinomialsystem::nonmoneqns =
"`1` is not a system of Binomial polynomial equations.";
SolveNonSingularBinomialSystem::notimplementedyet =
"Only symmetries that multiply variables by numbers are implemented \
at the moment.";
SolveNonSingularBinomialSystem::wrongsymmetriesformat =
"The list of variables appearing in the symmetries should be the \
same list as the list of given variables, i.e. `1` should be `2`";
BreakMultiplicativeSymmetry::clashingdemands =
"\"Demands\" `1` are not allowed to assign values to variables in \
\"Exclusions\" `2`.";
ValidSolutionQ::unresolvedequations =
"Validity could not be verified for the following equations: `1`. \nAssuming they are correct. You're advised to check final solutions";
SolveNonSingularBinomialSystem::novars =
"Nonempty set of equations, `1`, with empty set of variables. Assuming no solutions to system. If equalities are unresolved but true, then adding the option \"PreEqualCheck\"-> f (where f is a function that simplifies expressions) can get rid of unresolved equalities.";
ToRealGauge::nonunitarygauge =
"The F-symbols need to be in a unitary gauge to start with.";
ThreadMatrixEquality::dimensionsmismatch =
"The dimensions of the two matrices do not coincide.";
SolveMultiplicityFreePentagonEquations::noisomorphicsubring =
"The ring `1` is not isomorphic to any of the subrings of `2`";
SolvePentagonEquations::substitutesolutionwrongformat =
"\"SubstituteSolution\" should point to a couple { ring, solution } where ring is a fusion ring isomorphic to a subring of `1` and solution is a solution to the pentagon equations for ring."
