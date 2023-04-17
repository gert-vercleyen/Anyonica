(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2022-03-16 *)

initialize::usage =
"initialize[\"x\",r] initializes the type of affine Lie algebra, of type \"x\" and rank r, \
subject to the constraints displayed when the package was loaded.\n\
initialize[\"x\",r,level] initializes the type of affine Lie algebra, its rank and level.\n\
initialize[\"x\",r,level,rootfactor] initializes the type of affine Lie algebra, its rank, the level \
and the rootfactor, which sets the root of unity.";

initializelz::usage =
"initialize[\"x\", r, l, z] initializes the type of affine Lie algebra, its rank, and the \
root of unity as q = e^(2 Pi I z / l).";

initializelevel::usage =
"initializelevel[k] intializes the level to k.";

setprecision::usage =
"setprecision[prec] sets the precision to prec. Should be run before \
setrootofunity[rootfactor]. The default value is prec=100, which should be \
sufficient for most cases. If mathematica complains about ill-conditioned \
matrices, or if the fusion rules are inconsistent, etc., one can try to \
increase the precision.";

initializerootofunity::usage =
"initializerootofunity[rootfactor] sets the value of q to \
e^(2 \[Pi] i rootfactor/(tmax (k+g))), \
where k is the level, g the dual coxeter number of the affine Lie algebra, and \
tmax = 1 for the simply laced cases, tmax = 3 for g_2 and tmax = 2 for the other \
non-simply laced cases (g and tmax are set when intialize[\"x\",r] is run).";

setrootofunity::usage =
"setrootofunity[rootfactor] sets the value of q to e^(2 \[Pi] i rootfactor/(tmax (k+g))), \
where k is the level, g the dual coxeter number of the affine Lie algebra, and \
tmax = 1 for the simply laced cases, tmax = 3 for g_2 and tmax = 2 for the other \
non-simply laced cases (g and tmax are set when intialize[\"x\",r] is run). Identical to \
initializerootofunity[rootfactor], but kept for backwards compatibility.";

possiblerootfactors::usage =
"possiblerootfactors[\"x\",rank,level] gives the possible rootfactors \
for the selected type, rank and level. A list containing the lists with the possible rootfactors \
for the uniform and non-uniform cases is returned. The list for non-uniform cases might be empty.";

calculatefsymbols::usage =
"calculatefsymbols[] calculates the F-symbols, after the type of algebra, rank, \
level and root of unity have been selected. The calculation is done in several steps. Most \
importantly, the q-CG coefficients are constructed first. The pentagon equations are \
verified, as well as some properties of the F-matrices.";

calculatersymbols::usage =
"calculatersymbols[] calculates the R-symbols, after the F-symbols have been \
calculated. The hexagon equations are verified, as well as some properties of \
the R-matrices.";

calculatemodulardata::usage =
"calculatemodulardata[] calculates the modular data, once the F- and R-symbols are \
calculated: pivotal structure, Frodenius-Schur indicators, Frobenius-Perron \
dimensions, quantum dimensions, scaling dimensions, central charge adn the S-matrix. \
calculatemodulardata[pivotalstructure] calculates the modular data, but with the selected \
pivotal structure (which has to be spherical) instead.";

diagonalizermatrices::usage =
"diagonalizermatrices[] diagonalizes the R-matrices, if they are not diagonal already. \
This will change both F- and R-symbols, so the pentagon and hexagon equations will be \
re-checked for security.";

undiagonalizermatrices::usage =
"undiagonalizermatrices[] reverts the process of diagonalizing the R-matrices. \
This will revert both F- and R-symbols back to their original values. This is necessary \
if one after diagonalizing the R-matrices, wants to obtain the exact representation of the
F-symbols. Diagonalizing the R-matrices can lead to values for the F-symbols that can not \
be described in terms of the general numberfield that is used to describe the F- and R-symbols.";

(*
donotcheckpentagon::usage =
	"Run donotcheckpentagon[] (after the type of algebra, rank and level have been initialized) \
if you do not want the pentagon equations to be checked. \
Though it is of course safer to check the pentagon equations, checking the pentagon equations \
can take a long time (though typically not as long as calculating the F-symbols in the first place). \
By running donotcheckpentagon[], the \
pentagon equations will not be checked. It is recommended to generate the R-symbols as well, so \
that at least the hexagon equations can be checked. If these hold, it is likely that the \
pentagon equations are also satisfied."
*)

(*
docheckpentagon::usage =
	"Run docheckpentagon[] (before calculating the F-symbols) if you ran donotcheckpentagon[] \
accidentally, but still want to check the pentagon equations."
*)

displayinfo::usage =
"displayinfo[] displays some general information and basic instructions on how \
to use the package.";

qCG::usage =
"qcg[j1,{m1,n1},j2,{m2,n2},j3,{m3,n3},v1] gives the q-CG coefficient. \
The j's (highest weights) and m's (weights) are list of length 'rank'. \
The n's are integers distinguishing the weights (due to weight multiplicities) and \
v1 is an integer labeling which j3 in the tensor product of j1 and j2 one is considering \
(due to fusion multiplicity).";

Fsym::usage =
"Fsym[a,b,c,d,e,f,{v1,v2,v3,v4}] gives the F-symbol. The a,b,...,f label the particles \
(lists of length 'rank'), while the v's (integers) label the four vertices.";

Fsymexact::usage =
"Fsymexact[a,b,c,d,e,f,{v1,v2,v3,v4}] gives the exact representation of the F-symbol. \
The exact form of the F-symbols is \
Exp[Pi I \[Alpha]] Sqrt[Sum[a[i] Cos[2 Pi z / l]^i, {i, 0, EulerPhi[l]/2 - 1}]], \
where \[Alpha] and a[i] are rational. Typically, \[Alpha] \[Element] {0,1/2,1,-1/2} \
(if one does not diagonalize the R-matrices). The exact F-symbols are given in terms of \
\[Alpha] and a[i] as {\[Alpha], {a[0], a[1], ... }}.";

Fmat::usage =
"Fmat[a,b,c,d] gives the F-matrix. a,b,c,d label the particles \
(lists of length 'rank').";

Rsym::usage =
"Rsym[a,b,c,{v1,v2}] gives the R-symbol. a,b,c label the particles \
(lists of length 'rank'), while the v's (integers) label the two vertices.";

Rsyminv::usage =
"Rsyminv[a,b,c,{v1,v2}] gives the inverse R-symbol, calculated by taking the \
approriate elements of the inverse of the R-matrix. a,b,c label the particles \
(lists of length 'rank'), while the v's (integers) label the two vertices.";

Rsymexact::usage =
"Rsymexact[a,b,c,{v1,v2}] gives the exact representation of the R-symbol. \
The exact form of the R-symbols is \
Exp[Pi I \[Alpha]] Sqrt[Sum[a[i] Cos[2 Pi z / l]^i, {i, 0, EulerPhi[l]/2 - 1}]], \
where \[Alpha] and a[i] are rational. \
For the R-symbols corresponding to diagonal R-matrices, the R-symbol is a pure phase, so that
the square root factor equals one. \
The exact R-symbols are given in terms of \[Alpha] and a[i] as {\[Alpha], {a[0], a[1], ... }}.";

Rsyminvexact::usage =
"Rsyminvexact[a,b,c,{v1,v2}] gives the exact representation of the inverse R-symbol. \
For the format, see the documentation in the accompanying notebook.";

Rmat::usage =
"Rmat[a,b,c,d] gives the R-matrix. a,b,c label the particles \
(lists of length 'rank').";

Nmat::usage =
"Nmat[a] gives the fusion matrix for fusion with particle type a.";

Nvertex::usage =
"Nvertex[a,b,c] gives the number of vertices of type (a,b,c).";

Fusion::usage =
"Fusion[a,b] gives the possible fusion outcomes of a x b, that is, without(!) \
taking fusion multiplicities into account. Use Nvertex[a,b,c] to obtain the number \
of vertices of type (a,b,c).";

possiblepivotalstructures::usage =
"possiblepivotalstructures[] calculates the solutions to the pivotal equations. \
Note that only one these solutions is actually realized by the selected quantum group!";

possiblesphericalpivotalstructures::usage =
"possiblesphericalpivotalstructures[] calculates the solutions to the pivotal equations, \
but only returns the spherical ones (i.e., all coefficients equal to either +1 or -1). \
Note that only one these solutions is actually realized by the selected quantum group!";

findexactfsymbols::usage =
"findexactfsymbols[] takes the numerical values of the F-symbols, and converts them \
into an exact representation. If q = Exp[2 Pi I z / l], it is assumed that the F-symbols \
take the form \
Exp[Pi I \[Alpha]] Sqrt[Sum[a[i] Cos[2 Pi z / l]^i, {i, 0, EulerPhi[l]/2 - 1}]], \
where \[Alpha] and a[i] are rational. Typically, \[Alpha] \[Element] {0,1/2,1,-1/2} \
(if one does not diagonalize the R-matrices). The exact F-symbols are given in terms of \
\[Alpha] and a[i] as {\[Alpha], {a[0], a[1], ... }}.";

findexactrsymbols::usage =
"findexactrsymbols[] takes the numerical values of the R-symbols, and converts them \
into an exact representation. If q = Exp[2 Pi I z / l], it is assumed that the R-symbols \
take the form \
Exp[Pi I \[Alpha]] Sqrt[Sum[a[i] Cos[2 Pi z / l]^i, {i, 0, EulerPhi[l]/2 - 1}]], \
where \[Alpha] and a[i] are rational. \
For the R-symbols corresponding to diagonal R-matrices, the R-symbol is a pure phase, so that
the square root factor equals one. \
The exact R-symbols are given in terms of \[Alpha] and a[i] as {\[Alpha], {a[0], a[1], ... }}.";

findexactmodulardata::usage =
"findexactmodulardata[] converts the numerical modular data into an exact representation.";

(*
checkpentagonalgebraically::usage =
	"checkpentagonexactformalgebraically[] checks the pentagon equations algebraically, using \
the exact form of the F-symbols. Note that this is much slower than checking the pentagon \
equations for the exact form of the F-symbols numercially with high precision (say 200 digits)!"
*)

(*
checkhexagonalgebraically::usage =
	"checkhexagonexactformalgebraically[] checks the hexagon equations algebraically, using \
the exact form of the F- and R-symbols. Note that this is much slower than checking the hexagon \
equations for the exact form of the F- and R-symbols numercially with high precision (say 200 digits)!"
*)

(*
checkmodulardataalgebraically::usage =
	"checkmodulardataalgebraically[] checks the exact form of the modular data algebraically using \
the exact form of the F- and R-symbols. Note that this is much slower than the standard check of \
the exact form of the modular data, which is done numerically with high (i.e., 200 digit) precision."
*)

checkpentagonexactformnumerically::usage =
"checkpentagonexactformnumerically[] numerically checks the pentagon equations, using the exact \
form of the F-symbols. The precision used is 200 digits.";

checkhexagonexactformnumerically::usage =
"checkhexagonexactformnumerically[] numerically checks the hexagon equations, using the exact \
form of the F- and R-symbols. The precision used is 200 digits.";

savecurrentdata::usage =
"savecurrentdata[] saves the (exact form of the) F-symbols, R-symbols, the modular data \
and some additional information, such that it can be loaded at later stage.";

loaddata::usage =
"loaddata[type, rank, level, rootfactor] loads the F-symbols, R-symbols and the modular data \
and initializes the system, such that it is in the same state as if one just had calculated this \
data (and its exact form). The exception is that most of the Lie algebra data and qCG coefficients \
are not loaded (this data is not stored).";

loaddatalz::usage =
"loaddata[type, rank, l, z] loads the F-symbols, R-symbols and the modular data \
and initializes the system using the l-z notation (parametrizing the root of unity as \
q = e^(2 Pi I z / l) ), such that it is in the same state as if one just had calculated this \
data (and its exact form). The exception is that most of the Lie algebra data and qCG coefficients \
are not loaded (this data is not stored).";
