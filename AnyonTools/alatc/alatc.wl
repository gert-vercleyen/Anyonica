(* ::Package:: *)

(* ::Section:: *)
(*Begin Package*)


BeginPackage["alatc`"];


(* ::Subsection::Closed:: *)
(*Usage*)


initialize::usage = 
	"initialize[\"x\",r] initializes the type of affine Lie algebra, of type \"x\" and rank r, \
subject to the constraints displayed when the package was loaded.\n\
initialize[\"x\",r,level] initializes the type of affine Lie algebra, its rank and level.\n\
initialize[\"x\",r,level,rootfactor] initializes the type of affine Lie algebra, its rank, the level \
and the rootfactor, which sets the root of unity."

initializelz::usage = 
	"initialize[\"x\", r, l, z] initializes the type of affine Lie algebra, its rank, and the \
root of unity as q = e^(2 Pi I z / l)."
	
initializelevel::usage =
	"initializelevel[k] intializes the level to k."

setprecision::usage =
	"setprecision[prec] sets the precision to prec. Should be run before \
setrootofunity[rootfactor]. The default value is prec=100, which should be \
sufficient for most cases. If mathematica complains about ill-conditioned \
matrices, or if the fusion rules are inconsistent, etc., one can try to \
increase the precision."

initializerootofunity::usage =
	"initializerootofunity[rootfactor] sets the value of q to \
e^(2 \[Pi] i rootfactor/(tmax (k+g))), \
where k is the level, g the dual coxeter number of the affine Lie algebra, and \
tmax = 1 for the simply laced cases, tmax = 3 for g_2 and tmax = 2 for the other \
non-simply laced cases (g and tmax are set when intialize[\"x\",r] is run)."

setrootofunity::usage =
	"setrootofunity[rootfactor] sets the value of q to e^(2 \[Pi] i rootfactor/(tmax (k+g))), \
where k is the level, g the dual coxeter number of the affine Lie algebra, and \
tmax = 1 for the simply laced cases, tmax = 3 for g_2 and tmax = 2 for the other \
non-simply laced cases (g and tmax are set when intialize[\"x\",r] is run). Identical to \
initializerootofunity[rootfactor], but kept for backwards compatibility."
		
possiblerootfactors::usage =
	"possiblerootfactors[\"x\",rank,level] gives the possible rootfactors \
for the selected type, rank and level. A list containing the lists with the possible rootfactors \
for the uniform and non-uniform cases is returned. The list for non-uniform cases might be empty."	
	
calculatefsymbols::usage = 
	"calculatefsymbols[] calculates the F-symbols, after the type of algebra, rank, \
level and root of unity have been selected. The calculation is done in several steps. Most \
importantly, the q-CG coefficients are constructed first. The pentagon equations are \
verified, as well as some properties of the F-matrices."
	
calculatersymbols::usage =
	"calculatersymbols[] calculates the R-symbols, after the F-symbols have been \
calculated. The hexagon equations are verified, as well as some properties of \
the R-matrices."

calculatemodulardata::usage =
	"calculatemodulardata[] calculates the modular data, once the F- and R-symbols are \
calculated: pivotal structure, Frodenius-Schur indicators, Frobenius-Perron \
dimensions, quantum dimensions, scaling dimensions, central charge adn the S-matrix. \
calculatemodulardata[pivotalstructure] calculates the modular data, but with the selected \
pivotal structure (which has to be spherical) instead."

diagonalizermatrices::usage = 
	"diagonalizermatrices[] diagonalizes the R-matrices, if they are not diagonal already. \
This will change both F- and R-symbols, so the pentagon and hexagon equations will be \
re-checked for security."

undiagonalizermatrices::usage = 
	"undiagonalizermatrices[] reverts the process of diagonalizing the R-matrices. \
This will revert both F- and R-symbols back to their original values. This is necessary \
if one after diagonalizing the R-matrices, wants to obtain the exact representation of the
F-symbols. Diagonalizing the R-matrices can lead to values for the F-symbols that can not \
be described in terms of the general numberfield that is used to describe the F- and R-symbols."

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
to use the package."

qCG::usage =
	"qcg[j1,{m1,n1},j2,{m2,n2},j3,{m3,n3},v1] gives the q-CG coefficient. \
The j's (highest weights) and m's (weights) are list of length 'rank'. \
The n's are integers distinguishing the weights (due to weight multiplicities) and \
v1 is an integer labeling which j3 in the tensor product of j1 and j2 one is considering \
(due to fusion multiplicity)."

Fsym::usage =
	"Fsym[a,b,c,d,e,f,{v1,v2,v3,v4}] gives the F-symbol. The a,b,...,f label the particles \
(lists of length 'rank'), while the v's (integers) label the four vertices."

Fsymexact::usage =
	"Fsymexact[a,b,c,d,e,f,{v1,v2,v3,v4}] gives the exact representation of the F-symbol. \
The exact form of the F-symbols is \
Exp[Pi I \[Alpha]] Sqrt[Sum[a[i] Cos[2 Pi z / l]^i, {i, 0, EulerPhi[l]/2 - 1}]], \
where \[Alpha] and a[i] are rational. Typically, \[Alpha] \[Element] {0,1/2,1,-1/2} \
(if one does not diagonalize the R-matrices). The exact F-symbols are given in terms of \
\[Alpha] and a[i] as {\[Alpha], {a[0], a[1], ... }}."

Fmat::usage =
	"Fmat[a,b,c,d] gives the F-matrix. a,b,c,d label the particles \
(lists of length 'rank')."

Rsym::usage =
	"Rsym[a,b,c,{v1,v2}] gives the R-symbol. a,b,c label the particles \
(lists of length 'rank'), while the v's (integers) label the two vertices."

Rsyminv::usage =
	"Rsyminv[a,b,c,{v1,v2}] gives the inverse R-symbol, calculated by taking the \
approriate elements of the inverse of the R-matrix. a,b,c label the particles \
(lists of length 'rank'), while the v's (integers) label the two vertices."

Rsymexact::usage =
	"Rsymexact[a,b,c,{v1,v2}] gives the exact representation of the R-symbol. \
The exact form of the R-symbols is \
Exp[Pi I \[Alpha]] Sqrt[Sum[a[i] Cos[2 Pi z / l]^i, {i, 0, EulerPhi[l]/2 - 1}]], \
where \[Alpha] and a[i] are rational. \
For the R-symbols corresponding to diagonal R-matrices, the R-symbol is a pure phase, so that
the square root factor equals one. \
The exact R-symbols are given in terms of \[Alpha] and a[i] as {\[Alpha], {a[0], a[1], ... }}."

Rsyminvexact::usage =
	"Rsyminvexact[a,b,c,{v1,v2}] gives the exact representation of the inverse R-symbol. \
For the format, see the documentation in the accompanying notebook."

Rmat::usage =
	"Rmat[a,b,c,d] gives the R-matrix. a,b,c label the particles \
(lists of length 'rank')."

Nmat::usage =
	"Nmat[a] gives the fusion matrix for fusion with particle type a."

Nvertex::usage =
	"Nvertex[a,b,c] gives the number of vertices of type (a,b,c)."
	
Fusion::usage =
	"Fusion[a,b] gives the possible fusion outcomes of a x b, that is, without(!) \
taking fusion multiplicities into account. Use Nvertex[a,b,c] to obtain the number \
of vertices of type (a,b,c)."

possiblepivotalstructures::usage =
	"possiblepivotalstructures[] calculates the solutions to the pivotal equations. \
Note that only one these solutions is actually realized by the selected quantum group!"

possiblesphericalpivotalstructures::usage =
	"possiblesphericalpivotalstructures[] calculates the solutions to the pivotal equations, \
but only returns the spherical ones (i.e., all coefficients equal to either +1 or -1). \
Note that only one these solutions is actually realized by the selected quantum group!"

findexactfsymbols::usage = 
	"findexactfsymbols[] takes the numerical values of the F-symbols, and converts them \
into an exact representation. If q = Exp[2 Pi I z / l], it is assumed that the F-symbols \
take the form \
Exp[Pi I \[Alpha]] Sqrt[Sum[a[i] Cos[2 Pi z / l]^i, {i, 0, EulerPhi[l]/2 - 1}]], \
where \[Alpha] and a[i] are rational. Typically, \[Alpha] \[Element] {0,1/2,1,-1/2} \
(if one does not diagonalize the R-matrices). The exact F-symbols are given in terms of \
\[Alpha] and a[i] as {\[Alpha], {a[0], a[1], ... }}."
	
findexactrsymbols::usage = 
	"findexactrsymbols[] takes the numerical values of the R-symbols, and converts them \
into an exact representation. If q = Exp[2 Pi I z / l], it is assumed that the R-symbols \
take the form \
Exp[Pi I \[Alpha]] Sqrt[Sum[a[i] Cos[2 Pi z / l]^i, {i, 0, EulerPhi[l]/2 - 1}]], \
where \[Alpha] and a[i] are rational. \
For the R-symbols corresponding to diagonal R-matrices, the R-symbol is a pure phase, so that
the square root factor equals one. \
The exact R-symbols are given in terms of \[Alpha] and a[i] as {\[Alpha], {a[0], a[1], ... }}."

findexactmodulardata::usage =
	"findexactmodulardata[] converts the numerical modular data into an exact representation."

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
form of the F-symbols. The precision used is 200 digits."

checkhexagonexactformnumerically::usage =
	"checkhexagonexactformnumerically[] numerically checks the hexagon equations, using the exact \
form of the F- and R-symbols. The precision used is 200 digits."

savecurrentdata::usage = 
	"savecurrentdata[] saves the (exact form of the) F-symbols, R-symbols, the modular data \
and some additional information, such that it can be loaded at later stage."

loaddata::usage = 
	"loaddata[type, rank, level, rootfactor] loads the F-symbols, R-symbols and the modular data \
and initializes the system, such that it is in the same state as if one just had calculated this \
data (and its exact form). The exception is that most of the Lie algebra data and qCG coefficients \
are not loaded (this data is not stored)."

loaddatalz::usage = 
	"loaddata[type, rank, l, z] loads the F-symbols, R-symbols and the modular data \
and initializes the system using the l-z notation (parametrizing the root of unity as \
q = e^(2 Pi I z / l) ), such that it is in the same state as if one just had calculated this \
data (and its exact form). The exception is that most of the Lie algebra data and qCG coefficients \
are not loaded (this data is not stored)."


(* ::Section:: *)
(*Begin `Private` Context*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*General functions*)


displayinfo[] := With[{},
   textStyle = 
    Style[ #,  FontSize -> 15, FontFamily -> "Source Sans Pro" ] &;
   textStyleBold = 
    Style[ #,  FontSize -> 15, FontFamily -> "Source Sans Pro", Bold] &;
   codeStyle = 
    Style[ #,  FontSize -> 14, FontFamily -> "Source Code Pro" ] &;
    
   
   Print[Sequence @@
     {
      Style[ "affine-lie-algebra-tensor-category (alatc) package\n" , FontSize -> 36, 
       FontFamily -> "Source Sans Pro"],
      Style[ "Authors: Eddy Ardonne\n" , FontSize -> 15, 
       FontFamily -> "Source Sans Pro", Bold],
      Style[ "Many thanks to: Fran\[CCedilla]ois Brunault, Achim Krause, Eric Rowell, \
Steve Simon, Joost Slingerland, Gert Vercleyen\n" , 
       FontSize -> 15, FontFamily -> "Source Sans Pro", Bold],
      Style[ 
       "License: GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007\n\
" , FontSize -> 15, FontFamily -> "Source Sans Pro", Bold],
      Style[ 
       "Last revision: 2022-03-04\n\
" , FontSize -> 15, FontFamily -> "Source Sans Pro", Bold],


      textStyle["\nThis package is based on the paper:\n"],
      Style[ 
       "Clebsch-Gordan and 6j-coefficients for rank two quantum \
groups,\n" , FontSize -> 15, FontFamily -> "Source Sans Pro", Italic],
      textStyle["Eddy Ardonne, Joost Slingerland\n"],
      textStyle["J. Phys. A 43, 395205 (2010)\n"],
      textStyle["https://doi.org/10.1088/1751-8113/43/39/395205\n"],
      textStyle["https://arxiv.org/abs/1004.5456\n"],
      textStyle[
       "\n Quantum groups based on non-twisted affine Lie algebras \
give rise to tensor categories. With this package, one can, \
numerically, calculate the associated F- and R-symbols, as well as \
the modular data.\n"],
      textStyle[
       "The types of non-twisted affine Lie algebras possible are:\n"],
      TextGrid[{
        {"type", "rank", "tmax", "g", "note"},
        {"\"a\"", "r \[GreaterEqual] 1", "1", "r+1", "su(r+1)"},
        {"\"b\"", "r \[GreaterEqual] 3", "2", "2r-1", 
         "so(2r+1), so(5) is implemented as sp(4)"},
        {"\"c\"", "r \[GreaterEqual] 2", "2", "r+1", "sp(2r)"},
        {"\"d\"", "r \[GreaterEqual] 4", "1", "2r-2", "so(2r)"},
        {"\"e\"", "r \[Element] {6,7,8}", "1", "{12,18,30}"},
        {"\"f\"", "r == 4", "2", "9"},
        {"\"g\"", "r == 2", "3", "4"}
        }],
      textStyle[
       "\n One starts by selecting the type of affine Lie algebra and \
its rank by running "],
      codeStyle["initialize[\"x\",rank]"],
      textStyle[" where "],
      codeStyle["x \[Element] {a,b,c,d,e,f,g}"],
      textStyle[
       " and the rank satisfies the appropriate constraint form \
above. Subsequently, one selects the level, which has to be a \
positive integer, by running "],
      codeStyle["initializelevel[k]"],
      textStyle[
       ".\n Once the type of algebra, rank and level are set, the \
possible values of the deformation parameter q are fixed. q can take \
the values\n\
q = e^(2 \[Pi] i rootfactor/(tmax (k + g))) ,\n where k is the level, \
g the dual coxeter number of the \
algebra, and tmax is the ratio of the length of the long and short \
roots, if any (see the table above for the values of g and tmax). \
For the standard, uniform cases, rootfactor should be relative prime \
with tmax(k+g). See the section `More detailed information' of the \
accompanying notebook alatc-info-examples.nb for more information on \
the non-uniform cases. The value of q is set by running "],
      codeStyle["initializerootofunity[rootfactor]"],
      textStyle[" .\n"],
      textStyle[
       "After the type of algebra, rank, level and rootfacter are \
set, one can proceed by calculating the F- and R-symbols, as well as \
the modular data, by running the commands (in this order)\n"],
      codeStyle["calculatefsymbols[]\n"],
      codeStyle["calculatersymbols[]\n"],
      codeStyle["calculatemodulardata[]\n"],
      textStyle[
       "If the R-matrices are not diagonal, they can be diagonalized \
using "],
      codeStyle["diagonalizermatrices[]"],
      textStyle[
       ".\nInformation on how to access the F- and R-symbols, can be \
found in the accompanying notebook, which contains some examples as \
well.\n"],
      textStyle["This information can be displayed by running "],
      codeStyle["displayinfo[]"],
      textStyle[".\n"],
      textStyle["\nSome technical notes.\n"],
      textStyle["\nIf Mathematica complains about \
ill-conditioned matrices, or if the fusion rules are inconsistent etc., \
one can try to increase the precision (which is standard set to 100, \
so pretty high already) by running "],
      codeStyle["setprecision[precision]"],
      textStyle[" where precision should be an integer > 100. "],
      codeStyle["setprecision[precision]"],
      textStyle[" should be run "],
      textStyleBold["before"],
      textStyle[" running "],
      codeStyle["initializerootofunity[rootfactor]"],
      textStyle[". Warnings are generated if the deviation is bigger than 10^(-20). \
Otherwise, the maximum deviation is given, to give a sense of the accuracy. Typically, \
the precision is much better than 10^(-20).\n"],
      textStyle["\nWhen loading the package, the recusion limit is set to 10000.\n"],
      textStyle["\nThe gauge choices made during the calculation follow, to a \
large extend, the ones described in the paper above."]
      }
    ];
   ];

Clear[nq];
nq[0,t_]:=0;
nq[1,t_]:=1;
nq[n_/;n<0,t_]:=-nq[-n,t];

Off[Solve::svars];

qnn[n_]:=(q^(n)-q^(-n))/(q^(1)-q^(-1));
qnnser[n_/;n==0]:=0;
qnnser[n_/;n>0]:=Sum[q^(j/2),{j,-(n-1),n-1,2}];
qnnser[n_/;n<0]:=Sum[-q^(j/2),{j,-(-n-1),-n-1,2}];
qnnser[n_/;n>0,t_]:=Sum[q^(j*t/(2)),{j,-(n-1),n-1,2}];
qnnser[n_/;n<0,t_]:=Sum[-q^(j*t/(2)),{j,-(-n-1),-n-1,2}];

nq[n_,t_]:=qnnser[n,t];

zeromatrix[dim_Integer]:= Table[0,{i1,1,dim},{i2,1,dim}];

precision=100;
typerankinitok=False;
typeranklevelinitok=False;
typeranklevelrootinitok=False;
fsymbolscalculated=False;
rsymbolscalculated=False;
modulardatacalculated=False;
pentagontobechecked=True;
recheck=False;
typerankinfo=True;
levelinfo=True;
rootfacinfo=True;
initfromlz=False;
fsymbolsexactfound=False;
rsymbolsexactfound=False;
modulardataexactfound=False;
rmatricesdiagonalized=False;


(* ::Subsection::Closed:: *)
(*Functions to access the data*)


qCG[j1_,m1x_,j2_,m2x_,j3_,m3x_,v_]:=qcg[j1,m1x,j2,m2x,j3,m3x,v];

Fsym[a_,b_,c_,d_,e_,f_,vvec_]:=fsym[a,b,c,d,e,f,vvec];

Fsymexact[a_,b_,c_,d_,e_,f_,vvec_]:=fsymexact[a,b,c,d,e,f,vvec];

Fmat[a_,b_,c_,d_]:=fmat[a,b,c,d];

Rsym[a_,b_,c_,vvec_]:=rsym[a,b,c,vvec];

Rsyminv[a_,b_,c_,vvec_]:=rsyminv[a,b,c,vvec];

Rsymexact[a_,b_,c_,vvec_]:=rsymexact[a,b,c,vvec];

Rsyminvexact[a_,b_,c_,vvec_]:=rsyminvexact[a,b,c,vvec];

Rmat[a_,b_,c_]:=rmat[a,b,c];

Nmat[a_]:=nmat[a];

Nvertex[a_,b_,c_]:= nv[a,b,c];

Fusion[a_,b_]:= fusion[a,b];

pivotalequations[]:=
Module[{value,valueok},
  valueok=True;
  Flatten[
    Table[
      value = 
       Sum[
        fsym[ir1, ir2, dual[ir3], irreps[[1]], ir3, dual[ir1], {v1, 1, v2, 1}]*
        fsym[ir2, dual[ir3], ir1, irreps[[1]], dual[ir1], dual[ir2], {v2, 1, v3, 1}]*
        fsym[dual[ir3], ir1, ir2, irreps[[1]], dual[ir2], ir3, {v3, 1, v1, 1}]
       ,{v2, 1, nv[ir2, dual[ir3], dual[ir1]]},{v3, 1, nv[dual[ir3], ir1, dual[ir2]]}];
     If[Chop[value-Round[value],10^(-20)]==0,
      value=Round[value],
       If[valueok,
        Print["The numerical value appering in the pivotal equation is not equal to +1 or -1, \
as should be the case for this fusion category."];
       valueok=False;
      ];
    ];
   pivotvar[ir1] * pivotvar[ir2] / pivotvar[ir3]==value
 ,{ir1, irreps},{ir2, irreps},{ir3, fusion[ir1, ir2]},{v1, 1, nv[ir1, ir2, ir3]}],3]
];

possiblepivotalstructures[]:=
  Module[{piveqns,pivsols,allpivotalstructures},
    
  If[Not[typeranklevelrootinitok],
  Print["The type of algebra, rank, level and/or rootfactor were not \
  (correctly) initialized, please do so first, followed by calculating \
  the F-symbols, before calculating the possible pivotal structures."];
  ];
  If[typeranklevelrootinitok && Not[fsymbolscalculated],
  Print["The F-symbols were not calculated. Please do so first, before \
  calculating the possible pivotal structures."];
  ];
  
  If[typeranklevelrootinitok && fsymbolscalculated,
    piveqns=pivotalequations[]//Union;
    pivsols=Solve[piveqns];
    If[Length[pivsols] != numofsimplecurrents,
     Print["The number of solutions of the pivotal equations is not equal to the number of simple currents! Better check."];
    ];
    allpivotalstructures=pivsols[[All,All,2]];
    allpivotalstructures=Abs[#]Exp[I Arg[#]]&/@allpivotalstructures;
    allpivotalstructures
  ]
];

possiblesphericalpivotalstructures[] :=
  Module[{allpivotalstructures,allsphericalpivotalstructures},
    
  If[Not[typeranklevelrootinitok],
  Print["The type of algebra, rank, level and/or rootfactor were not \
  (correctly) initialized, please do so first, followed by calculating \
  the F-symbols, before calculating the possible spherical pivotal structures."];
  ];
  If[typeranklevelrootinitok && Not[fsymbolscalculated],
  Print["The F-symbols were not calculated. Please do so first, before \
  calculating the possible spherical pivotal structures."];
  ];
  
  If[typeranklevelrootinitok && fsymbolscalculated,
  
  allpivotalstructures = possiblepivotalstructures[];
  
  allsphericalpivotalstructures = Cases[allpivotalstructures, x_/;ContainsOnly[x,{1,-1}]];
  
  allsphericalpivotalstructures

 ]
];


(* ::Subsection::Closed:: *)
(*General initialization*)


$RecursionLimit=10000;


dualcoxeter[type_, rank_] :=
  Piecewise[
   {
    {rank + 1, type == "a" && rank >= 1},
    {2 rank - 1, type == "b" && rank >= 3},
    {rank + 1, type == "c" && rank >= 2},
    {2 rank - 2, type == "d" && rank >= 4},
    {12, type == "e" && rank == 6},
    {18, type == "e" && rank == 7},
    {30, type == "e" && rank == 8},
    {9, type == "f" && rank == 4},
    {4, type == "g" && rank == 2}
    }
  ];

thvecshort[type_, rank_] :=
  Piecewise[
   {
    {{2}, type == "a" && rank == 1},
    {Table[If[i == 1 || i == rank, 1, 0], {i, 1, rank}], type == "a" && rank > 1},
    {Table[If[i == 1, 1, 0], {i, 1, rank}], type == "b" && rank >= 3},
    {Table[If[i == 2, 1, 0], {i, 1, rank}], type == "c" && rank >= 2},
    {Table[If[i == 2, 1, 0], {i, 1, rank}], type == "d" && rank >= 4},
    {{0, 0, 0, 0, 0, 1}, type == "e" && rank == 6},
    {{1, 0, 0, 0, 0, 0, 0}, type == "e" && rank == 7},
    {{1, 0, 0, 0, 0, 0, 0, 0}, type == "e" && rank == 8},
    {{0, 0, 0, 1}, type == "f" && rank == 4},
    {{0, 1}, type == "g" && rank == 2}
   }
  ];
  
thveclong[type_, rank_] :=
  Piecewise[
   {
    {{2}, type == "a" && rank == 1},
    {Table[If[i == 1 || i == rank, 1, 0], {i, 1, rank}], type == "a" && rank > 1},
    {Table[If[i == 2, 1, 0], {i, 1, rank}], type == "b" && rank >= 3},
    {Table[If[i == 1, 2, 0], {i, 1, rank}], type == "c" && rank >= 2},
    {Table[If[i == 2, 1, 0], {i, 1, rank}], type == "d" && rank >= 4},
    {{0, 0, 0, 0, 0, 1}, type == "e" && rank == 6},
    {{1, 0, 0, 0, 0, 0, 0}, type == "e" && rank == 7},
    {{1, 0, 0, 0, 0, 0, 0, 0}, type == "e" && rank == 8},
    {{1, 0, 0, 0}, type == "f" && rank == 4},
    {{1, 0}, type == "g" && rank == 2}
   }
 ];  

cartanmatrix[type_, rank_] :=
  Piecewise[
   {
    {SparseArray[{{i_, i_} -> 2, {i_, j_} /; Abs[i - j] == 1 -> -1}, {rank, rank}] // Normal, type == "a"},
    {SparseArray[{{i_, i_} -> 2, {i_, j_} /; Abs[i - j] == 1 && j < rank -> -1, {rank - 1, rank} -> -2}, {rank, rank}] // Normal, type == "b" && rank >= 3},
    {SparseArray[{{i_, i_} -> 2, {i_, j_} /; Abs[i - j] == 1 && i < rank -> -1, {rank, rank - 1} -> -2}, {rank, rank}] // Normal, type == "c" && rank >= 2},
    {SparseArray[{{i_, i_} -> 2, {i_, j_} /; Abs[i - j] == 1 && i < rank && j < rank -> -1, {rank - 2, rank} -> -1, {rank, rank - 2} -> -1}, {rank, rank}] // Normal, type == "d" && rank >= 4},
    {SparseArray[{{i_, i_} -> 2, {i_, j_} /; Abs[i - j] == 1 && i < rank && j < rank -> -1, {3, 6} -> -1, {6, 3} -> -1}, {rank, rank}] // Normal, type == "e" && rank == 6},
    {SparseArray[{{i_, i_} -> 2, {i_, j_} /; Abs[i - j] == 1 && i < rank && j < rank -> -1, {3, 7} -> -1, {7, 3} -> -1}, {rank, rank}] // Normal, type == "e" && rank == 7},
    {SparseArray[{{i_, i_} -> 2, {i_, j_} /; Abs[i - j] == 1 && i < rank && j < rank -> -1, {5, 8} -> -1, {8, 5} -> -1}, {rank, rank}] // Normal, type == "e" && rank == 8},
    {SparseArray[{{i_, i_} -> 2, {i_, j_} /; Abs[i - j] == 1 && {i, j} != {2, 3} -> -1, {2, 3} -> -2}, {rank, rank}] // Normal, type == "f" && rank == 4},
    {{{2, -3}, {-1, 2}}, type == "g" && rank == 2}
   }
  ];
  
qfmatrix[type_, rank_] :=
  With[
  {ict = Transpose[Inverse[cartanmatrix[type, rank]]],
   rlfs = rootlengthfactors[type, rank]},
   Table[1/rlfs[[i]] ict[[i]], {i, 1, rank}]
   ]; 
  


tmaxvalue[type_] :=
  Piecewise[
   {
    {1, type == "a" || type == "d" || type == "e"},
    {2, type == "b" || type == "c" || type == "f"},
    {3, type == "g"}
   }
  ];


rootlengthfactors[type_, rank_] :=
  Piecewise[
   {
    {Table[1, {i, 1, rank}], (type == "a" && rank >= 1) || (type == "d" && rank >= 4) || (type == "e" && 6 <= rank <= 8)},
    {Table[If[i < rank, 1, 2], {i, 1, rank}], type == "b" && rank >= 3},
    {Table[If[i < rank, 2, 1], {i, 1, rank}], type == "c" && rank >= 2},
    {{1, 1, 2, 2}, type == "f" && rank == 4},
    {{1, 3}, type == "g" && rank == 2}
   }
 ];
 
rootlengthfactorsinverted[type_, rank_] :=
  tmaxvalue[type] * Table[1/rlf ,{rlf, rootlengthfactors[type, rank]}];


rangeok[type_,rank_]:=
 Piecewise[
  {
   {True,rank\[Element]Integers&&type=="a"&&rank>=1},
   {True,rank\[Element]Integers&&type=="b"&&rank>=3},
   {True,rank\[Element]Integers&&type=="c"&&rank>=2},
   {True,rank\[Element]Integers&&type=="d"&&rank>=4},
   {True,rank\[Element]Integers&&type=="e"&&6<=rank<=8},
   {True,rank\[Element]Integers&&type=="f"&&rank==4},
   {True,rank\[Element]Integers&&type=="g"&&rank==2}
  },
 False
];  


(* ::Subsection::Closed:: *)
(*Routines to initialization of the current algebra, rank, level and root of unity*)


initialize[atype_, rr_] :=
  Module[{},
    If[
      rangeok[atype, rr],
      (* THEN *)
      (* General initialization *)
      typerankinitok = False; (* Will be set to True once we're done *)
      typeranklevelinitok = False;
      typeranklevelrootinitok = False;
      fsymbolscalculated = False;
      rsymbolscalculated = False;
      fsymbolsexactfound = False;
      rsymbolsexactfound = False;
      modulardatacalculated = False;
      modulardataexactfound = False;
      pentagontobechecked = True;
      recheck = False;
      rmatricesdiagonalized = False;
      clearvariables[];
      clearglobalvariables[];

      type = atype;
      rank = rr;

      th = thveclong[type, rank];
      g = dualcoxeter[type, rank];
      cartan = cartanmatrix[type, rank];
      icartan = Inverse[cartan];
      qfm = qfmatrix[type, rank];

      tmax = tmaxvalue[type];
      tvec = rootlengthfactorsinverted[type, rank];
      rho = Table[1, {j, 1, rank}];
      a = icartan.rho;


    (* Generate the roots of the algebra *)
    pos = 1;
    roots = {th};
    While[
      pos <= Dimensions[roots][[1]],
      For[
        i = 1,
        i <= rank,
        i++,
        If[
          roots[[pos, i]] > 0,
          Do[
            If[
              Not[ MemberQ[ roots, roots[[pos]] - j cartan[[i]] ] ],
              roots = Append[ roots, roots[[pos]] - j cartan[[i]] ] ],
            {j, roots[[pos, i]] }
          ]
        ]
     ];
     pos = pos + 1
    ];
    roots = 
     Sort[roots, 
      Sum[(#1.icartan)[[j]] a[[j]], {j, 1, rank}] >= Sum[(#2.icartan)[[j]] a[[j]], {j, 1, rank}] &];
    na =
      Dimensions[roots][[1]];
    If[
      typerankinfo,
      Print["The type of algebra and rank have been set to ", {type,rank}];
    ];
    
    typerankinitok =
      True;
    , (* ELSE *)
    If[
      typerankinfo,
      Print["The type of algebra and the rank are not compatible!"];
      Print["Run initialize[\"x\",r] again to set the type of algebra and its rank."];
    ];
    , (* IF rangeok IS NOT A BOOLEAN  *)
    If[typerankinfo,
      Print["The type of algebra and the rank are not compatible!"];
      Print["Run initialize[\"x\",r] again to set the type of algebra and its rank."];
    ];
    ];
  ];
   
   
initializelevel[lev_] :=
  Module[{n},  
  If[typerankinitok,
   If[IntegerQ[lev] && lev >= 0,
     typeranklevelinitok = False; (* will be set to True once we're done *)
     typeranklevelrootinitok = False;
     fsymbolscalculated = False;
     rsymbolscalculated = False;
     fsymbolsexactfound = False;
     rsymbolsexactfound = False;
     modulardatacalculated = False;
     modulardataexactfound = False;
     pentagontobechecked = True;
     rmatricesdiagonalized = False;
     recheck = False;
     level = lev;
     rootofunity = 1/(g + level);
     canbenonuniform = nonuniformpossible[type, rank, level];
     rootfactorsuniform = possiblerootfactorsuniform[type, rank, level];
     If[canbenonuniform,
       rootfactorsnonuniform = possiblerootfactorsnonuniform[type, rank, level];,
       rootfactorsnonuniform = {};
     ];
     rootfactors = Union[rootfactorsuniform, rootfactorsnonuniform];
     
     typeranklevelinitok = True;
     If[levelinfo,
     Print["The level has been set to ",level];
     If[canbenonuniform,
     Print["The possible roots of unity are ", 
      Exp[2 Pi I "rootfactor" rootofunity/(tmax)] // TraditionalForm, 
      ",\n with rootfactor an element of the set: ", rootfactorsuniform, " for the uniform cases, or \n\
with rootfactor an element of the set: ", rootfactorsnonuniform, " for the non-uniform cases."];,
     Print["The possible roots of unity are ", 
      Exp[2 Pi I "rootfactor" rootofunity/(tmax)] // TraditionalForm, 
      ",\n with rootfactor an element of the set: ", rootfactorsuniform, " for the uniform cases.\n There are no non-uniform cases."];
      ];
     ];
     ,
     If[levelinfo,
     Print["The level has to be a positive integer!"];
     Print["Run initializelevel[level] again to set the level."];
     ];
     ,
     If[levelinfo,
     Print["The level has to be a positive integer!"];
     Print["Run initializelevel[level] again to set the level."];
     ];
     ];
     ,
    Print["The type of algebra and rank are not correctly initialized, please do so first!"];
     ];
   ];   
   
setrootofunity[rootfac_] := initializerootofunity[rootfac];
   
initializerootofunity[rootfac_] :=
  Piecewise[{
    {
     If[
       MemberQ[rootfactors, rootfac],
       rootfactor = rootfac;
       q = N[Exp[2 Pi I rootfactor rootofunity/tmax], precision];
       
       If[MemberQ[rootfactorsuniform, rootfac], uniform = True;, uniform = False;];
       
       If[uniform,
       cosdenominator = 1/rootofunity*tmax;
       cosnumerator = rootfactor;,
       cosdenominator = 1/rootofunity;
       cosnumerator = rootfactor/tmax;
       ];
       
       If[
         uniform,
         irreps = irrepsuniform[type, rank, level];,
         irreps = irrepsnonuniform[type, rank, level];
       ];
       numofirreps = Length[irreps];
    
       
       If[rootfacinfo, Print["The rootfactor has been set to ",rootfactor];];
       If[uniform && Not[initfromlz], Print["The type of algebra, rank, level and rootfactor are initialized, and set to ", {type, rank, level, rootfactor}, ". This is a uniform case."];];
       If[uniform && initfromlz, Print["The type of algebra, rank, l and z are initialized, and set to ", {type, rank, lval, zval}, ". This is a uniform case."];];
       If[Not[uniform] && Not[initfromlz], Print["The type of algebra, rank, level and rootfactor are initialized, and set to ", {type, rank, level, rootfactor}, ". This is a non-uniform case."];];
       If[Not[uniform] && initfromlz, Print["The type of algebra, rank, l and z are initialized, and set to ", {type, rank, lval, zval}, ". This is a non-uniform case."];];

       typeranklevelrootinitok = True;
       fsymbolscalculated = False;
       rsymbolscalculated = False;
       fsymbolsexactfound = False;
       rsymbolsexactfound = False;
       modulardatacalculated = False;
       modulardataexactfound = False;
       rmatricesdiagonalized = False;
       recheck = False;
              
       Print["You can proceed to calculate the F-symbols :-)"];
       ,
       typeranklevelrootinitok = False;
       If[canbenonuniform, Print["The possible roots of unity are ", Exp[2 Pi I "rootfactor" rootofunity/(tmax)] // TraditionalForm, ",\n\ with rootfactor an element of the set: ", rootfactorsuniform, " for the uniform cases, or \n\with rootfactor an element of the set: ", rootfactorsnonuniform, " for the non-uniform cases."];,
        Print["The possible roots of unity are ", 
        Exp[2 Pi I "rootfactor" rootofunity/(tmax)] // TraditionalForm, 
        ",\n\ with rootfactor an element of the set: ", rootfactorsuniform, " for the uniform cases.\n\There are no non-uniform cases."];
       ];
        Print["Run initializerootofunity[rootfactor] again to select a valid rootfactor."];
       ,
       typeranklevelrootinitok = False;
        If[canbenonuniform,
        Print["The possible roots of unity are ", 
        Exp[2 Pi I "rootfactor" rootofunity/(tmax)] // TraditionalForm,
        ",\n\
with rootfactor an element of the set: ", rootfactorsuniform, " for the uniform cases, or \n\
with rootfactor an element of the set: ", rootfactorsnonuniform, " for the non-uniform cases."];,
        Print["The possible roots of unity are ", 
        Exp[2 Pi I "rootfactor" rootofunity/(tmax)] // TraditionalForm, 
        ",\n\
with rootfactor an element of the set: ", rootfactorsuniform, " for the uniform cases.\n\
There are no non-uniform cases."];
        ];
        Print["Run initializerootofunity[rootfactor] again to select a valid rootfactor."];
       ];
     , typeranklevelinitok},
    {Print["The type of algebra and rank are not correctly initialized, please do so first!"];
     , Not[typerankinitok]},
    {Print["The level is not correctly initialized, please do so first!"];
     , typerankinitok && Not[typeranklevelinitok]}
    }];
    
   
initialize[type_, rank_, level_] :=
    Module[{typerangeok, levelok},
   
   If[Not[rangeok[type, rank]],
    typerangeok = False;
    Print["The type of algebra and the rank are not compatible!"];,
    typerangeok = True;,
    typerangeok = False;
    Print["The type of algebra and the rank are not compatible!"];
    ];
   
   If[IntegerQ[level] && level > 0,
    levelok = True,
    Print["The level is not a positive integer!"];
    levelok = False;,
    Print["The level is not a positive integer!"];
    levelok = False;
    ];
   
   If[typerangeok && levelok,
    typerankinfo = False;
    levelinfo = False;
    initialize[type, rank];
    initializelevel[level];
    Print["The type of algebra, rank, and level are initialized, and set to ",{type, rank, level}];
    If[canbenonuniform,
     Print["The possible roots of unity are ", 
      Exp[2 Pi I "rootfactor" rootofunity/(tmax)] // TraditionalForm,
      ",\n\
with rootfactor an element of the set: ", rootfactorsuniform, " for the uniform cases, or \n\
with rootfactor an element of the set: ", rootfactorsnonuniform, " for the non-uniform cases."];,
     Print["The possible roots of unity are ", 
      Exp[2 Pi I "rootfactor" rootofunity/(tmax)] // TraditionalForm, 
      ",\n\
with rootfactor an element of the set: ", rootfactorsuniform, " for the uniform cases.\n\
There are no non-uniform cases."];
     ];      
      
      
    typerankinfo = True;
    levelinfo = True;
    ,
    Print["No initialization was done!"];
    ];
   
   ];   
   
initialize[type_, rank_, level_, rootfac_] :=
  Module[{typerangeok, levelok, rootfacok, posrootfacsall, posrootfacsuniform, posrootfacsnonuniform, canbenonuniform},
      If[Not[rangeok[type, rank]],
        typerangeok = False;
        Print["The type of algebra and the rank are not compatible!"];,
        typerangeok = True;,
        typerangeok = False;
        Print["The type of algebra and the rank are not compatible!"];
        ];
      
      If[IntegerQ[level] && level > 0,
        levelok = True,
        Print["The level is not a positive integer!"];
        levelok = False;,
        Print["The level is not a positive integer!"];
        levelok = False;
        ];
        
    If[typerangeok && levelok,
     canbenonuniform = nonuniformpossible[type, rank, level];
     posrootfacsuniform = possiblerootfactorsuniform[type, rank, level];
     If[canbenonuniform,
       posrootfacsnonuniform = possiblerootfactorsnonuniform[type, rank, level];,
       posrootfacsnonuniform = {};
     ];
     posrootfacsall = Union[posrootfacsuniform, posrootfacsnonuniform];

    If[MemberQ[posrootfacsall, rootfac],
     rootfacok = True;,
     If[canbenonuniform,
      Print["The rootfactor should be a member of the set ", posrootfacsuniform," for the uniform case, or of the set ",
      posrootfacsnonuniform," for the non-uniform case."];,
      Print["The rootfactor should be a member of the set ", posrootfacsuniform," for the uniform case. There are no non-uniform cases."];
      ];
     rootfacok = False;,
     If[canbenonuniform,
      Print["The rootfactor should be a member of the set ", posrootfacsuniform," for the uniform case, or of the set ",
      posrootfacsnonuniform," for the non-uniform case."];,
      Print["The rootfactor should be a member of the set ", posrootfacsuniform," for the uniform case. There are no non-uniform cases."];
      ];
     rootfacok = False;
     ];
   
     ]; 
    
    
    If[typerangeok && levelok && rootfacok,
        typerankinfo = False;
        levelinfo = False;
        rootfacinfo = False;
        initialize[type, rank];
        initializelevel[level];
        initializerootofunity[rootfac];
        typerankinfo = True;
        levelinfo = True;
        rootfacinfo = True;
        ,
        Print["No initialization was done!"];
        ];
      ]; 
      
initializelz[type_, rank_, lvalue_, zvalue_]:=
Module[{typerangeok, currentlvalueok, currentzvalueok, currentlzvaluesok, lzlevel, lzrootfac},

  If[Not[rangeok[type, rank]],
   typerangeok = False;
   Print["The type of algebra and the rank are not compatible!"];,
   typerangeok = True;
   ,
   typerangeok = False;
   Print["The type of algebra and the rank are not compatible!"];
 ];
 
 
 If[typerangeok,
  If[Not[lvalueok[type, rank, lvalue]],
  currentlvalueok = False;
  Print["The value of l is not compatible with the type of algebra and its rank."];
  lvaluespossible[type, rank];,
  currentlvalueok = True;,
  currentlvalueok = False;
  Print["The value of l is not compatible with the type of algebra and its rank."];
  lvaluespossible[type, rank];
 ];
 ];
 
 If[typerangeok && currentlvalueok,
   If[Not[zvalueok[lvalue,zvalue]],
    currentzvalueok = False;
    currentlzvaluesok = False;
    Print["The value of z is not compatible with l. The possible values for z are: 0 < z < ", lvalue, ", such that \
GDC[z,",lvalue,"] == 1."];,
    currentzvalueok = True;
    currentlzvaluesok = True;,
    currentzvalueok = False;
    currentlzvaluesok = False;
    Print["The value of z is not compatible with l. The possible values for z are: 0 < z < ", lvalue, ", such that \
GDC[z,",lvalue,"] == 1."];
   ];
  ];
 
 If[typerangeok && currentlvalueok && currentzvalueok,
    typerankinfo = False;
    levelinfo = False;
    rootfacinfo = False;
    initialize[type,rank];
    uniform = lzvaluesuniform[type, rank, lvalue, zvalue];
    lzlevel = If[uniform, lvalue/tmax - g, lvalue - g];
    lzrootfac = If[uniform, zvalue, tmax*zvalue];
    initializelevel[lzlevel];
    lval = lvalue;
    zval = zvalue;
    initfromlz = True;
    initializerootofunity[lzrootfac];
    typerankinfo = True;
    levelinfo = True;
    rootfacinfo = True;
    initfromlz = False;
    ,
    Print["No initialization was done!"];,
    Print["No initialization was done!"];
 ];
 
 
];



setprecision[prec_] := With[{},
   If[IntegerQ[prec] && prec > 100,
     precision = prec;
     Print["The precision has been set to ", precision];
     ,
     precision = 100;
     Print["The precision should be an integer \[GreaterEqual] 100"];
     Print["The precision has been set to ", precision];
     ,
     precision = 100;
     Print["The precision should be an integer \[GreaterEqual] 100"];
     Print["The precision has been set to ", precision];
     ];
   ];         
   
donotcheckpentagon[] := With[{},
   pentagontobechecked = False;
   Print["You opted to hop over the step of checking the pentagon \
equations, so you proceed at your own risk. \n\
It is recommended that the R-symbols are also generated, so that \
the hexagon equations are checked. \
If they hold, it is likely the pentagon equations are also \
satisfied."];
   ];   
   
docheckpentagon[] := With[{},
   pentagontobechecked = True;
   Print["The pentagon equations will be checked."];
   ];   


(* ::Subsection::Closed:: *)
(*Functions for the non-uniform case*)


nonuniformpossible[type_, rank_, level_] :=
  Piecewise[
   {
    {True, type == "b" && rank >= 3 && Mod[level, 2] == 0 && level >= 1},
    {True, type == "c" && rank >= 2 && Mod[level + rank, 2] == 0 && level >= rank - 1},
    {True, type == "f" && rank == 4 && Mod[level, 2] == 0 && level >= 3},
    {True, type == "g" && rank == 2 && (Mod[level, 3] == 0 || Mod[level, 3] == 1) && level >= 2}
   },
   False
  ];

irrepsuniform[type_, rank_, level_] :=
  Solve[
  (Table[avar[i] + 1, {i, 1, rank}]).qfmatrix[type, rank].thveclong[type, rank] < level + dualcoxeter[type, rank] &&
  And @@ Table[avar[i] >= 0, {i, 1, rank}], Integers][[All, All, 2]] // Sort;
  
irrepsnonuniform[type_, rank_, level_] :=
 Solve[
   tmaxvalue[type] (Table[avar[i] + 1, {i, 1, rank}]).qfmatrix[type, rank].thvecshort[type, rank] < level + dualcoxeter[type, rank] && 
   And @@ Table[avar[i] >= 0, {i, 1, rank}], Integers][[All, All, 2]] // Sort;
      
possiblerootfactorsnonuniform[type_, rank_, level_] :=
  Module[
   {tmax = tmaxvalue[type], g = dualcoxeter[type, rank], res},
   If[nonuniformpossible[type, rank, level],
    res = Range[tmax (level + g)];
    res = Cases[res, x_ /; Mod[x, tmax] == 0 && GCD[x, level + g] == 1];,
    res = {};
    ];
    res
   ];

possiblerootfactorsuniform[type_, rank_, level_] :=
  Module[
   {tmax = tmaxvalue[type], g = dualcoxeter[type, rank], res},
   res = Range[tmax (level + g)];
   res = Cases[res, x_ /; GCD[x, tmax(level + g)] == 1]
   ];
   
possiblerootfactors[type_, rank_, level_] :=
  Module[{tmax, g, rfpossible,rfuniform,rfnonuniform},
   If[rangeok[type, rank],
    If[IntegerQ[level] && level > 0,
     rfuniform = possiblerootfactorsuniform[type,rank,level];
     rfnonuniform = possiblerootfactorsnonuniform[type,rank,level];
     rfpossible = {rfuniform, rfnonuniform}
     , Print["The level is not a positve integer"];
     ]
    , Print["The type of algebra and rank are not compatible!"];
    ]
   ];    
           
lzvaluesok[type_, rank_Integer, lvalue_Integer, zvalue_Integer] :=
  Piecewise[
   {
    (* uniform cases: *)
    {True, type == "a" && rank >= 1 && lvalue >= 1*(rank + 1) && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {True, type == "b" && rank >= 3 && lvalue >= 2*(2 rank - 1) && Mod[lvalue, 2] == 0 && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {True, type == "c" && rank >= 2 && lvalue >= 2*(rank + 1) && Mod[lvalue, 2] == 0 && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {True, type == "d" && rank >= 4 && lvalue >= 1*(2 rank - 2) && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {True, type == "e" && rank == 6 && lvalue >= 1*(12) && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {True, type == "e" && rank == 7 && lvalue >= 1*(18) && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {True, type == "e" && rank == 8 && lvalue >= 1*(30) && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {True, type == "f" && rank == 4 && lvalue >= 2*(9) && Mod[lvalue, 2] == 0 && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {True, type == "g" && rank == 2 && lvalue >= 3*(4) && Mod[lvalue, 3] == 0 && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    (* non-uniform cases: *)
    {True, type == "b" && rank >= 3 && lvalue >= 2 rank + 1 && Mod[lvalue, 2] == 1 && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {True, type == "c" && rank >= 2 && lvalue >= 2 rank + 1 && Mod[lvalue, 2] == 1 && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {True, type == "f" && rank == 4 && lvalue >= 13 && Mod[lvalue, 2] == 1 && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {True, type == "g" && rank == 2 && lvalue >= 7 && (Mod[lvalue, 3] == 1 || Mod[lvalue, 3] == 2) && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue}
    },
   False
   ];
   
lvalueok[type_, rank_Integer, lvalue_Integer] :=
  Piecewise[
   {
    (* uniform cases: *)
    {True, type == "a" && rank >= 1 && lvalue >= 1*(rank + 1)},
    {True, type == "b" && rank >= 3 && lvalue >= 2*(2 rank - 1) && Mod[lvalue, 2] == 0},
    {True, type == "c" && rank >= 2 && lvalue >= 2*(rank + 1) && Mod[lvalue, 2] == 0},
    {True, type == "d" && rank >= 4 && lvalue >= 1*(2 rank - 2)},
    {True, type == "e" && rank == 6 && lvalue >= 1*(12)},
    {True, type == "e" && rank == 7 && lvalue >= 1*(18)},
    {True, type == "e" && rank == 8 && lvalue >= 1*(30)},
    {True, type == "f" && rank == 4 && lvalue >= 2*(9) && Mod[lvalue, 2] == 0},
    {True, type == "g" && rank == 2 && lvalue >= 3*(4) && Mod[lvalue, 3] == 0},
    (* non-uniform cases: *)
    {True, type == "b" && rank >= 3 && lvalue >= 2 rank + 1 && Mod[lvalue, 2] == 1},
    {True, type == "c" && rank >= 2 && lvalue >= 2 rank + 1 && Mod[lvalue, 2] == 1},
    {True, type == "f" && rank == 4 && lvalue >= 13 && Mod[lvalue, 2] == 1},
    {True, type == "g" && rank == 2 && lvalue >= 7 && (Mod[lvalue, 3] == 1 || Mod[lvalue, 3] == 2)}
    },
   False
   ];
   
zvalueok[lvalue_Integer, zvalue_Integer]:= (GCD[lvalue,zvalue] == 1) && (0 < zvalue < lvalue);
   
lzvaluesuniform[type_, rank_Integer, lvalue_Integer, zvalue_Integer] :=
  Piecewise[
   {
    (* uniform cases: *)
    {True, type == "a" && rank >= 1 && lvalue >= 1*(rank + 1) && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {True, type == "b" && rank >= 3 && lvalue >= 2*(2 rank - 1) && Mod[lvalue, 2] == 0 && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {True, type == "c" && rank >= 2 && lvalue >= 2*(rank + 1) && Mod[lvalue, 2] == 0 && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {True, type == "d" && rank >= 4 && lvalue >= 1*(2 rank - 2) && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {True, type == "e" && rank == 6 && lvalue >= 1*(12) && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {True, type == "e" && rank == 7 && lvalue >= 1*(18) && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {True, type == "e" && rank == 8 && lvalue >= 1*(30) && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {True, type == "f" && rank == 4 && lvalue >= 2*(9) && Mod[lvalue, 2] == 0 && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {True, type == "g" && rank == 2 && lvalue >= 3*(4) && Mod[lvalue, 3] == 0 && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    (* non-uniform cases: *)
    {False, type == "b" && rank >= 3 && lvalue >= 2 rank + 1 && Mod[lvalue, 2] == 1 && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {False, type == "c" && rank >= 2 && lvalue >= 2 rank + 1 && Mod[lvalue, 2] == 1 && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {False, type == "f" && rank == 4 && lvalue >= 13 && Mod[lvalue, 2] == 1 && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue},
    {False, type == "g" && rank == 2 && lvalue >= 7 && (Mod[lvalue, 3] == 1 || Mod[lvalue, 3] == 2) && GCD[zvalue, lvalue] == 1 && 0 < zvalue < lvalue}
    }
   ];
    
lvaluespossible[type_,rank_Integer]:=
  Piecewise[
  {
  {Print["The possible values for l (only uniform cases) are: l >= ", rank + 1, "."];, type == "a" && rank >= 1},
  {Print["The possible values for l even (uniform case) are: l >= ", 4 rank - 2, ",\n\
the possible values for l odd (non-uniform case) are: l >= ", 2 rank + 1, "."],type == "b" && rank >= 3},
  {Print["The possible values for l even (uniform case): l >= ", 2 rank + 2, ",\n\
the possible values for l odd (non-uniform case): l >= ", 2 rank + 1, "."],type == "c" && rank >= 2},
  {Print["The possible values for l (only uniform cases): l >= ", 2 rank - 2, "."],type == "d" && rank >= 4},
  {Print["The possible values for l (only uniform cases): l >= ", 12, "."],type == "e" && rank == 6},
  {Print["The possible values for l (only uniform cases): l >= ", 18, "."],type == "e" && rank == 7},
  {Print["The possible values for l (only uniform cases): l >= ", 30, "."],type == "e" && rank == 8},
  {Print["The possible values for l even (uniform case): l >= ", 18, ",\n\
the possible values for l odd (non-uniform case): l >= ", 13, "."],type == "f" && rank == 4},
  {Print["The possible values for l a multiple of three (uniform case): l >= ", 12, ",\n\
the possible values for l not a multiple of three (non-uniform case): l >= ", 7,"."],type == "g" && rank == 2}
  },
  Print["The type and rank are not compatible!"];
 ];
 


(* ::Subsection::Closed:: *)
(*Routines for the inner product*)


cleargeneralinnerproduct[] :=
 With[{},
  Clear[generalinnerproduct];
  generalinnerproduct[hw_, operators_] := 
   generalinnerproduct[hw, operators] =
    Module[{leftmostlowerpos},
     If[
      
      (* Check if raising and lowering operators are consistent and only give states in the representation *)
      
      Sort[Cases[operators, x_ /; x[[2]] == 1][[All, 1]]] != 
        Sort[Cases[operators, x_ /; x[[2]] == -1][[All, 1]]] || Not[
        Module[{test, len},
         len = Length[operators];
         If[len > 0,
          test[len + 1] = hw;
          
          Do[test[pos] = 
            test[pos + 1] + (operators[[pos, 2]]) cartan[[operators[[pos, 1]]]]
            , {pos, len, 1, -1}];
          
          And @@ Table[
            MemberQ[weights[hw], test[pos]], {pos, 1, len}], True
          ]
         ]
        ],
      0,
      If[
       (* End of the recursion *)
       operators == {},
       1,
       If[
        (* (conjugate) raising operator acts on the highest weight *)
        operators[[1, 2]] == -1,
        0,
        (* Actual recursion step *)
        leftmostlowerpos = 
         Position[operators, x_ /; x[[2]] == -1, {1}, 1, 
           Heads -> False][[1, 1]];
        If[
         (* Check if lefmost lowering operator commutes with raising operator which is one position to the left *)
         operators[[leftmostlowerpos, 1]] != 
          operators[[leftmostlowerpos - 1, 1]],
         (* commutes, only one term contributes *)
         generalinnerproduct[hw, 
          ReplacePart[
           operators, {leftmostlowerpos -> 
             operators[[leftmostlowerpos - 1]], 
            leftmostlowerpos - 1 -> operators[[leftmostlowerpos]]}]]
            ,
         (* does not commute, two terms contribute *)
         generalinnerproduct[hw, 
           ReplacePart[
            operators, {leftmostlowerpos -> 
              operators[[leftmostlowerpos - 1]], 
             leftmostlowerpos - 1 -> operators[[leftmostlowerpos]]}]] +
          nq[(hw - Sum[cartan[[operators[[i, 1]]]], {i, 1, leftmostlowerpos - 2}])[[
             operators[[leftmostlowerpos, 1]]]], 
            tvec[[operators[[leftmostlowerpos, 1]]]]]*
           generalinnerproduct[hw, 
            Delete[operators, {{leftmostlowerpos}, {leftmostlowerpos - 1}}]]
        ]
        ]
       ]
      ]
     ]
  ];



clearinnerproduct[] :=
 With[{},
  Clear[innerproduct];
  innerproduct[hw1_, lowops1_, hw2_, lowops2_] := 
   innerproduct[hw1, lowops1, hw2, lowops2] = 
    If[hw1 != hw2 || Sort[lowops1] != Sort[lowops2], 0,
     Chop[
      generalinnerproduct[hw1, Flatten[{Table[{op1, 1}, {op1, Reverse[lowops1]}], Table[{op2, -1}, {op2, lowops2}]}, 1]]
      , 10^(-(Max[10, precision - 20]))]]
  ];

initializeinnerproduct[] :=
  With[{},
   clearinnerproduct[];
   cleargeneralinnerproduct[];
   ];


(* ::Subsection::Closed:: *)
(*Routine to initialize the weight spaces*)


initializeweightspaces[irreps_] := Module[{inprod, factor, w, pos, dims, dim, temp, p1},
    
    inprod[lp_, mm_] := lp.qfm.mm;
    factor[lp_, hw_] := 2/((hw + rho).qfm.(hw + rho) - (lp + rho).qfm.(lp + rho));


    (*
    The [0] in w[0], dims[0] etc, is a leftover from code to calculate the dimensions of weight spaces of affine representations.
    Here, we only need the classical case, i.e. 0.
    *)
   
    Do[
      Clear[w];
      w[0] = {hw};
       pos = 1;
       While[pos <= Dimensions[w[0]][[1]], 
        For[i = 1, i <= rank, i++, 
         If[w[0][[pos, i]] > 0, 
          Do[If[Not[
             MemberQ[w[0], w[0][[pos]] - j cartan[[i]]]], 
            w[0] = 
             Append[w[0], w[0][[pos]] - j cartan[[i]]]], {j, w[0][[pos, i]]}]]
         ];
         pos = pos + 1];
      
      (* Sort the weigths according to their `heigth' *)
      w[0] = 
        Sort[w[0], 
         Sum[#1[[j]]* a[[j]], {j, 1, rank}] >= 
           Sum[#2[[j]] *a[[j]], {j, 1, rank}] &];
      
      weights[hw] = w[0];
      
      
      (* Create the table of dimensions *)
     Clear[dims];
     dims[0] = {};
     
     Do[
      dims[0] = Insert[dims[0], {}, 1]
      , {j, Dimensions[w[0]][[1]]}
     ];
     
     dims[0] = Insert[dims[0], 1, {1, 1}];

      (* Freudenthal formula, https://en.wikipedia.org/wiki/Weyl_character_formula*)
     Do[
      If[Length[dims[0][[j]]] == 0, 
         dims[0] = 
         Insert[
           dims[0],
           factor[ w[0][[j]], hw ] *
              Sum[
              Catch[
                For[ temp = 0; p1 = 1, True,
                 If[ MemberQ[w[0], w[0][[j]] + p1 roots[[a]]],
                  temp += 
                   dims[0][[ Position[w[0], w[0][[j]] + p1 roots[[a]]][[1, 1]] , 1 ]]*
                     (inprod[roots[[a]],  w[0][[j]] + p1 roots[[a]]]); p1++, 
                  Throw[temp]]]
               ]
               , {a, 1, (na - 1)/2}]
            , {j, 1}
         ];
        ]
       , {j, 1, Dimensions[w[0]][[1]]}
     ];
         
      Do[dim[hw, w[0][[i]]] = dims[0][[i, 1]], {i, 1, Length[dims[0]]}];
      
      
      Do[wdim[hw, weights[hw][[i]]] = dim[hw, weights[hw][[i]]], {i, 1, Length[weights[hw]]}];
      
      
      , {hw, irreps}
    ];
     
     Do[
      raising[ir, w, alpha] = 
       If[MemberQ[weights[ir], w + cartan[[alpha]]], 
        w + cartan[[alpha]], {}];
      lowering[ir, w, alpha] = 
       If[MemberQ[weights[ir], w - cartan[[alpha]]], 
        w - cartan[[alpha]], {}];
      , {ir, irreps}, {w, weights[ir]}, {alpha, 1, rank}];
     
     Clear[w, l];

];


(* ::Subsection::Closed:: *)
(*Routines to construct the bases for the weight spaces*)


(*
generatestatespossible[] should not be necessary anymore, after the update dealing the precision in a better way.
It is only used when the standard way of generating the states needed does not work
*)

generatestatespossible[] :=
  
  Module[{weightsabove, allbasis, subs, subslength, notfound, subspos},
   
   Clear[statespossible];
   
   Do[
    
    statespossible[l, l] = {{}};
    
    Do[
     
     weightsabove = 
      DeleteCases[Table[{i, raising[l, currentweight, i]}, {i, 1, rank}],
        x_ /; x[[2]] == {}];
     
     allbasis = Flatten[
       Table[
        Table[
         Prepend[j, weightsabove[[i, 1]]]
         , {j, statespossible[l, weightsabove[[i, 2]]]}]
        , {i, 1, Length[weightsabove]}]
       , 1];
     
     statespossible[l, currentweight] = allbasis;
     
     , {currentweight, weights[l][[2 ;; -1]]}];
    
    Clear[weightsabove, allbasis, subs, subslength, notfound, subspos];
    
    , {l, irreps}];
   
   ];
   
   
generatestatesneeded[] :=
  
  Module[{weightsabove, allbasis, subs, subslength, notfound, subspos},
   
   Clear[statesneeded];
   
   Do[
    
    statesneeded[l, l] = {{}};
    
    Do[
     weightsabove = 
      DeleteCases[Table[{i, raising[l, currentweight, i]}, {i, 1, rank}],
        x_ /; x[[2]] == {}];
     
     allbasis = Flatten[
       Table[
        Table[
         Prepend[j, weightsabove[[i, 1]]]
         ,{j, statesneeded[l, weightsabove[[i, 2]]]}]
        , {i, 1, Length[weightsabove]}]
       , 1];
     
     subs = Subsets[Range[Length[allbasis]], {wdim[l, currentweight]}];
     subslength = Length[subs];
     notfound = True;
     subspos = 1;
     
     While[notfound && subspos <= subslength,
      If[
       MatrixRank[Table[
          Chop[innerproduct[l, w1, l, w2], 10^(-(Max[10, precision - 20]))]
          , {w1, allbasis[[subs[[subspos]]]]}
          , {w2, allbasis[[subs[[subspos]]]]}]
          ] == wdim[l, currentweight]
       ,
       notfound = False;
       statesneeded[l, currentweight] = allbasis[[subs[[subspos]]]];
       ];
      subspos++];
     
     If[notfound,
      
      Print["Reverting to the other method.", {l, currentweight}];
      
      If[statespossible[irreps[[1]], irreps[[1]]] == {{}}, Null, generatestatespossible[];];
      
      allbasis = statespossible[l, currentweight];
      
      subs = 
       Subsets[Range[Length[allbasis]], {wdim[l, currentweight]}];
      subslength = Length[subs];
      notfound = True;
      subspos = 1;
      
      While[notfound && subspos <= subslength,
       If[
        MatrixRank[Table[
           Chop[innerproduct[l, w1, l, w2], 
            10^(-(Max[10, precision - 20]))]
           , {w1, allbasis[[subs[[subspos]]]]}
           , {w2, allbasis[[subs[[subspos]]]]}]] == wdim[l, currentweight]
        ,
        notfound = False;
        statesneeded[l, currentweight] = allbasis[[subs[[subspos]]]]
        ];
       subspos++];
      
      If[notfound, 
       Print["No correct basis was found!! ", {l, currentweight}]];
      
      
      ];
     
     , {currentweight, weights[l][[2 ;; -1]]}];
    
    Clear[weightsabove, allbasis, subs, subslength, notfound, subspos];
    
    , {l, irreps}];
   
   ];   


constructgramm[] := With[{},
   Do[gramm[ir, w] = Table[
       Chop[innerproduct[ir, w1, ir, w2], 
        10^(-(Max[10, precision - 20]))]
       , {w1, statesneeded[ir, w]}
       , {w2, statesneeded[ir, w]}];
    , {ir, irreps}, {w, weights[ir]}];
   ];
   
constructbasis[] := Module[{norm},
   
   Do[
    basis[ir, w, i] =
     Table[If[j == i, 1, 0], {j, 1, wdim[ir, w]}] - 
      Sum[basis[ir, w, j] (basis[ir, w, j].gramm[ir, w])[[i]], {j, 1, i - 1}];
        
    norm = basis[ir, w, i].gramm[ir, w].basis[ir, w, i];
    
    If[Chop[norm, 10^(-(Max[10, precision - 20]))] != 0, 
     basis[ir, w, i] = 
       Chop[1/Sqrt[norm] basis[ir, w, i], 
        10^(-(Max[10, precision - 20]))];, 
     Print["The norm of a state is zero! ", {ir, w, i}]];
    Clear[norm];
    , {ir, irreps}, {w, weights[ir]}, {i, 1, wdim[ir, w]}];
   
   Do[
    basison[ir, w] = Table[basis[ir, w, i], {i, wdim[ir, w]}]
    , {ir, irreps}, {w, weights[ir]}];
   
   ];   

checkweightspaceorthogonality[] := Module[{maxdev, tempdev},
   weightspaceorthogonal = True;
   maxdev = 0;
   Do[
    tempdev =
     Max[(Abs /@ Flatten[(basison[ir, w].gramm[ir, w].Transpose[basison[ir, w]] - IdentityMatrix[wdim[ir, w]])])];
    (*If[tempdev > maxdev, maxdev = tempdev];*)
    (*
      Max deals better with high precision numbers than > (Greater).
      This caused maxdev to remain 0, even if it should have been f.i. 0``73.93384483647623 .
    *)
    maxdev=Max[maxdev,tempdev];   

    , {ir, irreps}, {w, weights[ir]}];
    
  
    If[
     Max[maxdev,10^(-20)] == maxdev,
     weightspaceorthogonal = False; 
    ];

   weightspacedeviation = maxdev;

    

      If[weightspaceorthogonal,
    Print[
     "The constructed bases for the weightspaces are orthonormal :-)"];
    Print["The maximum deviation is: ", weightspacedeviation]; 
     ,
    Print[
     "The constructed bases for the weightspaces are not orthonormal :-(, if the maximum deviation ", weightspacedeviation, 
     " is small, you can try to proceed at your own risk."]
    ];
   
   ];         
   
   
initializeweightspacebasis[] := With[{},
   initializeinnerproduct[];
   generatestatesneeded[];
   constructgramm[];
   constructbasis[];
   checkweightspaceorthogonality[];
   ];     
         


(* ::Subsection::Closed:: *)
(*Routines to generate the fusion rules*)


myorthogonalize[vecs_] := Module[
  {numvecs, tempbas},
  numvecs = Length[vecs];
  Do[tempbas[i] =
    vecs[[i]] - 
     Sum[tempbas[
        j] (tempbas[j].vecs[[i]])/(tempbas[j].tempbas[j]), {j, 1, i - 1}]
   , {i, 1, numvecs}];
  Chop[Table[If[Chop[(tempbas[i].tempbas[i]), 10^(-(Max[10, precision - 20]))] != 0, 
     tempbas[i]/Sqrt[tempbas[i].tempbas[i]], tempbas[i]]
     , {i, 1, numvecs}], 10^(-(Max[10, precision - 20]))]
  ];
  
hwtpraising[hw1_, hw2_, hw3_, alpha_, vec_] := Module[
  {currstates, resvec, newstates, newweight, newstate, newstatepos, newcontrib},

  currstates = tpstates[hw1, hw2, hw3, hw3];
  resvec = Table[0, {i, 1, Length[tpstatesraising[hw1, hw2, hw3, alpha]]}];
  newstates = tpstatesraising[hw1, hw2, hw3, alpha];
  
  Do[
   newweight = raising[hw1, currstates[[i, 1, 1]], alpha];
   If[
    newweight != {},
    Do[
      newstate = {{newweight, j1}, currstates[[i, 2]]};
      newstatepos = Position[newstates, newstate][[1, 1]];
      newcontrib = 
       vec[[i]]* q^(currstates[[i, 2, 1, alpha]] * tvec[[alpha]]/(4))*
        basisraising[hw1, currstates[[i, 1, 1]], alpha][[currstates[[i, 1, 2]], j1]];
      resvec = 
       ReplacePart[resvec, 
        newstatepos -> resvec[[newstatepos]] + newcontrib];
      , {j1, 1, wdim[hw1, newweight]}];
    ];
   
   
   newweight = raising[hw2, currstates[[i, 2, 1]], alpha];
   If[
    newweight != {},
    Do[
      newstate = {currstates[[i, 1]], {newweight, j2}};
      newstatepos = Position[newstates, newstate][[1, 1]];
      newcontrib = 
       vec[[i]]* q^(-currstates[[i, 1, 1, alpha]] * tvec[[alpha]]/(4))*
        basisraising[hw2, currstates[[i, 2, 1]], alpha][[currstates[[i, 2, 2]], j2]];
      resvec = 
       ReplacePart[resvec, 
        newstatepos -> resvec[[newstatepos]] + newcontrib];
      , {j2, 1, wdim[hw2, newweight]}];
    ];
   
   
   , {i, 1, Length[vec]}];
  
  resvec
  
  ];  
  
  
hwtpraisingconditions[hw1_, hw2_, hw3_, alpha_] :=
  Chop[hwtpraising[hw1, hw2, hw3, alpha, Table[vars[i], {i, 1, Length[tpstates[hw1, hw2, hw3, hw3]]}]],10^(-(Max[10, precision - 20]))];  
  
  
hwtpraisingsol[hw1_, hw2_, hw3_] :=
 Chop[Solve[
   Table[(hwtpraisingconditions[hw1, hw2, hw3, alpha]) == 
     Table[0, {i, 1, Length[tpstatesraising[hw1, hw2, hw3, alpha]]}], {alpha, 1, rank}],
   Table[vars[i], {i, 1, Length[tpstates[hw1, hw2, hw3, hw3]]}]], 10^(-(Max[10, precision - 20]))];  
   
generatefr[hw1_, hw2_, hw3_] := 
  Module[{tpdim, result, sol, variables, numofrawsol, rawstates, rawipmat, numofsol},
   tpdim = Length[tpstates[hw1, hw2, hw3, hw3]];
   
   (* If there are no possible tensor product states, hw3 will not be in
   the fusion product, so nothing needs to be done *)
   
   If[tpdim > 0,
    result = Table[vars[i], {i, 1, tpdim}];
    sol = hwtpraisingsol[hw1, hw2, hw3];
    If[Length[sol] != 1, 
     Print["There is a problem with the heighest weight solution."]];
    sol = sol[[1]];
    
    (* For all the varialbles, the state is a hw, so nv[]=tpdim *)
   
     If[sol == {},
     fusion[hw1, hw2] = Append[fusion[hw1, hw2], hw3];
     nv[hw1, hw2, hw3] = tpdim;
     ];
    
    
    (* In this case, we need to find the number of independent solutions *)
    
    If[sol != {} && tpdim > Length[sol],
     variables = Union[Flatten[Variables /@ sol[[All, 2]]]];
     numofrawsol = Length[variables];
     result = result /. sol;
     
     rawstates = Table[
       result /. 
        Table[variables[[j]] -> If[j == i, 1, 0], {j, 1, numofrawsol}]
       , {i, 1, numofrawsol}];
     
     
     rawipmat = 
      Table[rawstates[[i]].rawstates[[j]], {i, 1, numofrawsol}, {j, 1, numofrawsol}];
         
     numofsol = MatrixRank[rawipmat];
     If[numofsol > 0,
      fusion[hw1, hw2] = Append[fusion[hw1, hw2], hw3];
      nv[hw1, hw2, hw3] = numofsol;
      ];
     ];
    ];
   ];   
   
   
generateraisingloweringoperators[] := With[{},
   
   Do[
    If[raising[ir, w, alpha] == {}
     , stateraising[ir, w, alpha] = {},
     stateraising[ir, w, alpha] =
      Table[
       generalinnerproduct[ir, 
        Flatten[{Table[{op1, 1}, {op1, Append[Reverse[statesneeded[ir, raising[ir, w, alpha]][[j]]], alpha]}],
           Table[{op2, -1}, {op2, statesneeded[ir, w][[i]]}]}, 1]]
       , {i, wdim[ir, w]}, {j, wdim[ir, raising[ir, w, alpha]]}]
     ]
    , {ir, irreps}, {w, weights[ir]}, {alpha, 1, rank}];
   
   Do[
    If[lowering[ir, w, alpha] == {}
     , statelowering[ir, w, alpha] = {},
     statelowering[ir, w, alpha] =
      Table[
       generalinnerproduct[ir, 
        Flatten[{Table[{op1, 1}, {op1, Reverse[statesneeded[ir, lowering[ir, w, alpha]][[j]]]}], 
          Table[{op2, -1}, {op2, Prepend[statesneeded[ir, w][[i]], alpha]}]}, 1]]
       , {i, wdim[ir, w]}, {j, wdim[ir, lowering[ir, w, alpha]]}]
     ]
    , {ir, irreps}, {w, weights[ir]}, {alpha, 1, rank}];
   
   Do[
    If[raising[ir, w, alpha] == {},
     basisraising[ir, w, alpha] = {},
     basisraising[ir, w, alpha] = basison[ir, w].stateraising[ir, w, alpha].Transpose[basison[ir, raising[ir, w, alpha]]]
     ]
    , {ir, irreps}, {w, weights[ir]}, {alpha, 1, rank}];
   
   Do[
    If[lowering[ir, w, alpha] == {},
     basislowering[ir, w, alpha] = {},
     basislowering[ir, w, alpha] = basison[ir, w].statelowering[ir, w, alpha].Transpose[basison[ir, lowering[ir, w, alpha]]]
     ]
    , {ir, irreps}, {w, weights[ir]}, {alpha, 1, rank}];
   
   ];   
   
initializetpstates[] := With[{},
   
   Do[
    Module[{res},
      res = 
       Cases[Table[{w1, (w - w1)}, {w1, weights[hw1]}], x_ /; MemberQ[weights[hw2], x[[2]]]];
      tpstates[hw1, hw2, hw3, w] =
       Flatten[
        Table[{{res[[i, 1]], v1}, {res[[i, 2]], v2}}
        , {i, 1, Length[res]}, {v1, 1, wdim[hw1, res[[i, 1]]]}, {v2, 1, wdim[hw2, res[[i, 2]]]}], 2];
      ];
    , {hw1, irreps}, {hw2, irreps}, {hw3, irreps}, {w, weights[hw3]}];
   
   Do[
    Module[{res},
      res = 
       Cases[Table[{w1, (hw3 + cartan[[alpha]] - w1)}, {w1, weights[hw1]}], x_ /; MemberQ[weights[hw2], x[[2]]]];
      tpstatesraising[hw1, hw2, hw3, alpha] =
       Flatten[
        Table[{{res[[i, 1]], v1}, {res[[i, 2]], v2}}
        , {i, 1, Length[res]}, {v1, 1, wdim[hw1, res[[i, 1]]]}, {v2, 1, wdim[hw2, res[[i, 2]]]}], 2];
      ];
    , {hw1, irreps}, {hw2, irreps}, {hw3, irreps}, {alpha, 1, rank}];
   
   ];   
   
   
intializefusionrules[] := With[{},
  Do[fusion[hw1, hw2] = {};, {hw1, irreps}, {hw2, irreps}];
  ];
  
     
checkfusionrules[] := Module[{dualpos, tempok, irreppos},
  
  fusionrulesok = True;
  
  Do[
   nmat[ir1] = Table[
     If[Head[nv[ir1, ir2, ir3]] === Integer, nv[ir1, ir2, ir3], 0]
     , {ir2, irreps}, {ir3, irreps}]
  , {ir1, irreps}];
  
  tempok = True;
  Do[
   dualpos = 
    Flatten[Position[Table[fusion[ir1, ir2], {ir2, irreps}], x_ /; MemberQ[x, irreps[[1]]]]];
   If[
    Length[dualpos] == 1, Null, fusionrulesok = False; 
    tempok = False;];
   , {ir1, irreps}];
   
  If[tempok, Null, 
   Print["Not every particle type has a unique dual :-("];];
  
  dualpos = 
   Table[Position[Table[fusion[ir1, ir2], {ir2, irreps}], x_ /; MemberQ[x, irreps[[1]]]][[1, 1]], {ir1, irreps}];
  dualirreps = irreps[[dualpos]];
  Do[
  dualirrep[irreps[[i]]] = irreps[[dualpos[[i]]]]
  , {i, 1, numofirreps}];
  
  tempok = True;
  Do[
   If[nmat[ir1].nmat[ir2] == nmat[ir2].nmat[ir1], Null,
     fusionrulesok = False;
     tempok = False;
     ];
   , {ir1, irreps}, {ir2, irreps}];
  If[tempok, Null, 
   Print["The fusion rules are not associatve :-("];];
  
  
  tempok = True;
  Do[irreppos[irreps[[i]]] = i, {i, 1, numofirreps}];
  
  Do[
   If[
     nmat[ir1][[irreppos[ir2], irreppos[ir3]]] == 
      nmat[ir2][[irreppos[ir1], irreppos[ir3]]] == 
      nmat[ir2][[irreppos[dualirrep[ir3]], irreppos[dualirrep[ir1]]]] == 
      nmat[dualirrep[ir1]][[irreppos[dualirrep[ir2]], 
       irreppos[dualirrep[ir3]]]]
     , Null, fusionrulesok = False;
     tempok = False;
     ];
   , {ir1, irreps}, {ir2, irreps}, {ir3, irreps}];
  
  If[tempok, Null, 
   Print["The vertex properties of the fusionrules are not satisfied :-("];];
  
  
  If[fusionrulesok,
   Print["The calculated fusion rules are consistent :-)"];,
   Print["The calculated fusion rules are not consistent :-( something went wrong..."];
   ];
  
  irrepsdual = 
   Table[
   irreps[[Position[nmat[irreps[[i]]], x_ /; x[[1]] == 1][[1, 1]]]]
      , {i, 1, numofirreps}] // Quiet;
       
  Do[
   dual[irreps[[i]]] = irrepsdual[[i]]
   , {i, 1, numofirreps}];
  
  selfdualvec = 
   Table[irreps[[i]] == irrepsdual[[i]], {i, 1, numofirreps}];
  
  numofselfduals = Count[selfdualvec, True];
   
  Do[
   selfdual[irreps[[i]]] = selfdualvec[[i]]
   , {i, 1, numofirreps}];
   
  simplecurrentvec =
  Table[
   Table[Sum[nv[ir1, ir2, ir3], {ir3, fusion[ir1, ir2]}], {ir2, irreps}] == Table[1, {i, 1, numofirreps}]
   , {ir1, irreps}];

  numofsimplecurrents = Count[simplecurrentvec, True];  
  
  ];
  
generatefusionrules[] :=
  Module[{},
   
   generateraisingloweringoperators[];
   
   initializetpstates[];
   
   intializefusionrules[];
   
   Do[generatefr[hw1, hw2, hw3], {hw1, irreps}, {hw2, irreps}, {hw3, irreps}];
   
   checkfusionrules[];
   
   flist = 
    Flatten[Table[{a, b, c, d, e, f, {v1, v2, v3, v4}}
    , {a, irreps}, {b, irreps}, {c, irreps}, {e, fusion[a, b]}, {d, fusion[e, c]}
    , {f, Cases[fusion[b, c], x_ /; MemberQ[fusion[a, x], d]]}
    , {v1, nv[a, b, e]}, {v2, nv[e, c, d]}, {v3, nv[b, c, f]}, {v4, nv[a, f, d]}]
    , 9];
   
   fmatlist = flist[[All, 1 ;; 4]] // Union;
   
   rlist = Flatten[
     Table[{a, b, c, {v1, v2}}
     , {a, irreps}, {b, irreps}, {c, fusion[a, b]}, {v1, nv[a, b, c]}, {v2, nv[a, b, c]}]
     , 4];
   
   rmatlist = rlist[[All, 1 ;; 3]] // Union;
   
   nvlist = Table[
     nv[Sequence @@ rmatlist[[i]]]
     , {i, 1, Length[rmatlist]}];
   
   maxmultiplicity = Max[nvlist];
   
   multiplicity = Not[(nvlist // Union) == {1}];
   
   numoffusmultiplicities = Count[nvlist, x_ /; x > 1];
   
   Global`irreps = irreps;
   
   Global`flist = flist;
   Global`fmatlist = fmatlist;
   Global`rlist = rlist;
   Global`rmatlist = rmatlist;
   Global`maxmultiplicity = maxmultiplicity;
   Global`multiplicity = multiplicity;
   Global`numberoffusionmultiplicities = numoffusmultiplicities;
   Global`numberofsimplecurrents = numofsimplecurrents;
   Global`numberofselfdualparticles = numofselfduals;
         
   ]; 


(* ::Subsection::Closed:: *)
(*Routines to generate the tensor product highest weight states*)


sethwstates[hw1_, hw2_, hw3_] := Module[
   {tpdim, result, sol, variables, numofstates, rawstates, 
    numofrawsol, rawipmat, subs, subslength, notfound, subspos},
   
   tpdim = Length[tpstates[hw1, hw2, hw3, hw3]];
   numofstates = nv[hw1, hw2, hw3];
   
   (* Although it should not be possible, let's check if tpdim > 0 *)

      If[tpdim == 0,
    Print["No states in the tensor product, something's wrong!"];,
    
    result = Table[vars[i], {i, 1, tpdim}];
    sol = hwtpraisingsol[hw1, hw2, hw3];
    If[Length[sol] != 1, 
     Print["There is a problem with the heighest weight solution."]];
    sol = sol[[1]];
    
    (* sol={}, so for each veriable there is a hwstate *)
    
    If[sol == {},
     result = IdentityMatrix[tpdim];
     ];
    
    (* In this case, we need to construct nv independent states *)
   
     If[sol != {} && tpdim > Length[sol],
     variables = Union[Flatten[Variables /@ sol[[All, 2]]]];
     numofrawsol = Length[variables];
     result = result /. sol;
     rawstates = Table[
       result /. Table[variables[[j]] -> If[j == i, 1, 0], {j, 1, numofrawsol}]
       , {i, 1, numofrawsol}];
     rawipmat = 
      Table[rawstates[[i]].rawstates[[j]]
      , {i, 1, numofrawsol}, {j, 1, numofrawsol}];
     
     subs = Subsets[Range[numofrawsol], {numofstates}];
     subslength = Length[subs];
     notfound = True;
     subspos = 1;
     While[notfound && subspos <= subslength,
      If[
       MatrixRank[rawipmat[[subs[[subspos]], subs[[subspos]]]]] == numofstates,
       notfound = False;
       result = Chop[myorthogonalize[rawstates[[subs[[subspos]]]]], 10^(-(Max[10, precision - 20])) ];
       ];
      subspos++];
     If[notfound, 
      Print["The number of highest weight states (as previously determined) were not found, so there's a problem"]];
     ];
    
    If[result == Table[vars[i], {i, 1, tpdim}],
     Print["No highest weight states were found, somthing's wrong!"]
     ];
    
    Do[
     tpstatevec[hw1, hw2, hw3, i, {}] = result[[i]];
     , {i, 1, numofstates}];
    
    ];
   ];
   
generatehwstates[] := Module[{},
  Do[sethwstates[hw1, hw2, hw3], {hw1, irreps}, {hw2, irreps}, {hw3, fusion[hw1, hw2]}];
  ];   


(* ::Subsection::Closed:: *)
(*Routines for the tensor product lowering operations & calculation of the q - CG coefficients*)


tplowering[hw1_, hw2_, hw3_, w3_, alpha_, vec_] := Module[
   {currstates, resvec, newstates, newweight, newstate, newstatepos, 
    newcontrib},
   currstates = tpstates[hw1, hw2, hw3, w3];
   newstates = tpstates[hw1, hw2, hw3, w3 - cartan[[alpha]]];
   resvec = Table[0, {i, 1, Length[newstates]}];
   
   Do[
    newweight = lowering[hw1, currstates[[i, 1, 1]], alpha];
    If[
     newweight != {},
     Do[
       newstate = {{newweight, j1}, currstates[[i, 2]]};
       newstatepos = Position[newstates, newstate][[1, 1]];
       newcontrib = 
        vec[[i]]*q^(currstates[[i, 2, 1, alpha]]* tvec[[alpha]]/(4))*
         basislowering[hw1, currstates[[i, 1, 1]], alpha][[currstates[[i, 1, 2]], j1]];
       resvec = 
        ReplacePart[resvec, newstatepos -> resvec[[newstatepos]] + newcontrib];
       , {j1, 1, wdim[hw1, newweight]}];
     ];
    
    
    newweight = lowering[hw2, currstates[[i, 2, 1]], alpha];
    If[
     newweight != {},
     Do[
       newstate = {currstates[[i, 1]], {newweight, j2}};
       newstatepos = Position[newstates, newstate][[1, 1]];
       newcontrib = 
        vec[[i]]* q^(-currstates[[i, 1, 1, alpha]]*tvec[[alpha]]/(4))*
         basislowering[hw2, currstates[[i, 2, 1]], alpha][[currstates[[i, 2, 2]], j2]];
       resvec = 
        ReplacePart[resvec, newstatepos -> resvec[[newstatepos]] + newcontrib];
       , {j2, 1, wdim[hw2, newweight]}];
     ];
    
    
    , {i, 1, Length[vec]}];
   
   resvec
   
   ];
   
lowertpstates[] := With[{},
   
   Do[
     tpstatevec[hw1, hw2, hw3, v, state] = 
       Chop[tplowering[hw1, hw2, hw3, w3 + cartan[[state[[1]]]], 
         state[[1]], tpstatevec[hw1, hw2, hw3, v, state[[2 ;; -1]]]],10^(-(Max[10, precision - 20]))];
     , {hw1, irreps}, {hw2, irreps}, {hw3, fusion[hw1, hw2]}
     , {w3, weights[hw3][[2 ;; -1]]}, {state, statesneeded[hw3, w3]}, {v, 1, nv[hw1, hw2, hw3]}];
   
   ];   
   
generatetpbasis[] := With[{},
   
   Do[
     tpbasis[hw1, hw2, hw3, v, {w3, i}] = 
       Chop[Sum[basison[hw3, w3][[i, j]] *tpstatevec[hw1, hw2, hw3, v, statesneeded[hw3, w3][[j]]], {j, 1, wdim[hw3, w3]}] , 10^(-(Max[10, precision - 20]))];
     , {hw1, irreps}, {hw2, irreps}, {hw3, fusion[hw1, hw2]}
     , {w3, weights[hw3]}, {i, 1, wdim[hw3, w3]}, {v, 1, nv[hw1, hw2, hw3]}];
   
   ]; 

   
checkorthonormality[] := Module[{maxdev, tempdev},
   orthonormalityok = True;
   maxdev = 0;

   Do[
   
    tempdev = 
       Max[Abs /@ (Flatten[
           Table[(tpbasis[hw1, hw2, hw3, v, {w3, i}].tpbasis[hw1, hw2, hw3, v, {w3, j}])
           , {i, 1, wdim[hw3, w3]}, {j, 1, wdim[hw3, w3]}] - IdentityMatrix[wdim[hw3, w3]]])];

    (*If[tempdev > maxdev, maxdev = tempdev];*)
    (*
     Max deals better with high precision numbers than > (Greater).
     This caused maxdev to remain 0, even if it should have been f.i. 0``73.93384483647623 .
    *)
    maxdev=Max[maxdev,tempdev];           
                 
    , {hw1, irreps}, {hw2, irreps}, {hw3, fusion[hw1, hw2]}
    , {w3, weights[hw3]}, {v, 1, nv[hw1, hw2, hw3]}];
    
    
    If[
     Max[maxdev,10^(-20)] == maxdev,
     orthonormalityok = False; 
    ];

   qCGdeviation = maxdev;    
    
   
   If[orthonormalityok,
    Print["The calculated q-CG coefficients satisfy the orthonormality conditions :-)"];
    Print["The maximum deviation is: ", qCGdeviation];
    ,
    Print["The constructed q-CG coefficients do not satisfy the orthonormalilty conditions :-( If the maximum deviation ", qCGdeviation, 
      " is small, you can try to proceed at your own risk; typically, the numerical error in the F-sybmols is smaller. Increasing the precision might also help."];
    ];
   ]; 

   
         
generatealltpstates[] := With[{},
   
   lowertpstates[];
   
   generatetpbasis[];
   
   checkorthonormality[];
   
   ];
   
storeqcgcoeffs[] := With[{},
   
   numqCGcoef = 0;
   
   Do[
    With[
     {currstates = tpstates[hw1, hw2, hw3, w3]},
     Do[
       qcg[hw1, currstates[[j, 1]], hw2, currstates[[j, 2]], hw3, {w3, i}, v] = 
        Chop[tpbasis[hw1, hw2, hw3, v, {w3, i}][[j]] , 10^(-(Max[10, precision - 20]))];
       numqCGcoef++;
       , {i, 1, wdim[hw3, w3]}, {j, 1, Length[currstates]}];
     ]
    , {hw1, irreps}, {hw2, irreps}, {hw3, fusion[hw1, hw2]}
    , {w3, weights[hw3]}, {v, 1, nv[hw1, hw2, hw3]}];
   
   ];
   
generateqcgcoefficients[] := With[{},
   
   generatealltpstates[];
   
   storeqcgcoeffs[];
   
   ];               
            


(* ::Subsection::Closed:: *)
(*Routines to calculate the F - symbols*)


constructfsymbols[] := With[{},
   Do[
     fsym[Sequence @@ flist[[i]]] = N[Chop[Sum[
        qcg[flist[[i, 1]], {m1, d1}, flist[[i, 2]], {m2, d2}, flist[[i, 5]], {m12, d12}, flist[[i, 7, 1]]] *
          qcg[flist[[i, 5]], {m12, d12}, flist[[i, 3]], {m3, d3}, flist[[i, 4]], {flist[[i, 4]], 1}, flist[[i, 7, 2]]] *
          qcg[flist[[i, 2]], {m2, d2}, flist[[i, 3]], {m3, d3}, flist[[i, 6]], {m23, d23}, flist[[i, 7, 3]]] *
          qcg[flist[[i, 1]], {m1, d1}, flist[[i, 6]], {m23, d23}, flist[[i, 4]], {flist[[i, 4]], 1}, flist[[i, 7, 4]]],
        {m1, weights[flist[[i, 1]]]}, {d1, 1, wdim[flist[[i, 1]], m1]},
        {m2, weights[flist[[i, 2]]]}, {d2, 1, wdim[flist[[i, 2]], m2]},
        {m12, Cases[weights[flist[[i, 5]]], x_ /; (m1 + m2) == x]}, {d12, 1, wdim[flist[[i, 5]], m12]},
        {m3, Cases[weights[flist[[i, 3]]], x_ /; (m12 + x) == flist[[i, 4]]]}, {d3, 1, wdim[flist[[i, 3]], m3]}, 
        {m23, Cases[weights[flist[[i, 6]]], x_ /; (m2 + m3) == x]}, {d23, 1, wdim[flist[[i, 6]], m23]}
        ] , 10^(-20)], precision];
    , {i, 1, Length[flist]}];
   
   Do[
    fmat[Sequence @@ fm] =
     Flatten[
      Table[
       fsym[fm[[1]], fm[[2]], fm[[3]], fm[[4]], e, f, {v1, v2, v3, v4}],
       {e, Cases[fusion[fm[[1]], fm[[2]]], x_ /; MemberQ[fusion[x, fm[[3]]], fm[[4]]]]},
       {f, Cases[fusion[fm[[2]], fm[[3]]], x_ /; MemberQ[fusion[fm[[1]], x], fm[[4]]]]},
       {v1, nv[fm[[1]], fm[[2]], e]},
       {v2, nv[e, fm[[3]], fm[[4]]]},
       {v3, nv[fm[[2]], fm[[3]], f]},
       {v4, nv[fm[[1]], f, fm[[4]]]}
       ]
      , {{1, 3, 4}, {2, 5, 6}}];
    
    fmatdim[Sequence @@ fm] = Length[fmat[Sequence @@ fm]];
    , {fm, fmatlist}];
   
   fmatdimtab = Table[fmatdim[Sequence @@ fm], {fm, fmatlist}];
   
   If[pentagontobechecked,
    numofpentagonequations = Sum[
    nv[a, b, f]*nv[f, c, g]*nv[g, d, e]* nv[c, d, gp]*nv[b, gp, fp]*nv[a, fp, e]
    , {a, irreps}, {b, irreps}, {c, irreps}, {d, irreps}
    , {f, fusion[a, b]}, {g, fusion[f, c]}, {e, fusion[g, d]}
    , {gp, Cases[fusion[c, d], x_ /; MemberQ[fusion[f, x], e]]}
    , {fp, Cases[fusion[b, gp], x_ /; MemberQ[fusion[a, x], e]]}];
   ];
   
 ];
   

     
checkpentagon[] := Module[{maxdev, tempdev},
   
   pentholds = True;
   pentundecidable = False;
   maxdev = 0;
   
   If[pentagontobechecked,
   If[Not[recheck],Print["Checking the ", numofpentagonequations, " pentagon equations..."];];
   If[recheck,Print["Re-checking the ", numofpentagonequations, " pentagon equations..."];];
   counter = 0;

       Do[
       

       tempdev = 
       Abs[
        Sum[(fsym[f, c, d, e, g, gp, {v2, v3, v4, v1s}]*
             fsym[a, b, gp, e, f, fp, {v1, v1s, v5, v6}])
             , {v1s, nv[f, gp, e]}] - 
         Sum[(fsym[a, b, c, g, f, h, {v1, v2, v2s, v3s}] *
             fsym[a, h, d, e, g, fp, {v3s, v3, v4s, v6}]*
              fsym[b, c, d, fp, h, gp, {v2s, v4s, v4, v5}])
              , {h, Quiet[Cases[fusion[b, c], x_ /; MemberQ[fusion[a, x], g] && 
               MemberQ[fusion[x, d], fp]]]}
               , {v2s, nv[b, c, h]}, {v3s, nv[a, h, g]}, {v4s, nv[h, d, fp]}]];
               
               
               
       (*If[tempdev > maxdev, maxdev = tempdev];*)
      (*
      Max deals better with high precision numbers than > (Greater).
      This caused maxdev to remain 0, even if it should have been f.i. 0``73.93384483647623 .
      *)
      
      If[NumberQ[tempdev],
      maxdev=Max[maxdev,tempdev];,
      If[Not[pentundecidable], 
        Print["At least one of the pentagon equations is not decidable! Something went wrong :-("];
        pentundecidable = True;];
      ];
      counter++;

      , 
      {a, irreps}, {b, irreps}, {c, irreps}, {d, irreps},
      {f, fusion[a, b]}, {g, fusion[f, c]}, {e, fusion[g, d]},
      {gp, Cases[fusion[c, d], x_ /; MemberQ[fusion[f, x], e]]},
      {fp, Cases[fusion[b, gp], x_ /; MemberQ[fusion[a, x], e]]},
      {v1, nv[a, b, f]}, {v2, nv[f, c, g]}, {v3, nv[g, d, e]},
      {v4, nv[c, d, gp]}, {v5, nv[b, gp, fp]}, {v6, nv[a, fp, e]}
      
      ];
      
            
      
    If[
     Max[maxdev,10^(-20)] == maxdev,
     pentholds = False; 
    ];

   pentagondeviation = maxdev;
   Clear[counter];     
      
    
    ,
     If[Not[recheck],
     Print["The pentagon equations were not checked, because you opted not \
to do so. Proceed with care!"];
     ];
     If[recheck,
     Print["The pentagon equations were not (re)-checked, because you opted not \
to do so. Proceed with care!"];
    ];
    
    

  ];
   
   
   
   
   If[pentagontobechecked,
  If[pentholds && Not[pentundecidable],
    Print["The pentagon equations are satisfied :-)"];
    Print["The maximum deviation is: ", pentagondeviation];,
    Print["At least one of the pentagon equations is not satisfied :-(, the maximum deviation is: ", pentagondeviation, " something went wrong..."];];
  ];

   If[Not[recheck],
   If[multiplicity,
    If[numoffusmultiplicities == 1,
     Print["There is one fusion multiplicity."];, 
     Print["There are ", numoffusmultiplicities," fusion multiplicities."]];
    Print["The largest fusion multiplicity is: ", maxmultiplicity];
    ,
    Print["There are no fusion multiplicities."];
    ];
    ];
    
   
   fsymsreal = 
    And @@ (Element[#, Reals] & /@ 
       Table[Chop[ fsym[Sequence @@ i] , 10^(-20) ], {i, flist}]);
   
   If[
    And @@ Table[
       (Chop[ (fmat[Sequence @@ fm].Conjugate[Transpose[fmat[Sequence @@ fm]]] - 
              IdentityMatrix[fmatdim[Sequence @@ fm]]) , 10^(-20) ] // Flatten // Union) == {0}
       , {fm, fmatlist}] && And @@ Table[(Chop[ (Conjugate[Transpose[fmat[Sequence @@ fm]]].fmat[Sequence @@ fm] - 
              IdentityMatrix[fmatdim[Sequence @@ fm]]) , 10^(-20) ] // Flatten // Union) == {0}
       , {fm, fmatlist}],
    fmatunitary = True;
    Print["The F-matrices are unitary."];,
    fmatunitary = False;
    Print[
     "The F-matrices are not all unitary."];
    ];
   
   
   If[
    And @@ Table[
       ( Chop[ (fmat[Sequence @@ fm].Transpose[fmat[Sequence @@ fm]] - IdentityMatrix[fmatdim[Sequence @@ fm]]) , 10^(-20) ] // Flatten // Union) == {0}
       , {fm, fmatlist}] &&
    And @@ Table[( Chop[ (Transpose[fmat[Sequence @@ fm]].fmat[Sequence @@ fm] - IdentityMatrix[fmatdim[Sequence @@ fm]]) , 10^(-20)] // Flatten // Union) == {0}
       , {fm, fmatlist}],
    fsymsFTFone = True;
    ,
    fsymsFTFone = False;
    ];
   
   fsymbolarguements = Rationalize[Arg[Chop[fsym[Sequence @@ #]& /@ flist , 10^(-20)]]/Pi]//Union;
   
   fsymbolsallrealorimaginary = SubsetQ[{-1/2,0,1/2,1}, fsymbolarguements];
   
   If[fsymsreal && fsymsFTFone,
    Print["The F-matrices are orthogonal."]
    ];
   
   If[fsymsreal && Not[fsymsFTFone],
    Print["The F-matrices are real."];
    Print["The F-matrices do not all satisfy F.(F^T) = (F^T).F = 1. One should check if this is reasonalbe or not!"];
    ];
   
   If[Not[fsymsreal] && fsymsFTFone,
    Print["The F-matrices are not all real."];
    Print["The F-matrices satisfy F.(F^T) = (F^T).F = 1"];
    ];
   
   If[Not[fsymsreal] && Not[fsymsFTFone],
    Print["The F-matrices are not all real."];
    Print["The F-matrices do not all satisfy F.(F^T) = (F^T).F = 1. One should check if this is reasonalbe or not!"];
    ];
    
  If[Not[fsymsreal] && fsymbolsallrealorimaginary,
   Print["The F-symbols are all real or purely imaginary."];
    ];  

  If[fsymsreal,
   Print["The F-symbols are all real or purely imaginary (all real, in fact)."];
    ];  

        
    If[(pentagontobechecked && pentholds && Not[pentundecidable]) || Not[pentagontobechecked],
     fsymbolscalculated = True;
     If[Not[recheck],
      Print["You can proceed to calculate the R-symbols :-)"];
     ];
    ];
   
   ];      
   
   
calculatefsymbols[] :=
 With[{},
  
  If[typeranklevelrootinitok,
  
  (* Generate the states for all the weights in all the irreps at this level, uniform or non-uniform case *)
  Print["Initializing the weight spaces..."];
  initializeweightspaces[irreps];
  
  Print["Constructing the bases for the weightspaces..."];
  initializeweightspacebasis[];
  
  Print["Constructing the fusion rules..."];
  generatefusionrules[];
  
  Print["Constructing the bases for the tensorproduct weightspaces..."];
  generatehwstates[];
  generateqcgcoefficients[];
  
  Print["Calculating the ", Length[flist], " F-symbols from the ", 
   numqCGcoef, " q-CG coefficients..."];
   
  constructfsymbols[];
   
  checkpentagon[]; 
  
  Print["Done :-)"];
  
  ,
  Print["The type of algebra, rank, level and/or rootfactor were not (correctly) initialized, please do so first!"];
  ,
  Print["The type of algebra, rank, level and/or rootfactor were not (correctly) initialized, please do so first!"];
  ];

  
  ];   


(* ::Subsection::Closed:: *)
(*Routines to calculate the R - symbols*)


constructrsymbols[] := Module[
   {hw1, hw2, hw3, currtpstates, curr1stweights, goodpositions, 
    temphex, goodposonesign, signeqleft, goodsignvars, 
    goodsignvarsunion, goodsignfirstpos, solsign, goodposonephase, 
    phaseeqleft, goodphasevars, goodphasevarsunion, goodphasefirstpos,
     solphase, firstlinpos, lineqleft},
   
   
   Do[
    hw1 = rlist[[i, 1]];
    hw2 = rlist[[i, 2]];
    hw3 = rlist[[i, 3]];
    
    If[
     
     nv[hw1, hw2, hw3] == 1,
     
     currtpstates = tpstates[hw1, hw2, hw3, hw3];
     curr1stweights = currtpstates[[All, 1, 1]];
     
     goodpositions = Position[
        Table[
         Or @@ Table[MemberQ[curr1stweights, curr1stweights[[i]] - cartan[[j]]], {j, 1, rank}]
         , {i, 1, Length[curr1stweights]}]
        , False] // Flatten;
     
     
     If[(curr1stweights[[goodpositions]] // Union // Length) != 1, 
      Print["There is more than one different minimal weight. The \
last one is being used, and no check is done if the other(s) give the \
same result or not... If the hexagon equations are not satisfied, \
this issue should be investigated further!"]];
     
     goodpositions = 
      Position[curr1stweights, curr1stweights[[goodpositions[[-1]]]]] // Flatten;
     
     If[Length[goodpositions] == 1,
      rsym[rlist[[i, 1]], rlist[[i, 2]], rlist[[i, 3]], rlist[[i, 4]]] = 
        q^(tmax/2 currtpstates[[goodpositions[[1]], 1, 1]].qfm.currtpstates[[goodpositions[[1]], 2, 1]])*
         qcg[hw1, currtpstates[[goodpositions[[1]], 1]], hw2, currtpstates[[goodpositions[[1]], 2]], hw3, {hw3, 1}, rlist[[i, 4, 1]]]/
          qcg[hw2, currtpstates[[goodpositions[[1]], 2]], hw1, currtpstates[[goodpositions[[1]], 1]], hw3, {hw3, 1}, rlist[[i, 4, 1]]];
      ,
      rsym[rlist[[i, 1]], rlist[[i, 2]], rlist[[i, 3]], rlist[[i, 4]]] =
        sign[rlist[[i, 1]], rlist[[i, 2]], rlist[[i, 3]], rlist[[i, 4]]]*
         q^(tmax/2 currtpstates[[goodpositions[[1]], 1, 1]].qfm.currtpstates[[goodpositions[[1]], 2, 1]])*
         Sqrt[
           Sum[(qcg[hw1, currtpstates[[pos, 1]], hw2, currtpstates[[pos, 2]], hw3, {hw3, 1}, rlist[[i, 4, 1]]])^2, {pos, goodpositions}]]/
          Sqrt[Sum[(qcg[hw2, currtpstates[[pos, 2]], hw1, currtpstates[[pos, 1]], hw3, {hw3, 1}, rlist[[i, 4, 1]]])^2, {pos, goodpositions}]];
      ];
     
     ,
     
     Do[
       rsym[hw1, hw2, hw3, {v1, v2}] = phase[hw1, hw2, hw3, {v1, v2}];
       
       , {v1, 1, nv[hw1, hw2, hw3]}, {v2, 1, nv[hw1, hw2, hw3]}];
     
     ];
    
    , {i, 1, Length[rlist]}];
   
   
   temphex =
    DeleteCases[
     Table[
      
      Chop[(Sum[rsym[i[[1]], i[[2]], i[[5]], {i[[7, 1]], v8}]*
           fsym[i[[1]], i[[2]], i[[3]], i[[4]], i[[5]], i[[6]], {v8, i[[7, 2]], v9, i[[7, 4]]}]*
           rsym[i[[3]], i[[2]], i[[6]], {v9, i[[7, 3]]}]
           ,{v8, nv[i[[1]], i[[2]], i[[5]]]}, {v9, nv[i[[3]], i[[2]], i[[6]]]}]
         -
         Sum[fsym[i[[2]], i[[1]], i[[3]], i[[4]], i[[5]], j, {i[[7, 1]], i[[7, 2]], v5, v6}]*
           rsym[j, i[[2]], i[[4]], {v6, v7}]*
           fsym[i[[1]], i[[3]], i[[2]], i[[4]], j, i[[6]], {v5, v7, i[[7, 3]], i[[7, 4]]}]
          , {j, Cases[irreps, x_ /; MemberQ[fusion[i[[1]], i[[3]]], x] &&
              MemberQ[fusion[i[[2]], x], i[[4]]]]}
              , {v5, nv[i[[1]], i[[3]], j]}, {v6, nv[i[[2]], j, i[[4]]]}, {v7,  nv[i[[2]], j, i[[4]]]}]) // Expand , 10^(-20) ]
      , {i, flist}]
     , 0];
      
   goodposonesign = Position[temphex, x_Complex + c1_Complex sign[y1___], {1} , Heads -> False] // Flatten;
   
   If[Length[goodposonesign] > 0, signeqleft = True];
   
   While[
    signeqleft
    ,
    signeqleft = False;
    goodsignvars = Variables /@ temphex[[goodposonesign]] // Flatten;
    goodsignvarsunion = Union[goodsignvars];
    goodsignfirstpos = Table[Position[goodsignvars, currvar, {1}, 1][[1, 1]], {currvar, goodsignvarsunion}];
    solsign = 
     Solve[Table[
       temphex[[goodposonesign[[i]]]] == 0, {i, 1, Length[goodposonesign]}]];
    
    If[Length[solsign] != 1, Print["There is more than one solution for the signs! Something went wrong!"]];
    solsign = Chop[ solsign[[1]] , 10^(-20) ];
    
    Do[
    rsym[Sequence @@ rlist[[i]]] = Chop[ rsym[Sequence @@ rlist[[i]]] /. solsign , 10^(-20)];
    , {i, 1, Length[rlist]}];
    
    temphex =
     DeleteCases[
      Table[
       
       Chop[(Sum[rsym[i[[1]], i[[2]], i[[5]], {i[[7, 1]], v8}]*
            fsym[i[[1]], i[[2]], i[[3]], i[[4]], i[[5]], i[[6]], {v8, i[[7, 2]], v9, i[[7, 4]]}]*
            rsym[i[[3]], i[[2]], i[[6]], {v9, i[[7, 3]]}]
            , {v8, nv[i[[1]], i[[2]], i[[5]]]}, {v9, 
            nv[i[[3]], i[[2]], i[[6]]]}]
          -
          Sum[fsym[i[[2]], i[[1]], i[[3]], i[[4]], i[[5]], j, {i[[7, 1]], i[[7, 2]], v5, v6}]*
            rsym[j, i[[2]], i[[4]], {v6, v7}]*
            fsym[i[[1]], i[[3]], i[[2]], i[[4]], j, i[[6]], {v5, v7, i[[7, 3]], i[[7, 4]]}]
           , {j, Cases[irreps,  x_ /; MemberQ[fusion[i[[1]], i[[3]]], x] &&
             MemberQ[fusion[i[[2]], x], i[[4]]]]}
           , {v5, nv[i[[1]], i[[3]], j]}, {v6, nv[i[[2]], j, i[[4]]]}, {v7, nv[i[[2]], j, i[[4]]]}]) // Expand, 10^(-20) ]
       
       , {i, flist}]
      
      , 0];
    
    goodposonesign = Position[temphex, x_Complex + c1_Complex sign[y1___], {1} , Heads -> False] // Flatten;
    
    If[Length[goodposonesign] > 0, signeqleft = True];
    
    ];
   
   If[
    MemberQ[Table[rsym[Sequence @@ rs], {rs, rlist}], sign[___]],
    Print["There are variables of type sign[___] left, but no linear equations with just one sign[___] variable."];
    ];
   
   goodposonephase = 
    Position[temphex, x_Complex + c1_Complex phase[y1___], {1} , Heads -> False] // Flatten;
   
   If[Length[goodposonephase] > 0, phaseeqleft = True];
   
   While[
    phaseeqleft
    ,
    phaseeqleft = False;
    goodphasevars = Variables /@ temphex[[goodposonephase]] // Flatten;
    goodphasevarsunion = Union[goodphasevars];
    goodphasefirstpos = Table[Position[goodphasevars, currvar, {1}, 1][[1, 1]], {currvar, goodphasevarsunion}];
    
    solphase = 
     Solve[Table[
       temphex[[goodposonephase[[i]]]] == 0, {i, 1, Length[goodposonephase]}]];
    
    If[Length[solphase] != 1, 
     Print["There is more than one solution for the phases! Something went wrong!"]];
    solphase = Chop[ solphase[[1]] , 10^(-20) ];
    
    Do[rsym[Sequence @@ rlist[[i]]] = Chop[ rsym[Sequence @@ rlist[[i]]] /. solphase , 10^(-20) ];
    , {i, 1, Length[rlist]}];
    
    temphex =
     DeleteCases[
      Table[
       Chop[(Sum[rsym[i[[1]], i[[2]], i[[5]], {i[[7, 1]], v8}]*
            fsym[i[[1]], i[[2]], i[[3]], i[[4]], i[[5]], i[[6]], {v8, i[[7, 2]], v9, i[[7, 4]]}]*
            rsym[i[[3]], i[[2]], i[[6]], {v9, i[[7, 3]]}]
            , {v8, nv[i[[1]], i[[2]], i[[5]]]}, {v9, nv[i[[3]], i[[2]], i[[6]]]}]
          -
          Sum[fsym[i[[2]], i[[1]], i[[3]], i[[4]], i[[5]], j, {i[[7, 1]], i[[7, 2]], v5, v6}]*
            rsym[j, i[[2]], i[[4]], {v6, v7}]*
            fsym[i[[1]], i[[3]], i[[2]], i[[4]], j, i[[6]], {v5, v7, i[[7, 3]], i[[7, 4]]}]
           , {j, Cases[irreps,  x_ /; MemberQ[fusion[i[[1]], i[[3]]], x] &&
             MemberQ[fusion[i[[2]], x], i[[4]]]]}
           , {v5, nv[i[[1]], i[[3]], j]}, {v6, nv[i[[2]], j, i[[4]]]}, {v7, nv[i[[2]], j, i[[4]]]}]) // Expand , 10^(-20) ]
       
       , {i, flist}]
      
      , 0];
    
    goodposonephase = 
     Position[temphex, x_Complex + c1_Complex phase[y1___], {1} , Heads -> False] // Flatten;
    
    If[Length[goodposonephase] > 0, phaseeqleft = True];
    
    ];
   
   
   firstlinpos = 
    Position[temphex, x_ /; FreeQ[x, phase[___]^2, Infinity], 1, 1, Heads -> False] // Flatten;
   
   If[temphex != {},
    If[
      firstlinpos == {},
      Print["There are variables phase[___] left, but no linear equations!"];,
      firstlinpos = firstlinpos[[1]];
      lineqleft = True;
      ];
    ];
   
   While[
    lineqleft,
    
    lineqleft = False;
    
    solphase = 
     Solve[temphex[[firstlinpos]] == 0, 
      Variables[temphex[[firstlinpos]]][[1]]];
    If[Length[solphase] != 1, 
     Print["There is more than one solution for the phases! Something went wrong!"]];
    solphase = Chop[ solphase[[1]] , 10^(-20) ];
    
    Do[rsym[Sequence @@ rlist[[i]]] = Chop[ rsym[Sequence @@ rlist[[i]]] /. solphase , 10^(-20) ];
    , {i, 1, Length[rlist]}];
    
    temphex =
     DeleteCases[
      Table[
       Chop[(Sum[rsym[i[[1]], i[[2]], i[[5]], {i[[7, 1]], v8}]*
             fsym[i[[1]], i[[2]], i[[3]], i[[4]], i[[5]], i[[6]], {v8, i[[7, 2]], v9, i[[7, 4]]}]*
             rsym[i[[3]], i[[2]], i[[6]], {v9, i[[7, 3]]}]
             , {v8, nv[i[[1]], i[[2]], i[[5]]]}, {v9,  nv[i[[3]], i[[2]], i[[6]]]}]
           -
           Sum[fsym[i[[2]], i[[1]], i[[3]], i[[4]], i[[5]], j, {i[[7, 1]], i[[7, 2]], v5, v6}]*
             rsym[j, i[[2]], i[[4]], {v6, v7}]*
             fsym[i[[1]], i[[3]], i[[2]], i[[4]], j, i[[6]], {v5, v7, i[[7, 3]], i[[7, 4]]}]
            , {j, Cases[irreps, x_ /; MemberQ[fusion[i[[1]], i[[3]]], x] &&
              MemberQ[fusion[i[[2]], x], i[[4]]]]}
            , {v5, nv[i[[1]], i[[3]], j]}, {v6, nv[i[[2]], j, i[[4]]]}, {v7, nv[i[[2]], j, i[[4]]]}]) // Expand , 10^(-20) ]
       
       , {i, flist}]
      
      , 0];
    
    firstlinpos = Position[temphex, x_ /; FreeQ[x, phase[___]^2, Infinity], 1, 1, Heads -> False] // Flatten;
    If[temphex != {},
     If[
       firstlinpos == {},
       Print["There are variables phase[___] left, but no linear equations!"];,
       firstlinpos = firstlinpos[[1]];
       lineqleft = True;
       ];
     ];
    
    
    ];

   Do[
    rsym[Sequence @@ rs] = N[Chop[rsym[Sequence @@ rs], 10^(-20)], precision];
   , {rs, rlist}];

         
   (* Calculating the inverses of the R-symbols, 
   NOT assuming that the R-matrices are unitairy *)
   Do[
    rmat[Sequence @@ rm] = 
     Table[rsym[Sequence @@ rm[[1 ;; 3]], {v1, v2}]
     , {v1, nv[Sequence @@ rm[[1 ;; 3]]]}, {v2, nv[Sequence @@ rm[[1 ;; 3]]]}];
    rmatinv[Sequence @@ rm] = N[Chop[rmat[Sequence @@ rm] // Inverse, 10^(-20)], precision];
    Do[
     rsyminv[Sequence @@ rm, {v1, v2}] = rmatinv[Sequence @@ rm][[v1, v2]];
     , {v1, 1, nv[Sequence @@ rm]}, {v2, 1, nv[Sequence @@ rm]}];
   , {rm, rmatlist}];
   
   
   rmatunitary = And @@ Table[
       ( Chop[ rmat[Sequence @@ rm].Conjugate[Transpose[rmat[Sequence @@ rm]]] - IdentityMatrix[nv[Sequence @@ rm]] , 10^(-20) ] // Flatten // Union) == {0}
       , {rm, rmatlist}] &&
       And @@ Table[(Chop[ Conjugate[Transpose[rmat[Sequence @@ rm]]].rmat[Sequence @@ rm] - IdentityMatrix[nv[Sequence @@ rm]] , 10^(-20) ] // Flatten // Union) == {0}
       , {rm, rmatlist}];
   
   rmatdiagonal = And @@ Table[
      (Chop[ (rmat[Sequence @@ rm] - DiagonalMatrix[Diagonal[rmat[Sequence @@ rm]]]) , 10^(-20) ] // Flatten // Union) == {0}
      , {rm, rmatlist}];
   
   (* Calculating the eigenvalues of the R-matrices, 
   to check if they can be made unitary if they are not already *)
   
   rmatevallist = Table[
       N[rmat[Sequence @@ rm] , precision ]// Eigenvalues
       , {rm, rmatlist}] // Flatten;
   
   rmatevalabslist = Abs /@ rmatevallist;
   
   If[
    (Chop[ (rmatevalabslist - 1) , 10^(-20) ] // Union) == {0},
    rmatevalsarephases = True;,
    rmatevalsarephases = False;
    ];
   
   rmatevalarglist = Rationalize[1/Pi (Arg /@ rmatevallist)];
   rmatevalarglistunion = rmatevalarglist // Union;
   
   rmatevalargmaxdenom = (Denominator /@ rmatevalarglist) // Max;
   
   
   
   
   ];(* End of constructrsymbols[] *)


checkhexagon[] := Module[{maxdev, tempdev, maxdevhex, maxdevhexinv},
   
   hexholds = True;
   hexrundecidable = False;
   hexrinvundecidable = False;
   maxdev = 0;
   
   Do[
    
      tempdev = Abs[
        Sum[rsym[i[[1]], i[[2]], i[[5]], {i[[7, 1]], v8}]*
           fsym[i[[1]], i[[2]], i[[3]], i[[4]], i[[5]], i[[6]], {v8, i[[7, 2]], v9, i[[7, 4]]}]*
           rsym[i[[3]], i[[2]], i[[6]]
           , {v9, i[[7, 3]]}], {v8, nv[i[[1]], i[[2]], i[[5]]]}, {v9, nv[i[[3]], i[[2]], i[[6]]]}]
           -
         Sum[fsym[i[[2]], i[[1]], i[[3]], i[[4]], i[[5]], j, {i[[7, 1]], i[[7, 2]], v5, v6}]*
           rsym[j, i[[2]], i[[4]], {v6, v7}]*
           fsym[i[[1]], i[[3]], i[[2]], i[[4]], j, i[[6]], {v5, v7, i[[7, 3]], i[[7, 4]]}]
          , {j, Cases[irreps, x_ /; MemberQ[fusion[i[[1]], i[[3]]], x] &&
            MemberQ[fusion[i[[2]], x], i[[4]]]]}
          , {v5, nv[i[[1]], i[[3]], j]}, {v6, nv[i[[2]], j, i[[4]]]}, {v7, nv[i[[2]], j, i[[4]]]}]
        
        ];

      (*If[tempdev > maxdev, maxdev = tempdev];*)
      (*
      Max deals better with high precision numbers than > (Greater).
      This caused maxdev to remain 0, even if it should have been f.i. 0``73.93384483647623 .
      *)
      If[NumberQ[tempdev],
      maxdev=Max[maxdev,tempdev];,
      If[Not[hexrundecidable], 
        Print["At least one of the hexagon equations for R is not decidable! Something went wrong :-("];
        hexrundecidable = True;];
      ];         
    
    , {i, flist}];
    
    hexagonRdeviation = maxdev;
    maxdev = 0;
   
   (* Checking the hexagon for the inverses of the R-matrices, 
   NOT assuming the R-matrices are unitary *)
   Do[
    
    tempdev = Abs[
        Sum[rsyminv[i[[2]], i[[1]], i[[5]], {i[[7, 1]], v8}]*
           fsym[i[[1]], i[[2]], i[[3]], i[[4]], i[[5]], i[[6]], {v8, i[[7, 2]], v9, i[[7, 4]]}]*
           rsyminv[i[[2]], i[[3]], i[[6]], {v9, i[[7, 3]]}]
           , {v8, nv[i[[1]], i[[2]], i[[5]]]}, {v9, nv[i[[3]], i[[2]], i[[6]]]}]
           -
         Sum[fsym[i[[2]], i[[1]], i[[3]], i[[4]], i[[5]], j, {i[[7, 1]], i[[7, 2]], v5, v6}]*
           rsyminv[i[[2]], j, i[[4]], {v6, v7}]*
           fsym[i[[1]], i[[3]], i[[2]], i[[4]], j, i[[6]], {v5, v7, i[[7, 3]], i[[7, 4]]}]
          , {j, Cases[irreps, x_ /; MemberQ[fusion[i[[1]], i[[3]]], x] &&
            MemberQ[fusion[i[[2]], x], i[[4]]]]}
            , {v5, nv[i[[1]], i[[3]], j]}, {v6, nv[i[[2]], j, i[[4]]]}, {v7, nv[i[[2]], j, i[[4]]]}]
        ];
        
        
      (*If[tempdev > maxdev, maxdev = tempdev];*)
      (*
      Max deals better with high precision numbers than > (Greater).
      This caused maxdev to remain 0, even if it should have been f.i. 0``73.93384483647623 .
      *)
      If[NumberQ[tempdev],
      maxdev=Max[maxdev,tempdev];,
      If[Not[hexrinvundecidable], 
        Print["At least one of the hexagon equations for R-inverse is not decidable! Something went wrong :-("];
        hexrinvundecidable = True;];
      ];         

   , {i, flist}];
   
   
   hexagonRinversedeviation = maxdev;
   hexagondeviation = Max[hexagonRdeviation,hexagonRinversedeviation];
   
   If[
     Max[hexagondeviation,10^(-20)] == hexagondeviation,
     hexholds = False; 
    ];
   
   If[hexholds && Not[hexrundecidable] && Not[hexrinvundecidable],
    Print["The hexagon equations are satisfied :-)"];
    Print["The maximum devitation is: ", hexagondeviation];
    rsymbolscalculated = True;,
    Print["The hexagon equations are not satisfied :-(, the maximum deviation is: ", maxdev, " something went wrong..."];
    ];
   
   If[
    rmatunitary,
    Print["The R-matrices are unitary."];
    If[
     rmatdiagonal,
     Print["The R-matrices are diagonal."];
     Print["Thus all the (non-zero) R-symbols are phases. The largest denominator s in R^{a,b}_c = e^(i\[Pi] r/s) one encounters is: ", rmatevalargmaxdenom];
     ,
     Print["The R-matrices are not all diagonal."];
     Print["All the eigenvalues of the R-matrices are phases, because they are unitary. The largest denominator s in R^{a,b}_c = e^(i\[Pi] r/s) one encounters after diagonalization is: ", 
       rmatevalargmaxdenom];
     If[pentagontobechecked,
      Print["The R-matrices can be diagonalized by running diagonalizermatrices[].\n This will change both F- and R-symbols, so by running diagonalizermatrices[], the pentagon and hexagon equations will be checked again."];,
      Print["The R-matrices can be diagonalized by running diagonalizermatrices[].\n This will change both F- and R-symbols, so by running diagonalizermatrices[], the hexagon equations will be checked again, but not the pentagon equations, because you opted not to do so."];
      ];
     ];
    ];
   
   If[
    Not[rmatunitary],
    Print[
     "The R-matrices are not all unitary (at least in the basis used here)."];
    If[rmatevalsarephases,
     Print["All the eigenvalues of the R-matrices are phases. The largest denominator s in R^{a,b}_c = e^(i\[Pi] r/s) one encounters after diagonalization is: ", 
       rmatevalargmaxdenom];
     If[pentagontobechecked,
      Print["The R-matrices can be diagonalized by running diagonalizermatrices[].\n This will change both F- and R-symbols, so by running diagonalizermatrices[], the pentagon and hexagon equations will be checked again."];,
      Print["The R-matrices can be diagonalized by running diagonalizermatrices[].\n This will change both F- and R-symbols, so by running diagonalizermatrices[], the hexagon equations will be checked again, but not the pentagon equations, because you opted not to do so."];
     ];
     ,
     Print["NOT all the eigenvalues of the R-matrices are phases. One perhaps should check if one can choose a different gauge where they are!"]
     ];
    ];
    
    If[hexholds && Not[hexrundecidable] && Not[hexrinvundecidable] && Not[recheck],
    Print["You can proceed to calculate the modular data :-)"];
    ];    
             
   ];(* End of checkhexagon[] *)   


calculatersymbols[] := With[{},
   
   If[Not[typeranklevelrootinitok],
   Print["The type of algebra, rank, level and/or rootfactor were not \
(correctly) initialized, please do so first, followed by calculating \
the F-symbols, before calculating the R-symbols."];
   ];

  If[typeranklevelrootinitok && Not[fsymbolscalculated],
  Print["The F-symbols were not calculated. Please do so first, \
before calculating the R-symbols."];
  ];

   If[typeranklevelrootinitok && fsymbolscalculated,
   
   Print["Constructing the R-symbols..."];
   
   constructrsymbols[];
   
   Print["Checking the ", 2*Length[flist]," hexagon equations..."];
   
   checkhexagon[];
   
   Print["Done :-)"];
   ];
   
  ];


(* ::Subsection::Closed:: *)
(*Routine to diagonalize the R - matrices*)


diagonalizermatrices[] := Module[{tempmat,tempmatinv,rmatdiagonallist,rmatnondiagonallist,rmatnondiagonallistswap,rmatnondiagonallistreduced,rmatnondiagonallistsymmetric},

 If[typeranklevelrootinitok && fsymbolscalculated && rsymbolscalculated,
   
   If[Not[rmatdiagonal],
   
   Print["Diagonalizing the R-matrices might lead to values for the F-symbols that \
can not be described in terms of the general number field that is used for the \
exact representation for the F- and R-symbols. Thus, if one afterwards wants to obtain the \
exact representation of the F- and/or R-symbols, the original, non-diagonalized form \
should be used! One can `undiagonalize' the R-matrices by running undiagonalizermatrices[]."];
   
   Print["Diagonalizing the R-matrices..."];
   
   rmatricesdiagonalized = True;
   recheck = True;
   
   rmatdiagonallist = DeleteCases[Table[
      If[ Chop[ rmat[Sequence @@ rm] , 10^(-20) ] // DiagonalMatrixQ , rm]
      , {rm, rmatlist}], Null];
      
   rmatnondiagonallist = DeleteCases[Table[
      If[Not[ Chop[ rmat[Sequence @@ rm] , 10^(-20) ] // DiagonalMatrixQ], rm]
      , {rm, rmatlist}], Null];
   
   rmatnondiagonallistswap =
   Table[{rm[[2]], rm[[1]], rm[[3]]}, {rm, rmatnondiagonallist}] // Sort;
   
   If[
   Not[rmatnondiagonallist == rmatnondiagonallistswap],
   Print["There is a non-diagonal R-matrix R^{a,b}_c, such that R^{b,a}_c is diagonal. \
This means that the R-matrices can not all be diagonalized at the same time. \
Something seems to have gone wrong."];
   ];
   
   rmatnondiagonallistreduced = 
    Cases[rmatnondiagonallist, x_ /; x[[1]] != x[[2]] && OrderedQ[x[[1 ;; 2]]]];
   
   rmatnondiagonallistsymmetric =
    Cases[rmatnondiagonallist, x_ /; x[[1]] == x[[2]]];
   
   Do[
    umat[Sequence @@ rm] = IdentityMatrix[nv[Sequence @@ rm]];
    umatinv[Sequence @@ rm] = umat[Sequence @@ rm];
    , {rm, rmatdiagonallist}];
   

    Do[
    umat[Sequence @@ rm] = (Chop[ rmat[Sequence @@ rm] // Eigensystem , 10^(-20) ])[[2]] // Transpose;
    umatinv[Sequence @@ rm] = Inverse[umat[Sequence @@ rm]];
    , {rm, rmatnondiagonallistsymmetric}];
    
    Do[
    tempmat = (Chop[ rmat[Sequence @@ rm] // Eigensystem , 10^(-20) ])[[2]] // Transpose;
    tempmatinv = (Chop[ rmat[Sequence @@ rm] // Eigensystem , 10^(-20) ])[[2]] // Transpose // Inverse;
    umat[rm[[2]], rm[[1]], rm[[3]]] = tempmat;
    umatinv[rm[[2]], rm[[1]], rm[[3]]] = tempmatinv;
    
    umatinv[rm[[1]], rm[[2]], rm[[3]]] = tempmatinv;
    umat[rm[[1]], rm[[2]], rm[[3]]] = tempmat;
        
    , {rm, rmatnondiagonallistreduced}];
  
   Do[
    rsymold[Sequence @@ rs] = rsym[Sequence @@ rs]
    , {rs, rlist}];
   
   Do[
    fsymold[Sequence @@ fs] = fsym[Sequence @@ fs]
    , {fs, flist}];
   
   Do[
    rmatold[Sequence @@ rm] = rmat[Sequence @@ rm]
    , {rm, rmatlist}];
   
   Do[
    fmatold[Sequence @@ fm] = fmat[Sequence @@ fm]
    , {fm, fmatlist}];
   
   Do[
    rsym[Sequence @@ rs] = Sum[
       umatinv[rs[[1]], rs[[2]], rs[[3]]][[rs[[4, 1]], v1]]*
        rsymold[rs[[1]], rs[[2]], rs[[3]], {v1, v2}]*
        umat[rs[[2]], rs[[1]], rs[[3]]][[v2, rs[[4, 2]]]]
       , {v1, 1, nv[rs[[1]], rs[[2]], rs[[3]]]}, {v2, 1, nv[rs[[1]], rs[[2]], rs[[3]]]}];
    , {rs, rlist}];
   
   Do[
    fsym[Sequence @@ fs] = Sum[
      umatinv[fs[[1]], fs[[2]], fs[[5]]][[fs[[7, 1]], v1]]*
       umatinv[fs[[5]], fs[[3]], fs[[4]]][[fs[[7, 2]], v2]]*
       fsymold[fs[[1]], fs[[2]], fs[[3]], fs[[4]], fs[[5]], fs[[6]], {v1, v2, v3, v4}]*
       umat[fs[[2]], fs[[3]], fs[[6]]][[v3, fs[[7, 3]]]]*
       umat[fs[[1]], fs[[6]], fs[[4]]][[v4, fs[[7, 4]]]]
      , {v1, 1, nv[fs[[1]], fs[[2]], fs[[5]]]}
      , {v2, 1, nv[fs[[5]], fs[[3]], fs[[4]]]}
      , {v3, 1, nv[fs[[2]], fs[[3]], fs[[6]]]}
      , {v4, 1, nv[fs[[1]], fs[[6]], fs[[4]]]}]
    , {fs, flist}];
   
   Do[
    fmat[Sequence @@ fm] =
      Flatten[
       Table[
        fsym[fm[[1]], fm[[2]], fm[[3]], fm[[4]], e,  f, {v1, v2, v3, v4}]
        ,{e, Cases[fusion[fm[[1]], fm[[2]]], x_ /; MemberQ[fusion[x, fm[[3]]], fm[[4]]]]}
        ,{f, Cases[fusion[fm[[2]], fm[[3]]], x_ /; MemberQ[fusion[fm[[1]], x], fm[[4]]]]}
        ,{v1, nv[fm[[1]], fm[[2]], e]}
        ,{v2, nv[e, fm[[3]], fm[[4]]]}
        ,{v3, nv[fm[[2]], fm[[3]], f]}
        ,{v4, nv[fm[[1]], f, fm[[4]]]}
        ]
       , {{1, 3, 4}, {2, 5, 6}}];
    , {fm, fmatlist}];
     
   Do[
    rmat[Sequence @@ rm] = 
     Table[rsym[Sequence @@ rm[[1 ;; 3]], {v1, v2}]
     , {v1, nv[Sequence @@ rm[[1 ;; 3]]]}
     , {v2, nv[Sequence @@ rm[[1 ;; 3]]]}];
    
    rmatinv[Sequence @@ rm] = rmat[Sequence @@ rm] // Inverse;
    
    Do[
     rsyminv[Sequence @@ rm, {v1, v2}] = 
       rmatinv[Sequence @@ rm][[v1, v2]];
     , {v1, 1, nv[Sequence @@ rm]}, {v2, 1, nv[Sequence @@ rm]}];
    
    , {rm, rmatlist}];
   
   
   rmatunitary = And @@ Table[
       ( Chop[ rmat[Sequence @@ rm].Conjugate[Transpose[rmat[Sequence @@ rm]]] - IdentityMatrix[nv[Sequence @@ rm]] , 10^(-20) ] // Flatten // Union) == {0}
       , {rm, rmatlist}] &&
       And @@ Table[(Chop[ Conjugate[Transpose[rmat[Sequence @@ rm]]].rmat[Sequence @@ rm] - IdentityMatrix[nv[Sequence @@ rm]] , 10^(-20) ] // Flatten // Union) == {0}
       , {rm, rmatlist}];
   
   rmatdiagonal = And @@ Table[
      (Chop[ (rmat[Sequence @@ rm] - DiagonalMatrix[Diagonal[rmat[Sequence @@ rm]]]) , 10^(-20) ] // Flatten // Union) == {0}
      , {rm, rmatlist}];
   
   rmatevallist = Table[
       N[rmat[Sequence @@ rm] , precision ]// Eigenvalues
       , {rm, rmatlist}] // Flatten;
   
   rmatevalabslist = Abs /@ rmatevallist;
   
   If[
    (Chop[ (rmatevalabslist - 1) , 10^(-20) ] // Union) == {0},
    rmatevalsarephases = True;,
    rmatevalsarephases = False;
    ];
   
   rmatevalarglist = Rationalize[1/Pi (Arg /@ rmatevallist)];
   rmatevalarglistunion = rmatevalarglist // Union;
   
   rmatevalargmaxdenom = (Denominator /@ rmatevalarglist) // Max;
   
   checkpentagon[];
   
   Print["Re-checking the ", 2*Length[flist]," hexagon equations..."];
   checkhexagon[];
   
   If[
    (Not[pentagontobechecked] || (pentholds && Not[pentundecidable])) && hexholds && Not[hexrundecidable] && Not[hexrinvundecidable],
    Clear[umat, umatinv];
    ];
   
   Print["Done :-)"];
   
   ,
   Print["The R-matrices are already diagonal, so there's no need to diagonalize them."];
   Print["Done :-)"];
   ];
   ,
   Print["The R-symbols have not been calculated, please do so first!"];
   
   ];
   
   ];


undiagonalizermatrices[] :=
  With[{},
  
    If[Not[typeranklevelrootinitok && fsymbolscalculated && rsymbolscalculated],
    Print["The R-symbols were not calculated, so the R-matrices were not \
diagonalized either."];
    ];
  
     If[typeranklevelrootinitok && fsymbolscalculated && rsymbolscalculated &&
       Not[rmatricesdiagonalized],
    Print["The R-matrices were not diagonalized, \
so there is no need to undiagonalize them."];
    ];
   
   
   If[typeranklevelrootinitok && fsymbolscalculated && rsymbolscalculated && 
     rmatricesdiagonalized,
    Print["The R-matrices will be brought back to their original form, \
in which not all R-matrices are diagonal. This will revert the \
F- and R-symbols to their original values."];
    
    Do[
     rsym[Sequence @@ rs] = rsymold[Sequence @@ rs]
     , {rs, rlist}];
    
     Do[
      fsym[Sequence @@ fs] = fsymold[Sequence @@ fs]
     , {fs, flist}];
    
     Do[
     rmat[Sequence @@ rm] = rmatold[Sequence @@ rm]
     , {rm, rmatlist}];
    
     Do[
     fmat[Sequence @@ fm] = fmatold[Sequence @@ fm]
     , {fm, fmatlist}];
    
    Do[
     rmatinv[Sequence @@ rm] = rmat[Sequence @@ rm] // Inverse;
     
     Do[
      rsyminv[Sequence @@ rm, {v1, v2}] = 
        rmatinv[Sequence @@ rm][[v1, v2]];
      , {v1, 1, nv[Sequence @@ rm]}, {v2, 1, nv[Sequence @@ rm]}];
     
     , {rm, rmatlist}];
    
    rmatricesdiagonalized = False;

    rmatunitary = And @@ Table[
       ( Chop[ rmat[Sequence @@ rm].Conjugate[Transpose[rmat[Sequence @@ rm]]] - IdentityMatrix[nv[Sequence @@ rm]] , 10^(-20) ] // Flatten // Union) == {0}
       , {rm, rmatlist}] &&
       And @@ Table[(Chop[ Conjugate[Transpose[rmat[Sequence @@ rm]]].rmat[Sequence @@ rm] - IdentityMatrix[nv[Sequence @@ rm]] , 10^(-20) ] // Flatten // Union) == {0}
       , {rm, rmatlist}];
   
    rmatdiagonal = And @@ Table[
      (Chop[ (rmat[Sequence @@ rm] - DiagonalMatrix[Diagonal[rmat[Sequence @@ rm]]]) , 10^(-20) ] // Flatten // Union) == {0}
      , {rm, rmatlist}];
   
    rmatevallist = Table[
       N[rmat[Sequence @@ rm] , precision ]// Eigenvalues
       , {rm, rmatlist}] // Flatten;
   
    rmatevalabslist = Abs /@ rmatevallist;
   
    If[
     (Chop[ (rmatevalabslist - 1) , 10^(-20) ] // Union) == {0},
     rmatevalsarephases = True;,
     rmatevalsarephases = False;
    ];
   
    rmatevalarglist = Rationalize[1/Pi (Arg /@ rmatevallist)];
    rmatevalarglistunion = rmatevalarglist // Union;
   
    rmatevalargmaxdenom = (Denominator /@ rmatevalarglist) // Max;
    
    Clear[rsymold, fsymold, rmatold, fmatold];
    
    Print["Done :-)"];
    
    ];
   
   ];


(* ::Subsection::Closed:: *)
(*Command to calculate the modular data*)


calculatemodulardata[] := Module[{qd,pivot,theta},
  
  If[Not[typeranklevelrootinitok],
  Print["The type of algebra, rank, level and/or rootfactor were not \
(correctly) initialized, please do so first, followed by calculating \
the F- and R-symbols, before calculating the modular data."];
  ];

If[typeranklevelrootinitok && Not[fsymbolscalculated],
  Print["The F-symbols were not calculated. Please do so first, \
followed by calculating the R-symbols, before calculating the modular \
data."];
  ];

If[typeranklevelrootinitok && fsymbolscalculated && 
   Not[rsymbolscalculated],
  Print["The R-symbols were not calculated. Please do so first, \
before calculating the modular data."];
  ];
  
  If[typeranklevelrootinitok && fsymbolscalculated && rsymbolscalculated,

  
  numposroots = Position[roots, Table[0, {i, 1, rank}], 1][[1, 1]] - 1;
  posroots = roots[[1 ;; numposroots]];
      
    qdimvec = Table[
     N[Chop[Product[
       nq[tmax (ir + rho).qfm.posroots[[pr]], 1] / nq[ tmax (rho).qfm.posroots[[pr]], 1]
       , {pr, 1, numposroots}], 10^(-20)], precision]
    , {ir, irreps}];
    
  Do[
   qd[irreps[[i]]] = qdimvec[[i]]
   , {i, 1, numofirreps}];
   
  qdtot2 = Sum[qdimvec[[i]]^2, {i, 1, numofirreps}];
  qdtot = Sqrt[qdtot2];
  qdimspositive = And @@ (# > 0 & /@ qdimvec);
  
  fpdimvec = 
   Table[
    (Cases[Chop[ (N[ nmat[ir] , precision ] // Eigenvalues) , 10^(-(Max[10, precision - 20])) ], x_ /; x \[Element] Reals] // Sort)[[-1]]
   , {ir, irreps}];
  
  qdim1overfvec = 
   Table[
    N[ 1/fsym[irreps[[i]], irrepsdual[[i]], irreps[[i]], irreps[[i]], irreps[[1]], irreps[[1]], {1, 1, 1, 1}] , precision ]
   , {i, 1, numofirreps}];
  
  pivotlist = 
   Table[qdimvec[[i]]/qdim1overfvec[[i]], {i, 1, numofirreps}];

  Do[pivot[irreps[[i]]] = pivotlist[[i]], {i, 1, numofirreps}];

  
  pivoteqnok = (Table[
        Chop[ N[ pivot[ir1] pivot[ir2]/pivot[ir3] -
           Sum[fsym[ir1, ir2, dual[ir3], irreps[[1]], ir3, dual[ir1], {v1, 1, v2, 1}]*
             fsym[ir2, dual[ir3], ir1, irreps[[1]], dual[ir1], dual[ir2], {v2, 1, v3, 1}]*
             fsym[dual[ir3], ir1, ir2, irreps[[1]], dual[ir2], ir3, {v3, 1, v1, 1}]
            , {v2, 1, nv[ir2, dual[ir3], dual[ir1]]}, {v3, 1, nv[dual[ir3], ir1, dual[ir2]]}] , precision ] , 10^(-20) ] 
        , {ir1, irreps}, {ir2, irreps}, {ir3, fusion[ir1, ir2]}, {v1, 1, nv[ir1, ir2, ir3]}] // Flatten // Union) == {0};
  
  If[Not[MemberQ[pivotlist, x_ /; Not[Chop[x - 1, 10^(-20)] == 0 || Chop[x + 1, 10^(-20)] == 0 ]]],
   pivotlist = Round[pivotlist];
  ];
  
 
  thetalist = Table[
    1/qd[ir] Sum[qd[ir1] rsym[ir, ir, ir1, {v, v}], {ir1, fusion[ir, ir]}, {v, 1, nv[ir, ir, ir1]}]
    , {ir, irreps}];
    
  Do[
   theta[irreps[[i]]] = thetalist[[i]]
   , {i, 1, numofirreps}];

  hlist = 
   Table[Mod[Rationalize[Chop[1/(2 Pi I) Log[thetalist[[i]]], 10^(-20) ]], 1]
   , {i, 1, numofirreps}];
  
  frobschurlist = Chop[ Table[
     1/qdtot2 Sum[nv[ir, ir1, ir2] qd[ir1] qd[ir2] theta[ir1]^2/theta[ir2]^2, {ir1, irreps}, {ir2, fusion[ir, ir1]}]
     , {ir, irreps}] , 10^(-20) ];
   
  If[Not[MemberQ[frobschurlist, x_ /; Not[Chop[x - 1, 10^(-20)] == 0 || Chop[x + 1, 10^(-20)] == 0 || Chop[x, 10^(-20)] == 0]]],
   frobschurlist = Round[frobschurlist];
  ];
  
  smat = Chop[ 1/(qdtot) Table[ 
      Sum[qd[ir3] theta[ir3] * nv[dual[ir1], ir2, ir3]/(theta[dual[ir1]] * theta[ir2]), {ir3, fusion[dual[ir1], ir2]}]
        , {ir1, irreps}, {ir2, irreps}] , 10^(-20) ];
  
  cmat = SparseArray[
    Table[{i, Position[irrepsdual, irreps[[i]]][[1, 1]]} -> 1, {i, 1, numofirreps}]
      ] // Normal;
  
  tmat = DiagonalMatrix[thetalist];
  
  modular = (Chop[ smat.ConjugateTranspose[smat] - IdentityMatrix[numofirreps] , 10^(-20) ] // Flatten // Union) == {0};
  
  pplus = Sum[qd[ir]^2 theta[ir], {ir, irreps}];
  pminus = Sum[qd[ir]^2 /theta[ir], {ir, irreps}];
  modular2 = (Chop[ pplus pminus - qdtot2 , 10^(-20) ]) == 0;
  If[modular != modular2, Print["The two ways of determining modularity do NOT agree!"]];
  If[modular,
   centralcharge = Chop[ 8/(2 Pi I) Log[pplus/qdtot] , 10^(-20) ] // Rationalize;
   centralcharge = Mod[centralcharge, 8];,
   centralcharge = Missing["The theory is not modular, the central charge is not defined"];,
   centralcharge = Missing["The theory is not modular, the central charge is not defined"];
   ];
  
  If[modular,
  modularrelationsok =
   (( Chop[ MatrixPower[smat.tmat, 3] - Exp[2 Pi I/8*centralcharge] smat.smat // Flatten , 10^(-20) ] // Union) == {0}) &&
   (( Chop[ smat.smat - cmat , 10^(-20) ] // Flatten // Union) == {0});,
   modularrelationsok = Missing["The theory is not modular, so there are no modular relations to be checked."];,
   modularrelationsok = Missing["The theory is not modular, so there are no modular relations to be checked."];
  ];
  
  
  Print["The labels of the irreps are: ", irreps];
  Print["Are the particles selfdual: ", selfdualvec];
  Print["There are ", numofselfduals, " selfdual particles."];
  Print["Are the particles simple currents: ", simplecurrentvec];
  Print["There are ", numofsimplecurrents, " simple currents."];
  Print["The pivotal structure is: ", pivotlist];
  If[pivoteqnok,
   Print["The pivotal equations are satisfied :-)"];,
   Print["The pivotal equations are NOT satisfied :-("];,
   Print["The pivotal equations could not be checked."];];
  Print["The Frobenius-Schur indicators are: ", frobschurlist];
  Print["The Frobenius-Perron dimensions are: ", fpdimvec // N];
  Print["The quantum dimensions are: ", qdimvec // N];
  Print["The scaling dimensions modulo one (calculated from the twists) are: ", hlist];
  
  If[fmatunitary && Not[qdimspositive],
   Print["The F-matrices are all unitary, but the quantum dimensions are not all positive. \
Presumably, there exists a different pivotal structure, such that all the quantum dimensions are positive."];
   ];

  If[Not[fmatunitary] && qdimspositive,
   Print["The F-matrices are not all unitary, but the quantum dimensions are all positive. Better check!"];
   ];
  
  If[
   qdimspositive,
   Print["The theory is unitary."];,
   Print["The theory is NOT unitary."];
   ];
  
  If[modular,
   Print["The theory is modular."];,
   Print["The theory is NOT modular."];,
   Print["The modularity of the theory could not be determined."];];
  
  If[modular && qdimspositive,
   Print["The central charge is: ", centralcharge, " (determined mod 8)"];
   ];
  If[modular && Not[qdimspositive],
   Print["The central charge is: ", centralcharge, " (determined mod 4)"];
   ];
  
  If[modular && qdimspositive,
   Print["The S-matrix is given by:"]; 
   Print[smat // N //Chop // MatrixForm];
   ];
  If[modular && Not[qdimspositive],
   Print["The S-matrix is given by (up to an overall sign):"];
   Print[smat // N //Chop // MatrixForm];
   ];
   
  If[Not[modular] && qdimspositive,
   Print["The (non-modular!) S-matrix is given by:"]; 
   Print[smat // N //Chop // MatrixForm];
   ];
  If[Not[modular] && Not[qdimspositive],
   Print["The (non-modular!) S-matrix is given by (up to an overall sign):"];
   Print[smat // N //Chop // MatrixForm];
   ];
   
   
  If[modular && modularrelationsok,
   Print["The modular relations Exp[- 2 Pi I/8 centralcharge](S.T)^3 = S^2 = C are satisfied :-)"]
  ];
  
  If[modular && Not[modularrelationsok],
    Print["The modular relations Exp[- 2 Pi I/8 centralcharge](S.T)^3 = S^2 = C are not satisfied :-("]
  ];

  modulardatacalculated = True;
  
  Global`FPdimlist = fpdimvec;
  Global`qdimlist = qdimvec;
  Global`selfduallist = selfdualvec;
  Global`simplecurrentlist = simplecurrentvec;
  Global`pivotlist = pivotlist;
  Global`thetalist = thetalist;
  Global`hlist = hlist;
  Global`FSlist = frobschurlist;
  Global`smat = smat;
  Global`tmat = tmat;
  Global`cmat = cmat;
  Global`centralcharge = centralcharge;
  Global`modular = modular;
  Global`unitary = qdimspositive;
  
  Print["Done :-)"];
  
  ];
  
 ];
 


calculatemodulardata[pivotlist_List] := Module[
  {pivotlistok,pivotlistquantumgroup,pivot,qd,theta},
  
  If[Not[typeranklevelrootinitok],
  Print["The type of algebra, rank, level and/or rootfactor were not \
(correctly) initialized, please do so first, followed by calculating \
the F- and R-symbols, before calculating the modular data."];
  ];

If[typeranklevelrootinitok && Not[fsymbolscalculated],
  Print["The F-symbols were not calculated. Please do so first, \
followed by calculating the R-symbols, before calculating the modular \
data."];
  ];

If[typeranklevelrootinitok && fsymbolscalculated && 
   Not[rsymbolscalculated],
  Print["The R-symbols were not calculated. Please do so first, \
before calculating the modular data."];
  ];
  
  pivotlistok = True;
  
  If[Length[pivotlist] != numofirreps,
  pivotlistok = False;  
  Print["The list of pivotal coefficients does not have the correct length (i.e., the \
number of irreps)."];
  ,
  Null,
  pivotlistok = False;  
  Print["The list of pivotal coefficients does not have the correct length (i.e., the \
number of irreps)."];
  ];
  
  If[pivotlist,
    If[MemberQ[pivotlist,x_/;Not[x==1]&&Not[x==-1]],
     pivotlistok = False;
     Print["The pivotal coefficients are not all either +1 or -1, \
we only allow spherical cases here."];,
     Null,
     pivotlistok = False;
     Print["The pivotal coefficients are not all either +1 or -1, \
we only allow spherical cases here."];
    ];
  ];
  
  If[pivotlistok,
    Do[pivot[irreps[[i]]] = pivotlist[[i]], {i, 1, numofirreps}];

    pivoteqnok = (Table[
        Chop[ N[ pivot[ir1] pivot[ir2]/pivot[ir3] -
           Sum[fsym[ir1, ir2, dual[ir3], irreps[[1]], ir3, dual[ir1], {v1, 1, v2, 1}]*
             fsym[ir2, dual[ir3], ir1, irreps[[1]], dual[ir1], dual[ir2], {v2, 1, v3, 1}]*
             fsym[dual[ir3], ir1, ir2, irreps[[1]], dual[ir2], ir3, {v3, 1, v1, 1}]
            , {v2, 1, nv[ir2, dual[ir3], dual[ir1]]}, {v3, 1, nv[dual[ir3], ir1, dual[ir2]]}] , precision ] , 10^(-20) ] 
        , {ir1, irreps}, {ir2, irreps}, {ir3, fusion[ir1, ir2]}, {v1, 1, nv[ir1, ir2, ir3]}] // Flatten // Union) == {0};
        
    If[pivoteqnok,
      Print["The pivotal equations are satisfied :-) "];,
      pivotlistok = False;
      Print["The pivotal equations are not satisfied :-("];,
      pivotlistok = False;
      Print["The pivotal equations are not satisfied :-("];
    ];    

  ];

  If[Not[pivoteqnok],
  Print["Please provide a valid list of pivotal coefficients. \
These can be obtained by running `possiblesphericalpivotalstructures[]'"];
  ];
    
  
  If[typeranklevelrootinitok && fsymbolscalculated && rsymbolscalculated && pivoteqnok,
  
  numposroots = Position[roots, Table[0, {i, 1, rank}], 1][[1, 1]] - 1;
  posroots = roots[[1 ;; numposroots]];
  
  qdim1overfvec = 
   Table[
    N[ 1/fsym[irreps[[i]], irrepsdual[[i]], irreps[[i]], irreps[[i]], irreps[[1]], irreps[[1]], {1, 1, 1, 1}] , precision ]
   , {i, 1, numofirreps}];


  qdimvec = Table[pivotlist[[i]] * qdim1overfvec[[i]], {i,1, numofirreps}];
  
    
    qdimvecquantumgroup = Table[
     N[Chop[Product[
       nq[tmax (ir + rho).qfm.posroots[[pr]], 1] / nq[ tmax (rho).qfm.posroots[[pr]], 1]
       , {pr, 1, numposroots}], 10^(-20)], precision]
    , {ir, irreps}];
 
    
  pivotlistquantumgroup = 
   Table[qdimvecquantumgroup[[i]]/qdim1overfvec[[i]], {i, 1, numofirreps}];

  If[Not[MemberQ[pivotlistquantumgroup, x_ /; Not[Chop[x - 1, 10^(-20)] == 0 || Chop[x + 1, 10^(-20)] == 0 ]]],
   pivotlistquantumgroup = Round[pivotlistquantumgroup];,
   Print["The pivotal coefficients coming from the quantum group are \
not all either +1 or -1, better check!"];,
   Print["The pivotal coefficients coming from the quantum group are \
not all either +1 or -1, better check!"];   
  ];
  
  If[pivotlistquantumgroup==pivotlist,
  Print["The specified pivotal structure is the same as the one coming from the quantum group."];,
  Print["The specified pivotal structure differs from the one coming form the \
quantum group, namely ", pivotlistquantumgroup,". The calculated modular data is not \
realized by the selected quantum group. If you later want to obtain the exact representation
of the modular data for the quantum group, you should run calculatemodulardata[] first."]
  ];  
    
    
  Do[
   qd[irreps[[i]]] = qdimvec[[i]]
   , {i, 1, numofirreps}];
   
  qdtot2 = Sum[qdimvec[[i]]^2, {i, 1, numofirreps}];
  qdtot = Sqrt[qdtot2];
  qdimspositive = And @@ (# > 0 & /@ qdimvec);
  
  fpdimvec = 
   Table[
    (Cases[Chop[ (N[ nmat[ir] , precision ] // Eigenvalues) , 10^(-(Max[10, precision - 20])) ], x_ /; x \[Element] Reals] // Sort)[[-1]]
   , {ir, irreps}];
    
 
  thetalist = Table[
    1/qd[ir] Sum[qd[ir1] rsym[ir, ir, ir1, {v, v}], {ir1, fusion[ir, ir]}, {v, 1, nv[ir, ir, ir1]}]
    , {ir, irreps}];
    
  Do[
   theta[irreps[[i]]] = thetalist[[i]]
   , {i, 1, numofirreps}];

  hlist = 
   Table[Mod[Rationalize[Chop[1/(2 Pi I) Log[thetalist[[i]]], 10^(-20) ]], 1]
   , {i, 1, numofirreps}];
  
  frobschurlist = Chop[ Table[
     1/qdtot2 Sum[nv[ir, ir1, ir2] qd[ir1] qd[ir2] theta[ir1]^2/theta[ir2]^2, {ir1, irreps}, {ir2, fusion[ir, ir1]}]
     , {ir, irreps}] , 10^(-20) ];
   
  If[Not[MemberQ[frobschurlist, x_ /; Not[Chop[x - 1, 10^(-20)] == 0 || Chop[x + 1, 10^(-20)] == 0 || Chop[x, 10^(-20)] == 0]]],
   frobschurlist = Round[frobschurlist];
  ];
  
  smat = Chop[ 1/(qdtot) Table[ 
      Sum[qd[ir3] theta[ir3] * nv[dual[ir1], ir2, ir3]/(theta[dual[ir1]] * theta[ir2]), {ir3, fusion[dual[ir1], ir2]}]
        , {ir1, irreps}, {ir2, irreps}] , 10^(-20) ];
  
  cmat = SparseArray[
    Table[{i, Position[irrepsdual, irreps[[i]]][[1, 1]]} -> 1, {i, 1, numofirreps}]
      ] // Normal;
  
  tmat = DiagonalMatrix[thetalist];
  
  modular = (Chop[ smat.ConjugateTranspose[smat] - IdentityMatrix[numofirreps] , 10^(-20) ] // Flatten // Union) == {0};
  
  pplus = Sum[qd[ir]^2 theta[ir], {ir, irreps}];
  pminus = Sum[qd[ir]^2 /theta[ir], {ir, irreps}];
  modular2 = (Chop[ pplus pminus - qdtot2 , 10^(-20) ]) == 0;
  If[modular != modular2, Print["The two ways of determining modularity do NOT agree!"]];
  If[modular,
   centralcharge = Chop[ 8/(2 Pi I) Log[pplus/qdtot] , 10^(-20) ] // Rationalize;
   centralcharge = Mod[centralcharge, 8];
   ];
  
  If[modular,
  modularrelationsok =
   (( Chop[ MatrixPower[smat.tmat, 3] - Exp[2 Pi I/8*centralcharge] smat.smat // Flatten , 10^(-20) ] // Union) == {0}) &&
   (( Chop[ smat.smat - cmat , 10^(-20) ] // Flatten // Union) == {0});
  ];
  
  
  Print["The labels of the irreps are: ", irreps];
  Print["Are the particles selfdual: ", selfdualvec];
  Print["There are ", numofselfduals, " selfdual particles."];
  Print["Are the particles simple currents: ", simplecurrentvec];
  Print["There are ", numofsimplecurrents, " simple currents."];
  Print["The Frobenius-Schur indicators are: ", frobschurlist];
  Print["The Frobenius-Perron dimensions are: ", fpdimvec // N];
  Print["The quantum dimensions are: ", qdimvec // N];
  Print["The scaling dimensions modulo one (calculated from the twists) are: ", hlist];
  
  If[fmatunitary && Not[qdimspositive],
   Print["The F-matrices are all unitary, but the quantum dimensions are not all positive. \
Presumably, there exists a different pivotal structure, such that all the quantum dimensions are positive."];
   ];

  If[Not[fmatunitary] && qdimspositive,
   Print["The F-matrices are not all unitary, but the quantum dimensions are all positive. Better check!"];
   ];
  
  If[
   qdimspositive,
   Print["The theory is unitary."];,
   Print["The theory is NOT unitary."];
   ];
  
  If[modular,
   Print["The theory is modular."];,
   Print["The theory is NOT modular."];,
   Print["The modularity of the theory could not be determined."];];
  
  If[modular && qdimspositive,
   Print["The central charge is: ", centralcharge, " (determined mod 8)"];
   ];
  If[modular && Not[qdimspositive],
   Print["The central charge is: ", centralcharge, " (determined mod 4)"];
   ];
  
  If[modular && qdimspositive,
   Print["The S-matrix is given by:"]; 
   Print[smat // N //Chop // MatrixForm];
   ];
  If[modular && Not[qdimspositive],
   Print["The S-matrix is given by (up to an overall sign):"];
   Print[smat // N //Chop // MatrixForm];
   ];
   
  If[Not[modular] && qdimspositive,
   Print["The (non-modular!) S-matrix is given by:"]; 
   Print[smat // N //Chop // MatrixForm];
   ];
  If[Not[modular] && Not[qdimspositive],
   Print["The (non-modular!) S-matrix is given by (up to an overall sign):"];
   Print[smat // N //Chop // MatrixForm];
   ];
   
   
  If[modular && modularrelationsok,
   Print["The modular relations Exp[- 2 Pi I/8 centralcharge](S.T)^3 = S^2 = C are satisfied :-)"]
  ];
  
  If[modular && Not[modularrelationsok],
    Print["The modular relations Exp[- 2 Pi I/8 centralcharge](S.T)^3 = S^2 = C are not satisfied :-("]
  ];

  modulardatacalculated = pivotlistquantumgroup==pivotlist;
  
  Global`FPdimlist = fpdimvec;
  Global`qdimlist = qdimvec;
  Global`selfduallist = selfdualvec;
  Global`simplecurrentlist = simplecurrentvec;
  Global`pivotlist = pivotlist;
  Global`thetalist = thetalist;
  Global`hlist = hlist;
  Global`FSlist = frobschurlist;
  Global`smat = smat;
  Global`tmat = tmat;
  Global`cmat = cmat;
  Global`centralcharge = centralcharge;
  Global`modular = modular;
  Global`unitary = qdimspositive;
  
  Print["Done :-)"];
  
  
  
  ];
  
 ];


(* ::Subsection::Closed:: *)
(*Routines to find an exact representation of the F - and R - symbols*)


findexactabs[number_, cosdenom_, cosnum_] :=
  Module[
  {numberok, numberinternal, numprec, cosvec, matrix, matrixreduced, resultvec},
   (* This routine gives the exact representation non-negative real numbers *)
   (* List with coefficients is returned *)
   
   numberok = False;
   If[Chop[number, 10^(-20)] \[Element] Reals && Chop[number, 10^(-20)] >= 0, numberok = True];
   
   If[numberok,
    
    If[number == 0, 
     resultvec = Table[0, {i, 0, EulerPhi[cosdenom]/2 - 1}]];
    
    If[number != 0,
     If[ExactNumberQ[number], numberinternal = N[number, 100], numberinternal = number];
     numprec = Floor[Precision[numberinternal]];
     cosvec = Table[
     N[If[i > 0, Cos[2 Pi cosnum/cosdenom]^i, 1], numprec]
     , {i, 0, EulerPhi[cosdenom]/2 - 1}];
     
     AppendTo[cosvec, N[numberinternal, numprec]];
     
     matrix = 
      Append[IdentityMatrix[Length[cosvec]], Round[10^numprec cosvec]] // Transpose;
     
     (* LatticeReduce implements the LLL algorithm. *)
     
     matrixreduced = LatticeReduce[matrix];
     resultvec = -matrixreduced[[1, 1 ;; -3]]/matrixreduced[[1, -2]];
     
     ];
    
    resultvec,
    
    Print["The number is not a non-negative real number!"];
    
    ]
   
   ];


makefsymbolsexact[cosdenom_, cosnum_] := Module[
   {arglist, abs2fsyms, abs2rationalfsyms, abs2rationalfsymsunion, 
    abs2result, firstpos, ratiotoresult, resultlist},
   
   (* We do not assume anymore that the fsymbols are either real, 
      or purely imaginary (this is the case after calculating the 
      f-symbols, i.e., if one does not diagonalize the R-matrices *)
   (* So we write the f-symbols as a square root times a phase.  *)
   (* In the exact representation, the square root is implicit, so only
      the phase (converted to a rational number -1 < r/s \[LessEqual] 1) and the
      coefficients under the square root are returned. *)
   
   arglist = Rationalize[Arg[Chop[fsym[Sequence @@ #] & /@ flist , 10^(-20)]]/Pi];
   
   abs2fsyms = Table[Abs[fsym[Sequence @@ fs]]^2, {fs, flist}];
   
   abs2rationalfsyms = Rationalize[#, 10^(-40)] & /@ abs2fsyms;
   
   abs2rationalfsymsunion = abs2rationalfsyms // Union;
   
   firstpos = Table[
     Position[abs2rationalfsyms, abs2rationalfsymsunion[[i]], 1][[1, 1]]
     , {i, 1, Length[abs2rationalfsymsunion]}];
   
   abs2result = 
    Table[findexactabs[abs2fsyms[[firstpos[[i]]]], cosdenom, cosnum]
      , {i, 1, Length[abs2rationalfsymsunion]}];
   
   Do[
    ratiotoresult[abs2rationalfsymsunion[[i]]] = abs2result[[i]]
    , {i, 1, Length[abs2rationalfsymsunion]}];
   
   resultlist = Table[
     {arglist[[i]], ratiotoresult[abs2rationalfsyms[[i]]]}
     , {i, 1, Length[abs2rationalfsyms]}];
   
   Do[
    fsymexact[Sequence @@ flist[[i]]] = resultlist[[i]]
    , {i, 1, Length[flist]}];
   
   Clear[arglist, abs2fsyms, abs2rationalfsyms, 
    abs2rationalfsymsunion, firstpos, abs2result, ratiotoresult, 
    resultlist];
   
   ];


makersymbolsexact[cosdenom_, cosnum_] := Module[
   {arglist, abs2rsyms, abs2rationalrsyms, abs2rationalrsymsunion, 
    abs2result, firstpos, ratiotoresult, resultlist},
   
   (* We assume that the rsymbols take the same form as the fsymbols *)
   (* So we write it as a square root times a phase *)
   (* In the exact representation, the square root is implicit, so only
      the overall phase and the coefficients under the square root are
      returned. *)
   
   (* The R-symbols *)
   
   arglist = 
    Rationalize[
     Arg[Chop[rsym[Sequence @@ #] & /@ rlist , 10^(-20)]]/Pi];
   
   abs2rsyms = Table[Abs[rsym[Sequence @@ rs]]^2, {rs, rlist}];
   
   abs2rationalrsyms = Rationalize[#, 10^(-40)] & /@ abs2rsyms;
   
   abs2rationalrsymsunion = abs2rationalrsyms // Union;
   
   firstpos = Table[
     Position[abs2rationalrsyms, abs2rationalrsymsunion[[i]], 1][[1, 1]]
     , {i, 1, Length[abs2rationalrsymsunion]}];
   
   abs2result = 
    Table[findexactabs[abs2rsyms[[firstpos[[i]]]], cosdenom,  cosnum]
    , {i, 1, Length[abs2rationalrsymsunion]}];
   
   Do[
    ratiotoresult[abs2rationalrsymsunion[[i]]] = abs2result[[i]]
    , {i, 1, Length[abs2rationalrsymsunion]}];
   
   resultlist = Table[
     {arglist[[i]], ratiotoresult[abs2rationalrsyms[[i]]]}
     , {i, 1, Length[abs2rationalrsyms]}];
   
   Do[
    rsymexact[Sequence @@ rlist[[i]]] = resultlist[[i]]
    , {i, 1, Length[rlist]}];
   
   
   (* The inverse R-symbols *)
   
   arglist = 
    Rationalize[
     Arg[Chop[rsyminv[Sequence @@ #] & /@ rlist , 10^(-20)]]/Pi];
   
   abs2rsyms = Table[Abs[rsyminv[Sequence @@ rs]]^2, {rs, rlist}];
   
   abs2rationalrsyms = Rationalize[#, 10^(-40)] & /@ abs2rsyms;
   
   abs2rationalrsymsunion = abs2rationalrsyms // Union;
   
   firstpos = Table[
     Position[abs2rationalrsyms, abs2rationalrsymsunion[[i]], 1][[1, 1]]
     , {i, 1, Length[abs2rationalrsymsunion]}];
   
   abs2result = 
    Table[findexactabs[abs2rsyms[[firstpos[[i]]]], cosdenom, cosnum]
    , {i, 1, Length[abs2rationalrsymsunion]}];
   
   Do[
    ratiotoresult[abs2rationalrsymsunion[[i]]] = abs2result[[i]]
    , {i, 1, Length[abs2rationalrsymsunion]}];
   
   resultlist = Table[
     {arglist[[i]], ratiotoresult[abs2rationalrsyms[[i]]]}
     , {i, 1, Length[abs2rationalrsyms]}];
   
   Do[
    rsyminvexact[Sequence @@ rlist[[i]]] = resultlist[[i]]
    , {i, 1, Length[rlist]}];
   
   Clear[arglist, abs2rsyms, abs2rationalrsyms, 
    abs2rationalrsymsunion, firstpos, abs2result, ratiotoresult, 
    resultlist];
   
   ];


toexactvalue[fsexact_, cosdenom_, cosnum_] :=
  Exp[fsexact[[1]]*Pi*I] *
    Sqrt[fsexact[[2]].Table[If[i > 0, Cos[2 Pi cosnum/cosdenom]^i, 1], {i, 0, EulerPhi[cosdenom]/2 - 1}]];
    
toexactnumericalvalue[fsexact_, cosdenom_, cosnum_, prec_] :=
  N[Exp[fsexact[[1]]*Pi*I] * 
    Sqrt[fsexact[[2]].Table[If[i > 0, Cos[2 Pi cosnum/cosdenom]^i, 1], {i, 0, EulerPhi[cosdenom]/2 - 1}]], prec];    


checkpentagonexactform[cosdenom_, cosnum_, prec_] := Module[
   {maxdev, tempdev, cosvec, fsymexnum},
   
   If[prec < 100,
    Print[
      "The selected precision for checking the pentagon equations \
with the exact F-symbols is smaller than 100. The value should be at \
least 100; the default used is 2*precision, with a default precision of 100."];
    ];
   
   If[prec >= 100,
    pentholdsexact = False;
    maxdev = 0;
    cosvec = N[Table[If[i > 0, Cos[2 Pi cosnum/cosdenom]^i, 1], {i, 0, EulerPhi[cosdenom]/2 - 1}], prec];
    
    Do[
     fsymexnum[Sequence @@ fs] = 
       Exp[fsymexact[Sequence @@ fs][[1]]*Pi*I]*
        Sqrt[(fsymexact[Sequence @@ fs][[2]]).cosvec];
     , {fs, flist}];
    counter = 0;

  Do[
       tempdev = 
        Abs[
        Sum[(fsymexnum[f, c, d, e, g, gp, {v2, v3, v4, v1s}]*
           fsymexnum[a, b, gp, e, f, fp, {v1, v1s, v5, v6}])
        , {v1s, nv[f, gp, e]}] - 
        Sum[(fsymexnum[a, b, c, g, f, h, {v1, v2, v2s, v3s}] *
           fsymexnum[a, h, d, e, g, fp, {v3s, v3, v4s, v6}]*
           fsymexnum[b, c, d, fp, h, gp, {v2s, v4s, v4, v5}])
        , {h,  Quiet[Cases[fusion[b, c],
            x_ /; MemberQ[fusion[a, x], g] && 
            MemberQ[fusion[x, d], fp]]]}
        , {v2s, nv[b, c, h]}
        , {v3s, nv[a, h, g]}
        , {v4s, nv[h, d, fp]}]];

    (* If[tempdev > maxdev, maxdev = tempdev]; *)
    (*
     Max deals better with high precision numbers than > (Greater).
     This caused maxdev to remain 0, even if it should have been f.i. 0``73.93384483647623 . *)
     maxdev = Max[maxdev, tempdev];
     counter++;

      , {a, irreps}, {b, irreps}, {c, irreps}, {d, irreps}
      , {f, fusion[a, b]}
      , {g, fusion[f, c]}
      , {e, fusion[g, d]}
      , {gp, Cases[fusion[c, d], x_ /; MemberQ[fusion[f, x], e]]}
      , {fp, Cases[fusion[b, gp], x_ /; MemberQ[fusion[a, x], e]]}
      , {v1, nv[a, b, f]}
      , {v2, nv[f, c, g]}
      , {v3, nv[g, d, e]}
      , {v4, nv[c, d, gp]}
      , {v5, nv[b, gp, fp]}
      , {v6, nv[a, fp, e]}
   ];
    
    If[Chop[maxdev, 10^(-(prec - 20))] != 0,
     Print["The pentagon equations were checked with the exact form of \
the F-symbols, with precision ", prec, " but the pentagon equations are \
not satisfied with accuracy ", prec - 20, ". The maximum deviation is ", maxdev, " :-( \
Better check what went wrong!"];
     ];
    
    If[Chop[maxdev, 10^(-(prec - 20))] == 0,
     pentholdsexact = True;
     Print["The pentagon equations were checked with the exact form of the \
F-symbols, with precision ", prec, " and they hold with accuracy ", 
      Floor[Accuracy[maxdev]], " :-)"];
     ];
    Clear[counter];
    ];
   
   ];


checkhexagonexactform[cosdenom_, cosnum_, prec_] := Module[
   {maxdev, tempdev, cosvec, fsymexnum, rsymexnum, rsyminvexnum, accuracyobtained},
   
   If[prec < 100,
    Print["The selected precision for checking the hexagon equations with \
the exact F- and R-symbols is smaller than 100. The value should be at least 100; \
the default used is 2*precision, with a default precision of 100."];
   ];
   
   If[prec >= 100,
    hexholdsexact = False;
    maxdev = 0;
    cosvec = N[Table[If[i > 0, Cos[2 Pi cosnum/cosdenom]^i, 1], {i, 0, EulerPhi[cosdenom]/2 - 1}], prec];
    
    Do[
     fsymexnum[Sequence @@ fs] = 
       Exp[fsymexact[Sequence @@ fs][[1]]*Pi*I] *
        Sqrt[(fsymexact[Sequence @@ fs][[2]]).cosvec];
     , {fs, flist}];
     
    Do[
     rsymexnum[Sequence @@ rs] = 
       Exp[rsymexact[Sequence @@ rs][[1]]*Pi*I] *
        Sqrt[(rsymexact[Sequence @@ rs][[2]]).cosvec];
     , {rs, rlist}];
     
    Do[
     rsyminvexnum[Sequence @@ rs] = 
       Exp[rsyminvexact[Sequence @@ rs][[1]]*Pi*I] *
        Sqrt[(rsyminvexact[Sequence @@ rs][[2]]).cosvec];
     , {rs, rlist}];
    
    Do[
     tempdev = Abs[
       Sum[
       rsymexnum[i[[1]], i[[2]], i[[5]], {i[[7, 1]], v8}]*
       fsymexnum[i[[1]], i[[2]], i[[3]], i[[4]], i[[5]], i[[6]], {v8, i[[7, 2]], v9, i[[7, 4]]}]*
       rsymexnum[i[[3]], i[[2]], i[[6]], {v9, i[[7, 3]]}]
       , {v8, nv[i[[1]], i[[2]], i[[5]]]}
       , {v9, nv[i[[3]], i[[2]], i[[6]]]}]
       -
       Sum[
       fsymexnum[i[[2]], i[[1]], i[[3]], i[[4]], i[[5]], j, {i[[7, 1]], i[[7, 2]], v5, v6}]*
       rsymexnum[j, i[[2]], i[[4]], {v6, v7}]*
       fsymexnum[i[[1]], i[[3]], i[[2]], i[[4]], j, i[[6]], {v5, v7, i[[7, 3]], i[[7, 4]]}]
       , {j, Cases[irreps, x_ /; MemberQ[fusion[i[[1]], i[[3]]], x] &&
             MemberQ[fusion[i[[2]], x], i[[4]]]]}
       , {v5, nv[i[[1]], i[[3]], j]}
       , {v6, nv[i[[2]], j, i[[4]]]}
       , {v7, nv[i[[2]], j, i[[4]]]}]
     ];
     
    (*If[tempdev > maxdev, maxdev =  tempdev];*)
    (* Max deals better with high precision numbers than > (Greater).
    This caused maxdev to remain 0, even if it should have been f.i. 0``73.93384483647623 . *)
     
     maxdev = Max[maxdev, tempdev];
 , {i, flist}];

  Do[
     tempdev = Abs[
       Sum[
       rsymexnum[i[[1]], i[[2]], i[[5]], {i[[7, 1]], v8}]*
       fsymexnum[i[[1]], i[[2]], i[[3]], i[[4]], i[[5]], i[[6]], {v8, i[[7, 2]], v9, i[[7, 4]]}]*
       rsymexnum[i[[3]], i[[2]], i[[6]], {v9, i[[7, 3]]}]
       , {v8, nv[i[[1]], i[[2]], i[[5]]]}
       , {v9, nv[i[[3]], i[[2]], i[[6]]]}]
       -
       Sum[
       fsymexnum[i[[2]], i[[1]], i[[3]], i[[4]], i[[5]], j, {i[[7, 1]], i[[7, 2]], v5, v6}]*
       rsymexnum[j, i[[2]], i[[4]], {v6, v7}]*
       fsymexnum[i[[1]], i[[3]], i[[2]], i[[4]], j, i[[6]], {v5, v7, i[[7, 3]], i[[7, 4]]}]
       , {j, Cases[irreps, x_ /; MemberQ[fusion[i[[1]], i[[3]]], x] &&
             MemberQ[fusion[i[[2]], x], i[[4]]]]}
       , {v5, nv[i[[1]], i[[3]], j]}
       , {v6, nv[i[[2]], j, i[[4]]]}
       , {v7, nv[i[[2]], j, i[[4]]]}]
               
    ];
     
    (*If[tempdev > maxdev, maxdev = tempdev];*)
    (*Max deals better with high precision numbers than > (Greater).
    This caused maxdev to remain 0,  even if it should have been f.i. 0``73.93384483647623 . *)
     
     maxdev = Max[maxdev, tempdev];
        
  , {i, flist}];
    
    If[Chop[maxdev, 10^(-(prec - 20))] != 0,
     Print["The hexagon equations were checked with the exact form of the \
F- and R-symbols, with precision ", prec, " but the hexagon equations are not \
satisfied with accuracy ", prec - 20, ". The maximum deviation is ", maxdev, " :-( \
Better check what went wrong!"];
     ];
    
    
    If[Chop[maxdev, 10^(-(prec - 20))] == 0,
     hexholdsexact = True;
     Print["The hexagon equations were checked with the exact form of the \
F- and R-symbols, with precision ", prec, " and they hold with accuracy ", 
     Floor[Accuracy[maxdev]], " :-)"];
     ];
    
    
    ];
   
   ];


findexactfsymbols[] := With[{},
      If[Not[typeranklevelrootinitok],
       Print["The type of algebra, rank, level and/or rootfactor were not \
(correctly) initialized, please do so first, followed by \
calculating the F-symbols, before trying to find the exact form of the \
F-symbols."];
       ];
   
     If[typeranklevelrootinitok && Not[fsymbolscalculated],
      Print["The F-symbols were not calculated. Please do so first, \
before trying to find the exact form of the F-symbols."];
      ];
      
    If[typeranklevelrootinitok && fsymbolscalculated && rmatricesdiagonalized,
      Print["The R-matrices were diagonalized. This can result in values of the \
F-symbols, that can not be written in terms of the general numberfield used for \
the exact representation of the F- and R-symbols. Therefore, one should revert back \
to the original form of the F- and R-symbols by running undiagonalizermatrices[]."];
    ];
   
    If[typeranklevelrootinitok && fsymbolscalculated && Not[rmatricesdiagonalized],
     Print["Trying to find the exact form of the F-symbols..."];
    

    makefsymbolsexact[cosdenominator, cosnumerator]; 
          
    Print["Found the exact form of the F-symbols :-)"];
    
     If[pentagontobechecked,
     Print["Checking the ", numofpentagonequations, 
      " pentagon equations, using the exact form of the F-symbols..."];
          
      checkpentagonexactform[cosdenominator, cosnumerator, 2*precision];
     
           ,
      Print["The pentagon equations were not checked using the exact form \
of the F-symbols, because you opted to not check the pentagon equations. Proceed \
with care!"];
     ];
    
    fsymbolsexactfound = True;
    Print["Done :-)"];
    
    ];
   
   ];


findexactrsymbols[] := With[{},
      If[Not[typeranklevelrootinitok],
       Print["The type of algebra, rank, level and/or rootfactor were not \
(correctly) initialized, please do so first, followed by calculating \
the F- and R-symbols, before trying to find the exact form of the R-symbols."];
       ];
   
     If[typeranklevelrootinitok && Not[fsymbolscalculated],
      Print["The F-symbols were not calculated. Please do so first, \
followed by calculating the R-symbols, before trying to find the exact \
form of the R-symbols."];
      ];
   
    If[typeranklevelrootinitok && fsymbolscalculated && Not[rsymbolscalculated],
      Print["The R-symbols were not calculated. Please do so first, before \
trying to find the exact form of the R-symbols."];
      ];
      
    If[typeranklevelrootinitok && fsymbolscalculated && rsymbolscalculated && rmatricesdiagonalized,
      Print["The R-matrices were diagonalized. This can result in values of the \
F-symbols, that can not be written in terms of the general numberfield used for \
the exact representation of the F- and R-symbols. Therefore, one should revert back \
to the original form of the F- and R-symbols by running undiagonalizermatrices[]."];
    ];
      
   
    If[typeranklevelrootinitok && fsymbolscalculated && rsymbolscalculated && Not[rmatricesdiagonalized],
       
    Print["Trying to find the exact form of the R-symbols..."];
         
   makersymbolsexact[cosdenominator, cosnumerator];
     
     
    
    Print["Found the exact form of the R-symbols :-)"];
    
    If[fsymbolsexactfound,
     Print["Checking the ", 2*Length[flist], " hexagon equations, \
using the exact form of the F- and R-symbols..."];
    
    checkhexagonexactform[cosdenominator, cosnumerator, 2*precision];
      
      
     
     ,
     Print["The hexagon equations were not checked with the exact form of \
the R-symbols, because the exact form of the F-symbols was not obtained."];
     ];
    
    rsymbolsexactfound = True;
    
    Print["Done :-)"];

    ];
   
   ];


(* ::Subsection::Closed:: *)
(*Routines to check the exact representation of the F - and R - symbols numerically*)


checkpentagonexactformnumerically[]:=With[{},

If[Not[typeranklevelrootinitok],
  Print["The type of algebra, rank, level and/or rootfactor were not \
(correctly) initialized, please do so first, followed by \
calculating the F-symbols and obtaining the exact form of the F-symbols \
before trying to check the pentagon equations numerically, using the \
exact form of the F-symbols."];
];

If[typeranklevelrootinitok && Not[fsymbolscalculated],
  Print["The F-symbols were not calculated. Please do so first, \
followed by obtaining the exact form of the F-symbols, \
before trying to check the pentagon equations numerically, using the \
exact form of the F-symbols."];
];

If[typeranklevelrootinitok && fsymbolscalculated && Not[fsymbolsexactfound],
  Print["The exact form of the F-symbols was not obtained. Please do \
so first, before trying to check the pentagon equations numerically, using the \
exact form of the F-symbols."];
];

If[typeranklevelrootinitok && fsymbolscalculated && fsymbolsexactfound,
  Print["Checking the ", numofpentagonequations, 
      " pentagon equations numerically, using the exact form of the F-symbols..."];

  checkpentagonexactform[cosdenominator, cosnumerator, 2*precision];
  
  Print["Done :-)"];
];

];


checkhexagonexactformnumerically[]:=With[{},

If[Not[typeranklevelrootinitok],
  Print["The type of algebra, rank, level and/or rootfactor were not \
(correctly) initialized, please do so first, followed by \
calculating the F- and R-symbols and obtaining the exact form of the \
F- and R-symbols before trying to check the hexagon equations numerically, using the \
exact form of the F- and R-symbols."];
];

If[typeranklevelrootinitok && Not[fsymbolscalculated],
  Print["The F-symbols were not calculated. Please do so first, \
followed by calculating the R-symbols and obtaining the exact form of \
the F- and R-symbols, before trying to check the hexagon equations \
numerically, using the exact form of the F- and R-symbols."];
];

If[typeranklevelrootinitok && fsymbolscalculated && Not[fsymbolsexactfound] && Not[rsymbolscalculated],
  Print["The R-symbols were not calculated. Please do so first, \
followed by obtaining the exact form of the F- and R-symbols, before \
trying to check the hexagon equations numerically, using the \
exact form of the F- and R-symbols."];
];

If[typeranklevelrootinitok && fsymbolscalculated && fsymbolsexactfound && Not[rsymbolscalculated],
  Print["The R-symbols were not calculated. Please do so first, \
followed by obtaining the exact form of the R-symbols, before \
trying to check the hexagon equations numerically, using the \
exact form of the F- and R-symbols."];
];

If[typeranklevelrootinitok && fsymbolscalculated && rsymbolscalculated && Not[fsymbolsexactfound] && Not[rsymbolsexactfound],
  Print["The exact form of the F- and R-symbols were not obtained. Please do so first, \
before trying to check the hexagon equations numerically, using the \
exact form of the F- and R-symbols."];
];

If[typeranklevelrootinitok && fsymbolscalculated && rsymbolscalculated && Not[fsymbolsexactfound] && rsymbolsexactfound,
  Print["The exact form of the F-symbols was not obtained. Please do so first, \
before trying to check the hexagon equations numerically, using the \
exact form of the F- and R-symbols."];
];

If[typeranklevelrootinitok && fsymbolscalculated && rsymbolscalculated && fsymbolsexactfound && Not[rsymbolsexactfound],
  Print["The exact form of the R-symbols was not obtained. Please do so first, \
before trying to check the hexagon equations numerically, using the \
exact form of the F- and R-symbols."];
];

If[typeranklevelrootinitok && fsymbolscalculated && rsymbolscalculated && fsymbolsexactfound && rsymbolsexactfound,
  Print["Checking the ", 2*Length[flist], " hexagon equations numerically, \
using the exact form of the F- and R-symbols..."];
  
    checkhexagonexactform[cosdenominator, cosnumerator, 2*precision];
    Print["Done :-)"];

];

];


(* ::Subsection::Closed:: *)
(*Routines to check the exact representation of the F - and R - symbols algebraically*)


removezeroes[coefs_, len_] := Module[{currlen, res},
   res = coefs;
   currlen = Length[res];
   While[
    currlen > len && res[[-1]] == 0,
    res = res[[1 ;; -2]];
    currlen = Length[res];
    ];
   res
   ];
   
exactproduct[cosdenom_, factor1_, factor2_] := 
  Module[{factor, sqrtproduct, len, currlen, minpol, minpolcoefs, 
    reductioncoefs},
   factor = factor1[[1]] + factor2[[1]];
   factor = Mod[factor, 2, -1];
   If[factor == -1, factor = 1];
   minpol = MinimalPolynomial[Cos[2 Pi/cosdenom], x];
   minpolcoefs = CoefficientList[minpol, x];
   reductioncoefs = -minpolcoefs[[1 ;; -2]]/minpolcoefs[[-1]];
   len = Length[factor1[[2]]];
   sqrtproduct = 
    Table[Sum[
      factor1[[2, j1 + 1]]*factor2[[2, i - j1 + 1]]
      , {j1, Max[0, i + 1 - len], Min[len - 1, i]}]
    , {i, 0, 2 len - 2}];
   sqrtproduct = removezeroes[sqrtproduct, len];
   currlen = Length[sqrtproduct];
   While[
    currlen > len,
    sqrtproduct = 
     PadLeft[reductioncoefs, currlen - 1]*sqrtproduct[[-1]] +  sqrtproduct[[1 ;; -2]];
    sqrtproduct = removezeroes[sqrtproduct, len];
    currlen = Length[sqrtproduct];
    ];
   If[sqrtproduct == zerorep[cosdenom][[2]],
    factor = 0;
   ];     
   {factor, sqrtproduct}
   ];

exactproduct[cosdenom_, factor1_, factor2_, factor3_] := 
  exactproduct[cosdenom, exactproduct[cosdenom, factor1, factor2], factor3];
  
exactproductnophase[cosdenom_, factor1_, factor2_] := 
  Module[{sqrtproduct, len, currlen, minpol, minpolcoefs, 
    reductioncoefs},
   minpol = MinimalPolynomial[Cos[2 Pi/cosdenom], x];
   minpolcoefs = CoefficientList[minpol, x];
   reductioncoefs = -minpolcoefs[[1 ;; -2]]/minpolcoefs[[-1]];
   len = Length[factor1];
   sqrtproduct = 
    Table[Sum[
      factor1[[j1 + 1]]*factor2[[i - j1 + 1]]
      , {j1, Max[0, i + 1 - len], Min[len - 1, i]}]
    , {i, 0, 2 len - 2}];
   sqrtproduct = removezeroes[sqrtproduct, len];
   currlen = Length[sqrtproduct];
   While[
    currlen > len,
    sqrtproduct = 
     PadLeft[reductioncoefs, currlen - 1]*sqrtproduct[[-1]] + sqrtproduct[[1 ;; -2]];
    sqrtproduct = removezeroes[sqrtproduct, len];
    currlen = Length[sqrtproduct];
    ];
   sqrtproduct
   ];  
   
   
exactproductnophase[cosdenom_, factor1_, factor2_, factor3_] := 
  exactproductnophase[cosdenom, exactproductnophase[cosdenom, factor1, factor2], factor3];
  
findsqrt[cosdenom_, cosnum_, factor_] := 
  Module[{eqncoefs, numofvars, vars, sols, sol1, sol2, res, cosvec, done},
   done = False;
   numofvars = EulerPhi[cosdenom]/2;
   (* check if the factor corresponds to zero (routine below would give only one
   solution, causing troubles ) *)
   If[factor == Table[0,numofvars],
   res = factor;
   done = True;
   ];
   
   If[Not[done],
   cosvec = 
    Table[If[i > 0, Cos[2 Pi cosnum/cosdenom]^i, 1], {i, 0, numofvars - 1}];
   If[Not[factor.cosvec >= 0], 
    Print["The numerical value corresponding to the input does not \
correspond to a non-negative number, the evaluation is aborted!"];
    res = {};
    ,
    vars = Table[var[i], {i, 0, numofvars - 1}];
    eqncoefs = exactproductnophase[cosdenom, vars, vars];
    sols = Solve[eqncoefs == factor, vars, Rationals];
    sols = Cases[sols, x_/;FreeQ[x,ConditionalExpression]];
    If[Length[sols] == 0,
     res = {};
     Print["No solution for the sqare root was found."];
     ];
    If[Length[sols] == 2,
     sol1 = sols[[1, All, 2]];
     sol2 = sols[[2, All, 2]];
     If[
      sol1.cosvec >= 0,
      res = sol1;,
      res = sol2;
      ];
     ];
    If[Length[sols] != 2 && Length[sols] != 0,
     res = {};
     Print[
      "The number of solutions to the equations was neither zero nor \
two, something went wrong!" ];
     ];
    ];
    ];
   res
   ];
   
newabsrep[cosdenom_, cosnum_, altcosdenom_, oldrep_] := 
  Module[{exactvalue, numvalue, ok, newabsrep, newexactvalue},
   
   (* It is assumed that the oldrep corresponds to a non-
   negative arguement of the square root! *)
   
   exactvalue = toexactvalue[{0, oldrep}, cosdenom, cosnum]^2;
   numvalue = toexactnumericalvalue[{0, oldrep}, cosdenom, cosnum, 100]^2;
   
   newabsrep = findexactabs[numvalue, altcosdenom, 1];
   newexactvalue = toexactvalue[{0, newabsrep}, altcosdenom, 1]^2;
   
   ok = FullSimplify[exactvalue == newexactvalue];
   
   If[ok, Null,
    Print["The new representation is not equal to the old one!"];,
    Print["It could not be established if the new representation is equal \
to the old one or not!"];
    ];
   
   newabsrep
   
   ];         

  
newabsrep[cosdenom_, cosnum_, altcosdenom_, altcosnum_, oldrep_] := 
  Module[{exactvalue, numvalue, ok, newabsrep, newexactvalue},
   
   (* It is assumed that the oldrep corresponds to a non-
   negative arguement of the square root! *)
   
   exactvalue = toexactvalue[{0, oldrep}, cosdenom, cosnum]^2;
   numvalue = toexactnumericalvalue[{0, oldrep}, cosdenom, cosnum, 100]^2;
   
   newabsrep = findexactabs[numvalue, altcosdenom, altcosnum];
   newexactvalue = toexactvalue[{0, newabsrep}, altcosdenom, altcosnum]^2;
   
   ok = FullSimplify[exactvalue == newexactvalue];
   
   If[ok, Null,
    Print["The new representation is not equal to the old one!"];,
    Print["It could not be established if the new representation is equal \
to the old one or not!"];
    ];
   
   newabsrep
   
   ];         


reduceabs2[cosdenom_, cosnum_, sumlist_] :=
  reducesum[cosdenom, cosnum,
   combinesum[
    removepairs[cosdenom,
     combinesumsymmetricphases[cosdenom, cosnum,
      combinesum[
       removepairs[cosdenom,
        abs2[cosdenom, sumlist]
        ]
       ]
      ]
     ]
    ]
   ];


impartzerocheck[cosdenom_, cosnum_, sumlist_] := Module[
   {sintab, sign, cos2expnum, cos2expression, cos2expressionok, 
    sinexpression,
    sintababs2, result},
   
   (* It is assumed that the overall phase has been removed!! *)
   
   (* for the imaginary part, we first evaluate sin\phi = +/- sqrt(1-
   cos\phi^2),
       and multiply it with the sqrts of the equation. 
       we then take the square of the result, 
       and find expressions for the sqrts appearing, 
       which turns out to be possible.
       finally, 
   we add things upp to see if it all sums to zero as it should *)

   sintab = DeleteCases[
     Table[
      sign = Sign[N[Sin[Pi sumlist[[i, 1]]], 200] // Chop];
      cos2expnum = N[Cos[Pi sumlist[[i, 1]]]^2, 200];
      cos2expression = findexactabs[cos2expnum, cosdenom, cosnum];
      cos2expressionok = 
       FullSimplify[
        toexactvalue[{0, cos2expression}, cosdenom, cosnum]^2 -
        Cos[Pi sumlist[[i, 1]]]^2 == 0];
      If[cos2expressionok, Null, 
       Print["An expression for a cos^2 could not be checked \
exactly!"];, 
       Print["An expression for a cos^2 could not be checked \
exactly!"];];
      
      If[sign == 1, 
       sinexpression = {0, Table[If[i == 1, 1, 0], {i, 1, Length[cos2expression]}] - 
          cos2expression}];
      If[sign == -1, 
       sinexpression = {1, Table[If[i == 1, 1, 0], {i, 1, Length[cos2expression]}] - 
          cos2expression}];
      
      If[sign == 0,
       Null,
       exactproduct[cosdenom, sinexpression, {0, sumlist[[i, 2]]}]
       ]
      
      , {i, 1, Length[sumlist]}]
     
     , Null];
   
   (* We only want to know if the result is zero, 
   so we can take the absolute value, and check if that is zero. *)
   
   If[sintab == {},
    sintababs2 = zerorep[cosdenom];
    ];
   
   If[sintab =!= {},
    sintababs2 = reduceabs2[cosdenom, cosnum, sintab];
    ];
   
   
   If[sintababs2 == zerorep[cosdenom],
    result = True;,
    result = False;,
    result = False;
    ];
   
   result
   
   ];


checkeqn[cosdenom_, cosnum_, equation_] := Module[
   {eqn, eqnok, lhs, rhs, lhsnum, rhsnum, eqnnumok,
    arglhs, argrhs, lhsimok, rhsimok, lhsabs2, rhsabs2, abs2ok},
   
   eqn = equation;
   eqnok = False;
   
    If[eqn[[1]] == eqn[[2]], eqnok = True];
    lhs = Sum[toexactvalue[eqn[[1, i]], cosdenom, cosnum], {i, 1, Length[eqn[[1]]]}];
    rhs = Sum[toexactvalue[eqn[[2, i]], cosdenom, cosnum], {i, 1, Length[eqn[[2]]]}];
    lhsnum = N[#, 200] & /@ lhs;
    rhsnum = N[#, 200] & /@ rhs;
    eqnnumok = Chop[lhsnum - rhsnum, 10^(-190)] == 0;
    If[Not[eqnnumok],
     Print["Numerically, the equation does not hold, something went \
wrong!"];
    ];
   
   If[Not[eqnok] && eqnnumok,
    
    (* make sure that both the lhs and rhs are non-
    negative real numbers *)
    
    arglhs = Rationalize[Arg[lhsnum]/Pi];
    argrhs = Rationalize[Arg[rhsnum]/Pi];
    (* This should not be possible after the numerical check, but let's check if the
       arguemtent of the lhs and rhs match in any case *)
    If[arglhs =!= argrhs,
    Print["The arguement of the lhs and rhs are not equal to each other. \
Something went wrong!"];
    ];
    
    eqn =
     {
     Table[{Mod[eqn[[1, j, 1]] - arglhs, 2, -1], eqn[[1, j, 2]]}
     , {j, 1, Length[eqn[[1]]]}]
     , 
     Table[{Mod[eqn[[2, j, 1]] - arglhs, 2, -1], eqn[[2, j, 2]]}
     , {j, 1, Length[eqn[[2]]]}]};
    
    (* check that the imaginary part of the lhs and rhs are indeed \
zero *)
    
    lhsimok = impartzerocheck[cosdenom, cosnum, eqn[[1]]];
    rhsimok = impartzerocheck[cosdenom, cosnum, eqn[[2]]];
    
    (* calculate the abs2 of both the lhs and rhs *)
    
    lhsabs2 = reduceabs2[cosdenom, cosnum, eqn[[1]]];
    rhsabs2 = reduceabs2[cosdenom, cosnum, eqn[[2]]];
    
    abs2ok = lhsabs2 == rhsabs2;
    
    (* the eqn holds if the absolute values match, and the
    imaginary parts of the lhs and rhs are zero *)
    
    If[abs2ok && lhsimok && rhsimok,
     eqnok = True;
     ];
    
    ];
   
   eqnok
   
   ];


reducereal[cosdenom_, cosnum_, list_] := Module[
   {sumlist, allphases, nophases, numvalue, sign, abs2list, res, 
    positive, abs2listnosqrt, abs2listnosqrtsum,
    numvaluecheck},
   
   (* Make sure that the `phases' are in [0,2) *)
   
   sumlist = Table[{Mod[list[[i, 1]], 2], list[[i, 2]]}, {i, 1, Length[list]}];
   
   allphases = sumlist[[All, 1]] // Union;
   
   nophases = ContainsOnly[allphases, {0, 1}];
   
   If[nophases,
    
    numvalue = 
     Sum[toexactnumericalvalue[sumlist[[i]], cosdenom, cosnum, 200], {i, 1, Length[sumlist]}];
    
    If[
     Chop[numvalue, 10^(-180)] == 0, sign = 0;,
     sign = Sign[Chop[numvalue, 10^(-180)]];
     ];
    
    If[sign != 0,
     positive = FullSimplify[
       Sum[toexactvalue[sumlist[[i]], cosdenom, cosnum], {i, 1, Length[sumlist]}] > 0
       ];
     If[(positive && sign == 1) || (Not[positive] && sign == -1), Null,
      Print["Something went wrong while checking the sign of the expression!"];,
      Print["Something went wrong while checking the sign of the expression!"];
      ];
     ];
    
    
    (* take the absolute value squared, and sum the result *)
 
    abs2listnosqrtsum = 
     Sum[
      If[i == j, sumlist[[i, 2]],
         2*(-1)^(sumlist[[i, 1]] + sumlist[[j, 1]])*
         findsqrt[cosdenom, cosnum, exactproductnophase[cosdenom, sumlist[[i, 2]], sumlist[[j, 2]]]]
      ]
     , {i, 1, Length[sumlist]}, {j, i, Length[sumlist]}]; 
       
    
    If[sign == 0 && abs2listnosqrtsum != zerorep[cosdenom][[2]],
     Print["The sign was determined to be zero, but the result is not \
zero, something went wrong!"];,
     Null,
     Print["The sign was determined to be zero, but the result is not \
zero, something went wrong!"];
     ];
    
    res = {If[sign == -1, 1, 0], abs2listnosqrtsum};
    
    numvaluecheck = toexactnumericalvalue[res, cosdenom, cosnum, 200];
    
    If[Chop[numvalue - numvaluecheck, 10^(-180)] == 0, Null,
     Print["The calculated value is not equal to the original one. \
Something went wrong!"];,
     Print["The calculated value is not equal to the original one. \
Something went wrong!"];
     ];
    
    ,
    res = sumlist;
    Print[
     "The sum contains phases, reducereal is only for real \
expressions! The original list is returned."];,
    res = sumlist;
    Print[
     "The sum contains phases, reducereal is only for real \
expressions! The original list is returned."];
    ];
   
   res
   
   ];


checkpentagonexactformalgebraically[cosdenom_, cosnum_] := Module[
   {eqnok, eqn, lhs, rhs, nophases, allphases},
   
   Do[
    eqn = {
      Table[
       exactproduct[
        cosdenom,
        fsymexact[f, c, d, e, g, gp, {v2, v3, v4, v1s}],
        fsymexact[a, b, gp, e, f, fp, {v1, v1s, v5, v6}]
        ]
       , {v1s, nv[f, gp, e]}]
      ,
      Flatten[
       Table[
        exactproduct[
         cosdenom,
         fsymexact[a, b, c, g, f, h, {v1, v2, v2s, v3s}],
         fsymexact[a, h, d, e, g, fp, {v3s, v3, v4s, v6}],
         fsymexact[b, c, d, fp, h, gp, {v2s, v4s, v4, v5}]
         ]
        , {h, Quiet[Cases[fusion[b, c], x_ /; MemberQ[fusion[a, x], g] && MemberQ[fusion[x, d], fp]]]}
        , {v2s, nv[b, c, h]}
        , {v3s, nv[a, h, g]}
        , {v4s, nv[h, d, fp]}
        ]
       , 3]
      };
      
    If[eqn[[1]] == {}, eqn[[1]] = {zerorep[cosdenom]}];  
    If[eqn[[2]] == {}, eqn[[2]] = {zerorep[cosdenom]}];  
    
    
    eqnok = False;
    If[eqn[[1]] == eqn[[2]], eqnok = True;];
    
    
    
   If[Not[eqnok],
    
    eqn[[1]] = removepairs[cosdenom, combinesum[eqn[[1]]]];
    eqn[[2]] = removepairs[cosdenom, combinesum[eqn[[2]]]];
    

    nophases = False;
    allphases = (eqn[[All, All, 1]]) // Flatten // Union;

    If[ContainsOnly[allphases, {0, 1}],
     nophases = True;
    ];
    
    If[ContainsOnly[allphases, {-1/2, 1/2}],
     nophases = True;
     eqn[[1]] = Table[{eqn[[1, i, 1]] + 1/2, eqn[[1, i, 2]]}, {i, 1, Length[eqn[[1]]]}];
     eqn[[2]] = Table[{eqn[[2, i, 1]] + 1/2, eqn[[2, i, 2]]}, {i, 1, Length[eqn[[2]]]}];
    ];
    
    If[nophases,
     lhs = reducereal[cosdenom, cosnum, eqn[[1]]];
     rhs = reducereal[cosdenom, cosnum, eqn[[2]]];
     eqnok = lhs == rhs;,
     (* this should not happen with the original form of the F-symbols, but could
        happen if one diagonalizes the R-matrices, *)
     eqnok = checkeqn[cosdenom, cosnum, eqn];
    ];
   ];
    
    If[eqnok,
     Null,
     pentholdsalgebraically = False;
     Print["The exact form of the F-symbols do not satisfy at least one of \
the pentagon equations. The check will be aborted :-("];
     Break[];,
     pentholdsalgebraically = False;
     Print["The exact form of the F-symbols could not be verified for at least one of \
the pentagon equations. The check will be aborted :-("];
     Break[];
     ];
    
    , {a, irreps}
    , {b, irreps}
    , {c, irreps}
    , {d, irreps}
    , {f, fusion[a, b]}
    , {g, fusion[f, c]}
    , {e, fusion[g, d]}
    , {gp, Cases[fusion[c, d], x_ /; MemberQ[fusion[f, x], e]]}
    , {fp, Cases[fusion[b, gp], x_ /; MemberQ[fusion[a, x], e]]}
    , {v1, nv[a, b, f]}
    , {v2, nv[f, c, g]}
    , {v3, nv[g, d, e]}
    , {v4, nv[c, d, gp]}
    , {v5, nv[b, gp, fp]}
    , {v6, nv[a, fp, e]}
    ];
   
   If[eqnok,
    pentholdsalgebraically = True;
    Print[
     "It was checked algebraically that the exact form of the \
F-symbols satisfy the pentagon equations :-)"];

    ];
   
   ];


checkhexagonexactformalgebraically[cosdenom_, cosnum_] := Module[
   {eqnok, eqn, allphasedenominators, altdenom, needaltdenom, neweqn},
   
   allphasedenominators = Table[
     Denominator[rsymexact[Sequence @@ rs][[1]]]
     ,{rs,rlist}]//Union;
   
   altdenom = LCM[Sequence @@ allphasedenominators, cosdenom];
   
   If[altdenom != cosdenom, needaltdenom = True, needaltdenom = False,
     needaltdenom = False;
     Print["Something went wrong while checking if a new denominator for the \
cosines is necesarry, better check!"];
   ];
   
   Do[
    eqn = {
      Flatten[
       Table[
        exactproduct[
         cosdenom,
         rsymexact[i[[1]], i[[2]], i[[5]], {i[[7, 1]], v8}],
         fsymexact[i[[1]], i[[2]], i[[3]], i[[4]], i[[5]], i[[6]], {v8, i[[7, 2]], v9, i[[7, 4]]}],
         rsymexact[i[[3]], i[[2]], i[[6]], {v9, i[[7, 3]]}]
         ]
        , {v8, nv[i[[1]], i[[2]], i[[5]]]}
        , {v9, nv[i[[3]], i[[2]], i[[6]]]}]
       , 1]
      ,
      Flatten[
       Table[
        exactproduct[
         cosdenom,
         fsymexact[i[[2]], i[[1]], i[[3]], i[[4]], i[[5]], j, {i[[7, 1]], i[[7, 2]], v5, v6}], 
         rsymexact[j, i[[2]], i[[4]], {v6, v7}],
         fsymexact[i[[1]], i[[3]], i[[2]], i[[4]], j, i[[6]], {v5, v7, i[[7, 3]], i[[7, 4]]}]
         ]
        , {j, Cases[irreps, x_ /; MemberQ[fusion[i[[1]], i[[3]]], x] && MemberQ[fusion[i[[2]], x], i[[4]]]]}
        , {v5, nv[i[[1]], i[[3]], j]}
        , {v6, nv[i[[2]], j, i[[4]]]}
        , {v7, nv[i[[2]], j, i[[4]]]}
        ]
       , 3]
      };
      
    If[eqn[[1]] == {}, eqn[[1]] = {zerorep[cosdenom]}];  
    If[eqn[[2]] == {}, eqn[[2]] = {zerorep[cosdenom]}];  
      
    
    If[Not[needaltdenom],
    eqnok = checkeqn[cosdenom, cosnum, eqn];
    ];
    
    If[needaltdenom,
     neweqn = Table[ 
     {eqn[[i, j, 1]], newabsrep[cosdenom, cosnum, altdenom, eqn[[i, j, 2]]]}
     , {i, 1, 2}, {j, 1, Length[eqn[[i]]]}];
    eqnok = checkeqn[altdenom, 1, neweqn];
    ];
    
    
    If[Not[eqnok],
     hexholdsalgebraically = False;
     Print["The exact form of the F- and R-symbols do not satisfy at least \
one of the hexagon equations. The check will be aborted :-("];
    Break[];
     ];
    
    , {i, flist}];
   
   
   If[eqnok,
    
    Do[
      eqn = {
        Flatten[
         Table[
          exactproduct[
           cosdenom,
           rsyminvexact[i[[1]], i[[2]], i[[5]], {i[[7, 1]], v8}],
           fsymexact[i[[1]], i[[2]], i[[3]], i[[4]], i[[5]], i[[6]], {v8, i[[7, 2]], v9, i[[7, 4]]}],
           rsyminvexact[i[[3]], i[[2]], i[[6]], {v9, i[[7, 3]]}]
           ]
          , {v8, nv[i[[1]], i[[2]], i[[5]]]}
          , {v9, nv[i[[3]], i[[2]], i[[6]]]}]
         , 1]
        ,
        Flatten[
         Table[
          exactproduct[
           cosdenom,
           fsymexact[i[[2]], i[[1]], i[[3]], i[[4]], i[[5]], j, {i[[7, 1]], i[[7, 2]], v5, v6}], 
           rsyminvexact[j, i[[2]], i[[4]], {v6, v7}],
           fsymexact[i[[1]], i[[3]], i[[2]], i[[4]], j, i[[6]], {v5, v7, i[[7, 3]], i[[7, 4]]}]
           ]
          , {j, Cases[irreps, x_ /; MemberQ[fusion[i[[1]], i[[3]]], x] && MemberQ[fusion[i[[2]], x], i[[4]]]]}
          , {v5, nv[i[[1]], i[[3]], j]}
          , {v6, nv[i[[2]], j, i[[4]]]}
          , {v7, nv[i[[2]], j, i[[4]]]}
          ]
         , 3]
        };
        
    If[Not[needaltdenom],
    eqnok = checkeqn[cosdenom, cosnum, eqn];
    ];
    
    If[needaltdenom,
     neweqn = Table[ 
     {eqn[[i, j, 1]], newabsrep[cosdenom, cosnum, altdenom, eqn[[i, j, 2]]]}
     , {i, 1, 2}, {j, 1, Length[eqn[[i]]]}];
    eqnok = checkeqn[altdenom, 1, neweqn];
    ];        
      
      
      If[Not[eqnok],
       hexholdsalgebraically = False;
       Print["The exact form of the F- and R-symbols do not satisfy at \
least one of the hexagon equations. The check will be aborted :-("];
       Break[];
       ];
      
      , {i, flist}];
    
    ];
   

   If[eqnok,
    hexholdsalgebraically = True;
    Print["It was checked algebraically that the exact form of the F- and \
R-symbols satisfy the hexagon equations :-)"];
    ];
   
   ];


checkpentagonalgebraically[]:=With[{},

If[Not[typeranklevelrootinitok],
  Print["The type of algebra, rank, level and/or rootfactor were not \
(correctly) initialized, please do so first, followed by \
calculating the F-symbols and obtaining the exact form of the F-symbols \
before trying to check the pentagon equations algebraically."];
];

If[typeranklevelrootinitok && Not[fsymbolscalculated],
  Print["The F-symbols were not calculated. Please do so first, \
followed by obtaining the exact form of the F-symbols, \
before trying to check the pentagon equations algebraically."];
];

If[typeranklevelrootinitok && fsymbolscalculated && Not[fsymbolsexactfound],
  Print["The exact form of the F-symbols was not obtained. Please do \
so first, before trying to check the pentagon equations algebraically."];
];

If[typeranklevelrootinitok && fsymbolscalculated && fsymbolsexactfound,
  Print["Checking the ", numofpentagonequations, 
      " pentagon equations algebraically..."];
  checkpentagonexactformalgebraically[cosdenominator,cosnumerator];
  Print["Done :-)"];
];

];


checkhexagonalgebraically[]:=With[{},

If[Not[typeranklevelrootinitok],
  Print["The type of algebra, rank, level and/or rootfactor were not \
(correctly) initialized, please do so first, followed by \
calculating the F- and R-symbols and obtaining the exact form of the \
F- and R-symbols before trying to check the hexagon equations algebraically."];
];

If[typeranklevelrootinitok && Not[fsymbolscalculated],
  Print["The F-symbols were not calculated. Please do so first, \
followed by calculating the R-symbols and obtaining the exact form of \
the F- and R-symbols, before trying to check the hexagon equations \
algebraically."];
];

If[typeranklevelrootinitok && fsymbolscalculated && Not[fsymbolsexactfound] && Not[rsymbolscalculated],
  Print["The R-symbols were not calculated. Please do so first, \
followed by obtaining the exact form of the F- and R-symbols, before \
trying to check the hexagon equations algebraically."];
];

If[typeranklevelrootinitok && fsymbolscalculated && fsymbolsexactfound && Not[rsymbolscalculated],
  Print["The R-symbols were not calculated. Please do so first, \
followed by obtaining the exact form of the R-symbols, before \
trying to check the hexagon equations algebraically."];
];

If[typeranklevelrootinitok && fsymbolscalculated && rsymbolscalculated && Not[fsymbolsexactfound] && Not[rsymbolsexactfound],
  Print["The exact form of the F- and R-symbols were not obtained. Please do so first, \
before trying to check the hexagon equations algebraically."];
];

If[typeranklevelrootinitok && fsymbolscalculated && rsymbolscalculated && Not[fsymbolsexactfound] && rsymbolsexactfound,
  Print["The exact form of the F-symbols was not obtained. Please do so first, \
before trying to check the hexagon equations algebraically."];
];

If[typeranklevelrootinitok && fsymbolscalculated && rsymbolscalculated && fsymbolsexactfound && Not[rsymbolsexactfound],
  Print["The exact form of the R-symbols was not obtained. Please do so first, \
before trying to check the hexagon equations algebraically."];
];

If[typeranklevelrootinitok && fsymbolscalculated && rsymbolscalculated && fsymbolsexactfound && rsymbolsexactfound,
  Print["Checking the ", 2*Length[flist], " hexagon equations algebraically..."];
  checkhexagonexactformalgebraically[cosdenominator,cosnumerator];
  Print["Done :-)"];
];

];


(* ::Subsection::Closed:: *)
(*Routines to find an exact representation for the modular data*)


zerorep[cosdenom_]:={0,Table[0,EulerPhi[cosdenom]/2]};
onerep[cosdenom_]:={0,Table[If[i==1,1,0],{i,1,EulerPhi[cosdenom]/2}]};
minusonerep[cosdenom_] := {1, Table[If[i == 1, 1, 0], {i, 1, EulerPhi[cosdenom]/2}]};
phaserep[cosdenom_,arg_]:={arg,Table[If[i==1,1,0],{i,1,EulerPhi[cosdenom]/2}]};


findexactmodulardata[] := 
  Module[{FPdimlistexactnum, qdimlistexactnum, qdtot2exactnum, 
    qdtotexactnum, qdtot1overexactnum, 
    qdim1overfexactlistnum,
    thetasum, sumres, qd, res, thetalistexactnum, thetalistexactnum1,
    arg, abs2, coscoefs, smatexactnum, smatexactnum1, tmatexactnum,
    pplusexactnum, pplusexactnum1, pminusexactnum, pminusexactnum1,
    ssdagmaxdev, sdagsmaxdev, pivotexact, pivotmaxdev,
    twistsarephases, st3maxdev, s2maxdev,
    pivotexactok, frobschurexactok, FPdimexactok, qdimexactok, qdtot2exactok, qdtotexactok,
    qdtot1overexactok, thetalistexactok, smatexactok, pplusexactok, pminusexactok, modularrelationsexactok,
    realphasepos, denom, currreal, currreal100, gamma, beta, coefs, alphacheck, alphacheckok
    },
   
   If[Not[typeranklevelrootinitok],
    Print["The type of algebra, rank, level and/or rootfactor were not \
(correctly) initialized, please do so first, followed by \
calculating the F- and R-symbols, \
the modular data and obtaining the exact form of the F- and \
R-symbols before calculating the \
exact form of the modular data."];
    ];
   
   If[typeranklevelrootinitok && Not[fsymbolscalculated],
    Print["The F-symbols were not calculated. Please do so first, followed \
by calculating the R-symbols \
and obtaining the exact form of the F- and R-symbols, the \
modular data and obtaining the exact form of the \
F- and R-symbols before calculating the exact form of the \
modular data."];
    ];
   
   If[typeranklevelrootinitok && fsymbolscalculated && 
     Not[fsymbolsexactfound] && Not[rsymbolscalculated], 
    Print["The R-symbols were not calculated. Please do so first, \
followed by obtaining the exact form \
of the F- and R-symbols, the modular data and obtaining the \
exact form of the F- and R-symbols before calculating the exact form of the \
modular data."];
    ];
   
   If[typeranklevelrootinitok && fsymbolscalculated && 
     fsymbolsexactfound && Not[rsymbolscalculated], 
    Print["The R-symbols were not calculated. Please do so first, \
followed by obtaining the exact form of the R-symbols, the modular \
data and obtaining the exact form of the R-symbols before calculating \
the exact form of the modular data."];
    ];
   
   If[typeranklevelrootinitok && fsymbolscalculated && 
     rsymbolscalculated && 
     Not[modulardatacalculated] && Not[fsymbolsexactfound] && 
     Not[rsymbolsexactfound],
    Print["The modular data was not calculated. Please do so first, \
followed by obtaining the exact form of the F- and R-symbols, \
before calculating the exact form of the modular data."];
    ];
   
   If[typeranklevelrootinitok && fsymbolscalculated && 
     fsymbolsexactfound && rsymbolscalculated && 
     Not[modulardatacalculated] && 
     Not[rsymbolsexactfound],
    Print["The modular data was not calculated. Please do so first, \
followed by obtaining the exact form of the R-symbols, before \
calculating the exact form of the modular data."];
    ];
   
   If[typeranklevelrootinitok && fsymbolscalculated && 
     rsymbolscalculated && 
     Not[modulardatacalculated] && Not[fsymbolsexactfound] && 
     rsymbolsexactfound,
    Print["The modular data was not calculated. Please do so first, \
followed by obtaining the exact form of the F-symbols, before \
calculating the exact form of the modular data."];
    ];
   
   If[typeranklevelrootinitok && fsymbolscalculated && 
     fsymbolsexactfound && rsymbolscalculated && 
     Not[modulardatacalculated] && 
     rsymbolsexactfound,
    Print["The modular data was not calculated. Please do so first, \
before calculating the exact form of the modular data."];
    ];
   
   If[typeranklevelrootinitok && fsymbolscalculated && 
     rsymbolscalculated && 
     modulardatacalculated && Not[fsymbolsexactfound] && 
     Not[rsymbolsexactfound],
    Print["The exact form of the F- and R-symbols was not obtained. \
Please do so first, before calculating the exact form of the \
modular data."];
    ];
   
   If[typeranklevelrootinitok && fsymbolscalculated && 
     rsymbolscalculated && 
     modulardatacalculated && Not[fsymbolsexactfound] && 
     rsymbolsexactfound,
    Print["The exact form of the F-symbols was not obtained. Please do so \
first, before calculating the exact form of the modular data."];
    ];
   
   If[typeranklevelrootinitok && fsymbolscalculated && 
     fsymbolsexactfound && rsymbolscalculated && 
     modulardatacalculated && 
     Not[rsymbolsexactfound],
    Print["The exact form of the R-symbols was not obtained. Please do so \
first, before calculating the exact form of the modular data."];
    ];

     If[typeranklevelrootinitok && fsymbolscalculated && fsymbolsexactfound && 
   rsymbolscalculated && modulardatacalculated && 
   rsymbolsexactfound && rmatricesdiagonalized,
      Print["The R-matrices were diagonalized. This can result in values of the \
F-symbols, that can not be written in terms of the general numberfield used for \
the exact representation of the F- and R-symbols. Therefore, one should revert back \
to the original form of the F- and R-symbols by running undiagonalizermatrices[]."];
    ];
     
   
   
   If[typeranklevelrootinitok && fsymbolscalculated && fsymbolsexactfound && 
   rsymbolscalculated && modulardatacalculated &&
   rsymbolsexactfound && Not[rmatricesdiagonalized],
    
    
    (* Here comes the actual calculation !! *)
    
    
    pivotlistexact = pivotlist;
    
    Do[
     pivotexact[irreps[[i]]] = pivotlistexact[[i]];
     , {i, 1, numofirreps}];
    
    pivotmaxdev =
     Max[(Abs /@ (Table[
           pivotexact[ir1]*pivotexact[ir2]/pivotexact[ir3] -
            Sum[
            toexactnumericalvalue[
            fsymexact[ir1, ir2, dual[ir3], irreps[[1]], ir3, dual[ir1], {v1, 1, v2, 1}]
            , cosdenominator, cosnumerator, 2*precision]*
            toexactnumericalvalue[
            fsymexact[ir2, dual[ir3], ir1, irreps[[1]], dual[ir1], dual[ir2], {v2, 1, v3, 1}]
            , cosdenominator, cosnumerator, 2*precision]*
            toexactnumericalvalue[
            fsymexact[dual[ir3], ir1, ir2, irreps[[1]], dual[ir2], ir3, {v3, 1, v1, 1}]
            , cosdenominator, cosnumerator, 2*precision]
            , {v2, 1, nv[ir2, dual[ir3], dual[ir1]]}
            , {v3, 1, nv[dual[ir3], ir1, dual[ir2]]}
            ]
            , {ir1, irreps}
            , {ir2, irreps}
            , {ir3, fusion[ir1, ir2]}
            , {v1, 1, nv[ir1, ir2, ir3]}] // Flatten))];
            
    pivotexactok = Chop[pivotmaxdev, 10^(-(2*precision-20))] == 0;
    
    pivotlistexact = Table[ 
     Which[
      pivotlistexact[[i]] == 1, onerep[cosdenominator],
      pivotlistexact[[i]] == -1, phaserep[cosdenominator,1]
     ]
     , {i,1,numofirreps}];
     
     Do[
     pivotexact[irreps[[i]]] = pivotlistexact[[i]];
     , {i, 1, numofirreps}];
    
    frobschurexactok = True;
    frobschurlistexact = Table[
    Which[
    frobschurlist[[i]] == 1, onerep[cosdenominator],
    frobschurlist[[i]] == -1, phaserep[cosdenominator,1],
    frobschurlist[[i]] == 0, zerorep[cosdenominator],
    True, 
    frobschurexactok = False;
    Print["The Frobenius-Schur indicator is not +1, -1 or 0, something went wrong!"]; frobschurlist[[i]]
   ]
    , {i, 1, numofirreps}];
    
    FPdimlistexact = Table[
      {Rationalize[Arg[fp]/Pi], findexactabs[fp^2, cosdenominator, cosnumerator]}
      , {fp, fpdimvec}];
    
    FPdimlistexactnum = 
     Table[
     toexactnumericalvalue[fpe, cosdenominator, cosnumerator, 2*precision]
     , {fpe, FPdimlistexact}];
    
    FPdimexactok = 
    (Chop[FPdimlistexactnum - fpdimvec, 10^(-Max[10, precision - 20])] // Union) == {0};
    
    If[FPdimexactok, Null,
     Print["The exact representations of the Frobenius-Perron dimensions \
do not agree with the numerical ones :-("];,
     Print["The exact representations of the Frobenius-Perron dimensions \
do not agree with the numerical ones :-("];
     ];
    
    qdimlistexact = Table[
      {Rationalize[Arg[qdim]/Pi], findexactabs[qdim^2, cosdenominator, cosnumerator]}
      , {qdim, qdimvec}];
    Do[
     qdimexact[irreps[[i]]] = qdimlistexact[[i]]
     , {i, 1, numofirreps}];
    
    qdimspositiveexact = (qdimlistexact[[All, 1]] // Union) == {0};
    
    qdimlistexactnum = 
     Table[
     toexactnumericalvalue[qde, cosdenominator, cosnumerator, 2*precision]
     , {qde, qdimlistexact}];
    
    qdimexactok = 
    (Chop[qdimlistexactnum - qdimvec, 10^(-Max[10, precision - 20])] // Union) == {0};
    
    If[qdimexactok, Null,
     Print["The exact representations of the quantum dimensions do not \
agree with the numerical ones :-("];,
     Print["The exact representations of the quantum dimensions do not \
agree with the numerical ones :-("];
     ];
    
    
    qdtot2exact = {0, findexactabs[qdtot2^2, cosdenominator, cosnumerator]};
    
    qdtot2exactnum = 
     toexactnumericalvalue[qdtot2exact, cosdenominator, cosnumerator, 2*precision];
    
    qdtot2exactok = 
     Chop[qdtot2exactnum - qdtot2, 10^(-Max[10, precision - 20])] == 0;
    
    If[qdtot2exactok, Null,
     Print["The exact representation of the total quantum dimension does \
not agree with the numerical one :-("];,
     Print["The exact representation of the total quantum dimension does \
not agree with the numerical one :-("];
     ];
    
    
    qdtotexact = {0, findexactabs[qdtot2, cosdenominator, cosnumerator]};
    
    qdtotexactnum = 
     toexactnumericalvalue[qdtotexact, cosdenominator, cosnumerator, 2*precision];
    
    qdtotexactok = 
     Chop[qdtotexactnum - qdtot, 10^(-Max[10, precision - 20])] == 0;
    
    If[qdtotexactok, Null,
     Print["The exact representation of the square root of the total \
quantum dimension does not agree with the numerical one :-("];,
     Print["The exact representation of the square root of the total \
quantum dimension does not agree with the numerical one :-("];
     ];
    
    
    qdtot1overexact = 
     findoneover[cosdenominator, qdtotexact];
    
    qdtot1overexactnum = 
     toexactnumericalvalue[qdtot1overexact, cosdenominator, cosnumerator, 2*precision];
    
    qdtot1overexactok = 
     Chop[qdtot1overexactnum - 1/qdtot, 10^(-Max[10, precision - 20])] == 0;
    
    If[qdtot1overexactok, Null,
     Print["The exact representation of one over the square root of the \
total quantum dimension does not agree with the numerical one :-("];,
     Print["The exact representation of one over the square root of the \
total quantum dimension does not agree with the numerical one :-("];
     ];
     
     
     qdim1overfexactlist  = Table[
      findoneover[cosdenominator,
        fsymexact[irreps[[i]], irrepsdual[[i]], irreps[[i]], irreps[[i]], irreps[[1]], irreps[[1]], {1, 1, 1, 1}]
      ]
      , {i, 1, numofirreps}];


      thetalistexactnum1 = Table[
   1/toexactnumericalvalue[qdimexact[ir], cosdenominator, cosnumerator,  2*precision]*
    Sum[
     toexactnumericalvalue[qdimexact[ir1], cosdenominator, cosnumerator, 2*precision]*
      toexactnumericalvalue[rsymexact[ir, ir, ir1, {v, v}], cosdenominator, cosnumerator, 2*precision]
     , {ir1, fusion[ir, ir]}, {v, 1, nv[ir, ir, ir1]}]
   , {ir, irreps}];
   
   thetalistexact = 
     Table[
     {Rationalize[Arg[thetalistexactnum1[[i]]]/Pi],
     findexactabs[Abs[thetalistexactnum1[[i]]]^2, cosdenominator, cosnumerator]}
   , {i, 1, numofirreps}];     
   
   Do[
     thetaexact[irreps[[i]]] = thetalistexact[[i]]
     , {i, 1, numofirreps}];
   
   thetalistexactnum = Table[
    toexactnumericalvalue[thetalistexact[[i]], cosdenominator, cosnumerator, 2*precision]
    , {i, 1, numofirreps}];
   
   thetalistexactok = 
    (Chop[thetalistexactnum1 - thetalistexactnum, 10^(-(2 * precision - 20))] // Union) == {0};  
    
   twistsarephases = (thetalistexact[[All,2]] // Union) == {onerep[cosdenominator][[2]]};
     
       
    If[thetalistexactok, Null,
     Print["The exact representations of the twist factors do not agree \
with the numerical ones :-("];,
     Print["The exact representations of the twist factors do not agree \
with the numerical ones :-("];
     ];
    
    smatexactnum1 = 
    1/(toexactnumericalvalue[qdtotexact, cosdenominator, cosnumerator, 2*precision])*
    Table[
     Sum[
     toexactnumericalvalue[qdimexact[ir3], cosdenominator, cosnumerator, 2*precision]*
     toexactnumericalvalue[thetaexact[ir3], cosdenominator, cosnumerator, 2*precision]*
     nv[dual[ir1], ir2, ir3] / 
     (
      toexactnumericalvalue[thetaexact[ir1], cosdenominator, cosnumerator, 2*precision]*
      toexactnumericalvalue[thetaexact[ir2], cosdenominator, cosnumerator, 2*precision]
     )
     , {ir3, fusion[dual[ir1], ir2]}]
    , {ir1, irreps}, {ir2, irreps}];
    
    smatexact = 
     Table[
     {Rationalize[Arg[smatexactnum1[[i, j]]]/Pi],
      findexactabs[Abs[smatexactnum1[[i, j]]]^2, cosdenominator, cosnumerator]}
     , {i, 1, numofirreps}, {j, 1, numofirreps}];
   
   
    smatexactnum = 
    Table[
     toexactnumericalvalue[smatexact[[i, j]], cosdenominator, cosnumerator, 2*precision]
    , {i, 1, numofirreps}, {j, 1, numofirreps}];
    
    smatexactok = 
    (Chop[smatexactnum1 - smatexactnum, 10^(-(2*precision - 20))] // Flatten // Union) == {0};
    
    realphasepos = Position[smatexact, x_ /; Head[x[[1]]] == Real, {2}, Heads -> False];
    denom = LCM[Sequence @@ (Denominator /@ (smatexact[[All, All, 2]] // Flatten) // Union)];
    Do[
      currreal = smatexact[[Sequence @@ realphasepos[[i]]]][[1]];
      currreal100 = N[currreal, 100];
      gamma = If[currreal100 > 0, 0, 1];
      beta = If[Abs[currreal100] < 0.5, 0, 1];
      coefs = findexactabs[Cos[Pi*currreal100]^2, denom, 1];
      alphacheck = (-1)^gamma ArcCos[toexactnumericalvalue[{beta,coefs}, denom, 1, 2*precision]]/Pi;
      alphacheckok = Chop[alphacheck-currreal, 10^(-(2 * precision - 20))] == 0;
      If[alphacheckok,
       smatexact[[Sequence @@ realphasepos[[i]], 1]] = {gamma, {beta, coefs}, denom};,
       smatexact[[Sequence @@ realphasepos[[i]], 1]] = currreal100;,
       smatexact[[Sequence @@ realphasepos[[i]], 1]] = currreal100;
      ];
    , {i, 1, Length[realphasepos]}];
    
            
    If[smatexactok, Null,
     Print["The exact representations of the elements of the S-matrix do \
not agree with the numerical ones :-("];,
     Print["The exact representations of the elements of the S-matrix do \
not agree with the numerical ones :-("];
     ];
    
    
    tmatexact = 
     Table[
     If[i == j,
     thetalistexact[[i]], 
     zerorep[cosdenominator]
     ]
     , {i, 1, numofirreps}, {j, 1, numofirreps}];
    
   tmatexactnum = 
     Table[
     toexactnumericalvalue[tmatexact[[i, j]], cosdenominator, cosnumerator, 2 * precision]
       , {i, 1, numofirreps}, {j, 1, numofirreps}];
       
    cmatexact = Table[
    If[cmat[[i, j]] == 1, onerep[cosdenominator], zerorep[cosdenominator]]
    , {i, 1, numofirreps}, {j, 1, numofirreps}];
        
    pplusexactnum1 =
     Sum[
      (toexactnumericalvalue[qdimexact[ir], cosdenominator, cosnumerator, 2*precision]^2)*
       toexactnumericalvalue[thetaexact[ir], cosdenominator, cosnumerator, 2*precision]
     , {ir, irreps}];
    
    pplusexact = {Rationalize[Arg[pplusexactnum1] / Pi], 
      findexactabs[Abs[pplusexactnum1]^2, cosdenominator, cosnumerator]}; 
    
    pplusexactnum = toexactnumericalvalue[pplusexact, cosdenominator, cosnumerator, 2*precision];
    pplusexactok = Chop[pplusexactnum1 - pplusexactnum, 10^(-(2 * precision - 20))] == 0;
   
                 
    pminusexactnum1 =
     Sum[
      (toexactnumericalvalue[qdimexact[ir], cosdenominator, cosnumerator, 2*precision]^2) / 
       toexactnumericalvalue[thetaexact[ir], cosdenominator, cosnumerator, 2*precision]
     , {ir, irreps}];
    
    pminusexact = {Rationalize[Arg[pminusexactnum1] / Pi], 
      findexactabs[Abs[pminusexactnum1]^2, cosdenominator, cosnumerator]}; 

        
    pminusexactnum = toexactnumericalvalue[pminusexact, cosdenominator, cosnumerator, 2*precision];
    pminusexactok = Chop[pminusexactnum1 - pminusexactnum, 10^(-(2 * precision - 20))] == 0;    
    
     If[pplusexactok && pminusexactok, Null,
     Print["The exact representations of pplus and/or pminus do \
not agree with the numerical ones :-("];,
     Print["The exact representations of pplus and/or pminus do \
not agree with the numerical ones :-("];
     ];   
    
    ssdagmaxdev = 
     Max[Abs /@ ((smatexactnum.ConjugateTranspose[smatexactnum] - 
           IdentityMatrix[numofirreps]) // Flatten)];
    sdagsmaxdev = 
     Max[Abs /@ ((ConjugateTranspose[smatexactnum].smatexactnum - 
           IdentityMatrix[numofirreps]) // Flatten)];
    
    modularexact = Chop[Max[ssdagmaxdev, sdagsmaxdev], 10^(-(2*precision - 20))] == 0;
    
    If[modularexact,
     
     st3maxdev = 
      Max[Abs /@ ((MatrixPower[smatexactnum.tmatexactnum, 3] - 
            Exp[2 Pi I/8*centralcharge]*smatexactnum.smatexactnum) // 
          Flatten)];
     
     s2maxdev = 
      Max[Abs /@ ((smatexactnum.smatexactnum - cmat) // Flatten)];
     
     modularrelationsexactok = 
      Chop[Max[s2maxdev, st3maxdev], 10^(-(2*precision - 20))] == 0;,
      
     modularrelationsexactok = Missing["The theory is not modular, so there are no modular relations to be checked."];,
     modularrelationsexactok = Missing["The theory is not modular, so there are no modular relations to be checked."];
   ];
     
    modulardataexactok = And @@ {pivotexactok, frobschurexactok, FPdimexactok, 
       qdimexactok, qdtot2exactok, qdtotexactok, qdtot1overexactok,
       twistsarephases, thetalistexactok, smatexactok, pplusexactok, pminusexactok,
       If[modularexact, modularrelationsexactok, True]}; 
     
    Print["The exact form of the pivotal structure is: ", pivotlistexact];
     
    
    If[pivotexactok,
     Print["The pivotal equations were checked with the exact form of the \
F-symbols, with precision ", 2*precision, " and they hold with accuracy ", 
       Floor[Accuracy[pivotmaxdev]], " :-)"];,
     Print["The pivotal equations were checked with the exact form of the \
F-symbols, with precision ", 2*precision, ", but they do not hold with accuracy ", 2 * precision - 20, ". \
The maximum deviation is ", pivotmaxdev, " :-(" ];,
     Print["The pivotal equations were checked with the exact form of the \
F-symbols, with precision ", 2*precision, ", but they do not hold with accuracy ", 2 * precision - 20, ". \
The maximum deviation is ", pivotmaxdev, " :-(" ];
     ];
     
    Print["The exact forms of the Frobenius-Schur indicators are: ", frobschurlistexact];
        
    Print["The exact forms of the Frobenius-Perron dimensions are: ", 
     FPdimlistexact];
    
    Print["The exact forms of the quantum dimensions are: ", qdimlistexact];

    Print["The exact forms of the twist factors are: ", thetalistexact];
     
    
    If[modularexact && qdimspositiveexact,
     Print["The exact form of the S-matrix is given by:"]; 
     Print[MatrixForm[smatexact, TableDepth -> 2]];
     ];
    
    If[modularexact && Not[qdimspositiveexact],
     Print["The exact form of the S-matrix is given by (up to an overall \
sign):"]; 
     Print[MatrixForm[smatexact, TableDepth -> 2]];
     ];
    
    If[Not[modularexact] && qdimspositiveexact,
     Print["The exact form of the (non-modular!) S-matrix is given by:"]; 
     Print[MatrixForm[smatexact, TableDepth -> 2]];
     ];
    
    If[Not[modularexact] && Not[qdimspositiveexact],
     Print["The exact form of the (non-modular!) S-matrix is given by (up \
to an overall sign):"]; 
     Print[MatrixForm[smatexact, TableDepth -> 2]];
     ];
    
    
    If[modularexact && modularrelationsexactok,
     Print["The modular relations Exp[- 2 Pi I/8 centralcharge](S.T)^3 = \
S^2 = C were checked with the exact form of the S-matrix, with precision ", 2*precision,
" and they hold with accuracy ", Floor[Accuracy[Max[s2maxdev, st3maxdev]]] , " :-)"];
     ];
    If[modularexact && Not[modularrelationsexactok],
     Print["The modular relations Exp[- 2 Pi I/8 centralcharge](S.T)^3 = \
S^2 = C were checked with the exact form of the S-matrix, with precision ", 2*precision,
", but they do not hold with accuracy ", 2 * precision -20, ". The maxium deviation \
is ", Max[s2maxdev, st3maxdev], " :-( Better check what went wrong!"];
     ];
    
    If[modulardataexactok,
       Print["Done, all relations hold :-)"];,
       Print["Done, not all relations hold :-("];,
       Print["Done, not all relations hold :-("];
       ];
    
  
    Global`FPdimlistexact = FPdimlistexact;
    Global`qdimlistexact = qdimlistexact;
    Global`pivotlistexact = pivotlistexact;
    Global`thetalistexact = thetalistexact;
    Global`FSlistexact = frobschurlistexact;
    Global`smatexact = smatexact;
    Global`tmatexact = tmatexact;
    Global`cmatexact = cmatexact;
    
    (* end of the actual calculation *)
    
    modulardataexactfound = True;
    
    ];
   
   
   ];


(* ::Subsection::Closed:: *)
(*Routines to check the exact representation of the modular data algebraically*)


reducesum[cosdenomorg_, cosnumorg_, sumlist_] := Module[
   {cosdenom, cosnum, alldenoms, newcosdenom, newnumberfield,
    numvalue, arg, sumlistabs, sqsum, cosexpnum, sign, cosexpression, 
    cosexpressionok, costab, costabsum, cos2expnum, sintab, 
    cos2expression, cos2expressionok, sinexpression, sqsintab, 
    sqsintabsum, imok, res, numres, resok, done, sumlistreduced},
    
   
   done = False;
   newnumberfield = False;
   numvalue = Sum[toexactnumericalvalue[term, cosdenomorg, cosnumorg, 100], {term, sumlist}];
   sumlistreduced = Table[
     If[sumlist[[i,2]] == zerorep[cosdenomorg][[2]],
     zerorep[cosdenomorg],
     sumlist[[i]]
     ]
     , {i, 1, Length[sumlist]}];
   sumlistreduced = DeleteCases[sumlistreduced, zerorep[cosdenomorg]];
   sumlistreduced = Table[
    {Mod[sumlistreduced[[i,1]],2,-1]/.{-1->1},sumlistreduced[[i,2]]}
    , {i, 1, Length[sumlistreduced]}];
   
   
   sumlistreduced = combinesum[sumlistreduced];
   sumlistreduced = removepairs[cosdenomorg, sumlistreduced];
        
   If[sumlistreduced == {},
    done = True;
    res = zerorep[cosdenomorg];
   ];
   
   If[Not[done] && Length[sumlistreduced] == 1,
   done = True;
   res = sumlistreduced[[1]];
   ];
   
   If[
   Not[done],

   
   (* we will first check if we need to work in a different numberfield, this is sometimes necessary *)
   
   alldenoms = Table[Denominator[sumlistreduced[[i,1]]], {i, 1, Length[sumlistreduced]}];
   newcosdenom = LCM[Sequence @@ alldenoms, cosdenomorg];
   
   (* in this case, we do not need a new numberfield *)
   If[newcosdenom == cosdenomorg,
     cosdenom = cosdenomorg;
     cosnum = cosnumorg;   
   ];
   
   (* in this case, we do need a new numberfield, so we will write sumlistreduced in terms of the new
   representation, which has cosnum = 1 by default. *)
   If[newcosdenom > cosdenomorg,
     newnumberfield = True;
     cosdenom = newcosdenom;
     cosnum = 1;
     sumlistreduced = Table[
       {sumlistreduced[[i,1]], newabsrep[cosdenomorg, cosnumorg, cosdenom, sumlistreduced[[i,2]]]}
       , {i, 1, Length[sumlistreduced]}];
   ];
   
   (* In this case, something went wrong! *)
   If[newcosdenom < cosdenomorg,
   Print["The denominator for the new numberfield is smaller than \
the original denominator. Something went wrong!"];
   ];
   
   
   imok = True;
  
   
   (* remove the phase; we'll check that the imagingary part is zero; 
   add the phase back in the end *)
   arg = Rationalize[Arg[numvalue]/Pi];
   sumlistabs = Table[{term[[1]] - arg, term[[2]]}, {term, sumlistreduced}];
   (* we start by calculating the square, term by term *)
   
   sqsum = Flatten[
     Table[
      If[i == j,
       {2 sumlistabs[[i, 1]], sumlistabs[[i, 2]]},
       {sumlistabs[[i, 1]] + sumlistabs[[j, 1]],
        2 findsqrt[cosdenom, cosnum, 
        exactproductnophase[cosdenom, sumlistabs[[i, 2]], sumlistabs[[j, 2]]]]
        }
       ]
      , {i, 1, Length[sumlistabs]}, {j, i, Length[sumlistabs]}]
     , 1];
   
    (* to take the phases into account, we write e^i\phi = cos\phi + 
       I sin\phi, and find an expression
       for the cos\phi part, in terms of the numberfield we are using, 
       and multiply it with sqsum, which
       does not contain square roots anymore  *)
   
   costab = DeleteCases[
     Table[
      cosexpnum = N[Cos[Pi sqsum[[i, 1]]], 200];
      sign = Sign[cosexpnum // Chop];
      cosexpression = findexactabs[sign*cosexpnum, cosdenom, cosnum];
      cosexpressionok = FullSimplify[
        toexactvalue[{0, cosexpression}, cosdenom, cosnum]^2 
        - sign*Cos[Pi sqsum[[i, 1]]] == 0
        ];
      If[cosexpressionok, Null, 
       Print["An expression for a cosine could not be checked exactly!"];,
       Print["An expression for a cosine could not be checked exactly!"];
       ];
      If[sign == 1,
       cosexpression = exactproduct[cosdenom, {0, cosexpression}, {0, sqsum[[i, 2]]}];
       ];
      If[sign == -1,
       cosexpression = exactproduct[cosdenom, {1, cosexpression}, {0, sqsum[[i, 2]]}];
       ];
      If[sign == 0, cosexpression = Null;];
      cosexpression
      , {i, 1, Length[sqsum]}]
     , Null];
   costabsum = 
    Sum[((-1)^costab[[i, 1]]) costab[[i, 2]]
    , {i, 1, Length[costab]}];
   If[costabsum == 0,
    costabsum = zerorep[cosdenom][[2]];
    ];
   
   
   (* for the imaginary part, we first evaluate sin\phi = +/- sqrt(1-
   cos\phi^2),
       and multiply it with the sqrts of the equation. 
       we then take the sqruare of the result, 
       and find expressions for the sqrts appearing, 
       which turns out to be possible.
       finally, 
   we add things upp to see if it all sums to zero as it should *)

   sintab = DeleteCases[
     Table[
      sign = Sign[N[Sin[Pi sumlistabs[[i, 1]]], 200] // Chop];
      cos2expnum = N[Cos[Pi sumlistabs[[i, 1]]]^2, 200];
      cos2expression = findexactabs[cos2expnum, cosdenom, cosnum];
      cos2expressionok = 
       FullSimplify[toexactvalue[{0, cos2expression}, cosdenom, cosnum]^2 
        - Cos[Pi sumlistabs[[i, 1]]]^2 == 0];
      If[cos2expressionok, Null, 
       Print["An expression for a cos^2 could not be checked exactly!"];, 
       Print["An expression for a cos^2 could not be checked exactly!"];
       ];
      If[sign == 1, 
       sinexpression = {0, Table[If[i == 1, 1, 0], {i, 1, Length[cos2expression]}] - 
          cos2expression}];
      If[sign == -1, 
       sinexpression = {1, Table[If[i == 1, 1, 0], {i, 1, Length[cos2expression]}] - 
          cos2expression}];
      If[sign == 0,
       Null, exactproduct[cosdenom, sinexpression, {0, sumlistabs[[i, 2]]}]
       ]
      , {i, 1, Length[sumlistabs]}]
     , Null];
   
   sqsintab = Flatten[
     Table[
      If[i == j,
       {2 sintab[[i, 1]], 
        sintab[[i, 2]]}, {(sintab[[i, 1]] + sintab[[j, 1]]), 
        2 findsqrt[cosdenom, cosnum, 
          exactproductnophase[cosdenom, sintab[[i, 2]], 
           sintab[[j, 2]]]]}]
      , {i, 1, Length[sintab]}, {j, i, Length[sintab]}]
     , 1];
   
   sqsintabsum = 
    Sum[(-1)^(sqsintab[[i, 1]]) sqsintab[[i, 2]], {i, 1, Length[sqsintab]}];
   
   If[sqsintabsum == 0, 
    sqsintabsum = zerorep[cosdenom][[2]];];
   
   If[sqsintabsum == zerorep[cosdenom][[2]],
    Null,
    Print[
     "After removing the phase, the imaginary part is not zero, something went wrong!"];
    imok = False;,
    Print[
     "After removing the phase, the imaginary part is not zero, something went wrong!"];
    imok = False;
    ];
   
   If[newnumberfield,
   res = {arg, newabsrep[cosdenom, cosnum, cosdenomorg, cosnumorg, costabsum]},
   res = {arg, costabsum};
   ];
   
   ]; (* end if Not[done] *)
   
   numres = toexactnumericalvalue[res, cosdenomorg, cosnumorg, 100];
   
   If[Chop[numres - numvalue, 10^(-90)] == 0,
    resok = True;,
    resok = False;
    Print[
     "The result is numerically not equal to the input, something went wrong!"];,
    resok = False;
    Print[
     "The result is numerically not equal to the input, something went wrong!"];
    ];
   
   If[resok,
    res,
    sumlistreduced,
    sumlistreduced
    ]
   
   ];


combinesum[list_] := Module[{newlist, coefs, mult},
   
   (* make sure that the range of the phases is (-1,1] *)
   
   newlist = Table[
     {Mod[list[[i, 1]], 2, -1]/.{-1->1}, list[[i, 2]]}
     , {i, 1, Length[list]}];
   
   coefs = newlist // Union;
   
   Table[
    mult = Count[newlist, coefs[[i]]];
    {coefs[[i, 1]], mult^2*coefs[[i, 2]]}
    , {i, 1, Length[coefs]}]
   
   ];


combinesumnophases[list_] := Module[{listint, coefs, res, mult},
   listint = 
    Table[{Mod[list[[i, 1]], 2], list[[i, 2]]}
    , {i, 1, Length[list]}];
   
   coefs = listint[[All, 2]] // Union;
   
   res = Table[
     mult = 
      Count[listint, x_ /; x[[2]] == coefs[[i]] && x[[1]] == 0] - 
       Count[listint, x_ /; x[[2]] == coefs[[i]] && x[[1]] == 1];
     Which[
      mult == 0, {},
      mult > 0, {0, mult^2*coefs[[i]]},
      mult < 0, {1, mult^2*coefs[[i]]}
      ]
     , {i, 1, Length[coefs]}];
   
   res = DeleteCases[res, {}];
   
   If[res == {}, res = {{0, Table[0, Length[list[[1, 2]]]]}}];
   
   res
   
   ];


combinesumsymmetricphases[cosdenom_, cosnum_, sumlist_] := 
  Module[{res, denomlist, denomlist2, denomlist2union, rest, tab, 
    result, curr, currcoefs, currphases, phasesok, currcos, sign, 
    currcos2exact, currcosnum, cosexpressionok},
   
   denomlist = 
    Table[{Denominator[sumlist[[i, 1]]], sumlist[[i, 2]]}
    , {i, 1, Length[sumlist]}];
   
   denomlist2 = Cases[denomlist, x_ /; x[[1]] > 1];
   denomlist2union = denomlist2 // Union;
   rest = Cases[sumlist, x_ /; Denominator[x[[1]]] == 1];
   
   tab = Table[
     curr = 
      Cases[sumlist, x_ /; Denominator[x[[1]]] == denomlist2union[[i, 1]] && 
         x[[2]] == denomlist2union[[i, 2]]];
     currcoefs = denomlist2union[[i, 2]];
     currphases = curr[[All, 1]] // Sort;
     phasesok = (Mod[Length[currphases], 2] == 0) &&
     (And @@  Table[currphases[[i]] == -currphases[[Length[currphases] + 1 - i]]
     , {i, 1, Floor[Length[currphases]/2]}]);
     
     If[phasesok,
      currcos = 
       Sum[2 Cos[Pi currphases[[i]]], {i, 1, Length[currphases]/2}];
      currcosnum = 
       Sum[N[2 Cos[Pi currphases[[i]]], 200]
       , {i, 1, Length[currphases]/2}];
      sign = Sign[currcosnum // Chop];
      currcos2exact = findexactabs[currcosnum^2, cosdenom, cosnum];
      result = {If[sign == -1, 1, 0], 
        exactproductnophase[cosdenom, currcos2exact, currcoefs]};
      
      cosexpressionok = FullSimplify[
        sign * toexactvalue[{0, currcos2exact}, cosdenom, cosnum] - currcos == 0
        ];
      If[cosexpressionok, Null, 
       Print["An expression for a cosine could not be checked exactly!"];
       , Print["An expression for a cosine could not be checked exactly!"];
       ];
      
      ];
     
     result
     
     , {i, 1, Length[denomlist2union]}];
   
   res = Join[tab, rest];
   res = Table[{Mod[res[[i, 1]], 2], res[[i, 2]]}
   , {i, 1, Length[res]}];
   res
   ];


findoneoverabs[cosdenom_,coslist_]:=
Module[{eqn,sol,var},
 eqn = exactproductnophase[cosdenom, coslist, Table[var[i], {i, 0, Length[coslist] - 1}]];
 sol = Solve[eqn == onerep[cosdenom][[2]]];
 sol[[1, All, 2]]
];


findoneover[cosdenom_, number_] :=
 {-number[[1]], findoneoverabs[cosdenom, number[[2]]]};


abs2[cosdenom_, list_] :=
  Flatten[
   Table[exactproduct[cosdenom, list[[i]], {-list[[j, 1]], list[[j, 2]]}]
    , {i, 1, Length[list]}, {j, 1, Length[list]}]
   , 1];


nqexact[cosdenom_, cosnum_, n_] := 
  Module[{numvalue, sign, exactabs2, exactrep, exactcos, exactok},
   
   numvalue = 
    If[Mod[n, cosdenom] != 0, 
     Chop[N[nq[n, 1] /. q -> Exp[2 Pi I cosnum/cosdenom], 200]],
     N[0, 200]];
   sign = Sign[numvalue];
   exactabs2 = findexactabs[numvalue^2, cosdenom, cosnum];
   exactrep = {If[sign == 0 || sign == 1, 0, 1], exactabs2};
   exactcos = toexactvalue[exactrep, cosdenom, cosnum];
   
   (* The sign is ok `automatically', 
   so we'll only algebraically check the square of the expression *)
  
   exactok = 
    FullSimplify[
    (Sin[n*Pi cosnum/cosdenom]^2 // TrigReduce) == (Sin[Pi cosnum/cosdenom]^2 exactcos^2 //  TrigReduce)];
   
   If[exactok,
    exactrep,
    Print[
     "The expression for the q-number could not be verified exactly!"];
    exactrep,
    Print[
     "The expression for the q-number could not be verified exactly!"];
    exactrep
    ]
   
   ];


exactproduct[cosdenom_, factorlist_List] := Module[{res},
   res = onerep[cosdenom];
   Do[
    res = exactproduct[cosdenominator, res, factorlist[[i]]]
    , {i, 1, Length[factorlist]}];
   res
   ];


removepairs[cosdenom_, list_] := 
  Module[{newlist, coeflist, currterms, currphases, numofpairs},
   
   (* make sure that the phases are in the range [-1,1) *)
   
   newlist = 
    Table[{Mod[list[[i, 1]], 2, -1], list[[i, 2]]}
    , {i, 1, Length[list]}];
   
   coeflist = newlist[[All, 2]] // Union;
   
   Do[
    currterms = Cases[newlist, x_ /; x[[2]] == coeflist[[i]], {1}];
    currphases = currterms[[All, 1]] // Union;
    currphases = Cases[currphases, x_ /; x >= 0, {1}];
    Do[
     numofpairs = 
      Min[Count[newlist, {currphases[[j]], coeflist[[i]]}], 
       Count[newlist, {currphases[[j]] - 1, coeflist[[i]]}]];
     newlist = 
      DeleteCases[newlist, {currphases[[j]], coeflist[[i]]}, {1}, numofpairs];
     newlist = 
      DeleteCases[newlist, {currphases[[j]] - 1, coeflist[[i]]}, {1}, numofpairs];
     , {j, 1, Length[currphases]}];
    , {i, 1, Length[coeflist]}];
   
   If[newlist == {},
    newlist = {zerorep[cosdenom]};
    ];
   
   newlist = Table[{newlist[[i,1]]/.{-1 -> 1}, newlist[[i,2]]}, {i, 1, Length[newlist]}];
   
   newlist
   
   ];


checkmodulardataalgebraically[] := Module[
   {allok,
    qdimalgebraic, qdimalgebraicok, qdimsreal,
    qdtotalgebraic, qdtot2algebraic, qdtotalgebraicok, 
    qdtot2algebraicok,
    qdim1overfalgebraic, qdim1overfalgebraicok,
    pivotalgebraic, pivotalgebraicok,
    thetalistalgebraic, thetalistalgebraicok,
    thetasum, sumres, qd, res,
    frobschurlistalgebraic, frobschurlistalgebraicok,
    fptab, fpsum, frobschur, result,
    smatalgebraic, smatalgebraicok,
    smattab, finalres,
    pminusalgebraic, pplusalgebraic, pplustab, pplustabcoefsunion, 
    pminustab, pminustabcoefsunion,
    modular2algebraic, modular2algebraicok, centralchargealgebraic, 
    centralchargealgebraicok,
    ssdagalgebraic, sdagsalgebraic,
    modularalgebraic, modularalgebraicok,
    s2algebraic, s2algebraicok,
    altcosdenom, coefslist, coefsreplace, newrep, altsmatalgebraic, 
    altstalgebraic, altstalgebraiccoefs,
    altst3algebraic, altst3calgebraic, altst3cmincalgebraic,
    altst3cmincreducealgebraic,
    altstalgebraicsqrt, altstalgebraicsqrtreplace, 
    altstalgebraicnosqrt,
    elem, elemabs2, done, currphases, altst3calgebraicnosqrt, 
    st3calgebraicok,
    currterms
    },

   If[Not[typeranklevelrootinitok],
    Print[
      "The type of algebra, rank, level and/or rootfactor were not \
(correctly) initialized, please do so first, followed by calculating the F- and R-symbols, \
the modular data and obtaining the exact form of the F- and R-symbols \
and the modular data, before trying to verify the \
exact form of the modular data algebraicaly."];
    ];

   If[typeranklevelrootinitok &&
      (Not[fsymbolscalculated] || Not[rsymbolscalculated] || Not[modulardatacalculated]),
      Print["The F-symbols, R-symbols and/or the modular data were not calculated. \
Please calculate these first, and obtain their exact form, before trying to verify the \
exact form of the modular data algebraically."];
    ]; 

   If[typeranklevelrootinitok && fsymbolscalculated && rsymbolscalculated && modulardatacalculated &&
      (Not[fsymbolsexactfound] || Not[rsymbolsexactfound] || Not[modulardataexactfound]),
      Print["The exact form of the F-symbols, R-symbols and/or the modular data were not obtained. \
Please obtain these first, before trying to verify the \
exact form of the modular data algebraically."];
    ]; 

If[typeranklevelrootinitok && fsymbolscalculated && rsymbolscalculated && modulardatacalculated && 
    fsymbolsexactfound && rsymbolsexactfound && modulardataexactfound,
      
   (* The actual calculation! *)   
            
   allok = True;
   
   Print["Checking the quantum dimensions algebraically..."];
   
   qdimalgebraic = 
    Table[
    exactproduct[cosdenominator, 
     Table[
      exactproduct[cosdenominator,
        nqexact[cosdenominator, cosnumerator, tmax (ir + rho).qfm.posroots[[pr]]], 
        findoneover[cosdenominator, 
         nqexact[cosdenominator, cosnumerator, tmax (rho).qfm.posroots[[pr]]]]]
      , {pr, 1, numposroots}]]
     , {ir, irreps}];
   
   qdimalgebraicok = 
    qdimlistexact[[All, 2]] ==  qdimalgebraic[[All, 2]] &&
    (Mod[#, 2] & /@  qdimlistexact[[All, 1]]) == (Mod[#, 2] & /@ qdimalgebraic[[All, 1]]);
   
   If[qdimalgebraicok,
    Print["The exact form of the quantum dimensions coincides with \
the algebraically obtained one :-)"];,
    Print["The exact form of the quantum dimensions does not coincide with \
the algebraically obtained one :-("];
    allok = False;,
    Print["The exact form of the quantum dimensions does not coincide with \
the algebraically obtained one :-("];
    allok = False;
    ];
   
   qdimsreal = Union[Mod[#, 1] & /@ qdimalgebraic[[All, 1]]] == {0};
   
   If[qdimsreal, Null,
    Print["The algebraic form of the quantum dimensions are not all real :-("];
    allok = False;,
    Print["The algebraic form of the quantum dimensions are not all real :-("];
    allok = False;
    ];
   
   qdtotalgebraic = {0, Plus @@ qdimalgebraic[[All, 2]]};
   qdtot2algebraic = {0, 
     exactproductnophase[cosdenominator, 
      Plus @@ qdimalgebraic[[All, 2]], 
      Plus @@ qdimalgebraic[[All, 2]]]
      };
   
   qdtotalgebraicok = qdtotalgebraic == qdtotexact;
   qdtot2algebraicok = qdtot2algebraic == qdtot2exact;
   
   
   qdim1overfalgebraic = Table[
     findoneover[
      cosdenominator,
      fsymexact[irreps[[i]], irrepsdual[[i]], irreps[[i]], irreps[[i]], irreps[[1]], irreps[[1]]
      , {1, 1, 1, 1}]
      ]
     , {i, 1, numofirreps}];
   
   qdim1overfalgebraicok = 
    qdim1overfalgebraic[[All, 2]] ==  qdim1overfexactlist[[All, 2]] &&
    (Mod[#, 2] & /@  qdim1overfalgebraic[[All, 1]]) == (Mod[#, 2] & /@ qdim1overfexactlist[[All, 1]]);

   Print["Checking the pivotal structure algebraically..."];
   
   pivotalgebraic = Table[
     exactproduct[cosdenominator,
      qdimalgebraic[[i]],
      fsymexact[irreps[[i]], irrepsdual[[i]], irreps[[i]], irreps[[i]], irreps[[1]], irreps[[1]]
      , {1, 1, 1, 1}]
      ]
     , {i, 1, numofirreps}];
   
   pivotalgebraicok = 
    pivotalgebraic[[All, 2]] == pivotlistexact[[All,  2]] &&
    (Mod[#, 2] & /@ pivotalgebraic[[All, 1]]) == (Mod[#, 2] & /@ pivotlistexact[[All, 1]]);

   If[pivotalgebraicok,
    Print["It was checked algebraically that the pivotal equations are satisfied :-)"];,
    Print["At least one of the pivotal equations does not hold algebraically :-("];
    allok = False;,
    Print["At least one of the pivotal equations does not hold algebraically :-("];
    allok = False;
    ];

   Print["Checking the twist factors algebraically..."];   
         
   thetalistalgebraic = Table[
     thetasum = 
      Flatten[Table[
        exactproduct[cosdenominator, qdimexact[ir1], 
         rsymexact[ir, ir, ir1, {v, v}]]
         , {ir1, fusion[ir, ir]}, {v, 1, nv[ir, ir, ir1]}], 1];
     
     sumres = reducesum[cosdenominator, cosnumerator, thetasum];
     qd = qdimexact[ir];
     If[sumres[[2]] == qd[[2]],
      res = {sumres[[1]] - qd[[1]], Table[If[i == 1, 1, 0], {i, 1, Length[qd[[2]]]}]};,
      Print["The twistfactor is not a phase, something went wrong!"];
      res = Null;,
      Print["The twistfactor is not a phase, something went wrong!"];
      res = Null;
      ];
     res
     , {ir, irreps}];
   
   thetalistalgebraicok = 
    thetalistexact[[All, 2]] == thetalistalgebraic[[All, 2]] &&
    (Mod[#, 2] & /@ thetalistexact[[All, 1]]) == (Mod[#, 2] & /@ thetalistalgebraic[[All, 1]]);


   If[thetalistalgebraicok,
    Print["The exact form of the twist factors coincides with \
the algebraically obtained one :-)"];,
    Print["The exact form of the twist factors does not coincide with \
the algebraically obtained one :-("];
    allok = False;,
    Print["The exact form of the twist factors does not coincide with \
the algebraically obtained one :-("];
    allok = False;
    ];

                  
   Print["Checking the Frobenius-Schur indicators algebraically..."];
   
   frobschurlistalgebraic = Table[
     fptab = Flatten[
       Table[
        res = 
         exactproduct[cosdenominator, qdimexact[ir1], qdimexact[ir2]];
        res[[1]] = 
         Mod[res[[1]] + 2 thetaexact[ir1][[1]] - 2 thetaexact[ir2][[1]], 2, -1];
        res[[2]] = (nv[ir, ir1, ir2])^2 * res[[2]];
        res
        , {ir1, irreps}, {ir2, fusion[ir, ir1]}]
       , 1];
     
     result =
      combinesumnophases[combinesumsymmetricphases[cosdenominator, cosnumerator, fptab]];
     
     result = reducesum[cosdenominator, cosnumerator, result];
     
     result =
      Which[
       result[[2]] == Table[0, Length[result[[2]]]],
       result,
       result[[2]] == qdtot2exact[[2]],
       {result[[1]], Table[If[i == 1, 1, 0], {i, 1, Length[result[[2]]]}]},
       True, 
       Print["The FS indicactor is neither zero, nor +/- 1. Better check!"];
       result
       ];
     
     result
     
     
     , {ir, irreps}];
   
   frobschurlistalgebraicok = 
    frobschurlistalgebraic[[All, 2]] == frobschurlistexact[[All, 2]] &&
    (Mod[#, 2] & /@ frobschurlistalgebraic[[All, 1]]) == (Mod[#, 2] & /@ frobschurlistexact[[All, 1]]);

   If[frobschurlistalgebraicok,
    Print["The exact form of the Frobenius-Schur indicators coincides with \
the algebraically obtained one :-)"];,
    Print["The exact form of the Frobenius-Schur indicators does not coincide with \
the algebraically obtained one :-("];
    allok = False;,
    Print["The exact form of the Frobenius-Schur indicators does not coincide with \
the algebraically obtained one :-("];
    allok = False;
    ];


   Print["Calculating the S-matrix algebraically..."];
   
   smatalgebraic = Table[
     smattab = Table[
       res = qdimexact[ir3];
       res[[1]] = 
        Mod[res[[1]] + thetaexact[ir3][[1]] - thetaexact[ir1][[1]] -  thetaexact[ir2][[1]], 2, -1];
       res[[2]] = (nv[dual[ir1], ir2, ir3])^2 * res[[2]];
       res
       , {ir3, fusion[dual[ir1], ir2]}];
     finalres = reducesum[cosdenominator, cosnumerator, smattab];
     finalres = exactproduct[cosdenominator, qdtot1overexact, finalres]
     
     , {ir1, irreps}, {ir2, irreps}];
   
   smatalgebraicok = 
    Flatten[smatalgebraic, 1][[All, 2]] == Flatten[smatexact, 1][[All, 2]] &&
    (Mod[#, 2] & /@ Flatten[smatalgebraic, 1][[All, 1]]) ==
    (Mod[#, 2] & /@ Flatten[smatexact, 1][[All, 1]]);

   
   pplustab = Table[
     res = 
      exactproduct[cosdenominator, qdimexact[ir], qdimexact[ir]];
     res[[1]] = Mod[res[[1]] + thetaexact[ir][[1]], 2, -1];
     res
     , {ir, irreps}];
   pplustabcoefsunion = pplustab[[All, 2]] // Union;
   res = Table[
     reducesum[cosdenominator, cosnumerator,
       removepairs[cosdenominator,
        combinesum[Cases[pplustab, x_ /; x[[2]] == ccoefs]]
       ]
     ]
     , {ccoefs, pplustabcoefsunion}];
   
   pplusalgebraic = reducesum[cosdenominator, cosnumerator, 
    removepairs[cosdenominator, combinesum[res]] 
   ];
   
   pminustab = Table[
     res = 
      exactproduct[cosdenominator, qdimexact[ir], qdimexact[ir]];
     res[[1]] = Mod[res[[1]] - thetaexact[ir][[1]], 2, -1];
     res
     , {ir, irreps}];
   pminustabcoefsunion = pminustab[[All, 2]] // Union;
   res = Table[
     reducesum[cosdenominator, cosnumerator,
      removepairs[cosdenominator,
       combinesum[Cases[pminustab, x_ /; x[[2]] == ccoefs]]
      ]
     ]
     , {ccoefs, pminustabcoefsunion}];
   
   pminusalgebraic = reducesum[cosdenominator, cosnumerator, 
    removepairs[cosdenominator, combinesum[res]] 
   ];
   

   
   modular2algebraic = 
    exactproduct[cosdenominator, pplusalgebraic, pminusalgebraic] == qdtot2algebraic;

         
   modular2algebraicok = modular2algebraic == modular2;
   
   If[modular2algebraic,
    centralchargealgebraic = 4 * pplusalgebraic[[1]];
    centralchargealgebraicok = centralchargealgebraic == centralcharge;
    ];
   
   
   ssdagalgebraic = Table[
     reducesum[cosdenominator, cosnumerator,
      removepairs[cosdenominator,
      Table[
       exactproduct[cosdenominator, smatalgebraic[[i, k]],
       {-smatalgebraic[[k, j, 1]], smatalgebraic[[k, j, 2]]}]
       , {k, 1, numofirreps}]
       ]
      ]
     , {i, 1, numofirreps}, {j, 1, numofirreps}];
    ssdagalgebraic = Table[
     If[ssdagalgebraic[[i, j, 2]] == zerorep[cosdenominator][[2]],
       zerorep[cosdenominator],
       ssdagalgebraic[[i, j]]
     ]
    , {i, 1, numofirreps}, {j, 1, numofirreps}];
     
   sdagsalgebraic = Table[
     reducesum[cosdenominator, cosnumerator,
     removepairs[cosdenominator,
      Table[exactproduct[
        cosdenominator, {-smatalgebraic[[i, k, 1]], smatalgebraic[[i, k, 2]]},
        smatalgebraic[[k, j]]]
       , {k, 1, numofirreps}]
      ]
      ]
     , {i, 1, numofirreps}, {j, 1, numofirreps}];
     sdagsalgebraic = Table[
     If[sdagsalgebraic[[i, j, 2]] == zerorep[cosdenominator][[2]],
       zerorep[cosdenominator],
       sdagsalgebraic[[i, j]]
     ]
    , {i, 1, numofirreps}, {j, 1, numofirreps}];
     
   
   modularalgebraic = 
    ContainsOnly[
      Flatten[ssdagalgebraic, 1], {onerep[cosdenominator], zerorep[cosdenominator]}] && 
     ssdagalgebraic[[All, All, 2, 1]] == IdentityMatrix[numofirreps] && 
     ContainsOnly[
      Flatten[sdagsalgebraic, 1], {onerep[cosdenominator], zerorep[cosdenominator]}] && 
     sdagsalgebraic[[All, All, 2, 1]] == IdentityMatrix[numofirreps];
   
   modularalgebraicok = modularalgebraic == modular;

(* We checked modularity, so we now print if the s-matrix calculation went ok or not. *)

   If[smatalgebraicok,
    Print["The exact form of the S-matrix coincides with \
the algebraically obtained one :-)"];,
    Print["The exact form of the S-matrix does not coincide with \
the algebraically obtained one :-("];
    allok = False;,
    Print["The exact form of the S-matrix does not coincide with \
the algebraically obtained one :-("];
    allok = False;
    ];

   
  If[modularalgebraic,
  
   Print["Checking the modular relations algebraically..."];       

         
   s2algebraic = Table[
     reducesum[cosdenominator, cosnumerator,
     removepairs[cosdenominator,
      Table[
       exactproduct[cosdenominator, smatalgebraic[[i, k]], smatalgebraic[[k, j]]]
       , {k, 1, numofirreps}]
      ]
      ]
     , {i, 1, numofirreps}, {j, 1, numofirreps}];
     s2algebraic = Table[
     If[s2algebraic[[i, j, 2]] == zerorep[cosdenominator][[2]],
       zerorep[cosdenominator],
       s2algebraic[[i, j]]
     ]
    , {i, 1, numofirreps}, {j, 1, numofirreps}];
   
   s2algebraicok = 
    ContainsOnly[
      Flatten[s2algebraic, 1], {onerep[cosdenominator], zerorep[cosdenominator]}] && 
     s2algebraic[[All, All, 2, 1]] == cmat;
   
   
   altcosdenom = 
    LCM[Sequence @@ (Join[
          smatalgebraic[[All, All, 1]] // Flatten // Denominator // Union,
          thetalistalgebraic[[All, 1]] // Denominator, {cosdenominator}] // Union)];
   
   coefslist = Flatten[smatalgebraic[[All, All, 2]], 1] // Union;
   
   coefsreplace = {};
   Do[
    newrep = 
     findexactabs[
      toexactnumericalvalue[{0, coefslist[[i]]}, cosdenominator, cosnumerator, 100]^2, altcosdenom, 1];
    If[
     toexactvalue[{0, newrep}, altcosdenom, 1]^2 -
     toexactvalue[{0, coefslist[[i]]}, cosdenominator,  cosnumerator]^2 == 0 // FullSimplify,
     AppendTo[coefsreplace, coefslist[[i]] -> newrep];,
     Print["The new representation is not equal to the original one!"];,
     Print["It could not be checked if the new representation is equal to the original one!"];
     ];
    , {i, 1, Length[coefslist]}];
     
   
   altsmatalgebraic = Table[
     {smatalgebraic[[i, j, 1]], 
      smatalgebraic[[i, j, 2]] /. coefsreplace}
     , {i, 1, numofirreps}, {j, 1, numofirreps}];
   Clear[coefsreplace];
   
   altstalgebraic = Table[
    If[
     altsmatalgebraic[[i, j, 2]] == zerorep[altcosdenom][[2]],
     altsmatalgebraic[[i, j]],
      {Mod[altsmatalgebraic[[i, j, 1]] + thetalistalgebraic[[j, 1]], 2, -1],
      altsmatalgebraic[[i, j, 2]]}
    ]
   , {i, 1, numofirreps}, {j, 1, numofirreps}];
   
   altst3algebraic = Table[
     removepairs[altcosdenom,
      DeleteCases[
       Flatten[
       Table[
        exactproduct[altcosdenom, 
        altstalgebraic[[i, k1]], altstalgebraic[[k1, k2]], altstalgebraic[[k2, j]]]
       , {k1, 1, numofirreps}, {k2, 1, numofirreps}]
      , 1]
     , x_ /; x[[2]] == zerorep[altcosdenom][[2]]]
    ]
   , {i, 1, numofirreps}, {j, 1, numofirreps}];
   
   
   altst3calgebraic = Table[
    combinesum[
     removepairs[altcosdenom,
      Table[
       If[
        altst3algebraic[[i, j, k, 2]] == zerorep[altcosdenom][[2]],
        altst3algebraic[[i, j, k]],
         {Mod[altst3algebraic[[i, j, k, 1]] - centralchargealgebraic/4, 2, -1]
         , altst3algebraic[[i, j, k, 2]]}
       ]
      , {k, 1, Length[altst3algebraic[[i, j]]]}]
     ]
    ]
   , {i, 1, numofirreps}, {j, 1, numofirreps}];
   
   altst3cmincalgebraic = Table[
    If[
     cmat[[i, j]] == 0,
      altst3calgebraic[[i, j]],
      Append[altst3calgebraic[[i, j]], minusonerep[altcosdenom]]
    ]
   , {i, 1, numofirreps}, {j, 1, numofirreps}];
   
   
   altst3cmincreducealgebraic = Table[
    reducesum[altcosdenom, 1,
     combinesum[
      removepairs[altcosdenom,
       combinesumsymmetricphases[altcosdenom, 1,
        combinesum[
         removepairs[altcosdenom,
          abs2[altcosdenom, altst3cmincalgebraic[[i, j]]]
         ]
        ]
       ]
      ]
     ]
    ]
   , {i, 1, numofirreps}, {j, 1, numofirreps}];
   
   st3calgebraicok = altst3cmincreducealgebraic == 
    Table[zerorep[altcosdenom],{i,1,numofirreps},{j,1,numofirreps}];
   
   
  If[st3calgebraicok && s2algebraicok,
  
  Print["It was checked algebraically that the modular relations are satisfied :-)"];,
  Print["At least one of the modular relations does not hold algebraically :-("];,
  Print["At least one of the modular relations does not hold algebraically :-("];  
  
  ];
   
   ];

If[modularalgebraic &&  And @@ {qdimalgebraicok, qdtotalgebraicok, qdtot2algebraicok, 
    qdim1overfalgebraicok, pivotalgebraicok, thetalistalgebraicok, 
    frobschurlistalgebraicok, smatalgebraicok, modular2algebraicok, 
    modularalgebraicok, s2algebraicok, st3calgebraicok},
    Print["Done, all relations hold :-)"];
];
If[modularalgebraic &&  Not[And @@ {qdimalgebraicok, qdtotalgebraicok, qdtot2algebraicok, 
    qdim1overfalgebraicok, pivotalgebraicok, thetalistalgebraicok, 
    frobschurlistalgebraicok, smatalgebraicok, modular2algebraicok, 
    modularalgebraicok, s2algebraicok, st3calgebraicok}],
    Print["Done, not all relations hold :-("];
];
If[Not[modularalgebraic] &&  And @@ {qdimalgebraicok, qdtotalgebraicok, qdtot2algebraicok, 
    qdim1overfalgebraicok, pivotalgebraicok, thetalistalgebraicok, 
    frobschurlistalgebraicok, smatalgebraicok, modular2algebraicok, 
    modularalgebraicok},
    Print["Done, all relations hold :-)"];
];
If[Not[modularalgebraic] &&  Not[And @@ {qdimalgebraicok, qdtotalgebraicok, qdtot2algebraicok, 
    qdim1overfalgebraicok, pivotalgebraicok, thetalistalgebraicok, 
    frobschurlistalgebraicok, smatalgebraicok, modular2algebraicok, 
    modularalgebraicok}],
    Print["Done, not all relations hold :-("];
];


]; (* end of the actual calculation! *)
         
   ];


(* ::Subsection:: *)
(*Routines for saving and loading the data, in its exact form*)


filename[type_, rank_, cosdenom_, cosnum_] :=
  "alatc_type=" <> ToString[type] <> "_rank=" <> 
   StringPadLeft[ToString[rank], 3, "0"] <> "_denom=" <> 
   StringPadLeft[ToString[cosdenom], 3, "0"] <> "_num=" <> 
   StringPadLeft[ToString[cosnum], 3, "0"] <> ".wdx";
(* no check is done to see if the arguments are consistent! *)

fullfilename[type_, rank_, cosdenom_, cosnum_] :=
  FileNameJoin[{$UserBaseDirectory, "ApplicationData", "alatc", filename[type, rank, cosdenom, cosnum]}];
(* no check is done to see if the arguments are consistent! *)


savecurrentdata[] := Module[{dir, dirok, name, file, fileexists, data, files, filesizes, totalfilesize},
   
   If[Not[fsymbolsexactfound && rsymbolsexactfound && modulardataexactfound],
     Print["The exact form F-symbols, R-symbols and/or modular data was not obtained. Please do so first, \
before saving the data!"];   
   ];
   
   If[fsymbolsexactfound && rsymbolsexactfound && modulardataexactfound &&
      Not[pentholdsexact && hexholdsexact && modulardataexactok],
     Print["The exact form F-symbols, R-symbols and/or modular data was obtained, but at least one relation \
does not hold. This should be sorted out, before one can save the data!"];   
   ];
   
   If[fsymbolsexactfound && rsymbolsexactfound && modulardataexactfound &&
      pentholdsexact && hexholdsexact && modulardataexactok,
   
   (* all is ok, we can save the date, if it's not there yet! *)
   
   dir = FileNameJoin[{$UserBaseDirectory, "ApplicationData", "alatc"}];
   
   dirok = 
    FileExistsQ[dir];
   
   name = filename[type, rank, cosdenominator, cosnumerator];
   file = FileNameJoin[{dir, name}];
   
   If[Not[dirok],
    CreateDirectory[dir];
    Print["The directory ", dir, " was created."];
    ];
   
   fileexists = FileExistsQ[file];
   
   If[fileexists,
    Print["The file with the data for the current algebra, rank and root \
of unity already exists!"];
    Print["No data was saved."];
    Print["Done :-)"];
    ];
   
   If[Not[fileexists],
    Print["Saving the current data..."];
    
    data = {
      (* 1 the irreps *)
      irreps,
      (* 2 selfduallist *)
      selfdualvec,
      (* 3 simplecurrentlist *)
      simplecurrentvec,
      (* 4 the fusion matrices *)
      Table[nmat[ir], {ir, irreps}],
      (* 5 the list of F-symbols *)
      Table[fsymexact[Sequence @@ fs], {fs, flist}],
      (* 6 a list of booleans, containing info about the F-symbols matrices *)
      {fsymsreal, fmatunitary, fsymsFTFone, fsymbolsallrealorimaginary},
      (* 7 the list of R-symbols *)
      Table[rsymexact[Sequence @@ rs], {rs, rlist}],
      (* 8 the list of inverse R-symbols *)
      Table[rsyminvexact[Sequence @@ rs], {rs, rlist}],
      (* 9 a list of booleans, containing info about the R-symbols matrices *)
      {rmatunitary, rmatdiagonal, rmatevalsarephases},
      (* 10 pivotlist *)
      pivotlistexact,
      (* 11 FSlist *)
      frobschurlistexact,
      (* 12 FPdimlist *)
      FPdimlistexact,
      (* 13 qdimlist *)
      qdimlistexact,
      (* 14 qdtot2 *)
      qdtot2exact,
      (* 15 qdtot *)
      qdtotexact,
      (* 16 qdim1overf *)
      qdim1overfexactlist,
      (* 17 thetalist *)
      thetalistexact,
      (* 18 hlist *)
      hlist,
      (* 19 smat *)
      smatexact,
      (* 20 tmat *)
      tmatexact,
      (* 21 cmat *)
      cmatexact,
      (* 22 pplus *)
      pplusexact,
      (* 23 pminus *)
      pminusexact,
      (* 24 central charge *)
      centralcharge,
      (* 25 a list of boolians *)
      {qdimspositive, modular, modular2, modularrelationsok},
      (* 26 roots, the root system of the algebra, convenient to have when checking the modular 
         data algebraically. The other lie algebra data can easliy be set from the type and rank *)
      roots
      };
    
    Export[file, data];
    
    files = FileNames[All, dir];
    files = Cases[files, x_ /; StringPart[x,-4;;-1] == {".", "w", "d", "x"} ];
    filesizes = FileSize /@ files;
    totalfilesize = Plus @@ filesizes;
    
    
    Print[
     "The data for the current algebra, rank and root of unity was saved in the file:"];
    Print[file];    
    Print["The size of the data directory is ", totalfilesize];
    Print["Done :-)"];
    
    ];
  
  ]; 
 
 ];


initfusionfromnmatrices[nmatrices_, irreps_] := Module[{irreptopos},
   
   Clear[fusion, nv];
   
   Do[
    irreptopos[irreps[[i]]] = i;
    , {i, 1, numofirreps}];
   
   Do[
    fusion[ir1, ir2] = {}
    , {ir1, irreps}, {ir2, irreps}];
   
   Do[
    If[nmatrices[[irreptopos[ir1], irreptopos[ir2], irreptopos[ir3]]] > 0,
       nv[ir1, ir2, ir3] = 
        nmatrices[[irreptopos[ir1], irreptopos[ir2], irreptopos[ir3]]];
      ];
    , {ir1, irreps}, {ir2, irreps}, {ir3, irreps}];
   
   Do[
    If[Head[nv[ir1, ir2, ir3]] == Integer,
      AppendTo[fusion[ir1, ir2], ir3];
      ];
    , {ir1, irreps}, {ir2, irreps}, {ir3, irreps}];
   
   irrepsdual = Table[
      irreps[[Position[nmat[irreps[[i]]], x_ /; x[[1]] == 1][[1, 1]]]]
      , {i, 1, numofirreps}] // Quiet;
   
   Do[dual[irreps[[i]]] = irrepsdual[[i]], {i, 1, numofirreps}];
   
   flist = Flatten[
     Table[{a, b, c, d, e, f, {v1, v2, v3, v4}}
      , {a, irreps}
      , {b, irreps}
      , {c, irreps}
      , {e, fusion[a, b]}
      , {d, fusion[e, c]}
      , {f, Cases[fusion[b, c], x_ /; MemberQ[fusion[a, x], d]]}
      , {v1, nv[a, b, e]}
      , {v2, nv[e, c, d]}
      , {v3, nv[b, c, f]}
      , {v4, nv[a, f, d]}]
     , 9];
   
   fmatlist = flist[[All, 1 ;; 4]] // Union;
   
   numofpentagonequations = Sum[
       nv[a, b, f]*nv[f, c, g]*nv[g, d, e]* nv[c, d, gp]*nv[b, gp, fp]*nv[a, fp, e]
       , {a, irreps}
       , {b, irreps}
       , {c, irreps}
       , {d, irreps}
       , {f, fusion[a, b]}
       , {g, fusion[f, c]}
       , {e, fusion[g, d]}
       , {gp, Cases[fusion[c, d], x_ /; MemberQ[fusion[f, x], e]]}
       , {fp, Cases[fusion[b, gp], x_ /; MemberQ[fusion[a, x], e]]}
       ];
   
   rlist = Flatten[
     Table[{a, b, c, {v1, v2}}
      , {a, irreps}
      , {b, irreps}
      , {c, fusion[a, b]}
      , {v1, nv[a, b, c]}
      , {v2, nv[a, b, c]}]
     , 4];
   
   rmatlist = rlist[[All, 1 ;; 3]] // Union;
   
   nvlist = Table[nv[Sequence @@ rmatlist[[i]]], {i, 1, Length[rmatlist]}];
   maxmultiplicity = Max[nvlist];
   multiplicity = Not[(nvlist // Union) == {1}];
   numoffusmultiplicities = Count[nvlist, x_ /; x > 1];
   
   Global`flist = flist;
   Global`fmatlist = fmatlist;
   Global`rlist = rlist;
   Global`rmatlist = rmatlist;
   Global`maxmultiplicity = maxmultiplicity;
   Global`multiplicity = multiplicity;
   Global`numberoffusionmultiplicities = numoffusmultiplicities;
   
   ];


importdata[file_] := Module[{data},
  
  data = Import[file];
  
    (* Take care of the fusion rules *)
    
    irreps = data[[1]];
    numofirreps = Length[irreps];
    Global`irreps = irreps;
    
    selfdualvec = data[[2]];
    Do[selfdual[irreps[[i]]] = selfdualvec[[i]], {i, 1, numofirreps}];
    numofselfduals = Count[selfdualvec, True];
    
    simplecurrentvec = data[[3]];
    numofsimplecurrents = Count[simplecurrentvec, True];
    
    Do[
       nmat[irreps[[i]]] = data[[4, i]];
       , {i, 1, numofirreps}];
    
    initfusionfromnmatrices[data[[4]], irreps];
    
    (* Take care of the F-symbols *)
    
    Do[
       fsymexact[Sequence @@ flist[[i]]] =  data[[5, i]];
       fsym[Sequence @@ flist[[i]]] = 
         toexactnumericalvalue[fsymexact[Sequence @@ flist[[i]]], cosdenominator, cosnumerator, 200];
       , {i, 1, Length[flist]}];
    
    Do[fmat[Sequence @@ fm] =
      Flatten[
        Table[fsym[fm[[1]], fm[[2]], fm[[3]], fm[[4]], e, f, {v1, v2, v3, v4}]
        , {e, Cases[fusion[fm[[1]], fm[[2]]], x_ /; MemberQ[fusion[x, fm[[3]]], fm[[4]]]]}
        , {f, Cases[fusion[fm[[2]], fm[[3]]], x_ /; MemberQ[fusion[fm[[1]], x], fm[[4]]]]}
        , {v1, nv[fm[[1]], fm[[2]], e]}
        , {v2, nv[e, fm[[3]], fm[[4]]]}
        , {v3, nv[fm[[2]], fm[[3]], f]}
        , {v4, nv[fm[[1]], f, fm[[4]]]}]
           , {{1, 3, 4}, {2, 5, 6}}];
       
       fmatdim[Sequence @@ fm] = Length[fmat[Sequence @@ fm]];
       
       , {fm, fmatlist}];
    
    fmatdimtab = Table[fmatdim[Sequence @@ fm], {fm, fmatlist}];
    
    fsymbolarguements = data[[5, All, 1]] // Union;
    
    {fsymsreal, fmatunitary, fsymsFTFone, fsymbolsallrealorimaginary} = data[[6]];
    
    (* Take care of the R-symbols *)
    
    Do[
       rsymexact[Sequence @@ rlist[[i]]] =  data[[7, i]];
       rsyminvexact[Sequence @@ rlist[[i]]] =  data[[8, i]];
       rsym[Sequence @@ rlist[[i]]] = 
         toexactnumericalvalue[rsymexact[Sequence @@ rlist[[i]]], cosdenominator, cosnumerator, 200];
       rsyminv[Sequence @@ rlist[[i]]] = 
         toexactnumericalvalue[rsyminvexact[Sequence @@ rlist[[i]]], cosdenominator, cosnumerator, 200];
       
    , {i, 1, Length[rlist]}];
    
    
    Do[
      rmat[Sequence @@ rm] =
         Table[
           rsym[Sequence @@ rm[[1 ;; 3]], {v1, v2}]
           , {v1, nv[Sequence @@ rm[[1 ;; 3]]]}
           , {v2, nv[Sequence @@ rm[[1 ;; 3]]]}];
       rmatinv[Sequence @@ rm] =
         Table[
           rsyminv[Sequence @@ rm[[1 ;; 3]], {v1, v2}]
           , {v1, nv[Sequence @@ rm[[1 ;; 3]]]}
           , {v2, nv[Sequence @@ rm[[1 ;; 3]]]}];
       , {rm, rmatlist}];
    
    rmatevallist = Table[N[rmat[Sequence @@ rm], 200] // Eigenvalues, {rm, rmatlist}] //  Flatten;
    rmatevalabslist = Abs /@ rmatevallist;
    rmatevalarglist = Rationalize[1/Pi (Arg /@ rmatevallist)];
    rmatevalarglistunion = rmatevalarglist // Union;
    rmatevalargmaxdenom = (Denominator /@ rmatevalarglist) // Max;
    
    {rmatunitary, rmatdiagonal, rmatevalsarephases} = data[[9]];
    
    (* The modular data *)
    
    (* The pivotal structure *)
    pivotlistexact = data[[10]];
    
    pivotlist = Table[
         If[pivotlistexact[[i, 1]] == 0, 1, -1]
         , {i, 1, numofirreps}];
    
    (* FSlist *)
    
    frobschurlistexact = data[[11]];
    frobschurlist = Table[
         Which[
           frobschurlistexact[[i]] == onerep[cosdenominator], 1,
           frobschurlistexact[[i]] == phaserep[cosdenominator, 1], -1,
           frobschurlistexact[[i]] == zerorep[cosdenominator], 0
           ]
         , {i, 1, numofirreps}];
    
    (* 12 FPdimlist *)
    FPdimlistexact = data[[12]];
    fpdimvec = Table[
         toexactnumericalvalue[FPdimlistexact[[i]], cosdenominator, cosnumerator, 200]
         , {i, 1, numofirreps}];
    
    (* 13 qdimlist *)
    qdimlistexact = data[[13]];
    Do[qdimexact[irreps[[i]]] = qdimlistexact[[i]]
        , {i, 1, numofirreps}];
    qdimvec = Table[
         toexactnumericalvalue[qdimlistexact[[i]], cosdenominator, cosnumerator, 200]
         , {i, 1, numofirreps}];
    
    (* 14 qdtot2 *)
    qdtot2exact = data[[14]];
    qdtot2 = toexactnumericalvalue[qdtot2exact, cosdenominator, cosnumerator, 200];
    
    (* 15 qdtot *)
    qdtotexact = data[[15]];
    qdtot = toexactnumericalvalue[qdtotexact, cosdenominator,  cosnumerator, 200];
    qdtot1overexact = findoneover[cosdenominator, qdtotexact];
    
    (* 16 qdim1overf *)
    qdim1overfexactlist = data[[16]];
    qdim1overfvec = Table[
         toexactnumericalvalue[qdim1overfexactlist[[i]], cosdenominator, cosnumerator, 200]
         , {i, 1, numofirreps}];
    
    (* 17 thetalist *)
    thetalistexact = data[[17]];
    thetalist = Table[
         toexactnumericalvalue[thetalistexact[[i]], cosdenominator, cosnumerator, 200]
         , {i, 1, numofirreps}];
    Do[
       thetaexact[irreps[[i]]] = thetalistexact[[i]];
       , {i, 1, numofirreps}];
    
    (* 18 hlist *)
    hlist = data[[18]];
    
    (* 19 smat *)
    smatexact = data[[19]];
    smat =
     Table[
      Which[
       Head[smatexact[[i, j, 1]]] === List,
       Exp[smatexact[[i, j, 1, 1]] * ArcCos[
             toexactnumericalvalue[smatexact[[i, j, 1, 2]], smatexact[[i, j, 1, 3]], 1, 200]] * I] *
             toexactnumericalvalue[{0, smatexact[[i, j, 2]]}, cosdenominator, cosnumerator, 200],
       True,
       toexactnumericalvalue[smatexact[[i, j]], cosdenominator, cosnumerator, 200]]
     , {i, 1, numofirreps}, {j, 1, numofirreps}];
    
    (* 20 tmat *)
    tmatexact = data[[20]];
    tmat = DiagonalMatrix[
      Table[
       toexactnumericalvalue[tmatexact[[i, i]], cosdenominator, cosnumerator, 200]
      , {i, 1, numofirreps}]
         ];
    
    (* 21 cmat *)
    cmatexact = data[[21]];
    cmat = cmatexact[[All, All, 2, 1]];
    
    (* 22 pplus *)
    pplusexact = data[[22]];
    pplus = toexactnumericalvalue[pplusexact, cosdenominator, cosnumerator, 200];
    
    (* 23 pminus *)
    pminusexact = data[[23]];
    pminus = toexactnumericalvalue[pminusexact, cosdenominator, cosnumerator, 200];
    
    (* 24 central charge *)
    centralcharge = data[[24]];
    
    (* 25 a list of boolians *)
    {qdimspositive, modular, modular2, modularrelationsok} = data[[25]];
    
    (* 26 roots, take care of the necessary lie-algebra data *)
    
    roots = data[[26]];
    numposroots = Position[roots, Table[0, {i, 1, rank}], 1][[1, 1]] - 1;
    posroots = roots[[1 ;; numposroots]];
    qfm = qfmatrix[type, rank];
    rho = Table[1, {j, 1, rank}];
    
    Global`FPdimlist = fpdimvec;
    Global`qdimlist = qdimvec;
    Global`selfduallist = selfdualvec;
    Global`simplecurrentlist = simplecurrentvec;
    Global`pivotlist = pivotlist;
    Global`thetalist = thetalist;
    Global`hlist = hlist;
    Global`FSlist = frobschurlist;
    Global`smat = smat;
    Global`tmat = tmat;
    Global`cmat = cmat;
    Global`centralcharge = centralcharge;
    Global`modular = modular;
    Global`unitary = qdimspositive;
    
    Global`FPdimlistexact = FPdimlistexact;
    Global`qdimlistexact = qdimlistexact;
    Global`pivotlistexact = pivotlistexact;
    Global`thetalistexact = thetalistexact;
    Global`FSlistexact = frobschurlistexact;
    Global`smatexact = smatexact;
    Global`tmatexact = tmatexact;
    Global`cmatexact = cmatexact;

];


loaddata[atype_, rr_, lev_, rootfac_] := Module[
   {data, typerankok, levelok, rootfacok, canbenonuniformtemp,
    posrootfacsuniform, posrootfacsnonuniform, posrootfacsall,
    gtemp, tmaxtemp, rootofunitytemp, uniformtemp, cosdenominatortemp,
     cosnumeratortemp,
    dir, name, file, fileexists},
   
   typerankok = rangeok[atype, rr];
   levelok = IntegerQ[lev] && lev >= 0;
   rootfacok = False;
   
   If[typerankok && levelok,
    canbenonuniformtemp = nonuniformpossible[atype, rr, lev];
    posrootfacsuniform = possiblerootfactorsuniform[atype, rr, lev];
    If[canbenonuniformtemp,
     posrootfacsnonuniform = 
       possiblerootfactorsnonuniform[atype, rr, lev];,
     posrootfacsnonuniform = {};
     ];
    posrootfacsall = Union[posrootfacsuniform, posrootfacsnonuniform];
    
    If[MemberQ[posrootfacsall, rootfac],
     rootfacok = True;
     ];
    
    ];
   
   If[Not[typerankok && levelok && rootfacok],
    Print["The type of algebra, rank, level and/or rootfactor are not compatible, \
no data was loaded, no initialization was done!"];,
    Null,
    Print["The type of algebra, rank, level and/or rootfactor are not compatible, \
no data was loaded, no initialization was done!"];
    ];
   
   If[typerankok && levelok && rootfacok,
    (* we check if the correct file exists, so we need cosdenom and cosnum first *)
    
    gtemp = dualcoxeter[atype, rr];
    tmaxtemp = tmaxvalue[atype];
    rootofunitytemp = 1/(gtemp + lev);
    uniformtemp = MemberQ[posrootfacsuniform, rootfac];
    If[uniformtemp,
     cosdenominatortemp = 1/rootofunitytemp*tmaxtemp;
     cosnumeratortemp = rootfac;,
     cosdenominatortemp = 1/rootofunitytemp;
     cosnumeratortemp = rootfac/tmaxtemp;
     ];
    dir = FileNameJoin[{$UserBaseDirectory, "ApplicationData", "alatc"}];
    name = filename[atype, rr, cosdenominatortemp, cosnumeratortemp];
    file = FileNameJoin[{dir, name}];
    fileexists = FileExistsQ[file];
    If[Not[fileexists],
     Print["The data file for the specified type, rank, level and rootfactor \
does not exist, no data was loaded, no initialization was done!" ];
     ];
    ];
   
   If[typerankok && levelok && rootfacok && fileexists,
    
    (* all good, the file exits, 
    we can do the initialization and load the data *)
    
    Print["The data file for the specified type, rank, level and rootfactor \
exists, the data will be loaded and the system will be initialized..."];
    
    clearvariables[];
    clearglobalvariables[];
    typerankinitok = True;
    typeranklevelinitok = True;
    typeranklevelrootinitok = True;
    fsymbolscalculated = True;
    rsymbolscalculated = True;
    fsymbolsexactfound = True;
    rsymbolsexactfound = True;
    pentholds = True;
    pentundecidable = False;
    pentholdsexact = True;
    hexholds = True;
    hexrundecidable = False;
    hexrinvundecidable = False;
    hexholdsexact = True;
    modulardatacalculated = True;
    modulardataexactfound = True;
    pentagontobechecked = True;
    recheck = False;
    rmatricesdiagonalized = False;
    
    type = atype;
    rank = rr;
    level = lev;
    rootfactor = rootfac;
    g = gtemp;
    tmax = tmaxtemp;
    rootofunity = rootofunitytemp;
    canbenonuniform = canbenonuniformtemp;
    rootfactorsuniform = posrootfacsuniform;
    If[canbenonuniform,
     rootfactorsnonuniform = possiblerootfactorsnonuniform[type, rank, level];,
     rootfactorsnonuniform = {};
     ];
    rootfactors = Union[rootfactorsuniform, rootfactorsnonuniform];
    q = N[Exp[2 Pi I rootfactor rootofunity/tmax], 200];
    uniform = uniformtemp;
    cosdenominator = cosdenominatortemp;
    cosnumerator = cosnumeratortemp;
    
    importdata[file];
    
    Print["Done :-)"];
    
    ];
   
   ];


loaddatalz[atype_, rr_, lvalue_, zvalue_] := Module[
   {typerankok, currentlvalueok, currentzvalueok,
    dir, name, file, fileexists},
   
   typerankok = rangeok[atype, rr];
   If[typerankok,
    currentlvalueok = lvalueok[atype, rr, lvalue];
    ];
   If[typerankok && currentlvalueok,
    currentzvalueok = zvalueok[lvalue, zvalue];
    ];
   If[Not[typerankok && currentlvalueok && currentzvalueok],
    Print["The type of algebra, rank, the value of l and/or the value of \
z are not compatible, no data was loaded, no initialization was done!"];,
    Null,
    Print["The type of algebra, rank, the value of l and/or the value of \
z are not compatible, no data was loaded, no initialization was done!"];
    ];
   
   
   If[
    typerankok && currentlvalueok && currentzvalueok,
    (* let's check if the file exists *)
    
    dir = FileNameJoin[{$UserBaseDirectory, "ApplicationData", "alatc"}];
    name = filename[atype, rr, lvalue, zvalue];
    file = FileNameJoin[{dir, name}];
    fileexists = FileExistsQ[file];
    If[Not[fileexists],
     Print["The data file for the specified type, rank, the value of l \
and the value of z does not exist, no data was loaded, no \
initialization was done!" ];
     ];
    ];
   
   If[typerankok && currentlvalueok && currentzvalueok && fileexists,
    
    (* all good, the file exits, we can do the initialization and load the data *)
    
    Print["The data file for the specified type, rank, the value of l and \
the value of z exists, the data will be loaded and the system will \
be initialized..."];

    clearvariables[];
    clearglobalvariables[];
    typerankinitok = True;
    typeranklevelinitok = True;
    typeranklevelrootinitok = True;
    fsymbolscalculated = True;
    rsymbolscalculated = True;
    fsymbolsexactfound = True;
    rsymbolsexactfound = True;
    pentholdsexact = True;
    pentholds = True;
    pentundecidable = False;
    hexholds = True;
    hexrundecidable = False;
    hexrinvundecidable = False;        
    hexholdsexact = True;
    modulardatacalculated = True;
    modulardataexactfound = True;
    pentagontobechecked = True;
    recheck = False;
    rmatricesdiagonalized = False;
    
    type = atype;
    rank = rr;
    g = dualcoxeter[type, rank];
    tmax = tmaxvalue[type];
    uniform = lzvaluesuniform[type, rank, lvalue, zvalue];
    level = If[uniform, lvalue/tmax - g, lvalue - g];
    rootfactor = If[uniform, zvalue, tmax*zvalue];
    rootofunity = 1/(g + level);
    canbenonuniform = nonuniformpossible[type, rank, level];
    rootfactorsuniform = possiblerootfactorsuniform[type, rank, level];
    If[canbenonuniform,
     rootfactorsnonuniform = possiblerootfactorsnonuniform[type, rank, level];,
     rootfactorsnonuniform = {};
     ];
    rootfactors = Union[rootfactorsuniform, rootfactorsnonuniform];
    q = N[Exp[2 Pi I rootfactor rootofunity/tmax], 200];
    cosdenominator = lvalue;
    cosnumerator = zvalue;
    
    importdata[file];
    
    Print["Done :-)"];
    
    ];
   
   ];


(* ::Subsection::Closed:: *)
(*Cleanup*)


clearvariables[] := Clear[
   weights, wdim,
   raising, lowering, irreps,
   rootfactor,
   generalinnerproduct, innerproduct,
   statespossible, statesneeded,
   tpstatesraising, basisraising, basislowering, stateraising,
   statelowering, tpstatevec, tpbasis, basis, basison, gramm,
   tpstates, fusion, nv, nmat, dualirreps, dualirrep, flist, fmatlist, 
   pentlist, rlist, rmatlist, nvlist, maxmultiplicity, multiplicity,
   numoffusmultiplicities, qcg, fsym, fmat, fmatdim, fmatdimtab, rsym,
   rsyminv, rmat, rmatinv, sign, phase, fsymold, rsymold, fmatold,
   rmatold, umat, umatinv, numofirreps, numposroots, posroots, qdimvec,
   qdtot2, qdtot, qdimspositive, fpdimvec, irrepsdual, dual,
   selfdualvec, selfdual, qdim1overfvec, pivotlist, pivoteqnok, thetalist,
   hlist, frobschurlist, smat, cmat, tmat,
   modular, pplus, pminus, modular2, centralcharge, modularrelationsok,
   fsymbolsallrealorimaginary, fsymbolarguements, weightspaceorthogonal,
   weightspacedeviation, orthonormalityok, qCGdeviation,
   pentagondeviation, pentholds, pentagoncounter, hexrundecidable,
   hexagonRdeviation, hexrinvundecidable, hexagonRinversedeviation,
   hexagondeviation, hexholds, simplecurrentvec,
   dual, selfdual,
   uniform, canbenonuniform, rootfactorsuniform, rootfactorsnonuniform,
   lval, zval,
   fsymexact, rsymexact, rsyminvexact,
   pentholdsexact, hexholdsexact,
   cosdenominator, cosnumerator,
   pentholdsalgebraically, hexholdsalgebraically,
   pivotlistexact, pivotexactok,
   FPdimlistexact, FPdimexactok,
   frobschurlistexact, frobschurexactok, 
   qdimlistexact, qdimexact, qdimspositiveexact, qdimexactok,
   qdtot2exact, qdtot2exactok,
   qdtotexact, qdtotexactok, qdtot1overexactok, qdim1overfexactlistok,
   thetalistexact, thetaexact, thetalistexactok,
   smatexact, smatexactok,
   tmatexact, tmatexactok,
   cmatexact,
   pplusexact, pminusexact, pplusexactok, pminusexactok,
   modularexact, modularrelationsexactok,
   modulardataexactok,
   numofsimplecurrents, numofselfduals
   ];
   
clearglobalvariables[] := Clear[
  Global`irreps, Global`flist, Global`fmatlist, Global`rlist, 
  Global`rmatlist, Global`maxmultiplicity,  Global`multiplicity,
  Global`numberoffusionmultiplicities, Global`FPdimlist,
  Global`qdimlist, Global`pivotlist, Global`thetalist, Global`hlist,
  Global`FSlist, Global`smat, Global`tmat, Global`cmat,
  Global`centralcharge, Global`modular, Global`unitary,
  Global`selfduallist, Global`simplecurrentlist
 ];


(* ::Section::Closed:: *)
(*End `Private` Context*)


End[];


(* ::Section::Closed:: *)
(*End Package*)


EndPackage[];
