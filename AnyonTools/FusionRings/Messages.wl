(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(*
+---------------------------------------------------------------------------+
|                                                                           |
|                               FUSION RINGS                                |
|                                                                           |
+---------------------------------------------------------------------------+

+---------------------------------------------------------------------------+
|                                 GENERAL                                   |
+---------------------------------------------------------------------------+

+---------------------------------------------------------------------------+
|                              Usage Messages                               |
+---------------------------------------------------------------------------+
*)

(*
  TODO: finish user messages
  TODO: Give proper headers for sections of usage messages
*)

FusionRings::usage =
"This package contains a List of fusion rings and several useful tools for working with fusion rings. It also adds formatting tools and the possibility to perform calculations with elements of fusion rings without the need for cumbersome notation.";
FusionRing::usage =
"FusionRing[ \"MultiplicationTable\" -> multTab ] initializes a fusion ring based on the given multiplication table multTab. Extra options include\n(i) \"ElementsName\" -> s which gives all elements an indexed name with String or Symbol s, \n(ii) \"ElementNames\" -> elNames where elNames is a list of Strings, Integers or Symbols representing the names of the different elements (giving this option overwrites the \"ElementsName\" option, \n(iii) \"Names\" -> names where names is a list of String's representing the possible names of the fusion ring.";
FusionRingZn::usage =
"FusionRingZn[n] returns the group fusion ring \!\(\*SubscriptBox[\(\[DoubleStruckCapitalZ]\), \(n\)]\)";
FusionRingSU2k::usage =
"FusionRingSU2k[k] returns the fusion ring \!\(\*
StyleBox[\"SU\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"(\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"2\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[SubscriptBox[\")\", \"k\"],\nFontWeight->\"Bold\"]\)";
FusionRingPSU2k::usage =
"FusionRingPSU2k[k] returns the fusion ring \!\(\*
StyleBox[\"PSU\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"(\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"2\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[SubscriptBox[\")\", \"k\"],\nFontWeight->\"Bold\"]\)";
FusionRingFromGroup::usage =
"FusionRingFromGroup[g], where g is one of the standard built-in groups, returns a fusion ring whose multiplication matches that of Group. Its name will be set to that of the group if the group is a PermutationGroup, SymmetricGroup, AlternatingGroup, CyclicGroup, DihedralGroup or AbelianGroup, or will be the value given by the option \"Names\".
FusionRingFromGroup[multtable], where multtable is the cayley table of a group, returns a fusion ring whose multiplication matches that of the multiplication table from the group.";
FusionRingHI::usage =
"FusionRingHI[g] returns the Haagerup-Izumi fusion ring associated to the built-in abelian group g. FusionRingHI[multtable] returns the Haagerup-Izumi fusion ring associated to the group with multiplication table multtable.";
FusionRingTY::usage =
"FusionRingTY[g] returns the Tambara-Yamagami fusion ring associated to the built-in abelian group g. FusionRingTY[multtable] returns the Tambara-Yamagami fusion ring associated to the group with multiplication table multtable.";
FusionRingCharacters::usage =
"FusionRingCharacters[r], with r a commutative ring, returns a symbolic character table of r or Missing[] if no symbolic form was found.";
FRC::usage =
"Shorthand for FusionRingCharacters";
NFusionRingCharacters::usage =
"NFusionRingCharacters[r], with r a commutative ring, returns a machine precision numeric character table of r.";
NFRC::usage =
"Shorthand for NFusionRingCharacters.";
SMatrices::usage =
"SMatrices[ring] returns a list of S matrices of the ring, if it has one, and an empty list otherwise.";
SM::usage =
"Shorthand for SMatrices.";
TwistFactors::usage =
"TwistFactors[ring] returns a vector of rational numbers denoting the factors q in the expressions Exp[ 2 \[Pi] \[ImaginaryI] q ] that appear in the T-matrix of the ring."
TF::usage =
"Shorthand for TwistFactors.";
ModularData::usage =
"ModularData[ring] returns a list of associations <| \"SMatrix\" -> Si, \"TwistFactors\" -> Qi |>, where the Si are the S matrices of the ring and the Qi lists of twist factors for for which the corresponding T-matrix obeys (ST\!\(\*SuperscriptBox[\()\), \(3\)]\) == \!\(\*SuperscriptBox[\(\[Lambda]S\), \(2\)]\) with \[Lambda] a non-zero complex number. If there are no compatible T-matrices for any S-matrix an empty list is returned.";
MD::usage =
"Shorthand for ModularData.";
(*
+---------------------------------------------------------------------------+
|                              Error Messages                               |
+---------------------------------------------------------------------------+
*)

FusionRing::nomulttable =
"No multiplication data is provided so no ring could be initialized.";
FusionRing::wrongdim =
"Wrong dimensions: `1`. The dimensions of the multiplication table should be {m,m,m}, with m the amount of particles.";
FusionRing::notassociative =
"Multiplication table does not correspond to associative multiplication.";
FusionRing::nounit =
"The first element in the multiplication table is not a unit element.";
FusionRing::noinverse =
"The multiplication table contains particles with no antiparticle.";
FusionRing::multipleinverse =
"The multiplication table contains particles with multiple antiparticles.";
FusionRing::badcoeff =
"The multiplication table contains arguments that are not positive integers.";
FusionRing::elnameslength =
"Length of list of names `1` is not equal to amount of generators `2` of the fusion ring.";
FusionRing::elnamesdifferentheads =
"The elements of list `1` should have equal Head's.";
FusionRing::elnameswrongheads =
"The elements of list `1` should have Head Integer, String, or Symbol.";
FusionRing::badargnames =
"OptionValue `1` of \"Names\" should be a String or a list of Strings.";
FusionRingTY::notgrouptable =
"The multiplication table `1` must be a group multiplication table.";
FusionRingHI::nonsymmulttab =
"The multiplication table `1` must be symmetric.";
FusionRingHI::notgrouptable =
"The multiplication table `1` must be a group multiplication table.";


(* ::Subsubsection::Closed:: *)
(*Properties of fusion rings*)


FusionRingQ::usage =
"FusionRingQ[ring] returns True if ring is a FusionRing and False otherwise.";
FusionElement::usage =
"FusionElement[ring, i] represents a symbolic form of the i'th generator of the fusion ring r. If no \"ElementsName\" option is set, FusionElement[ring, i] will be formated as \!\(\*SubscriptBox[\(\[Psi]\), \(i\)]\). If the \"ElementsName\" option is set to \"string\" then the FusionElement[ring,i] will be formatted as \!\(\*SubscriptBox[\(string\), \(i\)]\). If the option \"ElementNames\" is set then FusionElement[ring,i] will be formatted by the i'th entry of ring[\"ElementNames\"]";
FusionProduct::usage =
"FusionProduct[ring, {i,j}] returns a sum of FusionElements that equals the fusion product of FusionElements i and j.";
MultiplicationTable::usage =
"MultiplicationTable[ring] returns a triple nested list l where l[[a,b,c]] is the structure constant N_{a,b}^c. Upon initializing a fusion ring it is necessary to provide the option \"MultiplicationTable\"->table where the table must represent the multiplication table of a unital, associative fusion ring with unique inverses.";
MT::usage =
"MT is shorthand for MultiplicationTable.";
SymbolicMultiplicationTable::usage =
"SymbolicMultiplicationTable[ring] returns a table l where l[[a,b]] equals fusion.";
SMT::usage =
"SMT is shorthand for SymbolicMultiplicationTable.";
Names::usage =
"Names[ring] returns a list of possible names of the fusion ring. The possible names of a fusion ring can set upon initialization by adding the option \"Names\"->{...} or afterwards by issuing ring[\"Names\"] = {...}";
ElementsName::usage =
"ElementsName[ring] returns a string that is used as an indexed variable for naming the generators of the fusion ring. Indexed names are only used for formatting the elements during calculations. To access the element of an indexed name, use ring[[index]] instead. This name can be given as an option \"ElementsName\"->\"...\" to FusionRing or added later via ring[\"ElementsName\"] = \"...\".";
ElementNames::usage =
"ElementNames[ring] returns a list of strings denoting the names of the current generators of the fusion ring. These will be used to label the elements of the ring during calculations and can also be used to access the elements via ring[[el]], where el can either be the name as a string or as a symbol. ElementNames can be given as an option \"ElementNames\" -> stringlist to FusionRing upon initialization or can be added later via ring[\"ElementNames\"] = stringlist. The format required is a list of strings with the same length as the amount of generators.";
GroupQ::usage =
"GroupQ[ring] returns True if the multiplication table comes from a finite group.";
GQ::usage =
"Shorthand for GroupQ.";
Multiplicity::usage =
"Multiplicity[ring] returns and integer that denotes the highest structure constant.";
Mult::usage =
"MC is shorthand for Multiplicity.";
NNonZeroStructureConstants::usage =
"NNonZeroStructureConstants[ring] returns an Integer that denotes the amount of nonzero structure constants.";
NNZSC::usage =
"NNZC is shorthand for NNonZeroStructureConstants.";
NonZeroStructureConstants::usage =
"NonZeroStructureConstants[ring] returns a list of triples of indices for which the structure constants are non-zero";
NZSC::usage =
"NZC is shorthand for NonZeroStructureConstants.";
Rank::usage =
"Rank[ring] returns the amount of generators (particles) of the fusion ring.";
CommutativeQ::usage =
"CommutativeQ[ring] returns True if ring is commutative and False otherwise.";
CQ::usage =
"CQ is shorthand for CommutativeQ.";
MultiplicityFreeQ::usage =
"MultiplicityFreeQ[ring] returns True if ring has no structure constants bigger than 1.";
MFQ::usage =
"MFQ is shorthand for MultiplicityFreeQ.";
NSelfDual::usage =
"NSelfDual[ring] returns the amount of particles that are their own antiparticle. This includes the vacuum particle";
NSD::usage =
"NSD is shorthand for NSelfDual.";
NNonSelfDual::usage =
"NNonSelfDual[ring] returns the amount of particles that are not their own antiparticle";
NNSD::usage =
"NNSD is shorthand for NNonSelfDual.";
NSelfDualNonSelfDual::usage =
"NSelfDualNonSelfDual[ring] returns a tuple {sd,nsd} where sd is the amount of particles that are their own antiparticle (including the vacuum particle and nsd is the amount of particles that are not their own antiparticle";
NSDNSD::usage =
"NSDNSD is shorthand for NSelfDualNonSelfDual.";
AntiparticleMatrix::usage =
"AntiparticleMatrix[ring] returns a matrix that maps each particle to its antiparticle.";
AM::usage =
"AM is shorthand for AntiparticleMatrix.";
QuantumDimensions::usage =
"QuantumDimensions[Ring] returns the list of quantum dimensions of each generator in the same order as that of the generators";
QD::usage =
"QD is shorthand for QuantumDimensions.";
TotalQuantumDimensionSquared::usage =
"TotalQuantumDimensionSquared[Ring] returns the sum of the squares of the quantum dimensions of the generators of Ring";
TQDS::usage =
"TQDS is shorthand for TotalQuantumDimensionSquared.";
Barcode::usage =
"Barcode[ring] returns a number associated to the set of rings obtained by permuting the elements of ring.
It equals the maximum of the numbers obtained flattening each multiplication table and interpreting the lists of integers as digits of an integer in base Multiplicity[ring] + 1. It is therefore an invariant under permutation of the elements of a ring and for rings with equal multiplicity it uniquely determines the structure of a ring.";
FormalCode::usage =
"FormalCode[ring] returns a 4-tuple that uniquely classifies the ring. The first 3 numbers are the Rank, Multiplicity, and Number of non-seldual particles, while the 4th is the position in the list of rings with common first 3 numbers, sorted by amount of nonzero structure constants and Barcode.";
FC::usage =
"Shorthand for FormalCode";
ConjugateCharge::usage =
"ConjugateCharge[ring] returns a function that maps an integer index to the dual index.";
CC::usage =
"Shorthand for ConjugateCharge";
FusionRingAutomorphisms::usage =
"FusionRingAutomorphisms[ring] returns a list of permutation vectors that leaves the multiplication table of ring invariant.";
FRA::usage =
"Shorthand for FusionRingAuthomorphisms.";
OptimizedImport::usage =
"OptimizedImport[ fileName, importDirectory ] checks whether a file named filename, with extension .mx exists and if so imports it. If it doesn't exist it checks whether a file named filename with extension .wdx exists and if so, imports it and exports it as a .mx file for future use.";
AdjointFusionRing::usage =
"AdjointFusionRing[ ring ] returns a couple { el, fr } where fr is the adjoint fusion sub-ring of ring and el corresponds to the elements of ring that form this sub-ring (and are ordered such that the first element of el corresponds to the first element of fr, and so on).";
UpperCentralSeries::usage =
"UpperCentralSeries[ fusionRing ] returns a list of couples { c[1], ..., c[n] } where c[i] = { el[i], adj[i] } and subring [i] corresponds to the adjoint subring of adj[i-1] and el[i] to the elements of adj[i-1] that form this subring.";
NilpotentFusionRingQ::usage =
"NilpotentFusionRingQ[ fusionRing ] returns True if the fusion ring fusionRing is nilpotent and Fasle otherwise.";
AdjointIrreps::usage =
"AdjointIrreps[ fusionRing ] returns a partition of the elements of fusionRing, where each set is invariant under left-and right action of the adjoint subring. The first element always corresponds to the adjoint subring itself.";
UniversalGrading::usage =
"UniversalGrading[ ring ] returns a couple { grading, groupRing } where grading is a list of rules of elements from ring to groupring and groupring is the universal group that grades the elements of the fusion ring.";
LeftOrderedFusionTrees::usage =
"LeftOrderedFusionTrees[ ring, n ] returns lists of labels that form valid left-ordered fusion trees with n vertices of the Fusion Ring ring. The labels are ordered such that the first n + 1 labels denote the horizontal labels of the fusion tree (input anyons) and the last n labels denote the diagonal labels (fusion outcomes) from top to bottom.";
RightOrderedFusionTrees::usage =
"RightOrderedFusionTrees[ ring, n ] returns lists of labels that form valid right-ordered fusion trees with n vertices of the Fusion Ring ring. The labels are ordered such that the first n + 1 labels denote the horizontal labels of the fusion tree (input anyons) and the last n labels denote the diagonal labels (fusion outcomes) from top to bottom.";
(*ToWikiTable::usage =
	"ToWikiTable[table] returns a string that formats the table into the default format for a 'wikitable sortable' table on mediawiki.";*)
LeftTreeDiagram::usage =
"LeftTreeDiagram[labels] returns a graphics object of a left ordered fusion tree labeled by labels. Here the convention is that the horizontal labels (read from left to right) are the first l + 1 labels (where l is the number of vertices) and the diagonal labels (read from top to bottom) are the last l labels.";
RightTreeDiagram::usage =
"RightTreeDiagram[labels] returns a graphics object of a right ordered fusion tree labeled by labels. Here the convention is that the horizontal labels (read from left to right) are the first l + 1 labels (where l is the number of vertices) and the diagonal labels (read from top to bottom) are the last l labels.";
(* ::Subsubsection:: *)
(*Operations on fusion rings*)


PermutedRing::usage =
"PermutedRing[ring,permutationvec] returns a ring with multiplication table obtained after permuting the labels of ring according to the permutation induced by the permutation vector permutationvec.
PermutedRing[ring,cycles] returns a ring with multiplication table obtained after permuting the labels of ring according to the permutation induced by cycles.";
(* ERROR MESSAGES *)
PermutedRing::invalidpermutation =
"Permutation vector `1` should either be of length `2` and contain all entries  in the range 1...`2` exactly once with a 1 on the first position or should contain all entries in the range 2...`2` exactly once";
SortedRing::usage =
"SortedRing[ring] returns a ring whose generators are ordered according by increasing quantum dimension. SortedRing has the option \"SortBy\" that allows to choose a different order from the following: \"Selfdual-Conjugates\" or \"Conjugates-Selfdual\"";
RenameElements::usage =
"RenameElements[ring,elementnames] returns a ring with with elements named after elementnames. Here elementnames is only allowed to be a list of either String's, Integer's or Symbol's and must have a length equal to the rank of the ring.";
AddName::usage =
"AddName[ring,string] returns a ring where the name string is added to the list of possbile names of ring.";
SetNames::usage =
"SetNames[ring,stringlist] returns a ring for which the names are now stringlist.";


(* ::Subsubsection::Closed:: *)
(*Combining, Decomposing and comparing fusion rings*)


DirectProduct::usage =
"DirectProduct[ring1,ring2] returns the direct ring product of ring1 and ring2.";
EquivalentFusionRingQ::usage =
"EquivalentFusionRingQ[ring1,ring2] returns True if the elements of ring1 are a relabeling of the elements of ring2 and False otherwise.";
EFQ::usage =
"Shorthand for EquivalentFusionRingQ.";
(*WhichPermutation::usage =
	"WhichPermutation[ring1,ring2] returns a permutation vector v such that PermutedRing[ring1,v] equals ring2";*)
EquivalentFusionRings::usage =
"EquivalentFusionRings[ring] returns a list of all rings r for which there exists a permutation vector \[Sigma] with \[Sigma][[1]] == 1 and r equals PermutedRing[ring,\[Sigma]].";
WhichDecompositions::usage =
"WhichDecompositions[ring] returns a list of lists of fusion rings whose direct product is a fusion ring isomorphic to ring.";
WD::usage =
"Shorthand for WhichDecompositions.";
SubFusionRings::usage =
"SubFusionRings[r] returns a list of tuples { s[i], ring[i] } where s[i] is a list of indices such that the restriction of the fusion ring r to the elements with those indices gives the fusion ring r[i].";
InjectionForm::usage =
"InjectionForm[ ring, subring ] returns a fusion ring homomorphism as an association that maps each element of subring to a corresponding element of ring.";


(* ::Subsubsection::Closed:: *)
(*Working with elements*)


(* ERROR MESSAGES *)
FusionRing::elnotfound =
"`1` is not a known name of an element";
FusionRing::eloutofbounds =
"Particle number `1` does not belong to any particle in the ring";
LeftTreeDiagram::wrongnumberoflabels =
"The number of labels should be of the form 2 n + 1, where n is an integer greater than 0.";
RightTreeDiagram::wrongnumberoflabels =
"The number of labels should be of the form 2 n + 1, where n is an integer greater than 0.";

(* ::Subsubsection::Closed:: *)
(*Dataset*)


FusionRingList::usage =
"FusionRingList is a list of all saved FusionRing objects.";
FRL::usage =
"Shorthand for FusionRingList.";
AllFusionRingsQ::usage =
"AllFusionRingsAvailableQ[ r, m ] returns true if FusionRingList contains all fusion rings of rank r and multiplicity m, and False otherwise.";
AFRQ::usage =
"Shorthand for AllFusionRingsAvailableQ.";
FusionRingByCode::usage =
"FusionRingByCode[fourtuple] returns the fusion ring with formal code equal to fourtuple.";
FRBC::usage =
"Shorthand for FusionRingByCode.";
(* :Date: 2022-02-18 *)