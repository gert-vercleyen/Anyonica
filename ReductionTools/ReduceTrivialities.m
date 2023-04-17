(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2022-11-25 *)

Package["Anyonica`"]

Options[ReduceTrivialities] := { "SimplifyBy" -> Identity, "Parameters" -> {} };
ReduceTrivialities[ eqns_?ListOfEquationsQ, vars_, OptionsPattern[] ] :=
  Module[ { newEqns, newVars, revertVars, s, newSystem, simplify },
    s =
      Unique["x"];
    simplify =
      OptionValue["SimplifyBy"];
    { newEqns, newVars, revertVars } =
      SimplifyVariables[ eqns, vars, s ];

    newSystem =
      UpdateSystemViaTrivialities[ s, simplify ][ {}, {}, newEqns ];
    
    Remove[s];
    
    Normal[newSystem]/.revertVars
  ];

(* Update system to find all trivial information *)
UpdateSystemViaTrivialities[ s_Symbol, simplify_ ][
  knownVars_List,
  mapToReps_List,
  eqns_?ListOfEquationsQ
  ] :=
  With[{
    newSystem =
      Catch[
        FixedPoint[ MoldEquationsViaTrivialities[s,simplify], { knownVars, mapToReps, eqns } ]
      ]
    },
    If[
      newSystem === {False},
      { {}, {}, {False} },
      newSystem
    ]
  ];

MoldEquationsViaTrivialities[ s_Symbol, simplify_ ][ { knownVars_List, mapToReps_List, {} } ] :=
  { knownVars, mapToReps, {} };
MoldEquationsViaTrivialities[ s_Symbol, simplify_ ][
  { knownVars_List, mapToReps_, eqns_?ListOfEquationsQ }
  ] :=
  With[{
    newVarsAndEqns =
      FixedPoint[ UpdateVariablesAndSystem[s,simplify], {  knownVars, eqns } ]
    },
    With[{
      newEquivClassesAndEqns =
        FixedPoint[ UpdateEquivalenceClasses[s,simplify], {  mapToReps, newVarsAndEqns[[2]] } ]
      },
      {
        newVarsAndEqns[[1]],
        newEquivClassesAndEqns[[1]],
        DeleteDuplicates @
          Map[
            Sort,
            newEquivClassesAndEqns[[2]]
          ]
      }
    ]
  ];

UpdateVariablesAndSystem[ s_Symbol, simplify_ ][ { knownVars_List, {} } ] :=
  { knownVars, {} };
UpdateVariablesAndSystem[ s_Symbol, simplify_ ][ { knownVars_List, eqns_?ListOfEquationsQ } ] :=
  With[{
    newKnownVars =
      UpdateKnownVars[ knownVars, eqns, s ] //
      simplify
    },
    {
      newKnownVars,
      ReplaceAll[ eqns, Dispatch[newKnownVars] ] //
      simplify //
      DeleteCases[True] //
      CheckSystemValidity
    }
  ];

UpdateKnownVars[ knownVars_List, eqns_?ListOfEquationsQ , s_Symbol ] :=
  Join[
    knownVars,
    Cases[ eqns, s[i__] == x_?NumericQ | x_?NumericQ == s[i__] :> ( s[i] -> x ) ]
  ];

(* Replace all equivalent F symbols by representatives and update eqns *)
UpdateEquivalenceClasses[ s_Symbol, simplify_ ][ { mapToReps_, {} } ] :=
  { mapToReps, {} };
UpdateEquivalenceClasses[ s_Symbol, simplify_ ][ { mapToReps_, eqns_?ListOfEquationsQ } ] :=
  Module[{
    newReps =
      Dispatch @
      MapToReps @
      CreateEquivalenceClasses[s][ eqns ]
    },
    {
      Dispatch @ Join[ Normal @ mapToReps /. newReps, Normal @ newReps ],
      ( eqns /. newReps ) //
      simplify //
      DeleteCases[True] //
      CheckSystemValidity
    }
  ];

(* To use all equations of the form s[i__] == s[j__] we construct equivalence classes *)
CreateEquivalenceClasses[ s_Symbol ][ eqns_?ListOfEquationsQ ] :=
  ConnectedComponents @
  Graph @
  Cases[ eqns, s[a__] == s[b__] :> UndirectedEdge[ s[a], s[b] ] ];

MapToReps[ equivClasses_List ] :=
  Flatten[
    Thread[ Most[#] -> Last[#] ]& /@
    equivClasses
  ];