(* ::Package:: *)

(*
+---------------------------------------------------------------------------+
|                                                                           |
|                             FINDING 0-VALUES                              |
|                                                                           |
+---------------------------------------------------------------------------+
*)

Options[ FindZeroValues ] := { "InvertibleMatrices" -> {} };
FindZeroValues[ eqns_List, vars_, opts:OptionsPattern[] ] :=
  Module[ {
    regMats,binomialLists, nonZeroVars, extraKnowns, extraReps, simpleEqns,
    newBinomialLists, permCond, unknowns, simpleNonZeroVars,
    simpleVars, revertVars, simpleMats, zeroCond, s, procID,
    trivialKnowns, trivialReps, reducedEqns, simplifyBinomialLists, result, absTime
    },
    
    procID =
      ToString[Unique[]];

    printlog["FZV:init", {procID,eqns,vars,{opts}}];
    
    { absTime, result } =
    AbsoluteTiming[
      regMats =
        Normal @
        OptionValue["InvertibleMatrices"];

      If[
        MemberQ[ PermanentConditions @ regMats, False ] || vars === {},
        Return[ {} ]
      ];

      s =
        Unique["x"];

      nonZeroVars =
        Cases[ regMats, {{a_}} /; a =!= 0 :> (a -> 1) ];

      If[
        Length[nonZeroVars] == Length[vars],
        Return[ {{}} ]
      ];
      
      (* TODO: we don't use the non-zero vars efficiently I think *)
      
      { { simpleEqns, simpleNonZeroVars, simpleMats }, simpleVars, revertVars } =
        SimplifyVariables[ { eqns, nonZeroVars, regMats }, vars, s ];
      
      { trivialKnowns, trivialReps, reducedEqns } =
        ReduceTrivialities[ simpleEqns, simpleVars ];
      
      simplifyBinomialLists[ monLists_ ] :=
        monLists //
        ReplaceAll[ Power[ a_, b_Integer ] :> a ] //
        ReplaceAll[ Times[ a_Integer, b__ ] :> Times[b] ];
      
      binomialLists =
        DeleteDuplicates[
          Sort /@
          Select[ Not @* TrivialMonListQ ] @
          simplifyBinomialLists[
            Join @@@
            MonomialList[ List @@@ reducedEqns ]
          ]
        ];

      If[
        MemberQ[ _?InconsistentMonListQ ] @ binomialLists,
        printlog["FZV:inconsistent_system", {procID,binomialLists,eqns}];
        Return[ {} ];
      ];
      
      { extraKnowns, extraReps, newBinomialLists } =
        UpdateSystem[s][ {}, {}, binomialLists ];
      
      If[ (* If after updating system all variables turn out to be non-zero, system is trivial *)
        Length[ extraKnowns ] === Length[ vars ],
        Return[ { Thread[ vars -> vars ] } ]
      ];
      
      If[
        MemberQ[ _?InconsistentMonListQ ] @ newBinomialLists,
        printlog["FZV:inconsistent_system", {procID,eqns}];
        Return[ {} ];
      ];
      
      zeroCond = (* Note that these are valid for both binomial and non-binomial equations *)
        ( Total[#] != 1 )& /@
        newBinomialLists;

      permCond =
        DeleteDuplicates[
          PermanentConditions[ simpleMats ~ WithMinimumDimension ~ 2 ]/.
          trivialReps/.
          trivialKnowns/.
          extraReps/.
          extraKnowns
        ];

      unknowns =
        Complement[
          simpleVars/.trivialReps/.extraReps,
          Join[
            trivialKnowns,
            extraKnowns
          ][[;;,1]]
        ];

      ReplaceAll[
        SortBy[First] /@
        Cases[ HoldPattern[ x_ -> 0 ] ] /@
        Flatten /@
        Map[
          Orbit[ Normal @ trivialReps ] @* Orbit[ Normal @ extraReps ],
          Sort[ Join[ trivialKnowns, extraKnowns, # ] ]& /@
          SolveDiophantineEquations[ Join[ zeroCond, permCond  ], unknowns , { 0, 1 } ],
          {2}
        ],
        revertVars
      ]
    ];
    printlog["FZV:solutions", {procID,result}];
    printlog["Gen:results", {procID,result,absTime}];

    Remove[s];

    result
  ];

TrivialMonListQ[ list_ ] :=
  Or[
    MatchQ[ list, Repeated[ _?NumericQ ] /; Total[list] =!= 1 ],
    Length[list] == 2 && list[[1]] === list[[2]]
  ];

InconsistentMonListQ[ list_ ] :=
  MatchQ[ list, Repeated[ _?NumericQ ] /; Total[list] === 1 ];

BinomialSystemQ[ eqns_List ] :=
  !MemberQ[ eqns, eqn_ /; !BinomialEquationQ[ eqn ] ];

PermanentConditions[ mats_List ] :=
  Map[
    ( Expand[Permanent[#]] != 0 )&,
    mats
  ];

(* Update system to find all trivial information *)
UpdateSystem[ s_Symbol ][ knownVars_List, mapToReps_List, eqns_List ] :=
  FixedPoint[ MoldEquations[s], { knownVars, mapToReps, eqns } ];

MoldEquations[ s_Symbol ][ { knownVars_List, mapToReps_List, {} } ] :=
  { knownVars, mapToReps, {} };
MoldEquations[ s_Symbol ][ { knownVars_List, mapToReps_, eqns_List } ] :=
  With[{
    deducedData = FixedPoint[ Deduce[s], { knownVars, eqns } ]
    },
    With[{
      reducedData =
        FixedPoint[ ReduceVariables[s], { mapToReps, deducedData[[2]] } ] },
      { deducedData[[1]], reducedData[[1]], RemoveEquivalentMonLists[ reducedData[[2]] ] }
    ]
  ];

(* Deduce info about non-zero F symbols from the mon eqns *)
Deduce[ s_Symbol ][ { knownVars_List, {} } ] :=
  { knownVars, {} };
Deduce[ s_Symbol ][ { knownVars_List, monList_List } ] :=
  With[{
    newMonLists =
      (
        DeleteDuplicates @*
        SplitLists[s] @*
        DeleteCases[ _?TrivialMonListQ ]
      ) @
      ReplaceAll[ monList, knownVars ]
    },
    {
      UpdateKnownVars[ knownVars, newMonLists, s ],
      DeleteCases[ newMonLists, { 1, s[__] } ]
    }
  ];

(* Splitting the equations in smaller parts *)
SplitLists[ s_Symbol ][ {} ] :=
  {};
SplitLists[ s_Symbol ][ monLists_List ] :=
  Module[{
    SplitList
    },
    SplitList[ { 1, HoldPattern[ Times[ x__ ] ] } ] :=
      Sow[ { 1 , # } ]& /@  {x};
    SplitList[ { HoldPattern[ Times[ x__ ] ], 1 } ] :=
      Sow[ { 1 , # } ]& /@  {x};
    SplitList[ { s[i__], 1 } ] :=
      Sow[ { 1, s[i] } ];
    SplitList[ { s[i__], 0} ] :=
      Sow[ { 0, s[i] } ];
    SplitList[x_] :=
      Sow[x];
    Reap[
      SplitList /@
      monLists
    ][[2,1]]
  ];

UpdateKnownVars[ knownVars_List, monLists_ , s_Symbol ] :=
  Join[
    knownVars,
    Cases[ monLists, { n_Integer, s[i__] } :> ( s[i] -> n ) ]
  ];

(* Replace all equivalent F symbols by representatives and update eqns *)
ReduceVariables[ s_Symbol ][ { mapToReps_, {} } ] :=
  { mapToReps, {} };
ReduceVariables[ s_Symbol ][ { mapToReps_, monLists_ } ] :=
  Module[{
    newReps =
      Dispatch @
      MapToReps @
      CreateBinomialEquivalenceClasses[s][ monLists/.(Power[a_,_Integer] :> a)]
    },
    {
      Dispatch @ Join[ Normal @ mapToReps /. newReps, Normal @ newReps ],
      DeleteCases[ _?TrivialMonListQ ] @ ( monLists /. newReps )
    }
  ];

(* To use all equations of the form s[i__] == s[j__] we construct equivalence classes *)
CreateBinomialEquivalenceClasses[ s_Symbol ][ monLists_List ] :=
  ConnectedComponents @
  Graph @
  Cases[ monLists, { s[a__], s[b__] } :> UndirectedEdge[ s[a], s[b] ] ];

MapToReps[ equivClasses_List ] :=
  Flatten[
    Thread[ Rest[#] -> First[#] ]& /@ equivClasses
  ];

Orbit[ mapToReps_ ][ var_ ] :=
  If[
    FreeQ[ mapToReps[[;;,2]], var ],
    var,
    With[ { asso = Association @@ mapToReps },
      Append[ var ] @
      Select[ Keys[asso], asso[#] === var & ]
    ]
  ];

Orbit[ mapToReps_ ][ var_ -> val_ ] :=
  If[
    FreeQ[ mapToReps[[;;,2]], var ],
    var -> val,
    With[ { asso = Association @@ mapToReps },
      Map[
        (# -> val)&,
        Append[ var ] @
        Select[ Keys[asso], asso[#] === var & ]
      ]
    ]
  ];

RemoveEquivalentMonLists[ monLists_List ] :=
  DeleteDuplicates[ monLists, Sort[#1] === Sort[#2]& ];