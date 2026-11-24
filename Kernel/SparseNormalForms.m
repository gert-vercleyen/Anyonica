(* ::Package:: *)

Package["Anyonica`"]

(*
+---------------------------------------------------------------------------+
|                                                                           |
|       SMITH AND HERMITE DECOMPOSITIONS OF SPARSE INTEGER MATRICES         |
|                                                                           |
+---------------------------------------------------------------------------+

  The built-in SmithDecomposition and HermiteDecomposition convert their
  argument to a dense matrix, which makes them unusable for the large sparse
  matrices that come from pentagon and hexagon equations.

  The functions in this file hand the nonzero entries of the matrix to the
  Java class anyonica.SparseNormalForms (SparseNormalForms.jar, next to this
  file; source in JavaSource/SparseNormalForms). Java is started through
  J/Link, which uses the Java runtime that is part of every Mathematica
  installation, so nothing has to be installed or configured by the user.

  If Java cannot be used the functions fall back to the built-in functions
  and say so.
*)

PackageExport["SparseSmithDecomposition"]

SparseSmithDecomposition::usage = 
  "Like Mathematica's build-in SmithDecomposition but optimized for sparse matrices."<>
  "\nUses JAVASparseSmithDecomposition and checks results afterwards. If the sparse "<>
  "decomposition failed the method will revert to the build-in SmithDecomposition.";

SparseSmithDecomposition::javafailed =
  "Computing the Smith decomposition using JAVA failed. Using built-in method instead.";

SparseSmithDecomposition[ m_ ] := 
  Module[{ u, d, v },
    { u, d, v } = JAVASparseSmithDecomposition @ m;
    If[ 
      TrueQ[ u.m.v == d ],
      Return @ {u,d,v},
      Message @ SparseSmithDecomposition::javafailed;
      Return @ SmithDecomposition[m]
    ]
  ];

PackageExport["JAVASparseSmithDecomposition"]

JAVASparseSmithDecomposition::usage =
  "JAVASparseSmithDecomposition[ mat ] returns { u, d, v } where u and v are " <>
  "unimodular, d is diagonal with d[[i,i]] dividing d[[i+1,i+1]], and " <>
  "u.mat.v == d. The matrix mat must be an integer matrix (a SparseArray " <>
  "or a list of lists); u, d and v are returned as SparseArrays. " <>
  "Java processes zero-background sparse matrices directly. A nonzero " <>
  "background or a fallback to the built-in function expands the input.\n" <>
  "Options:\n" <>
  "\"OmitTransformations\" -> None | \"Left\" | \"Right\" | All : " <>
  "transformation matrices that are not needed. Java skips them and " <>
  "Missing[\"NotComputed\"] is returned in their place. For a matrix with " <>
  "many more rows than columns, u takes most of the time and memory.\n" <>
  "\"ApplyUTo\" -> b : return u.b instead of u, where b has as many rows " <>
  "as mat. If b has few columns this is much cheaper than computing u.\n" <>
  "\"ApplyVTo\" -> c : return c.v instead of v, where c has as many " <>
  "columns as mat. Nonzero factors with coefficients of magnitude 2^31 " <>
  "or greater use the full transformation followed by exact multiplication.";

JAVASparseSmithDecomposition::intmat =
  "The first argument should be a non-empty matrix of integers.";

JAVASparseSmithDecomposition::omit =
  "`1` is not a valid value for the option \"OmitTransformations\". " <>
  "Use None, \"Left\", \"Right\" or All.";

JAVASparseSmithDecomposition::factor =
  "The value of the option `1` should be an integer matrix with `2` `3`.";

JAVASparseSmithDecomposition::nojava =
  "The Java class anyonica.SparseNormalForms could not be loaded from `1`. " <>
  "Using the built-in SmithDecomposition, which is slow for large matrices.";

JAVASparseSmithDecomposition::failed =
  "The calculation failed. If a message above mentions " <>
  "java.lang.OutOfMemoryError, give Java more memory with " <>
  "ReinstallJava[ JVMArguments -> \"-Xmx16g\" ] from J/Link " <>
  "(here 16 GB) and try again.";

Options[JAVASparseSmithDecomposition] =
  {
    "OmitTransformations" -> None,
    "ApplyUTo" -> None,
    "ApplyVTo" -> None
  };

JAVASparseSmithDecomposition[ mat_, opts:OptionsPattern[] ] :=
  Module[ { sa, omit, b, c, m, n },
    sa = toSparseIntegerMatrix @ mat;

    If[
      sa === $Failed,
      Message[ JAVASparseSmithDecomposition::intmat ];
      Return @ $Failed
    ];

    { m, n } = Dimensions @ sa;

    omit = transformationsToOmit @ OptionValue @ "OmitTransformations";

    If[
      omit === $Failed,
      Message[ JAVASparseSmithDecomposition::omit, OptionValue @ "OmitTransformations" ];
      Return @ $Failed
    ];

    b = toFactor[ OptionValue @ "ApplyUTo", 1, m ];

    If[
      b === $Failed,
      Message[ JAVASparseSmithDecomposition::factor, "\"ApplyUTo\"", m, "rows" ];
      Return @ $Failed
    ];

    c = toFactor[ OptionValue @ "ApplyVTo", 2, n ];

    If[
      c === $Failed,
      Message[ JAVASparseSmithDecomposition::factor, "\"ApplyVTo\"", n, "columns" ];
      Return @ $Failed
    ];

    sparseDecomposition[ JAVASparseSmithDecomposition, "Smith", sa, omit, b, c ]
  ];


PackageExport["SparseHermiteDecomposition"]

SparseHermiteDecomposition::usage = 
  "Like Mathematica's build-in HermiteDecomposition but optimized for sparse matrices."<>
  "\nUses JAVASparseHermiteDecomposition and checks results afterwards. If the sparse "<>
  "decomposition failed the method will revert to the build-in SmithDecomposition.";

SparseHermiteDecomposition::javafailed =
  "Computing the Hermite decomposition using JAVA failed. Using built-in method instead.";

SparseHermiteDecomposition[ m_ ] := 
  Module[{ u, h },
    { u, h } = JAVASparseHermiteDecomposition @ m;
    If[ 
      TrueQ[ u.m == h ],
      Return @ {u,h},
      Message @ SparseHermiteDecomposition::javafailed;
      Return @ HermiteDecomposition[m]
    ]
  ];

JAVASparseHermiteDecomposition::usage =
  "SparseHermiteDecomposition[ mat ] returns { u, h } where u is " <>
  "unimodular, h is the row Hermite normal form of mat (row echelon form " <>
  "with positive pivots, entries above a pivot p in the range 0,...,p-1, " <>
  "zero rows at the bottom), and u.mat == h. The matrix mat must be an " <>
  "integer matrix (a SparseArray or a list of lists); u and h are " <>
  "returned as SparseArrays. Java processes zero-background sparse matrices " <>
  "directly. A nonzero background or a fallback to the built-in function " <>
  "expands the input.\n" <>
  "Options:\n" <>
  "\"OmitTransformations\" -> None | \"Left\" | All : with \"Left\" or " <>
  "All, Java skips u and Missing[\"NotComputed\"] is returned in " <>
  "its place.\n" <>
  "\"ApplyUTo\" -> b : return u.b instead of u, where b has as many rows " <>
  "as mat. Nonzero factors with coefficients of magnitude 2^31 or greater " <>
  "use the full transformation followed by exact multiplication.";

JAVASparseHermiteDecomposition::intmat =
  "The first argument should be a non-empty matrix of integers.";

JAVASparseHermiteDecomposition::omit =
  "`1` is not a valid value for the option \"OmitTransformations\". " <>
  "Use None, \"Left\" or All.";

JAVASparseHermiteDecomposition::factor =
  "The value of the option `1` should be an integer matrix with `2` `3`.";

JAVASparseHermiteDecomposition::nojava =
  "The Java class anyonica.SparseNormalForms could not be loaded from `1`. " <>
  "Using the built-in HermiteDecomposition, which is slow for large matrices.";

JAVASparseHermiteDecomposition::failed =
  "The calculation failed. If a message above mentions " <>
  "java.lang.OutOfMemoryError, give Java more memory with " <>
  "ReinstallJava[ JVMArguments -> \"-Xmx16g\" ] from J/Link " <>
  "(here 16 GB) and try again.";

Options[JAVASparseHermiteDecomposition] =
  {
    "OmitTransformations" -> None,
    "ApplyUTo" -> None
  };

JAVASparseHermiteDecomposition[ mat_, opts:OptionsPattern[] ] :=
  Module[ { sa, omit, b, m, n },
    sa = toSparseIntegerMatrix @ mat;

    If[
      sa === $Failed,
      Message[ JAVASparseHermiteDecomposition::intmat ];
      Return @ $Failed
    ];

    { m, n } = Dimensions @ sa;

    omit = transformationsToOmit[ OptionValue["OmitTransformations"] ];

    If[
      omit === $Failed,
      Message[ JAVASparseHermiteDecomposition::omit, OptionValue["OmitTransformations"] ];
      Return @ $Failed
    ];

    b =
      toFactor[ OptionValue["ApplyUTo"], 1, m ];

    If[
      b === $Failed,
      Message[ JAVASparseHermiteDecomposition::factor, "\"ApplyUTo\"", m, "rows" ];
      Return @ $Failed 
    ];

    sparseDecomposition[ JAVASparseHermiteDecomposition, "Hermite", sa, omit, b, None ]
  ];


(*
+---------------------------------------------------------------------------+
|                              Implementation                               |
+---------------------------------------------------------------------------+
*)

(* Directory of this file, recorded while the package is being loaded *)
$sparseNormalFormsDirectory =
  DirectoryName[ $InputFileName ];

(* The jar is expected next to this file. As a second guess, look in the
   installed paclet. *)
$SparseNormalFormsJar :=
  With[
    {
      candidates =
        {
          FileNameJoin[ { $sparseNormalFormsDirectory, "SparseNormalForms.jar" } ],
          Quiet @ Check[
            FileNameJoin[
              { PacletObject["Anyonica"]["Location"], "Kernel", "ReductionTools", "SparseNormalForms.jar" }
            ],
            ""
          ]
        }
    },
    SelectFirst[ candidates, StringQ[#] && FileExistsQ[#]&, First @ candidates ]
  ];

$notComputed = Missing["NotComputed"];

(* Returns mat as a SparseArray with integer entries and background 0,
   or $Failed if mat is not a non-empty integer matrix. *)
toSparseIntegerMatrix[ mat_ ] :=
  Module[ { sa },
    sa = Quiet @ Check[ SparseArray[ mat ], $Failed ];

    Which[
      Head[ sa ] =!= SparseArray || ArrayDepth[ sa ] =!= 2,
      $Failed,
      Min[ Dimensions[ sa ] ] < 1,
      $Failed,
      sa["Background"] =!= 0,
      toSparseIntegerMatrix[ Normal[ sa ] ],
      !VectorQ[ sa["NonzeroValues"], IntegerQ ],
      $Failed,
      True,
      sa
    ]
  ];

(* { omit u, omit v } *)
transformationsToOmit[ None | {} ] := { False, False };

transformationsToOmit[ "Left" | { "Left" } ] := { True, False };

transformationsToOmit[ "Right" | { "Right" } ] := { False, True };

transformationsToOmit[ All | { "Left", "Right" } | { "Right", "Left" } ] := { True, True };

transformationsToOmit[ _ ] := $Failed;

(* The matrix given to "ApplyUTo" (pos = 1, size = number of rows of mat)
   or "ApplyVTo" (pos = 2, size = number of columns of mat). *)
toFactor[ None, _, _ ] :=
  None;

toFactor[ f_, pos_, size_ ] :=
  With[ { sf = toSparseIntegerMatrix[ f ] },
    If[
      sf === $Failed || Dimensions[ sf ][[ pos ]] =!= size,
      $Failed,
      sf
    ]
  ];

(* Factors are passed to Java only if all their entries are 32 bit integers.
   Otherwise the transformation is computed in full and multiplied here. *)
javaFactorQ[ None ] := False;

javaFactorQ[ sa_ ] :=
  With[ { vals = sa["NonzeroValues"] },
    vals =!= {} && Max[ Abs[ vals ] ] < 2^31
  ];

sparseDecomposition[ head_, type_, sa_, { omitU_, omitV_ }, b_, c_ ] :=
  Module[ { m, n, computeU, computeV, bInJava, cInJava, result, u, d, v },
    { m, n } =
      Dimensions[ sa ];

    If[ Total[ Abs[ sa["NonzeroValues"] ] ] == 0,
      (* The identity transformations send each supplied factor to itself. *)
      u = Which[ omitU, $notComputed, b =!= None, b, True, IdentityMatrix[ m, SparseArray ] ];
      d = SparseArray[ {}, { m, n } ];
      If[ type === "Hermite", Return[ { u, d } ] ];
      v = Which[ omitV, $notComputed, c =!= None, c, True, IdentityMatrix[ n, SparseArray ] ];
      Return[ { u, d, v } ]
    ];

    (* Applying any transformation to a zero factor gives the same zero factor. *)
    computeU =
      !omitU && (b === None || AnyTrue[ b["NonzeroValues"], # =!= 0 & ]);

    computeV =
      type === "Smith" && !omitV && (c === None || AnyTrue[ c["NonzeroValues"], # =!= 0 & ]);

    bInJava =
      computeU && javaFactorQ[ b ];

    cInJava =
      computeV && javaFactorQ[ c ];

    result =
      javaDecomposition[
        type, sa, computeU, computeV,
        If[ bInJava, b, None ],
        If[ cInJava, c, None ]
      ];

    If[
      result === "NoJava",
      Message[ MessageName[ head, "nojava" ], $SparseNormalFormsJar ];
      bInJava = cInJava = False;
      result = builtInDecomposition[ type, sa, computeU, computeV ]
    ];

    If[
      !MatchQ[ result, { _, _, _ } ],
      Message[ MessageName[ head, "failed" ] ];
      Return[ $Failed ]
    ];

    { u, d, v } =
      result;

    If[ !omitU && b =!= None && !bInJava, u = If[ computeU, u . b, b ] ];

    If[ !omitV && c =!= None && !cInJava, v = If[ computeV, c . v, c ] ];

    If[ type === "Smith", { u, d, v }, { u, d } ]
  ];

(* Returns { u, d, v } (entries that were not asked for are $notComputed),
   "NoJava" if the Java class cannot be used, or $Failed. *)
javaDecomposition[ type_, sa_, wantU_, wantV_, b_, c_ ] :=
  Block[ { $ContextPath = $ContextPath },
    (* Needs puts JLink` on the context path; Block undoes that afterwards *)
    Quiet[
      Needs["JLink`"];
      JLink`InstallJava[];
      (* The class path of J/Link is reset by ReinstallJava, so add the jar every time *)
      With[ { jar = $SparseNormalFormsJar }, JLink`AddToClassPath[ jar ] ]
    ];

    JLink`JavaBlock[
      Module[ { snf, m, n, uColumns, vRows, status, u, d, v },
        { m, n } =
          Dimensions[ sa ];

        u = v = $notComputed;

        snf =
          Quiet @ JLink`JavaNew["anyonica.SparseNormalForms"];

        If[
          !TrueQ[ JLink`JavaObjectQ[ snf ] ]
          ,
          "NoJava"
          ,
          (* J/Link returns Null on success for each of these void methods. *)
          If[ Check[sendMatrix[snf, sa], $Failed] =!= Null, Return[$Failed] ];
          uColumns = sendFactor[snf, b, 2];
          If[ uColumns === $Failed, Return[$Failed] ];
          vRows = sendFactor[snf, c, 1];
          If[ vRows === $Failed, Return[$Failed] ];

          status = Check[
            If[type === "Smith", snf @ smith[wantU, wantV], snf @ hermite[wantU]],
            $Failed
          ];
          If[ status =!= Null, Return[$Failed] ];

          d = readMatrix[snf, "D", {m, n}];
          If[ wantU,
            u = readMatrix[snf, "U", {m, If[b === None, m, Last @ Dimensions @ b]}, uColumns]
          ];
          If[ wantV,
            v = readMatrix[snf, "V", {If[c === None, n, First @ Dimensions @ c], n}, vRows]
          ];
          snf @ clear[];
          If[ MemberQ[{u, d, v}, $Failed], $Failed, {u, d, v} ]
        ]
      ]
    ]
  ];

(* Arrays of 32 bit integers are transferred quickly by J/Link. Larger
   entries are rare and are sent as text. *)
sendMatrix[ snf_, sa_ ] :=
  Module[ { pos, vals, m, n },
    { m, n } =
      Dimensions[ sa ];

    pos =
      sa["NonzeroPositions"];

    vals =
      sa["NonzeroValues"];

    If[
      Max[ Abs[ vals ] ] < 2^31,
      snf @ setMatrix[ m, n, pos[[ All, 1 ]], pos[[ All, 2 ]], vals ],
      snf @ setMatrixBig[
        m, n, pos[[ All, 1 ]], pos[[ All, 2 ]],
        StringRiffle[ ToString[ #, InputForm ]& /@ vals, " " ]
      ]
    ]
  ];

(* An m-by-k left factor can have a column label larger than a Java int.
   Row operations mix its rows but never its columns. The same holds for the
   row labels of a right factor under column operations. Retain the occupied
   labels so readMatrix can restore their original positions. *)
sendFactor[ _, None, _ ] := None;

sendFactor[ snf_, factor_, freeAxis_ ] :=
  Module[ { indices, values, labels = None, status },
    indices = Transpose[factor["NonzeroPositions"]];
    values = factor["NonzeroValues"];
    If[ Dimensions[factor][[freeAxis]] >= 2^31,
      labels = Union[indices[[freeAxis]]];
      indices[[freeAxis]] = Lookup[
        AssociationThread[labels, Range[Length[labels]]], indices[[freeAxis]]
      ]
    ];
    status = Check[
      If[ freeAxis === 2,
        snf @ setLeftFactor[indices[[1]], indices[[2]], values],
        snf @ setRightFactor[indices[[1]], indices[[2]], values]
      ],
      $Failed
    ];
    If[ status === Null, labels, $Failed ]
  ];

(* which is "U", "D" or "V"; for a Hermite decomposition "D" is the matrix h *)
readMatrix[ snf_, which_, dims_, labels_:None ] :=
  Module[ { ri, ci, vi },
    ri =
      snf @ cooRows[ which ];

    ci =
      snf @ cooCols[ which ];

    vi =
      If[
        TrueQ[ snf @ cooFitsInt[ which ] ],
        snf @ cooValues[ which ],
        ToExpression[ snf @ cooValuesString[ which ] ]
      ];

    Which[
      !( ListQ[ ri ] && ListQ[ ci ] && ListQ[ vi ] ),
      $Failed,
      !( Length[ ri ] == Length[ ci ] == Length[ vi ] ),
      $Failed,
      ri === {},
      SparseArray[ {}, dims ],
      True,
      Check[
        If[ labels =!= None,
          If[which === "U", ci = labels[[ci]], ri = labels[[ri]]]
        ];
        SparseArray[ Transpose[ { ri, ci } ] -> vi, dims ],
        $Failed
      ]
    ]
  ];

builtInDecomposition[ "Smith", sa_, wantU_, wantV_ ] :=
  With[ { r = SmithDecomposition[ Normal[ sa ] ] },
    If[
      MatchQ[ r, { _?MatrixQ, _?MatrixQ, _?MatrixQ } ],
      {
        If[ wantU, SparseArray[ r[[1]] ], $notComputed ],
        SparseArray[ r[[2]] ],
        If[ wantV, SparseArray[ r[[3]] ], $notComputed ]
      },
      $Failed
    ]
  ];

builtInDecomposition[ "Hermite", sa_, wantU_, _ ] :=
  With[ { r = HermiteDecomposition[ Normal[ sa ] ] },
    If[
      MatchQ[ r, { _?MatrixQ, _?MatrixQ } ],
      {
        If[ wantU, SparseArray[ r[[1]] ], $notComputed ],
        SparseArray[ r[[2]] ],
        $notComputed
      },
      $Failed
    ]
  ];
