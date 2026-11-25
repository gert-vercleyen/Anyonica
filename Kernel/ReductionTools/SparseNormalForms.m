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

  This file belongs together with two other files, which have to be in the
  same directory as this file (Kernel/ReductionTools):

    SparseNormalForms.jar    the Java class anyonica.SparseNormalForms
    SparseNormalForms.java   the source of that class. The comment at its
                             top says how to rebuild the jar.

  JAVASparseSmithDecomposition and JAVASparseHermiteDecomposition hand the
  nonzero entries of a matrix to the Java class and return what it computes:
  the normal form, the transformation matrices, and the inverses of the
  transformation matrices. Java is started through J/Link, which uses the
  Java runtime that is part of every Mathematica installation, so nothing
  has to be installed or configured by the user.

  SparseSmithDecomposition and SparseHermiteDecomposition are the functions
  that the rest of the package uses. They call the two functions above,
  verify the result with exact integer arithmetic, and use the built-in
  functions if Java could not be used or if the result is not correct.
*)

PackageExport["SparseSmithDecomposition"]

SparseSmithDecomposition::usage =
  "SparseSmithDecomposition[ mat ] returns { u, d, v }, like the built-in " <>
  "SmithDecomposition: u and v are unimodular, d is diagonal with " <>
  "non-negative entries each of which divides the next, and u.mat.v == d. " <>
  "The matrix mat must be an integer matrix (a SparseArray or a list of " <>
  "lists); u, d and v are returned as SparseArrays.\n" <>
  "The decomposition is computed by JAVASparseSmithDecomposition and then " <>
  "verified with exact arithmetic. If Java cannot be used, the calculation " <>
  "fails, or the result is not correct, a message is issued and the " <>
  "built-in SmithDecomposition is used instead.";

SparseSmithDecomposition::javafailed =
  "No Smith decomposition was obtained from Java. Using the built-in " <>
  "SmithDecomposition instead, which is slow for large matrices.";

SparseSmithDecomposition::wrong =
  "The Smith decomposition that Java returned is not correct: `1`. Using " <>
  "the built-in SmithDecomposition instead. This should not happen; please " <>
  "keep the matrix and report it.";

SparseSmithDecomposition[ mat_ ] :=
  Module[ { sa, result, failure },
    sa =
      toSparseIntegerMatrix[ mat ];

    If[
      sa === $Failed
      ,
      (* Not a matrix that Java can be given: leave it to the built-in function *)
      SmithDecomposition[ mat ]
      ,
      result =
        JAVASparseSmithDecomposition[ sa ];

      failure =
        If[
          MatchQ[ result, { _, _, _, _, _ } ],
          smithDecompositionFailure[ sa, result ],
          $noResult
        ];

      Which[
        failure === None
        ,
        result[[ { 1, 3, 4 } ]]
        ,
        failure === $noResult
        ,
        Message[ SparseSmithDecomposition::javafailed ];
        builtInSmithDecomposition[ sa ]
        ,
        True
        ,
        Message[ SparseSmithDecomposition::wrong, failure ];
        builtInSmithDecomposition[ sa ]
      ]
    ]
  ];


PackageExport["SparseHermiteDecomposition"]

SparseHermiteDecomposition::usage =
  "SparseHermiteDecomposition[ mat ] returns { u, h }, like the built-in " <>
  "HermiteDecomposition: u is unimodular, h is the row Hermite normal form " <>
  "of mat (row echelon form with positive pivots, entries above a pivot p " <>
  "in the range 0,...,p-1, zero rows at the bottom), and u.mat == h. " <>
  "The matrix mat must be an integer matrix (a SparseArray or a list of " <>
  "lists); u and h are returned as SparseArrays.\n" <>
  "The decomposition is computed by JAVASparseHermiteDecomposition and then " <>
  "verified with exact arithmetic. If Java cannot be used, the calculation " <>
  "fails, or the result is not correct, a message is issued and the " <>
  "built-in HermiteDecomposition is used instead.";

SparseHermiteDecomposition::javafailed =
  "No Hermite decomposition was obtained from Java. Using the built-in " <>
  "HermiteDecomposition instead, which is slow for large matrices.";

SparseHermiteDecomposition::wrong =
  "The Hermite decomposition that Java returned is not correct: `1`. Using " <>
  "the built-in HermiteDecomposition instead. This should not happen; please " <>
  "keep the matrix and report it.";

SparseHermiteDecomposition[ mat_ ] :=
  Module[ { sa, result, failure },
    sa =
      toSparseIntegerMatrix[ mat ];

    If[
      sa === $Failed
      ,
      (* Not a matrix that Java can be given: leave it to the built-in function *)
      HermiteDecomposition[ mat ]
      ,
      result =
        JAVASparseHermiteDecomposition[ sa ];

      failure =
        If[
          MatchQ[ result, { _, _, _ } ],
          hermiteDecompositionFailure[ sa, result ],
          $noResult
        ];

      Which[
        failure === None
        ,
        result[[ { 1, 3 } ]]
        ,
        failure === $noResult
        ,
        Message[ SparseHermiteDecomposition::javafailed ];
        builtInHermiteDecomposition[ sa ]
        ,
        True
        ,
        Message[ SparseHermiteDecomposition::wrong, failure ];
        builtInHermiteDecomposition[ sa ]
      ]
    ]
  ];


PackageExport["JAVASparseSmithDecomposition"]

JAVASparseSmithDecomposition::usage =
  "JAVASparseSmithDecomposition[ mat ] computes a Smith decomposition of " <>
  "the integer matrix mat (a SparseArray or a list of lists) with the Java " <>
  "class anyonica.SparseNormalForms and returns { u, ui, d, v, vi }, where " <>
  "u.mat.v == d, d is diagonal with non-negative entries each of which " <>
  "divides the next, u and v are unimodular, ui is the inverse of u and vi " <>
  "is the inverse of v. The five matrices are SparseArrays.\n" <>
  "The result is returned as Java computed it, without verification. " <>
  "$Failed is returned if Java cannot be used or if the calculation fails. " <>
  "SparseSmithDecomposition verifies the result and falls back on the " <>
  "built-in SmithDecomposition.";

JAVASparseSmithDecomposition::intmat =
  "The argument should be a non-empty matrix of integers.";

JAVASparseSmithDecomposition::nojava =
  "The Java class anyonica.SparseNormalForms could not be loaded from `1`.";

JAVASparseSmithDecomposition::oldjar =
  "Java is using an old version of the class anyonica.SparseNormalForms. " <>
  "If the file `1` has just been replaced, evaluate ReinstallJava[] from " <>
  "J/Link or restart the kernel. Otherwise the file is outdated.";

JAVASparseSmithDecomposition::failed =
  "The calculation failed. If a message above mentions " <>
  "java.lang.OutOfMemoryError, give Java more memory with " <>
  "ReinstallJava[ JVMArguments -> \"-Xmx16g\" ] from J/Link " <>
  "(here 16 GB) and try again.";

JAVASparseSmithDecomposition[ mat_ ] :=
  With[ { sa = toSparseIntegerMatrix[ mat ] },
    If[
      sa === $Failed,
      Message[ JAVASparseSmithDecomposition::intmat ];
      $Failed,
      sparseDecomposition[ JAVASparseSmithDecomposition, "Smith", sa ]
    ]
  ];


PackageExport["JAVASparseHermiteDecomposition"]

JAVASparseHermiteDecomposition::usage =
  "JAVASparseHermiteDecomposition[ mat ] computes a Hermite decomposition " <>
  "of the integer matrix mat (a SparseArray or a list of lists) with the " <>
  "Java class anyonica.SparseNormalForms and returns { u, ui, h }, where " <>
  "u.mat == h, h is the row Hermite normal form of mat (row echelon form " <>
  "with positive pivots, entries above a pivot p in the range 0,...,p-1, " <>
  "zero rows at the bottom), u is unimodular and ui is the inverse of u. " <>
  "The three matrices are SparseArrays.\n" <>
  "The result is returned as Java computed it, without verification. " <>
  "$Failed is returned if Java cannot be used or if the calculation fails. " <>
  "SparseHermiteDecomposition verifies the result and falls back on the " <>
  "built-in HermiteDecomposition.";

JAVASparseHermiteDecomposition::intmat =
  "The argument should be a non-empty matrix of integers.";

JAVASparseHermiteDecomposition::nojava =
  "The Java class anyonica.SparseNormalForms could not be loaded from `1`.";

JAVASparseHermiteDecomposition::oldjar =
  "Java is using an old version of the class anyonica.SparseNormalForms. " <>
  "If the file `1` has just been replaced, evaluate ReinstallJava[] from " <>
  "J/Link or restart the kernel. Otherwise the file is outdated.";

JAVASparseHermiteDecomposition::failed =
  "The calculation failed. If a message above mentions " <>
  "java.lang.OutOfMemoryError, give Java more memory with " <>
  "ReinstallJava[ JVMArguments -> \"-Xmx16g\" ] from J/Link " <>
  "(here 16 GB) and try again.";

JAVASparseHermiteDecomposition[ mat_ ] :=
  With[ { sa = toSparseIntegerMatrix[ mat ] },
    If[
      sa === $Failed,
      Message[ JAVASparseHermiteDecomposition::intmat ];
      $Failed,
      sparseDecomposition[ JAVASparseHermiteDecomposition, "Hermite", sa ]
    ]
  ];


(*
+---------------------------------------------------------------------------+
|                               Verification                                |
+---------------------------------------------------------------------------+

  A result of Java is accepted only if it passes the tests below. All of
  them are exact calculations with integers.

  If u and ui are integer matrices with u.ui equal to the identity matrix,
  then det(u) det(ui) == 1, so det(u) is 1 or -1 and u is unimodular. The
  same holds for v and vi. Together with u.mat.v == d and with d being in
  Smith normal form, this says that { u, d, v } is a Smith decomposition of
  mat. Likewise u.mat == h, with u unimodular and h in Hermite normal form,
  says that { u, h } is a Hermite decomposition of mat. Since both normal
  forms are unique, d and h are then the same matrices that the built-in
  functions return (up to the signs of the entries of d, which the built-in
  SmithDecomposition does not always make positive).
*)

(* Returned instead of a description of what is wrong when there is no result at all *)
$noResult = "no result";

(* None if { u, d, v } is a Smith decomposition of the integer sparse matrix a
   and ui, vi are the inverses of u, v. Otherwise a string that says which
   test failed. *)
smithDecompositionFailure[ a_, { u_, ui_, d_, v_, vi_ } ] :=
  Module[ { m, n },
    { m, n } =
      Dimensions[ a ];

    Quiet @ Check[
      Which[
        !integerSparseMatrixQ[ u, { m, m } ],
        "u is not an integer matrix of the right dimensions",
        !integerSparseMatrixQ[ ui, { m, m } ],
        "the inverse of u is not an integer matrix of the right dimensions",
        !integerSparseMatrixQ[ d, { m, n } ],
        "d is not an integer matrix of the right dimensions",
        !integerSparseMatrixQ[ v, { n, n } ],
        "v is not an integer matrix of the right dimensions",
        !integerSparseMatrixQ[ vi, { n, n } ],
        "the inverse of v is not an integer matrix of the right dimensions",
        !smithNormalFormQ[ d ],
        "d is not in Smith normal form",
        !zeroMatrixQ[ u . ui - IdentityMatrix[ m, SparseArray ] ],
        "u times its inverse is not the identity matrix",
        !zeroMatrixQ[ v . vi - IdentityMatrix[ n, SparseArray ] ],
        "v times its inverse is not the identity matrix",
        !zeroMatrixQ[ u . a . v - d ],
        "u.mat.v is not equal to d",
        True,
        None
      ],
      "a message was generated during the verification"
    ]
  ];

(* The same for a Hermite decomposition { u, h } and the inverse ui of u. *)
hermiteDecompositionFailure[ a_, { u_, ui_, h_ } ] :=
  Module[ { m, n },
    { m, n } =
      Dimensions[ a ];

    Quiet @ Check[
      Which[
        !integerSparseMatrixQ[ u, { m, m } ],
        "u is not an integer matrix of the right dimensions",
        !integerSparseMatrixQ[ ui, { m, m } ],
        "the inverse of u is not an integer matrix of the right dimensions",
        !integerSparseMatrixQ[ h, { m, n } ],
        "h is not an integer matrix of the right dimensions",
        !hermiteNormalFormQ[ h ],
        "h is not in Hermite normal form",
        !zeroMatrixQ[ u . ui - IdentityMatrix[ m, SparseArray ] ],
        "u times its inverse is not the identity matrix",
        !zeroMatrixQ[ u . a - h ],
        "u.mat is not equal to h",
        True,
        None
      ],
      "a message was generated during the verification"
    ]
  ];

(* Is x a SparseArray with the given dimensions, background 0 and integer entries? *)
integerSparseMatrixQ[ x_, dims_ ] :=
  TrueQ[
    Head[ x ] === SparseArray &&
    Dimensions[ x ] === dims &&
    x["Background"] === 0 &&
    VectorQ[ x["NonzeroValues"], IntegerQ ]
  ];

(* Are all entries of the integer matrix x zero? Arithmetic with SparseArrays
   can leave zeros among the stored entries, so the stored entries are
   inspected instead of counted. *)
zeroMatrixQ[ x_SparseArray ] :=
  TrueQ[ x["Background"] === 0 && allZeroQ[ x["NonzeroValues"] ] ];

zeroMatrixQ[ x_List ] :=
  allZeroQ[ Flatten[ x ] ];

zeroMatrixQ[ _ ] :=
  False;

allZeroQ[ { } ] :=
  True;

allZeroQ[ values_List ] :=
  TrueQ[ Min[ values ] == 0 && Max[ values ] == 0 ];

(* Is the integer sparse matrix d in Smith normal form: no nonzero entries
   outside the diagonal, the nonzero diagonal entries first, all of them
   positive, and each of them dividing the next? *)
smithNormalFormQ[ d_ ] :=
  Module[ { diagonal, r, first },
    diagonal =
      Normal[ Diagonal[ d ] ];

    r =
      Total[ Unitize[ diagonal ] ];

    first =
      Take[ diagonal, r ];

    TrueQ[
      (* d has r nonzero entries on its diagonal and r nonzero entries in all *)
      Total[ Unitize[ d["NonzeroValues"] ] ] == r &&
      (* these are the first r diagonal entries, and they are positive *)
      ( r == 0 || Min[ first ] > 0 ) &&
      ( r < 2 || ( And @@ Divisible[ Rest[ first ], Most[ first ] ] ) )
    ]
  ];

(* Is the integer sparse matrix h in row Hermite normal form: the nonzero rows
   first, the first nonzero entry of each of them (its pivot) positive and to
   the right of the pivot of the row above it, and the entries above a pivot p
   in the range 0,...,p-1? *)
hermiteNormalFormQ[ h_ ] :=
  Module[
    { n, positions, values, keep, order, rows, columns, length, starts, r,
      pivotColumns, pivots, bound, above },
    n =
      Last[ Dimensions[ h ] ];

    positions =
      h["NonzeroPositions"];

    values =
      h["NonzeroValues"];

    (* Leave out zeros that are stored *)
    keep =
      Pick[ Range[ Length[ values ] ], Unitize[ values ], 1 ];

    If[
      keep === { }
      ,
      True
      ,
      positions =
        positions[[ keep ]];

      values =
        values[[ keep ]];

      (* Sort the entries by row and then by column *)
      order =
        Ordering[ positions ];

      rows =
        positions[[ order, 1 ]];

      columns =
        positions[[ order, 2 ]];

      values =
        values[[ order ]];

      length =
        Length[ rows ];

      (* The places in these lists of the first entries of the nonzero rows *)
      starts =
        Pick[ Range[ length ], Unitize[ Differences[ Prepend[ rows, 0 ] ] ], 1 ];

      r =
        Length[ starts ];

      pivotColumns =
        columns[[ starts ]];

      pivots =
        values[[ starts ]];

      (* bound[[ k ]] is the pivot in the column of entry k, and 0 if that
         column has no pivot or if entry k is a pivot itself *)
      bound =
        ConstantArray[ 0, n ];

      bound[[ pivotColumns ]] =
        pivots;

      bound =
        bound[[ columns ]];

      bound[[ starts ]] =
        0;

      above =
        Pick[ Range[ length ], Unitize[ bound ], 1 ];

      TrueQ[
        (* the nonzero rows are the first r rows *)
        rows[[ starts ]] === Range[ r ] &&
        (* each pivot lies to the right of the one above it; then there
           are no nonzero entries below a pivot *)
        ( r < 2 || Min[ Differences[ pivotColumns ] ] > 0 ) &&
        Min[ pivots ] > 0 &&
        (
          above === { } ||
          (
            Min[ values[[ above ]] ] > 0 &&
            Min[ bound[[ above ]] - values[[ above ]] ] > 0
          )
        )
      ]
    ]
  ];

(* The built-in functions need a dense matrix. *)
builtInSmithDecomposition[ sa_ ] :=
  With[ { result = SmithDecomposition[ Normal[ sa ] ] },
    If[
      MatchQ[ result, { _?MatrixQ, _?MatrixQ, _?MatrixQ } ],
      SparseArray /@ result,
      result
    ]
  ];

builtInHermiteDecomposition[ sa_ ] :=
  With[ { result = HermiteDecomposition[ Normal[ sa ] ] },
    If[
      MatchQ[ result, { _?MatrixQ, _?MatrixQ } ],
      SparseArray /@ result,
      result
    ]
  ];


(*
+---------------------------------------------------------------------------+
|                            Communication with Java                        |
+---------------------------------------------------------------------------+
*)

(* Version of the Java class that this file needs. It is the constant VERSION
   in SparseNormalForms.java. *)
$javaClassVersion = 2;

(* Directory of this file, recorded while the package is being loaded *)
$sparseNormalFormsDirectory =
  DirectoryName[ $InputFileName ];

(* The jar is expected next to this file. As a second and a third guess, look
   in the directories of the paclet where it could be. *)
$SparseNormalFormsJar :=
  With[
    {
      candidates =
        Join[
          { FileNameJoin[ { $sparseNormalFormsDirectory, "SparseNormalForms.jar" } ] },
          Quiet @ Check[
            With[ { root = PacletObject["Anyonica"]["Location"] },
              {
                FileNameJoin[ { root, "Kernel", "ReductionTools", "SparseNormalForms.jar" } ],
                FileNameJoin[ { root, "Kernel", "SparseNormalForms.jar" } ]
              }
            ],
            { }
          ]
        ]
    },
    SelectFirst[ candidates, StringQ[ # ] && FileExistsQ[ # ]&, First @ candidates ]
  ];

(* Returns mat as a SparseArray with integer entries and background 0,
   or $Failed if mat is not a non-empty integer matrix. *)
toSparseIntegerMatrix[ mat_ ] :=
  Module[ { sa },
    sa =
      Quiet @ Check[ SparseArray[ mat ], $Failed ];

    Which[
      Head[ sa ] =!= SparseArray || ArrayDepth[ sa ] =!= 2,
      $Failed,
      Min[ Dimensions[ sa ] ] < 1,
      $Failed,
      sa["Background"] =!= 0,
      (* With a nonzero background almost all entries are nonzero, so the
         matrix is not sparse and nothing is lost by writing it out. *)
      If[
        IntegerQ[ sa["Background"] ],
        toSparseIntegerMatrix[ Normal[ sa ] ],
        $Failed
      ],
      !VectorQ[ sa["NonzeroValues"], IntegerQ ],
      $Failed,
      True,
      sa
    ]
  ];

(* The result of Java for the integer sparse matrix sa, or $Failed after a
   message of the function head that says what went wrong. *)
sparseDecomposition[ head_, type_, sa_ ] :=
  Module[ { m, n, result },
    { m, n } =
      Dimensions[ sa ];

    If[
      Total[ Abs[ sa["NonzeroValues"] ] ] == 0
      ,
      (* The zero matrix is its own normal form *)
      If[
        type === "Smith",
        {
          IdentityMatrix[ m, SparseArray ], IdentityMatrix[ m, SparseArray ],
          SparseArray[ { }, { m, n } ],
          IdentityMatrix[ n, SparseArray ], IdentityMatrix[ n, SparseArray ]
        },
        {
          IdentityMatrix[ m, SparseArray ], IdentityMatrix[ m, SparseArray ],
          SparseArray[ { }, { m, n } ]
        }
      ]
      ,
      result =
        javaDecomposition[ type, sa ];

      Which[
        ListQ[ result ]
        ,
        result
        ,
        result === "NoJava"
        ,
        Message[ MessageName[ head, "nojava" ], $SparseNormalFormsJar ];
        $Failed
        ,
        result === "OldJar"
        ,
        Message[ MessageName[ head, "oldjar" ], $SparseNormalFormsJar ];
        $Failed
        ,
        True
        ,
        Message[ MessageName[ head, "failed" ] ];
        $Failed
      ]
    ]
  ];

(* Returns { u, ui, d, v, vi } for type "Smith" and { u, ui, h } for type
   "Hermite", "NoJava" if the Java class cannot be loaded, "OldJar" if Java
   has an older version of the class, or $Failed. *)
javaDecomposition[ type_, sa_ ] :=
  Block[ { $ContextPath = $ContextPath },
    (* Needs puts JLink` on the context path; Block undoes that afterwards *)
    Quiet[
      Needs["JLink`"];
      JLink`InstallJava[];
      (* The class path of J/Link is reset by ReinstallJava, so add the jar every time *)
      With[ { jar = $SparseNormalFormsJar }, JLink`AddToClassPath[ jar ] ]
    ];

    JLink`JavaBlock[
      Catch[
        Module[ { snf, m, n, names, dims, matrices },
          { m, n } =
            Dimensions[ sa ];

          (* Java counts rows and columns with 32 bit integers *)
          If[ Max[ m, n ] >= 2^31 - 16, Throw[ $Failed, $javaTag ] ];

          snf =
            Quiet @ JLink`JavaNew["anyonica.SparseNormalForms"];

          If[ !TrueQ[ JLink`JavaObjectQ[ snf ] ], Throw[ "NoJava", $javaTag ] ];

          (* The first version of the class has no method getVersion *)
          If[
            !TrueQ[ Quiet[ snf @ getVersion[] ] >= $javaClassVersion ],
            Throw[ "OldJar", $javaTag ]
          ];

          (* J/Link returns Null for a method without a result that ended
             normally, and it issues a message if Java threw an exception. *)
          If[
            Check[ sendMatrix[ snf, sa ], $Failed ] =!= Null,
            Throw[ $Failed, $javaTag ]
          ];

          If[
            Check[ If[ type === "Smith", snf @ smith[], snf @ hermite[] ], $Failed ] =!= Null,
            Throw[ $Failed, $javaTag ]
          ];

          (* If the evaluation is aborted while Java is at work, the Java method
             returns early and J/Link aborts the evaluation. Should the
             evaluation go on all the same, there is no result: abort here. *)
          If[ TrueQ[ snf @ wasAborted[] ], Abort[] ];

          { names, dims } =
            If[
              type === "Smith",
              {
                { "U", "UI", "D", "V", "VI" },
                { { m, m }, { m, m }, { m, n }, { n, n }, { n, n } }
              },
              {
                { "U", "UI", "H" },
                { { m, m }, { m, m }, { m, n } }
              }
            ];

          matrices =
            MapThread[ readMatrix[ snf, #1, #2 ]&, { names, dims } ];

          snf @ clear[];

          If[ MemberQ[ matrices, $Failed ], $Failed, matrices ]
        ],
        $javaTag
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

(* The matrix that Java calls which ("U", "UI", "D", "V", "VI" or "H"), as a
   SparseArray with dimensions dims, or $Failed. *)
readMatrix[ snf_, which_, dims_ ] :=
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
      ri === { },
      SparseArray[ { }, dims ],
      True,
      Check[ SparseArray[ Transpose[ { ri, ci } ] -> vi, dims ], $Failed ]
    ]
  ];
