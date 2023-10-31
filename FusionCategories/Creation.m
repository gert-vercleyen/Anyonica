(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gert *)
(* :Date: 2023-10-20 *)

(* Fusion Categories with A_k+1 rules == SU(2)_k rules.
   All of these come from quantum group representations and
   have been classified. See book Quantum Groups, Quantum Categories and
   Quantum Field Theory from Jürg Fröhlich , Thomas Kerler.
   
   The specific formula we use for the F symbols comes from
   Non-Abelian Anyons and Interferometry, Thesis by Parsa Hassan Bonderson.
   
   The formula we use for the R matrices comes from A Field Guide to Categories
   with A_n Fusion Rules from Cain Edie-Michell and Scott Morrison
*)

(*
  Find F symbols for an SU(2)_k fusion ring with
  q  = Exp[ 2 Pi I l / ( k + 2 ) ]
*)

SU2klFSymbols[ k_Integer, l_Integer ] :=
  Module[ { q, qnr, qf, qbin, d, ring, sixj, sixjLabels, fs },
    ring = FusionRingSU2k[ k ];
    q    = Exp[ 2 Pi I l/( k + 2 ) ];
    (* We use memoization to increase the speed *)
    qnr[x_] :=
      qnr[x] = (q^(x/2) - q^(-x/2))/(q^(1/2) - q^(-1/2));
    qf[n_] :=
      qf[n] = Product[qnr[m], {m, n}];
    qf[0] = 1;
    
    d[a_, b_, c_] :=
      Sqrt[ (qf[-a + b + c] qf[a - b + c] qf[a + b - c])/ qf[a + b + c + 1] ];

    fs          = FSymbols @ ring;
    sixjLabels  = (List @@@ fs) /. i_Integer :> (i - 1)/2;

    sixj[j1_, j2_, j3_, j_, j12_, j23_] :=
      (
      If[
        Not @ And[
          Abs[j1 - j2] <= j12 <= Min[j1 + j2, k - j1 - j2],
          IntegerQ[ j1 + j2 + j12 ],
          Abs[j12 - j3] <= j <= Min[j12 + j3, k - j12 - j3],
          IntegerQ[ j + j3 + j12 ],
          Abs[j2 - j3] <= j23 <= Min[j2 + j3, k - j2 - j3],
          IntegerQ[ j2 + j3 + j23 ],
          Abs[j1 - j23] <= j <= Min[j1 + j23, k - j1 - j23],
          IntegerQ[ j1 + j + j23 ]
        ]
        ,
        Return @ 0
      ];

      With[{
        zmin = Max[j1 + j2 + j12, j1 + j + j23, j + j3 + j12, j3 + j2 + j23],
        zmax = Min[j1 + j2 + j3 + j, j1 + j3 + j12 + j23, j2 + j + j12 + j23]
        },

        (-1)^(j1 + j2 + j3 + j) *
        Sqrt[ qnr[2 j12 + 1] * qnr[2 j23 + 1] ] *
        d[j1, j2, j12]*d[j12, j3, j] * d[j2, j3,j23] * d[j1, j23, j] *
        Sum[
          ((-1)^z qf[z + 1])/(
          qf[z - j1 - j2 - j12] * qf[z - j1 - j - j23] * qf[z - j - j3 - j12] *
          qf[z - j3 - j2 - j23] * qf[j1 + j2 + j3 + j - z] * qf[j1 + j3 + j12 + j23 - z] *
          qf[j2 + j + j12 + j23 - z])
          ,
          { z, zmin, zmax }
        ]
      ]
      );
    
    Thread[ fs -> (sixj @@@ sixjLabels)]
  ];

PackageExport["SU2kFSymbols"]

(* Find all F-symbols for a SU(2)_k fusion ring *)
SU2kFSymbols[k_] :=
  With[{ lValues = Cases[ Range[ (k + 2) ], l_ /; GCD[l, k + 2] == 1 ] },
    SU2klFSymbols[k, #] & /@ lValues
  ];

