Package["Anyonica`"]

PackageScope["OptimizedImport"]

OptimizedImport::usage =
"OptimizedImport[ fileName, importDirectory ] checks whether a file named filename, with extension .mx exists and if so imports it. If it doesn't exist it checks whether a file named filename with extension .wdx exists and if so, imports it and exports it as a .mx file for future use.";

OptimizedImport[ fileName_String, importDirectory_String ] :=
	Module[{ data, fullName },
		fullName =
			FileNameJoin[ { importDirectory, fileName <> ".mx" } ];
		If[(* Have MX file *)
			MemberQ[  FileNames[ All, importDirectory ] , fullName ],
			(* THEN: Import  MX file *)
			Import[ fullName, "MX" ],
			(* ELSE: Create MX file from WDX file *)
			PrintTemporary["Import not yet optimized for this machine. Optimizing for future use..."];
			PrintTemporary[ ProgressIndicator[Appearance -> "Necklace"] ];
			data = Uncompress @ Import[ FileNameJoin[{ importDirectory, fileName<>".wdx" }], "WDX" ];
			Export[ fullName, data, "MX" ];
			PrintTemporary["Import optimized for future use. Importing data..."];
			data
		]
	];

(* Import the FusionRingList *)
currentDirectory =
	Directory[];

importDirectory =
	Quiet[
		Check[ SetDirectory @ DirectoryName @ $InputFileName,    (* If not using notebook interface *)
		SetDirectory @ NotebookDirectory[]],SetDirectory::fstr   (* If using notebook interface *)
	];


PackageExport["FusionRingByCode"]

FusionRingByCode::usage =
	"FusionRingByCode[four-tuple] returns the fusion ring with formal code equal to four-tuple.";


PackageExport["FRBC"]

FRBC::usage =
	"Shorthand for FusionRingByCode.";

FusionRingByCode =
	FRBC =
		OptimizedImport[ "FusionRingAssociation", importDirectory ];


PackageExport["FusionRingList"]

FusionRingList::usage =
	"FusionRingList is a list of all saved FusionRing objects.";


PackageExport["FRL"]

FRL::usage =
	"Shorthand for FusionRingList.";

FRL =
	FusionRingList =
		OptimizedImport[ "FusionRingList", importDirectory ];
	
SetDirectory @
	currentDirectory;


PackageExport["AllFusionRingsQ"]

AllFusionRingsQ::usage =
	"AllFusionRingsQ[ r, m ] returns True if FusionRingList contains all fusion rings of rank r and multiplicity m, and False otherwise.";

AllFusionRingsQ[ r_Integer, m_Integer ] :=
	Or[
		r == 1,
		MemberQ[r] @ { 2, 3, 4 } && m <= 16,
		r == 5 && m <= 12,
		r == 6 && m <= 4,
		r == 7 && m <= 2,
		r == 8 && m == 1,
		r == 9 && m == 1
	];


PackageExport["AFRQ"]

AFRQ::usage =
"Shorthand for AllFusionRingsQ.";

AFRQ[ r_, m_ ] :=
	AllFusionRingsQ[ r, m ];

(* Formatting *)

Format[ ring:FusionRing[r_Association], StandardForm ] :=
	If[ r["Names"] === {},
		If[
			r["FormalParameters"] =!= Missing[],
			"FR"[ Sequence @@ r["FormalParameters"] ],
			"FR"[ Rank[ring], Multiplicity[ring], NNSD[ring], "_" ]
		],
		"FR"[ r["Names"] // First ]
	];

Format[ FusionElement[ a_, el_Integer], StandardForm ] :=
	Format[ FusionElement[ a, el], StandardForm ] =
		If[ 1 <= el <= Rank[a],
			a["ElementNames"][[el]],
			Message[ FusionRing::eloutofbounds ]
		];

Format[ FusionElement[ a_, el_String], StandardForm ] :=
	Format[ FusionElement[ a, el], StandardForm ] =
		If[ MemberQ[a["ElementNames"], el ],
			el,
			Message[ FusionRing::elnotfound, el ]
		];

Format[ FusionElement[ a_, el_Symbol], StandardForm ] :=
	Format[ FusionElement[ a, el], StandardForm ] =
		If[ MemberQ[a["ElementNames"], ToString[el] ],
			ToString[el],
			Message[ FusionRing::elnotfound, el ]
		];
