(* :Author:Konstantin K.Konstantinov *)
(* :Summary:2-nd power polynom terms and related. *)
(* :Copyright:K^3,2013 *)
(* :Version: Revision: 1.06.001, Date:2013/10/07 *)
(* :Mathematica Version:7.0-9.0 *)
(* ==============================================*)


NoOfTermsF2=3;
CoeffF2Zero={Table[0,{ii,1,NoOfTermsF2}]};
CoeffF2=CoeffF2Zero;

TermMatrixF2=Table[0,{ii,1,NoOfTermsF2}];
TermExampleMatrixF2=Table[0,{ii,1,NoOfTermsF2}];

(* All terms for convenience. *)
(*
idxD2T4Cabcd;
idxD2T2Cabbd;
idxD2T0Cabab;
*)

(* Creates terms for 2-nd power polynom. *)
InitializeTermsF2[]:=Module[{},
Print["Constructing 2-nd power terms."];

Print["D = 2, 4 terms."];
Print["D2T4Cabcd"];
idxD2T4Cabcd=1;
TermMatrixF2[[idxD2T4Cabcd]]="coeffD2T4Cabcd";
TermExampleMatrixF2[[idxD2T4Cabcd]]=S[[1,2]]*S[[3,4]];
Print[strSeparatorSmall];

Print["D = 2, 2 terms."];
Print["D2T2Cabbd"];
idxD2T2Cabbd=2;
TermMatrixF2[[idxD2T2Cabbd]]="coeffD2T2Cabbd";
TermExampleMatrixF2[[idxD2T2Cabbd]]=S[[1,2]]*S[[2,4]];
Print[strSeparatorSmall];

Print["D = 2, 0 terms."];
Print["D2T0Cabab"];
idxD2T0Cabab=3;
TermMatrixF2[[idxD2T0Cabab]]="coeffD2T0Cabab";
TermExampleMatrixF2[[idxD2T0Cabab]]=S[[1,2]]*S[[1,2]];
Print[strSeparatorSmall];
];




