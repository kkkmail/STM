(* :Author:Konstantin K.Konstantinov *)
(* :Summary:4-th power polynom terms and related. *)
(* :Copyright:K^3,2013 *)
(* :Version: Revision: 1.06.001, Date:2013/10/07 *)
(* :Mathematica Version:7.0-9.0 *)
(* ==============================================*)



(* Ok. We just know that we will have 23 terms.*)
NoOfTermsF4=23;
CoeffF4Zero={Table[0,{ii,1,NoOfTermsF4}]};
CoeffF4=CoeffF4Zero;

(* All terms for convenience. *)

(*
idxD4T8Cabcdefgh;
idxD4T6Cabbdefgh;
idxD4T5Cabbdbfgh;
idxD4T4Cabbdbfbh;
idxD4T4Cababefgh;
idxD4T4Cabbddfgh;
idxD4T4Cabbdeffh;
idxD4T3Cababbfgh;
idxD4T3Cabbddfdh;
idxD4T2Cababbfbh;
idxD4T2Cababeffh;
idxD4T2Cabbddffh;
idxD4T2Cabbdbddh;
idxD4T2Cabababgh;
idxD4T2Cabbdadgh;
idxD4T1Cababbffh;
idxD4T1Cabbdaddh;
idxD4T1Cabababbh;
idxD4T0Cababbfbf;
idxD4T0Cababefef;
idxD4T0Cabbddfaf;
idxD4T0Cabbdbdad;
idxD4T0Cabababab;
*)

(* Creates terms for 4th power polynom. *)
InitializeTermsF4[]:=Module[{},
Print["Constructing 4-th power terms."];

Print["D = 4, 8 terms."];
Print["D4T8Cabcdefgh"];
idxD4T8Cabcdefgh=NextTermNo[];
AddTerm[idxD4T8Cabcdefgh,"coeffD4T8Cabcdefgh",S[[1,2]]*S[[3,4]]*S[[5,6]]*S[[7,8]]];
Print[strSeparatorSmall];

Print["D = 4, 6 terms."];
Print["D4T6Cabbdefgh"];
idxD4T6Cabbdefgh=NextTermNo[];
AddTerm[idxD4T6Cabbdefgh,"coeffD4T6Cabbdefgh",S[[1,2]]*S[[2,4]]*S[[5,6]]*S[[7,8]]];
Print[strSeparatorSmall];

Print["D = 4, 5 terms."];
Print["D4T5Cabbdbfgh"];
idxD4T5Cabbdbfgh=NextTermNo[];
AddTerm[idxD4T5Cabbdbfgh,"coeffD4T5Cabbdbfgh",S[[1,2]]*S[[2,4]]*S[[2,6]]*S[[7,8]]];
Print[strSeparatorSmall];

Print["D = 4, 4 terms."];
Print["D4T4Cabbdbfbh"];
idxD4T4Cabbdbfbh=NextTermNo[];
AddTerm[idxD4T4Cabbdbfbh,"coeffD4T4Cabbdbfbh",S[[1,2]]*S[[2,4]]*S[[2,6]]*S[[2,8]]];

Print["D4T4Cababefgh"];
idxD4T4Cababefgh=NextTermNo[];
AddTerm[idxD4T4Cababefgh,"coeffD4T4Cababefgh",S[[1,2]]*S[[1,2]]*S[[5,6]]*S[[7,8]]];

Print["D4T4Cabbddfgh"];
idxD4T4Cabbddfgh=NextTermNo[];
AddTerm[idxD4T4Cabbddfgh,"coeffD4T4Cabbddfgh",S[[1,2]]*S[[2,4]]*S[[4,6]]*S[[7,8]]];

Print["D4T4Cabbdeffh"];
idxD4T4Cabbdeffh=NextTermNo[];
AddTerm[idxD4T4Cabbdeffh,"coeffD4T4Cabbdeffh",S[[1,2]]*S[[2,4]]*S[[5,6]]*S[[6,8]]];
Print[strSeparatorSmall];

Print["D = 4, 3 terms."];
Print["D4T3Cababbfgh"];
idxD4T3Cababbfgh=NextTermNo[];
AddTerm[idxD4T3Cababbfgh,"coeffD4T3Cababbfgh",S[[1,2]]*S[[1,2]]*S[[2,6]]*S[[7,8]]];

Print["D4T3Cabbddfdh"];
idxD4T3Cabbddfdh=NextTermNo[];
AddTerm[idxD4T3Cabbddfdh,"coeffD4T3Cabbddfdh",S[[1,2]]*S[[2,4]]*S[[4,6]]*S[[4,8]]];
Print[strSeparatorSmall];

Print["D = 4, 2 terms."];
Print["D4T2Cababbfbh"];
idxD4T2Cababbfbh=NextTermNo[];
AddTerm[idxD4T2Cababbfbh,"coeffD4T2Cababbfbh",S[[1,2]]*S[[1,2]]*S[[2,6]]*S[[2,8]]];

Print["D4T2Cababeffh"];
idxD4T2Cababeffh=NextTermNo[];
AddTerm[idxD4T2Cababeffh,"coeffD4T2Cababeffh",S[[1,2]]*S[[1,2]]*S[[5,6]]*S[[6,8]]];

Print["D4T2Cabbddffh"];
idxD4T2Cabbddffh=NextTermNo[];
AddTerm[idxD4T2Cabbddffh,"coeffD4T2Cabbddffh",S[[1,2]]*S[[2,4]]*S[[4,6]]*S[[6,8]]];

Print["D4T2Cabbdbddh"];
idxD4T2Cabbdbddh=NextTermNo[];
AddTerm[idxD4T2Cabbdbddh,"coeffD4T2Cabbdbddh",S[[1,2]]*S[[2,4]]*S[[2,4]]*S[[4,8]]];

Print["D4T2Cabababgh"];
idxD4T2Cabababgh=NextTermNo[];
AddTerm[idxD4T2Cabababgh,"coeffD4T2Cabababgh",S[[1,2]]*S[[1,2]]*S[[1,2]]*S[[7,8]]];

Print["D4T2Cabbdadgh"];
idxD4T2Cabbdadgh=NextTermNo[];
AddTerm[idxD4T2Cabbdadgh,"coeffD4T2Cabbdadgh",S[[1,2]]*S[[2,4]]*S[[1,4]]*S[[7,8]]];
Print[strSeparatorSmall];

Print["D = 4, 1 terms."];
Print["D4T1Cababbffh"];
idxD4T1Cababbffh=NextTermNo[];
AddTerm[idxD4T1Cababbffh,"coeffD4T1Cababbffh",S[[1,2]]*S[[1,2]]*S[[2,6]]*S[[6,8]]];

Print["D4T1Cabbdaddh"];
idxD4T1Cabbdaddh=NextTermNo[];
AddTerm[idxD4T1Cabbdaddh,"coeffD4T1Cabbdaddh",S[[1,2]]*S[[2,4]]*S[[1,4]]*S[[4,8]]];

Print["D4T1Cabababbh"];
idxD4T1Cabababbh=NextTermNo[];
AddTerm[idxD4T1Cabababbh,"coeffD4T1Cabababbh",S[[1,2]]*S[[1,2]]*S[[1,2]]*S[[2,8]]];
Print[strSeparatorSmall];

Print["D = 4, 0 terms."];
Print["D4T0Cababbfbf"];
idxD4T0Cababbfbf=NextTermNo[];
AddTerm[idxD4T0Cababbfbf,"coeffD4T0Cababbfbf",S[[1,2]]*S[[1,2]]*S[[2,6]]*S[[2,6]]];

Print["D4T0Cababefef"];
idxD4T0Cababefef=NextTermNo[];
AddTerm[idxD4T0Cababefef,"coeffD4T0Cababefef",S[[1,2]]*S[[1,2]]*S[[5,6]]*S[[5,6]]];

Print["D4T0Cabbddfaf"];
idxD4T0Cabbddfaf=NextTermNo[];
AddTerm[idxD4T0Cabbddfaf,"coeffD4T0Cabbddfaf",S[[1,2]]*S[[2,4]]*S[[4,6]]*S[[1,6]]];

Print["D4T0Cabbdbdad"];
idxD4T0Cabbdbdad=NextTermNo[];
AddTerm[idxD4T0Cabbdbdad,"coeffD4T0Cabbdbdad",S[[1,2]]*S[[2,4]]*S[[2,4]]*S[[1,4]]];

Print["D4T0Cabababab"];
idxD4T0Cabababab=NextTermNo[];
AddTerm[idxD4T0Cabababab,"coeffD4T0Cabababab",S[[1,2]]*S[[1,2]]*S[[1,2]]*S[[1,2]]];

Print[strSeparatorSmall];

Print["Total number of terms NoTermCnt = ",NoTermCnt];
Print[strSeparator];
PrintTimeUsed[];
];
