(* :Author:Konstantin K.Konstantinov *)
(* :Summary:SOME 6th power polynom terms and related. *)
(* :Copyright:K^3, 2013 *)
(* :Version: Revision: 1.03.001, Date:2013/10/04 *)
(* :Mathematica Version:7.0-9.0 *)
(* ==============================================*)

(* Ok. The actual number of terms for Poly 6 is 200. *)
(* Until we can automate their creation we can implement only the ones we need the most. *)
NoOfTermsF6=1;
CoeffF6Zero={Table[0,{ii,1,NoOfTermsF6}]};
CoeffF6=CoeffF6Zero;

(* Creates SOME terms for 6th power polynom. *)
InitializeTermsF6[]:=Module[{},
Print["Constructing SOME 6-th power terms."];

Print["D6T0Cabababababab"];
idxD6T0Cabababababab=NextTermNoF6[];
AddTermF6[idxD6T0Cabababababab,"coeffD6T0Cabababababab",S[[1,2]]*S[[1,2]]*S[[1,2]]*S[[1,2]]*S[[1,2]]*S[[1,2]]];
 
Print[strSeparatorSmall];
];
