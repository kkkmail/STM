(* :Author: Konstantin K.Konstantinov *)
(* :Summary: Re Im for STM. *)
(* :Copyright: K^3, 2013 - 2014 *)
(* :Version:Revision: 2.00.001,Date: 2014/03/05 *)
(* :Mathematica Version: 7.0 - 9.0 *)
(* ============================================== *)

(* ============================================== *)
(* Function to create variables and various helpers. Creates GLOBAL variables as follows:  *)
(* S - square matrix of variables. *)
(* A - square matrix of antisymmetric variables. *)

(* SabStr - square string matrix of variables for functions. *)
(* SL - linearized symmetric variables. *)
(* AL - linearized antisymmetric variables. *)
(* WL = Join[SL, AL] *)
(* SLV - linearized string variables for function arguments. *)
(* SLVM - mapping from sab string matrix to linear string for functions. *)

CreateVariables[nn_?IntegerQ,rawOpts___]:=Module[{MaxIdxLen,slvmHlp,na,opts,useAlternatingASVal,nal,ss},
Print["CreateVariables::Starting..."];
opts=ProcessOptions[rawOpts];

useAlternatingASVal=UseAlternatingAS/.opts /.Options[STMCommon];
UseAlternatingASValue=useAlternatingASVal;

MaxIdxLen=StringLength[ToString[nn]];
na=nn*(nn-1)/2;
nal=2*na;

(* Square matrix of variables *)
Print["CreateVariables::Creating real S, A and related..."];

S=Table[If[ii==jj,0,If[ii<jj,ToExpression["Sr"<>ToStringPadded[ii,MaxIdxLen]<>"c"<>ToStringPadded[jj,MaxIdxLen]],ToExpression["Sr"<>ToStringPadded[jj,MaxIdxLen]<>"c"<>ToStringPadded[ii,MaxIdxLen]]]],{ii,1,nn},{jj,1,nn}];

Print["SL - Linearized variables. "];
SL=ToSLinear[S];

Print["Ap - preliminary matrix of antysimmetric variables (without accounting for the sign swap for pairs)."];
Ap=Table[If[ii==jj,0,If[ii<jj,ToExpression["Ar"<>ToStringPadded[ii,MaxIdxLen]<>"c"<>ToStringPadded[jj,MaxIdxLen]],-ToExpression["Ar"<>ToStringPadded[jj,MaxIdxLen]<>"c"<>ToStringPadded[ii,MaxIdxLen]]]],{ii,1,nn},{jj,1,nn}];

Print["AL - Linearized antisymmetric variables. "];
AL=Flatten[Table[Table[Ap[[ii,jj]],{jj,ii+1,nn}],{ii,1,nn-1}]];

Print["A - matrix of all antisymmetric variables."];
A=ToAMatrix[AL];

Print["Ap = ", Ap // MatrixForm];
Print["AL = ", AL];
Print["A = ", A // MatrixForm];

Print["Check ToALinear..."];
Print["(AL - ToALinear[A]) = ", (AL - ToALinear[A])];

Print["Check ToAMatrix..."];
Print["(A - ToAMatrix[AL]) = ", (A - ToAMatrix[AL]) // MatrixForm];

Print["Creating WL"];
WL=ToLinear[S,A];
Print["WL = ", WL];

Print["CreateVariables::Completed."];
Print[strSeparator];
];

(* ==============================================*)
