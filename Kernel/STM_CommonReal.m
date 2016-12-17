(* :Author: Konstantin K.Konstantinov *)
(* :Summary: Reals for STM. *)
(* :Copyright: K^3, 2013 - 2014 *)
(* :Version:Revision: 1.24.001,Date: 2014/03/02 *)
(* :Mathematica Version: 7.0 - 9.0 *)
(* ============================================== *)

(* ============================================== *)
(* Function to create variables and various helpers. Creates GLOBAL variables as follows:  *)
(* S - square matrix of variables. *)
(* A - square matrix of antisymmetric variables. *)

(* SabStr - square string matrix of variables for functions. *)
(* SL - linearized symmetric variables. *)
(* AL - linearized antisymmetric variables. *)
(* SLV - linearized string variables for function arguments. *)
(* SLVM - mapping from sab string matrix to linear string for functions. *)
(* remapTable & remapReverseTable to go from SM to SL and back. *)
(* EE, EI, EIT square matrix of 1, column of 1 and row of 1. *)
(* ZZ - zero matrix, ZL - zero vector (for convenience). *)
(* II - Identity matrix. *)
(* *)
(* EEIJ *)
(* IIIJ*)
(* alphaIJ *)
(* alphaIJpm1 *)
(* eprimIJidentity *)
(* YA - symbolic symmetric matrix of Y with diagonal elements as variables *)
(* YD - just diagonal elements of matrix Y as variables *)
(* YL - linear symbolic representation of Y (contains NA elements). *)

CreateVariables[nn_?IntegerQ,rawOpts___]:=Module[{MaxIdxLen,slvmHlp,na,opts,nal,ss},
opts=ProcessOptions[rawOpts];

MaxIdxLen=StringLength[ToString[nn]];
na=nn*(nn-1)/2;
nal=na;

(* Square matrix of variables *)
Print["CreateVariables::Creating real S, ST, and SabStr."];
S=Table[If[ii==jj,0,If[ii<jj,ToExpression["Sr"<>ToStringPadded[ii,MaxIdxLen]<>"c"<>ToStringPadded[jj,MaxIdxLen]],ToExpression["Sr"<>ToStringPadded[jj,MaxIdxLen]<>"c"<>ToStringPadded[ii,MaxIdxLen]]]],{ii,1,nn},{jj,1,nn}];

Print["SL - Linearized variables. "];
SL=ToSLinear[S];
];

(* ==============================================*)
