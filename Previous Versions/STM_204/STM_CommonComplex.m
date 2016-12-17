(* :Author: Konstantin K.Konstantinov *)
(* :Summary: Complex for STM. *)
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

CreateVariables[nn_?IntegerQ,rawOpts___]:=Module[{MaxIdxLen,slvmHlp,na,opts,calculateEVPrimDataVal,useEVPrimNumericVal,useHighPrecisionVal,highPrecisionValueVal,useAlternatingASVal,nal,ss},

Print["TODO::CreateVariables::Complex is not yet implemeneted!!!"];
Return[];


opts=ProcessOptions[rawOpts];

calculateEVPrimDataVal=CalculateEVPrimData /.opts /.Options[STMCommon];
useEVPrimNumericVal=UseEVPrimNumeric/.opts /.Options[STMCommon];
useHighPrecisionVal=UseHighPrecision /.opts /.Options[STMCommon];
highPrecisionValueVal=HighPrecisionValue/.opts /.Options[STMCommon];
useAlternatingASVal=UseAlternatingAS/.opts /.Options[STMCommon];

UseAlternatingASValue=useAlternatingASVal;

MaxIdxLen=StringLength[ToString[nn]];
na=nn*(nn-1)/2;
nal=na;

(* Square matrix of variables *)
Print["S - matrix of all variables."];

ss=Table[If[ii==jj,0,If[ii<jj,ToExpression["Sr"<>ToStringPadded[ii,MaxIdxLen]<>"c"<>ToStringPadded[jj,MaxIdxLen]],ToExpression["Sr"<>ToStringPadded[jj,MaxIdxLen]<>"c"<>ToStringPadded[ii,MaxIdxLen]]]],{ii,1,nn},{jj,1,nn}];

If[!UseComplexVariablesValue,
(
Print["Creating real S, ST, and SabStr"];
S=ss;
WS=S;
WA=Table[0,{ii,1,nn},{jj,1,nn}];

Print["ST - matrix of all variables as functions of t."];
ST=Table[If[ii==jj,0,If[ii<jj,ToExpression["Sr"<>ToStringPadded[ii,MaxIdxLen]<>"c"<>ToStringPadded[jj,MaxIdxLen]][t],ToExpression["Sr"<>ToStringPadded[jj,MaxIdxLen]<>"c"<>ToStringPadded[ii,MaxIdxLen]][t]]],{ii,1,nn},{jj,1,nn}];

Print["SabStr - matrix of string variables."];
SabStr=Table[If[ii==jj,0,If[ii<jj,"sab[["<>ToString[ii]<>","<>ToString[jj] <> "]]","sab[["<>ToString[jj]<>","<>ToString[ii] <> "]]"]],{ii,1,nn},{jj,1,nn}];
),
(
If[UseReImVariablesValue,
(
),
(
)
];

Print["Creating complex S, ST, and SabStr"];
nal=2*na;
S=Table[If[ii==jj,0,ToExpression["Wr"<>ToStringPadded[ii,MaxIdxLen]<>"c"<>ToStringPadded[jj,MaxIdxLen]]],{ii,1,nn},{jj,1,nn}];

WS=ss;

Print["Ap - preliminary matrix of antysimmetric variables (without accounting for the sign swap for pairs)."];
Ap=Table[If[ii==jj,0,If[ii<jj,ToExpression["Ar"<>ToStringPadded[ii,MaxIdxLen]<>"c"<>ToStringPadded[jj,MaxIdxLen]],-ToExpression["Ar"<>ToStringPadded[jj,MaxIdxLen]<>"c"<>ToStringPadded[ii,MaxIdxLen]]]],{ii,1,nn},{jj,1,nn}];

Print["Ap = ", Ap // MatrixForm];

Print["AL - Linearized antisymmetric variables. "];
AL=Flatten[Table[Table[Ap[[ii,jj]],{jj,ii+1,nn}],{ii,1,nn-1}]];
Print["AL = ", AL];

Print["A - matrix of all antisymmetric variables."];
A=ToASMatrix[AL];
Print["A = ", A // MatrixForm];

WA=A;

Print["ST - matrix of all variables as functions of t."];
ST=Table[If[ii==jj,0,ToExpression["Wr"<>ToStringPadded[ii,MaxIdxLen]<>"c"<>ToStringPadded[jj,MaxIdxLen]][t]],{ii,1,nn},{jj,1,nn}];

Print["SabStr - matrix of string variables."];
SabStr=Table[If[ii==jj,0,"wab[["<>ToString[ii]<>","<>ToString[jj] <> "]]"],{ii,1,nn},{jj,1,nn}];
)
];

Print["SL - Linearized variables. "];
SL=ToLinear[S];
WSL=ToSLinear[WS];
WAL=ToASLinear[WA];

(*

Print["Check ToASLinear..."];
Print["(AL - ToASLinear[A]) = ", (AL - ToASLinear[A])];

Print["Check ToASMatrix..."];
Print["(A - ToASMatrix[AL]) = ", (A - ToASMatrix[AL]) // MatrixForm];
*)

Print["STL - Linearized variables as functions of t. "];
(* STL=Flatten[Table[Table[ST[[ii,jj]],{jj,ii+1,nn}],{ii,1,nn-1}]]; *)
STL=ToLinear[ST];

Print["SLV - Linearized string variables for functions. "];
SLV=StringReplace[StringReplace[StringReplace[ToString[SL],"," -> "_,"],"}" -> "_"],"{" -> ""];

Print["SLVM - String mapping from sab matrix to linearized string variables for functions. "];
(* slvmHlp=Flatten[Table[Table[SabStr[[ii,jj]],{jj,ii+1,nn}],{ii,1,nn-1}]]; *)
slvmHlp=ToLinear[SabStr];
SLVM=StringReplace[StringReplace[ToString[slvmHlp],"}" -> ""],"{" -> ""];

Print["SLtRule={x -> x[t],y -> y[t],z -> z[t]}, ..."];
SLtRule=Table[SL[[ii]] -> SL[[ii]][t],{ii,1,nal}];

Print["SLt"];
SLt=SL /.SLtRule;

Print["EE - matrix of all 1."];
EE=Table[1,{a,1,nn},{b,1,nn}];

Print["EI - vector (column) of all 1, EIT - row of all 1."];
EI=Table[{1},{a,1,nn}];
EIT=Transpose[EI];
Print[strSeparatorSmall];

Print["eXXTIdentity - Identity matrix of size nn."];
eXXTIdentity=IdentityMatrix[nn];

Print["ZZ - matrix of all 0."];
ZZ=Table[0,{a,1,nn},{b,1,nn}];

Print["ZL - vector (of nal length) of all 0."];
ZL=Table[0,{a,1,nal}];

Print["II - Identity matrix."];
II=DiagonalMatrix[Table[1,{ii,1,nn}]];

Print["EEIJ - matrix of all 1 of dimension (nn-1) x (nn-1)"];
EEIJ=Table[1,{a,1,nn-1},{b,1,nn-1}];

Print["IIIJ"];
IIIJ=DiagonalMatrix[Table[1,{ii,1,nn-1}]];

Print["eeIJmeeIJT = IIIJ-(1/nn)*EEIJ"];
eeIJmeeIJT=IIIJ-(1/nn)*EEIJ;

Print["eprimIJidentity - (n-1) x (n-1) Identity matrix."];
eprimIJidentity=IIIJ;

If[calculateEVPrimDataVal,
(
If[useEVPrimNumericVal,
(
Print["Calculating alphaIJ and alphaIJpm1 numerically."];

If[useHighPrecisionVal,
(
Print["Using high precision arithmetics."];
eeIJmeeIJT=SetPrecision[eeIJmeeIJT,highPrecisionValueVal];
),
(
eeIJmeeIJT=N[eeIJmeeIJT];
)
];
)
];

Print["alphaIJ = MatrixPower[eeIJmeeIJT,(1/2)]"];
alphaIJ=MatrixPower[eeIJmeeIJT,(1/2)];

Print["alphaIJpm1 = Inverse[alphaIJ]"];
alphaIJpm1=Inverse[alphaIJ];
),
(
Print["!!! NOT calculating alphaIJ, alphaIJpm1."];
)
];

Print["YA"];
YA=Table[If[ii==jj,ToExpression["Yr"<>ToStringPadded[ii,MaxIdxLen]<>"c"<>ToStringPadded[jj,MaxIdxLen]],If[ii<jj,ToExpression["Yr"<>ToStringPadded[ii,MaxIdxLen]<>"c"<>ToStringPadded[jj,MaxIdxLen]],ToExpression["Yr"<>ToStringPadded[jj,MaxIdxLen]<>"c"<>ToStringPadded[ii,MaxIdxLen]]]],{ii,1,nn},{jj,1,nn}];

Print["YL - DOES NOT have diagonal elements."];
YL=Flatten[Table[Table[YA[[ii,jj]],{jj,ii+1,nn}],{ii,1,nn-1}]];

Print["YD"];
YD=Table[YA[[ii,ii]],{ii,1,nn}];

Print["YDRule"];
YDRule=Table[YA[[ii,ii]] -> (-Sum[If[ii==jj,0,YA[[ii,jj]]],{jj,1,nn}]),{ii,1,nn}];
];

(* ==============================================*)

