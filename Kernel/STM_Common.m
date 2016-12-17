(* ============================================== *)
(* :Summary: Common STM logic. *)
(* ============================================== *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL v3 or any later version, see http://www.gnu.org/licenses/ *)
(* :Copyright: K^3, 2013 - 2015 *)
(* :Version: Revision: 2.04.001, Date: 2015/08/30 *)
(* :Mathematica Version: 10.0 *)
(* ============================================== *)
(* This program is free software: you can redistribute it and/or modify it under the terms *)
(* of the GNU General Public License as published by the Free Software Foundation, *)
(* either version 3 of the License, or any later version. This program is distributed in the hope that  *)
(* it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *) 
(* or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. *)
(* You should have received a copy of the GNU General Public License along with this program. *) 
(* If not, see <http://www.gnu.org/licenses/>. *)
(* ============================================== *)
(* ============================================== *)
(* There are many global variables for simplicity (though at some cost, of course). *)
(* Some of the most important ones are: *)
(* NNN - number of elements. *)
(* NA = NNN*(NNN-1)/2 number of independent variables in the upper triangle. *)
(* NAL - Total number of independent variables. If real variables are used then NAL = NA. If complex variable are used then NAL = 2*NA *)
(* S - symbolic symmetric matrix of intervals. *)
(* SL - linear symbolic representation of S (contains NA elements). *)
(* SI - matrix of initial values. *)
(* SIL - linear version of the same initial values (for convenience). *)
(* SIrule - linear matrix of rules to convert symbolic values of S (or SL) into initial values of SI (or SIL). *)
(* ============================================== *)
Print["TODO::STM_Common::Propagate UseHighPrecision and HighPrecisionValue throughout code..."];
Options[STMCommon]={CalculateEVPrimData -> False,UseEVPrimNumeric -> True,UseHighPrecision -> False,HighPrecisionValue -> 200,UseAlternatingAS -> False};
(* ============================================== *)
NT=1;
NS=3;
TTTval=1;
M2Mult=1;
(* ============================================== *)
strSeparator="============================================="<>FromCharacterCode[10];
strSeparatorSmall="---------------------------------------------";
strCRLF=FromCharacterCode[10];
(* ============================================== *)

(* Used by F4Func to distinguish between initialization and calculations. *)
IsInitialized=False;
IsInitializedF6=False;

UseIntegerDistribution = False;
rndFunc=NormalDistribution;
rndArgs={};

(* Threshold to normalize the sum of initial variables by adjusting the last value instead of dividing by the sum. *)
InitialValuesZeroTolerance=10^-6;

UseAlternatingASValue=True;
(* ============================================== *)

If[$VersionNumber<=9.,
(
If[!SilentRunValue,Print["Initializing BooleanQ."]];
BooleanQ[x_]:=If[Element[x,Booleans],True,False,False];
),
(
If[!SilentRunValue,Print["Version number is ",$VersionNumber," NOT initializing BooleanQ."]];
)
];
(* ============================================== *)

If[$VersionNumber <=8.,
If[UseIntegerDistribution == True,RandomVariate[distr_]:=RandomInteger[distr],RandomVariate[distr_]:=RandomReal[distr],RandomVariate[distr_]:=RandomReal[distr]]
];

Print[Definition[RandomVariate]];

(* ============================================== *)

Reset[]:=Module[{},
Print["Resetting 4th power polynom model."];
NoTermCnt=0;
TermMatrix={};
TermExampleMatrix={};

NoInvariantCnt=0;
InvariantMatrix={};
InvariantExpressionMatrix={};
InvariantStrExpressionMatrix={};
];

(* ============================================== *)

(* 4-th power polynom *)
NextTermNo[]:=Module[{},
TermMatrix=Join[TermMatrix,{0}];
TermExampleMatrix=Join[TermExampleMatrix,{0}];
NoTermCnt++;
Return[NoTermCnt];
];

(* ============================================== *)

AddTerm[idxTerm_?IntegerQ,termName_,termExample_]:=Module[{},
If[idxTerm<1||idxTerm>NoTermCnt,(Print["AddTerm::idxTerm is out of range."];Return[])];
TermMatrix[[idxTerm]]=termName;
TermExampleMatrix[[idxTerm]]=termExample;
];

(* ============================================== *)

NextInvariantNo[]:=Module[{},
InvariantMatrix=Join[InvariantMatrix,{0}];
InvariantExpressionMatrix=Join[InvariantExpressionMatrix,{0}];
InvariantStrExpressionMatrix=Join[InvariantStrExpressionMatrix,{0}];
NoInvariantCnt++;
Return[NoInvariantCnt];
];

(* ============================================== *)

AddInvariant[idxInvariant_?IntegerQ,invariantName_,invariantExpression_,invariantStrExpression_]:=Module[{},
If[idxInvariant<1||idxInvariant>NoTermCnt,(Print["AddInvariant::idxInvariant is out of range."];Return[])];
InvariantMatrix[[idxInvariant]]=invariantName;
InvariantExpressionMatrix[[idxInvariant]]=invariantExpression;
InvariantStrExpressionMatrix[[idxInvariant]]=invariantStrExpression;
];

(* ============================================== *)

(* 6-th power polynom *)
NextTermNoF6[]:=Module[{},
TermMatrixF6=Join[TermMatrixF6,{0}];
TermExampleMatrixF6=Join[TermExampleMatrixF6,{0}];
NoTermCntF6++;
Return[NoTermCntF6];
];

(* ============================================== *)

AddTermF6[idxTerm_?IntegerQ,termName_,termExample_]:=Module[{},
If[idxTerm<1||idxTerm>NoTermCntF6,(Print["AddTermF6::idxTerm is out of range."];Return[])];
TermMatrixF6[[idxTerm]]=termName;
TermExampleMatrixF6[[idxTerm]]=termExample;
];

(* ============================================== *)

NextInvariantNoF6[]:=Module[{},
InvariantMatrixF6=Join[InvariantMatrixF6,{0}];
InvariantExpressionMatrixF6=Join[InvariantExpressionMatrixF6,{0}];
InvariantStrExpressionMatrixF6=Join[InvariantStrExpressionMatrixF6,{0}];
NoInvariantCntF6++;
Return[NoInvariantCntF6];
];

(* ============================================== *)

AddInvariantF6[idxInvariant_?IntegerQ,invariantName_,invariantExpression_,invariantStrExpression_]:=Module[{},
If[idxInvariant<1||idxInvariant>NoTermCntF6,(Print["AddInvariantF6::idxInvariant is out of range."];Return[])];
InvariantMatrixF6[[idxInvariant]]=invariantName;
InvariantExpressionMatrixF6[[idxInvariant]]=invariantExpression;
InvariantStrExpressionMatrixF6[[idxInvariant]]=invariantStrExpression;
];

(* ============================================== *)

tStart=AbsoluteTime[];
tMid=tStart;

PrintTimeUsed[showTime_]:=Module[{tEnd},tEnd=AbsoluteTime[];
If[showTime==True,Print["Time used: ",(tEnd-tMid),", total time used: ",(tEnd-tStart),FromCharacterCode[10]<>strSeparatorSmall],Print["Time used reset."],Print["Time used reset."]];
tMid=tEnd;];

PrintTimeUsed[]:=PrintTimeUsed[True];

(* ============================================== *)

ToStringPadded[obj_,n_?IntegerQ]:=Module[{s,retVal,len, sPad,ii,padChar},
padChar="0";
s=ToString[obj];
len=StringLength[s];
retVal=s;

If[len < n,
(
sPad=StringJoin[Table[padChar,{ii,1,n}]];
retVal=StringTake[sPad <> s, -n];
)
];

Return[retVal];
];

(* ============================================== *)

InitializeVariables[nn_?IntegerQ,rawOpts___]:=Module[{},
Print["Initializing models & related coefficients."];
InitializeModels[];

Print["Reinitializing variables for numeric run."];
Initialize[NumberOfElements,rawOpts];
PrintTimeUsed[];
];

(* ============================================== *)

Initialize[nn_?IntegerQ,rawOpts___]:=Module[{opts,na,nal},
Print["Initializing."];
opts=ProcessOptions[rawOpts];

NNN=nn;
NA=NNN*(NNN-1)/2;
NAL=NA;

If[UseComplexVariablesValue,
(
NAL=2*NA;
)
];

GradientMultiplier=(1/NAL);

Print["NNN = ",NNN];
Print["NA = ",NA];
Print["GradientMultiplier = ",GradientMultiplier];

Print["!!! Clearing t !!!"];
Clear[t];

Print["remapTable"];
remapTable=Table[Which[ii==jj,0,ii<jj,((ii-1)*(2*nn-ii)/2)+(jj-ii),ii>jj,((jj-1)*(2*nn-jj)/2)+(ii-jj)],{ii,1,nn},{jj,1,nn}];

Print["remapReverseTable"];
remapReverseTable=Flatten[Table[{ii,jj},{ii,1,nn-1},{jj,ii+1,nn}],1];

CreateVariables[nn,rawOpts];
CreateEValues[nn,rawOpts];
];

(* ============================================== *)

CreateEValues[nn_?IntegerQ,rawOpts___]:=Module[{opts,useEVPrimNumericVal,calculateEVPrimDataVal,useHighPrecisionVal,highPrecisionValueVal,na},
opts=ProcessOptions[rawOpts];
useEVPrimNumericVal=UseEVPrimNumeric/.opts /.Options[STMCommon];
calculateEVPrimDataVal=CalculateEVPrimData /.opts /.Options[STMCommon];
useHighPrecisionVal=UseHighPrecision /.opts /.Options[STMCommon];
highPrecisionValueVal=HighPrecisionValue/.opts /.Options[STMCommon];

na=nn*(nn-1)/2;

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

Print["ZL - vector (of na length) of all 0."];
ZL=Table[0,{a,1,na}];

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
];

(* ============================================== *)

(* Sorts second list using the normal ordering of the first one. *)
SortByFirst[lstOrder:{__},lstToBeSorted:{__}]:=Module[{retVal,nn,lst,lstSorted},
nn=Length[lstOrder];

If[Length[lstToBeSorted]!= nn,(Print["SortByFirt::Lists have different length!"]; Return[Indeterminate];)];

lst=Table[{lstOrder[[ii]],lstToBeSorted[[ii]]},{ii,1,nn}];

lstSorted=SortBy[lst,First];
retVal=Table[lstSorted[[ii,2]],{ii,1,nn}];

Return[retVal];
];

(* ==============================================*)

(* Converts from linear to {matrixS, matrixA} representation. Expects the values of NNN and NA to be correct. *)

ToMatrix[varLinear_?VectorQ]:=Module[{varMatrixS,varMatrixA,ii,jj},
varMatrixS=ToSMatrix[varLinear];
varMatrixA=ToAMatrix[varLinear,True];
Return[{varMatrixS,varMatrixA}];
];

(* ==============================================*)

(* Converts from matrix to linear representation. Expects the values of NNN and NA to be correct. *)
ToLinear[varMatrixS_?MatrixQ,varMatrixA_?MatrixQ]:=Module[{varLinear},
varLinear=Join[ToSLinear[varMatrixS],ToALinear[varMatrixA]];
Return[varLinear];
];

(* ==============================================*)

(* Converts from linear to symmetric matrix representation. *)
ToSMatrix[varLinear_?VectorQ]:=Module[{varMatrix,ii,jj},
(*
Print["ToSMatrix::Starting"];
Print["ToSMatrix::varLinear = ", varLinear // MatrixForm];
*)
varMatrix=Table[If[ii!= jj,varLinear[[remapTable[[ii,jj]]]],0],{ii,1,NNN},{jj,1,NNN}];
(* Print["ToSMatrix::varMatrix = ", varMatrix // MatrixForm]; *)
Return[varMatrix];
];

(* ==============================================*)

(* Converts from symmetric matrix to linear representation. *)
ToSLinear[varMatrix_?MatrixQ]:=Module[{varLinear},
varLinear=Table[varMatrix[[remapReverseTable[[ii,1]],remapReverseTable[[ii,2]]]],{ii,1,NA}];
Return[varLinear];
];

(* ==============================================*)

(* Converts from linear to antisymmetric matrix representation. *)

ToAMatrix[varLinear_?VectorQ]:=ToAMatrix[varLinear,False];

ToAMatrix[varLinear_?VectorQ,addNA_?BooleanQ]:=Module[{varMatrix,shift},
shift=0;
If[addNA,shift=NA];

(*
Print["ToAMatrix::Starting"];
Print["ToAMatrix::varLinear = ", varLinear // MatrixForm];
Print["ToAMatrix::shift = ", shift];
*)

If[UseAlternatingASValue,
(
varMatrix=Table[If[ii!= jj,(If[ii> jj,1,-1])*((-1)^(ii+jj))*varLinear[[shift+remapTable[[ii,jj]]]],0],{ii,1,NNN},{jj,1,NNN}];
)
,
(
varMatrix=Table[-If[ii!= jj,(If[ii> jj,1,-1])*varLinear[[shift+remapTable[[ii,jj]]]],0],{ii,1,NNN},{jj,1,NNN}];
)
];

(* Print["ToSMatrix::ToAMatrix = ", varMatrix // MatrixForm]; *)

Return[varMatrix];
];

(* ==============================================*)

(* Converts from antisymmetric matrix to linear representation. Expects the values of NNN and NA to be correct. *)
ToALinear[varMatrix_?MatrixQ]:=Module[{varLinear},
If[UseAlternatingASValue,
(
varLinear=Table[-((-1)^(remapReverseTable[[ii,1]]+remapReverseTable[[ii,2]]))*varMatrix[[remapReverseTable[[ii,1]],remapReverseTable[[ii,2]]]],{ii,1,NA}];
)
,
(
varLinear=Table[varMatrix[[remapReverseTable[[ii,1]],remapReverseTable[[ii,2]]]],{ii,1,NA}];
)
];

Return[varLinear];
];

(* ==============================================*)
(* Function to convert 4D Hessian into matrix form *)
ToLinearHessian[varHessian:{{{{__},___},___},___}]:=Module[{varLinearHessian,varLinearHessian11,varLinearHessian12,varLinearHessian21,varLinearHessian22,ii,jj},
varLinearHessian=Table[varHessian[[remapReverseTable[[ii,1]],remapReverseTable[[ii,2]],remapReverseTable[[jj,1]],remapReverseTable[[jj,2]]]],{ii,1,NA},{jj,1,NA}];

Return[varLinearHessian];
];

(* ==============================================*)

BlockMatrix[a11_?MatrixQ,a12_?MatrixQ,a21_?MatrixQ,a22_?MatrixQ]:=Module[{a11RowLen,a11ColLen,a12RowLen,a12ColLen,a21RowLen,a21ColLen,a22RowLen,a22ColLen,retVal},
a11RowLen=Length[a11];
a11ColLen=Length[a11[[1]]];

a12RowLen=Length[a12];
a12ColLen=Length[a12[[1]]];

a21RowLen=Length[a21];
a21ColLen=Length[a21[[1]]];

a22RowLen=Length[a22];
a22ColLen=Length[a22[[1]]];

retVal=Indeterminate;

If[((a11RowLen == a12RowLen) && (a21RowLen == a22RowLen) && (a11ColLen == a21ColLen) && (a12ColLen == a22ColLen)),
(
retVal=ArrayFlatten[{{a11,a12},{a21,a22}}];
),
(
Print["BlockMatrix::Invalid data: "];
Print["BlockMatrix::a11RowLen =  ", a11RowLen, ", a11ColLen = ", a11ColLen];
Print["BlockMatrix::a12RowLen =  ", a12RowLen, ", a12ColLen = ", a12ColLen];
Print["BlockMatrix::a21RowLen =  ", a21RowLen, ", a21ColLen = ", a21ColLen];
Print["BlockMatrix::a22RowLen =  ", a22RowLen, ", a22ColLen = ", a22ColLen];
)
];

Return[retVal];
];

(* ==============================================*)

ToLinearHessian[varHessianSS:{{{{__},___},___},___},varHessianSA:{{{{__},___},___},___},varHessianAS:{{{{__},___},___},___},varHessianAA:{{{{__},___},___},___}]:=Module[{varLinearHessianSS,varLinearHessianAA,varLinearHessianSA,varLinearHessianAS,ii,jj,varLinearHessian},
varLinearHessianSS=ToLinearHessian[varHessianSS];
varLinearHessianAA=ToLinearHessian[varHessianAA];
varLinearHessianSA=ToLinearHessian[varHessianSA];
varLinearHessianAS=ToLinearHessian[varHessianAS];

varLinearHessian=BlockMatrix[varLinearHessianSS,varLinearHessianSA,varLinearHessianAS,varLinearHessianAA];

Return[varLinearHessian];
];

(* ==============================================*)

SumAll[expr_,idxLst_?VectorQ]:=Module[{retVal,idxLen,idxAllLst,ii,exprLst},
(* Print["SumAll::Startring"]; *)

idxLen=Length[idxLst];
idxAllLst=Table[{idxLst[[ii]],1,NNN},{ii,1,idxLen}];
exprLst=Join[{Hold[expr]},idxAllLst];
retVal=Quiet[Apply[Sum,ReleaseHold[exprLst]]];

(*
Print["SumAll::exprLst = ", ToString[exprLst]];
Print["SumAll::retVal = ", retVal];
*)

Return[retVal];
];

SetAttributes[SumAll,HoldFirst];

(* ==============================================*)

