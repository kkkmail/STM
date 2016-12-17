(* ============================================== *)
(* :Summary:Various calculations for STM. *)
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
Print["TODO::STM_Calc::Change Transpose to ConjugateTranspose."];
DoRemoveI=True;
DebugRemoveI=False;
(* ============================================== *)

(* Returns log for numbers > 1 multiplied by the sign of the numbers. *)
LogVal[val_?NumericQ]:=Module[{retVal,REval},
REval=Re[val];
retVal=If[Abs[REval] > 1, Log[Abs[REval],10]*Sign[REval],Indeterminate];
Return[retVal];
];

(* ============================================== *)

(* Returns mean of the linear S variables *)
MeanLinear[sl_?VectorQ]:=Module[{retVal},
(* retVal=(1/NA)*Sum[sl[[ii]],{ii,1,NA}]; *)
retVal=Mean[sl];
Return[retVal];
];

(* ============================================== *)

(* Returns standard deviation of the linear S variables *)
StdDevLinear[sl_?VectorQ]:=Module[{retVal},
(* retVal=Sqrt[((1/NA)*Sum[sl[[ii]]^2,{ii,1,NA}])-MeanLinear[sl]^2]; *)
retVal=StandardDeviation[sl];
Return[retVal];
];

(* ============================================== *)

(* sab is a table of intervals; p0Idx, p1Idx, p2Idx are indicies of points *)
GammaFunc[p0Idx_,p1Idx_,p2Idx_,sab_?MatrixQ]:=Module[{retVal},
retVal=sab[[p0Idx,p1Idx]]+sab[[p0Idx,p2Idx]]-sab[[p1Idx,p2Idx]];
Return[retVal];
];

(* ============================================== *)

(* *)
LinearDependenceTestFunc[pnIdx_?VectorQ,sab_?MatrixQ]:=Module[{retVal,matr},
matr=LinearDependenceMatrix[pnIdx,sab];
(* Print["Fn::matr = ", matr // MatrixForm ]; *)
retVal=Det[matr];
Return[retVal];
];

(* ============================================== *)

LinearDependenceMatrix[pnIdx_?VectorQ,sab_?MatrixQ]:=Module[{len,ii,jj,matr},
len=Length[pnIdx];
matr=Table[GammaFunc[pnIdx[[1]],pnIdx[[ii]],pnIdx[[jj]],sab],{ii,2,len},{jj,2,len}];
Return[matr];
];

(* ============================================== *)

LambdaFunc[idxVal_?NumericQ,sol:{__}]:=Module[{idx,retVal,LambdaSorted},idx=Round[idxVal];
LambdaSorted=GetLambdaSorted[sol];
retVal=If[idx<1||idx>NNN,Indeterminate,LambdaSorted[[idx]]];
Return[retVal];
];

(* ============================================== *)

xRangeFunc[idxVal_?NumericQ,sol:{__}]:=Module[{idx,retVal,xRangeSorted},idx=Round[idxVal];
xRangeSorted=GetxRangeSorted[sol];
retVal=If[idx<1||idx>NNN,Indeterminate,xRangeSorted[[idx]]];
Return[retVal];
];

(* ============================================== *)

xRangeSigmaFunc[idxVal_?NumericQ,sol:{__}]:=Module[{idx,retVal,xRangeSigmaSorted},idx=Round[idxVal];
xRangeSigmaSorted=GetxRangeSigmaSorted[sol];
retVal=If[idx<1||idx>NNN,Indeterminate,xRangeSigmaSorted[[idx]]];
Return[retVal];
];

(* ============================================== *)

(* Function to plot vectors. *)
IndexedVariableFunc[idxVal_?NumericQ,vect_?VectorQ]:=Module[{idx,retVal,len},
len=Length[vect];
idx=Round[idxVal];
retVal=If[idx<1||idx>len,Indeterminate,vect[[idx]]];
Return[retVal];
];

(* ============================================== *)

(* Returns delta lambda. Minus sign used in consistency with the gradient usage in ODE. *)
DeltaLambdaFunc[idxVal_?NumericQ,deltaLmbSort_?VectorQ]:=-IndexedVariableFunc[idxVal,deltaLmbSort];

(* ============================================== *)

SValFunc[idxVal_?NumericQ,sValSort_?VectorQ]:=IndexedVariableFunc[idxVal,sValSort];

(* ============================================== *)

(*Returns Lambda sorted from a solution *)
GetLambdaSorted[sol:{__}]:=sol[[1]];

(*Returns sorted range of X from a solution *)
GetxRangeSorted[sol:{__}]:=sol[[2]];

(*Returns Lambda from a solution*)
GetLambda[sol:{__}]:=sol[[3]];

GetLambdaVec[sol:{__}]:=Module[{len,retVal,lambda,ii},
lambda=GetLambda[sol];
len=Length[lambda];
retVal=Table[lambda[[ii,ii]],{ii,1,len}];
Return[retVal];
];

(* ============================================== *)

(*Returns orthogonal matrix e (Y = e . Lambda . eH) from a solution *)
GetE[sol:{__}]:=sol[[4]];

(*Returns orthogonal (n-1) x (n-1) matrix from a solution *)
GetEprimIJ[sol:{__}]:=sol[[6]];

(*Returns sorted sigma range of X from a solution. *)
GetxRangeSigmaSorted[sol:{__}]:=sol[[5]];

(*Returns coordinates X from a solution. *)
GetCoord[sol:{__}]:=sol[[7]];

(*Returns  Y matrix from a solution. *)
GetY[sol:{__}]:=sol[[8]];

(* ============================================== *)

(* Calculates change in matrix Lambda (delta Lambda) due to change in (Sab  \[Rule] Sab + h*Gab) *)
CalculateDeltaLambda[gabVal:{{__},___},eXXTval:{{__},___}]:=CalculateDeltaLambda[gabVal,eXXTval,True];

CalculateDeltaLambda[gabVal:{{__},___},eXXTval:{{__},___},doSetPrecision_?BooleanQ]:=Module[{Gab,retVal,SumGab,TrZ,Zaa,Z,nn, delta1,eXXT},
Print["TODO::CalculateDeltaLambda::Change Transpose to ConjugateTranspose and update function for hermitian matrix."];
Return[Indeterminate];

nn=Length[gabVal];

If[doSetPrecision,
(
Gab=SetPrecision[gabVal,100];
eXXT=SetPrecision[eXXTval,100];
),
(
Gab=gabVal;
eXXT=eXXTval;
)
];

SumGab=Table[Sum[Gab[[aa,bb]],{bb,1,nn}],{aa,1,nn}];
TrZ=(1/(2*nn))*Sum[SumGab[[aa]],{aa,1,nn}];
Zaa=(1/nn)*Table[SumGab[[aa]]-TrZ,{aa,1,nn}];
Z=Table[If[aa==bb,Zaa[[aa]],(Zaa[[aa]]+Zaa[[bb]]-Gab[[aa,bb]])/2],{aa,1,nn},{bb,1,nn}];
delta1=ConjugateTranspose[eXXT] . Z . eXXT;
(* Print["delta1 = ", delta1 // MatrixForm]; *)

retVal=Table[delta1[[ii,ii]],{ii,1,nn}];
(* Print["delta Lambda (retVal) = ", retVal // MatrixForm]; *)
Return[retVal];
];

(* ============================================= *)

(* Calculates matrix Y from given matrix Sab *)
CalculateY[sabVal_?MatrixQ]:=CalculateY[sabVal,True];
CalculateY[wabVal_?MatrixQ,doSetPrecision_?BooleanQ]:=Module[{nn,wab,SumSabBYb,SumSabBYa,TrXXT,XXT,aa,bb,XXTaaBYa,XXTaaBYb},
nn=Length[wabVal];
wab=If[doSetPrecision,SetPrecision[wabVal,100],wabVal];

SumSabBYb=Table[Sum[wab[[aa,bb]],{bb,1,nn}],{aa,1,nn}];
SumSabBYa=Table[Sum[wab[[aa,bb]],{aa,1,nn}],{bb,1,nn}];

TrXXT=(1/(2*nn))*Sum[(SumSabBYb[[aa]]+SumSabBYa[[aa]])/2,{aa,1,nn}];

XXTaaBYa=(1/nn)*Table[SumSabBYa[[aa]]-TrXXT,{aa,1,nn}];
XXTaaBYb=(1/nn)*Table[SumSabBYb[[aa]]-TrXXT,{aa,1,nn}];

XXT=Table[((XXTaaBYb[[aa]]+XXTaaBYa[[bb]]-wab[[aa,bb]])/2),{aa,1,nn},{bb,1,nn}];

(* Print["Y = ", Chop[N[XXT]] // MatrixForm]; *)
Return[XXT];
];

(* ============================================= *)

CalculateYOld[sabVal_?MatrixQ,doSetPrecision_?BooleanQ]:=Module[{nn,sab,SumSab,TrXXT,XXTaa,XXT,aa,bb},
nn=Length[sabVal];
sab=If[doSetPrecision,SetPrecision[sabVal,100],sabVal];
SumSab=Table[Sum[sab[[aa,bb]],{bb,1,nn}],{aa,1,nn}];
TrXXT=(1/(2*nn))*Sum[SumSab[[aa]],{aa,1,nn}];
XXTaa=(1/nn)*Table[SumSab[[aa]]-TrXXT,{aa,1,nn}];
XXT=Table[If[aa==bb,XXTaa[[aa]],(XXTaa[[aa]]+XXTaa[[bb]]-sab[[aa,bb]])/2],{aa,1,nn},{bb,1,nn}];
Return[XXT];
];

(* ============================================= *)

(* Calculates matrix Y from given matrix Sab and Aab *)
CalculateHY[sabVal_?MatrixQ,aabVal_?MatrixQ]:=CalculateY[sabVal+I*aabVal,True];

CalculateHY[sabVal_?MatrixQ,aabVal_?MatrixQ,doSetPrecision_?BooleanQ]:=CalculateY[sabVal+I*aabVal,doSetPrecision];

(* ============================================= *)

CalculateSfromY[yabVal_?MatrixQ]:=Module[{retVal,a,b,len},
len=Length[yabVal];
retVal=Table[yabVal[[a,a]]+yabVal[[b,b]]-2*yabVal[[a,b]],{b,1,len},{a,1,len}];
Return[retVal];
];

(* ============================================= *)

CalculateSfromLambdaVec[lambdaVec_?VectorQ,eeMatrix_?MatrixQ]:=Module[{retVal,Y,len,a,b},
len=Length[lambdaVec];
Y=eeMatrix . DiagonalMatrix[lambdaVec] . ConjugateTranspose[eeMatrix];
(* retVal=Table[Y[[a,a]]+Y[[b,b]]-2*Y[[a,b]],{a,1,len},{b,1,len}]; *)
retVal=CalculateSfromY[Y];
Return[retVal];
];
(* ============================================= *)

CalculateLambdaSorted[sabVal_?MatrixQ]:=CalculateLambdaSorted[sabVal,True];
CalculateLambdaSorted[sabVal_?MatrixQ,doSetPrecision_?BooleanQ]:=Module[{yab,eVal,retVal},
yab=CalculateY[sabVal,doSetPrecision];
eVal=Eigenvalues[yab];
retVal=Sort[Re[eVal]];
Return[retVal];
];

(* ============================================= *)
(* Finds uncorrelated coordinates in the form *)
(* XXT = eXXT . Lambda . ConjugateTranspose[eXXT] *)
(* Where XXT (Y) is obtained from matrix Sab *)
FindUncorrelatedCoordinates[sabVal_?MatrixQ,rawOpts___]:=Module[{sab,retVal,SumSab,TrXXT,XXTaa,XXT,chkXXT,detXXT,eSys,eValXXT,LambdaSorted,Lambda,detLambda,LambdaSqrt,eVecXXT,eXXT,xSol,xRange,xRangeSorted,xChk1,aa,bb,nn,xRangeSigma,xRangeSigmaSorted,eeIJ,ii,jj,eprimIJ,opts,useHighPrecisionVal,highPrecisionValueVal},

opts=ProcessOptions[rawOpts];
useHighPrecisionVal=UseHighPrecision /.opts /.Options[STMCommon];
highPrecisionValueVal=HighPrecisionValue /.opts /.Options[STMCommon];

nn=Length[sabVal];

If[useHighPrecisionVal,
(
sab=SetPrecision[sabVal,highPrecisionValueVal];
),
(
sab=sabVal;
)
];

XXT=CalculateY[sabVal,False];

eSys=Eigensystem[XXT];
eValXXT=eSys[[1]];
LambdaSorted=Sort[Re[eValXXT]];
Lambda=DiagonalMatrix[eValXXT];
LambdaSqrt=MatrixPower[Lambda,(1/2)];
eVecXXT=eSys[[2]];
eXXT=ConjugateTranspose[eVecXXT];
xSol=eXXT.LambdaSqrt;

xRange=Table[(Max[If[Sign[Re[eValXXT[[ii]]]]>0,Re[Transpose[xSol][[ii]]],Im[Transpose[xSol][[ii]]]]]-Min[If[Re[Sign[eValXXT[[ii]]]]>0,Re[Transpose[xSol][[ii]]],Im[Transpose[xSol][[ii]]]]])*Sign[Re[eValXXT[[ii]]]],{ii,1,nn}];

xRangeSigma=Table[Sign[Re[eValXXT[[ii]]]]*(1/nn)*Abs[Sum[Transpose[xSol][[ii,kk]]^2,{kk,1,nn}]],{ii,1,nn}];

xRangeSorted=Sort[xRange];
xRangeSigmaSorted=Sort[xRangeSigma];

eeIJ=Table[eXXT[[ii,jj]],{ii,1,nn-1},{jj,1,nn-1}];
eprimIJ=alphaIJpm1 . eeIJ;

retVal={LambdaSorted,xRangeSorted,Lambda,eXXT,xRangeSigmaSorted,eprimIJ,xSol,XXT};

(*
Print["Check - XXT - eXXT . Lambda . ConjugateTranspose[eXXT]"];
Print[Chop[(XXT - eXXT . Lambda . ConjugateTranspose[eXXT])]// MatrixForm];
Print["eXXT = ", Chop[eXXT] // MatrixForm];
Print["xSol = ", Chop[xSol] // MatrixForm];
Print["xRange = ", Chop[xRange] // MatrixForm];
Print["xRangeSigma = ", Chop[xRangeSigma] // MatrixForm];
Print["xRangeSorted = ", Chop[xRangeSorted] // MatrixForm];
Print["xRangeSigmaSorted = ", Chop[xRangeSigmaSorted] // MatrixForm];
*)

Return[retVal];
];

Print["TODO::STM_Calc::eeReconstruct now supports only complex variables. "];
(* Reconstructs n x n orthogonal matrix ee based on *)
(* orthogonal (n-1) x (n-1) matrix eePrim *)
eeReconstruct[eePrim_?MatrixQ]:=Module[{eerpFinal,eeIJr,eerp,varHeadRe,varHeadIm,varLstRe,varLstIm,varLst,IIp,eqp,ii,jj,solP,epsCol,aCol,eerpH,eqpRe,eqpIm},
If[(Length[eePrim] != (NNN-1)) || (Length[eePrim[[1]]] != (NNN-1)),
(
Print["eeReconstruct::eePrim has incorrect size = ", Length[eePrim] , " x ",  Length[eePrim[[1]]]];
Return[Indeterminate];
)
];

varHeadRe="eeVarre";
varHeadIm="eeVarim";

varLstRe=Table[ToExpression[varHeadRe <> ToString[jj]],{jj,1,NNN-1}];
varLstIm=Table[ToExpression[varHeadIm <> ToString[jj]],{jj,1,NNN-1}];

varLst=Join[varLstRe,varLstIm];

(*
Print["varLstRe = ", varLstRe // MatrixForm];
Print["varLstIm = ", varLstIm // MatrixForm];
Print["varLst = ", varLst // MatrixForm];
*)

Apply[Clear,varLst];

eeIJr=alphaIJ . eePrim;

eerp=Table[If[ii < NNN && jj < NNN,eeIJr[[ii,jj]],If[jj==NNN,(1/Sqrt[NNN]),varLstRe[[jj]]+I*varLstIm[[jj]]]],{ii,1,NNN},{jj,1,NNN}];

eerpH=Simplify[ConjugateTranspose[eerp],Element[varLst,Reals]];

(* Print["eerpH = ", Chop[N[eerpH]] // MatrixForm]; *)

IIp=eerp . eerpH;

eqpRe=Table[RemoveI[(IIp-II)[[ii,NNN]]]==0,{ii,1,NNN-1}];
eqpIm=Table[RemoveI[-I*((IIp-II)[[ii,NNN]])]==0,{ii,1,NNN-1}];

(*
Print["eqpRe = ",Chop[ N[eqpRe] ] // MatrixForm];
Print["eqpIm = ",Chop[ N[eqpIm] ] // MatrixForm];
*)

eqp=Join[eqpRe,eqpIm];

solP=Solve[eqp,varLst];
epsCol=Transpose[{Table[1/Sqrt[NNN],{ii,1,NNN-1}]}];
aCol=-(1/Sqrt[NNN])*ConjugateTranspose[eePrim].alphaIJpm1.epsCol;
eerpFinal=eerp /. solP[[1]];

(*
Print["eeIJr = alphaIJ . eprimIJ"];
Print["IIp = ", Chop[N[IIp]]  // MatrixForm];
Print["eqp = ",Chop[ N[eqp] ] // MatrixForm];
Print["solP = ", Chop[N[solP]]];
Print["epsCol = ", Chop[N[epsCol]]];
Print["aCol = ", Chop[N[aCol]]];
Print["eerpFinal = ", Chop[N[eerpFinal]]  // MatrixForm];
*)

Return[eerpFinal];
];

(* Returns two pairs of dimensionless ranking numbers. *)
(* The smaller the numbers, the closer is lambda to R31 or R13. *)
(* R13SizeRank - how large is R13 compared to the others. *)
(* R13S3Rank - how close to S3 (or T3 ???) is the spatial part. *)
(* lambSort is a vector of sorted values of lambda. *)
LambdaRankFunc[lambSort_?VectorQ]:=Module[{retVal,R13SizeRank1,R13S3Rank1,R13SizeRank2,R13S3Rank2,mean1,stdDev1,mean2,stdDev2,lst1,lst2,nexttoT1,nexttoT2,nexttoS1,nexttoS2,lambdaT1, lambdaT2},

lst1={lambSort[[1]],lambSort[[2]],lambSort[[3]]};
lst2={lambSort[[NNN]],lambSort[[NNN-1]],lambSort[[NNN-2]]};

lambdaT1=lambSort[[NNN]];
lambdaT2=lambSort[[1]];

nexttoT1=lambSort[[NNN-1]];
nexttoT2=lambSort[[2]];

nexttoS1=lambSort[[4]];
nexttoS2=lambSort[[NNN-3]];

mean1=Mean[lst1];
mean2=Mean[lst2];

stdDev1=StandardDeviation[lst1];
stdDev2=StandardDeviation[lst2];

R13SizeRank1=(Abs[(nexttoT1/lambdaT1)]+Abs[(nexttoS1/mean1)])/2;
R13S3Rank1=Abs[stdDev1/mean1];

R13SizeRank2=(Abs[(nexttoT2/lambdaT2)]+Abs[(nexttoS2/mean2)])/2;
R13S3Rank2=Abs[stdDev2/mean2];

retVal={{R13SizeRank1,R13S3Rank1},{R13SizeRank2,R13S3Rank2}};
Return[retVal];
];

GetR13SizeRank[useFirstSpace_?BooleanQ,lambdaRank_?MatrixQ]:=If[useFirstSpace,lambdaRank[[1,1]],lambdaRank[[2,1]]];
GetR13S3Rank[useFirstSpace_?BooleanQ,lambdaRank_?MatrixQ]:=If[useFirstSpace,lambdaRank[[1,2]],lambdaRank[[2,2]]];

(* Returns table of solutions for the whole domain of InterpolationFunction *)
GetSolutionTableRange[solTbl:{__}]:=solTbl[[1]];
GetSolutionTableValues[solTbl:{__}]:=solTbl[[2]];

GetSolutionTable[sol:{__},tMultiplier_?NumericQ,noOfTsteps_?IntegerQ]:=Module[{f,tDomain,tMax,deltaT,retVal},
f=sEvolution/. sol[[1]];
tDomain=f["Domain"][[1,2]];
If[tMultiplier < 0 || tMultiplier > 1, (Print["tMultiplier is out of range."]; Return[Indeterminate];)];
tMax=tMultiplier*tDomain;
deltaT=tMax/noOfTsteps;
retVal={{0,tMax,noOfTsteps},Table[f[deltaT*ii],{ii,0,noOfTsteps}]};
Return[retVal];
];

(* Returns table of Lambda based on the solution table as obtained by calling to GetSolutionTable. *)
GetLambdaTable[solTbl:{{__},__}]:=Module[{retVal,tIdx,noOfTsteps,tMin,tMax,tRange,values,deltaT,stl,st,solValue,lambda,ii},
Print["GetLambdaTable::Starting..."];
tRange=GetSolutionTableRange[solTbl];
values=GetSolutionTableValues[solTbl];

tMin=tRange[[1]];
tMax=tRange[[2]];
noOfTsteps=tRange[[3]];
deltaT=(tMax-tMin)/noOfTsteps;

retVal=Table[Indeterminate,{ii,0,noOfTsteps}];

For[tIdx = 0, tIdx <= noOfTsteps,tIdx++,
(
stl=values[[tIdx+1]];
st=ToMatrix[stl];
solValue=FindUncorrelatedCoordinates[st];
lambda=GetLambda[solValue];
retVal[[tIdx+1]]=lambda;
)
];

PrinTimeUsed[];
Return[retVal];
];

(* ============================================== *)
Print["sm Analytinc functions for R(NT,NS,TTT)"];
smAnalyticFunc[kk_?IntegerQ,nt_,ns_,ttt_]:=(ns+nt*(-ttt)^kk)/(ns+nt*ttt^2)^(kk/2);
mAnalyticFunc[kk_?IntegerQ,nt_,ns_,ttt_]:=(ns+nt*(-ttt)^kk);

(* ============================================= *)

lambdaFunc[NoOfElements_?IntegerQ,NT_?IntegerQ,NS_?IntegerQ]:=lambdaFunc[NoOfElements,NT,NS,1,0,0];

lambdaFunc[NoOfElements_?IntegerQ,NT_?IntegerQ,NS_?IntegerQ,TTT_]:=lambdaFunc[NoOfElements,NT,NS,TTT,0,0];

lambdaFunc[NoOfElements_?IntegerQ,NT_?IntegerQ,NS_?IntegerQ,TTT_,eps_,epsST_]:=Table[If[ii<= NT,-TTT*(1+epsST*(ii-(NT+1)/2)),If[ii<= NT+NS,(1+epsST*(ii-NT-(NS+1)/2)),eps]],{ii,1,NoOfElements}];

(* ============================================= *)

MFunc[level_?IntegerQ,lst_?VectorQ]:=Module[{retVal,len,ii},
len=Length[lst];
retVal=Sum[lst[[ii]]^level,{ii,1,len}];
Return[retVal];
];

(* ============================================= *)

MFuncY[level_?IntegerQ,yab_?MatrixQ]:=Module[{retVal},
retVal=Tr[MatrixPower[yab,level]];
Return[retVal];
];

(* ============================================= *)

MFuncSL[level_?IntegerQ,sLinear_?VectorQ]:=Module[{retVal,sab,yab,yabOld},
sab=ToSMatrix[sLinear];
yab=CalculateY[sab,False];
retVal=Tr[MatrixPower[yab,level]];

(*
Print["sab = ", sab // MatrixForm];
Print["yab = ", yab // MatrixForm];
Print["retVal = ", retVal];
*)

Return[retVal];
];

(* ============================================== *)

MFuncSL[level_?IntegerQ,sLinear_?VectorQ,aLinear_?VectorQ]:=Module[{retVal,sab,aab,yab,yabOld},
sab=ToSMatrix[sLinear];
aab=ToAMatrix[aLinear];
yab=CalculateHY[sab,aab,False];
retVal=Tr[MatrixPower[yab,level]];

(*
Print["sab = ", sab // MatrixForm];
Print["aab = ", aab // MatrixForm];
Print["yab = ", Expand[yab] // MatrixForm];
Print["retVal = ", Expand[retVal]];
*)

Return[retVal];
];

(* ============================================= *)
(* Function to remove I *)

RemoveI[expr_]:=Module[{exprVal},
exprVal=expr;

If[DoRemoveI,
(
exprVal=Simplify[ToExpression[StringReplace[ToString[InputForm[Expand[exprVal],NumberMarks -> False]],"I" -> "0"]]];
If[DebugRemoveI,
(
Print["RemoveI::Removing I."];
Print["expr = ", InputForm[expr,NumberMarks -> False]];
Print["exprVal = ", InputForm[exprVal,NumberMarks -> False]];
Print[sep];
)
];
)
];

Return[exprVal];
];
(* ============================================= *)


