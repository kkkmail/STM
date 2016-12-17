(* :Author: Konstantin K.Konstantinov *)
(* :Summary: Minimization algorithms based on F2 + F2^2 model. *)
(* :Copyright: K^3,2013 *)
(* :Version: Revision: 1.10.001, Date:2013/10/14 *)
(* :Mathematica Version:7.0-9.0 *)
(* ==============================================*)

RFCF2MethodMin=1;
RFCF2MethodFindMinimum=1;
RFCF2MethodNMinimize=2;
RFCF2MethodMax=2;

(* ==============================================*)

RFCF2SABMethodMin=1;
RFCF2SABMethodFindMinimum=1;
RFCF2SABMethodNMinimize=2;
RFCF2SABMethodMax=2;

(* ==============================================*)

Options[STMMinF2]={PrecisionDigits -> 200,RFCF2Method -> RFCF2MethodNMinimize,F2SqMinPrintStep -> 200,RFCF2SABMethod -> RFCF2SABMethodFindMinimum,F2SqMinSabLinearPrintStep -> 10};

(* ==============================================*)

RunFindCoeffF2[initCoeffF2Val_?VectorQ,noOfElements_?IntegerQ,rawOpts___]:=Module[{coeffF2,opts,sol,varListF2,findMinInitValsF2,initCoeffF2,precisionDigitsVal,ii,constr,rfcf2MethodVal,sab,idxTerm,fValLst,eqLst,fVal},
(* We are looking for a solution for CoeffF2, which result in zero value of F2 function for R (1,3) elements. *)
opts=ProcessOptions[rawOpts];
$MaxPrecision=Infinity;

precisionDigitsVal=PrecisionDigits/.opts /.Options[STMMinF2];
rfcf2MethodVal=RFCF2Method/.opts /.Options[STMMinF2];

If[Length[initCoeffF2Val] !=NoOfTermsF2,
(
Print["RunFindCoeffF2::initCoeffF2Val has incorrect length = ", Length[initCoeffF2Val]];
Return[Indeterminate];
)
];

(* Initializing varialbes for numeric run. *)
Initialize[noOfElements];

(* Creating initial values in R (1,3) for numerical run. *)
CreateInitialvalues[IVT4DPoints,NIVnone];
sab=SetPrecision[SI,precisionDigitsVal];

fValLst=Table[Indeterminate,{ii,1,NoOfTermsF2}];

For[idxTerm = 1, idxTerm <= NoOfTermsF2, idxTerm++,
(
initCoeffF2=Table[If[ii== idxTerm,1,0],{ii,1,NoOfTermsF2}];
UpdateAllCoeffF2[SetPrecision[initCoeffF2,precisionDigitsVal]];
fValLst[[idxTerm]]=FP2All[sab];
)
];

varListF2=Table[ToExpression["varF2n" <> TermMatrixF2[[ii]]],{ii,1,NoOfTermsF2}];

constr=(Sum[varListF2[[ii]]^2,{ii,1,NoOfTermsF2}]-1) ;
eqLst={Sum[varListF2[[ii]]*fValLst[[ii]],{ii,1,NoOfTermsF2}]==0,constr==0};
Print["eqLst = ", eqLst];
sol=NSolve[eqLst,varListF2];

(*
initCoeffF2=Normalize[SetPrecision[initCoeffF2Val,precisionDigitsVal]];

Print["RunFindCoeffF2::Normalized initial coefficients F2 are: ", initCoeffF2];


findMinInitValsF2=Table[{varListF2[[ii]],initCoeffF2[[ii]]},{ii,1,Length[varListF2]}];

Print["RunFindCoeffF2::Running constrained minimization."];


If[rfcf2MethodVal == RFCMethodFindMinimum,
(
Print["RunFindCoeffF2::Using FindMinimum and constrained optimization."];
sol=FindMinimum[{F2SqMinFunc[varListF2,sab],constrF2==0},findMinInitValsF2,WorkingPrecision -> precisionDigitsVal];
)
];

If[rfcf2MethodVal == RFCMethodNMinimize,
(
Print["RunFindCoeffF2::Using NMinimize and constrained optimization."];
sol=NMinimize[{F2SqMinFunc[varListF2,sab],constrF2==0},varListF2,WorkingPrecision -> precisionDigitsVal];
)
];
*)
Print["sol = ", sol];

Print["Checking"];
initCoeffF2=varListF2 /. sol[[1]];
UpdateAllCoeffF2[SetPrecision[initCoeffF2,precisionDigitsVal]];
fVal=FP2All[[sab]];
Print["fVal = ", fVal];
Return[sol];
]/; VectorQ[initCoeffF2Val,NumericQ];

(* ==============================================*)

cntF2SqMinFunc=0;
MinF2SqMinFuncRetVal=10^50;
CoeffF2F2SqMinFunc={};
IdxF2SqMinFunc=0;

(* ==============================================*)

F2SqMinFunc[coeffF2_?VectorQ,sab_?MatrixQ,rawOpts___]:=Module[{retVal,opts,f2SqMinPrintStepVal,precisionDigitsVal},
cntF2SqMinFunc++;
$MaxPrecision=Infinity;
opts=ProcessOptions[rawOpts];
f2SqMinPrintStepVal=F2SqMinPrintStep/.opts /.Options[STMMinF2];
precisionDigitsVal=PrecisionDigits/.opts /.Options[STMMinF2];
UpdateAllCoeffF2[SetPrecision[coeffF2,precisionDigitsVal]];
retVal=FP2All[SetPrecision[sab,precisionDigitsVal]]^2;

If[retVal <MinF2SqMinFuncRetVal,
(
MinF2SqMinFuncRetVal=retVal;
CoeffF2F2SqMinFunc=coeffF2;
IdxF2SqMinFunc=cntF2SqMinFunc;
)
];

If[Mod[cntF2SqMinFunc,f2SqMinPrintStepVal]==0,
(
Print["F2SqMinFunc::cntF2SqMinFunc = ", cntF2SqMinFunc, ", retVal = ", retVal, ", coeffF2 = ", coeffF2];
Print["F2SqMinFunc::Known minimum run counter is: ", IdxF2SqMinFunc];
Print["F2SqMinFunc::Minimum value = ", MinF2SqMinFuncRetVal];
Print["    CoeffF2F2SqMinFunc = ", CoeffF2F2SqMinFunc];
PrintTimeUsed[];
)
];

Return[retVal];
]/; (VectorQ[coeffF2,NumericQ] && MatrixQ[sab,NumericQ]);

(* ==============================================*)

RunF2SqMinSabLinearFunc[initCoeffF2Val_?VectorQ,noOfElements_?IntegerQ,rawOpts___]:=Module[{coeffF2,opts,sol,finMinInitValLst,initCoeffF2,precisionDigitsVal,ii,rfcf2SabMethodVal,stl,st,stlSorted,solValue,lambda,eXXT,gradVal,gradSortedVal,idx,lambdaSorted,lambdaRank,R13testVal,constr,sabLinearVal},
opts=ProcessOptions[rawOpts];
$MaxPrecision=Infinity;

precisionDigitsVal=PrecisionDigits/.opts /.Options[STMMinF2];
rfcf2SabMethodVal=RFCF2SABMethod/.opts /.Options[STMMinF2];

If[Length[initCoeffF2Val] !=NoOfTermsF2,
(
Print["RunF2SqMinSabLinearFunc::initCoeffF2Val has incorrect length = ", Length[initCoeffF2Val]];
Return[Indeterminate];
)
];

(* Initializing varialbes for numeric run. *)
Initialize[noOfElements];

(* Creating initial values in R'(NNN) for numerical run. *)
CreateInitialvalues[IVTdistribution,NIVnone];

initCoeffF2=Normalize[SetPrecision[initCoeffF2Val,precisionDigitsVal]];
Print["RunF2SqMinSabLinearFunc::Normalized initial coefficients F2 are: ", initCoeffF2];
UpdateAllCoeffF2[initCoeffF2];

sabLinearVal=SetPrecision[SIL,precisionDigitsVal];
finMinInitValLst=Table[{SL[[ii]],sabLinearVal[[ii]]},{ii,1,NA}];
Print["Initial: Mean = ",MeanLinear[SIL],", Standard Deviation = ",StdDevLinear[SIL]];

PrintResults[SIL,"RunF2SqMinSabLinearFunc::Initial Values"];

Print["RunF2SqMinSabLinearFunc::Running unconstrained minimization."];

constr=(Sum[SL[[ii]],{ii,1,NA}]-Sum[sabLinearVal[[ii]],{ii,1,NA}]);

If[rfcf2SabMethodVal == RFCF2SABMethodFindMinimum,
(
Print["RunF2SqMinSabLinearFunc::Using FindMinimum and constrained optimization."];
sol=FindMinimum[{F2SqMinSabLinearFunc[SL,rawOpts],constr== 0},finMinInitValLst ,WorkingPrecision -> precisionDigitsVal];

(*
Print["RunF2SqMinSabLinearFunc::Using FindMinimum and unconstrained optimization with tangential gradient."];
sol=FindMinimum[F2SqMinSabLinearFunc[SL,rawOpts],finMinInitValLst  ,Gradient :> GradF2SqMinSabLinearFunc[SL,True,rawOpts],WorkingPrecision -> precisionDigitsVal ,Method -> "ConjugateGradient" ];
*)
),
(
Print["TODO::RunF2SqMinSabLinearFunc::Not implemented for rfcf2SabMethodVal = ", rfcf2SabMethodVal];
Return[Indeterminate];
)
];

stl=SL /. sol[[2]];
PrintResults[SIL,"RunF2SqMinSabLinearFunc::Final Values"];
Return[sol];
];

(* ==============================================*)

cntF2SqMinSabLinearFunc=0;
MinF2SqMinSabLinearFuncRetVal=10^50;
sablF2SqMinSabLinearFunc={};
IdxF2SqMinSabLinearFunc=0;

(* ==============================================*)

F2SqMinSabLinearFunc[sabLinearVal_?VectorQ,rawOpts___]:=Module[{sabLinear,retVal,opts,f2SqMinSabLinearPrintStepVal,precisionDigitsVal,sab,stl,st,stlSorted,solValue,lambda,eXXT,gradVal,gradSortedVal,idx,lambdaSorted,lambdaRank,R13testVal},
cntF2SqMinSabLinearFunc++;
$MaxPrecision=Infinity;
opts=ProcessOptions[rawOpts];
f2SqMinSabLinearPrintStepVal=F2SqMinSabLinearPrintStep/.opts /.Options[STMMinF2];
precisionDigitsVal=PrecisionDigits/.opts /.Options[STMMinF2];

sabLinear=SetPrecision[sabLinearVal,precisionDigitsVal];
sab=ToMatrix[sabLinear];
retVal=FP2All[sab]^2;

stl=sabLinear;
st=ToMatrix[stl];
stlSorted=Sort[stl];
solValue=FindUncorrelatedCoordinates[st];
lambda=GetLambda[solValue];
eXXT=GetE[solValue];
gradVal=GradF2SqMinSabLinearFunc[stl,True,rawOpts];
gradSortedVal=SortByFirst[stl,gradVal];
lambdaSorted=GetLambdaSorted[solValue];
lambdaRank=N[LambdaRankFunc[lambdaSorted]];

R13testVal=Min[GetR13SizeRank[True,lambdaRank]*R13SizeWeight+GetR13S3Rank[True,lambdaRank]*R13S3Weight,GetR13SizeRank[False,lambdaRank]*R13SizeWeight+GetR13S3Rank[False,lambdaRank]*R13S3Weight];

retVal+=R13testVal;

If[retVal <MinF2SqMinFuncRetVal,
(
MinF2SqMinSabLinearFuncRetVal=retVal;
sablF2SqMinSabLinearFunc=sabLinear;
IdxF2SqMinSabLinearFunc=cntF2SqMinSabLinearFunc;
)
];

If[Mod[cntF2SqMinSabLinearFunc,f2SqMinSabLinearPrintStepVal]==0,
(
stl=sablF2SqMinSabLinearFunc;

Print["F2SqMinSabLinearFunc::cntF2SqMinSabLinearFunc = ", cntF2SqMinSabLinearFunc, ", cntGradF2SqMinSabLinearFunc = ", cntGradF2SqMinSabLinearFunc, ", retVal = ", retVal];
Print["   ...   "];
Print["F2SqMinSabLinearFunc::Known minimum run counter is: ", IdxF2SqMinSabLinearFunc];
Print["F2SqMinSabLinearFunc::Minimum value = ", MinF2SqMinSabLinearFuncRetVal];
(* Print["    sablF2SqMinSabLinearFunc = ", sablF2SqMinSabLinearFunc]; *)
Print["F2SqMinSabLinearFunc::FP2All[st]^2 = ", FP2All[st]^2];
Print["Initial: Mean = ",MeanLinear[SIL],", Standard Deviation = ",StdDevLinear[SIL]];
Print["Min Value: Mean = ",N[MeanLinear[stl]],", Standard Deviation = ",N[StdDevLinear[stl]]];

PrintResults[stl,"F2SqMinSabLinearFunc"];

PrintTimeUsed[];
)
];

Return[retVal];
]/; (VectorQ[sabLinearVal,NumericQ]);

(* ==============================================*)

cntGradF2SqMinSabLinearFunc=0;
GradF2SqMinSabLinearFunc[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,rawOpts___]:=Module[{gradVal,sab,precisionDigitsVal,opts,retVal},
cntGradF2SqMinSabLinearFunc++;
opts=ProcessOptions[rawOpts];
precisionDigitsVal=PrecisionDigits/.opts /.Options[STMMinF2];
sab=ToMatrix[SetPrecision[sabLinear,precisionDigitsVal]];
gradVal=2*FP2All[sab]*GradP2OnlyAll[sab,False];
retVal=TangentialGradient[gradVal,returnTangentialGradient];
(* Print["GradF2SqMinSabLinearFunc::retVal = ", retVal]; *)
Return[retVal];
]/; (VectorQ[sabLinear,NumericQ]);

(* ==============================================*)

PrintResults[stl_?VectorQ,descr_]:=Module[{st,stlSorted,solValue,lambda,eXXT,gradVal,gradSortedVal,idx,lambdaSorted,lambdaRank,R13testVal},
st=ToMatrix[stl];
stlSorted=Sort[stl];
solValue=FindUncorrelatedCoordinates[st];
lambda=GetLambda[solValue];
eXXT=GetE[solValue];
gradVal=GradF2SqMinSabLinearFunc[stl,True,rawOpts];
gradSortedVal=SortByFirst[stl,gradVal];
lambdaSorted=GetLambdaSorted[solValue];
lambdaRank=N[LambdaRankFunc[lambdaSorted]];

R13testVal=Min[GetR13SizeRank[True,lambdaRank]*R13SizeWeight+GetR13S3Rank[True,lambdaRank]*R13S3Weight,GetR13SizeRank[False,lambdaRank]*R13SizeWeight+GetR13S3Rank[False,lambdaRank]*R13S3Weight];

Print[ToString[descr],"::Sorted values of ALL sab, Lambda and x sigma range."];
Print[Plot[SValFunc[idx,stlSorted],{idx,1,NA},Evaluate[defaultPltOpts]],Plot[LambdaFunc[idx,solValue],{idx,1,NNN},Evaluate[defaultPltOpts]],Plot[xRangeSigmaFunc[idx,solValue],{idx,1,NNN},Evaluate[defaultPltOpts]]];

Print["Sorted values of gradient."];
Print[Plot[SValFunc[idx,gradSortedVal],{idx,1,NA},Evaluate[defaultPltOpts]]];

Print[ToString[descr],"::lambdaRank = ", lambdaRank];
Print[ToString[descr],"::R13testVal = ", R13testVal];
Print[ToString[descr],"::lambdaSorted = ", N[lambdaSorted]];
];

