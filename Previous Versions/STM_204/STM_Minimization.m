(* :Author:Konstantin K.Konstantinov *)
(* :Summary:Various minimization algorithms. *)
(* :Copyright:K^3,2013 *)
(* :Version: Revision: 1.12.001, Date:2013/10/18 *)
(* :Mathematica Version:7.0-9.0 *)
(* ==============================================*)

RFCMethodMin=1;
RFCMethodFindMinimum=1;
RFCMethodNMinimize=2;
RFCMethodSingleRun=3;
RFCMethodMax=3;


FCMFMethodMin=1;
FCMFMethodODE=1;
FCMFMethodFindMinimum=2;
FCMFMethodFindRoot=3;
FCMFMethodMax=3;

P8ModelTypeMin=1;
P8ModelType8s=1;
P8ModelType8sX4=2;
P8ModelType8s1=3;
P8ModelTypeMax=3;

Options[STMMinimization]={PlotODEResults -> True, ReturnTangentialGradient -> False, DoPrintInfo -> True, NoOfTsteps -> 3,PlotCombinedResults -> True, SolveForCoeff -> True,DoNotConstrainCoefficients -> False,InvalidCoeffWeight -> 10^6,PositiveCoeffF4Sign -> True,IdxSolveCoeffF4 :>idxD4T0Cababefef,PositiveCoeffF2Sign -> False,IdxSolveCoeffF2 :>idxD2T2Cabbd,RescaleOutOfRangeCoeff -> True,UseP4Model -> True,UseP2Model -> True,OutputLambda -> True,OutputAllTIdx -> True,OutputF4P -> True, UseSetPrecision -> True, PrecisionDigits -> 100,WorkingPrecisionDigits -> 200,MaximumIterations -> 20,FCMFMethod -> FCMFMethodFindMinimum,RFCMethod -> RFCMethodNMinimize,P8ModelType -> P8ModelType8s,UseStepMonitor -> False};

RunFindCoefficientsCounter=0;

RunFindCoefficients[initCoeffF2Val_?VectorQ,initCoeffF4Val_?VectorQ,tMax_?NumericQ,R13SizeWeight_?NumericQ,R13S3Weight_?NumericQ,ModelDivergenceWeight_?NumericQ,rawOpts___]:=Module[{sol,varListF4,varListF2,ii, initCoeffF4,initCoeffF2,opts,constrF4,constrF2,findMinInitValsF4,findMinInitValsF2,solveForCoeffVal,idxSolveCoeffF4Val,coeffF4Sign,invalidCoeffWeightVal,idxSolveCoeffF2Val,coeffF2Sign,useP2ModelVal,useP4ModelVal,findMinInitVals,useSetPrecisionVal,precisionDigitsVal,rfcMethodVal,constrLst,funcAndConstrLst,varList,doNotConstrainCoefficientsVal},

opts=ProcessOptions[rawOpts];
Print["RunFindCoefficients::$MaxPrecision = ", $MaxPrecision];
$MaxPrecision=Infinity;

solveForCoeffVal=SolveForCoeff /.opts /.Options[STMMinimization];

idxSolveCoeffF4Val=IdxSolveCoeffF4/.opts /.Options[STMMinimization];
coeffF4Sign=If[(PositiveCoeffF4Sign/.opts /.Options[STMMinimization]),1,-1,1];
useP4ModelVal = UseP4Model/.opts /.Options[STMMinimization];

idxSolveCoeffF2Val=IdxSolveCoeffF2/.opts /.Options[STMMinimization];
coeffF2Sign=If[(PositiveCoeffF2Sign/.opts /.Options[STMMinimization]),1,-1,1];
useP2ModelVal = UseP2Model/.opts /.Options[STMMinimization];

invalidCoeffWeightVal=InvalidCoeffWeight/.opts /.Options[STMMinimization];
useSetPrecisionVal=UseSetPrecision/.opts /.Options[STMMinimization];
precisionDigitsVal=PrecisionDigits/.opts /.Options[STMMinimization];
rfcMethodVal=RFCMethod/.opts /.Options[STMMinimization];
doNotConstrainCoefficientsVal=DoNotConstrainCoefficients/.opts /.Options[STMMinimization];

If[rfcMethodVal == RFCMethodSingleRun,
(
Print["RunFindCoefficients::Single run."];
solveForCoeffVal=False;
)
];

If[rfcMethodVal<RFCMethodMin || rfcMethodVal>RFCMethodMax,
(
Print["RunFindCoefficients::RFCMethod is out of range. Value = ",rfcMethodVal];
Return[Indeterminate];
)
];

If[!useP2ModelVal && !useP4ModelVal,
(
Print["RunFindCoefficients::At least one of F4 or F2 models must be included."];
Return[Indeterminate];
)
];

If[Length[initCoeffF2Val] !=NoOfTermsF2,
(
Print["RunFindCoefficients::initCoeffF2Val has incorrect length = ", Length[initCoeffF2Val]];
Return[Indeterminate];
)
];

If[Length[initCoeffF4Val] !=NoOfTermsF4,
(
Print["RunFindCoefficients::initCoeffF4Val has incorrect length = ", Length[initCoeffF4Val]];
Return[Indeterminate];
)
];

initCoeffF2=initCoeffF2Val;
initCoeffF4=initCoeffF4Val;

If[useSetPrecisionVal,
(
initCoeffF2=SetPrecision[initCoeffF2,precisionDigitsVal];
initCoeffF4=SetPrecision[initCoeffF4,precisionDigitsVal];
)
];

initCoeffF2=Normalize[initCoeffF2];
initCoeffF4=Normalize[initCoeffF4];

(* Variables *)
varListF2=Table[ToExpression["varF2n" <> TermMatrixF2[[ii]]],{ii,1,NoOfTermsF2}];
varListF4=Table[ToExpression["varF4n" <> TermMatrix[[ii]]],{ii,1,NoOfTermsF4}];

If[!useP2ModelVal,
(
initCoeffF2=0*initCoeffF2;
varListF2=0*varListF2;
)
];

If[!useP4ModelVal,
(
initCoeffF4=0*initCoeffF4;
varListF4=0*varListF4;
)
];

(*
Print["RunFindCoefficients::Normalized initial coefficients F2 are: ", initCoeffF2];
Print["RunFindCoefficients::Normalized initial coefficients F4 are: ", initCoeffF4];
*)

If[solveForCoeffVal,
(
varListF2=Delete[varListF2,idxSolveCoeffF2Val];
initCoeffF2=Delete[initCoeffF2,idxSolveCoeffF2Val];

varListF4=Delete[varListF4,idxSolveCoeffF4Val];
initCoeffF4=Delete[initCoeffF4,idxSolveCoeffF4Val];
)
];

(*
Print["initCoeffF2 = ", initCoeffF2];
Print["varListF2 = ", varListF2];

Print["initCoeffF4 = ", initCoeffF4];
Print["varListF4 = ", varListF4];
*)

varList={};
(* Initial Values for Find Minimum *)
If[useP2ModelVal,
(
findMinInitValsF2=Table[{varListF2[[ii]],initCoeffF2[[ii]]},{ii,1,Length[varListF2]}];
varList=Join[varList,varListF2];
),
(
findMinInitValsF2={};

)
];

If[useP4ModelVal,
(
findMinInitValsF4=Table[{varListF4[[ii]],initCoeffF4[[ii]]},{ii,1,Length[varListF4]}];
varList=Join[varList,varListF4];
),
(
findMinInitValsF4={};
)
];
(*
Print["findMinInitValsF2 = ", findMinInitValsF2];
Print["findMinInitValsF4 = ", findMinInitValsF4];
*)
findMinInitVals=Join[findMinInitValsF2,findMinInitValsF4];
(*
Print["findMinInitVals = ", findMinInitVals];
Print["varList = ", varList];
*)
(* ============================================= *)
(* Main minimization call. *)

If[rfcMethodVal == RFCMethodSingleRun,
(
Print["RunFindCoefficients::Calling FindCoefficientsMinFunc"];
sol=FindCoefficientsMinFunc[initCoeffF2,initCoeffF4,tMax,R13SizeWeight,R13S3Weight,ModelDivergenceWeight,SolveForCoeff -> False,rawOpts];
Return[sol];
)
];

If[solveForCoeffVal,
(
Print["RunFindCoefficients::Running unconstrained minimization while solving for two coefficients."];

If[rfcMethodVal == RFCMethodFindMinimum,
(
Print["RunFindCoefficients::Using FindMinimum wiht ConjugateGradient method."];
sol=FindMinimum[FindCoefficientsMinFunc[varListF2,varListF4,tMax,R13SizeWeight,R13S3Weight,ModelDivergenceWeight,rawOpts],findMinInitVals,Method -> "ConjugateGradient"];
)
];

If[rfcMethodVal == RFCMethodNMinimize,
(
Print["RunFindCoefficients::Using NMinimize."];
Print["RunFindCoefficients::TODO::Not Implemented yet!!!"];
Return[Indeterminate];
)
];
),
(
If[!doNotConstrainCoefficientsVal,
(
Print["RunFindCoefficients::Running constrained minimization."];

If[useP2ModelVal,
(
constrF2={Sum[varListF2[[ii]]^2,{ii,1,NoOfTermsF2}]-1 ==0};
Print["constrF2 = ", constrF2];
),
(
constrF2={};
)
];

If[useP4ModelVal,
(
constrF4={Sum[varListF4[[ii]]^2,{ii,1,NoOfTermsF4}]-1 ==0};
Print["constrF4 = ", constrF4];
),
(
constrF4={};
)
];

constrLst=Join[constrF2,constrF4];
(* Print["constrLst = ", constrLst]; *)

funcAndConstrLst=Join[{FindCoefficientsMinFunc[varListF2,varListF4,tMax,R13SizeWeight,R13S3Weight,ModelDivergenceWeight,rawOpts]},constrLst];
(* Print["funcAndConstrLst = ", funcAndConstrLst]; *)

),
(
Print["RunFindCoefficients::Running fully unconstrained minimization while rescaling coefficients."];
funcAndConstrLst=FindCoefficientsMinFunc[varListF2,varListF4,tMax,R13SizeWeight,R13S3Weight,ModelDivergenceWeight,rawOpts];
)
];

If[rfcMethodVal == RFCMethodFindMinimum,
(
Print["RunFindCoefficients::Using FindMinimum."];
sol=FindMinimum[funcAndConstrLst,findMinInitVals,WorkingPrecision -> 50];
)
];

If[rfcMethodVal == RFCMethodNMinimize,
(
Print["RunFindCoefficients::Using NMinimize."];
sol=NMinimize[funcAndConstrLst,varList,WorkingPrecision -> 50];
)
];
)
];

Print["RunFindCoefficients::RunFindCoefficientsCounter = ", RunFindCoefficientsCounter, ", sol = ", sol];

Return[sol];
];

MinFindCoefficientsRetVal=10^50;
CoeffF2Min={};
CoeffF4Min={};
LambdaSortedMin={};
MinIdx=0;
MinLambdaRank={};

FindCoefficientsMinFunc[initCoeffF2Val_?VectorQ,initCoeffF4Val_?VectorQ,tMax_?NumericQ,R13SizeWeight_?NumericQ,R13S3Weight_?NumericQ,ModelDivergenceWeight_?NumericQ,rawOpts___]:=Module[{retVal,opts,sol,returnTangentialGradientVal,f,fDomain,tPlotMultiplier,tPlot,plotODEResultsVal,divergenceTestVal,st,stl,solValue,lambdaSorted,lambdaRank,R13testVal,doPrintInfoVal,noOfTsteps,pltOpts,DeltaT,plotCombinedResultsVal,stlSortedTbl,solValueTbl,deltaLambdaSortedTbl,tIdx,tVal,stlSorted,lambda,eXXT,gradLinearFunc,gValLinear,gValMatrix,deltaLambda,deltaLambdaSorted,sValFuncTbl,lambdaFuncTbl,deltaLambdaFuncTbl,xRangeFuncTbl,xRangeSigmaFuncTbl,solveForCoeffVal,idxSolveCoeffF4Val,coeffF4Sign,invalidCoeffWeightVal,initCoeffF4,initCoeffF2,coeffF4Sq,coeffF4Val,coeffF2Sq,coeffF2Val,coeffOutOfRangeTestVal,ii,idx,idxSolveCoeffF2Val,coeffF2Sign,useP4ModelVal,useP2ModelVal,outputLambdaVal,outputAllTIdxVal,outputF4PVal(* ,useFindMinimumVal *),findMinInitVals,useSetPrecisionVal,precisionDigitsVal,gradVal,gradSortedVal,fcmfMethodVal,funcVal,stpCount,workingPrecisionDigitsVal, maximumIterationsVal,fLinearFunc,rescaleOutOfRangeCoeffVal,useStepMonitorVal,p8ModelTypeVal},

opts=ProcessOptions[rawOpts];
(* Print["FindCoefficientsMinFunc::$MaxPrecision = ", $MaxPrecision]; *)
$MaxPrecision=Infinity;
noOfTsteps=NoOfTsteps /.opts /.Options[STMMinimization];
plotODEResultsVal=PlotODEResults /.opts /.Options[STMMinimization];
returnTangentialGradientVal=ReturnTangentialGradient  /.opts /.Options[STMMinimization];
doPrintInfoVal=DoPrintInfo  /.opts /.Options[STMMinimization];
plotCombinedResultsVal=PlotCombinedResults  /.opts /.Options[STMMinimization];

solveForCoeffVal=SolveForCoeff /.opts /.Options[STMMinimization];

idxSolveCoeffF4Val=IdxSolveCoeffF4/.opts /.Options[STMMinimization];
coeffF4Sign=If[(PositiveCoeffF4Sign/.opts /.Options[STMMinimization]),1,-1,1];
useP4ModelVal = UseP4Model/.opts /.Options[STMMinimization];

idxSolveCoeffF2Val=IdxSolveCoeffF2/.opts /.Options[STMMinimization];
coeffF2Sign=If[(PositiveCoeffF2Sign/.opts /.Options[STMMinimization]),1,-1,1];
useP2ModelVal = UseP2Model/.opts /.Options[STMMinimization];

invalidCoeffWeightVal=InvalidCoeffWeight/.opts /.Options[STMMinimization];
outputLambdaVal=OutputLambda/.opts /.Options[STMMinimization];
outputAllTIdxVal=OutputAllTIdx/.opts /.Options[STMMinimization];
outputF4PVal=OutputF4P/.opts /.Options[STMMinimization];
(* useFindMinimumVal=UseFindMinimum/.opts /.Options[STMMinimization]; *)
fcmfMethodVal=FCMFMethod/.opts /.Options[STMMinimization];
workingPrecisionDigitsVal = WorkingPrecisionDigits/.opts /.Options[STMMinimization];
maximumIterationsVal=MaximumIterations/.opts /.Options[STMMinimization];
(* useP2P4P8sX4val *)
p8ModelTypeVal=P8ModelType/.opts /.Options[STMMinimization];
rescaleOutOfRangeCoeffVal=RescaleOutOfRangeCoeff/.opts /.Options[STMMinimization];
useStepMonitorVal=UseStepMonitor/.opts /.Options[STMMinimization];

(*Print["rescaleOutOfRangeCoeffVal = ", rescaleOutOfRangeCoeffVal];*)

If[fcmfMethodVal < FCMFMethodMin || fcmfMethodVal > FCMFMethodMax,
(
Print["FindCoefficientsMinFunc::FCMFMethod is out of range. Value = ",fcmfMethodVal];
Return[Indeterminate];
)
 ];

useSetPrecisionVal=UseSetPrecision/.opts /.Options[STMMinimization];
precisionDigitsVal=PrecisionDigits/.opts /.Options[STMMinimization];

If[(fcmfMethodVal==FCMFMethodFindMinimum ||fcmfMethodVal==FCMFMethodFindRoot )  &&  !UseP8sGrad,
(
Print["FindCoefficientsMinFunc::FCMFMethod, which requires UseP8sGrad, was requested but UseP8sGrad is not set. Ignoring FCMFMethod."];
fcmfMethodVal=FCMFMethodODE;
)
];

RunFindCoefficientsCounter++;

coeffOutOfRangeTestVal=0;

pltOpts={PlotRange -> All, Frame -> True, PlotStyle -> Thick, GridLines -> Automatic, PlotPoints -> 200, Method -> {PlotDivision -> 1}, ImageSize -> IMAGESIZE};

Print[strSeparator];
Print[strSeparator];

If[doPrintInfoVal,
(
Print["FindCoefficientsMinFunc is called: ", RunFindCoefficientsCounter," time(s)."];
(*
Print["initCoeffF2Val = ", N[initCoeffF2Val]];
Print["initCoeffF4Val = ", N[initCoeffF4Val]];
Print["returnTangentialGradientVal = ", returnTangentialGradientVal];
*)
)
];

If[!solveForCoeffVal,
(
Print["NOT solving for coefficients. "];
If[useP2ModelVal,
(
initCoeffF2 = initCoeffF2Val;
),
(
initCoeffF2 = 0*initCoeffF2Val;
)
];

If[useP4ModelVal,
(
initCoeffF4 = initCoeffF4Val;
),
(
initCoeffF4 = 0*initCoeffF4Val;
)
];
),
(
Print["Solving for coefficients."];
If[useP2ModelVal,
(
initCoeffF2=initCoeffF2Val;
coeffF2Sq=1-Sum[initCoeffF2[[ii]]^2,{ii,1,NoOfTermsF2-1}];

If[coeffF2Sq>=0,
(
coeffF2Val=coeffF2Sign*Sqrt[coeffF2Sq];
),
(
coeffF2Val=0;
If[rescaleOutOfRangeCoeffVal,
(
Print["FindCoefficientsMinFunc::CoeffF2 are out of range. Rescaling."];
initCoeffF2=Normalize[initCoeffF2];
),
(
coeffOutOfRangeTestVal+=-coeffF2Sq*invalidCoeffWeightVal;
)
];
)
];

initCoeffF2=Insert[initCoeffF2,coeffF2Val,idxSolveCoeffF2Val];
),
(
initCoeffF2=0*Insert[initCoeffF2Val,0,idxSolveCoeffF2Val];
)
];

If[useP4ModelVal,
(
initCoeffF4=initCoeffF4Val;
coeffF4Sq=1-Sum[initCoeffF4[[ii]]^2,{ii,1,NoOfTermsF4-1}];
If[coeffF4Sq>=0,
(
coeffF4Val=coeffF4Sign*Sqrt[coeffF4Sq];
),
(
coeffF4Val=0;
If[rescaleOutOfRangeCoeffVal,
(
Print["FindCoefficientsMinFunc::CoeffF4 are out of range. Rescaling."];
initCoeffF4=Normalize[initCoeffF4];
),
(
coeffOutOfRangeTestVal+=-coeffF4Sq*invalidCoeffWeightVal;
)
];
)
];
initCoeffF4=Insert[initCoeffF4,coeffF4Val,idxSolveCoeffF4Val];
),
(
initCoeffF4=0*Insert[initCoeffF4Val,0,idxSolveCoeffF4Val];
)
];
)
];

If[useSetPrecisionVal,
(
Print["FindCoefficientsMinFunc::Setting precision for coefficients and rescaling."];
initCoeffF2=Normalize[SetPrecision[initCoeffF2,precisionDigitsVal]];
initCoeffF4=Normalize[SetPrecision[initCoeffF4,precisionDigitsVal]];
)
];

If[doPrintInfoVal,Print["FindCoefficientsMinFunc::initCoeffF2 = ", N[initCoeffF2]]];
If[doPrintInfoVal,Print["FindCoefficientsMinFunc::initCoeffF4 = ", N[initCoeffF4]]];

Print["Sum of all initCoeffF2[[ii]]^2 = ", Sum[initCoeffF2[[ii]]^2,{ii,1,NoOfTermsF2}]];
Print["Sum of all initCoeffF4[[ii]]^2 = ", Sum[initCoeffF4[[ii]]^2,{ii,1,NoOfTermsF4}]];

If[useP2ModelVal,
(
If[useP4ModelVal,
(
If[UseP8sGrad,
(

If[p8ModelTypeVal == P8ModelType8s,
(
Print["FindCoefficientsMinFunc::Using GradP2P4P8sAllLinearNumeric and FP2P4P8sAllLinearNumeric."];
gradLinearFunc=GradP2P4P8sAllLinearNumeric;
fLinearFunc=FP2P4P8sAllLinearNumeric;
)
];

If[p8ModelTypeVal == P8ModelType8sX4,
(
Print["FindCoefficientsMinFunc::Using GradP2P4P8sX4AllLinearNumeric and FP2P4P8sX4AllLinearNumeric."];
gradLinearFunc=GradP2P4P8sX4AllLinearNumeric;
fLinearFunc=FP2P4P8sX4AllLinearNumeric;
)
];

If[p8ModelTypeVal == P8ModelType8s1,
(
Print["FindCoefficientsMinFunc::Using GradP2P4P8s1AllLinearNumeric and FP2P4P8s1AllLinearNumeric."];
gradLinearFunc=GradP2P4P8s1AllLinearNumeric;
fLinearFunc=FP2P4P8s1AllLinearNumeric;
)
];
),
(
Print["FindCoefficientsMinFunc::Using GradP2P4AllLinear."];
gradLinearFunc=GradP2P4AllLinear;
fLinearFunc=Indeterminate;
)
];
),
(
If[UseP8sGrad,
(
Print["FindCoefficientsMinFunc::GradP2P8s is not yet implemented."];
Return[Indeterminate];
),
(
Print["FindCoefficientsMinFunc::Using GradP2AllLinear."];
gradLinearFunc=GradP2OnlyAllLinear;
fLinearFunc=Indeterminate;
)
];
)
];
),
(
If[useP4ModelVal,
(
If[UseP8sGrad,
(
Print["FindCoefficientsMinFunc::Using GradP4P8sAllLinearNumeric."];
gradLinearFunc=GradP4P8sAllLinearNumeric;
fLinearFunc=FP4P8sAllLinearNumeric;
),
(
Print["FindCoefficientsMinFunc::Using GradP4AllLinear."];
gradLinearFunc=GradP4AllLinear;
fLinearFunc=FP4AllLinearNumeric;
)
];
),
(
Print["FindCoefficientsMinFunc::Either F2 or F4 model must be specified."];
Return[Indeterminate];
)
];
)
];
(*
Print["gradLinearFunc = ", Definition[gradLinearFunc]];
Print["fLinearFunc = ", Definition[fLinearFunc]];
*)
UpdateAllCoeffF2[initCoeffF2];
UpdateAllCoeffF4[initCoeffF4];

If[RunFindCoefficientsCounter==1,
(
Print["Initial function value FP4AllLinearNumeric[SIL] = ", FP4AllLinearNumeric[SIL]];
(*
Print["Initial gradient value GradP4AllLinearNumeric[SIL,False] = ", GradP4AllLinearNumeric[SIL,False]];
*)
solValue=FindUncorrelatedCoordinates[SI];
PlotUncorrelatedCoordinates[NNN,solValue,RunFindCoefficientsCounter,"FindCoefficientsMinFunc::Initial values."];
)
];

(* ============================================= *)

If[fcmfMethodVal == FCMFMethodFindMinimum  || fcmfMethodVal == FCMFMethodFindRoot ,
(
If[useSetPrecisionVal,
(
Print["Setting precision for initial values. "];
findMinInitVals=Table[{SL[[ii]],SetPrecision[SIL[[ii]],precisionDigitsVal]},{ii,1,NA}];
),
(
findMinInitVals=Table[{SL[[ii]],SIL[[ii]]},{ii,1,NA}];
)
];
)
];

If[fcmfMethodVal == FCMFMethodFindMinimum,
(
Print["FindCoefficientsMinFunc::Using FindMinimum."];
(* Print["findMinInitVals = ", findMinInitVals]; *)

stpCount=1;
Print["FindCoefficientsMinFunc::Calling FindMinimum..."];

If[useStepMonitorVal,
(
sol=FindMinimum[fLinearFunc[SL],findMinInitVals,Gradient:>gradLinearFunc[SL,False], WorkingPrecision -> workingPrecisionDigitsVal, PrecisionGoal -> Infinity,Method -> "ConjugateGradient",MaxIterations -> maximumIterationsVal,StepMonitor :> Print["Step count = ",stpCount++, ", fLinearFunc[SL] = ", fLinearFunc[SL]]];
),
(
sol=FindMinimum[fLinearFunc[SL],findMinInitVals,Gradient:>gradLinearFunc[SL,False], WorkingPrecision -> workingPrecisionDigitsVal, PrecisionGoal -> Infinity,Method -> "ConjugateGradient",MaxIterations -> maximumIterationsVal,StepMonitor :> (stpCount++)];
)
];

(* Print["FindCoefficientsMinFunc::sol = ", sol]; *)
Print["FindCoefficientsMinFunc::stpCount = ", stpCount, ", cntNumeric = ", cntNumeric, ", cntGradNumeric = ", cntGradNumeric];
cntNumeric=0;
cntGradNumeric=0;
PrintTimeUsed[];

stl=SL /. sol[[2]];
)
];

(* ============================================= *)

If[fcmfMethodVal == FCMFMethodFindRoot,
(
Print["FindCoefficientsMinFunc::Using FindRoot."];
funcVal=-1/(2*coeffP8sGrad);

sol=FindRoot[FP4AllLinearNumeric[SL] == funcVal,findMinInitVals];
Print["FindCoefficientsMinFunc::cntFP4AllLinearNumeric = ", cntFP4AllLinearNumeric];
cntFP4AllLinearNumeric=0;

PrintTimeUsed[];
stl=SL /. sol;
)
];

(* ============================================= *)

If[fcmfMethodVal == FCMFMethodFindMinimum  || fcmfMethodVal == FCMFMethodFindRoot ,
(
st=ToMatrix[stl];
Print["FindCoefficientsMinFunc::fLinearFunc[stl] = ", fLinearFunc[stl]];
Print["FindCoefficientsMinFunc::FP4All[st] = ", FP4All[st]];
Print["FindCoefficientsMinFunc::FP2All[st] = ", FP2All[st]];

stlSorted=Sort[stl];
solValue=FindUncorrelatedCoordinates[st];
lambda=GetLambda[solValue];
eXXT=GetE[solValue];
gradVal=gradLinearFunc[stl,False];
gradSortedVal=SortByFirst[stl,gradVal];

(*
gValLinear=GradientMultiplier*gradVal;
gValMatrix=ToMatrix[gValLinear];
deltaLambda=CalculateDeltaLambda[gValMatrix,eXXT];
deltaLambdaSorted=SortByFirst[lambda,deltaLambda];
*)

Print["Sorted values of ALL sab, Lambda and x sigma range."];
Print[Plot[SValFunc[idx,stlSorted],{idx,1,NA},Evaluate[defaultPltOpts]],Plot[LambdaFunc[idx,solValue],{idx,1,NNN},Evaluate[defaultPltOpts]],Plot[xRangeSigmaFunc[idx,solValue],{idx,1,NNN},Evaluate[defaultPltOpts]]];

Print["Sorted values of gradient."];
Print[Plot[SValFunc[idx,gradSortedVal],{idx,1,NA},Evaluate[defaultPltOpts]]];

(*
Print["Sorted values of ALL sab, Lambda and Delta Lambda."];
Print[Plot[SValFunc[idx,stlSorted],{idx,1,NA},Evaluate[defaultPltOpts]],Plot[LambdaFunc[idx,solValue],{idx,1,NNN},Evaluate[defaultPltOpts]],Plot[Re[DeltaLambdaFunc[idx,deltaLambdaSorted]],{idx,1,NNN},Evaluate[defaultPltOpts]]];

Print["x range and x sigma range."];
Print[Plot[xRangeFunc[idx,solValue],{idx,1,NNN},Evaluate[pltOpts]],Plot[xRangeSigmaFunc[idx,solValue],{idx,1,NNN},Evaluate[defaultPltOpts]]];
Print[strSeparatorSmall];
*)

lambdaSorted=GetLambdaSorted[solValue];
lambdaRank=N[LambdaRankFunc[lambdaSorted]];
Print["FindCoefficientsMinFunc::lambdaRank = ", lambdaRank];
Print["FindCoefficientsMinFunc::lambdaSorted = ", N[lambdaSorted]];

R13testVal=Min[GetR13SizeRank[True,lambdaRank]*R13SizeWeight+GetR13S3Rank[True,lambdaRank]*R13S3Weight,GetR13SizeRank[False,lambdaRank]*R13SizeWeight+GetR13S3Rank[False,lambdaRank]*R13S3Weight];

divergenceTestVal=ModelDivergenceWeight*0;
retVal=R13testVal+divergenceTestVal+coeffOutOfRangeTestVal;
Print["FindCoefficientsMinFunc::R13testVal = ", R13testVal, ", divergenceTestVal = ", divergenceTestVal, ", coeffOutOfRangeTestVal = ", coeffOutOfRangeTestVal, ", retVal = ",retVal];
)
];

(* ============================================= *)

If[fcmfMethodVal == FCMFMethodODE,
(
Print["FindCoefficientsMinFunc::Using RunNDSolve."];
sol=RunNDSolve[RunFindCoefficientsCounter,tMax,gradLinearFunc,returnTangentialGradientVal,ODEPrintInfo -> False];
f=sEvolution/. sol[[1]];
fDomain=f["Domain"][[1,2]];

tPlotMultiplier=0.99;
tPlot=tPlotMultiplier*fDomain;
Print["FindCoefficientsMinFunc::tPlot = ", tPlot];

Print["StdDevLinear[f[t]], MeanLinear[f[t]], and (MeanLinear[f[t]] / StdDevLinear[f[t]])."];
Print[Plot[Re[StdDevLinear[f[t]]],{t,0,tPlot},Evaluate[pltOpts]],Plot[MeanLinear[f[t]],{t,0,tPlot},Evaluate[pltOpts]],Plot[Re[MeanLinear[f[t]]/StdDevLinear[f[t]]],{t,0,tPlot},Evaluate[pltOpts]]];

If[outputF4PVal,
(
Print["FP4All."];
Print[Plot[FP4All[ToMatrix[f[t]]],{t,0,tPlot},Evaluate[pltOpts]]];
)
];

(* Print[Plot[{Re[StdDevLinear[f[t]]],Im[StdDevLinear[f[t]]]},{t,0,tPlot},Evaluate[pltOpts]]]; *)

DeltaT=tPlot/noOfTsteps;

(* ============================================= *)
(* Copied from STM_Tests::TestRunNDSolve *)

If[plotCombinedResultsVal,
(
stlSortedTbl=Table[Indeterminate,{ii,0,noOfTsteps}];
solValueTbl=Table[Indeterminate,{ii,0,noOfTsteps}];
deltaLambdaSortedTbl =Table[Indeterminate,{ii,0,noOfTsteps}];

Print["TestRunNDSolve::Creating table of results."];
For[tIdx=0,tIdx <= noOfTsteps,tIdx++,
(
tVal=DeltaT*tIdx;
stl=f[tVal];
st=ToMatrix[stl];

stlSorted=Sort[stl];
solValue=FindUncorrelatedCoordinates[st];
lambda=GetLambda[solValue];
eXXT=GetE[solValue];
gValLinear=GradientMultiplier*gradLinearFunc[stl,returnTangentialGradientVal];
gValMatrix=ToMatrix[gValLinear];
deltaLambda=CalculateDeltaLambda[gValMatrix,eXXT];
deltaLambdaSorted=SortByFirst[lambda,deltaLambda];

stlSortedTbl[[tIdx+1]]=stlSorted;
solValueTbl[[tIdx+1]]=solValue;
deltaLambdaSortedTbl[[tIdx+1]]=deltaLambdaSorted;

If[outputLambdaVal,
(
lambdaSorted=GetLambdaSorted[solValue];
Print["FindCoefficientsMinFunc::For tVal = ", tVal, ", lambdaSorted = ", N[lambdaSorted]];

If[outputAllTIdxVal,
(
Print["Sorted values of ALL sab, Lambda and Delta Lambda."];
Print[Plot[SValFunc[idx,stlSorted],{idx,1,NA},Evaluate[defaultPltOpts]],Plot[LambdaFunc[idx,solValue],{idx,1,NNN},Evaluate[defaultPltOpts]],Plot[Re[DeltaLambdaFunc[idx,deltaLambdaSorted]],{idx,1,NNN},Evaluate[defaultPltOpts]]];

Print["x range and x sigma range."];
Print[Plot[xRangeFunc[idx,solValue],{idx,1,NNN},Evaluate[pltOpts]],Plot[xRangeSigmaFunc[idx,solValue],{idx,1,NNN},Evaluate[defaultPltOpts]]];
Print[strSeparatorSmall];
)
];
)
];
)
];

Print["Sorted values of ALL sab, Lambda and Delta Lambda."];
sValFuncTbl=Table[SValFunc[idx,stlSortedTbl[[ii+1]]],{ii,0,noOfTsteps}];
lambdaFuncTbl=Table[LambdaFunc[idx,solValueTbl[[ii+1]]],{ii,0,noOfTsteps}];
deltaLambdaFuncTbl=Table[Re[DeltaLambdaFunc[idx,deltaLambdaSortedTbl[[ii+1]]]],{ii,0,noOfTsteps}];
xRangeFuncTbl=Table[xRangeFunc[idx,solValueTbl[[ii+1]]],{ii,0,noOfTsteps}];
xRangeSigmaFuncTbl=Table[xRangeSigmaFunc[idx,solValueTbl[[ii+1]]],{ii,0,noOfTsteps}];

Print[Plot[sValFuncTbl,{idx,1,NA},Evaluate[defaultPltOpts]],Plot[lambdaFuncTbl,{idx,1,NNN},Evaluate[defaultPltOpts]],Plot[deltaLambdaFuncTbl,{idx,1,NNN},Evaluate[defaultPltOpts]]];

Print["x range and x sigma range."];
Print[Plot[xRangeFuncTbl,{idx,1,NNN},Evaluate[pltOpts]],Plot[xRangeSigmaFuncTbl,{idx,1,NNN},Evaluate[defaultPltOpts]]];
)
];

(* ============================================= *)

stl=f[tPlot];
solValue=FindUncorrelatedCoordinates[st];
lambdaSorted=GetLambdaSorted[solValue];
lambdaRank=N[LambdaRankFunc[lambdaSorted]];
Print["FindCoefficientsMinFunc::lambdaRank = ", lambdaRank];

R13testVal=Min[GetR13SizeRank[True,lambdaRank]*R13SizeWeight+GetR13S3Rank[True,lambdaRank]*R13S3Weight,GetR13SizeRank[False,lambdaRank]*R13SizeWeight+GetR13S3Rank[False,lambdaRank]*R13S3Weight];

divergenceTestVal=ModelDivergenceWeight*If[fDomain <=tMax,((tMax/fDomain)-1),0];
retVal=R13testVal+divergenceTestVal+coeffOutOfRangeTestVal;
Print["FindCoefficientsMinFunc::R13testVal = ", R13testVal, ", divergenceTestVal = ", divergenceTestVal, ", coeffOutOfRangeTestVal = ", coeffOutOfRangeTestVal, ", retVal = ",retVal];
)
];

(* ============================================= *)

PrintTimeUsed[];

If[retVal <MinFindCoefficientsRetVal,
(
MinFindCoefficientsRetVal=retVal;
LambdaSortedMin=lambdaSorted;
CoeffF2Min=initCoeffF2;
CoeffF4Min=initCoeffF4;
MinIdx=RunFindCoefficientsCounter;
MinLambdaRank=lambdaRank;
)
];

If[doPrintInfoVal,
(
Print[strSeparatorSmall];
Print["FindCoefficientsMinFunc::Known minimum value is: = ", N[MinFindCoefficientsRetVal], " for run counter = ", MinIdx];
Print["    CoeffF2Min = ", N[CoeffF2Min]];
Print["    CoeffF4Min = ", N[CoeffF4Min]];
Print["    MinLambdaRank = ", N[MinLambdaRank]];
Print["    LambdaSortedMin = ", N[LambdaSortedMin]];
Print[Plot[IndexedVariableFunc[idx,LambdaSortedMin],{idx,1,NNN},Evaluate[defaultPltOpts]]];

Print[strSeparator];
)
];

Return[retVal];
]/; VectorQ[initCoeffF2Val,NumericQ]&& VectorQ[initCoeffF4Val,NumericQ];


