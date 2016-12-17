(* :Author: Konstantin K.Konstantinov *)
(* :Summary: Various test calculations for STM. *)
(* :Copyright: K^3, 2013 *)
(* :Version: Revision: 1.05.001, Date:2013/10/06 *)
(* :Mathematica Version: 7.0 - 9.0 *)
(* ============================================== *)

Options[STMTests]={PlotSeparateDeltaLambda -> False, PlotDetailedResults -> False, PlotCombinedResults -> True};

(* Test of RunNDSolve and related. *)
TestRunNDSolve[runID_?IntegerQ,tMax_?NumericQ,noOfTsteps_?IntegerQ,gradLinearFunc_,returnTangentialGradient_?BooleanQ,rawOpts___ ]:=Module[{sol,pltOpts,f,tPlotMultiplier,tPlot,t,DeltaT,tIdx,tVal,stl,st,solValue,lambda,eXXT,gValLinear,gValMatrix,deltaLambda,deltaLambdaSorted,idx,coeffIdx,stlSorted,stlSortedTbl,solValueTbl,lambdaTbl,deltaLambdaSortedTbl,sValFuncTbl,opts,plotSeparateDeltaLambdaVal,plotDetailedResultsVal,plotCombinedResultsVal,lambdaFuncTbl,deltaLambdaFuncTbl,xRangeFuncTbl,xRangeSigmaFuncTbl},

Print["TestRunNDSolve::Testing RunNDSolve for runID = ", runID];
Print["CoeffF4 = ", CoeffF4 // MatrixForm];
Print["CoeffF6 = ", CoeffF6 // MatrixForm];
Print["returnTangentialGradient = ", returnTangentialGradient];

opts=ProcessOptions[rawOpts];
plotSeparateDeltaLambdaVal=PlotSeparateDeltaLambda /.opts /.Options[STMTests];
plotDetailedResultsVal=PlotDetailedResults /.opts /.Options[STMTests];
plotCombinedResultsVal=PlotCombinedResults /.opts /.Options[STMTests];

sol=RunNDSolve[runID,tMax,gradLinearFunc,returnTangentialGradient,ODEPrintInfo -> False];
Print["Plot"];
f=sEvolution/. sol[[1]];
tPlotMultiplier=0.99;
tPlot=tPlotMultiplier*f["Domain"][[1,2]];
Print["tPlot = ", tPlot];

pltOpts={PlotRange -> All, Frame -> True, PlotStyle -> Thick, GridLines -> Automatic, PlotPoints -> 200, Method -> {PlotDivision -> 1}, ImageSize -> IMAGESIZE};

Print["StdDevLinear[f[t]], MeanLinear[f[t]], and (MeanLinear[f[t]] / StdDevLinear[f[t]])."];
Print[Plot[Re[StdDevLinear[f[t]]],{t,0,tPlot},Evaluate[pltOpts]],Plot[MeanLinear[f[t]],{t,0,tPlot},Evaluate[pltOpts]],Plot[Re[MeanLinear[f[t]]/StdDevLinear[f[t]]],{t,0,tPlot},Evaluate[pltOpts]]];

DeltaT=tPlot/noOfTsteps;
PrintTimeUsed[];

(* ============================================= *)

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
gValLinear=GradientMultiplier*gradLinearFunc[stl,returnTangentialGradient];
gValMatrix=ToMatrix[gValLinear];
deltaLambda=CalculateDeltaLambda[gValMatrix,eXXT];
deltaLambdaSorted=SortByFirst[lambda,deltaLambda];

stlSortedTbl[[tIdx+1]]=stlSorted;
solValueTbl[[tIdx+1]]=solValue;
deltaLambdaSortedTbl[[tIdx+1]]=deltaLambdaSorted;
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

If[plotDetailedResultsVal,
(
For[tIdx=0,tIdx <= noOfTsteps,tIdx++,
(
Print[strSeparatorSmall];
Print[strSeparatorSmall];
tVal=DeltaT*tIdx;
Print["tIdx = ", tIdx, ", t = ", tVal];

stl=f[tVal];
st=ToMatrix[stl];

Print["Plotting sorted values of sab."];
stlSorted=Sort[stl];
Print[Plot[SValFunc[idx,stlSorted],{idx,1,NA},Evaluate[defaultPltOpts]]];

Print["Finding uncorrelated coordinates."];
solValue=FindUncorrelatedCoordinates[st];
lambda=GetLambda[solValue];
eXXT=GetE[solValue];
PlotUncorrelatedCoordinates[NNN,solValue,0,"values for t = " <> ToString[tVal]];

Print["DeltaLambdaFunc for ALL."];
gValLinear=GradientMultiplier*gradLinearFunc[stl,returnTangentialGradient];
gValMatrix=ToMatrix[gValLinear];
deltaLambda=CalculateDeltaLambda[gValMatrix,eXXT];
deltaLambdaSorted=SortByFirst[lambda,deltaLambda];
(* Print[Plot[DeltaLambdaFunc[idx,deltaLambdaSorted],{idx,1,NNN},Evaluate[defaultPltOpts]]]; *)
Print[Plot[{Re[DeltaLambdaFunc[idx,deltaLambdaSorted]],Im[DeltaLambdaFunc[idx,deltaLambdaSorted]]},{idx,1,NNN},Evaluate[defaultPltOpts]]];
PrintTimeUsed[];

If[plotSeparateDeltaLambdaVal,
(
For[coeffIdx = 1, coeffIdx<= NoOfTermsF4, coeffIdx++, 
(
If[CoeffF4[[1,coeffIdx]] != 0,
(
Print["DeltaLambdaFunc for ", TermMatrix[[coeffIdx]], "."];
gValLinear=GradientMultiplier*CoeffF4[[1,coeffIdx]]*(GradP4FuncList[[coeffIdx]])[st,returnTangentialGradient,True];
gValMatrix=ToMatrix[gValLinear];
deltaLambda=CalculateDeltaLambda[gValMatrix,eXXT];
deltaLambdaSorted=SortByFirst[lambda,deltaLambda];
Print[Plot[{Re[DeltaLambdaFunc[idx,deltaLambdaSorted]],Im[DeltaLambdaFunc[idx,deltaLambdaSorted]]},{idx,1,NNN},Evaluate[defaultPltOpts]]];
PrintTimeUsed[];
)
];
)
];

If[CoeffF6[[1,idxD6T0Cabababababab]] !=0,
(
Print["DeltaLambdaFunc for Cabababababab."];
gValLinear=GradientMultiplier*CoeffF6[[1,idxD6T0Cabababababab]]*GradP6T0Cabababababab[st,returnTangentialGradient];
gValMatrix=ToMatrix[gValLinear];
deltaLambda=CalculateDeltaLambda[gValMatrix,eXXT];
deltaLambdaSorted=SortByFirst[lambda,deltaLambda];
Print[Plot[{Re[DeltaLambdaFunc[idx,deltaLambdaSorted]],Im[DeltaLambdaFunc[idx,deltaLambdaSorted]]},{idx,1,NNN},Evaluate[defaultPltOpts]]];
PrintTimeUsed[];
)
];
)
];
)
];
)
];

(* ============================================= *)

Return[sol];
];

(* ============================================= *)

(* Test various gradient numerically using brute force method. *)
TestF4GradientsNumeric[sol:{__},startRunID_?IntegerQ,endRunID_?IntegerQ]:=Module[{invIdx,tngIdx,runID,gValLinear,gValMatrix,deltaLambda,deltaLambdaSorted,lambda,eXXT},
Print["TestF4GradientsNumeric::Testing various gradients numerically using brute force method..."];

lambda=GetLambda[sol];
eXXT=GetE[sol];

For[invIdx=1,invIdx <=2, invIdx++,
(
If[invIdx ==1,ReturnInvariants=False,ReturnInvariants=True];
Print[strSeparator];

For[tngIdx=1,tngIdx <=2, tngIdx++,
(
If[tngIdx ==1,ReturnTangentialGradient=False,ReturnTangentialGradient=True];
Print[strSeparator];

For[runID=startRunID, runID <= endRunID,runID++,
(
Print["Running for runID = ", runID, If[ReturnInvariants,", invariant name = " <> InvariantMatrix[[runID]],", term name = " <> TermMatrix[[runID]]], ", TangentialGradient = ",ToString[ReturnTangentialGradient], If[ReturnInvariants, ", using INVARIANTS.", ", using COEFFICIENTS."]];

ResetCoeffF4[];
UpdateCoeffF4[runID,1];
gValLinear=Grad4NumericLinearFunc[SIL,ReturnInvariants,ReturnTangentialGradient];
gValMatrix=ToMatrix[gValLinear];
deltaLambda=CalculateDeltaLambda[gValMatrix,eXXT];
deltaLambdaSorted=SortByFirst[lambda,deltaLambda];

Print["DeltaLambdaFunc"];
Print[Plot[DeltaLambdaFunc[idx,deltaLambdaSorted],{idx,1,NNN},Evaluate[defaultPltOpts]]];
PrintTimeUsed[];
)
];
)
];
)
];

PrintTimeUsed[];
Return[];
];

(* ============================================= *)

(* Test all known symbolic (fast) expressions for F4 gradients. *)
TestF4GradientsFast[sol:{__}]:=TestF4GradientsFast[sol,1,23];
TestF4GradientsFast[sol:{__},startRunID_?IntegerQ,endRunID_?IntegerQ]:=Module[{lambda,eXXT,ReturnTangentialGradient,ReturnInvariants,tngIdx,gValLinear,gValMatrix,deltaLambda,deltaLambdaSorted,idx},
Print["TestF4GradientsFast::Testing various gradients..."];

lambda=GetLambda[sol];
eXXT=GetE[sol];

Print[strSeparator];
ReturnTangentialGradient=True;
ReturnInvariants=True;

Print[strSeparator];

For[tngIdx=1,tngIdx <=2, tngIdx++,
(
If[tngIdx ==1,ReturnTangentialGradient=False,ReturnTangentialGradient=True];
Print[strSeparator];

Print["Running for term name = ", TermMatrix[[idxD4T0Cabbddfaf]], ", TangentialGradient = ",ToString[ReturnTangentialGradient]];
gValLinear=Grad4T0Cabbddfaf[SI,ReturnTangentialGradient];
gValMatrix=ToMatrix[gValLinear];
deltaLambda=CalculateDeltaLambda[gValMatrix,eXXT];
deltaLambdaSorted=SortByFirst[lambda,deltaLambda];
Print["DeltaLambdaFunc"];
Print[Plot[DeltaLambdaFunc[idx,deltaLambdaSorted],{idx,1,NNN},Evaluate[defaultPltOpts]]];
PrintTimeUsed[];

Print["Running for EXT grad term name = ", TermMatrix[[idxD4T0Cabbddfaf]], ", TangentialGradient = ",ToString[ReturnTangentialGradient]];
gValLinear=Grad4T0CabbddfafEXT[SI,ReturnTangentialGradient];
gValMatrix=ToMatrix[gValLinear];
deltaLambda=CalculateDeltaLambda[gValMatrix,eXXT];
deltaLambdaSorted=SortByFirst[lambda,deltaLambda];
Print["DeltaLambdaFunc"];
Print[Plot[DeltaLambdaFunc[idx,deltaLambdaSorted],{idx,1,NNN},Evaluate[defaultPltOpts]]];
PrintTimeUsed[];

Print[strSeparatorSmall];
Print["Running for term name = ", TermMatrix[[idxD4T0Cabababab]], ", TangentialGradient = ",ToString[ReturnTangentialGradient]];
gValLinear=Grad4T0Cabababab[SI,ReturnTangentialGradient];
gValMatrix=ToMatrix[gValLinear];
deltaLambda=CalculateDeltaLambda[gValMatrix,eXXT];
deltaLambdaSorted=SortByFirst[lambda,deltaLambda];
Print["DeltaLambdaFunc"];
Print[Plot[DeltaLambdaFunc[idx,deltaLambdaSorted],{idx,1,NNN},Evaluate[defaultPltOpts]]];

Print[strSeparatorSmall];
Print["Running for term name = ", TermMatrixF6[[idxD6T0Cabababababab]], ", TangentialGradient = ",ToString[ReturnTangentialGradient]];
gValLinear=Grad6T0Cabababababab[SI,ReturnTangentialGradient];
gValMatrix=ToMatrix[gValLinear];
deltaLambda=CalculateDeltaLambda[gValMatrix,eXXT];
deltaLambdaSorted=SortByFirst[lambda,deltaLambda];
Print["DeltaLambdaFunc"];
Print[Plot[DeltaLambdaFunc[idx,deltaLambdaSorted],{idx,1,NNN},Evaluate[defaultPltOpts]]];

PrintTimeUsed[];
)
];

Print[strSeparator];
];

(* ============================================= *)

