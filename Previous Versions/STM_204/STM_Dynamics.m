(* :Author: Konstantin K.Konstantinov *)
(* :Summary: Dynamics for STM. *)
(* :Copyright: K^3, 2013 - 2014 *)
(* :Version:Revision: 2.00.001,Date: 2014/03/05 *)
(* :Mathematica Version: 7.0 - 9.0 *)
(* ============================================== *)

Options[STMDynamics]={UseGamma2Value -> True, Gamma2Value -> 1,UseSymplecticIntegrator -> True, UseUUL2 -> True,UseFullySymmetricPoly -> True,DoPlotSab -> False,TPlotMultiplier -> 1,MaxNoOfSteps -> 10^6,StepN -> 10^3,NAPlot -> 1,NAPlotMax -> 10,UseCompensatedSummation -> True, SPRKDifferenceOrder -> Automatic,PrintNDSolveCoord -> False};

(* ============================================== *)

Needs["DifferentialEquations`InterpolatingFunctionAnatomy`"];

(* ============================================== *)

InitializeDynamicModel[nn_?IntegerQ,rawOpts___]:=Module[{opts,ii,useGamma2ValueVal,gamma2ValueVal},
Print["Initializing models & related coefficients."];
InitializeModels[];

Print["Reinitializing variables for numeric run."];
Initialize[NumberOfElements,rawOpts];
PrintTimeUsed[];

Print["Initializing Dynamic Model..."];
opts=ProcessOptions[rawOpts];

useGamma2ValueVal=UseGamma2Value /.opts /.Options[STMDynamics];
gamma2ValueVal=Gamma2Value /.opts /.Options[STMDynamics];

If[useGamma2ValueVal,Print["Using gamma2 value = ", gamma2ValueVal]];

AlphaValList=Table[1,{ii,1,MaxMoment}];
GammaValList=Table[If[ii != 2,smAnalyticFunc[ii,NT,NS,TTTval],If[useGamma2ValueVal,gamma2ValueVal,mAnalyticFunc[ii,NT,NS,TTTval]]],{ii,1,MaxMoment}];

Print["NoOfElements = ", nn];
Print["NT = ", NT];
Print["NS = ", NS];
Print["TTT = ", TTTval];
Print["GammaValList = ", N[GammaValList] // MatrixForm];
Print["GammaValList (exact) = ", GammaValList // MatrixForm];
Print["AlphaValList = ", AlphaValList // MatrixForm];
];

(* ============================================= *)

DynamicsRun[tMaxVal_?NumericQ,gammaValList_,alphaValList_,rawOpts___]:=Module[{stepCount,tStartNDSolve,tPrev,tCurr,initValuesList,initValuesDerivativeList,kkk,vvv,hhh,tEndNDSolve,opts,useSymplecticIntegratorVal,useUUL2Val,useFullySymmetricPolyVal,sortArr,sol,sortArrD,lambdaVec,doPlotSabVal,tPlot,tPlotMultiplierVal,idxNAplot,maxNoOfStepsVal,stepNval,naPlotVal,naPlotMaxVal,useCompensatedSummationVal,SPRKDifferenceOrderVal,printNDSolveCoordVal,ifun,coords},
Print["DynamicsRun::Starting..."];
opts=ProcessOptions[rawOpts];

useSymplecticIntegratorVal=UseSymplecticIntegrator /.opts /.Options[STMDynamics];
useUUL2Val=UseUUL2 /.opts /.Options[STMDynamics];
useFullySymmetricPolyVal=UseFullySymmetricPoly /.opts /.Options[STMDynamics];
doPlotSabVal=DoPlotSab/.opts /.Options[STMDynamics];
tPlotMultiplierVal=TPlotMultiplier/.opts /.Options[STMDynamics];
maxNoOfStepsVal=MaxNoOfSteps/.opts /.Options[STMDynamics];
stepNval=StepN/.opts /.Options[STMDynamics];
naPlotVal=NAPlot/.opts /.Options[STMDynamics];
naPlotMaxVal=NAPlotMax/.opts /.Options[STMDynamics];
useCompensatedSummationVal=UseCompensatedSummation/.opts /.Options[STMDynamics];
SPRKDifferenceOrderVal=SPRKDifferenceOrder/.opts /.Options[STMDynamics];
printNDSolveCoordVal=PrintNDSolveCoord/.opts /.Options[STMDynamics];

stepCount=0;
tStartNDSolve=AbsoluteTime[];
tPrev=0;
tCurr=0;

Print["DynamicsRun::initValuesList"];
initValuesList=SIL;
(* Print["initValuesList = ", initValuesList]; *)

Print["DynamicsRun::initValuesDerivativeList"];
initValuesDerivativeList=Table[0,{ii,1,NA}];
(* Print["initValuesDerivativeList = ", initValuesDerivativeList]; *)
Clear[t,xxx,ppp];

Print["DynamicsRun::Calling NDSolve."];

If[useSymplecticIntegratorVal,
(
Print["DynamicsRun::Using SymplecticPartitionedRungeKutta method..."];
sol=NDSolve[{D[xxx[t],{t,1}]== ppp[t],D[ppp[t],{t,1}]==-DUUAllLinearNumeric[xxx[t],gammaValList,alphaValList,useUUL2Val,useFullySymmetricPolyVal],xxx[0]== initValuesList,ppp[0]== initValuesDerivativeList},{xxx,ppp},{t,0,tMaxVal},Method->{"SymplecticPartitionedRungeKutta","DifferenceOrder"->SPRKDifferenceOrderVal,"PositionVariables"->{xxx}},MaxSteps -> maxNoOfStepsVal,"CompensatedSummation"->useCompensatedSummationVal,
StepMonitor :> 
(
If[Mod[stepCount,stepNval]==0,
(
tCurr=t;
tEndNDSolve=AbsoluteTime[];
Print[strSeparatorSmall];
kkk=((ppp[t]) . (ppp[t])/2);
vvv=UUAllLinear[xxx[t],gammaValList,alphaValList,useUUL2Val,useFullySymmetricPolyVal];
hhh=kkk+vvv;
Print["stepCount = ",stepCount, ", t = ", t,", H = ", hhh,", time used = ",(tEndNDSolve-tStartNDSolve)];
sortArr=Sort[xxx[t]];
sortArrD=SortByFirst[xxx[t],ppp[t]];
lambdaVec=CalculateLambdaSorted[ToMatrix[xxx[t]]];
(* Print[Plot[{hhh,kkk,vvv},{t,tPrev,tCurr},Evaluate[defaultPltOpts]]]; *)
Print[Plot[IndexedVariableFunc[idx,lambdaVec],{idx,1,NNN},Evaluate[defaultPltOpts]],Plot[IndexedVariableFunc[idx,sortArr],{idx,1,NA},Evaluate[defaultPltOpts]],Plot[IndexedVariableFunc[idx,sortArrD],{idx,1,NA},Evaluate[defaultPltOpts]]];
tPrev=t;
)
];
stepCount++;
)
];

f=xxx/. sol[[1]];
fp=ppp/. sol[[1]];
),
(
Print["DynamicsRun::Using automatic method..."];
sol=NDSolve[{D[xxx[t],{t,2} ]== -DUUAllLinearNumeric[xxx[t],gammaValList,alphaValList,useUUL2Val,useFullySymmetricPolyVal],xxx[0]== initValuesList,xxx'[0]== initValuesDerivativeList},xxx,{t,0,tMaxVal},MaxSteps -> maxNoOfStepsVal,
StepMonitor :> 
(
If[Mod[stepCount,stepNval]==0,
(
tCurr=t;
tEndNDSolve=AbsoluteTime[];
Print[strSeparatorSmall];
kkk=((xxx'[t]) . (xxx'[t])/2);
vvv=UUAllLinear[xxx[t],gammaValList,alphaValList,useUUL2Val,useFullySymmetricPolyVal];
hhh=kkk+vvv;
Print["stepCount = ",stepCount, ", t = ", t,", H = ", hhh,", time used = ",(tEndNDSolve-tStartNDSolve)];
sortArr=Sort[xxx[t]];
sortArrD=SortByFirst[xxx[t],xxx'[t]];
lambdaVec=CalculateLambdaSorted[ToMatrix[xxx[t]]];
(* Print[Plot[{hhh,kkk,vvv},{t,tPrev,tCurr},Evaluate[defaultPltOpts]]]; *)
Print[Plot[IndexedVariableFunc[idx,lambdaVec],{idx,1,NNN},Evaluate[defaultPltOpts]],Plot[IndexedVariableFunc[idx,sortArr],{idx,1,NA},Evaluate[defaultPltOpts]],Plot[IndexedVariableFunc[idx,sortArrD],{idx,1,NA},Evaluate[defaultPltOpts]]];
tPrev=t;
)
];
stepCount++;
)
];

f=xxx/. sol[[1]];
fp[t_]=f'[t];
)
];

PrintTimeUsed[];
(* ============================================= *)

Print["DynamicsRun::Plotting"];

tPlot=tPlotMultiplierVal*f["Domain"][[1,2]];
Print["tPlot = ", tPlot];

If[doPlotSabVal,
(
For[idxNAplot=0, (idxNAplot <= naPlotMaxVal && idxNAplot < NA),idxNAplot+= naPlotVal,
(
Print["Plotting f[t] for idxNAplot = ", (idxNAplot+1), " to " , Min[idxNAplot+naPlotVal,NA]];
Print[Plot[Table[f[t][[ii]],{ii,idxNAplot+1,Min[idxNAplot+naPlotVal,NA]}],{t,0,tPlot},Evaluate[defaultPltOpts]]];
)
];
PrintTimeUsed[];
),
(
Print["DynamicsRun:: !!! NOT Plotting f[t] !!!"];
)
];

(* ============================================= *)

Print["Hamiltonian"];
TT[t_]:=((fp[t]) . (fp[t])/2);

VV[t_]:=UUAllLinear[f[t],gammaValList,alphaValList,useUUL2Val,useFullySymmetricPolyVal];
HH[t_]:=TT[t]+VV[t];

HH0=HH[0];
Print["HH[0] = ",HH0];

Print["H[t] / H[0]"];
Print[Plot[HH[t]/HH0,{t,0,tPlot},Evaluate[defaultPltOpts]]];
PrintTimeUsed[];

Print["K/H[0] and V/H[0]"];
Print[Plot[{TT[t]/HH0,VV[t]/HH0},{t,0,tPlot},Evaluate[defaultPltOpts]]];
PrintTimeUsed[];

(* ============================================= *)

Print["Lambda - Plot"];
Print["t = ",0];
lambdaVec=CalculateLambdaSorted[ToMatrix[f[0]]];
Print[Plot[IndexedVariableFunc[idx,lambdaVec],{idx,1,NNN},Evaluate[defaultPltOpts]]];
PrintTimeUsed[];
(* ============================================= *)

Print["t = ",tPlot];
lambdaVec=CalculateLambdaSorted[ToMatrix[f[tPlot]]];
Print[Plot[IndexedVariableFunc[idx,lambdaVec],{idx,1,NNN},Evaluate[defaultPltOpts]]];
PrintTimeUsed[];
(* ============================================= *)

Print["Lambda - Plot3D"];
Print["CalculateLambdaSorted[ToMatrix[f[0]]] = ", N[CalculateLambdaSorted[ToMatrix[f[0]]]]];
Print["CalculateLambdaSorted[ToMatrix[f[",tPlot,"]]] = ", N[CalculateLambdaSorted[ToMatrix[f[tPlot]]]]];

Lambda3D[idxVal_,t_]:=Module[{retVal,idx,fVals},
idx=Round[idxVal];
If[idx < 1 || idx > NNN,Return[Indeterminate]];
fVals=CalculateLambdaSorted[ToMatrix[f[t]]];
retVal=fVals[[idx]];
Return[retVal];
];

Print[Plot3D[Lambda3D[idx,t],{idx,1,NNN},{t,0,tPlot},Evaluate[defaultPlt3DOpts]]];
PrintTimeUsed[];

(*
Print[Plot3D[Lambda3D[idx,t],{idx,1,NNN},{t,0,tPlot},PlotRange->All,ImageSize ->IMAGESIZE, PlotPoints -> 100]];
*)

PrintTimeUsed[];

If[printNDSolveCoordVal,
(
Print["DynamicsRun::Values of evolution parameter"];
ifun=First[xxx /. sol];
coords=First[InterpolatingFunctionCoordinates[ifun]];
Print[strSeparatorSmall];
Print["coords = ", InputForm[coords, NumberMarks->False]];
)
];
Return[sol];
];
(* ============================================= *)
