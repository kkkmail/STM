(* :Author: Konstantin K.Konstantinov *)
(* :Summary: Various test gradients for STM. *)
(* :Copyright: K^3, 2013 *)
(* :Version: Revision: 1.03.001, Date:2013/10/04 *)
(* :Mathematica Version: 7.0 - 9.0 *)
(* ============================================== *)

TestCoeffCircle=0;
TestCoeffDualCircle=0;
TestCoeffSpMM=0;
TestCoeffSpMMv2=0;

TestMMMCircle=0;
TestMMMDualCircle=0;
TestMMMSpMM=0;
TestMMMSpMMv2=0;

(* Linear version of analytical expression for gradient for circular term with all included. *)
TestGradCircleLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=Module[{sab},
sab=ToMatrix[sabLinear];
Return[TestGradCircular[sab,returnTangentialGradient]];
];

(* Matrix version of analytical expression for gradient for circular term with all included. *)
TestGradCircle[sab_?MatrixQ,returnTangentialGradient_?BooleanQ]:=Module[{gradVal,SpM,mm},
mm=Round[TestMMMCircle];

If[mm > 0,
(
SpM=MatrixPower[sab,mm-1];
gradVal=Table[(SpM[[remapReverseTable[[ii,1]],remapReverseTable[[ii,2]]]]),{ii,1,NA}];
),
(
gradVal=Table[0,{ii,1,NA}];
)
];

Return[TangentialGradient[gradVal,returnTangentialGradient]];
];

(* Linear version of analytical expression for gradient for circular term with all included. *)
TestGradDualCircleLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=Module[{sab},
sab=ToMatrix[sabLinear];
Return[TestGradDualCircle[sab,returnTangentialGradient]];
];

(* Matrix version of analytical expression for gradient for circular term with all included. *)
TestGradDualCircle[sab_?MatrixQ,returnTangentialGradient_?BooleanQ]:=Module[{gradVal,S2,S2pMd2,mm},
mm=Round[TestMMMDualCircle];
If[Mod[mm,2]== 0 && mm > 4,
(
S2=sab^2;
S2pMd2=MatrixPower[S2,(mm/2)-1];
gradVal=Table[(sab[[remapReverseTable[[ii,1]],remapReverseTable[[ii,2]]]]*S2pMd2[[remapReverseTable[[ii,1]],remapReverseTable[[ii,2]]]]),{ii,1,NA}];
),
(
(* Cannot have dual circle for odd mm!!! *)
gradVal=Table[0,{ii,1,NA}];
)
];

Return[TangentialGradient[gradVal,returnTangentialGradient]];
];

(* Linear version of analytical expression for gradient for Sab^mm term. Coefficient is ignored. *)
TestGradSpMMLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=Module[{sab},
sab=ToMatrix[sabLinear];
Return[TestGradSpMM[sab,returnTangentialGradient]];
];

(* Matrix version of analytical expression for gradient for Sab^mm term. Coefficient is ignored. *)
TestGradSpMM[sab_?MatrixQ,returnTangentialGradient_?BooleanQ]:=Module[{gradVal,mm},
mm=Round[TestMMMSpMM];
Return[TestGradSpMMvar[sab,returnTangentialGradient,mm]];
];

TestGradSpMMvar[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,mm_?IntegerQ]:=Module[{gradVal},
If[mm > 0,
(
gradVal=Table[sab[[remapReverseTable[[ii,1]],remapReverseTable[[ii,2]]]]^(mm-1),{ii,1,NA}];
),
(
gradVal=Table[0,{ii,1,NA}];
)
];

Return[TangentialGradient[gradVal,returnTangentialGradient]];
];

(* Linear version of analytical expression for gradient. *)
TestGradAllLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=Module[{sab},
sab=ToMatrix[sabLinear];
Return[TestGradAll[sab,returnTangentialGradient]];
];

(* Matrix version of analytical expression for gradient. *)
(* Uses global values of coefficients. *)
TestGradAll[sab_?MatrixQ,returnTangentialGradient_?BooleanQ]:=Module[{gradVal},
gradVal=(TestCoeffCircle*TestGradCircle[sab,returnTangentialGradient]+TestCoeffDualCircle*TestGradDualCircle[sab,returnTangentialGradient]+TestCoeffSpMM*TestGradSpMM[sab,returnTangentialGradient]+TestCoeffSpMMv2*TestGradSpMMvar[sab,returnTangentialGradient,TestMMMSpMMv2]);
Return[TangentialGradient[gradVal,returnTangentialGradient]];
];

(* Test of RunNDSolve and related. *)
TestGradRunNDSolve[tMax_?NumericQ,noOfTsteps_?IntegerQ]:=Module[{sol,pltOpts,f,tPlotMultiplier,tPlot,t,DeltaT,tIdx,tVal,stl,st,solValue,lambda,eXXT,gValLinear,gValMatrix,deltaLambda,deltaLambdaSorted,idx,gradLinearFunc},

Print["TestGradRunNDSolve::Testing RunNDSolve..."];
gradLinearFunc=TestGradAllLinear;
sol=RunNDSolve[tMax,gradLinearFunc,False];
Print["Plot"];
f=sEvolution/. sol[[1]];
tPlotMultiplier=0.99;
tPlot=tPlotMultiplier*f["Domain"][[1,2]];
Print["tPlot = ", tPlot];

pltOpts={PlotRange -> All, Frame -> True, PlotStyle -> Thick, GridLines -> Automatic, PlotPoints -> 200, Method -> {PlotDivision -> 1}, ImageSize -> IMAGESIZE};

Print["StdDevLinear[f[t]]"];
Print[Plot[StdDevLinear[f[t]],{t,0,tPlot},Evaluate[pltOpts]]];

Print["MeanLinear[f[t]]"];
Print[Plot[MeanLinear[f[t]],{t,0,tPlot},Evaluate[pltOpts]]];

Print["MeanLinear[f[t]]/StdDevLinear[f[t]]"];
Print[Plot[MeanLinear[f[t]]/StdDevLinear[f[t]],{t,0,tPlot},Evaluate[pltOpts]]];

DeltaT=tPlot/noOfTsteps;

PrintTimeUsed[];

For[tIdx=0,tIdx <= noOfTsteps,tIdx++,
(
Print[strSeparatorSmall];
Print[strSeparatorSmall];
tVal=DeltaT*tIdx;
Print["tIdx = ", tIdx, ", t = ", tVal];

stl=f[tVal];
st=ToMatrix[stl];

Print["Finding uncorrelated coordinates."];
solValue=FindUncorrelatedCoordinates[st];
lambda=GetLambda[solValue];
eXXT=GetE[solValue];
PlotUncorrelatedCoordinates[NNN,solValue,0,"values for t = " <> ToString[tVal]];

Print["DeltaLambdaFunc for ALL."];
gValLinear=GradientMultiplier*gradLinearFunc[stl,False];
gValMatrix=ToMatrix[gValLinear];
deltaLambda=CalculateDeltaLambda[gValMatrix,eXXT];
deltaLambdaSorted=SortByFirst[lambda,deltaLambda];
Print[Plot[{Re[DeltaLambdaFunc[idx,deltaLambdaSorted]],Im[DeltaLambdaFunc[idx,deltaLambdaSorted]]},{idx,1,NNN},Evaluate[defaultPltOpts]]];
PrintTimeUsed[];

Print["DeltaLambdaFunc for TestGradCircle."];
gValLinear=GradientMultiplier*TestGradCircle[st,False];
gValMatrix=ToMatrix[gValLinear];
deltaLambda=CalculateDeltaLambda[gValMatrix,eXXT];
deltaLambdaSorted=SortByFirst[lambda,deltaLambda];
Print[Plot[{Re[DeltaLambdaFunc[idx,deltaLambdaSorted]],Im[DeltaLambdaFunc[idx,deltaLambdaSorted]]},{idx,1,NNN},Evaluate[defaultPltOpts]]];
PrintTimeUsed[];

Print["DeltaLambdaFunc for TestGradDualCircle."];
gValLinear=GradientMultiplier*TestGradDualCircle[st,False];
gValMatrix=ToMatrix[gValLinear];
deltaLambda=CalculateDeltaLambda[gValMatrix,eXXT];
deltaLambdaSorted=SortByFirst[lambda,deltaLambda];
Print[Plot[{Re[DeltaLambdaFunc[idx,deltaLambdaSorted]],Im[DeltaLambdaFunc[idx,deltaLambdaSorted]]},{idx,1,NNN},Evaluate[defaultPltOpts]]];
PrintTimeUsed[];

Print["DeltaLambdaFunc for TestGradSpMM."];
gValLinear=GradientMultiplier*TestGradSpMM[st,False];
gValMatrix=ToMatrix[gValLinear];
deltaLambda=CalculateDeltaLambda[gValMatrix,eXXT];
deltaLambdaSorted=SortByFirst[lambda,deltaLambda];
Print[Plot[{Re[DeltaLambdaFunc[idx,deltaLambdaSorted]],Im[DeltaLambdaFunc[idx,deltaLambdaSorted]]},{idx,1,NNN},Evaluate[defaultPltOpts]]];
PrintTimeUsed[];
)
];

Return[sol];
];
