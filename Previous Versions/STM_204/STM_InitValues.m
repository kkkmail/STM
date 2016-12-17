(* :Author: Konstantin K.Konstantinov *)
(* :Summary: Initial values for STM. *)
(* :Copyright: K^3, 2013 - 2014 *)
(* :Version:Revision: 2.00.001,Date: 2014/03/05 *)
(* :Mathematica Version: 7.0 - 9.0 *)
(* ============================================== *)

Options[STMInitValues]={OutputVariables -> False};

(* Rule to replace Lambda0 by sum of all lambdas *)
(* This rule is used only *)
InitLambda0Rule={};

(* Initial value of Lambda *)
InitLambda=Indeterminate;
InitLambda0ReverseExpr=Indeterminate;
(* ============================================== *)

(* The following types of initial values are defined. *)
(* Minimum value for error checking. *)
IVTmin=0; 

(* Default initial values (distribution). Uses rndFunc and rndArgs to get the random values. *)
IVTdistribution=0; 

(* Pure 4D Points. *)
IVT4DPoints=1; 
(* 4D points with some N dimensional normally distributed noise. Must specify NoiseLevel. *)
IVT4DPointsWithNoise=2; 

(* Pure 4D x S1 points. Must specify S1Radius (radius of S1). *)
IVT4DmS1Points=3; 
(* 4D x S1 points with some N dimensional normally distributed noise. Must specify S1Radius and NoiseLevel. *)
IVT4DmS1PointsWithNoise=4; 

(* TODO 4D x S2 - Add !!! *)
(* Pure 4D x S2 points. Must specify S1Radius (radius of S2). *)
IVT4DmS2Points=5; 
(* 4D x S1 points with some N dimensional normally distributed noise. Must specify S1Radius and NoiseLevel. *)
IVT4DmS2PointsWithNoise=6; 

(* Pure R (1,3) + 1 proper time + all others small (EPS) spacelike coordinates. *)
(* Must specify IVTEpsilonVal and IVTEpsilonSpaceVal (difference between spatial eigenvalues) *)
IVTT1S3EPS=7;

(* Symbolic Lambda *)
IVTLAMBDA=8;

(* Increading Integers starting from 1 *)
IVTIncreasingIntegers = 9;

(* 4D points, which minimize the polynom *)
IVT4DMinPolyValue = 10;

(* Maximum value for error checking. *)
IVTmax=10; 

(* Noise level (if used). Must be defined to be non zero. *)
NoiseLevel=0;

(* S1 radius (if used). Must be defined to be non zero. *)
S1Radius = 0;

(* Scales of space and time in 4D points. *)
IVTSpaceScale=1;
IVTTimeScale=1;

(*
IVTSpaceScale=1/Sqrt[3];
IVTTimeScale=1;
*)

(* *)
IVTEpsilonVal=0;
IVTEpsilonSpaceVal=0;

(* The following types of normalization of initial values are defined. *)
NIVmin=0;(* Minimum value for error checking. *)
NIVnone=0; (* Do not normalize initial values at all. *)
NIVuseOne=1; (* Normalize initial values to have their sum = 1. *)
NIVuseNA=2; (* Normalize initial values to have their sum = NA - DEFAULT VALUE!!! *)
NIVuseSqrtNA=3;  (* Normalize initial values to have their sum = Sqrt[NA]. *)
NIVmax=3; (* Maximum value for error checking. *)

(* ============================================== *)

(* This function creates initial values of intervals. Parameters are described above. *)
CreateInitialvalues[initValuesType_?IntegerQ,normInitValType_?IntegerQ,rawOpts___]:=Module[{x4D,int4D,initValTblSum,coordSumInitVal,normalizeInitialValues,addNoise,ii,jj,kk,intNoiseVal,lambda,eps,eps1,SYY,SYYL,SYYLrule,eXXT,XXT,aa,bb,coorNorm,coords,lambdaVec,lambdaVecNew,opts,useHighPrecisionInitValuesVal,initValuesPrecisionVal,outputVars,SILre,SILim,intNoiseValRe,intNoiseValIm},
Print["Creating initial values..."];

opts=ProcessOptions[rawOpts];

useHighPrecisionInitValuesVal=UseHighPrecision /.opts /.Options[STMCommon];
initValuesPrecisionVal=HighPrecisionValue /.opts /.Options[STMCommon];
outputVars=OutputVariables /.opts /.Options[STMInitValues];

(* Set symmetric and antisymmetric parts to zero. *)
SI=ZZ;
AI=ZZ;
SIL=ZL;
AIL=ZL;

If[initValuesType < IVTmin || initValuesType > IVTmax,
(
Print["CreateInitialvalues::initValuesType is out of range. initValuesType = ", initValuesType];
Return[];
)
];

If[normInitValType < NIVmin || normInitValType > NIVmax,
(
Print["CreateInitialvalues::normInitValType is out of range. normInitValType = ", normInitValType];
Return[];
)
];

normalizeInitialValues=True;
coorNorm=1;
If[normInitValType == NIVnone, normalizeInitialValues = False];
If[normInitValType ==NIVuseOne,coorNorm = 1];
If[normInitValType ==NIVuseNA,coorNorm = NA];
If[normInitValType ==NIVuseSqrtNA,coorNorm =Sqrt[NA]];
Print["coorNorm = ",coorNorm];

addNoise=False;
If[initValuesType == IVT4DPointsWithNoise || initValuesType == IVT4DmS1PointsWithNoise,addNoise=True];

(* Resetting initial lambda and lambda 0 rule. *)
InitLambda=Indeterminate;
InitLambda0Rule={};
InitLambda0ReverseExpr=Indeterminate;

If[initValuesType== IVTdistribution,
(
Print["CreateInitialvalues::IVTdistribution."];

SIL=Table[RandomVariate[Apply[rndFunc,rndArgs]],{ii,1,NA}];

If[UseComplexVariablesValue,
(
AIL=Table[RandomVariate[Apply[rndFunc,rndArgs]],{ii,1,NA}];
)
];

If[useHighPrecisionInitValuesVal,
(
Print["Setting precision ", initValuesPrecisionVal, " to SIL"];
SIL=SetPrecision[SIL,initValuesPrecisionVal];
AIL=SetPrecision[AIL,initValuesPrecisionVal];
)
];
)
];

If[initValuesType== IVT4DPoints || initValuesType == IVT4DPointsWithNoise,
(
Print["CreateInitialvalues::IVT4DPoints || IVT4DPointsWithNoise."];
Print["TODO::CreateInitialvalues::Not fixed yet!"];
Return[];

x4D=Table[RandomVariate[Apply[rndFunc,rndArgs]]*If[jj==1,I*IVTTimeScale,IVTSpaceScale],{ii,1,NNN},{jj,1,4}];

If[useHighPrecisionInitValuesVal,
(
Print["Setting precision ", initValuesPrecisionVal, " to x4D"];
x4D=SetPrecision[x4D,initValuesPrecisionVal];
)
];

If[outputVars,Print["x4D = ",x4D//MatrixForm]];

int4D=Table[If[ii!=jj,Re[Sum[(x4D[[ii,kk]]-x4D[[jj,kk]])^2,{kk,1,4}]],0],{ii,1,NNN},{jj,1,NNN}];

If[outputVars,Print["int4D = ",int4D//MatrixForm]];
SIL=ToLinear[int4D];
)
];

If[initValuesType== IVT4DMinPolyValue,
(
Print["CreateInitialvalues::IVT4DMinPolyValue."];
Print["TODO::CreateInitialvalues::Not fixed yet!"];
Return[];

x4D=Table[RandomVariate[Apply[rndFunc,rndArgs]]*If[jj==1,I*IVTTimeScale,IVTSpaceScale],{ii,1,NNN},{jj,1,4}];

If[useHighPrecisionInitValuesVal,
(
Print["Setting precision ", initValuesPrecisionVal, " to x4D"];
x4D=SetPrecision[x4D,initValuesPrecisionVal];
)
];

If[outputVars,Print["x4D = ",x4D//MatrixForm]];
int4D=Table[If[ii!=jj,Re[Sum[(x4D[[ii,kk]]-x4D[[jj,kk]])^2,{kk,1,4}]],0],{ii,1,NNN},{jj,1,NNN}];
If[outputVars,Print["int4D = ",int4D//MatrixForm]];

coords=FindUncorrelatedCoordinates[int4D,True];
lambdaVec=GetLambdaVec[coords];
lambdaVecNew=Table[If[ii > 4,0,If[lambdaVec[[ii]]>= 0,IVTSpaceScale,-IVTTimeScale]],{ii,1,NNN}];
eXXT=GetE[coords];

SI=CalculateSfromLambdaVec[lambdaVecNew,eXXT];

SIL=ToLinear[SI];

Print["lambdaVec = ", lambdaVec // MatrixForm];
Print["lambdaVecNew = ", lambdaVecNew // MatrixForm];
(*
Print["eXXT = ", eXXT // MatrixForm];
Print["SIL = ", SIL // MatrixForm];
*)
)
];

If[initValuesType== IVT4DmS1Points || initValuesType == IVT4DmS1PointsWithNoise,
(
Print["!!! CreateInitialvalues:: IVT4DmS1Points || IVT4DmS1PointsWithNoise are not implemented yet !!!"];
Return[];
)
];

If[initValuesType== IVT4DmS2Points || initValuesType == IVT4DmS2PointsWithNoise,
(
Print["!!! CreateInitialvalues:: IVT4DmS2Points || IVT4DmS2PointsWithNoise are not implemented yet !!!"];
Return[];
)
];

If[initValuesType== IVTT1S3EPS,
(
Print["CreateInitialvalues::IVTT1S3EPS."];
Print["TODO::CreateInitialvalues::Not fixed yet!"];
Return[];

Clear[TTT,SSS];

lambda=Expand[(1/SSS)*DiagonalMatrix[Table[If[ii==1,-TTT,If[ii== NNN,0,If[ii== 2,SSS+eps1,If[ii==3,SSS,If[ii==4,SSS-eps1,eps]]]]],{ii,1,NNN}]] /. {eps -> IVTEpsilonVal,eps1 -> IVTEpsilonSpaceVal}];

eXXT=eeReconstruct[eprimIJidentity];
XXT = Expand[eXXT . lambda . Transpose[eXXT]];
SI=Table[Expand[XXT[[aa,aa]]+XXT[[bb,bb]]-2*XXT[[aa,bb]]],{aa,1,NNN},{bb,1,NNN}];
SIL=ToLinear[SI];

Print["lambda = ", lambda // MatrixForm];
Print["eXXT = ", eXXT // MatrixForm];
Print["XXT = ", XXT // MatrixForm];
Print["SI = ", SI // MatrixForm];
)
];

If[initValuesType==IVTLAMBDA,
(
Print["CreateInitialvalues::IVTLAMBDA."];
Print["TODO::CreateInitialvalues::Not fixed yet!"];
Return[];

lambda=DiagonalMatrix[Table[If[ii== NNN,0,Subscript[\[CapitalLambda],ii]],{ii,1,NNN}]];
eXXT=eeReconstruct[eprimIJidentity];
XXT = Expand[eXXT . lambda . Transpose[eXXT]];
SI=Table[Expand[XXT[[aa,aa]]+XXT[[bb,bb]]-2*XXT[[aa,bb]]],{aa,1,NNN},{bb,1,NNN}];
InitLambda0ReverseExpr=(1/(Sqrt[NNN]+1)^2)*Sum[Subscript[\[CapitalLambda],ii],{ii,1,NNN-1}];
InitLambda0Rule={Subscript[\[CapitalLambda],0] -> InitLambda0ReverseExpr};
InitLambda=lambda;

For[ii=1,ii<=NNN-1,ii++,
(
SI[[NNN,ii]]=Simplify[SI[[NNN,ii]]-InitLambda0ReverseExpr+Subscript[\[CapitalLambda],0]];
SI[[ii,NNN]]=Simplify[SI[[ii,NNN]]-InitLambda0ReverseExpr+Subscript[\[CapitalLambda],0]];
)
];

SIL=ToSLinear[SI];

Print["lambda = ", lambda // MatrixForm];
Print["eXXT = ", eXXT // MatrixForm];
Print["XXT = ", XXT // MatrixForm];
Print["SI = ", SI // MatrixForm];
)
];

If[initValuesType==IVTIncreasingIntegers,
(
Print["CreateInitialvalues::IVTIncreasingIntegers."];
Print["TODO::CreateInitialvalues::Not fixed yet!"];
Return[];

SIL=Table[ii,{ii,1,NA}];
SI=ToSMatrix[SIL];
)
];

If[addNoise,
(
Print["CreateInitialvalues::Adding noise."];
intNoiseVal=Table[NoiseLevel*RandomReal[NormalDistribution[]],{ii,1,NA}];
Print["TODO::CreateInitialvalues::Not fixed yet!"];
Return[];

If[UseComplexVariablesValue,
(
If[UseReImVariablesValue,
(
Print["CreateInitialvalues::Adding noise for Re Im variables is not implemented yet."];
Return[];
)
];

Print["CreateInitialvalues::Creating complex noise..."];
intNoiseValRe=intNoiseVal;
intNoiseValIm=Table[NoiseLevel*RandomReal[NormalDistribution[]],{ii,1,NA}];
intNoiseVal=Join[intNoiseValRe+I*intNoiseValIm,intNoiseValRe-I*intNoiseValIm];
)
];

If[useHighPrecisionInitValuesVal,
(
Print["CreateInitialvalues::Setting precision ", initValuesPrecisionVal, " to intNoiseVal"];
intNoiseVal=SetPrecision[intNoiseVal,initValuesPrecisionVal];
)
];

If[outputVars,Print["CreateInitialvalues::intNoiseVal = ", intNoiseVal]];
SIL=SIL+intNoiseVal;
)
];

If[normalizeInitialValues && initValuesType!=IVTT1S3EPS,
(
Print["CreateInitialvalues::Normalizing..."];
Print["TODO::CreateInitialvalues::Not fixed yet!"];
Return[];

If[!UseComplexVariablesValue,
(
Print["CreateInitialvalues::Normalizing initial values."];
initValTblSum=Sum[SIL[[ii]],{ii,1,NA}];
Print["Unadjusted sum of initial values (initValTblSum) = ",initValTblSum];

If[Abs[initValTblSum]>=InitialValuesZeroTolerance,
(
SIL=SIL*(coorNorm/initValTblSum);
),
(
Print["Sum of initial values is too small. Adjusting the last initial value."];
SIL[[NA]]=coorNorm-Sum[SIL[[ii]],{ii,1,NA-1}];
)
];
coordSumInitVal=Sum[SIL[[ii]],{ii,1,NA}];
Print["coordSumInitVal (adjusted sum of initial values) = ",coordSumInitVal];
),
(
Print["TODO::CreateInitialvalues: Normalization of initial values for complex variables is not yet implemented!!!"];
)
];
)
];

Print["Initial values rule (SIrule = SL -> SIL), (AIrule = AL -> AIL)."];
SIrule=Table[SL[[ii]]->SIL[[ii]],{ii,1,NA}];
AIrule=Table[AL[[ii]]->AIL[[ii]],{ii,1,NA}];

Print["Matrix of initial values (SI = ToSMatrix[SIL], AI = ToAMatrix[AIL])."];
SI = ToSMatrix[SIL];
AI = ToAMatrix[AIL];
Print[strSeparatorSmall];
];

(* ============================================== *)


