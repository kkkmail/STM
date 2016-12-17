(* :Author: Konstantin K.Konstantinov *)
(* :Summary: Delta lambda gradient tests for STM. *)
(* :Copyright: K^3, 2013 *)
(* :Version: Revision: 1.12.001, Date:2013/10/18 *)
(* :Mathematica Version: 7.0 - 9.0 *)
(* ============================================== *)

TestF4GradientsLambdaIgnoreLevel=0;
TestF4GradientsLambdaIgnoreIncorrectTimeSign=False;
TestF4GradientsLambdaIgnoreComplexTSolutions=False;
TestF4GradientsLambdaComplexTolerance=10^-3;

RunIDidx=1;
TermNameIdx=2;
NoOfSolIdx=3;
IgnoredSolIdx=4;
TdSatSmaxIdx=5;

SwapSignIdx=6;
SSignDeltaLambdaIdx=7;
EpsSignDeltaLambdaIdx=8;
SNm2minusNm1SignIdx=9;
SNminusNm1SignIdx=10;

MaxTestF4GradientsLambdaIdx=10;

(* ============================================= *)

TestF4GradientsLambdaSingleRun[runID_?IntegerQ,sMinVal_?NumericQ,sMaxVal_?NumericQ,tVal_?NumericQ]:=Module[{termsVal,invIdx,tngIdx,gValLinear,gValMatrix,deltaLambda,deltaLambdaSorted,lambda,eXXT,gVal,sol,f4Val,deltaLambda11Func,solLen,TdS,deltaLambda12Func,ignoreSol,ii,SwapSign,deltaLambda12,gradT,gradS1,gradS2,gradS3,gradEps,tttRule},
termsVal=Table[Indeterminate,{ii,1,MaxTestF4GradientsLambdaIdx}];
termsVal[[RunIDidx]]= runID;
termsVal[[TermNameIdx]]= TermMatrix[[runID]];

ignoreSol=False;

eXXT=eeReconstruct[eprimIJidentity];

Print["GradP4All[SI,False]"];
gValLinear = GradP4All[SI,False];

Print["gValMatrix"];
gValMatrix=ToMatrix[gValLinear];

Print["deltaLambda"];
deltaLambda=Expand[CalculateDeltaLambda[gValMatrix,eXXT,False]];
Print["deltaLambda = ", deltaLambda // MatrixForm];
PrintTimeUsed[];

Print["grad functions."];
(*
gradT[TTT_,SSS_]=deltaLambda[[1]];
gradS1[TTT_,SSS_]=deltaLambda[[2]];
gradS2[TTT_,SSS_]=deltaLambda[[3]];
gradS3[TTT_,SSS_]=deltaLambda[[4]];
gradEps[TTT_,SSS_]=deltaLambda[[5]];
*)
tttRule={TTT -> tVal};
gradT[SSS_]=deltaLambda[[1]] /. tttRule;
gradS1[SSS_]=deltaLambda[[2]]/. tttRule;
gradS2[SSS_]=deltaLambda[[3]]/. tttRule;
gradS3[SSS_]=deltaLambda[[4]]/. tttRule;
gradEps[SSS_]=deltaLambda[[5]]/. tttRule;

Print["Definition[gradT] = ", Definition[gradT]];

(* Print[Plot3D[gradT[TTT,SSS],{TTT,sMinVal,2*sMaxVal},{SSS,sMinVal,sMaxVal},ImageSize ->IMAGESIZE]]; *)
(*
Print[Plot[{gradT[tVal,SSS],gradS1[tVal,SSS],gradS2[tVal,SSS],gradS3[tVal,SSS],gradEps[tVal,SSS]},{SSS,sMinVal,sMaxVal},Evaluate[defaultPltOpts]]];
*)

Print[Plot[{gradT[SSS],gradS1[SSS],gradS2[SSS],gradS3[SSS],gradEps[SSS]},{SSS,sMinVal,sMaxVal},Evaluate[defaultPltOpts2]]];

];

(* ============================================= *)

TestF4GradientsLambdaSingleRunOld[runID_?IntegerQ,sMinVal_?NumericQ,sMaxVal_?NumericQ]:=Module[{termsVal,invIdx,tngIdx,gValLinear,gValMatrix,deltaLambda,deltaLambdaSorted,lambda,eXXT,gVal,sol,f4Val,deltaLambda11Func,solLen,TdS,deltaLambda12Func,ignoreSol,ii,SwapSign,deltaLambda12},
termsVal=Table[Indeterminate,{ii,1,MaxTestF4GradientsLambdaIdx}];
termsVal[[RunIDidx]]= runID;
termsVal[[TermNameIdx]]= TermMatrix[[runID]];

ignoreSol=False;
(*
Print["F4Func[S,False]"];
f4Val=Expand[F4Func[S,False]];
gVal=Table[D[f4Val,SL[[ii]]],{ii,1,NA}];
*)

eXXT=eeReconstruct[eprimIJidentity];

Print["GradP4All[SI,False]"];
gValLinear = GradP4All[SI,False];

Print["gValMatrix"];
gValMatrix=ToMatrix[gValLinear];

Print["deltaLambda"];
deltaLambda=Expand[CalculateDeltaLambda[gValMatrix,eXXT,False]];
Print["deltaLambda = ", deltaLambda // MatrixForm];

Print["deltaLambda[[2]] = ", Simplify[deltaLambda[[2]]]];
Print["Solving for proper time coordinate."];
sol=Solve[deltaLambda[[2]]==0,TTT];
solLen=Length[sol];
Print["solLen = ", solLen];
termsVal[[NoOfSolIdx]]= solLen;

ttt1Val=TTT /.sol[[1]];
ttt1[SSS_]=ttt1Val;

deltaLambda11=deltaLambda /. sol[[1]];
deltaLambda11Func[SSS_]=deltaLambda11;

TdS=Re[ttt1[sMaxVal]]/sMaxVal;
termsVal[[TdSatSmaxIdx]]= N[TdS];

Print["TdS[sMaxVal] = ", N[TdS]];

SwapSign=-Sign[Re[deltaLambda11Func[sMaxVal]][[1]]];
termsVal[[SwapSignIdx]]= SwapSign;

If[SwapSign<0,
(
Print["!!! Sign of Delta Lambda for T is positive. Swapping sign !!!"];
deltaLambda11Func[SSS_]=-deltaLambda11;
)
];

termsVal[[EpsSignDeltaLambdaIdx]]= Sign[Re[deltaLambda11Func[sMaxVal]][[3]]];
termsVal[[SSignDeltaLambdaIdx]]= Sign[Re[deltaLambda11Func[sMaxVal]][[NNN-1]]];

termsVal[[SNm2minusNm1SignIdx]]= Sign[Re[deltaLambda11Func[sMaxVal]][[NNN-2]]-Re[deltaLambda11Func[sMaxVal]][[NNN-1]]];
termsVal[[SNminusNm1SignIdx]]= Sign[Re[deltaLambda11Func[sMaxVal]][[NNN]]-Re[deltaLambda11Func[sMaxVal]][[NNN-1]]];

If[(TestF4GradientsLambdaIgnoreIncorrectTimeSign && TdS < 0) || (Abs[TdS] < TestF4GradientsLambdaIgnoreLevel),
(
ignoreSol=True;
If[(TestF4GradientsLambdaIgnoreIncorrectTimeSign && TdS < 0),
(
Print["Ignoring due to negative sign of TdS."];
)
];
If[(Abs[TdS] < TestF4GradientsLambdaIgnoreLevel),
(
Print["ignoring due to small absolute value of TdS."];
)
];
),
(
If[solLen ==3,
(
ttt2Val=TTT /.sol[[2]];
ttt3Val=TTT /.sol[[3]];

ttt2[SSS_]=ttt2Val;
ttt3[SSS_]=ttt3Val;

Print["Plotting solutions."];
Print[Plot[{Re[ttt1[sss]],Im[ttt1[sss]]},{sss,sMinVal,sMaxVal},Evaluate[defaultPltOpts]],
Plot[{Re[ttt2[sss]],Im[ttt2[sss]]},{sss,sMinVal,sMaxVal},Evaluate[defaultPltOpts]],
Plot[{Re[ttt3[sss]],Im[ttt3[sss]]},{sss,sMinVal,sMaxVal},Evaluate[defaultPltOpts]]];

),
(
If[solLen ==2,
(
ttt2Val=TTT /.sol[[2]];
ttt2[SSS_]=ttt2Val;

Print["Plotting TWO solutions."];
Print[Plot[{Re[ttt1[sss]],Im[ttt1[sss]]},{sss,sMinVal,sMaxVal},Evaluate[defaultPltOpts]],
Plot[{Re[ttt2[sss]],Im[ttt2[sss]]},{sss,sMinVal,sMaxVal},Evaluate[defaultPltOpts]]];

If[TestF4GradientsLambdaIgnoreComplexTSolutions,
(
If[(Abs[Im[ttt1[sMaxVal]]/Re[ttt1[sMaxVal]]] > TestF4GradientsLambdaComplexTolerance) || (Abs[Im[ttt2[sMaxVal]]/Re[ttt2[sMaxVal]]] > TestF4GradientsLambdaComplexTolerance),
(
Print["Both solutions are sufficiently complex - IGNORING!!!"];
ignoreSol=True;
)
];
)
];
),
(
Print["Plotting SINGLE solution."];
Print[Plot[{Re[ttt1[sss]],Im[ttt1[sss]]},{sss,sMinVal,sMaxVal},Evaluate[defaultPltOpts]]];
)
];
)
];

If[solLen != 2,
(
If[!ignoreSol,
(
Print["Time, proper time, small spaces..."];
Print[Plot[Re[deltaLambda11Func[sss][[1]]],{sss,sMinVal,sMaxVal},PlotRange -> Automatic, Evaluate[defaultPltOpts]],Plot[Re[deltaLambda11Func[sss][[2]]],{sss,sMinVal,sMaxVal},PlotRange -> Automatic, Evaluate[defaultPltOpts]],Plot[Re[deltaLambda11Func[sss][[3]]],{sss,sMinVal,sMaxVal},PlotRange -> Automatic, Evaluate[defaultPltOpts]]];

Print["Spaces, differences, and (time/space)."];
Print[Plot[{Re[deltaLambda11Func[sss][[NNN-2]]],Re[deltaLambda11Func[sss][[NNN-1]]],Re[deltaLambda11Func[sss][[NNN]]]},{sss,sMinVal,sMaxVal},PlotRange -> Automatic, Evaluate[defaultPltOpts]],Plot[{Re[deltaLambda11Func[sss][[NNN-2]]-deltaLambda11Func[sss][[NNN-1]]],Re[deltaLambda11Func[sss][[NNN]]-deltaLambda11Func[sss][[NNN-1]]],Re[deltaLambda11Func[sss][[NNN-2]]-deltaLambda11Func[sss][[NNN]]]},{sss,sMinVal,sMaxVal},PlotRange -> Automatic, Evaluate[defaultPltOpts]],Plot[Re[ttt1[sss]]/sss,{sss,sMinVal,sMaxVal},PlotRange -> Automatic, Evaluate[defaultPltOpts]]];
)
];
),
(
If[!ignoreSol,
(
Print["Time, proper time, small spaces for 1-st (out of 2) solutions..."];
Print[Plot[Re[deltaLambda11Func[sss][[1]]],{sss,sMinVal,sMaxVal},PlotRange -> Automatic, Evaluate[defaultPltOpts]],Plot[Re[deltaLambda11Func[sss][[2]]],{sss,sMinVal,sMaxVal},PlotRange -> Automatic, Evaluate[defaultPltOpts]],Plot[Re[deltaLambda11Func[sss][[3]]],{sss,sMinVal,sMaxVal},PlotRange -> Automatic, Evaluate[defaultPltOpts]]];

Print["Spaces, differences, and (time/space)."];
Print[Plot[{Re[deltaLambda11Func[sss][[NNN-2]]],Re[deltaLambda11Func[sss][[NNN-1]]],Re[deltaLambda11Func[sss][[NNN]]]},{sss,sMinVal,sMaxVal},PlotRange -> Automatic, Evaluate[defaultPltOpts]],Plot[{Re[deltaLambda11Func[sss][[NNN-2]]-deltaLambda11Func[sss][[NNN-1]]],Re[deltaLambda11Func[sss][[NNN]]-deltaLambda11Func[sss][[NNN-1]]],Re[deltaLambda11Func[sss][[NNN-2]]-deltaLambda11Func[sss][[NNN]]]},{sss,sMinVal,sMaxVal},PlotRange -> Automatic, Evaluate[defaultPltOpts]],Plot[Re[ttt1[sss]]/sss,{sss,sMinVal,sMaxVal},PlotRange -> Automatic, Evaluate[defaultPltOpts]]];

Print[strSeparatorSmall];

deltaLambda12=deltaLambda /. sol[[2]];
deltaLambda12Func[SSS_]=deltaLambda12;

Print["Time, proper time, small spaces for 2-nd (out of 2) solutions..."];
Print[Plot[Re[deltaLambda12Func[sss][[1]]],{sss,sMinVal,sMaxVal},PlotRange -> Automatic, Evaluate[defaultPltOpts]],Plot[Re[deltaLambda12Func[sss][[2]]],{sss,sMinVal,sMaxVal},PlotRange -> Automatic, Evaluate[defaultPltOpts]],Plot[Re[deltaLambda12Func[sss][[3]]],{sss,sMinVal,sMaxVal},PlotRange -> Automatic, Evaluate[defaultPltOpts]]];

Print["Spaces, differences, and (time/space)."];
Print[Plot[{Re[deltaLambda12Func[sss][[NNN-2]]],Re[deltaLambda12Func[sss][[NNN-1]]],Re[deltaLambda12Func[sss][[NNN]]]},{sss,sMinVal,sMaxVal},PlotRange -> Automatic, Evaluate[defaultPltOpts]],Plot[{Re[deltaLambda12Func[sss][[NNN-2]]-deltaLambda12Func[sss][[NNN-1]]],Re[deltaLambda12Func[sss][[NNN]]-deltaLambda12Func[sss][[NNN-1]]],Re[deltaLambda12Func[sss][[NNN-2]]-deltaLambda12Func[sss][[NNN]]]},{sss,sMinVal,sMaxVal},PlotRange -> Automatic, Evaluate[defaultPltOpts]],Plot[Re[ttt1[sss]]/sss,{sss,sMinVal,sMaxVal},PlotRange -> Automatic, Evaluate[defaultPltOpts]]];
)
];
)
];
)
];

termsVal[[IgnoredSolIdx]]= If[ignoreSol,1,0];

Return[termsVal];
];

TestF4GradientsLambda[startRunID_?IntegerQ,endRunID_?IntegerQ,sMaxVal_?NumericQ]:=Module[{sMinVal,runID,termsVal,termsTbl},
Print["TestF4GradientsLambda::Testing all gradients for R(1,3) + proper time + small spaces model..."];

termsTbl=Table[Indeterminate,{ii,1,(endRunID-startRunID+1)}];

sMinVal=2*IVTEpsilonSpaceVal;
For[runID=startRunID, runID <= endRunID,runID++,
(
Print[strSeparator];
Print["Running for runID = ", runID, ", term name = " <> TermMatrix[[runID]]];

Print["ResetCoeffF4"];
ResetCoeffF4[];
UpdateCoeffF4[runID,1];

termsVal=TestF4GradientsLambdaSingleRun[runID,sMinVal,sMaxVal,TVal];

termsTbl[[runID-startRunID+1]]=termsVal;
PrintTimeUsed[];
)
];

Print["termsTbl = ", termsTbl // MatrixForm];
PrintTimeUsed[];
Return[];
];

(* ============================================= *)

TestF4GradientsLambda2Terms[CabcdefghStart_?NumericQ,CabcdefghEnd_?NumericQ,noOfPoins_?IntegerQ,sMaxVal_?NumericQ]:=Module[{runID,step,CabcdefghVal,termsTbl,termsVal,sMinVal},

step=(CabcdefghEnd-CabcdefghStart)/noOfPoins;
termsTbl=Table[Indeterminate,{ii,1,noOfPoins+1}];
sMinVal=2*IVTEpsilonSpaceVal;

For[runID=0,runID<=noOfPoins,runID++,
(
Print[strSeparator];
CabcdefghVal=CabcdefghStart+step*runID;
Print["Running for runID = ",runID,", CabcdefghVal = ",N[CabcdefghVal]];
ResetCoeffF4[];
UpdateCoeffF4[idxD4T4Cabbdbfbh,1];
UpdateCoeffF4[idxD4T8Cabcdefgh,CabcdefghVal];
termsVal=TestF4GradientsLambdaSingleRun[runID,sMinVal,sMaxVal,TVal];
termsTbl[[runID+1]]=termsVal;
PrintTimeUsed[];
)
];

Print["termsTbl = ",termsTbl//MatrixForm];
PrintTimeUsed[];
Return[];
];

