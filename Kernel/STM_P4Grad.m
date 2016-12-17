(* :Author:Konstantin K.Konstantinov *)
(* :Summary:Gradient of 4-th power polynom minimization function and its decomposition into invariants. *)
(* :Copyright:K^3,2013 *)
(* :Version: Revision: 1.10.001, Date:2013/10/14 *)
(* :Mathematica Version:7.0-9.0 *)
(* ==============================================*)

(* Default scale for numeric evaluation of gradient. *)
GradScaleValue = 10^-2;
ExpandGradient = True;

(* If True, then P8s gradient is added. *)
UseP8sGrad=False;
coeffP8sGrad=1;

(* Renormalization coefficients for gradient. Used for convenience. *)
CoeffF4NormIdentity=Table[1,{ii,1,NoOfTermsF4}];

(* The values in CoeffF4NormF4 are such so that to have gradient coincide with the F4 old style function. *)
CoeffF4NormF4={1/384,1/16,1/12,1/24,1/16,1/4,1/8,1/2,1/2,1/2,1/4,1/2,1/2,1/4,1/12,1,1/2,1,1/2,1/8,1/8,1/2,1/2};

CoeffF4Norm=CoeffF4NormF4;

(* Resets CoeffF4 to all zeros. *)
ResetCoeffF4[]:=ResetCoeffF4[False];
ResetCoeffF4[printOut_?BooleanQ]:=Module[{},
If[printOut,Print["Resetting CoeffF4."]];
CoeffF4=CoeffF4Zero;
IsGradientInitialized=False;
];

(* Updates CoeffF4. *)
UpdateCoeffF4[idx_?IntegerQ,coeffVal_?NumericQ]:=UpdateCoeffF4[idx,coeffVal,False];
UpdateCoeffF4[idx_?IntegerQ,coeffVal_?NumericQ,printOut_?BooleanQ]:=Module[{},
If[printOut,Print["Updating CoeffF4."]];
If[idx < 1 || idx > NoTermCnt,(Print["UpdateCoeffF4::idx is out of range. idx = ", idx]; ResetCoeffF4[];Return[];)];
CoeffF4[[1,idx]] = coeffVal;
IsGradientInitialized=False;
];

UpdateAllCoeffF4[allCoeff_?VectorQ]:=Module[{ii},
If[Length[allCoeff]!= NoOfTermsF4,
(
Print["UpdateAllCoeffF4::allCoeff have incorrect length = ",Length[allCoeff]];
Abort[];
Return[];
)
];

ResetCoeffF4[];

For[ii=1,ii <= NoOfTermsF4,ii++,
(
UpdateCoeffF4[ii,allCoeff[[ii]]];
)
];
];

(* Calculates invariants, which are needed for calculation of all gradient components and stores them in global variables *)
CalculateP4Invariants[sab_?MatrixQ]:=Module[{aa,bb,i1},
(* Print["CalculateP4Invariants::Starting."]; *)
CalculateFP4CommonInvariants[sab];

SumSmS2by1=Table[Sum[S2mS[[i1,aa]],{i1,1,NNN}],{aa,1,NNN}];
SumSSumSby1p2by1=Table[Sum[sab[[aa,i1]]*SumSby1[[i1]]^2,{i1,1,NNN}],{aa,1,NNN}];
SumSx1Sy1S12=Table[Sum[sab[[aa,i1]]*sab[[bb,i1]]*SumSby1[[i1]],{i1,1,NNN}],{aa,1,NNN},{bb,1,NNN}];

(* Helpers *)
SumSx1p3 = SumS3by1;
SumSx1p2 = SumS2by1;
SumSx1 = SumSby1;

SumSx1S12S23 = SumSp3by1;
SumSx1S12S13 = SumSSumSby1p2by1;

SumSx1p2S12 = SumS2mSby1;
SumSx1S12p2 = SumSmS2by1;
SumSx1S12 = SumSp2by1;
SumSx1Sy2S12 = Sp3;

(* SumSx1Sy1S12[[x,y]] = !!! SAME !!!; *)
SumSx1p2Sy1 = S2mS;
SumSx1Sy1 = Sp2;
];

(* If returnTangentialGradient then returns tangential gradient. Otherwise return the original gradient. *)
TangentialGradient[gradVal:{__},returnTangentialGradient_?BooleanQ]:=Module[{retVal,gNVal},
If[!returnTangentialGradient,
retVal=gradVal,
(
(* Normal part of gradient *)
gNVal=(1/NA)*Sum[gradVal[[ii]],{ii,1,NA}];
(* Print["Normal component of Gradient - gNVal = ", gNVal]; *)

(* Subtracting scalar from each of the component of the vector gradVal *)
retVal=gradVal-gNVal;
(* Print["Tangential component of Gradient - gTVal = ", gNVal]; *)
),
retVal=Indeterminate
];

Return[retVal];
];

(* ==============================================*)

(* Linear version of analytical expression for gradient for all known terms. *)
cntGradNumeric=0;
cntGradP2P4P8sAllLinearNumeric=0;
GradP2P4P8sAllLinearNumeric[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=Module[{sab,gradVal,gradValP4,gradValP2,gradSorted,sabSorted,idx},
cntGradP2P4P8sAllLinearNumeric++;
cntGradNumeric++;
sab=ToMatrix[sabLinear];
gradValP4=GradP4All[sab,False];
gradValP2=GradP2All[sab,False];
gradVal=If[UseP8sGrad,(gradValP2+gradValP4)*(1+2*coeffP8sGrad*(FP2All[sab,False]+FP4All[sab,False])),(gradValP2+gradValP4)];
Return[TangentialGradient[gradVal,returnTangentialGradient]];
]/; VectorQ[sabLinear,NumericQ];

cntGradP2P4P8sX4AllLinearNumeric=0;
GradP2P4P8sX4AllLinearNumeric[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=Module[{sab,gradVal,gradValP4,gradValP2,gradSorted,sabSorted,idx},
cntGradP2P4P8sX4AllLinearNumeric++;
cntGradNumeric++;
sab=ToMatrix[sabLinear];
gradValP4=GradP4All[sab,False];
gradValP2=GradP2All[sab,False];
gradVal=If[UseP8sGrad,(gradValP2+gradValP4*(1+2*coeffP8sGrad*FP4All[sab,False])),(gradValP2+gradValP4)];
Return[TangentialGradient[gradVal,returnTangentialGradient]];
]/; VectorQ[sabLinear,NumericQ];

cntGradP2P4P8s1AllLinearNumeric=0;
GradP2P4P8s1AllLinearNumeric[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=Module[{sab,gradVal,gradValP4,gradValP2,gradSorted,sabSorted,idx,gradValP8},
cntGradP2P4P8s1AllLinearNumeric++;
cntGradNumeric++;
sab=ToMatrix[sabLinear];
gradValP4=GradP4All[sab,False];
gradValP2=GradP2All[sab,False];
gradVal=(gradValP2+gradValP4);

If[UseP8sGrad,
(
gradValP8=16*coeffP8sGrad*SumS12p2^3*Table[sabLinear[[idx]],{idx,1,NA}];
gradVal+=gradValP8;
)
];

Return[TangentialGradient[gradVal,returnTangentialGradient]];
] /; VectorQ[sabLinear,NumericQ] ;

(* ==============================================*)

cntGradP4P8sAllLinearNumeric=0;
GradP4P8sAllLinearNumeric[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=Module[{sab,gradVal,gradSorted,sabSorted,idx},
cntGradP4P8sAllLinearNumeric++;
cntGradNumeric++;
sab=ToMatrix[sabLinear];
gradVal=GradP4All[sab,False];
If[UseP8sGrad,gradVal=gradVal*(1+2*coeffP8sGrad*FP4All[sab,False])];
Return[TangentialGradient[gradVal,returnTangentialGradient]];
]/; VectorQ[sabLinear,NumericQ];

cntGradP4P8s1AllLinearNumeric=0;
GradP4P8s1AllLinearNumeric[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=Module[{sab,gradVal,gradValP4,gradSorted,sabSorted,idx,gradValP8},
cntGradP4P8s1AllLinearNumeric++;
cntGradNumeric++;
sab=ToMatrix[sabLinear];
gradValP4=GradP4All[sab,False];
gradVal=(gradValP4);

If[UseP8sGrad,
(
gradValP8=16*coeffP8sGrad*SumS12p2^3*Table[sabLinear[[idx]],{idx,1,NA}];
gradVal+=gradValP8;
)
];

Return[TangentialGradient[gradVal,returnTangentialGradient]];
]/; VectorQ[sabLinear,NumericQ] ;


(* ==============================================*)

GradP4AllLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=Module[{sab},
sab=ToMatrix[sabLinear];
Return[GradP4All[sab,returnTangentialGradient]];
];

cntGradP4AllLinearNumeric=0;
GradP4AllLinearNumeric[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=Module[{sab,gradVal,gradSorted,sabSorted,idx},
cntGradP4AllLinearNumeric++;
cntGradNumeric++;
sab=ToMatrix[sabLinear];
gradVal=GradP4All[sab,False];
(*
Print["GradP4AllLinearNumeric::cntGradP4AllLinearNumeric = ", cntGradP4AllLinearNumeric , " sabLinear[[1]] = ",sabLinear[[1]],", CoeffF4 = ", CoeffF4, ", retVal = ", retVal];
*)
(*
sabSorted=Sort[sabLinear];
gradSorted=SortByFirst[sabLinear,retVal];
Print["GradP4AllLinearNumeric::cntGradP4AllLinearNumeric = ", cntGradP4AllLinearNumeric , Plot[SValFunc[idx,sabSorted],{idx,1,NA},Evaluate[defaultPltOpts]],Plot[SValFunc[idx,gradSorted],{idx,1,NA},Evaluate[defaultPltOpts]]];
*)
Return[TangentialGradient[gradVal,returnTangentialGradient]];
]/; VectorQ[sabLinear,NumericQ];

(* Matrix version of analytical expression for gradient for for all known terms. *)
(* Uses global values of coefficients as stored in CoeffF4. *)
GradP4All[sab_?MatrixQ,returnTangentialGradient_?BooleanQ]:=Module[{gradVal,gradValTbl,ii},
CalculateP4Invariants[sab];
gradValTbl=Table[(GradP4FuncList[[ii]])[sab,False,False],{ii,1,NoOfTermsF4}];
gradVal=Sum[CoeffF4[[1,ii]]*gradValTbl[[ii]],{ii,1,NoOfTermsF4}];
Return[TangentialGradient[gradVal,returnTangentialGradient]];
];

(* ============================================= *)
(* ============================================= *)
(* ============================================= *)
Print["TODO::STM_P4Grad::The code below either is not used or should not be used. Remove."];

(* Numeric linear gradient. *)
Grad4NumericLinearFunc[sLab:{__},returnInvariants_?BooleanQ, returnTangentialGradient_?BooleanQ]:=Module[{gradVal,jj,slVal,var,sVal,gVal,gNVal,retVal},
gradVal=Table[Indeterminate,{jj,1,NA}];

For[jj=1,jj<= NA,jj++,
(
slVal=sLab;
slVal[[jj]]=var;
sVal=ToMatrix[slVal];
gVal=ND[F4Func[sVal,returnInvariants],var,sLab[[jj]], Scale -> GradScaleValue];
gradVal[[jj]]=gVal;
)
];

Return[TangentialGradient[gradVal,returnTangentialGradient]];
];

(* Symbolic Gradient. Returns linearized vector of gradients. *)
Grad4SymbLinearFunc[returnInvariants_?BooleanQ, returnTangentialGradient_?BooleanQ]:=Module[{retVal,f4Val,gradVal,gNVal},

f4Val=F4Func[S,returnInvariants];

(* Gradient *)
gradVal=Table[D[f4Val,SL[[ii]]],{ii,1,NA}];
(* Print["Gradient - gVal = ", gVal]; *)

retVal=TangentialGradient[gradVal,returnTangentialGradient];

If[ExpandGradient,
(
PrintTimeUsed[];

Print["Expanding gradient."];
retVal=Expand[retVal];
PrintTimeUsed[];
)
];

Return[retVal];
];

(* Gradient must be reinitialized after each uipdate to coefficients CoeffF4 *)
IsGradientInitialized=False;

InitializeGradient[returnInvariants_?BooleanQ, returnTangentialGradient_?BooleanQ]:=Module[{},
Print["InitializeGradient::Initializing with global strGrad4Flat and strGrad4."];
IsGradientInitialized=False;
Print["InitializeGradient::Using " <>  If[returnInvariants, "invariants","coefficients"] <>" and " <> If[returnTangentialGradient,"tangential", "full"] <> " gradient."];
strGrad4Flat="Grad4SymbFlatFunc[" <> SLV <> "]:=" <>ToString[InputForm[Grad4SymbLinearFunc[returnInvariants,returnTangentialGradient]]];

strGrad4="Grad4SymbFunc[sab:{{__},___}]:=Grad4SymbFlatFunc[" <> SLVM <> "]";

(*
Print["InitializeGradient::strGrad4Flat = ", strGrad4Flat];
Print["InitializeGradient::strGrad4 = ", strGrad4];
*)

(* TODO: Evaluate below doesn't work inside a module. *)
(*
Print["InitializeGradient::Evaluating Grad4SymbFlatFunc & Grad4SymbFunc."];
Evaluate[ToExpression[strGrad4Flat]];
Evaluate[ToExpression[strGrad4]];

Print["Definition of Grad4SymbFlatFunc = ", Definition[Grad4SymbFlatFunc]];
Print["Definition of Grad4SymbFunc = ", Definition[Grad4SymbFunc]];

*)

IsGradientInitialized=True;
PrintTimeUsed[];
Return[];
];

