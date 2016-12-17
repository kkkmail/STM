(* :Author:Konstantin K.Konstantinov *)
(* :Summary:4th power polynom minimization function and its decomposition into invariants. *)
(* :Copyright: K^3, 2013 - 2014 *)
(* :Version: Revision: 1.23.001, Date: 2014/02/21 *)
(* :Mathematica Version:7.0-9.0 *)
(* ==============================================*)
InitializeF4Model[]:=Module[{noOfElements,f4Val},
Print["Initializing P4 model."];
noOfElements=8;
Reset[];
Initialize[noOfElements];
InitializeTermsF4[];
f4Val=F4FuncMult[S];
IsInitialized=True;
];

(* ==============================================*)

(* F4 abd P4 common invariants. *)
CalculateFP4CommonInvariants[sab_?MatrixQ]:=Module[{aa,bb,i1},
(* Invariants *)
(* Print["CalculateFP4CommonInvariants::Starting."]; *)
Sp2 = sab . sab;
Sp3 = Sp2 . sab;
S2 = sab^2;
S3 = sab^3;
S2mS = S2 . sab;

TrSp3=Tr[Sp3];

(* Common Invariants *)
SumSby1=Table[Sum[sab[[aa,i1]],{i1,1,NNN}],{aa,1,NNN}];
SumS2by1=Table[Sum[S2[[aa,i1]],{i1,1,NNN}],{aa,1,NNN}];
SumS3by1=Table[Sum[S3[[aa,i1]],{i1,1,NNN}],{aa,1,NNN}];

SumSp2by1=Table[Sum[Sp2[[aa,i1]],{i1,1,NNN}],{aa,1,NNN}];
SumSp3by1=Table[Sum[Sp3[[aa,i1]],{i1,1,NNN}],{aa,1,NNN}];

SumS12p3 = Sum[SumS3by1[[i1]],{i1,1,NNN}];
SumS12p2 = Sum[SumS2by1[[i1]],{i1,1,NNN}];
SumS12 = Sum[SumSby1[[i1]],{i1,1,NNN}];

SumS2mSby1=Table[Sum[S2mS[[aa,i1]],{i1,1,NNN}],{aa,1,NNN}];

SumS12S13S14 = Sum[SumSby1[[i1]]^3,{i1,1,NNN}];
SumS12S13S24 = Sum[SumSp3by1[[i1]],{i1,1,NNN}];

SumS12S13S23 = TrSp3;
SumS12S13 = Sum[SumSp2by1[[i1]],{i1,1,NNN}];
SumS12p2S13 = Sum[SumS2mSby1[[i1]],{i1,1,NNN}];
];

(* ==============================================*)

CalculateF4Invariants[sab_?MatrixQ]:=CalculateF4Invariants[sab,True];
CalculateF4Invariants[sab_?MatrixQ,calculateCommonInvariants_?BooleanQ]:=Module[{aa,bb,i1},
(* Print["CalculateF4Invariants::Starting."]; *)
If[calculateCommonInvariants,CalculateFP4CommonInvariants[sab]];

Sp4=Sp3 . sab;
S4=sab^4;
S3mS=S3 . sab;
S2mS2=S2 . S2;
SmS2mS=sab . S2 . sab;
S2mSp2=S2 . Sp2;

(* Invariants *)
SumSp4=Sum[Sp4[[aa,bb]],{aa,1,NNN},{bb,1,NNN}];
SumS12S13S14S15=Sum[SumSby1[[aa]]^4,{aa,1,NNN}];
SumS12S13S14S25=Sum[SumSby1[[aa]]^2*SumSp2by1[[aa]],{aa,1,NNN}];
SumS12p2S13S14=Sum[SumSby1[[aa]]^2*SumS2by1[[aa]],{aa,1,NNN}];
SumS12p2S13S24=Sum[SmS2mS[[aa,bb]],{aa,1,NNN},{bb,1,NNN}];
SumS12p2S23S34=Sum[S2mSp2[[aa,bb]],{aa,1,NNN},{bb,1,NNN}];
SumS12S13S14S23=Sum[SumSby1[[aa]]*Sp3[[aa,aa]],{aa,1,NNN}];
SumS12S13S24S34=Tr[Sp4];
SumS3mS=Sum[S3mS[[aa,bb]],{aa,1,NNN},{bb,1,NNN}];
SumS2mS2=Sum[S2mS2[[aa,bb]],{aa,1,NNN},{bb,1,NNN}];
SumS12p2S13S23=Tr[S2mSp2];
SumS12p4=Sum[S4[[aa,bb]],{aa,1,NNN},{bb,1,NNN}];
];

(* ==============================================*)

(* Matrix version of analytical expression for function for for all known terms. *)
(* Uses global values of coefficients as stored in CoeffF4. *)

FP4AllLinear[sabLinear_?VectorQ]:=Module[{sab},
sab=ToMatrix[sabLinear];
Return[FP4All[sab]];
];

(* ==============================================*)

cntNumeric=0;
cntFP4AllLinearNumeric=0;
FP4AllLinearNumeric[sabLinear_?VectorQ]:=Module[{sab,fVal4},
cntFP4AllLinearNumeric++;
cntNumeric++;
sab=ToMatrix[sabLinear];
fVal4=FP4All[sab];
Return[fVal4];
]/; VectorQ[sabLinear,NumericQ];

(* ==============================================*)

cntFP2P4P8sAllLinearNumeric=0;
FP2P4P8sAllLinearNumeric[sabLinear_?VectorQ]:=Module[{sab,fVal4,fVal2,fVal},
cntFP2P4P8sAllLinearNumeric++;
cntNumeric++;
sab=ToMatrix[sabLinear];
fVal4=FP4All[sab];
fVal2=FP2All[sab,False];
fVal=fVal2+fVal4;
If[UseP8sGrad,fVal=fVal+coeffP8sGrad*fVal^2];
Return[fVal];
]/; VectorQ[sabLinear,NumericQ];

cntFP2P4P8sX4AllLinearNumeric=0;
FP2P4P8sX4AllLinearNumeric[sabLinear_?VectorQ]:=Module[{sab,fVal4,fVal2,fVal},
cntFP2P4P8sX4AllLinearNumeric++;
cntNumeric++;
sab=ToMatrix[sabLinear];
fVal4=FP4All[sab];
fVal2=FP2All[sab,False];
fVal=If[UseP8sGrad,(fVal2+fVal4+coeffP8sGrad*fVal4^2),(fVal2+fVal4)];
Return[fVal];
]/; VectorQ[sabLinear,NumericQ];

(* FP2P4P8s1AllLinearNumeric adds SumAll[S[[a,b]]^2,{a,b}]^4 *)
cntFP2P4P8s1AllLinearNumeric=0;
FP2P4P8s1AllLinearNumeric[sabLinear_?VectorQ]:=Module[{sab,fVal4,fVal2,fVal},
cntFP2P4P8s1AllLinearNumeric++;
cntNumeric++;
sab=ToMatrix[sabLinear];
fVal4=FP4All[sab];
fVal2=FP2All[sab,False];
fVal=If[UseP8sGrad,(fVal2+fVal4+coeffP8sGrad*SumS12p2^4),(fVal2+fVal4)];
Return[fVal];
]/; VectorQ[sabLinear,NumericQ]; 

(* ==============================================*)

cntFP4P8sAllLinearNumeric=0;
FP4P8sAllLinearNumeric[sabLinear_?VectorQ]:=Module[{sab,retVal},
cntFP4P8sAllLinearNumeric++;
cntNumeric++;
sab=ToMatrix[sabLinear];
retVal=FP4All[sab];
If[UseP8sGrad,retVal=retVal+coeffP8sGrad*retVal^2];
Return[retVal];
]/; VectorQ[sabLinear,NumericQ];

(* FP4P8s1AllLinearNumeric adds SumAll[S[[a,b]]^2,{a,b}]^4 *)
cntFP4P8s1AllLinearNumeric=0;
FP4P8s1AllLinearNumeric[sabLinear_?VectorQ]:=Module[{sab,fVal4,fVal},
cntFP4P8s1AllLinearNumeric++;
cntNumeric++;
sab=ToMatrix[sabLinear];
fVal4=FP4All[sab];
fVal=If[UseP8sGrad,(fVal4+coeffP8sGrad*SumS12p2^4),(fVal4)];
Return[fVal];
]/; VectorQ[sabLinear,NumericQ]; 

(* ==============================================*)

FP4All[sab_?MatrixQ]:=FP4All[sab,True];
FP4All[sab_?MatrixQ,calculateCommonInvariants_?BooleanQ]:=Module[{funcVal,funcValTbl,ii},
(* Print["FP4All::Starting."]; *)
CalculateF4Invariants[sab,calculateCommonInvariants];
funcValTbl=Table[(FP4FuncList[[ii]])[sab,False],{ii,1,NoOfTermsF4}];

funcVal=Sum[CoeffF4[[1,ii]]*funcValTbl[[ii]],{ii,1,NoOfTermsF4}];
Return[funcVal];
];

(* List of all FP4 functions. *)
FP4FuncList:={FP4T8Cabcdefgh,FP4T6Cabbdefgh,FP4T5Cabbdbfgh,FP4T4Cabbdbfbh,FP4T4Cababefgh,FP4T4Cabbddfgh,FP4T4Cabbdeffh,FP4T3Cababbfgh,FP4T3Cabbddfdh,FP4T2Cababbfbh,FP4T2Cababeffh,FP4T2Cabbddffh,FP4T2Cabbdbddh,FP4T2Cabababgh,FP4T2Cabbdadgh,FP4T1Cababbffh,FP4T1Cabbdaddh,FP4T1Cabababbh,FP4T0Cababbfbf,FP4T0Cababefef,FP4T0Cabbddfaf,FP4T0Cabbdbdad,FP4T0Cabababab};

(* Function, which returns specifically only Cabbddfaf term. *)
F4T0Cabbddfaf[sab:{{__},___}]:=Module[{retVal,oldCoeffF4},
oldCoeffF4=CoeffF4;
ResetCoeffF4[];
UpdateCoeffF4[idxD4T0Cabbddfaf,1];
retVal=F4Func[sab,False];
CoeffF4=oldCoeffF4;
Return[retVal];
];

(* ==============================================*)

(* Function, which returns invariants instead of multipliers (Tr[CoeffF4.invValTbl]) *)
F4FuncInv[sab:{{__},___}]:=F4Func[sab, True];

(* *Function, which returns multipliers (Tr[CoeffF4.invTrInvCoeffTbl.invValTbl]) *)
F4FuncMult[sab:{{__},___}]:=F4Func[sab,False];

(* Numeric version of F4Func *)
F4FuncNumeric[dummy_?NumberQ,sab:{{__},___},returnInvariants_?BooleanQ]:=F4Func[sab,returnInvariants];

(* Main 4th polynom minimization function. *)
F4Func[sab:{{__},___},returnInvariants_?BooleanQ]:=Module[{retVal,nn,SEE,S2,Sp2,SEE2,S3,Sp3,SEE3,vecSp3Diag,S4,Sp4,SEE4,TrSEE,TrS2EE,TrSp2EE,TrS3EE,TrSp3EE,TrSp3,TrSEE3,TrS4EE,TrSp4EE,TrSp4,TrSEE4,TrSEEp4,TrSp2EEmTrSEEp2,TrSEE3mTrSEE,TrS2EEmTrSEE,TrSp3EEmTrSEE,TrSp2EEp2,TrS2SEEmTrSEE,TrSp2EEmSEE2,TrS2EEmSEE2,TrS2EEmTrSp2EE,TrSS2SEE,TrS3EEmTrSEE,TrSp3mTrSEE,TrSp2S2EE,TrEITSvecSp3Diag,TrSS3EE,TrS2S2EE,TrS2EEp2,TrSp2Sp2,invValTbl},

If[UseComplexVariablesValue,
(
Print["TODO::F4Func is not implemented yet for complex variables!!!"];
Return[Indeterminate];
)
];

nn=Length[sab];

SEE=sab . EE;

S2=sab^2;
Sp2=sab.sab;
SEE2=SEE^2;

S3=sab^3;
Sp3=Sp2.sab;
SEE3=SEE^3;
vecSp3Diag=Table[{Sp3[[ii,ii]]},{ii,1,nn}];

S4=sab^4;
Sp4=Sp3.sab;
SEE4=SEE^4;

TrSEE=Tr[SEE];

TrS2EE=Tr[EIT.S2.EI];
TrSp2EE=Tr[EIT.Sp2.EI];

TrS3EE=Tr[EIT.S3.EI];
TrSp3EE=Tr[EIT.Sp3.EI];
TrSp3=Tr[Sp3];
TrSEE3=Tr[SEE3];

TrS4EE=Tr[EIT.S4.EI];
TrSp4EE=Tr[EIT.Sp4.EI];
TrSp4=Tr[Sp4];
TrSEE4=Tr[SEE4];

(* Invariants not calculated above *)
TrSEEp4=(1/16)*TrSEE^4;
TrSp2EEmTrSEEp2=(1/8)*TrSp2EE*TrSEE^2;
TrSEE3mTrSEE=(1/2)*TrSEE3*TrSEE;
TrS2EEmTrSEE=(1/8)*TrS2EE*TrSEE^2;
TrSp3EEmTrSEE=(1/4)*TrSp3EE*TrSEE;
TrSp2EEp2=(1/4)*TrSp2EE^2;
TrS2SEEmTrSEE=(1/2)*Tr[(EIT.S2).(sab.EI)]*TrSEE;
TrSp2EEmSEE2=Tr[(Sp2.EE)*SEE2];
TrS2EEmSEE2=(1/2)*Tr[(S2.EE)*SEE2];
TrS2EEmTrSp2EE=(1/4)*TrS2EE*TrSp2EE;
TrSS2SEE=(1/2)*Tr[((EIT . sab) . S2) . (sab . EI)];
TrS3EEmTrSEE=(1/4)*TrS3EE*TrSEE;
TrSp3mTrSEE=(1/12)*TrSp3*TrSEE;
TrSp2S2EE=Tr[(EIT . Sp2) . (S2 . EI)];
TrEITSvecSp3Diag=(1/2)*Tr[(EIT .sab ). vecSp3Diag];
TrSS3EE=Tr[(EIT . sab) . (S3 . EI)];
TrS2S2EE=(1/2)*Tr[(EIT . S2) . (S2 . EI)];
TrS2EEp2=(1/4)*TrS2EE^2;
TrSp2Sp2=(1/2)*Tr[Sp2.S2];

(* Adding Invariants if initialization is not completed. *)
If[!IsInitialized,
(
idxTrSEEp4=NextInvariantNo[];
AddInvariant[idxTrSEEp4,"TrSEEp4",TrSEEp4,"(1/16)*Tr[S . EE]^4"];

idxTrSp2EEmTrSEEp2=NextInvariantNo[];
AddInvariant[idxTrSp2EEmTrSEEp2,"TrSp2EEmTrSEEp2",TrSp2EEmTrSEEp2,"(1/8)*Tr[Sp2 . EE]*Tr[S . EE]^2"];

idxTrSEE3mTrSEE=NextInvariantNo[];
AddInvariant[idxTrSEE3mTrSEE,"TrSEE3mTrSEE",TrSEE3mTrSEE,"(1/2)*Tr[SEE3]*Tr[S . EE]"];

idxTrSEE4=NextInvariantNo[];
AddInvariant[idxTrSEE4,"TrSEE4",(1/2)*TrSEE4,"(1/2)*Tr[SEE4]"];

idxTrS2EEmTrSEEp2=NextInvariantNo[];
AddInvariant[idxTrS2EEmTrSEEp2,"TrS2EEmTrSEE",TrS2EEmTrSEE,"(1/8)*Tr[S2 . EE]*Tr[S . EE]^2"];

idxTrSp3EEmTrSEE=NextInvariantNo[];
AddInvariant[idxTrSp3EEmTrSEE,"TrSp3EEmTrSEE",TrSp3EEmTrSEE,"(1/4)*Tr[Sp3 . EE]*Tr[S . EE]"];

idxTrSp2EEp2=NextInvariantNo[];
AddInvariant[idxTrSp2EEp2,"TrSp2EEp2",TrSp2EEp2,"(1/4)*Tr[Sp2 . EE]^2"];

idxTrS2SEEmTrSEE=NextInvariantNo[];
AddInvariant[idxTrS2SEEmTrSEE,"TrS2SEEmTrSEE",TrS2SEEmTrSEE,"(1/2)*Tr[(EIT.S2).(sab.EI)]*Tr[S . EE]"];

idxTrSp2EEmSEE2=NextInvariantNo[];
AddInvariant[idxTrSp2EEmSEE2,"TrSp2EEmSEE2",TrSp2EEmSEE2,"Tr[(Sp2.EE)*SEE2]"];

idxTrS2EEmSEE2=NextInvariantNo[];
AddInvariant[idxTrS2EEmSEE2,"TrS2EEmSEE2",TrS2EEmSEE2,"(1/2)*Tr[(S2.EE)*SEE2]"];

idxTrS2EEmTrSp2EE=NextInvariantNo[];
AddInvariant[idxTrS2EEmTrSp2EE,"TrS2EEmTrSp2EE",TrS2EEmTrSp2EE,"(1/4)*Tr[S2 . EE]*Tr[Sp2 . EE]"];

idxTrSp4EE=NextInvariantNo[];
AddInvariant[idxTrSp4EE,"TrSp4EE",(1/2)*TrSp4EE,"(1/2)*Tr[Sp4 . EE]"];

idxTrSS2SEE=NextInvariantNo[];
AddInvariant[idxTrSS2SEE,"TrSS2SEE",TrSS2SEE,"(1/2)*Tr[((EIT . S) . S2) . (S . EI)]"];

idxTrS3EEmTrSEE=NextInvariantNo[];
AddInvariant[idxTrS3EEmTrSEE,"TrS3EEmTrSEE",TrS3EEmTrSEE,"(1/4)*Tr[S3 . EE]*Tr[S . EE]"];

idxTrSp3mTrSEE=NextInvariantNo[];
AddInvariant[idxTrSp3mTrSEE,"TrSp3mTrSEE",TrSp3mTrSEE,"(1/12)*Tr[Sp3]*Tr[S . EE]"];

idxTrSp2S2EE=NextInvariantNo[];
AddInvariant[idxTrSp2S2EE,"TrSp2S2EE",TrSp2S2EE,"Tr[Sp2 . S2 . EE]"];

idxTrEITSvecSp3Diag=NextInvariantNo[];
AddInvariant[idxTrEITSvecSp3Diag,"TrEITSvecSp3Diag",TrEITSvecSp3Diag,"(1/2)*Tr[(EIT .sab ). vecSp3Diag]"];

idxTrSS3EE=NextInvariantNo[];
AddInvariant[idxTrSS3EE,"TrSS3EE",TrSS3EE,"Tr[sab . S3 . EE]"];

idxTrS2S2EE=NextInvariantNo[];
AddInvariant[idxTrS2S2EE,"TrS2S2EE",TrS2S2EE,"(1/2)*Tr[S2 .S2 . EE]"];

idxTrS2EEp2=NextInvariantNo[];
AddInvariant[idxTrS2EEp2,"TrS2EEp2",TrS2EEp2,"(1/4)*Tr[S2 . EE]^2"];

idxTrSp4=NextInvariantNo[];
AddInvariant[idxTrSp4,"TrSp4",(1/2)*TrSp4,"(1/2)*Tr[Sp4]"];

idxTrSp2Sp2=NextInvariantNo[];
AddInvariant[idxTrSp2Sp2,"TrSp2Sp2",TrSp2Sp2,"(1/2)*Tr[Sp2.S2]"];

idxTrS4EE=NextInvariantNo[];
AddInvariant[idxTrS4EE,"TrS4EE",(1/2)*TrS4EE,"(1/2)*Tr[S4 . EE]"];

InitializeInvariantMatrix[];

retVal=Indeterminate;
),
(
invValTbl=Table[{0},{ii,1,NoOfTermsF4}];

invValTbl[[idxTrSEEp4,1]]=TrSEEp4;
invValTbl[[idxTrSp2EEmTrSEEp2,1]]=TrSp2EEmTrSEEp2;
invValTbl[[idxTrSEE3mTrSEE,1]]=TrSEE3mTrSEE;
invValTbl[[idxTrSEE4,1]]=(1/2)*TrSEE4;
invValTbl[[idxTrS2EEmTrSEEp2,1]]=TrS2EEmTrSEE;
invValTbl[[idxTrSp3EEmTrSEE,1]]=TrSp3EEmTrSEE;
invValTbl[[idxTrSp2EEp2,1]]=TrSp2EEp2;
invValTbl[[idxTrS2SEEmTrSEE,1]]=TrS2SEEmTrSEE;
invValTbl[[idxTrSp2EEmSEE2,1]]=TrSp2EEmSEE2;
invValTbl[[idxTrS2EEmSEE2,1]]=TrS2EEmSEE2;
invValTbl[[idxTrS2EEmTrSp2EE,1]]=TrS2EEmTrSp2EE;
invValTbl[[idxTrSp4EE,1]]=(1/2)*TrSp4EE;
invValTbl[[idxTrSS2SEE,1]]=TrSS2SEE;
invValTbl[[idxTrS3EEmTrSEE,1]]=TrS3EEmTrSEE;
invValTbl[[idxTrSp3mTrSEE,1]]=TrSp3mTrSEE;
invValTbl[[idxTrSp2S2EE,1]]=TrSp2S2EE;
invValTbl[[idxTrEITSvecSp3Diag,1]]=TrEITSvecSp3Diag;
invValTbl[[idxTrSS3EE,1]]=TrSS3EE;
invValTbl[[idxTrS2S2EE,1]]=TrS2S2EE;
invValTbl[[idxTrS2EEp2,1]]=TrS2EEp2;
invValTbl[[idxTrSp4,1]]=(1/2)*TrSp4;
invValTbl[[idxTrSp2Sp2,1]]=TrSp2Sp2;
invValTbl[[idxTrS4EE,1]]=(1/2)*TrS4EE;

retVal=If[returnInvariants,Tr[CoeffF4.invValTbl],Tr[CoeffF4.invTrInvCoeffTbl.invValTbl],Indeterminate];
)
];

Return[retVal];
];

(* ==============================================*)

(* Initialized global invariant mapping matrix invTrInvCoeffTbl and some helpers. *) (* This matrix allows calculating sums of various symmetries using invariants. *)
InitializeInvariantMatrix[]:=Module[{},
Print["InitializeInvariantMatrix::Starting..."];

If[!UseComplexVariablesValue,
(
Print["Matrix of coefficients invCoeffTbl."];
invCoeffTbl=Table[Coefficient[InvariantExpressionMatrix[[ii]],TermExampleMatrix[[jj]]],{ii,1,NoInvariantCnt},{jj,1,NoTermCnt}];
invCoeffTblSm=Table[Coefficient[InvariantExpressionMatrix[[ii]],TermExampleMatrix[[jj]]],{ii,1,NoInvariantCnt},{jj,2,NoTermCnt}];
termsTbl=Table[{ii,TermExampleMatrix[[ii]]},{ii,1,NoTermCnt}];
Print["termsTbl = ",termsTbl//MatrixForm];
PrintTimeUsed[];
invCoeffTblNamed=Table[If[jj==0,InvariantMatrix[[ii]],If[jj==NoTermCnt+1,InvariantStrExpressionMatrix[[ii]],Coefficient[InvariantExpressionMatrix[[ii]],TermExampleMatrix[[jj]]]]],{ii,1,NoInvariantCnt},{jj,0,NoTermCnt+1}];
Print["invCoeffTblNamed = ",invCoeffTblNamed//MatrixForm];
invCoeffTblRowCnt=Length[invCoeffTbl];
invCoeffTblColCnt=Length[invCoeffTbl[[1]]];
PrintTimeUsed[];

Print["Checking for linear independence of invCoeffTbl."];

If[invCoeffTblRowCnt==invCoeffTblColCnt,
(
detinvCoeffTbl=Det[invCoeffTbl];
rnk=MatrixRank[invCoeffTbl];
Print["detinvCoeffTbl = ",detinvCoeffTbl,", rnk = ",rnk];
PrintTimeUsed[];
(*invTrInvCoeffTbl=Transpose[Inverse[invCoeffTbl]];
Print["invTrInvCoeffTbl (Transpose[Inverse[invCoeffTbl]]) = ",invTrInvCoeffTbl//MatrixForm];*)invTrInvCoeffTbl=Inverse[invCoeffTbl];
Print["invTrInvCoeffTbl (Inverse[invCoeffTbl]) = ",invTrInvCoeffTbl//MatrixForm];
),
(
Print["Number of rows in the table of invariants, invCoeffTblRowCnt = ",invCoeffTblRowCnt," != number of columns, invCoeffTblColCnt = ",invCoeffTblColCnt];
invCoeffTblRowCntSm=Length[invCoeffTblSm];
invCoeffTblColCntSm=Length[invCoeffTblSm[[1]]];

If[invCoeffTblRowCntSm==invCoeffTblColCntSm,
(
detSm=Det[invCoeffTblSm];
Print["detSm = ",detSm];
),
(
Print["Small matrix is not square."];
)
];

rnkSmall=MatrixRank[invCoeffTbl];
Print["Rank of small matrix (rnkSmall) = ",rnkSmall];
)
];
Print[strSeparatorSmall];
),
(
Print["TODO::InitializeInvariantMatrix is not implemented yet for complex variables!!!"];
)
];
];

(* ==============================================*)
