(* :Author:Konstantin K.Konstantinov *)
(* :Summary: Implementation Y derivatives. *)
(* :Copyright: K^3, 2013 - 2014 *)
(* :Version: Revision: 1.23.001, Date: 2014/02/21 *)
(* :Mathematica Version:7.0-9.0 *)
(* ==============================================*)

(* Complex version ... *)
CalculateYInvariants[wab_?MatrixQ]:=Module[{aa,bb,i1,i2,i3,i4,i5,i6,wabT},
wabT=Transpose[wab];

(* ==============================================*)

Sp2 = wab . wab;
Sp3 = Sp2 . wab;
Sp4=Sp3 . wab;
Sp5=Sp4 . wab;

StS=wabT . wab;
StSp2=wabT . Sp2;

TrSp2=Tr[Sp2];
TrSp3=Tr[Sp3];
TrSp4=Tr[Sp4];
TrSp5=Tr[Sp5];

SumSby1=Table[Sum[wab[[i1,aa]],{i1,1,NNN}],{aa,1,NNN}];
SumSby2=Table[Sum[wab[[aa,i2]],{i2,1,NNN}],{aa,1,NNN}];

(* ==============================================*)

SumS12 = Sum[wab[[i1,i2]],{i1,1,NNN},{i2,1,NNN}];
SumS12S21=TrSp2;
SumS12S31=Sum[Sp2[[i3,i2]],{i3,1,NNN},{i2,1,NNN}];

SumS12S31S43S54S65=Sum[Sp5[[i6,i2]],{i6,1,NNN},{i2,1,NNN}];

SumS12S31S43S54=Sum[Sp4[[i5,i2]],{i5,1,NNN},{i2,1,NNN}];
SumS12S23S34S45S51=TrSp5;

SumS12S13S14=Sum[SumSby2[[i1]]^3,{i1,1,NNN}];
SumS12S13S24=Sum[StSp2[[i3,i4]],{i3,1,NNN},{i4,1,NNN}];
SumS12S31S43=Sum[Sp3[[i4,i2]],{i4,1,NNN},{i2,1,NNN}];
SumS12S23S34S41=TrSp4;

SumS12S13S23=Sum[Sp2[[i1,i3]]*wab[[i1,i3]],{i1,1,NNN},{i3,1,NNN}];
SumS12S13=Sum[StS[[i3,i2]],{i3,1,NNN},{i2,1,NNN}];
SumS12S23S31=TrSp3;

SumSx1=SumSby2;
SumS1x=SumSby1;

SumSx1S12S23S34=Table[Sum[Sp4[[aa,i4]],{i4,1,NNN}],{aa,1,NNN}];
SumS1xS21S32S43=Table[Sum[Sp4[[i4,aa]],{i4,1,NNN}],{aa,1,NNN}];

SumSx1S12S23=Table[Sum[Sp3[[aa,i3]],{i3,1,NNN}],{aa,1,NNN}];
SumS1xS21S32=Table[Sum[Sp3[[i3,aa]],{i3,1,NNN}],{aa,1,NNN}];

SumSx1S12=Table[Sum[Sp2[[aa,i2]],{i2,1,NNN}],{aa,1,NNN}];
SumS1xS21=Table[Sum[Sp2[[i2,aa]],{i2,1,NNN}],{aa,1,NNN}];

SumSy1S12S23S3x=Transpose[Sp4];
SumSy1S12S2x=Transpose[Sp3];
SumSy1S1x=Transpose[Sp2];

(* ==============================================*)

mFuncY2Val=mFuncY2[wab,False];
DmFuncY2Val=DmFuncY2[wab,False];
Return[True];
];
(* ==============================================*)

yFunc[wab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal,a,b},
If[calculateInvariants,CalculateYInvariants[wab]];
retVal=Table[((-SumS12 + NNN*(SumS1x[[b]] + SumSx1[[a]] - NNN*wab[[a,b]]))/(2*NNN^2)),{a,1,NNN},{b,1,NNN}];
Return[retVal];
];

mFuncY1[wab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal,a,b},
If[calculateInvariants,CalculateYInvariants[wab]];
retVal=(SumS12/(2*NNN));
Return[retVal];
];

mFuncY2[wab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal,a,b},
If[calculateInvariants,CalculateYInvariants[wab]];
retVal=((4*SumS12^2 + 4*NNN^2*SumS12S21 - 8*NNN*SumS12S31)/(16*NNN^2));
Return[retVal];
];

mFuncY3[wab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal,a,b},
If[calculateInvariants,CalculateYInvariants[wab]];
retVal=((SumS12^3-NNN^3*SumS12S23S31-3*NNN*SumS12*SumS12S31+3*NNN^2*SumS12S31S43)/(8*NNN^3));
Return[retVal];
];

mFuncY4[wab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal,a,b},
If[calculateInvariants,CalculateYInvariants[wab]];
retVal=((SumS12^4+NNN^4*SumS12S23S34S41-4*NNN*SumS12^2*SumS12S31+4*NNN^2*SumS12*SumS12S31S43+2*NNN^2*(SumS12S31^2-2*NNN*SumS12S31S43S54))/(16*NNN^4));
Return[retVal];
];

mFuncY5[wab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal,a,b},
If[calculateInvariants,CalculateYInvariants[wab]];
retVal=((SumS12^5-5*NNN*SumS12^3*SumS12S31+5*NNN^2*SumS12^2*SumS12S31S43+5*NNN^2*SumS12*(SumS12S31^2-NNN*SumS12S31S43S54)-NNN^3*(NNN^2*SumS12S23S34S45S51+5*SumS12S31*SumS12S31S43-5*NNN*SumS12S31S43S54S65))/(32*NNN^5));
Return[retVal];
];

mFuncYLst={mFuncY1,mFuncY2,mFuncY3,mFuncY4,mFuncY5};

mFuncYk[level_?IntegerQ,wab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal},
If[level<1 || level > 5,
(
Print["mFuncYk::level is out of range. level = ", level];
Return[Indeterminate];
)
];

retVal=mFuncYLst[[level]][wab,calculateInvariants];
Return[retVal];
];

(* ==============================================*)

DmFuncY1Table[wab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},
If[calculateInvariants,CalculateYInvariants[wab]];
gradVal=Table[If[a!=b,(1/(2*NNN)),0],{a,1,NNN},{b,1,NNN}];
Return[gradVal];
];

DmFuncY1[wab_?MatrixQ,calculateInvariants_?BooleanQ]:=ToLinear[DmFuncY1Table[wab,calculateInvariants]];

DmFuncY2Table[wab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},
If[calculateInvariants,CalculateYInvariants[wab]];
gradVal=Table[If[a!=b,((SumS12 - NNN*(SumS1x[[a]] + SumSx1[[b]]) + NNN^2*wab[[b,a]])/(2*NNN^2)),0],{a,1,NNN},{b,1,NNN}];
Return[gradVal];
];

DmFuncY2[wab_?MatrixQ,calculateInvariants_?BooleanQ]:=ToLinear[DmFuncY2Table[wab,calculateInvariants]];

DmFuncY3Table[wab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},
If[calculateInvariants,CalculateYInvariants[wab]];
gradVal=Table[If[a!=b,((3*(SumS12^2 - NNN*SumS12S31 + NNN*(-(SumS12*SumSx1[[b]]) + SumS1x[[a]]*(-SumS12 + NNN*SumSx1[[b]]) + NNN*(SumS1xS21[[a]] + SumSx1S12[[b]] - NNN*SumSy1S1x[[a,b]]))))/(8*NNN^3)),0],{a,1,NNN},{b,1,NNN}];
Return[gradVal];
];

DmFuncY3[wab_?MatrixQ,calculateInvariants_?BooleanQ]:=ToLinear[DmFuncY3Table[wab,calculateInvariants]];

DmFuncY4Table[wab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},
If[calculateInvariants,CalculateYInvariants[wab]];
gradVal=Table[If[a!=b,((SumS12^3/NNN^4 - (2*SumS12*SumS12S31)/NNN^3 + SumS12S31S43/NNN^2 - (SumS12^2*(SumS1x[[a]] + SumSx1[[b]]))/NNN^3 + (SumS12S31*(SumS1x[[a]] + SumSx1[[b]]))/NNN^2 + (SumS12*(SumS1xS21[[a]] + SumS1x[[a]]*SumSx1[[b]] + SumSx1S12[[b]]))/NNN^2 - (SumS1xS21S32[[a]] + SumS1xS21[[a]]*SumSx1[[b]] + SumS1x[[a]]*SumSx1S12[[b]] + SumSx1S12S23[[b]])/NNN + SumSy1S12S2x[[a,b]])/4),0],{a,1,NNN},{b,1,NNN}];
Return[gradVal];
];

DmFuncY4[wab_?MatrixQ,calculateInvariants_?BooleanQ]:=ToLinear[DmFuncY4Table[wab,calculateInvariants]];

DmFuncY5Table[wab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},
If[calculateInvariants,CalculateYInvariants[wab]];
gradVal=Table[If[a!=b,((5*(SumS12^4/NNN^5 - (3*SumS12^2*SumS12S31)/NNN^4 + SumS12S31^2/NNN^3 + (2*SumS12*SumS12S31S43)/NNN^3 - SumS12S31S43S54/NNN^2 - (SumS12^3*(SumS1x[[a]] + SumSx1[[b]]))/NNN^4 + (2*SumS12*SumS12S31*(SumS1x[[a]] + SumSx1[[b]]))/NNN^3 - (SumS12S31S43*(SumS1x[[a]] + SumSx1[[b]]))/NNN^2 + (SumS12^2*(SumS1xS21[[a]] + SumS1x[[a]]*SumSx1[[b]] + SumSx1S12[[b]]))/NNN^3 - (SumS12S31*(SumS1xS21[[a]] + SumS1x[[a]]*SumSx1[[b]] + SumSx1S12[[b]]))/NNN^2 - (SumS12*(SumS1xS21S32[[a]] + SumS1xS21[[a]]*SumSx1[[b]] + SumS1x[[a]]*SumSx1S12[[b]] + SumSx1S12S23[[b]]))/NNN^2 + (SumS1xS21S32S43[[a]] + SumS1xS21S32[[a]]*SumSx1[[b]] + SumS1xS21[[a]]*SumSx1S12[[b]] + SumS1x[[a]]*SumSx1S12S23[[b]] + SumSx1S12S23S34[[b]])/NNN - SumSy1S12S23S3x[[a,b]]))/32),0],{a,1,NNN},{b,1,NNN}];
Return[gradVal];
];

DmFuncY5[wab_?MatrixQ,calculateInvariants_?BooleanQ]:=ToLinear[DmFuncY5Table[wab,calculateInvariants]];

DmFuncYLst={DmFuncY1,DmFuncY2,DmFuncY3,DmFuncY4,DmFuncY5};
DmFuncYTableLst={DmFuncY1Table,DmFuncY2Table,DmFuncY3Table,DmFuncY4Table,DmFuncY5Table};

DmFuncYkTable[level_?IntegerQ,wab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal},
If[level<1 || level > 5,
(
Print["DmFuncYkTable::level is out of range. level = ", level];
Return[Indeterminate];
)
];

retVal=DmFuncYTableLst[[level]][wab,calculateInvariants];
Return[retVal];
];

DmFuncYk[level_?IntegerQ,wab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal},
If[level<1 || level > 5,
(
Print["DmFuncYk::level is out of range. level = ", level];
Return[Indeterminate];
)
];

retVal=DmFuncYLst[[level]][wab,calculateInvariants];
Return[retVal];
];

(* ==============================================*)

UU1[wab_?MatrixQ,gamma_,calculateInvariants_?BooleanQ]:=Module[{retVal},
retVal=(mFuncY1[wab,calculateInvariants]^2-gamma^2*mFuncY2Val)^2;
Return[retVal];
];

UU2L[wab_?MatrixQ,gamma_,calculateInvariants_?BooleanQ]:=Module[{retVal},
retVal=(mFuncY2[wab,calculateInvariants]-gamma)^2;
Return[retVal];
];

UU2[wab_?MatrixQ,gamma_,calculateInvariants_?BooleanQ]:=Module[{retVal},
retVal=(mFuncY2[wab,calculateInvariants]^2-gamma^2)^2;
Return[retVal];
];

UU3[wab_?MatrixQ,gamma_,calculateInvariants_?BooleanQ]:=Module[{retVal},
retVal=(mFuncY3[wab,calculateInvariants]^2-gamma^2*mFuncY2Val^3)^2;
Return[retVal];
];

UU4[wab_?MatrixQ,gamma_,calculateInvariants_?BooleanQ]:=Module[{retVal},
retVal=(mFuncY4[wab,calculateInvariants]^2-gamma^2*mFuncY2Val^4)^2;
Return[retVal];
];

UU5[wab_?MatrixQ,gamma_,calculateInvariants_?BooleanQ]:=Module[{retVal},
retVal=(mFuncY5[wab,calculateInvariants]^2-gamma^2*mFuncY2Val^5)^2;
Return[retVal];
];

UUFuncLst={UU1,UU2,UU3,UU4,UU5};
UULFuncLst={UU1,UU2L,UU3,UU4,UU5};

UUk[level_?IntegerQ,wab_?MatrixQ,gamma_,useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{retVal,dummy},
If[level<1 || level > 5,
(
Print["UUk::level is out of range. level = ", level];
Return[Indeterminate];
)
];

If[useUUL2,
(
retVal=UULFuncLst[[level]][wab,gamma,calculateInvariants];
),
(
retVal=UUFuncLst[[level]][wab,gamma,calculateInvariants];
)
];

If[useFullySymmetricPoly,
(
If[level==2,
(
dummy=0;
(*
If[useUUL2,
(
retVal*=mFuncY2[wab,False]^8;
),
(
retVal*=mFuncY2[wab,False]^6;
)
];
*)
),
(
retVal*=mFuncY2[wab,False]^(10-2*level);
)
];
)
];

Return[retVal];
];

UUAllLinear[wabLinear_?VectorQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_},useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ]:=Module[{retVal},
retVal=UUAll[ToMatrix[wabLinear],gammaLst,alphaLst,useUUL2,useFullySymmetricPoly];
Return[retVal];
];

UUAll[wab_?MatrixQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_},useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ]:=Module[{retVal,level},
CalculateYInvariants[wab];
retVal=Sum[alphaLst[[level]]*UUk[level,wab,gammaLst[[level]],useUUL2,useFullySymmetricPoly,False],{level,1,5}];
Return[retVal];
];

(* ==============================================*)

DUUkTable[level_?IntegerQ,wab_?MatrixQ,gamma_,useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{retVal,mFuncYkVal,mFuncY2Val,DUUkVal,DmFuncY2Val,DmFuncYkVal,UUkVal,DUU2Val,UU2Val},
If[level<1 || level > 5,
(
Print["DUUkTable::level is out of range. level = ", level];
Return[Indeterminate];
)
];

If[calculateInvariants,CalculateYInvariants[wab]];

mFuncY2Val=mFuncYk[2,wab,False];
DmFuncY2Val=DmFuncYkTable[2,wab,False];

If[level==2,
(
(* UU2Val=UUk[level,wab,gamma,useUUL2,False,False]; *)

If[useUUL2,
(
DUU2Val=2*(mFuncY2Val-gamma)*DmFuncY2Val;
(* retVal=If[useFullySymmetricPoly,(DUU2Val*mFuncY2Val^8+8*UU2Val*mFuncY2Val^7*DmFuncY2Val),(DUU2Val)]; *)
),
(
DUU2Val=2*(mFuncY2Val^2-gamma^2)*(2*mFuncY2Val*DmFuncY2Val);
(* retVal=If[useFullySymmetricPoly,(DUU2Val*mFuncY2Val^6+6*UU2Val*mFuncY2Val^5*DmFuncY2Val),(DUU2Val)]; *)
)
];

retVal=DUU2Val;
),
(
mFuncYkVal=mFuncYk[level,wab,False];
DmFuncYkVal=DmFuncYkTable[level,wab,False];

DUUkVal=2*(mFuncYkVal^2-gamma^2*mFuncY2Val^level)*(2*mFuncYkVal*DmFuncYkVal-level*gamma^2*mFuncY2Val^(level-1)*DmFuncY2Val);

If[useFullySymmetricPoly,
(
UUkVal=UUk[level,wab,gamma,useUUL2,False,False];
retVal=(DUUkVal*mFuncY2Val^(10-2*level)+(10-2*level)*UUkVal*mFuncY2Val^(10-2*level-1)*DmFuncY2Val);
),
(
retVal=DUUkVal;
)
];
)
];

Return[retVal];
];

DUUk[level_?IntegerQ,wab_?MatrixQ,gamma_,useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ,calculateInvariants_?BooleanQ]:=ToLinear[DUUkTable[level,wab,gamma,useUUL2,useFullySymmetricPoly,calculateInvariants]];

(* ==============================================*)
DUUAllLinear[wabLinear_?VectorQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_},useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ]:=Module[{retVal},
retVal=DUUAll[ToMatrix[wabLinear],gammaLst,alphaLst,useUUL2,useFullySymmetricPoly];
Return[retVal];
];

DUUAll[wab_?MatrixQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_},useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ]:=Module[{retVal,level},
CalculateYInvariants[wab];
retVal=Sum[alphaLst[[level]]*DUUk[level,wab,gammaLst[[level]],useUUL2,useFullySymmetricPoly,False],{level,1,5}];
Return[retVal];
];

DUUAllLinearNumeric[wabLinear_?VectorQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_},useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ]:=Module[{retVal},
retVal=DUUAll[ToMatrix[wabLinear],gammaLst,alphaLst,useUUL2,useFullySymmetricPoly];
Return[retVal];
]/; VectorQ[wabLinear,NumericQ];

(* ==============================================*)

DDmFuncY1[wab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{hessianVal,a,b,c,d},
If[calculateInvariants,CalculateYInvariants[wab]];
hessianVal=Table[If[((a!=b) && (c != d)),0000,0],{a,1,NNN},{b,1,NNN},{c,1,NNN},{d,1,NNN}];
Return[hessianVal];
];

DDmFuncY2[wab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{hessianVal,a,b,c,d},
If[calculateInvariants,CalculateYInvariants[wab]];
hessianVal=Table[If[((a!=b) && (c != d)),(((-1+NNN*KroneckerDelta[a,d])*(-1+NNN*KroneckerDelta[b,c]))/(2*NNN^2)),0],{a,1,NNN},{b,1,NNN},{c,1,NNN},{d,1,NNN}];
Return[hessianVal];
];

DDmFuncY3[wab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{hessianVal,a,b,c,d},
If[calculateInvariants,CalculateYInvariants[wab]];
hessianVal=Table[If[((a!=b) && (c != d)),((3*(2*SumS12 - NNN*(SumS1x[[a]] + SumS1x[[c]] + SumSx1[[b]] + SumSx1[[d]] + KroneckerDelta[a, d]*(SumS12 - NNN*(SumS1x[[c]] + SumSx1[[b]]) + NNN^2*wab[[b,c]]) - NNN*(wab[[b,c]] + wab[[d,a]]) + KroneckerDelta[b, c]*(SumS12 - NNN*(SumS1x[[a]] + SumSx1[[d]]) + NNN^2*wab[[d,a]]))))/(8*NNN^3)),0],{a,1,NNN},{b,1,NNN},{c,1,NNN},{d,1,NNN}];
Return[hessianVal];
];

DDmFuncY4[wab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{hessianVal,a,b,c,d},
If[calculateInvariants,CalculateYInvariants[wab]];
hessianVal=Table[If[((a!=b) && (c != d)),((3*SumS12^2 - 2*NNN*SumS12S31 + NNN*KroneckerDelta[b, c]*(-SumS12^2 + NNN*SumS12S31 + NNN*(SumS12*SumSx1[[d]] + SumS1x[[a]]*(SumS12 - NNN*SumSx1[[d]]) - NNN*(SumS1xS21[[a]] + SumSx1S12[[d]]) + NNN^2*SumSy1S1x[[a,d]])) + NNN*KroneckerDelta[a, d]*(-SumS12^2 + NNN*SumS12S31 + NNN*(SumS12*SumSx1[[b]] + SumS1x[[c]]*(SumS12 - NNN*SumSx1[[b]]) - NNN*(SumS1xS21[[c]] + SumSx1S12[[b]]) + NNN^2*SumSy1S1x[[c,b]])) + NNN*(-2*SumS12*(SumSx1[[b]] + SumSx1[[d]]) + SumS1x[[a]]*(-2*SumS12 + NNN*(SumS1x[[c]] + SumSx1[[b]] + SumSx1[[d]] - NNN*wab[[b,c]])) + NNN*(SumS1xS21[[a]] + SumS1xS21[[c]] + SumSx1S12[[b]] + SumSx1S12[[d]] - NNN*(SumSy1S1x[[a,d]] + SumSy1S1x[[c,b]]) + SumS12*(wab[[b,c]] + wab[[d,a]]) + (SumSx1[[b]] - NNN*wab[[b,c]])*(SumSx1[[d]] - NNN*wab[[d,a]])) + SumS1x[[c]]*(-2*SumS12 + NNN*(SumSx1[[b]] + SumSx1[[d]] - NNN*wab[[d,a]]))))/(4*NNN^4)),0],{a,1,NNN},{b,1,NNN},{c,1,NNN},{d,1,NNN}];
Return[hessianVal];
];

DDmFuncY5[wab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{hessianVal,a,b,c,d},
If[calculateInvariants,CalculateYInvariants[wab]];
hessianVal=Table[If[((a!=b) && (c != d)),((-5*(-4*SumS12^3 + 6*NNN*SumS12*SumS12S31 - 2*NNN^2*SumS12S31S43 + NNN*KroneckerDelta[b, c]*(SumS12^3 - 2*NNN*SumS12*SumS12S31 + NNN^2*SumS12S31S43 + NNN*(-(SumS12^2*SumSx1[[d]]) + NNN*SumS1xS21[[a]]*(SumS12 - NNN*SumSx1[[d]]) + SumS1x[[a]]*(-SumS12^2 + NNN*SumS12S31 + NNN*SumS12*SumSx1[[d]] - NNN^2*SumSx1S12[[d]]) + NNN*(SumS12S31*SumSx1[[d]] + SumS12*SumSx1S12[[d]] - NNN*(SumS1xS21S32[[a]] + SumSx1S12S23[[d]]) + NNN^2*SumSy1S12S2x[[a,d]]))) + NNN*KroneckerDelta[a, d]*(SumS12^3 - 2*NNN*SumS12*SumS12S31 + NNN^2*SumS12S31S43 + NNN*(-(SumS12^2*SumSx1[[b]]) + NNN*SumS1xS21[[c]]*(SumS12 - NNN*SumSx1[[b]]) + SumS1x[[c]]*(-SumS12^2 + NNN*SumS12S31 + NNN*SumS12*SumSx1[[b]] - NNN^2*SumSx1S12[[b]]) + NNN*(SumS12S31*SumSx1[[b]] + SumS12*SumSx1S12[[b]] - NNN*(SumS1xS21S32[[c]] + SumSx1S12S23[[b]]) + NNN^2*SumSy1S12S2x[[c,b]]))) + NNN*(-2*NNN*SumS12*SumS1xS21[[a]] - 2*NNN*SumS12*SumS1xS21[[c]] + NNN^2*SumS1xS21S32[[a]] + NNN^2*SumS1xS21S32[[c]] + 3*SumS12^2*SumSx1[[b]] - 2*NNN*SumS12S31*SumSx1[[b]] + NNN^2*SumS1xS21[[a]]*SumSx1[[b]] + NNN^2*SumS1xS21[[c]]*SumSx1[[b]] + 3*SumS12^2*SumSx1[[d]] - 2*NNN*SumS12S31*SumSx1[[d]] + NNN^2*SumS1xS21[[a]]*SumSx1[[d]] + NNN^2*SumS1xS21[[c]]*SumSx1[[d]] - 2*NNN*SumS12*SumSx1[[b]]*SumSx1[[d]] - 2*NNN*SumS12*SumSx1S12[[b]] + NNN^2*SumSx1[[d]]*SumSx1S12[[b]] - 2*NNN*SumS12*SumSx1S12[[d]] + NNN^2*SumSx1[[b]]*SumSx1S12[[d]] + NNN^2*SumSx1S12S23[[b]] + NNN^2*SumSx1S12S23[[d]] - NNN^3*SumSy1S12S2x[[a,d]] - NNN^3*SumSy1S12S2x[[c,b]] + NNN^2*SumS12*SumSy1S1x[[a,d]] - NNN^3*SumSx1[[b]]*SumSy1S1x[[a,d]] + NNN^2*SumS12*SumSy1S1x[[c,b]] - NNN^3*SumSx1[[d]]*SumSy1S1x[[c,b]] - NNN*SumS12^2*wab[[b,c]] + NNN^2*SumS12S31*wab[[b,c]] - NNN^3*SumS1xS21[[a]]*wab[[b,c]] + NNN^2*SumS12*SumSx1[[d]]*wab[[b,c]] - NNN^3*SumSx1S12[[d]]*wab[[b,c]] + NNN^4*SumSy1S1x[[a,d]]*wab[[b,c]] + SumS1x[[a]]*(3*SumS12^2 - 2*NNN*SumS12S31 + NNN*(-2*SumS12*(SumSx1[[b]] + SumSx1[[d]]) + SumS1x[[c]]*(-2*SumS12 + NNN*(SumSx1[[b]] + SumSx1[[d]])) + NNN*(SumS1xS21[[c]] + SumSx1[[b]]*SumSx1[[d]] + SumSx1S12[[b]] + SumSx1S12[[d]] - NNN*SumSy1S1x[[c,b]] + (SumS12 - NNN*SumSx1[[d]])*wab[[b,c]]))) + NNN*(-SumS12^2 + NNN*SumS12S31 + NNN*(SumS12*SumSx1[[b]] - NNN*(SumS1xS21[[c]] + SumSx1S12[[b]]) + NNN^2*SumSy1S1x[[c,b]]))*wab[[d,a]] + SumS1x[[c]]*(3*SumS12^2 - 2*NNN*SumS12S31 + NNN*(-2*SumS12*(SumSx1[[b]] + SumSx1[[d]]) + NNN*(SumS1xS21[[a]] + SumSx1[[b]]*SumSx1[[d]] + SumSx1S12[[b]] + SumSx1S12[[d]] - NNN*SumSy1S1x[[a,d]] + (SumS12 - NNN*SumSx1[[b]])*wab[[d,a]]))))))/(32*NNN^5)),0],{a,1,NNN},{b,1,NNN},{c,1,NNN},{d,1,NNN}];
Return[hessianVal];
];

(* ==============================================*)

DDmFuncYLst={DDmFuncY1,DDmFuncY2,DDmFuncY3,DDmFuncY4,DDmFuncY5};

DDmFuncYk[level_?IntegerQ,wab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal},
If[level<1 || level > 5,
(
Print["DDmFuncYk::level is out of range. level = ", level];
Return[Indeterminate];
)
];

retVal=DDmFuncYLst[[level]][wab,calculateInvariants];
Return[retVal];
];

(* ==============================================*)

DabDcdUUk[level_?IntegerQ,wab_?MatrixQ,gamma_,useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{retVal,mFuncYkVal,mFuncY2Val,DmFuncY2Val,DmFuncYkVal,UUkVal,DUU2Val,UU2Val,Ck,DabCk,DabDcdCk,a,b,c,d,DabDcdmFuncYkVal,DabDcdmFuncY2Val,Ak,DabAk,DabDcdAk,dummy},
(* Print["DabDcdUUk::Starting."]; *)

If[level<1 || level > 5,
(
Print["DabDcdUUk::level is out of range. level = ", level];
Return[Indeterminate];
)
];

If[calculateInvariants,CalculateYInvariants[wab]];

mFuncY2Val=mFuncYk[2,wab,False];
DmFuncY2Val=DmFuncYkTable[2,wab,False];
DabDcdmFuncY2Val=DDmFuncYk[2,wab,False];

If[level==2,
(
If[useUUL2,
(
Ck=(mFuncY2Val-gamma);
DabCk=DmFuncY2Val;
DabDcdCk=DabDcdmFuncY2Val;
),
(
Ck=(mFuncY2Val^2-gamma^2);
DabCk=(2*mFuncY2Val*DmFuncY2Val);
DabDcdCk=2*(mFuncY2Val*DabDcdmFuncY2Val+Table[DmFuncY2Val[[a,b]]*DmFuncY2Val[[c,d]],{a,1,NNN},{b,1,NNN},{c,1,NNN},{d,1,NNN}]);
)
];

(*
Print["DabDcdUUk::Ck = ", Ck];
Print["DabDcdUUk::DabCk = ", DabCk];
Print["DabDcdUUk::DabDcdCk = ", DabDcdCk];
*)
),
(
mFuncYkVal=mFuncYk[level,wab,False];
DmFuncYkVal=DmFuncYkTable[level,wab,False];
DabDcdmFuncYkVal=DDmFuncYk[level,wab,False];

(* DabDcdCk=Table[0,{a,1,NNN},{b,1,NNN},{c,1,NNN},{d,1,NNN}]; *)

Ck=(mFuncYkVal^2-gamma^2*mFuncY2Val^level);
DabCk=(2*mFuncYkVal*DmFuncYkVal-level*gamma^2*mFuncY2Val^(level-1)*DmFuncY2Val);
DabDcdCk=Table[2(mFuncYkVal*DabDcdmFuncYkVal[[a,b,c,d]]+DmFuncYkVal[[a,b]]*DmFuncYkVal[[c,d]])-level*gamma^2*(mFuncY2Val^(level-1)*DabDcdmFuncY2Val[[a,b,c,d]]+(level-1)*mFuncY2Val^(level-2)*DmFuncY2Val[[a,b]]*DmFuncY2Val[[c,d]]),{a,1,NNN},{b,1,NNN},{c,1,NNN},{d,1,NNN}];

(*
Print["DabDcdUUk::mFuncY2Val = ", mFuncY2Val];
Print["DabDcdUUk::DmFuncY2Val = ", DmFuncY2Val];
Print["DabDcdUUk::mFuncYkVal = ", mFuncYkVal];
Print["DabDcdUUk::DmFuncYkVal = ", DmFuncYkVal];
Print["DabDcdUUk::DabDcdmFuncY2Val = ", DabDcdmFuncY2Val];
Print["DabDcdUUk::DabDcdmFuncYkVal = ", DabDcdmFuncYkVal];
Print["DabDcdUUk::Ck = ", Ck];
Print["DabDcdUUk::DabCk = ", DabCk];
Print["DabDcdUUk::DabDcdCk = ", DabDcdCk];
*)

If[useFullySymmetricPoly,
(
Ak=mFuncY2Val^(10-2*level);
DabAk=(10-2*level)*mFuncY2Val^(10-2*level-1)*DmFuncY2Val;
DabDcdAk=(10-2*level)*(10-2*level-1)*mFuncY2Val^(10-2*level-2)*Table[DmFuncY2Val[[a,b]]*DmFuncY2Val[[c,d]],{a,1,NNN},{b,1,NNN},{c,1,NNN},{d,1,NNN}]+(10-2*level)*DabDcdmFuncY2Val;
DabDcdCk=Table[DabCk[[a,b]]*DabAk[[c,d]]+DabCk[[c,d]]*DabAk[[a,b]],{a,1,NNN},{b,1,NNN},{c,1,NNN},{d,1,NNN}]+Ck*DabDcdAk+Ak*DabDcdCk;
DabCk=(DabCk*Ak+Ck*DabAk);
Ck=Ck*Ak;
),
(
dummy=0;
)
];
)
];

retVal=Table[2*(Ck*DabDcdCk[[a,b,c,d]]+DabCk[[a,b]]*DabCk[[c,d]]),{a,1,NNN},{b,1,NNN},{c,1,NNN},{d,1,NNN}];
(* Print["DabDcdUUk::retVal = ", retVal]; *)

Return[retVal];
];

(* ==============================================*)

DabDcdUUAll[wab_?MatrixQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_},useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ]:=Module[{retVal,level},
CalculateYInvariants[wab];
retVal=Sum[alphaLst[[level]]*DabDcdUUk[level,wab,gammaLst[[level]],useUUL2,useFullySymmetricPoly,False],{level,1,5}];
Return[retVal];
];

DabDcdUUAllLinear[wabLinear_?VectorQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_},useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ]:=Module[{retVal},
retVal=DabDcdUUAll[ToMatrix[wabLinear],gammaLst,alphaLst,useUUL2,useFullySymmetricPoly];
Return[retVal];
];

DabDcdUUAllLinearNumeric[wabLinear_?VectorQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_},useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ]:=Module[{retVal},
retVal=DabDcdUUAll[ToMatrix[wabLinear],gammaLst,alphaLst,useUUL2,useFullySymmetricPoly];
Return[retVal];
]/; VectorQ[wabLinear,NumericQ];

(* ==============================================*)
(* Potential of max 10th power *)
Up10[wab_?MatrixQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_}]:=Module[{retVal,idx,M1,M2,M3,M4,M5,m532,m5221,m422,m211,m2,c1,c2,c3,c4,c5,mAll},
CalculateYInvariants[wab];

M1=mFuncY1[wab,False];
M2=mFuncY2[wab,False];
M3=mFuncY3[wab,False];
M4=mFuncY4[wab,False];
M5=mFuncY5[wab,False];

c1=gammaLst[[1]];
c2=gammaLst[[2]];
c3=gammaLst[[3]];
c4=gammaLst[[4]];
c5=gammaLst[[5]];

m532=(c3*c2*M5-c5*M3*M2)^2;
m5221=(c2^2*c1*M5-c5*M2^2*M1)^2;
m422=M2*(c2^2*M4-c4*M2^2)^2;
m211=M2^3*(c1^2*M2-c2*M1^2)^2;
m2=(M2-c2)^2;

mAll={m532,m5221,m422,m211,m2};

retVal=Sum[alphaLst[[idx]]*mAll[[idx]],{idx,1,5}];
Return[retVal];
];

(* ==============================================*)

(* Dab[Up10]*)
DUp10[wab_?MatrixQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_}]:=Module[{retVal,idx,M1,M2,M3,M4,M5,m532,m5221,m422,m211,m2,c1,c2,c3,c4,c5,dmAll,dM1,dM2,dM3,dM4,dM5,x532,dx532,dm532,x5221,dx5221,dm5221,x422,dx422,dm422,x211,dx211,dm211,x2,dx2,dm2},

CalculateYInvariants[wab];

c1=gammaLst[[1]];
c2=gammaLst[[2]];
c3=gammaLst[[3]];
c4=gammaLst[[4]];
c5=gammaLst[[5]];

M1=mFuncY1[wab,False];
M2=mFuncY2[wab,False];
M3=mFuncY3[wab,False];
M4=mFuncY4[wab,False];
M5=mFuncY5[wab,False];

dM1=DmFuncY1Table[wab,False];
dM2=DmFuncY2Table[wab,False];
dM3=DmFuncY3Table[wab,False];
dM4=DmFuncY4Table[wab,False];
dM5=DmFuncY5Table[wab,False];

(*
Print["M1 = ", N[M1]];
Print["M2 = ", N[M2]];
Print["M3 = ", N[M3]];
Print["M4 = ", N[M4]];
Print["M5 = ", N[M5]];

Print["dM1 = ", N[dM1]];
Print["dM2 = ", N[dM2]];
Print["dM3 = ", N[dM3]];
Print["dM4 = ", N[dM4]];
Print["dM5 = ", N[dM5]];
*)

x532=(c3*c2*M5-c5*M3*M2);
dx532=c3*c2*dM5-c5*DabXXmYY[M3,dM3,M2,dM2];
dm532=DabXXpK[x532,dx532,2];
(*
Print["x532 = ", N[x532]];
Print["dx532 = ", N[dx532]];
Print["dm532 = ", N[dm532]];
*)

x5221=(c2^2*c1*M5-c5*M2^2*M1);
dx5221=c2^2*c1*dM5-c5*DabXXpKmYYpN[M2,dM2,2,M1,dM1,1];
dm5221=DabXXpK[x5221,dx5221,2];

(*
Print["x5221 = ", Expand[x5221]];
Print["dx5221 = ", Expand[dx5221]];
Print["dm5221 = ", Expand[dm5221]];
*)
(*
Print["x5221 = ", N[x5221]];
Print["dx5221 = ", N[dx5221]];
Print["dm5221 = ", N[dm5221]];
*)
x422=(c2^2*M4-c4*M2^2);
dx422=c2^2*dM4-c4*DabXXpK[M2,dM2,2];
dm422=DabXXpKmYYpN[x422,dx422,2,M2,dM2,1];
(*
Print["x422 = ", N[x422]];
Print["dx422 = ", N[dx422]];
Print["dm422 = ", N[dm422]];
*)

x211=(c1^2*M2-c2*M1^2);
dx211=c1^2*dM2-c2*DabXXpK[M1,dM1,2];
dm211=DabXXpKmYYpN[x211,dx211,2,M2,dM2,3];
(*
Print["x211 = ", N[x211]];
Print["dx211 = ", N[dx211]];
Print["dm211 = ", N[dm211]];
*)

x2=(M2-c2);
dx2=dM2;
dm2=DabXXpK[x2,dx2,2];
(*
Print["x2 = ", N[x2]];
Print["dx2 = ", N[dx2]];
Print["dm2 = ", N[dm2]];
*)

dmAll={dm532,dm5221,dm422,dm211,dm2};

retVal=Sum[alphaLst[[idx]]*dmAll[[idx]],{idx,1,5}];
Return[retVal];
];

(* ==============================================*)

(* DabDcd[Up10] *)
DabDcdUp10[wab_?MatrixQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_}]:=Module[{retVal,idx,M1,M2,M3,M4,M5,m532,m5221,m422,m211,m2,c1,c2,c3,c4,c5,ddmAll,dM1,dM2,dM3,dM4,dM5,x532,dx532,ddx532,ddm532,x5221,dx5221,ddx5221,ddm5221,x422,dx422,ddx422,ddm422,x211,dx211,ddx211,ddm211,x2,dx2,ddx2,ddm2,ddM1,ddM2,ddM3,ddM4,ddM5},

CalculateYInvariants[wab];

c1=gammaLst[[1]];
c2=gammaLst[[2]];
c3=gammaLst[[3]];
c4=gammaLst[[4]];
c5=gammaLst[[5]];

M1=mFuncY1[wab,False];
M2=mFuncY2[wab,False];
M3=mFuncY3[wab,False];
M4=mFuncY4[wab,False];
M5=mFuncY5[wab,False];

dM1=DmFuncY1Table[wab,False];
dM2=DmFuncY2Table[wab,False];
dM3=DmFuncY3Table[wab,False];
dM4=DmFuncY4Table[wab,False];
dM5=DmFuncY5Table[wab,False];

ddM1=DDmFuncY1[wab,False];
ddM2=DDmFuncY2[wab,False];
ddM3=DDmFuncY3[wab,False];
ddM4=DDmFuncY4[wab,False];
ddM5=DDmFuncY5[wab,False];

(*
Print["M1 = ", N[M1]];
Print["M2 = ", N[M2]];
Print["M3 = ", N[M3]];
Print["M4 = ", N[M4]];
Print["M5 = ", N[M5]];

Print["dM1 = ", N[dM1]];
Print["dM2 = ", N[dM2]];
Print["dM3 = ", N[dM3]];
Print["dM4 = ", N[dM4]];
Print["dM5 = ", N[dM5]];

Print["ddM1 = ", N[ddM1]];
Print["ddM2 = ", N[ddM2]];
Print["ddM3 = ", N[ddM3]];
Print["ddM4 = ", N[ddM4]];
Print["ddM5 = ", N[ddM5]];
*)

x532=(c3*c2*M5-c5*M3*M2);
dx532=c3*c2*dM5-c5*DabXXmYY[M3,dM3,M2,dM2];
ddx532=c3*c2*ddM5-c5*DabDcdXXmYY[M3,dM3,ddM3,M2,dM2,ddM2];
ddm532=DabDcdXXpK[x532,dx532,ddx532,2];
(*
Print["x532 = ", N[x532]];
Print["dx532 = ", N[dx532]];
Print["ddx532 = ", N[ddx532]];
Print["ddm532 = ", N[ddm532]];
*)

x5221=(c2^2*c1*M5-c5*M2^2*M1);
dx5221=c2^2*c1*dM5-c5*DabXXpKmYYpN[M2,dM2,2,M1,dM1,1];
ddx5221=c2^2*c1*ddM5-c5*DabDcdXXpKmYYpN[M2,dM2,ddM2,2,M1,dM1,ddM1,1];
ddm5221=DabDcdXXpK[x5221,dx5221,ddx5221,2];
(*
Print["x5221 = ", N[x5221]];
Print["dx5221 = ", N[dx5221]];
Print["ddx5221 = ", N[ddx5221]];
Print["ddm5221 = ", N[ddm5221]];
*)

x422=(c2^2*M4-c4*M2^2);
dx422=c2^2*dM4-c4*DabXXpK[M2,dM2,2];
ddx422=c2^2*ddM4-c4*DabDcdXXpK[M2,dM2,ddM2,2];
ddm422=DabDcdXXpKmYYpN[x422,dx422,ddx422,2,M2,dM2,ddM2,1];
(*
Print["x422 = ", N[x422]];
Print["dx422 = ", N[dx422]];
Print["ddx422 = ", N[ddx422]];
Print["ddm422 = ", N[ddm422]];
*)

x211=(c1^2*M2-c2*M1^2);
dx211=c1^2*dM2-c2*DabXXpK[M1,dM1,2];
ddx211=c1^2*ddM2-c2*DabDcdXXpK[M1,dM1,ddM1,2];
ddm211=DabDcdXXpKmYYpN[x211,dx211,ddx211,2,M2,dM2,ddM2,3];
(*
Print["x211 = ", N[x211]];
Print["dx211 = ", N[dx211]];
Print["ddx211 = ", N[ddx211]];
Print["ddm211 = ", N[ddm211]];
*)

x2=(M2-c2);
dx2=dM2;
ddx2=ddM2;
ddm2=DabDcdXXpK[x2,dx2,ddx2,2];
(*
Print["x2 = ", N[x2]];
Print["dx2 = ", N[dx2]];
Print["ddx2 = ", N[ddx2]];
Print["ddm2 = ", N[ddm2]];
*)

ddmAll={ddm532,ddm5221,ddm422,ddm211,ddm2};

retVal=Sum[alphaLst[[idx]]*ddmAll[[idx]],{idx,1,5}];
Return[retVal];
];

(* ==============================================*)




