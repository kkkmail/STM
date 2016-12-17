(* :Author:Konstantin K.Konstantinov *)
(* :Summary: Implementation Y derivatives. *)
(* :Copyright: K^3, 2013 - 2014 *)
(* :Version: Revision: 1.22.001, Date: 2014/01/30 *)
(* :Mathematica Version:7.0-9.0 *)
(* ==============================================*)

CalculateYInvariants[sab_?MatrixQ]:=Module[{aa,bb,i1},
Sp2 = sab . sab;
Sp3 = Sp2 . sab;
Sp4=Sp3 . sab;
Sp5=Sp4 . sab;
S2 = sab^2;

TrSp3=Tr[Sp3];
SumSby1=Table[Sum[sab[[aa,i1]],{i1,1,NNN}],{aa,1,NNN}];
SumS2by1=Table[Sum[S2[[aa,i1]],{i1,1,NNN}],{aa,1,NNN}];
SumSp2by1=Table[Sum[Sp2[[aa,i1]],{i1,1,NNN}],{aa,1,NNN}];
SumSp3by1=Table[Sum[Sp3[[aa,i1]],{i1,1,NNN}],{aa,1,NNN}];
SumSp4by1=Table[Sum[Sp4[[aa,i1]],{i1,1,NNN}],{aa,1,NNN}];

SumS12 = Sum[SumSby1[[i1]],{i1,1,NNN}];
SumS12p2 = Sum[SumS2by1[[i1]],{i1,1,NNN}];
SumS12S13 = Sum[SumSp2by1[[i1]],{i1,1,NNN}];
SumS12S13S23 = TrSp3;
SumS12S13S24 = Sum[SumSp3by1[[i1]],{i1,1,NNN}];
SumS12S13S24S34=Tr[Sp4];
SumSp4=Sum[Sp4[[aa,bb]],{aa,1,NNN},{bb,1,NNN}];
SumSp5=Sum[Sp5[[aa,bb]],{aa,1,NNN},{bb,1,NNN}];
SumS12S13S24S35S45=Tr[Sp5];
SumS12S13S24S35S46=SumSp5;

SumSx1 = SumSby1;
SumSx1Sy1 = Sp2;
SumSx1S12 = SumSp2by1;
SumSx1Sy2S12 = Sp3;
SumSx1S12S23 = SumSp3by1;
SumSx1Sy2S13S23=Sp4;
SumSx1S12S23S34=SumSp4by1;

mFuncY2Val=mFuncY2[sab,False];
DmFuncY2Val=DmFuncY2[sab,False];
Return[True];
];
(* ==============================================*)

yFunc[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal,a,b},
If[calculateInvariants,CalculateYInvariants[sab]];
retVal=Table[(-SumS12/(2*NNN^2) + SumSx1[[a]]/(2*NNN) + SumSx1[[b]]/(2*NNN) - sab[[a,b]]/2),{a,1,NNN},{b,1,NNN}];
Return[retVal];
];

mFuncY1[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal,a,b},
If[calculateInvariants,CalculateYInvariants[sab]];
retVal=(SumS12/(2*NNN));
Return[retVal];
];

mFuncY2[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal,a,b},
If[calculateInvariants,CalculateYInvariants[sab]];
retVal=(SumS12^2/(4*NNN^2) + SumS12p2/4 - SumS12S13/(2*NNN));
Return[retVal];
];

mFuncY3[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal,a,b},
If[calculateInvariants,CalculateYInvariants[sab]];
retVal=((SumS12^3 - 3*NNN*SumS12*SumS12S13 - NNN^3*SumS12S13S23 + 3*NNN^2*SumS12S13S24)/(8*NNN^3));
Return[retVal];
];

mFuncY4[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal,a,b},
If[calculateInvariants,CalculateYInvariants[sab]];
retVal=((SumS12^4 - 4*NNN*SumS12^2*SumS12S13 + 2*NNN^2*SumS12S13^2 + 4*NNN^2*SumS12*SumS12S13S24 + NNN^4*SumS12S13S24S34 - 4*NNN^3*SumSp4)/(16*NNN^4));
Return[retVal];
];

mFuncY5[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal,a,b},
If[calculateInvariants,CalculateYInvariants[sab]];
retVal=((SumS12^5 - 5*NNN*SumS12^3*SumS12S13 + 5*NNN^2*SumS12^2*SumS12S13S24 - NNN^3*(5*SumS12S13*SumS12S13S24 + NNN*(NNN*SumS12S13S24S35S45 - 5*SumS12S13S24S35S46)) + 5*NNN^2*SumS12*(SumS12S13^2 - NNN*SumSp4))/(32*NNN^5));
Return[retVal];
];

mFuncYLst={mFuncY1,mFuncY2,mFuncY3,mFuncY4,mFuncY5};

mFuncYk[level_?IntegerQ,sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal},
If[level<1 || level > 5,
(
Print["mFuncYk::level is out of range. level = ", level];
Return[Indeterminate];
)
];

retVal=mFuncYLst[[level]][sab,calculateInvariants];
Return[retVal];
];

(* ==============================================*)

DmFuncY1Table[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},
If[calculateInvariants,CalculateYInvariants[sab]];
gradVal=Table[If[a!=b,(NNN^(-1)),0],{a,1,NNN},{b,1,NNN}];
Return[gradVal];
];

DmFuncY1[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=ToLinear[DmFuncY1Table[sab,calculateInvariants]];

DmFuncY2Table[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},
If[calculateInvariants,CalculateYInvariants[sab]];
gradVal=Table[If[a!=b,((SumS12 - NNN*SumSx1[[a]] - NNN*SumSx1[[b]] + NNN^2*sab[[a,b]])/NNN^2),0],{a,1,NNN},{b,1,NNN}];
Return[gradVal];
];

DmFuncY2[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=ToLinear[DmFuncY2Table[sab,calculateInvariants]];

DmFuncY3Table[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},
If[calculateInvariants,CalculateYInvariants[sab]];
gradVal=Table[If[a!=b,((-3*(-SumS12^2 + NNN*SumS12S13 + NNN*SumS12*SumSx1[[b]] + NNN*SumSx1[[a]]*(SumS12 - NNN*SumSx1[[b]]) - NNN^2*SumSx1S12[[a]] - NNN^2*SumSx1S12[[b]] + NNN^3*SumSx1Sy1[[a,b]]))/(4*NNN^3)),0],{a,1,NNN},{b,1,NNN}];
Return[gradVal];
];

DmFuncY3[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=ToLinear[DmFuncY3Table[sab,calculateInvariants]];

DmFuncY4Table[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},
If[calculateInvariants,CalculateYInvariants[sab]];
gradVal=Table[If[a!=b,((SumS12^3 - 2*NNN*SumS12*SumS12S13 + NNN^2*SumS12S13S24 + NNN^2*SumS12*SumSx1S12[[a]] - NNN*SumSx1[[b]]*(SumS12^2 - NNN*SumS12S13 + NNN^2*SumSx1S12[[a]]) + NNN^2*SumS12*SumSx1S12[[b]] + NNN*SumSx1[[a]]*(-SumS12^2 + NNN*SumS12S13 + NNN*SumS12*SumSx1[[b]] - NNN^2*SumSx1S12[[b]]) - NNN^3*SumSx1S12S23[[a]] - NNN^3*SumSx1S12S23[[b]] + NNN^4*SumSx1Sy2S12[[a,b]])/(2*NNN^4)),0],{a,1,NNN},{b,1,NNN}];
Return[gradVal];
];

DmFuncY4[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=ToLinear[DmFuncY4Table[sab,calculateInvariants]];

DmFuncY5Table[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},
If[calculateInvariants,CalculateYInvariants[sab]];
gradVal=Table[If[a!=b,((-5*(-SumS12^4 + 3*NNN*SumS12^2*SumS12S13 - NNN^2*SumS12S13^2 - 2*NNN^2*SumS12*SumS12S13S24 + NNN^3*SumSp4 - NNN^2*SumS12^2*SumSx1S12[[a]] + NNN^3*SumS12S13*SumSx1S12[[a]] - NNN^2*SumS12^2*SumSx1S12[[b]] + NNN^3*SumS12S13*SumSx1S12[[b]] - NNN^4*SumSx1S12[[a]]*SumSx1S12[[b]] + NNN^3*SumS12*SumSx1S12S23[[a]] + NNN*SumSx1[[b]]*(SumS12^3 - 2*NNN*SumS12*SumS12S13 + NNN^2*SumS12S13S24 + NNN^2*SumS12*SumSx1S12[[a]] - NNN^3*SumSx1S12S23[[a]]) + NNN^3*SumS12*SumSx1S12S23[[b]] + NNN*SumSx1[[a]]*(SumS12^3 - 2*NNN*SumS12*SumS12S13 + NNN^2*SumS12S13S24 + NNN*(-SumS12^2 + NNN*SumS12S13)*SumSx1[[b]] + NNN^2*SumS12*SumSx1S12[[b]] - NNN^3*SumSx1S12S23[[b]]) - NNN^4*SumSx1S12S23S34[[a]] - NNN^4*SumSx1S12S23S34[[b]] + NNN^5*SumSx1Sy2S13S23[[a,b]]))/(16*NNN^5)),0],{a,1,NNN},{b,1,NNN}];
Return[gradVal];
];

DmFuncY5[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=ToLinear[DmFuncY5Table[sab,calculateInvariants]];

DmFuncYLst={DmFuncY1,DmFuncY2,DmFuncY3,DmFuncY4,DmFuncY5};
DmFuncYTableLst={DmFuncY1Table,DmFuncY2Table,DmFuncY3Table,DmFuncY4Table,DmFuncY5Table};

DmFuncYkTable[level_?IntegerQ,sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal},
If[level<1 || level > 5,
(
Print["DmFuncYkTable::level is out of range. level = ", level];
Return[Indeterminate];
)
];

retVal=DmFuncYTableLst[[level]][sab,calculateInvariants];
Return[retVal];
];

DmFuncYk[level_?IntegerQ,sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal},
If[level<1 || level > 5,
(
Print["DmFuncYk::level is out of range. level = ", level];
Return[Indeterminate];
)
];

retVal=DmFuncYLst[[level]][sab,calculateInvariants];
Return[retVal];
];

(* ==============================================*)

UU1[sab_?MatrixQ,gamma_,calculateInvariants_?BooleanQ]:=Module[{retVal},
retVal=(mFuncY1[sab,calculateInvariants]^2-gamma^2*mFuncY2Val)^2;
Return[retVal];
];

UU2L[sab_?MatrixQ,gamma_,calculateInvariants_?BooleanQ]:=Module[{retVal},
retVal=(mFuncY2[sab,calculateInvariants]-gamma)^2;
Return[retVal];
];

UU2[sab_?MatrixQ,gamma_,calculateInvariants_?BooleanQ]:=Module[{retVal},
retVal=(mFuncY2[sab,calculateInvariants]^2-gamma^2)^2;
Return[retVal];
];

UU3[sab_?MatrixQ,gamma_,calculateInvariants_?BooleanQ]:=Module[{retVal},
retVal=(mFuncY3[sab,calculateInvariants]^2-gamma^2*mFuncY2Val^3)^2;
Return[retVal];
];

UU4[sab_?MatrixQ,gamma_,calculateInvariants_?BooleanQ]:=Module[{retVal},
retVal=(mFuncY4[sab,calculateInvariants]^2-gamma^2*mFuncY2Val^4)^2;
Return[retVal];
];

UU5[sab_?MatrixQ,gamma_,calculateInvariants_?BooleanQ]:=Module[{retVal},
retVal=(mFuncY5[sab,calculateInvariants]^2-gamma^2*mFuncY2Val^5)^2;
Return[retVal];
];

UUFuncLst={UU1,UU2,UU3,UU4,UU5};
UULFuncLst={UU1,UU2L,UU3,UU4,UU5};

UUk[level_?IntegerQ,sab_?MatrixQ,gamma_,useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{retVal,dummy},
If[level<1 || level > 5,
(
Print["UUk::level is out of range. level = ", level];
Return[Indeterminate];
)
];

If[useUUL2,
(
retVal=UULFuncLst[[level]][sab,gamma,calculateInvariants];
),
(
retVal=UUFuncLst[[level]][sab,gamma,calculateInvariants];
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
retVal*=mFuncY2[sab,False]^8;
),
(
retVal*=mFuncY2[sab,False]^6;
)
];
*)
),
(
retVal*=mFuncY2[sab,False]^(10-2*level);
)
];
)
];

Return[retVal];
];

UUAllLinear[sabLinear_?VectorQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_},useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ]:=Module[{retVal},
retVal=UUAll[ToMatrix[sabLinear],gammaLst,alphaLst,useUUL2,useFullySymmetricPoly];
Return[retVal];
];

UUAll[sab_?MatrixQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_},useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ]:=Module[{retVal,level},
CalculateYInvariants[sab];
retVal=Sum[alphaLst[[level]]*UUk[level,sab,gammaLst[[level]],useUUL2,useFullySymmetricPoly,False],{level,1,5}];
Return[retVal];
];

(* ==============================================*)

DUUkTable[level_?IntegerQ,sab_?MatrixQ,gamma_,useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{retVal,mFuncYkVal,mFuncY2Val,DUUkVal,DmFuncY2Val,DmFuncYkVal,UUkVal,DUU2Val,UU2Val},
If[level<1 || level > 5,
(
Print["DUUkTable::level is out of range. level = ", level];
Return[Indeterminate];
)
];

If[calculateInvariants,CalculateYInvariants[sab]];

mFuncY2Val=mFuncYk[2,sab,False];
DmFuncY2Val=DmFuncYkTable[2,sab,False];

If[level==2,
(
(* UU2Val=UUk[level,sab,gamma,useUUL2,False,False]; *)

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
mFuncYkVal=mFuncYk[level,sab,False];
DmFuncYkVal=DmFuncYkTable[level,sab,False];

DUUkVal=2*(mFuncYkVal^2-gamma^2*mFuncY2Val^level)*(2*mFuncYkVal*DmFuncYkVal-level*gamma^2*mFuncY2Val^(level-1)*DmFuncY2Val);

If[useFullySymmetricPoly,
(
UUkVal=UUk[level,sab,gamma,useUUL2,False,False];
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

DUUk[level_?IntegerQ,sab_?MatrixQ,gamma_,useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ,calculateInvariants_?BooleanQ]:=ToLinear[DUUkTable[level,sab,gamma,useUUL2,useFullySymmetricPoly,calculateInvariants]];

(* ==============================================*)
DUUAllLinear[sabLinear_?VectorQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_},useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ]:=Module[{retVal},
retVal=DUUAll[ToMatrix[sabLinear],gammaLst,alphaLst,useUUL2,useFullySymmetricPoly];
Return[retVal];
];

DUUAll[sab_?MatrixQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_},useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ]:=Module[{retVal,level},
CalculateYInvariants[sab];
retVal=Sum[alphaLst[[level]]*DUUk[level,sab,gammaLst[[level]],useUUL2,useFullySymmetricPoly,False],{level,1,5}];
Return[retVal];
];

DUUAllLinearNumeric[sabLinear_?VectorQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_},useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ]:=Module[{retVal},
retVal=DUUAll[ToMatrix[sabLinear],gammaLst,alphaLst,useUUL2,useFullySymmetricPoly];
Return[retVal];
]/; VectorQ[sabLinear,NumericQ];

(* ==============================================*)

DDmFuncY1[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{hessianVal,a,b,c,d},
If[calculateInvariants,CalculateYInvariants[sab]];
hessianVal=Table[If[((a!=b) && (c != d)),0000,0],{a,1,NNN},{b,1,NNN},{c,1,NNN},{d,1,NNN}];
Return[hessianVal];
];

DDmFuncY2[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{hessianVal,a,b,c,d},
If[calculateInvariants,CalculateYInvariants[sab]];
hessianVal=Table[If[((a!=b) && (c != d)),((2 - NNN*KroneckerDelta[b, c] + NNN*KroneckerDelta[a, d]*(-1 + NNN*KroneckerDelta[b, c]) - NNN*KroneckerDelta[b, d] + NNN*KroneckerDelta[a, c]*(-1 + NNN*KroneckerDelta[b, d]))/NNN^2),0],{a,1,NNN},{b,1,NNN},{c,1,NNN},{d,1,NNN}];
Return[hessianVal];
];

DDmFuncY3[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{hessianVal,a,b,c,d},
If[calculateInvariants,CalculateYInvariants[sab]];
hessianVal=Table[If[((a!=b) && (c != d)),((-3*(-4*SumS12 + NNN*SumS12*KroneckerDelta[b, c] + NNN*SumS12*KroneckerDelta[b, d] + 2*NNN*SumSx1[[a]] - NNN^2*KroneckerDelta[b, c]*SumSx1[[a]] - NNN^2*KroneckerDelta[b, d]*SumSx1[[a]] + 2*NNN*SumSx1[[b]] + 2*NNN*SumSx1[[c]] - NNN^2*KroneckerDelta[b, d]*SumSx1[[c]] + 2*NNN*SumSx1[[d]] - NNN^2*KroneckerDelta[b, c]*SumSx1[[d]] - NNN^2*sab[[a,c]] + NNN^3*KroneckerDelta[b, d]*sab[[a,c]] - NNN^2*sab[[a,d]] + NNN^3*KroneckerDelta[b, c]*sab[[a,d]] - NNN^2*sab[[b,c]] + NNN*KroneckerDelta[a, d]*(SumS12 - NNN*SumSx1[[b]] - NNN*SumSx1[[c]] + NNN^2*sab[[b,c]]) - NNN^2*sab[[b,d]] + NNN*KroneckerDelta[a, c]*(SumS12 - NNN*SumSx1[[b]] - NNN*SumSx1[[d]] + NNN^2*sab[[b,d]])))/(4*NNN^3)),0],{a,1,NNN},{b,1,NNN},{c,1,NNN},{d,1,NNN}];
Return[hessianVal];
];

DDmFuncY4[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{hessianVal,a,b,c,d},
If[calculateInvariants,CalculateYInvariants[sab]];
hessianVal=Table[If[((a!=b) && (c != d)),(((6*SumS12^2)/NNN^4 - (4*SumS12S13)/NNN^3 - (SumS12^2*(KroneckerDelta[a, c] + KroneckerDelta[b, c]))/NNN^3 + (SumS12S13*(KroneckerDelta[a, c] + KroneckerDelta[b, c]))/NNN^2 - (SumS12^2*(KroneckerDelta[a, d] + KroneckerDelta[b, d]))/NNN^3 + (SumS12S13*(KroneckerDelta[a, d] + KroneckerDelta[b, d]))/NNN^2 - (4*SumS12*(SumSx1[[a]] + SumSx1[[b]]))/NNN^3 - (4*SumS12*SumSx1[[c]])/NNN^3 + (SumS12*(KroneckerDelta[a, d] + KroneckerDelta[b, d])*SumSx1[[c]])/NNN^2 + (2*(SumSx1[[a]] + SumSx1[[b]])*SumSx1[[c]])/NNN^2 - (4*SumS12*SumSx1[[d]])/NNN^3 + (SumS12*(KroneckerDelta[a, c] + KroneckerDelta[b, c])*SumSx1[[d]])/NNN^2 + (2*(SumSx1[[a]] + SumSx1[[b]])*SumSx1[[d]])/NNN^2 + (2*SumSx1[[c]]*SumSx1[[d]])/NNN^2 + (2*(SumSx1[[a]]*SumSx1[[b]] + SumSx1S12[[a]] + SumSx1S12[[b]]))/NNN^2 + (2*SumSx1S12[[c]])/NNN^2 - ((KroneckerDelta[a, d] + KroneckerDelta[b, d])*SumSx1S12[[c]])/NNN + (2*SumSx1S12[[d]])/NNN^2 - ((KroneckerDelta[a, c] + KroneckerDelta[b, c])*SumSx1S12[[d]])/NNN + sab[[a,d]]*sab[[b,c]] + (SumS12*(KroneckerDelta[b, c]*SumSx1[[a]] + KroneckerDelta[a, c]*SumSx1[[b]] + sab[[a,c]] + sab[[b,c]]))/NNN^2 - (SumSx1[[d]]*(KroneckerDelta[b, c]*SumSx1[[a]] + KroneckerDelta[a, c]*SumSx1[[b]] + sab[[a,c]] + sab[[b,c]]))/NNN + sab[[a,c]]*sab[[b,d]] + (SumS12*(KroneckerDelta[b, d]*SumSx1[[a]] + KroneckerDelta[a, d]*SumSx1[[b]] + sab[[a,d]] + sab[[b,d]]))/NNN^2 - (SumSx1[[c]]*(KroneckerDelta[b, d]*SumSx1[[a]] + KroneckerDelta[a, d]*SumSx1[[b]] + sab[[a,d]] + sab[[b,d]]))/NNN + KroneckerDelta[b, d]*SumSx1Sy1[[a,c]] + KroneckerDelta[b, c]*SumSx1Sy1[[a,d]] + KroneckerDelta[a, d]*SumSx1Sy1[[b,c]] - (KroneckerDelta[b, c]*SumSx1S12[[a]] + KroneckerDelta[a, c]*SumSx1S12[[b]] + SumSx1[[b]]*sab[[a,c]] + SumSx1[[a]]*sab[[b,c]] + SumSx1Sy1[[a,c]] + SumSx1Sy1[[b,c]])/NNN + KroneckerDelta[a, c]*SumSx1Sy1[[b,d]] - (KroneckerDelta[b, d]*SumSx1S12[[a]] + KroneckerDelta[a, d]*SumSx1S12[[b]] + SumSx1[[b]]*sab[[a,d]] + SumSx1[[a]]*sab[[b,d]] + SumSx1Sy1[[a,d]] + SumSx1Sy1[[b,d]])/NNN)/2),0],{a,1,NNN},{b,1,NNN},{c,1,NNN},{d,1,NNN}];
Return[hessianVal];
];

DDmFuncY5[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{hessianVal,a,b,c,d},
If[calculateInvariants,CalculateYInvariants[sab]];
hessianVal=Table[If[((a!=b) && (c != d)),((5*((8*SumS12^3)/NNN^5 - (12*SumS12*SumS12S13)/NNN^4 + (4*SumS12S13S24)/NNN^3 - (SumS12^3*(KroneckerDelta[a, c] + KroneckerDelta[b, c]))/NNN^4 + (2*SumS12*SumS12S13*(KroneckerDelta[a, c] + KroneckerDelta[b, c]))/NNN^3 - (SumS12S13S24*(KroneckerDelta[a, c] + KroneckerDelta[b, c]))/NNN^2 - (SumS12^3*(KroneckerDelta[a, d] + KroneckerDelta[b, d]))/NNN^4 + (2*SumS12*SumS12S13*(KroneckerDelta[a, d] + KroneckerDelta[b, d]))/NNN^3 - (SumS12S13S24*(KroneckerDelta[a, d] + KroneckerDelta[b, d]))/NNN^2 - (6*SumS12^2*(SumSx1[[a]] + SumSx1[[b]]))/NNN^4 + (4*SumS12S13*(SumSx1[[a]] + SumSx1[[b]]))/NNN^3 - (6*SumS12^2*SumSx1[[c]])/NNN^4 + (4*SumS12S13*SumSx1[[c]])/NNN^3 + (SumS12^2*(KroneckerDelta[a, d] + KroneckerDelta[b, d])*SumSx1[[c]])/NNN^3 - (SumS12S13*(KroneckerDelta[a, d] + KroneckerDelta[b, d])*SumSx1[[c]])/NNN^2 + (4*SumS12*(SumSx1[[a]] + SumSx1[[b]])*SumSx1[[c]])/NNN^3 - (6*SumS12^2*SumSx1[[d]])/NNN^4 + (4*SumS12S13*SumSx1[[d]])/NNN^3 + (SumS12^2*(KroneckerDelta[a, c] + KroneckerDelta[b, c])*SumSx1[[d]])/NNN^3 - (SumS12S13*(KroneckerDelta[a, c] + KroneckerDelta[b, c])*SumSx1[[d]])/NNN^2 + (4*SumS12*(SumSx1[[a]] + SumSx1[[b]])*SumSx1[[d]])/NNN^3 + (4*SumS12*SumSx1[[c]]*SumSx1[[d]])/NNN^3 - (2*(SumSx1[[a]] + SumSx1[[b]])*SumSx1[[c]]*SumSx1[[d]])/NNN^2 + (4*SumS12*(SumSx1[[a]]*SumSx1[[b]] + SumSx1S12[[a]] + SumSx1S12[[b]]))/NNN^3 - (2*SumSx1[[c]]*(SumSx1[[a]]*SumSx1[[b]] + SumSx1S12[[a]] + SumSx1S12[[b]]))/NNN^2 - (2*SumSx1[[d]]*(SumSx1[[a]]*SumSx1[[b]] + SumSx1S12[[a]] + SumSx1S12[[b]]))/NNN^2 + (4*SumS12*SumSx1S12[[c]])/NNN^3 - (SumS12*(KroneckerDelta[a, d] + KroneckerDelta[b, d])*SumSx1S12[[c]])/NNN^2 - (2*SumSx1[[d]]*SumSx1S12[[c]])/NNN^2 + (4*SumS12*SumSx1S12[[d]])/NNN^3 - (SumS12*(KroneckerDelta[a, c] + KroneckerDelta[b, c])*SumSx1S12[[d]])/NNN^2 - (2*SumSx1[[c]]*SumSx1S12[[d]])/NNN^2 - (2*(SumSx1[[b]]*SumSx1S12[[a]] + SumSx1[[a]]*SumSx1S12[[b]] + SumSx1S12S23[[a]] + SumSx1S12S23[[b]]))/NNN^2 - (2*SumSx1S12S23[[c]])/NNN^2 + ((KroneckerDelta[a, d] + KroneckerDelta[b, d])*SumSx1S12S23[[c]])/NNN - (2*SumSx1S12S23[[d]])/NNN^2 + ((KroneckerDelta[a, c] + KroneckerDelta[b, c])*SumSx1S12S23[[d]])/NNN + (SumS12^2*(KroneckerDelta[b, c]*SumSx1[[a]] + KroneckerDelta[a, c]*SumSx1[[b]] + sab[[a,c]] + sab[[b,c]]))/NNN^3 - (SumS12*SumSx1[[d]]*(KroneckerDelta[b, c]*SumSx1[[a]] + KroneckerDelta[a, c]*SumSx1[[b]] + sab[[a,c]] + sab[[b,c]]))/NNN^2 - (SumS12S13*KroneckerDelta[b, c]*SumSx1[[a]] + SumS12S13*KroneckerDelta[a, c]*SumSx1[[b]] + 2*SumSx1[[a]]*SumSx1S12[[c]] + 2*SumSx1[[b]]*SumSx1S12[[c]] + SumS12S13*sab[[a,c]] + SumS12S13*sab[[b,c]])/NNN^2 + (SumS12^2*(KroneckerDelta[b, d]*SumSx1[[a]] + KroneckerDelta[a, d]*SumSx1[[b]] + sab[[a,d]] + sab[[b,d]]))/NNN^3 - (SumS12*SumSx1[[c]]*(KroneckerDelta[b, d]*SumSx1[[a]] + KroneckerDelta[a, d]*SumSx1[[b]] + sab[[a,d]] + sab[[b,d]]))/NNN^2 - (SumS12S13*KroneckerDelta[b, d]*SumSx1[[a]] + SumS12S13*KroneckerDelta[a, d]*SumSx1[[b]] + 2*SumSx1[[a]]*SumSx1S12[[d]] + 2*SumSx1[[b]]*SumSx1S12[[d]] + SumS12S13*sab[[a,d]] + SumS12S13*sab[[b,d]])/NNN^2 + (KroneckerDelta[b, d]*SumSx1[[a]]*SumSx1S12[[c]] + KroneckerDelta[a, d]*SumSx1[[b]]*SumSx1S12[[c]] + KroneckerDelta[b, c]*SumSx1[[a]]*SumSx1S12[[d]] + KroneckerDelta[a, c]*SumSx1[[b]]*SumSx1S12[[d]] + SumSx1S12[[d]]*sab[[a,c]] + SumSx1S12[[c]]*sab[[a,d]] + SumSx1S12[[d]]*sab[[b,c]] + SumSx1S12[[c]]*sab[[b,d]])/NNN - sab[[b,d]]*SumSx1Sy1[[a,c]] - sab[[b,c]]*SumSx1Sy1[[a,d]] - sab[[a,d]]*SumSx1Sy1[[b,c]] - (SumS12*(KroneckerDelta[b, c]*SumSx1S12[[a]] + KroneckerDelta[a, c]*SumSx1S12[[b]] + SumSx1[[b]]*sab[[a,c]] + SumSx1[[a]]*sab[[b,c]] + SumSx1Sy1[[a,c]] + SumSx1Sy1[[b,c]]))/NNN^2 + (SumSx1[[d]]*(KroneckerDelta[b, c]*SumSx1S12[[a]] + KroneckerDelta[a, c]*SumSx1S12[[b]] + SumSx1[[b]]*sab[[a,c]] + SumSx1[[a]]*sab[[b,c]] + SumSx1Sy1[[a,c]] + SumSx1Sy1[[b,c]]))/NNN - sab[[a,c]]*SumSx1Sy1[[b,d]] - (SumS12*(KroneckerDelta[b, d]*SumSx1S12[[a]] + KroneckerDelta[a, d]*SumSx1S12[[b]] + SumSx1[[b]]*sab[[a,d]] + SumSx1[[a]]*sab[[b,d]] + SumSx1Sy1[[a,d]] + SumSx1Sy1[[b,d]]))/NNN^2 + (SumSx1[[c]]*(KroneckerDelta[b, d]*SumSx1S12[[a]] + KroneckerDelta[a, d]*SumSx1S12[[b]] + SumSx1[[b]]*sab[[a,d]] + SumSx1[[a]]*sab[[b,d]] + SumSx1Sy1[[a,d]] + SumSx1Sy1[[b,d]]))/NNN - KroneckerDelta[b, d]*SumSx1Sy2S12[[a,c]] - KroneckerDelta[b, c]*SumSx1Sy2S12[[a,d]] - KroneckerDelta[a, d]*SumSx1Sy2S12[[b,c]] + (KroneckerDelta[b, c]*SumSx1S12S23[[a]] + KroneckerDelta[a, c]*SumSx1S12S23[[b]] + SumSx1S12[[b]]*sab[[a,c]] + SumSx1S12[[a]]*sab[[b,c]] + SumSx1[[b]]*SumSx1Sy1[[a,c]] + SumSx1[[a]]*SumSx1Sy1[[b,c]] + SumSx1Sy2S12[[a,c]] + SumSx1Sy2S12[[b,c]])/NNN - KroneckerDelta[a, c]*SumSx1Sy2S12[[b,d]] + (KroneckerDelta[b, d]*SumSx1S12S23[[a]] + KroneckerDelta[a, d]*SumSx1S12S23[[b]] + SumSx1S12[[b]]*sab[[a,d]] + SumSx1S12[[a]]*sab[[b,d]] + SumSx1[[b]]*SumSx1Sy1[[a,d]] + SumSx1[[a]]*SumSx1Sy1[[b,d]] + SumSx1Sy2S12[[a,d]] + SumSx1Sy2S12[[b,d]])/NNN))/16),0],{a,1,NNN},{b,1,NNN},{c,1,NNN},{d,1,NNN}];
Return[hessianVal];
];

(* ==============================================*)

DDmFuncYLst={DDmFuncY1,DDmFuncY2,DDmFuncY3,DDmFuncY4,DDmFuncY5};

DDmFuncYk[level_?IntegerQ,sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal},
If[level<1 || level > 5,
(
Print["DDmFuncYk::level is out of range. level = ", level];
Return[Indeterminate];
)
];

retVal=DDmFuncYLst[[level]][sab,calculateInvariants];
Return[retVal];
];

(* ==============================================*)

(* ToLinearHessian *)
DabDcdUUk[level_?IntegerQ,sab_?MatrixQ,gamma_,useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{retVal,mFuncYkVal,mFuncY2Val,DmFuncY2Val,DmFuncYkVal,UUkVal,DUU2Val,UU2Val,Ck,DabCk,DabDcdCk,a,b,c,d,DabDcdmFuncYkVal,DabDcdmFuncY2Val,Ak,DabAk,DabDcdAk,dummy},
(* Print["DabDcdUUk::Starting."]; *)

If[level<1 || level > 5,
(
Print["DabDcdUUk::level is out of range. level = ", level];
Return[Indeterminate];
)
];

If[calculateInvariants,CalculateYInvariants[sab]];

mFuncY2Val=mFuncYk[2,sab,False];
DmFuncY2Val=DmFuncYkTable[2,sab,False];
DabDcdmFuncY2Val=DDmFuncYk[2,sab,False];

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
mFuncYkVal=mFuncYk[level,sab,False];
DmFuncYkVal=DmFuncYkTable[level,sab,False];
DabDcdmFuncYkVal=DDmFuncYk[level,sab,False];

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

DabDcdUUAll[sab_?MatrixQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_},useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ]:=Module[{retVal,level},
CalculateYInvariants[sab];
retVal=Sum[alphaLst[[level]]*DabDcdUUk[level,sab,gammaLst[[level]],useUUL2,useFullySymmetricPoly,False],{level,1,5}];
Return[retVal];
];

DabDcdUUAllLinear[sabLinear_?VectorQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_},useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ]:=Module[{retVal},
retVal=DabDcdUUAll[ToMatrix[sabLinear],gammaLst,alphaLst,useUUL2,useFullySymmetricPoly];
Return[retVal];
];

DabDcdUUAllLinearNumeric[sabLinear_?VectorQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_},useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ]:=Module[{retVal},
retVal=DabDcdUUAll[ToMatrix[sabLinear],gammaLst,alphaLst,useUUL2,useFullySymmetricPoly];
Return[retVal];
]/; VectorQ[sabLinear,NumericQ];

(* ==============================================*)
(* Potential of max 10th power *)
Up10[sab_?MatrixQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_}]:=Module[{retVal,idx,M1,M2,M3,M4,M5,m532,m5221,m422,m211,m2,c1,c2,c3,c4,c5,mAll},
CalculateYInvariants[sab];

M1=mFuncY1[sab,False];
M2=mFuncY2[sab,False];
M3=mFuncY3[sab,False];
M4=mFuncY4[sab,False];
M5=mFuncY5[sab,False];

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
DUp10[sab_?MatrixQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_}]:=Module[{retVal,idx,M1,M2,M3,M4,M5,m532,m5221,m422,m211,m2,c1,c2,c3,c4,c5,dmAll,dM1,dM2,dM3,dM4,dM5,x532,dx532,dm532,x5221,dx5221,dm5221,x422,dx422,dm422,x211,dx211,dm211,x2,dx2,dm2},

CalculateYInvariants[sab];

c1=gammaLst[[1]];
c2=gammaLst[[2]];
c3=gammaLst[[3]];
c4=gammaLst[[4]];
c5=gammaLst[[5]];

M1=mFuncY1[sab,False];
M2=mFuncY2[sab,False];
M3=mFuncY3[sab,False];
M4=mFuncY4[sab,False];
M5=mFuncY5[sab,False];

dM1=DmFuncY1Table[sab,False];
dM2=DmFuncY2Table[sab,False];
dM3=DmFuncY3Table[sab,False];
dM4=DmFuncY4Table[sab,False];
dM5=DmFuncY5Table[sab,False];

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
DabDcdUp10[sab_?MatrixQ,gammaLst:{_,_,_,_,_},alphaLst:{_,_,_,_,_}]:=Module[{retVal,idx,M1,M2,M3,M4,M5,m532,m5221,m422,m211,m2,c1,c2,c3,c4,c5,ddmAll,dM1,dM2,dM3,dM4,dM5,x532,dx532,ddx532,ddm532,x5221,dx5221,ddx5221,ddm5221,x422,dx422,ddx422,ddm422,x211,dx211,ddx211,ddm211,x2,dx2,ddx2,ddm2,ddM1,ddM2,ddM3,ddM4,ddM5},

CalculateYInvariants[sab];

c1=gammaLst[[1]];
c2=gammaLst[[2]];
c3=gammaLst[[3]];
c4=gammaLst[[4]];
c5=gammaLst[[5]];

M1=mFuncY1[sab,False];
M2=mFuncY2[sab,False];
M3=mFuncY3[sab,False];
M4=mFuncY4[sab,False];
M5=mFuncY5[sab,False];

dM1=DmFuncY1Table[sab,False];
dM2=DmFuncY2Table[sab,False];
dM3=DmFuncY3Table[sab,False];
dM4=DmFuncY4Table[sab,False];
dM5=DmFuncY5Table[sab,False];

ddM1=DDmFuncY1[sab,False];
ddM2=DDmFuncY2[sab,False];
ddM3=DDmFuncY3[sab,False];
ddM4=DDmFuncY4[sab,False];
ddM5=DDmFuncY5[sab,False];

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




