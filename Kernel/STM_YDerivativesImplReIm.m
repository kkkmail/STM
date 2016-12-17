(* ============================================== *)
(* :Summary: Implementation of Y derivatives (Re Im version). *)
(* ============================================== *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL v3 or any later version, see http://www.gnu.org/licenses/ *)
(* :Copyright: K^3, 2013 - 2015 *)
(* :Version: Revision: 2.04.001, Date: 2015/08/30 *)
(* :Mathematica Version: 10.0 *)
(* ============================================== *)
(* This program is free software: you can redistribute it and/or modify it under the terms *)
(* of the GNU General Public License as published by the Free Software Foundation, *)
(* either version 3 of the License, or any later version. This program is distributed in the hope that  *)
(* it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *) 
(* or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. *)
(* You should have received a copy of the GNU General Public License along with this program. *) 
(* If not, see <http://www.gnu.org/licenses/>. *)
(* ============================================== *)
(* ============================================== *)
CalculateYInvariants[sab_?MatrixQ,aab_?MatrixQ]:=Module[{},
If[MaxMoment<6,
(
CalculateYInvariants5[sab,aab];
),
(
CalculateYInvariants5[sab,aab];
CalculateYInvariants6[sab,aab];
)
];

Return[True];
];

(* ============================================== *)

mFuncYk[level_?IntegerQ,sab_?MatrixQ, aab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal},
If[level<1||level>MaxMoment,
(
Print["mFuncYk::level is out of range. level = ",level];
Return[Indeterminate];
)
];

retVal=ToExpression["mFuncY" <>ToString[level]][sab,aab,calculateInvariants];
Return[retVal];
];

(* ==============================================*)

DmFuncYk[level_?IntegerQ,sab_?MatrixQ, aab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal},
If[level<1||level>MaxMoment,
(
Print["DmFuncYk::level is out of range. level = ",level];
Return[Indeterminate];
)
];

If[calculateInvariants,CalculateYInvariants[sab, aab]];

retVal=ToLinear[ToExpression["DmFuncY" <>ToString[level] <> "TableS"][sab,aab,False],ToExpression["DmFuncY" <>ToString[level] <> "TableA"][sab,aab,False]];
Return[retVal];
];

(* ==============================================*)

DDmFuncYk[level_?IntegerQ,sab_?MatrixQ, aab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{retVal,dSS,dSA,dAS,dAA},
If[level<1||level>MaxMoment,
(
Print["DDmFuncYk::level is out of range. level = ",level];
Return[Indeterminate];
)
];

If[calculateInvariants,CalculateYInvariants[sab, aab]];

dSS=ToExpression["DDmFuncY" <> ToString[level] <>"SS"][sab,aab,False];
dSA=ToExpression["DDmFuncY" <> ToString[level] <>"SA"][sab,aab,False];
dAS=ToExpression["DDmFuncY" <> ToString[level] <>"AS"][sab,aab,False];
dAA=ToExpression["DDmFuncY" <> ToString[level] <>"AA"][sab,aab,False];

retVal=ToLinearHessian[dSS,dSA,dAS,dAA];
Return[retVal];
];

(* ==============================================*)

UU1[sab_?MatrixQ, aab_?MatrixQ,gamma_,calculateInvariants_?BooleanQ]:=Module[{retVal},retVal=(mFuncY1[sab,aab,calculateInvariants]^2-gamma^2*mFuncY2Val)^2;
Return[retVal];];

UU2L[sab_?MatrixQ, aab_?MatrixQ,gamma_,calculateInvariants_?BooleanQ]:=Module[{retVal},retVal=(mFuncY2[sab,aab,calculateInvariants]-gamma)^2;
Return[retVal];];

UU2[sab_?MatrixQ, aab_?MatrixQ,gamma_,calculateInvariants_?BooleanQ]:=Module[{retVal},retVal=(mFuncY2[sab,aab,calculateInvariants]^2-gamma^2)^2;
Return[retVal];];

UU3[sab_?MatrixQ, aab_?MatrixQ,gamma_,calculateInvariants_?BooleanQ]:=Module[{retVal},retVal=(mFuncY3[sab,aab,calculateInvariants]^2-gamma^2*mFuncY2Val^3)^2;
Return[retVal];];

UU4[sab_?MatrixQ, aab_?MatrixQ,gamma_,calculateInvariants_?BooleanQ]:=Module[{retVal},retVal=(mFuncY4[sab,aab,calculateInvariants]^2-gamma^2*mFuncY2Val^4)^2;
Return[retVal];];

UU5[sab_?MatrixQ, aab_?MatrixQ,gamma_,calculateInvariants_?BooleanQ]:=Module[{retVal},retVal=(mFuncY5[sab,aab,calculateInvariants]^2-gamma^2*mFuncY2Val^5)^2;
Return[retVal];];

UU6[sab_?MatrixQ, aab_?MatrixQ,gamma_,calculateInvariants_?BooleanQ]:=Module[{retVal},retVal=(mFuncY6[sab,aab,calculateInvariants]^2-gamma^2*mFuncY2Val^6)^2;
Return[retVal];];

UUFuncLst={UU1,UU2,UU3,UU4,UU5,UU6};
UULFuncLst={UU1,UU2L,UU3,UU4,UU5,UU6};

(* ==============================================*)

UUk[level_?IntegerQ,sab_?MatrixQ, aab_?MatrixQ,gamma_,useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{retVal,dummy},
If[level<1||level>MaxMoment,
(
Print["UUk::level is out of range. level = ",level];
Return[Indeterminate];
)
];

If[useUUL2,
(
retVal=UULFuncLst[[level]][sab,aab,gamma,calculateInvariants];
),
(
retVal=UUFuncLst[[level]][sab,aab,gamma,calculateInvariants];
)
];

If[useFullySymmetricPoly,
(
If[level==2,
(
dummy=0;
(*If[useUUL2,(retVal*=mFuncY2[wab,False]^8;),(retVal*=mFuncY2[wab,False]^6;)];*)
),
(
retVal*=mFuncY2[sab,aab,False]^(2*MaxMoment-2*level);
)
];
)
];

Return[retVal];
];

(* ==============================================*)

UUAll[sab_?MatrixQ, aab_?MatrixQ,gammaLst_?VectorQ,alphaLst_?VectorQ,useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ]:=Module[{retVal,level},
CalculateYInvariants[sab, aab];
retVal=Sum[alphaLst[[level]]*UUk[level,sab,aab,gammaLst[[level]],useUUL2,useFullySymmetricPoly,False],{level,1,MaxMoment}];
Return[retVal];
];

(* ==============================================*)

DUUk[level_?IntegerQ,sab_?MatrixQ, aab_?MatrixQ,gamma_,useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{retVal,mFuncYkVal,mFuncY2Val,DUUkVal,DmFuncY2Val,DmFuncYkVal,UUkVal,DUU2Val,UU2Val},
If[level<1||level>MaxMoment,
(
Print["DUUkTable::level is out of range. level = ",level];
Return[Indeterminate];
)
];

If[calculateInvariants,CalculateYInvariants[sab, aab]];
mFuncY2Val=mFuncYk[2,sab, aab,False];
DmFuncY2Val=DmFuncYk[2,sab, aab,False];
If[level==2,
(
(*UU2Val=UUk[level,sab, aab,gamma,useUUL2,False,False];*)
If[useUUL2,
(
DUU2Val=2*(mFuncY2Val-gamma)*DmFuncY2Val;
(*retVal=If[useFullySymmetricPoly,(DUU2Val*mFuncY2Val^8+8*UU2Val*mFuncY2Val^7*DmFuncY2Val),(DUU2Val)];*)
),
(
DUU2Val=2*(mFuncY2Val^2-gamma^2)*(2*mFuncY2Val*DmFuncY2Val);
(*retVal=If[useFullySymmetricPoly,(DUU2Val*mFuncY2Val^6+6*UU2Val*mFuncY2Val^5*DmFuncY2Val),(DUU2Val)];*)
)
];

retVal=DUU2Val;
),
(
mFuncYkVal=mFuncYk[level,sab, aab,False];
DmFuncYkVal=DmFuncYk[level,sab, aab,False];
DUUkVal=2*(mFuncYkVal^2-gamma^2*mFuncY2Val^level)*(2*mFuncYkVal*DmFuncYkVal-level*gamma^2*mFuncY2Val^(level-1)*DmFuncY2Val);
If[useFullySymmetricPoly,
(
UUkVal=UUk[level,sab, aab,gamma,useUUL2,False,False];
retVal=(DUUkVal*mFuncY2Val^(2*MaxMoment-2*level)+(2*MaxMoment-2*level)*UUkVal*mFuncY2Val^(2*MaxMoment-2*level-1)*DmFuncY2Val);
),
(
retVal=DUUkVal;
)
];
)
];

Return[retVal];
];

(* ==============================================*)

DUUAll[sab_?MatrixQ, aab_?MatrixQ,gammaLst_?VectorQ,alphaLst_?VectorQ,useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ]:=Module[{retVal,level},
CalculateYInvariants[sab, aab];
retVal=Sum[alphaLst[[level]]*DUUk[level,sab, aab,gammaLst[[level]],useUUL2,useFullySymmetricPoly,False],{level,1,MaxMoment}];
Return[retVal];
];

(* ==============================================*)

DabDcdUUk[level_?IntegerQ,sab_?MatrixQ, aab_?MatrixQ,gamma_,useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{retVal,mFuncYkVal,mFuncY2Val,DmFuncY2Val,DmFuncYkVal,UUkVal,DUU2Val,UU2Val,Ck,DabCk,DabDcdCk,a,c,DabDcdmFuncYkVal,DabDcdmFuncY2Val,Ak,DabAk,DabDcdAk,dummy},
(*Print["DabDcdUUk::Starting."];*)
If[level<1||level>MaxMoment,
(
Print["DabDcdUUk::level is out of range. level = ",level];
Return[Indeterminate];
)
];

If[calculateInvariants,CalculateYInvariants[sab, aab]];

mFuncY2Val=mFuncYk[2,sab, aab,False];
DmFuncY2Val=DmFuncYk[2,sab, aab,False];
DabDcdmFuncY2Val=DDmFuncYk[2,sab, aab,False];
If[level==2,
(
If[useUUL2,
(
Ck=(mFuncY2Val-gamma);
DabCk=DmFuncY2Val;
DabDcdCk=DabDcdmFuncY2Val;),(Ck=(mFuncY2Val^2-gamma^2);
DabCk=(2*mFuncY2Val*DmFuncY2Val);
DabDcdCk=2*(mFuncY2Val*DabDcdmFuncY2Val+Table[DmFuncY2Val[[a]]*DmFuncY2Val[[c]],{a,1,NAL},{c,1,NAL}]);
)
];
(*
Print["DabDcdUUk::Ck = ",Ck];
Print["DabDcdUUk::DabCk = ",DabCk];
Print["DabDcdUUk::DabDcdCk = ",DabDcdCk];
*)
),
(
mFuncYkVal=mFuncYk[level,sab, aab,False];
DmFuncYkVal=DmFuncYk[level,sab, aab,False];
DabDcdmFuncYkVal=DDmFuncYk[level,sab, aab,False];
(*DabDcdCk=Table[0,{a,1,NNN},{b,1,NNN},{c,1,NNN},{d,1,NNN}];*)
Ck=(mFuncYkVal^2-gamma^2*mFuncY2Val^level);
DabCk=(2*mFuncYkVal*DmFuncYkVal-level*gamma^2*mFuncY2Val^(level-1)*DmFuncY2Val);
DabDcdCk=Table[2(mFuncYkVal*DabDcdmFuncYkVal[[a,c]]+DmFuncYkVal[[a]]*DmFuncYkVal[[c]])-level*gamma^2*(mFuncY2Val^(level-1)*DabDcdmFuncY2Val[[a,c]]+(level-1)*mFuncY2Val^(level-2)*DmFuncY2Val[[a]]*DmFuncY2Val[[c]]),{a,1,NAL},{c,1,NAL}];

(*
Print["DabDcdUUk::mFuncY2Val = ",mFuncY2Val];
Print["DabDcdUUk::DmFuncY2Val = ",DmFuncY2Val];
Print["DabDcdUUk::mFuncYkVal = ",mFuncYkVal];
Print["DabDcdUUk::DmFuncYkVal = ",DmFuncYkVal];
Print["DabDcdUUk::DabDcdmFuncY2Val = ",DabDcdmFuncY2Val];
Print["DabDcdUUk::DabDcdmFuncYkVal = ",DabDcdmFuncYkVal];
Print["DabDcdUUk::Ck = ",Ck];
Print["DabDcdUUk::DabCk = ",DabCk];
Print["DabDcdUUk::DabDcdCk = ",DabDcdCk];
*)

If[useFullySymmetricPoly,
(
Ak=mFuncY2Val^(2*MaxMoment-2*level);
DabAk=(2*MaxMoment-2*level)*mFuncY2Val^(2*MaxMoment-2*level-1)*DmFuncY2Val;
DabDcdAk=(2*MaxMoment-2*level)*(2*MaxMoment-2*level-1)*mFuncY2Val^(2*MaxMoment-2*level-2)*Table[DmFuncY2Val[[a]]*DmFuncY2Val[[c]],{a,1,NAL},{c,1,NAL}]+(2*MaxMoment-2*level)*DabDcdmFuncY2Val;
DabDcdCk=Table[DabCk[[a]]*DabAk[[c]]+DabCk[[c]]*DabAk[[a]],{a,1,NAL},{c,1,NAL}]+Ck*DabDcdAk+Ak*DabDcdCk;
DabCk=(DabCk*Ak+Ck*DabAk);
Ck=Ck*Ak;
),
(
dummy=0;
)
];
)
];

retVal=Table[2*(Ck*DabDcdCk[[a,c]]+DabCk[[a]]*DabCk[[c]]),{a,1,NAL},{c,1,NAL}];
(*Print["DabDcdUUk::retVal = ",retVal];*)
Return[retVal];];

(* ==============================================*)

DabDcdUUAll[sab_?MatrixQ, aab_?MatrixQ,gammaLst_?VectorQ,alphaLst_?VectorQ,useUUL2_?BooleanQ,useFullySymmetricPoly_?BooleanQ]:=Module[{retVal,level},
CalculateYInvariants[sab, aab];
retVal=Sum[alphaLst[[level]]*DabDcdUUk[level,sab, aab,gammaLst[[level]],useUUL2,useFullySymmetricPoly,False],{level,1,MaxMoment}];
Return[retVal];
];

(* ==============================================*)
(*Potential of max 10th power*)
Up10[sab_?MatrixQ, aab_?MatrixQ,gammaLst_?VectorQ,alphaLst_?VectorQ]:=Module[{retVal,idx,M1,M2,M3,M4,M5,m532,m5221,m422,m211,m2,c1,c2,c3,c4,c5,mAll},
CalculateYInvariants[sab, aab];
M1=mFuncY1[sab, aab,False];
M2=mFuncY2[sab, aab,False];
M3=mFuncY3[sab, aab,False];
M4=mFuncY4[sab, aab,False];
M5=mFuncY5[sab, aab,False];
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

(*Dab[Up10]*)
DUp10[sab_?MatrixQ, aab_?MatrixQ,gammaLst_?VectorQ,alphaLst_?VectorQ]:=Module[{retVal,idx,M1,M2,M3,M4,M5,m532,m5221,m422,m211,m2,c1,c2,c3,c4,c5,dmAll,dM1,dM2,dM3,dM4,dM5,x532,dx532,dm532,x5221,dx5221,dm5221,x422,dx422,dm422,x211,dx211,dm211,x2,dx2,dm2},
CalculateYInvariants[sab, aab];
c1=gammaLst[[1]];
c2=gammaLst[[2]];
c3=gammaLst[[3]];
c4=gammaLst[[4]];
c5=gammaLst[[5]];
M1=mFuncY1[sab, aab,False];
M2=mFuncY2[sab, aab,False];
M3=mFuncY3[sab, aab,False];
M4=mFuncY4[sab, aab,False];
M5=mFuncY5[sab, aab,False];
dM1=DmFuncYk[1,sab, aab,False];
dM2=DmFuncYk[2,sab, aab,False];
dM3=DmFuncYk[3,sab, aab,False];
dM4=DmFuncYk[4,sab, aab,False];
dM5=DmFuncYk[5,sab, aab,False];

(*
Print["M1 = ",N[M1]];
Print["M2 = ",N[M2]];
Print["M3 = ",N[M3]];
Print["M4 = ",N[M4]];
Print["M5 = ",N[M5]];
Print["dM1 = ",N[dM1]];
Print["dM2 = ",N[dM2]];
Print["dM3 = ",N[dM3]];
Print["dM4 = ",N[dM4]];
Print["dM5 = ",N[dM5]];
*)

x532=(c3*c2*M5-c5*M3*M2);
dx532=c3*c2*dM5-c5*DabXXmYY[M3,dM3,M2,dM2];
dm532=DabXXpK[x532,dx532,2];

(*
Print["x532 = ",N[x532]];
Print["dx532 = ",N[dx532]];
Print["dm532 = ",N[dm532]];
*)
x5221=(c2^2*c1*M5-c5*M2^2*M1);
dx5221=c2^2*c1*dM5-c5*DabXXpKmYYpN[M2,dM2,2,M1,dM1,1];
dm5221=DabXXpK[x5221,dx5221,2];

(*
Print["x5221 = ",Expand[x5221]];
Print["dx5221 = ",Expand[dx5221]];
Print["dm5221 = ",Expand[dm5221]];*)(*Print["x5221 = ",N[x5221]];
Print["dx5221 = ",N[dx5221]];
Print["dm5221 = ",N[dm5221]];
*)

x422=(c2^2*M4-c4*M2^2);
dx422=c2^2*dM4-c4*DabXXpK[M2,dM2,2];
dm422=DabXXpKmYYpN[x422,dx422,2,M2,dM2,1];

(*
Print["x422 = ",N[x422]];
Print["dx422 = ",N[dx422]];
Print["dm422 = ",N[dm422]];
*)

x211=(c1^2*M2-c2*M1^2);
dx211=c1^2*dM2-c2*DabXXpK[M1,dM1,2];
dm211=DabXXpKmYYpN[x211,dx211,2,M2,dM2,3];

(*
Print["x211 = ",N[x211]];
Print["dx211 = ",N[dx211]];
Print["dm211 = ",N[dm211]];
*)

x2=(M2-c2);
dx2=dM2;
dm2=DabXXpK[x2,dx2,2];

(*
Print["x2 = ",N[x2]];
Print["dx2 = ",N[dx2]];
Print["dm2 = ",N[dm2]];
*)

dmAll={dm532,dm5221,dm422,dm211,dm2};
retVal=Sum[alphaLst[[idx]]*dmAll[[idx]],{idx,1,5}];
Return[retVal];
];

(* ==============================================*)

(*DabDcd[Up10]*)
DabDcdUp10[sab_?MatrixQ, aab_?MatrixQ,gammaLst_?VectorQ,alphaLst_?VectorQ]:=Module[{retVal,idx,M1,M2,M3,M4,M5,m532,m5221,m422,m211,m2,c1,c2,c3,c4,c5,ddmAll,dM1,dM2,dM3,dM4,dM5,x532,dx532,ddx532,ddm532,x5221,dx5221,ddx5221,ddm5221,x422,dx422,ddx422,ddm422,x211,dx211,ddx211,ddm211,x2,dx2,ddx2,ddm2,ddM1,ddM2,ddM3,ddM4,ddM5},
CalculateYInvariants[sab, aab];
c1=gammaLst[[1]];
c2=gammaLst[[2]];
c3=gammaLst[[3]];
c4=gammaLst[[4]];
c5=gammaLst[[5]];
M1=mFuncY1[sab, aab,False];
M2=mFuncY2[sab, aab,False];
M3=mFuncY3[sab, aab,False];
M4=mFuncY4[sab, aab,False];
M5=mFuncY5[sab, aab,False];
dM1=DmFuncYk[1,sab, aab,False];
dM2=DmFuncYk[2,sab, aab,False];
dM3=DmFuncYk[3,sab, aab,False];
dM4=DmFuncYk[4,sab, aab,False];
dM5=DmFuncYk[5,sab, aab,False];
ddM1=DDmFuncYk[1,sab, aab,False];
ddM2=DDmFuncYk[2,sab, aab,False];
ddM3=DDmFuncYk[3,sab, aab,False];
ddM4=DDmFuncYk[4,sab, aab,False];
ddM5=DDmFuncYk[5,sab, aab,False];

(*
Print["M1 = ",N[M1]];
Print["M2 = ",N[M2]];
Print["M3 = ",N[M3]];
Print["M4 = ",N[M4]];
Print["M5 = ",N[M5]];
Print["dM1 = ",N[dM1]];
Print["dM2 = ",N[dM2]];
Print["dM3 = ",N[dM3]];
Print["dM4 = ",N[dM4]];
Print["dM5 = ",N[dM5]];
Print["ddM1 = ",N[ddM1]];
Print["ddM2 = ",N[ddM2]];
Print["ddM3 = ",N[ddM3]];
Print["ddM4 = ",N[ddM4]];
Print["ddM5 = ",N[ddM5]];
*)

x532=(c3*c2*M5-c5*M3*M2);
dx532=c3*c2*dM5-c5*DabXXmYY[M3,dM3,M2,dM2];
ddx532=c3*c2*ddM5-c5*DabDcdXXmYY[M3,dM3,ddM3,M2,dM2,ddM2];
ddm532=DabDcdXXpK[x532,dx532,ddx532,2];

(*
Print["x532 = ",N[x532]];
Print["dx532 = ",N[dx532]];
Print["ddx532 = ",N[ddx532]];
Print["ddm532 = ",N[ddm532]];
*)

x5221=(c2^2*c1*M5-c5*M2^2*M1);
dx5221=c2^2*c1*dM5-c5*DabXXpKmYYpN[M2,dM2,2,M1,dM1,1];
ddx5221=c2^2*c1*ddM5-c5*DabDcdXXpKmYYpN[M2,dM2,ddM2,2,M1,dM1,ddM1,1];
ddm5221=DabDcdXXpK[x5221,dx5221,ddx5221,2];

(*
Print["x5221 = ",N[x5221]];
Print["dx5221 = ",N[dx5221]];
Print["ddx5221 = ",N[ddx5221]];
Print["ddm5221 = ",N[ddm5221]];
*)

x422=(c2^2*M4-c4*M2^2);
dx422=c2^2*dM4-c4*DabXXpK[M2,dM2,2];
ddx422=c2^2*ddM4-c4*DabDcdXXpK[M2,dM2,ddM2,2];
ddm422=DabDcdXXpKmYYpN[x422,dx422,ddx422,2,M2,dM2,ddM2,1];

(*
Print["x422 = ",N[x422]];
Print["dx422 = ",N[dx422]];
Print["ddx422 = ",N[ddx422]];
Print["ddm422 = ",N[ddm422]];
*)

x211=(c1^2*M2-c2*M1^2);
dx211=c1^2*dM2-c2*DabXXpK[M1,dM1,2];
ddx211=c1^2*ddM2-c2*DabDcdXXpK[M1,dM1,ddM1,2];
ddm211=DabDcdXXpKmYYpN[x211,dx211,ddx211,2,M2,dM2,ddM2,3];

(*
Print["x211 = ",N[x211]];
Print["dx211 = ",N[dx211]];
Print["ddx211 = ",N[ddx211]];
Print["ddm211 = ",N[ddm211]];
*)

x2=(M2-c2);
dx2=dM2;
ddx2=ddM2;
ddm2=DabDcdXXpK[x2,dx2,ddx2,2];

(*
Print["x2 = ",N[x2]];
Print["dx2 = ",N[dx2]];
Print["ddx2 = ",N[ddx2]];
Print["ddm2 = ",N[ddm2]];
*)

ddmAll={ddm532,ddm5221,ddm422,ddm211,ddm2};
retVal=Sum[alphaLst[[idx]]*ddmAll[[idx]],{idx,1,5}];
Return[retVal];
];

(* ==============================================*)
(* ==============================================*)
(*Potential of max 12th power*)
Up12[sab_?MatrixQ, aab_?MatrixQ,gammaLst_?VectorQ,alphaLst_?VectorQ]:=Module[{retVal,idx,M1,M2,M3,M4,M5,M6,m532,m5221,m422,m211,mm2,m6222,c1,c2,c3,c4,c5,c6,mAll},
CalculateYInvariants[sab, aab];
M1=mFuncY1[sab, aab,False];
M2=mFuncY2[sab, aab,False];
M3=mFuncY3[sab, aab,False];
M4=mFuncY4[sab, aab,False];
M5=mFuncY5[sab, aab,False];
M6=mFuncY6[sab, aab,False];
c1=gammaLst[[1]];
c2=gammaLst[[2]];
c3=gammaLst[[3]];
c4=gammaLst[[4]];
c5=gammaLst[[5]];
c6=gammaLst[[6]];
m532=M2*(c3*c2*M5-c5*M3*M2)^2;
m5221=M2*(c2^2*c1*M5-c5*M2^2*M1)^2;
m422=M2^2*(c2^2*M4-c4*M2^2)^2;
m211=M2^4*(c1^2*M2-c2*M1^2)^2;
mm2=(M2-c2)^2;
m6222=(c2^3*M6-c6*M2^3)^2;
mAll={m532,m5221,m422,m211,mm2,m6222};
retVal=Sum[alphaLst[[idx]]*mAll[[idx]],{idx,1,6}];
Return[retVal];
];

(* ==============================================*)

(*Dab[Up12]*)
DUp12[sab_?MatrixQ, aab_?MatrixQ,gammaLst_?VectorQ,alphaLst_?VectorQ]:=Module[{retVal,idx,M1,M2,M3,M4,M5,M6,m532,m5221,m422,m211,mm2,c1,c2,c3,c4,c5,c6,dmAll,dM1,dM2,dM3,dM4,dM5,dM6,x532,dx532,dm532,x5221,dx5221,dm5221,x422,dx422,dm422,x211,dx211,dm211,x2,dx2,dm2,x6222,dx6222,dm6222},
CalculateYInvariants[sab, aab];
c1=gammaLst[[1]];
c2=gammaLst[[2]];
c3=gammaLst[[3]];
c4=gammaLst[[4]];
c5=gammaLst[[5]];
c6=gammaLst[[6]];
M1=mFuncY1[sab, aab,False];
M2=mFuncY2[sab, aab,False];
M3=mFuncY3[sab, aab,False];
M4=mFuncY4[sab, aab,False];
M5=mFuncY5[sab, aab,False];
M6=mFuncY6[sab, aab,False];
dM1=DmFuncYk[1,sab, aab,False];
dM2=DmFuncYk[2,sab, aab,False];
dM3=DmFuncYk[3,sab, aab,False];
dM4=DmFuncYk[4,sab, aab,False];
dM5=DmFuncYk[5,sab, aab,False];
dM6=DmFuncYk[6,sab, aab,False];

(*
Print["M1 = ",N[M1]];
Print["M2 = ",N[M2]];
Print["M3 = ",N[M3]];
Print["M4 = ",N[M4]];
Print["M5 = ",N[M5]];
Print["dM1 = ",N[dM1]];
Print["dM2 = ",N[dM2]];
Print["dM3 = ",N[dM3]];
Print["dM4 = ",N[dM4]];
Print["dM5 = ",N[dM5]];
*)

x532=(c3*c2*M5-c5*M3*M2);
dx532=c3*c2*dM5-c5*DabXXmYY[M3,dM3,M2,dM2];
dm532=DabXXpKmYYpN[x532,dx532,2,M2,dM2,1];

(*
Print["x532 = ",N[x532]];
Print["dx532 = ",N[dx532]];
Print["dm532 = ",N[dm532]];
*)
x5221=(c2^2*c1*M5-c5*M2^2*M1);
dx5221=c2^2*c1*dM5-c5*DabXXpKmYYpN[M2,dM2,2,M1,dM1,1];
dm5221=DabXXpKmYYpN[x5221,dx5221,2,M2,dM2,1];
(*
Print["x5221 = ",Expand[x5221]];
Print["dx5221 = ",Expand[dx5221]];
Print["dm5221 = ",Expand[dm5221]];*)(*Print["x5221 = ",N[x5221]];
Print["dx5221 = ",N[dx5221]];
Print["dm5221 = ",N[dm5221]];
*)

x422=(c2^2*M4-c4*M2^2);
dx422=c2^2*dM4-c4*DabXXpK[M2,dM2,2];
dm422=DabXXpKmYYpN[x422,dx422,2,M2,dM2,2];

(*
Print["x422 = ",N[x422]];
Print["dx422 = ",N[dx422]];
Print["dm422 = ",N[dm422]];
*)

x211=(c1^2*M2-c2*M1^2);
dx211=c1^2*dM2-c2*DabXXpK[M1,dM1,2];
dm211=DabXXpKmYYpN[x211,dx211,2,M2,dM2,4];

(*
Print["x211 = ",N[x211]];
Print["dx211 = ",N[dx211]];
Print["dm211 = ",N[dm211]];
*)

x2=(M2-c2);
dx2=dM2;
dm2=DabXXpK[x2,dx2,2];

(*
Print["x2 = ",N[x2]];
Print["dx2 = ",N[dx2]];
Print["dm2 = ",N[dm2]];
*)

x6222=(c2^3*M6-c6*M2^3);
dx6222=c2^3*dM6-c6*DabXXpK[M2,dM2,3];
dm6222=DabXXpK[x6222,dx6222,2];

dmAll={dm532,dm5221,dm422,dm211,dm2,dm6222};
retVal=Sum[alphaLst[[idx]]*dmAll[[idx]],{idx,1,6}];
Return[retVal];
];

(* ==============================================*)

(*DabDcd[Up12]*)
DabDcdUp12[sab_?MatrixQ, aab_?MatrixQ,gammaLst_?VectorQ,alphaLst_?VectorQ]:=Module[{retVal,idx,M1,M2,M3,M4,M5,m532,m5221,m422,m211,m2,c1,c2,c3,c4,c5,ddmAll,dM1,dM2,dM3,dM4,dM5,x532,dx532,ddx532,ddm532,x5221,dx5221,ddx5221,ddm5221,x422,dx422,ddx422,ddm422,x211,dx211,ddx211,ddm211,x2,dx2,ddx2,ddm2,ddM1,ddM2,ddM3,ddM4,ddM5,c6,M6,dM6,ddM6,x6222,dx6222,ddx6222,ddm6222},
CalculateYInvariants[sab, aab];
c1=gammaLst[[1]];
c2=gammaLst[[2]];
c3=gammaLst[[3]];
c4=gammaLst[[4]];
c5=gammaLst[[5]];
c6=gammaLst[[6]];
M1=mFuncY1[sab, aab,False];
M2=mFuncY2[sab, aab,False];
M3=mFuncY3[sab, aab,False];
M4=mFuncY4[sab, aab,False];
M5=mFuncY5[sab, aab,False];
M6=mFuncY6[sab, aab,False];
dM1=DmFuncYk[1,sab, aab,False];
dM2=DmFuncYk[2,sab, aab,False];
dM3=DmFuncYk[3,sab, aab,False];
dM4=DmFuncYk[4,sab, aab,False];
dM5=DmFuncYk[5,sab, aab,False];
dM6=DmFuncYk[6,sab, aab,False];
ddM1=DDmFuncYk[1,sab, aab,False];
ddM2=DDmFuncYk[2,sab, aab,False];
ddM3=DDmFuncYk[3,sab, aab,False];
ddM4=DDmFuncYk[4,sab, aab,False];
ddM5=DDmFuncYk[5,sab, aab,False];
ddM6=DDmFuncYk[6,sab, aab,False];

(*
Print["M1 = ",N[M1]];
Print["M2 = ",N[M2]];
Print["M3 = ",N[M3]];
Print["M4 = ",N[M4]];
Print["M5 = ",N[M5]];
Print["dM1 = ",N[dM1]];
Print["dM2 = ",N[dM2]];
Print["dM3 = ",N[dM3]];
Print["dM4 = ",N[dM4]];
Print["dM5 = ",N[dM5]];
Print["ddM1 = ",N[ddM1]];
Print["ddM2 = ",N[ddM2]];
Print["ddM3 = ",N[ddM3]];
Print["ddM4 = ",N[ddM4]];
Print["ddM5 = ",N[ddM5]];
*)

x532=(c3*c2*M5-c5*M3*M2);
dx532=c3*c2*dM5-c5*DabXXmYY[M3,dM3,M2,dM2];
ddx532=c3*c2*ddM5-c5*DabDcdXXmYY[M3,dM3,ddM3,M2,dM2,ddM2];
ddm532=DabDcdXXpKmYYpN[x532,dx532,ddx532,2,M2,dM2,ddM2,1];

(*
Print["x532 = ",N[x532]];
Print["dx532 = ",N[dx532]];
Print["ddx532 = ",N[ddx532]];
Print["ddm532 = ",N[ddm532]];
*)

x5221=(c2^2*c1*M5-c5*M2^2*M1);
dx5221=c2^2*c1*dM5-c5*DabXXpKmYYpN[M2,dM2,2,M1,dM1,1];
ddx5221=c2^2*c1*ddM5-c5*DabDcdXXpKmYYpN[M2,dM2,ddM2,2,M1,dM1,ddM1,1];
ddm5221=DabDcdXXpKmYYpN[x5221,dx5221,ddx5221,2,M2,dM2,ddM2,1];

(*
Print["x5221 = ",N[x5221]];
Print["dx5221 = ",N[dx5221]];
Print["ddx5221 = ",N[ddx5221]];
Print["ddm5221 = ",N[ddm5221]];
*)

x422=(c2^2*M4-c4*M2^2);
dx422=c2^2*dM4-c4*DabXXpK[M2,dM2,2];
ddx422=c2^2*ddM4-c4*DabDcdXXpK[M2,dM2,ddM2,2];
ddm422=DabDcdXXpKmYYpN[x422,dx422,ddx422,2,M2,dM2,ddM2,2];

(*
Print["x422 = ",N[x422]];
Print["dx422 = ",N[dx422]];
Print["ddx422 = ",N[ddx422]];
Print["ddm422 = ",N[ddm422]];
*)

x211=(c1^2*M2-c2*M1^2);
dx211=c1^2*dM2-c2*DabXXpK[M1,dM1,2];
ddx211=c1^2*ddM2-c2*DabDcdXXpK[M1,dM1,ddM1,2];
ddm211=DabDcdXXpKmYYpN[x211,dx211,ddx211,2,M2,dM2,ddM2,4];

(*
Print["x211 = ",N[x211]];
Print["dx211 = ",N[dx211]];
Print["ddx211 = ",N[ddx211]];
Print["ddm211 = ",N[ddm211]];
*)

x2=(M2-c2);
dx2=dM2;
ddx2=ddM2;
ddm2=DabDcdXXpK[x2,dx2,ddx2,2];

(*
Print["x2 = ",N[x2]];
Print["dx2 = ",N[dx2]];
Print["ddx2 = ",N[ddx2]];
Print["ddm2 = ",N[ddm2]];
*)

x6222=(c2^3*M6-c6*M2^3);
dx6222=c2^3*dM6-c6*DabXXpK[M2,dM2,3];
ddx6222=c2^3*ddM6-c6*DabDcdXXpK[M2,dM2,ddM2,3];
ddm6222=DabDcdXXpK[x6222,dx6222,ddx6222,2];


ddmAll={ddm532,ddm5221,ddm422,ddm211,ddm2,ddm6222};
retVal=Sum[alphaLst[[idx]]*ddmAll[[idx]],{idx,1,6}];
Return[retVal];
];

(* ==============================================*)






