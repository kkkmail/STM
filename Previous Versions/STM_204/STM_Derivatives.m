(* :Author: Konstantin K.Konstantinov *)
(* :Summary: Derivatives calculations for STM. *)
(* :Copyright: K^3, 2013 - 2014 *)
(* :Version:Revision: 2.01.001,Date: 2014/03/08 *)
(* :Mathematica Version: 7.0 - 9.0 *)
(* ============================================== *)

(* Matrix function to return Dab[X * Y] *)
DabXXmYY[XX_,DabXX_?MatrixQ,YY_,DabYY_?MatrixQ]:=Module[{retVal},
retVal=XX*DabYY+YY*DabXX;
Return[retVal];
];

(* ============================================== *)

(* Matrix function to return Dab[X^k] *)
DabXXpK[XX_,DabXX_?MatrixQ,kk_?IntegerQ]:=Module[{retVal},
If[kk==1,
(
retVal=DabXX;
),
(
retVal=kk*XX^(kk-1)*DabXX;
)
];

Return[retVal];
];

(* ============================================== *)

(* Matrix function to return Dab[X^k * Y^n] *)
DabXXpKmYYpN[XX_,DabXX_?MatrixQ,kk_?IntegerQ,YY_,DabYY_?MatrixQ,nn_?IntegerQ]:=Module[{retVal,XXpK,YYpN,dabXXpK,dabYYpN},
XXpK=XX^kk;
YYpN=YY^nn;
dabXXpK=DabXXpK[XX,DabXX,kk];
dabYYpN=DabXXpK[YY,DabYY,nn];
retVal=DabXXmYY[XXpK,dabXXpK,YYpN,dabYYpN];
Return[retVal];
];

(* ============================================== *)

(* 4D tensor to return DabDcd[XX * YY] *)
DabDcdXXmYY[XX_,DabXX_?MatrixQ,DabDcdXX_,YY_,DabYY_?MatrixQ,DabDcdYY_]:=Module[{retVal,a,b,c,d,len},
len=Length[DabXX];
retVal=XX*DabDcdYY+YY*DabDcdXX+Table[DabXX[[a,b]]*DabYY[[c,d]]+DabXX[[c,d]]*DabYY[[a,b]],{a,1,len},{b,1,len},{c,1,len},{d,1,len}];
Return[retVal];
];

(* ============================================== *)

(* 4D tensor to return DabDcd[XX^k] *)
DabDcdXXpK[XX_,DabXX_?MatrixQ,DabDcdXX_,kk_?IntegerQ]:=Module[{retVal,a,b,c,d,len},
len=Length[DabXX];

If[kk== 1,
(
retVal=DabDcdXX;
),
If[kk==2,
(
retVal=Table[kk*(XX^(kk-1)*DabDcdXX[[a,b,c,d]]+(kk-1)*DabXX[[a,b]]*DabXX[[c,d]]),{a,1,len},{b,1,len},{c,1,len},{d,1,len}];
),
(
retVal=Table[kk*(XX^(kk-1)*DabDcdXX[[a,b,c,d]]+(kk-1)*XX^(kk-2)*DabXX[[a,b]]*DabXX[[c,d]]),{a,1,len},{b,1,len},{c,1,len},{d,1,len}];
)
]
];

Return[retVal];
];

(* ============================================== *)

(* 4D tensor to return DabDcd[XX^k * YY^n] *)
DabDcdXXpKmYYpN[XX_,DabXX_?MatrixQ,DabDcdXX_,kk_?IntegerQ,YY_,DabYY_?MatrixQ,DabDcdYY_,nn_?IntegerQ]:=Module[{retVal,a,b,c,d,len,XXpK,YYpN,dabXXpK,dabYYpN,dabdcdXXpK,dabdcdYYpN},
len=Length[DabXX];
XXpK=XX^kk;
YYpN=YY^nn;
dabXXpK=DabXXpK[XX,DabXX,kk];
dabYYpN=DabXXpK[YY,DabYY,nn];
dabdcdXXpK=DabDcdXXpK[XX,DabXX,DabDcdXX,kk];
dabdcdYYpN=DabDcdXXpK[YY,DabYY,DabDcdYY,nn];
retVal=DabDcdXXmYY[XXpK,dabXXpK,dabdcdXXpK,YYpN,dabYYpN,dabdcdYYpN];
Return[retVal];
];

(* ============================================== *)
(* ============================================== *)

(* Vector function to return Dab[X * Y] *)
DabXXmYY[XX_,DabXX_?VectorQ,YY_,DabYY_?VectorQ]:=Module[{retVal},
retVal=XX*DabYY+YY*DabXX;
Return[retVal];
];

(* ============================================== *)

(* Vector function to return Dab[X^k] *)
DabXXpK[XX_,DabXX_?VectorQ,kk_?IntegerQ]:=Module[{retVal},
If[kk==1,
(
retVal=DabXX;
),
(
retVal=kk*XX^(kk-1)*DabXX;
)
];

Return[retVal];
];

(* ============================================== *)

(* Vector function to return Dab[X^k * Y^n] *)
DabXXpKmYYpN[XX_,DabXX_?VectorQ,kk_?IntegerQ,YY_,DabYY_?VectorQ,nn_?IntegerQ]:=Module[{retVal,XXpK,YYpN,dabXXpK,dabYYpN},
XXpK=XX^kk;
YYpN=YY^nn;
dabXXpK=DabXXpK[XX,DabXX,kk];
dabYYpN=DabXXpK[YY,DabYY,nn];
retVal=DabXXmYY[XXpK,dabXXpK,YYpN,dabYYpN];
Return[retVal];
];

(* ============================================== *)

(* Matrix function to return DabDcd[XX * YY] *)
DabDcdXXmYY[XX_,DabXX_?VectorQ,DabDcdXX_,YY_,DabYY_?VectorQ,DabDcdYY_]:=Module[{retVal,a,c,len},
len=Length[DabXX];
retVal=XX*DabDcdYY+YY*DabDcdXX+Table[DabXX[[a]]*DabYY[[c]]+DabXX[[c]]*DabYY[[a]],{a,1,len},{c,1,len}];
Return[retVal];
];

(* ============================================== *)

(* Matrix function to return DabDcd[XX^k] *)
DabDcdXXpK[XX_,DabXX_?VectorQ,DabDcdXX_,kk_?IntegerQ]:=Module[{retVal,a,c,len},
len=Length[DabXX];

If[kk== 1,
(
retVal=DabDcdXX;
),
If[kk==2,
(
retVal=Table[kk*(XX^(kk-1)*DabDcdXX[[a,c]]+(kk-1)*DabXX[[a]]*DabXX[[c]]),{a,1,len},{c,1,len}];
),
(
retVal=Table[kk*(XX^(kk-1)*DabDcdXX[[a,c]]+(kk-1)*XX^(kk-2)*DabXX[[a]]*DabXX[[c]]),{a,1,len},{c,1,len}];
)
]
];

Return[retVal];
];

(* ============================================== *)

(* Matrix function to return DabDcd[XX^k * YY^n] *)
DabDcdXXpKmYYpN[XX_,DabXX_?VectorQ,DabDcdXX_,kk_?IntegerQ,YY_,DabYY_?VectorQ,DabDcdYY_,nn_?IntegerQ]:=Module[{retVal,a,c,len,XXpK,YYpN,dabXXpK,dabYYpN,dabdcdXXpK,dabdcdYYpN},
len=Length[DabXX];
XXpK=XX^kk;
YYpN=YY^nn;
dabXXpK=DabXXpK[XX,DabXX,kk];
dabYYpN=DabXXpK[YY,DabYY,nn];
dabdcdXXpK=DabDcdXXpK[XX,DabXX,DabDcdXX,kk];
dabdcdYYpN=DabDcdXXpK[YY,DabYY,DabDcdYY,nn];
retVal=DabDcdXXmYY[XXpK,dabXXpK,dabdcdXXpK,YYpN,dabYYpN,dabdcdYYpN];
Return[retVal];
];

(* ============================================== *)

