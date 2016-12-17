(* :Author:Konstantin K.Konstantinov *)
(* :Summary:2nd power polynom minimization function and its decomposition into invariants. *)
(* :Copyright:K^3,2013 *)
(* :Version: Revision: 1.10.001, Date:2013/10/14 *)
(* :Mathematica Version:7.0-9.0 *)
(* ==============================================*)

InitializeF2Model[]:=Module[{noOfElements,f4Val},
Print["Initializing F2 model."];

noOfElements=8;
Reset[];
Initialize[noOfElements];


InitializeTermsF2[];
(* f4Val=F4FuncMult[S]; *)
(* IsInitialized=True; *)
];

FP2All[sab_?MatrixQ]:=FP2All[sab,True];
FP2All[sab_?MatrixQ,calculateCommonInvariants_?BooleanQ]:=Module[{funcVal,funcValTbl,ii},
(* Print["FP2All::Starting."]; *)
If[calculateCommonInvariants,CalculateF4Invariants[sab,calculateCommonInvariants]];
funcValTbl=Table[(FP2FuncList[[ii]])[sab,False],{ii,1,NoOfTermsF2}];

funcVal=Sum[CoeffF2[[1,ii]]*funcValTbl[[ii]],{ii,1,NoOfTermsF2}];
Return[funcVal];
];

(* List of all FP2 functions. *)
FP2FuncList:={FP2T4Cabcd,FP2T2Cabbd,FP2T0Cabab};

FP2T4Cabcd[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},
If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF2Norm[[idxD2T4Cabcd]]*(SumS12^2 + 2*SumS12p2 - 4*SumS12S13);
Return[funcVal];
];

FP2T2Cabbd[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},
If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF2Norm[[idxD2T2Cabbd]]*(-SumS12p2 + SumS12S13);
Return[funcVal];
];

FP2T0Cabab[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},
If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF2Norm[[idxD2T0Cabab]]*(SumS12p2);
Return[funcVal];
];



