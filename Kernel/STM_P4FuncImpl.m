(* :Author:Konstantin K.Konstantinov *)
(* :Summary: Implementation of 4-th power polynom minimization function. *)
(* :Copyright:K^3,2013 *)
(* :Version: Revision: 1.06.001, Date:2013/10/07 *)
(* :Mathematica Version:7.0-9.0 *)
(* ==============================================*)

FP4T8Cabcdefgh[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},
If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T8Cabcdefgh]]*(SumS12^4 + 12*SumS12^2*(SumS12p2 - 2*SumS12S13) - 32*SumS12*(6*SumS12p2S13 - 2*SumS12p3 - 2*SumS12S13S14 + SumS12S13S23 - 3*SumS12S13S24) + 12*((SumS12p2 - 2*SumS12S13)^2 + 4*(12*SumS12p2S13S14 - 8*SumS12p2S13S23 + 8*SumS12p2S13S24 + 8*SumS12p2S23S34 + 6*SumS12p4 - 2*SumS12S13S14S15 + 8*SumS12S13S14S23 - 8*SumS12S13S14S25 + SumS12S13S24S34 - 6*SumS2mS2 - 16*SumS3mS - 4*SumSp4)));
Return[funcVal];
];

FP4T6Cabbdefgh[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},
If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T6Cabbdefgh]]*(-2*(SumS12p2 - 2*SumS12S13)*(SumS12p2 - SumS12S13) + SumS12^2*(-SumS12p2 + SumS12S13) + 4*SumS12*(5*SumS12p2S13 - 2*SumS12p3 - SumS12S13S14 + SumS12S13S23 - 2*SumS12S13S24) - 8*(9*SumS12p2S13S14 - 8*SumS12p2S13S23 + 6*SumS12p2S13S24 + 7*SumS12p2S23S34 + 6*SumS12p4 - SumS12S13S14S15 + 7*SumS12S13S14S23 - 5*SumS12S13S14S25 + SumS12S13S24S34 - 6*SumS2mS2 - 14*SumS3mS - 3*SumSp4));
Return[funcVal];
];

FP4T5Cabbdbfgh[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},
If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T5Cabbdbfgh]]*(SumS12*(-3*SumS12p2S13 + 2*SumS12p3 + SumS12S13S14) + 2*(6*SumS12p2S13S14 - 6*SumS12p2S13S23 + 6*SumS12p2S13S24 + 3*SumS12p2S23S34 + 6*SumS12p4 - SumS12S13S14S15 + 3*SumS12S13S14S23 - 3*(SumS12S13S14S25 + SumS2mS2) - 14*SumS3mS));
Return[funcVal];
];

FP4T4Cabbdbfbh[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},
If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T4Cabbdbfbh]]*(-6*SumS12p2S13S14 - 6*SumS12p4 + SumS12S13S14S15 + 3*SumS2mS2 + 8*SumS3mS);
Return[funcVal];
];

FP4T4Cababefgh[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},
If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T4Cababefgh]]*(SumS12^2*SumS12p2 + 4*SumS12*(-2*SumS12p2S13 + SumS12p3) + 2*SumS12p2*(SumS12p2 - 2*SumS12S13) + 8*(2*SumS12p2S13S14 - SumS12p2S13S23 + SumS12p2S13S24 + 2*(SumS12p2S23S34 + SumS12p4 - SumS2mS2 - 2*SumS3mS)));
Return[funcVal];
];

FP4T4Cabbddfgh[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},
If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T4Cabbddfgh]]*(SumS12*(-2*SumS12p2S13 + SumS12p3 - SumS12S13S23 + SumS12S13S24) + 2*(4*SumS12p2S13S14 - 7*SumS12p2S13S23 + 3*SumS12p2S13S24 + 4*SumS12p2S23S34 + 4*SumS12p4 + 6*SumS12S13S14S23 - 2*SumS12S13S14S25 + SumS12S13S24S34 - 2*(2*SumS2mS2 + 4*SumS3mS + SumSp4)));
Return[funcVal];
];

FP4T4Cabbdeffh[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},
If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T4Cabbdeffh]]*(10*SumS12p2S13S14 - 12*SumS12p2S13S23 + 4*SumS12p2S13S24 + 12*SumS12p2S23S34 + 10*SumS12p4 + (SumS12p2 - SumS12S13)^2 - SumS12S13S14S15 + 8*SumS12S13S14S23 - 4*SumS12S13S14S25 + 2*SumS12S13S24S34 - 13*SumS2mS2 - 16*SumS3mS - 4*SumSp4);
Return[funcVal];
];

FP4T3Cababbfgh[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},
If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T3Cababbfgh]]*(SumS12*(SumS12p2S13 - SumS12p3) - 2*(SumS12p2S13S14 - SumS12p2S13S23 + SumS12p2S13S24 + SumS12p2S23S34 + 2*SumS12p4 - SumS2mS2 - 4*SumS3mS));
Return[funcVal];
];

FP4T3Cabbddfdh[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T3Cabbddfdh]]*(-SumS12p2S13S14 + 4*SumS12p2S13S23 - 2*SumS12p2S13S24 - SumS12p2S23S34 - 2*SumS12p4 - 2*SumS12S13S14S23 + SumS12S13S14S25 + SumS2mS2 + 4*SumS3mS);
Return[funcVal];];

FP4T2Cababbfbh[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},
If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T2Cababbfbh]]*(SumS12p2S13S14 + 2*SumS12p4 - SumS2mS2 - 2*SumS3mS);
Return[funcVal];
];

FP4T2Cababeffh[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},
If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T2Cababeffh]]*(-2*SumS12p2S13S14 + 2*SumS12p2S13S23 - 4*SumS12p2S23S34 - 4*SumS12p4 + SumS12p2*(-SumS12p2 + SumS12S13) + 6*SumS2mS2 + 4*SumS3mS);
Return[funcVal];
];

FP4T2Cabbddffh[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T2Cabbddffh]]*(-SumS12p2S13S14 + 3*SumS12p2S13S23 - 2*SumS12p2S23S34 - 2*SumS12p4 - 2*SumS12S13S14S23 - SumS12S13S24S34 + 3*SumS2mS2 + 2*SumS3mS + SumSp4);
Return[funcVal];];

FP4T2Cabbdbddh[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T2Cabbdbddh]]*(-SumS12p2S13S23 + SumS12p2S13S24 + SumS12p4 - 2*SumS3mS);
Return[funcVal];];

FP4T2Cabababgh[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T2Cabababgh]]*(SumS12*SumS12p3 + 2*SumS12p4 - 4*SumS3mS);
Return[funcVal];];

FP4T2Cabbdadgh[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T2Cabbdadgh]]*(6*SumS12p2S13S23 - 6*SumS12S13S14S23 + SumS12*SumS12S13S23);
Return[funcVal];];

FP4T1Cababbffh[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T1Cababbffh]]*(-SumS12p2S13S23 + SumS12p2S23S34 + SumS12p4 - SumS2mS2 - SumS3mS);
Return[funcVal];];

FP4T1Cabbdaddh[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T1Cabbdaddh]]*(-2*SumS12p2S13S23 + SumS12S13S14S23);
Return[funcVal];];

FP4T1Cabababbh[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T1Cabababbh]]*(-SumS12p4 + SumS3mS);
Return[funcVal];];

FP4T0Cababbfbf[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T0Cababbfbf]]*(-SumS12p4 + SumS2mS2);
Return[funcVal];];

FP4T0Cababefef[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T0Cababefef]]*(SumS12p2^2 + 2*SumS12p4 - 4*SumS2mS2);
Return[funcVal];];

FP4T0Cabbddfaf[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T0Cabbddfaf]]*(SumS12p4 + SumS12S13S24S34 - 2*SumS2mS2);
Return[funcVal];];

FP4T0Cabbdbdad[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T0Cabbdbdad]]*(SumS12p2S13S23);
Return[funcVal];];

FP4T0Cabababab[sab_?MatrixQ,calculateInvariants_?BooleanQ]:=Module[{funcVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
funcVal=CoeffF4Norm[[idxD4T0Cabababab]]*(SumS12p4);
Return[funcVal];];

