(* :Author:Konstantin K.Konstantinov *)
(* :Summary: Implementation of gradient of 4-th power polynom minimization function. *)
(* :Copyright:K^3,2013 *)
(* :Version: Revision: 1.06.001, Date:2013/10/07 *)
(* :Mathematica Version:7.0-9.0 *)
(* ==============================================*)

GradP4T8Cabcdefgh[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T8Cabcdefgh]]*(8*(SumS12^3 + 6*SumS12*SumS12p2 - 48*SumS12p2S13 + 16*SumS12p3 - 12*SumS12*SumS12S13 + 16*SumS12S13S14 - 8*SumS12S13S23 + 24*SumS12S13S24 - 6*SumS12^2*(SumSx1[[a]] + SumSx1[[b]]) - 12*SumS12p2*(SumSx1[[a]] + SumSx1[[b]]) + 24*SumS12S13*(SumSx1[[a]] + SumSx1[[b]]) + 24*SumS12*(SumSx1[[a]]^2 + SumSx1[[b]]^2) - 48*(SumSx1[[a]]^3 + SumSx1[[b]]^3) + 24*SumS12*(SumSx1[[a]]*SumSx1[[b]] + SumSx1S12[[a]] + SumSx1S12[[b]]) - 48*(SumSx1[[a]]^2*SumSx1[[b]] + SumSx1[[a]]*(SumSx1[[b]]^2 + 2*SumSx1S12[[a]]) + 2*SumSx1[[b]]*SumSx1S12[[b]] + SumSx1S12S13[[a]] + SumSx1S12S13[[b]]) - 48*(SumSx1[[b]]*SumSx1S12[[a]] + SumSx1[[a]]*SumSx1S12[[b]] + SumSx1S12S23[[a]] + SumSx1S12S23[[b]]) + 6*SumS12^2*sab[[a,b]] + 12*SumS12p2*sab[[a,b]] - 24*SumS12S13*sab[[a,b]] - 144*(SumSx1p2[[a]] + SumSx1p2[[b]])*sab[[a,b]] + 48*SumS12*sab[[a,b]]^2 + 288*sab[[a,b]]^3 + 96*(SumSx1p2S12[[a]] + SumSx1p2S12[[b]] + 2*SumSx1[[a]]*SumSx1[[b]]*sab[[a,b]]) - 24*SumS12*(SumSx1p2[[a]] + SumSx1p2[[b]] + 2*(SumSx1[[a]] + SumSx1[[b]])*sab[[a,b]]) + 48*(SumSx1[[b]]*SumSx1p2[[a]] + SumSx1[[a]]*SumSx1p2[[b]] + SumSx1S12p2[[a]] + SumSx1S12p2[[b]] + 2*SumSx1S12[[a]]*sab[[a,b]] + 2*SumSx1S12[[b]]*sab[[a,b]]) - 96*(SumSx1p3[[a]] + SumSx1p3[[b]] + 3*(SumSx1[[a]] + SumSx1[[b]])*sab[[a,b]]^2) + 144*(SumSx1[[a]]*SumSx1p2[[a]] + SumSx1[[a]]^2*sab[[a,b]] + SumSx1[[b]]*(SumSx1p2[[b]] + SumSx1[[b]]*sab[[a,b]])) - 24*SumS12*SumSx1Sy1[[a,b]] - 96*(SumSx1p2Sy1[[a,b]] + SumSx1p2Sy1[[b,a]] + 2*sab[[a,b]]*SumSx1Sy1[[a,b]]) + 48*SumSx1Sy2S12[[a,b]] + 48*(2*SumSx1[[a]]*SumSx1Sy1[[a,b]] + 2*SumSx1[[b]]*SumSx1Sy1[[a,b]] + 2*SumSx1Sy1S12[[a,b]] + SumSx1Sy2S12[[a,a]] + SumSx1Sy2S12[[b,b]]))),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T6Cabbdefgh[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T6Cabbdefgh]]*(2*(-2*SumS12*SumS12p2 + 20*SumS12p2S13 - 8*SumS12p3 + 2*SumS12*SumS12S13 - 4*SumS12S13S14 + 4*SumS12S13S23 - 8*SumS12S13S24 + SumS12^2*(SumSx1[[a]] + SumSx1[[b]]) + 6*SumS12p2*(SumSx1[[a]] + SumSx1[[b]]) - 8*SumS12S13*(SumSx1[[a]] + SumSx1[[b]]) - 6*SumS12*(SumSx1[[a]]^2 + SumSx1[[b]]^2) + 16*(SumSx1[[a]]^3 + SumSx1[[b]]^3) - 8*SumS12*(SumSx1[[a]]*SumSx1[[b]] + SumSx1S12[[a]] + SumSx1S12[[b]]) + 20*(SumSx1[[a]]^2*SumSx1[[b]] + SumSx1[[a]]*(SumSx1[[b]]^2 + 2*SumSx1S12[[a]]) + 2*SumSx1[[b]]*SumSx1S12[[b]] + SumSx1S12S13[[a]] + SumSx1S12S13[[b]]) + 24*(SumSx1[[b]]*SumSx1S12[[a]] + SumSx1[[a]]*SumSx1S12[[b]] + SumSx1S12S23[[a]] + SumSx1S12S23[[b]]) - 2*SumS12^2*sab[[a,b]] - 8*SumS12p2*sab[[a,b]] + 12*SumS12S13*sab[[a,b]] + 96*(SumSx1p2[[a]] + SumSx1p2[[b]])*sab[[a,b]] - 24*SumS12*sab[[a,b]]^2 - 192*sab[[a,b]]^3 - 48*(SumSx1p2S12[[a]] + SumSx1p2S12[[b]] + 2*SumSx1[[a]]*SumSx1[[b]]*sab[[a,b]]) + 10*SumS12*(SumSx1p2[[a]] + SumSx1p2[[b]] + 2*(SumSx1[[a]] + SumSx1[[b]])*sab[[a,b]]) - 28*(SumSx1[[b]]*SumSx1p2[[a]] + SumSx1[[a]]*SumSx1p2[[b]] + SumSx1S12p2[[a]] + SumSx1S12p2[[b]] + 2*SumSx1S12[[a]]*sab[[a,b]] + 2*SumSx1S12[[b]]*sab[[a,b]]) + 56*(SumSx1p3[[a]] + SumSx1p3[[b]] + 3*(SumSx1[[a]] + SumSx1[[b]])*sab[[a,b]]^2) - 72*(SumSx1[[a]]*SumSx1p2[[a]] + SumSx1[[a]]^2*sab[[a,b]] + SumSx1[[b]]*(SumSx1p2[[b]] + SumSx1[[b]]*sab[[a,b]])) + 12*SumS12*SumSx1Sy1[[a,b]] + 64*(SumSx1p2Sy1[[a,b]] + SumSx1p2Sy1[[b,a]] + 2*sab[[a,b]]*SumSx1Sy1[[a,b]]) - 32*SumSx1Sy2S12[[a,b]] - 28*(2*SumSx1[[a]]*SumSx1Sy1[[a,b]] + 2*SumSx1[[b]]*SumSx1Sy1[[a,b]] + 2*SumSx1Sy1S12[[a,b]] + SumSx1Sy2S12[[a,a]] + SumSx1Sy2S12[[b,b]]))),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T5Cabbdbfgh[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T5Cabbdbfgh]]*(-6*SumS12p2S13 + 4*SumS12p3 + 2*SumS12S13S14 + 3*SumS12*(SumSx1[[a]]^2 + SumSx1[[b]]^2) - 8*(SumSx1[[a]]^3 + SumSx1[[b]]^3) - 6*(SumSx1[[a]]^2*SumSx1[[b]] + SumSx1[[a]]*(SumSx1[[b]]^2 + 2*SumSx1S12[[a]]) + 2*SumSx1[[b]]*SumSx1S12[[b]] + SumSx1S12S13[[a]] + SumSx1S12S13[[b]]) - 24*(SumSx1p2[[a]] + SumSx1p2[[b]])*sab[[a,b]] + 12*SumS12*sab[[a,b]]^2 + 96*sab[[a,b]]^3 + 24*(SumSx1p2S12[[a]] + SumSx1p2S12[[b]] + 2*SumSx1[[a]]*SumSx1[[b]]*sab[[a,b]]) - 3*SumS12*(SumSx1p2[[a]] + SumSx1p2[[b]] + 2*(SumSx1[[a]] + SumSx1[[b]])*sab[[a,b]]) + 6*(SumSx1[[b]]*SumSx1p2[[a]] + SumSx1[[a]]*SumSx1p2[[b]] + SumSx1S12p2[[a]] + SumSx1S12p2[[b]] + 2*SumSx1S12[[a]]*sab[[a,b]] + 2*SumSx1S12[[b]]*sab[[a,b]]) - 28*(SumSx1p3[[a]] + SumSx1p3[[b]] + 3*(SumSx1[[a]] + SumSx1[[b]])*sab[[a,b]]^2) + 24*(SumSx1[[a]]*SumSx1p2[[a]] + SumSx1[[a]]^2*sab[[a,b]] + SumSx1[[b]]*(SumSx1p2[[b]] + SumSx1[[b]]*sab[[a,b]])) - 24*(SumSx1p2Sy1[[a,b]] + SumSx1p2Sy1[[b,a]] + 2*sab[[a,b]]*SumSx1Sy1[[a,b]]) + 6*(2*SumSx1[[a]]*SumSx1Sy1[[a,b]] + 2*SumSx1[[b]]*SumSx1Sy1[[a,b]] + 2*SumSx1Sy1S12[[a,b]] + SumSx1Sy2S12[[a,a]] + SumSx1Sy2S12[[b,b]])),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T4Cabbdbfbh[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T4Cabbdbfbh]]*(4*(SumSx1[[a]]^3 + SumSx1[[b]]^3 + 2*SumSx1p3[[a]] + 2*SumSx1p3[[b]] - 3*SumSx1[[a]]^2*sab[[a,b]] - 3*SumSx1[[b]]^2*sab[[a,b]] + 3*SumSx1p2[[a]]*sab[[a,b]] + 3*SumSx1p2[[b]]*sab[[a,b]] - 12*sab[[a,b]]^3 - 3*SumSx1[[a]]*(SumSx1p2[[a]] - 2*sab[[a,b]]^2) - 3*SumSx1[[b]]*(SumSx1p2[[b]] - 2*sab[[a,b]]^2))),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T4Cababefgh[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T4Cababefgh]]*(4*(SumS12*SumS12p2 - 4*SumS12p2S13 + 2*SumS12p3 - 2*SumS12*SumSx1p2[[a]] - 2*SumS12*SumSx1p2[[b]] + 4*SumSx1p2S12[[a]] + 4*SumSx1p2S12[[b]] - 8*SumSx1p3[[a]] - 8*SumSx1p3[[b]] + 4*SumSx1S12p2[[a]] + 4*SumSx1S12p2[[b]] + SumS12^2*sab[[a,b]] + 4*SumS12p2*sab[[a,b]] - 4*SumS12S13*sab[[a,b]] + 8*SumSx1[[a]]^2*sab[[a,b]] + 8*SumSx1[[b]]^2*sab[[a,b]] - 16*SumSx1p2[[a]]*sab[[a,b]] - 16*SumSx1p2[[b]]*sab[[a,b]] + 8*SumSx1S12[[a]]*sab[[a,b]] + 8*SumSx1S12[[b]]*sab[[a,b]] + 6*SumS12*sab[[a,b]]^2 + 32*sab[[a,b]]^3 - 2*SumSx1[[b]]*(SumS12p2 - 2*SumSx1p2[[a]] - 4*SumSx1p2[[b]] + 2*SumS12*sab[[a,b]] + 12*sab[[a,b]]^2) - 2*SumSx1[[a]]*(SumS12p2 - 4*SumSx1p2[[a]] - 2*SumSx1p2[[b]] + 2*SumS12*sab[[a,b]] - 4*SumSx1[[b]]*sab[[a,b]] + 12*sab[[a,b]]^2) - 4*SumSx1p2Sy1[[a,b]] - 4*SumSx1p2Sy1[[b,a]] - 8*sab[[a,b]]*SumSx1Sy1[[a,b]])),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T4Cabbddfgh[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T4Cabbddfgh]]*(2*(-2*SumS12p2S13 + SumS12p3 - SumS12S13S23 + SumS12S13S24 + SumS12*(SumSx1[[a]]*SumSx1[[b]] + SumSx1S12[[a]] + SumSx1S12[[b]]) - 2*(SumSx1[[a]]^2*SumSx1[[b]] + SumSx1[[a]]*(SumSx1[[b]]^2 + 2*SumSx1S12[[a]]) + 2*SumSx1[[b]]*SumSx1S12[[b]] + SumSx1S12S13[[a]] + SumSx1S12S13[[b]]) - 4*(SumSx1[[b]]*SumSx1S12[[a]] + SumSx1[[a]]*SumSx1S12[[b]] + SumSx1S12S23[[a]] + SumSx1S12S23[[b]]) - 16*(SumSx1p2[[a]] + SumSx1p2[[b]])*sab[[a,b]] + 3*SumS12*sab[[a,b]]^2 + 32*sab[[a,b]]^3 + 6*(SumSx1p2S12[[a]] + SumSx1p2S12[[b]] + 2*SumSx1[[a]]*SumSx1[[b]]*sab[[a,b]]) - SumS12*(SumSx1p2[[a]] + SumSx1p2[[b]] + 2*(SumSx1[[a]] + SumSx1[[b]])*sab[[a,b]]) + 4*(SumSx1[[b]]*SumSx1p2[[a]] + SumSx1[[a]]*SumSx1p2[[b]] + SumSx1S12p2[[a]] + SumSx1S12p2[[b]] + 2*SumSx1S12[[a]]*sab[[a,b]] + 2*SumSx1S12[[b]]*sab[[a,b]]) - 8*(SumSx1p3[[a]] + SumSx1p3[[b]] + 3*(SumSx1[[a]] + SumSx1[[b]])*sab[[a,b]]^2) + 8*(SumSx1[[a]]*SumSx1p2[[a]] + SumSx1[[a]]^2*sab[[a,b]] + SumSx1[[b]]*(SumSx1p2[[b]] + SumSx1[[b]]*sab[[a,b]])) - 3*SumS12*SumSx1Sy1[[a,b]] - 14*(SumSx1p2Sy1[[a,b]] + SumSx1p2Sy1[[b,a]] + 2*sab[[a,b]]*SumSx1Sy1[[a,b]]) + 8*SumSx1Sy2S12[[a,b]] + 6*(2*SumSx1[[a]]*SumSx1Sy1[[a,b]] + 2*SumSx1[[b]]*SumSx1Sy1[[a,b]] + 2*SumSx1Sy1S12[[a,b]] + SumSx1Sy2S12[[a,a]] + SumSx1Sy2S12[[b,b]]))),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T4Cabbdeffh[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T4Cabbdeffh]]*(-4*SumSx1[[a]]^3 - 4*SumSx1[[a]]^2*(SumSx1[[b]] - 5*sab[[a,b]]) - 4*SumSx1[[a]]*(SumS12p2 - SumS12S13 + SumSx1[[b]]^2 - 5*SumSx1p2[[a]] - 3*SumSx1p2[[b]] + 2*SumSx1S12[[a]] + 2*SumSx1S12[[b]] - 4*SumSx1[[b]]*sab[[a,b]] + 12*sab[[a,b]]^2 - 4*SumSx1Sy1[[a,b]]) - 4*(SumSx1[[b]]^3 - 2*SumSx1p2S12[[a]] - 2*SumSx1p2S12[[b]] + 4*SumSx1p3[[a]] + 4*SumSx1p3[[b]] - 3*SumSx1S12p2[[a]] - 3*SumSx1S12p2[[b]] + SumSx1S12S13[[a]] + SumSx1S12S13[[b]] + 2*SumSx1S12S23[[a]] + 2*SumSx1S12S23[[b]] - 2*SumS12p2*sab[[a,b]] + 2*SumS12S13*sab[[a,b]] - 5*SumSx1[[b]]^2*sab[[a,b]] + 13*SumSx1p2[[a]]*sab[[a,b]] + 13*SumSx1p2[[b]]*sab[[a,b]] - 6*SumSx1S12[[a]]*sab[[a,b]] - 6*SumSx1S12[[b]]*sab[[a,b]] - 20*sab[[a,b]]^3 + 6*SumSx1p2Sy1[[a,b]] + 6*SumSx1p2Sy1[[b,a]] + SumSx1[[b]]*(SumS12p2 - SumS12S13 - 3*SumSx1p2[[a]] - 5*SumSx1p2[[b]] + 2*SumSx1S12[[a]] + 2*SumSx1S12[[b]] + 12*sab[[a,b]]^2 - 4*SumSx1Sy1[[a,b]]) + 12*sab[[a,b]]*SumSx1Sy1[[a,b]] - 4*SumSx1Sy1S12[[a,b]] - 2*SumSx1Sy2S12[[a,a]] - 4*SumSx1Sy2S12[[a,b]] - 2*SumSx1Sy2S12[[b,b]])),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T3Cababbfgh[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T3Cababbfgh]]*(2*SumS12p2S13 - 2*SumS12p3 + 8*(SumSx1p2[[a]] + SumSx1p2[[b]])*sab[[a,b]] - 6*SumS12*sab[[a,b]]^2 - 32*sab[[a,b]]^3 - 4*(SumSx1p2S12[[a]] + SumSx1p2S12[[b]] + 2*SumSx1[[a]]*SumSx1[[b]]*sab[[a,b]]) + SumS12*(SumSx1p2[[a]] + SumSx1p2[[b]] + 2*(SumSx1[[a]] + SumSx1[[b]])*sab[[a,b]]) - 2*(SumSx1[[b]]*SumSx1p2[[a]] + SumSx1[[a]]*SumSx1p2[[b]] + SumSx1S12p2[[a]] + SumSx1S12p2[[b]] + 2*SumSx1S12[[a]]*sab[[a,b]] + 2*SumSx1S12[[b]]*sab[[a,b]]) + 8*(SumSx1p3[[a]] + SumSx1p3[[b]] + 3*(SumSx1[[a]] + SumSx1[[b]])*sab[[a,b]]^2) - 4*(SumSx1[[a]]*SumSx1p2[[a]] + SumSx1[[a]]^2*sab[[a,b]] + SumSx1[[b]]*(SumSx1p2[[b]] + SumSx1[[b]]*sab[[a,b]])) + 4*(SumSx1p2Sy1[[a,b]] + SumSx1p2Sy1[[b,a]] + 2*sab[[a,b]]*SumSx1Sy1[[a,b]])),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T3Cabbddfdh[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T3Cabbddfdh]]*(-4*SumSx1p2S12[[a]] - 4*SumSx1p2S12[[b]] + 4*SumSx1p3[[a]] + 4*SumSx1p3[[b]] - SumSx1S12p2[[a]] - SumSx1S12p2[[b]] + SumSx1S12S13[[a]] + SumSx1S12S13[[b]] + SumSx1[[a]]^2*(SumSx1[[b]] - 2*sab[[a,b]]) - 2*SumSx1[[b]]^2*sab[[a,b]] + 4*SumSx1p2[[a]]*sab[[a,b]] + 4*SumSx1p2[[b]]*sab[[a,b]] - 2*SumSx1S12[[a]]*sab[[a,b]] - 2*SumSx1S12[[b]]*sab[[a,b]] - 16*sab[[a,b]]^3 + 8*SumSx1p2Sy1[[a,b]] + 8*SumSx1p2Sy1[[b,a]] + SumSx1[[a]]*(SumSx1[[b]]^2 - 2*SumSx1p2[[a]] - SumSx1p2[[b]] + 2*SumSx1S12[[a]] - 8*SumSx1[[b]]*sab[[a,b]] + 12*sab[[a,b]]^2 - 4*SumSx1Sy1[[a,b]]) + 16*sab[[a,b]]*SumSx1Sy1[[a,b]] - SumSx1[[b]]*(SumSx1p2[[a]] + 2*SumSx1p2[[b]] - 2*SumSx1S12[[b]] - 12*sab[[a,b]]^2 + 4*SumSx1Sy1[[a,b]]) - 4*SumSx1Sy1S12[[a,b]] - 2*SumSx1Sy2S12[[a,a]] - 2*SumSx1Sy2S12[[b,b]]),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T2Cababbfbh[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T2Cababbfbh]]*(2*SumSx1[[a]]*SumSx1p2[[a]] + 2*SumSx1[[b]]*SumSx1p2[[b]] + 2*SumSx1[[a]]^2*sab[[a,b]] + 2*SumSx1[[b]]^2*sab[[a,b]] - 4*SumSx1p2[[a]]*sab[[a,b]] - 4*SumSx1p2[[b]]*sab[[a,b]] + 16*sab[[a,b]]^3 - 2*(SumSx1p3[[a]] + SumSx1p3[[b]] + 3*(SumSx1[[a]] + SumSx1[[b]])*sab[[a,b]]^2)),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T2Cababeffh[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T2Cababeffh]]*(2*SumS12p2*(SumSx1[[a]] + SumSx1[[b]]) - 8*SumS12p2*sab[[a,b]] + 4*SumS12S13*sab[[a,b]] + 24*(SumSx1p2[[a]] + SumSx1p2[[b]])*sab[[a,b]] - 32*sab[[a,b]]^3 - 4*(SumSx1[[b]]*SumSx1p2[[a]] + SumSx1[[a]]*SumSx1p2[[b]] + SumSx1S12p2[[a]] + SumSx1S12p2[[b]] + 2*SumSx1S12[[a]]*sab[[a,b]] + 2*SumSx1S12[[b]]*sab[[a,b]]) + 4*(SumSx1p3[[a]] + SumSx1p3[[b]] + 3*(SumSx1[[a]] + SumSx1[[b]])*sab[[a,b]]^2) - 4*(SumSx1[[a]]*SumSx1p2[[a]] + SumSx1[[a]]^2*sab[[a,b]] + SumSx1[[b]]*(SumSx1p2[[b]] + SumSx1[[b]]*sab[[a,b]])) + 4*(SumSx1p2Sy1[[a,b]] + SumSx1p2Sy1[[b,a]] + 2*sab[[a,b]]*SumSx1Sy1[[a,b]])),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T2Cabbddffh[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T2Cabbddffh]]*(-2*(-SumSx1p3[[a]] - SumSx1p3[[b]] + SumSx1S12p2[[a]] + SumSx1S12p2[[b]] - SumSx1S12S23[[a]] - SumSx1S12S23[[b]] + SumSx1[[a]]^2*sab[[a,b]] + SumSx1[[b]]^2*sab[[a,b]] - 6*SumSx1p2[[a]]*sab[[a,b]] - 6*SumSx1p2[[b]]*sab[[a,b]] + 2*SumSx1S12[[a]]*sab[[a,b]] + 2*SumSx1S12[[b]]*sab[[a,b]] + 8*sab[[a,b]]^3 - 3*SumSx1p2Sy1[[a,b]] - 3*SumSx1p2Sy1[[b,a]] - 6*sab[[a,b]]*SumSx1Sy1[[a,b]] + SumSx1[[b]]*(SumSx1p2[[a]] + SumSx1p2[[b]] - SumSx1S12[[a]] - 3*sab[[a,b]]^2 + 2*SumSx1Sy1[[a,b]]) + SumSx1[[a]]*(SumSx1p2[[a]] + SumSx1p2[[b]] - SumSx1S12[[b]] - 3*sab[[a,b]]^2 + 2*SumSx1Sy1[[a,b]]) + 2*SumSx1Sy1S12[[a,b]] + SumSx1Sy2S12[[a,a]] + 4*SumSx1Sy2S12[[a,b]] + SumSx1Sy2S12[[b,b]])),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T2Cabbdbddh[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T2Cabbdbddh]]*(2*(SumSx1p2S12[[a]] + SumSx1p2S12[[b]] - SumSx1p3[[a]] - SumSx1p3[[b]] + 2*SumSx1[[a]]*SumSx1[[b]]*sab[[a,b]] - 3*SumSx1[[a]]*sab[[a,b]]^2 - 3*SumSx1[[b]]*sab[[a,b]]^2 + 4*sab[[a,b]]^3 - SumSx1p2Sy1[[a,b]] - SumSx1p2Sy1[[b,a]] - 2*sab[[a,b]]*SumSx1Sy1[[a,b]])),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T2Cabababgh[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T2Cabababgh]]*(2*(SumS12p3 + 3*SumS12*sab[[a,b]]^2 + 8*sab[[a,b]]^3 - 2*(SumSx1p3[[a]] + SumSx1p3[[b]] + 3*(SumSx1[[a]] + SumSx1[[b]])*sab[[a,b]]^2))),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T2Cabbdadgh[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T2Cabbdadgh]]*(2*(SumS12S13S23 + 3*SumS12*SumSx1Sy1[[a,b]] + 6*(SumSx1p2Sy1[[a,b]] + SumSx1p2Sy1[[b,a]] + 2*sab[[a,b]]*SumSx1Sy1[[a,b]]) - 3*(2*SumSx1[[a]]*SumSx1Sy1[[a,b]] + 2*SumSx1[[b]]*SumSx1Sy1[[a,b]] + 2*SumSx1Sy1S12[[a,b]] + SumSx1Sy2S12[[a,a]] + SumSx1Sy2S12[[b,b]]))),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T1Cababbffh[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T1Cababbffh]]*(-SumSx1p3[[a]] - SumSx1p3[[b]] + SumSx1S12p2[[a]] + SumSx1S12p2[[b]] - 4*SumSx1p2[[a]]*sab[[a,b]] - 4*SumSx1p2[[b]]*sab[[a,b]] + 2*SumSx1S12[[a]]*sab[[a,b]] + 2*SumSx1S12[[b]]*sab[[a,b]] + 8*sab[[a,b]]^3 + SumSx1[[b]]*(SumSx1p2[[a]] - 3*sab[[a,b]]^2) + SumSx1[[a]]*(SumSx1p2[[b]] - 3*sab[[a,b]]^2) - 2*SumSx1p2Sy1[[a,b]] - 2*SumSx1p2Sy1[[b,a]] - 4*sab[[a,b]]*SumSx1Sy1[[a,b]]),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T1Cabbdaddh[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T1Cabbdaddh]]*(-4*SumSx1p2Sy1[[a,b]] - 4*SumSx1p2Sy1[[b,a]] + 2*SumSx1[[a]]*SumSx1Sy1[[a,b]] + 2*SumSx1[[b]]*SumSx1Sy1[[a,b]] - 8*sab[[a,b]]*SumSx1Sy1[[a,b]] + 2*SumSx1Sy1S12[[a,b]] + SumSx1Sy2S12[[a,a]] + SumSx1Sy2S12[[b,b]]),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T1Cabababbh[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T1Cabababbh]]*(SumSx1p3[[a]] + SumSx1p3[[b]] + (3*SumSx1[[a]] + 3*SumSx1[[b]] - 8*sab[[a,b]])*sab[[a,b]]^2),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T0Cababbfbf[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T0Cababbfbf]]*(4*sab[[a,b]]*(SumSx1p2[[a]] + SumSx1p2[[b]] - 2*sab[[a,b]]^2)),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T0Cababefef[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T0Cababefef]]*(8*sab[[a,b]]*(SumS12p2 - 2*SumSx1p2[[a]] - 2*SumSx1p2[[b]] + 2*sab[[a,b]]^2)),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T0Cabbddfaf[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T0Cabbddfaf]]*(8*(-(SumSx1p2[[a]]*sab[[a,b]]) - SumSx1p2[[b]]*sab[[a,b]] + sab[[a,b]]^3 + SumSx1Sy2S12[[a,b]])),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T0Cabbdbdad[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T0Cabbdbdad]]*(2*(SumSx1p2Sy1[[a,b]] + SumSx1p2Sy1[[b,a]] + 2*sab[[a,b]]*SumSx1Sy1[[a,b]])),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP4T0Cabababab[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},If[calculateInvariants,CalculateP4Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF4Norm[[idxD4T0Cabababab]]*(8*sab[[a,b]]^3),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

