(* :Author:Konstantin K.Konstantinov *)
(* :Summary:Gradient of 6-th power polynom minimization function and its decomposition into invariants. *)
(* :Copyright:K^3,2013 *)
(* :Version: Revision: 1.03.001, Date:2013/10/04 *)
(* :Mathematica Version:7.0-9.0 *)
(* ==============================================*)

(* Resets CoeffF6 to all zeros. *)
ResetCoeffF6[]:=ResetCoeffF6[False];
ResetCoeffF6[printOut_?BooleanQ]:=Module[{},
If[printOut,Print["Resetting CoeffF6."]];
CoeffF6=CoeffF6Zero;
IsGradientInitialized=False;
];

(* Updates CoeffF6. *)
UpdateCoeffF6[idx_?IntegerQ,coeffVal_?NumericQ]:=UpdateCoeffF6[idx,coeffVal,False];
UpdateCoeffF6[idx_?IntegerQ,coeffVal_?NumericQ,printOut_?BooleanQ]:=Module[{},
If[printOut,Print["Updating CoeffF6."]];
If[idx < 1 || idx > NoTermCnt,(Print["UpdateCoeffF6::idx is out of range. idx = ", idx]; ResetCoeffF6[];Return[];)];
CoeffF6[[1,idx]] = coeffVal;
IsGradientInitialized=False;
];

(* Linear version of analytical expression for gradient for all known terms. *)
GradP6AllLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=Module[{sab},
sab=ToMatrix[sabLinear];
Return[GradP6All[sab,returnTangentialGradient]];
];

(* Matrix version of analytical expression for gradient for all known terms. *)
(* Uses global values of coefficients as stored in CoeffF6. *)
GradP6All[sab_?MatrixQ,returnTangentialGradient_?BooleanQ]:=Module[{gradVal},
gradVal=CoeffF6[[1,idxD6T0Cabababababab]]*GradP6T0Cabababababab[sab,False];
Return[TangentialGradient[gradVal,returnTangentialGradient]];
];

(* Linear version of analytical expression for gradient for Cabababababab term only. *)
GradP6T0CababababababLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=Module[{sab},
sab=ToMatrix[sabLinear];
Return[GradP6T0Cabababababab[sab,returnTangentialGradient]];
];

(* Matrix version of analytical expression for gradient for Cabababababab term only. *)
GradP6T0Cabababababab[sab_?MatrixQ,returnTangentialGradient_?BooleanQ]:=Module[{gradVal},
gradVal=Table[6*sab[[remapReverseTable[[ii,1]],remapReverseTable[[ii,2]]]]^5,{ii,1,NA}];
Return[TangentialGradient[gradVal,returnTangentialGradient]];
];

