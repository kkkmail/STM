(* :Author:Konstantin K.Konstantinov *)
(* :Summary:Gradient of 2-nd power polynom minimization function and its decomposition into invariants. *)
(* :Copyright:K^3,2013 *)
(* :Version: Revision: 1.10.001, Date:2013/10/14 *)
(* :Mathematica Version:7.0-9.0 *)
(* ==============================================*)

(* MOVE somewhere to F2 ???*)
Print["TODO::In gradient implementation Tangential gradient was not included."];

(* ==============================================*)

(* Renormalization coefficients for gradient. Used for convenience. *)
CoeffF2NormIdentity={1,1,1};

(* The values in CoeffF4NormF4 are such so that to have gradient coincide with the F4 old style function. *)
Print["TODO::CoeffF2NormF2 must be changed from default value to be in the same form as for P4 model."];
CoeffF2NormF2={1,1,1};

CoeffF2Norm=CoeffF2NormF2;

(* Resets CoeffF2 to all zeros. *)
ResetCoeffF2[]:=ResetCoeffF2[False];
ResetCoeffF2[printOut_?BooleanQ]:=Module[{},
If[printOut,Print["Resetting CoeffF2."]];
CoeffF2=CoeffF2Zero;
IsGradientInitialized=False;
];

(* Updates CoeffF2. *)
UpdateCoeffF2[idx_?IntegerQ,coeffVal_?NumericQ]:=UpdateCoeffF2[idx,coeffVal,False];
UpdateCoeffF2[idx_?IntegerQ,coeffVal_?NumericQ,printOut_?BooleanQ]:=Module[{},
If[printOut,Print["Updating CoeffF2."]];
If[idx < 1 || idx > NoOfTermsF2,(Print["UpdateCoeffF2::idx is out of range. idx = ", idx]; ResetCoeffF2[];Return[];)];
CoeffF2[[1,idx]] = coeffVal;
IsGradientInitialized=False;
];

UpdateAllCoeffF2[allCoeff_?VectorQ]:=Module[{ii},
If[Length[allCoeff]!= NoOfTermsF2,
(
Print["UpdateAllCoeffF2::allCoeff have incorret length = ",Length[allCoeff]];
Abort[];
Return[];
)
];

ResetCoeffF2[];

For[ii=1,ii <= NoOfTermsF2,ii++,
(
UpdateCoeffF2[ii,allCoeff[[ii]]];
)
];
];

(* Linear version of analytical expression for gradient for all known terms. *)
GradP2AllLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=Module[{sab},
sab=ToMatrix[sabLinear];
Return[GradP2All[sab,returnTangentialGradient]];
];

(* Matrix version of analytical expression for gradient for for all known terms. *)
(* Uses global values of coefficients as stored in CoeffF2. *)
GradP2All[sab_?MatrixQ,returnTangentialGradient_?BooleanQ]:=Module[{gradVal,gradValTbl,ii},
(* CalculateP2Invariants[sab]; *)
gradValTbl=Table[(GradP2FuncList[[ii]])[sab,False,False],{ii,1,NoOfTermsF2}];
gradVal=Sum[CoeffF2[[1,ii]]*gradValTbl[[ii]],{ii,1,NoOfTermsF2}];
Return[TangentialGradient[gradVal,returnTangentialGradient]];
];

(* Must be used when only P2 model is used. *)
GradP2OnlyAllLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=Module[{sab},
sab=ToMatrix[sabLinear];
Return[GradP2OnlyAll[sab,returnTangentialGradient]];
];

(* Must be used when only P2 model is used. *)
GradP2OnlyAll[sab_?MatrixQ,returnTangentialGradient_?BooleanQ]:=Module[{},
CalculateP2Invariants[sab];
Return[GradP2All[sab,returnTangentialGradient]];
];

Print["TODO::Implement CalculateP2Invariants in a more efficient way."];
CalculateP2Invariants[sab_?MatrixQ]:=CalculateP4Invariants[sab];

GradP2T4CabcdLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP2T4CabcdLinear[sabLinear,returnTangentialGradient,True];

GradP2T2CabbdLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP2T2CabbdLinear[sabLinear,returnTangentialGradient,True];

GradP2T0CababLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP2T0CababLinear[sabLinear,returnTangentialGradient,True];

GradP2FuncList={GradP2T4Cabcd,GradP2T2Cabbd,GradP2T0Cabab};

GradP2T4Cabcd[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},
If[calculateInvariants,CalculateP2Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF2Norm[[idxD2T4Cabcd]]*(4*(SumS12 - 2*SumSx1[[a]] - 2*SumSx1[[b]] + 2*sab[[a,b]])),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP2T2Cabbd[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},
If[calculateInvariants,CalculateP2Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF2Norm[[idxD2T2Cabbd]]*(2*(SumSx1[[a]] + SumSx1[[b]] - 2*sab[[a,b]])),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];

GradP2T0Cabab[sab_?MatrixQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{gradVal,a,b},
If[calculateInvariants,CalculateP2Invariants[sab]];
gradVal=ToLinear[Table[If[a!=b,CoeffF2Norm[[idxD2T0Cabab]]*(4*sab[[a,b]]),0],{a,1,NNN},{b,1,NNN}]];
Return[gradVal];
];




