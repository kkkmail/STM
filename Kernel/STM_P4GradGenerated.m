(* :Author:Konstantin K.Konstantinov *)
(* :Summary: GENERATED CODE for gradient of 4-th power polynom minimization function *)
(*           and its decomposition into invariants. *)
(* :NOTE: See STM_P4Grad _Generator _ 106__ 001.xlsx *)
(* :Copyright:K^3,2013 *)
(* :Version: Revision: 1.06.001, Date:2013/10/07 *)
(* :Mathematica Version:7.0-9.0 *)
(* ==============================================*)

(* GradP4FuncList (below) is the list of all matrix gradient functions for table calls. *)
(* Generated in Excel. *)

GradP4T8CabcdefghLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T8CabcdefghLinear[sabLinear,returnTangentialGradient,True];
GradP4T6CabbdefghLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T6CabbdefghLinear[sabLinear,returnTangentialGradient,True];
GradP4T5CabbdbfghLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T5CabbdbfghLinear[sabLinear,returnTangentialGradient,True];
GradP4T4CabbdbfbhLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T4CabbdbfbhLinear[sabLinear,returnTangentialGradient,True];
GradP4T4CababefghLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T4CababefghLinear[sabLinear,returnTangentialGradient,True];
GradP4T4CabbddfghLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T4CabbddfghLinear[sabLinear,returnTangentialGradient,True];
GradP4T4CabbdeffhLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T4CabbdeffhLinear[sabLinear,returnTangentialGradient,True];
GradP4T3CababbfghLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T3CababbfghLinear[sabLinear,returnTangentialGradient,True];
GradP4T3CabbddfdhLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T3CabbddfdhLinear[sabLinear,returnTangentialGradient,True];
GradP4T2CababbfbhLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T2CababbfbhLinear[sabLinear,returnTangentialGradient,True];
GradP4T2CababeffhLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T2CababeffhLinear[sabLinear,returnTangentialGradient,True];
GradP4T2CabbddffhLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T2CabbddffhLinear[sabLinear,returnTangentialGradient,True];
GradP4T2CabbdbddhLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T2CabbdbddhLinear[sabLinear,returnTangentialGradient,True];
GradP4T2CabababghLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T2CabababghLinear[sabLinear,returnTangentialGradient,True];
GradP4T2CabbdadghLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T2CabbdadghLinear[sabLinear,returnTangentialGradient,True];
GradP4T1CababbffhLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T1CababbffhLinear[sabLinear,returnTangentialGradient,True];
GradP4T1CabbdaddhLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T1CabbdaddhLinear[sabLinear,returnTangentialGradient,True];
GradP4T1CabababbhLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T1CabababbhLinear[sabLinear,returnTangentialGradient,True];
GradP4T0CababbfbfLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T0CababbfbfLinear[sabLinear,returnTangentialGradient,True];
GradP4T0CababefefLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T0CababefefLinear[sabLinear,returnTangentialGradient,True];
GradP4T0CabbddfafLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T0CabbddfafLinear[sabLinear,returnTangentialGradient,True];
GradP4T0CabbdbdadLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T0CabbdbdadLinear[sabLinear,returnTangentialGradient,True];
GradP4T0CababababLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=GradP4T0CababababLinear[sabLinear,returnTangentialGradient,True];

(* *)

GradP4T8CabcdefghLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T8Cabcdefgh[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T6CabbdefghLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T6Cabbdefgh[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T5CabbdbfghLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T5Cabbdbfgh[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T4CabbdbfbhLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T4Cabbdbfbh[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T4CababefghLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T4Cababefgh[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T4CabbddfghLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T4Cabbddfgh[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T4CabbdeffhLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T4Cabbdeffh[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T3CababbfghLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T3Cababbfgh[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T3CabbddfdhLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T3Cabbddfdh[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T2CababbfbhLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T2Cababbfbh[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T2CababeffhLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T2Cababeffh[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T2CabbddffhLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T2Cabbddffh[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T2CabbdbddhLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T2Cabbdbddh[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T2CabababghLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T2Cabababgh[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T2CabbdadghLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T2Cabbdadgh[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T1CababbffhLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T1Cababbffh[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T1CabbdaddhLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T1Cabbdaddh[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T1CabababbhLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T1Cabababbh[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T0CababbfbfLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T0Cababbfbf[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T0CababefefLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T0Cababefef[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T0CabbddfafLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T0Cabbddfaf[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T0CabbdbdadLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T0Cabbdbdad[sab,returnTangentialGradient,calculateInvariants]];];
GradP4T0CababababLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ,calculateInvariants_?BooleanQ]:=Module[{sab},sab=ToMatrix[sabLinear];
Return[GradP4T0Cabababab[sab,returnTangentialGradient,calculateInvariants]];];

GradP4FuncList={GradP4T8Cabcdefgh,GradP4T6Cabbdefgh,GradP4T5Cabbdbfgh,GradP4T4Cabbdbfbh,GradP4T4Cababefgh,GradP4T4Cabbddfgh,GradP4T4Cabbdeffh,GradP4T3Cababbfgh,GradP4T3Cabbddfdh,GradP4T2Cababbfbh,GradP4T2Cababeffh,GradP4T2Cabbddffh,GradP4T2Cabbdbddh,GradP4T2Cabababgh,GradP4T2Cabbdadgh,GradP4T1Cababbffh,GradP4T1Cabbdaddh,GradP4T1Cabababbh,GradP4T0Cababbfbf,GradP4T0Cababefef,GradP4T0Cabbddfaf,GradP4T0Cabbdbdad,GradP4T0Cabababab};

