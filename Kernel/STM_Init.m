(* ============================================== *)
(* :Summary: Loading of the necessary modules for P4 and P6 models. *)
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
Options[STMInit]={UseComplexVariables -> False,UseReImVariables -> False,UseY6 -> False, SilentRun -> False};
(* ============================================== *)
(* If true then complex variables will be used. *)
UseComplexVariablesValue=False;
(* If complex variables are used then if true then Re and Im variables will be used. *)
UseReImVariablesValue=False;
(* If True then load and use Y6 - 6th power moments.*)
UseY6Value=False;
(* If True then some output is suppressed *)
SilentRunValue=False;
(* ============================================== *)
MaxMoment=5;
(* ============================================== *)
Needs["NumericalCalculus`"];
(* ============================================== *)
LoadModules[pathListVal_,rawOpts___]:=Module[{opts},
Print["Loading modules..."];
opts=ProcessOptions[rawOpts];
UseComplexVariablesValue=UseComplexVariables /.opts /.Options[STMInit];
SilentRunValue=SilentRun /.opts /.Options[STMInit];
UseReImVariablesValue=False;

If[UseComplexVariablesValue,
(
UseReImVariablesValue=UseReImVariables /.opts /.Options[STMInit];
)
];

If[UseComplexVariablesValue,
(
If[UseReImVariablesValue,
(
Print["LoadModules::Using Re Im variables."];
),
(
Print["LoadModules::Using complex variables."];
)
];
),
(
Print["LoadModules::Using real variables."];
)
];

UseY6Value=UseY6 /.opts /.Options[STMInit];

If[!UseY6Value,
(
Print["LoadModules::Moments 1 through 5 are being used."];
)
,
(
Print["LoadModules::Moments 1 through 6 are being used."];
MaxMoment=6;
)
];

Get["STM_Common.m",Path-> pathListVal];
Get["STM_InitValues.m",Path-> pathListVal];

(* ============================================== *)

Get["STM_P2Terms.m",Path-> pathListVal];
Get["STM_P2Func.m",Path-> pathListVal];
Get["STM_P2Grad.m",Path-> pathListVal];

(* ============================================== *)

Get["STM_P4Terms.m",Path-> pathListVal];
Get["STM_P4Func.m",Path-> pathListVal];
Get["STM_P4FuncImpl.m",Path-> pathListVal];

Get["STM_P4Grad.m",Path-> pathListVal];
Get["STM_P4GradGenerated.m",Path-> pathListVal];
Get["STM_P4GradImpl.m",Path-> pathListVal];

(* ============================================== *)

Get["STM_P6Terms.m",Path-> pathListVal];
Get["STM_P6Func.m",Path-> pathListVal];
Get["STM_P6Grad.m",Path-> pathListVal];

(* ============================================== *)

Get["STM_Output.m",Path-> pathListVal];
Get["STM_Calc.m",Path-> pathListVal];
Get["STM_Derivatives.m",Path-> pathListVal];
Get["STM_ODE.m",Path-> pathListVal];

Get["STM_Minimization.m",Path-> pathListVal];
Get["STM_MinF2.m",Path-> pathListVal];

(* ============================================== *)

Get["STM_Tests.m",Path-> pathListVal];

(* ============================================== *)

Get["STM_Dynamics.m",Path-> pathListVal];

(* ============================================== *)

If[!UseComplexVariablesValue,
(
Print["LoadModules::Loading STM_YDerivativesImpl.m, STM_CommonReal.m"];
Get["STM_YDerivativesImpl.m",Path-> pathListVal];
Get["STM_CommonReal.m",Path-> pathListVal];
),
(

If[UseReImVariablesValue,
(
Print["LoadModules::Loading STM_YDerivativesImplReImCYI.m, STM_YDerivativesImplReImGen.m, STM_YDerivativesImplReIm.m, STM_CommonReIm.m"];
Get["STM_YDerivativesImplReImCYI.m",Path-> pathListVal];
Get["STM_YDerivativesImplReImGen.m",Path-> pathListVal];
Get["STM_YDerivativesImplReIm.m",Path-> pathListVal];
Get["STM_CommonReIm.m",Path-> pathListVal];

If[UseY6Value,
(
Print["LoadModules::Loading STM_YDerivativesImpl_6_ReImCYI.m, STM_YDerivativesImpl_6_ReImGen.m"];
Get["STM_YDerivativesImpl_6_ReImCYI.m",Path-> pathListVal];
Get["STM_YDerivativesImpl_6_ReImGen.m",Path-> pathListVal];
)
];
),
(
Print["LoadModules::Loading STM_YDerivativesImplCplx.m, STM_CommonComplex.m"];
Get["STM_YDerivativesImplCplx.m",Path-> pathListVal];
Get["STM_CommonComplex.m",Path-> pathListVal];
)
];
)
];
];

(* ============================================== *)

(* ProcessOptions ensures that options are in a flat list. It wraps List over raw options if necessary. *)
ProcessOptions[rawOpts___]:=Module[{opts},
opts=Flatten[{rawOpts}];
Return[opts];
];

(* ============================================== *)

(* Initialize all models. *)
InitializeModels[]:=Module[{},
InitializeF2Model[];
InitializeF4Model[];
InitializeF6Model[];
];
(* ============================================== *)
(* Gradient for all models. *)
(* Linear version of analytical expression for gradient for all known terms. *)
GradAllLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=Module[{sab},
sab=ToMatrix[sabLinear];
Return[GradAll[sab,returnTangentialGradient]];
];
(* ============================================== *)
(* Matrix version of analytical expression for gradient for all known terms. *)
(* Uses global values of coefficients as stored in CoeffF2, CoeffF4, and CoeffF6. *)
GradAll[sab_?MatrixQ,returnTangentialGradient_?BooleanQ]:=Module[{gradVal,gradValP2,gradValP4,gradValP6},
(* !!! We must calculate GradP4All because GradP2All relies on certain variables, which GradP4All calculates calling CalculateP4Invariants !!! *)
gradValP4=GradP4All[sab,False];
gradValP2=GradP2All[sab,False];
gradValP6=GradP6All[sab,False];

gradVal=gradValP2+gradValP4+gradValP6;
Return[TangentialGradient[gradVal,returnTangentialGradient]];
];
(* ============================================== *)
GradP2P4AllLinear[sabLinear_?VectorQ,returnTangentialGradient_?BooleanQ]:=Module[{sab},
sab=ToMatrix[sabLinear];
Return[GradP2P4All[sab,returnTangentialGradient]];
];
(* ============================================== *)
(* Matrix version of analytical expression for gradient for all known terms. *)
(* Uses global values of coefficients as stored in CoeffF2, CoeffF4, and CoeffF6. *)
GradP2P4All[sab_?MatrixQ,returnTangentialGradient_?BooleanQ]:=Module[{gradVal,gradValP2,gradValP4},
(* !!! We must calculate GradP4All because GradP2All relies on certain variables, which GradP4All calculates calling CalculateP4Invariants !!! *)
gradValP4=GradP4All[sab,False];
gradValP2=GradP2All[sab,False];

gradVal=gradValP2+gradValP4;
Return[TangentialGradient[gradVal,returnTangentialGradient]];
];
(* ============================================== *)
