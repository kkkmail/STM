(* :Author: Konstantin K.Konstantinov *)
(* :Summary: ODE solving for STM. *)
(* :Copyright: K^3, 2013 *)
(* :Version: Revision: 1.05.001, Date:2013/10/06 *)
(* :Mathematica Version: 7.0 - 9.0 *)
(* ============================================== *)
GradientMultiplier=1;
NDSolveMaxSteps=50000;

Options[STMODE]={ODEPrintInfo -> True};

RunNDSolve[runID_?IntegerQ,tMax_?NumericQ,gradLinearFunc_,returnTangentialGradient_?BooleanQ,rawOpts___ ]:=Module[{sol,opts,odePrintInfoVal},
Print["RunNDSolve::Running for runID = ", runID];
opts=ProcessOptions[rawOpts];
Clear[sEvolution];

odePrintInfoVal=ODEPrintInfo /.opts /.Options[STMODE];

If[odePrintInfoVal,
(
Print["CoeffF4 = ", CoeffF4 // MatrixForm];
Print["CoeffF6 = ", CoeffF6 // MatrixForm];
Print["returnTangentialGradient = ", returnTangentialGradient];
Print["gradLinearFunc = ", Definition[gradLinearFunc]];
)
];

(* Print["gradLinearFunc[SIL, ", returnTangentialGradient, "] = ", gradLinearFunc[SIL,returnTangentialGradient]]; *)

sol=NDSolve[{D[sEvolution[t],t ]== -GradientMultiplier*gradLinearFunc[sEvolution[t],returnTangentialGradient],sEvolution[0]== SIL},sEvolution,{t,0,tMax},MaxSteps -> NDSolveMaxSteps];
PrintTimeUsed[];
Return[sol];
];