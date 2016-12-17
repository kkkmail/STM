(* :Author: Konstantin K.Konstantinov *)
(* :Summary: Epliog for Dynamics for STM. *)
(* :Copyright: K^3, 2013 *)
(* :Version: Revision: 1.16.001, Date:2013/11/11 *)
(* :Mathematica Version: 7.0 - 9.0 *)
(* ============================================== *)

Print["Initializing UAllSab."];
ySymb=CalculateY[S,False];
ySymbT=CalculateY[ST,False];
UAllSab=UAllYFunc[ySymb,kValList,GammaValList,AlphaValList,M2Val];
UAllSabT=UAllYFunc[ySymbT,kValList,GammaValList,AlphaValList,M2Val];
PrintTimeUsed[];

Print["Initializing Y derivatives."];
Print["dYAll"];
dYAll=Table[D[ySymb,SL[[ii]]],{ii,1,NA}]; 
PrintTimeUsed[];
(*
Print["Initializing UAllSab derivatives."];
Print["dUAllT"];
(* dUAllT=Table[D[UAllSabT,STL[[ii]]],{ii,1,NA}]; *)

DistributeDefinitions[UAllSabT,STL,NA];
dUAllT=ParallelTable[D[UAllSabT,STL[[ii]]],{ii,1,NA}];
PrintTimeUsed[];
*)