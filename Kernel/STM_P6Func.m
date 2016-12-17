(* :Author:Konstantin K.Konstantinov *)
(* :Summary:SOME 6th power polynom minimization function and related. *)
(* :Copyright:K^3,2013 *)
(* :Version: Revision: 1.03.001, Date:2013/10/04 *)
(* :Mathematica Version:7.0-9.0 *)
(* ==============================================*)
ResetF6[]:=Module[{},
Print["Resetting 6th power polynom model."];
NoTermCntF6=0;
TermMatrixF6={};
TermExampleMatrixF6={};

NoInvariantCntF6=0;
InvariantMatrixF6={};
InvariantExpressionMatrixF6={};
InvariantStrExpressionMatrixF6={};
];

InitializeF6Model[]:=Module[{noOfElements,f6Val},
Print["Initializing P6 model."];
noOfElements=12;
ResetF6[];
Initialize[noOfElements];
InitializeTermsF6[];
(* f6Val=F6FuncMult[S]; *)
IsInitializedF6=True;
];
