(* :Author:Konstantin K.Konstantinov *)
(* :Summary: Dab transformation. *)
(* :Copyright: K^3, 2013 - 2014 *)
(* :Version: Revision: 1.23.001, Date: 2014/02/21 *)
(* :Mathematica Version:7.0-9.0 *)
(* ==============================================*)

(*
Off[Part::"pspec"];
PathList={"W:\\STM\\"};
Get["STM_Symbolic.m",Path-> PathList];
Get["STM_SymbolicTrailer.m",Path-> PathList];
*)

DebugSymbolicDab = False;
DabShiftValue=4;
StringShift[shiftVal_?IntegerQ]:=Apply[StringJoin,Table[" ", {ii,1,shiftVal}]];

Dab[rawExpr_]:=Dab[rawExpr,"s"];
Dab[rawExpr_,diffVarName_?StringQ]:=Dab[rawExpr,diffVarName,-DabShiftValue+1];
Dab[rawExpr_,diffVarName_?StringQ,shiftVal_?IntegerQ]:=Module[{expr,exLen,exLst,exHead,ii,elem,elemDab,retVal,elemDabLen,elemDabPart,elemDabHead,elemDabLst,partLen,partHead,partElem,partLst,jj,kdLst, ind,kdRule,indNew,kdRuleElem,partLstNew,elemDabPartNew,elemDabNewLst,dummy,shift,antiSymVal,symVal,isConstant},

shift=shiftVal+DabShiftValue;

If[DebugSymbolicDab,
(
Print[StringShift[shift],"  "];
Print[StringShift[shift],sep];
Print[StringShift[shift],sep];
Print[StringShift[shift],sep];
Print[StringShift[shift],"Dab::Starting"];
)
];

expr=Expand[rawExpr];
exLen=Length[expr];
exLst=Apply[List,expr];
exHead=Head[expr];
antiSymVal=OptionValue[ToExpression[diffVarName],AntiSymmetric];
symVal=OptionValue[ToExpression[diffVarName],Symmetric];
isConstant=MemberQ[Attributes[expr],Constant];

If[DebugSymbolicDab,
(
Print[StringShift[shift],"expr = ", expr];
Print[StringShift[shift],"exLen = ", exLen];
Print[StringShift[shift],"exLst = ", exLst];
Print[StringShift[shift],"exHead = ", exHead];
Print[StringShift[shift],"diffVarName = ", diffVarName];
Print[StringShift[shift],"antiSymVal = ", antiSymVal];
Print[StringShift[shift],"symVal = ", symVal];
Print[StringShift[shift],"isConstant = ", isConstant];
)
];

If[exLen==0 && (ToString[exHead]== "Integer" || ToString[exHead]== "Real" || ToString[exHead]== "Rational"|| ToString[exHead]== "Complex" || ToString[expr]== "NNN" || ToString[expr]== "gamma1"|| ToString[expr]== "gamma2"|| ToString[expr]== "gamma3"|| ToString[expr]== "gamma4"|| ToString[expr]== "gamma5") || (ToString[exHead]== "Symbol"  || isConstant== True),
(
If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::Processing numbers or constants..."];)];
Return[0];
)
];

If[ToString[exHead]== "Plus",
(
If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::Processing Plus. Calling Dab..."]; )];
elemDab=Table[Dab[exLst[[ii]],diffVarName,shift],{ii,1,exLen}];
retVal=Apply[Plus,elemDab];
If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::Plus retVal = ", retVal]; )];
Return[retVal];
)
];

If[ToString[exHead]== "Times",
(
If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::Processing Times. Calling Dab..."]; )];
elemDab=Table[Dab[exLst[[ii]],diffVarName,shift],{ii,1,exLen}];
If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::Times elemDab = ", elemDab]; )];
retVal=Sum[Apply[Times,ReplacePart[exLst,ii -> elemDab[[ii]]]],{ii,1,exLen}];
If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::Times retVal = ", retVal]; )];
Return[retVal];
)
];

If[ToString[exHead]== "Power",
(
If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::Processing Power..."]; )];

If[DebugSymbolicDab,
(Print[StringShift[shift],"Dab::Power exLst[[1]] = ", exLst[[1]], ", exLst[[2]] = ", exLst[[2]]]; )];
If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::Calling Dab..."];)];
retVal=exLst[[2]]*Power[exLst[[1]],exLst[[2]]-1]*Dab[exLst[[1]],diffVarName,shift]+Log[exLst[[1]]]*Power[exLst[[1]],exLst[[2]]]*Dab[exLst[[2]],diffVarName,shift];
If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::Power retVal = ", retVal];)];
Return[retVal];
)
];

If[ToString[exHead]== "SumAll",
(
If[DebugSymbolicDab,
(
Print[StringShift[shift],"Dab::Processing SumAll..."];
Print[StringShift[shift],"expr = ", expr];
Print[StringShift[shift],"exLen = ", exLen];
Print[StringShift[shift],"exLst = ", exLst];
)
];

If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::Calling Dab..."];)];
elemDab=Expand[Dab[exLst[[1]],diffVarName,shift]];

If[elemDab ==0,
(
If[DebugSymbolicDab,(Print[StringShift[shift],"elemDab = ", elemDab];)];
Return[0];
)
];

elemDabLen=Length[elemDab];
elemDabHead=Head[elemDab];
elemDabLst=Apply[List,elemDab];
elemDabNewLst=Table[Indeterminate,{ii,1,elemDabLen}];
ind=exLst[[2]];

If[DebugSymbolicDab,
(
Print[StringShift[shift],"Dab::SumAll elemDab = ", elemDab];
Print[StringShift[shift],"Dab::SumAll elemDabLen = ", elemDabLen];
Print[StringShift[shift],"Dab::SumAll elemDabHead = ", elemDabHead];
Print[StringShift[shift],"Dab::SumAll elemDabLst = ", elemDabLst];
Print[StringShift[shift],"Dab::SumAll ind = ", ind];
Print[StringShift[shift],"Dab::SumAll elemDabNewLst = ", elemDabNewLst];
)];

If[ToString[elemDabHead]!="Plus",
(
If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::SumAll:: Creating fake Plus..."];)];
elemDabHead="Plus";
elemDabLen=1;
elemDabLst={elemDab};
elemDabNewLst=Table[Indeterminate,{ii,1,elemDabLen}];

If[DebugSymbolicDab,
(
Print[StringShift[shift],"Dab::SumAll updated elemDab = ", elemDab];
Print[StringShift[shift],"Dab::SumAll updated elemDabLen = ", elemDabLen];
Print[StringShift[shift],"Dab::SumAll updated elemDabHead = ", elemDabHead];
Print[StringShift[shift],"Dab::SumAll updated elemDabLst = ", elemDabLst];
Print[StringShift[shift],"Dab::SumAll updated ind = ", ind];
Print[StringShift[shift],"Dab::SumAll updated elemDabNewLst = ", elemDabNewLst];
)];
)
];

If[ToString[elemDabHead]== "Plus",
(
If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::SumAll:: Post processing Plus..."];)];

For[ii=1,ii <= elemDabLen, ii++,
(
elemDabPart=elemDabLst[[ii]];
partLen=Length[elemDabPart];
partHead=Head[elemDabPart];
partLst=Apply[List,elemDabPart];
partLstNew=partLst;

If[DebugSymbolicDab,
(
Print[StringShift[shift],sep];
Print[StringShift[shift],"Dab::SumAll::ii = ", ii, ", elemDabPart = ", elemDabPart];
Print[StringShift[shift],"Dab::SumAll partLen = ", partLen];
Print[StringShift[shift],"Dab::SumAll partHead = ", partHead];
Print[StringShift[shift],"Dab::SumAll partLst = ", partLst];
)
];

If[ToString[partHead]!="Times",
(
Print[StringShift[shift],sep];
Print[StringShift[shift],"Dab::SumAll::Plus post processing was expecting to see Times inside but got: ", elemDabPart];
Print[StringShift[shift],"expr = ", expr];
Print[StringShift[shift],"exLen = ", exLen];
Print[StringShift[shift],"exLst = ", exLst];
Print[StringShift[shift],"exHead = ", exHead];
Print[StringShift[shift],"diffVarName = ", diffVarName];
Print[StringShift[shift],"antiSymVal = ", antiSymVal];
Print[StringShift[shift],"symVal = ", symVal];
Print[StringShift[shift],"isConstant = ", isConstant];
Print[StringShift[shift],"Dab::SumAll elemDab = ", elemDab];
Print[StringShift[shift],"Dab::SumAll elemDabLen = ", elemDabLen];
Print[StringShift[shift],"Dab::SumAll elemDabHead = ", elemDabHead];
Print[StringShift[shift],"Dab::SumAll elemDabLst = ", elemDabLst];
Print[StringShift[shift],"Dab::SumAll ind = ", ind];
Print[StringShift[shift],"Dab::SumAll elemDabNewLst = ", elemDabNewLst];

Return[Indeterminate];
)
];

kdRule={};
indNew=ind;

For[jj=1,jj <= partLen,jj++,
(
If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::SumAll::jj = ", jj, ", partLst[[",jj,"]] = ", partLst[[jj]], ", head = ", Head[partLst[[jj]]]]; )];

If[ToString[Head[partLst[[jj]]]]=="KroneckerDelta",
(
kdLst=Apply[List,partLst[[jj]]];

If[DebugSymbolicDab,
(
Print[StringShift[shift],"We got KroneckerDelta!!!"];
Print[StringShift[shift],"Dab::SumAll::kdLst = ", kdLst];
)
];

If[Length[Intersection[{kdLst[[2]]},indNew]] ==0,
(
If[DebugSymbolicDab,
(Print[StringShift[shift],"Dab::SumAll::Found kdLst does not have common indicies with indNew. Igonring."]; )
];

dummy=1;
),
(
If[DebugSymbolicDab,
(Print[StringShift[shift],"Dab::SumAll::Found kdLst has common indicies with ind. Processing."]; )];
partLstNew[[jj]]=1;
kdRuleElem={kdLst[[2]] -> kdLst[[1]]};
If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::SumAll::kdRuleElem = ", kdRuleElem];)];
indNew=Complement[indNew,{kdLst[[2]]}];
kdRule=Join[kdRule,kdRuleElem];
)
];
)
];
)
];

If[DebugSymbolicDab,
(
Print[StringShift[shift],"Dab::SumAll indNew = ", indNew];
Print[StringShift[shift],"Dab::SumAll kdRule = ", kdRule];
Print[StringShift[shift],"Dab::SumAll partLstNew = ", partLstNew];
)
];

If[ToString[partHead]=="Times",
(
elemDabPartNew=Apply[Times,partLstNew] /. kdRule;
),
(
Print[StringShift[shift],"Dab::SumAll:: Cannot process (yet) partHead != Times."];
elemDabPartNew=Indeterminate;
)
];

If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::SumAll elemDabPartNew = ", elemDabPartNew]; )];

If[Length[indNew] ==0 ,
(
If[DebugSymbolicDab,(Print[StringShift[shift],"indNew has zero length ==> No sum."]; )];
elemDabNewLst[[ii]]=elemDabPartNew;

),
(
If[DebugSymbolicDab,(Print[StringShift[shift],"indNew has length > 0 ==> There will be sum."];)];
elemDabNewLst[[ii]]=SummmAll[elemDabPartNew,indNew];
)
];

If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::SumAll elemDabNewLst = ", elemDabNewLst]; )];

)
];

),
(
Print[StringShift[shift],"Dab::SumAll:: We actually expect plus here but did not receive it."];
Return[Indeterminate];
)
];

If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::SumAll FINAL elemDabNewLst = ", elemDabNewLst]; )];
retVal=Apply[Plus,elemDabNewLst];
If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::SumAll retVal = ", retVal]; )];
Return[retVal];
)
];

If[ToString[exHead]== diffVarName,
(
If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::Processing ",diffVarName,"..."]; )];
If[Length[exLst]==2 && ToString[Head[exLst]] == "List",
(
If[symVal && antiSymVal,
(
Print[StringShift[shift],"Dab::Error::",diffVarName," has both Symmetric and AntiSymmetric options set."];
Return[Indeterminate];
)
];

If[symVal,
(
If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::Processing Symmetric ",diffVarName,"..."]; )];
retVal=(KroneckerDelta[exLst[[1]],A]*KroneckerDelta[exLst[[2]],B]+KroneckerDelta[exLst[[1]],B]*KroneckerDelta[exLst[[2]],A]);

If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::",diffVarName," retVal = ", retVal]; )];
Return[retVal];
)
];

If[antiSymVal,
(
If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::Processing AntiSymmetric ",diffVarName,"..."]; )];
retVal=(KroneckerDelta[exLst[[1]],A]*KroneckerDelta[exLst[[2]],B]-KroneckerDelta[exLst[[1]],B]*KroneckerDelta[exLst[[2]],A]);

If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::",diffVarName," retVal = ", retVal]; )];
Return[retVal];
)
];

If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::Processing ",diffVarName," with no symmetry..."]; )];

retVal=(KroneckerDelta[exLst[[1]],A]*KroneckerDelta[exLst[[2]],B]);

If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::",diffVarName," retVal = ", retVal]; )];
Return[retVal];
),
(
Print[StringShift[shift],"Dab expects to see ",diffVarName,"[i,j] but got ", rawExpr , " instead."];
Return[Indeterminate];
)
];
),
(
If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::Processing derivative of ", exHead," by ",diffVarName,"..."]; )];
retVal=0;

If[DebugSymbolicDab,(Print[StringShift[shift],"Dab::",diffVarName," retVal = ", retVal]; )];
Return[retVal];
)
];

(* If we got here then we don't know what to do *)
Print[StringShift[shift],"Dab:: cannot determine what to do!"];
Return[Indeterminate];
];

(* ============================================= *)
