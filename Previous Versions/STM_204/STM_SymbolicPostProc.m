(* :Author:Konstantin K.Konstantinov *)
(* :Summary: Post processing of symbolic transformation. *)
(* :Copyright: K^3, 2013 - 2014 *)
(* :Version:Revision: 2.00.001,Date: 2014/03/05 *)
(* :Mathematica Version:7.0-9.0 *)
(* ==============================================*)

DebugProcessRuleValue=False;
DebugProcessRules=False;

(* ==============================================*)

ProcessRuleValue[rule_?StringQ]:=Module[{retVal,ruleVal,len,mlen,tokenLst,ii,IdxLst,IdxNumLst,IdxCountLst,IdxCountLen,IdxCountValLst,maxCount,IdxCountOrderedLst,tokenHeadLst,tokenNewHeadLst,IdxCountElem,pos,expr,tokenIdx,exprLst,IndexIdx,IdxLstPos,signExpr,exprStr,exprAllStr,doTranspose,doSum,exprHlpAllStr,exprHlpName,firstIdx,lastIdx,exprName,sumIdx,doTable,tableIdx,firstIdxStr,lastIdxStr,errVal},

ruleVal=StringReplace[rule, {"x" -> "a","y" -> "b"}];
errVal={rule,"Sum" <> rule <> " = Indeterminate;",""};

If[DebugProcessRuleValue,
(
Print["ProcessRuleValue::Starting..."];
Print["ruleVal = ", ruleVal];
)
];

len=StringLength[ruleVal];
If[Mod[len,3]!=0,
(
Print["ProcessRuleValue::Incorrect length: ", len];
Return[errVal];
)
];

mlen=len/3;
tokenLst=Table[StringTake[ruleVal,{1+(ii-1)*3,ii*3}],{ii,1,mlen}];
tokenHeadLst=Table[StringTake[ruleVal,{1+(ii-1)*3}],{ii,1,mlen}];
tokenNewHeadLst=Table[StringReplace[tokenHeadLst[[ii]],{"S" -> "sab","A" -> "aab","W" -> "wab"}],{ii,1,mlen}];

IdxLst=Table[{ToExpression[StringTake[tokenLst[[ii]],{2}]],ToExpression[StringTake[tokenLst[[ii]],{3}]]},{ii,1,mlen}];

IdxNumLst=DeleteCases[IdxLst,a,Infinity];
IdxNumLst=DeleteCases[IdxNumLst,b,Infinity];
IdxCountValLst=Table[Count[IdxLst,ii,Infinity],{ii,1,mlen+1}];
IdxCountValLst=Sort[DeleteCases[IdxCountValLst,0]];
maxCount=Max[IdxCountValLst];

IdxCountLst=Table[{If[ii==1,a,If[ii==2,b,ii-2]],Count[IdxLst,If[ii==1,a,If[ii==2,b,ii-2]],Infinity]},{ii,1,mlen+3}];
IdxCountLst=SortBy[DeleteCases[IdxCountLst,{_,0}],Last];
IdxCountOrderedLst=SortBy[IdxCountLst,First];
IdxCountLen=Length[IdxCountLst];

If[!((mlen == IdxCountLen) || (mlen == (IdxCountLen-1))),
(
Print["ProcessRuleValue::mlen = ", mlen, ", IdxCountLen = ", IdxCountLen, ",  - cannot process."];
Return[errVal];
)
];

If[DebugProcessRuleValue,
(
Print["mlen = ", mlen];
Print["tokenLst = ", tokenLst];
Print["tokenHeadLst = ", tokenHeadLst];
Print["tokenNewHeadLst = ", tokenNewHeadLst];
Print["IdxLst = ", IdxLst];
Print["IdxNumLst = ", IdxNumLst];
Print["IdxCountLen = ", IdxCountLen];
Print["IdxCountLst = ", IdxCountLst];
Print["IdxCountOrderedLst = ", IdxCountOrderedLst];
Print["IdxCountValLst = ", IdxCountValLst];
Print["maxCount = ", maxCount];
)
];

If[maxCount>2,
(
Print["Cannot process maxCount = ", maxCount];
Return[errVal];
)
];

If[DebugProcessRuleValue,
(
Print[sep];
)
];

exprLst={};
exprAllStr="";
doTranspose=False;
doSum=False;
doTable=False;

ii=1;
IdxCountElem= IdxCountLst[[ii]][[1]];
signExpr=1;
firstIdx=IdxCountElem;

If[DebugProcessRuleValue,
(
Print["ii = ",ii ];
Print["IdxCountLst[[", ii,"]] = ", IdxCountLst[[ii]] ];
Print["IdxCountElem = ",IdxCountElem ];
Print["IdxLst = ",IdxLst ];
Print["tokenHeadLst = ",tokenHeadLst ];
Print["tokenNewHeadLst = ",tokenNewHeadLst ];
Print["firstIdx = ",firstIdx ];
)
];

For[ii=2,ii<=(mlen+1),ii++,
(
pos=Position[IdxLst,IdxCountElem];
tokenIdx=pos[[1]][[1]];
IndexIdx=pos[[1]][[2]];
IdxLstPos=Extract[IdxLst,tokenIdx]; 
If[IndexIdx==2 && tokenHeadLst[[tokenIdx]]== "A",signExpr*=(-1)];
expr=tokenNewHeadLst[[tokenIdx]] <> "[[" <> ToString[IdxLst[[tokenIdx,IndexIdx]]] <> ", " <> ToString[IdxLst[[tokenIdx,3-IndexIdx]]]<> "]]";

exprLst=Join[exprLst,{expr}];
exprStr=tokenNewHeadLst[[tokenIdx]];

If[ii<(mlen+1),exprStr =exprStr <> " . "];
exprAllStr = exprAllStr <>exprStr;

IdxCountElem=IdxLst[[tokenIdx,3-IndexIdx]];

tokenHeadLst=Delete[tokenHeadLst,tokenIdx];
tokenNewHeadLst=Delete[tokenNewHeadLst,tokenIdx];
IdxLst=Delete[IdxLst,tokenIdx];

(*
If[(mlen == (IdxCountLen-1)),
(
If[ii<(mlen+1),lastIdx=IdxCountElem];
),
(
If[ii<=(mlen+1),lastIdx=IdxCountElem];
)
];
*)
lastIdx=IdxCountElem;

If[DebugProcessRuleValue,
(
Print[sep];
Print["ii = ",ii ];
Print["pos = ",pos ];
Print["tokenIdx = ",tokenIdx ];
Print["IndexIdx = ",IndexIdx ];
Print["IdxLstPos = ",IdxLstPos ];
Print["expr = ",expr ];
Print["..."];
Print["signExpr = ",signExpr ];
Print["IdxCountElem = ",IdxCountElem ];
Print["IdxLst = ",IdxLst ];
Print["tokenHeadLst = ",tokenHeadLst ];
Print["tokenNewHeadLst = ",tokenNewHeadLst ];
Print["exprLst = ",exprLst ];
Print["exprStr = ",exprStr ];
Print["exprAllStr = ",exprAllStr ];
Print["current lastIdx = ",lastIdx ];
)
];
)
];

If[DebugProcessRuleValue,
(
Print[sep];
Print["firstIdx = ",firstIdx];
Print["lastIdx = ",lastIdx];
Print["NumericQ[firstIdx] = ",NumericQ[firstIdx]];
Print["NumericQ[lastIdx] = ",NumericQ[lastIdx]];
)
];

sumIdx="";
tableIdx="";

If[(!NumericQ[firstIdx]) && (!NumericQ[lastIdx]),
(
If[DebugProcessRuleValue,
(
Print["Both are symbols."];
)
];

If[firstIdx==b && lastIdx==a,doTranspose=True];
firstIdxStr=ToString[firstIdx];
lastIdxStr=ToString[lastIdx];
),
doSum=True;
If[(NumericQ[firstIdx]) && (!NumericQ[lastIdx]),
(
If[DebugProcessRuleValue,(Print["First is number."];)];

doTable=True;
firstIdxStr="i" <> ToString[firstIdx];
lastIdxStr=ToString[lastIdx];
sumIdx="{" <> firstIdxStr <> ", 1, NNN}";
tableIdx="{" <> lastIdxStr <> ", 1, NNN}";
),
(
If[(!NumericQ[firstIdx]) && (NumericQ[lastIdx]),
(
If[DebugProcessRuleValue,(Print["Second is number."];)];

doTable=True;
firstIdxStr=ToString[firstIdx];
lastIdxStr="i" <>ToString[lastIdx];
sumIdx="{" <> lastIdxStr <> ", 1, NNN}";
tableIdx="{" <> firstIdxStr <> ", 1, NNN}";
),
(
firstIdxStr="i" <>ToString[firstIdx];
lastIdxStr="i" <>ToString[lastIdx];

If[firstIdx==lastIdx,
(
If[DebugProcessRuleValue,(Print["Two equal numbers."];)];
sumIdx="{" <> firstIdxStr <> ", 1, NNN}";
),
(
If[DebugProcessRuleValue,(Print["Two different numbers."];)];
sumIdx="{" <> firstIdxStr <> ", 1, NNN}, {" <> lastIdxStr <> ", 1, NNN}";
)
];
)
];
)
];
];

If[DebugProcessRuleValue,
(
Print["doSum = ",doSum ];
Print["doTable = ",doTable ];
Print["doTranspose = ",doTranspose ];
Print["sumIdx = ",sumIdx ];
Print["tableIdx = ",tableIdx ];
Print["firstIdxStr = ",firstIdxStr ];
Print["lastIdxStr = ",lastIdxStr ];
)
];

exprHlpAllStr="";
exprName="Sum" <> rule;
exprAllStr=If[signExpr > 0,"","-"] <> exprAllStr;

If[!doSum,
(
If[!doTranspose,
(
exprAllStr=exprName  <> " = "<>exprAllStr <> ";";
),
(
exprAllStr=exprName  <> " = Transpose["<>exprAllStr <> "];";
)
];
),
(
exprHlpName=exprName  <> "Hlp";
exprHlpAllStr=exprHlpName <>" = "<>exprAllStr <> ";";
exprAllStr="Sum[" <> exprHlpName <> "[[" <>firstIdxStr <> ", " <> lastIdxStr <> "]], " <> sumIdx <> "]";

If[doTable,
(
exprAllStr="Table[" <> exprAllStr <>", " <> tableIdx <> "]";
)
];
exprAllStr=exprName <> " = " <> exprAllStr <> ";";
)
];

retVal={rule,exprAllStr, exprHlpAllStr};

If[DebugProcessRuleValue,
(
Print["exprHlpAllStr = ",exprHlpAllStr ];
Print["exprAllStr = ",exprAllStr ];
Print["retVal = ",retVal ];
Print[sep];
)
];

Return[retVal];
];

(* ==============================================*)
ProcessRules[ruleLst_?VectorQ]:=Module[{len,retVal,ii,rule,ruleLen,ruleVal,ruleValLen,ruleValHead,doProcess,ruleStr,totalCount,ruleExpr,ruleExprLst},
len=Length[ruleLst];

If[DebugProcessRules,(
Print["ProcessRules::Starting..."];
Print["len = ", len];
)
];

totalCount=0;
ruleExprLst={};

For[ii=1,ii <=len,ii++,
(
doProcess=False;
rule=ruleLst[[ii]];
ruleLen=Length[rule];
ruleVal=rule[[2]];
ruleValLen=Length[ruleVal];
ruleValHead=ToString[Head[ruleVal]];

If[ruleValHead=="Symbol",
(
ruleStr=ToString[ruleVal];
If[StringTake[ruleStr,3]=="Sum",
(
doProcess=True;
ruleStr=StringTake[ruleStr,{4,StringLength[ruleStr]}];
)
];
),
If[ruleValHead=="Part",
(
ruleStr=ToString[ruleVal[[1]]];
If[StringTake[ruleStr,3]=="Sum",
(
doProcess=True;
ruleStr=StringTake[ruleStr,{4,StringLength[ruleStr]}];
)
];
)
];
];

If[doProcess,
(
totalCount++;
If[DebugProcessRules,(
Print[sep];
Print["ii = ", ii];
Print["rule = ", rule];
Print["ruleLen = ", ruleLen];
Print["ruleVal = ", FullForm[ruleVal]];
Print["ruleValLen = ", ruleValLen];
Print["ruleValHead = ",ruleValHead];
Print["ruleStr = ",ruleStr];
)
];

ruleExpr=ProcessRuleValue[ruleStr];
ruleExprLst=Join[ruleExprLst,{ruleExpr}];
)
];
)
];

If[DebugProcessRules,(
Print["ruleExprLst = ",ruleExprLst // MatrixForm];
)
];

Print["ProcessRules::Total number of rules = ", totalCount];

Return[ruleExprLst];
];

(* ==============================================*)




