(* :Author:Konstantin K.Konstantinov *)
(* :Summary: Various symbolic transformation (trailer). *)
(* :Copyright: K^3, 2013 - 2014 *)
(* :Version:Revision: 2.00.001,Date: 2014/03/05 *)
(* :Mathematica Version:7.0-9.0 *)
(* ==============================================*)

(* TODO Fix the issue below. *)
Print["!!! Symbolic transformations should NOT be used along with the models !!!"];
Print["TODO: Definition of SummmAll. Must be done AFTER expressions are created. Fix that!!!"];

SummmAll[exprVal_,indVar_?VectorQ]:=Module[{retVal,indLen,exHead,exLen,exLst,containsList,exNoInd,(* exInd, *) newIndRule,exIndLen,exIndLst,indElemTest,ii,jj,kk,indElemCompare,commonInd,hasCommonIndex,exIndShort,newExpr,newInd,part1,part2,newIndRuleCanonical,(* exIndCan, *) indCan,countTbl,exLstHlp,rankList,newIndRuleCanonical2,exLstHlpCmp,exIndCanCmp,indSwapRule,rankListCmp,cmpResult,newExprLst,multiplier,ind,exSumAll,exSumAllCorr,hasSummAllCnt,hasSummAllLst,summAllExprLst,sumAllIndLst,sumAllIndRule,exIndCnt,expr,doProcessTimes,exprPowerLst,exIndCanLst,spl,splInd,splNoInd},

If[DebugSymbolic,
(
Print["  "];
Print[sep];
Print[sep];
Print["SummmAll::Starting..."];
Print["exprVal = ", exprVal];
)
];

expr=CanonicOrdering[Expand[exprVal]];
ind=indVar;
indLen=Length[ind];
exHead=Head[expr];
idxSummmAll++;

newIndRule=Table[ind[[ii]] -> ToExpression["i" <>ToString[idxSummmAll]<>"c"<> ToString[ii]],{ii,1,indLen}];

exLst=Apply[List,expr];
exLen=Length[exLst];
multiplier=1;

If[DebugSymbolic,
(
Print["expr = ", expr];
Print["exHead = ", exHead];
Print["ind = ", ind];
Print["exLst = ", exLst];
)
];

If[ToString[exHead]== "Plus",
(
(* Plus commutes with SummmAll, so we swap them *)
If[DebugSymbolic,(Print["Processing Plus."];)];

newExprLst=Table[SummmAll[exLst[[ii]],ind],{ii,1,exLen}];
newExpr=Apply[Plus,newExprLst];

If[DebugSymbolic,
(
Print["newExprLst = ", newExprLst]; 
Print["newExpr = ", newExpr];
)
];

Return[newExpr];
)
];

(* If we got here then we have only singular terms (no Plus). *)

If[DebugSymbolic,Print["SummmAll::Getting rid of Power..."]];

doProcessTimes=False;

If[ToString[exHead]== "Power",
(
If[DebugSymbolic,Print["Processing single Power and replacing it by Times."]];

doProcessTimes=True;
exprPowerLst=Apply[List,expr];
If[DebugSymbolic,Print["exprPowerLst = ", exprPowerLst]];

If[IntegerQ[exprPowerLst[[2]]],
(
exLst=Table[exprPowerLst[[1]]^Sign[exprPowerLst[[2]]],{ii,1,Abs[exprPowerLst[[2]]]}];
exLen=Length[exLst];
exHead="Times";

If[DebugSymbolic,
(
Print["SummmAll::After single Power updated:"];
Print["exHead = ", exHead];
Print["ind = ", ind];
Print["exLst = ", exLst];
)
];
),
(
Print["SummmAll::Cannot process non integer powers: ",exprPowerLst[[2]]];
Return[Indeterminate];
)
];
),
(
If[ToString[exHead]== "Times",
(
If[DebugSymbolic,Print["There is no single Power. Checking deeper expressions in Times."]];

For[ii=1,ii <= exLen,ii++,
(
If[ToString[Head[exLst[[ii]]]]=="Power",
(
If[DebugSymbolic,Print["Level 2 Power found. Replacing by Times."]];
exprPowerLst=Apply[List,exLst[[ii]]];

If[DebugSymbolic,
(
Print["exLst[[",ii,"]] = ", exLst[[ii]]];
Print["exprPowerLst = ", exprPowerLst];
)
];

If[IntegerQ[exprPowerLst[[2]]],
(
exLst[[ii]]=Table[exprPowerLst[[1]]^Sign[exprPowerLst[[2]]],{ii,1,Abs[exprPowerLst[[2]]]}];
),
(
If[DebugSymbolic,(Print["SummmAll::Non integer power found."];)];
If[IgnoreNonIntegerPower,
(
If[DebugSymbolic,(Print["SummmAll::Non integer power IGNORED."];)];
),
(
Print["SummmAll::Cannot process non integer powers: ",exprPowerLst[[2]]];
Return[Indeterminate];
)
];
)
];
)
];
)
];

exLst=Flatten[exLst];
exLen=Length[exLst];
),
(
If[DebugSymbolic,
(
Print["!!! Possible error !!! "];
Print["expr = ", expr];
Print["exHead = ", exHead];
Print["ind = ", ind];
Print["exLst = ", exLst];
)
];
)
];
)
];

If[DebugSymbolic,
(
Print["After Power processing..."];
Print["exLst = ", exLst];
Print["exLen = ", exLen];
)
];

containsList=Table[MemberQ[exLst,ind[[jj]],Infinity],{jj,1,indLen}];

If[DebugSymbolic,
(
Print["Checking for redundant indicies (which are not used in the expression)."];
Print["containsList = ", containsList];
)
];

For[kk=indLen,kk >= 1,kk--,
(
(* Print["kk = ", kk]; *)
If[!containsList[[kk]],
(
If[DebugSymbolic,Print["Index ", ind[[kk]], " is redundant. Replacing it by NNN."]];
ind=Delete[ind,kk];
multiplier*=NNN;
)
];
)
];

(* Resetting the length of ind in case we deleted some unused indicies. *)
indLen=Length[ind];

If[DebugSymbolic,
(
Print["ind = ", ind];
Print["multiplier = ", multiplier];
)
];

If[ToString[exHead]== "Times",
(
If[DebugSymbolic,
(
Print["Checking if expression could be split into parts."];
Print["exLst (before split processing) = ", exLst];
Print["ind (before split processing = ", ind];
)
];

spl=SplitIndexedExpression[exLst,ind];
splNoInd=GetIndExprNoInd[spl];
splInd=GetIndExprPairs[spl];

If[DebugSymbolic,
(
Print["spl = ", spl];
Print["splNoInd = ", splNoInd];
Print["splInd = ", splInd];
)
];

If[Length[splInd]>1,
(
If[DebugSymbolic,
(
Print["Expression CAN be split into parts. Restarting."];
)
];

If[Length[splNoInd] >=1,
(
exNoInd=Apply[Times,splNoInd];
),
(
exNoInd=1;
)
];

If[DebugSymbolic,
(
Print["exNoInd = ", exNoInd];
)
];

retVal=multiplier*exNoInd*Apply[Times,Table[Apply[SummmAll,{Apply[Times,splInd[[ii]][[1]]],splInd[[ii]][[2]]}],{ii,1,Length[splInd]}]];
If[DebugSymbolic,
(
Print["retVal = ", retVal];
)
];

Return[retVal];
)
];

If[DebugSymbolic,
(
Print["Expression CANNOT be split into parts."];
)
];

doProcessTimes=True;
containsList=Table[Apply[Or,Table[MemberQ[exLst[[ii]],ind[[jj]],Infinity],{jj,1,indLen}]],{ii,1,exLen}];
exNoInd=Apply[Times,Table[If[containsList[[ii]],1,exLst[[ii]]],{ii,1,exLen}]];

exIndLst=Table[If[!containsList[[ii]],1,exLst[[ii]]],{ii,1,exLen}];
If[DebugSymbolic,Print["exIndLst (BEFORE deleting 1) = ", exIndLst]];
exIndLst=Delete[exIndLst,Position[exIndLst,1]];
If[DebugSymbolic,Print["exIndLst (AFTER deleting 1) = ", exIndLst]];

exIndLen=Length[exIndLst];
exIndCnt=Apply[Plus,Table[If[containsList[[ii]],1,0],{ii,1,exLen}]];

If[DebugSymbolic,
(
Print["Processing Times / first step."];
Print["exLst = ", exLst];
Print["ind = ", ind];
Print["containsList = ", containsList];
Print["exNoInd = ", exNoInd];
Print["exIndLst = ", exIndLst];
Print["exIndLen = ", exIndLen];
)
];
)
];

If[doProcessTimes,
(
If[DebugSymbolic,
(
Print["Processing Times / second step."];
Print["exNoInd = ", exNoInd];
Print["exIndLst = ", exIndLst];
Print["exIndLen = ", exIndLen];
)
];

If[DebugSymbolic,Print["Checking for SumAll in Times expression."]];
hasSummAllLst=Table[If[ToString[Head[exIndLst[[ii]]]]=="SumAll" || ToString[Head[exIndLst[[ii]]]]=="SummmAll",1,0],{ii,1,exIndLen}];
hasSummAllCnt=Sum[hasSummAllLst[[jj]],{jj,1,exIndLen}];

If[DebugSymbolic,
(
Print["hasSummAllLst = ", hasSummAllLst];
Print["hasSummAllCnt = ", hasSummAllCnt];
)
];

If[ hasSummAllCnt > 0,
(
If[DebugSymbolic,(Print["Processing SumAll(s) in Times expression."];)];

summAllExprLst={};
sumAllIndLst={};
For[ii=1,ii <= exIndLen,ii++,
(
If[ToString[Head[exIndLst[[ii]]]]=="SumAll" || ToString[Head[exIndLst[[ii]]]]=="SummmAll",
(
If[DebugSymbolic,(Print["SumAll || SummmAl for ii = ", ii];)];
exSumAll=Apply[List,exIndLst[[ii]]];
sumAllIndRule=Table[exSumAll[[2,jj]] -> ToExpression[ToString[exSumAll[[2,jj]]] <> "Idx" <> ToString[ii]],{jj,1,Length[exSumAll[[2]]]}];
summAllExprLst=Join [summAllExprLst,{exSumAll[[1]] /. sumAllIndRule}];
sumAllIndLst=Join [sumAllIndLst,exSumAll[[2]] /. sumAllIndRule];
),
(
If[DebugSymbolic,(Print["NOT (SumAll || SummmAl) for ii = ", ii];)];
summAllExprLst=Join [summAllExprLst,{exIndLst[[ii]]}];
)
];
)
];

exSumAllCorr={Apply[Times,summAllExprLst],Join[sumAllIndLst,ind]};

If[DebugSymbolic,
(
Print["summAllExprLst = ", summAllExprLst];
Print["sumAllIndLst = ", sumAllIndLst];
Print["exSumAllCorr = ", exSumAllCorr];
)
];

retVal=multiplier*exNoInd*Apply[SummmAll,exSumAllCorr];

If[DebugSymbolic,
(
Print["exSumAll = ", exSumAll];
Print["exSumAllCorr = ", exSumAllCorr];
Print["retVal = ", retVal];
)
];

Return[retVal];
)
];

If[Length[ind]==0,
(
retVal=exNoInd*multiplier;

If[DebugSymbolic,
(
Print["No indicies left."];
Print["retVal = ", retVal];
)
];

Return[retVal];
)
];

If[exIndLen > 1,
(
If[DebugSymbolic,
(
Print["......"];
Print["Checking absense of mutual indicies."];
Print["exIndLen = ", exIndLen];
)
];

For[ii=1,ii<= exIndLen,ii++,
(
If[DebugSymbolic,
(
Print["..."];
Print["Checking ii = ", ii, ", element = ", exIndLst[[ii]]];
)
];

hasCommonIndex=False;
indElemTest=Flatten[Table[If[MemberQ[exIndLst[[ii]],ind[[jj]],Infinity],{ind[[jj]]},{}],{jj,1,indLen}]];
If[DebugSymbolic,( Print["indElemTest = ", indElemTest]; )];

For[kk=1,kk<= exIndLen,kk++,
(
If[kk !=ii,
(
indElemCompare=Flatten[Table[If[MemberQ[exIndLst[[kk]],ind[[jj]],Infinity],{ind[[jj]]},{}],{jj,1,indLen}]];
commonInd=Intersection[indElemTest,indElemCompare];

If[DebugSymbolic,
(
Print["... element to compare = ", exIndLst[[kk]]];
Print["indElemCompare = ", indElemCompare];
Print["commonInd = ", commonInd];
)
];

If[Length[commonInd]>0,(hasCommonIndex=True; Break;)];
)
];
)
];

If[DebugSymbolic,(Print["hasCommonIndex = ", hasCommonIndex];)];

If[!hasCommonIndex,
(
exIndShort=Delete[exIndLst,ii];
newInd=Complement[ind,indElemTest];

If[DebugSymbolic,
(
Print["No common index found ==> can take out of the sum."];
Print["exIndShort = ", exIndShort];
Print["newInd = ", newInd];
Print["Calling SummmAll..."];
)
];

part1=SummmAll[exIndLst[[ii]],indElemTest];
If[DebugSymbolic,(Print["part1 = ", part1]; )];

part2=SummmAll[Apply[Times,exIndShort],newInd];
If[DebugSymbolic,(Print["part2 = ", part2]; )];

newExpr=multiplier*exNoInd*part1*part2; 
If[DebugSymbolic,(Print["newExpr = ", newExpr]; )];
Return[newExpr];
)
];
)
];
)
];

(* If we got here then the expression is already in a canonical order. *)
(* Which means that we have non separable sums. *)
(* newIndRuleCanonical=CanonicalOrderRule[exIndLst,ind]; *)
newIndRuleCanonical=CanonicalOrderPermutationRule[exIndLst,ind];
exIndCanLst=exIndLst/. newIndRuleCanonical;
indCan=Sort[ind/. newIndRuleCanonical];

If[DebugSymbolic,
(
Print["Processing expression in canonical order. "];
Print["newIndRule = ", newIndRule];
Print["newIndRuleCanonical = ", newIndRuleCanonical];
Print["exIndCanLst = ", exIndCanLst];
Print["indCan = ", indCan];
Print["Test if the there at least two indicies and if there are, then test that these first two indicies have the same number of occurencies and try to swap them if they are."];
)
];

exLstHlp=exIndCanLst;
countTbl=Table[GetNoOfOccurencies[exLstHlp,ind[[ii]]],{ii,1,indLen}];
rankList=GetExpressionRankList[exLstHlp,ind];

If[DebugSymbolic,
(
Print["exLstHlp = ", exLstHlp];
Print["countTbl = ", countTbl];
Print["rankList = ", rankList];
)
];

If[Length[countTbl] >=2 && Length[indCan] >= 2,
(
If[countTbl[[1]]==countTbl[[2]],
(
indSwapRule=Table[If[ii==1,indCan[[1]]-> indCan[[2]],If[ii==2,indCan[[2]]-> indCan[[1]],indCan[[ii]] -> indCan[[ii]]]],{ii,1,indLen}];
exIndCanCmp=exIndCanLst/.indSwapRule;
exLstHlpCmp=Sort[Apply[List,exIndCanCmp]];
rankListCmp=GetExpressionRankList[exLstHlpCmp,ind];
cmpResult=CompareRankLists[rankList,rankListCmp];

If[DebugSymbolic,
(
Print["Two first indicies are the same? Attempt to swap them. "];
Print["indSwapRule = ", indSwapRule];
Print["exIndCanCmp = ", exIndCanCmp];
Print["exLstHlpCmp = ", exLstHlpCmp];
Print["rankListCmp = ", rankListCmp];
Print["cmpResult = ", cmpResult];
)
];

If[cmpResult >0,
(
(* Print["Swapping is better."]; *)
exIndCanLst=exIndCanCmp;
)
];
)
];
)
];

If[DebugSymbolic,Print["TODO::Check::retVal=(exNoInd*SumAll[Apply[Times,exIndCanLst],indCan])"]];
retVal=(exNoInd*SumAll[Apply[Times,exIndCanLst],indCan]) ;
),
(
If[DebugSymbolic,(Print["Nothing to do. Replacing by SumAll..."];)];
If[Length[ind]==0,
(
If[DebugSymbolic,
(
Print["expr = ", expr];
)
];
retVal=expr;
),
(
retVal=SumAll[expr,ind] /. newIndRule;
If[DebugSymbolic,
(
Print["multiplier = ", multiplier];
Print["expr = ", expr];
Print["ind = ", ind];
Print["newIndRule = ", newIndRule];
Print["retVal = ", retVal];
)
];
)
];
)
];

If[DebugSymbolic,
(
Print["retVal = ", retVal];
Print[sep];
)
];

Return[multiplier*retVal];
];

(* ============================================= *)
Print["Select\:0443\:0432 SumF5Rules."];

SumF5Rules:={SumAll[s[i1, i2]*s[i1, i3]*s[i2, i4]*s[i3, i5]*s[i4, i5], {i1, i2, i3, i4, i5}] -> SumS12S13S24S35S45,SumAll[s[i1, i2]*s[i1, i3]*s[i2, i4]*s[i3, i5]*s[i4, i6], {i1, i2, i3, i4, i5, i6}] -> SumS12S13S24S35S46 ,SumAll[s[i1, i2]*s[i1, i3]*s[i2, i4]*s[i5, i6]*s[i5, i7], {i1, i2, i3, i4, i5, i6, i7}] -> SummmAll[s[i1, i2]*s[i1, i3]*s[i2, i4], {i1, i2, i3, i4}]*SummmAll[s[i5, i6]*s[i5, i7], {i5, i6, i7}],SumAll[s[x_, i1]*s[y_, i2]*s[i1, i3]*s[i2, i3], {i1, i2, i3}] -> SumSx1Sy2S13S23[[x,y]],
SumAll[s[x_, i1]*s[y_, i2]*s[i1, i3]*s[i2, i4], {i1, i2, i3, i4}] -> SummmAll[s[x, i1]*s[i1, i3], {i1, i3}]*SummmAll[s[y, i2]*s[i2, i4], {i2,i4}],
SumAll[s[x_, i1]*s[i1, i2]*s[i2, i3]*s[i3, i4], {i1, i2, i3, i4}] -> SumSx1S12S23S34[[x]],
SumAll[s[x_, i1]*s[i1, i2]*s[i3, i4]*s[i3, i5], {i1, i2, i3, i4, i5}] -> SummmAll[s[x, i1]*s[i1, i2], {i1, i2}]*SummmAll[s[i3, i4]*s[i3, i5], {i3, i4, i5}]};

(* ============================================= *)
Print["SumF4Rules - WILL RESULT IN WARNINGS !!!"];
(* TODO: Some of the rules are about the same object. Implement in SummmAll by additional sorting by the powers of coefficients. *)

(* TODO: These rules should be moved into SummmAll *)
Print["sumRuleS4Index6 - these rules implement separation of group of variables like in SumAll[s[i1, i2]*s[i1, i3]*s[i4, i5]*s[i4, i6], {i1, i2, i3, i4, i5, i6}]."];

sumRuleS4Index6={SumAll[s[i1, i2]*s[i1, i3]*s[i4, i5]*s[i4, i6], {i1, i2, i3, i4, i5, i6}] -> SummmAll[s[i1, i2]*s[i1, i3], {i1, i2, i3}]*SummmAll[s[i4, i5]*s[i4, i6], {i4, i5, i6}]};

sumRuleS4Index5={SumAll[s[i1, i2]*s[i1, i3]*s[i2, i4]*s[i3, i5], {i1, i2, i3, i4, i5}] -> SumSp4,SumAll[s[i1, i2]*s[i1, i3]*s[i1, i4]*s[i1, i5], {i1, i2, i3, i4, i5}] -> SumS12S13S14S15,SumAll[s[i1, i2]*s[i1, i3]*s[i1, i4]*s[i2, i5], {i1, i2, i3, i4, i5}] -> SumS12S13S14S25};

sumRuleS4Index4v1={SumAll[s[i1, i2]*s[i1, i3]^2*s[i1, i4], {i1, i2, i3, i4}] -> SumS12p2S13S14,SumAll[s[i1, i2]*s[i1, i3]*s[i1, i4]^2, {i1, i2, i3, i4}] -> SumS12p2S13S14, SumAll[s[i1, i2]^2*s[i1, i3]*s[i1, i4], {i1, i2, i3, i4}]-> SumS12p2S13S14};

sumRuleS4Index4v2={SumAll[s[i1, i2]^2*s[i1, i3]*s[i2, i4], {i1, i2, i3, i4}] -> SumS12p2S13S24};

sumRuleS4Index4v3={SumAll[s[i1, i2]*s[i1, i3]^2*s[i2, i4], {i1, i2, i3, i4}] -> SumS12p2S23S34,SumAll[s[i1, i2]*s[i1, i3]*s[i2, i4]^2, {i1, i2, i3, i4}] -> SumS12p2S23S34};

sumRuleS4Index4v4={SumAll[s[i1, i2]*s[i1, i3]*s[i1, i4]*s[i2, i3], {i1, i2, i3, i4}] -> SumS12S13S14S23,
SumAll[s[i1, i2]*s[i1, i3]*s[i2, i4]*s[i3, i4], {i1, i2, i3, i4}] -> SumS12S13S24S34};

sumRuleS4Index3v1={SumAll[s[i1, i2]^3*s[i1, i3], {i1, i2, i3}] -> SumS3mS,SumAll[s[i1, i2]*s[i1, i3]^3, {i1, i2, i3}] -> SumS3mS,SumAll[s[i1, i2]^2*s[i1, i3]^2, {i1, i2, i3}] -> SumS2mS2};

sumRuleS4Index3v2={SumAll[s[i1, i2]^2*s[i1, i3]*s[i2, i3], {i1, i2, i3}] -> SumS12p2S13S23,SumAll[s[i1, i2]*s[i1, i3]^2*s[i2, i3], {i1, i2, i3}] -> SumS12p2S13S23,SumAll[s[i1, i2]*s[i1, i3]*s[i2, i3]^2, {i1, i2, i3}] -> SumS12p2S13S23};

sumRuleS4Index2v1={SumAll[s[i1, i2]^4, {i1, i2}] -> SumS12p4};

sumRuleS4Index1={};

SumF4Rules=Flatten[{sumRuleS4Index6,sumRuleS4Index5,sumRuleS4Index4v1,sumRuleS4Index4v2,sumRuleS4Index4v3,sumRuleS4Index4v4,sumRuleS4Index3v1,sumRuleS4Index3v2,sumRuleS4Index2v1,sumRuleS4Index1}];

(* Print["SumF4Rules = ", SumF4Rules]; *)
Print["Length[SumF4Rules] = ", Length[SumF4Rules]];
Print[sep];

(* ============================================= *)

Print["SumRules - WILL RESULT IN WARNINGS !!!"];
sumRuleSii={SumAll[s[i1,i2]^3,{i1,i2}]-> SumS12p3,SumAll[s[i1,i2]^2,{i1,i2}]->SumS12p2,SumAll[s[i1,i2],{i1,i2}] ->SumS12};

sum4indSiiRule={SumAll[s[i1,i2] s[i1,i3] s[i1,i4],{i1,i2,i3,i4}] -> SumS12S13S14,SumAll[s[i1,i2] s[i1,i3] s[i2,i4],{i1,i2,i3,i4}] -> SumS12S13S24};

sum3indSiiRule={SumAll[s[i1,i2] s[i1,i3] s[i2,i3],{i1,i2,i3}] -> SumS12S13S23,SumAll[s[i1,i2] s[i1,i3],{i1,i2,i3}] -> SumS12S13};

sum3indSiiRule2={SumAll[s[i1,i2]^2 s[i1,i3],{i1,i2,i3}] -> SumS12p2S13,SumAll[s[i1,i2] s[i1,i3]^2,{i1,i2,i3}] -> SumS12p2S13};

sumRuleSxi={SumAll[s[x_,i1]^3,{i1}]-> SumSx1p3[[x]],SumAll[s[x_,i1]^2,{i1}] -> SumSx1p2[[x]],SumAll[s[x_,i1],{i1}] -> SumSx1[[x]]};

sum3indSxiRule={SumAll[s[x_,i1] s[i1,i2] s[i2,i3],{i1,i2,i3}] -> SumSx1S12S23[[x]],SumAll[s[x_,i1] s[i1,i2] s[i1,i3],{i1,i2,i3}] -> SumSx1S12S13[[x]]};

sum2indSxiRule={SumAll[s[x_,i1]^2 s[i1,i2],{i1,i2}]-> SumSx1p2S12[[x]],SumAll[s[x_,i1] s[i1,i2]^2,{i1,i2}] -> SumSx1S12p2[[x]],SumAll[s[x_,i1] s[i1,i2],{i1,i2}] -> SumSx1S12[[x]],SumAll[s[x_,i1] s[y_,i2] s[i1,i2],{i1,i2}] -> SumSx1Sy2S12[[x,y]],SumAll[s[x_,i1] s[y_,i1] s[i1,i2],{i1,i2}] ->SumSx1Sy1S12[[x,y]]};

sum1indSxyRule={SumAll[s[x_,i1]^2 s[y_,i1],{i1}] -> SumSx1p2Sy1[[x,y]],SumAll[s[x_,i1] s[y_,i1]^2,{i1}] ->  SumSx1p2Sy1[[y,x]],SumAll[s[x_,i1] s[y_,i1],{i1}] -> SumSx1Sy1[[x,y]]};
sabRule={s[a,b] -> sab[[a,b]],s[A,B] -> sab[[A,B]],s[c,d]-> sab[[c,d]],s[a,c]-> sab[[a,c]],s[a,d]-> sab[[a,d]],s[b,c]-> sab[[b,c]],s[b,d]-> sab[[b,d]]};

ABabRule={A -> a, B -> b};
abABrule={a -> A, b -> B};

ABcdRule={A -> c, B -> d};

(* ============================================= *)

SumSRules=Flatten[{SumF5Rules, SumF4Rules,sumRuleSii,sum4indSiiRule,sum3indSiiRule,sum3indSiiRule2,sumRuleSxi,sum3indSxiRule,sum2indSxiRule,sum1indSxyRule,sabRule}];
(* Print["SumSRules = ", SumSRules]; *)
Print["Length[SumSRules] = ", Length[SumSRules]];
Print[sep];

SumAllImpl[expr_,indVar_?VectorQ,nn_?IntegerQ]:=Module[{retVal,indLen,indSum,exprSum},
indLen=Length[indVar];
indSum=Table[{indVar[[ii]],1,nn},{ii,1,indLen}];
exprSum=Join[{expr},indSum];
retVal=Apply[Sum,exprSum];
Return[retVal];
];

(* ============================================= *)

Print["SumWRules - WILL RESULT IN WARNINGS !!!"];

sumRuleWii={SumAll[w[i1,i2]^3,{i1,i2}]-> SumW12p3,SumAll[w[i1,i2]^2,{i1,i2}]->SumW12p2,SumAll[w[i1,i2],{i1,i2}] ->SumW12,SumAll[w[i2,i1],{i1,i2}] ->SumW12,SumAll[w[i1, i2]*w[i2, i1], {i1, i2}] -> SumW12W21,SumAll[w[i1, i2]*w[i3, i1], {i1, i2, i3}] -> SumW12W31};

sum6indWiiRule={SumAll[w[i1, i2]*w[i3, i1]*w[i4, i3]*w[i5, i4]*w[i6, i5], {i1, i2, i3, i4, i5, i6}] -> SumW12SW31W43W54W65};

sum5indWiiRule={SumAll[w[i1, i2]*w[i3, i1]*w[i4, i3]*w[i5, i4], {i1, i2, i3, i4, i5}] -> SumW12W31W43W54,SumAll[w[i1, i2]*w[i2, i3]*w[i3, i4]*w[i4, i5]*w[i5, i1], {i1, i2, i3, i4, i5}] -> SumW12W23W34W45W51};

sum4indWiiRule={SumAll[w[i1,i2] w[i1,i3] w[i1,i4],{i1,i2,i3,i4}] -> SumW12W13W14,SumAll[w[i1,i2] w[i1,i3] w[i2,i4],{i1,i2,i3,i4}] -> SumW12W13W24,SumAll[w[i1, i2]*w[i3, i1]*w[i4, i3], {i1, i2, i3, i4}] -> SumW12W31W43,
SumAll[w[i1, i2]*w[i2, i3]*w[i3, i4]*w[i4, i1], {i1, i2, i3, i4}] ->SumW12W23W34W41};

sum3indWiiRule={SumAll[w[i1,i2] w[i1,i3] w[i2,i3],{i1,i2,i3}] -> SumW12W13W23,SumAll[w[i1,i2] w[i1,i3],{i1,i2,i3}] -> SumW12W13,SumAll[w[i1, i2]*w[i2, i3]*w[i3, i1], {i1, i2, i3}]-> SumW12W23W31};

(*
sum3indWiiRule2={SumAll[w[i1,i2]^2 w[i1,i3],{i1,i2,i3}] -> SumW12p2W13,SumAll[w[i1,i2] w[i1,i3]^2,{i1,i2,i3}] -> SumW12p2W13};
*)

sumRuleWxi={SumAll[w[xx_,i1]^3,{i1}]-> SumWx1p3[[xx]],SumAll[w[xx_,i1]^2,{i1}] -> SumWx1p2[[xx]],SumAll[w[xx_,i1],{i1}] -> SumWx1[[xx]],SumAll[w[i1, xx_], {i1}]->  SumW1x[[xx]]};

sum4indWxiRule={SumAll[w[xx_, i1]*w[i1, i2]*w[i2, i3]*w[i3, i4], {i1, i2, i3, i4}]-> SumWx1W12W23W34[[xx]],SumAll[w[i1, xx_]*w[i2, i1]*w[i3, i2]*w[i4, i3], {i1, i2, i3, i4}]-> SumW1xW21W32W43[[xx]]};

sum3indWxiRule={SumAll[w[xx_,i1] w[i1,i2] w[i2,i3],{i1,i2,i3}] -> SumWx1W12W23[[xx]],SumAll[w[xx_,i1] w[i1,i2] w[i1,i3],{i1,i2,i3}] -> SumWx1W12W13[[xx]],SumAll[w[i1, xx_]*w[i2, i1]*w[i3, i2], {i1, i2, i3}] -> SumW1xW21W32[[xx]]};

sum2indWxiRule={SumAll[w[xx_,i1]^2 w[i1,i2],{i1,i2}]-> SumWx1p2W12[[xx]],SumAll[w[xx_,i1] w[i1,i2]^2,{i1,i2}] -> SumWx1W12p2[[xx]],SumAll[w[xx_,i1] w[i1,i2],{i1,i2}] -> SumWx1W12[[xx]],SumAll[w[i1, xx_]*w[i2, i1], {i1, i2}] -> SumW1xW21[[xx]]};

sum3indWxyRule={SumAll[w[yy_, i1]*w[i1, i2]*w[i2, i3]*w[i3, xx_], {i1, i2, i3}] ->SumWy1W12W23W3x[[xx,yy]] }

sum2indWxyRule={SumAll[w[yy_, i1]*w[i1, i2]*w[i2, xx_], {i1, i2}] ->  SumWy1W12W2x[[xx,yy]]};

sum1indWxyRule={SumAll[w[xx_,i1] w[yy_,i2] w[i1,i2],{i1,i2}] -> SumWx1Wy2W12[[xx,yy]],SumAll[w[xx_,i1] w[yy_,i1] w[i1,i2],{i1,i2}] ->SumWx1Wy1W12[[xx,yy]],SumAll[w[xx_,i1]^2 w[yy_,i1],{i1}] -> SumWx1p2Wy1[[xx,yy]],SumAll[w[xx_,i1] w[yy_,i1]^2,{i1}] ->  SumWx1p2Wy1[[yy,xx]],SumAll[w[xx_,i1] w[yy_,i1],{i1}] -> SumWx1Wy1[[xx,yy]],SumAll[w[yy_, i1]*w[i1, xx_], {i1}]-> SumWy1W1x[[xx,yy]]};
wabRule={w[a,b] -> wab[[a,b]],w[b,a] -> wab[[b,a]],w[a,c]-> wab[[a,c]],w[c,a]-> wab[[c,a]],w[a,d]-> wab[[a,d]],w[d,a] -> wab[[d,a]],w[b,c]-> wab[[b,c]],w[c,b]-> wab[[c,b]],w[b,d]-> wab[[b,d]],w[d,b]-> wab[[d,b]],w[c,d]-> wab[[c,d]],w[d,c]-> wab[[d,c]],w[A,B] -> wab[[A,B]]};

(* ============================================= *)

SumWRules=Flatten[{sumRuleWii,sum6indWiiRule,sum5indWiiRule,sum4indWiiRule,sum3indWiiRule (* ,sum3indWiiRule2 *),sumRuleWxi,sum4indWxiRule,sum3indWxiRule,sum2indWxiRule,sum3indWxyRule,sum2indWxyRule,sum1indWxyRule,wabRule}];
(* Print["SumWRules = ", SumWRules]; *)
Print["Length[SumWRules] = ", Length[SumWRules]];
Print[sep];

(* ============================================= *)

sumRuleAii={SumAll[\[Alpha][i1, i2], {i1, i2}] -> 0,SumAll[\[Alpha][i1, i2]^2, {i1, i2}] -> SumA12A12};

sum6indAiiRule={SumAll[s[i1, i2]*s[i1, i3]*s[i4, i5]*s[i4, i6]*\[Alpha][i2, i5], {i1, i2, i3, i4, i5, i6}] -> SumS12S13S45S46A25,SumAll[s[i1, i2]*s[i3, i4]*s[i5, i6]*\[Alpha][i1, i3]*\[Alpha][i2, i5], {i1, i2, i3, i4, i5, i6}]-> SumS12S34S56A13A25,
SumAll[s[i1, i2]*s[i1, i3]*s[i4, i5]*\[Alpha][i2, i6]*\[Alpha][i3, i4], {i1, i2, i3, i4, i5, i6}]-> SumS12S13S45A26A34,
SumAll[s[i1,i2]*s[i3,i4]*\[Alpha][i1,i5]*\[Alpha][i2,i6]*\[Alpha][i3,i5],{i1,i2,i3,i4,i5,i6}]->SumS12S34A15A26A35,SumAll[s[i1,i2]*s[i1,i3]*s[i4,i5]*\[Alpha][i2,i4]*\[Alpha][i3,i6],{i1,i2,i3,i4,i5,i6}]->SumS12S13S45A24A36,SumAll[s[i1,i2]*s[i3,i4]*\[Alpha][i1,i5]*\[Alpha][i2,i6]*\[Alpha][i3,i6],{i1,i2,i3,i4,i5,i6}]->SumS12S34A15A26A36,SumAll[s[i1,i2]*s[i3,i4]*\[Alpha][i1,i3]*\[Alpha][i2,i5]*\[Alpha][i4,i6],{i1,i2,i3,i4,i5,i6}]->SumS12S34A13A25A46,SumAll[s[i1,i2]*s[i1,i3]*s[i4,i5]*\[Alpha][i2,i6]*\[Alpha][i4,i6],{i1,i2,i3,i4,i5,i6}]->SumS12S13S45A26A46,SumAll[s[i1,i2]*s[i1,i3]*s[i2,i4]*\[Alpha][i3,i5]*\[Alpha][i4,i6],{i1,i2,i3,i4,i5,i6}]->SumS12S13S24A35A46,SumAll[s[i1,i2]*s[i1,i3]*\[Alpha][i2,i4]*\[Alpha][i3,i5]*\[Alpha][i4,i6],{i1,i2,i3,i4,i5,i6}]->SumS12S13A24A35A46,SumAll[s[i1,i2]*s[i1,i3]*s[i4,i5]*\[Alpha][i2,i4]*\[Alpha][i5,i6],{i1,i2,i3,i4,i5,i6}]->SumS12S13S45A24A56,SumAll[s[i1,i2]*s[i3,i4]*\[Alpha][i1,i3]*\[Alpha][i2,i5]*\[Alpha][i5,i6],{i1,i2,i3,i4,i5,i6}]->SumS12S34A13A25A56,SumAll[s[i1,i2]*\[Alpha][i1,i3]*\[Alpha][i2,i5]*\[Alpha][i3,i4]*\[Alpha][i5,i6],{i1,i2,i3,i4,i5,i6}]->SumS12A13A25A34A56,SumAll[s[i1,i2]*s[i1,i3]*s[i2,i4]*\[Alpha][i3,i5]*\[Alpha][i5,i6],{i1,i2,i3,i4,i5,i6}]->SumS12S13S24A35A56,SumAll[s[i1,i2]*s[i1,i3]*\[Alpha][i2,i4]*\[Alpha][i3,i5]*\[Alpha][i5,i6],{i1,i2,i3,i4,i5,i6}]->SumS12S13A24A35A56,SumAll[s[i1,i2]*\[Alpha][i1,i3]*\[Alpha][i2,i4]*\[Alpha][i3,i5]*\[Alpha][i5,i6],{i1,i2,i3,i4,i5,i6}]->SumS12A13A24A35A56,SumAll[s[i1,i2]*s[i3,i4]*\[Alpha][i1,i5]*\[Alpha][i3,i6]*\[Alpha][i5,i6],{i1,i2,i3,i4,i5,i6}]->SumS12S34A15A36A56,SumAll[s[i1,i2]*s[i3,i4]*\[Alpha][i1,i3]*\[Alpha][i4,i5]*\[Alpha][i5,i6],{i1,i2,i3,i4,i5,i6}]->SumS12S34A13A45A56,SumAll[s[i1,i2]*\[Alpha][i1,i3]*\[Alpha][i2,i4]*\[Alpha][i4,i5]*\[Alpha][i5,i6],{i1,i2,i3,i4,i5,i6}]->SumS12A13A24A45A56,SumAll[\[Alpha][i1,i2]*\[Alpha][i1,i3]*\[Alpha][i2,i4]*\[Alpha][i4,i5]*\[Alpha][i5,i6],{i1,i2,i3,i4,i5,i6}]->SumA12A13A24A45A56,SumAll[s[i1,i2]*\[Alpha][i1,i3]*\[Alpha][i3,i4]*\[Alpha][i4,i5]*\[Alpha][i5,i6],{i1,i2,i3,i4,i5,i6}]->SumS12A13A34A45A56}

sum5indAiiRule={SumAll[s[i1, i2]*s[i3, i4]*\[Alpha][i1, i3]*\[Alpha][i2, i5], {i1, i2, i3, i4, i5}] -> SumS12S34A13A25,SumAll[s[i1, i2]*s[i3, i4]*\[Alpha][i1, i5]*\[Alpha][i3, i5], {i1, i2, i3, i4, i5}] -> SumS12S34A15A35,SumAll[s[i1, i2]*s[i1, i3]*\[Alpha][i2, i4]*\[Alpha][i3, i5], {i1, i2, i3, i4, i5}] -> SumS12S13A24A35,SumAll[s[i1, i2]*\[Alpha][i1, i3]*\[Alpha][i2, i4]*\[Alpha][i3, i5], {i1, i2, i3, i4, i5}] -> SumS12A13A24A35,SumAll[s[i1, i2]*s[i3, i4]*\[Alpha][i1, i3]*\[Alpha][i4, i5], {i1, i2, i3, i4, i5}] -> SumS12S34A13A45,SumAll[s[i1, i2]*s[i1, i3]*\[Alpha][i2, i4]*\[Alpha][i4, i5], {i1, i2, i3, i4, i5}]  -> SumS12S13A24A45,SumAll[s[i1, i2]*\[Alpha][i1, i3]*\[Alpha][i2, i4]*\[Alpha][i4, i5], {i1, i2, i3, i4, i5}] -> SumS12A13A24A45,
SumAll[\[Alpha][i1, i2]*\[Alpha][i1, i3]*\[Alpha][i2, i4]*\[Alpha][i4, i5], {i1, i2, i3, i4, i5}] -> SumA12A13A24A45,SumAll[s[i1, i2]*s[i1, i3]*s[i4, i5]*\[Alpha][i2, i4]*\[Alpha][i3, i5], {i1, i2, i3, i4, i5}]-> SumS12S13S45A24A35,
SumAll[s[i1,i2]*s[i1,i3]*s[i2,i4]*s[i3,i5]*\[Alpha][i4,i5],{i1,i2,i3,i4,i5}]->SumS12S13S24S35A45,SumAll[s[i1,i2]*s[i3,i4]*\[Alpha][i1,i3]*\[Alpha][i2,i5]*\[Alpha][i4,i5],{i1,i2,i3,i4,i5}]->SumS12S34A13A25A45,SumAll[s[i1,i2]*\[Alpha][i1,i3]*\[Alpha][i2,i5]*\[Alpha][i3,i4]*\[Alpha][i4,i5],{i1,i2,i3,i4,i5}]->SumS12A13A25A34A45,SumAll[s[i1,i2]*s[i1,i3]*s[i2,i4]*\[Alpha][i3,i5]*\[Alpha][i4,i5],{i1,i2,i3,i4,i5}]->SumS12S13S24A35A45,SumAll[s[i1,i2]*s[i1,i3]*\[Alpha][i2,i4]*\[Alpha][i3,i5]*\[Alpha][i4,i5],{i1,i2,i3,i4,i5}]->SumS12S13A24A35A45,SumAll[s[i1,i2]*\[Alpha][i1,i3]*\[Alpha][i2,i4]*\[Alpha][i3,i5]*\[Alpha][i4,i5],{i1,i2,i3,i4,i5}]->SumS12A13A24A35A45,SumAll[\[Alpha][i1,i2]*\[Alpha][i1,i3]*\[Alpha][i2,i4]*\[Alpha][i3,i5]*\[Alpha][i4,i5],{i1,i2,i3,i4,i5}]->SumA12A13A24A35A45};

sum4indAiiRule={SumAll[s[i1, i2]*s[i3, i4]*\[Alpha][i1, i3], {i1, i2, i3, i4}]  -> SumS12S34A13,SumAll[s[i1, i2]*\[Alpha][i1, i3]*\[Alpha][i2, i4], {i1, i2, i3, i4}]  -> SumS12A13A24,SumAll[\[Alpha][i1, i2]*\[Alpha][i1, i3]*\[Alpha][i2, i4], {i1, i2, i3, i4}]  ->SumA12A13A24,
SumAll[s[i1, i2]*\[Alpha][i1, i3]*\[Alpha][i3, i4], {i1, i2, i3, i4}] -> SumS12A13A34,SumAll[s[i1, i2]*s[i3, i4]*\[Alpha][i1, i3]*\[Alpha][i2, i4], {i1, i2, i3, i4}] ->SumS12S34A13A24,SumAll[s[i1, i2]*s[i1, i3]*s[i2, i4]*\[Alpha][i3, i4], {i1, i2, i3, i4}] -> SumS12S13S24A34,SumAll[s[i1, i2]*s[i1, i3]*\[Alpha][i2, i4]*\[Alpha][i3, i4], {i1, i2, i3, i4}] -> SumS12S13A24A34,SumAll[\[Alpha][i1, i2]*\[Alpha][i1, i3]*\[Alpha][i2, i4]*\[Alpha][i3, i4], {i1, i2, i3, i4}] -> SumA12A13A24A34};

sum3indAiiRule={SumAll[\[Alpha][i1, i2]*\[Alpha][i1, i3], {i1, i2, i3}] -> SumA12A13,SumAll[s[i1, i2]*s[i1, i3]*\[Alpha][i2, i3], {i1, i2, i3}] -> SumS12S13A23,SumAll[s[i1, i2]*\[Alpha][i1, i3]*\[Alpha][i2, i3], {i1, i2, i3}] -> SumS12A13A23,SumAll[\[Alpha][i1, i2]*\[Alpha][i1, i3]*\[Alpha][i2, i3], {i1, i2, i3}] ->SumA12A13A23 };

sum4indAxiRule={SumAll[s[i1,i2]*s[i3,i4]*\[Alpha][xx_,i1]*\[Alpha][i2,i3],{i1,i2,i3,i4}]->SumS12S34Ax1A23[[xx]],SumAll[s[i1,i2]*s[i3,i4]*\[Alpha][yy_,i1]*\[Alpha][i2,i3],{i1,i2,i3,i4}]->SumS12S34Ay1A23[[yy]],SumAll[s[i1,i2]*s[i1,i3]*\[Alpha][xx_,i4]*\[Alpha][i2,i4],{i1,i2,i3,i4}]->SumS12S13Ax4A24[[xx]],SumAll[s[i1,i2]*s[i1,i3]*\[Alpha][yy_,i4]*\[Alpha][i2,i4],{i1,i2,i3,i4}]->SumS12S13Ay4A24[[yy]],SumAll[s[xx_,i1]*s[i2,i3]*\[Alpha][i1,i4]*\[Alpha][i2,i4],{i1,i2,i3,i4}]->SumSx1S23A14A24[[xx]],SumAll[s[yy_,i1]*s[i2,i3]*\[Alpha][i1,i4]*\[Alpha][i2,i4],{i1,i2,i3,i4}]->SumSy1S23A14A24[[yy]],SumAll[s[i1,i2]*s[i1,i3]*\[Alpha][xx_,i2]*\[Alpha][i3,i4],{i1,i2,i3,i4}]->SumS12S13Ax2A34[[xx]],SumAll[s[i1,i2]*s[i1,i3]*\[Alpha][yy_,i2]*\[Alpha][i3,i4],{i1,i2,i3,i4}]->SumS12S13Ay2A34[[yy]],SumAll[s[xx_,i1]*s[i2,i3]*\[Alpha][i1,i2]*\[Alpha][i3,i4],{i1,i2,i3,i4}]->SumSx1S23A12A34[[xx]],SumAll[s[yy_,i1]*s[i2,i3]*\[Alpha][i1,i2]*\[Alpha][i3,i4],{i1,i2,i3,i4}]->SumSy1S23A12A34[[yy]],SumAll[s[xx_,i1]*s[i1,i2]*\[Alpha][i2,i3]*\[Alpha][i3,i4],{i1,i2,i3,i4}]->SumSx1S12A23A34[[xx]],SumAll[s[yy_,i1]*s[i1,i2]*\[Alpha][i2,i3]*\[Alpha][i3,i4],{i1,i2,i3,i4}]->SumSy1S12A23A34[[yy]],SumAll[\[Alpha][xx_,i1]*\[Alpha][i1,i2]*\[Alpha][i2,i3]*\[Alpha][i3,i4],{i1,i2,i3,i4}]->SumAx1A12A23A34[[xx]],SumAll[\[Alpha][yy_,i1]*\[Alpha][i1,i2]*\[Alpha][i2,i3]*\[Alpha][i3,i4],{i1,i2,i3,i4}]->SumAy1A12A23A34[[yy]],SumAll[s[i1,i2]*s[i1,i3]*s[i2,i4]*\[Alpha][xx_,i3],{i1,i2,i3,i4}]->SumS12S13S24Ax3[[xx]],SumAll[s[i1,i2]*s[i1,i3]*s[i2,i4]*\[Alpha][yy_,i3],{i1,i2,i3,i4}]->SumS12S13S24Ay3[[yy]],SumAll[s[xx_,i1]*s[i2,i3]*s[i2,i4]*\[Alpha][i1,i3],{i1,i2,i3,i4}]->SumSx1S23S24A13[[xx]],SumAll[s[yy_,i1]*s[i2,i3]*s[i2,i4]*\[Alpha][i1,i3],{i1,i2,i3,i4}]->SumSy1S23S24A13[[yy]],SumAll[s[xx_,i1]*s[i1,i2]*s[i3,i4]*\[Alpha][i2,i3],{i1,i2,i3,i4}]->SumSx1S12S34A23[[xx]],SumAll[s[yy_,i1]*s[i1,i2]*s[i3,i4]*\[Alpha][i2,i3],{i1,i2,i3,i4}]->SumSy1S12S34A23[[yy]],SumAll[s[i1,i2]*\[Alpha][xx_,i3]*\[Alpha][i1,i3]*\[Alpha][i2,i4],{i1,i2,i3,i4}]->SumS12Ax3A13A24[[xx]],SumAll[s[i1,i2]*\[Alpha][yy_,i3]*\[Alpha][i1,i3]*\[Alpha][i2,i4],{i1,i2,i3,i4}]->SumS12Ay3A13A24[[yy]],SumAll[s[xx_,i1]*s[i1,i2]*s[i2,i3]*\[Alpha][i3,i4],{i1,i2,i3,i4}]->SumSx1S12S23A34[[xx]],SumAll[s[yy_,i1]*s[i1,i2]*s[i2,i3]*\[Alpha][i3,i4],{i1,i2,i3,i4}]->SumSy1S12S23A34[[yy]],SumAll[s[i1,i2]*\[Alpha][xx_,i3]*\[Alpha][i1,i4]*\[Alpha][i3,i4],{i1,i2,i3,i4}]->SumS12Ax3A14A34[[xx]],SumAll[s[i1,i2]*\[Alpha][yy_,i3]*\[Alpha][i1,i4]*\[Alpha][i3,i4],{i1,i2,i3,i4}]->SumS12Ay3A14A34[[yy]],SumAll[s[i1,i2]*\[Alpha][xx_,i1]*\[Alpha][i2,i3]*\[Alpha][i3,i4],{i1,i2,i3,i4}]->SumS12Ax1A23A34[[xx]],SumAll[s[i1,i2]*\[Alpha][yy_,i1]*\[Alpha][i2,i3]*\[Alpha][i3,i4],{i1,i2,i3,i4}]->SumS12Ay1A23A34[[yy]],SumAll[s[xx_,i1]*\[Alpha][i1,i2]*\[Alpha][i2,i3]*\[Alpha][i3,i4],{i1,i2,i3,i4}]->SumSx1A12A23A34[[xx]],SumAll[s[yy_,i1]*\[Alpha][i1,i2]*\[Alpha][i2,i3]*\[Alpha][i3,i4],{i1,i2,i3,i4}]->SumSy1A12A23A34[[yy]]};

sum3indAxiRule={SumAll[s[i1, i2]*\[Alpha][xx_, i3]*\[Alpha][i1, i3], {i1, i2, i3}] -> SumS12Ax3A13[[xx]],SumAll[s[i1, i2]*\[Alpha][xx_, i1]*\[Alpha][i2, i3], {i1, i2, i3}]-> SumS12Ax1A23[[xx]],SumAll[s[xx_, i1]*\[Alpha][i1, i2]*\[Alpha][i2, i3], {i1, i2, i3}]-> SumSx1A12A23[[xx]],SumAll[s[i1, i2]*s[i1, i3]*\[Alpha][xx_, i2], {i1, i2, i3}]->  SumS12S13Ax2[[xx]],SumAll[s[xx_, i1]*s[i2, i3]*\[Alpha][i1, i2], {i1, i2, i3}]->SumSx1S23A12[[xx]],SumAll[s[xx_, i1]*s[i1, i2]*\[Alpha][i2, i3], {i1, i2, i3}]->SumSx1S12A23[[xx]],SumAll[\[Alpha][xx_, i1]*\[Alpha][i1, i2]*\[Alpha][i2, i3], {i1, i2, i3}]->SumAx1A12A23[[xx]]};

sum2indAxiRule={SumAll[\[Alpha][xx_, i1]*\[Alpha][i1, i2], {i1, i2}] ->  SumAx1A12[[xx]],SumAll[s[i1, i2]*\[Alpha][xx_, i1], {i1, i2}] -> SumS12Ax1[[xx]],SumAll[s[xx_, i1]*\[Alpha][i1, i2], {i1, i2}] ->  SumSx1A12[[xx]]};

sumRuleAxi={SumAll[\[Alpha][xx_, i1], {i1}] -> SumAx1[[xx]]};

sum3indAxyRule={SumAll[s[i1,i2]*s[i1,i3]*\[Alpha][xx_,i2]*\[Alpha][yy_,i3],{i1,i2,i3}]->SumS12S13Ax2Ay3[[xx,yy]],SumAll[s[i1,yy_]*s[i2,i3]*\[Alpha][i1,i3]*\[Alpha][xx_,i2],{i1,i2,i3}]->SumS1yS23A13Ax2[[xx,yy]],SumAll[s[i1,xx_]*s[i2,i3]*\[Alpha][i1,i3]*\[Alpha][yy_,i2],{i1,i2,i3}]->SumS1xS23A13Ay2[[xx,yy]],SumAll[s[i1,i2]*s[i1,yy_]*\[Alpha][i2,i3]*\[Alpha][xx_,i3],{i1,i2,i3}]->SumS12S1yA23Ax3[[xx,yy]],SumAll[s[i1,i2]*s[i1,xx_]*\[Alpha][i2,i3]*\[Alpha][yy_,i3],{i1,i2,i3}]->SumS12S1xA23Ay3[[xx,yy]],SumAll[s[i1,xx_]*s[i2,yy_]*\[Alpha][i1,i3]*\[Alpha][i2,i3],{i1,i2,i3}]->SumS1xS2yA13A23[[xx,yy]],SumAll[\[Alpha][i1,i3]*\[Alpha][i2,i3]*\[Alpha][xx_,i1]*\[Alpha][yy_,i2],{i1,i2,i3}]->SumA13A23Ax1Ay2[[xx,yy]],SumAll[s[i1,i2]*s[i1,yy_]*s[i2,i3]*\[Alpha][xx_,i3],{i1,i2,i3}]->SumS12S1yS23Ax3[[xx,yy]],SumAll[s[i1,i2]*s[i1,xx_]*s[i2,i3]*\[Alpha][yy_,i3],{i1,i2,i3}]->SumS12S1xS23Ay3[[xx,yy]],SumAll[s[i1,xx_]*s[i2,i3]*s[i2,yy_]*\[Alpha][i1,i3],{i1,i2,i3}]->SumS1xS23S2yA13[[xx,yy]],SumAll[s[i1,i3]*s[i1,xx_]*s[i2,yy_]*\[Alpha][i2,i3],{i1,i2,i3}]->SumS13S1xS2yA23[[xx,yy]],SumAll[s[i1,i2]*\[Alpha][i2,i3]*\[Alpha][xx_,i3]*\[Alpha][yy_,i1],{i1,i2,i3}]->SumS12A23Ax3Ay1[[xx,yy]],SumAll[s[i1,i2]*\[Alpha][i2,i3]*\[Alpha][xx_,i1]*\[Alpha][yy_,i3],{i1,i2,i3}]->SumS12A23Ax1Ay3[[xx,yy]],SumAll[s[i1,yy_]*\[Alpha][i1,i3]*\[Alpha][i2,i3]*\[Alpha][xx_,i2],{i1,i2,i3}]->SumS1yA13A23Ax2[[xx,yy]],SumAll[s[i1,xx_]*\[Alpha][i1,i3]*\[Alpha][i2,i3]*\[Alpha][yy_,i2],{i1,i2,i3}]->SumS1xA13A23Ay2[[xx,yy]]};

sum2indAxyRule={SumAll[s[i1, i2]*\[Alpha][xx_, i1]*\[Alpha][yy_, i2], {i1, i2}] -> SumS12Ax1Ay2[[xx,yy]],SumAll[s[xx_, i1]*\[Alpha][yy_, i2]*\[Alpha][i1, i2], {i1, i2}]-> SumSx1Ay2A12[[xx,yy]],SumAll[s[xx_, i1]*s[i1, i2]*\[Alpha][yy_, i2], {i1, i2}]-> SumSx1S12Ay2[[xx,yy]],SumAll[s[xx_, i1]*s[yy_, i2]*\[Alpha][i1, i2], {i1, i2}]-> SumSx1Sy2A12[[xx,yy]],SumAll[\[Alpha][xx_, i1]*\[Alpha][yy_, i2]*\[Alpha][i1, i2], {i1, i2}]->SumAx1Ay2A12[[xx,yy]]};

sum1indAxyRule={SumAll[\[Alpha][xx_, i1]*\[Alpha][yy_, i1], {i1}] -> SumAx1Ay1[[xx,yy]],SumAll[s[xx_, i1]*\[Alpha][yy_, i1], {i1}]->  SumSx1Ay1[[xx,yy]]};

aabRuleGeneric={\[Alpha][xx_, yy_]-> aab[[xx,yy]]};
aabRule={\[Alpha][a,b] -> aab[[a,b]],\[Alpha][b,a] -> aab[[b,a]],\[Alpha][a,c]-> aab[[a,c]],\[Alpha][c,a]-> aab[[c,a]],\[Alpha][a,d]-> aab[[a,d]],\[Alpha][d,a] -> aab[[d,a]],\[Alpha][b,c]-> aab[[b,c]],\[Alpha][c,b]-> aab[[c,b]],\[Alpha][b,d]-> aab[[b,d]],\[Alpha][d,b]-> aab[[d,b]],\[Alpha][c,d]-> aab[[c,d]],\[Alpha][d,c]-> aab[[d,c]],\[Alpha][A,B] -> aab[[A,B]]};

SumARules=Flatten[{sumRuleAii,sum6indAiiRule,sum5indAiiRule,sum4indAiiRule,sum3indAiiRule,sum2indAxiRule,sumRuleAxi,sum4indAxiRule,sum3indAxiRule,sum3indAxyRule,sum2indAxyRule,sum1indAxyRule,aabRule}];
Print["Length[SumARules] = ", Length[SumARules]];
Print[sep];

(* ============================================= *)

Print["allRules"];
allRules=Join[SumSRules,SumARules,SumWRules,ABabRule,{SumAll -> SumAllCanonical}];
Print["Length[allRules] = ", Length[allRules]];
Print[sep];

(* ============================================= *)

