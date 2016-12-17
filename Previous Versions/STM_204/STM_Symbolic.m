(* :Author:Konstantin K.Konstantinov *)
(* :Summary: Various symbolic transformation. *)
(* :Copyright: K^3, 2013 - 2014 *)
(* :Version:Revision: 2.00.001,Date: 2014/03/05 *)
(* :Mathematica Version:7.0-9.0 *)
(* ==============================================*)
DoRemoveI=True;
(* ==============================================*)
DebugSymbolic = False;
DebugSymbolicDab=False;
DebugSymbolicLocal = False;
DebugCanonicOrdering=False;
IgnoreNonIntegerPower =True;
DebugRemoveI=False;
DebugApplyRules = False;
ApplyRulesDonNotSimplify=False;
UseParallelTable=True;
(* ============================================= *)
idxSummmAll=0;
(* ============================================= *)
Print["Symbolic transformations for sums with exclusions and related stuff. Should NOT be used along with the models."];
(* ============================================= *)
strSeparator="============================================="<>FromCharacterCode[10];
strSeparatorSmall="---------------------------------------------";
tStart=AbsoluteTime[];
tMid=tStart;
strCRLF=FromCharacterCode[10];

(* ============================================== *)

BooleanQ[x_]:=If[Element[x,Booleans],True,False,False];

(* ============================================== *)

PrintTimeUsed[showTime_]:=Module[{tEnd},tEnd=AbsoluteTime[];
If[showTime==True,Print["Time used: ",(tEnd-tMid),", total time used: ",(tEnd-tStart),FromCharacterCode[10]<>strSeparatorSmall],Print["Time used reset."],Print["Time used reset."]];
tMid=tEnd;];

PrintTimeUsed[]:=PrintTimeUsed[True];

(* ============================================= *)

sep=strSeparatorSmall;
strCRLF=FromCharacterCode[10];

(* ============================================= *)

Clear[s,a,b,c,d,e,f,g,i,j,h,k,l,m,n,sab,w,w0,\[CurlyPhi],\[Rho],\[Alpha],SumAll,x,y,Y,TrY,SumExcl,SummmAll,SumAllCanonical,A,B,ii,NNN,xx,yy,AntiSymmetric,Symmetric];

(* ============================================= *)

s[xx_,xx_]=0;
SetAttributes[s,Orderless];
Options[s]={AntiSymmetric -> False,Symmetric ->True};

\[Rho][xx_,xx_]=0;
(* SetAttributes[\[Rho],Orderless]; *)
Options[\[Rho]]={AntiSymmetric -> False,Symmetric ->True};

\[CurlyPhi][xx_,xx_]=0;
Options[\[CurlyPhi]]={AntiSymmetric -> True,Symmetric -> False};

\[Alpha][xx_,xx_]=0;
Options[\[Alpha]]={AntiSymmetric -> True,Symmetric -> False};

w[xx_,xx_]=0;
Options[w]={AntiSymmetric -> False,Symmetric ->False};

w0[xx_,yy_]=\[Rho][xx,yy]*Exp[I*\[CurlyPhi][xx,yy]];
Options[w0]={AntiSymmetric -> False,Symmetric ->False};

wTOw0Rule={w -> w0};
wTOsaRule={w[xx_,yy_]-> (s[xx,yy]+I*\[Alpha][xx,yy])};
wTOsRule={w -> s};
roRule={\[Rho][xx_,yy_] -> w[xx,yy]*Exp[-I*\[CurlyPhi][xx,yy]]};
sumAllRule={SumAll -> SummmAll};

AllFunctionsList={s,\[Rho],\[CurlyPhi],w,\[Alpha]};

(* ==============================================*)
Print["SumSabBYb and SumSabBYa."];
sumIdx=0;
SumSabBYb[a_]:=Module[{retVal,idxVar},
sumIdx++;
idxVar=ToExpression["b" <>ToString[sumIdx]];
retVal=SummmAll[w[a,idxVar],{idxVar}];
Return[retVal];
];

SumSabBYa[a_]:=Module[{retVal,idxVar},
sumIdx++;
idxVar=ToExpression["b" <>ToString[sumIdx]];
retVal=SummmAll[w[idxVar,a],{idxVar}];
Return[retVal];
];

(* Print["SumSabBYb[a] = ", SumSabBYb[a]]; *)
(* ============================================= *)
Print["TrY"];
TrY[]:=Module[{retVal,idxVar},
sumIdx++;
idxVar=ToExpression["b" <>ToString[sumIdx]];
retVal=(1/(2*NNN))*SummmAll[(SumSabBYb[idxVar]+SumSabBYa[idxVar])/2,{idxVar}];
Return[retVal];
];

(* Print["TrY = ", TrY[]]; *)
(* ============================================= *)
Print["XXTaaBYa and XXTaaBYb."];
XXTaaBYa[a_]:=(1/NNN)*(SumSabBYa[a]-TrY[]);
XXTaaBYb[a_]:=(1/NNN)*(SumSabBYb[a]-TrY[]);
(* Print["XXTaaBYa[a] = ", XXTaaBYa[a]]; *)
(* ============================================= *)
Print["y"];
y[a_,b_]:=Expand[((XXTaaBYb[a]+XXTaaBYa[b]-w[a,b])/2)];
(* Print["y[a,b] = ", y[a,b]]; *)
(* SetAttributes[y,Orderless]; *)

(* ============================================= *)
Print["MFuncYall - expression for moments."];
MFuncYall[level_?IntegerQ]:=MFuncYall[level,False];

MFuncYall[level_?IntegerQ,useParallelTable_?BooleanQ]:=Module[{retVal,idxLst,ii,yLst,Yk,yIdxLst,YkLen,YkLst,YkResLst},
idxLst=Table[ToExpression["a" <> ToString[ii]],{ii,1,level}];
yIdxLst=Table[{idxLst[[ii]],If[ii<level,idxLst[[ii+1]],idxLst[[1]]]},{ii,1,level}];
yLst=Table[y[idxLst[[ii]],If[ii<level,idxLst[[ii+1]],idxLst[[1]]]],{ii,1,level}];
Yk=Expand[Apply[Times,yLst]];

Print["MFuncYall::Length[Yk] = ", Length[Yk]]; 

If[!useParallelTable,
(
retVal=Expand[SummmAll[Yk,idxLst]];
),
(
Print["MFuncYall::Using ParallelTable.."]; 
YkLst=Apply[List,Yk];
YkLen=Length[YkLst];
DistributeDefinitions[YkLst,SummmAll];
YkResLst=ParallelTable[SummmAll[YkLst[[ii]],idxLst],{ii,1,YkLen}];
retVal=Apply[Plus,YkResLst];
)
];

If[DebugSymbolicLocal,
(
Print["MFuncYall::level = ", level]; 
Print["MFuncYall::idxLst = ", idxLst // MatrixForm]; 
Print["MFuncYall::yIdxLst = ", yIdxLst // MatrixForm];
Print["MFuncYall::yLst = ", yLst // MatrixForm];
Print["MFuncYall::retVal = ", retVal];
Print[sep];
)
];

Return[retVal];
];
(* ============================================= *)
GammaName[level_?IntegerQ]:=ToExpression["gamma" <>ToString[level]];
(* ============================================= *)


SummmAll[Times[numb_?IntegerQ,expr_],ind_?VectorQ]:=numb*SummmAll[expr,ind];
SummmAll[expr1_*SummmAll[expr_,ind1_?VectorQ],ind2_?VectorQ]:=SummmAll[expr1*expr,Sort[Join[ind1,ind2]]];
SummmAll[exprLst_Plus,ind_?VectorQ]:=Apply[Plus,Table[Apply[SummmAll,{exprLst[[idxSumAll]],ind}],{idxSumAll,1,Length[exprLst]}]];
SummmAll[SummmAll[expr_,ind1_?VectorQ],ind2_?VectorQ]:=SummmAll[expr,Sort[Join[ind1,ind2]]];
SummmAll[0,ind_?VectorQ]:=0;
(* SetAttributes[SummmAll,HoldAll]; *)

(* ==============================================*)

SumExcl[term_,ind_,excl_?VectorQ]:=Module[{retVal,len,exclND,termLen,ii,termHead,partHead,part,partLen,exclRuleList,exclRule,exclTermList,exclTerms,exclTermsZ},
exclND=DeleteDuplicates[excl];
len=Length[exclND];

exclRuleList=Table[{ind -> exclND[[ii]]},{ii,1,len}];
exclTermList=Table[term /.exclRuleList[[ii]],{ii,1,len}];
exclTerms=Apply[Plus,exclTermList];
exclTermsZ=exclTerms;

retVal=(Distribute[SummmAll[term,{ind}]]-exclTermsZ); 
Return[retVal];
];

(* ==============================================*)

SumAll[Times[numb_?IntegerQ,expr_],ind_?VectorQ]:=numb*SumAll[expr,ind];
SumAll[exprLst_Plus,ind_?VectorQ]:=Apply[Plus,Table[Apply[SumAll,{exprLst[[idxSumAll]],ind}],{idxSumAll,1,Length[exprLst]}]];
SumAll[SumAll[expr_,ind1_?VectorQ],ind2_?VectorQ]:=SumAll[expr,Sort[Join[ind1,ind2]]];
SumAll[0,ind_?VectorQ]:=0;

(* ==============================================*)
(* Function to rename indicies into canonical names *)
SumAllCanonical[expr_,ind_?VectorQ]:=Module[{newIndRule,indLen,retVal},
indLen=Length[ind];
newIndRule=Table[ind[[ii]] -> ToExpression["i" <> ToString[ii]],{ii,1,indLen}];
retVal=SumAll[(expr /. newIndRule), (ind /. newIndRule)];
Return[retVal];
]
(* ==============================================*)
(* Sorts second list using the normal ordering of the first one. *)
SortByFirst[lstOrder:{__},lstToBeSorted:{__}]:=Module[{retVal,nn,lst,lstSorted},
nn=Length[lstOrder];

If[Length[lstToBeSorted]!= nn,(Print["SortByFirt::Lists have different length!"]; Return[Indeterminate];)];

lst=Table[{lstOrder[[ii]],lstToBeSorted[[ii]]},{ii,1,nn}];

lstSorted=SortBy[lst,First];
retVal=Table[lstSorted[[ii,2]],{ii,1,nn}];

Return[retVal];
];
(* Returns number of occurencies of a given index in the expression list *)
GetNoOfOccurencies[exprList_?VectorQ,index_]:=Module[{count,exprLen,ii},
(*
Print[sep];
Print["GetNoOfOccurencies::Starting..."];
Print["exprList = ", exprList];
Print["index = ", index];
*)
exprLen=Length[exprList];
count=Sum[If[MemberQ[exprList[[ii]],index,Infinity],1,0],{ii,exprLen}];

(* Print["count = ", count]; *)
Return[count];
];

(* PowerMultipler is used to rank power in the front of others (or at the end, which ever works out). *)
PowerMultipler=10^3;

(* Returns the list of element ranks (sum of indexes of ind, as occurs in each element of the list. )*)
GetExpressionRankList[exprList_?VectorQ,ind_?VectorQ]:=Module[{rankList,indLen,exprLen,ii,jj,rule,exprTest,indTest,powerList,exprTest1,exprTest1Sorted,exprTestFinal},
indLen=Length[ind];
exprLen=Length[exprList];

rule=Table[ind[[ii]] -> ToExpression["i" <> ToString[ii]],{ii,1,indLen}];

exprTest=Sort[exprList/.rule];
indTest=ind /. rule;

exprTest1=Table[If[ToString[Head[exprTest[[ii]]]]== "Power",Reverse[Apply[List,exprTest[[ii]]]],{1,exprTest[[ii]]}],{ii,1,exprLen}];
exprTest1Sorted=Sort[exprTest1];
exprTestFinal=Table[Apply[Power,Reverse[exprTest1Sorted[[ii]]]],{ii,1,exprLen}];
(*
powerList=Table[If[ToString[Head[exprTest[[ii]]]]== "Power",PowerMultipler,0],{ii,1,exprLen}];
*)
powerList=Table[If[ToString[Head[exprTestFinal[[ii]]]]== "Power",PowerMultipler,0],{ii,1,exprLen}];

(*
Print["GetExpressionRankList::Starting..."]; 
Print["GetExpressionRankList::exprList = ", exprList ] ;
Print["GetExpressionRankList::ind = ", ind ] ;
Print["GetExpressionRankList::exprTest = ", exprTest ] ;
Print["GetExpressionRankList::indTest = ", indTest ] ;
Print["GetExpressionRankList::powerList = ", powerList ] ;
Print["GetExpressionRankList::exprTest1 = ", exprTest1 ] ;
Print["GetExpressionRankList::exprTest1Sorted = ", exprTest1Sorted ] ;
Print["GetExpressionRankList::exprTestFinal = ", exprTestFinal ] ;
*)

(*
rankList=Table[Sum[jj*If[MemberQ[exprTest[[ii]],indTest[[jj]],Infinity],1,0],{jj,1,indLen}]+powerList[[ii]],{ii,1,exprLen}];
 *)

rankList=Table[Sum[jj*If[MemberQ[exprTestFinal[[ii]],indTest[[jj]],Infinity],1,0],{jj,1,indLen}]+powerList[[ii]],{ii,1,exprLen}];
 
(* 
Print["GetExpressionRankList::rankList = ", rankList];
Print[sep];
*)
Return[rankList];
];

GetSmallestList[listOfLists_?MatrixQ]:=Module[{noOfLists,maxIndex,ii,cmpResult},
noOfLists=Length[listOfLists];
maxIndex=1;
(*
Print["GetSmallestList::listOfLists = ", listOfLists // MatrixForm];
Print["GetSmallestList::noOfLists = ", noOfLists];
*)
For[ii=1,ii <= noOfLists,ii++,
(
cmpResult=CompareRankLists[listOfLists[[maxIndex]],listOfLists[[ii]]];
(*
Print["GetSmallestList::listOfLists[[maxIndex]] = ", listOfLists[[maxIndex]], ", listOfLists[[",ii,"]]] = ", listOfLists[[ii]]];
Print["GetSmallestList::cmpResult = ", cmpResult];
*)
If[cmpResult>0,
(
maxIndex=ii;
(* Print["Setting maxIndex to ", ii]; *)
)
];
)
];

Return[maxIndex];
];

(* Returns 0 if lists are the same, 1, if list1 > list2 and (-1) if list1 < list2 *)
CompareRankLists[rank1_?VectorQ,rank2_?VectorQ]:=Module[{len1,len2,len,ii,retVal},
len1=Length[rank1];
len2=Length[rank2];
len=Min[len1,len2];
retVal=0;
(*
Print["   "];
Print["CompareRankLists::rank1 = ", rank1, ", rank2 = ", rank2];
*)
For[ii=1,ii<=len,ii++,
(
(* Print["CompareRankLists::ii = ", ii]; *)
If[rank1[[ii]]> rank2[[ii]],
(
retVal=1;
(* Print["CompareRankLists:: rank1[[ii]]> rank2[[ii], ii = ", ii, ", retVal = ", retVal]; *)
Return[retVal];
),
(
If[rank1[[ii]]< rank2[[ii]],
(
retVal=-1;
(* Print["CompareRankLists:: rank1[[ii]]< rank2[[ii]], ii = ", ii, ", retVal = ", retVal]; *)
Return[retVal];
)
];
)
];
)
];

(* Print["CompareRankLists:: retVal = ", retVal]; *)
Return[retVal];
];

(* This function is supposed to return a canonical order for graphs. This is fucked. *)
(* So far we only support graphs of no more than about 3 vertices and no more than about 3 edges (or something like that). *)
CanonicalOrderRule[exprList_?VectorQ,ind_?VectorQ]:=Module[{rule,indLen,exprLen,dummy,countTbl,ii,indNew},
indLen=Length[ind];
exprLen=Length[exprList];
(* Print["CanonicalOrderRule::Starting..."]; *)

countTbl=Table[GetNoOfOccurencies[exprList,ind[[ii]]],{ii,1,indLen}];
(* Print["countTbl = ", countTbl]; *)

indNew=SortByFirst[countTbl,ind];
countTbl=Table[GetNoOfOccurencies[exprList,indNew[[ii]]],{ii,1,indLen}];

(*
Print["countTbl (after sorting) = ", countTbl];
Print["Sort indicies according to countTbl."];
Print["indNew = ", indNew];
*)

rule=Table[indNew[[ii]] -> ToExpression["i" <> ToString[ii]],{ii,1,indLen}];
Return[rule];
];

CanonicalOrderPermutationRule[exprList_?VectorQ,ind_?VectorQ]:=Module[{rule,indLen,exprLen,dummy,countTbl,ii,indNew,permut,permLen,rankTable,idxSmallest},
indLen=Length[ind];
exprLen=Length[exprList];
idxSummmAll++;

(* Print["CanonicalOrderPermutationRule::Starting..."]; *)

permut=Permutations[ind];
permLen=Length[permut];
rankTable=Table[GetExpressionRankList[exprList,permut[[ii]]],{ii,1,permLen}];
idxSmallest=GetSmallestList[rankTable];

(*
Print["CanonicalOrderPermutationRule::exprList = ", exprList ] ;
Print["CanonicalOrderPermutationRule::ind = ", ind ] ;
Print["CanonicalOrderPermutationRule::permut = ", permut // MatrixForm] ;
Print["CanonicalOrderPermutationRule::permLen = ", permLen];
Print["CanonicalOrderPermutationRule::rankTable = ", rankTable // MatrixForm];
Print["CanonicalOrderPermutationRule::idxSmallest = ", idxSmallest];
*)

rule=Table[permut[[idxSmallest,ii]] -> ToExpression["i" <>ToString[idxSummmAll]<>"c"<> ToString[ii]],{ii,1,indLen}];
(* Print["CanonicalOrderPermutationRule::rule = ", rule]; *)
Return[rule];
];

(* ==============================================*)
GetIndExprNoInd[lstExpr:{_,_}]:=lstExpr[[1]];
GetIndExprPairs[lstExpr:{_,_}]:=lstExpr[[2]];

(* Function to split list expression into independent parts based on the indicies. *)
SplitIndexedExpression[exLst:{___},ind:{___}]:=Module[{indLen,exLstHlp,exLen,exLstIndLstPairLst,idx,delLst,idxEx,exLstIndLstPair,ex,IsMerged,ii,idxInd,exLstIndLstPairLstChanged,changeReq,retVal},
indLen=Length[ind];
exLstHlp=exLst;
exLen=Length[exLst];

If[DebugSymbolic,
(
Print["  "];
Print[sep];
Print[sep];
Print["SplitExpression::Starting..."];
Print["SplitIndexedExpression::ind = ", ind];
Print["SplitIndexedExpression::indLen = ", indLen];
Print["SplitIndexedExpression::exLst = ", exLst];
Print["SplitIndexedExpression::exLen = ", exLen];
)
];

exLstIndLstPairLst={};

For[idx=1,idx <= indLen,idx++,
(
If[DebugSymbolic,
(
Print[sep];
Print["SplitIndexedExpression::idx = ", idx, ", ind[[",idx,"]] = ", ind[[idx]]];
)
];

delLst=Table[False,{ii,1,Length[exLstHlp]}];

For[idxEx=1,idxEx <=Length[exLstHlp],idxEx++,
(
If[DebugSymbolic,
(
Print["SplitIndexedExpression::idxEx = ", idxEx, ", exLstHlp[[",idxEx,"]] = ", exLstHlp[[idxEx]]];
)
];

If[MemberQ[exLstHlp[[idxEx]],ind[[idx]],Infinity],
(
If[DebugSymbolic,
(
Print["SplitIndexedExpression::exLstHlp[[",idxEx,"]] has ", ind[[idx]]];
)
];

delLst[[idxEx]]=True;
)
];
)
];

If[DebugSymbolic,
(
Print["SplitIndexedExpression::delLst = ", delLst];
)
];
exLstIndLstPair={{},{ind[[idx]]}};
If[DebugSymbolic,
(
Print["SplitIndexedExpression::Deleting elements with matched indicies, exLstHlp =  ", exLstHlp];
)
];

For[idxEx=Length[exLstHlp],idxEx>= 1,idxEx--,
(
If[DebugSymbolic,
(
Print["SplitIndexedExpression::idxEx = ", idxEx];
)
];

If[delLst[[idxEx]],
(
ex=exLstHlp[[idxEx]];

If[DebugSymbolic,
(
Print["SplitIndexedExpression::ex = ", ex];
Print["SplitIndexedExpression::Deleting exLstHlp[[",idxEx,"]] = ",exLstHlp[[idxEx]]];
)
];

exLstIndLstPair[[1]]=Prepend[exLstIndLstPair[[1]],ex];
exLstHlp=Delete[exLstHlp,idxEx];
If[DebugSymbolic,
(
Print["SplitIndexedExpression::exLstHlp = ", exLstHlp];
)
];
)
];
)
];
If[DebugSymbolic,
(
Print["SplitIndexedExpression::New exLstHlp = ", exLstHlp];
Print["SplitIndexedExpression::exLstIndLstPair = ", exLstIndLstPair];

Print["SplitIndexedExpression::Checking whether previous pairs have that index: ", ind[[idx]]];
Print["SplitIndexedExpression::exLstIndLstPairLst (before merge) = ", exLstIndLstPairLst];
)
];

IsMerged=False;
If[Length[exLstIndLstPairLst]==0,
(
exLstIndLstPairLst={exLstIndLstPair};

If[DebugSymbolic,
(
Print["SplitIndexedExpression::Length of exLstIndLstPairLst == 0. Adding exLstIndLstPair."];
Print["SplitIndexedExpression::exLstIndLstPairLst = ", exLstIndLstPairLst];
)
];
),
(
If[DebugSymbolic,
(
Print["SplitIndexedExpression::Length of exLstIndLstPairLst > 0. Merging exLstIndLstPair to the first found match."];
)
];

For[ii=1,ii<=Length[exLstIndLstPairLst],ii++,
(
If[DebugSymbolic,
(
Print["SplitIndexedExpression::Checking exLstIndLstPairLst[[",ii,"]] = ",exLstIndLstPairLst[[ii]]];
)
];

If[MemberQ[exLstIndLstPairLst[[ii]][[1]],ind[[idx]],Infinity],
(
exLstIndLstPairLst[[ii]]={Join[exLstIndLstPairLst[[ii]][[1]],exLstIndLstPair[[1]]],Append[exLstIndLstPairLst[[ii]][[2]],ind[[idx]]]};

If[DebugSymbolic,
(
Print["SplitIndexedExpression::Index ",ind[[idx]], " found. Merging"];
Print["SplitIndexedExpression::New exLstIndLstPairLst[[",ii,"]] = ",exLstIndLstPairLst[[ii]]];
)
];

IsMerged=True;
Break[];
)
];
)
];

If[!IsMerged,
(
If[DebugSymbolic,
(
Print["SplitIndexedExpression::Not merging!!!"];
)
];

exLstIndLstPairLst=Append[exLstIndLstPairLst,exLstIndLstPair];
)
];

If[DebugSymbolic,
(
Print["SplitIndexedExpression::exLstIndLstPairLst (after merge) = ", exLstIndLstPairLst];
)
];

If[Length[exLstIndLstPairLst]>1,
(
If[DebugSymbolic,
(
Print["SplitIndexedExpression::Length of exLstIndLstPairLst > 1. Checking existing pairs."];

Print["SplitIndexedExpression::Finding first element of exLstIndLstPairLst with ", ind[[idx]]];
)
];

For[idxInd=1,idxInd <= Length[exLstIndLstPairLst],idxInd++,
(
If[MemberQ[exLstIndLstPairLst[[idxInd]][[2]],ind[[idx]],Infinity],
(
If[DebugSymbolic,
(Print["SplitIndexedExpression::First element with ", ind[[idx]], " is exLstIndLstPairLst[[",idxInd,"]] = ",exLstIndLstPairLst[[idxInd]]];
)
];

Break[];
)
];
)
];

exLstIndLstPairLstChanged=True;

delLst=Table[False,{ii,1,Length[exLstIndLstPairLst]}];

While[exLstIndLstPairLstChanged,
(
If[DebugSymbolic,
(
Print["SplitIndexedExpression::Testing if some pairs should be merged for a given index ", ind[[idx]]];
)
];

changeReq=False;

For[ii=idxInd+1,ii<=Length[exLstIndLstPairLst],ii++,
(
If[DebugSymbolic,
(
Print["SplitIndexedExpression::exLstIndLstPairLst[[",ii,"]] = ", exLstIndLstPairLst[[ii]]];
)
];

If[MemberQ[exLstIndLstPairLst[[ii]][[1]],ind[[idx]],Infinity],
(
If[DebugSymbolic,
(
Print["SplitIndexedExpression::exLstIndLstPairLst[[",ii,"]][[1]] has ", ind[[idx]]];
)
];

changeReq=True;
delLst[[ii]]=True;

If[DebugSymbolic,
(
Print["SplitIndexedExpression::Merging element ", ii, " into element ",idxInd];
)
];
exLstIndLstPairLst[[idxInd]][[1]]=Join[exLstIndLstPairLst[[idxInd]][[1]],exLstIndLstPairLst[[ii]][[1]]];
exLstIndLstPairLst[[idxInd]][[2]]=Join[exLstIndLstPairLst[[idxInd]][[2]],exLstIndLstPairLst[[ii]][[2]]];

exLstIndLstPairLst=Delete[exLstIndLstPairLst,ii];
If[DebugSymbolic,
(
Print["SplitIndexedExpression::exLstIndLstPairLst[[", idxInd, "]] after merge = ",exLstIndLstPairLst[[idxInd]]];
Print["SplitIndexedExpression::exLstIndLstPairLst after merge = ",exLstIndLstPairLst];
)
];

)
];

If[changeReq,
(
If[DebugSymbolic,
(
Print["SplitIndexedExpression::Change requested. Breaking the loop."];
)
];
Break[];
)
];
)
];

If[DebugSymbolic,
(
Print["SplitIndexedExpression::delLst = ", delLst];
)
];

If[!changeReq,exLstIndLstPairLstChanged=False];

)
];
)
];
)
];
)
];

retVal={exLstHlp,exLstIndLstPairLst};

If[DebugSymbolic,
(
Print["SplitIndexedExpression::Final exLstHlp = ", exLstHlp];
Print["SplitIndexedExpression::Final exLstIndLstPairLst = ", exLstIndLstPairLst];
Print["SplitIndexedExpression::retVal = ", retVal];
)
];

(* Print["SplitExpression::exLst = ", exLst, ", ind = ", ind, ", retVal = ", retVal]; *)
Return[retVal];
];

(* ==============================================*)
(* Function to apply canonical ordering to final expressions using symmetric and antisymmetric properties of variables *)
CanonicOrdering[expr_]:=CanonicOrdering[expr,AllFunctionsList];
CanonicOrdering[expr_,varLst_?VectorQ]:=Module[{retVal,len,ii},
len=Length[varLst];

retVal=expr;
For[ii=1,ii <= len,ii++,
(
retVal=CanonicalOrderingExpr[retVal,varLst[[ii]]];
)
];

Return[retVal];
];
(* ==============================================*)
MaxPosLen=100;
CanonicalOrderingExpr[expr_,var_]:=Module[{retVal,len,ii,antiSymVal,symVal,pos,posLen,elemPos,elem,elemNew,exprNew,rule,elemLst,elemLstCanonical,jj,permutationSign},
antiSymVal=OptionValue[var,AntiSymmetric];
symVal=OptionValue[var,Symmetric];

exprNew=Hold[expr];
pos=Position[exprNew,var];
posLen=Length[pos];

If[DebugCanonicOrdering && posLen > MaxPosLen,
(
PrintTimeUsed[];
Print["CanonicalOrderingExpr::Starting..."];
Print["Length[expr] = ", Length[expr]];
Print["Length[exprNew] = ", Length[exprNew]];
Print["var = ", var];
Print["antiSymVal = ", antiSymVal];
Print["symVal = ", symVal];
Print["posLen = ", posLen];
)
];

For[ii=posLen,ii >= 1,ii--,
(
permutationSign=1;
elemPos=Delete[pos[[ii]],Length[pos[[ii]]]];
elem=Extract[exprNew,elemPos];
elemLst=Apply[List,elem];
elemLstCanonical=Sort[elemLst];

If[(antiSymVal ||symVal) && (!(antiSymVal && symVal)),
(
rule= Table[elemLst[[jj]] -> elemLstCanonical[[jj]],{jj,1,Length[elemLst]}];
),
(
rule= {};
)
];

If[antiSymVal,
(
If[elemLst =!= elemLstCanonical,
(
permutationSign=-1;
)
];
)
];

elemNew=permutationSign*(elem /. rule);
exprNew=ReplacePart[exprNew,elemPos ->elemNew ];

If[DebugCanonicOrdering && DebugSymbolicLocal && posLen > MaxPosLen,
(
Print[sep];
Print["ii = ", ii];
Print["elemPos = ", elemPos];
Print["elem = ", elem];
Print["elemLst = ", elemLst];
Print["elemLstCanonical = ", elemLstCanonical];
Print["rule = ", rule];
Print["permutationSign = ", permutationSign];
Print["elemNew = ", elemNew];
Print["exprNew = ", exprNew];
)
];
)
];

retVal=ReleaseHold[exprNew];
If[DebugCanonicOrdering && posLen > MaxPosLen,
(
(* Print["retVal = ", retVal]; *)
Print["Length[retVal] = ", Length[retVal]];
Print["CanonicalOrderingExpr::Ending"];
PrintTimeUsed[];
)
];

Return[retVal];
];
(* ==============================================*)

(* Function to apply various rules to the expression *)
ApplyRules[expr_,rules___]:=Module[{retVal,len,ii,rulesExp},
rulesExp={rules};
len=Length[rulesExp];
retVal=Expand[expr];
If[DebugApplyRules,
(
PrintTimeUsed[];
Print["ApplyRules::Starting..."];
Print["ApplyRules::Length[rulesExp] = ", len];
Print["ApplyRules::Length[expr] = ", Length[expr]];
)
];

(*
Print["ApplyRules::rules = ", rules];
*)

For[ii=1,ii<= len,ii++,
(
(*
Print[sep];
Print["ii = ", ii, ", rulesExp[[",ii,"]] = ",rulesExp[[ii]]];
*)
retVal=retVal /. rulesExp[[ii]];
)
];
(*
Print["ApplyRules::retVal = ", retVal];
Print[sep];
*)

If[DebugApplyRules,
(
Print["ApplyRules::Length[retVal] = ", Length[retVal]];
Print["ApplyRules::Ending..."];
PrintTimeUsed[];
)
];

Return[retVal];
];

(* ============================================= *)

(* Function to apply various rules to the expression *)
ApplyRulesSimplify[expr_,rules___]:=Module[{retVal},
If[!ApplyRulesDonNotSimplify,
(
If[DebugApplyRules,Print["ApplyRulesSimplify::Applying rules and simplifying."]];

retVal=Simplify[CanonicOrdering[ApplyRules[CanonicOrdering[ApplyRules[CanonicOrdering[expr],rules] /.{SumAll -> SummmAll}/. {SumAll -> SumAllCanonical}],rules]]];
),
(
If[DebugApplyRules,Print["ApplyRulesSimplify::Only applying rules."]];
retVal=ApplyRules[expr,rules];
)
];

Return[retVal];
];

(* ============================================= *)
(* Function to remove I *)

RemoveI[expr_]:=Module[{exprVal},
exprVal=expr;

If[DoRemoveI,
(
exprVal=Simplify[ToExpression[StringReplace[ToString[InputForm[Expand[exprVal]]],"I" -> "0"]]];
If[DebugRemoveI,
(
Print["RemoveI::Removing I."];
Print["expr = ", InputForm[expr]];
Print["exprVal = ", InputForm[exprVal]];
Print[sep];
)
];
)
];

Return[exprVal];
];
(* ============================================= *)
