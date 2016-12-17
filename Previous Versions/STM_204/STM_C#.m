(* :Author: Konstantin K.Konstantinov *)
(* :Summary: Various C # Unit Test Generators for STM. *)
(* :Copyright: K^3, 2013 - 2014 *)
(* :Version: Revision: 1.20.001, Date: 2014/01/12 *)
(* :Mathematica Version: 7.0 - 9.0 *)
(* ============================================== *)

CSharpVarNameColl={};
CSharpVarValueColl={};

CSharpExprNameColl={};
CSharpExprColl={};
CSharpExprValColl={};

(* ============================================= *)

InitializeCSharpGen[]:=Module[{},
CSharpVarNameColl={};
CSharpVarValueColl={};

CSharpExprNameColl={};
CSharpExprColl={};
CSharpExprValColl={};

];

(* ============================================= *)

Attributes[AddVariable]={HoldAll};
AddVariable[var_]:=Module[{varNameStr,valueStr},
varNameStr=SymbolName[Unevaluated[var]];
valueStr=ToCSharpString[var];
AddVar[varNameStr,valueStr];
];

Attributes[AddIntegerVariable]={HoldAll};
AddIntegerVariable[var_]:=Module[{varNameStr,valueStr},
varNameStr=SymbolName[Unevaluated[var]];
valueStr=ToString[var];
AddVar[varNameStr,valueStr];
];

(* ============================================= *)

AddVar[varNameStr_?StringQ,valueStr_?StringQ]:=AddVar[varNameStr,valueStr,True];
AddVar[varNameStr_?StringQ,valueStr_?StringQ,ignoreDuplicates_?BooleanQ]:=Module[{},

If[!MemberQ[CSharpVarNameColl,varNameStr],
(
CSharpVarNameColl=Join[CSharpVarNameColl,{varNameStr}];
CSharpVarValueColl=Join[CSharpVarValueColl,{valueStr}];
),
(
Print["AddVar::Variable ", varNameStr , " is already in collection. Ignoring..."];
)
];
];

(* ============================================= *)

AddTestResult[nameStr_?StringQ,exprStr_?StringQ,valueStr_?StringQ]:=Module[{},
If[!MemberQ[CSharpExprNameColl,nameStr],
(
CSharpExprNameColl=Join[CSharpExprNameColl,{nameStr}];
CSharpExprColl=Join[CSharpExprColl,{exprStr}];
CSharpExprValColl=Join[CSharpExprValColl,{valueStr}];
),
(
Print["AddTestResult::Expression with name ", nameStr , " is already in collection. Ignoring..."];
)
];
];

(* ============================================= *)

OutputCSharpCode[]:=Module[{sInit,sTest,lenInit,lenTest,ii,crlf},
lenInit=Length[CSharpVarNameColl];
lenTest=Length[CSharpExprNameColl];

crlf=FromCharacterCode[13];
sInit="";
sTest="";

For[ii=1,ii<=  lenInit,ii++,
(
sInit=sInit  <> "AddInitialVariable(\"" <> CSharpVarNameColl[[ii]] <> "\", " <> CSharpVarNameColl[[ii]] <> ", \"" <> CSharpVarValueColl[[ii]] <>"\");" <> crlf ;
)
];

For[ii=1,ii<=  lenTest,ii++,
(
(*
sTest=sTest  <> "AddTestResult(\"" <> CSharpExprNameColl[[ii]] <> "\", "<> CSharpExprColl[[ii]] <> ", \"" <> CSharpExprValColl[[ii]] <>"\");" <> crlf ;
*)

sTest=sTest  <> "AddTestResult(\"" <> CSharpExprNameColl[[ii]] <> "\", \""<>CSharpExprColl[[ii]]<> "\", "<> CSharpExprColl[[ii]] <> ", \"" <> CSharpExprValColl[[ii]] <>"\");" <> crlf ;
)
];

Print[crlf,crlf,strSeparator,strSeparator];
Print["C# initialization code:"];
Print[sInit];
Print[strSeparator,strSeparator];
Print["C# test code:"];
Print[sTest];
Print[strSeparator,strSeparator,crlf,crlf];

];

(* ============================================= *)

ToCSharpString[expr_]:=ToString[N[expr],InputForm,NumberMarks -> False];
SymbolNameFreeQ[s_?StringQ]:=StringFreeQ[s,"SymbolName["~~___];

(* ============================================= *)

Attributes[ToCSharpCall]={HoldAll};
ToCSharpCall[funcName_,varLst_]:=Module[{retVal,csValStr,csStr,argList,len,ii,varName,s,sSymb,expr,exprStr,symbName,result,resStr,crlf,exprValList,exprLst,exprLstSymbName},
len=Length[varLst];
crlf=FromCharacterCode[13];
symbName=SymbolName[funcName];
s=ToString[Unevaluated[varLst],InputForm];
sSymb=StringReplace[s,{" " -> "","{" -> "{SymbolName[Unevaluated[","," -> "]],SymbolName[Unevaluated[","}" -> "]]}"}];
Off[SymbolName::"sym"];
expr=ToExpression[sSymb];
exprLst=Map[ToString,expr];
exprLstSymbName=Map[SymbolNameFreeQ,exprLst];
exprValList=Map[ToCSharpString,varLst];
exprStr=symbName <> StringReplace[s,{"{" -> "(", "}" -> ")", "True" -> "true","False" -> "false"}];
result=N[Apply[funcName,varLst]];
resStr=ToString[result,InputForm,NumberMarks -> False];

(*
Print["funcName = ", funcName];
Print["s = ", s];
Print["varLst = ", varLst];
Print["len = ", len];
Print["sSymb = ", sSymb];
Print["expr = ", expr];
Print["exprLst = ", exprLst];
Print["exprValList = ", exprValList];
*)

(*
csStr="AddTestResult(\"" <> symbName <> "\", "<> exprStr <> ", \"" <> resStr <>"\");" ;
Print["csStr = ", csStr];
csValStr="AddVariable(!!! To be completed !!!);" ;
Print["csValStr = ", csValStr];

*)

AddTestResult[symbName,exprStr,resStr];

For[ii=1,ii <= len,ii++,
(
If[(Length[Names["System`" <> exprLst[[ii]]]] == 0) && exprLstSymbName[[ii]],
(
AddVar[exprLst[[ii]],exprValList[[ii]]];
)
];
)
];

retVal={csStr,csValStr};
On[SymbolName::"sym"];
Return[retVal];
];

(* ============================================= *)

Attributes[ToCSharpExpr]={HoldAll};
ToCSharpExpr[expr_]:=Module[{exprName,exprVal},
exprName=SymbolName[Unevaluated[expr]];
exprVal=ToCSharpString[expr];
AddTestResult[exprName,exprName,exprVal];
];
(* ============================================= *)

InitializeCSharpGen[];

(* ============================================= *)
