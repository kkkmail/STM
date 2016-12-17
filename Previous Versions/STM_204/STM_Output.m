(* :Author: Konstantin K.Konstantinov *)
(* :Summary: Output functions for STM. *)
(* :Copyright: K^3, 2013 - 2014 *)
(* :Version: Revision: 1.20.001, Date: 2014/01/12 *)
(* :Mathematica Version: 7.0 - 9.0 *)
(* ============================================== *)
Options[STMOutput]={UseKeyTypeInXML -> True, XMLKeyType -> "\"System.String, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089\"",UseValueTypeInXML -> True, XMLValueType -> "\"System.String, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089\"", XMLHeader -> "<?xml version=\"1.0\" encoding=\"utf-8\"?>"};
(* ============================================== *)
IMAGESIZE=500;
defaultPltOpts:={PlotRange->All,Frame->True,GridLines->Automatic,PlotStyle->Thick,ImageSize ->IMAGESIZE };
defaultPltOpts2:={Frame->True,GridLines->Automatic,PlotStyle->Thick,ImageSize ->IMAGESIZE };
defaultPlt3DOpts:={PlotRange->All,ImageSize ->IMAGESIZE };
(* ============================================== *)
PlotUncorrelatedCoordinates[nn_?IntegerQ,solCoord:{__},runID_?IntegerQ,description_]:=Module[{pltOpts,str},
pltOpts=defaultPltOpts;
str=ToString[description];

Print["Lambda (" <> str <> " for runID = ", runID];
Print[Plot[LambdaFunc[idx,solCoord],{idx,1,nn},Evaluate[pltOpts]]];
(*
Print["x range (" <> str <> ") for runID = ", runID];
Print[Plot[xRangeFunc[idx,solCoord],{idx,1,nn},Evaluate[pltOpts]]];
Print["x sigma range (" <> str <> ") for runID = ", runID];
Print[Plot[xRangeSigmaFunc[idx,solCoord],{idx,1,nn},Evaluate[pltOpts]]];
*)

Print["x range and x sigma range (" <> str <> ") for runID = ", runID];
Print[Plot[{xRangeFunc[idx,solCoord],xRangeSigmaFunc[idx,solCoord]},{idx,1,nn},Evaluate[pltOpts]]];

Print[strSeparatorSmall];
];

PlotUncorrelatedCoordinatesDifference[nn_?IntegerQ,solCoord1:{__},solCoord2:{__},runID_?IntegerQ]:=PlotUncorrelatedCoordinatesDifference[nn,solCoord1,solCoord2,runID,""];

PlotUncorrelatedCoordinatesDifference[nn_?IntegerQ,solCoord1:{__},solCoord2:{__},runID_?IntegerQ,description_]:=Module[{pltOpts,str},
pltOpts=defaultPltOpts;
str=ToString[description];
If[StringLength[str] > 0, str = ", " <> str];

Print["Lambda (difference" <> str <> " for runID = ", runID];
Print[Plot[LambdaFunc[idx,solCoord1]-LambdaFunc[idx,solCoord2],{idx,1,nn},Evaluate[pltOpts]]];
Print["x range (difference" <> str <> ") for runID = ", runID];
Print[Plot[xRangeFunc[idx,solCoord1]-xRangeFunc[idx,solCoord2],{idx,1,nn},Evaluate[pltOpts]]];
Print["x sigma range (difference" <> str <> ") for runID = ", runID];
Print[Plot[xRangeSigmaFunc[idx,solCoord1]-xRangeSigmaFunc[idx,solCoord2],{idx,1,nn},Evaluate[pltOpts]]];
Print[strSeparatorSmall];
];

(* ============================================= *)

PlotXXT[sab_?MatrixQ]:=Module[{nn,SumSab,TrXXT,XXTaa,XXT,aa,bb,XXTLinear,XXTDiagSorted,XXTLinearSorted,nna},
nn=Length[sab];
SumSab=Table[Sum[sab[[aa,bb]],{bb,1,nn}],{aa,1,nn}];
TrXXT=(1/(2*nn))*Sum[SumSab[[aa]],{aa,1,nn}];
XXTaa=(1/nn)*Table[SumSab[[aa]]-TrXXT,{aa,1,nn}];
XXT=Table[If[aa==bb,XXTaa[[aa]],(XXTaa[[aa]]+XXTaa[[bb]]-sab[[aa,bb]])/2],{aa,1,nn},{bb,1,nn}];
XXTLinear=ToLinear[XXT];
nna=Length[XXTLinear];
XXTLinearSorted=Sort[XXTLinear];
XXTDiagSorted=Sort[XXTaa];
(* *)
Print["XXT (off diagonal)"];
Print[Plot[IndexedVariableFunc[idx,XXTLinearSorted],{idx,1,nna},Evaluate[defaultPltOpts]]];
Print["XXT (diagonal)"];
Print[Plot[IndexedVariableFunc[idx,XXTDiagSorted],{idx,1,nn},Evaluate[defaultPltOpts]]];
];

(* ============================================= *)

ToXMLItem[name_,object_,rawOpts___]:=Module[{s,sName,sObj,crlf,opts,useKeyTypeInXMLVal,XMLKeyTypeVal,useValueTypeInXMLVal,XMLValueTypeVal},
opts =ProcessOptions[rawOpts];

useKeyTypeInXMLVal=UseKeyTypeInXML/.opts /.Options[STMOutput];
XMLKeyTypeVal=XMLKeyType/.opts /.Options[STMOutput];
useValueTypeInXMLVal=UseValueTypeInXML/.opts /.Options[STMOutput];
XMLValueTypeVal=XMLValueType/.opts /.Options[STMOutput];

sName=ToString[name];
sObj=ToString[InputForm[N[object], NumberMarks ->  False]];
(* crlf=FromCharacterCode[13]<>FromCharacterCode[10]; *)
crlf=FromCharacterCode[13];
s="";
s=s <>"  <item>" <> crlf;

If[useKeyTypeInXMLVal,
(
s=s <>"    <key type=" <> XMLKeyTypeVal <> ">" <> crlf;
),
(
s=s <>"    <key>" <> crlf;
)
];

s=s <>"      <string>" <> sName <> "</string>" <> crlf;
s=s <>"    </key>" <> crlf;

If[useValueTypeInXMLVal,
(
s=s <>"    <value type=" <> XMLValueTypeVal <> ">" <> crlf;
),
(
s=s <>"    <value>" <> crlf;
)
];

s=s <>"      <string>" <> sObj <> "</string>" <> crlf;
s=s <>"    </value>" <> crlf;
s=s <>"  </item>" <> crlf;
Return[s];
];

(* ============================================= *)

ToXMLStart[rawOpts___]:=Module[{s,opts,XMLHeaderVal,crlf},
opts =ProcessOptions[rawOpts];
XMLHeaderVal=XMLHeader/.opts /.Options[STMOutput];
crlf=FromCharacterCode[13];
s=XMLHeaderVal <> crlf;
s=s <> "<dictionary>" <> crlf;
Return[s];
];

(* ============================================= *)

Attributes[ToXMLAddItem]={HoldFirst};
ToXMLAddItem[xmlString_,name_,object_,rawOpts___]:=Module[{s,sItem},
sItem=ToXML[name,object,rawOpts];
xmlString=xmlString <> sItem;
Return[xmlString];
];

(* ============================================= *)

Attributes[ToXMLEnd]={HoldFirst};
ToXMLEnd[xmlString_,rawOpts___]:=Module[{crlf},
crlf=FromCharacterCode[13];
xmlString=xmlString <> "</dictionary>" <> crlf;
Return[xmlString];
];

(* ============================================= *)

