#!/usr/bin/env wolframscript
(* ::Package:: *)

Clear["Global`*"]




Needs["DifferentialEquations`NDSolveUtilities`"];
Needs["FunctionApproximations`"];


er=40;
SEL=ToExpression[$ScriptCommandLine[[7]]];

LL=ToExpression[$ScriptCommandLine[[8]]];

\[Lambda]=ToExpression[$ScriptCommandLine[[2]]];

cys=ToExpression[$ScriptCommandLine[[6]]];
p=7;
ReleaseHold[SetPrecision[Hold[
TT=ToExpression[$ScriptCommandLine[[5]]];
nt=-1;
AP={2,3,4};
\[Gamma]=1/10;
TOL=1/10],er]];


SetDirectory[$ScriptCommandLine[[3]]];


tempfile1=FileNameJoin[{Directory[],"savedBoundary.wl"}];
tempfile3=FileNameJoin[{Directory[],ToString[\[Lambda]]<>"-"<>"savedHeaaisanH.wl"}];
tempfile4=FileNameJoin[{Directory[],ToString[\[Lambda]]<>"-"<>ToString[TT]<>"-"<>"savedCVcoe.wdx"}];
tempfile5=FileNameJoin[{Directory[],ToString[\[Lambda]]<>"-"<>ToString[TT]<>"ResultMarkov.wdx"}];
tempfile6=FileNameJoin[{Directory[],ToString[\[Lambda]]<>"-"<>"core.mx"}];
tempfile7=FileNameJoin[{Directory[],ToString[\[Lambda]]<>"-"<>ToString[TT]<>"-"<>"ResultJ234.wdx"}];
tempfile8=FileNameJoin[{Directory[],ToString[\[Lambda]]<>"-"<>ToString[TT]<>"-"<>"CC.wdx"}];
tempfile11=FileNameJoin[{Directory[],ToString[\[Lambda]]<>"-"<>ToString[TT]<>"-"<>"CSM.wdx"}];
tempfile12=FileNameJoin[{Directory[],ToString[\[Lambda]]<>"-"<>ToString[TT]<>"-"<>"savedCVScoe.wdx"}];
tempfile13=FileNameJoin[{Directory[],"email.wl"}];


Get[tempfile13];


ArgM[z_,\[Sigma]_:0]:=Arg[z Exp[-I*(\[Sigma]*Pi)]]+\[Sigma]*Pi;



LogM[z_,\[Sigma]_:0]:=Log[z Exp[-I*(\[Sigma]*Pi)]]+I*\[Sigma]*Pi



Get[tempfile1];
ReleaseHold[SetPrecision[Hold[arv=Arv;Arv=\[Lambda]*Arv;],er]];



{gv,xv,zv}=SetPrecision[{gv,xv,zv},er];


Indexg=Table[{Quotient[x-1,6]+2,Quotient[Mod[x-1,6],3]*2+1,Mod[x-1,3]+1},{x,1,24}];
Indexz=Table[{Quotient[x-1,2]+1,2*Mod[x-1,2]+1},{x,1,20}];
Indexgc=Table[{Quotient[x-1,6]+2,Quotient[Mod[x-1,6],3]*2+2,Mod[x-1,3]+1},{x,1,24}];
Indexzc=Table[{Quotient[x-1,2]+1,2*Mod[x-1,2]+2},{x,1,20}];



J[{z0_,z1_}]:={-Conjugate[z1],Conjugate[z0]}


EE={{2,1},{1,3},{4,1},{1,5},{3,2},{2,4},{5,2},{4,3},{3,5},{5,4}};
EE=Map[Sort[#,Less]&,EE];
Index=Join[Indexg,Indexz,EE,Indexgc,Indexzc,EE];
\[Eta]=DiagonalMatrix[{-1,1,1,1}];


MEXP[x_,y_]:=Block[{z1,z2,z3,out},z1=1+(x[[1]]+I*y[[1]])/Sqrt[2];z2=(x[[2]]+I*y[[2]])/Sqrt[2];z3=(x[[3]]+I*y[[3]])/Sqrt[2];out={{z1,z2},{z3,(1+z2 z3)/z1}}];



GG[x_]:=MEXP[x[[1]],x[[2]]];



CGG[x_]:=Transpose[MEXP[x[[3]],x[[4]]]];




PC1=Table[If[EE[[x,1]]==EE[[y,1]]&&EE[[x,2]]==EE[[y,2]],1,0],{x,1,10},{y,1,10}];
PC2=Table[If[(EE[[x,1]]==EE[[y,1]]&&EE[[x,2]]!=EE[[y,2]])||(EE[[x,2]]==EE[[y,2]]&&EE[[x,1]]!=EE[[y,1]]),1,0],{x,1,10},{y,1,10}];
PC3=Table[If[EE[[x,1]]!=EE[[y,1]]&&EE[[x,2]]!=EE[[y,2]],1,0],{x,1,10},{y,1,10}];



Alpha=AP[[1]]*PC1+AP[[2]]*PC2+AP[[3]]*PC3;



Action1[aax_]:=Block[{aa,spin,xx,zz,CGv,Gv,zzv,czzv,Zv,CZv,Zv2,XZ,ZX,A,B,SC,S},
aa=aax;
zz=Table[0,{c,1,10},{a,1,4}];
xx=Table[0,{c,1,5},{a,1,4},{b,1,3}];
spin=Table[0,{a,1,5},{b,1,5}];
Table[xx[[Index[[a,1]],Index[[a,2]],Index[[a,3]]]]=aa[[a]],{a,1,24}];
Table[xx[[Index[[a,1]],Index[[a,2]],Index[[a,3]]]]=aa[[a]],{a,55,55+23}];
Table[zz[[Index[[a,1]],Index[[a,2]]]]=aa[[a]],{a,25,44}];
Table[spin[[Index[[a,1]],Index[[a,2]]]]=aa[[a]],{a,45,54}];
Table[zz[[Index[[a,1]],Index[[a,2]]]]=aa[[a]],{a,79,79+19}];
Table[spin[[Index[[a,1]],Index[[a,2]]]]=aa[[a]],{a,99,108}];
Gv=Table[gv[[a]].GG[xx[[a]]],{a,1,5}];
CGv=Table[CGG[xx[[a]]].ConjugateTranspose[gv[[a]]],{a,1,5}];
zzv=Table[{{0},{0}},{a,1,5},{b,1,5}];
czzv=Table[{{0},{0}},{a,1,5},{b,1,5}];
Table[zzv[[EE[[a,1]],EE[[a,2]]]]={{1},{zv[[EE[[a,1]],EE[[a,2]]]][[2,1]]+(zz[[a,1]]+I*zz[[a,2]])}},{a,1,10}];
Table[zzv[[EE[[a,2]],EE[[a,1]]]]=zzv[[EE[[a,1]],EE[[a,2]]]],{a,1,10}];
Table[czzv[[EE[[a,1]],EE[[a,2]]]]={{1},{Conjugate[zv[[EE[[a,1]],EE[[a,2]]]][[2,1]]]+(zz[[a,3]]+I*zz[[a,4]])}},{a,1,10}];
Table[czzv[[EE[[a,2]],EE[[a,1]]]]=czzv[[EE[[a,1]],EE[[a,2]]]],{a,1,10}];
Zv=Table[If[x==y,{0,0},CGv[[x]].zzv[[x,y]]],{x,1,5},{y,1,5}];
CZv=Table[If[x==y,{0,0},Transpose[czzv[[x,y]]].Gv[[x]]],{x,1,5},{y,1,5}];
Zv2=Table[If[x==y,1,(CZv[[x,y]].Zv[[x,y]])[[1,1]]],{x,1,5},{y,1,5}];
XZ=Table[If[x==y,1,(ConjugateTranspose[J[xv[[x,y]]]].Zv[[x,y]])[[1,1]]],{x,1,5},{y,1,5}];
ZX=Table[If[x==y,1,(CZv[[x,y]].xv[[x,y]])[[1,1]]],{x,1,5},{y,1,5}];
A=Table[Log[(ZX[[x,y]]*XZ[[y,x]])]-(1/2)*Log[(Zv2[[x,y]]*Zv2[[y,x]])],{x,1,5},{y,1,5}];
B=Table[((1/2)*Log[Zv2[[y,x]]/Zv2[[x,y]]])*I*\[Gamma],{x,1,5},{y,1,5}];
SC=A+B;
2*SC
]




aax=SetPrecision[Table[0,{x,1,108}],er];



pp=-I*Action1[aax]/\[Gamma];



Action2[aax_]:=Block[{aa,xx,zz,cspin,cspin1,spin,spin1,spin2,CGv,Gv,zzv,czzv,Zv,CZv,Zv2,XZ,ZX,A,B,SC,S,C1,C2},
aa=aax;
zz=Table[0,{c,1,10},{a,1,4}];
xx=Table[0,{c,1,5},{a,1,4},{b,1,3}];
spin=Table[0,{a,1,10}];
cspin=Table[0,{a,1,10}];
Table[xx[[Index[[a,1]],Index[[a,2]],Index[[a,3]]]]=aa[[a]],{a,1,24}];
Table[xx[[Index[[a,1]],Index[[a,2]],Index[[a,3]]]]=aa[[a]],{a,55,55+23}];
Table[zz[[Index[[a,1]],Index[[a,2]]]]=aa[[a]],{a,25,44}];
Table[spin[[a-44]]=(aa[[a]]+I*aa[[a+54]])+arv[[EE[[a-44,1]],EE[[a-44,2]]]],{a,45,54}];
Table[cspin[[a-44]]=(aa[[a]]-I*aa[[a+54]])+arv[[EE[[a-44,1]],EE[[a-44,2]]]],{a,45,54}];
Table[zz[[Index[[a,1]],Index[[a,2]]]]=aa[[a]],{a,79,79+19}];
Gv=Table[gv[[a]].GG[xx[[a]]],{a,1,5}];
CGv=Table[CGG[xx[[a]]].ConjugateTranspose[gv[[a]]],{a,1,5}];
zzv=Table[{{0},{0}},{a,1,5},{b,1,5}];
czzv=Table[{{0},{0}},{a,1,5},{b,1,5}];Table[zzv[[EE[[a,1]],EE[[a,2]]]]={{1},{zv[[EE[[a,1]],EE[[a,2]]]][[2,1]]+(zz[[a,1]]+I*zz[[a,2]])}},{a,1,10}];Table[zzv[[EE[[a,2]],EE[[a,1]]]]=zzv[[EE[[a,1]],EE[[a,2]]]],{a,1,10}];
Table[czzv[[EE[[a,1]],EE[[a,2]]]]={{1},{Conjugate[zv[[EE[[a,1]],EE[[a,2]]]][[2,1]]]+(zz[[a,3]]+I*zz[[a,4]])}},{a,1,10}];Table[czzv[[EE[[a,2]],EE[[a,1]]]]=czzv[[EE[[a,1]],EE[[a,2]]]],{a,1,10}];Zv=Table[If[x==y,{0,0},CGv[[x]].zzv[[x,y]]],{x,1,5},{y,1,5}];
CZv=Table[If[x==y,{0,0},Transpose[czzv[[x,y]]].Gv[[x]]],{x,1,5},{y,1,5}];
Zv2=Table[If[x==y,1,(CZv[[x,y]].Zv[[x,y]])[[1,1]]],{x,1,5},{y,1,5}];
XZ=Table[If[x==y,1,(ConjugateTranspose[J[xv[[x,y]]]].Zv[[x,y]])[[1,1]]],{x,1,5},{y,1,5}];
ZX=Table[If[x==y,1,(CZv[[x,y]].xv[[x,y]])[[1,1]]],{x,1,5},{y,1,5}];
A=Table[Log[(ZX[[x,y]]*XZ[[y,x]])]-(1/2)*Log[(Zv2[[x,y]]*Zv2[[y,x]])],{x,1,5},{y,1,5}];
B=Table[((1/2)*Log[Zv2[[y,x]]/Zv2[[x,y]]])*I*\[Gamma],{x,1,5},{y,1,5}];
spin1=Table[(spin[[a]]-arv[[EE[[a,1]],EE[[a,2]]]])/Sqrt[arv[[EE[[a,1]],EE[[a,2]]]]],{a,1,10}];
cspin1=Table[(cspin[[a]]-arv[[EE[[a,1]],EE[[a,2]]]])/Sqrt[arv[[EE[[a,1]],EE[[a,2]]]]],{a,1,10}];
spin2=Table[(spin[[a]]-arv[[EE[[a,1]],EE[[a,2]]]]),{a,1,10}];
C1=-(1/2)*\[Gamma]*({spin1}.Alpha.Transpose[{spin1}])[[1,1]];
C2=\[Gamma]*I*Sum[spin2[[a]]*pp[[Index[[a+44,1]],Index[[a+44,2]]]],{a,1,10}];
SC=A+B;S=Sum[2*spin[[x]]*SC[[EE[[x,1]],EE[[x,2]]]],{x,1,10}];
(S+C1-C2)
]



Action3[aax_]:=Block[{aa,xx,zz,spin,spin1,spin2,CGv,Gv,zzv,czzv,Zv,CZv,Zv2,XZ,ZX,A,B,SC,S,C1,C2,ZM,GM,CCC,sspin},
aa=aax;
zz=Table[0,{c,1,10},{a,1,4}];
xx=Table[0,{c,1,5},{a,1,4},{b,1,3}];
spin=Table[0,{a,1,5},{b,1,5}];
sspin=Table[0,{a,1,5},{b,1,5}];
Table[xx[[Index[[a,1]],Index[[a,2]],Index[[a,3]]]]=aa[[a]],{a,1,24}];
Table[xx[[Index[[a,1]],Index[[a,2]],Index[[a,3]]]]=aa[[a]],{a,55,55+23}];
Table[zz[[Index[[a,1]],Index[[a,2]]]]=aa[[a]],{a,25,44}];
Table[spin[[EE[[a-44,1]],EE[[a-44,2]]]]=(aa[[a]]+I*aa[[a+54]])*\[Lambda]+Arv[[EE[[a-44,1]],EE[[a-44,2]]]],{a,45,54}];
Table[spin[[EE[[a-44,2]],EE[[a-44,1]]]]=(aa[[a]]+I*aa[[a+54]])*\[Lambda]+Arv[[EE[[a-44,1]],EE[[a-44,2]]]],{a,45,54}];
Table[sspin[[EE[[a-44,1]],EE[[a-44,2]]]]=(aa[[a]]+I*aa[[a+54]])+arv[[EE[[a-44,1]],EE[[a-44,2]]]],{a,45,54}];
Table[sspin[[EE[[a-44,2]],EE[[a-44,1]]]]=(aa[[a]]+I*aa[[a+54]])+arv[[EE[[a-44,1]],EE[[a-44,2]]]],{a,45,54}];
Table[zz[[Index[[a,1]],Index[[a,2]]]]=aa[[a]],{a,79,79+19}];
Gv=Table[gv[[a]].GG[xx[[a]]],{a,1,5}];
CGv=Table[CGG[xx[[a]]].ConjugateTranspose[gv[[a]]],{a,1,5}];
zzv=Table[{{0},{0}},{a,1,5},{b,1,5}];
czzv=Table[{{0},{0}},{a,1,5},{b,1,5}];Table[zzv[[EE[[a,1]],EE[[a,2]]]]={{1},{zv[[EE[[a,1]],EE[[a,2]]]][[2,1]]+(zz[[a,1]]+I*zz[[a,2]])}},{a,1,10}];Table[zzv[[EE[[a,2]],EE[[a,1]]]]=zzv[[EE[[a,1]],EE[[a,2]]]],{a,1,10}];
Table[czzv[[EE[[a,1]],EE[[a,2]]]]={{1},{Conjugate[zv[[EE[[a,1]],EE[[a,2]]]][[2,1]]]+(zz[[a,3]]+I*zz[[a,4]])}},{a,1,10}];
Table[czzv[[EE[[a,2]],EE[[a,1]]]]=czzv[[EE[[a,1]],EE[[a,2]]]],{a,1,10}];
Zv=Table[If[x==y,{0,0},CGv[[x]].zzv[[x,y]]],{x,1,5},{y,1,5}];
CZv=Table[If[x==y,{0,0},Transpose[czzv[[x,y]]].Gv[[x]]],{x,1,5},{y,1,5}];
Zv2=Table[If[x==y,1,(CZv[[x,y]].Zv[[x,y]])[[1,1]]],{x,1,5},{y,1,5}];
XZ=Table[If[x==y,1,(ConjugateTranspose[J[xv[[x,y]]]].Zv[[x,y]])[[1,1]]],{x,1,5},{y,1,5}];
ZX=Table[If[x==y,1,(CZv[[x,y]].xv[[x,y]])[[1,1]]],{x,1,5},{y,1,5}];
B=Table[If[x==y,0,
		If[x<y,
		\[Gamma]*sspin[[x,y]]((CZv[[x,y]].PauliMatrix[t].xv[[x,y]])[[1,1]])/((CZv[[x,y]].xv[[x,y]])[[1,1]])
		,
		-\[Gamma]*sspin[[x,y]]((ConjugateTranspose[J[xv[[x,y]]]].(PauliMatrix[t].Zv[[x,y]]))[[1,1]])/((ConjugateTranspose[J[xv[[x,y]]]].Zv[[x,y]])[[1,1]])
		]
		],{t,1,3},{x,1,5},{y,1,5}];
A=Table[Total[Table[B[[x,a,b]]*B[[x,a,c]],{x,1,3}]],{a,1,5},{b,1,5},{c,1,5}];
ZM=Product[1/(Zv2[[EE[[t,1]],EE[[t,2]]]]*Zv2[[EE[[t,2]],EE[[t,1]]]]),{t,1,10}];
GM=Product[1/(CGG[xx[[a]]][[1,1]]*GG[xx[[a]]][[1,1]]),{a,1,4}];
CCC=(1/(2^7*Pi^4))^4*((Sqrt[1+I*\[Gamma]])/(Pi*Sqrt[1-I*\[Gamma]]))^10;
{A,Product[(2*spin[[EE[[t,1]],EE[[t,2]]]]+1),{t,1,10}]*ZM*GM,CCC,B}
]




DerSym[x_,aa_]:=Block[{aa1=aa,cmc},
aa1[[x]]=SetPrecision[cmc,er];
D[Action2[aa1],cmc]/.cmc->SetPrecision[aa[[x]],er]
]



DerSym2[x_,y_,aa_]:=Block[{aa1=aa,cmc1,cmc2,re,res},
If[x==y,{aa1[[x]]=cmc1;re=D[D[Action2[aa1],cmc1],cmc1]/.cmc1->aa[[x]]},{aa1[[x]]=cmc1;aa1[[y]]=cmc2;re=D[D[Action2[aa1],cmc1],cmc2];}];
	re/.{cmc1->aa[[x]],cmc2->aa[[y]]}
]



If[FileExistsQ[tempfile6],{Get[tempfile6];},{
AZ=Table[ToExpression["X"<>ToString[x]],{x,1,108}];
F1[ppp_]:=Flatten[{-Re[Table[D[Action2[AZ],AZ[[x]]],{x,1,54}]],Im[Table[D[Action2[AZ],AZ[[x]]],{x,1,54}]]}]/.Table[AZ[[a]]->ppp[[a]],{a,1,108}];
F2[ppp_]:=Table[D[D[Action2[AZ],AZ[[x]]],AZ[[y]]],{x,1,54},{y,1,54}]/.Table[AZ[[a]]->ppp[[a]],{a,1,108}];
F3[ppp_]:=\[Lambda]*Action2[AZ]/.Table[AZ[[a]]->ppp[[a]],{a,1,108}];
P4[ppp_]:=Action3[ppp][[1]];
P5[ppp_]:=TensorProduct[Action3[ppp][[1]],Action3[ppp][[1]]];
P6[ppp_]:=Action3[ppp][[2]];
P1=Compile[{{ppp,_Real,1}},Evaluate[F1[ppp]],RuntimeAttributes->{Listable},Parallelization->True,CompilationTarget->"C"]//Quiet;
P2=Compile[{{ppp,_Real,1}},Evaluate[F2[ppp]],RuntimeAttributes->{Listable},Parallelization->True,CompilationTarget->"C"]//Quiet;
P3=Compile[{{ppp,_Real,1}},Evaluate[F3[ppp]],RuntimeAttributes->{Listable},Parallelization->True,CompilationTarget->"C"]//Quiet;
DumpSave[tempfile6,{P1,P2,P3,P4,P5,P6}];//Quite;
}]

CloseKernels[];
LaunchKernels[ToExpression[$ScriptCommandLine[[4]]]];

If[FileExistsQ[tempfile3],Get[tempfile3], {TH=P2[aax];HE=ArrayFlatten[{{Re[TH],-Im[TH]},{-Im[TH],-Re[TH]}}]*\[Lambda];{AB,AT}=Eigensystem[HE];AC=Pick[AT,Negative[AB]];
AB=-Pick[AB,Negative[AB]];
Save[tempfile3,{AC,AB}];
}];



Ps[aax_]:=Block[{a},
a=Table[DerSym[x,aax],{x,1,54}];
Flatten[{-Re[a],Im[a]}]
]


oe=SetPrecision[Table[0,{x,1,108}],er];
IOI=Action2[oe];


HESS=Table[AC[[x,y]]+I*AC[[x,y+54]],{x,1,54},{y,1,54}];



AZ=Table[ToExpression["X"<>ToString[x]],{x,1,108}];


AZ1=Table[ToExpression["X"<>ToString[x]<>"["<>ToString[0]<>"]"],{x,1,108}];


AZ2=Table[Unevaluated[ToExpression["X"<>ToString[x]<>"["<>ToString[t]<>"]"]],{x,1,108}];


AZ3=Table[ToExpression["X"<>ToString[x]<>"'["<>ToString[t]<>"]"],{x,1,108}];
AA4=Flatten[{ParallelTable[AZ3[[a]]==(((Ps[AZ2]))[[a]]),{a,1,108}]}];//Quiet


LefsT[px_,HESA_,TS_]:=Block[{temp1={px},AA2,Sol,HH,HSD=HESA,i,PF1,GG,A1,A2,kkp,dds},
dds=SetPrecision[TS/100,er];
AA2=Table[AZ1[[a]]==px[[a]],{a,1,108}];
HH=Catch[Check[Sol=NDSolve[{AA4,AA2},AZ,{t,0,TS},Method->{"EquationSimplification"->"Residual"},Method->{"StiffnessSwitching", Method->{Automatic, "ImplicitRungeKutta"},PrecisionGoal->13,AccuracyGoal->13}],Throw[error]]];
If[HH===error,{PF1=0;HSD={{1}};temp1={1};},{
PF1=1;
GG=(AZ/.Sol)[[1]];
temp1=Table[Table[GG[[x]][y],{x,1,108}],{y,0,TS,dds}];
kkp=Re[P3[temp1]];
If[Abs[Im[P3[First[temp1]]]-Im[P3[Last[temp1]]]]>10^(-2),{PF1=2;},{
HSD={{1}};
}];
}];

{PF1,Det[HSD],First[temp1],Last[temp1]}
]


If[FileExistsQ[tempfile11],{Get[tempfile11]},{
CS={};h=1;cc=1;dct=TT;
BFBF=LefsT[aax+AC[[cc]]*h*10^(nt),HESS,dct]//Quiet;
While[BFBF[[1]]!=1,{
nt--;
BFBF=LefsT[aax+AC[[cc]]*h*10^(nt),HESS,dct]//Quiet;
}];
xx=1; 
While[xx<=Length[AC],{
BFBF=LefsT[aax+AC[[xx]]*h*10^(nt),HESS,dct]//Quiet;
If[Re[P3[BFBF[[4]]]]>-1&&BFBF[[1]]!=0,{
While[Re[P3[BFBF[[4]]]]>-10,{
nt++;
BFBF=LefsT[aax+AC[[xx]]*h*10^(nt),HESS,dct]//Quiet;
}];
nt--;
},{
If[Re[P3[BFBF[[4]]]]<-10||BFBF[[1]]==0,{
While[Re[P3[BFBF[[4]]]]<-1||BFBF[[1]]==0,{
nt--;
BFBF=LefsT[aax+AC[[xx]]*h*10^(nt),HESS,dct]//Quiet;
}];
}];
}];
PP=0;
nt--;
While[!IntervalMemberQ[Interval[{-15,-0.1}],Re[P3[BFBF[[4]]]]],{
If[BFBF[[1]]==0,{nt--;}];
h+=1;
PP++;
BFBF=LefsT[aax+AC[[xx]]*h*10^(nt),HESS,dct]//Quiet;
If[PP>500,Break[]];
}];
AppendTo[CS,(h)*10^(nt)];
h=1;
xx++;
}];
DumpSave[tempfile11,{TT,CS,dct,nt}];
}]




Lefs[px_,HESA_]:=Block[{temp1={px},AA2,Sol,HH,HSD=HESA,i,PF1,GG,A1,A2,kkp,dds},
dds=TT/2000;
AA2=Table[AZ1[[a]]==px[[a]],{a,1,108}];
HH=Catch[Check[Sol=NDSolve[{AA4,AA2},AZ,{t,0,TT},Method->{"EquationSimplification"->"Residual"},Method->{"StiffnessSwitching", Method->{Automatic, "ImplicitRungeKutta"}},PrecisionGoal->13,AccuracyGoal->13],Throw[error]]];
If[HH===error,{PF1=1;HSD={{1}};temp1={1};},{
PF1=0;
GG=(AZ/.Sol)[[1]];
temp1=Table[Table[GG[[x]][y],{x,1,108}],{y,0,TT,dds}];
kkp=Re[P3[temp1]];
If[Abs[Im[P3[First[temp1]]]-Im[P3[Last[temp1]]]]>10^(-5),{PF1=1;},{
	HH=Chop[P2[temp1],10^(-15)];
	For[i=0,i<Length[temp1]-1,i++,{
		HSD+=-(Conjugate[HH[[i+1]]].Conjugate[HSD])*dds;
	}];
}];
}];
{1-PF1,Det[HSD],First[temp1],Last[temp1]}
]


RKF45[a_,dt_]:=Block[{DS1,DS2,DS3,DS4,DS5,DS6,DS,DSP,RDAA},
DS1=P1[a];
RDAA=a+dt*(1/4)*DS1;
DS2=P1[RDAA];
RDAA=a+dt*((3/32)*DS1+(9/32)*DS2);
DS3=P1[RDAA];
RDAA=a+dt*((1932/2197)*DS1-(7200/2197)*DS2+(7296/2197)*DS3);
DS4=P1[RDAA];
RDAA=a+dt*((439/216)*DS1-8*DS2+(3680/513)*DS3-(845/4104)*DS4);
DS5=P1[RDAA];
RDAA=a+dt*(-(8/27)*DS1+2*DS2-(3544/2565)*DS3+(1859/4104)*DS4-(11/40)*DS5);
DS6=P1[RDAA];
DS=((25/216)*DS1+(1408/2565)*DS3+(2197/4104)*DS4-(1/5)*DS5)*dt;
DSP=((16/135)*DS1+(6656/12825)*DS3+(28561/56430)*DS4-(9/50)*DS5+2/55*DS6)*dt;

{DS,DSP}
]

RKF45M[a_,dt_,L_]:=Block[{DS1,DS2,DS3,DS4,DS5,DS6,DS,DSP,RDAA},
DS1=Exp[2 Re[P3[a]]/L]*P1[a];
RDAA=a+dt*(1/4)*DS1;
DS2=Exp[2 Re[P3[RDAA]]/L]*P1[RDAA];
RDAA=a+dt*((3/32)*DS1+(9/32)*DS2);
DS3=Exp[2 Re[P3[RDAA]]/L]*P1[RDAA];
RDAA=a+dt*((1932/2197)*DS1-(7200/2197)*DS2+(7296/2197)*DS3);
DS4=Exp[2 Re[P3[RDAA]]/L]*P1[RDAA];
RDAA=a+dt*((439/216)*DS1-8*DS2+(3680/513)*DS3-(845/4104)*DS4);
DS5=Exp[2 Re[P3[RDAA]]/L]*P1[RDAA];
RDAA=a+dt*(-(8/27)*DS1+2*DS2-(3544/2565)*DS3+(1859/4104)*DS4-(11/40)*DS5);
DS6=Exp[2 Re[P3[RDAA]]/L]*P1[RDAA];
DS=((25/216)*DS1+(1408/2565)*DS3+(2197/4104)*DS4-(1/5)*DS5)*dt;
DSP=((16/135)*DS1+(6656/12825)*DS3+(28561/56430)*DS4-(9/50)*DS5+2/55*DS6)*dt;

{DS,DSP}
]

STERKF[STE_,a_,dt_,e_]:=Block[{a1,a2,a3,DD,dtp,dt3,STS},
STS=STE+1;
a1=RKF45[a,dt];
a3=Abs[a1[[1]]-a1[[2]]];
If[Min[Abs[a3]]<10^(-20),{dtp=dt*Min[4,Max[1/8,10000^(1/5)]],DD=a1[[1]],dt3=dt},{
a2=Sqrt[Sum[(e*a[[x]]/(a3[[x]]))^2,{x,1,108}]*108];
If[a2>=1||STE>20,{dtp=dt*Min[4,Max[1/8,a2^(1/5)]];DD=a1[[1]];dt3=dt},{dtp=dt*a2^(1/5);{DD,dtp,dt3,STS}=STERKF[STS,a,dtp,e]}];
}];
{DD,dtp,dt3,STS}
]

STERKFM[STE_,a_,dt_,e_,l_]:=Block[{a1,a2,a3,DD,dtp,dt3,STS},
STS=STE+1;
a1=RKF45M[a,dt,l];
a3=Abs[a1[[1]]-a1[[2]]];
If[Min[Abs[a3]]<10^(-20),{dtp=dt*Min[4,Max[1/8,10000^(1/5)]],DD=a1[[1]],dt3=dt},{
a2=Sqrt[Sum[(e*a[[x]]/(a3[[x]]))^2,{x,1,108}]*108];
If[a2>=1||STE>50,{dtp=dt*Min[4,Max[1/8,a2^(1/5)]];DD=a1[[1]];dt3=dt},{dtp=dt*a2^(1/5);{DD,dtp,dt3,STS}=STERKF[STS,a,dtp,e]}];
}];
{DD,dtp,dt3,STS}
]

LefsM[aax_,HESS_]:=Block[{STE,e,px,T,DD,dt},
e=10^(-20);
px=aax;
T=0;
DD={px,TT/4000,TT/4000};
While[T<TT,{
STE=0;
DD=STERKF[STE,px,DD[[2]],e];
px+=DD[[1]];
dt=DD[[3]];
T+=dt;
}];
{1,1,aax,px}
]

LefsMH[aax_,HESS_]:=Block[{tx,e,px,T,DD,HSD=HESS,dt,HH,kps,ers,STE},
If[Abs[Im[P3[aax]]-\[Lambda]*Im[IOI]]>TOL,{
kps=0;
px=aax;
HSD={{1}};
tx=aax;
},{
kps=1;
e=10^(-20);
tx=aax;
px=aax;
If[Not[NumberQ[px[[1]]]],{kps=0;px=ConstantArray[0,108];HSD={{1}};tx=px;},{
ers=Im[Action2[px]];
T=0;
DD={px,TT/100,TT/100};
While[T<TT,{
STE=0;
DD=STERKF[STE,px,DD[[2]],e];
px+=DD[[1]];
If[Abs[Im[P3[px]]-\[Lambda]*ers]>TOL||dt<TT*10^(-6)||!NumberQ[px[[1]]],{kps=0;Break[];}];
HH=P2[px-DD[[1]]/2];
HSD+=-(Conjugate[HH.HSD])*dt;
dt=DD[[3]];
T+=dt;
}];
}];
}];
{kps,Det[HSD],tx,px}
]

LefsMHM[aax_,HESS_,l_]:=Block[{tx,e,px,T,DD,HSD=HESS,dt,HH,kps,ers,STE},
If[Abs[Im[P3[aax]]-\[Lambda]*Im[IOI]]>TOL,{
kps=0;
px=aax;
HSD={{1}};
tx=aax;
},{
kps=1;
e=10^(-20);
tx=aax;
px=aax;
If[Not[NumberQ[px[[1]]]],{kps=0;px=ConstantArray[0,108];HSD={{1}};tx=px;},{
ers=Im[Action2[px]];
T=0;
DD={px,TT/1000,TT/1000};
While[T<TT,{
STE=0;
DD=STERKFM[STE,px,DD[[2]],e,l];
px+=DD[[1]];
If[Abs[Im[P3[px]]-\[Lambda]*ers]>TOL||dt<TT*10^(-6)||!NumberQ[px[[1]]],{kps=0;Break[];}];
HH=P2[px-DD[[1]]/2];
HSD+=-(Conjugate[HH.HSD])*dt;
dt=DD[[3]];
T+=dt;
}];
}];
}];
{kps,Det[HSD],tx,px}
]

LefsA[aax_,HESS_,x_]:=Switch[x,
0,LefsMH[aax,HESS],
1,LefsMHM[aax,HESS,LL*\[Lambda]],
2,Lefs[aax,HESS]
]


G[a_,b_,c_,d_,m_,n_]:=J3[[m,a,b,n,c,d]]/J4-J2[[m,a,b]]/J4*(J2[[n,c,d]]/J4)


If[FileExistsQ[tempfile5],{
Get[tempfile5];
If[FileExistsQ[tempfile7],{
Get[tempfile7];
},{
J2=ParallelSum[Exp[I *Arg[DD2[[x,2]]]]*P6[DD2[[x,4]]]*P4[DD2[[x,4]]],{x,1,108}];
J3=ParallelSum[Exp[I *Arg[DD2[[x,2]]]]*P6[DD2[[x,4]]]*P5[DD2[[x,4]]],{x,1,108}];
J4=ParallelSum[Exp[I *Arg[DD2[[x,2]]]]*P6[DD2[[x,4]]],{x,1,108}];
DumpSave[tempfile7,{J2,J3,J4}];
}];
},{
DD1={};
TEMP1={};
TEMP2={};
px=ConstantArray[0,54];
For[i=1,i<55,i++,{
cx=px;
cy=px;
cx[[i]]=CS[[i]]*0.1;
cy[[i]]=-CS[[i]]*0.1;
AppendTo[DD1,cx.AC];
AppendTo[DD1,cy.AC];
}];
rs=1;
pc=Table[1/p,{x,1,p}];
DD2=ParallelTable[LefsMH[DD1[[x]],HESS],{x,1,108}];
TEMP1=Join[TEMP1,DD2[[1;;108,3]]];
TEMP2=Join[TEMP2,DD2[[1;;108,4]]];
J2=ParallelSum[Exp[I *Arg[DD2[[x,2]]]]*P6[DD2[[x,4]]]*P4[DD2[[x,4]]],{x,1,108}];
J3=ParallelSum[Exp[I *Arg[DD2[[x,2]]]]*P6[DD2[[x,4]]]*P5[DD2[[x,4]]],{x,1,108}];
J4=ParallelSum[Exp[I *Arg[DD2[[x,2]]]]*P6[DD2[[x,4]]],{x,1,108}];
DumpSave[tempfile5,{TEMP1,TEMP2,DD2,rs,pc}];DumpSave[tempfile7,{J2,J3,J4}];
}];




EFS[B_]:=(Log[Abs[B[[2]]]]+Re[P3[B[[4]]]]);


ZEO[B_]:=HeavisideTheta[TOL-Abs[Im[Action2[B[[4]]]]-Im[IOI]]*\[Lambda]];


ZEOs[B_]:=TOL-Abs[Im[Action2[B[[4]]]]-Im[IOI]]*\[Lambda];





ncr=p;


crl=Table[x/ncr,{x,1,p}];

UPSv[j_,DD2_,k_,cr_,r_]:=Block[{jk,up,dp,delta,gamma,e,eps,x1,x2,a,b,l,out},
jk=RandomVariate[UniformDistribution[{0,1}],54];
up=HeavisideTheta[cr-jk];
dp=Total[up];
If[dp==0,out=ConstantArray[0,108],{
delta=RandomInteger[{1,3}];
If[k==3,{gamma=r},{gamma=r*2.38/Sqrt[2*delta*dp]}];
e=RandomVariate[UniformDistribution[ConstantArray[{-0.005,0.005},54]]];
eps=RandomVariate[MultinormalDistribution[DiagonalMatrix[CS]*10^(-50)]];
x1=ConstantArray[0,54];
x2=ConstantArray[0,54];
For[l=1,l<delta+1,l++,{
a=RandomInteger[{1,108}];
b=RandomInteger[{1,108}];
While[a==b||a==j||b==j,{
a=RandomInteger[{1,108}];
b=RandomInteger[{1,108}];
}];
x1+=AC.DD2[[a,3]];
x2+=AC.DD2[[b,3]];
}];
out=(((ConstantArray[1,54]+e)*gamma*(x1-x2)+eps)*up).AC;
}];
out
]

(*UPS[j_,DD2_,k_,cr_]:=Block[{jk,up,dp,delta,gamma,e,eps,x1,x2,a,b,i,out},
jk=RandomVariate[UniformDistribution[{0,1}],54];
up=HeavisideTheta[cr-jk];
dp=Total[up];
If[dp==0,out=ConstantArray[0,108],{
delta=RandomInteger[{1,3}];
If[k==0,{gamma=0.5},{gamma=0.2*2.38/Sqrt[2*delta*dp]}];
e=RandomVariate[UniformDistribution[ConstantArray[{-0.005,0.005},54]]];
eps=RandomVariate[MultinormalDistribution[DiagonalMatrix[CS]*10^(-50)]];
x1=ConstantArray[0,54];
x2=ConstantArray[0,54];
For[i=1,i<delta+1,i++,{
a=RandomInteger[{1,108}];
b=RandomInteger[{1,108}];
While[a==b||a==j||b==j,{
a=RandomInteger[{1,108}];
b=RandomInteger[{1,108}];
}];
x1+=AC.DD2[[a,3]];
x2+=AC.DD2[[b,3]];
}];
out=(((ConstantArray[1,54]+e)*gamma*(x1-x2)+eps)*up).AC;
}];
out
]*)



delta=Table[0,{x,1,p}];
L=Table[0,{x,1,p}];

If[FileExistsQ[tempfile12],{Get[tempfile12];},{
If[FileExistsQ[tempfile5],{Get[tempfile5];}];
For[j=0,j<cys,j++,{
For[i=0,i<1000,i++,{
kcr=Table[Position[RandomVariate[MultinomialDistribution[1,pc]],1][[1,1]],{x,1,108}];
cr=Table[crl[[kcr[[x]]]]//N,{x,1,108}];
upc=Table[UPSv[x,DD2,Mod[i,5],cr[[x]],rs],{x,1,108}];
DD1=Table[DD2[[x,3]]+upc[[x]],{x,1,108}];
DDc=ParallelTable[LefsA[DD1[[x]],HESS,SEL],{x,1,108},Method->"CoarsestGrained"];
JK=RandomVariate[UniformDistribution[{0,1}],108];
JP=Map[Min[1,#]&,Exp[Clip[-Map[EFS[#]&,DD2]+Map[EFS[#]&,DDc],{-100,3}]]*Map[ZEO[#]*#[[1]]&,DDc]];
upe=HeavisideTheta[JP-JK];acr=Total[upe]/108;
If[acr<0.1&&Mod[i,5]!=3&&Total[Map[ZEO[#]&,DDc]]>20,rs*=0.5;];
If[acr>0.4&&Mod[i,5]!=3&&Total[Map[ZEO[#]&,DDc]]>20,rs*=1.2;];
DD3=Table[(1-upe[[x]])*DD2[[x]]+upe[[x]]*DDc[[x]],{x,1,108}];
JU=Table[Sum[(AC[[x]].DD2[[k,3]]-AC[[x]].DD3[[k,3]])^2/StandardDeviation[Table[AC[[x]].DD2[[y,3]],{y,1,108}]]^2,{x,1,54}],{k,1,108}];
DD2=DD3;
Do[L[[kcr[[x]]]]++,{x,1,108}];
Do[delta[[kcr[[x]]]]+=JU[[x]],{x,1,108}];
If[Mod[i,5]!=3,{
pc=Table[If[delta[[x]]!=0,delta[[x]]/L[[x]],0],{x,1,p}];
pc=pc/Total[pc];
}];
}];
DumpSave[tempfile5,{TEMP1,TEMP2,DD2,rs,pc}];DumpSave[tempfile7,{J2,J3,J4}];
SendMail[
"To"->Email[[5]],
"Subject"->"Progress of test Lambda(AMMC)="<>ToString[\[Lambda]]<>" with t ="<>ToString[TT],
"Body"->{"Burning in is ongoing ",
rs,i,j,
Total[upe]/108//N,
Map[ZEO[#]*#[[1]]&,DDc]
},
"From"->Email[[1]],
"Server"->Email[[2]],
"UserName"->Email[[3]],
"Password"->Email[[4]],
"PortNumber"->25,
"EncryptionProtocol"->"StartTLS",
"ServerAuthentication"->True
];
}];


TEMP1={};
TEMP2={};
TEMP1=Join[TEMP1,DD2[[1;;108,3]]];
TEMP2=Join[TEMP2,DD2[[1;;108,4]]];
J2=ParallelSum[Exp[I *Arg[DD2[[x,2]]]]*P6[DD2[[x,4]]]*P4[DD2[[x,4]]],{x,1,108}];
J3=ParallelSum[Exp[I *Arg[DD2[[x,2]]]]*P6[DD2[[x,4]]]*P5[DD2[[x,4]]],{x,1,108}];
J4=ParallelSum[Exp[I *Arg[DD2[[x,2]]]]*P6[DD2[[x,4]]],{x,1,108}];
DumpSave[tempfile5,{TEMP1,TEMP2,DD2,rs,pc}];DumpSave[tempfile7,{J2,J3,J4}];
DumpSave[tempfile12,{pc,rs}];


SendMail[
"To"->Email[[5]],
"Subject"->"Progress of test Lambda(AMMC)="<>ToString[\[Lambda]]<>" with t ="<>ToString[TT],
"Body"->{"Burning in has been finished ",
pc,
Total[upe]/108//N
},
"AttachedFiles"->{File[tempfile12]},
"From"->Email[[1]],
"Server"->Email[[2]],
"UserName"->Email[[3]],
"Password"->Email[[4]],
"PortNumber"->25,
"EncryptionProtocol"->"StartTLS",
"ServerAuthentication"->True
];
}]
TEK={};
If[FileExistsQ[tempfile8],{Get[tempfile8];}];

For[z=1,z<100,z++,{
For[j=0,j<1600,j++,{
kcr=Table[Position[RandomVariate[MultinomialDistribution[1,pc]],1][[1,1]],{x,1,108}];
cr=Table[crl[[kcr[[x]]]]//N,{x,1,108}];
upc=Table[UPSv[x,DD2,Mod[j,5],cr[[x]],rs],{x,1,108}];
DD1=Table[DD2[[x,3]]+upc[[x]],{x,1,108}];
DDc=ParallelTable[LefsA[DD1[[x]],HESS,SEL],{x,1,108},Method->"CoarsestGrained"];
JK=RandomVariate[UniformDistribution[{0,1}],108];
JP=Map[Min[1,#]&,Exp[Clip[-Map[EFS[#]&,DD2]+Map[EFS[#]&,DDc],{-100,3}]]*Map[ZEO[#]*#[[1]]&,DDc]];
upe=HeavisideTheta[JP-JK];
DD2=Table[(1-upe[[x]])*DD2[[x]]+upe[[x]]*DDc[[x]],{x,1,108}];
J2+=ParallelSum[Exp[I *Arg[DD2[[x,2]]]]*P6[DD2[[x,4]]]*P4[DD2[[x,4]]],{x,1,108}];
J3+=ParallelSum[Exp[I *Arg[DD2[[x,2]]]]*P6[DD2[[x,4]]]*P5[DD2[[x,4]]],{x,1,108}];
J4+=ParallelSum[Exp[I *Arg[DD2[[x,2]]]]*P6[DD2[[x,4]]],{x,1,108}];
}];
TEK=Join[TEK,{J2,J3,J4}];
DumpSave[tempfile5,{TEMP1,TEMP2,DD2,rs,pc}];DumpSave[tempfile7,{J2,J3,J4}];DumpSave[tempfile8,{TEK}];
SendMail[
"To"->Email[[5]],
"Subject"->"Progress of test Lambda(AMMC)="<>ToString[\[Lambda]]<>" with t ="<>ToString[TT],
"Body"->{"Length of test MarKov Chain of Lambda= "<>ToString[\[Lambda]]<>" has reached "<>ToString[z*1600*108],
J3[[1,2,3,4,1,5]]/J4*\[Lambda]^4,
J2[[1,2,3]]/J4*\[Lambda]^2,
J2[[4,1,5]]/J4*\[Lambda]^2,
J3[[1,2,3,4,1,5]]/J4*\[Lambda]^4-(J2[[1,2,3]]/J4*\[Lambda]^2)*(J2[[4,1,5]]/J4*\[Lambda]^2),
Table[G[2,3,x,y,1,4]*\[Lambda]^4,{x,1,5},{y,1,5}]//MatrixForm,
Total[upe]/108//N,
Map[ZEO[#]*#[[1]]&,DDc]
},
"AttachedFiles"->{File[tempfile7]},
"From"->Email[[1]],
"Server"->Email[[2]],
"UserName"->Email[[3]],
"Password"->Email[[4]],
"PortNumber"->25,
"EncryptionProtocol"->"StartTLS",
"ServerAuthentication"->True
]
}];








W=ParallelSum[Variance[Table[(AC.TEMP1[[x]])[[54]],{x,y,Length[TEMP2],10}]],{y,1,108}]/108;
tt=ParallelTable[Mean[Table[(AC.TEMP1[[x]])[[54]],{x,y,Length[TEMP2],10}]],{y,1,108}]/108;


B=Length[TEMP2]/(107)*Total[(tt-Mean[tt])^2]


V=(Length[TEMP2]-1)/Length[TEMP2]*W+(108+1)/(108*Length[TEMP2])*B


Sqrt[(57/55)V/W]

Histogram[Re[P3[TEMP2]]]
Table[G[2,3,x,y,1,4]*\[Lambda]^4,{x,1,5},{y,1,5}]//MatrixForm


Table[G[3,3,x,y,1,1]*\[Lambda]^4,{x,1,5},{y,1,5}]//MatrixForm

