(* ::Package:: *)

(* ::Title:: *)
(*IntegrateRational*)


(* ::Subtitle:: *)
(*Sam Blake, 2024*)


(* ::Text:: *)
(*Started on 22 December 2023.*)


(* ::Input:: *)
(*(* References: *)
(* -- Symbolic Integration 1, Manuel Bronstein, Springer, 2004.*)
(**)
(*-- Algorithms for Computer Algebra, Geddes et al, 1992.*)
(**)
(*-- Algebraic Factoring and Rational Function Integration, B. M. Trager, ACM Symposium on Symbolic and Algebraic Computation, 1976.*)
(**)
(*  -- Symbolic Integration towards Practical Algorithms, Manuel Bronstein, IBM Research Division, T. J. Watson Research Center, Yorktown Heights, NY 10598 *)
(* *)*)


(* ::Subsection::Closed:: *)
(*IntegrateRational*)


(* ::Input::Initialization:: *)
$debug = False;
debugprint[h_, io_,args___]:= If[$debug, Print[Style[Row @ {h,Spacer[10], io, Spacer[25],Row @ Riffle[{args},", "]}, Gray]]]


(* ::Input::Initialization:: *)
ClearAll[IntegrateRational];

IntegrateRational::inexact = "Integrand contains inexact numbers which cannot be coerced to exact numbers with Rationalize[number].";

Options[IntegrateRational]={"PFD" -> True, Extension->Automatic,"LogToArcTan" -> True};

IntegrateRational[f_,x_,opts:OptionsPattern[]] /; (PolynomialQ[f,x] || rationalQ[f,x]) := With[{integrale=integrateRational[f,x,opts]},
integrale /; FreeQ[integrale, $Failed]
]


(* ::Input::Initialization:: *)
ClearAll[integrateRational];

Options[integrateRational]=Options[IntegrateRational];

integrateRational[f_,x_, opts:OptionsPattern[]] := Module[
{inexact=False,integrand=f, constants,intrat,intlog,integrale, 
params,assums, log, atan, atanh},

(* Handle inexact integrands. *)
If[Count[integrand,e_/;InexactNumberQ[e],{0,Infinity}]>0, 
inexact=True;
integrand=Rationalize[integrand];
If[Precision[integrand]=!=Infinity, 
Message[IntegrateRational::inexact];
Return[$Failed, Module]]
];

(* Normalise. *)
integrand=canonic[integrand];

(* Find all parameters and set param > 0 if no other conditions are given. *)
params=Select[DeleteCases[Variables[integrand],x]//Sort,Refine[#>0] === #>0&];
(* Map[(#/:Sign[#]=1)&,params]; *)
assums=Append[#>0&/@params, Greater @@ params];

(* (Possibly) integrate term-by-term. *)
{intrat,intlog}=Assuming[assums,IntegrateRealRationalFunction[integrand,x,opts]];

(* Collect in terms of the Log's and simplify (possibly algebraic number) coefficients. *)
If[FreeQ[intlog, Root|RootSum|Function],
intlog=Collect[intlog // Expand, _Log | _ArcTan | _ArcTanh, simproot]];

(* Convert sums of logarithms to ArcTanh. *)
If[OptionValue["LogToArcTan"],
intlog=LogToArcTanh[intlog, x];
intlog=LogToArcTan[intlog,x];
If[FreeQ[intlog, Root|RootSum|Function],
intlog=Collect[intlog // Expand, _Log | _ArcTan | _ArcTanh, simproot]]];

integrale=canonic[intrat]+intlog;

If[inexact, integrale=N[integrale]];

If[!FreeQ[integrale, ConditionalExpression], 
If[canonic[D[integrale/.ConditionalExpression->(#1&),x]-integrand]===0, 
integrale=integrale/.ConditionalExpression->(#1&)
]
];

ClearAll/@params;
integrale
]


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*IntegrateRational[(-9+7 x+x^2-6 x^4-8 x^5+5 x^6+5 x^7)/(-12+48 x+24 x^2-144 x^3-87 x^4+42 x^5+21 x^6),x]*)


(* ::Input:: *)
(*IntegrateRational[(-9+4 x)/(-160+100 x+528 x^2+562 x^3+360 x^4+132 x^5+18 x^6),x]*)


(* ::Input:: *)
(*IntegrateRational[(-7-4 x-5 x^2)/(20+70 x-84 x^2-193 x^3+36 x^4+103 x^5+89 x^6-120 x^7+30 x^8),x]*)


(* ::Input:: *)
(*IntegrateRational[(-3+10 x+3 x^2)/((3-6 x-5 x^2) (-2-2 x+2 x^2) (-9-x-5 x^2-7 x^3-8 x^4)),x]*)


(* ::Input:: *)
(*IntegrateRational[(10-9 x-4 x^2)/(-4-8 x+7 x^2+8 x^3-4 x^4),x]*)


(* ::Input:: *)
(*IntegrateRational[(-10-7 x+4 x^2)/(x-3 x^2+3 x^3+2 x^4),x]*)


(* ::Input:: *)
(*IntegrateRational[(-8+6 x-x^2)/(-7-2 x+4 x^2+8 x^3-2 x^4),x]*)


(* ::Input:: *)
(*IntegrateRational[x/(a^3+x^5)^2,x]*)


(* ::Input:: *)
(*(* IntegrateRational[(b x^3+3)/((a x^6-2b)^2(x-2)^4),x] *)*)


(* ::Input:: *)
(*IntegrateRational[(b x^3+1)/((x^6-Sqrt[2] a^3 b)^2 x^4),x]*)


(* ::Input:: *)
(*IntegrateRational[(4 Sqrt[5] a^6 b^2-3 Sqrt[10] a^3 b x^6+2 Sqrt[2] a^3 b^2 x^9+3 Sqrt[5] x^12)/(a^6 b^2 x^4 (Sqrt[2] a^3 b-x^6)^2),x]*)


(* ::Input:: *)
(*IntegrateRational[((Sqrt[5]-a x) (-2 a^3+Sqrt[6] x^6))/(a^3 b x^3 ((-2+Sqrt[6]) a^3+(-3+Sqrt[6]) x^6)),x]*)


(* ::Input:: *)
(*IntegrateRational[1/(x^6-a^3 x^3-a^6),x](* Far from optimal. *)*)


(* ::Input:: *)
(*IntegrateRational[(a x+2b)^2/(x (a x^4-2b^2 x^2)^2),x]*)


(* ::Input:: *)
(*IntegrateRational[(a x^3+2b)/(x^4 (a x^6-2b^2 x^3+c)),x]*)


(* ::Input:: *)
(*IntegrateRational[(a x^3+2b)/(a x^6-2b^2 x^2),x]*)


(* ::Input:: *)
(*IntegrateRational[(a x-b)/(a x^6-b x^2),x]*)


(* ::Input:: *)
(*(1/((x^2-a)(x^2+Sqrt[2]a)(x^2+Sqrt[3]a)^2))*)
(*(* IntegrateRational[%,x] *)*)


(* ::Input:: *)
(*(a x^4+b)/((x^2-a)^2 (x^2+2a)^2)*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*(a x^4+b)/((x^2-1)(b x^4+a))*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*(a x^4+b)/((x^2-1)(b x^4+a x^3))*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*IntegrateRational[(a x^4+b)/(b^4 x^4+a^4)^4,x]*)


(* ::Input:: *)
(*IntegrateRational[(a x^2+b)/(b^4 x^4-a^4),x]*)


(* ::Input:: *)
(*IntegrateRational[x^3/(x^16-x^8-1),x]*)


(* ::Input:: *)
(*(*IntegrateRational[(-28 Sqrt[21]+252 x^2-176 Sqrt[21] x^4+2556 x^6+2416 Sqrt[21] x^8+13020 x^10-8820 Sqrt[21] x^12)/(-49+28 Sqrt[21] x^2+854 x^4-128 Sqrt[21] x^6-7387 x^8-304 Sqrt[21] x^10+17598 x^12+2940 Sqrt[21] x^14-21609 x^16),x]*)*)


(* ::Input:: *)
(*IntegrateRational[(-16 (105-10 Sqrt[21]) x-2352 Sqrt[21] x^3-2352 Sqrt[21] x^5)/(16+(896-480 Sqrt[21]) x^2+(1708-560 Sqrt[21]) x^4-588 (14+5 Sqrt[21]) x^6+21609 x^8),x]*)


(* ::Input:: *)
(*IntegrateRational[(40 (-12+8 Sqrt[15]) x-5040 x^3+4200 Sqrt[15] x^5)/(4+(560-360 Sqrt[15]) x^2+5 (296-80 Sqrt[15]) x^4-2100 (2+Sqrt[15]) x^6+11025 x^8),x]*)


(* ::Input:: *)
(*(* IntegrateRational[(-Sqrt[5] (-24+2 Sqrt[21])-60 Sqrt[21] x-Sqrt[5] (-105-15 Sqrt[21]) x^2)/(-16+(-48 Sqrt[5]+4 Sqrt[105]) x+(50+60 Sqrt[21]) x^2+(-210 Sqrt[5]-30 Sqrt[105]) x^3+525 x^4),x] *)*)


(* ::Input:: *)
(*IntegrateRational[(x^3 (2+x^2))/(-2+Sqrt[3]+(-2+Sqrt[3]) x^2-x^4)^2,x]*)


(* ::Input:: *)
(*IntegrateRational[(-43-27 Sqrt[3]+(110+73 Sqrt[3]) x^2+(67+46 Sqrt[3]) x^4+(19+8 Sqrt[3]) x^6)/(1+(2+Sqrt[3]) x^2+x^4)^2,x](* This result contains unnecessarily large constants.  *)*)


(* ::Input:: *)
(*IntegrateRational[(-2-2 Sqrt[3] x-3 x^2+4 Sqrt[3] x^3+7 x^4+6 Sqrt[3] x^5+10 x^6+2 Sqrt[3] x^7+6 x^8+x^10)/(1+3 x^2+Sqrt[3] x^3+3 x^4+x^6)^2,x]*)


(* ::Input:: *)
(*IntegrateRational[(-43-27 Sqrt[3]+(110+73 Sqrt[3]) x^2+(67+46 Sqrt[3]) x^4+(19+8 Sqrt[3]) x^6)/(1+(2+Sqrt[3]) x^2+x^4)^2,x]*)


(* ::Input:: *)
(*IntegrateRational[(1/((3 Sqrt[3]-6 x+Sqrt[3] x^2+x^4)^3))(27 Sqrt[3]+(-108-54 Sqrt[3]) x+(81+54 Sqrt[3]) x^2-36 x^3+(18+3 Sqrt[3]) x^4-18 Sqrt[3] x^5+(9-12 Sqrt[3]) x^6+3 Sqrt[3] x^8+2 Sqrt[3] x^9+x^10),x]*)


(* ::Input:: *)
(*IntegrateRational[(2-9 x^4+2 Sqrt[2] x^4-12 x^6-3 x^8)/(Sqrt[2]-3 x^2-x^4)^3,x](* Compare to Mathematica and Rubi. *)*)


(* ::Input:: *)
(*IntegrateRational[(1/((Sqrt[2]-3 x^2-x^4)^3))(23038-15444 Sqrt[2]+(10530-5562 Sqrt[2]) x^2+(-51201+34026 Sqrt[2]) x^4+(-63033+41310 Sqrt[2]) x^6+(-29811+20142 Sqrt[2]) x^8+(-4779+3300 Sqrt[2]) x^10),x]*)


(* ::Input:: *)
(*IntegrateRational[(2 x-9 x^9+2 Sqrt[2] x^9-12 x^13-3 x^17)/((Sqrt[2]-3 x^4-x^8)^2 (-18+8 Sqrt[2]-24 x^4+27 Sqrt[2] x^4-8 x^8+9 Sqrt[2] x^8)),x]*)


(* ::Input:: *)
(*IntegrateRational[(-500+192 Sqrt[7]+952 x+360 Sqrt[7] x+672 x^2+252 Sqrt[7] x^2+196 x^3+84 Sqrt[7] x^3+49 x^4)/((16-6 Sqrt[7]+14 x+6 Sqrt[7] x+7 x^2)^2 (2 Sqrt[7]+630 x+238 Sqrt[7] x+147 x^2+56 Sqrt[7] x^2)),x]*)


(* ::Input:: *)
(*IntegrateRational[(199290375 x^3+21907179 x^11-10200897 x^19+464373 x^27-8127 x^35+49 x^43)/(243-63 x^8+x^16)^3,x]*)


(* ::Input:: *)
(*IntegrateRational[(624 x^3+144 x^7+24 x^11)/(-460-936 x^4-376 x^8-36 x^12-x^16)^2,x]//Timing*)


(* ::Input:: *)
(*IntegrateRational[((3-2 Sqrt[2]+x^2)^2 (-3+2 Sqrt[2]+x^2))/(577-408 Sqrt[2]+328 x^2-232 Sqrt[2] x^2+78 x^4-56 Sqrt[2] x^4+8 x^6-8 Sqrt[2] x^6+x^8),x]*)


(* ::Input:: *)
(*IntegrateRational[(Sqrt[5] (-24-2 Sqrt[21])+Sqrt[5] (-105+15 Sqrt[21]) x^2)/(-16+(-48 Sqrt[5]+4 Sqrt[105]) x+(50+60 Sqrt[21]) x^2+(-210 Sqrt[5]-30 Sqrt[105]) x^3+525 x^4),x]*)


(* ::Input:: *)
(*IntegrateRational[(5000 Sqrt[5] (1000 Sqrt[3]-750 Sqrt[5]) x+5000 Sqrt[5] (-5250 Sqrt[3]-3150 Sqrt[5]) x^3+65625000 Sqrt[15] x^5)/(-1937500+500000 Sqrt[15]+(3125000-625000 Sqrt[15]) x^2+(-88750000-4062500 Sqrt[15]) x^4+(164062500+32812500 Sqrt[15]) x^6-172265625 x^8),x]*)


(* ::Input:: *)
(*IntegrateRational[(3 Sqrt[7]+6 Sqrt[11]+(-121 Sqrt[105]-98 Sqrt[165]) x^4)/(-12 Sqrt[15]+10200 x^4-118580 Sqrt[15] x^8),x]*)


(* ::Input:: *)
(*IntegrateRational[(8 Sqrt[11]-2 Sqrt[165]-14 Sqrt[15] x+(-70 Sqrt[11]-14 Sqrt[165]) x^2+770 x^3+245 Sqrt[11] x^4)/(8 (-30+8 Sqrt[15])+8 (-650+40 Sqrt[15]) x^2+8 (3850+1015 Sqrt[15]) x^4-107800 x^6),x]*)


(* ::Input:: *)
(*IntegrateRational[((-20 Sqrt[3]+3 Sqrt[5]) x+36 Sqrt[5] x^3-75 Sqrt[3] x^5)/(8 Sqrt[3]+4 Sqrt[3] (70-90 Sqrt[15]) x^2+4 Sqrt[3] (140-25 Sqrt[15]) x^4+4 Sqrt[3] (-150-150 Sqrt[15]) x^6+1800 Sqrt[3] x^8),x]*)


(* ::Input:: *)
(*IntegrateRational[(15 x-2 Sqrt[3] x+12 Sqrt[3] x^3+3 Sqrt[3] x^5)/(-4-20 x^2+60 Sqrt[3] x^2-100 x^4+10 Sqrt[3] x^4+12 x^6+60 Sqrt[3] x^6-36 x^8),x]*)


(* ::Input:: *)
(*IntegrateRational[(12 (-420+1999 Sqrt[3]) x)/((-1999+140 Sqrt[3]) (-4-2 Sqrt[3]-6 x^2+6 Sqrt[3] x^2-9 x^4)),x]*)


(* ::Input:: *)
(*IntegrateRational[(8 (15-2 Sqrt[3]) x+96 Sqrt[3] x^3+24 Sqrt[3] x^5)/(-4+(-20+60 Sqrt[3]) x^2+(-100+10 Sqrt[3]) x^4+(12+60 Sqrt[3]) x^6-36 x^8),x]*)


(* ::Input:: *)
(*IntegrateRational[(-80 (-3+2 Sqrt[3]) x-80 (3-2 Sqrt[3]) x^3-80 (90+30 Sqrt[3]) x^5-960 Sqrt[3] x^7-7440 Sqrt[3] x^9-1440 Sqrt[3] x^11)/(14-8 Sqrt[3]+(-28+20 Sqrt[3]) x^2+(-1110-418 Sqrt[3]) x^4+(296-268 Sqrt[3]) x^6+(434-2401 Sqrt[3]) x^8+(204-828 Sqrt[3]) x^10+(-324-468 Sqrt[3]) x^12-432 x^14+648 x^16),x]*)


(* ::Input:: *)
(*IntegrateRational[(-16 (3-2 Sqrt[3]) x-16 (15-10 Sqrt[3]) x^3-16 (90+30 Sqrt[3]) x^5+960 Sqrt[3] x^7+240 Sqrt[3] x^9-1440 Sqrt[3] x^11)/(14-8 Sqrt[3]+(140-100 Sqrt[3]) x^2+(1578+158 Sqrt[3]) x^4+(440-820 Sqrt[3]) x^6+(1682+1343 Sqrt[3]) x^8+(-2580-540 Sqrt[3]) x^10+(3132-468 Sqrt[3]) x^12-2160 x^14+648 x^16),x]*)


(* ::Input:: *)
(*IntegrateRational[(8 (3-2 Sqrt[3]) x+24 Sqrt[3] x^5)/(-28+16 Sqrt[3]+(-20+20 Sqrt[3]) x^2+(-148-22 Sqrt[3]) x^4+(60+60 Sqrt[3]) x^6-36 x^8),x]*)


(* ::Input:: *)
(*IntegrateRational[(-2 (-1-Sqrt[3])-8 Sqrt[3] x-4 x^2)/(2-2 Sqrt[3]+(4+4 Sqrt[3]) x-4 Sqrt[3] x^2-8 x^3+4 x^4),x]*)


(* ::Input:: *)
(*IntegrateRational[(6-14 x+5 x^2)/(9-42 x+43 x^2-14 x^3+x^4),x]*)


(* ::Input:: *)
(*IntegrateRational[(x (-1+2 x^2+x^4))/(1+2 x^2+5 x^4+4 x^6+x^8),x]*)


(* ::Input:: *)
(*IntegrateRational[1/(2-4 x+6 x^2-4 x^3+x^4),x]*)


(* ::Input:: *)
(*IntegrateRational[(2-4 x+2 x^2+4 x^3+2 x^4-4 x^5+2 x^6)/(1-x^2+2 x^4-x^6+x^8),x]*)


(* ::Input:: *)
(*IntegrateRational[(8-8 x-8 x^5+8 x^8)/(1+x^4+x^8+x^12),x]*)


(* ::Input:: *)
(*IntegrateRational[(16 x^3-248 x^11+80 x^15-392 x^19-80 x^23+24 x^27)/(1+10 x^4+26 x^8+40 x^12+71 x^16+40 x^20+26 x^24+10 x^28+x^32),x]*)


(* ::Input:: *)
(*IntegrateRational[(-48 x^3-640 x^7-896 x^11-928 x^15-3960 x^19-1312 x^23+224 x^27)/(16+76 x^4+217 x^8+576 x^12+771 x^16+460 x^20+238 x^24+88 x^28+8 x^32),x]*)


(* ::Input:: *)
(*IntegrateRational[(-14 x+5 x^5)/(7-5 x^4+63 x^8),x]*)


(* ::Input:: *)
(*IntegrateRational[(-x^3+x^7)/(1369+9576 x^4+10164 x^8+7056 x^12+1764 x^16),x]*)


(* ::Input:: *)
(*IntegrateRational[(24 x-2304 x^3+4992 x^5+2304 x^7+1728 x^9+3072 x^11+1536 x^13-3072 x^15+384 x^17)/(-3-624 x^4-212 x^8+640 x^12+240 x^16+256 x^20+64 x^24),x]*)


(* ::Input:: *)
(*IntegrateRational[(x^2-1)/(x^4+5x^2+9),x]*)


(* ::Input:: *)
(*IntegrateRational[(136 x^3+1092 x^7+3136 x^11+508 x^15-192 x^19+20 x^23)/(-25-211 x^4-424 x^8-3 x^12+48 x^16-11 x^20+x^24),x]*)


(* ::Input:: *)
(*IntegrateRational[(320 x^3+16 x^5+384 x^7+104 x^9-64 x^11-8 x^13)/(-4-32 x^2-12 x^4-64 x^6-5 x^8+32 x^10+2 x^12+x^16),x]*)


(* ::Input:: *)
(*IntegrateRational[(2 x-4 x^3-x^5)/(4+32 x^2+4 x^4+x^8),x]*)


(* ::Input:: *)
(*IntegrateRational[(2 x+x^5)/(4-16 x^2+12 x^4-8 x^6+x^8),x]*)


(* ::Input:: *)
(*IntegrateRational[(128+896 x^2+896 x^4+128 x^6)/(-64 x-112 x^3-112 x^5+68 x^7-56 x^9+28 x^11-8 x^13+x^15),x]*)


(* ::Input:: *)
(*IntegrateRational[x^2/(2-4 x+2 x^2+x^4),x]*)


(* ::Input:: *)
(*IntegrateRational[(-21504 x^3-3072 x^11-63744 x^19-3840 x^27)/(-1024-192 x^8-8688 x^16-1632 x^24+72 x^32-12 x^40+x^48),x]*)


(* ::Input:: *)
(*IntegrateRational[(-8+24 x^4-272 x^8-252 x^12+244 x^16-296 x^20-16 x^24-12 x^28)/(1-2 x^4+69 x^8-236 x^12-34 x^16-114 x^20+4 x^24-8 x^28+x^32),x]*)


(* ::Input:: *)
(*(* IntegrateRational[(264 Sqrt[6] x-528 Sqrt[3] x^3+24 Sqrt[2] x^5-96 x^7+60 Sqrt[2] x^9-24 x^11)/(99+6 Sqrt[3] x^4-6 Sqrt[6] x^6+(-4+3 Sqrt[3]) x^8+8 Sqrt[2] x^10-12 x^12+4 Sqrt[2] x^14-x^16),x] *)*)


(* ::Input:: *)
(*(* IntegrateRational[(-36-2 Sqrt[6] x^2)/(9 Sqrt[3]+3 Sqrt[2] x^2-x^4),x] *)(* Why is this taking so long? *)*)


(* ::Input:: *)
(*IntegrateRational[x/(x^8+1),x,"PFD"->True,Extension->Sqrt[2]]*)


(* ::Input:: *)
(*IntegrateRational[(-2^(1/4)+Sqrt[3] x+2^(1/4) x^2)^2/(Sqrt[3]+2 2^(1/4) x)^3,x]*)


(* ::Input:: *)
(*IntegrateRational[(-6+9 x+3 x^2-5 x^3)/(4-4 x-3 x^2-10 x^3-x^4),x]*)


(* ::Input:: *)
(*IntegrateRational[(22 x-6 x^2-12 x^3-13 x^4+6 x^5)/(1+4 x^2-2 x^3-3 x^4-4 x^5+x^6),x]*)


(* ::Input:: *)
(*IntegrateRational[((3-2 Sqrt[2]+x^2)^2 (-3+2 Sqrt[2]+x^2))/(577-408 Sqrt[2]+328 x^2-232 Sqrt[2] x^2+78 x^4-56 Sqrt[2] x^4+8 x^6-8 Sqrt[2] x^6+x^8),x]*)


(* ::Input:: *)
(*IntegrateRational[1/(4.5x^4-3.5),x]*)


(* ::Input:: *)
(*(* IntegrateRational[1/(4.5x^4-N[Pi]),x] *)*)


(* ::Input:: *)
(*IntegrateRational[(79-64 x)/(6+24 x+12 x^2-24 x^3),x]*)


(* ::Input:: *)
(*IntegrateRational[(1665+386 x+643 x^2)/(-6+x-6 x^3+x^4),x]*)


(* ::Input:: *)
(*IntegrateRational[(-9-10 x)/(-5-3 x+5 x^2-3 x^3-5 x^4),x]*)


(* ::Input:: *)
(*IntegrateRational[(8-9 x-8 x^2)/(-5-6 x^2+2 x^4),x]*)


(* ::Input:: *)
(*IntegrateRational[(x^5 (3-24 x^2+63 x^4-54 x^6-2 x^12+4 x^14))/(1-12 x^2+54 x^4-108 x^6+81 x^8-3 x^12+18 x^14-27 x^16+x^24),x]*)


(* ::Input:: *)
(*IntegrateRational[(x^3 (2-15 x^2+36 x^4-27 x^6-4 x^8+6 x^10))/(1-12 x^2+54 x^4-108 x^6+80 x^8+6 x^10-9 x^12+x^16),x]*)


(* ::Input:: *)
(*IntegrateRational[(-324+972 x-633 x^2-252 x^3+324 x^4+33 x^5+108 x^6-216 x^7+32 x^9)/(1296-7776 x+17064 x^2-14904 x^3-179 x^4+8364 x^5-3186 x^6-1836 x^7+1329 x^8+144 x^9-216 x^10+16 x^12),x]*)


(* ::Input:: *)
(*IntegrateRational[(4 Sqrt[3]-12 Sqrt[3] x^2+4 x^3+33 Sqrt[3] x^4+24 x^5)/(4-8 Sqrt[3] x+12 x^2+12 Sqrt[3] x^3-27 x^4-16 Sqrt[3] x^5+27 x^6+24 Sqrt[3] x^7+16 x^8),x]*)


(* ::Input:: *)
(*(* IntegrateRational[(48 Sqrt[3]+4 (12-12 Sqrt[5]) x+4 (6 Sqrt[3]-6 Sqrt[15]) x^2+4 (12-8 Sqrt[15]) x^3+4 (18+Sqrt[3]) x^4+8 Sqrt[3] x^5)/(144-96 Sqrt[15] x+(384-48 Sqrt[5]) x^2+(128 Sqrt[3]-48 Sqrt[15]) x^3+(60+32 Sqrt[3]-72 Sqrt[5]) x^4+(24 Sqrt[3]-48 Sqrt[5]-8 Sqrt[15]) x^5+(12+24 Sqrt[3]-8 Sqrt[15]) x^6+24 x^7+12 x^8),x] *)*)


(* ::Input:: *)
(*(* IntegrateRational[(-32 Sqrt[3]-4 (8-12 Sqrt[5]) x-4 (6 Sqrt[3]-6 Sqrt[15]) x^2-4 (12-8 Sqrt[15]) x^3-4 (18+Sqrt[3]) x^4-8 Sqrt[3] x^5)/(64-64 Sqrt[15] x+(336-32 Sqrt[5]) x^2+(112 Sqrt[3]-48 Sqrt[15]) x^3+(52+48 Sqrt[3]-72 Sqrt[5]) x^4+(24 Sqrt[3]-48 Sqrt[5]-8 Sqrt[15]) x^5+(12+24 Sqrt[3]-8 Sqrt[15]) x^6+24 x^7+12 x^8),x] *)*)


(* ::Input:: *)
(*IntegrateRational[(x (-2+x+18 x^2-7 x^3-56 x^4+15 x^5+72 x^6-12 x^7-36 x^8+3 x^9+5 x^10))/(1-12 x^2+54 x^4-113 x^6+111 x^8-45 x^10+5 x^12),x]*)


(* ::Input:: *)
(*IntegrateRational[x/(2+4 x+5 x^2+2 x^3+x^4),x]*)


(* ::Input:: *)
(*IntegrateRational[(3-7 x^2-21 x^4-32 x^5+72 x^6+108 x^7+45 x^8+6 x^9)/(1-9 x^2-4 x^3+37 x^4+30 x^5-75 x^6-90 x^7+48 x^8+104 x^9+54 x^10+12 x^11+x^12),x]*)


(* ::Input:: *)
(*IntegrateRational[(1-5 x-5 x^2+37 x^3+31 x^4-118 x^5-129 x^6+133 x^7+249 x^8+137 x^9+33 x^10+3 x^11)/(1-9 x^2-4 x^3+37 x^4+30 x^5-75 x^6-90 x^7+48 x^8+104 x^9+54 x^10+12 x^11+x^12),x]*)


(* ::Input:: *)
(*IntegrateRational[(2560 x^3-400 x^2-576 x-84)/(320 x^4+80 x^3-12 x^2+24 x+9),x]*)


(* ::Input:: *)
(*IntegrateRational[(-2 x-5 x^2+5 x^4+4 x^5+x^6)/(1+4 x+12 x^2+22 x^3+26 x^4+20 x^5+11 x^6+4 x^7+x^8),x]*)


(* ::Subsection::Closed:: *)
(*IntegrateRealRationalFunction*)


(* ::Input::Initialization:: *)
ClearAll[IntegrateRealRationalFunction];

(* Given a field of characteristic zero and f \in K(x), return \int f dx. *)

Options[IntegrateRealRationalFunction]=Options[IntegrateRational];

IntegrateRealRationalFunction[f_,x_, OptionsPattern[]] := Module[
{t,integrand, integrands, num, den, fracpart,rationalpartintegral, 
logpart, logterms, logintegral, result,Q,R, constants, params},

debugprint[IntegrateRealRationalFunction,"IN", f,x];

integrand=canonic[f];

params=DeleteCases[Variables[integrand],x];

{rationalpartintegral, logpart} = HermiteReduce[integrand, x];

{Q,R}=PolynomialQuotientRemainder[Numerator[logpart], Denominator[logpart], x];

If[R===0, 
Return[{rationalpartintegral+IntegratePolynomial[Q, x], 0}, Module]];

(* Partial fraction expansion. *)
If[OptionValue["PFD"],
integrands = apartList[R/Denominator[logpart], x, Extension->OptionValue[Extension]],
integrands={R/Denominator[logpart]}
];

(* Extract contants.  *)
{constants, integrands} = Transpose[extractConstants[#,x]& /@ integrands];

logpart = Table[

logterms=IntRationalLogPart[integrands[[k]],x, t, RootSum -> Not[OptionValue["LogToArcTan"] && params === {}]];

If[OptionValue["LogToArcTan"] && params === {},
logintegral=Sum[
With[{\[ScriptCapitalR] = logterm[[1]], \[ScriptCapitalS] = logterm[[2]]},
LogToReal[\[ScriptCapitalR], \[ScriptCapitalS], x, t]], 
{logterm, logterms}],
logintegral=logterms];

If[!FreeQ[logintegral, $Failed], 
logintegral=NaiveLogPart[integrands[[k]],x]];

If[FreeQ[logintegral, Root|RootSum|Function],
(* Factor common terms from logarithms. *)
logintegral = logintegral /. Log[e_] :> Log[Collect[FactorTermsList[monic[e,x],x] // Last,x]];
(* Collect in terms of Log, ArcTan, and ArcTanh. *)
logintegral = Collect[logintegral // Expand, _Log | _ArcTan | _ArcTanh, simproot];
];
constants[[k]]logintegral,
{k, integrands // Length}];

result = {IntegratePolynomial[Q, x]+rationalpartintegral,logpart // Total};

debugprint[IntegrateRealRationalFunction,"OUT", result];
result
]


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*(x^3+4x-1)/(6 x^4-x^2+1)*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//FullSimplify*)


(* ::Input:: *)
(*(32-16 Sqrt[3]+4 x^2+2 x^4+Sqrt[3] x^4)/((4+x^2) (144-80 Sqrt[3]+24 x^2+9 x^4+5 Sqrt[3] x^4))*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//FullSimplify*)


(* ::Input:: *)
(*(2560 x^3-400 x^2-576 x-84)/(320 x^4+80 x^3-12 x^2+24 x+9);*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//FullSimplify*)


(* ::Input:: *)
(*(28+110 x+147 x^2-1131 x^3-945 x^4-189 x^5)/(16-32 x+100 x^2-56 x^3+296 x^4+216 x^5+36 x^6);*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*(x^6+5 x^5+10 x^4+50 x^3+20 x^2+127 x-1)/(x^4+10 x^2+25)*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*(-84-576 x-400 x^2+2560 x^3)/(9+24 x-12 x^2+80 x^3+320 x^4)*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*1/((x^4+1)(1-x^8)^4)*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*1/(x^3+1)*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*1/(x^8+1);*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*x^2/(x^4+x^2+1)*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*(-4+4 Sqrt[3]-x^2-Sqrt[3] x^2)/(144-80 Sqrt[3]+24 x^2+9 x^4+5 Sqrt[3] x^4)*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*(32-16 Sqrt[3]+4 x^2+2 x^4+Sqrt[3] x^4)/((4+x^2) (144-80 Sqrt[3]+24 x^2+9 x^4+5 Sqrt[3] x^4))*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*((3-2 Sqrt[2]+x^2)^2 (-3+2 Sqrt[2]+x^2))/(577-408 Sqrt[2]+328 x^2-232 Sqrt[2] x^2+78 x^4-56 Sqrt[2] x^4+8 x^6-8 Sqrt[2] x^6+x^8)*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*((d+e x^2)/(a+b x^2+c x^4))*)
(*(* IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify *)*)


(* ::Input:: *)
(*(2 x+x^2)/(2+4 x+2 x^2+x^4)*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*x/(x^8+1)*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*1/((a+b x)^2 (c+d x))*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*(x^3+4x-1)/(6 x^4-x^2+1)*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*1/(x^8+x+1)*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*1/(2 x^3+8 x^2+9 x+2)*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*(28+110 x+147 x^2-1131 x^3-945 x^4-189 x^5)/(16-32 x+100 x^2-56 x^3+296 x^4+216 x^5+36 x^6)*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*(100+136 x-24 x^2-60 x^3+34 x^4+48 x^5+20 x^6+4 x^7)/(43+98 x+28 x^2-64 x^3-33 x^4+10 x^5+10 x^6+4 x^7+x^8)*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*((Sqrt[3]+3 Sqrt[21] x^2+5 Sqrt[2] x^4+4 Sqrt[2] x^5+2 Sqrt[14] x^7)/((1+x+Sqrt[7] x^3) (-Sqrt[3]+Sqrt[2] x^5)))*)
(*(* IntegrateRational[%,x]//Timing *)*)


(* ::Input:: *)
(*1/(x^4-4x^2+6)*)
(*IntegrateRational[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Subsection::Closed:: *)
(*IntRationalLogPart (Lazard-Rioboo-Trager)*)


(* ::Input::Initialization:: *)
ClearAll[IntRationalLogPart];
(*
Given a field K of characteristic 0 and A,D \in K[x] with \deg(A) < deg(D), D non-zero, square-free and coprime with A, return \int A/D dx. 

Ref: Bronstein, Symbolic Integration I, 2004, \textit{IntRationalLogPart -- Lazard--Rioboo--Trager algorithm}, pp. 51.
*)

Options[IntRationalLogPart] = {RootSum -> False};

IntRationalLogPart[f_,x_, t_, OptionsPattern[]] := Module[
{a,d, prs, degs, resultant, Q, S, s, A, g,r,result},

debugprint[IntRationalLogPart, "IN", f,x, t];

a=Numerator[f];
d=Denominator[f];
prs=SubresultantPolynomialRemainders[d, a - t D[d, x],x];
degs=Table[Exponent[s,x],{s, prs}];
resultant=primitive[Resultant[d, a - t D[d, x],x],t];
Q=SquareFree[resultant];
S=Table[
If[Exponent[Q[[i,1]],t]>0,
If[i == Exponent[d, x],
(* Re the following two lines (which are not in Symbolic Integration I) - see Example 11.12 in Algorithms for Computer Algebra, Geddes et al *)
r=ExtendedEuclidean[lc[d,x],Q[[i,1]],1,t]//First;
d=primitive[PolynomialRemainder[r d,Q[[i,1]],t],x];
d,
s=Extract[prs, Position[degs,i] // First];
s=primitive[s,x];
A=SquareFree[primitive[lc[s, x],t]] // ToRadicals;
Do[
g=PolynomialGCD[A[[j,1]], Q[[i,1]], Extension->Automatic];
s=exquo[s,g^j,t] (* x *),
{j, Length[A]}];
r=ExtendedEuclidean[lc[s,x],Q[[i,1]],1,t]//First;
s=primitive[PolynomialRemainder[r s,Q[[i,1]],t],x];
s],
{}],
{i,Length[Q]}];

(* Just for testing, returns the sum of logarithms: *)
If[OptionValue[RootSum],
result=Sum[
If[Exponent[Q[[i,1]],t]>0,
RootSum@@{Function@@{t,Q[[i,1]]}, Function@@{t,t Log[S[[i]]]}}, Sequence @@ {}],
 {i, Length[Q]}];
debugprint[result];
Return[result, Module]
];

result=Table[
If[Exponent[Q[[i,1]],t]>0,
{Q[[i,1]],S[[i]]}, Sequence @@ {}],
 {i, Length[Q]}];

debugprint[IntRationalLogPart, "OUT", result];
result
]


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*IntRationalLogPart[(x+2 x^3)/(-4+Sqrt[15]+(20+12 Sqrt[15]) x^2+(-60+4 Sqrt[15]) x^4+80 x^6-40 x^8),x,T]*)


(* ::Input:: *)
(*(264 Sqrt[6] x-528 Sqrt[3] x^3+24 Sqrt[2] x^5-96 x^7+60 Sqrt[2] x^9-24 x^11)/(99+6 Sqrt[3] x^4-6 Sqrt[6] x^6+(-4+3 Sqrt[3]) x^8+8 Sqrt[2] x^10-12 x^12+4 Sqrt[2] x^14-x^16);*)
(*(* IntRationalLogPart[%,x,T]//Timing *)*)


(* ::Input:: *)
(*(79-64 x)/(6+24 x+12 x^2-24 x^3);*)
(*IntRationalLogPart[%,x,t,RootSum->True]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*(28+110 x+147 x^2-1131 x^3-945 x^4-189 x^5)/(16-32 x+100 x^2-56 x^3+296 x^4+216 x^5+36 x^6);*)
(*IntRationalLogPart[%,x,t,RootSum->True]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*((x^4+x^3+x^2+x+1)/(x^5+x^4+2x^3+2x^2-2+4Sqrt[-1+Sqrt[3]]))(* Example 2.5, Bronstein, Symbolic Integration I. *)*)
(*(* IntRationalLogPart[%,x,t,RootSum -> True]*)
(*D[%,x]-%%//Simplify *)*)


(* ::Input:: *)
(*(8x^9+x^8-12x^7-4x^6-26x^5-6x^4+30x^3+23x^2-2x-7)/(x^10-2x^8-2x^7-4x^6+7x^4+10x^3+3x^2-4x-2)(* Exercise 2.2, Bronstein, Symbolic Integration I. *)*)
(*IntRationalLogPart[%,x,t,RootSum -> True]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*IntRationalLogPart[(x^4-3x^2+6)/(x^6-5x^4+5x^2+4),x,t,RootSum->True](* Bronstein example 2.4.1.  *)*)


(* ::Input:: *)
(*IntRationalLogPart[(-84-576 x-400 x^2+2560 x^3)/(9+24 x-12 x^2+80 x^3+320 x^4),x,t,RootSum->True]*)
(*D[%,x]-(-84-576 x-400 x^2+2560 x^3)/(9+24 x-12 x^2+80 x^3+320 x^4)//Simplify*)


(* ::Input:: *)
(*x^2/(x^4+x^2+1)*)
(*IntRationalLogPart[%,x,t,RootSum->True]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*IntRationalLogPart[(-1+x^2)/(1-2 x^2+2 x^4),x,t,RootSum->True]*)
(*D[%,x]-(-1+x^2)/(1-2 x^2+2 x^4)//Simplify*)


(* ::Input:: *)
(*IntRationalLogPart[(6x^5+6x^4-8x^3-18x^2+8x+8)/(x^6-5x^4-8x^3-2x^2+2x+1),x,t,RootSum->True]*)
(*D[%,x]-(6x^5+6x^4-8x^3-18x^2+8x+8)/(x^6-5x^4-8x^3-2x^2+2x+1)//Simplify*)


(* ::Input:: *)
(*(* This appears to be a bug! *)*)
(*(Sqrt[2] (-1+2 x))/(Sqrt[3]-x+x^2)+(Sqrt[3] (1+3 x^2))/(Sqrt[2]+x+x^3)//Together*)
(*(* IntRationalLogPart[%,x,t,RootSum->True]*)
(*D[%,x]-%%//Simplify *)*)


(* ::Input:: *)
(*(Sqrt[3]+3 Sqrt[21] x^2+5 Sqrt[2] x^4+4 Sqrt[2] x^5+2 Sqrt[14] x^7)/((1+x+Sqrt[7] x^3) (-Sqrt[3]+Sqrt[2] x^5))//Together*)
(*(* IntRationalLogPart[%,x,t,RootSum->True]*)
(*D[%,x]-%%//Simplify *)*)


(* ::Input:: *)
(*1/(x^8+1);*)
(*IntRationalLogPart[%,x,t,RootSum->True]*)
(*D[%,x]-%%//Simplify*)


(* ::Subsection::Closed:: *)
(*canonic*)


(* ::Input::Initialization:: *)
ClearAll[canonic];

canonic[expr_] := Module[{c=expr,params},
debugprint[canonic,"IN", expr];

If[FreeQ[c,Root|RootSum|Function],
params=Variables[c];
c=RootReduce[Collect[#,Alternatives @@ params,RootReduce]& //@ c]
];
c=Cancel[Together[c, Extension->Automatic],Extension->Automatic];
If[FreeQ[expr, Root],
c = c /. r_Root :> ToRadicals[r]];

debugprint[canonic,"OUT", c];
c
]


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*canonic[3/(-1+4 T)-(4 Sqrt[-1+Sqrt[3]])/(-1+4 T)-(34 T)/(-1+4 T)+(52 Sqrt[-1+Sqrt[3]] T)/(-1+4 T)+(136 T^2)/(-1+4 T)-(224 Sqrt[-1+Sqrt[3]] T^2)/(-1+4 T)-(192 T^3)/(-1+4 T)+(320 Sqrt[-1+Sqrt[3]] T^3)/(-1+4 T)]*)


(* ::Input:: *)
(*canonic[(256 Sqrt[2] T+4096 T^2+16384 Sqrt[2] T^3)/(-1-8 Sqrt[2] T-64 T^2)]*)


(* ::Input:: *)
(*canonic[(-(1/12) Sqrt[(37-2 Sqrt[183]-Sqrt[73-4 Sqrt[183]]) (37-2 Sqrt[183]+Sqrt[73-4 Sqrt[183]])]-1/12 Sqrt[(37+2 Sqrt[183]-Sqrt[73+4 Sqrt[183]]) (37+2 Sqrt[183]+Sqrt[73+4 Sqrt[183]])]+1/2 Sqrt[1/3 (37-2 Sqrt[183]-Sqrt[73-4 Sqrt[183]])] x-1/2 Sqrt[1/3 (37-2 Sqrt[183]+Sqrt[73-4 Sqrt[183]])] x+1/2 Sqrt[1/3 (37+2 Sqrt[183]-Sqrt[73+4 Sqrt[183]])] x-1/2 Sqrt[1/3 (37+2 Sqrt[183]+Sqrt[73+4 Sqrt[183]])] x+2 x^2)/(1/12 Sqrt[(37-2 Sqrt[183]-Sqrt[73-4 Sqrt[183]]) (37-2 Sqrt[183]+Sqrt[73-4 Sqrt[183]])]-1/12 Sqrt[(37+2 Sqrt[183]-Sqrt[73+4 Sqrt[183]]) (37+2 Sqrt[183]+Sqrt[73+4 Sqrt[183]])]-1/2 Sqrt[1/3 (37-2 Sqrt[183]-Sqrt[73-4 Sqrt[183]])] x+1/2 Sqrt[1/3 (37-2 Sqrt[183]+Sqrt[73-4 Sqrt[183]])] x+1/2 Sqrt[1/3 (37+2 Sqrt[183]-Sqrt[73+4 Sqrt[183]])] x-1/2 Sqrt[1/3 (37+2 Sqrt[183]+Sqrt[73+4 Sqrt[183]])] x)]*)


(* ::Input:: *)
(*canonic[1/6 (-12-Sqrt[3])+x^2+x Root[121 - 156 #^2 + 36 #^4& , 3, 0]]*)


(* ::Subsection::Closed:: *)
(*simproot*)


(* ::Input::Initialization:: *)
ClearAll[simproot];
simproot[e_]:=With[{ce = canonic[e]}, ce // RootReduce // ToRadicals]


(* ::Subsection::Closed:: *)
(*collectnum*)


(* ::Input::Initialization:: *)
ClearAll[collectnum];
collectnum[e_,x_, simp_:Identity]:=With[{ce=canonic[e]}, Collect[Numerator[e],x,simp]/Denominator[e]]


(* ::Subsection::Closed:: *)
(*apartList*)


(* ::Input::Initialization:: *)
ClearAll[apartList];

Options[apartList] = {Extension -> Automatic};

apartList[e_, x_, OptionsPattern[]] := Module[{pf},
	(* ed = Distribute[e, Plus, Times]; *)
	If[Head[e] === Plus,
	(* Partial fraction expansion of each term. *)
		pf = With[{ex=canonic[#]},
	Apart[
	Numerator[ex]/Factor[Denominator[ex], 
	Extension->OptionValue[Extension]], x]
	]& /@ e,
	(* Single term. *)
		pf = With[{ex=canonic[e]},
	Apart[
	Numerator[ex]/Factor[Denominator[ex], 
	Extension->OptionValue[Extension]], x]
	]
	];
	If[Head[pf] === Plus,
		List @@ pf,
		{pf}
	]
]


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*apartList[1/(x^6-1), x]*)


(* ::Input:: *)
(*apartList[1/(x^6-x+1), x]*)


(* ::Subsection::Closed:: *)
(*extractConstants*)


(* ::Text:: *)
(*Motivation: compare the Lazard-Rioboo-Trager algorithm on*)


(* ::Input:: *)
(*IntRationalLogPart[(12 (-420+1999 Sqrt[3]) x)/((-1999+140 Sqrt[3]) (-4-2 Sqrt[3]-6 x^2+6 Sqrt[3] x^2-9 x^4)),x,T]//RepeatedTiming*)


(* ::Text:: *)
(*compared to *)


(* ::Input:: *)
(*extractConstants[(12 (-420+1999 Sqrt[3]) x)/((-1999+140 Sqrt[3]) (-4-2 Sqrt[3]-6 x^2+6 Sqrt[3] x^2-9 x^4)),x]*)
(*IntRationalLogPart[%//Last,x,T]//RepeatedTiming*)


(* ::Input::Initialization:: *)
(* Integrate[c R[x], x] == c*Integrate[R[x], x] /; FreeQ[c, x], where R[x] is rational function. *)
extractConstants[f_,x_] := Module[{num,den,numfl, denfl,numconst,denconst},
num=Numerator[f];
den=Denominator[f];
numfl=FactorSquareFreeList[num, Extension -> Automatic];
denfl=FactorSquareFreeList[den, Extension -> Automatic];
numconst=Power@@numfl[[1]];
denconst=Power@@denfl[[1]];
num=Times@@Power@@@numfl[[2;;]];
den=Times@@Power@@@denfl[[2;;]];
{numconst/denconst, num/den}
]


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*extractConstants[1,x]*)


(* ::Input:: *)
(*extractConstants[x^2,x]*)


(* ::Input:: *)
(*extractConstants[1/(x^2-1),x]*)


(* ::Input:: *)
(*extractConstants[(12 (-420+1999 Sqrt[3]) x)/((-1999+140 Sqrt[3]) (-4-2 Sqrt[3]-6 x^2+6 Sqrt[3] x^2-9 x^4)),x]*)


(* ::Input:: *)
(*extractConstants[-((4 (-420+1999 Sqrt[3]) x)/((-1999+140 Sqrt[3]) (-2+Sqrt[3]+2 x^2+2 Sqrt[3] x^2-2 x^4))),x]*)


(* ::Input:: *)
(*extractConstants[(8 (15-2 Sqrt[3]) x+96 Sqrt[3] x^3+24 Sqrt[3] x^5)/(-4+(-20+60 Sqrt[3]) x^2+(-100+10 Sqrt[3]) x^4+(12+60 Sqrt[3]) x^6-36 x^8),x]*)


(* ::Subsection::Closed:: *)
(*biquadraticQ*)


(* ::Input::Initialization:: *)
(* Actually a n-quadratic generalisation. *)
biquadraticQ[p_,x_] := MatchQ[
CoefficientRules[p,x], 
Alternatives[{{n_}->_, {m_}->_,{0}->_}/;n==2m, {{n_}->_, {m_}->_}/;n==2m]]


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*biquadraticQ[x^4-3x^2+1,x]*)


(* ::Input:: *)
(*biquadraticQ[x^6-3x^3+1,x]*)


(* ::Input:: *)
(*biquadraticQ[x^6-3x^3,x]*)


(* ::Input:: *)
(*biquadraticQ[x^2-3x+3,x]*)


(* ::Subsection::Closed:: *)
(*NaiveLogPart*)


(* ::Input::Initialization:: *)
ClearAll[NaiveLogPart];

(* RootSum form where we use the full factorisation form of the integral. That is, 

\int a/d dx == \sum_{\alpha : d(x) == 0} ( a(\alpha) log(x - \alpha))/(D[d,x](\alpha))

 *)
NaiveLogPart[f_,x_] := Module[{a,d,dd,T, integrale},
a=Numerator[f];
d=Denominator[f];
dd=D[d,x];
integrale = RootSum @@ {Function @@ {x, d}, Function @@ {x, a Log[T-x]/dd}} /. T -> x;
If[biquadraticQ[d,x],
integrale // ToRadicals, 
integrale]
]


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*(-3484952+2016354 x+5342081 x^2+14402552 x^3)/(49593262 (9+x+5 x^2+7 x^3+8 x^4))*)
(*NaiveLogPart[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*(9-7 x+5 x^2-2 x^3)/(-10-10 x+5 x^2+3 x^3-4 x^4)*)
(*NaiveLogPart[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Input:: *)
(*1/(x^6-1)*)
(*NaiveLogPart[%,x]*)
(*D[%,x]-%%//Simplify*)


(* ::Subsection::Closed:: *)
(*IntegratePolynomial*)


(* ::Input::Initialization:: *)
ClearAll[IntegratePolynomial];
IntegratePolynomial[expr_,x_]:=IntegrateExpandedPolynomial[Expand[expr,x],x]


(* ::Input::Initialization:: *)
ClearAll[IntegrateExpandedPolynomial];
IntegrateExpandedPolynomial[a_,x_]/;FreeQ[a,x] := a x
IntegrateExpandedPolynomial[a_. x_^n_.,x_]/;FreeQ[{a,n},x] := If[n===-1, a Log[x],a x^(n+1)/(n+1)]
IntegrateExpandedPolynomial[expr_Plus,x_]:= IntegrateExpandedPolynomial[#,x]& /@ expr


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*IntegratePolynomial[0,x]*)


(* ::Input:: *)
(*IntegratePolynomial[x,x]*)


(* ::Input:: *)
(*IntegratePolynomial[x^2,x]*)


(* ::Input:: *)
(*IntegratePolynomial[(1-x^2)^3,x]*)


(* ::Input:: *)
(*IntegratePolynomial[(y^2-3y+1)^17 (1-x^2)^3,x]*)


(* ::Input:: *)
(*IntegrateExpandedPolynomial[1-x+c x^2,x]*)


(* ::Subsection::Closed:: *)
(*ExtendedEuclidean (Diophantine)*)


(* ::Input::Initialization:: *)
ClearAll[ExtendedEuclidean];
(* Given a Euclidean domain D and a,b,c in D with c in (a,b), return s,t in D such that s*a + t*b = c.

Ref: Symbolic Integration, Bronstein, p. 14 *)

ExtendedEuclidean[a_,b_,c_,x_]:=Module[
{g,t,s,q,r,res},
{g,{t,s}}=PolynomialExtendedGCD[a,b,x];
q=PolynomialQuotient[c,g,x];
{t,s}=q*{t,s};
{q,r}=PolynomialQuotientRemainder[t,b,x];
{r,Together[s+q*a]}
]


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*121-1131 T^2+4384 T^4-10240 T^6;*)
(*1-4 T^2+16 T^4;*)
(*ExtendedEuclidean[%%,%,1,T]*)
(*%[[1]]%%% + %[[2]] %%//Together*)


(* ::Input:: *)
(*(* TODO: unit tests *)*)


(* ::Subsection::Closed:: *)
(*exquo*)


(* ::Input::Initialization:: *)
ClearAll[exquo];
(* Write a/b == q*b + r, and check that r == 0. *)

exquo::remainder = "failed to produce a zero remainder.";

exquo[a_,b_,x_] := Module[{params,q,r},
params=DeleteCases[Variables[{a,b}],x];
{q,r} = PolynomialQuotientRemainder[a, b, x];
q=Collect[q // canonic,x,Collect[#,Alternatives @@ params,simproot]&];
r=Collect[r // canonic,x,Collect[#,Alternatives @@ params,simproot]&];
If[r =!= 0, 
Message[exquo::remainder];
a/b,
q
]
]


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*exquo[1-x-7 x^2+16 x^3-4 x^4+5 x^5, 5x^2-4x+1, x]*)


(* ::Input:: *)
(*exquo[16 (3936+2833 Sqrt[2]) (Sqrt[3]+2 2^(1/4) x)^3,11808 2^(1/4)+8499 2^(3/4)+22664 Sqrt[3] x+15744 Sqrt[6] x+22664 2^(1/4) x^2+15744 2^(3/4) x^2,x]*)


(* ::Input:: *)
(*exquo[256 Sqrt[2] T+4096 T^2+16384 Sqrt[2] T^3,-1-8 Sqrt[2] T-64 T^2,T]*)


(* ::Input:: *)
(*exquo[(-a+x^2)^2 (Sqrt[2] a+x^2)^2 (Sqrt[3] a+x^2)^2,-258 a^3-102 Sqrt[2] a^3+138 Sqrt[3] a^3+144 Sqrt[6] a^3+294 a^2 x^2+117 Sqrt[2] a^2 x^2-80 Sqrt[3] a^2 x^2-109 Sqrt[6] a^2 x^2+108 a x^4+54 Sqrt[2] a x^4-92 Sqrt[3] a x^4-78 Sqrt[6] a x^4-144 x^6-69 Sqrt[2] x^6+34 Sqrt[3] x^6+43 Sqrt[6] x^6,x]*)


(* ::Input:: *)
(*(* Can this be improved? *)*)
(*exquo[(588 a^2 x+234 Sqrt[2] a^2 x-160 Sqrt[3] a^2 x-218 Sqrt[6] a^2 x+432 a x^3+216 Sqrt[2] a x^3-368 Sqrt[3] a x^3-312 Sqrt[6] a x^3-864 x^5-414 Sqrt[2] x^5+204 Sqrt[3] x^5+258 Sqrt[6] x^5) (1/386 (-12397+17539/Sqrt[2]+Sqrt[3/2 (204827617-144834972 Sqrt[2])]) a^3+1/772 (56649-40053 Sqrt[2]+Sqrt[1/3 (19244965641-13608245704 Sqrt[2])]) a^2 x^2+1/386 (-20986-25705 Sqrt[2/3]+193 Sqrt[880460780/37249+(359446608 Sqrt[6])/37249]) a x^4+1/772 (10117-17539/Sqrt[3]-Sqrt[2/3 (307396501-177475452 Sqrt[3])]) x^6),-258 a^3-102 Sqrt[2] a^3+138 Sqrt[3] a^3+144 Sqrt[6] a^3+294 a^2 x^2+117 Sqrt[2] a^2 x^2-80 Sqrt[3] a^2 x^2-109 Sqrt[6] a^2 x^2+108 a x^4+54 Sqrt[2] a x^4-92 Sqrt[3] a x^4-78 Sqrt[6] a x^4-144 x^6-69 Sqrt[2] x^6+34 Sqrt[3] x^6+43 Sqrt[6] x^6,x]//Timing*)


(* ::Subsection::Closed:: *)
(*gcd*)


(* ::Input::Initialization:: *)
gcd[a_,b_,x_] := PolynomialExtendedGCD[a,b,x] // First


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*gcd[16-32 x+100 x^2-56 x^3+296 x^4+216 x^5+36 x^6,D[16-32 x+100 x^2-56 x^3+296 x^4+216 x^5+36 x^6,x],x]*)


(* ::Subsection::Closed:: *)
(*HermiteReduce (Mack's linear version)*)


(* ::Input::Initialization:: *)
ClearAll[HermiteReduce];
(* Given a field K and A,D which lie in K[x] with D nonzero and coprime with A,
return g,h in K(x) such that f = A/D = D[g,x] + h, and h has a squarefree denominator. 

Ref: Bronstein, Symbolic Integration I, 2004, \textit{HermiteReduce - Mack's linear version}, pp. 44. *)

HermiteReduce[f_, x_] := Module[
{a, d, g, dbar, dstar, dbartwo, dbarhat, dtil, b, c,k,result},
debugprint[HermiteReduce, "IN", f,x];
a=Numerator[f];
d=Denominator[f];
g=0;
dbar=gcd[d, D[d,x],x];
dstar=exquo[d, dbar, x];
While[Exponent[dbar,x] > 0,
dbartwo=gcd[dbar, D[dbar, x],x];
dbarhat=exquo[dbar,dbartwo,x];
dtil=-exquo[dstar D[dbar,x], dbar, x];
{b,c}=ExtendedEuclidean[dtil, dbarhat, a, x];
a=c - exquo[D[b,x]dstar, dbarhat, x];
g=g + canonic[b/dbar];
dbar=dbartwo;
];
result={g, canonic[a/dstar]};
debugprint[HermiteReduce, "OUT", result];
result
]


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*HermiteReduce[(28+110 x+147 x^2-1131 x^3-945 x^4-189 x^5)/(16-32 x+100 x^2-56 x^3+296 x^4+216 x^5+36 x^6),x]*)


(* ::Input:: *)
(*(* Checking variable ordering isn't an issue. *)*)
(*HermiteReduce[(b x+a)/(a x^2+b)^5,x]*)
(*HermiteReduce[(z x+y)/(y x^2+z)^5,x]/.{y->a,z->b}*)
(*%==%%*)


(* ::Input:: *)
(*HermiteReduce[(x^7-24x^4-4x^2+8x-8)/(x^8+6x^6+12x^4+8x^2),x](* Example from Bronstein, Symbolic Integration I, 2004. *)*)


(* ::Input:: *)
(*HermiteReduce[1/(x^4+1)^8,x]*)


(* ::Input:: *)
(*HermiteReduce[(-4+x-9 x^2)/(10 x-8 x^2)^3,x]*)


(* ::Input:: *)
(*HermiteReduce[(4+9 x+5 x^2)/(2-5 x+10 x^2-4 x^3)^4,x]*)


(* ::Input:: *)
(*HermiteReduce[(9+5 x+7 x^2-4 x^3)/(1+6 x^2+3 x^3+10 x^4)^4,x]*)


(* ::Subsection::Closed:: *)
(*primitive*)


(* ::Input::Initialization:: *)
ClearAll[content];
(* Let A = \sum_{i = 0}^{n} a_i x^i  \in  D[x] \ {0}. Then the content of A is 
gcd(a_0, a_1, \cdots, a_n) \in D. *)

content[0, _] := 0
content[p_, x_] := PolynomialGCD@@CoefficientList[p,x]


(* ::Input::Initialization:: *)
ClearAll[primitive];
(* Make the polynomial p primitive wrt x. pp(A) = A/content(A) \in D[x]. *)
primitive[p_,x_] /; FreeQ[p,x] := p
primitive[p_,x_]:=Module[{deg,c, cvec},
deg=Exponent[p,x];
c = content[p, x];
cvec=CoefficientList[p,x];
Sum[canonic[Expand[exquo[cvec[[k+1]], c, x]]]x^k, {k,0, deg}]
]


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*primitive[1-12 Sqrt[-1+Sqrt[3]] T-44 T^2+104 Sqrt[-1+Sqrt[3]] T^2+112 T^3-224 Sqrt[-1+Sqrt[3]] T^3+(3-4 Sqrt[-1+Sqrt[3]]-34 T+52 Sqrt[-1+Sqrt[3]] T+136 T^2-224 Sqrt[-1+Sqrt[3]] T^2-192 T^3+320 Sqrt[-1+Sqrt[3]] T^3) x+(2-24 T+96 T^2-128 T^3) x^2,x]*)


(* ::Input:: *)
(*primitive[0,x]*)


(* ::Input:: *)
(*primitive[45796+549552 t^2+2198208 t^4+2930944 t^6,t]*)


(* ::Input:: *)
(*(* TODO: more unit tests! *)*)


(* ::Subsection::Closed:: *)
(*leading coefficient*)


(* ::Input::Initialization:: *)
ClearAll[lc];
(* lc -- leading coefficient of the polynomial p wrt the variable x. *)
lc[p_,x_] := With[{deg=Exponent[p,x]},
If[deg==0,
p,
Coefficient[p,x^Exponent[p,x]]
]
]


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*lc[3x^2-x+1,x]*)


(* ::Input:: *)
(*lc[5,x]*)


(* ::Subsection::Closed:: *)
(*monic*)


(* ::Input::Initialization:: *)
ClearAll[monic];
(* Makes the polynomial, p, monic wrt x. *)
monic[p_,x_]:=Module[{cl},
If[FreeQ[p,x],Return[1,Module]];
cl=CoefficientList[p,x];
cl/=lc[p,x];
cl=canonic[cl]//RootReduce//ToRadicals;
FromDigits[Reverse[cl],x] // Expand
]


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*monic[7737169-5478212 Sqrt[2]+18693593 x-13215381 Sqrt[2] x+10956424 x^2-7737169 Sqrt[2] x^2-7737169 x^4+5478212 Sqrt[2] x^4,x]*)


(* ::Subsection::Closed:: *)
(*polymod*)


(* ::Input::Initialization:: *)
ClearAll[polymod];
polymod[a_,b_,x_]:=PolynomialReduce[a,b,{x}] // Last


(* ::Input:: *)
(*polymod[x,a,x]*)


(* ::Subsection::Closed:: *)
(*SquareFree*)


(* ::Input::Initialization:: *)
ClearAll[SquareFree];
(* Interface between Mathematica's FactorSquareFreeList and Bronstein's SquareFree. *)
SquareFree[p_] := Module[{sqf,fl},
sqf=FactorSquareFreeList[p,Extension->Automatic];
sqf=SortBy[sqf,Last];
sqf=GatherBy[sqf, Last];
sqf={Expand[Times@@#[[All,1]]], #[[1,-1]]}&/@sqf;
fl=First /@ Table[
Cases[sqf,{_,d}] /. {} -> {{1,d}}, 
{d, sqf[[-1,-1]]}];
fl
]


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*SquareFree[1+12 t^2+48 t^4+64 t^6]*)


(* ::Input:: *)
(*SquareFree[-14 T+800 T^3]*)


(* ::Input:: *)
(*FactorSquareFreeList[-(-1+t)^5 (1+t)^3]*)


(* ::Input:: *)
(*SquareFree[-(-1+t)^5 (1+t)^3]*)


(* ::Subsection::Closed:: *)
(*LogToAtan (Rioboo)*)


(* ::Input::Initialization:: *)
ClearAll[LogToAtan];
(* Rioboo's conversion of complex logarithms to real arc-tangents.

Given a field K of characteristic 0 such that Sqrt[-1] is not in K and
A,B in K[x] with B != 0, return a sum f of arctangents of polynomials in K[x]
such that D[f,x] = D[I Log[(a + I B)/(A - I B)],x]

Ref: Symbolic Integration, Bronstein, pp. 63
 *)

LogToAtan[a_, b_, x_] := Module[
{A,B,d, c, g,v,result},
debugprint[LogToAtan, "IN", a,b,x];

If[b=!=0 && FreeQ[b,x], 
If[FreeQ[a,x],
Return[0, Module],
Return[2ArcTan[canonic[simproot[a]/simproot[b]]], Module]]];

A=Collect[a,x,simproot];(* Fix for: IntegrateRational[x/(2 + 4*x + 5*x^2 + 2*x^3 + x^4), x] *)
B=Collect[b,x,simproot];

If[B=!=0&&(Mod[A,B]===0 || polymod[A,B,x]===0), 
Return[
If[FreeQ[exquo[A,B,x], x],
0,
2ArcTan[simproot[exquo[A,B,x]]]
], Module]];

If[Exponent[A, x] < Exponent[B,x],
Return[LogToAtan[-B,A, x], Module]];

{g,{d,c}}=PolynomialExtendedGCD[B,-A,x]; (* B*d - A*c = g *)

d=Collect[d,x,simproot];
c=Collect[c,x,simproot];

v=collectnum[simproot[(A d+B c)/g],x,simproot]//Together;
If[FreeQ[v, x],
LogToAtan[d,c,x],
2ArcTan[v] + LogToAtan[d,c,x]
]
]


(* ::Input:: *)
(*LogToAtan[a^3 x-b^3 x,(a (a^3-b^3))/b,x]*)
(*I Log[a^3 x-b^3 x+I ((a (a^3-b^3))/b)]-I Log[a^3 x-b^3 x-I ((a (a^3-b^3))/b)]*)
(*D[%-%%,x]//Together*)


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*LogToAtan[x,a,x]*)


(* ::Input:: *)
(*LogToAtan[-(81/2)+28 Sqrt[2]+467/(7-4 Sqrt[2])-(330 Sqrt[2])/(7-4 Sqrt[2])-81 x+56 Sqrt[2] x+(934 x)/(7-4 Sqrt[2])-(660 Sqrt[2] x)/(7-4 Sqrt[2]),81/2 Sqrt[1/17 (7+4 Sqrt[2])]-(1015 Sqrt[1/17 (7+4 Sqrt[2])])/(7-4 Sqrt[2])-28 Sqrt[2/17 (7+4 Sqrt[2])]+(716 Sqrt[2/17 (7+4 Sqrt[2])])/(7-4 Sqrt[2])+162 Sqrt[1/17 (7+4 Sqrt[2])] x-(2030 Sqrt[1/17 (7+4 Sqrt[2])] x)/(7-4 Sqrt[2])-112 Sqrt[2/17 (7+4 Sqrt[2])] x+(1432 Sqrt[2/17 (7+4 Sqrt[2])] x)/(7-4 Sqrt[2]),x]*)


(* ::Input:: *)
(*LogToAtan[217/4+557/(8 Sqrt[2])+19/4 (7+4 Sqrt[2])-(123 (7+4 Sqrt[2]))/(8 Sqrt[2])+52 x,-(35/2) Sqrt[1/34 (7+4 Sqrt[2])]-(41 (7+4 Sqrt[2])^(3/2))/(8 Sqrt[34])+123/8 Sqrt[17/2 (7+4 Sqrt[2])]-19/2 Sqrt[17 (7+4 Sqrt[2])],x]*)


(* ::Input:: *)
(*LogToAtan[x^3-3x,x^2-2,x]*)
(*I Log[x^3-3x+I(x^2-2)]-I Log[x^3-3x-I(x^2-2)]*)
(*D[%-%%,x]//Together*)


(* ::Input:: *)
(*LogToAtan[-955783307369327858013393221203257129515330867939109765120-666403699425797783011899459446297353534943780434926895104 Sqrt[2] x,-2288590706220923424037192140095851836585218428808963555328+1622187006795125641025292680649554483050274648374036660224 Sqrt[2] x,x]*)


(* ::Input:: *)
(*LogToAtan[1/4 (-2+Sqrt[2])+1/4 (2+Sqrt[2])-1/2 Sqrt[2+Sqrt[2]] x,-(1/2) Sqrt[(2-Sqrt[2]) (2+Sqrt[2])]+1/2 Sqrt[2-Sqrt[2]] x,x]*)


(* ::Input:: *)
(*(* TODO: more unit tests. *)*)


(* ::Subsection::Closed:: *)
(*LogToReal (Rioboo)*)


(* ::Input::Initialization:: *)
ClearAll[LogToReal];
(* Rioboo's conversion of sums of complex logarithms to real functions.

Given a real field, K, R \in K[t] and S \in K[t,x], return a real function, f, such that 

D[f, x] == D[RootSum[R[\[Alpha]] == 0, \[Alpha] Log[S[\[Alpha], x]]], x]

 *)

LogToReal[r_, s_, x_, t_] := Module[
{P, Q, A, B, u=Symbol["U"], v = Symbol["V"], res,solu, 
soluv, sum1, solnRa,sum2, result},
debugprint[LogToReal, "IN", r,s,x,t];
(*Print[Defer[LogToReal][r,s,x,t]];*)

{P,Q} = ReIm[r /. t->u+I v]//ComplexExpand;
{A,B} = ReIm[s /. t->u+I v]//ComplexExpand;

soluv=Solve[P==Q==0&&v>0, {u, v}, Reals] // ToRadicals // Quiet;
(*soluv=soluv/.ConditionalExpression -> (#1&);*)

If[LeafCount[soluv]> 100, 
debugprint[LogToReal,"OUT", $Failed];
Return[$Failed, Module]];

sum1=Sum[
With[{a=u /. soln, b = v /. soln, As=A /. soln, Bs=B /. soln},
a Log[canonic[As^2+Bs^2//Expand]]+b LogToAtan[As,Bs,x]],
{soln, soluv}];
sum1 = Distribute[sum1, Plus, Times];

solnRa = Solve[r==0, t, Reals] // ToRadicals // Quiet;
(*solnRa=solnRa/.ConditionalExpression -> (#1&);*)

If[MatchQ[solnRa, _Solve], 
debugprint[LogToReal,"OUT", $Failed];
Return[$Failed, Module]];

sum2=Sum[
With[{a=t /. soln, sax = s /. soln}, 
a Log[sax]],
 {soln, solnRa}];
sum2 = Distribute[sum2, Plus, Times];
sum2 = LogToArcTanh[sum2, x];

result=sum1 + sum2;

debugprint[LogToReal,"OUT", result];
result
]


(* ::Input:: *)
(*Assuming[a>0&&b>0,LogToReal[a^6-2 a^3 b^3+b^6+4 a^6 b^6 T^2,2 a^4 b^2 T+(a^3-b^3) x,x,T]]*)
(*Assuming[a>0&&b>0&&a>b,LogToReal[a^6-2 a^3 b^3+b^6+4 a^6 b^6 T^2,2 a^4 b^2 T+(a^3-b^3) x,x,T]]*)


(* ::Input:: *)
(*Assuming[a>0,LogToReal[-1+4 a T^2,2 a T+x,x,T]]*)
(*Assuming[a<0,LogToReal[-1+4 a T^2,2 a T+x,x,T]]*)


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*LogToReal[1+4 a^6 T^2,2 a^4 T+x,x,T]*)


(* ::Input:: *)
(*LogToReal[1-8 I T,-(-1)^(1/8)+x,x,T]*)


(* ::Input:: *)
(*LogToReal[1+4 T-20 T^2+544 T^4,135-70 T-2584 T^2+11152 T^3+52 x,x,T]*)


(* ::Input:: *)
(*LogToReal[4T^2+1,x^3+2T x^2-3x-4T,x,T]*)


(* ::Input:: *)
(*LogToReal[-1+256 T^4,16 T^2+4 T x,x,T]*)


(* ::Input:: *)
(*LogToReal[1+16777216 T^8,64 T^2+8 T x,x,T]*)


(* ::Input:: *)
(*LogToReal[9+7 T+4 T^2,7+4 T+(-7-4 T) x+27 x^2+9 x^3,x,T]*)
(*D[%,x]-(28+110 x+147 x^2-1131 x^3-945 x^4-189 x^5)/(16-32 x+100 x^2-56 x^3+296 x^4+216 x^5+36 x^6)//Simplify*)


(* ::Input:: *)
(*(* TODO: more unit tests!! *)*)


(* ::Subsection::Closed:: *)
(*LogToArcTanh*)


(* ::Input::Initialization:: *)
ClearAll[zeroQ];

zeroQ[e_] := TrueQ[canonic[e] == 0]


(* ::Code::Initialization:: *)
(* A simplification based on
	 D[(Log[a[x] + b[x]] - Log[-a[x] + b[x]]) - 2*ArcTanh[a[x]/b[x]], x] \[Equal] 0 *)
ClearAll[LogToArcTanh];

LogToArcTanh[e_, x_] := Module[{logCombineRule1,logCombineRule2,log2ArcTanhRule, simp},
debugprint[LogToArcTanh, "IN", e, x];

(* Log[f[x]] + Log[g[x]] -> Log[f[x]*g[x]] *)
logCombineRule1 = c0_. Log[p_] + c1_. Log[q_] /; zeroQ[c0 - c1] :> c0 Log[Expand[p q]];

(* Log[f[x]] - Log[g[x]] -> Log[f[x]/g[x]] *)
logCombineRule2 = c0_. Log[p_] + c1_. Log[q_] /; zeroQ[c0 + c1] && 
	zeroQ[c0 - (c0 - c1)/2] && 
	zeroQ[c1 - (c1 - c0)/2] && 
	FreeQ[canonic[p/q] // Denominator, x] :> c0 Log[canonic[p/q]];

log2ArcTanhRule = c1_. Log[p_] + c2_. Log[q_] /;
		! zeroQ[p - q] && 
		zeroQ[c1 + c2] && 
		zeroQ[(p + q)/2 + (p - q)/2 - p] && 
		zeroQ[(p + q)/2 - (p - q)/2 - q] &&
		FreeQ[canonic[(p + q)/(q - p)] // Denominator, x] :> 
			(c2 - c1) ArcTanh[canonic[(p + q)/(q - p)] // RootReduce // ToRadicals // canonic];

simp = e;

(* Simplify/combine logarithms. *)
If[FreeQ[e, Root|RootSum|Function],
	simp = simp /. c_ Log[a_] :> Expand[c Log[monic[a, x] // canonic]];
	simp = simp /. Log[c_ p_] /; FreeQ[c,x] :> Log[p]];
simp = simp //. logCombineRule1;
simp = simp //. logCombineRule2;
If[FreeQ[e, Root|RootSum|Function],
	simp = simp /. c_ Log[a_] :> Expand[c Log[monic[a, x] // canonic]];
	simp = simp /. Log[c_ p_] /; FreeQ[c,x] :> Log[p]];
	
(* Simplify sums of Log to ArcTanh. *)
simp = simp //. log2ArcTanhRule;

If[FreeQ[e, Root|RootSum|Function],
	simp = simp /. p_ /; PolynomialQ[p,x] :> (Collect[p, x, RootReduce] // canonic // ToRadicals)];

debugprint[LogToArcTanh, "OUT", simp];
simp
]


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*LogToArcTanh[-((4643 Sqrt[3/29] ArcTan[1/29 (Sqrt[87]+2 Sqrt[87] x)])/235924)-(11657 ArcTanh[1/5 (5 Sqrt[5]+2 Sqrt[5] x)])/(28618 Sqrt[5])-(1385 Sqrt[5/2] ArcTanh[1/10 (2 Sqrt[10]+3 Sqrt[10] x)])/13858+(5575 Log[5+5 x+x^2])/57236-(4599 Log[8+3 x+3 x^2])/471848-(4859 Log[-2+4 x+3 x^2])/55432,x]*)


(* ::Input:: *)
(*LogToArcTanh[RootSum[-1-#1+#1^11&,Log[x-#1]/(-1+11 #1^10)&],x]*)


(* ::Input:: *)
(*LogToArcTanh[1/12 (-1-Sqrt[61]) Log[25 Sqrt[6]-8/3 Sqrt[2] (-1-Sqrt[61])-(5 (-1-Sqrt[61])^2)/(2 Sqrt[6])+(-1-Sqrt[61])^3/(12 Sqrt[2])+10 x]+1/12 (1-Sqrt[61]) Log[25 Sqrt[6]-8/3 Sqrt[2] (1-Sqrt[61])-(5 (1-Sqrt[61])^2)/(2 Sqrt[6])+(1-Sqrt[61])^3/(12 Sqrt[2])+10 x]+1/12 (-1+Sqrt[61]) Log[25 Sqrt[6]-8/3 Sqrt[2] (-1+Sqrt[61])-(5 (-1+Sqrt[61])^2)/(2 Sqrt[6])+(-1+Sqrt[61])^3/(12 Sqrt[2])+10 x]+1/12 (1+Sqrt[61]) Log[25 Sqrt[6]-8/3 Sqrt[2] (1+Sqrt[61])-(5 (1+Sqrt[61])^2)/(2 Sqrt[6])+(1+Sqrt[61])^3/(12 Sqrt[2])+10 x],x]*)


(* ::Input:: *)
(*LogToArcTanh[(a Log[(-b^(1/4)+a^(1/4) x)/a^(1/4)])/(4 b)+(a Log[(-I b^(1/4)+a^(1/4) x)/a^(1/4)])/(4 b)+(a Log[(I b^(1/4)+a^(1/4) x)/a^(1/4)])/(4 b)+(a Log[(b^(1/4)+a^(1/4) x)/a^(1/4)])/(4 b),x]*)


(* ::Input:: *)
(*-(((-1)^(1/4) (a^5+11 b^5) Log[-(((-1)^(1/4) a (a^5+11 b^5))/b)+(a^5+11 b^5) x])/(4 a^15 b^5))+((-1)^(1/4) (a^5+11 b^5) Log[((-1)^(1/4) a (a^5+11 b^5))/b+(a^5+11 b^5) x])/(4 a^15 b^5)-((-1)^(3/4) (a^5+11 b^5) Log[-(((-1)^(3/4) a (a^5+11 b^5))/b)+(a^5+11 b^5) x])/(4 a^15 b^5)+((-1)^(3/4) (a^5+11 b^5) Log[((-1)^(3/4) a (a^5+11 b^5))/b+(a^5+11 b^5) x])/(4 a^15 b^5);*)
(*LogToArcTanh[%,x]*)
(*D[%-%%,x]//Simplify*)


(* ::Input:: *)
(*LogToArcTanh[-2 b^2 ArcTan[(b x)/a]+a^2 Log[a/b-x]+b^2 Log[a/b-x]+a^2 Log[a/b+x]-b^2 Log[a/b+x]-a^2 Log[a^2/b^2+x^2],x]*)


(* ::Input:: *)
(*LogToArcTanh[b^2 Log[a/b-x]-b^2 Log[a/b+x],x]*)
(*LogToArcTanh[b^2 Log[-(a/b)+x]-b^2 Log[a/b+x],x]*)


(* ::Input:: *)
(*LogToArcTanh[Sqrt[7/136+1/(17 Sqrt[2])] ArcTan[(-1-2 x)/Sqrt[7-4 Sqrt[2]]]+1/2 Sqrt[1/34 (7-4 Sqrt[2])] ArcTan[(1+2 x)/Sqrt[7+4 Sqrt[2]]]+Log[2-Sqrt[2]+x+x^2]/(4 Sqrt[2])-Log[2+Sqrt[2]+x+x^2]/(4 Sqrt[2]),x]*)


(* ::Input:: *)
(*LogToArcTanh[((-306+123 Sqrt[6]) Log[8-3 Sqrt[6]+x^4])/89888+((306-89 Sqrt[6]) Log[10-3 Sqrt[6]+x^4])/89888+((-306-123 Sqrt[6]) Log[8+3 Sqrt[6]+x^4])/89888+((306+89 Sqrt[6]) Log[10+3 Sqrt[6]+x^4])/89888,x]*)


(* ::Input:: *)
(*LogToArcTanh[1/74 (-(54/Sqrt[37])-37 Sqrt[520270387/700227072+1959217/(511488 Sqrt[37])]) Log[-598786415897268458136047+69586406499592574434096 (-(54/Sqrt[37])-37 Sqrt[520270387/700227072+1959217/(511488 Sqrt[37])])-393045076665952690176 (-(54/Sqrt[37])-37 Sqrt[520270387/700227072+1959217/(511488 Sqrt[37])])^2-34422783477400793088 (-(54/Sqrt[37])-37 Sqrt[520270387/700227072+1959217/(511488 Sqrt[37])])^3+239223931716184040902991 x^2]+1/74 (-(54/Sqrt[37])+37 Sqrt[520270387/700227072+1959217/(511488 Sqrt[37])]) Log[-598786415897268458136047+69586406499592574434096 (-(54/Sqrt[37])+37 Sqrt[520270387/700227072+1959217/(511488 Sqrt[37])])-393045076665952690176 (-(54/Sqrt[37])+37 Sqrt[520270387/700227072+1959217/(511488 Sqrt[37])])^2-34422783477400793088 (-(54/Sqrt[37])+37 Sqrt[520270387/700227072+1959217/(511488 Sqrt[37])])^3+239223931716184040902991 x^2]+(27/(37 Sqrt[37])-(13 Sqrt[1/222 (3078523-428941 Sqrt[37])])/3552) Log[-598786415897268458136047+5149394080969850508123104 (27/(37 Sqrt[37])-(13 Sqrt[1/222 (3078523-428941 Sqrt[37])])/3552)-2152314839822756931403776 (27/(37 Sqrt[37])-(13 Sqrt[1/222 (3078523-428941 Sqrt[37])])/3552)^2-13948938011846258978291712 (27/(37 Sqrt[37])-(13 Sqrt[1/222 (3078523-428941 Sqrt[37])])/3552)^3+239223931716184040902991 x^2]+(27/(37 Sqrt[37])+(13 Sqrt[1/222 (3078523-428941 Sqrt[37])])/3552) Log[-598786415897268458136047+5149394080969850508123104 (27/(37 Sqrt[37])+(13 Sqrt[1/222 (3078523-428941 Sqrt[37])])/3552)-2152314839822756931403776 (27/(37 Sqrt[37])+(13 Sqrt[1/222 (3078523-428941 Sqrt[37])])/3552)^2-13948938011846258978291712 (27/(37 Sqrt[37])+(13 Sqrt[1/222 (3078523-428941 Sqrt[37])])/3552)^3+239223931716184040902991 x^2],x]*)


(* ::Input:: *)
(*(* TODO: unit tests. *)*)


(* ::Subsection::Closed:: *)
(*LogToArcTan*)


(* ::Input::Initialization:: *)
ClearAll[im,re];

im[e_] := ComplexExpand[Im[e]]
re[e_] := ComplexExpand[Re[e]]


(* ::Input::Initialization:: *)
ClearAll[polynomialIm];

polynomialIm[p_,x_] := Module[{cl,imcl},
cl=CoefficientList[p,x];
imcl=im[cl];
FromDigits[Reverse[imcl],x] // Expand
]


(* ::Input:: *)
(*polynomialIm[((-1)^(1/4) b)/a+(-1)^(3/4) x,x]*)


(* ::Input::Initialization:: *)
ClearAll[polynomialRe];

polynomialRe[p_,x_] := Module[{cl,recl},
cl=CoefficientList[p,x];
recl=re[cl];
FromDigits[Reverse[recl],x] // Expand
]


(* ::Input:: *)
(*polynomialRe[((-1)^(1/4) b)/a+(-1)^(3/4) x,x]*)


(* ::Input:: *)
(*((-1)^(1/4) b)/a+(-1)^(3/4) x*)
(*polynomialIm[%,x]*)
(*polynomialRe[%%,x]*)
(*%+I %%-%%%//canonic*)


(* ::Input:: *)
(*D[ArcTan[f[x]/g[x]],x]//Cancel*)
(*-D[ArcTan[g[x]/f[x]],x]//Cancel*)


(* ::Input::Initialization:: *)
ClearAll[arcTanTerm];

(* Pick the nicer of ArcTan[re/im] or -ArcTan[im/re] as D[ArcTan[f[x]/g[x]], x] \[Equal] D[-ArcTan[g[x]/f[x]], x]. *)

arcTanTerm[re_, im_, x_] := Module[{r},

If[FreeQ[im,x], 
Return[ArcTan[re/im // canonic]]];

If[FreeQ[re,x],
Return[-ArcTan[im/re // canonic]]];

r=canonic[re/im];

If[FreeQ[Denominator[r],x], 
Return[ArcTan[r]]];

If[FreeQ[Numerator[r],x],
Return[-ArcTan[1/r//canonic]]];

ArcTan[re/im // canonic]
]


(* ::Input::Initialization:: *)
ClearAll[LogToArcTan];

(* D[ArcTan[A[x]/B[x]], x] \[Equal] D[I/2 Log[(A[x] + I B[x])/(A[x] - I B[x])], x] *)

LogToArcTan[e_, x_]:=Module[{logCombineRule1, logCombineRule2, log2ArcTanRule1,log2ArcTanRule2, simp},
debugprint[LogToArcTan, "IN", e, x];

(* Log[f[x]] + Log[g[x]] -> Log[f[x]*g[x]] *)
logCombineRule1 = c0_. Log[p_] + c1_. Log[q_] /; zeroQ[c0 - c1] :> c0 Log[Expand[p q]];

(* Log[f[x]] - Log[g[x]] -> Log[f[x]/g[x]] *)
logCombineRule2 = c0_. Log[p_] + c1_. Log[q_] /; zeroQ[c0 + c1] && 
	zeroQ[c0 - (c0 - c1)/2] && 
	zeroQ[c1 - (c1 - c0)/2] && 
	FreeQ[canonic[p/q] // Denominator, x] :> c0 Log[canonic[p/q]];

log2ArcTanRule1 = a_ Log[A_] + b_ Log[B_] /; 
		(FreeQ[a, x] && FreeQ[b, x] && !zeroQ[im[a]] && 
		zeroQ[im[a] + im[b]] && 
		zeroQ[(A + B)/2 + (A - B)/2 - A] && 
		zeroQ[(A + B)/2 - (A - B)/2 - B] && 
		zeroQ[polynomialIm[(A + B)/2, x]] && 
		zeroQ[polynomialRe[(A - B)/2, x]] && 
		! zeroQ[polynomialRe[(A + B)/2, x]] && 
		! zeroQ[polynomialIm[(A - B)/2, x]]) :> 
	2 im[a] arcTanTerm[polynomialRe[(A + B)/2, x], polynomialIm[(A - B)/2, x], x] + re[a] Log[A] + re[b] Log[B];

(* D[Log[A[x] + I B[x]] - Log[A[x] - I B[x]], x] == D[-2 I ArcTan[A[x]/B[x]], x] *)
log2ArcTanRule2 = a_ Log[A_] + b_ Log[B_] /; 
		(FreeQ[a, x] && FreeQ[b, x] && !zeroQ[re[a]] &&
		zeroQ[re[a] + re[b]] && 
		zeroQ[(A + B)/2 + (A - B)/2 - A] && 
		zeroQ[(A + B)/2 - (A - B)/2 - B] && 
		zeroQ[polynomialIm[(A + B)/2, x]] && 
		zeroQ[polynomialRe[(A - B)/2, x]] && 
		! zeroQ[polynomialRe[(A + B)/2, x]] && 
		! zeroQ[polynomialIm[(A - B)/2, x]]) :> 
	-2 I re[a] arcTanTerm[polynomialRe[(A + B)/2, x], polynomialIm[(A - B)/2, x], x] + im[a] Log[A] + im[b] Log[B];

(* Simplify/combine sums of logarithms. *)
simp = e;
If[FreeQ[e, Root|RootSum|Function],
	simp = simp /. c_ Log[a_] :> Expand[c Log[monic[a, x] // canonic]];
simp = simp /. Log[c_ p_] /; FreeQ[c,x] :> Log[p]];
simp = simp //. logCombineRule1;
simp = simp //. logCombineRule2;
If[FreeQ[e, Root|RootSum|Function],
	simp = simp /. c_ Log[a_] :> Expand[c Log[monic[a, x] // canonic]];
simp = simp /. Log[c_ p_] /; FreeQ[c,x] :> Log[p]];

(* Simplify sums of Log to ArcTan. *)
simp = simp //. {log2ArcTanRule1, log2ArcTanRule2};

(* Simplify remaining sums of logarithms. *)
simp = simp //. logCombineRule1;
simp = simp //. logCombineRule2;
If[FreeQ[e, Root|RootSum|Function],
	simp = simp /. c_ Log[a_] :> Expand[c Log[monic[a, x] // canonic]];
simp = simp /. Log[c_ p_] /; FreeQ[c,x] :> Log[p]];

If[FreeQ[e, Root|RootSum|Function],
	simp = simp /. p_ /; PolynomialQ[p,x] :> (Collect[p, x, RootReduce] // canonic // ToRadicals)];


debugprint[LogToArcTan, "OUT", simp];
simp
]


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*-(-1)^(1/4) Log[-(((-1)^(1/4) b)/a)+x]+(-1)^(1/4) Log[((-1)^(1/4) b)/a+x]-(-1)^(3/4) Log[-(((-1)^(3/4) b)/a)+x]+(-1)^(3/4) Log[((-1)^(3/4) b)/a+x];*)
(*LogToArcTan[%,x]//Timing*)


(* ::Subsection::Closed:: *)
(*rationalQ*)


rationalQ[e_, x_] := With[
    {te = Together[e]}, 
    Denominator[te] =!= 1 && PolynomialQ[Numerator[te], x] && PolynomialQ[Denominator[te], x]
]


(* ::Subsubsection::Closed:: *)
(*tests*)


(* ::Input:: *)
(*rationalQ[1,x]*)


(* ::Input:: *)
(*rationalQ[x^2,x]*)
