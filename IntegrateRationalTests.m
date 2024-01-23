(* ::Package:: *)

(* ::Title:: *)
(*IntegrateRational Tests*)


(* ::Text:: *)
(*samuel.thomas.blake@gmail.com*)


(* ::Subsubsection:: *)
(*tests*)


(* ::Input:: *)
(*IntegrateRational[(2 a (-b^3+2 Sqrt[2] a^2 b x^2))/(b^4-a^2 b^2 x^2+Sqrt[2] a^4 x^4),x](* BUG! *)*)


(* ::Input:: *)
(*IntegrateRational[(a (-3 c+2 Sqrt[2] c+Sqrt[6] a x))/(c^2-Sqrt[3] a c x+Sqrt[2] a^2 x^2),x](* This is a deficiency! *)*)


(* ::Input:: *)
(*IntegrateRational[(-Sqrt[3] b-2 a x+4 Sqrt[2] a x+3 b^2 x+2 Sqrt[3] a b x^2-2 Sqrt[6] a b x^2+2 Sqrt[2] a^2 x^3)/(1-2 Sqrt[3] b x-a x^2+3 b^2 x^2+Sqrt[3] a b x^3+Sqrt[2] a^2 x^4), x]*)


(* ::Input:: *)
(*IntegrateRational[(b^2 c^2-a c^3+b^4 x^2-3 a b^2 c x^2-a^2 c^2 x^2-2 a^2 b^2 x^4+a^3 c x^4+a^4 x^6)/(b c (-c^2+b^2 x^2-2 a c x^2-a^2 x^4)^2),x]*)


(* ::Input:: *)
(*IntegrateRational[1/(19 (Sqrt[6]-2^(1/4) 3^(3/4) x^2+3 x^4)^3) 3 x^3 (-16+20 2^(3/4) 3^(1/4)-8 2^(1/4) 3^(3/4)-12 Sqrt[6]+(36+12 2^(3/4) 3^(1/4)+18 2^(1/4) 3^(3/4)-30 Sqrt[6]) x^2+(-54-18 2^(3/4) 3^(1/4)+30 2^(1/4) 3^(3/4)-12 Sqrt[6]) x^4+(-30+9 2^(3/4) 3^(1/4)+4 2^(1/4) 3^(3/4)+6 Sqrt[6]) x^6), x](* This is unfortunate. *)*)


(* ::Input:: *)
(*IntegrateRational[(x^2 (Sqrt[6]-12 x^3) (Sqrt[6]-3 x^3)^2 (9-4 x^4+8 Sqrt[6] x^7-36 x^10+12 Sqrt[6] x^13-9 x^16))/(3 (-3-4 x^4+8 Sqrt[6] x^7-36 x^10+12 Sqrt[6] x^13-9 x^16)^2), x]*)


(* ::Input:: *)
(*IntegrateRational[(2 Sqrt[3] x (-8 Sqrt[6]+16 Sqrt[2-Sqrt[3]]+8 Sqrt[3 (2-Sqrt[3])]-8 Sqrt[2 (2-Sqrt[3])] x^2-6 Sqrt[2] x^4+2 Sqrt[6] x^4+12 Sqrt[2-Sqrt[3]] x^4+4 Sqrt[3 (2-Sqrt[3])] x^4-4 Sqrt[2 (2-Sqrt[3])] x^6+Sqrt[2-Sqrt[3]] x^8))/((2+2 Sqrt[3]-2 Sqrt[2] x^2+x^4)^2 (-4 Sqrt[3]+4 Sqrt[3 (2-Sqrt[3])] x^2-3 x^4+Sqrt[3] x^4)),x]*)


(* ::Input:: *)
(*IntegrateRational[(-Sqrt[3]+x+4 Sqrt[2] x+2 Sqrt[3] x^2-2 Sqrt[6] x^2+2 Sqrt[2] x^3)/(1-2 Sqrt[3] x+2 x^2+Sqrt[3] x^3+Sqrt[2] x^4), x]*)


(* ::Input:: *)
(*IntegrateRational[(-1-x+4 Sqrt[2] x+2 x^2-2 Sqrt[2] x^2+2 Sqrt[2] x^3)/(1-2 x+x^3+Sqrt[2] x^4), x]*)


(* ::Input:: *)
(*IntegrateRational[(x (2-5 Sqrt[3] x+12 x^2-3 Sqrt[3] x^3-20 x^4+10 Sqrt[3] x^5))/(1-4 Sqrt[3] x+18 x^2-12 Sqrt[3] x^3+8 x^4+2 Sqrt[3] x^5-3 x^6+5 x^8), x]*)


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
