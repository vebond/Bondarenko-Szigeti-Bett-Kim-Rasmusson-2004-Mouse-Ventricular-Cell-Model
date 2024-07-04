*****************************************************************
*
* Copyright 2003, Vladimir E. Bondarenko and Randall L. Rasmusson
*       Program m9a.f for mouse model. Apex action potential
*              Room temperature +25 C (298 K)
*
*****************************************************************
      program mouse
      implicit none

      integer NEQ,NT,i,ii,iii,j,NP,IP
      real time,Vrest,fun,TINI,TPER,TW,AMPL,STON(500),STOFF(500)
      real dt,durat,stimon1,stimoff1,stimst1,stimon2,stimoff2,stimst2
      real sum_i,Istim,Icas,Icab,Inaca,Ipca,Ina,Inab,Iclca
      real Inak,Ikto,Ik1,Iks,Ikur,Ikr,Ikur1,Ikur2,Ikur3
      real Jrelt,Jtrt,Jxfert,Jleak,Jup,Jtrpn
      real Acap,Vmyo,Vjsr,Vnsr,Vss,Cm,F,Bi,Bss,Bjsr
      real frel,fcal,fup,ftrpn,fnaca,fxfer,fleak,fcab,fpca
      real srel,scal,sup,strpn,snaca,sxfer,sleak,scab,spca
      real Y(50),YDOT(50)
      real Vm(25),Iksm1(25),Iksm2(25),Iksn(25),Iksnm,Ikst1,Ikst2
      real Pryr,t1,t2,Icasmax
      EXTERNAL FUN

      common/a1/Jrelt,Jtrt,Jxfert,Jleak,Jup,Jtrpn
      common/a2/Acap,Vmyo,Vjsr,Vnsr,Vss,Cm,F,Bi,Bss,Bjsr
      common/a10/Icas,Icab,Inaca,Ipca,Ina,Inab,Iclca,Pryr
      common/a11/Inak,Ikto,Ik1,Iks,Ikur,Ikr,Ikur1,Ikur2,Ikur3

      NEQ = 44
      NT  = 16

      Icasmax = 7.0
      t1  = 0.04
      t2  = -0.1/Icasmax

      dt       =     0.00025
      iii      =  2000
      ii       =     0  
      durat    =   100.0
      stimon1  =    20.0
      stimoff1 =    20.5
      stimst1  =    75.0
      stimon2  =   520.0
      stimoff2 =  1020.0
      stimst2  =    20.0
      Vrest    =   -77.5454
*
          NP   = 500
          TINI = 20
          TPER = 1000
          TW   = 0.5
          AMPL = 80.0
          DO i=1,NP
          STON(i)=TINI+(i-1)*TPER 
          STOFF(i)=TINI+(i-1)*TPER+TW
          END DO
*
* Initial Conditions
*
      frel  = 0.0
      fcal  = 0.0
      fup   = 0.0
      ftrpn = 0.0
      fnaca = 0.0
      fxfer = 0.0
      fleak = 0.0
      fcab  = 0.0
      fpca  = 0.0
*
      srel  = 0.0
      scal  = 0.0
      sup   = 0.0
      strpn = 0.0
      snaca = 0.0
      sxfer = 0.0
      sleak = 0.0
      scab  = 0.0
      spca  = 0.0
*
      time  =      0.0
      Y(1)  =  -0.824202E+02
      Y(2)  =   0.115001E+00
      Y(3)  =   0.129950E+04
      Y(4)  =   0.112684E+02
      Y(5)  =   0.125290E+03
      Y(6)  =   0.129950E+04
      Y(7)  =   0.115001E+00
      Y(8)  =   0.930308E-18
      Y(9)  =   0.999876E+00
      Y(10) =   0.124216E-03
      Y(11) =   0.578679E-08
      Y(12) =   0.119816E-12
      Y(13) =   0.497923E-18
      Y(14) =   0.345847E-13
      Y(15) =   0.185106E-13
      Y(16) =   0.999817E+00
      Y(17) =   0.167740E-03
      Y(18) =   0.149102E-04
      Y(19) =   0.951726E-10
      Y(20) =   0.624646E+00
      Y(21) =   0.207520E-01
      Y(22) =   0.279132E-03
      Y(23) =   0.142371E+05
      Y(24) =   0.143720E+06
      Y(25) =   0.265563E-02
      Y(26) =   0.999977E+00
      Y(27) =   0.262753E-03
      Y(28) =   0.417069E-03
      Y(29) =   0.998543E+00
      Y(30) =   0.998159E+00
      Y(31) =   0.992513E-03
      Y(32) =   0.641229E-03
      Y(33) =   0.175298E-03
      Y(34) =   0.319129E-04
      Y(35) =   0.417069E-03
      Y(36) =   0.998543E+00
      Y(37) =   0.713483E-06
      Y(38) =   0.153176E-03
      Y(39) =   0.673345E-06
      Y(40) =   0.155787E-08
      Y(41) =   0.113879E-01
      Y(42) =   0.342780E+00
      Y(43) =   0.417069E-03
      Y(44) =   0.100000E+01
*
      do 65 i=1,NEQ
      YDOT(i) = 0.0
  65  continue

  77  continue
      if (time.ge.durat) go to 88
      if(mod(ii,iii).ne.0) go to 6
*      if(time.le.95000.0) go to 6
      write(6,44) time,Y(1),Y(2),Y(3),Y(4),Y(5),Y(6)
      write(11,45) time,Y(1),Icas,Icab,Inaca,Ipca,Ina,Inab
      write(12,45) time,Y(1),Inak,Ikto,Ik1,Iks,Ikur,Ikr,Iclca
      write(13,44) time,Y(1),Y(2),Y(3),Y(4),Y(5),Y(6)
      write(14,44) time,Y(7),Y(8),Y(9),Y(10),Y(11),Y(12)
      write(15,44) time,Y(13),Y(14),Y(15),Y(16),Y(17),Y(18)
      write(16,44) time,Y(19),Y(20),Y(21),Y(22),Y(23),Y(24)
      write(17,44) time,Y(25),Y(26),Y(27),Y(28),Y(29),Y(30)
      write(18,44) time,Y(31),Y(32),Y(33),Y(34),Y(35),Y(36)
      write(19,44) time,Y(37),Y(38),Y(39),Y(40),Y(41),Y(42)
      write(10,44) time,Y(43),Y(44),Y(18)+Y(19)
      write(20,44) time,Y(1),Ikto+Iks+Ikur+Ikr,Ikur1,Ikur2,Ikur3,Iclca
      write(30,44) time,frel,fcal,fup,ftrpn,fnaca,fxfer,fleak,fcab,fpca
      write(31,44) time,Bi,Bss,Bjsr
      write(32,44) time,srel,scal,sup,strpn,snaca,sxfer,sleak,scab,spca
      write(33,44) time,Pryr
   6  continue
      ii = ii + 1
*
        DO I=1,NP
        IF((time.GE.STON(I)).AND.(time.LE.STOFF(I))) THEN
              Istim = AMPL
              Pryr = 0.0
              IP = I
              GO TO 78
        ELSE
              Istim = 0.0
        END IF
        END DO
  78    CONTINUE
*
       Y(9)  = 1.0-(Y(8)+Y(10)+Y(11)+Y(12)+Y(13)+Y(14)+Y(15))
       Y(16) = 1.0-(Y(17)+Y(18)+Y(19))
       Y(20) = 1.0-(Y(21)+Y(22)+Y(37)+Y(38)+Y(39)+Y(40)+Y(41)+Y(42))
       Y(30) = 1.0-(Y(31)+Y(32)+Y(33)+Y(34))
*
       call RK4(FUN,NEQ,Y,YDOT,TIME,dt,Istim)
*
      frel  = Jrelt*Vjsr/Vmyo
      fcal  = -1.0*Icas*Acap*Cm/(2.0*F*Vmyo)
      fup   = -1.0*Jup
      ftrpn = -1.0*Jtrpn
      fnaca = 2.0*Inaca*Acap*Cm/(F*Vmyo)
      fxfer = Jxfert
      fleak = Jleak
      fcab  = -1.0*Icab*Acap*Cm/(F*Vmyo)
      fpca  = -1.0*Ipca*Acap*Cm/(F*Vmyo)
*
        if((time.ge.9020).and.(time.le.10020)) then
      srel  = srel+frel*dt
      scal  = scal+fcal*dt
      sup   = sup+fup*dt
      strpn = strpn+ftrpn*dt
      snaca = snaca+fnaca*dt
      sxfer = sxfer+fxfer*dt
      sleak = sleak+fleak*dt
      scab  = scab+fcab*dt
      spca  = spca+fpca*dt
        end if
*
      Pryr = Pryr-t1*Pryr*dt
     *       +t2*Icas*dt*exp((Y(1)-5.0)*(Y(1)-5.0)/(-648.))
*
      go to 77
*
  88  continue

  44  format(f12.5,12e15.6)
  45  format(f12.5,10e15.6)
* END of Program!
      stop
      end
*****************************************************************
*
      subroutine fun(NEQ,time,Y,YDOT,Istim)
*
*****************************************************************
      implicit none
      integer NEQ,i
      real V,Va,Vi,time
      real temp,temp1,temp2,temp3,temp4,temp5,temp6,temp7
      real temp8,temp9,temp10,temp11,temp12,temp13
      real tempa1,tempa2,tempa3,tempa41,tempa42,tempa4
      real tempa5,tempa6,tempa7,tempa8,tempa9,tempa10,tempa11
      real Bi,Bss,Bjsr,sigma
      real alp11,bet11,alp12,bet12,alp13,bet13
      real alp2,bet2,alp3,bet3,alp4,bet4,alp5,bet5
      real alp25,bet25,alp26,bet26,alp27,bet27
      real ass,iss1,taua1,ala0,bea0,ala1,bea1,ali,bei
      real iss2,taua2,taua3
      real Acap,Vmyo,Vjsr,Vnsr,Vss,cKo,cNao,cCao
      real v1,v2,v3,Kmup,Ttr,Txfer,Kap,Kam,Kbp,Kbm,Kcp,Kcm
      real Kpcmax,Kpchalf,Kpcb,Kpcf
      real SFC4I1,SFC4I2,SFC4I3,SFOI1,SFOI2,SFI1I3,SFI2I3,SFICA
      real cLTRPNtot,cHTRPNtot,khtrpnp,khtrpnm,kltrpnp,kltrpnm
      real cCMDNtot,cCSQNtot,Kmcmdn,Kmcsqn
      real Cm,F,T,R,factor,ifactor,Gna,Gkp,Pnak,knaca,Kmna,Kmca,ksat
      real nu,Inakmax,Kmnai,Kmko,Ipcamax,Kmpca,Gcab,Gnab
      real Gkto,Gks,Gkur,kf,kb,Gkr,alpha,beta,gamma,alpha1,t1,t2
      real Gclca,Poclcamax,Kmclca,Ecl,Ecan,ENa,EK,EKr
      real sum_i,Istim,Icas,Icab,Inaca,Ipca,Ina,Inab,Iclca
      real Inak,Ikto,Ik1,Iks,Ikur,Ikr,Ikur1,Ikur2,Ikur3
      real Jrelt,Jtrt,Jxfert,Jleak,Jup,Jtrpn
      real Gkur1,Gkur2,Gkur3,taui1,taui2,taui3,Pryr
      real Y(50),YDOT(50)
      real dito
*
      common/a1/Jrelt,Jtrt,Jxfert,Jleak,Jup,Jtrpn
      common/a2/Acap,Vmyo,Vjsr,Vnsr,Vss,Cm,F,Bi,Bss,Bjsr
      common/a10/Icas,Icab,Inaca,Ipca,Ina,Inab,Iclca,Pryr
      common/a11/Inak,Ikto,Ik1,Iks,Ikur,Ikr,Ikur1,Ikur2,Ikur3
*
* Cell Geometry Parameters
*
      Acap =  1.5340e-4
      Vmyo = 25.8400e-6
      Vjsr =  0.1200e-6
      Vnsr =  2.0980e-6
      Vss  =  1.4850e-9
*
* Standard Ionic Concentrations
*
      cKo  =   5400.0
      cNao = 140000.0
      cCao =   1800.0
*
* SR Parameters
*
      v1    =  4.5
      v2    =  5.8e-5*0.3
      v3    =  1.8*0.25
      Kmup  =  0.5
      Ttr   = 20.0
      Txfer =  2.0*4.0
      Kap   =  0.01215/2.0
      Kam   =  0.1425/2.0
      Kbp   =  0.00405
      Kbm   =  1.93/2.0
      Kcp   =  0.018/2.0
      Kcm   =  0.0008
*
* L-type Ca2+ Channel Parameters

      Kpcmax  =  0.11662*2.0
      Kpchalf = 20.0
      Kpcb    =  0.0005
      Kpcf    = 13.0
      SFC4I1  =  0.01
      SFC4I2  =  0.002
      SFC4I3  =  1.0
      SFOI1   =  1.0
      SFOI2   =  0.001
      SFI1I3  =  0.001
      SFI2I3  =  1.0
      SFICA   =  4.0*1.2/(36.0*6.0)

* Buffering Parameters

      cLTRPNtot =    70.0
      cHTRPNtot =   140.0
      khtrpnp   =     0.00237
      khtrpnm   =     0.000032
      kltrpnp   =     0.0327
      kltrpnm   =     0.0196
      cCMDNtot  =    50.0
      cCSQNtot  = 15000.0
      Kmcmdn    =     2.38/10.0
      Kmcsqn    =   800.0

* Membrane Current Parameters

      Cm      =     1.0
      F       =    96.5
      T       =   298.0
      R       =     8.314
      factor  =    R*T/F
      ifactor =    F/(R*T)
      Gna     =    13.0
      Gkp     =     0.00828
      Pnak    =     0.01833
      knaca   =  2928.*0.1
      Kmna    = 87500.0
      Kmca    =  1380.0
      ksat    =     0.1
      nu      =     0.35
      Inakmax =     0.88
      Kmnai   = 21000.0
      Kmko    =  1500.0
      sigma   = (exp(cNao/67300.0)-1.0)/7.0
      Ipcamax =     1.0
      Kmpca   =     0.5
      Gcab    =     0.000367
      Gnab    =     0.0026
      Gkto    =     0.705/2.08
      dito    =     7.0
      Gks     =     0.46/80.
      Gkur1   =     0.0
      Gkur2   =     0.071*2.*1.13
      Gkur3   =     0.023*2.*1.08
      Gkto    =   Gkto*1.2
      Gkur1   =   Gkur1
      Gkur2   =   Gkur2
      Gkur3   =   Gkur3

* HERG current parameters

      kf  = 0.023761
      kb  = 0.036778
      Gkr = 0.078

* Calcium activated chloride current

      Gclca     = 10.0
      Poclcamax = 0.2
      Kmclca    = 10.0
      Ecl       = -40.0

************************************************************************
*            Calcium fluxes 
************************************************************************
*      Pryr     = exp((Y(1)-5.0)*(Y(1)-5.0)/(-648.))
      Jrelt    = v1*(Y(18)+Y(19))*(Y(6)-Y(7))*Pryr
      Jtrt     = (Y(3)-Y(6))/Ttr
      Jxfert   = (Y(7)-Y(2))/Txfer
      Jleak    = v2*(Y(3)-Y(2))
      Jup      = v3*Y(2)*Y(2)/(Kmup*Kmup+Y(2)*Y(2))
      temp12    = khtrpnp*Y(2)*(cHTRPNtot-Y(5))-khtrpnm*Y(5)
      temp13    = kltrpnp*Y(2)*(cLTRPNtot-Y(4))-kltrpnm*Y(4)
      Jtrpn    = temp12 + temp13
************************************************************************
*             Ionic currents
************************************************************************
*
* Y(1)   : Membrane potential (mV)
*
      V = Y(1)
*
* Icab   : Calcium background current
*
      Ecan = 0.5*factor*alog(cCao/Y(2))
      Icab = Gcab*(Y(1)-Ecan)
*
* Ipca   : Calcium pump current
*
      Ipca = Ipcamax*Y(2)*Y(2)/(Kmpca*Kmpca+Y(2)*Y(2))
*
* Inaca  : Na-Ca exchange current
*
      tempa1 = knaca/(Kmna*Kmna*Kmna+cNao*cNao*cNao)
      tempa2 = 1.0/(Kmca+cCao)
      tempa3 = 1.0/(1.0+ksat*exp((nu-1.0)*Y(1)*ifactor))
          tempa41 = exp(nu*Y(1)*ifactor)*Y(23)*Y(23)*Y(23)*cCao
          tempa42 = exp((nu-1.0)*Y(1)*ifactor)*cNao*cNao*cNao*Y(2)
      tempa4 = tempa41-tempa42
      Inaca = tempa1*tempa2*tempa3*tempa4
*
* Ica    : L-type calcium current
*
      Icas = SFICA*7.78111*(Y(1)-63.0)*Y(8)
*
* Ina    : Na fast current (Luo and Rudy, 1994)
*
C      ENa = factor*alog(cNao/Y(23))
      ENa = factor*alog((0.9*cNao+0.1*cKo)/(0.9*Y(23)+0.1*Y(24)))
C      Ina = Gna*Y(20)*Y(20)*Y(20)*Y(21)*Y(22)*(Y(1)-ENa)
      Ina = Gna*Y(37)*(Y(1)-ENa)
*
* Inab   : Na background current
*
      Inab = Gnab*(Y(1)-ENa)
*
* Inak    : Na-K exchange current
*
      tempa5 = 1.0/(1.0+(Kmnai/Y(23))*sqrt(Kmnai/Y(23)))
      tempa6 = cKo/(cKo+Kmko)
      tempa11= 1.0+0.1245*exp(-0.1*Y(1)*ifactor)
     *         +0.0365*sigma*exp(-1.0*Y(1)*ifactor)
      Inak = Inakmax*tempa5*tempa6/tempa11
*
* Ikto    : transient outward current (Liu and Rasmusson, 1997)
*
      EK = factor*alog(cKo/Y(24))
      Ikto = Gkto*Y(25)*Y(25)*Y(25)*Y(26)*(Y(1)-EK)
*
* Ik1    : Time independent K+ current (Rasmusson et al. 1990)
*
      tempa7 = 0.2938*cKo/(cKo+210.0)
      tempa8 = 1.0+exp(0.0896*(Y(1)-EK))
      Ik1 = tempa7*(Y(1)-EK)/tempa8
*
* Iks    : Delayed rectifier K+ current (Rasmusson et al. 1990)
*
      Iks = Gks*Y(27)*Y(27)*(Y(1)-EK)
*
* Ikur   : Ultra-rapidly activating delayed rectifier Ikur
*                  (Zhou et al., 1998)
*
      Ikur1 = Gkur1*Y(28)*Y(29)*(Y(1)-EK)
      Ikur2 = Gkur2*Y(35)*Y(36)*(Y(1)-EK)
      Ikur3 = Gkur3*Y(43)*Y(44)*(Y(1)-EK)
      Ikur = Ikur1+Ikur2+Ikur3
*
* Ikr    : HERG current (Wang et al., 1997)
*
      EKr = factor*alog((0.98*cKo+0.02*cNao)/(0.98*Y(24)+0.02*Y(23)))
      Ikr = Gkr*Y(33)*(Y(1)-EKr)
*
* Iclca  : calcium-activated chloride current (Xu et al., 2002)
*
      tempa9  = Poclcamax/(1+exp((46.7-Y(1))/7.8))
      tempa10 = Y(2)/(Y(2)+Kmclca)
      Iclca = Gclca*tempa9*tempa10*(Y(1)-Ecl)
*
************************************************************************
*
* Y(1)  membrane potential
*
      sum_i = Icas+Icab+Inaca+Ipca+Ina+Inab+Iclca
     *       +Inak+Ikto+Ik1+Iks+Ikur+Ikr-Istim
      YDOT(1) = -sum_i/Cm
*
* Y(2)  intracellular calcium Cai
*
      Bi = 1.0/(1+cCMDNtot*Kmcmdn/((Kmcmdn+Y(2))*(Kmcmdn+Y(2))))
      temp1 = 0.5*(Icab - 2.0*Inaca + Ipca)*Acap/(Vmyo*F)
      YDOT(2) = Bi*(Jleak + Jxfert - Jup - Jtrpn - temp1)
*
* Y(3)  network SR calcium Cansr
*
      YDOT(3) = (Jup-Jleak)*Vmyo/Vnsr - Jtrt*Vjsr/Vnsr
*
* Y(4)  cLTRPNca
*
      YDOT(4) = kltrpnp*Y(2)*(cLTRPNtot-Y(4)) - kltrpnm*Y(4)
*
* Y(5)  cHTRPNca
*
      YDOT(5) = khtrpnp*Y(2)*(cHTRPNtot-Y(5)) - khtrpnm*Y(5)
*
*     Partial contributions
*     
      alpha1 = 0.4 * exp((Y(1)+12.0)/10.0)
      beta  = 0.05 * exp(-1.0*(Y(1)+12.0)/13.0)
      t1 = 0.7*exp(-1.0*(Y(1)+40.0)*(Y(1)+40.0)/10.0)
      t2 = -0.75*exp(-1.0*(Y(1)+20.0)*(Y(1)+20.0)/400.0)
      alpha = alpha1*(1+t1+t2)/(1.0+0.3*alpha1)
      Kpcf = 13.0*(1.0-exp(-1.0*(Y(1)+14.5)*(Y(1)+14.5)/100.0))
*
      gamma = Kpcmax*Y(7)/(Kpchalf+Y(7))
*
* Y(6) cCajsr junction SR calcium concentartion
*
      temp2 = (Kmcsqn+Y(6))*(Kmcsqn+Y(6))
      Bjsr  = 1.0/(1.0+cCSQNtot*Kmcsqn/temp2)
      YDOT(6) = Bjsr*(Jtrt-Jrelt)
*
* Y(7) cCass  subspace calcium concentration
*
      temp3 = (Kmcmdn+Y(7))*(Kmcmdn+Y(7))
      Bss   = 1.0/(1.0+cCMDNtot*Kmcmdn/temp3)
      temp4 = Jrelt*Vjsr/Vss-Jxfert*Vmyo/Vss
      YDOT(7) = Bss*(temp4-Icas*Acap/(2.0*Vss*F))
*
* Y(8)     O  Ca channel variable
*
      YDOT(8) = 
     *   alpha*Y(12) - 4.0*beta*Y(8)
     * + SFOI1*Kpcb*Y(13) - SFOI1*gamma*Y(8)
     * + SFOI2*alpha*Y(14) - SFOI2*Kpcf*Y(8)
*
* Y(9)    C1  Ca channel variable
*
      YDOT(9) = beta*Y(10)-4.0*alpha*Y(9)
*
* Y(10)   C2  Ca channel variable
*
      YDOT(10) = 
     *    4.0*alpha*Y(9) - beta*Y(10)
     *  + 2.0*beta*Y(11) - 3.0*alpha*Y(10)
*
* Y(11)   C3  Ca channel variable
*
      YDOT(11) = 
     *    3.0*alpha*Y(10) - 2.0*beta*Y(11)
     *  + 3.0*beta*Y(12) - 2.0*alpha*Y(11) 
*
* Y(12)   C4  Ca channel variable
*
      YDOT(12) = 
     *    2.0*alpha*Y(11) - 3.0*beta*Y(12)
     *  + 4.0*beta*Y(8) - alpha*Y(12)
     *  + 4.0*SFC4I1*Kpcb*beta*Y(13) - SFC4I1*alpha*gamma*Y(12)
     *  + 4.0*SFC4I2*beta*Y(14) - SFC4I2*Kpcf*Y(12)
     *  + 4.0*SFC4I3*beta*Kpcb*Y(15) - SFC4I3*gamma*Kpcf*Y(12)
*
* Y(13)   I1  Ca channel variable
*
      YDOT(13) = 
     *    SFOI1*gamma*Y(8) - SFOI1*Kpcb*Y(13)
     *  + SFI1I3*alpha*Y(15) - SFI1I3*Kpcf*Y(13)
     *  + SFC4I1*alpha*gamma*Y(12) - 4.0*SFC4I1*beta*Kpcb*Y(13)
*
* Y(14)   I2  Ca channel variable
*
      YDOT(14) = 
     *    SFOI2*Kpcf*Y(8) - SFOI2*alpha*Y(14)
     *  + SFI2I3*Kpcb*Y(15) - SFI2I3*gamma*Y(14)
     *  + SFC4I2*Kpcf*Y(12) - 4.0*SFC4I2*beta*Y(14)
*
* Y(15)   I3  Ca channel variable
*
      YDOT(15) = 
     *    SFI1I3*Kpcf*Y(13) - SFI1I3*alpha*Y(15)
     *  + SFI2I3*gamma*Y(14) - SFI2I3*Kpcb*Y(15)    
     *  + SFC4I3*gamma*Kpcf*Y(12) - 4.0*SFC4I3*beta*Kpcb*Y(15)
* 
* Y(16)-Y(19)  RyR channel states
*
      temp5 = Y(7)*Y(7)*Y(7)
      temp6 = Y(7)*Y(7)*Y(7)*Y(7)
      YDOT(16)=-Kap*temp6*Y(16)+Kam*Y(18)
      YDOT(17)= Kcp*Y(18)-Kcm*Y(17)
      YDOT(18)= Kap*temp6*Y(16)-Kam*Y(18)
     *         -Kbp*temp5*Y(18)+Kbm*Y(19)
     *         -Kcp*Y(18)+Kcm*Y(17)
      YDOT(19)= Kbp*temp5*Y(18)-Kbm*Y(19)
*
*  Y(20)-Y(22),Y(37)-Y(42) Na fast current (Clancy-Rudy, Circulation, 2002)
*
*      alp11 = 3.802/(0.1027*exp(-Y(1)/17.0)+0.20*exp(-Y(1)/150.))
*      alp12 = 3.802/(0.1027*exp(-Y(1)/15.0)+0.23*exp(-Y(1)/150.))
*      alp13 = 3.802/(0.1027*exp(-Y(1)/12.0)+0.25*exp(-Y(1)/150.))
*      bet11 = 0.1917*exp(-Y(1)/20.3)
*      bet12 = 0.20*exp(-(Y(1)-5.0)/20.3)
*      bet13 = 0.22*exp(-(Y(1)-10.0)/20.3)
*      alp3  = 3.7933e-9*exp(-Y(1)/5.2)
*      bet3  = (0.0084+0.00002*Y(1))
*      alp2  = 6.178*exp((Y(1)+15.0)/120.0)
*      bet2  = alp13*alp2*alp3/(bet13*bet3)
*      alp4  = alp2/1000.
*      bet4  = alp3
*
      Va=Y(1)+2.5
      Vi=Y(1)+7.0
      alp11 = 3.802/(0.1027*exp(-Va/17.0)+0.20*exp(-Va/150.))
      alp12 = 3.802/(0.1027*exp(-Va/15.0)+0.23*exp(-Va/150.))
      alp13 = 3.802/(0.1027*exp(-Va/12.0)+0.25*exp(-Va/150.))
      bet11 = 0.1917*exp(-Va/20.3)
      bet12 = 0.20*exp(-(Va-5.0)/20.3)
      bet13 = 0.22*exp(-(Va-10.0)/20.3)
      alp3  = 7.0e-7*exp(-Vi/7.7)
      bet3  = (0.0084+0.00002*Vi)
      alp2  = 1.0/(0.188495*exp(-Vi/16.6)+0.393956)
      bet2  = alp13*alp2*alp3/(bet13*bet3)
      alp4  = alp2/1000.
      bet4  = alp3
      alp5  = alp2/9.5e4
      bet5  = alp3/50.0
*
      YDOT(20) = bet11*Y(21) - alp11*Y(20)
     *         + alp3*Y(42)  - bet3*Y(20)
      YDOT(21) = alp11*Y(20) - bet11*Y(21)
     *         + bet12*Y(22) - alp12*Y(21)
     *         + alp3*Y(41)  - bet3*Y(21)
      YDOT(22) = alp12*Y(21) - bet12*Y(22)
     *         + bet13*Y(37) - alp13*Y(22)
     *         + alp3*Y(38)  - bet3*Y(22)
      YDOT(37) = alp13*Y(22) - bet13*Y(37)
     *         + bet2*Y(38)  - alp2*Y(37)
      YDOT(38) = alp2*Y(37)  - bet2*Y(38)
     *         + bet3*Y(22)  - alp3*Y(38)
     *         + bet4*Y(39)  - alp4*Y(38)
     *         + alp12*Y(41) - bet12*Y(38)
      YDOT(39) = alp4*Y(38)  - bet4*Y(39)
     *         + bet5*Y(40)  - alp5*Y(39)
      YDOT(40) = alp5*Y(39)  - bet5*Y(40)
      YDOT(41) = alp11*Y(42) - bet11*Y(41)
     *         + bet12*Y(38) - alp12*Y(41)
     *         + bet3*Y(21)  - alp3*Y(41)
      YDOT(42) = bet11*Y(41) - alp11*Y(42)
     *         + bet3*Y(20)  - alp3*Y(42)
*
*  Y(23)  Na intracellular concentration
*
      YDOT(23) = -1.0*(Ina+Inab+3.0*(Inaca+Inak))*Acap/(Vmyo*F)
*
*  Y(24)  K  intracellular concentration
*
      YDOT(24) = -1.0*(Ikto+Ik1+Iks+Ikur+Ikr-2.0*Inak)*Acap/(Vmyo*F)
*
*  Y(25),Y(26)  ato and ito gating variables for Ikto
*
      alp25 = 0.04516*exp(0.03577*(Y(1)+30.0))*4.0
      bet25 = 0.0989*exp(-0.06237*(Y(1)+30.0))*4.0
       temp7 = 0.0019*exp((Y(1)+13.5)/(-1.0*dito))
       temp8 = 0.067083*exp((Y(1)+13.5+20.0)/(-1.0*dito))
*       temp8 = 0.0
      alp26 = 0.08*temp7/(1.0+temp8)
       temp9  = 0.0019*exp((Y(1)+13.5+20.0)/dito)
       temp10 = 0.051335*exp((Y(1)+13.5+20.0)/dito)
      bet26 = 0.5*temp9/(1.0+temp10)
*
      YDOT(25) = alp25*(1.0-Y(25)) - bet25*Y(25)
      YDOT(26) = alp26*(1.0-Y(26)) - bet26*Y(26)
*
*  Y(27)  nks gating variable for Iks
*
      temp11 = 0.00001444*(Y(1)+26.5)
      alp27  = temp11/(1.0-exp(-0.128*(Y(1)+26.5)))/3.
      bet27  = 0.000286*exp(-0.038*(Y(1)+26.5))/3.
*
      YDOT(27) = alp27*(1.0-Y(27)) - bet27*Y(27)
*
*  Y(28), Y(29)  aur and iur gating variables for Ikur1
*
      ass = 1.0/(1.0+exp((-12.5-10.0-Y(1))/7.7))
      iss1 = 1.0/(1.0+exp((35.2+10.0+Y(1))/5.7))
      taua1 = (0.493*exp(-0.0629*Y(1))+2.058)
      taui1 = 270.+1050/(1.0+exp((35.2+10.0+Y(1))/5.7))
*
      YDOT(28) = (ass-Y(28))/taua1
      YDOT(29) = (iss1-Y(29))/taui1
*
*  Y(35), Y(36)  aur and iur gating variables for Ikur2
*
      iss2 = 1.0/(1.0+exp((35.2+10.0+Y(1))/5.7))
*      taua2 = (12.282*exp(-1.0*(Y(1)+12.5)*(Y(1)+12.5)/600.0)+1.064)
      taua2 = (0.493*exp(-0.0629*Y(1))+2.058)
      taui2 = 1200.-170./(1.0+exp((35.2+10.0+Y(1))/5.7))
*
      YDOT(35) = (ass-Y(35))/taua2
      YDOT(36) = (iss2-Y(36))/taui2
*
*  Y(43), Y(44)  aur and iur gating variables for Ikur3
*
      taua3 = (39.3*exp(-0.0862*Y(1))+13.17)
*      taua3 = (54.684*exp(-1.0*(Y(1)+12.5)*(Y(1)+12.5)/412.0)+14.71)
      YDOT(43) = (ass-Y(43))/taua3
      YDOT(44) = 0.0
*
* Y(30)-Y(34) HERG channel state variables
*
      ala0 = 0.022348*exp(0.01176*Y(1))
      bea0 = 0.047002*exp(-0.0631*Y(1))
      ala1 = 0.013733*exp(0.038198*Y(1))
      bea1 = 0.0000689*exp(-0.04178*Y(1))
      ali  = 0.090821*exp(0.023391*(Y(1)+5.0))
      bei  = 0.006497*exp(-0.03268*(Y(1)+5.0))
*
      YDOT(30) = bea0*Y(31)-ala0*Y(30)
      YDOT(31) = ala0*Y(30)-bea0*Y(31)+kb*Y(32)-kf*Y(31)
      YDOT(32) = kf*Y(31)-kb*Y(32)+bea1*Y(33)-ala1*Y(32)
      YDOT(33) = ala1*Y(32)-bea1*Y(33)+bei*Y(34)-ali*Y(33)
      YDOT(34) = ali*Y(33)-bei*Y(34)
*
  11  continue
      return
      end
C*********************************************************************** 
C 
      SUBROUTINE RK4(FUN,NEQ,Y,YDOT,TIME,delt,Istim)
C 
C*********************************************************************** 
      implicit none
      integer NEQ,i
      real time,Istim
      real XK1(50),XK2(50),XK3(50),XK4(50),Y1(50),TT,delt
      real Y(50),YDOT(50)
      EXTERNAL FUN
C
          CALL FUN(NEQ,TIME,Y,YDOT,Istim)
C
        DO I=1,NEQ
        XK1(I) = YDOT(I) * delt
        Y1(I) = Y(I) + XK1(I)/2. 
        END DO
C
        TT = TIME + delt/2.
          CALL FUN(NEQ,TT,Y1,YDOT,Istim)
C
        DO I=1,NEQ
        XK2(I) = YDOT(I) * delt
        Y1(I) = Y(I) + XK2(I)/2.
        END DO
C
          CALL FUN(NEQ,TT,Y1,YDOT,Istim)
C
        DO I=1,NEQ
        XK3(I) = YDOT(I) * delt
        Y1(I) = Y(I) + XK3(I)
        END DO
C
        TT = TIME + delt
          CALL FUN(NEQ,TT,Y1,YDOT,Istim)
C
        DO I=1,NEQ
        XK4(I) = YDOT(I) * delt
        END DO    
C
        DO I=1,NEQ               
        Y(I) = Y(I) + (XK1(I)+2.0*XK2(I)+2.0*XK3(I)+XK4(I))/6.0
        END DO   
C
        TIME = TIME + delt
        RETURN 
        END
C***********************************************************************

