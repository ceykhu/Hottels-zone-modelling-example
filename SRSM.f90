!  SRSM.f90 
!
!  FUNCTIONS:
!  SRSM - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: SRSM
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    PROGRAM SRSM

    !IMPLICIT NONE

    ! Variables
    DIMENSION NCOE(708,38),COEN(6,35,288),GCOE(708,38),COEG(6,35,288),AAA(35),&
    FDNET(288,6),FDGRO(288,6),FDPOS(288,6),FDNEG(288,6),V(4),G2G(288,6),G2W(288,3,2),&
    W2G(324,2),G2O(288,6,2),O2G(438,6,3),G2B(288,4,2),B2G(32,2),SS(794,794,4),SG(794,288,4),&
    GG(288,288,4),GS(288,794,4),AS(794,1),EMIS(794,1),FKV(288,4),B1(4),B2(4),B3(4),KGN(4),F(288),&
    TG(288),TG2(288),QWALL(794),TW(794),TWF2(794),TWF(5,324),TWB(3,32),TWO(5,5,73),TSP(4),PB(4),PC(4),RT(4),&
    SUMSG(288),AVE(73),EA(6),MAIR(6),BMAX(6),GE(6),SETPT(342,4),CAREA(288,1),GCOOL(288),TD(4),TM(4),ERR1(4),&
    ERR2(4),ERR3(4),ERR4(4),INCREASE(4),TPV(4),KP(4),KI(4),KD(4)
    
    INTEGER NX,NY,NZ,NF,GX,GY,GZ,GF,DT,TIME,WR,OPTION,CRITERA_STOP,CRITERA_CONDTIONS,RUNTIME,DS,DE
    REAL NV,GV,PC,NONZERO,MAIR,FUELSUM,GCONVECTION,MASSIN,MASSOUT,TD,PRODUCT
    CHARACTER*164 NCOE,NETCOE,GCOE,SLABT,DROPT,FUELIN,SLABTD,SPENCON,WALLTEMP,TEST,SETPOINT,COOLING,GAST,WALLT,WNT,BNT,ONT,CONTROL,PFACTOR
    CHARACTER*64 FILENAME,FORM
    EXTERNAL FS
    
    COMMON /COMM1/G2G,G2W,W2G,G2O,O2G,G2B,B2G,AS,FKV,FDPOS,FDNEG,SS,SG,GS,GG,SUMSG,AVE,AAA,FDGRO,FDNET,GE,SETPT,DS,DE
    COMMON /COMM2/TG,TG2,TGG,TSP,TPV,RT,QWALL,PB,KGN,F,NCOE,COEN,GCOE,COEG,B1,B2,B3
    COMMON /COMM3/XSAIR1,XSAIR2,SAR,RHCP,PPCS,PPC,TA,TW,TWF2,CVN
    COMMON /COMM4/TWF,TWB,TWO,EMIS
    COMMON /COMM5/TIME
    COMMON /COMM6/FSUM,FMEAN,FMAX,EMEAN,EMAX,IC
    COMMON /COMM7/PC,V,BMAX,FD,TAM,MU,EA,MAIR,RUPEFF,ENTHAIR,SUMAIR,NONZERO
    COMMON /COMM8/CAREA
    COMMON /COMM9/GCOOL
    COMMON /COMM10/GCONVECTION
    COMMON /COMM11/TD,TM
    
    INTEGER :: TIME_BEGIN, TIME_END,COUNT_RATE
    CALL SYSTEM_CLOCK(TIME_BEGIN)
    
    DATA B1/0.245282778,0.284409909,0.380029865,0.090277448/
    DATA B2/1.13E-04,0.000184829,-0.000224733,-7.26494E-05/
    DATA B3/3.06716E-08,-8.02274E-08,3.3316E-08,1.62397E-08/
    DATA SI,HTCIF,HTCIO/5.67E-08,20.0,30.0/ !SIGMA, HEAT TRANSFER COEFFICIENT, AMBIENT TEMPERATURE
    DATA XSAIR1,XSAIR2,SAR,RHCP,PPCS/8.0,5.0,2.2155,1.23,0.3124/
    DATA RUPEFF/0.6/ !Recuperator efficiency
    DATA TAM,TA/293.0,293.0/
    DATA TSP/1473.0,1573.0,1523.0,1523.0/ !K
    DATA PB/200,200,200,200/ !Poropotional band of temperature controller
    DATA V/1,1,1,1/ !Normalized fuel input velocity
    DATA TM/0.95,0.8,0.17,0.75/
    DATA TD/0.076,0.04,0.03,0.05/
    DATA FD/1.4225/ !Flue gas density, kg/m3
    DATA BMAX/2.09E+06,1.98E+06,2.56E+06,2.79E+06,0.23E+06,0.91E+06/ !Burner max firing rate, W or J/s
    DATA CVN/8.7337E+06/ !Heating value, J/kg
    DATA KP/0.05,0.05,0.05,0.05/
    DATA KI/0.5,0.5,0.5,0.5/
    DATA KD/0.5,0.5,0.5,0.5/
    
    ! Body of SRSM
    PRINT *, 'Welcome to Energy Balance of SRSM Furnace'
    
    OPEN(UNIT=1,FILE='NCOE.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=2,FILE='GCOE.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=3,FILE='G2G.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=4,FILE='G2W.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=5,FILE='W2G.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=6,FILE='G2O.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=7,FILE='O2G.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=8,FILE='G2B.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=9,FILE='B2G.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=10,FILE='SS.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=11,FILE='SG.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=12,FILE='GG.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=13,FILE='GS.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=14,FILE='AS.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=15,FILE='EMIS.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=16,FILE='FKV.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=24,FILE='SETPOINT.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=25,FILE='COOLING.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=26,FILE='GasT.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=27,FILE='FurnaceWallNodeT.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=28,FILE='BafWallNodeT.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=29,FILE='ObsWallNodeT.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    OPEN(UNIT=30,FILE='PC.TXT',ACCESS='SEQUENTIAL',STATUS='OLD')
    
    !WRITE(TEST,*) 'Test.txt'
    !OPEN(UNIT=24,FILE=TEST,STATUS='REPLACE',IOSTAT=NERR)
    !WRITE(24,'(F12.1)') LAMBDA(3,3)
    
    READ(1,*) ((NCOE(I,J), J=1,38), I=1,708)
    READ(2,*) ((GCOE(I,J), J=1,38), I=1,708)
    READ(3,*) ((G2G(I,J), J=1,6), I=1,288)
    READ(4,*) (((G2W(I,J,K), J=1,3), I=1,288),K=1,2)
    READ(5,*) ((W2G(I,J), J=1,2), I=1,324)
    READ(6,*) (((G2O(I,J,K), J=1,6), I=1,288),K=1,2)
    READ(7,*) (((O2G(I,J,K), J=1,6), I=1,438),K=1,3)
    READ(8,*) (((G2B(I,J,K), J=1,4), I=1,288),K=1,2)
    READ(9,*) ((B2G(I,J), J=1,2), I=1,32)
    READ(10,*) (((SS(I,J,K), J=1,794), I=1,794),K=1,4)
    READ(11,*) (((SG(I,J,K), J=1,288), I=1,794),K=1,4)
    READ(12,*) (((GG(I,J,K), J=1,288), I=1,288),K=1,4)
    READ(13,*) (((GS(I,J,K), J=1,794), I=1,288),K=1,4)
    READ(14,*) ((AS(I,J), J=1,1), I=1,794)
    READ(15,*) ((EMIS(I,J), J=1,1), I=1,794)
    READ(16,*) ((FKV(I,J), J=1,4), I=1,288)
    READ(24,*) ((SETPT(I,J), J=1,4), I=1,342)
    READ(25,*) ((CAREA(I,J), J=1,1), I=1,288)
    
    
    DO I=1,708
        IF((I<163).OR.((I>270).AND.(I<385)).OR.((I>468).AND.(I<604))) THEN
            READ(NCOE(I,1)(2:2),'(I4)') NX
            READ(NCOE(I,1)(4:4),'(I4)') NY
            READ(NCOE(I,1)(6:6),'(I4)') NZ
            READ(NCOE(I,1)(9:9),'(I4)') NF
        
            READ(GCOE(I,1)(2:2),'(I4)') GX
            READ(GCOE(I,1)(4:4),'(I4)') GY
            READ(GCOE(I,1)(6:6),'(I4)') GZ
            READ(GCOE(I,1)(9:9),'(I4)') GF
        ELSE
            READ(NCOE(I,1)(2:3),'(I4)') NX
            READ(NCOE(I,1)(5:5),'(I4)') NY
            READ(NCOE(I,1)(7:7),'(I4)') NZ
            READ(NCOE(I,1)(10:10),'(I4)') NF
        
            READ(GCOE(I,1)(2:3),'(I4)') GX
            READ(GCOE(I,1)(5:5),'(I4)') GY
            READ(GCOE(I,1)(7:7),'(I4)') GZ
            READ(GCOE(I,1)(10:10),'(I4)') GF
        ENDIF
        
        DO J=4,38
            READ(NCOE(I,J),*) NV
            COEN(NF,(J-3),(NX+16*(NY-1)+48*(NZ-1))) = NV
            SELECTCASE(NF) !Set the coefficients to the connect faces
                CASE(1)
                    COEN(NF+3,(J-3),(NX+16*(NY-1)+48*(NZ-1))+1) = COEN(NF,(J-3),(NX+16*(NY-1)+48*(NZ-1)))
                CASE(2)
                    COEN(NF+3,(J-3),(NX+16*(NY-1)+48*(NZ-1))+48) = COEN(NF,(J-3),(NX+16*(NY-1)+48*(NZ-1)))
                CASE(3)
                    COEN(NF+3,(J-3),(NX+16*(NY-1)+48*(NZ-1))+16) = COEN(NF,(J-3),(NX+16*(NY-1)+48*(NZ-1)))
            ENDSELECT
            READ(GCOE(I,J),*) GV
            COEG(GF,(J-3),(GX+16*(GY-1)+48*(GZ-1))) = GV
            SELECTCASE(GF) !Set the coefficients to the connect faces
                CASE(1)
                    COEG(GF+3,(J-3),(GX+16*(GY-1)+48*(GZ-1))+1) = COEG(GF,(J-3),(GX+16*(GY-1)+48*(GZ-1)))
                CASE(2)
                    COEG(GF+3,(J-3),(GX+16*(GY-1)+48*(GZ-1))+48) = COEG(GF,(J-3),(GX+16*(GY-1)+48*(GZ-1)))
                CASE(3)
                    COEG(GF+3,(J-3),(GX+16*(GY-1)+48*(GZ-1))+16) = COEG(GF,(J-3),(GX+16*(GY-1)+48*(GZ-1)))
            ENDSELECT
        ENDDO
    ENDDO
    
    !*********Fitted equations of flow data*********!
    AAA(1) = V(1)**3 !a111
    AAA(2) = V(2)**3 !a222
    AAA(3) = V(3)**3 !a333
    AAA(4) = V(4)**3 !a444
    AAA(5) = V(1)**2*V(2) !a112
    AAA(6) = V(1)**2*V(3) !a113
    AAA(7) = V(1)**2*V(4) !a114
    AAA(8) = V(1)*V(2)**2 !a122
    AAA(9) = V(1)*V(2)*V(3) !a123
    AAA(10) = V(1)*V(2)*V(4) !a124
    AAA(11) = V(1)*V(3)**2 !a133
    AAA(12) = V(1)*V(3)*V(4) !a134
    AAA(13) = V(1)*V(4)**2 !a144
    AAA(14) = V(2)**2*V(3) !a223
    AAA(15) = V(2)**2*V(4) !a224
    AAA(16) = V(2)*V(3)**2 !a233
    AAA(17) = V(2)*V(3)*V(4) !a234
    AAA(18) = V(2)*V(4)**2 !a244
    AAA(19) = V(3)**2*V(4) !a334
    AAA(20) = V(3)*V(4)**2 !a344
    AAA(21) = V(1)**2 !a11
    AAA(22) = V(2)**2 !a22
    AAA(23) = V(3)**2 !a33
    AAA(24) = V(4)**2 !a44
    AAA(25) = V(1)*V(2) !a12
    AAA(26) = V(1)*V(3) !a13
    AAA(27) = V(1)*V(4) !a14
    AAA(28) = V(2)*V(3) !a23
    AAA(29) = V(2)*V(4) !a24
    AAA(30) = V(3)*V(4) !a34
    AAA(31) = V(1) !a1
    AAA(32) = V(2) !a2
    AAA(33) = V(3) !a3
    AAA(34) = V(4) !a4
    AAA(35) = 1 !a0
    
    DO I=1,288
        DO J=1,6            
            FDNET(I,J) = DOT_PRODUCT(COEN(J,1:35,I),AAA(1:35))
            FDGRO(I,J) = DOT_PRODUCT(COEG(J,1:35,I),AAA(1:35))
            FDPOS(I,J) = FD*ABS((FDNET(I,J)+FDGRO(I,J))/2) !Flow to positive direction
            FDNEG(I,J) = FD*ABS((FDNET(I,J)-FDGRO(I,J))/2) !Flow to negtive direction
        ENDDO
    ENDDO
    
    WR = 180 !Walk rate
    TIME = 0
    RUNTIME = 0
    DT = 20
    FUELSUM = 0.0
    HST = 0.0
    HWC = 0.0
    HL  = 0.0
    HE  = 0.0
    HA  = 0.0
    PRODUCT = 0.0
    
    IF(CRITERA_CONDITIONS==1) THEN
    TW  = 293.0
    TG  = 293.0
    TWF = 293.0
    TWB = 293.0
    TWO = 293.0
    PC  = 1.0
    ELSE
    READ(26,*) (TG(I), I=1,288) !Read gas zone initial temperature
    READ(27,*) ((TWF(I,J), J=1,324), I=1,5) !Read wall nodes initial temperature
    READ(28,*) ((TWB(I,J), J=1,32), I=1,3) !Read baf. nodes initial temperature
    READ(29,*) (((TWO(I,J,K), J=1,5), I=1,5), K=1,73) !Read obs. nodes initial temperature
    READ(30,*) (PC(I), I=1,4) !Read propotional factor
    DO I = 1,324
        TW(I) = TWF(1,I) !Copy inside wall surface node temperature to wall
    ENDDO
    DO I = 325,356
        TW(I) = TWB(1,I-324) !Copy node T to baf.
    ENDDO
    DO I = 357,789,6 !Start from 324 (wall) + 32 (baf.)
        SUMTL = 0
        DO M = 1,5
            SUMTL = TWO(M,1,(I+3)/6-59) + SUMTL
        ENDDO
        TW(I) = SUMTL/5 !Mean node temperature of left wall
    
        SUMTR = 0
        DO M = 1,5
            SUMTR = TWO(M,5,(I+3)/6-59) + SUMTR
        ENDDO
        TW(I+1) = SUMTR/5 !Mean node temperature of right wall                         
    
        SUMTB = 0
        DO M = 1,5
            SUMTB = TWO(5,M,(I+3)/6-59) + SUMTB
        ENDDO
        TW(I+2) = SUMTB/5 !Mean node temperature of bottom wall
    
        SUMTT = 0
        DO M = 1,5
            SUMTT = TWO(1,M,(I+3)/6-59) + SUMTT
        ENDDO
        TW(I+3) = SUMTT/5 !Mean node temperature of top wall
    
        !Mean node temperature of opposite slab surface
        TW(I+4) = (TW(I) + TW(I+1) + TW(I+2) + TW(I+3))/4 !Mean temperature of back wall
    
        !Mean node temperature of front slab surface
        TW(I+5) = TW(I+4) !Mean node temperature of front wall
    ENDDO
    ENDIF
    
    ERR1 = 0.0
    ERR2 = 0.0
    ERR3 = 0.0
    ERR4 = 0.0
    INCREASE = 0.0
    
    WRITE(*,*)
40  WRITE(*,'(A)') 'Please Select Stop Critera?\N'C, '1. Drop-out temperature change < 1E-07;\N'C, '2. Specify running time;\N'C
    WRITE(*,'(A\)') '>_'
    READ(*,'(I1)') CRITERA_STOP
    IF(CRITERA_STOP==2) THEN
    WRITE(*,*)
    WRITE(*,'(A\)') 'How long time do you want to run? (s)>_'
    READ(*,'(I12)') RUNTIME
    ENDIF
    
    WRITE(*,*)
    WRITE(*,'(A)') 'Product Delayed?\N'C, '1. No;\N'C, '2. Yes;\N'C
    WRITE(*,'(A\)') '>_'
    READ(*,'(I1)') CRITERA_CONDTIONS
    IF(CRITERA_CONDTIONS==2) THEN
    WRITE(*,*)
    WRITE(*,'(A\)') 'Delay start point? (s)>_'
    READ(*,'(I12)') DS
    !DS = ANINT(REAL(DS)/REAL(WR))*WR
    WRITE(*,'(A\)') 'Delay end point? (s)>_'
    READ(*,'(I12)') DE
    !DE = ANINT(REAL(DE)/REAL(WR))*WR
    ENDIF
    
    WRITE(DROPT,*) 'DropT.txt'
    OPEN(UNIT=17,FILE=DROPT,STATUS='UNKNOWN',IOSTAT=NERR)
    
    WRITE(FUELIN,*) 'FuelIn.txt'
    OPEN(UNIT=18,FILE=FUELIN,STATUS='UNKNOWN',IOSTAT=NERR)
    
    WRITE(SLABTD,*) 'SlabTD.txt'
    OPEN(UNIT=19,FILE=SLABTD,STATUS='UNKNOWN',IOSTAT=NERR)
    
    WRITE(SPENCON,*) 'SpEnCon.txt'
    OPEN(UNIT=20,FILE=SPENCON,STATUS='UNKNOWN',IOSTAT=NERR)
    
    WRITE(WALLTEMP,*) 'WallTemp.txt'
    OPEN(UNIT=21,FILE=WALLTEMP,STATUS='UNKNOWN',IOSTAT=NERR)
    
    WRITE(SLABT,*) 'SlabT.txt'
    OPEN(UNIT=23,FILE=SLABT,STATUS='UNKNOWN',IOSTAT=NERR)
    
    WRITE(PFACTOR,*) 'ProportionalFactors.txt'
    OPEN(UNIT=31,FILE=PFACTOR,STATUS='UNKNOWN',IOSTAT=NERR)
    
    !WRITE(TEST,*) 'Test.txt'
    !OPEN(UNIT=24,FILE=TEST,STATUS='UNKNOWN',IOSTAT=NERR)
    
1   TIME = TIME + INT(DT)
    !TW726 = TW(726)
    DO I = 1,324
    TWF2(I) = (TWF(1,I)+TWF(2,I)+TWF(3,I)+TWF(4,I)+TWF(5,I))/5
    ENDDO
    
    SCONVECTION = 0.0
    
    !Place to add cotroller to regular firing rate
    !RT(1)= ((TW(40)+TW(88))/2 + (TG(40)+TG(280))/2) / 2
    !RT(2)= ((TW(44)+TW(92))/2 + (TG(44)+TG(284))/2) / 2
    !RT(4)= ((TW(48)+TW(96))/2 + (TG(48)+TG(288))/2) / 2
    !RT(3)= ((TW(16)+TW(64))/2 + (TG(16)+TG(256))/2) / 2
    RT(1)= (TW(40)+TW(88))/2
    RT(2)= (TW(44)+TW(92))/2
    RT(4)= (TW(48)+TW(96))/2
    RT(3)= (TW(16)+TW(64))/2
    
    !*********Output wall temp. of control zones (C)*********!
    WRITE(21,'(I8,4F12.3)') TIME, RT(1)-273.0, RT(2)-273.0, RT(3)-273.0, RT(4)-273.0
    
    !Temperature controller
    !IF(MOD(INT(TIME),98)==0) THEN
        DO I = 1,4
            IF((CRITERA_CONDTIONS==2).AND.(MOD(INT(TIME),60)==0)) TSP(I) = SETPT(MOD(INT(TIME),20460)/60,I) + 273
            TPV(I) = RT(I)
            ERR1(I) = TSP(I) - TPV(I)
            INCREASE(I) = KP(I)*(ERR1(I)-ERR2(I)) + KI(I)*ERR1(I) + KD(I)*(ERR1(I)-2*ERR2(I)+ERR3(I))
            TPV(I) = TPV(I) + INCREASE(I)
            ERR3(I) = ERR2(I)
            ERR2(I) = ERR1(I)
            RT(I) = TPV(I)
            PC(I) = (TM(I)-TD(I))/PB(I)*(TSP(I)-RT(I)) + PC(I)
            IF(PC(I)>TM(I)) PC(I) = TM(I)
            IF(PC(I)<TD(I)) PC(I) = TD(I)
        ENDDO
    !ENDIF
    !*********Output proportional factors*******!
    WRITE(31,'(I8,4F12.3)') TIME, PC(1), PC(2), PC(3), PC(4)
    
    !*********Fitted equations of flow data*********!
    AAA(1) = (V(1)*PC(1))**3 !a111
    AAA(2) = (V(2)*PC(2))**3 !a222
    AAA(3) = (V(3)*PC(3))**3 !a333
    AAA(4) = (V(4)*PC(4))**3 !a444
    AAA(5) = (V(1)*PC(1))**2*(V(2)*PC(2)) !a112
    AAA(6) = (V(1)*PC(1))**2*(V(3)*PC(3)) !a113
    AAA(7) = (V(1)*PC(1))**2*(V(4)*PC(4)) !a114
    AAA(8) = (V(1)*PC(1))*(V(2)*PC(2))**2 !a122
    AAA(9) = (V(1)*PC(1))*(V(2)*PC(2))*(V(3)*PC(3)) !a123
    AAA(10) = (V(1)*PC(1))*(V(2)*PC(2))*(V(4)*PC(4)) !a124
    AAA(11) = (V(1)*PC(1))*(V(3)*PC(3))**2 !a133
    AAA(12) = (V(1)*PC(1))*(V(3)*PC(3))*(V(4)*PC(4)) !a134
    AAA(13) = (V(1)*PC(1))*(V(4)*PC(4))**2 !a144
    AAA(14) = (V(2)*PC(2))**2*(V(3)*PC(3)) !a223
    AAA(15) = (V(2)*PC(2))**2*(V(4)*PC(4)) !a224
    AAA(16) = (V(2)*PC(2))*(V(3)*PC(3))**2 !a233
    AAA(17) = (V(2)*PC(2))*(V(3)*PC(3))*(V(4)*PC(4)) !a234
    AAA(18) = (V(2)*PC(2))*(V(4)*PC(4))**2 !a244
    AAA(19) = (V(3)*PC(3))**2*(V(2)*PC(2)) !a334
    AAA(20) = (V(3)*PC(3))*(V(4)*PC(4))**2 !a344
    AAA(21) = (V(1)*PC(1))**2 !a11
    AAA(22) = (V(2)*PC(2))**2 !a22
    AAA(23) = (V(3)*PC(3))**2 !a33
    AAA(24) = (V(4)*PC(4))**2 !a44
    AAA(25) = (V(1)*PC(1))*(V(2)*PC(2)) !a12
    AAA(26) = (V(1)*PC(1))*(V(3)*PC(3)) !a13
    AAA(27) = (V(1)*PC(1))*(V(4)*PC(4)) !a14
    AAA(28) = (V(2)*PC(2))*(V(3)*PC(3)) !a23
    AAA(29) = (V(2)*PC(2))*(V(4)*PC(4)) !a24
    AAA(30) = (V(3)*PC(3))*(V(4)*PC(4)) !a34
    AAA(31) = (V(1)*PC(1)) !a1
    AAA(32) = (V(2)*PC(2)) !a2
    AAA(33) = (V(3)*PC(3)) !a3
    AAA(34) = (V(4)*PC(4)) !a4
    AAA(35) = 1 !a0
    
    DO I=1,288
        DO J=1,6            
            FDNET(I,J) = DOT_PRODUCT(COEN(J,1:35,I),AAA(1:35))
            FDGRO(I,J) = DOT_PRODUCT(COEG(J,1:35,I),AAA(1:35))
            FDPOS(I,J) = FD*ABS((FDNET(I,J)+FDGRO(I,J))/2) !Flow to positive direction
            FDNEG(I,J) = FD*ABS((FDNET(I,J)-FDGRO(I,J))/2) !Flow to negtive direction
        ENDDO
    ENDDO
    
    !*********Calculate surface radiation heat flow to each gas zone***********!
    DO I = 1,288
        SUMSG(I) = 0.0
        DO L = 1,794 !Wall surfaces + Baf. surfaces + Obs. surfaces
            SGDFA = 0.0
            DO K = 1,4
                SGDFA = (B1(K)+B2(K)*TW(L)+B3(K)*TW(L)**2)*SG(L,I,K) + SGDFA
            ENDDO
            SUMSG(I) = SUMSG(I) + SGDFA*(SI*TW(L)**4)
        ENDDO
    ENDDO
    
    !*********Call newton raphson solver***********!
    CALL SOLVE(TG,F)
    
    !**************** Heat balance on wall surface zone I ****************!
    DO I = 1,324 !Furnace wall surfaces
        SUMSS = 0
        DO J = 1,794
            SSDFA = 0
            DO K = 1,4
                SSDFA = (B1(K)+B2(K)*TW(J)+B3(K)*TW(J)**2)*SS(J,I,K) + SSDFA
            ENDDO
            SUMSS = SUMSS + SSDFA*(SI*TW(J)**4)
        ENDDO
        
        SUMGS = 0
        DO L = 1,288
            GSDFA = 0
            DO K = 1,4
                GSDFA = (B1(K)+B2(K)*TG(L)+B3(K)*TG(L)**2)*GS(L,I,K) + GSDFA
            ENDDO
            SUMGS = SUMGS + GSDFA*(SI*TG(L)**4)
        ENDDO
        
        SEMIT = 0
        DO K = 1,4
            SEMIT = (B1(K)+B2(K)*TW(I)+B3(K)*TW(I)**2)*EMIS(I,1) + SEMIT
        ENDDO
        SEMIT = SEMIT*AS(I,1)*(SI*TW(I)**4)
        
        SCONV = HTCIF*W2G(I,1)*(TG(INT(W2G(I,2)))-TW(I))
                 
    QWALL(I) = SUMSS + SUMGS - SEMIT + SCONV
    SCONVECTION = SCONVECTION + SCONV
    ENDDO
    
    !*********Call 1D conduction model for furnace wall*********!
    DO I = 1,324
        Q = QWALL(I)/AS(I,1)
        CALL CONDEXP1D_F(Q,I,DT)
    ENDDO
    
    DO I = 1,324
        TW(I) = TWF(1,I) !Copy inside wall surface node temperature to wall
    ENDDO
    
    DO I = 325,356 !Baf. surfaces
        SUMSS = 0
        DO J = 1,794
            SSDFA = 0
            DO K = 1,4
                SSDFA = (B1(K)+B2(K)*TW(J)+B3(K)*TW(J)**2)*SS(J,I,K) + SSDFA
            ENDDO
            SUMSS = SUMSS + SSDFA*(SI*TW(J)**4)
        ENDDO
        
        SUMGS = 0
        DO L = 1,288
            GSDFA = 0
            DO K = 1,4
                GSDFA = (B1(K)+B2(K)*TG(L)+B3(K)*TG(L)**2)*GS(L,I,K) + GSDFA
            ENDDO
            SUMGS = SUMGS + GSDFA*(SI*TG(L)**4)
        ENDDO
        
        SEMIT = 0
        DO K = 1,4
            SEMIT = (B1(K)+B2(K)*TW(I)+B3(K)*TW(I)**2)*EMIS(I,1) + SEMIT
        ENDDO
        SEMIT = SEMIT*AS(I,1)*(SI*TW(I)**4)
        
        SCONV = HTCIF*B2G(I-324,1)*(TG(INT(B2G(I-324,2)))-TW(I))
                 
    QWALL(I) = SUMSS + SUMGS - SEMIT + SCONV
    SCONVECTION = SCONVECTION + SCONV
    ENDDO
    
    !*********Call 1D conduction model for baf. wall*********!
    DO I = 325,356
        Q = QWALL(I)/AS(I,1)
        CALL CONDEXP1D_B(Q,I,DT)
    ENDDO
    
    DO I = 325,356
        TW(I) = TWB(1,I-324) !Copy node T to baf.
    ENDDO
    
    !**************** Heat balance on obs. surface zone I ****************!
    DO I = 357,789,6 !Obs. surfaces
        SUMSS = 0
        DO J = 1,794
            SSDFA = 0
            DO K = 1,4
                SSDFA = (B1(K)+B2(K)*TW(J)+B3(K)*TW(J)**2)*SS(J,I,K) + SSDFA
            ENDDO
            SUMSS = SUMSS + SSDFA*(SI*TW(J)**4)
        ENDDO
        
        SUMGS = 0
        DO L = 1,288
            GSDFA = 0
            DO K = 1,4
                GSDFA = (B1(K)+B2(K)*TG(L)+B3(K)*TG(L)**2)*GS(L,I,K) + GSDFA
            ENDDO
            SUMGS = SUMGS + GSDFA*(SI*TG(L)**4)
        ENDDO
        
        SEMIT = 0
        DO K = 1,4
            SEMIT = (B1(K)+B2(K)*TW(I)+B3(K)*TW(I)**2)*EMIS(I,1) + SEMIT
        ENDDO
        SEMIT = SEMIT*AS(I,1)*(SI*TW(I)**4)
        
        SCONV = 0.0
        DO M = 1,6
        SCONV = HTCIO*O2G(I-356,M,1)*(TG(ABS(INT(O2G(I-356,M,2))))-(TW(I)+TW(I+1)+TW(I+2)+TW(I+3))/4) + SCONV !Calculated based on average wall temperature 
        ENDDO
                 
    QWALL(I) = SUMSS + SUMGS - SEMIT + SCONV
    SCONVECTION = SCONVECTION + SCONV
    
        SUMSS = 0
        DO J = 1,794
            SSDFA = 0
            DO K = 1,4
                SSDFA = (B1(K)+B2(K)*TW(J)+B3(K)*TW(J)**2)*SS(J,I+1,K) + SSDFA
            ENDDO
            SUMSS = SUMSS + SSDFA*(SI*TW(J)**4)
        ENDDO
        
        SUMGS = 0
        DO L = 1,288
            GSDFA = 0
            DO K = 1,4
                GSDFA = (B1(K)+B2(K)*TG(L)+B3(K)*TG(L)**2)*GS(L,I+1,K) + GSDFA
            ENDDO
            SUMGS = SUMGS + GSDFA*(SI*TG(L)**4)
        ENDDO
        
        SEMIT = 0
        DO K = 1,4
            SEMIT = (B1(K)+B2(K)*TW(I+1)+B3(K)*TW(I+1)**2)*EMIS(I+1,1) + SEMIT
        ENDDO
        SEMIT = SEMIT*AS(I+1,1)*(SI*TW(I+1)**4)
        
        SCONV = 0.0
        DO M = 1,6
        SCONV = HTCIO*O2G(I-356+1,M,1)*(TG(ABS(INT(O2G(I-356+1,M,2))))-(TW(I)+TW(I+1)+TW(I+2)+TW(I+3))/4) + SCONV
        ENDDO
    
    QWALL(I+1) = SUMSS + SUMGS - SEMIT + SCONV
    SCONVECTION = SCONVECTION + SCONV
    
        SUMSS = 0
        DO J = 1,794
            SSDFA = 0
            DO K = 1,4
                SSDFA = (B1(K)+B2(K)*TW(J)+B3(K)*TW(J)**2)*SS(J,I+2,K) + SSDFA
            ENDDO
            SUMSS = SUMSS + SSDFA*(SI*TW(J)**4)
        ENDDO
        
        SUMGS = 0
        DO L = 1,288
            GSDFA = 0
            DO K = 1,4
                GSDFA = (B1(K)+B2(K)*TG(L)+B3(K)*TG(L)**2)*GS(L,I+2,K) + GSDFA
            ENDDO
            SUMGS = SUMGS + GSDFA*(SI*TG(L)**4)
        ENDDO
        
        SEMIT = 0
        DO K = 1,4
            SEMIT = (B1(K)+B2(K)*TW(I+2)+B3(K)*TW(I+2)**2)*EMIS(I+2,1) + SEMIT
        ENDDO
        SEMIT = SEMIT*AS(I+2,1)*(SI*TW(I+2)**4)
        
        SCONV = 0.0
        DO M = 1,6
        SCONV = HTCIO*O2G(I-356+2,M,1)*(TG(ABS(INT(O2G(I-356+2,M,2))))-(TW(I)+TW(I+1)+TW(I+2)+TW(I+3))/4) + SCONV
        ENDDO
    
    QWALL(I+2) = SUMSS + SUMGS - SEMIT + SCONV
    SCONVECTION = SCONVECTION + SCONV
    
        SUMSS = 0
        DO J = 1,794
            SSDFA = 0
            DO K = 1,4
                SSDFA = (B1(K)+B2(K)*TW(J)+B3(K)*TW(J)**2)*SS(J,I+3,K) + SSDFA
            ENDDO
            SUMSS = SUMSS + SSDFA*(SI*TW(J)**4)
        ENDDO
        
        SUMGS = 0
        DO L = 1,288
            GSDFA = 0
            DO K = 1,4
                GSDFA = (B1(K)+B2(K)*TG(L)+B3(K)*TG(L)**2)*GS(L,I+3,K) + GSDFA
            ENDDO
            SUMGS = SUMGS + GSDFA*(SI*TG(L)**4)
        ENDDO
        
        SEMIT = 0
        DO K = 1,4
            SEMIT = (B1(K)+B2(K)*TW(I+3)+B3(K)*TW(I+3)**2)*EMIS(I+3,1) + SEMIT
        ENDDO
        SEMIT = SEMIT*AS(I+3,1)*(SI*TW(I+3)**4)
        
        SCONV = 0.0
        DO M = 1,6
        SCONV = HTCIO*O2G(I-356+3,M,1)*(TG(ABS(INT(O2G(I-356+3,M,2))))-(TW(I)+TW(I+1)+TW(I+2)+TW(I+3))/4) + SCONV
        ENDDO
    
    QWALL(I+3) = SUMSS + SUMGS - SEMIT + SCONV
    SCONVECTION = SCONVECTION + SCONV
    
        SUMSS = 0
        DO J = 1,794
            SSDFA = 0
            DO K = 1,4
                SSDFA = (B1(K)+B2(K)*TW(J)+B3(K)*TW(J)**2)*SS(J,I+4,K) + SSDFA
            ENDDO
            SUMSS = SUMSS + SSDFA*(SI*TW(J)**4)
        ENDDO
        
        SUMGS = 0
        DO L = 1,288
            GSDFA = 0
            DO K = 1,4
                GSDFA = (B1(K)+B2(K)*TG(L)+B3(K)*TG(L)**2)*GS(L,I+4,K) + GSDFA
            ENDDO
            SUMGS = SUMGS + GSDFA*(SI*TG(L)**4)
        ENDDO
        
        SEMIT = 0
        DO K = 1,4
            SEMIT = (B1(K)+B2(K)*TW(I+4)+B3(K)*TW(I+4)**2)*EMIS(I+4,1) + SEMIT
        ENDDO
        SEMIT = SEMIT*AS(I+4,1)*(SI*TW(I+4)**4)
        
        SCONV = 0.0
        DO M = 1,6
        SCONV = HTCIO*O2G(I-356+4,M,1)*(TG(ABS(INT(O2G(I-356+4,M,2))))-(TW(I)+TW(I+1)+TW(I+2)+TW(I+3))/4) + SCONV
        ENDDO
    
    QWALL(I+4) = SUMSS + SUMGS - SEMIT + SCONV
    SCONVECTION = SCONVECTION + SCONV
    
        SUMSS = 0
        DO J = 1,794
            SSDFA = 0
            DO K = 1,4
                SSDFA = (B1(K)+B2(K)*TW(J)+B3(K)*TW(J)**2)*SS(J,I+5,K) + SSDFA
            ENDDO
            SUMSS = SUMSS + SSDFA*(SI*TW(J)**4)
        ENDDO
        
        SUMGS = 0
        DO L = 1,288
            GSDFA = 0
            DO K = 1,4
                GSDFA = (B1(K)+B2(K)*TG(L)+B3(K)*TG(L)**2)*GS(L,I+5,K) + GSDFA
            ENDDO
            SUMGS = SUMGS + GSDFA*(SI*TG(L)**4)
        ENDDO
        
        SEMIT = 0
        DO K = 1,4
            SEMIT = (B1(K)+B2(K)*TW(I+5)+B3(K)*TW(I+5)**2)*EMIS(I+5,1) + SEMIT
        ENDDO
        SEMIT = SEMIT*AS(I+5,1)*(SI*TW(I+5)**4)
        
        SCONV = 0.0
        DO M = 1,6
        SCONV = HTCIO*O2G(I-356+5,M,1)*(TG(ABS(INT(O2G(I-356+5,M,2))))-(TW(I)+TW(I+1)+TW(I+2)+TW(I+3))/4) + SCONV
        ENDDO
    
    QWALL(I+5) = SUMSS + SUMGS - SEMIT + SCONV
    SCONVECTION = SCONVECTION + SCONV
    
    ENDDO
    
    !*********Call 2D conduction model for obs. wall*********!
    DO I = 357,789,6
        QW = (QWALL(I)+(QWALL(I+4)+QWALL(I+5))/4)/AS(I,1)     !Q of left hand side
        QE = (QWALL(I+1)+(QWALL(I+4)+QWALL(I+5))/4)/AS(I+1,1) !Q of right hand side
        QS = (QWALL(I+2)+(QWALL(I+4)+QWALL(I+5))/4)*0.959/AS(I+2,1) !Q of bottom
        QN = (QWALL(I+3)+(QWALL(I+4)+QWALL(I+5))/4)/AS(I+3,1) !Q of top
        CALL CONDEXP2D(QN,QS,QW,QE,I,DT)
    ENDDO
    
    DO I = 357,789,6 !Start from 324 (wall) + 32 (baf.)
        SUMTL = 0
        DO M = 1,5
            SUMTL = TWO(M,1,(I+3)/6-59) + SUMTL
        ENDDO
        TW(I) = SUMTL/5 !Mean node temperature of left wall
    
        SUMTR = 0
        DO M = 1,5
            SUMTR = TWO(M,5,(I+3)/6-59) + SUMTR
        ENDDO
        TW(I+1) = SUMTR/5 !Mean node temperature of right wall                         
    
        SUMTB = 0
        DO M = 1,5
            SUMTB = TWO(5,M,(I+3)/6-59) + SUMTB
        ENDDO
        TW(I+2) = SUMTB/5 !Mean node temperature of bottom wall
    
        SUMTT = 0
        DO M = 1,5
            SUMTT = TWO(1,M,(I+3)/6-59) + SUMTT
        ENDDO
        TW(I+3) = SUMTT/5 !Mean node temperature of top wall
    
        !Mean node temperature of opposite slab surface
        TW(I+4) = (TW(I) + TW(I+1) + TW(I+2) + TW(I+3))/4 !Mean temperature of back wall
    
        !Mean node temperature of front slab surface
        TW(I+5) = TW(I+4) !Mean node temperature of front wall
    ENDDO
    
    !*********Shift load temperature***********!
    SELECT CASE(CRITERA_CONDTIONS)
    CASE(1)
        IF(MOD(INT(TIME),WR)==0) THEN
    !*********Shift load temperature***********!    
        DO L = 73,2,-1
            DO N = 1,5
                DO M = 1,5
                    TWO(M,N,L) = TWO(M,N,(L-1))
                ENDDO
            ENDDO
        ENDDO
    
        DO N = 1,5
            DO M = 1,5
                TWO(M,N,1) = 293.0
            ENDDO
        ENDDO
        
        DO I = 357,789,6 !Start from 324 (wall) + 32 (baf.)
            SUMTL = 0
            DO M = 1,5
                SUMTL = TWO(M,1,(I+3)/6-59) + SUMTL
            ENDDO
            TW(I) = SUMTL/5 !Mean node temperature of left wall
    
            SUMTR = 0
            DO M = 1,5
                SUMTR = TWO(M,5,(I+3)/6-59) + SUMTR
            ENDDO
            TW(I+1) = SUMTR/5 !Mean node temperature of right wall                         
    
            SUMTB = 0
            DO M = 1,5
                SUMTB = TWO(5,M,(I+3)/6-59) + SUMTB
            ENDDO
            TW(I+2) = SUMTB/5 !Mean node temperature of bottom wall
    
            SUMTT = 0
            DO M = 1,5
                SUMTT = TWO(1,M,(I+3)/6-59) + SUMTT
            ENDDO
            TW(I+3) = SUMTT/5 !Mean node temperature of top wall
    
            !Mean node temperature of opposite slab surface
            TW(I+4) = (TW(I) + TW(I+1) + TW(I+2) + TW(I+3))/4 !Mean temperature of back wall
    
            !Mean node temperature of front slab surface
            TW(I+5) = TW(I+4) !Mean node temperature of front wall
        ENDDO
        
    !*********Output drop-out temp. (C)*********!
        WRITE(17,'(I8,F12.3)') TIME, TW(810)-273.0
    !*********Output maximum slab temp. difference (C)*********!
        CALL bubble_sort(TWO,25,STD)
        WRITE(19,'(I8,F12.3)') TIME, STD
        ENDIF    
    CASE(2)
        IF(TIME<DS) THEN
            !IF(MOD(INT(TIME),60)==0) WRITE(23,'(I8,F12.3,F12.3,F12.3)') TIME, (TW(360+(TIME/WR-1)*6)-273.0), TWO(3,3,TIME/WR)-273.0, (TW(359+(TIME/WR-1)*6)-273.0)
            IF(MOD(INT(TIME),WR)==0) THEN !Walk rate
                WRITE(23,'(I8,F12.3,F12.3,F12.3)') TIME, (TW(360+(TIME/WR-1)*6)-273.0), TWO(3,3,TIME/WR)-273.0, (TW(359+(TIME/WR-1)*6)-273.0)
                WRITE(17,'(I8,F12.3)') TIME, TWO(3,3,73)-273.0!TW(810)-273.0
                CALL bubble_sort(TWO,25,STD)
                WRITE(19,'(I8,F12.3)') TIME, STD
                DO L = 73,2,-1
                    DO N = 1,5
                        DO M = 1,5
                            TWO(M,N,L) = TWO(M,N,(L-1))
                        ENDDO
                    ENDDO
                ENDDO
    
                DO N = 1,5
                    DO M = 1,5
                        TWO(M,N,1) = 293.0
                    ENDDO
                ENDDO
                DO I = 357,789,6 !Start from 324 (wall) + 32 (baf.)
                    SUMTL = 0
                    DO M = 1,5
                        SUMTL = TWO(M,1,(I+3)/6-59) + SUMTL
                    ENDDO
                    TW(I) = SUMTL/5 !Mean node temperature of left wall
    
                    SUMTR = 0
                    DO M = 1,5
                        SUMTR = TWO(M,5,(I+3)/6-59) + SUMTR
                    ENDDO
                    TW(I+1) = SUMTR/5 !Mean node temperature of right wall                         
    
                    SUMTB = 0
                    DO M = 1,5
                        SUMTB = TWO(5,M,(I+3)/6-59) + SUMTB
                    ENDDO
                    TW(I+2) = SUMTB/5 !Mean node temperature of bottom wall
    
                    SUMTT = 0
                    DO M = 1,5
                        SUMTT = TWO(1,M,(I+3)/6-59) + SUMTT
                    ENDDO
                    TW(I+3) = SUMTT/5 !Mean node temperature of top wall
    
                    !Mean node temperature of opposite slab surface
                    TW(I+4) = (TW(I) + TW(I+1) + TW(I+2) + TW(I+3))/4 !Mean temperature of back wall
    
                    !Mean node temperature of front slab surface
                    TW(I+5) = TW(I+4) !Mean node temperature of front wall
                ENDDO
            ENDIF
        ELSEIF((TIME>=DS).AND.(TIME<=DE)) THEN
            !IF(MOD(INT(TIME),60)==0) WRITE(23,'(I8,F12.3,F12.3,F12.3)') TIME, (TW(360+(DS/WR-1)*6)-273.0), TWO(3,3,DS/WR)-273.0, (TW(359+(DS/WR-1)*6)-273.0)
            IF(MOD(INT(TIME),WR)==0) THEN !Walk rate
                WRITE(23,'(I8,F12.3,F12.3,F12.3)') TIME, (TW(360+(DS/WR-1)*6)-273.0), TWO(3,3,DS/WR)-273.0, (TW(359+(DS/WR-1)*6)-273.0)
                WRITE(17,'(I8,F12.3)') TIME, TWO(3,3,73)-273.0!TW(810)-273.0
                CALL bubble_sort(TWO,25,STD)
                WRITE(19,'(I8,F12.3)') TIME, STD
            ENDIF
        ELSEIF(TIME>DE) THEN
            !IF(MOD(INT(TIME),60)==0) WRITE(23,'(I8,F12.3,F12.3,F12.3)') TIME, (TW(360+((TIME-(DE-DS))/WR-1)*6)-273.0), TWO(3,3,(TIME-(DE-DS))/WR)-273.0, (TW(359+((TIME-(DE-DS))/WR-1)*6)-273.0)
            IF((MOD(INT(TIME-(DE-DS)),WR)==0).AND.(TIME<(WR*73+(DE-DS)))) THEN
                WRITE(23,'(I8,F12.3,F12.3,F12.3)') TIME, (TW(360+((TIME-(DE-DS))/WR-1)*6)-273.0), TWO(3,3,(TIME-(DE-DS))/WR)-273.0, (TW(359+((TIME-(DE-DS))/WR-1)*6)-273.0)
                WRITE(17,'(I8,F12.3)') TIME, TWO(3,3,73)-273.0!TW(810)-273.0
                CALL bubble_sort(TWO,25,STD)
                WRITE(19,'(I8,F12.3)') TIME, STD
                DO L = 73,2,-1
                    DO N = 1,5
                        DO M = 1,5
                            TWO(M,N,L) = TWO(M,N,(L-1))
                        ENDDO
                    ENDDO
                ENDDO
    
                DO N = 1,5
                    DO M = 1,5
                        TWO(M,N,1) = 293.0
                    ENDDO
                ENDDO
                DO I = 357,789,6 !Start from 324 (wall) + 32 (baf.)
                    SUMTL = 0
                    DO M = 1,5
                        SUMTL = TWO(M,1,(I+3)/6-59) + SUMTL
                    ENDDO
                    TW(I) = SUMTL/5 !Mean node temperature of left wall
    
                    SUMTR = 0
                    DO M = 1,5
                        SUMTR = TWO(M,5,(I+3)/6-59) + SUMTR
                    ENDDO
                    TW(I+1) = SUMTR/5 !Mean node temperature of right wall                         
    
                    SUMTB = 0
                    DO M = 1,5
                        SUMTB = TWO(5,M,(I+3)/6-59) + SUMTB
                    ENDDO
                    TW(I+2) = SUMTB/5 !Mean node temperature of bottom wall
    
                    SUMTT = 0
                    DO M = 1,5
                        SUMTT = TWO(1,M,(I+3)/6-59) + SUMTT
                    ENDDO
                    TW(I+3) = SUMTT/5 !Mean node temperature of top wall
    
                    !Mean node temperature of opposite slab surface
                    TW(I+4) = (TW(I) + TW(I+1) + TW(I+2) + TW(I+3))/4 !Mean temperature of back wall
    
                    !Mean node temperature of front slab surface
                    TW(I+5) = TW(I+4) !Mean node temperature of front wall
                ENDDO
            ENDIF
        ENDIF
    END SELECT
    
    !*********Output fuel input (kg/hr)*********!
    FUELSUM = FUELSUM + &
            (8*(V(1)*PC(1))*BMAX(1) + &
            10*(V(1)*PC(1))*BMAX(2) + &
             8*(V(2)*PC(2))*BMAX(3) + &
             8*(V(2)*PC(2))*BMAX(4) + &
            28*(V(4)*PC(4))*BMAX(5) + &
             9*(V(3)*PC(3))*BMAX(6))/1000000000*DT
    
    WRITE(18,'(I8,F12.3,F12.3,F12.3,F12.3)') TIME, (V(1)*PC(1))*BMAX(1)/CVN*3600, &
                                                   (V(1)*PC(1))*BMAX(2)/CVN*3600, &
                                                   (V(2)*PC(2))*BMAX(3)/CVN*3600, &
                                                   (V(2)*PC(2))*BMAX(4)/CVN*3600, &
                                                   (V(4)*PC(4))*BMAX(5)/CVN*3600, &
                                                   (V(3)*PC(3))*BMAX(6)/CVN*3600, &
                                             FUELSUM
    
    !*********Output cummulated production (ton)
    SELECT CASE(CRITERA_CONDTIONS)
    CASE(1)
        PRODUCT = TIME*(0.355*0.305*7.5*7.85/WR)
    CASE(2)
        IF(TIME<=DS) THEN
            PRODUCT = TIME*(0.355*0.305*7.5*7.85/WR)
        ELSEIF(TIME>DS.AND.TIME<DE) THEN
            PRODUCT = DS*(0.355*0.305*7.5*7.85/WR)
        ELSEIF(TIME>=DE) THEN
            PRODUCT = (TIME-(DE-DS))*(0.355*0.305*7.5*7.85/WR)
        ENDIF
    ENDSELECT
    
    !*********Output specific energy consumption (GJ/ton)*********!
    SEC = FUELSUM/PRODUCT
    
    WRITE(20,'(I8,F12.3)') TIME, SEC
    
    DO I = 1,356
        HL = QWALL(I)/1000000000*DT + HL
    ENDDO
    DO I = 357,794
        HST = QWALL(I)/1000000000*DT + HST
    ENDDO
    DO J = 1,288
       HWC = GCOOL(J)/1000000000*DT + HWC
    ENDDO
    HE = (FDNEG(1,4)*ENTH(TG(1),XSAIR1) + &
         FDNEG(49,4)*ENTH(TG(49),XSAIR1) + &
         FDNEG(97,4)*ENTH(TG(97),XSAIR1) + &
         FDNEG(145,4)*ENTH(TG(145),XSAIR1) + &
         FDNEG(193,4)*ENTH(TG(193),XSAIR1) + &
         FDNEG(241,4)*ENTH(TG(241),XSAIR1))/1000000000*DT + HE
    
    HA = (8*((V(1)*PC(1))*BMAX(1)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA(TA)) + &
         10*((V(1)*PC(1))*BMAX(2)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA(TA)) + &
          8*((V(2)*PC(2))*BMAX(3)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA(TA)) + &
          8*((V(2)*PC(2))*BMAX(4)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA(TA)) + &
         28*((V(4)*PC(4))*BMAX(5)/CVN*(1+XSAIR2/100.0)*SAR*ENTHA(TA)) + &
          9*((V(3)*PC(3))*BMAX(6)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA(TA)))/1000000000*DT + HA
    
    SELECTCASE(CRITERA_STOP)
        CASE(1)    
!            ERM = 0.0
!            DO 11 I = 1,288
!            ER = ABS((TW(726)-TW726)/TW726)
!11          IF(ER>1.E-05) GOTO 1
             ERM = 0.0
             ER = 0.0
             DO 11 I = 1,324
             ER = ABS(((TWF(1,I)+TWF(2,I)+TWF(3,I)+TWF(4,I)+TWF(5,I))/5-TWF2(I))/TWF2(I))
11           IF(ER.GT.ERM) ERM=ER
             IF(ERM>1.0E-06) GO TO 1
        CASE(2)
            IF(TIME<RUNTIME) GOTO 1
    ENDSELECT
    
    !*********Print out resluts of each iteration***********!
    SELECTCASE(TIME)
        CASE(1:9)
            WRITE(FORM,'(I1)') TIME
        CASE(10:99)
            WRITE(FORM,'(I2)') TIME
        CASE(100:999)
            WRITE(FORM,'(I3)') TIME
        CASE(1000:9999)
            WRITE(FORM,'(I4)') TIME
        CASE(10000:99999)
            WRITE(FORM,'(I5)') TIME
        CASE(100000:999999)
            WRITE(FORM,'(I6)') TIME
        CASE(1000000:9999999)
            WRITE(FORM,'(I7)') TIME
        ENDSELECT
    
        WRITE(FILENAME,*) 'Operation Time ',TRIM(FORM),'s.txt'
        OPEN(UNIT=22,FILE=FILENAME)
    
    !*********Print out resluts of each iteration***********!
    WRITE(22,'(A,I2,A)') 'Convergence after',IC,' iterations'
    WRITE(22,*)
    WRITE(22,*)
    WRITE(22,'(A)') 'Gas zone temperature (C):'
    DO J = 1,6 !layers
        WRITE(22,'(16F12.3)') (TG(I+32+48*(J-1))-273.0, I=1,16)
        WRITE(22,'(16F12.3)') (TG(I+16+48*(J-1))-273.0, I=1,16)
        WRITE(22,'(16F12.3)') (TG(I+48*(J-1))-273.0, I=1,16)
        WRITE(22,*)
    ENDDO
    WRITE(22,*)
    
    DO I = 1,324
        WRITE(22,'(A,I3,A)') 'Node temperature on funace wall',INT(I),' (C):'
        WRITE(22,'(5F12.3)') (TWF(J,I)-273.0, J=1,5)
        WRITE(22,*)
    ENDDO
    WRITE(22,*)
    
    DO I = 325,356
        WRITE(22,'(A,I3,A)') 'Node temperature on baf. wall',INT(I-324),' (C):'
        WRITE(22,'(3F12.3)') (TWB(J,I-324)-273.0, J=1,3)
        WRITE(22,*)
    ENDDO
    WRITE(22,*)
    
    WRITE(22,'(A)') 'Updated surface temperature (C):'
    WRITE(22,'(A)') 'Left   '
    DO J = 3,1,-1
        WRITE(22,'(6F12.3)') (TW(I)-273.0, I=97+(J-1)*6,102+(J-1)*6)
    ENDDO
    WRITE(22,*)
    WRITE(22,'(A)') 'Roof   '
    WRITE(22,'(16F12.3)') (TW(I)-273.0, I=229,324)
    WRITE(22,*)
    WRITE(22,'(A)') 'Buttom '
    WRITE(22,'(16F12.3)') (TW(I)-273.0, I=133,228)
    WRITE(22,*)
    WRITE(22,'(A)') 'Back   '
    DO J = 3,1,-1
        WRITE(22,'(16F12.3)') (TW(I)-273.0, I=1+(J-1)*16,16+(J-1)*16)
    ENDDO
    WRITE(22,*)
    WRITE(22,'(A)') 'Front  '
    DO J = 3,1,-1
        WRITE(22,'(16F12.3)') (TW(I)-273.0, I=49+(J-1)*16,64+(J-1)*16)
    ENDDO
    WRITE(22,*)
    WRITE(22,'(A)') 'Right  '
    DO J = 3,1,-1
        WRITE(22,'(6F12.3)') (TW(I)-273.0, I=115+(J-1)*6,120+(J-1)*6)
    ENDDO
    WRITE(22,*)
    WRITE(22,*)
    
    WRITE(22,'(A)') 'Heat transfer to the furnace surfaces (W):'
    WRITE(22,'(A)') 'Left   '
    DO J = 3,1,-1
        WRITE(22,'(6F12.3)') (QWALL(I), I=97+(J-1)*6,102+(J-1)*6)
    ENDDO
    WRITE(22,*)
    WRITE(22,'(A)') 'Roof   '
    WRITE(22,'(16F12.3)') (QWALL(I), I=229,324)
    WRITE(22,*)
    WRITE(22,'(A)') 'Buttom '
    WRITE(22,'(16F12.3)') (QWALL(I), I=133,228)
    WRITE(22,*)
    WRITE(22,'(A)') 'Back   '
    DO J = 3,1,-1
        WRITE(22,'(16F12.3)') (QWALL(I), I=1+(J-1)*16,16+(J-1)*16)
    ENDDO
    WRITE(22,*)
    WRITE(22,'(A)') 'Front  '
    DO J = 3,1,-1
        WRITE(22,'(16F12.3)') (QWALL(I), I=49+(J-1)*16,64+(J-1)*16)
    ENDDO
    WRITE(22,*)
    WRITE(22,'(A)') 'Right  '
    DO J = 3,1,-1
        WRITE(22,'(6F12.3)') (QWALL(I), I=115+(J-1)*6,120+(J-1)*6)
    ENDDO
    WRITE(22,*)
    WRITE(22,'(A)') 'Baf.'
    WRITE(22,'(16F12.3)') (QWALL(I), I=325,356)
    WRITE(22,*)
    WRITE(22,*)
    
    WRITE(22,'(A)') 'Heat transfer to the obs. surfaces (W):'
    WRITE(22,'(A)') '                    Left       Right      Bottom         Top'
    DO I = 357,789,6
        WRITE(22,'(A,I3,A\)') 'Obs.',INT((I+3)/6-59),' (C):'
        WRITE(22,'(4F12.3)') QWALL(I-2*(((I+3)/6-59)-1)),QWALL(I+1-2*(((I+3)/6-59)-1)),QWALL(I+2-2*(((I+3)/6-59)-1)),QWALL(I+3-2*(((I+3)/6-59)-1))
        WRITE(22,*)
    ENDDO
    WRITE(22,*)
        
    WRITE(22,'(A)') 'Obstacle CORSS-SECTION temperature (C):'
    DO K = 1,73
        WRITE(22,'(A,I2)') 'Obs.',K
        WRITE(22,'(5F12.3)') ((TWO(I,J,K)-273.0, J=1,5), I=1,5)
    ENDDO
    WRITE(22,*)
    WRITE(22,*)
    WRITE(22,'(A)') 'Obstacle surface zone temperature (C):'
    
    WRITE(22,'(A)') '                    Left       Right      Bottom         Top      Centre'
    DO I = 357,789,6
        WRITE(22,'(A,I3,A\)') 'Obs.',INT((I+3)/6-59),' (C):'
        WRITE(22,'(5F12.3)') TW(I)-273.0,TW(I+1)-273.0,TW(I+2)-273.0,TW(I+3)-273.0,TWO(3,3,(I+3)/6-59)-273.0
        WRITE(22,*)
        AVE((I+3)/6-59)=TWO(3,3,(I+3)/6-59)-273.0
    ENDDO
    WRITE(22,*)
    
    !*****************To calculate preheated air temperature*****************!
    !TA = 692.0
    !Z0 = (TA-1400.)/200.
    !NONZERO = (ENTHAIR/1E+06)/SUMAIR
    !CALL DNEWT(Z0,FS,NONZERO)
    !IF(L.NE.0) THEN
    !    WRITE(22,'(A,F12.1,A)') ' Preheated air temerature:',(Z0*200+1400)-273.0,' C'
    !ENDIF
    WRITE(22,'(A,F12.1,A)') ' Preheated air temerature:',TA-273.0,' C'
    WRITE(22,*)
    WRITE(22,*)
    
    !*****************Start to print out energy balance results*****************!
    
    WRITE(22,*) 'Energy balance check:' 
    WRITE(22,*)
    WRITE(22,*) 'Energy input'
    
    QF = 8*((V(1)*PC(1))*BMAX(1)) + &
        10*((V(1)*PC(1))*BMAX(2)) + &
         8*((V(2)*PC(2))*BMAX(3)) + &
         8*((V(2)*PC(2))*BMAX(4)) + &
        28*((V(4)*PC(4))*BMAX(5)) + &
         9*((V(3)*PC(3))*BMAX(6))
    
    QA = 8*((V(1)*PC(1))*BMAX(1)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA(TA)) + &
        10*((V(1)*PC(1))*BMAX(2)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA(TA)) + &
         8*((V(2)*PC(2))*BMAX(3)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA(TA)) + &
         8*((V(2)*PC(2))*BMAX(4)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA(TA)) + &
        28*((V(4)*PC(4))*BMAX(5)/CVN*(1+XSAIR2/100.0)*SAR*ENTHA(TA)) + &
         9*((V(3)*PC(3))*BMAX(6)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA(TA))
    
    !QA = 8*((V(1)*PC(1))*BMAX(1)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA((Z0*200+1400))) + &
    !    10*((V(1)*PC(1))*BMAX(2)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA((Z0*200+1400))) + &
    !     8*((V(2)*PC(2))*BMAX(3)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA((Z0*200+1400))) + &
    !     8*((V(2)*PC(2))*BMAX(4)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA((Z0*200+1400))) + &
    !    28*((V(4)*PC(4))*BMAX(5)/CVN*(1+XSAIR2/100.0)*SAR*ENTHA((Z0*200+1400))) + &
    !     9*((V(3)*PC(3))*BMAX(6)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA((Z0*200+1400)))
    
    MASSIN = 8*((V(1)*PC(1))*BMAX(1))/CVN + &
            10*((V(1)*PC(1))*BMAX(2))/CVN + &
             8*((V(2)*PC(2))*BMAX(3))/CVN + &
             8*((V(2)*PC(2))*BMAX(4))/CVN + &
            28*((V(4)*PC(4))*BMAX(5))/CVN + &
             9*((V(3)*PC(3))*BMAX(6))/CVN + &
             8*((V(1)*PC(1))*BMAX(1)/CVN*(1+XSAIR1/100.0)*SAR) + &
            10*((V(1)*PC(1))*BMAX(2)/CVN*(1+XSAIR1/100.0)*SAR) + &
             8*((V(2)*PC(2))*BMAX(3)/CVN*(1+XSAIR1/100.0)*SAR) + &
             8*((V(2)*PC(2))*BMAX(4)/CVN*(1+XSAIR1/100.0)*SAR) + &
            28*((V(4)*PC(4))*BMAX(5)/CVN*(1+XSAIR2/100.0)*SAR) + &
             9*((V(3)*PC(3))*BMAX(6)/CVN*(1+XSAIR1/100.0)*SAR)
    
    MASSOUT = FDNEG(1,4) + FDNEG(49,4) + FDNEG(97,4) + FDNEG(145,4) + FDNEG(193,4) + FDNEG(241,4)
    
    !WRITE(22,'(A,E12.3,A)') ' Qfuel+Qair:',QC,' W'
    WRITE(22,'(A,E12.3,A)') ' Qfuel:',QF,' W'
    WRITE(22,'(A,E12.3,A)') ' Qair:',QA,' W'
    WRITE(22,*)
    WRITE(22,*) 'Energy output'
    
    QS = 0.0
    DO I = 1,356
        QS = QWALL(I) + QS
    ENDDO
    WRITE(22,'(A,E12.3,A)') ' Q to wall surfaces:',QS,' W'
    QO = 0.0
    DO I = 357,794
        QO = QWALL(I) + QO
    ENDDO
    WRITE(22,'(A,E12.3,A)') ' Q to slab surfaces:',QO,' W'
    GC = 0.0
    DO J = 1,288
       GC = GCOOL(J) + GC
    ENDDO
    WRITE(22,'(A,E12.3,A)') ' Q to cooling surfaces:',GC,' W'
    
    QEXIT = FDNEG(1,4)*ENTH(TG(1),XSAIR1) + &
            FDNEG(49,4)*ENTH(TG(49),XSAIR1) + &
            FDNEG(97,4)*ENTH(TG(97),XSAIR1) + &
            FDNEG(145,4)*ENTH(TG(145),XSAIR1) + &
            FDNEG(193,4)*ENTH(TG(193),XSAIR1) + &
            FDNEG(241,4)*ENTH(TG(241),XSAIR1)
    
    WRITE(22,'(A,E12.3,A)') ' Q loss from exit:  ',QEXIT,' W'
    WRITE(22,*)
    WRITE(22,'(A,F5.1,A)') ' Energy balance = (Energy input-Energy output)/Energy input = ',(QF+QA-QS-QO-GC-QEXIT)*100/(QF+QA),' %'
    WRITE(22,*)
    CQ = GC/QF*100
    WRITE(22,'(A,E12.3,A)') ' Cooling Q / Fuel Q :',CQ,' %'
    
    WRITE(22,*)
    WRITE(22,'(A,E12.3)') ' Mass input:', MASSIN
    WRITE(22,'(A,E12.3)') ' Mass output:', MASSOUT
    
    
    WRITE(22,*)
    WRITE(22,'(A,E12.5)') 'Gas convection = ', GCONVECTION
    WRITE(22,'(A,E12.5)') 'Wall convection = ', SCONVECTION
    
    WRITE(22,*)
    WRITE(22,'(A)') 'Mass balance check for internal boundaries'
    DO I=1,288
        DO J=1,6            
            FDNET(I,J) = DOT_PRODUCT(COEN(J,1:35,I),AAA(1:35))
            FDGRO(I,J) = DOT_PRODUCT(COEG(J,1:35,I),AAA(1:35))
            FDPOS(I,J) = FD*ABS((FDNET(I,J)+FDGRO(I,J))/2) !Flow to positive direction
            FDNEG(I,J) = FD*ABS((FDNET(I,J)-FDGRO(I,J))/2) !Flow to negtive direction
            SELECT CASE (I)
                CASE(38:41) !Control zone 1
                    GCOMB = (V(1)*PC(1))*BMAX(1)/CVN + (V(1)*PC(1))*BMAX(1)/CVN*(1+XSAIR1/100.0)*SAR
                    FDPOS(I,5) = GCOMB
                CASE(278:281)
                    GCOMB = (V(1)*PC(1))*BMAX(1)/CVN + (V(1)*PC(1))*BMAX(1)/CVN*(1+XSAIR1/100.0)*SAR
                    FDNEG(I,2) = GCOMB
                CASE(4:6,9) !Control zone 2
                    GCOMB = (V(1)*PC(1))*BMAX(2)/CVN + (V(1)*PC(1))*BMAX(2)/CVN*(1+XSAIR1/100.0)*SAR
                    FDPOS(I,5) = GCOMB
                CASE(244:246,249)
                    GCOMB = (V(1)*PC(1))*BMAX(2)/CVN + (V(1)*PC(1))*BMAX(2)/CVN*(1+XSAIR1/100.0)*SAR
                    FDNEG(I,2) = GCOMB
                CASE(7,8) !Half burner in control zone 2
                    GCOMB = 0.5*((V(1)*PC(1))*BMAX(2)/CVN + (V(1)*PC(1))*BMAX(2)/CVN*(1+XSAIR1/100.0)*SAR)
                    FDPOS(I,5) = GCOMB
                CASE(247,248) !Half burner in control zone 2
                    GCOMB = 0.5*((V(1)*PC(1))*BMAX(2)/CVN + (V(1)*PC(1))*BMAX(2)/CVN*(1+XSAIR1/100.0)*SAR)
                    FDNEG(I,2) = GCOMB
                CASE(42:45) !Control zone 3
                    GCOMB = (V(2)*PC(2))*BMAX(3)/CVN + (V(2)*PC(2))*BMAX(3)/CVN*(1+XSAIR1/100.0)*SAR
                    FDPOS(I,5) = GCOMB
                CASE(282:285) !Control zone 3
                    GCOMB = (V(2)*PC(2))*BMAX(3)/CVN + (V(2)*PC(2))*BMAX(3)/CVN*(1+XSAIR1/100.0)*SAR
                    FDNEG(I,2) = GCOMB
                CASE(10:13) !Control zone 4
                    GCOMB = (V(2)*PC(2))*BMAX(4)/CVN + (V(2)*PC(2))*BMAX(4)/CVN*(1+XSAIR1/100.0)*SAR
                    FDPOS(I,5) = GCOMB
                CASE(250:253) !Control zone 4
                    GCOMB = (V(2)*PC(2))*BMAX(4)/CVN + (V(2)*PC(2))*BMAX(4)/CVN*(1+XSAIR1/100.0)*SAR
                    FDNEG(I,2) = GCOMB
                CASE(46,47,94,95,238,239,286,287) !Control zone 5
                    GCOMB = (V(4)*PC(4))*BMAX(5)/CVN + (V(4)*PC(4))*BMAX(5)/CVN*(1+XSAIR2/100.0)*SAR
                    FDNEG(I,3) = GCOMB
                CASE(142,143,190,191) !One and half burners in Control zone 5 
                    GCOMB = 1.5*((V(4)*PC(4))*BMAX(5)/CVN + (V(4)*PC(4))*BMAX(5)/CVN*(1+XSAIR2/100.0)*SAR)
                    FDNEG(I,3) = GCOMB
                CASE(48,96,240,288) !Two burners in Control zone 5 
                    GCOMB = 2*((V(4)*PC(4))*BMAX(5)/CVN + (V(4)*PC(4))*BMAX(5)/CVN*(1+XSAIR2/100.0)*SAR)
                    FDNEG(I,3) = GCOMB
                CASE(144,192) !Three burners in Control zone 5 
                    GCOMB = 3*((V(4)*PC(4))*BMAX(5)/CVN + (V(4)*PC(4))*BMAX(5)/CVN*(1+XSAIR2/100.0)*SAR)
                    FDNEG(I,3) = GCOMB
                CASE(16,256) !Control zone 6
                    GCOMB = (V(3)*PC(3))*BMAX(6)/CVN + (V(3)*PC(3))*BMAX(6)/CVN*(1+XSAIR1/100.0)*SAR
                    FDNEG(I,1) = GCOMB
                CASE(112,160) !Control zone 6
                    GCOMB = 1.5*((V(3)*PC(3))*BMAX(6)/CVN + (V(3)*PC(3))*BMAX(6)/CVN*(1+XSAIR1/100.0)*SAR)
                    FDNEG(I,1) = GCOMB
                CASE(64,208) !Control zone 6
                    GCOMB = 2*((V(3)*PC(3))*BMAX(6)/CVN + (V(3)*PC(3))*BMAX(6)/CVN*(1+XSAIR1/100.0)*SAR)
                    FDNEG(I,1) = GCOMB
            ENDSELECT
        ENDDO
        WRITE(22,'(A,I5,F12.5,F12.5,F12.5,F12.5,F12.5,F12.5,F12.5,F12.5,F12.5,F12.5,F12.5,F12.5)') 'Gas zone ', I, FDPOS(I,1), FDNEG(I,1), FDPOS(I,2), FDNEG(I,2), FDPOS(I,3), FDNEG(I,3), FDPOS(I,4), FDNEG(I,4), FDPOS(I,5), FDNEG(I,5), FDPOS(I,6), FDNEG(I,6)
    ENDDO
    
    WRITE(22,*)
    WRITE(22,'(A)') '     Comparison of SRSM Furnace Energy Balance by UoSW and STC'
    WRITE(22,'(A)')           '--------------------------------------------------------------------------------------------------'
    WRITE(22,'(A)')          '                            Input                  Output                       Performance'
    WRITE(22,'(A)')           '                         ----------     ---------------------------    ---------------------------'
    WRITE(22,'(A)')          '              Units      Hf      Ha     Hst      He     Hwc      Hl    Ceff,%  Feff,%   SEC,GJ/t'
    WRITE(22,'(A)')           '--------------------------------------------------------------------------------------------------'
    WRITE(22,'(A,6F8.3)')    'UoSW Trial       (MW)', FUELSUM*1000/TIME, HA*1000/TIME, HST*1000/TIME, HE*1000/TIME, HWC*1000/TIME, HL*1000/TIME
    WRITE(22,'(A,XA,8F8.3)') 'Energy Balance  (%Hf)', '100.000', HA/FUELSUM*100, HST/FUELSUM*100, HE/FUELSUM*100, HWC/FUELSUM*100, HL/FUELSUM*100, (1+(HA*1000/TIME)/(FUELSUM*1000/TIME)-(HE*1000/TIME)/(FUELSUM*1000/TIME))*100,((HST*1000/TIME)/(FUELSUM*1000/TIME))*100,SEC
    WRITE(22,'(A)')           '--------------------------------------------------------------------------------------------------'
    WRITE(22,'(A)')          'STC Trial        (MW)  41.600   5.500  18.900  15.900   8.100   4.200'
    WRITE(22,'(A)')          'Energy Balance  (%Hf) 100.000  13.000  45.000  38.000  19.000  10.000  75.100  45.500   1.760'
    WRITE(22,'(A)')           '--------------------------------------------------------------------------------------------------'
    
    CALL SYSTEM_CLOCK(TIME_END,COUNT_RATE)
    WRITE(22,*)
    WRITE(22,*)
    WRITE(22,*) 'Time of cpu operation was:', INT(REAL(TIME_END - TIME_BEGIN)/REAL(COUNT_RATE)),'seconds'
    WRITE(*,*)
    WRITE(*,*) 'Time of cpu operation was:', INT(REAL(TIME_END - TIME_BEGIN)/REAL(COUNT_RATE)),'seconds'
    WRITE(*,*)
    WRITE(*,*)
    
30  WRITE(*,'(A)') 'What do you want to do next?\N'C, '1. Plot slab temperature;\N'C, '2. Change walk rate;\N'C, '3. Plot drop-out temperature;\N'C, '4. Plot fuel input;\N'C, '5. Plot slab maximum temp. defference;\N'C, '6. Specific energy consumption;\N'C, '7. Wall temperatures of control zones;\N'C, '8. Save output data;\N'C, '9. Quit;\N'C
    WRITE(*,'(A\)') '>_'
    READ(*,'(I1)') OPTION
    WRITE(*,*)
    WRITE(*,*)
    SELECTCASE(OPTION)
        CASE(1)
    !*******************Call Matlab to plot results*******************!
            IF(CRITERA_CONDTIONS==1) THEN 
                DO K=1,73
                    WRITE(23,'(I8,F12.3,F12.3,F12.3)') (MOD(TIME,WR)+WR*(K-1)), (TW(360+6*(K-1))-273.0), AVE(K), (TW(359+6*(K-1))-273.0)
                ENDDO
            ENDIF
            CALL SYSTEM("matlab -r PLOT_SlabT")
            GOTO 30
        
        CASE(2)
        !*******************Save convergent data*******************!
            WRITE(*,'(A\)') 'Please specify walk rate (s):>_ '
            READ(*,'(I4)') WR
            WRITE(*,*)
            CLOSE(UNIT=17,STATUS='DELETE')
            CLOSE(UNIT=18,STATUS='DELETE')
            CLOSE(UNIT=19,STATUS='DELETE')
            CLOSE(UNIT=20,STATUS='DELETE')
            CLOSE(UNIT=21,STATUS='DELETE')
    
            DT = 10
            TIME = 0
            FUELSUM = 0
            GOTO 40
    
        CASE(3)
            CALL SYSTEM("matlab -r PLOT_DropT")
            GOTO 30
    
        CASE(4)
            CALL SYSTEM("matlab -r PLOT_FuelIn")
            GOTO 30
    
        CASE(5)
            CALL SYSTEM("matlab -r PLOT_SlabTD")
            GOTO 30
    
        CASE(6)
            CALL SYSTEM("matlab -r PLOT_SpEnCon")
            GOTO 30
    
        CASE(7)
            CALL SYSTEM("matlab -r PLOT_WallTemp")
            GOTO 30
        
            !WRITE(SLABT,*) 'SlabT.txt'
            !OPEN(UNIT=23,FILE=SLABT,STATUS='UNKNOWN',IOSTAT=NERR)
            !DO K=1,73
            !WRITE(23,'(I8,F12.3,F12.3,F12.3)') (MOD(TIME,WR)+WR*(K-1)), (TW(360+6*(K-1))-273.0), AVE(K), (TW(359+6*(K-1))-273.0)
            !ENDDO
        
        CASE(8)
            WRITE(GAST,*) 'GasT.txt'
            OPEN(UNIT=26,FILE=GAST,STATUS='UNKNOWN',IOSTAT=NERR)
            WRITE(26,'(F12.3)') (TG(I), I=1,288)
            
            WRITE(WNT,*) 'FurnaceWallNodeT.txt'
            OPEN(UNIT=27,FILE=WNT,STATUS='UNKNOWN',IOSTAT=NERR)
            WRITE(27,'(5F12.3)') ((TWF(I,J), J=1,324), I=1,5)
            
            WRITE(BNT,*) 'BafWallNodeT.txt'
            OPEN(UNIT=28,FILE=BNT,STATUS='UNKNOWN',IOSTAT=NERR)
            WRITE(28,'(3F12.3)') ((TWB(I,J), J=1,32), I=1,3)
            
            WRITE(ONT,*) 'ObsWallNodeT.txt'
            OPEN(UNIT=29,FILE=ONT,STATUS='UNKNOWN',IOSTAT=NERR)
            DO K=1,73
            WRITE(29,'(5F12.3)') ((TWO(I,J,K), J=1,5), I=1,5)
            WRITE(29,*)
            ENDDO
            
            WRITE(CONTROL,*) 'PC.txt'
            OPEN(UNIT=30,FILE=CONTROL,STATUS='UNKNOWN',IOSTAT=NERR)
            WRITE(30,'(4F12.3)') (PC(I), I=1,4)

        CASE(9)
            CONTINUE
    ENDSELECT
    
    END PROGRAM SRSM

    
    SUBROUTINE FX(TG,F)
    DIMENSION NCOE(708,38),COEN(6,35,288),GCOE(708,38),COEG(6,35,288),AAA(35),&
    FDNET(288,6),FDGRO(288,6),FDPOS(288,6),FDNEG(288,6),V(4),G2G(288,6),G2W(288,3,2),&
    W2G(324,2),G2O(288,6,2),O2G(438,6,3),G2B(288,4,2),B2G(32,2),SS(794,794,4),SG(794,288,4),&
    GG(288,288,4),GS(288,794,4),AS(794,1),EMIS(794,1),FKV(288,4),B1(4),B2(4),B3(4),KGN(4),F(288),&
    TG(288),TG2(288),QWALL(794),TW(794),TWF2(794),TWF(5,324),TWB(3,32),TWO(5,5,73),TSP(4),PB(4),PC(4),RT(4),&
    SUMSG(288),AVE(73),EA(6),MAIR(6),BMAX(6),GE(6),SETPT(342,4),CAREA(288,1),GCOOL(288),TD(4),TM(4),ERR1(4),&
    ERR2(4),ERR3(4),ERR4(4),INCREASE(4),TPV(4),KP(4),KI(4),KD(4)
    REAL MAIR,GCONVECTION
    COMMON /COMM1/G2G,G2W,W2G,G2O,O2G,G2B,B2G,AS,FKV,FDPOS,FDNEG,SS,SG,GS,GG,SUMSG,AVE,AAA,FDGRO,FDNET,GE,SETPT
    COMMON /COMM3/XSAIR1,XSAIR2,SAR,RHCP,PPCS,PPC,TA,TW,TWF2,CVN
    COMMON /COMM4/TWF,TWB,TWO,EMIS
    COMMON /COMM7/PC,V,BMAX,FD,TAM,MU,EA,MAIR,RUPEFF,ENTHAIR,SUMAIR,NONZERO
    COMMON /COMM8/CAREA
    COMMON /COMM9/GCOOL
    COMMON /COMM10/GCONVECTION
    
    DATA B1/0.245282778,0.284409909,0.380029865,0.090277448/
    DATA B2/1.13E-04,0.000184829,-0.000224733,-7.26494E-05/
    DATA B3/3.06716E-08,-8.02274E-08,3.3316E-08,1.62397E-08/
    DATA SI,HTCIF,HTCIO,TAM/5.67E-08,20.0,30.0,293/ !SIGMA, HEAT TRANSFER COEFFICIENT, AMBIENT TEMPERATURE
    DATA MU/11.0/ !Forced liquid (flowing) water - Free Convection Gas: 10-40 W/m2K (combustion chamber + radiation)
    
    !SUMAIR = 0.0
    !MAIR = 0.0
    !MAIR(1) = 8*(V(1)*PC(1))*BMAX(1)/CVN*(1+XSAIR1/100.0)*SAR
    !MAIR(2) = 10*(V(1)*PC(1))*BMAX(2)/CVN*(1+XSAIR1/100.0)*SAR
    !MAIR(3) = 8*(V(2)*PC(2))*BMAX(3)/CVN*(1+XSAIR1/100.0)*SAR
    !MAIR(4) = 8*(V(2)*PC(2))*BMAX(4)/CVN*(1+XSAIR1/100.0)*SAR
    !MAIR(5) = 28*(V(4)*PC(4))*BMAX(5)/CVN*(1+XSAIR2/100.0)*SAR
    !MAIR(6) = 9*(V(3)*PC(3))*BMAX(6)/CVN*(1+XSAIR1/100.0)*SAR
    !SUMAIR = MAIR(1) + MAIR(2) + MAIR(3) + MAIR(4) + MAIR(5) + MAIR(6)
    
    TA = (FDNEG(  1,4)*TG(  1)/(FDNEG(1,4)+FDNEG(49,4)+FDNEG(97,4)+FDNEG(145,4)+FDNEG(193,4)+FDNEG(241,4)) + &
          FDNEG( 49,4)*TG( 49)/(FDNEG(1,4)+FDNEG(49,4)+FDNEG(97,4)+FDNEG(145,4)+FDNEG(193,4)+FDNEG(241,4)) + &
          FDNEG( 97,4)*TG( 97)/(FDNEG(1,4)+FDNEG(49,4)+FDNEG(97,4)+FDNEG(145,4)+FDNEG(193,4)+FDNEG(241,4)) + &
          FDNEG(145,4)*TG(145)/(FDNEG(1,4)+FDNEG(49,4)+FDNEG(97,4)+FDNEG(145,4)+FDNEG(193,4)+FDNEG(241,4)) + &
          FDNEG(193,4)*TG(193)/(FDNEG(1,4)+FDNEG(49,4)+FDNEG(97,4)+FDNEG(145,4)+FDNEG(193,4)+FDNEG(241,4)) + &
          FDNEG(241,4)*TG(241)/(FDNEG(1,4)+FDNEG(49,4)+FDNEG(97,4)+FDNEG(145,4)+FDNEG(193,4)+FDNEG(241,4)) - &
          TAM)*RUPEFF + TAM
    
    !DO L = 1,6
    !EA(L) = ENTHAIR*(MAIR(L)/SUMAIR) !Times mass weighted factor
    !ENDDO
    
    !************************* Heat balance on gas zone I **************************
    GCONVECTION = 0.0
    DO I = 1,288
        SUMGG = 0 !SUMGG
        DO J = 1,288
            GGDFA = 0
        DO K = 1,4
            GGDFA = (B1(K)+B2(K)*TG(J)+B3(K)*TG(J)**2)*GG(J,I,K) + GGDFA
        ENDDO
            SUMGG = SUMGG + GGDFA*(SI*TG(J)**4)
        ENDDO
        
        GEMIT = 0 !GEMIT
        DO K = 1,4
            GEMIT = (B1(K)+B2(K)*TG(I)+B3(K)*TG(I)**2)*FKV(I,K) + GEMIT
        ENDDO
        GEMIT = GEMIT*(SI*TG(I)**4)
        
        GCONV = 0 !GCONV
        !Convction to furnace wall
        DO M = 1,3
            IF(G2W(I,M,2) > 0) THEN
                GCONV = HTCIF*G2W(I,M,1)*(TG(I)-TW(INT(ABS(G2W(I,M,2))))) + GCONV
            ENDIF
        ENDDO
        !Convction to baf. wall
        DO M = 1,4
            IF(G2B(I,M,2) > 0) THEN
                GCONV = HTCIF*G2B(I,M,1)*(TG(I)-TW(324+INT(ABS(G2B(I,M,2))))) + GCONV
            ENDIF
        ENDDO
        !Convction to obs. wall
        DO M = 1,6
            IF(G2O(I,M,2) > 0) THEN
                GCONV = HTCIO*G2O(I,M,1)*(TG(I)- &
                (TW(356+(INT(ABS(G2O(I,M,2)))-1)*6+1) + &
                 TW(356+(INT(ABS(G2O(I,M,2)))-1)*6+2) + &
                 TW(356+(INT(ABS(G2O(I,M,2)))-1)*6+3) + &
                 TW(356+(INT(ABS(G2O(I,M,2)))-1)*6+4))/4) + GCONV
            ENDIF
        ENDDO
        
        GCOMB = 0 !GCOMB
        DO M = 1,3
            SELECT CASE (INT(ABS(G2W(I,M,2))))
                CASE(38:41,86:89) !Control zone 1
                    GCOMB = (V(1)*PC(1))*BMAX(1) + (V(1)*PC(1))*BMAX(1)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA(TA)
                    !GCOMB = (V(1)*PC(1))*BMAX(1) + EA(1)/8
                CASE(4:6,9,52:54,57) !Control zone 2
                    GCOMB = (V(1)*PC(1))*BMAX(2) + (V(1)*PC(1))*BMAX(2)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA(TA)
                    !GCOMB = (V(1)*PC(1))*BMAX(2) + EA(2)/10
                CASE(7,8,55,56) !Half burner in control zone 2
                    GCOMB = 0.5*(V(1)*PC(1)*BMAX(2) + (V(1)*PC(1))*BMAX(2)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA(TA))
                    !GCOMB = 0.5*(V(1)*PC(1)*BMAX(2) + EA(2)/10)
                CASE(42:45,90:93) !Control zone 3
                    GCOMB = (V(2)*PC(2))*BMAX(3) + (V(2)*PC(2))*BMAX(3)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA(TA)
                    !GCOMB = (V(2)*PC(2)*BMAX(3) + EA(3)/8)
                CASE(10:13,58:61) !Control zone 4
                    GCOMB = (V(2)*PC(2))*BMAX(4) + (V(2)*PC(2))*BMAX(4)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA(TA)
                    !GCOMB = (V(2)*PC(2)*BMAX(4) + EA(4)/8)
                CASE(242,243,258,259,306,307,322,323) !Control zone 5
                    GCOMB = (V(4)*PC(4))*BMAX(5) + (V(4)*PC(4))*BMAX(5)/CVN*(1+XSAIR2/100.0)*SAR*ENTHA(TA)
                    !GCOMB = (V(4)*PC(4)*BMAX(5) + EA(5)/28)
                CASE(274,275,290,291) !One and half burners in Control zone 5 
                    GCOMB = 1.5*(V(4)*PC(4)*BMAX(5) + (V(4)*PC(4))*BMAX(5)/CVN*(1+XSAIR2/100.0)*SAR*ENTHA(TA))
                    !GCOMB = 1.5*(V(4)*PC(4)*BMAX(5) + EA(5)/28)
                CASE(244,260,308,324) !Two burners in Control zone 5 
                    GCOMB = 2*(V(4)*PC(4)*BMAX(5) + (V(4)*PC(4))*BMAX(5)/CVN*(1+XSAIR2/100.0)*SAR*ENTHA(TA))
                    !GCOMB = 2*(V(4)*PC(4)*BMAX(5) + EA(5)/28)
                CASE(276,292) !Three burners in Control zone 5 
                    GCOMB = 3*(V(4)*PC(4)*BMAX(5) + (V(4)*PC(4))*BMAX(5)/CVN*(1+XSAIR2/100.0)*SAR*ENTHA(TA))
                    !GCOMB = 3*(V(4)*PC(4)*BMAX(5) + EA(5)/28)
                CASE(115,120) !Control zone 6
                    GCOMB = (V(3)*PC(3))*BMAX(6) + (V(3)*PC(3))*BMAX(6)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA(TA)
                    !GCOMB = (V(3)*PC(3)*BMAX(6) + EA(6)/9)
                CASE(117,118) !Control zone 6
                    GCOMB = 1.5*(V(3)*PC(3)*BMAX(6) + (V(3)*PC(3))*BMAX(6)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA(TA))
                    !GCOMB = 1.5*(V(3)*PC(3)*BMAX(6) + EA(6)/9)
                CASE(116,119) !Control zone 6
                    GCOMB = 2*(V(3)*PC(3)*BMAX(6) + (V(3)*PC(3))*BMAX(6)/CVN*(1+XSAIR1/100.0)*SAR*ENTHA(TA))
                    !GCOMB = 2*(V(3)*PC(3)*BMAX(6) + EA(6)/9)
            ENDSELECT
        ENDDO
        
        GENTH = 0 !GENTH
        GE = 0
        SELECT CASE(I)
            CASE(1,49,97,145,193,241)
            DO M = 1,6
                IF(G2G(I,M) > 0) THEN
                    SELECT CASE(M)
                        CASE(1,2,3)
                            GE(M) = FDNEG(I,M)*ENTH(TG(INT(ABS(G2G(I,M)))),XSAIR1) - FDPOS(I,M)*ENTH(TG(I),XSAIR1)
                        CASE(4)
                            GE(M) = - FDNEG(I,M)*ENTH(TG(I),XSAIR1) !Enthalpy out the gas zone
                        CASE(5,6)
                            GE(M) = FDPOS(I,M)*ENTH(TG(INT(ABS(G2G(I,M)))),XSAIR1) - FDNEG(I,M)*ENTH(TG(I),XSAIR1)
                    ENDSELECT
                ENDIF
            ENDDO
            GENTH = GE(1) + GE(2) + GE(3) + GE(4) + GE(5) + GE(6)
            CASE DEFAULT
            DO M = 1,6
                IF(G2G(I,M) > 0) THEN
                    SELECT CASE(M)
                        CASE(1,2,3)
                            GE(M) = FDNEG(I,M)*ENTH(TG(INT(ABS(G2G(I,M)))),XSAIR1) - FDPOS(I,M)*ENTH(TG(I),XSAIR1)   
                        CASE(4,5,6)
                            GE(M) = FDPOS(I,M)*ENTH(TG(INT(ABS(G2G(I,M)))),XSAIR1) - FDNEG(I,M)*ENTH(TG(I),XSAIR1)
                    ENDSELECT
                ENDIF
            ENDDO
            GENTH = GE(1) + GE(2) + GE(3) + GE(4) + GE(5) + GE(6)
            ENDSELECT
            
            GCOOL(I) = MU*CAREA(I,1)*(TG(I)-288.0) !288.0 is cooling water temperature in K.
    
    F(I) = SUMGG + SUMSG(I) - GEMIT - GCONV + GCOMB + GENTH - GCOOL(I)
    GCONVECTION = GCONVECTION + GCONV
    ENDDO
    RETURN
    END
    
    
    SUBROUTINE CONDEXP1D_F(Q,I,DT)
!   1D conduction model for wall
    DIMENSION TWF(5,324),TWB(3,32),TWO(5,5,73),LAMBDA(5),CP(5),A(4),B(3),AC(50,4),FO1D(5),EMIS(794,1) !TWF, Wall surface T; AC, Array coefficients
    REAL LAMBDA, CP
    INTEGER DT
    COMMON /COMM4/TWF,TWB,TWO,EMIS
!   POLYNOMIAL COEFFICIENTS FOR THERMAL CONDUCTIVITY OF INSULATING FIRE BRICK, TUCKER'S BOOK P406
    !DATA A/0.34166735E+00,0.71161834E-04,-0.3412567E-07,0.3240756E-10/
!   POLYNOMIAL COEFFICIENTS FOR SPECIFIC HEAT OF REFRACTORIES, TUCKER'S BOOK P404
    DATA B/0.100164E+04,0.1583E+00,-0.458133E-04/
!   POLYNOMIAL COEFFICIENTS FOR THERMAL CONDUCTIVITY OF DENSE BRICK, TUCKER'S BOOK P406
    DATA A/1.8406105E+00,0.10118265E-03,-0.516367E-07,0.12778578E-09/
    DATA SI,HTCO,TAM/5.67E-08,10.0,293/ !SIGMA, HEAT TRANSFER COEFFICIENT, AMBIENT TEMPERATURE
    DATA ROU/2600.0/
    DATA THICK_WALL/0.4500/
    DATA DX_WALL/0.1125/
    
    !IF((I>=65).AND.(I<=73)) THEN
    !THICK = THICK_CEILING
    !DX = DX_CEILING
    !ELSEIF((I>=53).AND.(I<=64)) THEN
    !THICK = THICK_HEARTH
    !DX = DX_HEARTH
    !ELSE
    THICK = THICK_WALL
    DX = DX_WALL
    !ENDIF
    
    NX = THICK/DX + 1 ! numbers of nodes.
    
    DO K = 1,NX
    LAMBDA(K) = A(1) + A(2)*(TWF(K,I)-273.0) + A(3)*(TWF(K,I)-273.0)**2 + A(4)*(TWF(K,I)-273.0)**3
    CP(K) = B(1) + B(2)*(TWF(K,I)-273.0) + B(3)*(TWF(K,I)-273.0)**2
    FO1D(K) = LAMBDA(K)*DT/ROU/CP(K)/DX**2
    ENDDO
    
    AC(1,1) = 0.
    AC(1,2) = 1.+FO1D(1)
    AC(1,3) = -FO1D(1)
    AC(NX,1) = -FO1D(NX)
    AC(NX,2) = 1+FO1D(NX)+FO1D(NX)*HTCO/LAMBDA(NX)*DX+FO1D(NX)*EMIS(I,1)*SI*DX/LAMBDA(NX)*TWF(NX,I)**3
    AC(NX,3) = 0.
    DO K = 2,NX-1
    AC(K,1) = FO1D(K)/2.
    AC(K,2) = -(1+FO1D(K))
    AC(K,3) = FO1D(K)/2.
    ENDDO
    AC(1,4) = (1-FO1D(1))*TWF(1,I) + FO1D(1)*TWF(2,I) + (FO1D(1)*DX/LAMBDA(1))*Q*2 !Q, W/m2
    AC(NX,4) = FO1D(NX)*TWF(NX-1,I) + (1-FO1D(NX)-FO1D(NX)*HTCO/LAMBDA(NX)*DX)*TWF(NX,I) + 2*FO1D(NX)*HTCO/LAMBDA(NX)*DX*TAM + 2*FO1D(NX)*EMIS(I,1)*SI*DX/LAMBDA(NX)*TAM**4 - FO1D(NX)*EMIS(I,1)*SI*DX/LAMBDA(NX)*TWF(NX,I)**4
    DO K = 2,NX-1
    AC(K,4) = -FO1D(K)/2*TWF(K-1,I) - (1-FO1D(K))*TWF(K,I) - FO1D(K)/2*TWF(K+1,I)
    ENDDO
    CALL TRDG(AC,NX)
    DO K = 1,NX
    TWF(K,I) = AC(K,4)
    ENDDO
    RETURN
    END
    

    SUBROUTINE CONDEXP1D_B(Q,I,DT)
!   1D conduction model for baf.
    DIMENSION TWF(5,324),TWB(3,32),TWO(5,5,73),LAMBDA(3),CP(3),A(4),B(3),AC(50,4),FO1D(3),THICK(2),DX(2) !TWB, Baf. surface T; AC, Array coefficients
    REAL LAMBDA, CP
    INTEGER DT
    COMMON /COMM4/TWF,TWB,TWO,EMIS
!   POLYNOMIAL COEFFICIENTS FOR THERMAL CONDUCTIVITY OF INSULATING FIRE BRICK, TUCKER'S BOOK P406
    !DATA A/0.34166735E+00,0.71161834E-04,-0.3412567E-07,0.3240756E-10/
!   POLYNOMIAL COEFFICIENTS FOR SPECIFIC HEAT OF REFRACTORIES, TUCKER'S BOOK P404
    DATA B/0.100164E+04,0.1583E+00,-0.458133E-04/
!   POLYNOMIAL COEFFICIENTS FOR THERMAL CONDUCTIVITY OF DENSE BRICK, TUCKER'S BOOK P406
    DATA A/1.8406105E+00,0.10118265E-03,-0.516367E-07,0.12778578E-09/
    DATA ROU/2600.0/
    DATA THICK/0.115,0.23/
    DATA DX/0.0575,0.115/
    
    NX = INT(THICK(1)/DX(1) + 1) ! numbers of nodes.
    
    IF(I>324.AND.I<337) THEN    
    J = 1
    DO K = 1,NX
    LAMBDA(K) = A(1) + A(2)*(TWB(K,I-324)-273.0) + A(3)*(TWB(K,I-324)-273.0)**2 + A(4)*(TWB(K,I-324)-273.0)**3
    CP(K) = B(1) + B(2)*(TWB(K,I-324)-273.0) + B(3)*(TWB(K,I-324)-273.0)**2
    FO1D(K) = LAMBDA(K)*DT/ROU/CP(K)/DX(J)**2
    ENDDO
    AC(1,1) = 0.
    AC(1,2) = 1.+FO1D(1)
    AC(1,3) = -FO1D(1)
    AC(NX,1) = -FO1D(NX)
    AC(NX,2) = 1+FO1D(NX)
    AC(NX,3) = 0.
    DO K = 2,NX-1
    AC(K,1) = FO1D(K)/2.
    AC(K,2) = -(1+FO1D(K))
    AC(K,3) = FO1D(K)/2.
    ENDDO
    AC(1,4) = (1-FO1D(1))*TWB(1,I-324) + FO1D(1)*TWB(2,I-324) + (FO1D(1)*DX(J)/LAMBDA(1))*Q*2 !QL, W/m2
    AC(NX,4) = FO1D(NX)*TWB(NX-1,I-324) + (1-FO1D(NX))*TWB(NX,I-324)
    DO K = 2,NX-1
    AC(K,4) = -FO1D(K)/2*TWB(K-1,I-324) - (1-FO1D(K))*TWB(K,I-324) - FO1D(K)/2*TWB(K+1,I-324)
    ENDDO
    CALL TRDG(AC,NX)
    DO K = 1,NX
    TWB(K,I-324) = AC(K,4)
    ENDDO
    ELSE
    J = 2
    DO K = 1,NX
    LAMBDA(K) = A(1) + A(2)*(TWB(K,I-324)-273.0) + A(3)*(TWB(K,I-324)-273.0)**2 + A(4)*(TWB(K,I-324)-273.0)**3
    CP(K) = B(1) + B(2)*(TWB(K,I-324)-273.0) + B(3)*(TWB(K,I-324)-273.0)**2
    FO1D(K) = LAMBDA(K)*DT/ROU/CP(K)/DX(J)**2
    ENDDO
    AC(1,1) = 0.
    AC(1,2) = 1.+FO1D(1)
    AC(1,3) = -FO1D(1)
    AC(NX,1) = -FO1D(NX)
    AC(NX,2) = 1+FO1D(NX)
    AC(NX,3) = 0.
    DO K = 2,NX-1
    AC(K,1) = FO1D(K)/2.
    AC(K,2) = -(1+FO1D(K))
    AC(K,3) = FO1D(K)/2.
    ENDDO
    AC(1,4) = (1-FO1D(1))*TWB(1,I-324) + FO1D(1)*TWB(2,I-324) + (FO1D(1)*DX(J)/LAMBDA(1))*Q*2 !QL, W/m2
    AC(NX,4) = FO1D(NX)*TWB(NX-1,I-324) + (1-FO1D(NX))*TWB(NX,I-324)
    DO K = 2,NX-1
    AC(K,4) = -FO1D(K)/2*TWB(K-1,I-324) - (1-FO1D(K))*TWB(K,I-324) - FO1D(K)/2*TWB(K+1,I-324)
    ENDDO
    CALL TRDG(AC,NX)
    DO K = 1,NX
    TWB(K,I-324) = AC(K,4)
    ENDDO
    ENDIF
    RETURN
    END
    
    
    SUBROUTINE CONDEXP2D(QN,QS,QW,QE,I,DT)
!   2D conduction model for obs.
    DIMENSION TWF(5,324),TWB(3,32),LAMBDA(5,5),CP(5,5),ALOW(6),AHIGH(7),BLOW(7),BMID(7),BHIGH(6), &
    A(5,5), RA(5,5),B(5,5), RAB(5,5), C(5,5), RAC(5,5), D(5), RAD(5), TWO(5,5,73), TWO1(5,5,73), TWO2(5,5,73), FO2D(5,5)
    REAL LAMBDA, CP
    INTEGER DT
    COMMON /COMM4/TWF,TWB,TWO,EMIS
    !POLYNOMIAL COEFFICIENTS FOR THERMAL CONDUCTIVITY OF MILD STEEL, TUCKER'S BOOK P406
    DATA ALOW/0.519059E+02,-0.369417E-03,-0.768098E-04,-0.811310E-08,0.212134E-09,-0.180744E-12/ !(< 800 C)
    DATA AHIGH/0.302492E+02,-0.155686E-01,0.144759E-04,-0.982726E-08,0.159948E-10,-0.936461E-14,0.148732E-17/ !(> 800 C)
    !POLYNOMIAL COEFFICIENTS FOR SPECIFIC HEAT OF MILD STEEL, TUCKER'S BOOK P404
    DATA BLOW/0.459389E+03,0.927605E+00,-0.892667E-02,0.427170E-04,-0.823237E-07,0.617737E-10,-0.885174E-14/ !(<750 C)
    DATA BMID/0.960497E+04,0.311055E+02,-0.821919E-01,-0.996642E-05,-0.291067E-08,0.166675E-09,-0.112167E-12/ !(>750, <900 C)
    DATA BHIGH/0.595783E+03,0.809290E+00,-0.172302E-02,0.113957E-05,-0.946037E-10,-0.762604E-13/ !(>900 C)
    DATA EL,EH,ROU,DX,DY/0.305,0.355,7850.0,0.07625,0.08875/ !Obs. wide, height; density, delta x, delta y.
    NX = EL/DX + 1 ! numbers of nodes.
    NY = EH/DY + 1

!*********** First-half of time step (i-exmplicit, j-implicit) ***********!
    DO K = 1,NX
        DO J = 1,NY
            IF(((TWO(K,J,(I+3)/6-59)-273.0)>0.0).AND.((TWO(K,J,(I+3)/6-59)-273.0)<=750.0)) THEN
                CP(K,J) = BLOW(1) + BLOW(2)*(TWO(K,J,(I+3)/6-59)-273.0) + BLOW(3)*(TWO(K,J,(I+3)/6-59)-273.0)**2 + BLOW(4)*(TWO(K,J,(I+3)/6-59)-273.0)**3 + BLOW(5)*(TWO(K,J,(I+3)/6-59)-273.0)**4 + BLOW(6)*(TWO(K,J,(I+3)/6-59)-273.0)**5 + BLOW(7)*(TWO(K,J,(I+3)/6-59)-273.0)**6
                LAMBDA(K,J) = ALOW(1) + ALOW(2)*(TWO(K,J,(I+3)/6-59)-273.0) + ALOW(3)*(TWO(K,J,(I+3)/6-59)-273.0)**2 + ALOW(4)*(TWO(K,J,(I+3)/6-59)-273.0)**3 + ALOW(5)*(TWO(K,J,(I+3)/6-59)-273.0)**4 + ALOW(6)*(TWO(K,J,(I+3)/6-59)-273.0)**5 !MILD STEEL
            ELSEIF(((TWO(J,K,(I+3)/6-59)-273.0)>750.0).AND.((TWO(J,K,(I+3)/6-59)-273.0)<=800.0)) THEN
                CP(K,J) = BMID(1) + BMID(2)*(TWO(K,J,(I+3)/6-59)-273.0) + BMID(3)*(TWO(K,J,(I+3)/6-59)-273.0)**2 + BMID(4)*(TWO(K,J,(I+3)/6-59)-273.0)**3 + BMID(5)*(TWO(K,J,(I+3)/6-59)-273.0)**4 + BMID(6)*(TWO(K,J,(I+3)/6-59)-273.0)**5 + BMID(7)*(TWO(K,J,(I+3)/6-59)-273.0)**6
                LAMBDA(K,J) = ALOW(1) + ALOW(2)*(TWO(K,J,(I+3)/6-59)-273.0) + ALOW(3)*(TWO(K,J,(I+3)/6-59)-273.0)**2 + ALOW(4)*(TWO(K,J,(I+3)/6-59)-273.0)**3 + ALOW(5)*(TWO(K,J,(I+3)/6-59)-273.0)**4 + ALOW(6)*(TWO(K,J,(I+3)/6-59)-273.0)**5
            ELSEIF(((TWO(J,K,(I+3)/6-59)-273.0)>800.0).AND.((TWO(J,K,(I+3)/6-59)-273.0)<=900.0)) THEN
                CP(K,J) = BMID(1) + BMID(2)*(TWO(K,J,(I+3)/6-59)-273.0) + BMID(3)*(TWO(K,J,(I+3)/6-59)-273.0)**2 + BMID(4)*(TWO(K,J,(I+3)/6-59)-273.0)**3 + BMID(5)*(TWO(K,J,(I+3)/6-59)-273.0)**4 + BMID(6)*(TWO(K,J,(I+3)/6-59)-273.0)**5 + BMID(7)*(TWO(K,J,(I+3)/6-59)-273.0)**6
                LAMBDA(K,J) = AHIGH(1) + AHIGH(2)*(TWO(K,J,(I+3)/6-59)-273.0) + AHIGH(3)*(TWO(K,J,(I+3)/6-59)-273.0)**2 + AHIGH(4)*(TWO(K,J,(I+3)/6-59)-273.0)**3 + AHIGH(5)*(TWO(K,J,(I+3)/6-59)-273.0)**4 + AHIGH(6)*(TWO(K,J,(I+3)/6-59)-273.0)**5 +AHIGH(7)*(TWO(K,J,(I+3)/6-59)-273.0)**6 !MILD STEEL
            ELSE !>900 C
                CP(K,J) = BHIGH(1) + BHIGH(2)*(TWO(K,J,(I+3)/6-59)-273.0) + BHIGH(3)*(TWO(K,J,(I+3)/6-59)-273.0)**2 + BHIGH(4)*(TWO(K,J,(I+3)/6-59)-273.0)**3 + BHIGH(5)*(TWO(K,J,(I+3)/6-59)-273.0)**4 + BHIGH(6)*(TWO(K,J,(I+3)/6-59)-273.0)**5
                LAMBDA(K,J) = AHIGH(1) + AHIGH(2)*(TWO(K,J,(I+3)/6-59)-273.0) + AHIGH(3)*(TWO(K,J,(I+3)/6-59)-273.0)**2 + AHIGH(4)*(TWO(K,J,(I+3)/6-59)-273.0)**3 + AHIGH(5)*(TWO(K,J,(I+3)/6-59)-273.0)**4 + AHIGH(6)*(TWO(K,J,(I+3)/6-59)-273.0)**5 +AHIGH(7)*(TWO(K,J,(I+3)/6-59)-273.0)**6 !MILD STEEL
            ENDIF
            
            FO2D(K,J) = LAMBDA(K,J)*DT/ROU/CP(K,J)/DX/DY
        ENDDO
    ENDDO
    
! Solve A*T1(1,:)' = B*T(1,:)' + C*T(2,:)' + D for i=1, j=1:Nj
    A(1,2) = DX/DY
    A(NY,NY-1) = DX/DY
    D(1) = -QW*(DX/LAMBDA(1,1))-QN*(DY/LAMBDA(1,1))
    D(NY) = -QE*(DX/LAMBDA(1,NY))-QN*(DY/LAMBDA(1,NY))
    DO L = 1,NY
        A(L,L) = -DX/DY-1/FO2D(1,L)
        B(L,L) = DY/DX-1/FO2D(1,L)
        C(L,L) = -DY/DX
    ENDDO
    DO L = 1,NY-2
        A(L+1,L+2) = DX/2/DY
        A(L+1,L) = DX/2/DY
        D(L+1) = -QN*(DY/LAMBDA(1,L+1))
    ENDDO

    RA = 0.0
    CALL REVERSE(A,RA,NY)
    RAB = MATMUL(RA,B)
    RAC = MATMUL(RA,C)
    RAD = MATMUL(RA,D)

    DO L = 1,NY
    TWO1(1,L,(I+3)/6-59) = DOT_PRODUCT(RAB(L,1:NY),TWO(1,1:NY,(I+3)/6-59)) + DOT_PRODUCT(RAC(L,1:NY),TWO(2,1:NY,(I+3)/6-59)) + RAD(L)
    ENDDO
    
    ! Solve A*T1(i,:)' = B*T(i-1,:)' + C*T(i,:) + B*T(i+1,:) + D for i=2:(Ni-1), j=1:Nj 
    DO K = 2,NX-1
    A(1,1) = -DX/DY-1/FO2D(K,1)
    A(NY,NY) = -DX/DY-1/FO2D(K,NY)
    B(1,1) = -DY/2/DX
    B(NY,NY) = -DY/2/DX
    C(1,1) = DY/DX-1/FO2D(K,1)
    C(NY,NY) = DY/DX-1/FO2D(K,NY)
    D(1) = -QW*(DX/LAMBDA(K,1))
    D(NY) = -QE*(DX/LAMBDA(K,NY))
    DO L = 1,NY-1
        A(L,L+1) = DX/DY
        A(L+1,L) = DX/DY
    ENDDO
    DO L = 1,NY-2
        A(L+1,L+1) = 2*(-DX/DY-1/FO2D(K,L+1))
        B(L+1,L+1) = -DY/DX
        C(L+1,L+1) = 2*(DY/DX-1/FO2D(K,L+1))
        D(L+1) = 0.0
    ENDDO

    RA = 0.0
    CALL REVERSE(A,RA,NY)
    RAB = MATMUL(RA,B)
    RAC = MATMUL(RA,C)
    RAD = MATMUL(RA,D)

    DO L = 1,NY
    TWO1(K,L,(I+3)/6-59) = DOT_PRODUCT(RAB(L,1:NY),TWO(K-1,1:NY,(I+3)/6-59)) + DOT_PRODUCT(RAC(L,1:NY),TWO(K,1:NY,(I+3)/6-59)) + DOT_PRODUCT(RAB(L,1:NY),TWO(K+1,1:NY,(I+3)/6-59))+ RAD(L)
    ENDDO

    ENDDO
    
    ! Solve A*T1(Ni,:)' = B*T(Ni-1,:)' + C*T(Ni,:)' + D for i=Ni, j=1:Nj 
    A(1,2) = DX/DY
    A(NY,NY-1) = DX/DY
    D(1) = -QW*(DX/LAMBDA(NX,1))-QS*(DY/LAMBDA(NX,1))
    D(NY) = -QE*(DX/LAMBDA(NX,NY))-QS*(DY/LAMBDA(NX,NY))
    DO L = 1,NY
        A(L,L) = -DX/DY-1/FO2D(NX,L)
        B(L,L) = -DY/DX
        C(L,L) = DY/DX-1/FO2D(NX,L)
    ENDDO
    DO L = 1,NY-2
        A(L+1,L+2) = DX/2/DY
        A(L+1,L) = DX/2/DY
        D(L+1) = -QS*(DY/LAMBDA(NX,L+1))
    ENDDO

    RA = 0.0
    CALL REVERSE(A,RA,NY)
    RAB = MATMUL(RA,B)
    RAC = MATMUL(RA,C)
    RAD = MATMUL(RA,D)

    DO L = 1,NY
    TWO1(NX,L,(I+3)/6-59) = DOT_PRODUCT(RAB(L,1:NY),TWO(NX-1,1:NY,(I+3)/6-59)) + DOT_PRODUCT(RAC(L,1:NY),TWO(NX,1:NY,(I+3)/6-59)) + RAD(L)
    ENDDO

    !*********** Second-half of time step (i-implicit, j-explicit) ***********!
    DO K = 1,NX
        DO J = 1,NY
            IF(((TWO1(K,J,(I+3)/6-59)-273.0)>0.0).AND.((TWO1(K,J,(I+3)/6-59)-273.0)<=750.0)) THEN
                CP(K,J) = BLOW(1) + BLOW(2)*(TWO1(K,J,(I+3)/6-59)-273.0) + BLOW(3)*(TWO1(K,J,(I+3)/6-59)-273.0)**2 + BLOW(4)*(TWO1(K,J,(I+3)/6-59)-273.0)**3 + BLOW(5)*(TWO1(K,J,(I+3)/6-59)-273.0)**4 + BLOW(6)*(TWO1(K,J,(I+3)/6-59)-273.0)**5 + BLOW(7)*(TWO1(K,J,(I+3)/6-59)-273.0)**6
                LAMBDA(K,J) = ALOW(1) + ALOW(2)*(TWO1(K,J,(I+3)/6-59)-273.0) + ALOW(3)*(TWO1(K,J,(I+3)/6-59)-273.0)**2 + ALOW(4)*(TWO1(K,J,(I+3)/6-59)-273.0)**3 + ALOW(5)*(TWO1(K,J,(I+3)/6-59)-273.0)**4 + ALOW(6)*(TWO1(K,J,(I+3)/6-59)-273.0)**5 !MILD STEEL
            ELSEIF(((TWO1(J,K,(I+3)/6-59)-273.0)>750.0).AND.((TWO1(J,K,(I+3)/6-59)-273.0)<=800.0)) THEN
                CP(K,J) = BMID(1) + BMID(2)*(TWO1(K,J,(I+3)/6-59)-273.0) + BMID(3)*(TWO1(K,J,(I+3)/6-59)-273.0)**2 + BMID(4)*(TWO1(K,J,(I+3)/6-59)-273.0)**3 + BMID(5)*(TWO1(K,J,(I+3)/6-59)-273.0)**4 + BMID(6)*(TWO1(K,J,(I+3)/6-59)-273.0)**5 + BMID(7)*(TWO1(K,J,(I+3)/6-59)-273.0)**6
                LAMBDA(K,J) = ALOW(1) + ALOW(2)*(TWO1(K,J,(I+3)/6-59)-273.0) + ALOW(3)*(TWO1(K,J,(I+3)/6-59)-273.0)**2 + ALOW(4)*(TWO1(K,J,(I+3)/6-59)-273.0)**3 + ALOW(5)*(TWO1(K,J,(I+3)/6-59)-273.0)**4 + ALOW(6)*(TWO1(K,J,(I+3)/6-59)-273.0)**5
            ELSEIF(((TWO1(J,K,(I+3)/6-59)-273.0)>800.0).AND.((TWO1(J,K,(I+3)/6-59)-273.0)<=900.0)) THEN
                CP(K,J) = BMID(1) + BMID(2)*(TWO1(K,J,(I+3)/6-59)-273.0) + BMID(3)*(TWO1(K,J,(I+3)/6-59)-273.0)**2 + BMID(4)*(TWO1(K,J,(I+3)/6-59)-273.0)**3 + BMID(5)*(TWO1(K,J,(I+3)/6-59)-273.0)**4 + BMID(6)*(TWO1(K,J,(I+3)/6-59)-273.0)**5 + BMID(7)*(TWO1(K,J,(I+3)/6-59)-273.0)**6
                LAMBDA(K,J) = AHIGH(1) + AHIGH(2)*(TWO1(K,J,(I+3)/6-59)-273.0) + AHIGH(3)*(TWO1(K,J,(I+3)/6-59)-273.0)**2 + AHIGH(4)*(TWO1(K,J,(I+3)/6-59)-273.0)**3 + AHIGH(5)*(TWO1(K,J,(I+3)/6-59)-273.0)**4 + AHIGH(6)*(TWO1(K,J,(I+3)/6-59)-273.0)**5 +AHIGH(7)*(TWO1(K,J,(I+3)/6-59)-273.0)**6 !MILD STEEL
            ELSE !>900 C
                CP(K,J) = BHIGH(1) + BHIGH(2)*(TWO1(K,J,(I+3)/6-59)-273.0) + BHIGH(3)*(TWO1(K,J,(I+3)/6-59)-273.0)**2 + BHIGH(4)*(TWO1(K,J,(I+3)/6-59)-273.0)**3 + BHIGH(5)*(TWO1(K,J,(I+3)/6-59)-273.0)**4 + BHIGH(6)*(TWO1(K,J,(I+3)/6-59)-273.0)**5
                LAMBDA(K,J) = AHIGH(1) + AHIGH(2)*(TWO1(K,J,(I+3)/6-59)-273.0) + AHIGH(3)*(TWO1(K,J,(I+3)/6-59)-273.0)**2 + AHIGH(4)*(TWO1(K,J,(I+3)/6-59)-273.0)**3 + AHIGH(5)*(TWO1(K,J,(I+3)/6-59)-273.0)**4 + AHIGH(6)*(TWO1(K,J,(I+3)/6-59)-273.0)**5 +AHIGH(7)*(TWO1(K,J,(I+3)/6-59)-273.0)**6 !MILD STEEL
            ENDIF
            
            FO2D(K,J) = LAMBDA(K,J)*DT/ROU/CP(K,J)/DX/DY
        ENDDO
    ENDDO
    
    ! Solve A*T2(:,1) = B*T1(:,1) + C*T1(:,2) + D  for i=1:Ni, j=1         
    A(1,2) = DY/DX
    A(NX,NX-1) = DY/DX
    D(1) = -QW*(DX/LAMBDA(1,1))-QN*(DY/LAMBDA(1,1))
    D(NX) = -QW*(DX/LAMBDA(NX,1))-QS*(DY/LAMBDA(NX,1))
    DO L = 1,NX
        A(L,L) = -DY/DX-1/FO2D(L,1)
        B(L,L) = DX/DY-1/FO2D(L,1)
        C(L,L) = -DX/DY
    ENDDO
    DO L = 1,NX-2
        A(L+1,L+2) = DY/2/DX
        A(L+1,L) = DY/2/DX
        D(L+1) = -QW*(DX/LAMBDA(L+1,1))
    ENDDO

    RA = 0.0
    CALL REVERSE(A,RA,NX)
    RAB = MATMUL(RA,B)
    RAC = MATMUL(RA,C)
    RAD = MATMUL(RA,D)

    DO L = 1,NX
    TWO2(L,1,(I+3)/6-59) = DOT_PRODUCT(RAB(L,1:NX),TWO1(1:NX,1,(I+3)/6-59)) + DOT_PRODUCT(RAC(L,1:NX),TWO1(1:NX,2,(I+3)/6-59)) + RAD(L)
    ENDDO
    
    ! Solve A*T2(i,j) = B*T1(:,j-1) + C*T1(:,j) + B*T1(:,j+1) + D for i=1:Ni, j=2:(Nj-1)
    DO J = 2,NY-1
    A(1,1) = -DY/DX-1/FO2D(1,J)
    A(NX,NX) = -DY/DX-1/FO2D(NX,J)
    B(1,1) = -DX/2/DY
    B(NX,NX) = -DX/2/DY
    C(1,1) = DX/DY-1/FO2D(1,J)
    C(NX,NX) = DX/DY-1/FO2D(NX,J)
    D(1) = -QN*(DY/LAMBDA(1,J))
    D(NX) = -QS*(DY/LAMBDA(NX,J))
    DO L = 1,NX-1
        A(L,L+1) = DY/DX
        A(L+1,L) = DY/DX
    ENDDO
    DO L = 1,NX-2
        A(L+1,L+1) = 2*(-DY/DX-1/FO2D(L+1,J))
        B(L+1,L+1) = -DX/DY
        C(L+1,L+1) = 2*(DX/DY-1/FO2D(L+1,J))
        D(L+1) = 0.0
    ENDDO

    RA = 0.0
    CALL REVERSE(A,RA,NX)
    RAB = MATMUL(RA,B)
    RAC = MATMUL(RA,C)
    RAD = MATMUL(RA,D)

    DO L = 1,NX
    TWO2(L,J,(I+3)/6-59) = DOT_PRODUCT(RAB(L,1:NX),TWO1(1:NX,J-1,(I+3)/6-59)) + DOT_PRODUCT(RAC(L,1:NX),TWO1(1:NX,J,(I+3)/6-59)) + DOT_PRODUCT(RAB(L,1:NY),TWO1(1:NY,J+1,(I+3)/6-59))+ RAD(L)
    ENDDO

    ENDDO

    ! Solve A*T2(:,Nj) = B*T1(:,Nj-1) + C*T1(:,Nj) + D for i=1:Ni, j=Nj
    A(1,2) = DY/DX
    A(NX,NX-1) = DY/DX
    D(1) = -QE*(DX/LAMBDA(1,NY))-QN*(DY/LAMBDA(1,NY))
    D(NX) = -QE*(DX/LAMBDA(NX,NY))-QS*(DY/LAMBDA(NX,NY))
    DO L = 1,NX
        A(L,L) = -DY/DX-1/FO2D(L,J)
        B(L,L) = -DX/DY
        C(L,L) = DX/DY-1/FO2D(L,J)
    ENDDO
    DO L = 1,NX-2
        A(L+1,L+2) = DY/2/DX
        A(L+1,L) = DY/2/DX
        D(L+1) = -QE*(DX/LAMBDA(L+1,NY))
    ENDDO

    RA = 0.0
    CALL REVERSE(A,RA,NX)
    RAB = MATMUL(RA,B)
    RAC = MATMUL(RA,C)
    RAD = MATMUL(RA,D)

    DO L = 1,NX
    TWO2(L,NY,(I+3)/6-59) = DOT_PRODUCT(RAB(L,1:NX),TWO1(1:NX,NY-1,(I+3)/6-59)) + DOT_PRODUCT(RAC(L,1:NX),TWO1(1:NX,NY,(I+3)/6-59)) + RAD(L)
    ENDDO
    
    DO K = 1,NX
        DO J = 1,NY
            TWO(K,J,(I+3)/6-59) = TWO2(K,J,(I+3)/6-59)
        ENDDO
    ENDDO

    RETURN
    END
    
    
    SUBROUTINE SOLVE(TG,F)
      DIMENSION TG(288),F(288),F1(288),E(288),H(288,288),TGG(288)
      COMMON /COMM6/FSUM,FMEAN,FMAX,EMEAN,EMAX,IC
      NX=288
      ICMAX=1000
      TOL=2.5E-6
      
      IC=0
    4 IC=IC+1 
      IF(IC.GT.ICMAX) STOP 7777 
      ERMAX=0.0 
      DO 8 I=1,NX 
    8 TGG(I)=TG(I)  
      CALL FX(TG,F)
      DO 2 K=1,NX 
      A=TG(K)
      TG(K)=0.999*A
      CALL FX(TG,F1) 
      DO 12 I=1,NX
      H(I,K)=(F(I)-F1(I))/0.001/A    
      TG(K)=A
   12 CONTINUE        
    2 CONTINUE
      CALL LINEQ(NX,H,F,E)
      DO 3 I=1,NX 
    3 TG(I)=TG(I)-E(I)
      ERR=0.0 
      FSUM=0.0
      DO 7 I=1,NX 
    7 FSUM=FSUM+ABS(F(I)) 
      WRITE(*,1001) FSUM
1001  FORMAT('  ABS.SUM ERRORS =',E12.3)
      DO 9 I=1,NX 
      ERR=ABS((TG(I)-TGG(I))/TGG(I))
    9 IF(ERR.GT.ERMAX) ERMAX=ERR
      IF(ERMAX.GT.TOL) GO TO 4
      RETURN  
      END 
    
    
      SUBROUTINE LINEQ(N,H,F,E)     
      DIMENSION H(288,288),F(288),E(288)
      NM1=N-1   
      DO 1070 K=1,NM1   
      KP1=K+1   
      L=K   
      DO 1020 I=KP1,N   
      IF(ABS(H(I,K))-ABS(H(L,K)))1020,1020,1010   
1010  L=I   
1020  CONTINUE  
      IF(L-K)1050,1050,1030   
1030  DO 1040 J=K,N   
      TEMP=H(K,J)   
      H(K,J)=H(L,J)   
1040  H(L,J)=TEMP   
      TEMP=F(K)   
      F(K)=F(L)   
      F(L)=TEMP   
1050  DO 1070 I=KP1,N   
      FACTOR=H(I,K)/H(K,K)    
      H(I,K)=0.0  
      DO 1060 J=KP1,N   
1060  H(I,J)=H(I,J)-FACTOR*H(K,J)   
1070  F(I)=F(I)-FACTOR*F(K)   
      E(N)=F(N)/H(N,N)    
      I=NM1   
1080  IP1=I+1   
      SUM=0.0   
      DO 1090 J=IP1,N   
1090  SUM=SUM+H(I,J)*E(J)     
      E(I)=(F(I)-SUM)/H(I,I)    
      I=I-1   
      IF(I)1100,1100,1080     
1100  RETURN  
      END
      
      
    FUNCTION ENTHA(T)
!   Air specific enthalpy function
    DIMENSION U(6)
    DATA U/1.234164,0.243788,0.003322,-0.000059,0.000041,0.000006/ 
    Z=(T-1400.)/200.
    ENTHA=((((U(6)*Z+U(5))*Z+U(4))*Z+U(3))*Z+U(2))*Z+U(1) 
    ENTHA=ENTHA*1.0E+06
    RETURN
    END
    
    
    SUBROUTINE FS(Z,Y,DY,NONZERO)
    DIMENSION R(6)
    REAL NONZERO
    DATA R/1.234164,0.243788,0.003322,-0.000059,0.000041,0.000006/
    Y = R(6)*Z**5 + R(5)*Z**4 + R(4)*Z**3 + R(3)*Z**2 + R(2)*Z + R(1) - NONZERO
    DY = 5*R(6)*Z**4 + 4*R(5)*Z**3 + 3*R(4)*Z**2 + 2*R(3)*Z + R(2)
    RETURN
    END
    
    
    SUBROUTINE DNEWT(Z,FS,NONZERO)
    REAL NONZERO
    L= 60
    EPS = 1.0E-06
    CALL FS(Z,Y,DY,NONZERO)
10  IF(ABS(DY)+1.0.EQ.1.0) THEN
    L= 0
    WRITE(*,20)
    RETURN
    ENDIF
20  FORMAT(1X,'ERR')
    Z1=Z-Y/DY
    CALL FS(Z1,Y,DY,NONZERO)
    IF((ABS(Z1-Z).GE.EPS).OR.(ABS(Y).GE.EPS)) THEN
    L=L-1
    Z=Z1
    IF(L.EQ.0) RETURN
    GOTO 10
    END IF
    Z=Z1
    RETURN
    END
    
    
    FUNCTION ENTHF(T)
!   Fuel specific enthalpy function
    DIMENSION W(6)
    DATA W/1.34771,0.26941,0.00415,0.00047526,0.000232474,0.0000225272/
    Z=(T-1400.)/200.
    ENTHF=((((W(6)*Z+W(5))*Z+W(4))*Z+W(3))*Z+W(2))*Z+W(1) 
    ENTHF=ENTHF*1.0E+06
    RETURN
    END
    
    
    FUNCTION ENTH(T,XSAIR)
!   Combustion product specific enthalpy function. Returns specific enthalpy as J/kg.
    DIMENSION V(6)
    COMMON /COMM3/XSAIR1,XSAIR2,SAR,RHCP,PPCS,PPC,TA,TW,TWF2,CVN
    XTOT=SAR+1+XSAIR/100.*SAR 
    XMC=(SAR+1)/XTOT
    XMA=XSAIR/100.*SAR/XTOT 
    !********Oil*******!
    !V(1)=XMC*1.34771+XMA*1.234164
    !V(2)=XMC*0.26941+XMA*0.243788
    !V(3)=XMC*0.00415+XMA*0.003322
    !V(4)=XMC*0.00047526-XMA*0.000059
    !V(5)=XMC*0.000232474+XMA*0.000041
    !V(6)=XMC*0.0000225272+XMA*0.000006
    !********NG*******!
    V(1)=XMC*1.399955+XMA*1.234164
    V(2)=XMC*0.28089+XMA*0.243788
    V(3)=XMC*0.004526+XMA*0.003322
    V(4)=XMC*0.000402-XMA*0.000059
    V(5)=XMC*0.000224+XMA*0.000041
    V(6)=XMC*0.000023+XMA*0.000006
    Z=(T-1400.)/200.
    ENTH=((((V(6)*Z+V(5))*Z+V(4))*Z+V(3))*Z+V(2))*Z+V(1)
    ENTH=ENTH*1.0E+06*1.04
    RETURN
    END
    
        
    SUBROUTINE TRDG(X,N)
    DIMENSION X(50,4), A(50), B(50), C(50), D(50)
    DO I = 1,N
    B(I) = X(I,1)
    D(I) = X(I,2)
    A(I) = X(I,3)
    C(I) = X(I,4)
    ENDDO
    
    DO I = 2,N
    R = B(I)/D(I-1)
    D(I) = D(I) - R*A(I-1)
    C(I) = C(I) - R*C(I-1)
    ENDDO
    
    C(N) = C(N)/D(N)
    DO I = 2,N
    J = N-I+1
    C(J) = (C(J)-A(J)*C(J+1))/D(J)
    ENDDO
    
    DO I = 1,N
    X(I,4) = C(I)
    ENDDO
    RETURN
    END
    
    
    SUBROUTINE REVERSE(AA,RA,N) !! AA old matrix，B the reverse matrix of AA，N dimensiom of AA
    INTEGER N,I,J,L  
    REAL AA(N,N),RA(N,N),A(N,N)
    A=AA  
    DO I=1,N    
    RA(I,I)=1  
    ENDDO  
    DO I=1,N    
    RA(I,:)=RA(I,:)/A(I,I)    
    A(I,I:N)=A(I,I:N)/A(I,I)    
    DO J=I+1,N          
    DO L=1,N      
    RA(J,L)=RA(J,L)-RA(I,L)*A(J,I)  
    ENDDO
    A(J,I:N)=A(J,I:N)-A(I,I:N)*A(J,I) 
    ENDDO
    ENDDO
    DO I=N,1,-1    
    DO J=I-1,1,-1   
    DO L=1,N     
    RA(J,L)=RA(J,L)-RA(I,L)*A(J,I)    
    ENDDO
    ENDDO 
    ENDDO
    END
    
    
    SUBROUTINE BUBBLE_SORT(A,N,D)
    IMPLICIT NONE
    DIMENSION A(5,5,73), B(25)
    INTEGER N,I,J,TEMP
    REAL A,B,D
    DO I=1,5
        DO J=1,5
            B(J+(I-1)*5)=A(I,J,73)
        END DO
    END DO
    DO I=N-1,1,-1   
    DO J=1,I      
    IF (B(J) > B(J+1)) THEN
    TEMP=B(J)
    B(J)=B(J+1)
    B(J+1)=TEMP
    END IF
    END DO
    END DO
    D=B(25)-B(1)
    RETURN
    END SUBROUTINE
    
    
    BLOCK DATA
    DIMENSION NCOE(708,38),COEN(6,35,288),GCOE(708,38),COEG(6,35,288),AAA(35),&
    FDNET(288,6),FDGRO(288,6),FDPOS(288,6),FDNEG(288,6),V(4),G2G(288,6),G2W(288,3,2),&
    W2G(324,2),G2O(288,6,2),O2G(438,6,3),G2B(288,4,2),B2G(32,2),SS(794,794,4),SG(794,288,4),&
    GG(288,288,4),GS(288,794,4),AS(794,1),EMIS(794,1),FKV(288,4),B1(4),B2(4),B3(4),KGN(4),F(288),&
    TG(288),TG2(288),QWALL(794),TW(794),TWF2(794),TWF(5,324),TWB(3,32),TWO(5,5,73),TSP(4),PB(4),PC(4),RT(4),&
    SUMSG(288),AVE(73),EA(6),MAIR(6),BMAX(6),GE(6),SETPT(342,4),CAREA(288,1),GCOOL(288),TD(4),TM(4),ERR1(4),&
    ERR2(4),ERR3(4),ERR4(4),INCREASE(4),TPV(4),KP(4),KI(4),KD(4)
    COMMON /COMM1/G2G,G2W,W2G,G2O,O2G,G2B,B2G,AS,FKV,FDPOS,FDNEG,SS,SG,GS,GG,SUMSG,AVE,AAA,FDGRO,FDNET,GE,SETPT
    COMMON /COMM2/TG,TG2,TGG,TSP,TPV,RT,QWALL,PB,KGN,F,NCOE,COEN,GCOE,COEG,B1,B2,B3
    COMMON /COMM3/XSAIR1,XSAIR2,SAR,RHCP,PPCS,PPC,TA,TW,TWF2,CVN
    COMMON /COMM4/TWF,TWB,TWO,EMIS
    COMMON /COMM5/TIME
    COMMON /COMM6/FSUM,FMEAN,FMAX,EMEAN,EMAX,IC
    COMMON /COMM7/PC,V,BMAX,FD,TAM,MU,EA,MAIR,RUPEFF,ENTHAIR,SUMAIR,NONZERO
    COMMON /COMM8/CAREA
    COMMON /COMM9/GCOOL
    COMMON /COMM10/GCONVECTION
    COMMON /COMM11/TD,TM
    COMMON /COMM12/KP,KI,KD
    COMMON /COMM13/ERR1,ERR2,ERR3,ERR4,INCREASE
    END