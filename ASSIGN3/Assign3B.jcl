//KC03BB6A JOB ,'N FLORES',MSGCLASS=H
//*
//*****************************************************************
//*                                                               *
//*  CSCI 465               ASSIGNMENT 3               FALL 2021  *
//*                                                               *
//*  DEVELOPER NAME: Noah Flores                                  *
//*  DATE DUE: 09/20/2021                                         *
//*                                                               *
//*  Assemble the code, bind and fetch and execute program        *
//*                                                               *
//*****************************************************************
//*
//JSTEP01  EXEC PGM=ASMA90,PARM=ASA
//*
//*****************************************************************
//* JSTEP01  CALLS HIGH LEVEL ASSEMBER AND ASSEMBLE CODE          *
//*                                                               *
//* DDNAME             FILE DESCRIPTION                           *
//*                                                               *
//* SYSLIN    OUTPUT: TEMP DATASET TO PASS TO BINDER              *
//*                                                               *
//*****************************************************************
//*
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//*
//SYSLIN   DD  DSN=&&OBJECT,SPACE=(CYL,(1,1)),DISP=(MOD,PASS)
//*
//SYSPRINT DD  SYSOUT=*
//*
//SYSUT1   DD  SPACE=(CYL,(1,1))
//*
         PRINT  NOGEN                MACRO CALL SUPPRESSION
**********************************************************************
*                                                                    *
* PROGRAM:    ASSEMBL3                                               *
* AUTHOR:     GEOFFREY D. DECKER                                     *
* DATE:       09/12/2021                                             *
*                                                                    *
* FUNCTION:  THIS PROGRAM READS INPUT DATA RECORDS AND THEN WRITES   *
*            THEM TO STANDARD OUTPUT.                                *
*                                                                    *
* INPUT:     INDATA - THE INPUT DATA RECORDS READ IN.                *
*                                                                    *
* OUTPUT:    REPORT - THE INPUT DATA RECORDS WRITTEN OUT.            *
*                                                                    *
**********************************************************************
ASSEMBL3 CSECT                       BEGIN ASSEMBL3
*
         STM   14,12,12(13)          SAVE CALLER'S REGS
         LR    12,15                 SET R12 TO R15
         USING ASSEMBL3,12           ESTABLISH R12 AS 1ST BASE REG
         LA    14,REGS               R14 -> CURRENT SAVE AREA
         ST    13,4(,14)             SAVE CALLER'S SAVE AREA ADDR
         ST    14,8(,14)             SAVE CURRENT SAVE AREA ADDR
         LR    13,14                 R13 -> CURRENT SAVE AREA
*
         OPEN  (INDCB,(INPUT))       OPEN INPUT FILE
         LTR   15,15                 CHECK FOR SUCCESSFUL OPEN
         BZ    OPENOK1               BRANCH IF SUCCESSFUL TO OPENOK1
         ABEND 222,DUMP              ABEND IF OPEN FAILS & DISPLAY 222
*
OPENOK1  OPEN  (RPTDCB,(OUTPUT))     OPEN OUTPUT FILE
         LTR   15,15                 CHECK FOR SUCCESSFUL OPEN
         BZ    OPENOK2               BRANCH IF SUCCESSFUL TO OPENOK2
         ABEND 333,DUMP              ABEND IF OPEN FAILS & DISPLAY 333
*
OPENOK2  GET   INDCB,RECORD          READ THE FIRST RECORD
*
LOOP1    CLI   EOFFLAG,C'Y'          CHECK FOR END OF INPUT DATA
*
         BE    ENDLOOP1              BRANCH TO ENDLOOP1 IF EOF
*
         AP    RECCTR(2),=PL1'1'     ADD 1 TO RECORD COUNTER
*
         MVC   OUTDATA(80),RECORD    MOVE FROM INPUT DATA TO PRINT
         PUT   RPTDCB,DETAIL         WRITE THE DETAIL LINE
*
         GET   INDCB,RECORD          GET THE NEXT RECORD
*
         B     LOOP1                 BRANCH TO TOP OF LOOP1
*
ENDLOOP1 ED    ORECCTR(4),RECCTR     EDIT REC COUNTER INTO TOTALS
         PUT   RPTDCB,TOTALS         WRITE THE TOTALS LINE
*
         CLOSE (INDCB,,RPTDCB)       CLOSE THE TWO DCBs
*
         SR    15,15                 SET RC TO 0
         L     13,4(,13)             R13 -> CALLER'S SAVE AREA
         L     14,12(,13)            RESTORE R14
         LM    0,12,20(13)           RESTORE R0 THROUGH R12
*
         BR    14                    RETURN TO CALLER
*
         LTORG                       LTORG TO ORGANIZE LITERALS
*
         ORG   ASSEMBL3+((*-ASSEMBL3+31)/32)*32
         DC    C'HERE IS THE STORAGE FOR ASSEMBL3'
*
REGS     DC    18F'0'                MAINREGS FOR STANDARD LINKAGE
*
RECCTR   DC    PL2'0'                PACKED RECORD CTR FIELD
*
RECORD   DC    80C'@'                INPUT BUFFER FILL WITH '@'
*
DETAIL   DC    C'0'                  DETAIL CARRIAGE CONTROL
OUTDATA  DC    80C' '                80 BYTE STORAGE FOR PRINTING
         DC    52C' '                FILLER FOR PRINT RECORD
*
TOTALS   DC    C'0'                  TOTALS1 CARRIAGE CONTROL
         DC    C'RECORD COUNT:'      LABEL FOR RECORD COUNT
ORECCTR  DC    X'40202120'           OUTPUT FOR RECORD COUNT
         DC    115C' '               SPACES
*
*  INPUT DCB
*
INDCB    DCB   DDNAME=INDATA,                                          X
               DEVD=DA,                                                X
               DSORG=PS,                                               X
               MACRF=GM,                                               X
               RECFM=FB,                                               X
               LRECL=80,                                               X
               EODAD=EOFRTN
*
*  INPUT DCB'S EOF FLAG AND EOF ROUTINE
*
EOFFLAG  DC    C'N'                 EOFFLAG FOR INPUT FILE
*
EOFRTN   MVI   EOFFLAG,C'Y'          SET EOF FLAG TO Y
         BR    14                    BRANCH TO STMT FOLLOWING GET
*
*  OUTPUT DCB
*
RPTDCB   DCB   DDNAME=RPTDATA,                                         X
               DEVD=DA,                                                X
               DSORG=PS,                                               X
               MACRF=PM,                                               X
               RECFM=FBA,                                              X
               LRECL=133,                                              X
               BLKSIZE=2660
*
         END   ASSEMBL3
//*
//SYSUDUMP DD   SYSOUT=*
//*
//JSTEP02  EXEC PGM=HEWL,COND=(0,LT)
//*
//*****************************************************************
//* JSTEP02  CALL BINDER TO CREATE EXCUTABLE FOR OUR CODE         *
//*                                                               *
//* DDNAME             FILE DESCRIPTION                           *
//*                                                               *
//*****************************************************************
//*
//SYSLIN   DD  DSN=&&OBJECT,DISP=(OLD,DELETE)
//*
//SYSLIB   DD  DSN=CEE.SCEELKED,DISP=SHR
//*
//SYSLMOD  DD  DSN=KC03BB6.CSCI465.LOADLIB(ASSEMBL3),
//             SPACE=(1024,(50,20,1)),DSNTYPE=LIBRARY,
//             DISP=(MOD,KEEP,KEEP)
//*
//SYSPRINT DD  SYSOUT=*
//*
//SYSTERM  DD  SYSOUT=*
//*
//JSTEP03  EXEC PGM=ASSEMBL3,COND=(0,LT)
//*
//*****************************************************************
//* JSTEP03  EXCUTE THE ASSEMBLER PROGRAM AND DISPLAY OUT         *
//*                                                               *
//* DDNAME             FILE DESCRIPTION                           *
//*                                                               *
//* RECSIN     INPUT:  DATA3 FROM DATAFA21 CONTAINS DATA FROM PGM *
//* RPTDATA    OUTPUT: THE OUTPUT FROM PROGRAM                    *
//*                                                               *
//*****************************************************************
//*
//STEPLIB  DD  DSN=KC03BB6.CSCI465.LOADLIB,
//             DISP=SHR
//*
//INDATA   DD  DSN=KC02322.CSCI465.DATAFA21(DATA3),
//             DISP=SHR
//*
//RPTDATA  DD  SYSOUT=*
//*
//SYSUDUMP DD  SYSOUT=*
//*
