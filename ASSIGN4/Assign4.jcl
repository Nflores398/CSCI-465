//KC03BB6A JOB ,'N FLORES',MSGCLASS=H
//*
//*****************************************************************
//*                                                               *
//*  CSCI 465               ASSIGNMENT 4               FALL 2021  *
//*                                                               *
//*  DEVELOPER NAME: Noah Flores                                  *
//*  DATE DUE: 09/01/2021                                         *
//*                                                               *
//* Use COBOL Complier the code that will read in data and        *
//* print out the data in standerd output. Then bind, fetch, and  *
//* execute program                                               *
//*                                                               *
//*****************************************************************
//*
//JSTEP01  EXEC PGM=IGYCRCTL,REGION=0M,PARM=APOST
//*
//*****************************************************************
//* JSTEP01  CALL COBOL COMPLIER AND COMPLIER PROGRAM             *
//*                                                               *
//* DDNAME             FILE DESCRIPTION                           *
//*                                                               *
//* SYSLIN    OUTPUT: TEMP DATASET TO PASS TO BINDER              *
//*                                                               *
//*****************************************************************
//*
//STEPLIB  DD  DSN=IGY630.SIGYCOMP,DISP=SHR
//*
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//*
//SYSLIN   DD  DSN=&&OBJECT,DISP=(MOD,PASS),
//             DCB=(LRECL=80)
//*
//SYSPRINT DD  SYSOUT=*
//*
//SYSUT1   DD  SPACE=(CYL,(1,1))
//*
//SYSUT2   DD  SPACE=(CYL,(1,1))
//*
//SYSUT3   DD  SPACE=(CYL,(1,1))
//*
//SYSUT4   DD  SPACE=(CYL,(1,1))
//*
//SYSUT5   DD  SPACE=(CYL,(1,1))
//*
//SYSUT6   DD  SPACE=(CYL,(1,1))
//*
//SYSUT7   DD  SPACE=(CYL,(1,1))
//*
//SYSUT8   DD  SPACE=(CYL,(1,1))
//*
//SYSUT9   DD  SPACE=(CYL,(1,1))
//*
//SYSUT10  DD  SPACE=(CYL,(1,1))
//*
//SYSUT11  DD  SPACE=(CYL,(1,1))
//*
//SYSUT12  DD  SPACE=(CYL,(1,1))
//*
//SYSUT13  DD  SPACE=(CYL,(1,1))
//*
//SYSUT14  DD  SPACE=(CYL,(1,1))
//*
//SYSUT15  DD  SPACE=(CYL,(1,1))
//*
//SYSMDECK DD  SPACE=(CYL,(1,1))
//*
      ****************************************************************
      *                                                              *
      * PROGRAM NAME:  SALESRPT.                                     *
      *                                                              *
      * FUNCTION:  THIS PROGRAM READS INPUT DATA RECORDS AND THEN    *
      *            WRITES THEM TO STANDARD OUTPUT.                   *
      *                                                              *
      * INPUT:     SALESFLE - THE INPUT DATA RECORDS READ IN.        *
      *            WHICH CONTAIN A BRACH NAME, BROKER NAME, AND      *
      *             DEPOSIT AMOUT.                                   *
      *                                                              *
      * OUTPUT:    RPTFLE - THE INPUT DATA RECORDS WRITTEN OUT.      *
      *                                                              *
      ****************************************************************

       IDENTIFICATION DIVISION.

       PROGRAM-ID.    SALESRPT.
       AUTHOR.        NOAH L FLORES.
       DATE-WRITTEN.  09/25/2021.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT SALE-FILE ASSIGN TO SALESFLE.
           SELECT REPORT-FILE ASSIGN TO RPTFLE.

       DATA DIVISION.

       FILE SECTION.

      ****************************************************************
      * SALE-FILE THE INPUT DATA RECORDS READ IN.                    *
      * WHICH CONTAIN A BRACH NAME, BROKER NAME, AND EPOSIT AMOUT.   *
      ****************************************************************
       FD  SALE-FILE
           RECORDING MODE F.

       01  SALE-RECORD.
          05   IN-BRANCH-NME           PIC X(25).
          05   IN-BROKER-NME           PIC X(25).
          05   IN-DEP-AMT              PIC S9(9)V99.
          05   FILLER                  PIC X(3).
          05   IN-COMM-FLG             PIC 9(1).
          05   FILLER                  PIC X(15).

      ****************************************************************
      * REPORT-FILE THE OUTPUT IS PRINTED HERE WITH ALL DETAILS      *
      ****************************************************************
       FD  REPORT-FILE
           RECORDING MODE F
           BLOCK CONTAINS 20 RECORDS.

       01  REPORT-RECORD               PIC X(132).

       WORKING-STORAGE SECTION.
      ****************************************************************
      * VARIABLES:                                                   *
      * EOFFLAG              CHANGES TO 'Y' WHEN END OF FILE OCCURS. *
      *                                                              *
      * TOT-DEP-BAL          IS THE TOTAL DEPOSITE BALACE.           *
      *                                                              *
      * DETAIL-1             USED TO PRINT OUT BRANCH NAME. BROKER   *
      *                       AME, DEPOSITE AMOUNT, AND COM AMOUNT.  *
      *                                                              *
      * TOTAL-LINE           USED TO PRINT OUT THE TOTALS AT THE END *
      *                      OF THE REPORT.                          *
      ****************************************************************

       01  VARIABLES.
           05  EOFFLAG                  PIC X VALUE 'N'.
           05  TOT-DEP-BAL             PIC S9(11)V99 VALUE 0.

       01  DETAIL-1.
          05   DETAIL-LINE.
               10  OUT-BRANCH-NME      PIC X(25).
               10  FILLER              PIC X(10) VALUE SPACE.
               10  OUT-BROKER-NME      PIC X(25).
               10  FILLER              PIC X(10) VALUE SPACE.
               10  OUT-DEP-AMT         PIC $$$$,$$$,$$9.99.
               10  FILLER              PIC X(10) VALUE SPACE.
               10  OUT-COMM-AMT        PIC X(1).
               10  FILLER              PIC X(36) VALUE SPACE.
          05   DETAIL-LINE-SPACES REDEFINES DETAIL-LINE
                                       PIC X(132).
       01  TOTAL-LINE.
           05  FILLER                  PIC X(67) VALUE SPACES.
           05  OUT-TOT-DEP-BAL         PIC $$$,$$$,$$$,$$9.99.
           05  FILLER                  PIC X(47) VALUE SPACES.

       PROCEDURE DIVISION.
      ****************************************************************
      *0000-MAIN THIS ROUTINE CONTROLS THE FLOW OF THE PROGRAM. FIRST*
      *IT READS IN DATA FROM INPUT AND MOVES IT TO OUTPUT.THEN SECOND*
      *IT WILL ADD THE TOTAL ACCOUNT BALANCE. REPEAT UNTIL ENDOFFILE *
      ****************************************************************
       0000-MAIN.

           OPEN INPUT SALE-FILE
               OUTPUT REPORT-FILE.

           READ SALE-FILE
            AT END MOVE 'Y' TO EOFFLAG
           END-READ.

           IF EOFFLAG = 'N'

           READ SALE-FILE
            AT END MOVE 'Y' TO EOFFLAG
           END-READ

           PERFORM 0100-PROCESS-RECORD
               UNTIL EOFFLAG = 'Y'

           PERFORM 0200-PROCESS-TOTALS

           END-IF.

           CLOSE SALE-FILE
                REPORT-FILE.

           GOBACK.

       0000-EXIT. EXIT.

      ****************************************************************
      *0100-PROCESS-RECORD THIS ROUTINE MOVE INPUT IN TO OUTPUT      *
      *THEN READ NEXT FILE UNTIL END OF FILE. IT WILL ALSO ADD       *
      *IN-DEP-AMT INTO TOT-DEP-BAL TO GET A GRAND TOTAL.             *
      ****************************************************************
       0100-PROCESS-RECORD.

           MOVE IN-BRANCH-NME TO OUT-BRANCH-NME.
           MOVE IN-BROKER-NME TO OUT-BROKER-NME.
           MOVE IN-DEP-AMT TO OUT-DEP-AMT.
           MOVE IN-COMM-FLG TO OUT-COMM-AMT.

           COMPUTE TOT-DEP-BAL ROUNDED =
                   TOT-DEP-BAL + IN-DEP-AMT.

           WRITE REPORT-RECORD FROM DETAIL-LINE AFTER 2.

           READ SALE-FILE
           AT END MOVE 'Y' TO EOFFLAG
           END-READ.
       0100-EXIT. EXIT.

      ****************************************************************
      *0200-PROCESS-TOTALS THIS ROUNTINE IS USED TO MOVE             *
      *TOT-DEP-BAL TO OUT-TOT-DEP-BAL SO IT CAN BE PRINTED           *
      ****************************************************************
       0200-PROCESS-TOTALS.

           MOVE TOT-DEP-BAL TO OUT-TOT-DEP-BAL.

           WRITE REPORT-RECORD FROM TOTAL-LINE AFTER 2.

       0200-EXIT. EXIT.
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
//SYSLMOD  DD  DSN=KC03BB6.CSCI465.LOADLIB(SALESRPT),
//             SPACE=(1024,(50,20,1)),DSNTYPE=LIBRARY,
//             DISP=(MOD,KEEP,KEEP)
//*
//SYSPRINT DD  SYSOUT=*
//*
//SYSTERM  DD  SYSOUT=*
//*
//JSTEP03  EXEC PGM=SALESRPT,COND=(0,LT)
//*
//*****************************************************************
//* JSTEP03  EXCUTE THE COBOL PROGRAM AND DISPLAY OUT             *
//*                                                               *
//* DDNAME             FILE DESCRIPTION                           *
//*                                                               *
//* SALESFLE   INPUT:  DATA4 FROM DATAFA21 CONTAINS DATA FROM PGM *
//* RPTFLE     OUTPUT: THE OUTPUT FROM PROGRAM                    *
//*                                                               *
//*****************************************************************
//*
//STEPLIB  DD  DSN=KC03BB6.CSCI465.LOADLIB,
//             DISP=SHR
//*
//SALESFLE DD  DSN=KC02322.CSCI465.DATAFA21(DATA4),
//             DISP=SHR
//*
//RPTFLE   DD  SYSOUT=*
//*
//SYSUDUMP DD  SYSOUT=*
//*




