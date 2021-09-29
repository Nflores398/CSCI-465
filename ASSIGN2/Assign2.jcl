//KC03BB6A JOB ,'N FLORES',MSGCLASS=H
//*
//*****************************************************************
//*                                                               *
//*  CSCI 465               ASSIGNMENT 2               FALL 2021  *
//*                                                               *
//*  DEVELOPER NAME: Noah Flores                                  *
//*  DATE DUE: 09/3/2021                                          *
//*                                                               *
//*  Fetch a program to read a records then store data into a     *
//*  temp data set and fetch another program to write record.     *
//*                                                               *
//*****************************************************************
//*
//JSTEP01  EXEC PGM=READRECS
//*
//*****************************************************************
//* JSTEP01 FETCHES READRECS AND SENDS IN DATASET AND SENDS OUT   *
//* A TEMP DATA SET.                                              *
//*                                                               *
//* DDNAME             FILE DESCRIPTION                           *
//*                                                               *
//* RECSIN     INPUT:  DATA2 FROM DATAFA21 CONTAINS DATA FROM PGM *
//*                                                               *
//* RECSOUT    OUTPUT: TEMP DATASET(DATAOUT) RESULT FROM PGM      *
//*                                                               *
//*****************************************************************
//*
//STEPLIB  DD  DSN=KC02322.CSCI465.LOADLIB,
//             DISP=SHR
//*
//RECSIN   DD  DSN=KC02322.CSCI465.DATAFA21(DATA2),
//             DISP=SHR
//*
//RECSOUT  DD  DSN=&&DATAOUT,DISP=(MOD,PASS),
//             DCB=(LRECL=80)
//*
//SYSUDUMP DD  SYSOUT=*
//*
//* END OF JSTEP01
//*
//JSTEP02  EXEC PGM=WRTERECS,COND=(0,LT)
//*
//*****************************************************************
//* JSTEP02 FETCHES WRTERECS AND SENDS IN DATASET AND SENDS OUT   *
//* A TEXT TO BE PRINTED                                          *
//*                                                               *
//* DDNAME             FILE DESCRIPTION                           *
//*                                                               *
//* RECSIN     INPUT: SENDS IN &&DATAOUT INTO WRTERECS            *
//*                                                               *
//* RECSOUT    OUTPUT: TEXT FROM WRTERECS AND PRINTED IN STDOUTPUT*
//*                                                               *
//*****************************************************************
//*
//STEPLIB  DD  DSN=KC02322.CSCI465.LOADLIB,
//             DISP=SHR
//*
//RECSIN   DD  DSN=&&DATAOUT,
//             DISP=(SHR,PASS),
//             DCB=(LRECL=80)
//*
//RECSOUT  DD   SYSOUT=*
//*
//* END OF JSTEP02
//*