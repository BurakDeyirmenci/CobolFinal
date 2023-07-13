//JPBEGT10 JOB 1,NOTIFY=&SYSUID
//***************************************************/
//* Copyright Contributors to the COBOL Programming Course
//* SPDX-License-Identifier: CC-BY-4.0
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(PBEGT011),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(PBEGT011),DISP=SHR
//***************************************************/
// IF RC < 5 THEN
//***************************************************/
//DELET100 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
   DELETE Z95628.QSAM.RST NONVSAM
   IF LASTCC LE 08 THEN SET MAXCC = 00
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(PBEGT010),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(PBEGT010),DISP=SHR
//LKED.SYSLIB  DD DSN=&SYSUID..LOAD(PBEGT011),DISP=SHR
//RUN     EXEC PGM=PBEGT010
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//INPFILE   DD DSN=&SYSUID..QSAM.INP,DISP=SHR
//IDXFILE   DD DSN=&SYSUID..VSAM.AA,DISP=SHR
//OUTFILE   DD DSN=&SYSUID..QSAM.RST,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,
//             SPACE=(TRK,(10,10),RLSE),
//             DCB=(RECFM=FB,LRECL=104,BLKSIZE=0)
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
