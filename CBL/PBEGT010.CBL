       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBEGT010.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INP-FILE  ASSIGN TO INPFILE
                            STATUS ST-INP.
           SELECT OUT-FILE  ASSIGN TO OUTFILE
                            STATUS ST-OUT.
       DATA DIVISION.
       FILE SECTION.
        FD  INP-FILE RECORDING MODE F.
         01  FLTIN.
           05 REC-PROCESS    PIC 9(01).
           05 REC-ID         PIC 9(05).
           05 REC-DVZ        PIC 9(03).
        FD  OUT-FILE RECORDING MODE F.
         01  PRINT-REC.
           05 OUT-PROC-TYPE  PIC X(01).
           05 REC-ID-O       PIC 9(05).
           05 REC-DVZ-O      PIC 9(03).
           05 REC-FILLER-RC  PIC X(03).
           05 REC-RETRN-CODE PIC 9(02).
           05 REC-DESC-O     PIC X(30).
           05 REC-NFROM-O    PIC X(15).
           05 REC-SNFROM-O   PIC X(15).
           05 REC-NTO-O      PIC X(15).
           05 REC-SNTO-O     PIC X(15).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    HEADER AREA STRAT                                           *
      *----------------------------------------------------------------*
       01  HEADER-1.
         05  FILLER         PIC X(01) VALUE ' '.
         05  FILLER         PIC X(05) VALUE 'Id   '.
         05  FILLER         PIC X(03) VALUE 'Dvz'.
         05  FILLER         PIC X(05) VALUE 'RC   '.
         05  FILLER         PIC X(30) VALUE 'Description    '.
         05  FILLER         PIC X(30) VALUE 'Old Name Surname'.
         05  FILLER         PIC X(30) VALUE 'New Name Surname'.
      *
       01  HEADER-2.
         05  FILLER         PIC X(01) VALUE '-'.
         05  FILLER         PIC X(05) VALUE '-----'.
         05  FILLER         PIC X(03) VALUE '---'.
         05  FILLER         PIC X(05) VALUE '-----'.
         05  FILLER         PIC X(30)
                              VALUE '------------------------------'.
         05  FILLER         PIC X(30)
                              VALUE '------------------------------'.
         05  FILLER         PIC X(30)
                              VALUE '------------------------------'.
      *----------------------------------------------------------------*
      *    HEADER AREA END                                             *
      *----------------------------------------------------------------*
       01 WS-WORK-AREA.
         05 WS-CALLCBL     PIC X(08) VALUE 'PBEGT011'.
         05 ST-INP         PIC 9(2).
              88 INP-FILE-EOF                   VALUE 10.
              88 INP-FILE-SUCCESS               VALUE 0 97.
              88 INP-FILE-NOTFND                VALUE 23.
         05 ST-OUT         PIC 9(2).
              88 OUT-FILE-SUCCESS               VALUE 0 97.
         03 WS-LINKGROUP.
            05 WS-SUB-ID         PIC 9(05).
            05 WS-SUB-DVZ        PIC 9(03).
            05 WS-SUB-RC         PIC 9(02).
            05 WS-SUB-DESC       PIC X(30).
            05 WS-SUB-DATA       PIC X(60).
            05 WS-FUNCTION       PIC 9(01).
              88 WS-FUNC-OPEN         VALUE 1.
              88 WS-FUNC-READ         VALUE 2.
              88 WS-FUNC-UPDT         VALUE 3.
              88 WS-FUNC-ADD          VALUE 4.
              88 WS-FUNC-DEL          VALUE 5.
              88 WS-FUNC-CLOS         VALUE 6.
              88 WS-FUNC-ERR          VALUE 0.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM H100-OPEN-FILES.
           WRITE PRINT-REC FROM HEADER-1.
           WRITE PRINT-REC FROM HEADER-2.
           PERFORM H200-PROCESS UNTIL INP-FILE-EOF.
           PERFORM H999-PROGRAM-EXIT.
       0000-END. EXIT.

       H100-OPEN-FILES.
           OPEN INPUT  INP-FILE.
           OPEN OUTPUT OUT-FILE.
           IF NOT INP-FILE-SUCCESS
           DISPLAY 'UNABLE TO OPEN INPFILE: ' ST-INP
           MOVE ST-INP TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.

           IF NOT OUT-FILE-SUCCESS
           DISPLAY 'UNABLE TO OPEN OUTFILE: ' ST-OUT
           MOVE ST-OUT TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.

           SET WS-FUNC-OPEN TO TRUE.
           CALL WS-CALLCBL USING WS-LINKGROUP.
       H100-END. EXIT.

       H200-PROCESS.
           READ INP-FILE.
           IF (NOT INP-FILE-SUCCESS) AND (INP-FILE-NOTFND)
           DISPLAY 'UNABLE TO READ INPFILE: ' ST-INP
           MOVE ST-INP TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
           IF (NOT INP-FILE-EOF) AND (NOT REC-PROCESS = 4)
              MOVE REC-ID TO WS-SUB-ID
              MOVE REC-DVZ TO WS-SUB-DVZ
              SET WS-FUNC-READ TO TRUE
              CALL WS-CALLCBL USING WS-LINKGROUP
           END-IF.

           PERFORM H201-PROCESS-SELECT.

           PERFORM H300-WRITE-RECORD.
       H200-END. EXIT.

       H201-PROCESS-SELECT.
           MOVE REC-ID TO WS-SUB-ID
           MOVE REC-DVZ TO WS-SUB-DVZ
           EVALUATE REC-PROCESS
              WHEN 3
                 SET WS-FUNC-UPDT TO TRUE
              WHEN 4
                 SET WS-FUNC-ADD TO TRUE
              WHEN 5
                 SET WS-FUNC-DEL TO TRUE
           END-EVALUATE
           CALL WS-CALLCBL USING WS-LINKGROUP.
       H201-END. EXIT.

       H300-WRITE-RECORD.
           MOVE REC-PROCESS           TO  OUT-PROC-TYPE.
           MOVE WS-SUB-ID             TO  REC-ID-O.
           MOVE WS-SUB-DVZ            TO  REC-DVZ-O.
           MOVE 'RC:'                 TO  REC-FILLER-RC.
           MOVE WS-SUB-RC             TO  REC-RETRN-CODE.
           MOVE WS-SUB-DESC           TO  REC-DESC-O.
           MOVE WS-SUB-DATA(1:15)     TO  REC-NFROM-O.
           MOVE WS-SUB-DATA(16:15)    TO  REC-SNFROM-O.
           MOVE WS-SUB-DATA(30:15)    TO  REC-NTO-O.
           MOVE WS-SUB-DATA(45:15)    TO  REC-SNTO-O.
           WRITE PRINT-REC.
       H300-END. EXIT.

       H999-PROGRAM-EXIT.
           CLOSE INP-FILE.
           SET WS-FUNC-CLOS TO TRUE.
           CALL WS-CALLCBL USING WS-LINKGROUP.
           CLOSE OUT-FILE.
           GOBACK.
       H999-EXIT.
