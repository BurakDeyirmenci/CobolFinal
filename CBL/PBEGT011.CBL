       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBEGT011.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE  ASSIGN TO IDXFILE
                            ORGANIZATION INDEXED
                            ACCESS RANDOM
                            RECORD KEY IDX-KEY
                            STATUS ST-IDX.
       DATA DIVISION.
       FILE SECTION.
       FD  IDX-FILE.
         01  IDX-REC.
           05 IDX-KEY.
              07 IDX-ID      PIC S9(5) COMP-3.
              07 IDX-DVZ     PIC S9(3) COMP.
           05 IDX-NAME       PIC X(15).
           05 IDX-SURNAME    PIC X(15).
           05 IDX-DATE       PIC S9(7) COMP-3.
           05 IDX-BALANCE    PIC S9(15) COMP-3.
       WORKING-STORAGE SECTION.
         01  WS-WORK-AREA.
           05 ST-IDX         PIC 9(2).
              88 IDX-FILE-SUCCESS               VALUE 0 97.
              88 IDX-FILE-NOTFND                VALUE 23.
           05 WS-OLD-NAME    PIC X(30).
           05 WS-TEMP-NAME   PIC X(15).
           05 WS-COUNTER     PIC 9(02).
           05 WS-O-COUNTER     PIC 9(02).

       LINKAGE SECTION.
         01 LS-WORK-AREA.
           05 WS-SUB-ID         PIC 9(05).
           05 WS-SUB-DVZ        PIC 9(03).
           05 WS-SUB-RC         PIC 9(02).
           05 WS-SUB-DESC       PIC X(30).
           05 WS-SUB-DATA       PIC X(60).
           05 WS-FUNCTION       PIC 9(01).
              88 WS-FUNC-OPEN                   VALUE 1.
              88 WS-FUNC-READ                   VALUE 2.
              88 WS-FUNC-UPDT                   VALUE 3.
              88 WS-FUNC-ADD                    VALUE 4.
              88 WS-FUNC-DEL                    VALUE 5.
              88 WS-FUNC-CLOS                   VALUE 6.
              88 WS-FUNC-ERR                    VALUE 0.

      *--------------------
       PROCEDURE DIVISION USING LS-WORK-AREA.
       0000-MAIN.
           MOVE SPACES TO WS-SUB-DESC
           MOVE SPACES TO WS-SUB-DATA
           EVALUATE TRUE
              WHEN WS-FUNC-OPEN
                 PERFORM H100-OPEN-FILES
              WHEN WS-FUNC-READ
                 PERFORM H200-READ-RECORD
              WHEN WS-FUNC-UPDT
                 PERFORM H300-UPDATE-RECORD
              WHEN WS-FUNC-ADD
                 PERFORM H400-ADD-RECORD
              WHEN WS-FUNC-DEL
                 PERFORM H500-DELETE-RECORD
              WHEN WS-FUNC-CLOS
                 PERFORM H999-PROGRAM-EXIT
              WHEN OTHER
                 DISPLAY 'INVALID FUNCTION: ' WS-FUNCTION
              END-EVALUATE.
              GOBACK.
       0000-END. EXIT.

       H100-OPEN-FILES.
           OPEN I-O    IDX-FILE.
           IF NOT IDX-FILE-SUCCESS
           DISPLAY 'UNABLE TO OPEN IDXFILE: ' ST-IDX
           MOVE ST-IDX TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
           GOBACK.
       H100-END. EXIT.

       H200-READ-RECORD.
           COMPUTE IDX-ID = WS-SUB-ID
           COMPUTE IDX-DVZ = WS-SUB-DVZ
           READ IDX-FILE KEY IDX-KEY
             INVALID KEY
                 MOVE 23 TO WS-SUB-RC
                 MOVE 'RECORD NOT FOUND' to WS-SUB-DESC
                 GOBACK
             NOT INVALID KEY CONTINUE
           GOBACK.
       H200-END. EXIT.

       H300-UPDATE-RECORD.
           STRING IDX-NAME DELIMITED BY SIZE
               IDX-SURNAME DELIMITED BY SIZE
            INTO WS-OLD-NAME.
           MOVE SPACES TO WS-TEMP-NAME.
           MOVE 1 TO WS-O-COUNTER.

           PERFORM VARYING WS-COUNTER FROM 1 BY 1
                     UNTIL WS-COUNTER > LENGTH OF IDX-NAME
                     IF  IDX-NAME (WS-COUNTER:1) = ' '
                         CONTINUE
                     ELSE
                         MOVE IDX-NAME (WS-COUNTER:1) TO
                                         WS-TEMP-NAME(WS-O-COUNTER:1)
                         ADD 1 TO WS-O-COUNTER
                     END-IF
           END-PERFORM.
           MOVE WS-TEMP-NAME TO IDX-NAME
           INSPECT IDX-SURNAME REPLACING ALL "E" BY "I"
           INSPECT IDX-SURNAME REPLACING ALL "A" BY "E"
           REWRITE IDX-REC
           STRING WS-OLD-NAME DELIMITED BY SIZE
               IDX-NAME DELIMITED BY SIZE
               IDX-SURNAME DELIMITED BY SIZE
            INTO WS-SUB-DATA.
            MOVE 'NAME CHANCED' TO WS-SUB-DESC
           GOBACK.
       H300-END. EXIT.

       H400-ADD-RECORD.
           COMPUTE IDX-ID = WS-SUB-ID
           COMPUTE IDX-DVZ = WS-SUB-DVZ
           READ IDX-FILE KEY IDX-KEY
             INVALID KEY PERFORM H401-NEW-RECORD
             NOT INVALID KEY PERFORM H300-UPDATE-RECORD.
       H400-END. EXIT.

       H401-NEW-RECORD.
           MOVE WS-SUB-ID TO IDX-ID
           MOVE WS-SUB-DVZ TO IDX-DVZ
           MOVE 'BURAK' TO IDX-NAME
           MOVE 'DEYIRMENCI' TO IDX-SURNAME
           MOVE 1999348 TO IDX-DATE
           MOVE ZEROS TO IDX-BALANCE
           WRITE IDX-REC
           MOVE SPACES TO WS-OLD-NAME.
           STRING WS-OLD-NAME DELIMITED BY SIZE
               IDX-NAME DELIMITED BY SIZE
               IDX-SURNAME DELIMITED BY SIZE
            INTO WS-SUB-DATA.
           MOVE 'NEW NAME ADDED' TO WS-SUB-DESC
           GOBACK.
       H401-END. EXIT.

       H500-DELETE-RECORD.
           COMPUTE IDX-ID = WS-SUB-ID
           COMPUTE IDX-DVZ = WS-SUB-DVZ
           READ IDX-FILE KEY IDX-KEY
             INVALID KEY
                 MOVE 23 TO WS-SUB-RC
                 MOVE 'RECORD NOT FOUND' to WS-SUB-DESC
                 GOBACK
             NOT INVALID KEY CONTINUE.
             DELETE IDX-FILE RECORD
               INVALID KEY
                 MOVE 23 TO WS-SUB-RC
                 MOVE 'RECORD NOT FOUND' to WS-SUB-DESC
                 GOBACK
               NOT INVALID KEY MOVE 'RECORD DELETED' TO WS-SUB-DESC
           MOVE SPACES TO WS-OLD-NAME.
           STRING IDX-NAME DELIMITED BY SIZE
               IDX-SURNAME DELIMITED BY SIZE
               WS-OLD-NAME DELIMITED BY SIZE
            INTO WS-SUB-DATA.
           GOBACK.
       H500-END. EXIT.

       H999-PROGRAM-EXIT.
           CLOSE IDX-FILE.
           GOBACK.
       H999-END. EXIT.
