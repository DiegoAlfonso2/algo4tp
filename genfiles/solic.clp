       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TRABAJO-PRACTICO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SOLIC
               ASSIGN TO 'SOLIC.TXT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS SOLIC-FS.
           SELECT SOLIC-OUT
               ASSIGN TO 'SOLIC.DAT'
               ORGANIZATION IS INDEXED
               ACCESS IS RANDOM
               RECORD KEY IS SOLIC-OUT-PK
               ALTERNATE RECORD KEY IS SOLIC-OUT-COD-VEND
                   WITH DUPLICATES
               FILE STATUS IS SOLIC-OUT-FS.

       DATA DIVISION.
       FILE SECTION.
           FD SOLIC.
           01  SOLIC-RECORD.
               05 SOLIC-COD-SOL            PIC 9(6).
               05 SOLIC-FECHA              PIC X(10).
               05 SOLIC-COD-PROD           PIC 9(4).
               05 SOLIC-CANTIDAD           PIC 9(4).
               05 SOLIC-COD-VEND           PIC 9(3).
               05 SOLIC-IMPORTE            PIC 9(7)V99.
           FD SOLIC-OUT.
           01  SOLIC-OUT-RECORD.
               03 SOLIC-OUT-PK.
                   05 SOLIC-OUT-COD-SOL    PIC 9(6).
                   05 SOLIC-OUT-FECHA      PIC X(10).
                   05 SOLIC-OUT-COD-PROD   PIC 9(4).
               03 SOLIC-OUT-CANTIDAD       PIC 9(4).
               03 SOLIC-OUT-COD-VEND       PIC 9(3).
               03 SOLIC-OUT-IMPORTE        PIC 9(7)V99.
       WORKING-STORAGE SECTION.
      * FILE STATUSES DE ARCHIVOS
           01 SOLIC-FS                     PIC 9(2).
               88 SOLIC-OK                 VALUE '00'.
               88 SOLIC-EOF                VALUE '10'.
           01 SOLIC-OUT-FS                     PIC 9(2).
               88 SOLIC-OUT-OK                 VALUE '00'.
               88 SOLIC-OUT-EOF                VALUE '10'.
           01 WS-MENSAJE-ERROR            PIC X(50).
               
       PROCEDURE DIVISION.
       PRINCIPAL.
           OPEN INPUT SOLIC.
           OPEN OUTPUT SOLIC-OUT.
           PERFORM LEER-SOLIC.
           PERFORM PROCESO UNTIL SOLIC-EOF.
           PERFORM FIN.

       PROCESO.
           DISPLAY "|" SOLIC-RECORD "|".
           MOVE SOLIC-RECORD TO SOLIC-OUT-RECORD.
           PERFORM GRABAR-SOLIC-OUT.
           PERFORM LEER-SOLIC.

       LEER-SOLIC.
           READ SOLIC.
           IF NOT SOLIC-OK AND NOT SOLIC-EOF THEN
               MOVE 'ERROR LEYENDO ARCHIVO SOLIC.TXT' 
                        TO WS-MENSAJE-ERROR
               PERFORM MANEJAR-ERROR
           END-IF.

        GRABAR-SOLIC-OUT.
           DISPLAY ">" SOLIC-OUT-PK "<".
           WRITE SOLIC-OUT-RECORD
               INVALID KEY 
                    MOVE 'PK-ERROR ESCRIBIENDO ARCHIVO DE SALIDA'
                        TO WS-MENSAJE-ERROR
                    PERFORM MANEJAR-ERROR.
           IF NOT SOLIC-OUT-OK THEN
               MOVE 'ERROR ESCRIBIENDO ARCHIVO DE SALIDA' 
                        TO WS-MENSAJE-ERROR
               PERFORM MANEJAR-ERROR
           END-IF.

       MANEJAR-ERROR.
           DISPLAY WS-MENSAJE-ERROR.
           PERFORM FIN.

       FIN.
           CLOSE SOLIC.
           CLOSE SOLIC-OUT.
           STOP RUN.
