       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TRABAJO-PRACTICO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PVE
               ASSIGN TO 'PROD-VEND.TXT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS PVE-FS.
           SELECT PVE-OUT
               ASSIGN TO 'PROD-VEND.DAT'
               ORGANIZATION IS INDEXED
               ACCESS IS RANDOM
               RECORD KEY IS PVE-OUT-PK
               FILE STATUS IS PVE-OUT-FS.

       DATA DIVISION.
       FILE SECTION.
           FD PVE.
           01  PVE-RECORD.
               05 PVE-COD-PROD           PIC 9(4).
               05 PVE-FECHA              PIC X(10).
               05 PVE-CANTIDAD           PIC 9(4).
               05 PVE-IMPORTE            PIC 9(7)V99.
           FD PVE-OUT.
           01  PVE-OUT-RECORD.
               03 PVE-OUT-PK.
                   05 PVE-OUT-COD-PROD   PIC 9(4).
                   05 PVE-OUT-FECHA      PIC X(10).
               03 PVE-OUT-CANTIDAD       PIC 9(4).
               03 PVE-OUT-IMPORTE        PIC 9(7)V99.
       WORKING-STORAGE SECTION.
      * FILE STATUSES DE ARCHIVOS
           01 PVE-FS                     PIC 9(2).
               88 PVE-OK                 VALUE '00'.
               88 PVE-EOF                VALUE '10'.
           01 PVE-OUT-FS                     PIC 9(2).
               88 PVE-OUT-OK                 VALUE '00'.
               88 PVE-OUT-EOF                VALUE '10'.
           01 WS-MENSAJE-ERROR            PIC X(50).
               
       PROCEDURE DIVISION.
       PRINCIPAL.
           OPEN INPUT PVE.
           OPEN OUTPUT PVE-OUT.
           PERFORM LEER-PVE.
           PERFORM PROCESO UNTIL PVE-EOF.
           PERFORM FIN.

       PROCESO.
           DISPLAY PVE-RECORD.
           MOVE PVE-RECORD TO PVE-OUT-RECORD.
           PERFORM GRABAR-PVE-OUT.
           PERFORM LEER-PVE.

       LEER-PVE.
           READ PVE.
           IF NOT PVE-OK AND NOT PVE-EOF THEN
               MOVE 'ERROR LEYENDO ARCHIVO PROD-VEND.TXT' 
                        TO WS-MENSAJE-ERROR
               PERFORM MANEJAR-ERROR
           END-IF.

        GRABAR-PVE-OUT.
           WRITE PVE-OUT-RECORD
               INVALID KEY 
                    MOVE 'PK-ERROR ESCRIBIENDO ARCHIVO DE SALIDA'
                        TO WS-MENSAJE-ERROR
                    PERFORM MANEJAR-ERROR.
           IF NOT PVE-OUT-OK THEN
               MOVE 'ERROR ESCRIBIENDO ARCHIVO DE SALIDA' 
                        TO WS-MENSAJE-ERROR
               PERFORM MANEJAR-ERROR
           END-IF.

       MANEJAR-ERROR.
           DISPLAY WS-MENSAJE-ERROR.
           PERFORM FIN.

       FIN.
           CLOSE PVE.
           CLOSE PVE-OUT.
           STOP RUN.
