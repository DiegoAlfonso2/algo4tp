       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TRABAJO-PRACTICO2.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT SOLIC
               ASSIGN TO 'SOLIC.DAT'
               ORGANIZATION IS INDEXED
               ACCESS IS SEQUENTIAL
               RECORD KEY IS SOLIC-PK
               ALTERNATE RECORD KEY IS SOLIC-COD-VEND
                   WITH DUPLICATES
               FILE STATUS IS SOLIC-FS.

       DATA DIVISION.
       FILE SECTION.
           FD SOLIC.
           01 SOLIC-RECORD.
               03 SOLIC-PK.
                   05 SOLIC-COD-SOL    PIC 9(6).
                   05 SOLIC-FECHA      PIC X(10).
                   05 SOLIC-COD-PROD   PIC 9(4).
               03 SOLIC-CANTIDAD       PIC 9(4).
               03 SOLIC-COD-VEND       PIC 9(3).
               03 SOLIC-IMPORTE        PIC 9(7)V99.
       WORKING-STORAGE SECTION.
      * FILE STATUSES DE ARCHIVOS
           01 SOLIC-FS                     PIC 9(2).
               88 SOLIC-OK                 VALUE '00'.
               88 SOLIC-EOF                VALUE '10'.

       PROCEDURE DIVISION.
       PRINCIPAL.
           PERFORM INICIO.
           PERFORM PROCESO.
           PERFORM FIN.
    
       INICIO.
           OPEN INPUT SOLIC.

       PROCESO.
           PERFORM LEER-SOLIC.
           PERFORM SEGUIR-LEYENDO UNTIL SOLIC-EOF.

       LEER-SOLIC.
           READ SOLIC RECORD.
           IF NOT SOLIC-OK AND NOT SOLIC-EOF THEN
               DISPLAY 'ERROR LEYENDO ARCHIVO SOLIC.DAT: ' SOLIC-FS
               PERFORM FIN
           END-IF.

       SEGUIR-LEYENDO.
           DISPLAY SOLIC-RECORD.
           PERFORM LEER-SOLIC.

       FIN.
           CLOSE SOLIC.
           STOP RUN.
