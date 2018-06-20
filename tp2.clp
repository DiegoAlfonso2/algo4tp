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
            SELECT SORTFILE
               ASSIGN TO DISK
               SORT STATUS IS SORT-FS.

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
           SD SORTFILE.
           01 SORT-RECORD.
               03 SORT-PK.
                   05 SORT-COD-SOL    PIC 9(6).
                   05 SORT-FECHA      PIC X(10).
                   05 SORT-COD-PROD   PIC 9(4).
               03 SORT-CANTIDAD       PIC 9(4).
               03 SORT-COD-VEND       PIC 9(3).
               03 SORT-IMPORTE        PIC 9(7)V99.
       WORKING-STORAGE SECTION.
      * FILE STATUSES DE ARCHIVOS
           01 SOLIC-FS                     PIC 9(2).
               88 SOLIC-OK                 VALUE '00'.
               88 SOLIC-EOF                VALUE '10'.
           01 SORT-FS                     PIC 9(2).
               88 SORT-OK                 VALUE '00'.
               88 SORT-EOF                VALUE '10'.
           01 FIN-SORT                    PIC X.

       PROCEDURE DIVISION.
       PRINCIPAL.
           SORT SORTFILE
               ON ASCENDING KEY SORT-COD-PROD
               INPUT PROCEDURE IS ENTRADA
               OUTPUT PROCEDURE IS SALIDA.
           STOP RUN.

       ENTRADA SECTION.
           PERFORM INICIO.
           PERFORM PROCESO.
           PERFORM FIN.
    
       RUTINAS-ENTRADA SECTION.
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
           DISPLAY '--> ' SOLIC-RECORD.
           MOVE SOLIC-RECORD TO SORT-RECORD.
           RELEASE SORT-RECORD.
           PERFORM LEER-SOLIC.

       FIN.
           CLOSE SOLIC.

       SALIDA SECTION.
       SALIDAPPAL.
           RETURN SORTFILE AT END MOVE 'S' TO FIN-SORT.
           PERFORM LEER-SORT UNTIL FIN-SORT = 'S' OR SORT-EOF.

       RUTINAS-SALIDA SECTION.
       LEER-SORT.
           DISPLAY '<-- ' SORT-RECORD.
           RETURN SORTFILE AT END MOVE 'S' TO FIN-SORT.
