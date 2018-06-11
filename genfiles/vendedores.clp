       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TRABAJO-PRACTICO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VEND
               ASSIGN TO 'VENDEDORES.TXT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS VEND-FS.
           SELECT VEND-OUT
               ASSIGN TO 'VENDEDORES.DAT'
               ORGANIZATION IS INDEXED
               RECORD KEY IS VEND-OUT-COD-VEND
               FILE STATUS IS VEND-OUT-FS.

       DATA DIVISION.
       FILE SECTION.
           FD VEND.
           01  VEND-RECORD.
               05 VEND-COD-VEND           PIC 9(3).
               05 VEND-FECHA-INGR         PIC 9(8).
               05 VEND-DIRECCION          PIC X(20).
               05 VEND-APEYNOM            PIC X(25).
               05 VEND-TELEFONO           PIC 9(10).
               05 VEND-CATEGORIA          PIC X.
           FD VEND-OUT.
           01  VEND-OUT-RECORD.
               05 VEND-OUT-COD-VEND           PIC 9(3).
               05 VEND-OUT-FECHA-INGR         PIC 9(8).
               05 VEND-OUT-DIRECCION          PIC X(20).
               05 VEND-OUT-APEYNOM            PIC X(25).
               05 VEND-OUT-TELEFONO           PIC 9(10).
               05 VEND-OUT-CATEGORIA          PIC X.
       WORKING-STORAGE SECTION.
      * FILE STATUSES DE ARCHIVOS
           01 VEND-FS                     PIC 9(2).
               88 VEND-OK                 VALUE '00'.
               88 VEND-EOF                VALUE '10'.
           01 VEND-OUT-FS                     PIC 9(2).
               88 VEND-OUT-OK                 VALUE '00'.
               88 VEND-OUT-EOF                VALUE '10'.
           01 WS-MENSAJE-ERROR            PIC X(50).
               
       PROCEDURE DIVISION.
       PRINCIPAL.
           OPEN INPUT VEND.
           OPEN OUTPUT VEND-OUT.
           PERFORM LEER-VEND.
           PERFORM PROCESO UNTIL VEND-EOF.
           PERFORM FIN.

       PROCESO.
           DISPLAY VEND-RECORD.
           MOVE VEND-RECORD TO VEND-OUT-RECORD.
           PERFORM GRABAR-VEND-OUT.
           PERFORM LEER-VEND.

       LEER-VEND.
           READ VEND.
           IF NOT VEND-OK AND NOT VEND-EOF THEN
               MOVE 'ERROR LEYENDO ARCHIVO VEND.TXT' 
                        TO WS-MENSAJE-ERROR
               PERFORM MANEJAR-ERROR
           END-IF.

        GRABAR-VEND-OUT.
           WRITE VEND-OUT-RECORD
               INVALID KEY 
                    MOVE 'PK-ERROR ESCRIBIENDO ARCHIVO DE SALIDA'
                        TO WS-MENSAJE-ERROR
                    PERFORM MANEJAR-ERROR.
           IF NOT VEND-OUT-OK THEN
               MOVE 'ERROR ESCRIBIENDO ARCHIVO DE SALIDA' 
                        TO WS-MENSAJE-ERROR
               PERFORM MANEJAR-ERROR
           END-IF.

       MANEJAR-ERROR.
           DISPLAY WS-MENSAJE-ERROR.
           PERFORM FIN.

       FIN.
           CLOSE VEND.
           CLOSE VEND-OUT.
           STOP RUN.
