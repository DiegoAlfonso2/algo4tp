       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TRABAJO-PRACTICO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PROD
               ASSIGN TO 'PROD.TXT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS PROD-FS.
           SELECT SOL1
               ASSIGN TO 'SOL1.TXT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS SOL1-FS.
           SELECT SOL2
               ASSIGN TO 'SOL2.TXT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS SOL2-FS.
           SELECT SOL3
               ASSIGN TO 'SOL3.TXT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS SOL3-FS.
           SELECT MAE
               ASSIGN TO 'MAE.TXT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS MAE-FS.

       DATA DIVISION.
       FILE SECTION.
           FD PROD.
           01 PROD-RECORD.
               05 PROD-COD-PROD           PIC 9(4).
               05 PROD-FECHA-ALTA         PIC 9(8).
               05 PROD-DESCRIP            PIC X(15).
           FD SOL1.
           01 SOL1-RECORD.
               05 SOL1-COD-SOL            PIC 9(6).
               05 SOL1-FECHA.
                   07 SOL1-FECHA-AAAA     PIC X(4).
                   07 FILLER              PIC X(1).
                   07 SOL1-FECHA-MM       PIC X(2).
                   07 FILLER              PIC X(1).
                   07 SOL1-FECHA-DD       PIC X(2).
               05 SOL1-COD-PROD           PIC 9(4).
               05 SOL1-CANTIDAD           PIC 9(4).
               05 SOL1-COD-VENDEDOR       PIC 9(3).
               05 SOL1-IMPORTE            PIC 9(7)V99.
           FD SOL2.
           01 SOL2-RECORD.
               05 SOL2-COD-SOL            PIC 9(6).
               05 SOL2-FECHA.
                   07 SOL2-FECHA-AAAA     PIC X(4).
                   07 FILLER              PIC X(1).
                   07 SOL2-FECHA-MM       PIC X(2).
                   07 FILLER              PIC X(1).
                   07 SOL2-FECHA-DD       PIC X(2).
               05 SOL2-COD-PROD           PIC 9(4).
               05 SOL2-CANTIDAD           PIC 9(4).
               05 SOL2-COD-VENDEDOR       PIC 9(3).
               05 SOL2-IMPORTE            PIC 9(7)V99.
           FD SOL3.
           01 SOL3-RECORD.
               05 SOL3-COD-SOL            PIC 9(6).
               05 SOL3-FECHA.
                   07 SOL3-FECHA-AAAA     PIC X(4).
                   07 FILLER              PIC X(1).
                   07 SOL3-FECHA-MM       PIC X(2).
                   07 FILLER              PIC X(1).
                   07 SOL3-FECHA-DD       PIC X(2).
               05 SOL3-COD-PROD           PIC 9(4).
               05 SOL3-CANTIDAD           PIC 9(4).
               05 SOL3-COD-VENDEDOR       PIC 9(3).
               05 SOL3-IMPORTE            PIC 9(7)V99.
           FD MAE.
           01 MAE-RECORD.
               05 MAE-COD-SOL            PIC 9(6).
               05 MAE-FECHA.
                   07 MAE-FECHA-AAAA     PIC X(4).
                   07 FILLER              PIC X(1).
                   07 MAE-FECHA-MM       PIC X(2).
                   07 FILLER              PIC X(1).
                   07 MAE-FECHA-DD       PIC X(2).
               05 MAE-COD-PROD           PIC 9(4).
               05 MAE-CANTIDAD           PIC 9(4).
               05 MAE-COD-VENDEDOR       PIC 9(3).
               05 MAE-IMPORTE            PIC 9(7)V99.

       WORKING-STORAGE SECTION.
      * FILE STATUSES DE ARCHIVOS
           01 PROD-FS                     PIC 9(2).
               88 PROD-OK                 VALUE '00'.
               88 PROD-EOF                VALUE '10'.
           01 SOL1-FS                     PIC 9(2).
               88 SOL1-OK                 VALUE '00'.
               88 SOL1-EOF                VALUE '10'.
           01 SOL2-FS                     PIC 9(2).
               88 SOL2-OK                 VALUE '00'.
               88 SOL2-EOF                VALUE '10'.
           01 SOL3-FS                     PIC 9(2).
               88 SOL3-OK                 VALUE '00'.
               88 SOL3-EOF                VALUE '10'.
           01 MAE-FS                     PIC 9(2).
               88 MAE-OK                 VALUE '00'.
               88 MAE-EOF                VALUE '10'.

      * TABLA DE PRODUCTOS
           01 PRODUCTO-TABLE.
               03 PRODUCTO                OCCURS 2000 TIMES
                                          INDEXED BY IX-PROD.
                   05 PRODUCTO-COD        PIC 9(4).
                   05 PRODUCTO-FEC-ALTA   PIC 9(8).
                   05 PRODUCTO-DESCRIP    PIC X(15).

      * VARIABLES
           01 WS-ARCHIVO-ERROR            PIC X(50).

       PROCEDURE DIVISION.
       PRINCIPAL.
           PERFORM INICIO.
           PERFORM FIN.
       
       RECORRER-TABLA.
           DISPLAY PRODUCTO(IX-PROD).

       INICIO.
           PERFORM INICIALIZAR-VARIABLES.
           PERFORM ABRIR-ARCHIVOS.
           PERFORM LEER-PROD VARYING IX-PROD FROM 1 BY 1 
                  UNTIL PROD-EOF OR IX-PROD > 2000.
           PERFORM RECORRER-TABLA VARYING IX-PROD FROM 1 BY 1 
                  UNTIL IX-PROD > 2000.

       ABRIR-ARCHIVOS.
           OPEN INPUT PROD.
           IF NOT PROD-OK THEN
               MOVE 'ERROR ABRIENDO ARCHIVO PROD.TXT' 
                        TO WS-ARCHIVO-ERROR
               PERFORM MANEJAR-ERROR-ARCHIVO
           END-IF.
           OPEN INPUT SOL1.
           IF NOT SOL1-OK THEN
               MOVE 'ERROR ABRIENDO ARCHIVO SOL1.TXT' 
                        TO WS-ARCHIVO-ERROR
               PERFORM MANEJAR-ERROR-ARCHIVO
           END-IF.
           OPEN INPUT SOL2.
           IF NOT SOL2-OK THEN
               MOVE 'ERROR ABRIENDO ARCHIVO SOL2.TXT' 
                        TO WS-ARCHIVO-ERROR
               PERFORM MANEJAR-ERROR-ARCHIVO
           END-IF.
           OPEN INPUT SOL3.
           IF NOT SOL3-OK THEN
               MOVE 'ERROR ABRIENDO ARCHIVO SOL3.TXT' 
                        TO WS-ARCHIVO-ERROR
               PERFORM MANEJAR-ERROR-ARCHIVO
           END-IF.
           OPEN INPUT MAE.
           IF NOT MAE-OK THEN
               MOVE 'ERROR ABRIENDO ARCHIVO MAE.TXT' 
                        TO WS-ARCHIVO-ERROR
               PERFORM MANEJAR-ERROR-ARCHIVO
           END-IF.


       INICIALIZAR-VARIABLES.
           INITIALIZE PRODUCTO-TABLE REPLACING 
                                NUMERIC DATA BY HIGH-VALUES
                                ALPHANUMERIC DATA BY HIGH-VALUES.

       LEER-PROD.
           READ PROD.
           MOVE PROD-RECORD TO PRODUCTO(IX-PROD).
      *    DISPLAY PROD-DESCRIP.
        
       MANEJAR-ERROR-ARCHIVO.
           DISPLAY WS-ARCHIVO-ERROR.
           PERFORM FIN.

       FIN.
           CLOSE PROD.
           CLOSE SOL1.
           CLOSE SOL2.
           CLOSE SOL3.
           CLOSE MAE.
           STOP RUN.
