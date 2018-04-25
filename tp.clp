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
               03 SOL1-CLAVE.
                   05 SOL1-COD-SOL            PIC 9(6).
                   05 SOL1-FECHA.
                       07 SOL1-FECHA-AAAA     PIC X(4).
                       07 FILLER              PIC X(1).
                       07 SOL1-FECHA-MM       PIC X(2).
                       07 FILLER              PIC X(1).
                       07 SOL1-FECHA-DD       PIC X(2).
                   05 SOL1-COD-PROD           PIC 9(4).
               03 SOL1-CANTIDAD           PIC 9(4).
               03 SOL1-COD-VENDEDOR       PIC 9(3).
               03 SOL1-IMPORTE            PIC 9(7)V99.
           FD SOL2.
           01 SOL2-RECORD.
               03 SOL2-CLAVE.
                   05 SOL2-COD-SOL            PIC 9(6).
                   05 SOL2-FECHA.
                       07 SOL2-FECHA-AAAA     PIC X(4).
                       07 FILLER              PIC X(1).
                       07 SOL2-FECHA-MM       PIC X(2).
                       07 FILLER              PIC X(1).
                       07 SOL2-FECHA-DD       PIC X(2).
                   05 SOL2-COD-PROD           PIC 9(4).
               03 SOL2-CANTIDAD           PIC 9(4).
               03 SOL2-COD-VENDEDOR       PIC 9(3).
               03 SOL2-IMPORTE            PIC 9(7)V99.
           FD SOL3.
           01 SOL3-RECORD.
               03 SOL3-CLAVE.
                   05 SOL3-COD-SOL            PIC 9(6).
                   05 SOL3-FECHA.
                       07 SOL3-FECHA-AAAA     PIC X(4).
                       07 FILLER              PIC X(1).
                       07 SOL3-FECHA-MM       PIC X(2).
                       07 FILLER              PIC X(1).
                       07 SOL3-FECHA-DD       PIC X(2).
                   05 SOL3-COD-PROD           PIC 9(4).
               03 SOL3-CANTIDAD           PIC 9(4).
               03 SOL3-COD-VENDEDOR       PIC 9(3).
               03 SOL3-IMPORTE            PIC 9(7)V99.
           FD MAE.
           01 MAE-RECORD.
               03 MAE-CLAVE.
                   05 MAE-COD-SOL            PIC 9(6).
                   05 MAE-FECHA.
                       07 MAE-FECHA-AAAA     PIC X(4).
                       07 FILLER              PIC X(1).
                       07 MAE-FECHA-MM       PIC X(2).
                       07 FILLER              PIC X(1).
                       07 MAE-FECHA-DD       PIC X(2).
                   05 MAE-COD-PROD           PIC 9(4).
               03 MAE-CANTIDAD           PIC 9(4).
               03 MAE-COD-VENDEDOR       PIC 9(3).
               03 MAE-IMPORTE            PIC 9(7)V99.

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
           01 WS-MENSAJE-ERROR            PIC X(50).

       PROCEDURE DIVISION.
       PRINCIPAL.
           PERFORM INICIO.
           PERFORM PROCESO.
           PERFORM FIN.
       
       RECORRER-TABLA.
           DISPLAY PRODUCTO(IX-PROD).

       INICIO.
           PERFORM INICIALIZAR-VARIABLES.
           PERFORM ABRIR-ARCHIVOS.
           PERFORM LEER-PROD.
           PERFORM CARGAR-PROD VARYING IX-PROD FROM 1 BY 1 
                  UNTIL PROD-EOF OR IX-PROD > 2000.

       ABRIR-ARCHIVOS.
           OPEN INPUT PROD.
           IF NOT PROD-OK THEN
               MOVE 'ERROR ABRIENDO ARCHIVO PROD.TXT' 
                        TO WS-MENSAJE-ERROR
               PERFORM MANEJAR-ERROR
           END-IF.
           OPEN INPUT SOL1.
           IF NOT SOL1-OK THEN
               MOVE 'ERROR ABRIENDO ARCHIVO SOL1.TXT' 
                        TO WS-MENSAJE-ERROR
               PERFORM MANEJAR-ERROR
           END-IF.
           OPEN INPUT SOL2.
           IF NOT SOL2-OK THEN
               MOVE 'ERROR ABRIENDO ARCHIVO SOL2.TXT' 
                        TO WS-MENSAJE-ERROR
               PERFORM MANEJAR-ERROR
           END-IF.
           OPEN INPUT SOL3.
           IF NOT SOL3-OK THEN
               MOVE 'ERROR ABRIENDO ARCHIVO SOL3.TXT' 
                        TO WS-MENSAJE-ERROR
               PERFORM MANEJAR-ERROR
           END-IF.
           OPEN INPUT MAE.
           IF NOT MAE-OK THEN
               MOVE 'ERROR ABRIENDO ARCHIVO MAE.TXT' 
                        TO WS-MENSAJE-ERROR
               PERFORM MANEJAR-ERROR
           END-IF.


       INICIALIZAR-VARIABLES.
           INITIALIZE PRODUCTO-TABLE REPLACING 
                                NUMERIC DATA BY HIGH-VALUES
                                ALPHANUMERIC DATA BY HIGH-VALUES.

       LEER-PROD.
           READ PROD.
           IF NOT PROD-OK AND NOT PROD-EOF THEN
               MOVE 'ERROR LEYENDO ARCHIVO PROD.TXT' 
                        TO WS-MENSAJE-ERROR
               PERFORM MANEJAR-ERROR
           END-IF.

       LEER-SOL1.
           READ SOL1.
           EVALUATE TRUE
               WHEN SOL1-OK
                   CONTINUE
               WHEN SOL1-EOF
                   MOVE HIGH-VALUES TO SOL1-CLAVE
               WHEN OTHER
                   MOVE 'ERROR LEYENDO ARCHIVO SOL1.TXT' 
                        TO WS-MENSAJE-ERROR
                   PERFORM MANEJAR-ERROR
           END-EVALUATE.

       LEER-SOL2.
           READ SOL2.
           EVALUATE TRUE
               WHEN SOL2-OK
                   CONTINUE
               WHEN SOL2-EOF
                   MOVE HIGH-VALUES TO SOL2-CLAVE
               WHEN OTHER
                   MOVE 'ERROR LEYENDO ARCHIVO SOL2.TXT' 
                        TO WS-MENSAJE-ERROR
                   PERFORM MANEJAR-ERROR
           END-EVALUATE.

       LEER-SOL3.
           READ SOL3.
           EVALUATE TRUE
               WHEN SOL3-OK
                   CONTINUE
               WHEN SOL3-EOF
                   MOVE HIGH-VALUES TO SOL3-CLAVE
               WHEN OTHER
                   MOVE 'ERROR LEYENDO ARCHIVO SOL3.TXT' 
                        TO WS-MENSAJE-ERROR
                   PERFORM MANEJAR-ERROR
           END-EVALUATE.

       LEER-MAE.
           READ MAE.
           EVALUATE TRUE
               WHEN MAE-OK
                   CONTINUE
               WHEN MAE-EOF
                   MOVE HIGH-VALUES TO MAE-CLAVE
               WHEN OTHER
                   MOVE 'ERROR LEYENDO ARCHIVO MAE.TXT' 
                        TO WS-MENSAJE-ERROR
                   PERFORM MANEJAR-ERROR
           END-EVALUATE.

       CARGAR-PROD.
           MOVE PROD-RECORD TO PRODUCTO(IX-PROD).
           PERFORM LEER-PROD.

       PROCESO.
           PERFORM LEER-SOL1.
           PERFORM LEER-SOL2.
           PERFORM LEER-SOL3.
           PERFORM LEER-MAE.
           PERFORM CICLO-PRINCIPAL UNTIL SOL1-EOF 
                                   AND   SOL2-EOF 
                                   AND   SOL3-EOF 
                                   AND   MAE-EOF.

       CICLO-PRINCIPAL.
           EVALUATE TRUE ALSO TRUE ALSO TRUE
               WHEN         SOL1-CLAVE <= SOL2-CLAVE 
                       ALSO SOL1-CLAVE <= SOL3-CLAVE 
                       ALSO SOL1-CLAVE <= MAE-CLAVE
                   DISPLAY 'SOL1 ' SOL1-CLAVE
                   PERFORM LEER-SOL1
               WHEN         SOL2-CLAVE <= SOL1-CLAVE 
                       ALSO SOL2-CLAVE <= SOL3-CLAVE 
                       ALSO SOL2-CLAVE <= MAE-CLAVE
                   DISPLAY 'SOL2 ' SOL2-CLAVE
                   PERFORM LEER-SOL2
               WHEN         SOL3-CLAVE <= SOL1-CLAVE 
                       ALSO SOL3-CLAVE <= SOL2-CLAVE 
                       ALSO SOL3-CLAVE <= MAE-CLAVE
                   DISPLAY 'SOL3 ' SOL3-CLAVE
                   PERFORM LEER-SOL3
               WHEN         MAE-CLAVE <= SOL1-CLAVE 
                       ALSO MAE-CLAVE <= SOL2-CLAVE 
                       ALSO MAE-CLAVE <= SOL3-CLAVE
                   DISPLAY 'MAE  ' MAE-CLAVE
                   PERFORM LEER-MAE
               WHEN OTHER
                   DISPLAY 'SOL1-CLAVE' SOL1-CLAVE
                   DISPLAY 'SOL2-CLAVE' SOL2-CLAVE
                   DISPLAY 'SOL3-CLAVE' SOL3-CLAVE
                   DISPLAY 'MAE-CLAVE' MAE-CLAVE
                   MOVE 'ERROR NO CONTEMPLADO PROCESANDO ARCHIVOS' 
                                            TO WS-MENSAJE-ERROR
                   PERFORM MANEJAR-ERROR
           END-EVALUATE.
        
       MANEJAR-ERROR.
           DISPLAY WS-MENSAJE-ERROR.
           PERFORM FIN.

       FIN.
           CLOSE PROD.
           CLOSE SOL1.
           CLOSE SOL2.
           CLOSE SOL3.
           CLOSE MAE.
           STOP RUN.
