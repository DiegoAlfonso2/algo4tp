       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TRABAJO-PRACTICO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
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
                   05 SOL1-SUBCLAVE.
                       07 SOL1-COD-SOL            PIC 9(6).
                       07 SOL1-FECHA.
                           10 SOL1-FECHA-AAAA     PIC X(4).
                           10 FILLER              PIC X(1).
                           10 SOL1-FECHA-MM       PIC X(2).
                           10 FILLER              PIC X(1).
                           10 SOL1-FECHA-DD       PIC X(2).
                   05 SOL1-COD-PROD           PIC 9(4).
               03 SOL1-CANTIDAD           PIC 9(4).
               03 SOL1-COD-VENDEDOR       PIC 9(3).
               03 SOL1-IMPORTE            PIC 9(7)V99.
           FD SOL2.
           01 SOL2-RECORD.
               03 SOL2-CLAVE.
                   05 SOL2-SUBCLAVE.
                       07 SOL2-COD-SOL            PIC 9(6).
                       07 SOL2-FECHA.
                           10 SOL2-FECHA-AAAA     PIC X(4).
                           10 FILLER              PIC X(1).
                           10 SOL2-FECHA-MM       PIC X(2).
                           10 FILLER              PIC X(1).
                           10 SOL2-FECHA-DD       PIC X(2).
                   05 SOL2-COD-PROD           PIC 9(4).
               03 SOL2-CANTIDAD           PIC 9(4).
               03 SOL2-COD-VENDEDOR       PIC 9(3).
               03 SOL2-IMPORTE            PIC 9(7)V99.
           FD SOL3.
           01 SOL3-RECORD.
               03 SOL3-CLAVE.
                   05 SOL3-SUBCLAVE.
                       07 SOL3-COD-SOL            PIC 9(6).
                       07 SOL3-FECHA.
                           10 SOL3-FECHA-AAAA     PIC X(4).
                           10 FILLER              PIC X(1).
                           10 SOL3-FECHA-MM       PIC X(2).
                           10 FILLER              PIC X(1).
                           10 SOL3-FECHA-DD       PIC X(2).
                   05 SOL3-COD-PROD           PIC 9(4).
               03 SOL3-CANTIDAD           PIC 9(4).
               03 SOL3-COD-VENDEDOR       PIC 9(3).
               03 SOL3-IMPORTE            PIC 9(7)V99.
           FD MAE.
           01 MAE-RECORD.
               03 MAE-CLAVE.
                   05 MAE-SUBCLAVE.
                       07 MAE-COD-SOL            PIC 9(6).
                       07 MAE-FECHA.
                           10 MAE-FECHA-AAAA     PIC X(4).
                           10 FILLER              PIC X(1).
                           10 MAE-FECHA-MM       PIC X(2).
                           10 FILLER              PIC X(1).
                           10 MAE-FECHA-DD       PIC X(2).
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

      * REPORTE 1
           01 REP1-LINEAS.
               03 REP1-HEADER1.
                   05 FILLER                      VALUE 'Fecha: '.
                   05 REP1-HEADER1-FECHA.
                       07 REP1-HEADER1-FEC-A      PIC X(4).
                       07 FILLER                  VALUE '/'.
                       07 REP1-HEADER1-FEC-M      PIC X(2).
                       07 FILLER                  VALUE '/'.
                       07 REP1-HEADER1-FEC-D      PIC X(2).
                   05 FILLER                    PIC X(52) VALUE SPACES.
                   05 FILLER                      VALUE 'Hoja nro '.
                   05 REP1-HEADER1-HOJA           PIC 99. 
               03 REP1-HEADER2.
                   05 FILLER                    PIC X(30) VALUE SPACES.
                   05 FILLER                    VALUE 
                                                'LISTADO DE IMPORTES'.
                   05 FILLER                    PIC X(31) VALUE SPACES.
               03 REP1-BLANCO.
                   05 FILLER                    PIC X(80) VALUE SPACES.
               03 REP1-SOLICITUD.
                   05 FILLER                    VALUE 'COD.SOLICITUD: '.
                   05 REP1-SOLICITUD-COD        PIC 9(6).
                   05 FILLER                    PIC X(59) VALUE SPACES.
               03 REP1-FECHA.
                   05 FILLER                    VALUE 'FECHA: '.
                   05 REP1-FECHA-FECHA.
                       07 REP1-FECHA-DD         PIC X(2).
                       07 FILLER                VALUE '/'.
                       07 REP1-FECHA-MM         PIC X(2).
                       07 FILLER                VALUE '/'.
                       07 REP1-FECHA-AA         PIC X(2).
                   05 FILLER                    PIC X(65) VALUE SPACES.
               03 REP1-IMPORTE.
                   05 FILLER                    VALUE 'IMPORTE TOTAL: '.
                   05 REP1-IMPORTE-IMPORTE      PIC $ZZ.ZZ9,99.
                   05 FILLER                    PIC X(55) VALUE SPACES.

           01 REP1-CONTROL.
               03 REP1-NRO-LINEA                  PIC X(2).
           01 REP1-ACUM.
               03 REP1-ACUM-SUBCLAVE.
                   07 REP1-ACUM-COD-SOL               PIC 9(6).
                   07 REP1-ACUM-FECHA.
                       10 REP1-ACUM-FECHA-AAAA        PIC X(4).
                       10 FILLER                      PIC X(1).
                       10 REP1-ACUM-FECHA-MM          PIC X(2).
                       10 FILLER                      PIC X(1).
                       10 REP1-ACUM-FECHA-DD          PIC X(2).
               03 REP1-ACUM-IMPORTE                   PIC 9(7)V99.


      * OTRAS VARIABLES
           01 WS-MENSAJE-ERROR                        PIC X(50).
           01 REG-ACT.
               03 REG-ACT-CLAVE.
                   05 REG-ACT-SUBCLAVE.
                       07 REG-ACT-COD-SOL                 PIC 9(6).
                       07 REG-ACT-FECHA.
                           10 REG-ACT-FECHA-AAAA          PIC X(4).
                           10 FILLER                      PIC X(1).
                           10 REG-ACT-FECHA-MM            PIC X(2).
                           10 FILLER                      PIC X(1).
                           10 REG-ACT-FECHA-DD            PIC X(2).
                   05 REG-ACT-COD-PROD                PIC 9(4).
               03 REG-ACT-CANTIDAD                    PIC 9(4).
               03 REG-ACT-COD-VENDEDOR                PIC 9(3).
               03 REG-ACT-IMPORTE                     PIC 9(7)V99.

      * CONSTANTES
           77 REPORTE1-MAX-LINEAS         PIC 9(2) VALUE 60.

       PROCEDURE DIVISION.
       PRINCIPAL.
           PERFORM INICIO.
           PERFORM PROCESO.
           PERFORM FIN.

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
           MOVE SOL1-SUBCLAVE TO REP1-ACUM-SUBCLAVE.
           PERFORM LEER-SOL2.
           IF SOL2-SUBCLAVE < REP1-ACUM-SUBCLAVE THEN
               MOVE SOL2-SUBCLAVE TO REP1-ACUM-SUBCLAVE
           END-IF.
           PERFORM LEER-SOL3.
           IF SOL3-SUBCLAVE < REP1-ACUM-SUBCLAVE THEN
               MOVE SOL3-SUBCLAVE TO REP1-ACUM-SUBCLAVE
           END-IF.
           PERFORM LEER-MAE.
           IF MAE-SUBCLAVE < REP1-ACUM-SUBCLAVE THEN
               MOVE MAE-SUBCLAVE TO REP1-ACUM-SUBCLAVE
           END-IF.
      * TODO: TOMAR LA FECHA DE UN ACCEPT O DEL SISTEMA     
           MOVE '2018' TO REP1-HEADER1-FEC-A.
           MOVE '05' TO REP1-HEADER1-FEC-M.
           MOVE '08' TO REP1-HEADER1-FEC-D.
           PERFORM IMPRIMIR-REP1-HEADER.
           PERFORM CICLO-PRINCIPAL UNTIL SOL1-EOF 
                                   AND   SOL2-EOF 
                                   AND   SOL3-EOF 
                                   AND   MAE-EOF.
           IF REP1-ACUM-SUBCLAVE <> HIGH-VALUES THEN
               PERFORM IMPRIMIR-REP1-ITEM
           END-IF.

       CICLO-PRINCIPAL.
           EVALUATE TRUE ALSO TRUE ALSO TRUE
               WHEN         SOL1-CLAVE <= SOL2-CLAVE 
                       ALSO SOL1-CLAVE <= SOL3-CLAVE 
                       ALSO SOL1-CLAVE <= MAE-CLAVE
                   MOVE SOL1-RECORD TO REG-ACT
                   PERFORM LEER-SOL1
               WHEN         SOL2-CLAVE <= SOL1-CLAVE 
                       ALSO SOL2-CLAVE <= SOL3-CLAVE 
                       ALSO SOL2-CLAVE <= MAE-CLAVE
                   MOVE SOL2-RECORD TO REG-ACT
                   PERFORM LEER-SOL2
               WHEN         SOL3-CLAVE <= SOL1-CLAVE 
                       ALSO SOL3-CLAVE <= SOL2-CLAVE 
                       ALSO SOL3-CLAVE <= MAE-CLAVE
                   MOVE SOL3-RECORD TO REG-ACT
                   PERFORM LEER-SOL3
               WHEN         MAE-CLAVE <= SOL1-CLAVE 
                       ALSO MAE-CLAVE <= SOL2-CLAVE 
                       ALSO MAE-CLAVE <= SOL3-CLAVE
                   MOVE MAE-RECORD TO REG-ACT
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
      * TODO: ESCRIBIR A ARCHIVO
      *     DISPLAY REG-ACT.
           IF REG-ACT-SUBCLAVE <> REP1-ACUM-SUBCLAVE THEN
               PERFORM IMPRIMIR-REP1-ITEM
               MOVE REG-ACT-SUBCLAVE TO REP1-ACUM-SUBCLAVE
               MOVE ZEROES TO REP1-ACUM-IMPORTE
           END-IF.
           ADD REG-ACT-IMPORTE TO REP1-ACUM-IMPORTE.
           

       IMPRIMIR-REP1-HEADER.
           DISPLAY REP1-HEADER1.
           DISPLAY REP1-HEADER2.
           DISPLAY REP1-BLANCO.
           MOVE 4 TO REP1-NRO-LINEA.

       IMPRIMIR-REP1-ITEM.
           PERFORM IMPRIMIR-REP1-SOLICITUD.
           PERFORM IMPRIMIR-REP1-FECHA.
           PERFORM IMPRIMIR-REP1-IMPORTE.
           DISPLAY REP1-BLANCO.

       IMPRIMIR-REP1-SOLICITUD.
           MOVE REP1-ACUM-COD-SOL TO REP1-SOLICITUD-COD.
           DISPLAY REP1-SOLICITUD.

       IMPRIMIR-REP1-FECHA.
           MOVE REP1-ACUM-FECHA-DD TO REP1-FECHA-DD.
           MOVE REP1-ACUM-FECHA-MM TO REP1-FECHA-MM.
           MOVE REP1-ACUM-FECHA-AAAA(3:2) TO REP1-FECHA-AA.
           DISPLAY REP1-FECHA.

       IMPRIMIR-REP1-IMPORTE.
           MOVE REP1-ACUM-IMPORTE TO REP1-IMPORTE-IMPORTE.
           DISPLAY REP1-IMPORTE.
        
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
