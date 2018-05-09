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
           SELECT MAE-AC
               ASSIGN TO 'MAE-AC.TXT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS MAE-AC-FS.
           SELECT REP1
               ASSIGN TO 'REP1.TXT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS REP1-FS.

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
           FD MAE-AC.
           01 MAE-AC-RECORD.
               03 MAE-AC-CLAVE.
                   05 MAE-AC-SUBCLAVE.
                       07 MAE-AC-COD-SOL            PIC 9(6).
                       07 MAE-AC-FECHA.
                           10 MAE-AC-FECHA-AAAA     PIC X(4).
                           10 FILLER              PIC X(1).
                           10 MAE-AC-FECHA-MM       PIC X(2).
                           10 FILLER              PIC X(1).
                           10 MAE-AC-FECHA-DD       PIC X(2).
                   05 MAE-AC-COD-PROD           PIC 9(4).
               03 MAE-AC-CANTIDAD           PIC 9(4).
               03 MAE-AC-COD-VENDEDOR       PIC 9(3).
               03 MAE-AC-IMPORTE            PIC 9(7)V99.
           FD REP1.
           01 REP1-RECORD.
               03 REP1-LINEA                PIC X(80).

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
           01 MAE-FS                      PIC 9(2).
               88 MAE-OK                  VALUE '00'.
               88 MAE-EOF                 VALUE '10'.
           01 MAE-AC-FS                   PIC 9(2).
               88 MAE-AC-OK               VALUE '00'.
           01 REP1-FS                     PIC 9(2).
               88 REP1-OK                 VALUE '00'.

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
               03 REP1-NRO-LINEA                  PIC 9(2) VALUE 1.

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

           01 MENOR.
               03 MENOR-CLAVE.
                   05 MENOR-SUBCLAVE.
                       07 MENOR-COD-SOL                 PIC 9(6).
                       07 MENOR-FECHA.
                           10 MENOR-FECHA-AAAA          PIC X(4).
                           10 FILLER                      PIC X(1).
                           10 MENOR-FECHA-MM            PIC X(2).
                           10 FILLER                      PIC X(1).
                           10 MENOR-FECHA-DD            PIC X(2).
                   05 MENOR-COD-PROD                PIC 9(4).
               03 MENOR-CANTIDAD                    PIC 9(4).
               03 MENOR-COD-VENDEDOR                PIC 9(3).
               03 MENOR-IMPORTE                     PIC 9(7)V99.

           01 WS-MENSAJE-ERROR                        PIC X(50).

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
           OPEN OUTPUT MAE-AC.
           IF NOT MAE-AC-OK THEN
               MOVE 'ERROR ABRIENDO ARCHIVO MAE-AC.TXT' 
                        TO WS-MENSAJE-ERROR
               PERFORM MANEJAR-ERROR
           END-IF.
           OPEN OUTPUT REP1.
           IF NOT REP1-OK THEN
               MOVE 'ERROR ABRIENDO ARCHIVO REP1.TXT' 
                        TO WS-MENSAJE-ERROR
               PERFORM MANEJAR-ERROR
           END-IF.

       INICIALIZAR-VARIABLES.
           INITIALIZE PRODUCTO-TABLE REPLACING 
                                NUMERIC DATA BY HIGH-VALUES
                                ALPHANUMERIC DATA BY HIGH-VALUES.
           MOVE '2018' TO REP1-HEADER1-FEC-A.
           MOVE '05' TO REP1-HEADER1-FEC-M.
           MOVE '08' TO REP1-HEADER1-FEC-D.
           
       LEER-PROD.
           READ PROD.
           IF NOT PROD-OK AND NOT PROD-EOF THEN
               MOVE 'ERROR LEYENDO ARCHIVO PROD.TXT' 
                        TO WS-MENSAJE-ERROR
               PERFORM MANEJAR-ERROR
           END-IF.

       CARGAR-PROD.
           MOVE PROD-RECORD TO PRODUCTO(IX-PROD).
           PERFORM LEER-PROD.

       PROCESO.
           PERFORM IMPRIMIR-REP1-HEADER.
           PERFORM LEER-SOL1.
           PERFORM LEER-SOL2.
           PERFORM LEER-SOL3.
           PERFORM LEER-MAE.
           PERFORM CICLO-PRINCIPAL UNTIL SOL1-EOF 
                                   AND   SOL2-EOF 
                                   AND   SOL3-EOF 
                                   AND   MAE-EOF.
 
       CICLO-PRINCIPAL.
           PERFORM DETERMINAR-MENOR-SUBCLAVE.
           MOVE ZEROES TO REP1-ACUM-IMPORTE.
           PERFORM PROCESAR-REGISTROS-SUBCLAVE 
                              UNTIL SOL1-SUBCLAVE <> REP1-ACUM-SUBCLAVE
                              AND   SOL2-SUBCLAVE <> REP1-ACUM-SUBCLAVE
                              AND   SOL3-SUBCLAVE <> REP1-ACUM-SUBCLAVE
                              AND   MAE-SUBCLAVE <> REP1-ACUM-SUBCLAVE.
           PERFORM IMPRIMIR-REP1-ITEM.
        
       DETERMINAR-MENOR-SUBCLAVE.
           MOVE SOL1-SUBCLAVE TO REP1-ACUM-SUBCLAVE.
           IF SOL2-SUBCLAVE < REP1-ACUM-SUBCLAVE THEN
               MOVE SOL2-SUBCLAVE TO REP1-ACUM-SUBCLAVE
           END-IF.
           IF SOL3-SUBCLAVE < REP1-ACUM-SUBCLAVE THEN
               MOVE SOL3-SUBCLAVE TO REP1-ACUM-SUBCLAVE
           END-IF.
           IF MAE-SUBCLAVE < REP1-ACUM-SUBCLAVE THEN
               MOVE MAE-SUBCLAVE TO REP1-ACUM-SUBCLAVE
           END-IF.
    
       PROCESAR-REGISTROS-SUBCLAVE.
           PERFORM DETERMINAR-MENOR-CLAVE.
           PERFORM PROCESAR-REGISTROS-MAE 
                                    UNTIL MAE-EOF
                                    OR    MAE-CLAVE <> MENOR-CLAVE.
           PERFORM PROCESAR-REGISTROS-SOL1 
                                    UNTIL SOL1-EOF
                                    OR    SOL1-CLAVE <> MENOR-CLAVE.
           PERFORM PROCESAR-REGISTROS-SOL2 
                                    UNTIL SOL2-EOF
                                    OR    SOL2-CLAVE <> MENOR-CLAVE.
           PERFORM PROCESAR-REGISTROS-SOL3 
                                    UNTIL SOL3-EOF
                                    OR    SOL3-CLAVE <> MENOR-CLAVE.

       DETERMINAR-MENOR-CLAVE.
           MOVE SOL1-CLAVE TO MENOR-CLAVE.
           IF SOL2-CLAVE < MENOR-CLAVE THEN
               MOVE SOL2-CLAVE TO MENOR-CLAVE
           END-IF.
           IF SOL3-CLAVE < MENOR-CLAVE THEN
               MOVE SOL3-CLAVE TO MENOR-CLAVE
           END-IF.
           IF MAE-CLAVE < MENOR-CLAVE THEN
               MOVE MAE-CLAVE TO MENOR-CLAVE
           END-IF.
        
       PROCESAR-REGISTROS-MAE.
           ADD MAE-IMPORTE TO REP1-ACUM-IMPORTE.
           MOVE MAE-RECORD TO MAE-AC-RECORD.
           PERFORM GRABAR-MAE-AC.
           PERFORM LEER-MAE.

       PROCESAR-REGISTROS-SOL1.
           ADD SOL1-IMPORTE TO REP1-ACUM-IMPORTE.
           MOVE SOL1-RECORD TO MAE-AC-RECORD.
           PERFORM GRABAR-MAE-AC.
           PERFORM LEER-SOL1.
           
       PROCESAR-REGISTROS-SOL2.
           ADD SOL2-IMPORTE TO REP1-ACUM-IMPORTE.
           MOVE SOL2-RECORD TO MAE-AC-RECORD.
           PERFORM GRABAR-MAE-AC.
           PERFORM LEER-SOL2.
           
       PROCESAR-REGISTROS-SOL3.
           ADD SOL3-IMPORTE TO REP1-ACUM-IMPORTE.
           MOVE SOL3-RECORD TO MAE-AC-RECORD.
           PERFORM GRABAR-MAE-AC.
           PERFORM LEER-SOL3.

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
    
       GRABAR-MAE-AC.
           WRITE MAE-AC-RECORD.
           IF NOT MAE-AC-OK THEN
               MOVE 'ERROR ESCRIBIENDO ARCHIVO MAE-AC.TXT'
                    TO WS-MENSAJE-ERROR
               PERFORM MANEJAR-ERROR
           END-IF.

       IMPRIMIR-REP1-HEADER.
           ADD 1 TO REP1-HEADER1-HOJA.
           MOVE REP1-HEADER1 TO REP1-LINEA.
           PERFORM IMPRIMIR-REP1-LINEA.
           MOVE REP1-HEADER2 TO REP1-LINEA.
           PERFORM IMPRIMIR-REP1-LINEA.

       IMPRIMIR-REP1-ITEM.
           PERFORM IMPRIMIR-REP1-BLANCO.
           PERFORM IMPRIMIR-REP1-SOLICITUD.
           PERFORM IMPRIMIR-REP1-FECHA.
           PERFORM IMPRIMIR-REP1-IMPORTE.

       IMPRIMIR-REP1-BLANCO.
           MOVE REP1-BLANCO TO REP1-LINEA.
           PERFORM IMPRIMIR-REP1-LINEA.

       IMPRIMIR-REP1-SOLICITUD.
           MOVE REP1-ACUM-COD-SOL TO REP1-SOLICITUD-COD.
           MOVE REP1-SOLICITUD TO REP1-LINEA.
           PERFORM IMPRIMIR-REP1-LINEA.

       IMPRIMIR-REP1-FECHA.
           MOVE REP1-ACUM-FECHA-DD TO REP1-FECHA-DD.
           MOVE REP1-ACUM-FECHA-MM TO REP1-FECHA-MM.
           MOVE REP1-ACUM-FECHA-AAAA(3:2) TO REP1-FECHA-AA.
           MOVE REP1-FECHA TO REP1-LINEA.
           PERFORM IMPRIMIR-REP1-LINEA.

       IMPRIMIR-REP1-IMPORTE.
           MOVE REP1-ACUM-IMPORTE TO REP1-IMPORTE-IMPORTE.
           MOVE REP1-IMPORTE TO REP1-LINEA.
           PERFORM IMPRIMIR-REP1-LINEA.
    
       IMPRIMIR-REP1-LINEA.
           WRITE REP1-RECORD.
           ADD 1 TO REP1-NRO-LINEA.
           IF REP1-NRO-LINEA > REPORTE1-MAX-LINEAS THEN
               MOVE 1 TO REP1-NRO-LINEA
               PERFORM IMPRIMIR-REP1-HEADER
           END-IF.

       MANEJAR-ERROR.
           DISPLAY WS-MENSAJE-ERROR.
           PERFORM FIN.

       FIN.
           CLOSE PROD.
           CLOSE SOL1.
           CLOSE SOL2.
           CLOSE SOL3.
           CLOSE MAE.
           CLOSE MAE-AC.
           CLOSE REP1.
           STOP RUN.
