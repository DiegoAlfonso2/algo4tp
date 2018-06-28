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
            SELECT VEN
               ASSIGN TO 'VENDEDORES.DAT'
               ORGANIZATION IS INDEXED
               ACCESS IS RANDOM
               RECORD KEY IS VEN-COD-VEND
               FILE STATUS IS VEN-FS.
            SELECT REPA
               ASSIGN TO 'REPORTEA.TXT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS REPA-FS.
            SELECT REPNOA
               ASSIGN TO 'REPORTEOTROS.TXT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS REPNOA-FS.

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
           FD VEN.
           01  VEN-RECORD.
               05 VEN-COD-VEND           PIC 9(3).
               05 VEN-FECHA-INGR         PIC 9(8).
               05 VEN-DIRECCION          PIC X(20).
               05 VEN-APEYNOM            PIC X(25).
               05 VEN-TELEFONO           PIC 9(10).
               05 VEN-CATEGORIA          PIC X.
           SD SORTFILE.
           01 SORT-RECORD.
               03 SORT-TIP-CATEG              PIC 9.
                   88 SORT-TIP-CATEG-A        VALUE '1'.
                   88 SORT-TIP-CATEG-OTRA     VALUE '2'.
               03 SORT-KEY.
                   05 SORT-APEYNOM            PIC X(25).
                   05 SORT-COD-VEND           PIC 9(3).
                   05 SORT-COD-SOL            PIC 9(6).
                   05 SORT-COD-PROD           PIC 9(4).
               03 SORT-TELEFONO               PIC 9(10).
               03 SORT-CANTIDAD               PIC 9(4).
               03 SORT-IMPORTE                PIC 9(7)V99.
           FD REPA.
           01 REPA-RECORD                     PIC X(80).
           FD REPNOA.
           01 REPNOA-RECORD                   PIC X(80).
       WORKING-STORAGE SECTION.
      * FILE STATUSES DE ARCHIVOS
           01 SOLIC-FS                     PIC 9(2).
               88 SOLIC-OK                 VALUE '00'.
               88 SOLIC-EOF                VALUE '10'.
           01 VEN-FS                       PIC 9(2).
               88 VEN-OK                   VALUE '00'.
               88 VEN-EOF                  VALUE '10'.
           01 SORT-FS                      PIC 9(2).
               88 SORT-OK                  VALUE '00'.
               88 SORT-EOF                 VALUE '10'.
           01 FIN-SORT                     PIC X.
           01 REPA-FS                      PIC 9(2).
               88 REPA-OK                  VALUE '00'.
               88 REPA-EOF                 VALUE '10'.
           01 REPNOA-FS                    PIC 9(2).
               88 REPNOA-OK                VALUE '00'.
               88 REPNOA-EOF               VALUE '10'.
           01 WS-ERROR.
               03 WS-MENSAJE-ERROR         PIC X(80).
               03 WS-COD-ERROR             PIC 9(2). 

      * INTERCAMBIO CON RUTINA EXTERNA
           01 UPD-PROD-VEN-INPUT.
               03 UPD-PV-IN-MODO           PIC X.
                   88 UPD-PV-MODO-OPEN     VALUE 'O'.
                   88 UPD-PV-MODO-CLOSE    VALUE 'C'.
                   88 UPD-PV-MODO-UPD      VALUE 'U'.
               03 UPD-PV-IN-COD-PROD       PIC 9(4).
               03 UPD-PV-IN-FECHA          PIC X(10).
               03 UPD-PV-IN-CANTIDAD       PIC 9(4).
               03 UPD-PV-IN-IMPORTE        PIC 9(7)V99.
           01 UPD-PROD-VEN-OUTPUT. 
               03 UPD-PV-OUT-RET-COD       PIC 9(2).
                   88 UPD-PV-RET-OK        VALUE '00'.

      * REPORTES
           01 REP-LINEAS.
               03 REP-HEADER1.
                   05 FILLER                      VALUE 'Fecha: '.
                   05 REP-HEADER1-FECHA.
                       07 REP-HEADER1-FEC-A      PIC X(4)
                                                  VALUE '2018'.
                       07 FILLER                  VALUE '/'.
                       07 REP-HEADER1-FEC-M      PIC X(2)
                                                  VALUE '06'.
                       07 FILLER                  VALUE '/'.
                       07 REP-HEADER1-FEC-D      PIC X(2)
                                                  VALUE '01'.
                   05 FILLER                    PIC X(52) VALUE SPACES.
                   05 FILLER                      VALUE 'Hoja nro '.
                   05 REP-HEADER1-HOJA           PIC 99. 
               03 REP-HEADERA.
                   05 FILLER                  PIC X(20) VALUE SPACES.
                   05 FILLER                  VALUE 'LISTADO DE VENTAS'.
                   05 FILLER                  VALUE ' – VENDEDORES '.
                   05 FILLER                  VALUE 'CATEG “A“'.
                   05 FILLER                  PIC X(20) VALUE SPACES.
               03 REP-HEADERNOA.
                   05 FILLER                  PIC X(20) VALUE SPACES.
                   05 FILLER                  VALUE 'LISTADO DE VENTAS'.
                   05 FILLER                  VALUE ' – VENDEDORES '.
                   05 FILLER                  VALUE 'CATEG DISTINTA DE'.
                   05 FILLER                  VALUE ' “A“'.
                   05 FILLER                  PIC X(20) VALUE SPACES.
               03 REP-BLANCO.
                   05 FILLER                  PIC X(80) VALUE SPACES.
               03 REP-SUBHEADER1.
                   05 FILLER                  VALUE 
                                               'Apellido y Nombre: '.
                   05 REP-SUBH1-APENOM        PIC X(20).
                   05 FILLER                  VALUE 
                                               '  Código Vendedor: '.
                   05 REP-SUBH1-COD-VEN       PIC 9(5).
                   05 FILLER                  VALUE '  Tel: '.
                   05 REP-SUBH1-TEL-CAR       PIC 9(4).
                   05 FILLER                  VALUE '-'.
                   05 REP-SUBH1-TEL-NUM       PIC 9(4).
               03 REP-SUBHEADER2.
                   05 FILLER                  VALUE 
                                               'Código de Solicitud: '.
                   05 REP-SUBH2-SOLIC         PIC 9(6).
                   05 FILLER                  PIC X(20) VALUE SPACES.
               03 REP-HEADER-DETALLE.
                   05 FILLER                  PIC X(6) VALUE SPACES.
                   05 FILLER                  VALUE 'Cód Prod'.
                   05 FILLER                  PIC X(16) VALUE SPACES.
                   05 FILLER                  VALUE 'Cantidad'.
                   05 FILLER                  PIC X(9) VALUE SPACES.
                   05 FILLER                  VALUE 'Cód Vend'.
                   05 FILLER                  PIC X(13) VALUE SPACES.
                   05 FILLER                  VALUE 'Importe'.
                   05 FILLER                  PIC X(5) VALUE SPACES.
               03 REP-LINEA-HORIZ             PIC X(80) VALUE ALL '-'.
               03 REP-DETALLE.
                   05 FILLER                  PIC X(7) VALUE SPACES.
                   05 REP-DETALLE-COD-PROD    PIC 9(6).
                   05 FILLER                  PIC X(19) VALUE SPACES.
                   05 REP-DETALLE-CANTIDAD    PIC 9(4).
                   05 FILLER                  PIC X(13) VALUE SPACES.
                   05 REP-DETALLE-COD-VEND    PIC 9(4).
                   05 FILLER                  PIC X(14) VALUE SPACES.
                   05 REP-DETALLE-IMPORTE     PIC $ZZ.ZZ9,99.
                   05 FILLER                  PIC X(3) VALUE SPACES.


           01 REPA-CTL.
               03 REPA-CTL-NRO-LINEA          PIC 9(2) VALUE 1.
               03 REPA-MAX-LINEAS             VALUE '60'.
               03 REPA-COD-VEND-ACTUAL        PIC 9(3).
               03 REPA-COD-SOLIC-ACTUAL       PIC 9(6).
           01 REPNOA-CTL.
               03 REPNOA-CTL-NRO-LINEA        PIC 9(2) VALUE 1.
               03 REPNOA-MAX-LINEAS           VALUE '60'.
               03 REPNOA-COD-VEND-ACTUAL      PIC 9(3).
               03 REPNOA-COD-SOLIC-ACTUAL     PIC 9(6).

       PROCEDURE DIVISION.
       PRINCIPAL.
           SORT SORTFILE
               ON ASCENDING KEY SORT-TIP-CATEG
               ON ASCENDING KEY SORT-KEY
               INPUT PROCEDURE IS ENTRADA
               OUTPUT PROCEDURE IS SALIDA.
           STOP RUN.

       MANEJAR-ERROR.
           DISPLAY WS-MENSAJE-ERROR.
           DISPLAY 'COD. RETORNO: ' WS-COD-ERROR.
           CLOSE VEN.
           CLOSE SOLIC.
           SET UPD-PV-MODO-CLOSE TO TRUE.
           CALL 'UPD-PROD-VEND' USING UPD-PROD-VEN-INPUT,
                                      UPD-PROD-VEN-OUTPUT.
           STOP RUN.

       ENTRADA SECTION.
           PERFORM INICIO.
           PERFORM PROCESO.
           PERFORM FIN.
    
       RUTINAS-ENTRADA SECTION.
       INICIO.
           OPEN INPUT SOLIC.
           OPEN INPUT VEN.
           SET UPD-PV-MODO-OPEN TO TRUE.
           CALL 'UPD-PROD-VEND' USING UPD-PROD-VEN-INPUT,
                                      UPD-PROD-VEN-OUTPUT.

       PROCESO.
           PERFORM LEER-SOLIC.
           PERFORM PROCESAR-SOLIC UNTIL SOLIC-EOF.

       LEER-SOLIC.
           READ SOLIC RECORD.
           IF NOT SOLIC-OK AND NOT SOLIC-EOF THEN
               MOVE 'ERROR LEYENDO ARCHIVO SOLIC.DAT: ' 
                   TO WS-MENSAJE-ERROR
               MOVE SOLIC-FS TO WS-COD-ERROR
               PERFORM MANEJAR-ERROR
           END-IF.

       PROCESAR-SOLIC.
           MOVE SOLIC-COD-VEND TO VEN-COD-VEND.
           PERFORM BUSCAR-DATOS-VENDEDOR.
           IF VEN-CATEGORIA = 'A' THEN
               PERFORM ACTUALIZAR-PRODUCTO
           END-IF.
           PERFORM ARMAR-SORT-RECORD.
           RELEASE SORT-RECORD.
           PERFORM LEER-SOLIC.

       BUSCAR-DATOS-VENDEDOR.
           READ VEN RECORD
               INVALID KEY
                   DISPLAY 'VENDEDOR INEXISTENTE: ' VEN-COD-VEND
                   MOVE 'VENDEDOR INEXISTENTE EN VENDEDORES.DAT'
                       TO WS-MENSAJE-ERROR
                   MOVE VEN-FS TO WS-COD-ERROR
                   PERFORM MANEJAR-ERROR.
           IF NOT VEN-OK THEN
                   MOVE 'ERROR LEYENDO ARCHIVO VENDEDORES.DAT'
                       TO WS-MENSAJE-ERROR
                   MOVE VEN-FS TO WS-COD-ERROR
                   PERFORM MANEJAR-ERROR
           END-IF.

       ARMAR-SORT-RECORD.
           EVALUATE VEN-CATEGORIA
               WHEN "A"
                   SET SORT-TIP-CATEG-A TO TRUE
               WHEN OTHER 
                   SET SORT-TIP-CATEG-OTRA TO TRUE
           END-EVALUATE.
           MOVE VEN-APEYNOM TO SORT-APEYNOM.
           MOVE SOLIC-COD-SOL TO SORT-COD-SOL.
           MOVE SOLIC-COD-PROD TO SORT-COD-PROD.
           MOVE SOLIC-COD-VEND TO SORT-COD-VEND.
           MOVE VEN-TELEFONO TO SORT-TELEFONO.
           MOVE SOLIC-CANTIDAD TO SORT-CANTIDAD.
           MOVE SOLIC-IMPORTE TO SORT-IMPORTE.

       ACTUALIZAR-PRODUCTO.
           MOVE SOLIC-COD-PROD TO UPD-PV-IN-COD-PROD.
           MOVE SOLIC-FECHA TO UPD-PV-IN-FECHA.
           MOVE SOLIC-CANTIDAD TO UPD-PV-IN-CANTIDAD.
           MOVE SOLIC-IMPORTE TO UPD-PV-IN-IMPORTE.
           SET UPD-PV-MODO-UPD TO TRUE.
           CALL 'UPD-PROD-VEND' USING UPD-PROD-VEN-INPUT,
                                      UPD-PROD-VEN-OUTPUT.
           IF NOT UPD-PV-RET-OK THEN
               MOVE 'ERROR ACTUALIZANDO PROD-VEND' TO WS-MENSAJE-ERROR
               MOVE UPD-PV-OUT-RET-COD TO WS-COD-ERROR
               PERFORM MANEJAR-ERROR
           END-IF.

       FIN.
           CLOSE SOLIC.
           CLOSE VEN.
           SET UPD-PV-MODO-CLOSE TO TRUE.
           CALL 'UPD-PROD-VEND' USING UPD-PROD-VEN-INPUT,
                                      UPD-PROD-VEN-OUTPUT.

       SALIDA SECTION.
           PERFORM INICIALIZAR-REPA.
           PERFORM LEER-SORT.
           PERFORM PROCESAR-CAT-A UNTIL NOT SORT-TIP-CATEG-A OR
                       FIN-SORT = 'S' OR SORT-EOF.
           PERFORM INICIALIZAR-REPNOA.
           PERFORM PROCESAR-CAT-NO-A UNTIL NOT SORT-TIP-CATEG-OTRA OR
                       FIN-SORT = 'S' OR SORT-EOF.
           PERFORM FIN-REPORTES.

       RUTINAS-SALIDA SECTION.
       LEER-SORT.
           RETURN SORTFILE AT END MOVE 'S' TO FIN-SORT.
           IF NOT SORT-OK AND NOT SORT-EOF THEN
               MOVE 'ERROR LEYENDO ARCHIVO DE SORT' TO WS-MENSAJE-ERROR
               MOVE SORT-FS TO WS-COD-ERROR
           END-IF.
       
       PROCESAR-CAT-A.
           MOVE SORT-COD-VEND TO REPA-COD-VEND-ACTUAL.
           MOVE SORT-COD-SOL TO REPA-COD-SOLIC-ACTUAL.
           PERFORM IMPRIMIR-REPA-SUBHEADER.
           PERFORM PROCESAR-VEND-SOLIC-A UNTIL 
                   NOT SORT-TIP-CATEG-A 
                   OR FIN-SORT = 'S' 
                   OR SORT-EOF
                   OR SORT-COD-VEND <> REPA-COD-VEND-ACTUAL
                   OR SORT-COD-SOL <> REPA-COD-SOLIC-ACTUAL.

       PROCESAR-VEND-SOLIC-A.
           PERFORM IMPRIMIR-REPA-DETALLE.
           PERFORM LEER-SORT.

       PROCESAR-CAT-NO-A.
           MOVE SORT-COD-VEND TO REPNOA-COD-VEND-ACTUAL.
           MOVE SORT-COD-SOL TO REPNOA-COD-SOLIC-ACTUAL.
           PERFORM IMPRIMIR-REPNOA-SUBHEADER.
           PERFORM PROCESAR-VEND-SOLIC-NO-A UNTIL 
                   NOT SORT-TIP-CATEG-OTRA 
                   OR FIN-SORT = 'S' 
                   OR SORT-EOF
                   OR SORT-COD-VEND <> REPNOA-COD-VEND-ACTUAL
                   OR SORT-COD-SOL <> REPNOA-COD-SOLIC-ACTUAL.

       PROCESAR-VEND-SOLIC-NO-A.
           PERFORM IMPRIMIR-REPNOA-DETALLE.
           PERFORM LEER-SORT.

       IMPRIMIR-REPA-LINEA.
           WRITE REPA-RECORD.
           ADD 1 TO REPA-CTL-NRO-LINEA.
           IF REPA-CTL-NRO-LINEA > REPA-MAX-LINEAS THEN
               MOVE 1 TO REPA-CTL-NRO-LINEA
               PERFORM IMPRIMIR-REPA-HEADER
           END-IF.

       IMPRIMIR-REPNOA-LINEA.
           WRITE REPNOA-RECORD.
           ADD 1 TO REPNOA-CTL-NRO-LINEA.
           IF REPNOA-CTL-NRO-LINEA > REPNOA-MAX-LINEAS THEN
               MOVE 1 TO REPNOA-CTL-NRO-LINEA
               PERFORM IMPRIMIR-REPNOA-HEADER
           END-IF.

       INICIALIZAR-REPA.
           OPEN OUTPUT REPA.
           MOVE 0 TO REP-HEADER1-HOJA.
           PERFORM IMPRIMIR-REPA-HEADER.

       INICIALIZAR-REPNOA.
           OPEN OUTPUT REPNOA.
           MOVE 0 TO REP-HEADER1-HOJA.
           PERFORM IMPRIMIR-REPNOA-HEADER.

       IMPRIMIR-REPA-HEADER.
           ADD 1 TO REP-HEADER1-HOJA.
           MOVE REP-HEADER1 TO REPA-RECORD.
           PERFORM IMPRIMIR-REPA-LINEA.
           MOVE REP-HEADERA TO REPA-RECORD.
           PERFORM IMPRIMIR-REPA-LINEA.

       IMPRIMIR-REPNOA-HEADER.
           ADD 1 TO REP-HEADER1-HOJA.
           MOVE REP-HEADER1 TO REPNOA-RECORD.
           PERFORM IMPRIMIR-REPNOA-LINEA.
           MOVE REP-HEADERNOA TO REPNOA-RECORD.
           PERFORM IMPRIMIR-REPNOA-LINEA.

       IMPRIMIR-REPA-SUBHEADER.
           MOVE REP-BLANCO TO REPA-RECORD.
           PERFORM IMPRIMIR-REPA-LINEA.
           MOVE SORT-APEYNOM TO REP-SUBH1-APENOM.
           MOVE SORT-COD-VEND TO REP-SUBH1-COD-VEN.
           MOVE SORT-TELEFONO(3:4) TO REP-SUBH1-TEL-CAR.
           MOVE SORT-TELEFONO(7:4) TO REP-SUBH1-TEL-NUM.
           MOVE REP-SUBHEADER1 TO REPA-RECORD.
           PERFORM IMPRIMIR-REPA-LINEA.
           MOVE SORT-COD-SOL TO REP-SUBH2-SOLIC.
           MOVE REP-SUBHEADER2 TO REPA-RECORD.
           PERFORM IMPRIMIR-REPA-LINEA.
           MOVE REP-BLANCO TO REPA-RECORD.
           PERFORM IMPRIMIR-REPA-LINEA.
           MOVE REP-HEADER-DETALLE TO REPA-RECORD.
           PERFORM IMPRIMIR-REPA-LINEA.
           MOVE REP-LINEA-HORIZ TO REPA-RECORD.
           PERFORM IMPRIMIR-REPA-LINEA.

       IMPRIMIR-REPNOA-SUBHEADER.
           MOVE REP-BLANCO TO REPNOA-RECORD.
           PERFORM IMPRIMIR-REPNOA-LINEA.
           MOVE SORT-APEYNOM TO REP-SUBH1-APENOM.
           MOVE SORT-COD-VEND TO REP-SUBH1-COD-VEN.
           MOVE SORT-TELEFONO(3:4) TO REP-SUBH1-TEL-CAR.
           MOVE SORT-TELEFONO(7:4) TO REP-SUBH1-TEL-NUM.
           MOVE REP-SUBHEADER1 TO REPNOA-RECORD.
           PERFORM IMPRIMIR-REPNOA-LINEA.
           MOVE SORT-COD-SOL TO REP-SUBH2-SOLIC.
           MOVE REP-SUBHEADER2 TO REPNOA-RECORD.
           PERFORM IMPRIMIR-REPNOA-LINEA.
           MOVE REP-BLANCO TO REPNOA-RECORD.
           PERFORM IMPRIMIR-REPNOA-LINEA.
           MOVE REP-HEADER-DETALLE TO REPNOA-RECORD.
           PERFORM IMPRIMIR-REPNOA-LINEA.
           MOVE REP-LINEA-HORIZ TO REPNOA-RECORD.
           PERFORM IMPRIMIR-REPNOA-LINEA.

       IMPRIMIR-REPA-DETALLE.
           MOVE SORT-COD-PROD TO REP-DETALLE-COD-PROD.
           MOVE SORT-CANTIDAD TO REP-DETALLE-CANTIDAD.
           MOVE SORT-COD-VEND TO REP-DETALLE-COD-VEND.
           MOVE SORT-IMPORTE TO REP-DETALLE-IMPORTE.
           MOVE REP-DETALLE TO REPA-RECORD.
           PERFORM IMPRIMIR-REPA-LINEA.

       IMPRIMIR-REPNOA-DETALLE.
           MOVE SORT-COD-PROD TO REP-DETALLE-COD-PROD.
           MOVE SORT-CANTIDAD TO REP-DETALLE-CANTIDAD.
           MOVE SORT-COD-VEND TO REP-DETALLE-COD-VEND.
           MOVE SORT-IMPORTE TO REP-DETALLE-IMPORTE.
           MOVE REP-DETALLE TO REPNOA-RECORD.
           PERFORM IMPRIMIR-REPNOA-LINEA.

       FIN-REPORTES.
           CLOSE REPA.
           CLOSE REPNOA.

