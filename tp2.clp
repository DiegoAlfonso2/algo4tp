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
                   05 SORT-COD-SOL            PIC 9(6).
                   05 SORT-COD-PROD           PIC 9(4).
               03 SORT-COD-VEND               PIC 9(3).
               03 SORT-TELEFONO               PIC 9(10).
               03 SORT-CANTIDAD               PIC 9(4).
               03 SORT-IMPORTE                PIC 9(7)V99.
       WORKING-STORAGE SECTION.
      * FILE STATUSES DE ARCHIVOS
           01 SOLIC-FS                     PIC 9(2).
               88 SOLIC-OK                 VALUE '00'.
               88 SOLIC-EOF                VALUE '10'.
           01 VEN-FS                     PIC 9(2).
               88 VEN-OK                 VALUE '00'.
               88 VEN-EOF                VALUE '10'.
           01 SORT-FS                     PIC 9(2).
               88 SORT-OK                 VALUE '00'.
               88 SORT-EOF                VALUE '10'.
           01 FIN-SORT                    PIC X.

           01 WS-ERROR.
               03 WS-MENSAJE-ERROR           PIC X(80).
               03 WS-COD-ERROR               PIC 9(2). 

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
           STOP RUN.

       ENTRADA SECTION.
           PERFORM INICIO.
           PERFORM PROCESO.
           PERFORM FIN.
    
       RUTINAS-ENTRADA SECTION.
       INICIO.
           OPEN INPUT SOLIC.
           OPEN INPUT VEN.

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

       FIN.
           CLOSE SOLIC.
           CLOSE VEN.

       SALIDA SECTION.
           RETURN SORTFILE AT END MOVE 'S' TO FIN-SORT.
           PERFORM LEER-SORT UNTIL FIN-SORT = 'S' OR SORT-EOF.

       RUTINAS-SALIDA SECTION.
       LEER-SORT.
           DISPLAY '<-- ' SORT-RECORD.
           RETURN SORTFILE AT END MOVE 'S' TO FIN-SORT.
