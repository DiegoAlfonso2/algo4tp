       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TRABAJO-PRACTICO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT PVE
               ASSIGN TO 'PROD-VEND.DAT'
               ORGANIZATION IS INDEXED
               ACCESS IS RANDOM
               RECORD KEY IS PVE-PK
               FILE STATUS IS PVE-FS.
       
       DATA DIVISION.
       FILE SECTION.
            FD  PVE.
            01  PVE-RECORD.
               03 PVE-PK.
                    05 PVE-COD-PROD   PIC 9(4).
                    05 PVE-FECHA      PIC X(10).
               03 PVE-CANTIDAD        PIC 9(4).
               03 PVE-IMPORTE         PIC 9(7)V99.

       WORKING-STORAGE SECTION.
          01   PAR-IN.
               03 PAR-IN-MODO            PIC X.
               03 PAR-IN-PK.
                    05 PAR-IN-COD-PROD   PIC 9(4).
                    05 PAR-IN-FECHA      PIC X(10).
               03 PAR-IN-CANTIDAD        PIC 9(4).
               03 PAR-IN-IMPORTE         PIC 9(7)V99.
          01   PAR-OUT.
               03 PAR-OUT-RET-COD         PIC 9(2).
            01 PVE-FS                     PIC 9(2).
               88 PVE-OK                 VALUE '00'.
               88 PVE-EOF                VALUE '10'.

       PROCEDURE DIVISION.
       PRINCIPAL.
            DISPLAY 'ANTES DE INVOCAR EL SUBPROGRAMA'.
            MOVE '7774' TO PAR-IN-COD-PROD.
            MOVE '2018-04-25' TO PAR-IN-FECHA.
            MOVE '23' TO PAR-IN-CANTIDAD.
            MOVE '40.85' TO PAR-IN-IMPORTE.
            PERFORM LEER-PVE.
            MOVE 'O' TO PAR-IN-MODO.
            CALL 'UPD-PROD-VEND' USING PAR-IN, PAR-OUT.
            MOVE 'U' TO PAR-IN-MODO.
            DISPLAY 'INVOCANDO SUB-PROGRAMA CON ' PAR-IN.
            CALL 'UPD-PROD-VEND' USING PAR-IN, PAR-OUT.
            DISPLAY 'RESULTADO DE LA INVOCACION: ' PAR-OUT.
            MOVE 'C' TO PAR-IN-MODO.
            CALL 'UPD-PROD-VEND' USING PAR-IN, PAR-OUT.
            DISPLAY 'DESPUES DE INVOCAR EL SUBPROGRAMA'.
            PERFORM LEER-PVE.
            STOP RUN.

        LEER-PVE.
            OPEN I-O PVE.
            MOVE PAR-IN-COD-PROD TO PVE-COD-PROD.
            MOVE PAR-IN-FECHA TO PVE-FECHA.
            READ PVE RECORD 
                INVALID KEY
                    DISPLAY 'CLAVE NO ENCONTRADA'.
            IF PVE-OK THEN
                DISPLAY 'CANT: ' PVE-CANTIDAD ' IMP: ' PVE-IMPORTE
            END-IF.
            CLOSE PVE.
