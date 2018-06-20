       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TRABAJO-PRACTICO.

       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
          01   PAR-IN.
               03 PAR-IN-COD-PROD        PIC 9(4).
               03 PAR-IN-FECHA           PIC X(10).
               03 PAR-IN-CANTIDAD        PIC 9(4).
               03 PAR-IN-IMPORTE         PIC 9(7)V99.
          01   PAR-OUT.
               03 PAR-OUT-RET-COD         PIC 9(2).

       PROCEDURE DIVISION.
       PRINCIPAL.
            MOVE '1152' TO PAR-IN-COD-PROD.
            MOVE '2018-04-24' TO PAR-IN-FECHA.
            MOVE '23' TO PAR-IN-CANTIDAD.
            MOVE '40.85' TO PAR-IN-IMPORTE.
            DISPLAY 'INVOCANDO SUB-PROGRAMA CON ' PAR-IN.
            CALL 'UPD-PROD-VEND' USING PAR-IN, PAR-OUT.
            DISPLAY 'RESULTADO DE LA INVOCACION: ' PAR-OUT.
            STOP RUN.
