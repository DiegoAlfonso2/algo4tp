       IDENTIFICATION DIVISION.
       PROGRAM-ID.  UPD-PROD-VEND.

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
      * FILE STATUSES DE ARCHIVOS
            01 PVE-FS                     PIC 9(2).
               88 PVE-OK                 VALUE '00'.
               88 PVE-EOF                VALUE '10'.

       LINKAGE SECTION.
            01   PAR-IN.
               03 PAR-IN-COD-PROD        PIC 9(4).
               03 PAR-IN-FECHA           PIC X(10).
               03 PAR-IN-CANTIDAD        PIC 9(4).
               03 PAR-IN-IMPORTE         PIC 9(7)V99.
            01   PAR-OUT.
               03 PAR-OUT-RET-COD        PIC 9(2).

       PROCEDURE DIVISION USING PAR-IN, PAR-OUT.
       PRINCIPAL.

            DISPLAY PAR-IN.
            DISPLAY PAR-OUT.
            MOVE '45' TO PAR-OUT-RET-COD.
            EXIT PROGRAM.
