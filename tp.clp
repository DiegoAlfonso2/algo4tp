       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TRABAJO-PRACTICO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PROD
               ASSIGN TO 'PROD.TXT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS PROD-FS.

       DATA DIVISION.
       FILE SECTION.
           FD PROD.
           01 PROD-RECORD.
               05 PROD-COD-PROD           PIC 9(4).
               05 PROD-FECHA-ALTA         PIC 9(8).
               05 PROD-DESCRIP            PIC X(15).

       WORKING-STORAGE SECTION.
           01 PROD-FS                     PIC 9(2).
               88 PROD-OK                 VALUE '00'.
               88 PROD-EOF                VALUE '10'.
       
       PROCEDURE DIVISION.
       PRINCIPAL.
           OPEN INPUT PROD.
           PERFORM LEER-PROD UNTIL PROD-EOF.
           DISPLAY "I did it again".
           CLOSE PROD.
           STOP RUN.
       
       LEER-PROD.
           READ PROD.
           DISPLAY PROD-DESCRIP.