       IDENTIFICATION DIVISION.
       PROGRAM-ID.    AKLAB8TABLES.
       AUTHOR.        CYNTHIAGJENSEN.
       
      *****************************************************************
      *
      *  THIS PROGRAM VALIDATES THE VENDOR CODE FOR THE INCOMING
      *  RECORD.  WHEN VALID VENDOR CODES ARE FOUND THE ENTIRE
      *  RECORD IS WRITTEN OUT TO A NEW PRODUCT INVENTORY FILE
      *  IF THE VENDOR CODE IS NOT FOUND THE ENTIRE RECORD IS 
      *  WRITTEN OUT TO AN ERROR FILE
      *     
      *  INPUT: 
      *     KEY FIELDS:  
      *        WAREHOUSE ID - SIZE 4
      *        VENDOR ID - SIZE 1 - SHOULD BE VALIDATED FROM A TABLE
      *        PRODUCT ID  - SIZE 3
      *        FILLER SIZE- SIZE 120
      *****************************************************************
      *
      *  OUTPUT: 
      *    NEW PRODUCT INVENTORY FILE WITH ONLY VALID VENDOR ID'S
      *    ERROR FILE WITH RECORDS THAT HAVE INVALID VENDOR ID'S
      *    DISPLAY A MESSAGE ON THE SCREEN WITH THE
      *    NUMBER OF ERROR RECORDS WRITTEN OUT
      *    
      *****************************************************************
      *  CALCULATIONS: 
      *    ACCUMULATE AN ERROR COUNT
      *****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PRODUCT-FILE 
               ASSIGN TO 'PRODUCT-INVEN.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

      *ADD NEEDED SELECT STATEMENTS HERE

           SELECT NEW-PRODUCT-FILE
               ASSIGN TO 'NEW-PROD-INVEN.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERROR-FILE
               ASSIGN TO 'ERROR-FILE.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.
 




       DATA DIVISION.
       FILE SECTION.

       FD PRODUCT-FILE
          RECORD CONTAINS 128 CHARACTERS.
       01 PROD-REC.
          05  PR-WAREHOUSE-ID               PIC X(4).
          05  PR-VENDOR-ID                  PIC X.
          05  PR-PRODUCT-ID                 PIC X(3).
          05  FILLER                        PIC X(120).

      * ADD FD AND RECORD FOR NEEDED FILES HERE
 
        FD NEW-PRODUCT-FILE
            RECORD CONTAINS 120 CHARACTERS.
        01 NEW-PROD-REC                     PIC X(120).
 
        FD ERROR-FILE
            RECORD CONTAINS 120 CHARACTERS.
        01 ERROR-FILE-REC                   PIC X(120).





       WORKING-STORAGE SECTION.

      *****************VARIABLE SECTION*************************

       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                PIC X     VALUE SPACE.
                88  MORE-RECORDS                 VALUE 'Y'.
                88  NO-MORE-RECORDS              VALUE 'N'.
 
       01  REPORT-FIELDS.
      * ADD ERROR COUNT FIELD HERE
           05 ERROR-COUNT              PIC 999   VALUE 0.

      ***********************TABLE**********************************

       01  VENDOR-TEXT.

           05  PIC X(13)    VALUE 'AAMEL LTD.'.
           05  PIC X(13)    VALUE 'IMADEINHOUSE'.
           05  PIC X(13)    VALUE 'TTANSIA CORP.'.
           05  PIC X(13)    VALUE 'WWEST CORP.'.

      * CODE VENDOR TEXT REDEFINES HERE WITH AN INDEX

       01 VENDOR-TABLE REDEFINES
          VENDOR-TEXT OCCURS 4 TIMES 
          INDEXED BY VEN-INDEX.
             05 VEND-ID                   PIC X.
             05 VEND-NAME                 PIC X(12).

          


      ***********************PROCEDURE DIVISION************************

       PROCEDURE DIVISION.

       100-MAIN-MODULE.
  
           PERFORM 110-HOUSEKEEPING
           PERFORM 120-READ-RECORDS
           PERFORM 140-CLOSE-ROUTINE
         .

       110-HOUSEKEEPING.

           OPEN    INPUT   PRODUCT-FILE
                   OUTPUT  NEW-PRODUCT-FILE
                   OUTPUT  ERROR-FILE
         .
      *
       120-READ-RECORDS.

             PERFORM UNTIL NO-MORE-RECORDS
                  READ PRODUCT-FILE
                      AT END
                          MOVE 'N' TO EOF-FLAG
                      NOT AT END
                          PERFORM 130-SEARCH-VENDOR
                  END-READ
              END-PERFORM
         .
      *
       130-SEARCH-VENDOR.

      * CODE VENDOR SEARCH HERE USE AN INDEX
      * 
 

         SET VEN-INDEX TO 1
         SEARCH VENDOR-TABLE
             AT END
                  MOVE PROD-REC TO ERROR-FILE-REC
                  WRITE ERROR-FILE-REC
                  ADD 1 TO ERROR-COUNT
             WHEN PR-VENDOR-ID = VEND-ID (VEN-INDEX)
                  MOVE PROD-REC TO NEW-PROD-REC
                  WRITE NEW-PROD-REC
             END-SEARCH







         .
      *
       140-CLOSE-ROUTINE.
      * CODE TO PUT AN ERROR MESSAGE WITH THE ERROR COUNT ON THE
      * SCREEN GOES HERE

              DISPLAY ERROR-COUNT ' . RECORD WRITTEN TO ERROR FILE'


              CLOSE    PRODUCT-FILE
                       NEW-PRODUCT-FILE
                       ERROR-FILE
              STOP RUN
         .
