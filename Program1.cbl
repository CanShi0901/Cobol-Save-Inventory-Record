       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAVE-INVENTORY-RECORD.
       AUTHOR. CAN SHI.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *If INVFILE.TXT file does not exist, the program will create one. 
           SELECT OPTIONAL INVENTORY-FILE-OUT ASSIGN TO 'INVFILE.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD INVENTORY-FILE-OUT.
       01 INVENTORY-RECORD PIC X(41).
           
       WORKING-STORAGE SECTION.
       01 WS-INVENTORY-RECORD.
           05 WS-PART-NUMBER PIC 9(9).
           05 WS-PART-NAME PIC X(20).
           05 WS-QUANTITY-ON-HAND PIC 9(3).
           05 WS-UNIT-PRICE PIC 9(4).
           05 WS-SUPPLIER-CODE PIC X(5).                                
       02 WS-ENTER-RECORD PIC A(1).                                     
      *Conditional names, which will be used to to determine whether a 
      * record will be entered.
           88 WS-ENTER-RECORD-YES VALUE 'Y' 'y'.
           88 WS-ENTER-RECORD-NO VALUE 'N' 'n'.

       PROCEDURE DIVISION.
 
       0100-SAVE-INVENTORY-RECORD.
           PERFORM 0201-INITIATE-IVENTORY-RECORDING.
           PERFORM 0202-SAVE-INV-RECORD UNTIL WS-ENTER-RECORD-NO.
           PERFORM 0203-TERMINATE-RECORDING-JOB.
           STOP RUN.

       0201-INITIATE-IVENTORY-RECORDING.
           PERFORM 0301-OPEN-FILE
           PERFORM 0302-PROMPT-TO-RECORD.
           
       0202-SAVE-INV-RECORD.
      *Prompt user to enter each data field.
           PERFORM 0303-PROMPT-PART-NUMBER
           PERFORM 0304-PROMPT-PART-NAME
           PERFORM 0305-PROMPT-QUANTITY
           PERFORM 0306-PROMPT-UNIT-PRICE
           PERFORM 0307-PROMPT-SUPPLIER-CODE
           PERFORM 0308-WRITE-INV-FILE.
      *Ask the user again if another record is to be recorded.          
           PERFORM 0302-PROMPT-TO-RECORD.
                   
       0203-TERMINATE-RECORDING-JOB.
           PERFORM 0309-CLOSE-FILE.
      
       0301-OPEN-FILE.
      *Opens the file to be written. 
           OPEN OUTPUT INVENTORY-FILE-OUT.

       0302-PROMPT-TO-RECORD.
      *Prompt user whether a record will be entered 
           DISPLAY "Record to enter? y/n"
              LINE 2 COLUMN 4
           ACCEPT WS-ENTER-RECORD
              LINE 2 COLUMN 25.
       
       0303-PROMPT-PART-NUMBER.
           DISPLAY 'Enter part number'
              LINE 4 COLUMN 5.
           ACCEPT WS-PART-NUMBER
              LINE 5 COLUMN 10.
       
       0304-PROMPT-PART-NAME.
           DISPLAY 'Enter part name'
              LINE 6 COLUMN 5
           ACCEPT WS-PART-NAME
              LINE 7 COLUMN 10.
              
       0305-PROMPT-QUANTITY.
           DISPLAY 'Enter quantity on hand'
              LINE 8 COLUMN 5
           ACCEPT WS-QUANTITY-ON-HAND
              LINE 9 COLUMN 10.
              
       0306-PROMPT-UNIT-PRICE.
           DISPLAY 'Enter unit price'
              LINE 10 COLUMN 5
           ACCEPT WS-UNIT-PRICE
              LINE 11 COLUMN 10.
              
       0307-PROMPT-SUPPLIER-CODE.
           DISPLAY 'Enter supplier code'
              LINE 12 COLUMN 5
           ACCEPT WS-SUPPLIER-CODE
               LINE 13 COLUMN 10.
               
       0308-WRITE-INV-FILE.
      *Move date accepted into the inventory-record and write to
      *inventory-file-out.
           MOVE WS-INVENTORY-RECORD TO INVENTORY-RECORD
           WRITE INVENTORY-RECORD
           END-WRITE.
                   
       0309-CLOSE-FILE.
           DISPLAY 'Stopping...'
               lINE 25 COLUMN 4
           CLOSE INVENTORY-FILE-OUT.
       