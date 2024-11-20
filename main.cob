      ******************************************************************
      * author: Group 2
      * date: 11/20/2024
      * purpose: Create a calculator for loans
      * tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. loan-calculator.


       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      * This string variable contains the username input at the 
      * beginning of the program
           01 customer-name                   PIC X(99).
           01 string-input                    PIC X(20).           

      * These variables are use for computations
           01 computational-variables.
               05 loan-amount              PIC S9(10)V9999 VALUE -1.  
               05 annual-interest-rate     PIC S9(10)V99999999 VALUE 0.
               05 monthly-interest-rate    PIC 9(10)V9999999999 VALUE 0.
               05 loan-term-years          PIC S9(10)V9999 VALUE 0.
               05 loan-term-months         PIC 9(10)V9999 VALUE 0.
               05 installment-period       PIC 9(3) VALUE 0.
               05 total-principal          PIC 9(10)V9999 VALUE 0.
               05 total-interest           PIC 9(10)V9999 VALUE 0.
               05 total-payments           PIC 9(10)V9999 VALUE 0.
               
      * These variables are use to remove the exceeding zeroes from the
      * floats or integer values
            01 z-computational-variables.
               05 z-loan-amount              PIC Z(10).99.
               05 z-annual-interest-rate     PIC Z(10).99.
               05 z-number-of-monthly        PIC Z(10).
               05 z-balloon-rate             PIC Z(10).99.
               05 z-loan-term-years          PIC Z(10).
               05 z-grace-period             PIC Z(10).  


      * These variables are use for generic purposes 
      * (you will see these all throughout the paragraphs)
           01 global-formula-variables.
               05 principal                        PIC 9(10)V9999999999.
               05 rate                             PIC 9(3)V99999999999.
               05 number-of-payments               PIC 9(10)V9999999999.
               05 principal-payment                PIC 9(10)V9999999999.
               05 interest-payment                 PIC 9(10)V9999999999.
               05 remaining-balance                PIC 9(10)V9999999999.
               05 equated-monthly-installment      PIC 9(10)V9999999999.
               05 amount-payable                   PIC 9(10)V9999999999.


      * This variable is use specifically for fixed principal 
      * amortization case (paragraph-b)
           01 fixed-principal-amortization-case.
               05 outstanding-balance              PIC 9(10)V9999999999.

      * This variable is use specifically for balloon case (paragraph-d)
           01 balloon-case.
               05 balloon-payment          PIC 9(10)V9999999999.
               05 balloon-rate             PIC S9(10)V9999 VALUE -1.


      * This variable is use specifically for grace period case (paragraph-c)
           01 grace-period-case.   
               05 grace-period              PIC S9(10)V9999 VALUE -1.                      


      * These variables are used to display the outputs to the table 
           01 display-variables.

      *    These variables are used for the header of the table
               05 display-headers.
                   07 period-header    PIC X(21) VALUE
      -                                "Installment Period".
                   07 date-header      PIC X(17) VALUE "Payment-Date".
                   07 principal-header PIC X(20) VALUE "Principal".
                   07 interest-header  PIC X(20) VALUE "Interest".
                   07 payments-header  PIC X(20) VALUE "Payments".
                   07 balance-header   PIC X(20) VALUE "Balance".

      *    These variables are used to display outputs to the footer of the table 
               05 display-footers.
                   07 period-footer    PIC X(20) VALUE "  Total: ".
                   07 date-footer      PIC X(15) VALUE SPACES.
                   07 principal-footer PIC Z(15).99.
                   07 interest-footer  PIC Z(15).99.
                   07 payments-footer  PIC Z(15).99.
                   07 balance-footer   PIC X(20) VALUE SPACES.

      *    These variables are used to display outputs inside the 
      *    table -> period value, principal value, interest, etc.
               05 display-values.
                   07 period-value                 PIC X(20).
                   07 date-value                   PIC X(15).
                   07 principal-value              PIC Z(15).99.
                   07 interest-value               PIC Z(15).99.
                   07 total-payments-value         PIC Z(15).99.
                   07 balance-value                PIC Z(15).99.


      * These are miscellaneous variables, you will see this used for designs and such.
           01 miscellaneous-variables.
               05 i                            PIC 9(3).
               05 n                            PIC 9(3) VALUE 119.
               05 col-space                    PIC X(3) VALUE SPACES.
               05 invalid-input                PIC X(1).
               05 clear-command                PIC X(20) VALUE "clear".
               05 loan-type-choice             PIC X(1).
               05 back-choice                  PIC X(1).
               05 peso-symbol                  PIC X(3) VALUE '₱'. 
               05 space-value                  PIC X(1) VALUE SPACES.
               05 flag                         PIC X(10) VALUE "False".


      * This is where the logic starts
       PROCEDURE DIVISION.
           CALL "SYSTEM" USING clear-command
           PERFORM design
           DISPLAY "WELCOME TO YOUR LOAN CALCULATOR!"
           PERFORM design


      * Ask username
           DISPLAY "Input username: " WITH NO ADVANCING
           ACCEPT customer-name

           CALL "SYSTEM" USING clear-command

      * Redirect user to choose what type of loan (going to function type-of-loan)
           PERFORM type-of-loan

           STOP RUN.


      * This is where the user will be asked regarding the type of loan
       type-of-loan.
           CALL "SYSTEM" USING clear-command
           PERFORM greet-user

      * Ask user to choose what type of loan
           PERFORM design
           DISPLAY "PLEASE CHOOSE THE TYPE OF LOAN:"
           PERFORM design

           DISPLAY "(a) Fixed Equal Amortization Case"
           DISPLAY "(b) Fixed Principal Amortization Case"
           DISPLAY "(c) Fixed Equal Amortization Case with Grace Period"
           DISPLAY "(d) Periodic Interest Payment, Balloon Payment "
      -                 "at Maturity"
           DISPLAY "(e) Exit"

           DISPLAY ""
           DISPLAY ""

           PERFORM design
           DISPLAY "TYPE 'A', 'B', 'C', 'D', or 'E'"
           PERFORM design

           DISPLAY "INPUT: " WITH NO ADVANCING
           ACCEPT loan-type-choice

      * Convert the user-input to uppercase
           MOVE FUNCTION UPPER-CASE(loan-type-choice)
      -    TO loan-type-choice 

      * Check if input is valid
      * If the input is valid, the user will be asked if he/she wants 
      * to continue then will be directed to the paragraph he/she chose.
           IF loan-type-choice IS ALPHABETIC 
           AND loan-type-choice = 'A'
           OR loan-type-choice = 'B' 
           OR loan-type-choice = 'C' 
           OR loan-type-choice = 'D' 
           OR loan-type-choice = 'E'
                PERFORM go-back 
           ELSE
               DISPLAY "Invalid input...Please choose from "
      -                "'A', 'B', 'C', 'D' or 'E'"
               ACCEPT invalid-input
               PERFORM type-of-loan
           END-IF.
           EXIT.


      * This paragraph will ask if the user wants to continue then will 
      * be redirected to the paragraph he chose, otherwise the user will
      * be redirected to the type-of-loan function.
       go-back.
           CALL "SYSTEM" USING clear-command
           DISPLAY "Are you sure you want to continue [Y/N]? " WITH NO
           ADVANCING
           ACCEPT back-choice.
           MOVE FUNCTION UPPER-CASE(back-choice) TO back-choice
           IF back-choice IS NOT ALPHABETIC 
           OR back-choice NOT = 'N' 
           AND back-choice NOT = 'Y'
               DISPLAY "Please choose if Y/N..."
               ACCEPT invalid-input
               PERFORM go-back         
           ELSE IF back-choice = 'N' 
               PERFORM type-of-loan
           ELSE IF back-choice = 'Y'
               PERFORM redirect-user
           END-IF
           EXIT.

      * If the use picked yes, the user will be directed to this function
      * This function then will again redirect the user to another function
      * that matches the user's choice to the said statements below.
      * Redirects the user to paragraphs' 'A', 'B', 'C', or 'D'.
       redirect-user.
           EVALUATE loan-type-choice
               WHEN 'A'
                   PERFORM paragraph-a
               WHEN 'B'
                   PERFORM paragraph-b
               WHEN 'C'
                   PERFORM paragraph-c
               WHEN 'D'
                   PERFORM paragraph-d
               WHEN 'E'
                   PERFORM exit-program
               WHEN OTHER
                   DISPLAY "Invalid input...Please choose from "
      -                    "'A', 'B', 'C' OR 'D'"
                   ACCEPT invalid-input
                   PERFORM type-of-loan
           END-EVALUATE.
           EXIT.


      * This function will perform the fixed equal amortization case.
      * The formula is based from the Banko Sentral ng Pilipinas.
       paragraph-a.
      * This will check if the necessary details are empty in value.
      * If it's equal to zero, the program will redirect the user to 
      * do the necessary actions, such as inputting the loan-amount, etc. 
           IF loan-amount = 0 OR loan-term-years = 0
               PERFORM ask-loan-details
           END-IF.

      * After asking for the loan details, the program will then perform
      * loan-summary, headers, and such.
           PERFORM loan-summary
           PERFORM headers
           PERFORM table-initial-value
           PERFORM input-assignment
           

      * This part compute the necessary values for the table outputs and 
      * print it until remaining balance is less than 0.01 => remaining balance === 0.00 
           PERFORM UNTIL remaining-balance < 0.01
         
      *        Increments the installment period by 1
               COMPUTE installment-period = installment-period + 1

      *        Computes the EMI or monthly installmment for the following period
      *        EMI = ((p * r) * (1 + r)^n)/((1 + r)^n)-1  
               COMPUTE equated-monthly-installment = ((principal * rate
      -        * ((1 + rate) ** number-of-payments))) / (((1 + rate) **
      -        number-of-payments) - 1)

      *        Computes the interest for the following period 
               COMPUTE interest-payment = remaining-balance * rate
       
      *        Computes the principal for the following period 
               COMPUTE principal-payment = equated-monthly-installment -
      -                                    interest-payment

      *        Computes the remaining balance to pay
               COMPUTE amount-payable = remaining-balance -
      -                                 principal-payment

      *        Computes the total interest to be paid 
               COMPUTE total-interest ROUNDED = total-interest +
      -                                         interest-payment

      *        Computes the total principal to be paid
               COMPUTE total-principal ROUNDED = total-principal +
      -                                          principal-payment

      *        Computes overall payment
               COMPUTE total-payments  ROUNDED = total-payments +
      -                               equated-monthly-installment

      *        Moving the computed values from computational variables
      *        to displaying variables 
               PERFORM table-value-assignment

      *        Prints the value being computed
               PERFORM table-values

      *        Updates the new remaining balance
               MOVE amount-payable TO remaining-balance

           END-PERFORM

      * Prints the footer section and necessarry values
           PERFORM footer-and-values
           DISPLAY ""
           DISPLAY ""
           PERFORM design
           DISPLAY "Press return to choose type of loan."  *> redirects user to go back to type-of-loan
      -    WITH NO ADVANCING
           ACCEPT invalid-input
           PERFORM clear-values
           PERFORM type-of-loan
           EXIT.


      * This function will perform the fixed principal amortization case.
      * The formula is based from the Banko Sentral ng Pilipinas.
       paragraph-b.
      * This will check if the necessary details are empty in value.
      * If it's equal to zero, the program will redirect the user to 
      * do the necessary actions, such as inputting the loan-amount, etc. 
           IF loan-amount = 0 OR loan-term-years = 0
               PERFORM ask-loan-details
           END-IF.

      * After asking for the loan details, the program will then perform
      * loan-summary, headers, and such.
           PERFORM loan-summary
           PERFORM headers
           PERFORM table-initial-value
           PERFORM input-assignment

      * This part compute the necessary values for the table outputs and 
      * print it until remaining balance is less than 0.01 => remaining balance === 0.00 
           PERFORM UNTIL remaining-balance < 0.01
      
      *        Initialized and updates the outstanding balance value
               MOVE remaining-balance TO outstanding-balance

      *        Increments the installment period by 1
               COMPUTE installment-period = installment-period + 1


      *        Computes the principal payment for the following period
               COMPUTE principal-payment = loan-amount /
      -                                    number-of-payments

      *        Computes the interest payment for the following period
               COMPUTE interest-payment = outstanding-balance * rate


      *        Computes the EMI or monthly installment for the following period
               COMPUTE equated-monthly-installment = principal-payment +
      -                                              interest-payment

      *        Computes the remaining balance for the following period
               COMPUTE amount-payable = remaining-balance -
      -                                 principal-payment

      *        Computes the total interest to be paid
               COMPUTE total-interest ROUNDED = total-interest +
      -                                         interest-payment

      *        Computes the total principal to be paid
               COMPUTE total-principal ROUNDED = total-principal +
      -                                          principal-payment

      *        Computes the overall payments to be paid
               COMPUTE total-payments ROUNDED = total-payments +
      -                                 equated-monthly-installment

      *        Assigns computed value from computational variables to 
      *        displaying variables
               PERFORM table-value-assignment

      *        Prints the value being computed
               PERFORM table-values

      *        Updates the new remaining balance
               MOVE amount-payable TO remaining-balance

           END-PERFORM

      * Prints the footer section and necessarry values
           PERFORM footer-and-values
           DISPLAY ""
           DISPLAY ""
           PERFORM design
           DISPLAY "Press return to choose type of loan." *> redirects user to go back to type-of-loan
      -    WITH NO ADVANCING
           ACCEPT invalid-input
           PERFORM clear-values
           PERFORM type-of-loan
           EXIT.


      * This function will perform the fixed equal amortization case with grace period.
      * The formula is based from the Banko Sentral ng Pilipinas.
       paragraph-c.
      * This will check if the necessary details are empty in value.
      * If it's equal to zero, the program will redirect the user to 
      * do the necessary actions, such as inputting the loan-amount, etc. 
           IF loan-amount = 0 OR loan-term-years = 0
               PERFORM ask-loan-details
           END-IF.

      * After asking for the loan details, the program will then perform
      * loan-summary, headers, and such.
           PERFORM loan-summary
           PERFORM headers
           PERFORM table-initial-value
           PERFORM input-assignment

      * This part compute the necessary values for the table outputs and 
      * print it until remaining balance is less than 0.01 => remaining 
      * balance === 0.00  and installment period is equal to the number 
      * of payments added to the number of grace period.
           PERFORM UNTIL installment-period = number-of-payments + 
                         grace-period OR remaining-balance < 0.01
               
      *        Increments the installment period by 1
               COMPUTE installment-period = installment-period + 1
             
      *        While the installment period is less than or equal to the 
      *        number of grace period, the value for the specific cell 
      *        of principal, interest, and monthly installment for this 
      *        period will zero. Otherwise, EMI, interest payment, and  
      *        principal payment will be calculated in regards to the 
      *        user's inputted value for loan-amount, number of years,
      *        and  annual interest rate.
               IF installment-period <= grace-period 
                   MOVE 0 TO principal-payment
                   MOVE 0 TO interest-payment
                   MOVE 0 TO equated-monthly-installment
               ELSE               
                   COMPUTE equated-monthly-installment = ((principal   
      -            * rate * ((1 + rate) ** number-of-payments))) / 
      -            (((1 + rate) ** number-of-payments) - 1)

                   COMPUTE interest-payment = remaining-balance * rate

                   COMPUTE principal-payment =                         
      -                    equated-monthly-installment -interest-payment
               END-IF

      *        Computes the remaining balance for the following period
               COMPUTE amount-payable = remaining-balance -
      -                                 principal-payment

      *        Computes total interest
               COMPUTE total-interest ROUNDED = total-interest +
      -                                         interest-payment

      *        Computes total principal
               COMPUTE total-principal ROUNDED = total-principal +
      -                                          principal-payment

      *        Computes overall payment
               COMPUTE total-payments ROUNDED = total-payments +
      -                                    equated-monthly-installment

      *        Assigns computed value from computational variables to 
      *        displaying variables
               PERFORM table-value-assignment

      *        Prints the value being computed
               PERFORM table-values
           
      *        Updates the new remaining balance
               MOVE amount-payable TO remaining-balance
           
           END-PERFORM

      * Prints the footer section and necessarry values
           PERFORM footer-and-values
           DISPLAY ""
           DISPLAY ""
           PERFORM design
           DISPLAY "Press return to choose type of loan." *> redirects user to go back to type-of-loan
      -    WITH NO ADVANCING
           ACCEPT invalid-input
           PERFORM clear-values
           PERFORM type-of-loan
           EXIT.


      * This function will perform the periodic interest payment, balloon 
      * payment at maturity. The formula is based from the Banko Sentral ng Pilipinas.
       paragraph-d.
      * This will check if the necessary details are empty in value.
      * If it's equal to zero, the program will redirect the user to 
      * do the necessary actions, such as inputting the loan-amount, etc. 
           IF loan-amount = 0 OR loan-term-years = 0
               PERFORM ask-loan-details
           END-IF.

      * After asking for the loan details, the program will then perform
      * loan-summary, headers, and such.
           PERFORM loan-summary
           PERFORM headers
           PERFORM table-initial-value
           PERFORM input-assignment

      * Computes the balloon payment
           COMPUTE balloon-payment  = principal * balloon-rate 

      * Computes the principal payment
           COMPUTE principal-payment = (principal - balloon-payment)
      -                                / number-of-payments

      * This part compute the necessary values for the table outputs and 
      * print it until remaining balance is less than the balloon payment 
      * => remaining balance === balloon payment  or installment period  
      * is equal to the numberof payments.
           PERFORM UNTIL remaining-balance <= balloon-payment OR 
                         installment-period = number-of-payments

      *        Increments installment period by 1          
               COMPUTE installment-period = installment-period + 1
               
      *        If remaining balance is less than or equal to balloon 
      *        payment or installment period is equal to the number of 
      *        payments, assign the the balloon payment to principal 
      *        payment, and principal payment to remaining balance
               IF remaining-balance <= balloon-payment OR 
                  installment-period = number-of-payments
                   MOVE balloon-payment TO principal-payment
                   MOVE principal-payment TO remaining-balance
               END-IF

      *        Computes the interest payment for the following period
               COMPUTE interest-payment = remaining-balance * rate

      *        Computes the EMI or monthly installment for the following period
               COMPUTE equated-monthly-installment = principal-payment +
      -                                              interest-payment

      *        Computes the remaining balance for the following period
               COMPUTE amount-payable = remaining-balance -
      -                                 principal-payment

      *        Computes the total interest
               COMPUTE total-interest ROUNDED = total-interest +
      -                                         interest-payment

      *        Computes the total principal
               COMPUTE total-principal ROUNDED = total-principal +
      -                                          principal-payment

      *        Computes the overall payment
               COMPUTE total-payments ROUNDED = total-payments +
      -                                 equated-monthly-installment
      *        Assigns computed value from computational variables to 
      *        displaying variables           
               PERFORM table-value-assignment
      
      *        Prints the value being computed
               PERFORM table-values
               
      *        Updates the new remaining balance
               MOVE amount-payable TO remaining-balance
           END-PERFORM

      * Prints the footer section and necessarry values
           PERFORM footer-and-values
           DISPLAY ""
           DISPLAY ""
           PERFORM design
           DISPLAY "Press return to choose type of loan." *> redirects user to go back to type-of-loan
      -    WITH NO ADVANCING
           ACCEPT invalid-input
           PERFORM clear-values
           PERFORM type-of-loan

           EXIT.


      * This function ask the loan details from the user
       ask-loan-details.
           CALL "SYSTEM" USING clear-command
           PERFORM greet-user
           PERFORM design

      *    While loan-amount is not a positive number, or not numeric,
      *    this code will iterate until the condition is met 
           PERFORM UNTIL loan-amount >= 0
               PERFORM ask-loan-amount
      *        IF loan-amount = 0 or loan-amount IS NOT NUMERIC
               PERFORM isPunct
               IF string-input IS ALPHABETIC
               OR flag = "True"
                   DISPLAY "Invalid input...Press return"
                   ACCEPT invalid-input
               ELSE 
                   MOVE string-input TO loan-amount
                   IF loan-amount < 0
                       DISPLAY "Invalid input...Press return"
                       ACCEPT invalid-input
                   ELSE 
                       MOVE loan-amount TO z-loan-amount
                   END-IF
               END-IF
               MOVE "False" TO flag
           END-PERFORM.


      *    While annual-interest rate is not a positive number, or not 
      *    numeric, this code will iterate until the condition is met 
           PERFORM UNTIL annual-interest-rate > 0
               PERFORM ask-annual-interest-rate
               PERFORM isPunct
               IF string-input IS ALPHABETIC
               OR flag = "True"
                   DISPLAY "Invalid input...Press return"
                   ACCEPT invalid-input
               ELSE 
                   MOVE string-input TO annual-interest-rate
                   IF annual-interest-rate < 0
                       DISPLAY "Invalid input...Press return"
                       ACCEPT invalid-input
                   ELSE 
                       MOVE annual-interest-rate 
                       TO z-annual-interest-rate
                   END-IF
               END-IF
               MOVE "False" TO flag
           END-PERFORM.

      *    Converts the annual interest rate to monthly interest rate
           COMPUTE monthly-interest-rate = (annual-interest-rate / 100)
      -                                    / 12

      *    While loan term years is not a positive number, or not 
      *    numeric, this code will iterate until the condition is met 
           PERFORM UNTIL loan-term-years > 0
               PERFORM ask-loan-term-years

               PERFORM isPunct
               IF string-input IS ALPHABETIC
               OR flag = "True"
                   DISPLAY "Invalid input...Press return"
                   ACCEPT invalid-input
               ELSE 
                   MOVE string-input TO loan-term-years
                   IF loan-term-years < 0
                       DISPLAY "Invalid input...Press return"
                       ACCEPT invalid-input
                   ELSE 
                       MOVE loan-term-years 
                       TO z-loan-term-years
                   END-IF
               END-IF
               MOVE "False" TO flag
           END-PERFORM.

      *    Convert the years into months
           COMPUTE loan-term-months = loan-term-years * 12
           MOVE loan-term-months TO z-number-of-monthly *> assigns value to displaying variables
           
      *    If the user's choice of type of loan is 'C', this will be executed
            IF loan-type-choice = 'C'

      *        While grace period is not a positive number, or not 
      *        numeric, this code will iterate until the condition is met 
               PERFORM UNTIL grace-period IS NUMERIC
               AND grace-period >= 0 
               AND grace-period <= 100
                   PERFORM ask-grace-period

                   PERFORM isPunct
                   IF string-input IS ALPHABETIC
                   OR flag = "True"
                       DISPLAY "Invalid input...Press return"
                       ACCEPT invalid-input
                   ELSE 
                       MOVE string-input TO grace-period
                       IF grace-period < 0
                           DISPLAY "Invalid input...Press return"
                           ACCEPT invalid-input
                       ELSE 
                           MOVE grace-period 
                           TO z-grace-period
                       END-IF
                   END-IF
                   MOVE "False" TO flag
               END-PERFORM
           END-IF.

      *    If the user's choice of type of loan is 'D', this will be executed
           IF loan-type-choice = 'D'

      *        While grace period is not a positive number, or not 
      *        numeric, this code will iterate until the condition is met 
               PERFORM UNTIL balloon-rate >= 0
               AND balloon-rate <= 100
                   PERFORM ask-balloon-rate

                   PERFORM isPunct
                   IF string-input IS ALPHABETIC
                   OR flag = "True"
                       DISPLAY "Invalid input...Press return"
                       ACCEPT invalid-input
                   ELSE 
                       MOVE string-input TO balloon-rate
                       IF balloon-rate < 0
                           DISPLAY "Invalid input...Press return"
                           ACCEPT invalid-input
                       ELSE 
                           MOVE balloon-rate 
                           TO z-balloon-rate
                       END-IF
                   END-IF
                   MOVE "False" TO flag
     
               END-PERFORM
           END-IF.
      *    Convert ballon rate to decimal for computations
           COMPUTE balloon-rate = balloon-rate / 100
           EXIT.

      * This will ask the loan amount
       ask-loan-amount.
           CALL "SYSTEM" USING clear-command
           PERFORM greet-user
           PERFORM design
           DISPLAY "Loan Amount: " WITH NO ADVANCING
           ACCEPT string-input
      *    MOVE loan-amount TO z-loan-amount
           EXIT.


      * This will ask the annual interest rate
       ask-annual-interest-rate.
           CALL "SYSTEM" USING clear-command
           PERFORM greet-user
           PERFORM design
           DISPLAY "Loan Amount: " peso-symbol z-loan-amount
           DISPLAY "Annual Interest Rate (%): " WITH NO ADVANCING
           ACCEPT string-input
      *    MOVE annual-interest-rate to z-annual-interest-rate
           EXIT.


      * This will ask the loan term years
       ask-loan-term-years.
           CALL "SYSTEM" USING clear-command
           PERFORM greet-user
           PERFORM design
           DISPLAY "Loan Amount: " peso-symbol z-loan-amount
           DISPLAY "Annual Interest Rate (%): " z-annual-interest-rate
           DISPLAY "Loan Term (years): " WITH NO ADVANCING
           ACCEPT string-input
      *    MOVE loan-term-years TO z-loan-term-years
           EXIT.


      * This will ask the balloon rate
       ask-balloon-rate.
           CALL "SYSTEM" USING clear-command
           PERFORM greet-user
           PERFORM design
           DISPLAY "Loan Amount: " peso-symbol z-loan-amount
           DISPLAY "Annual Interest Rate (%): " z-annual-interest-rate
           DISPLAY "Loan Term (years): " z-loan-term-years
           DISPLAY "Balloon Payment (%): " WITH NO ADVANCING
           ACCEPT string-input
      *    MOVE balloon-rate TO z-balloon-rate
           EXIT.


      * This will ask the grace period
       ask-grace-period.
           CALL "SYSTEM" USING clear-command
           PERFORM greet-user
           PERFORM design
           DISPLAY "Loan Amount: " peso-symbol z-loan-amount
           DISPLAY "Annual Interest Rate (%): " z-annual-interest-rate
           DISPLAY "Loan Term (years): " z-loan-term-years
           DISPLAY "Grace Period: " WITH NO ADVANCING
           ACCEPT string-input
      *    MOVE grace-period TO z-grace-period
           EXIT.


      * This will print the header of the table
       headers.
           PERFORM design
           DISPLAY "|" period-header col-space "|" WITH NO ADVANCING
      *    DISPLAY date-header col-space "|" WITH NO ADVANCING
           DISPLAY principal-header col-space "|" WITH NO ADVANCING
           DISPLAY interest-header col-space "|" WITH NO ADVANCING
           DISPLAY payments-header col-space "|" WITH NO ADVANCING
           DISPLAY balance-header "|"
           PERFORM design
           EXIT.


      * This will print the summary of loan
       loan-summary.
           CALL "SYSTEM" USING clear-command
           PERFORM design
           DISPLAY "LOAN SUMMARY"
           PERFORM design

           IF loan-type-choice = 'A'
               DISPLAY "Loan Type: Fixed Equal Amortization Case"
           ELSE IF loan-type-choice = 'B'
               DISPLAY "Loan Type: Fixed Principal Amortization Case"
           ELSE IF loan-type-choice = 'C'
               DISPLAY "Loan Type: Fixed Equal Amortization Case with "
      -                "Grace Period"
           ELSE IF loan-type-choice = 'D'
               DISPLAY "Loan Type: Periodic Interest Payment, "
      -                "Balloon Payment at Maturity"
           ELSE
               DISPLAY "N/A"
           END-IF.

           DISPLAY "Loan Amount: " peso-symbol z-loan-amount
           DISPLAY "Annual Interest: " z-annual-interest-rate "%"
           DISPLAY "No. of Monthly Installments: " z-number-of-monthly
                      
           IF loan-type-choice = 'C'
               DISPLAY "Grace Period: " z-grace-period
           ELSE IF loan-type-choice = 'D'
               DISPLAY "Balloon Payment (%): " z-balloon-rate "%"
           END-IF.
           
           PERFORM design
           PERFORM design
           DISPLAY "AMORTIZATION SCHEDULE"
              EXIT.

       
      * This will print the zero period inside the table before printing 
      * the computed values in regards to user's inputted loan details
       table-initial-value.
           MOVE 000 TO period-value
           MOVE 0 TO principal-value
           MOVE 0 TO interest-value
           MOVE 0 TO total-payments-value
           MOVE loan-amount TO balance-value
           
           DISPLAY "| " period-value col-space "| " 
      -    WITH NO ADVANCING
      *    DISPLAY date-value col-space "|" WITH NO ADVANCING
           DISPLAY peso-symbol principal-value col-space "| " 
      -    WITH NO ADVANCING
           DISPLAY peso-symbol interest-value col-space "| " 
      -    WITH NO ADVANCING
           DISPLAY peso-symbol total-payments-value col-space "| " 
      -    WITH NO ADVANCING
           DISPLAY peso-symbol balance-value "|"
           PERFORM design
           EXIT.

      * This will print the computed values inside the table
       table-values.
           DISPLAY "| " period-value col-space "| " 
      -    WITH NO ADVANCING
      *    DISPLAY date-value col-space "|" WITH NO ADVANCING
           DISPLAY peso-symbol principal-value col-space "| " 
      -    WITH NO ADVANCING
           DISPLAY peso-symbol interest-value col-space "| " 
      -    WITH NO ADVANCING
           DISPLAY peso-symbol total-payments-value col-space "| " 
      -    WITH NO ADVANCING
           DISPLAY peso-symbol balance-value "|"
           PERFORM design
           EXIT.


      * This will convert the computed values to displaying values
       table-value-assignment.
           MOVE installment-period TO period-value
           MOVE principal-payment TO principal-value
           MOVE interest-payment TO interest-value
           MOVE equated-monthly-installment TO total-payments-value
           MOVE amount-payable TO balance-value
           MOVE total-principal TO principal-footer
           MOVE total-interest TO interest-footer
           MOVE total-payments TO payments-footer
           EXIT.


      * This assigns the user's loan details to the designated variables 
      * for computations.
       input-assignment.
           MOVE loan-amount TO principal
           MOVE loan-amount TO remaining-balance
           MOVE monthly-interest-rate TO rate
           MOVE loan-term-months TO number-of-payments
           EXIT.

      * This prints the footer section
       footer-and-values.
           DISPLAY period-footer col-space space-value "   "
      -    WITH NO ADVANCING
           DISPLAY peso-symbol principal-footer col-space space-value"" 
      -    WITH NO ADVANCING
           DISPLAY peso-symbol interest-footer col-space space-value""
      -    WITH NO ADVANCING
           DISPLAY peso-symbol payments-footer col-space space-value""
      -    WITH NO ADVANCING
           DISPLAY balance-footer
           DISPLAY ""
           EXIT.

      * Checks if input is equal to any of these characters
       isPunct.
           IF string-input = ','
           OR string-input = '?'
           OR string-input = '!'
           OR string-input = '.'
           OR string-input = '-'
           OR string-input = '+'
           OR string-input = '='
           OR string-input = '\'
               MOVE "True" TO flag 
           END-IF
           EXIT.


      * This will greet the user after inputting the username
       greet-user.
      * Greets the user
           MOVE FUNCTION UPPER-CASE(customer-name) TO customer-name
           DISPLAY "HELLO " WITH NO ADVANCING
           DISPLAY FUNCTION TRIM(customer-name TRAILING) "!"
           EXIT.

       

      * This will clear the values after being used
       clear-values.
           MOVE 0 TO installment-period
           MOVE -1 TO loan-amount
           MOVE 0 TO annual-interest-rate
           MOVE 0 TO loan-term-years
           MOVE 0 TO balloon-payment
           MOVE -1 TO balloon-rate
           MOVE 0 TO total-interest
           MOVE 0 TO total-payments
           MOVE 0 TO total-principal
           MOVE -1 TO grace-period
           EXIT.


      * This wil print horizontal dashed lines in the terminal ('-')
       design.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > n
               DISPLAY "-" WITH NO ADVANCING
           END-PERFORM.
           DISPLAY ""
           EXIT.

      * This will exit the program if the user choose 'E'
       exit-program.
           CALL "SYSTEM" USING clear-command
           DISPLAY "Exiting...press return"
           ACCEPT invalid-input
           CALL "SYSTEM" USING clear-command
           DISPLAY "Bye!"
           STOP RUN.

       END PROGRAM loan-calculator.









