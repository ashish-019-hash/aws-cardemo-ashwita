# CardDemo COBOL Business Rules Analysis

## Overview

This document contains business-level calculation rules extracted from the CardDemo COBOL application. The analysis focused exclusively on business calculations, financial formulas, and conditional business logic while excluding technical infrastructure, validation routines, and I/O operations.

**Analysis Scope:**
- 18 COBOL files in the 00.phase-1-input folder
- Focus on loan/premium calculations, tax/interest formulas, conditional business decisions, and aggregation/financial rules
- Exclusion of input validation, file handling, screen layout, error handling, and database operations

## Business Rules

### RULE-CALC-001: Bill Payment Balance Calculation
- **Rule Description**: Updates account current balance by subtracting the full payment amount when processing online bill payments
- **COBOL Source Location**: COBIL00C.cbl, line 234
- **Involved Variables**: 
  - ACCT-CURR-BAL (Account Current Balance)
  - TRAN-AMT (Transaction Amount)
- **Input Conditions**: 
  - Payment confirmation received (CONF-PAY-YES)
  - Valid account with existing balance
  - Transaction amount equals current account balance
- **Calculation Logic**: 
  ```cobol
  COMPUTE ACCT-CURR-BAL = ACCT-CURR-BAL - TRAN-AMT
  ```

### RULE-CALC-002: Transaction ID Generation
- **Rule Description**: Generates unique sequential transaction IDs by finding the highest existing transaction ID and incrementing by 1
- **COBOL Source Location**: 
  - COTRN02C.cbl, lines 448-449 (Add Transaction)
  - COBIL00C.cbl, lines 216-217 (Bill Payment)
- **Involved Variables**:
  - WS-TRAN-ID-N (Working Storage Transaction ID Numeric)
  - TRAN-ID (Transaction ID)
- **Input Conditions**: New transaction creation (bill payment or regular transaction)
- **Calculation Logic**:
  ```cobol
  MOVE TRAN-ID TO WS-TRAN-ID-N
  ADD 1 TO WS-TRAN-ID-N
  MOVE WS-TRAN-ID-N TO TRAN-ID
  ```

### RULE-CALC-003: Transaction Amount Format Conversion
- **Rule Description**: Converts user-entered transaction amounts from character format to numeric format for processing and storage
- **COBOL Source Location**: COTRN02C.cbl, lines 383-386 and 456-458
- **Involved Variables**:
  - TRNAMTI (Transaction Amount Input - Character)
  - WS-TRAN-AMT-N (Working Storage Transaction Amount - Numeric)
  - TRAN-AMT (Transaction Amount - Final)
- **Input Conditions**: Valid numeric amount entered by user
- **Calculation Logic**:
  ```cobol
  COMPUTE WS-TRAN-AMT-N = FUNCTION NUMVAL-C(TRNAMTI OF COTRN2AI)
  MOVE WS-TRAN-AMT-N TO TRAN-AMT
  ```

### RULE-CALC-004: Monthly Report Date Range Calculation
- **Rule Description**: Calculates start and end dates for monthly transaction reports using current date and date arithmetic
- **COBOL Source Location**: CORPT00C.cbl, lines 217-236
- **Involved Variables**:
  - WS-CURDATE-YEAR, WS-CURDATE-MONTH, WS-CURDATE-DAY
  - WS-START-DATE, WS-END-DATE
  - WS-CURDATE-N (Numeric date for calculations)
- **Input Conditions**: Monthly report request selected
- **Calculation Logic**:
  ```cobol
  MOVE WS-CURDATE-YEAR TO WS-START-DATE-YYYY
  MOVE WS-CURDATE-MONTH TO WS-START-DATE-MM  
  MOVE '01' TO WS-START-DATE-DD
  ADD 1 TO WS-CURDATE-MONTH
  IF WS-CURDATE-MONTH > 12
      ADD 1 TO WS-CURDATE-YEAR
      MOVE 1 TO WS-CURDATE-MONTH
  END-IF
  COMPUTE WS-CURDATE-N = FUNCTION DATE-OF-INTEGER(
          FUNCTION INTEGER-OF-DATE(WS-CURDATE-N) - 1)
  ```

### RULE-CALC-005: Yearly Report Date Range Calculation  
- **Rule Description**: Sets date range for yearly transaction reports from January 1st to December 31st of current year
- **COBOL Source Location**: CORPT00C.cbl, lines 243-253
- **Involved Variables**:
  - WS-CURDATE-YEAR
  - WS-START-DATE, WS-END-DATE
- **Input Conditions**: Yearly report request selected
- **Calculation Logic**:
  ```cobol
  MOVE WS-CURDATE-YEAR TO WS-START-DATE-YYYY, WS-END-DATE-YYYY
  MOVE '01' TO WS-START-DATE-MM, WS-START-DATE-DD
  MOVE '12' TO WS-END-DATE-MM
  MOVE '31' TO WS-END-DATE-DD
  ```

### RULE-CALC-006: Page Number Calculation for Transaction Listing
- **Rule Description**: Calculates page numbers for paginated transaction displays with forward and backward navigation
- **COBOL Source Location**: COTRN00C.cbl, lines 306-308 and 317-318, 364
- **Involved Variables**:
  - CDEMO-CT00-PAGE-NUM (Current Page Number)
  - WS-IDX (Record Counter)
- **Input Conditions**: Transaction listing with pagination (PF7/PF8 keys)
- **Calculation Logic**:
  ```cobol
  COMPUTE CDEMO-CT00-PAGE-NUM = CDEMO-CT00-PAGE-NUM + 1
  SUBTRACT 1 FROM CDEMO-CT00-PAGE-NUM
  ```

### RULE-CALC-007: Screen Record Counter for Card Listing
- **Rule Description**: Counts and manages screen records for credit card listing pagination
- **COBOL Source Location**: COCRDLIC.cbl, lines 492, 508, 1163, 1178, 1284-1286, 1307
- **Involved Variables**:
  - WS-CA-SCREEN-NUM (Screen Number)
  - WS-SCRN-COUNTER (Screen Counter)
  - WS-MAX-SCREEN-LINES (Maximum Screen Lines = 7)
- **Input Conditions**: Credit card listing navigation (next/previous page)
- **Calculation Logic**:
  ```cobol
  ADD +1 TO WS-CA-SCREEN-NUM
  SUBTRACT 1 FROM WS-CA-SCREEN-NUM  
  ADD 1 TO WS-SCRN-COUNTER
  COMPUTE WS-SCRN-COUNTER = WS-MAX-SCREEN-LINES + 1
  SUBTRACT 1 FROM WS-SCRN-COUNTER
  ```

### RULE-CALC-008: Date Validation and Conversion
- **Rule Description**: Validates and converts date formats using CEEDAYS API for business date processing
- **COBOL Source Location**: CSUTLDTC.cbl, lines 116-124
- **Involved Variables**:
  - WS-DATE-TO-TEST (Input Date)
  - OUTPUT-LILLIAN (Lillian Date Format)
  - FEEDBACK-CODE (Validation Result)
- **Input Conditions**: Date input requiring validation and conversion
- **Calculation Logic**:
  ```cobol
  CALL "CEEDAYS" USING WS-DATE-TO-TEST, WS-DATE-FORMAT, 
                       OUTPUT-LILLIAN, FEEDBACK-CODE
  MOVE SEVERITY OF FEEDBACK-CODE TO WS-SEVERITY-N
  MOVE MSG-NO OF FEEDBACK-CODE TO WS-MSG-NO-N
  ```

## Business Rule Dependencies

The following dependencies exist between the identified business rules:

1. **RULE-CALC-002 → RULE-CALC-001**: Transaction ID Generation must complete before Bill Payment Balance Calculation can create the payment transaction record

2. **RULE-CALC-003 → RULE-CALC-001**: Transaction Amount Format Conversion must occur before the amount can be used in Bill Payment Balance Calculation

3. **RULE-CALC-008 → RULE-CALC-004, RULE-CALC-005**: Date Validation and Conversion supports the date range calculations for monthly and yearly reports

4. **RULE-CALC-006 → RULE-CALC-007**: Page Number Calculation logic is similar to Screen Record Counter logic for different listing screens

## Analysis Summary

**Total Business Rules Identified**: 8

**Rule Categories**:
- Financial Calculations: 3 rules (RULE-CALC-001, RULE-CALC-002, RULE-CALC-003)
- Date/Time Calculations: 3 rules (RULE-CALC-004, RULE-CALC-005, RULE-CALC-008)  
- Pagination/Navigation: 2 rules (RULE-CALC-006, RULE-CALC-007)

**Files with Business Logic**: 6 out of 18 files
- COBIL00C.cbl: Bill payment processing
- COTRN02C.cbl: Transaction addition
- CORPT00C.cbl: Report date calculations
- COTRN00C.cbl: Transaction listing pagination
- COCRDLIC.cbl: Card listing pagination
- CSUTLDTC.cbl: Date validation utility

**Files with No Business Calculations**: 12 files
- COMEN01C.cbl: Menu navigation only
- COADM01C.cbl: Administrative menu only
- COSGN00C.cbl: Sign-on processing only
- COUSR00C.cbl, COUSR01C.cbl, COUSR02C.cbl, COUSR03C.cbl: User management (CRUD operations only)
- COTRN01C.cbl: Transaction display only
- COACTUPC.cbl: Account update (validation only, no calculations)
- COACTVWC.cbl: Account view only
- COCRDUPC.cbl: Card update (validation only, no calculations)
- COCRDSLC.cbl: Card detail display only

This analysis confirms that the CardDemo application focuses primarily on data management and display operations, with core business calculations concentrated in payment processing, transaction management, and reporting functions.
