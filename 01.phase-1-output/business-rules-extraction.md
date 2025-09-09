# COBOL Business Rules Extraction Report

## Overview
This document contains business-level calculation rules extracted from the CardDemo COBOL application. Only business logic rules are included - validation, technical infrastructure, and UI handling logic have been excluded.

## Business Rules

### RULE-CALC-001: Bill Payment Balance Calculation
**Rule Description:** Calculate new account balance after bill payment by subtracting transaction amount from current balance
**COBOL Source Location:** COBIL00C.cbl, line 234
**Involved Variables:** 
- ACCT-CURR-BAL (current account balance)
- TRAN-AMT (transaction amount)
**Input Conditions:** 
- CONF-PAY-YES (payment confirmed)
- ACCT-CURR-BAL > ZEROS (account has balance to pay)
**Calculation Logic:** 
```
COMPUTE ACCT-CURR-BAL = ACCT-CURR-BAL - TRAN-AMT
```

### RULE-CALC-002: Transaction ID Generation
**Rule Description:** Generate sequential transaction ID by reading highest existing ID and adding 1
**COBOL Source Location:** COBIL00C.cbl, lines 216-217; COTRN02C.cbl, lines 448-449
**Involved Variables:**
- TRAN-ID (transaction identifier)
- WS-TRAN-ID-NUM (working storage for ID calculation)
**Input Conditions:** 
- Transaction file accessible
- HIGH-VALUES used to position at end of file
**Calculation Logic:**
```
MOVE TRAN-ID TO WS-TRAN-ID-NUM
ADD 1 TO WS-TRAN-ID-NUM
```

### RULE-CALC-003: Transaction Amount Processing
**Rule Description:** Convert and validate transaction amount from input format to numeric format
**COBOL Source Location:** COTRN02C.cbl, lines 383-386, 456-458
**Involved Variables:**
- TRNAMTI (input transaction amount)
- WS-TRAN-AMT-N (numeric transaction amount)
- WS-TRAN-AMT-E (edited transaction amount)
**Input Conditions:**
- Amount format: -99999999.99
- First character must be '+' or '-'
- Decimal point at position 10
**Calculation Logic:**
```
COMPUTE WS-TRAN-AMT-N = FUNCTION NUMVAL-C(TRNAMTI OF COTRN2AI)
MOVE WS-TRAN-AMT-N TO TRAN-AMT
```

### RULE-CALC-004: Monthly Report Date Range Calculation
**Rule Description:** Calculate start and end dates for monthly transaction reports
**COBOL Source Location:** CORPT00C.cbl, lines 217-236
**Involved Variables:**
- WS-START-DATE (report start date)
- WS-END-DATE (report end date)
- WS-CURDATE-YEAR, WS-CURDATE-MONTH (current date components)
**Input Conditions:**
- Monthly report option selected
- Current date available
**Calculation Logic:**
```
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

### RULE-CALC-005: Page Number Calculation for Listings
**Rule Description:** Calculate page numbers for transaction and user listings with forward/backward navigation
**COBOL Source Location:** COTRN00C.cbl, lines 306-318; COUSR00C.cbl, lines 309-322
**Involved Variables:**
- CDEMO-CT00-PAGE-NUM (current page number)
- WS-IDX (record counter)
**Input Conditions:**
- Records available to display
- Page navigation requested (PF7/PF8)
**Calculation Logic:**
```
COMPUTE CDEMO-CT00-PAGE-NUM = CDEMO-CT00-PAGE-NUM + 1
COMPUTE WS-IDX = WS-IDX + 1
SUBTRACT 1 FROM CDEMO-CT00-PAGE-NUM
```

### RULE-CALC-006: Date Validation and Processing
**Rule Description:** Validate and process date inputs for custom report date ranges
**COBOL Source Location:** CORPT00C.cbl, lines 305-327
**Involved Variables:**
- SDTMMI, SDTDDI, SDTYYYYI (start date components)
- EDTMMI, EDTDDI, EDTYYYYI (end date components)
- WS-NUM-99, WS-NUM-9999 (numeric conversion variables)
**Input Conditions:**
- Custom report option selected
- Date components provided
**Calculation Logic:**
```
COMPUTE WS-NUM-99 = FUNCTION NUMVAL-C(SDTMMI OF CORPT0AI)
COMPUTE WS-NUM-9999 = FUNCTION NUMVAL-C(SDTYYYYI OF CORPT0AI)
```

## Rule Dependencies

The following dependencies exist between business rules:

1. **RULE-CALC-001** depends on **RULE-CALC-002**: Bill payment requires transaction ID generation
2. **RULE-CALC-001** depends on **RULE-CALC-003**: Bill payment uses transaction amount processing
3. **RULE-CALC-004** depends on **RULE-CALC-006**: Monthly reports use date processing logic
4. **RULE-CALC-005** is independent but used by transaction and user listing functions

## Summary

Total Business Rules Identified: 6
- Financial Calculations: 3 (RULE-CALC-001, RULE-CALC-002, RULE-CALC-003)
- Date/Time Calculations: 2 (RULE-CALC-004, RULE-CALC-006)
- Navigation/Pagination: 1 (RULE-CALC-005)

These rules represent the core business logic for the CardDemo application's financial processing, reporting, and user interface navigation capabilities.
