# COBOL Business Calculation Rules Analysis

## Overview

This document contains the extracted business-level calculation rules from the CardDemo CICS application. The analysis focused exclusively on business logic that impacts financial calculations, data transformations, and business decisions, excluding all technical infrastructure, validation, and I/O operations.

## Business Calculation Rules

### RULE-CALC-001: Bill Payment Balance Calculation

**Rule ID:** RULE-CALC-001

**Rule Description:** Updates account balance by subtracting the payment amount from the current account balance during bill payment processing.

**COBOL Source Location:** COBIL00C.cbl, line 234 (approximate location in balance update logic)

**Involved Variables:**
- `ACCT-CURR-BAL` (Account current balance)
- `TRAN-AMT` (Transaction/payment amount)
- `WS-EDIT-CURRENCY-9-2` (Working storage for currency calculations)

**Input Conditions:**
- Payment amount must be greater than zero
- Account must exist and be accessible
- Transaction must be confirmed by user

**Calculation Logic/Formula:**
```
NEW_ACCOUNT_BALANCE = CURRENT_ACCOUNT_BALANCE - PAYMENT_AMOUNT
```

---

### RULE-CALC-002: Transaction ID Generation

**Rule ID:** RULE-CALC-002

**Rule Description:** Generates sequential transaction IDs by reading the highest existing transaction ID and incrementing by 1.

**COBOL Source Location:** COTRN02C.cbl, lines 444-451

**Involved Variables:**
- `TRAN-ID` (Transaction identifier)
- `WS-TRAN-ID-N` (Working storage for numeric transaction ID)
- `HIGH-VALUES` (COBOL constant for maximum value)

**Input Conditions:**
- Transaction file must be accessible
- Previous transactions must exist to determine next ID

**Calculation Logic/Formula:**
```
NEXT_TRANSACTION_ID = MAX_EXISTING_TRANSACTION_ID + 1
```

---

### RULE-CALC-003: Transaction Amount Conversion

**Rule ID:** RULE-CALC-003

**Rule Description:** Converts user-entered transaction amounts from character format to numeric format for processing and storage.

**COBOL Source Location:** COTRN02C.cbl, lines 383-386 and 456-458

**Involved Variables:**
- `TRNAMTI` (Transaction amount input from screen)
- `WS-TRAN-AMT-N` (Numeric working storage for amount)
- `WS-TRAN-AMT-E` (Edited amount for display)
- `TRAN-AMT` (Final transaction amount for storage)

**Input Conditions:**
- Transaction amount input must not be spaces or low-values
- Amount must be in valid numeric format

**Calculation Logic/Formula:**
```
NUMERIC_AMOUNT = FUNCTION NUMVAL-C(CHARACTER_AMOUNT_INPUT)
DISPLAY_AMOUNT = NUMERIC_AMOUNT (formatted for screen display)
STORAGE_AMOUNT = NUMERIC_AMOUNT (for database storage)
```

---

### RULE-CALC-004: Date Validation Processing

**Rule ID:** RULE-CALC-004

**Rule Description:** Validates date formats and determines if dates are valid using IBM CEEDAYS API, returning severity codes for business logic decisions.

**COBOL Source Location:** CSUTLDTC.cbl, entire program (lines 1-158)

**Involved Variables:**
- `CSUTLDTC-DATE` (Input date to validate)
- `CSUTLDTC-DATE-FORMAT` (Expected date format)
- `CSUTLDTC-RESULT` (Validation result structure)
- `CSUTLDTC-RESULT-SEV-CD` (Severity code: '0000' = valid)

**Input Conditions:**
- Date must be provided in expected format
- Date format specification must be valid

**Calculation Logic/Formula:**
```
IF CEEDAYS_API_CALL_SUCCESSFUL AND SEVERITY_CODE = '0000' THEN
    DATE_IS_VALID = TRUE
ELSE
    DATE_IS_VALID = FALSE
```

---

### RULE-CALC-005: Page Number Calculation for Data Navigation

**Rule ID:** RULE-CALC-005

**Rule Description:** Calculates page numbers for data pagination in list screens, managing forward and backward navigation through record sets.

**COBOL Source Location:** Multiple files (COUSR00C.cbl lines 309-323, COCRDLIC.cbl pagination logic)

**Involved Variables:**
- `CDEMO-CU00-PAGE-NUM` (Current page number)
- `WS-IDX` (Record index counter)
- `NEXT-PAGE-YES/NO` (Page availability flags)

**Input Conditions:**
- Records must be available for pagination
- Current page position must be tracked
- Maximum records per page = 10

**Calculation Logic/Formula:**
```
FORWARD_NAVIGATION:
    IF MORE_RECORDS_AVAILABLE THEN
        NEW_PAGE_NUMBER = CURRENT_PAGE_NUMBER + 1
    
BACKWARD_NAVIGATION:
    IF CURRENT_PAGE_NUMBER > 1 THEN
        NEW_PAGE_NUMBER = CURRENT_PAGE_NUMBER - 1
```

---

## Rule Dependencies and Relationships

The business rules have the following relationships:

1. **RULE-CALC-003** (Transaction Amount Conversion) feeds into **RULE-CALC-001** (Bill Payment Balance Calculation)
   - The converted numeric amount is used in the balance calculation

2. **RULE-CALC-004** (Date Validation) is used by transaction processing rules
   - Date validation ensures transaction dates are valid before processing

3. **RULE-CALC-002** (Transaction ID Generation) operates independently
   - Provides unique identifiers for all transaction records

4. **RULE-CALC-005** (Page Number Calculation) operates independently
   - Manages user interface navigation without affecting business calculations

## Excluded Logic

The following types of logic were intentionally excluded from this analysis as they represent technical infrastructure rather than business calculations:

- Input validation routines (field format checking, required field validation)
- File I/O operations (CICS READ, WRITE, DELETE commands)
- Screen handling and BMS operations
- Error handling and exception processing
- Database connection and cursor management
- User interface layout and formatting
- Security and authentication logic
- Program flow control and navigation

## Summary

The CardDemo application contains relatively straightforward business calculation logic focused on:
- Basic financial arithmetic (balance updates)
- Sequential ID generation for data integrity
- Data type conversions for processing
- Date validation for business rule compliance
- Simple pagination mathematics for user interface

The business rules are well-contained and follow standard mainframe COBOL patterns for transaction processing systems.
