# COBOL Business Rules Extraction

## Overview
This document contains business-level calculation rules extracted from the CardDemo COBOL application. These rules focus on financial calculations, business logic, and conditional decisions that impact business operations.

## Business Rules

### RULE-CALC-001: Bill Payment Balance Update
**Rule Description**: Core financial calculation that updates account balance when processing bill payments
**COBOL Source Location**: COBIL00C.cbl, lines 234-235
**Involved Variables**: 
- ACCT-CURR-BAL (Account Current Balance)
- TRAN-AMT (Transaction Amount)
**Input Conditions**: 
- IF CONF-PAY-YES (Payment confirmed)
- Account balance > 0
**Calculation Logic**: 
```
COMPUTE ACCT-CURR-BAL = ACCT-CURR-BAL - TRAN-AMT
```
The account current balance is reduced by the transaction amount for bill payments.

### RULE-CALC-002: Transaction ID Sequential Generation
**Rule Description**: Business rule for generating unique sequential transaction IDs
**COBOL Source Location**: COBIL00C.cbl, lines 216-217; COTRN02C.cbl, lines 448-449
**Involved Variables**:
- TRAN-ID (Transaction ID)
- WS-TRAN-ID-N (Working storage transaction ID number)
**Input Conditions**: 
- When creating new transactions (bill payments or manual transactions)
**Calculation Logic**:
```
MOVE TRAN-ID TO WS-TRAN-ID-N
ADD 1 TO WS-TRAN-ID-N
```
Sequential increment of transaction ID to ensure uniqueness.

### RULE-CALC-003: Monthly Report Date Range Calculation
**Rule Description**: Calculates start and end dates for monthly transaction reports
**COBOL Source Location**: CORPT00C.cbl, lines 217-236
**Involved Variables**:
- WS-START-DATE-YYYY, WS-START-DATE-MM, WS-START-DATE-DD
- WS-END-DATE-YYYY, WS-END-DATE-MM, WS-END-DATE-DD
- WS-CURDATE-YEAR, WS-CURDATE-MONTH, WS-CURDATE-DAY
**Input Conditions**: 
- WHEN MONTHLYI OF CORPT0AI NOT = SPACES AND LOW-VALUES
**Calculation Logic**:
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
Sets start date to first day of current month, calculates end date as last day of current month.

### RULE-CALC-004: Yearly Report Date Range Calculation  
**Rule Description**: Calculates start and end dates for yearly transaction reports
**COBOL Source Location**: CORPT00C.cbl, lines 243-253
**Involved Variables**:
- WS-START-DATE-YYYY, WS-START-DATE-MM, WS-START-DATE-DD
- WS-END-DATE-YYYY, WS-END-DATE-MM, WS-END-DATE-DD
- WS-CURDATE-YEAR
**Input Conditions**:
- WHEN YEARLYI OF CORPT0AI NOT = SPACES AND LOW-VALUES
**Calculation Logic**:
```
MOVE WS-CURDATE-YEAR TO WS-START-DATE-YYYY, WS-END-DATE-YYYY
MOVE '01' TO WS-START-DATE-MM, WS-START-DATE-DD
MOVE '12' TO WS-END-DATE-MM
MOVE '31' TO WS-END-DATE-DD
```
Sets date range from January 1st to December 31st of current year.

### RULE-CALC-005: Transaction Amount Numeric Conversion
**Rule Description**: Converts and validates transaction amounts from input format
**COBOL Source Location**: COTRN02C.cbl, lines 383-386; lines 456-458
**Involved Variables**:
- TRNAMTI OF COTRN2AI (Transaction Amount Input)
- WS-TRAN-AMT-N (Numeric Transaction Amount)
- TRAN-AMT (Transaction Record Amount)
**Input Conditions**:
- During transaction entry and validation
**Calculation Logic**:
```
COMPUTE WS-TRAN-AMT-N = FUNCTION NUMVAL-C(TRNAMTI OF COTRN2AI)
MOVE WS-TRAN-AMT-N TO TRAN-AMT
```
Converts character-based amount input to numeric format for processing.

### RULE-CALC-006: Date Validation Using CEEDAYS API
**Rule Description**: Business rule for validating dates using IBM CEEDAYS API
**COBOL Source Location**: CSUTLDTC.cbl, lines 116-149
**Involved Variables**:
- WS-DATE-TO-TEST (Input date)
- WS-DATE-FORMAT (Date format)
- OUTPUT-LILLIAN (Lillian date output)
- FEEDBACK-CODE (API response)
**Input Conditions**:
- When date validation is required across the application
**Calculation Logic**:
```
CALL "CEEDAYS" USING WS-DATE-TO-TEST, WS-DATE-FORMAT, OUTPUT-LILLIAN, FEEDBACK-CODE
EVALUATE TRUE
    WHEN FC-INVALID-DATE -> 'Date is valid'
    WHEN FC-BAD-DATE-VALUE -> 'Datevalue error'
    WHEN FC-INVALID-MONTH -> 'Invalid month'
    [other validation conditions]
END-EVALUATE
```
Uses IBM CEEDAYS API to validate date formats and values for business processing.

## Rule Dependencies

The following dependencies exist between business rules:

1. **RULE-CALC-001** (Bill Payment Balance Update) depends on **RULE-CALC-002** (Transaction ID Generation) - Bill payments require unique transaction IDs
2. **RULE-CALC-005** (Transaction Amount Conversion) feeds into **RULE-CALC-001** (Bill Payment Balance Update) - Amount must be converted before balance calculation
3. **RULE-CALC-006** (Date Validation) is used by **RULE-CALC-003** and **RULE-CALC-004** (Report Date Calculations) - Date ranges must be validated
4. **RULE-CALC-002** (Transaction ID Generation) is used by **RULE-CALC-005** (Transaction Amount Processing) - Manual transactions also need unique IDs

## Summary

Total Business Rules Identified: 6
- 2 Financial calculation rules (balance updates, amount processing)
- 2 Date/time calculation rules (monthly/yearly ranges)
- 1 ID generation rule (sequential numbering)
- 1 Date validation rule (business date validation)

These rules represent the core business logic that should be preserved during modernization efforts.
