# COBOL Business Calculation Rules Analysis

## Overview
This document contains the business-level calculation rules extracted from the CardDemo COBOL application. The analysis focused exclusively on business-relevant calculations such as financial computations, excluding technical logic like input validation, file handling, and UI operations.

## Business Calculation Rules

### RULE-CALC-001: Bill Payment Balance Calculation

**Rule ID**: RULE-CALC-001

**Rule Description**: Calculates the new account balance after processing a bill payment by subtracting the transaction amount from the current account balance.

**COBOL Source Location**: 
- Program: COBIL00C.cbl
- Line: 234-235

**Involved Variables**:
- `ACCT-CURR-BAL` - Current account balance (PIC S9(10)V99)
- `TRAN-AMT` - Transaction amount for bill payment (PIC S9(9)V99)

**Input Conditions**:
- User confirms payment (`CONF-PAY-YES` is true)
- Account has a positive balance (`ACCT-CURR-BAL > ZEROS`)
- Valid account ID provided and found in system

**Calculation Logic/Formula**:
```cobol
COMPUTE ACCT-CURR-BAL = ACCT-CURR-BAL - TRAN-AMT
```

**Business Context**: This rule implements the core financial logic for online bill payments, ensuring that when a customer pays their full account balance, the system correctly updates the account to reflect the payment by reducing the balance to zero.

## Analysis Summary

**Total Files Analyzed**: 18 COBOL programs
**Business Calculation Rules Found**: 1
**Technical/Infrastructure Logic Excluded**: Input validation, file I/O operations, screen handling, menu navigation, user management, report generation utilities

## Files Analyzed
- COBIL00C.cbl (Bill Payment Processing) ✓ - Contains business rule
- COACTUPC.cbl (Account Updates) - Data validation only
- COCRDUPC.cbl (Credit Card Updates) - Data validation only  
- COTRN00C.cbl (Transaction Listing) - Display logic only
- COTRN01C.cbl (Transaction View) - Display logic only
- COTRN02C.cbl (Transaction Entry) - Input validation only
- COACTVWC.cbl (Account View) - Display logic only
- COCRDLIC.cbl (Credit Card List) - Display logic only
- COCRDSLC.cbl (Credit Card Details) - Display logic only
- COMEN01C.cbl (Main Menu) - Navigation logic only
- COADM01C.cbl (Admin Menu) - Navigation logic only
- COSGN00C.cbl (Sign-on) - Authentication logic only
- COUSR00C.cbl (User List) - Display logic only
- COUSR01C.cbl (User Add) - Data validation only
- COUSR02C.cbl (User Update) - Data validation only
- COUSR03C.cbl (User Delete) - Data validation only
- CORPT00C.cbl (Report Generation) - Date processing only
- CSUTLDTC.cbl (Date Utility) - Technical utility only
