# CardDemo Business Calculation Rules Analysis

## Executive Summary

This document presents the results of a comprehensive analysis of all COBOL files in the CardDemo application to identify business-level calculation rules. After systematically examining 18 COBOL programs, only one business calculation rule was identified.

## Analysis Methodology

The analysis focused exclusively on business-level calculations while excluding:
- Input validation rules
- Technical or infrastructure logic (file handling, cursor processing, screen layout)
- Error handling and database connection logic
- I/O operations and structure definitions
- Technical routines that don't impact business decisions

## Files Analyzed

The following 18 COBOL files were systematically examined:

1. **COBIL00C.cbl** - Bill payment processing (Contains business rule)
2. **COACTUPC.cbl** - Account update functionality
3. **COACTVWC.cbl** - Account view functionality
4. **COADM01C.cbl** - Admin menu navigation
5. **COCRDLIC.cbl** - Credit card listing
6. **COCRDSLC.cbl** - Credit card detail view
7. **COCRDUPC.cbl** - Credit card update functionality
8. **COMEN01C.cbl** - Main menu for regular users
9. **CORPT00C.cbl** - Report generation via batch jobs
10. **COSGN00C.cbl** - User sign-on screen
11. **COTRN00C.cbl** - Transaction listing
12. **COTRN01C.cbl** - Transaction detail view
13. **COTRN02C.cbl** - New transaction entry
14. **COUSR00C.cbl** - User listing functionality
15. **COUSR01C.cbl** - User creation
16. **COUSR02C.cbl** - User updates
17. **COUSR03C.cbl** - User deletion
18. **CSUTLDTC.cbl** - Date validation utility

## Business Calculation Rules Identified

### RULE-CALC-001: Bill Payment Balance Calculation

**Rule Description:** When a customer makes a bill payment, the account balance is reduced by the payment amount to reflect the transaction.

**COBOL Source Location:** COBIL00C.cbl, line 234

**Involved Variables:**
- `ACCT-CURR-BAL` - Current account balance
- `TRAN-AMT` - Transaction amount (payment amount)

**Input Conditions:**
- Valid payment amount entered by user
- Account exists and is accessible
- Payment amount is greater than zero

**Calculation Logic:**
```
New Account Balance = Current Account Balance - Payment Amount
ACCT-CURR-BAL = ACCT-CURR-BAL - TRAN-AMT
```

**Business Impact:** This is the core financial transaction logic that ensures customer payments are properly reflected in their account balance, maintaining accurate financial records.

## Analysis Findings

### Application Architecture
The CardDemo application is primarily designed as a demonstration of CICS transaction processing patterns rather than a complex business calculation engine. The system focuses on:

- **User Interface Management** - Screen navigation, input handling, and display formatting
- **Data Management** - CRUD operations on VSAM files (ACCTDAT, CUSTDAT, CARDDAT, TRANSACT, USRSEC)
- **Authentication & Authorization** - User sign-on and role-based access control
- **Administrative Functions** - User management and system administration

### Why Only One Business Rule?
The limited number of business calculation rules reflects the application's purpose as a demonstration system. Most financial calculations that would typically exist in a production banking system (such as interest calculations, fee computations, credit limit adjustments, or complex financial formulas) are not implemented in this demo application.

### Technical vs Business Logic
The majority of the code consists of:
- Input validation and error handling
- File I/O operations with VSAM datasets
- Screen management using BMS (Basic Mapping Support)
- Navigation between different functional modules
- Data formatting and display logic

## Recommendations for Modernization

When modernizing this application, consider that:

1. **Limited Business Logic** - The core business logic is minimal, making migration straightforward
2. **Focus on Architecture** - Emphasis should be on modernizing the technical architecture rather than complex business rule migration
3. **Extensibility** - The modernized system should be designed to accommodate additional business rules that may be needed in a production environment

## Conclusion

The CardDemo application contains only one identifiable business calculation rule related to bill payment processing. This reflects its nature as a demonstration system focused on CICS transaction processing patterns rather than complex financial calculations. The single rule identified is fundamental to the payment functionality and represents the core business logic that must be preserved during modernization efforts.
