# Business Calculation Rules - CardDemo COBOL Application

## Overview
This document contains business-level calculation rules extracted from the CardDemo COBOL application. These rules represent core financial and business logic that impacts business decisions, excluding validation, I/O operations, and technical infrastructure.

## Business Calculation Rules

### RULE-CALC-001: Bill Payment Balance Update
- **Rule ID**: RULE-CALC-001
- **Rule Description**: Updates account current balance by subtracting the payment amount when processing bill payments
- **COBOL Source Location**: COBIL00C.cbl, line 234
- **Involved Variables**: 
  - `ACCT-CURR-BAL` (Account Current Balance)
  - `TRAN-AMT` (Transaction Amount)
- **Input Conditions**: 
  - Payment confirmation is received (`CONF-PAY-YES`)
  - Account balance is greater than zero
- **Calculation Logic**: `COMPUTE ACCT-CURR-BAL = ACCT-CURR-BAL - TRAN-AMT`

### RULE-CALC-002: Transaction ID Generation
- **Rule ID**: RULE-CALC-002
- **Rule Description**: Generates unique transaction ID by incrementing the highest existing transaction ID
- **COBOL Source Location**: COBIL00C.cbl, line 217; COTRN02C.cbl, line 449
- **Involved Variables**:
  - `WS-TRAN-ID-NUM` (Working Storage Transaction ID Number)
  - `TRAN-ID` (Transaction ID from file)
- **Input Conditions**: New transaction is being created
- **Calculation Logic**: `ADD 1 TO WS-TRAN-ID-NUM` (after reading highest existing ID)

### RULE-CALC-003: Transaction Amount Processing
- **Rule ID**: RULE-CALC-003
- **Rule Description**: Converts transaction amount from character format to numeric for processing
- **COBOL Source Location**: COTRN02C.cbl, lines 456-458
- **Involved Variables**:
  - `TRNAMTI OF COTRN2AI` (Transaction Amount Input)
  - `WS-TRAN-AMT-N` (Working Storage Transaction Amount Numeric)
  - `TRAN-AMT` (Transaction Amount in record)
- **Input Conditions**: Valid transaction amount is entered
- **Calculation Logic**: `COMPUTE WS-TRAN-AMT-N = FUNCTION NUMVAL-C(TRNAMTI OF COTRN2AI)` followed by `MOVE WS-TRAN-AMT-N TO TRAN-AMT`

### RULE-CALC-004: Credit Limit Calculation
- **Rule ID**: RULE-CALC-004
- **Rule Description**: Converts and validates credit limit amounts for account updates
- **COBOL Source Location**: COACTUPC.cbl, lines 1079-1080
- **Involved Variables**:
  - `ACRDLIMI OF CACTUPAI` (Credit Limit Input)
  - `ACUP-NEW-CREDIT-LIMIT-N` (New Credit Limit Numeric)
- **Input Conditions**: Credit limit field is not empty and passes numeric validation
- **Calculation Logic**: `COMPUTE ACUP-NEW-CREDIT-LIMIT-N = FUNCTION NUMVAL-C(ACRDLIMI OF CACTUPAI)`

### RULE-CALC-005: Cash Credit Limit Calculation
- **Rule ID**: RULE-CALC-005
- **Rule Description**: Converts and validates cash credit limit amounts for account updates
- **COBOL Source Location**: COACTUPC.cbl, lines 1093-1094
- **Involved Variables**:
  - `ACSHLIMI OF CACTUPAI` (Cash Limit Input)
  - `ACUP-NEW-CASH-CREDIT-LIMIT-N` (New Cash Credit Limit Numeric)
- **Input Conditions**: Cash limit field is not empty and passes numeric validation
- **Calculation Logic**: `COMPUTE ACUP-NEW-CASH-CREDIT-LIMIT-N = FUNCTION NUMVAL-C(ACSHLIMI OF CACTUPAI)`

### RULE-CALC-006: Current Balance Processing
- **Rule ID**: RULE-CALC-006
- **Rule Description**: Converts current balance from input format to numeric for account updates
- **COBOL Source Location**: COACTUPC.cbl, lines 1107-1108
- **Involved Variables**:
  - `ACURBALI OF CACTUPAI` (Current Balance Input)
  - `ACUP-NEW-CURR-BAL-N` (New Current Balance Numeric)
- **Input Conditions**: Current balance field is not empty and passes numeric validation
- **Calculation Logic**: `COMPUTE ACUP-NEW-CURR-BAL-N = FUNCTION NUMVAL-C(ACUP-NEW-CURR-BAL-X)`

### RULE-CALC-007: Current Cycle Credit Processing
- **Rule ID**: RULE-CALC-007
- **Rule Description**: Converts current cycle credit amount for account processing
- **COBOL Source Location**: COACTUPC.cbl, lines 1121-1122
- **Involved Variables**:
  - `ACRCYCRI OF CACTUPAI` (Current Cycle Credit Input)
  - `ACUP-NEW-CURR-CYC-CREDIT-N` (New Current Cycle Credit Numeric)
- **Input Conditions**: Current cycle credit field is not empty and passes numeric validation
- **Calculation Logic**: `COMPUTE ACUP-NEW-CURR-CYC-CREDIT-N = FUNCTION NUMVAL-C(ACRCYCRI OF CACTUPAI)`

### RULE-CALC-008: Current Cycle Debit Processing
- **Rule ID**: RULE-CALC-008
- **Rule Description**: Converts current cycle debit amount for account processing
- **COBOL Source Location**: COACTUPC.cbl, lines 1135-1136
- **Involved Variables**:
  - `ACRCYDBI OF CACTUPAI` (Current Cycle Debit Input)
  - `ACUP-NEW-CURR-CYC-DEBIT-N` (New Current Cycle Debit Numeric)
- **Input Conditions**: Current cycle debit field is not empty and passes numeric validation
- **Calculation Logic**: `COMPUTE ACUP-NEW-CURR-CYC-DEBIT-N = FUNCTION NUMVAL-C(ACRCYDBI OF CACTUPAI)`

### RULE-CALC-009: Bill Payment Amount Determination
- **Rule ID**: RULE-CALC-009
- **Rule Description**: Sets transaction amount equal to current account balance for full bill payment
- **COBOL Source Location**: COBIL00C.cbl, line 224
- **Involved Variables**:
  - `ACCT-CURR-BAL` (Account Current Balance)
  - `TRAN-AMT` (Transaction Amount)
- **Input Conditions**: Bill payment is confirmed and account has positive balance
- **Calculation Logic**: `MOVE ACCT-CURR-BAL TO TRAN-AMT`

### RULE-CALC-010: Balance Validation for Payment
- **Rule ID**: RULE-CALC-010
- **Rule Description**: Validates that account has a positive balance before allowing payment
- **COBOL Source Location**: COBIL00C.cbl, line 198
- **Involved Variables**:
  - `ACCT-CURR-BAL` (Account Current Balance)
- **Input Conditions**: Account ID is provided and valid
- **Calculation Logic**: `IF ACCT-CURR-BAL <= ZEROS` (business rule: no payment if balance is zero or negative)

## Rule Dependencies and Relationships

1. **Transaction Processing Flow**:
   - RULE-CALC-002 (Transaction ID Generation) → RULE-CALC-001 (Balance Update)
   - RULE-CALC-009 (Payment Amount) → RULE-CALC-001 (Balance Update)

2. **Account Update Flow**:
   - RULE-CALC-004 (Credit Limit) → Account Master Update
   - RULE-CALC-005 (Cash Limit) → Account Master Update
   - RULE-CALC-006 (Current Balance) → Account Master Update

3. **Payment Validation Flow**:
   - RULE-CALC-010 (Balance Validation) → RULE-CALC-009 (Payment Amount) → RULE-CALC-001 (Balance Update)

## Business Impact

These calculation rules form the core financial processing logic of the CardDemo application:

- **Payment Processing**: Rules RULE-CALC-001, RULE-CALC-009, and RULE-CALC-010 handle bill payment calculations
- **Account Management**: Rules RULE-CALC-004 through RULE-CALC-008 manage account limits and balances
- **Transaction Management**: Rules RULE-CALC-002 and RULE-CALC-003 handle transaction processing

## Notes for Modernization

When migrating these business rules to modern systems:

1. **Preserve Calculation Logic**: The exact mathematical operations must be maintained
2. **Maintain Business Constraints**: Validation rules like positive balance checks are critical
3. **Consider Precision**: COBOL's decimal precision handling should be replicated
4. **Transaction Integrity**: The sequence of calculations within transactions must be preserved

---

*Generated from CardDemo COBOL source code analysis*
*Source Repository: ashish-019-hash/aws-cardemo-ashwita*
*Analysis Date: September 2025*
