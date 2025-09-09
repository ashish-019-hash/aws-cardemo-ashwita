# CardDemo COBOL Business Calculation Rules

This document contains the business-level calculation rules extracted from the CardDemo COBOL application. Only business logic rules are included - technical validation, I/O operations, and infrastructure logic have been excluded.

## RULE-CALC-001: Bill Payment Balance Validation

**Rule ID:** RULE-CALC-001

**Rule Description:** Validates that an account has a positive balance before allowing bill payment processing. Prevents payment attempts on accounts with zero or negative balances.

**COBOL Source Location:** COBIL00C.cbl, lines 198-205

**Involved Variables:**
- `ACCT-CURR-BAL` - Current account balance from account record
- `ACTIDINI OF COBIL0AI` - Account ID input from screen
- `WS-ERR-FLG` - Error flag indicator
- `WS-MESSAGE` - Error message text

**Input Conditions:**
```cobol
IF ACCT-CURR-BAL <= ZEROS AND
   ACTIDINI OF COBIL0AI NOT = SPACES AND LOW-VALUES
```

**Calculation Logic/Formula:**
```
IF current_account_balance <= 0 AND account_id_is_provided THEN
    SET error_flag = 'Y'
    SET message = 'You have nothing to pay...'
    PREVENT payment_processing
END IF
```

## RULE-CALC-002: Bill Payment Amount Calculation

**Rule ID:** RULE-CALC-002

**Rule Description:** Calculates the bill payment transaction amount by setting it equal to the current account balance, effectively paying the full balance.

**COBOL Source Location:** COBIL00C.cbl, lines 224-234

**Involved Variables:**
- `ACCT-CURR-BAL` - Current account balance
- `TRAN-AMT` - Transaction amount for the payment
- `WS-TRAN-ID-NUM` - Generated transaction ID
- `TRAN-RECORD` - Transaction record structure

**Input Conditions:**
```cobol
IF CONF-PAY-YES
```

**Calculation Logic/Formula:**
```
payment_amount = current_account_balance
new_account_balance = current_account_balance - payment_amount
new_account_balance = 0 (full payment)

COMPUTE ACCT-CURR-BAL = ACCT-CURR-BAL - TRAN-AMT
```

## RULE-CALC-003: Transaction Amount Processing

**Rule ID:** RULE-CALC-003

**Rule Description:** Converts transaction amount from string input format to numeric format for processing and storage.

**COBOL Source Location:** COTRN02C.cbl, lines 456-458

**Involved Variables:**
- `TRNAMTI OF COTRN2AI` - Transaction amount input (string format)
- `WS-TRAN-AMT-N` - Transaction amount numeric working storage
- `TRAN-AMT` - Transaction amount in record structure

**Input Conditions:**
- Transaction amount input is provided and confirmed

**Calculation Logic/Formula:**
```
numeric_amount = FUNCTION NUMVAL-C(string_amount_input)
transaction_record.amount = numeric_amount
```

## RULE-CALC-004: Credit Limit Validation and Conversion

**Rule ID:** RULE-CALC-004

**Rule Description:** Validates and converts credit limit input from string to numeric format for account updates.

**COBOL Source Location:** COACTUPC.cbl, lines 1078-1080

**Involved Variables:**
- `ACRDLIMI OF CACTUPAI` - Credit limit input from screen
- `ACUP-NEW-CREDIT-LIMIT-X` - Credit limit string working storage
- `ACUP-NEW-CREDIT-LIMIT-N` - Credit limit numeric working storage

**Input Conditions:**
```cobol
IF FUNCTION TEST-NUMVAL-C(ACUP-NEW-CREDIT-LIMIT-X) = 0
```

**Calculation Logic/Formula:**
```
IF credit_limit_input_is_valid_numeric THEN
    numeric_credit_limit = FUNCTION NUMVAL-C(credit_limit_input)
ELSE
    SET validation_error
END IF
```

## RULE-CALC-005: Cash Credit Limit Processing

**Rule ID:** RULE-CALC-005

**Rule Description:** Validates and converts cash credit limit input from string to numeric format for account updates.

**COBOL Source Location:** COACTUPC.cbl, lines 1092-1094

**Involved Variables:**
- `ACSHLIMI OF CACTUPAI` - Cash credit limit input from screen
- `ACUP-NEW-CASH-CREDIT-LIMIT-X` - Cash credit limit string working storage
- `ACUP-NEW-CASH-CREDIT-LIMIT-N` - Cash credit limit numeric working storage

**Input Conditions:**
```cobol
IF FUNCTION TEST-NUMVAL-C(ACUP-NEW-CASH-CREDIT-LIMIT-X) = 0
```

**Calculation Logic/Formula:**
```
IF cash_credit_limit_input_is_valid_numeric THEN
    numeric_cash_credit_limit = FUNCTION NUMVAL-C(cash_credit_limit_input)
ELSE
    SET validation_error
END IF
```

## RULE-CALC-006: Current Cycle Credit Processing

**Rule ID:** RULE-CALC-006

**Rule Description:** Validates and converts current cycle credit amount from string to numeric format for account updates.

**COBOL Source Location:** COACTUPC.cbl, lines 1120-1122

**Involved Variables:**
- `ACRCYCRI OF CACTUPAI` - Current cycle credit input from screen
- `ACUP-NEW-CURR-CYC-CREDIT-X` - Current cycle credit string working storage
- `ACUP-NEW-CURR-CYC-CREDIT-N` - Current cycle credit numeric working storage

**Input Conditions:**
```cobol
IF FUNCTION TEST-NUMVAL-C(ACUP-NEW-CURR-CYC-CREDIT-X) = 0
```

**Calculation Logic/Formula:**
```
IF current_cycle_credit_input_is_valid_numeric THEN
    numeric_current_cycle_credit = FUNCTION NUMVAL-C(current_cycle_credit_input)
ELSE
    SET validation_error
END IF
```

## RULE-CALC-007: Transaction ID Generation

**Rule ID:** RULE-CALC-007

**Rule Description:** Generates unique transaction IDs by finding the highest existing transaction ID and incrementing by 1.

**COBOL Source Location:** 
- COBIL00C.cbl, lines 216-217
- COTRN02C.cbl, lines 448-449

**Involved Variables:**
- `TRAN-ID` - Transaction ID from database
- `WS-TRAN-ID-NUM` / `WS-TRAN-ID-N` - Working storage for new transaction ID

**Input Conditions:**
- New transaction needs to be created

**Calculation Logic/Formula:**
```
highest_existing_transaction_id = READ_LAST_TRANSACTION_ID()
new_transaction_id = highest_existing_transaction_id + 1

ADD 1 TO WS-TRAN-ID-NUM
```

## Summary

The CardDemo application contains 7 primary business calculation rules focused on:

1. **Balance Management** - Validating account balances and calculating payment amounts
2. **Amount Processing** - Converting string inputs to numeric values for calculations
3. **Credit Limit Management** - Validating and processing various credit limit types
4. **Transaction Processing** - Generating unique IDs and processing transaction amounts

The system is primarily a transaction processing application with straightforward business rules. No complex financial calculations such as interest rates, fees, penalties, or sophisticated business formulas were found in the codebase.

All rules follow a consistent pattern of input validation, numeric conversion, and data movement between working storage and record structures, which is typical for COBOL-based transaction processing systems.
