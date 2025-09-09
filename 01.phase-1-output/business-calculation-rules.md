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

## RULE-CALC-008: Report Date Range Calculation

**Rule ID:** RULE-CALC-008

**Rule Description:** Calculates end date for report generation by incrementing month and handling year rollover when month exceeds 12.

**COBOL Source Location:** CORPT00C.cbl, lines 224-230

**Involved Variables:**
- `WS-CURDATE-MONTH` - Current month for calculation
- `WS-CURDATE-YEAR` - Current year for calculation
- `WS-CURDATE-DAY` - Day component (set to 1)
- `WS-CURDATE-N` - Numeric date for calculation

**Input Conditions:**
- Report date range needs to be calculated

**Calculation Logic/Formula:**
```
SET day = 1
month = month + 1
IF month > 12 THEN
    year = year + 1
    month = 1
END IF
end_date = FUNCTION DATE-OF-INTEGER(FUNCTION INTEGER-OF-DATE(current_date) - 1)
```

## RULE-CALC-009: Transaction Pagination Index Calculation

**Rule ID:** RULE-CALC-009

**Rule Description:** Manages pagination for transaction browsing by calculating page numbers and index positions for forward and backward navigation.

**COBOL Source Location:** COTRN00C.cbl, lines 301, 306-307, 317-318, 355, 364

**Involved Variables:**
- `WS-IDX` - Current index position in transaction list
- `CDEMO-CT00-PAGE-NUM` - Current page number
- `TRANSACT-NOT-EOF` - End of file indicator
- `ERR-FLG-OFF` - Error flag status

**Input Conditions:**
- User navigating through transaction pages (forward/backward)
- Transaction records available for display

**Calculation Logic/Formula:**
```
FOR forward_navigation:
    index = index + 1
    IF more_records_available THEN
        page_number = page_number + 1
    END IF

FOR backward_navigation:
    index = index - 1
    IF previous_page_exists AND page_number > 1 THEN
        page_number = page_number - 1
    ELSE
        page_number = 1
    END IF
```

## RULE-CALC-010: Current Balance Processing

**Rule ID:** RULE-CALC-010

**Rule Description:** Validates and converts current account balance from string input to numeric format for account updates.

**COBOL Source Location:** COACTUPC.cbl, lines 1107-1108

**Involved Variables:**
- `ACURBALI OF CACTUPAI` - Current balance input from screen
- `ACUP-NEW-CURR-BAL-X` - Current balance string working storage
- `ACUP-NEW-CURR-BAL-N` - Current balance numeric working storage

**Input Conditions:**
```cobol
IF FUNCTION TEST-NUMVAL-C(ACUP-NEW-CURR-BAL-X) = 0
```

**Calculation Logic/Formula:**
```
IF current_balance_input_is_valid_numeric THEN
    numeric_current_balance = FUNCTION NUMVAL-C(current_balance_input)
ELSE
    SET validation_error
END IF
```

## RULE-CALC-011: Current Cycle Debit Processing

**Rule ID:** RULE-CALC-011

**Rule Description:** Validates and converts current cycle debit amount from string input to numeric format for account updates.

**COBOL Source Location:** COACTUPC.cbl, lines 1135-1136

**Involved Variables:**
- `ACRCYDBI OF CACTUPAI` - Current cycle debit input from screen
- `ACUP-NEW-CURR-CYC-DEBIT-X` - Current cycle debit string working storage
- `ACUP-NEW-CURR-CYC-DEBIT-N` - Current cycle debit numeric working storage

**Input Conditions:**
```cobol
IF FUNCTION TEST-NUMVAL-C(ACUP-NEW-CURR-CYC-DEBIT-X) = 0
```

**Calculation Logic/Formula:**
```
IF current_cycle_debit_input_is_valid_numeric THEN
    numeric_current_cycle_debit = FUNCTION NUMVAL-C(current_cycle_debit_input)
ELSE
    SET validation_error
END IF
```

## Summary

The CardDemo application contains 11 primary business calculation rules focused on:

1. **Balance Management** - Validating account balances and calculating payment amounts
2. **Amount Processing** - Converting string inputs to numeric values for calculations
3. **Credit Limit Management** - Validating and processing various credit limit types
4. **Transaction Processing** - Generating unique IDs and processing transaction amounts
5. **Date Calculations** - Computing report date ranges with month/year rollover logic
6. **Pagination Logic** - Managing transaction browsing with page number calculations

The system is primarily a transaction processing application with straightforward business rules. No complex financial calculations such as interest rates, fees, penalties, or sophisticated business formulas were found in the codebase.

All rules follow a consistent pattern of input validation, numeric conversion, data movement between working storage and record structures, and basic arithmetic operations for pagination and date handling, which is typical for COBOL-based transaction processing systems.
