# COBOL Business Calculation Rules Documentation

## Overview

This document contains the business-level calculation rules extracted from the CardDemo COBOL application. These rules represent the core business logic for financial calculations, excluding technical infrastructure, input validation, and I/O operations.

## Business Rules

### RULE-CALC-001: Bill Payment Balance Update

**Rule ID:** RULE-CALC-001

**Rule Description:** Updates the account current balance by subtracting the full payment amount when a bill payment is processed. This is the core financial calculation that reduces the customer's outstanding balance.

**COBOL Source Location:** COBIL00C.cbl, line 234

**Involved Variables:**
- `ACCT-CURR-BAL` - Account current balance (input/output)
- `TRAN-AMT` - Transaction amount (input)

**Input Conditions:**
- Payment confirmation is 'Y' (`CONF-PAY-YES`)
- Account current balance is greater than zero
- No error flags are set (`NOT ERR-FLG-ON`)

**Calculation Logic:**
```
New Account Balance = Current Account Balance - Transaction Amount
COMPUTE ACCT-CURR-BAL = ACCT-CURR-BAL - TRAN-AMT
```

---

### RULE-CALC-002: Transaction ID Generation

**Rule ID:** RULE-CALC-002

**Rule Description:** Generates a unique sequential transaction ID by finding the highest existing transaction ID and incrementing it by 1. This ensures each transaction has a unique identifier.

**COBOL Source Location:** COBIL00C.cbl, line 217; COTRN02C.cbl, line 449

**Involved Variables:**
- `WS-TRAN-ID-NUM` - Working storage transaction ID number (output)
- `TRAN-ID` - Transaction ID from file record (input)

**Input Conditions:**
- Transaction file browse is successful
- Previous transaction record is found

**Calculation Logic:**
```
New Transaction ID = Highest Existing Transaction ID + 1
ADD 1 TO WS-TRAN-ID-NUM
```

---

### RULE-CALC-003: FICO Score Validation Range

**Rule ID:** RULE-CALC-003

**Rule Description:** Validates that customer FICO credit scores fall within the acceptable business range of 300 to 850, which represents the standard FICO scoring model range used in credit decisions.

**COBOL Source Location:** COACTUPC.cbl, lines 848-849

**Involved Variables:**
- `ACUP-NEW-CUST-FICO-SCORE` - Customer FICO score (input)

**Input Conditions:**
- FICO score field is not blank or spaces
- FICO score is numeric

**Calculation Logic:**
```
Valid FICO Score Range: 300 ≤ FICO Score ≤ 850
88 FICO-RANGE-IS-VALID VALUES 300 THROUGH 850
```

---

### RULE-CALC-004: Transaction Amount Conversion

**Rule ID:** RULE-CALC-004

**Rule Description:** Converts transaction amount from character format with currency symbols to numeric format for processing. This handles user input with dollar signs and commas.

**COBOL Source Location:** COTRN02C.cbl, lines 456-457; COACTUPC.cbl, lines 1078-1080

**Involved Variables:**
- `TRNAMTI` - Transaction amount input (character format)
- `WS-TRAN-AMT-N` - Transaction amount numeric (output)
- `TRAN-AMT` - Final transaction amount (output)

**Input Conditions:**
- Transaction amount field is not blank
- Input passes NUMVAL-C function validation

**Calculation Logic:**
```
Numeric Amount = FUNCTION NUMVAL-C(Character Amount Input)
COMPUTE WS-TRAN-AMT-N = FUNCTION NUMVAL-C(TRNAMTI OF COTRN2AI)
```

---

### RULE-CALC-005: Credit Limit Validation and Conversion

**Rule ID:** RULE-CALC-005

**Rule Description:** Validates and converts credit limit amounts from character input to numeric format, ensuring proper handling of currency formatting for credit limit business decisions.

**COBOL Source Location:** COACTUPC.cbl, lines 1078-1080

**Involved Variables:**
- `ACRDLIMI` - Credit limit input (character format)
- `ACUP-NEW-CREDIT-LIMIT-X` - Credit limit character working field
- `ACUP-NEW-CREDIT-LIMIT-N` - Credit limit numeric (output)

**Input Conditions:**
- Credit limit field is not blank or low-values
- Input passes TEST-NUMVAL-C validation (returns 0 for valid)

**Calculation Logic:**
```
IF FUNCTION TEST-NUMVAL-C(Credit Limit Input) = 0 THEN
    Numeric Credit Limit = FUNCTION NUMVAL-C(Credit Limit Input)
COMPUTE ACUP-NEW-CREDIT-LIMIT-N = FUNCTION NUMVAL-C(ACRDLIMI OF CACTUPAI)
```

## Rule Dependencies and Relationships

The business rules have the following dependencies:

1. **RULE-CALC-002** (Transaction ID Generation) → **RULE-CALC-001** (Balance Update)
   - Transaction ID must be generated before balance update can be recorded

2. **RULE-CALC-004** (Amount Conversion) → **RULE-CALC-001** (Balance Update)
   - Transaction amount must be converted to numeric before balance calculation

3. **RULE-CALC-003** (FICO Score Validation) - Independent validation rule
   - Used for customer creditworthiness assessment

4. **RULE-CALC-005** (Credit Limit Validation) - Independent validation rule
   - Used for account setup and credit limit management

## Summary

This CardDemo application contains 5 core business calculation rules focused on:
- Financial balance updates (payment processing)
- Transaction management (ID generation, amount conversion)
- Credit risk assessment (FICO score validation, credit limits)

The rules are relatively straightforward, reflecting a demonstration application rather than complex financial calculations. The primary business logic centers around basic payment processing and credit validation rather than advanced interest calculations, fee computations, or complex financial formulas.
