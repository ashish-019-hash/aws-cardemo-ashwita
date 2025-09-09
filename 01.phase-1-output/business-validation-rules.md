# CardDemo COBOL Business Validation Rules

This document contains business-level validation rules extracted from the CardDemo COBOL application. These rules are intended for migration to modern validation layers (frontend + backend APIs).

## Table of Contents

1. [Authentication Validations](#authentication-validations)
2. [Account Management Validations](#account-management-validations)
3. [Card Management Validations](#card-management-validations)
4. [Transaction Management Validations](#transaction-management-validations)
5. [User Management Validations](#user-management-validations)
6. [Administrative Validations](#administrative-validations)
7. [Utility Validations](#utility-validations)
8. [Validation Relationships Diagram](#validation-relationships-diagram)

---

## Authentication Validations

### Validation Rule ID: RULE-VAL-001
**Rule Description:** User ID cannot be empty
**COBOL Source Location:** COSGN00C.cbl, lines 118-122, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** USERIDI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** When ENTER key is pressed on signon screen

### Validation Rule ID: RULE-VAL-002
**Rule Description:** Password cannot be empty
**COBOL Source Location:** COSGN00C.cbl, lines 123-127, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** PASSWDI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** When ENTER key is pressed on signon screen

### Validation Rule ID: RULE-VAL-003
**Rule Description:** Password must match stored password
**COBOL Source Location:** COSGN00C.cbl, lines 223-246, READ-USER-SEC-FILE paragraph
**Field(s) Involved:** SEC-USR-PWD, WS-USER-PWD
**Validation Condition:** SEC-USR-PWD = WS-USER-PWD
**Trigger Conditions:** After successful user record retrieval from USRSEC file

---

## Account Management Validations

### Validation Rule ID: RULE-VAL-004
**Rule Description:** Account status must be Y or N
**COBOL Source Location:** COACTUPC.cbl, lines 192-195, WS-NON-KEY-FLAGS section
**Field(s) Involved:** WS-EDIT-ACCT-STATUS
**Validation Condition:** Field must be 'Y' or 'N'
**Trigger Conditions:** During account update validation

### Validation Rule ID: RULE-VAL-005
**Rule Description:** Credit limit must be valid numeric
**COBOL Source Location:** COACTUPC.cbl, lines 196-199, WS-NON-KEY-FLAGS section
**Field(s) Involved:** WS-EDIT-CREDIT-LIMIT
**Validation Condition:** Field must be valid numeric value
**Trigger Conditions:** During account update validation

### Validation Rule ID: RULE-VAL-006
**Rule Description:** Cash credit limit must be valid numeric
**COBOL Source Location:** COACTUPC.cbl, lines 200-203, WS-NON-KEY-FLAGS section
**Field(s) Involved:** WS-EDIT-CASH-CREDIT-LIMIT
**Validation Condition:** Field must be valid numeric value
**Trigger Conditions:** During account update validation

### Validation Rule ID: RULE-VAL-007
**Rule Description:** Current balance must be valid numeric
**COBOL Source Location:** COACTUPC.cbl, lines 204-207, WS-NON-KEY-FLAGS section
**Field(s) Involved:** WS-EDIT-CURR-BAL
**Validation Condition:** Field must be valid numeric value
**Trigger Conditions:** During account update validation

### Validation Rule ID: RULE-VAL-008
**Rule Description:** Current cycle credit must be valid numeric
**COBOL Source Location:** COACTUPC.cbl, lines 208-211, WS-NON-KEY-FLAGS section
**Field(s) Involved:** WS-EDIT-CURR-CYC-CREDIT
**Validation Condition:** Field must be valid numeric value
**Trigger Conditions:** During account update validation

### Validation Rule ID: RULE-VAL-009
**Rule Description:** Current cycle debit must be valid numeric
**COBOL Source Location:** COACTUPC.cbl, lines 212-215, WS-NON-KEY-FLAGS section
**Field(s) Involved:** WS-EDIT-CURR-CYC-DEBIT
**Validation Condition:** Field must be valid numeric value
**Trigger Conditions:** During account update validation

### Validation Rule ID: RULE-VAL-010
**Rule Description:** Date of birth must be valid date format
**COBOL Source Location:** COACTUPC.cbl, lines 216-230, WS-EDIT-DT-OF-BIRTH-FLGS section
**Field(s) Involved:** Date of birth year, month, day components
**Validation Condition:** Year, month, and day must be valid date components
**Trigger Conditions:** During account update validation

### Validation Rule ID: RULE-VAL-011
**Rule Description:** FICO score must be valid numeric
**COBOL Source Location:** COACTUPC.cbl, lines 231-234, WS-NON-KEY-FLAGS section
**Field(s) Involved:** WS-EDIT-FICO-SCORE-FLGS
**Validation Condition:** Field must be valid numeric value
**Trigger Conditions:** During account update validation

### Validation Rule ID: RULE-VAL-012
**Rule Description:** Account open date must be valid date format
**COBOL Source Location:** COACTUPC.cbl, lines 235-248, WS-EDIT-OPEN-DATE-FLGS section
**Field(s) Involved:** Open date year, month, day components
**Validation Condition:** Year, month, and day must be valid date components
**Trigger Conditions:** During account update validation

### Validation Rule ID: RULE-VAL-013
**Rule Description:** US Phone number must follow valid format
**COBOL Source Location:** COACTUPC.cbl, lines 82-115, WS-EDIT-US-PHONE-NUM section
**Field(s) Involved:** WS-EDIT-US-PHONE-NUMA, WS-EDIT-US-PHONE-NUMB, WS-EDIT-US-PHONE-NUMC
**Validation Condition:** Phone number must be in format (XXX)XXX-XXXX with valid numeric components
**Trigger Conditions:** During account update validation

### Validation Rule ID: RULE-VAL-014
**Rule Description:** US SSN must follow valid format and rules
**COBOL Source Location:** COACTUPC.cbl, lines 117-146, WS-EDIT-US-SSN section
**Field(s) Involved:** WS-EDIT-US-SSN-PART1, WS-EDIT-US-SSN-PART2, WS-EDIT-US-SSN-PART3
**Validation Condition:** SSN parts must be numeric, first part cannot be 0, 666, or 900-999
**Trigger Conditions:** During account update validation

### Validation Rule ID: RULE-VAL-015
**Rule Description:** Yes/No fields must be Y or N
**COBOL Source Location:** COACTUPC.cbl, lines 76-80, WS-EDIT-YES-NO section
**Field(s) Involved:** WS-EDIT-YES-NO
**Validation Condition:** Field must be 'Y' or 'N'
**Trigger Conditions:** During account update validation

### Validation Rule ID: RULE-VAL-016
**Rule Description:** Signed numeric fields must be valid
**COBOL Source Location:** COACTUPC.cbl, lines 55-59, WS-GENERIC-EDITS section
**Field(s) Involved:** WS-EDIT-SIGNED-NUMBER-9V2-X
**Validation Condition:** Field must be valid signed numeric value
**Trigger Conditions:** During account update validation

### Validation Rule ID: RULE-VAL-017
**Rule Description:** Alpha-only fields must contain only alphabetic characters
**COBOL Source Location:** COACTUPC.cbl, lines 64-67, WS-GENERIC-EDITS section
**Field(s) Involved:** WS-EDIT-ALPHA-ONLY-FLAGS
**Validation Condition:** Field must contain only alphabetic characters
**Trigger Conditions:** During account update validation

### Validation Rule ID: RULE-VAL-018
**Rule Description:** Alphanumeric fields must be valid
**COBOL Source Location:** COACTUPC.cbl, lines 68-71, WS-GENERIC-EDITS section
**Field(s) Involved:** WS-EDIT-ALPHANUM-ONLY-FLAGS
**Validation Condition:** Field must contain valid alphanumeric characters
**Trigger Conditions:** During account update validation

### Validation Rule ID: RULE-VAL-019
**Rule Description:** Mandatory fields cannot be blank
**COBOL Source Location:** COACTUPC.cbl, lines 72-75, WS-GENERIC-EDITS section
**Field(s) Involved:** WS-EDIT-MANDATORY-FLAGS
**Validation Condition:** Field must not be blank or empty
**Trigger Conditions:** During account update validation

---

## Card Management Validations

### Validation Rule ID: RULE-VAL-020
**Rule Description:** Account number must be provided for card operations
**COBOL Source Location:** COCRDUPC.cbl, lines 177-178, WS-RETURN-MSG section
**Field(s) Involved:** Account number field
**Validation Condition:** Account number must not be empty
**Trigger Conditions:** During card update operations

### Validation Rule ID: RULE-VAL-021
**Rule Description:** Card number must be provided for card operations
**COBOL Source Location:** COCRDUPC.cbl, lines 179-180, WS-RETURN-MSG section
**Field(s) Involved:** Card number field
**Validation Condition:** Card number must not be empty
**Trigger Conditions:** During card update operations

### Validation Rule ID: RULE-VAL-022
**Rule Description:** Card name must be provided
**COBOL Source Location:** COCRDUPC.cbl, lines 181-182, WS-RETURN-MSG section
**Field(s) Involved:** Card name field
**Validation Condition:** Card name must not be empty
**Trigger Conditions:** During card update operations

### Validation Rule ID: RULE-VAL-023
**Rule Description:** Card name can only contain alphabets and spaces
**COBOL Source Location:** COCRDUPC.cbl, lines 183-184, WS-RETURN-MSG section
**Field(s) Involved:** Card name field
**Validation Condition:** Field must contain only alphabetic characters and spaces
**Trigger Conditions:** During card update operations

### Validation Rule ID: RULE-VAL-024
**Rule Description:** Account number must be non-zero 11-digit number
**COBOL Source Location:** COCRDUPC.cbl, lines 189-192, WS-RETURN-MSG section
**Field(s) Involved:** Account number field
**Validation Condition:** Field must be numeric, 11 digits, and not zero
**Trigger Conditions:** During card update operations

### Validation Rule ID: RULE-VAL-025
**Rule Description:** Card number must be 16-digit number if supplied
**COBOL Source Location:** COCRDUPC.cbl, lines 193-194, WS-RETURN-MSG section
**Field(s) Involved:** Card number field
**Validation Condition:** Field must be numeric and exactly 16 digits
**Trigger Conditions:** During card update operations

### Validation Rule ID: RULE-VAL-026
**Rule Description:** Card active status must be Y or N
**COBOL Source Location:** COCRDUPC.cbl, lines 195-196, WS-RETURN-MSG section
**Field(s) Involved:** Card status field
**Validation Condition:** Field must be 'Y' or 'N'
**Trigger Conditions:** During card update operations

### Validation Rule ID: RULE-VAL-027
**Rule Description:** Card expiry month must be between 1 and 12
**COBOL Source Location:** COCRDUPC.cbl, lines 197-198, WS-RETURN-MSG section
**Field(s) Involved:** Card expiry month field
**Validation Condition:** Field must be numeric value between 1 and 12
**Trigger Conditions:** During card update operations

### Validation Rule ID: RULE-VAL-028
**Rule Description:** Card expiry year must be valid
**COBOL Source Location:** COCRDUPC.cbl, lines 199-200, WS-RETURN-MSG section
**Field(s) Involved:** Card expiry year field
**Validation Condition:** Field must be valid year value
**Trigger Conditions:** During card update operations

### Validation Rule ID: RULE-VAL-029
**Rule Description:** Valid month range check
**COBOL Source Location:** COCRDUPC.cbl, lines 95, CARD-MONTH-CHECK-N section
**Field(s) Involved:** CARD-MONTH-CHECK-N
**Validation Condition:** Month must be between 1 and 12
**Trigger Conditions:** During card validation

### Validation Rule ID: RULE-VAL-030
**Rule Description:** Valid year range check
**COBOL Source Location:** COCRDUPC.cbl, lines 99, CARD-YEAR-CHECK-N section
**Field(s) Involved:** CARD-YEAR-CHECK-N
**Validation Condition:** Year must be between 1950 and 2099
**Trigger Conditions:** During card validation

### Validation Rule ID: RULE-VAL-031
**Rule Description:** Yes/No validation for card fields
**COBOL Source Location:** COCRDUPC.cbl, lines 91, FLG-YES-NO-CHECK section
**Field(s) Involved:** FLG-YES-NO-CHECK
**Validation Condition:** Field must be 'Y' or 'N'
**Trigger Conditions:** During card validation

### Validation Rule ID: RULE-VAL-063
**Rule Description:** Account must have positive balance for bill payment
**COBOL Source Location:** COBIL00C.cbl, lines 198-206, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** ACCT-CURR-BAL
**Validation Condition:** Account current balance must be greater than zero
**Trigger Conditions:** During bill payment validation

### Validation Rule ID: RULE-VAL-064
**Rule Description:** Account view account number must be non-zero 11-digit number
**COBOL Source Location:** COACTVWC.cbl, lines 125-128, WS-RETURN-MSG section
**Field(s) Involved:** Account number field
**Validation Condition:** Field must be numeric, 11 digits, and not zero
**Trigger Conditions:** During account view operations

### Validation Rule ID: RULE-VAL-065
**Rule Description:** Card list selection must be valid action code
**COBOL Source Location:** COCRDLIC.cbl, lines 77-82, WS-EDIT-SELECT section
**Field(s) Involved:** WS-EDIT-SELECT
**Validation Condition:** Field must be 'S' (select) or 'U' (update)
**Trigger Conditions:** During card list selection

### Validation Rule ID: RULE-VAL-066
**Rule Description:** Card detail account number must be non-zero 11-digit number
**COBOL Source Location:** COCRDSLC.cbl, lines 144-147, WS-RETURN-MSG section
**Field(s) Involved:** Account number field
**Validation Condition:** Field must be numeric, 11 digits, and not zero
**Trigger Conditions:** During card detail operations

### Validation Rule ID: RULE-VAL-067
**Rule Description:** Card detail card number must be 16-digit number if supplied
**COBOL Source Location:** COCRDSLC.cbl, lines 148-149, WS-RETURN-MSG section
**Field(s) Involved:** Card number field
**Validation Condition:** Field must be numeric and exactly 16 digits
**Trigger Conditions:** During card detail operations

---

## Transaction Management Validations

### Validation Rule ID: RULE-VAL-032
**Rule Description:** Account ID must be numeric for transactions
**COBOL Source Location:** COTRN02C.cbl, lines 197-203, VALIDATE-INPUT-KEY-FIELDS paragraph
**Field(s) Involved:** ACTIDINI
**Validation Condition:** Field must be numeric
**Trigger Conditions:** During transaction entry validation

### Validation Rule ID: RULE-VAL-033
**Rule Description:** Card number must be numeric for transactions
**COBOL Source Location:** COTRN02C.cbl, lines 211-217, VALIDATE-INPUT-KEY-FIELDS paragraph
**Field(s) Involved:** CARDNINI
**Validation Condition:** Field must be numeric
**Trigger Conditions:** During transaction entry validation

### Validation Rule ID: RULE-VAL-034
**Rule Description:** Account or Card number must be entered
**COBOL Source Location:** COTRN02C.cbl, lines 224-230, VALIDATE-INPUT-KEY-FIELDS paragraph
**Field(s) Involved:** ACTIDINI, CARDNINI
**Validation Condition:** At least one of account ID or card number must be provided
**Trigger Conditions:** During transaction entry validation

### Validation Rule ID: RULE-VAL-035
**Rule Description:** Transaction type code cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 252-257, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TTYPCDI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction entry validation

### Validation Rule ID: RULE-VAL-036
**Rule Description:** Transaction category code cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 258-263, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TCATCDI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction entry validation

### Validation Rule ID: RULE-VAL-037
**Rule Description:** Transaction source cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 264-269, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TRNSRCI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction entry validation

### Validation Rule ID: RULE-VAL-038
**Rule Description:** Transaction description cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 270-275, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TDESCI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction entry validation

### Validation Rule ID: RULE-VAL-039
**Rule Description:** Transaction amount cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 276-281, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TRNAMTI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction entry validation

### Validation Rule ID: RULE-VAL-040
**Rule Description:** Transaction original date cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 282-287, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TORIGDTI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction entry validation

### Validation Rule ID: RULE-VAL-041
**Rule Description:** Transaction process date cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 288-293, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TPROCDTI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction entry validation

### Validation Rule ID: RULE-VAL-042
**Rule Description:** Merchant ID cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 294-299, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** MIDI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction entry validation

### Validation Rule ID: RULE-VAL-043
**Rule Description:** Merchant name cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 300-305, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** MNAMEI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction entry validation

### Validation Rule ID: RULE-VAL-044
**Rule Description:** Merchant city cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 306-311, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** MCITYI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction entry validation

### Validation Rule ID: RULE-VAL-045
**Rule Description:** Merchant zip cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 312-317, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** MZIPI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction entry validation

### Validation Rule ID: RULE-VAL-046
**Rule Description:** Transaction type code must be numeric
**COBOL Source Location:** COTRN02C.cbl, lines 323-328, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TTYPCDI
**Validation Condition:** Field must be numeric
**Trigger Conditions:** During transaction entry validation

### Validation Rule ID: RULE-VAL-047
**Rule Description:** Transaction category code must be numeric
**COBOL Source Location:** COTRN02C.cbl, lines 329-334, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TCATCDI
**Validation Condition:** Field must be numeric
**Trigger Conditions:** During transaction entry validation

### Validation Rule ID: RULE-VAL-048
**Rule Description:** Transaction amount must be in format -99999999.99
**COBOL Source Location:** COTRN02C.cbl, lines 340-348, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TRNAMTI
**Validation Condition:** Amount must start with + or -, followed by 8 numeric digits, decimal point, and 2 decimal places
**Trigger Conditions:** During transaction entry validation

### Validation Rule ID: RULE-VAL-049
**Rule Description:** Original date must be in format YYYY-MM-DD
**COBOL Source Location:** COTRN02C.cbl, lines 354-363, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TORIGDTI
**Validation Condition:** Date must be in YYYY-MM-DD format with numeric components
**Trigger Conditions:** During transaction entry validation

### Validation Rule ID: RULE-VAL-050
**Rule Description:** Process date must be in format YYYY-MM-DD
**COBOL Source Location:** COTRN02C.cbl, lines 368-373, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TPROCDTI
**Validation Condition:** Date must be in YYYY-MM-DD format with numeric components
**Trigger Conditions:** During transaction entry validation

### Validation Rule ID: RULE-VAL-051
**Rule Description:** Transaction confirmation must be Y or N
**COBOL Source Location:** COTRN02C.cbl, lines 169-188, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** CONFIRMI
**Validation Condition:** Field must be 'Y', 'y', 'N', 'n', SPACES, or LOW-VALUES
**Trigger Conditions:** During transaction confirmation

---

## User Management Validations

### Validation Rule ID: RULE-VAL-052
**Rule Description:** First name cannot be empty
**COBOL Source Location:** COUSR01C.cbl, lines 118-123, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** FNAMEI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During user creation

### Validation Rule ID: RULE-VAL-053
**Rule Description:** Last name cannot be empty
**COBOL Source Location:** COUSR01C.cbl, lines 124-129, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** LNAMEI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During user creation

### Validation Rule ID: RULE-VAL-054
**Rule Description:** User ID cannot be empty
**COBOL Source Location:** COUSR01C.cbl, lines 130-135, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** USERIDI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During user creation

### Validation Rule ID: RULE-VAL-055
**Rule Description:** Password cannot be empty
**COBOL Source Location:** COUSR01C.cbl, lines 136-141, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** PASSWDI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During user creation

### Validation Rule ID: RULE-VAL-056
**Rule Description:** User type cannot be empty
**COBOL Source Location:** COUSR01C.cbl, lines 142-147, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** USRTYPEI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During user creation

### Validation Rule ID: RULE-VAL-057
**Rule Description:** User ID must be unique
**COBOL Source Location:** COUSR01C.cbl, lines 260-266, WRITE-USER-SEC-FILE paragraph
**Field(s) Involved:** SEC-USR-ID
**Validation Condition:** User ID must not already exist in USRSEC file
**Trigger Conditions:** During user creation when writing to database

---

## Administrative Validations

### Validation Rule ID: RULE-VAL-058
**Rule Description:** Administrative access control
**COBOL Source Location:** COSGN00C.cbl, lines 230-240, READ-USER-SEC-FILE paragraph
**Field(s) Involved:** CDEMO-USER-TYPE
**Validation Condition:** User type determines access to administrative functions
**Trigger Conditions:** After successful authentication

### Validation Rule ID: RULE-VAL-061
**Rule Description:** Bill payment account ID cannot be empty
**COBOL Source Location:** COBIL00C.cbl, lines 159-164, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** ACTIDINI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During bill payment processing

### Validation Rule ID: RULE-VAL-062
**Rule Description:** Bill payment confirmation must be Y or N
**COBOL Source Location:** COBIL00C.cbl, lines 173-191, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** CONFIRMI
**Validation Condition:** Field must be 'Y', 'y', 'N', 'n', SPACES, or LOW-VALUES
**Trigger Conditions:** During bill payment confirmation

### Validation Rule ID: RULE-VAL-068
**Rule Description:** Menu option must be numeric and within valid range
**COBOL Source Location:** COMEN01C.cbl, lines 127-134, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** WS-OPTION
**Validation Condition:** Field must be numeric, greater than zero, and not exceed menu option count
**Trigger Conditions:** During menu option selection

### Validation Rule ID: RULE-VAL-069
**Rule Description:** User access control for admin-only options
**COBOL Source Location:** COMEN01C.cbl, lines 136-143, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** CDEMO-USRTYP-USER, CDEMO-MENU-OPT-USRTYPE
**Validation Condition:** Regular users cannot access admin-only menu options
**Trigger Conditions:** During menu option selection

### Validation Rule ID: RULE-VAL-070
**Rule Description:** Admin menu option must be numeric and within valid range
**COBOL Source Location:** COADM01C.cbl, lines 127-134, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** WS-OPTION
**Validation Condition:** Field must be numeric, greater than zero, and not exceed admin option count
**Trigger Conditions:** During admin menu option selection

---

## Reporting Validations

### Validation Rule ID: RULE-VAL-071
**Rule Description:** Report start date month cannot be empty
**COBOL Source Location:** CORPT00C.cbl, lines 259-265, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** SDTMMI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During custom report generation

### Validation Rule ID: RULE-VAL-072
**Rule Description:** Report start date day cannot be empty
**COBOL Source Location:** CORPT00C.cbl, lines 266-272, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** SDTDDI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During custom report generation

### Validation Rule ID: RULE-VAL-073
**Rule Description:** Report start date year cannot be empty
**COBOL Source Location:** CORPT00C.cbl, lines 273-279, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** SDTYYYYI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During custom report generation

### Validation Rule ID: RULE-VAL-074
**Rule Description:** Report end date month cannot be empty
**COBOL Source Location:** CORPT00C.cbl, lines 280-286, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** EDTMMI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During custom report generation

### Validation Rule ID: RULE-VAL-075
**Rule Description:** Report end date day cannot be empty
**COBOL Source Location:** CORPT00C.cbl, lines 287-293, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** EDTDDI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During custom report generation

### Validation Rule ID: RULE-VAL-076
**Rule Description:** Report end date year cannot be empty
**COBOL Source Location:** CORPT00C.cbl, lines 294-300, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** EDTYYYYI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During custom report generation

### Validation Rule ID: RULE-VAL-077
**Rule Description:** Report start date month must be valid (1-12)
**COBOL Source Location:** CORPT00C.cbl, lines 329-336, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** SDTMMI
**Validation Condition:** Field must be numeric and not greater than 12
**Trigger Conditions:** During custom report generation

### Validation Rule ID: RULE-VAL-078
**Rule Description:** Report start date day must be valid (1-31)
**COBOL Source Location:** CORPT00C.cbl, lines 338-345, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** SDTDDI
**Validation Condition:** Field must be numeric and not greater than 31
**Trigger Conditions:** During custom report generation

### Validation Rule ID: RULE-VAL-079
**Rule Description:** Report start date year must be numeric
**COBOL Source Location:** CORPT00C.cbl, lines 347-353, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** SDTYYYYI
**Validation Condition:** Field must be numeric
**Trigger Conditions:** During custom report generation

### Validation Rule ID: RULE-VAL-080
**Rule Description:** Report end date month must be valid (1-12)
**COBOL Source Location:** CORPT00C.cbl, lines 355-362, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** EDTMMI
**Validation Condition:** Field must be numeric and not greater than 12
**Trigger Conditions:** During custom report generation

### Validation Rule ID: RULE-VAL-081
**Rule Description:** Report end date day must be valid (1-31)
**COBOL Source Location:** CORPT00C.cbl, lines 364-371, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** EDTDDI
**Validation Condition:** Field must be numeric and not greater than 31
**Trigger Conditions:** During custom report generation

### Validation Rule ID: RULE-VAL-082
**Rule Description:** Report end date year must be numeric
**COBOL Source Location:** CORPT00C.cbl, lines 373-379, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** EDTYYYYI
**Validation Condition:** Field must be numeric
**Trigger Conditions:** During custom report generation

### Validation Rule ID: RULE-VAL-083
**Rule Description:** Transaction ID must be numeric for transaction listing
**COBOL Source Location:** COTRN00C.cbl, lines 209-218, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** TRNIDINI
**Validation Condition:** Field must be numeric if provided
**Trigger Conditions:** During transaction listing

### Validation Rule ID: RULE-VAL-084
**Rule Description:** Transaction selection must be valid action code
**COBOL Source Location:** COTRN00C.cbl, lines 185-203, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** CDEMO-CT00-TRN-SEL-FLG
**Validation Condition:** Field must be 'S' or 's' for selection
**Trigger Conditions:** During transaction list selection

### Validation Rule ID: RULE-VAL-085
**Rule Description:** Transaction ID cannot be empty for transaction view
**COBOL Source Location:** COTRN01C.cbl, lines 147-152, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** TRNIDINI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction view

### Validation Rule ID: RULE-VAL-086
**Rule Description:** User selection must be valid action code
**COBOL Source Location:** COUSR00C.cbl, lines 189-215, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** CDEMO-CU00-USR-SEL-FLG
**Validation Condition:** Field must be 'U', 'u', 'D', or 'd' for update/delete operations
**Trigger Conditions:** During user list selection

### Validation Rule ID: RULE-VAL-087
**Rule Description:** User ID cannot be empty for user update
**COBOL Source Location:** COUSR02C.cbl, lines 146-151, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** USRIDINI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During user update operations

### Validation Rule ID: RULE-VAL-088
**Rule Description:** First name cannot be empty for user update
**COBOL Source Location:** COUSR02C.cbl, lines 186-191, UPDATE-USER-INFO paragraph
**Field(s) Involved:** FNAMEI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During user update validation

### Validation Rule ID: RULE-VAL-089
**Rule Description:** Last name cannot be empty for user update
**COBOL Source Location:** COUSR02C.cbl, lines 192-197, UPDATE-USER-INFO paragraph
**Field(s) Involved:** LNAMEI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During user update validation

### Validation Rule ID: RULE-VAL-090
**Rule Description:** Password cannot be empty for user update
**COBOL Source Location:** COUSR02C.cbl, lines 198-203, UPDATE-USER-INFO paragraph
**Field(s) Involved:** PASSWDI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During user update validation

### Validation Rule ID: RULE-VAL-091
**Rule Description:** User type cannot be empty for user update
**COBOL Source Location:** COUSR02C.cbl, lines 204-209, UPDATE-USER-INFO paragraph
**Field(s) Involved:** USRTYPEI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During user update validation

### Validation Rule ID: RULE-VAL-092
**Rule Description:** User ID cannot be empty for user deletion
**COBOL Source Location:** COUSR03C.cbl, lines 145-150, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** USRIDINI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During user deletion operations

### Validation Rule ID: RULE-VAL-093
**Rule Description:** User ID cannot be empty for user deletion confirmation
**COBOL Source Location:** COUSR03C.cbl, lines 177-182, DELETE-USER-INFO paragraph
**Field(s) Involved:** USRIDINI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During user deletion confirmation

---

## Utility Validations

### Validation Rule ID: RULE-VAL-059
**Rule Description:** Date format validation using CEEDAYS API
**COBOL Source Location:** CSUTLDTC.cbl, lines 128-149, A000-MAIN paragraph
**Field(s) Involved:** LS-DATE, LS-DATE-FORMAT
**Validation Condition:** Date must be valid according to specified format using CEEDAYS API
**Trigger Conditions:** When date validation utility is called

### Validation Rule ID: RULE-VAL-060
**Rule Description:** Date format must be valid
**COBOL Source Location:** CSUTLDTC.cbl, lines 62-70, FEEDBACK-CODE section
**Field(s) Involved:** Date input and format parameters
**Validation Condition:** Date format must be supported and date value must be valid
**Trigger Conditions:** During date validation processing

---

## Validation Relationships Diagram

```svg
<svg width="800" height="600" xmlns="http://www.w3.org/2000/svg">
  <defs>
    <style>
      .title { font: bold 16px sans-serif; text-anchor: middle; }
      .group-title { font: bold 12px sans-serif; text-anchor: middle; }
      .rule-text { font: 10px sans-serif; text-anchor: middle; }
      .group-box { fill: #f0f8ff; stroke: #4682b4; stroke-width: 2; }
      .auth-box { fill: #ffe4e1; stroke: #dc143c; stroke-width: 2; }
      .account-box { fill: #f0fff0; stroke: #228b22; stroke-width: 2; }
      .card-box { fill: #fff8dc; stroke: #daa520; stroke-width: 2; }
      .transaction-box { fill: #e6e6fa; stroke: #9370db; stroke-width: 2; }
      .user-box { fill: #ffefd5; stroke: #ff8c00; stroke-width: 2; }
      .utility-box { fill: #f5f5dc; stroke: #808080; stroke-width: 2; }
      .connection { stroke: #666; stroke-width: 1; stroke-dasharray: 5,5; }
    </style>
  </defs>
  
  <!-- Title -->
  <text x="400" y="25" class="title">CardDemo COBOL Validation Rules Relationships</text>
  
  <!-- Authentication Group -->
  <rect x="50" y="50" width="150" height="80" class="auth-box"/>
  <text x="125" y="70" class="group-title">Authentication</text>
  <text x="125" y="85" class="rule-text">RULE-VAL-001: User ID</text>
  <text x="125" y="100" class="rule-text">RULE-VAL-002: Password</text>
  <text x="125" y="115" class="rule-text">RULE-VAL-003: Auth Check</text>
  
  <!-- Account Management Group -->
  <rect x="250" y="50" width="200" height="120" class="account-box"/>
  <text x="350" y="70" class="group-title">Account Management</text>
  <text x="350" y="85" class="rule-text">RULE-VAL-004-019: Field Validations</text>
  <text x="350" y="100" class="rule-text">• Status (Y/N)</text>
  <text x="350" y="115" class="rule-text">• Numeric Fields</text>
  <text x="350" y="130" class="rule-text">• Date Formats</text>
  <text x="350" y="145" class="rule-text">• Phone/SSN Patterns</text>
  <text x="350" y="160" class="rule-text">• Alpha/Alphanumeric</text>
  
  <!-- Card Management Group -->
  <rect x="500" y="50" width="180" height="120" class="card-box"/>
  <text x="590" y="70" class="group-title">Card Management</text>
  <text x="590" y="85" class="rule-text">RULE-VAL-020-031: Card Rules</text>
  <text x="590" y="100" class="rule-text">• Required Fields</text>
  <text x="590" y="115" class="rule-text">• Numeric Formats</text>
  <text x="590" y="130" class="rule-text">• Date Ranges</text>
  <text x="590" y="145" class="rule-text">• Status Values</text>
  <text x="590" y="160" class="rule-text">• Name Patterns</text>
  
  <!-- Transaction Management Group -->
  <rect x="50" y="200" width="200" height="140" class="transaction-box"/>
  <text x="150" y="220" class="group-title">Transaction Management</text>
  <text x="150" y="235" class="rule-text">RULE-VAL-032-051, 083-085: Transaction Rules</text>
  <text x="150" y="250" class="rule-text">• Key Field Validations</text>
  <text x="150" y="265" class="rule-text">• Required Data Fields</text>
  <text x="150" y="280" class="rule-text">• Numeric Validations</text>
  <text x="150" y="295" class="rule-text">• Amount Format</text>
  <text x="150" y="310" class="rule-text">• Date Format (YYYY-MM-DD)</text>
  <text x="150" y="325" class="rule-text">• Confirmation (Y/N)</text>

  <!-- User Management Group -->
  <rect x="300" y="200" width="180" height="120" class="user-box"/>
  <text x="390" y="220" class="group-title">User Management</text>
  <text x="390" y="235" class="rule-text">RULE-VAL-052-057, 086-093: User Rules</text>
  <text x="390" y="250" class="rule-text">• Required Fields</text>
  <text x="390" y="265" class="rule-text">• Unique Constraints</text>
  <text x="390" y="280" class="rule-text">• User Types</text>
  <text x="390" y="295" class="rule-text">• Admin Access</text>
  <text x="390" y="310" class="rule-text">• CRUD Operations</text>
  
  <!-- Reporting Group -->
  <rect x="530" y="200" width="150" height="100" class="utility-box"/>
  <text x="605" y="220" class="group-title">Reporting Validations</text>
  <text x="605" y="235" class="rule-text">RULE-VAL-071-082: Report Rules</text>
  <text x="605" y="250" class="rule-text">• Date Components</text>
  <text x="605" y="265" class="rule-text">• Range Validation</text>
  <text x="605" y="280" class="rule-text">• Numeric Checks</text>
  <text x="605" y="295" class="rule-text">• Required Fields</text>

  <!-- Utility Validations Group -->
  <rect x="50" y="360" width="150" height="80" class="utility-box"/>
  <text x="125" y="380" class="group-title">Utility Validations</text>
  <text x="125" y="395" class="rule-text">RULE-VAL-059-060: Date Utils</text>
  <text x="125" y="410" class="rule-text">• CEEDAYS API</text>
  <text x="125" y="425" class="rule-text">• Format Validation</text>
  
  <!-- Shared Validation Patterns -->
  <rect x="220" y="460" width="400" height="120" class="group-box"/>
  <text x="420" y="480" class="group-title">Shared Validation Patterns</text>
  <text x="420" y="500" class="rule-text">Common Validation Types Used Across Modules:</text>
  <text x="420" y="520" class="rule-text">• Required Field Checks (SPACES/LOW-VALUES)</text>
  <text x="420" y="535" class="rule-text">• Numeric Validation (IS NUMERIC)</text>
  <text x="420" y="550" class="rule-text">• Yes/No Validation ('Y'/'N')</text>
  <text x="420" y="565" class="rule-text">• Date Format Validation (YYYY-MM-DD)</text>
  
  <!-- Connection Lines -->
  <line x1="200" y1="90" x2="250" y2="90" class="connection"/>
  <line x1="450" y1="110" x2="500" y2="110" class="connection"/>
  <line x1="125" y1="130" x2="150" y2="200" class="connection"/>
  <line x1="350" y1="170" x2="390" y2="200" class="connection"/>
  <line x1="590" y1="170" x2="605" y2="200" class="connection"/>
  
  <!-- Legend -->
  <text x="50" y="620" class="group-title">Validation Flow Dependencies:</text>
  <text x="50" y="640" class="rule-text">Authentication → Account/Card/Transaction/User/Reporting Operations</text>
  <text x="50" y="655" class="rule-text">Utility Validations → Called by all modules for date/format checks</text>
  <text x="50" y="670" class="rule-text">Shared patterns ensure consistency across all business domains</text>
</svg>
```

---

## Summary

This document contains **93 business validation rules** extracted from the CardDemo COBOL application, organized into 8 functional areas:

- **Authentication**: 3 rules (RULE-VAL-001 to 003)
- **Account Management**: 19 rules (RULE-VAL-004 to 019, 061, 063-064)
- **Card Management**: 15 rules (RULE-VAL-020 to 031, 065-067)
- **Transaction Management**: 23 rules (RULE-VAL-032 to 051, 083-085)
- **User Management**: 11 rules (RULE-VAL-052 to 057, 086-093)
- **Administrative**: 4 rules (RULE-VAL-058, 062, 068-070)
- **Reporting**: 12 rules (RULE-VAL-071 to 082)
- **Utility**: 2 rules (RULE-VAL-059 to 060)

These validation rules represent the core business logic that should be migrated to modern validation layers in frontend and backend APIs, ensuring data integrity and business rule compliance in the modernized system.
