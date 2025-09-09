# CardDemo Application - Business Validation Rules

This document contains all business-level validation rules extracted from the CardDemo COBOL application. These rules are organized by functional category and are intended for migration to modern validation layers (frontend + backend APIs).

## Table of Contents

1. [User Authentication](#user-authentication)
2. [Menu Navigation](#menu-navigation)
3. [User Management](#user-management)
4. [Account Management](#account-management)
5. [Credit Card Management](#credit-card-management)
6. [Transaction Processing](#transaction-processing)
7. [Bill Payment](#bill-payment)
8. [Date/Time Validation](#datetime-validation)
9. [Reporting](#reporting)

---

## User Authentication

### RULE-VAL-001
**Rule Description**: User ID cannot be empty during sign-on
**COBOL Source Location**: COSGN00C.cbl, lines 118-122, PROCESS-ENTER-KEY paragraph
**Field(s) Involved**: USERIDI
**Validation Condition**: Field must not equal SPACES or LOW-VALUES
**Trigger Conditions**: When ENTER key is pressed on sign-on screen

### RULE-VAL-002
**Rule Description**: Password cannot be empty during sign-on
**COBOL Source Location**: COSGN00C.cbl, lines 123-127, PROCESS-ENTER-KEY paragraph
**Field(s) Involved**: PASSWDI
**Validation Condition**: Field must not equal SPACES or LOW-VALUES
**Trigger Conditions**: When ENTER key is pressed on sign-on screen

---

## Menu Navigation

### RULE-VAL-003
**Rule Description**: Menu option must be numeric and within valid range
**COBOL Source Location**: COMEN01C.cbl, lines 127-134, PROCESS-ENTER-KEY paragraph
**Field(s) Involved**: WS-OPTION
**Validation Condition**: Must be numeric, greater than zero, and not exceed CDEMO-MENU-OPT-COUNT
**Trigger Conditions**: When ENTER key is pressed on main menu screen

### RULE-VAL-004
**Rule Description**: Admin-only menu options restricted to admin users
**COBOL Source Location**: COMEN01C.cbl, lines 136-143, PROCESS-ENTER-KEY paragraph
**Field(s) Involved**: CDEMO-MENU-OPT-USRTYPE, CDEMO-USRTYP-USER
**Validation Condition**: If user type is 'USER' and menu option type is 'A' (Admin), access denied
**Trigger Conditions**: When regular user attempts to access admin-only menu option

### RULE-VAL-005
**Rule Description**: Admin menu option must be numeric and within valid range
**COBOL Source Location**: COADM01C.cbl, lines 127-134, PROCESS-ENTER-KEY paragraph
**Field(s) Involved**: WS-OPTION
**Validation Condition**: Must be numeric, greater than zero, and not exceed CDEMO-ADMIN-OPT-COUNT
**Trigger Conditions**: When ENTER key is pressed on admin menu screen

---

## User Management

### RULE-VAL-006
**Rule Description**: First name cannot be empty when adding user
**COBOL Source Location**: COUSR01C.cbl, lines 118-123, PROCESS-ENTER-KEY paragraph
**Field(s) Involved**: FNAMEI
**Validation Condition**: Field must not equal SPACES or LOW-VALUES
**Trigger Conditions**: When ENTER key is pressed on user add screen

### RULE-VAL-007
**Rule Description**: Last name cannot be empty when adding user
**COBOL Source Location**: COUSR01C.cbl, lines 124-129, PROCESS-ENTER-KEY paragraph
**Field(s) Involved**: LNAMEI
**Validation Condition**: Field must not equal SPACES or LOW-VALUES
**Trigger Conditions**: When ENTER key is pressed on user add screen

### RULE-VAL-008
**Rule Description**: User ID cannot be empty when adding user
**COBOL Source Location**: COUSR01C.cbl, lines 130-135, PROCESS-ENTER-KEY paragraph
**Field(s) Involved**: USERIDI
**Validation Condition**: Field must not equal SPACES or LOW-VALUES
**Trigger Conditions**: When ENTER key is pressed on user add screen

### RULE-VAL-009
**Rule Description**: Password cannot be empty when adding user
**COBOL Source Location**: COUSR01C.cbl, lines 136-141, PROCESS-ENTER-KEY paragraph
**Field(s) Involved**: PASSWDI
**Validation Condition**: Field must not equal SPACES or LOW-VALUES
**Trigger Conditions**: When ENTER key is pressed on user add screen

### RULE-VAL-010
**Rule Description**: User type cannot be empty when adding user
**COBOL Source Location**: COUSR01C.cbl, lines 142-147, PROCESS-ENTER-KEY paragraph
**Field(s) Involved**: USRTYPEI
**Validation Condition**: Field must not equal SPACES or LOW-VALUES
**Trigger Conditions**: When ENTER key is pressed on user add screen

### RULE-VAL-011
**Rule Description**: User ID cannot be empty when updating user
**COBOL Source Location**: COUSR02C.cbl, lines 146-151, PROCESS-ENTER-KEY paragraph
**Field(s) Involved**: USRIDINI
**Validation Condition**: Field must not equal SPACES or LOW-VALUES
**Trigger Conditions**: When ENTER key is pressed on user update screen

### RULE-VAL-012
**Rule Description**: First name cannot be empty when updating user
**COBOL Source Location**: COUSR02C.cbl, lines 186-191, UPDATE-USER-INFO paragraph
**Field(s) Involved**: FNAMEI
**Validation Condition**: Field must not equal SPACES or LOW-VALUES
**Trigger Conditions**: When PF5 key is pressed to update user information

### RULE-VAL-013
**Rule Description**: Last name cannot be empty when updating user
**COBOL Source Location**: COUSR02C.cbl, lines 192-197, UPDATE-USER-INFO paragraph
**Field(s) Involved**: LNAMEI
**Validation Condition**: Field must not equal SPACES or LOW-VALUES
**Trigger Conditions**: When PF5 key is pressed to update user information

### RULE-VAL-014
**Rule Description**: Password cannot be empty when updating user
**COBOL Source Location**: COUSR02C.cbl, lines 198-203, UPDATE-USER-INFO paragraph
**Field(s) Involved**: PASSWDI
**Validation Condition**: Field must not equal SPACES or LOW-VALUES
**Trigger Conditions**: When PF5 key is pressed to update user information

### RULE-VAL-015
**Rule Description**: User ID cannot be empty when deleting user
**COBOL Source Location**: COUSR03C.cbl, lines 145-150, PROCESS-ENTER-KEY paragraph
**Field(s) Involved**: USRIDINI
**Validation Condition**: Field must not equal SPACES or LOW-VALUES
**Trigger Conditions**: When ENTER key is pressed on user delete screen

### RULE-VAL-016
**Rule Description**: User selection code must be valid for user list operations
**COBOL Source Location**: COUSR00C.cbl, lines 189-210, PROCESS-ENTER-KEY paragraph
**Field(s) Involved**: CDEMO-CU00-USR-SEL-FLG
**Validation Condition**: Must be 'U' or 'u' for update, 'D' or 'd' for delete
**Trigger Conditions**: When user selection is made from user list screen

---

## Account Management

### RULE-VAL-017
**Rule Description**: Account number must be numeric when provided
**COBOL Source Location**: COACTUPC.cbl, lines 3800-3850, EDIT-ACCT-NUM paragraph
**Field(s) Involved**: ACCT-ID input field
**Validation Condition**: Must be numeric if not SPACES or LOW-VALUES
**Trigger Conditions**: When account number is entered for account update

### RULE-VAL-018
**Rule Description**: Account status must be Y or N
**COBOL Source Location**: COACTUPC.cbl, lines 193-195, WS-NON-KEY-FLAGS section
**Field(s) Involved**: WS-EDIT-ACCT-STATUS
**Validation Condition**: Must have values 'Y' or 'N' (88-level: FLG-ACCT-STATUS-ISVALID)
**Trigger Conditions**: When account status is being validated during account update

### RULE-VAL-019
**Rule Description**: Account number must be non-zero 11-digit number
**COBOL Source Location**: COACTVWC.cbl, lines 125-128, WS-RETURN-MSG section
**Field(s) Involved**: Account number input
**Validation Condition**: Must be 11 digits and not equal to zeros
**Trigger Conditions**: When account number is provided for account view

### RULE-VAL-020
**Rule Description**: SSN first part cannot be invalid values
**COBOL Source Location**: COACTUPC.cbl, lines 121-123, WS-EDIT-US-SSN section
**Field(s) Involved**: WS-EDIT-US-SSN-PART1-N
**Validation Condition**: Cannot be 0, 666, or 900-999 (88-level: INVALID-SSN-PART1)
**Trigger Conditions**: When SSN is being validated during account processing

### RULE-VAL-021
**Rule Description**: US phone number format validation
**COBOL Source Location**: COACTUPC.cbl, lines 102-115, WS-EDIT-US-PHONE-NUM-FLGS section
**Field(s) Involved**: WS-EDIT-US-PHONEA-FLG, WS-EDIT-US-PHONEB-FLG, WS-EDIT-US-PHONEC-FLG
**Validation Condition**: Each phone number part must be valid (88-level: FLG-EDIT-US-PHONEA-ISVALID, etc.)
**Trigger Conditions**: When US phone number is being validated during account processing

### RULE-VAL-022
**Rule Description**: Date of birth validation
**COBOL Source Location**: COACTUPC.cbl, lines 216-230, WS-EDIT-DT-OF-BIRTH-FLGS section
**Field(s) Involved**: WS-EDIT-DT-OF-BIRTH-YEAR-FLG, WS-EDIT-DT-OF-BIRTH-MONTH, WS-EDIT-DT-OF-BIRTH-DAY
**Validation Condition**: Year, month, and day components must be valid (88-level conditions)
**Trigger Conditions**: When date of birth is being validated during account processing

---

## Credit Card Management

### RULE-VAL-023
**Rule Description**: Card number must be 16-digit numeric
**COBOL Source Location**: COCRDUPC.cbl, lines 193-194, WS-RETURN-MSG section
**Field(s) Involved**: Card number input
**Validation Condition**: Must be 16 digits if supplied
**Trigger Conditions**: When card number is provided for card operations

### RULE-VAL-024
**Rule Description**: Card status must be Y or N
**COBOL Source Location**: COCRDUPC.cbl, lines 195-196, WS-RETURN-MSG section
**Field(s) Involved**: Card status field
**Validation Condition**: Must be 'Y' or 'N'
**Trigger Conditions**: When card active status is being set

### RULE-VAL-025
**Rule Description**: Card expiry month must be between 1 and 12
**COBOL Source Location**: COCRDUPC.cbl, lines 197-198, WS-RETURN-MSG section
**Field(s) Involved**: Card expiry month
**Validation Condition**: Must be numeric value between 1 and 12
**Trigger Conditions**: When card expiry date is being validated

### RULE-VAL-026
**Rule Description**: Card expiry year must be valid
**COBOL Source Location**: COCRDUPC.cbl, lines 199-200, WS-RETURN-MSG section
**Field(s) Involved**: Card expiry year
**Validation Condition**: Must be within valid year range
**Trigger Conditions**: When card expiry date is being validated

### RULE-VAL-027
**Rule Description**: Card name cannot be empty
**COBOL Source Location**: COCRDUPC.cbl, lines 181-182, WS-RETURN-MSG section
**Field(s) Involved**: Card name field
**Validation Condition**: Must not be empty
**Trigger Conditions**: When card name is being validated

### RULE-VAL-028
**Rule Description**: Card name must contain only alphabets and spaces
**COBOL Source Location**: COCRDUPC.cbl, lines 183-184, WS-RETURN-MSG section
**Field(s) Involved**: Card name field
**Validation Condition**: Can only contain alphabetic characters and spaces
**Trigger Conditions**: When card name format is being validated

### RULE-VAL-029
**Rule Description**: Account number must be provided for card operations
**COBOL Source Location**: COCRDUPC.cbl, lines 177-178, WS-RETURN-MSG section
**Field(s) Involved**: Account number
**Validation Condition**: Must not be empty
**Trigger Conditions**: When account number is required for card operations

### RULE-VAL-030
**Rule Description**: Card selection code must be valid
**COBOL Source Location**: COCRDLIC.cbl, lines 77-82, WS-EDIT-SELECT-ARRAY section
**Field(s) Involved**: WS-EDIT-SELECT
**Validation Condition**: Must be 'S' for select or 'U' for update (88-level: SELECT-OK)
**Trigger Conditions**: When card selection is made from card list

### RULE-VAL-031
**Rule Description**: Account number must be non-zero 11-digit number for card operations
**COBOL Source Location**: COCRDSLC.cbl, lines 144-147, WS-RETURN-MSG section
**Field(s) Involved**: Account number
**Validation Condition**: Must be 11 digits and not equal to zeros
**Trigger Conditions**: When account number is provided for card detail operations

---

## Transaction Processing

### RULE-VAL-032
**Rule Description**: Account ID must be numeric for transaction processing
**COBOL Source Location**: COTRN02C.cbl, lines 197-202, VALIDATE-INPUT-KEY-FIELDS paragraph
**Field(s) Involved**: ACTIDINI
**Validation Condition**: Must be numeric if not SPACES or LOW-VALUES
**Trigger Conditions**: When account ID is provided for transaction add

### RULE-VAL-033
**Rule Description**: Card number must be numeric for transaction processing
**COBOL Source Location**: COTRN02C.cbl, lines 211-216, VALIDATE-INPUT-KEY-FIELDS paragraph
**Field(s) Involved**: CARDNINI
**Validation Condition**: Must be numeric if not SPACES or LOW-VALUES
**Trigger Conditions**: When card number is provided for transaction add

### RULE-VAL-034
**Rule Description**: Either account or card number must be entered
**COBOL Source Location**: COTRN02C.cbl, lines 224-229, VALIDATE-INPUT-KEY-FIELDS paragraph
**Field(s) Involved**: ACTIDINI, CARDNINI
**Validation Condition**: At least one of account ID or card number must be provided
**Trigger Conditions**: When neither account nor card number is provided

### RULE-VAL-035
**Rule Description**: Transaction type code cannot be empty
**COBOL Source Location**: COTRN02C.cbl, lines 252-257, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved**: TTYPCDI
**Validation Condition**: Must not equal SPACES or LOW-VALUES
**Trigger Conditions**: When transaction data is being validated

### RULE-VAL-036
**Rule Description**: Transaction category code cannot be empty
**COBOL Source Location**: COTRN02C.cbl, lines 258-263, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved**: TCATCDI
**Validation Condition**: Must not equal SPACES or LOW-VALUES
**Trigger Conditions**: When transaction data is being validated

### RULE-VAL-037
**Rule Description**: Transaction source cannot be empty
**COBOL Source Location**: COTRN02C.cbl, lines 264-269, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved**: TRNSRCI
**Validation Condition**: Must not equal SPACES or LOW-VALUES
**Trigger Conditions**: When transaction data is being validated

### RULE-VAL-038
**Rule Description**: Transaction description cannot be empty
**COBOL Source Location**: COTRN02C.cbl, lines 270-275, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved**: TDESCI
**Validation Condition**: Must not equal SPACES or LOW-VALUES
**Trigger Conditions**: When transaction data is being validated

### RULE-VAL-039
**Rule Description**: Transaction amount cannot be empty
**COBOL Source Location**: COTRN02C.cbl, lines 276-281, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved**: TRNAMTI
**Validation Condition**: Must not equal SPACES or LOW-VALUES
**Trigger Conditions**: When transaction data is being validated

### RULE-VAL-040
**Rule Description**: Transaction original date cannot be empty
**COBOL Source Location**: COTRN02C.cbl, lines 282-287, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved**: TORIGDTI
**Validation Condition**: Must not equal SPACES or LOW-VALUES
**Trigger Conditions**: When transaction data is being validated

### RULE-VAL-041
**Rule Description**: Transaction processing date cannot be empty
**COBOL Source Location**: COTRN02C.cbl, lines 288-293, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved**: TPROCDTI
**Validation Condition**: Must not equal SPACES or LOW-VALUES
**Trigger Conditions**: When transaction data is being validated

### RULE-VAL-042
**Rule Description**: Merchant ID cannot be empty
**COBOL Source Location**: COTRN02C.cbl, lines 294-299, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved**: MIDI
**Validation Condition**: Must not equal SPACES or LOW-VALUES
**Trigger Conditions**: When transaction data is being validated

### RULE-VAL-043
**Rule Description**: Confirmation must be Y or N for transaction add
**COBOL Source Location**: COTRN02C.cbl, lines 169-187, PROCESS-ENTER-KEY paragraph
**Field(s) Involved**: CONFIRMI
**Validation Condition**: Must be 'Y', 'y', 'N', or 'n'
**Trigger Conditions**: When transaction add confirmation is requested

### RULE-VAL-044
**Rule Description**: Transaction ID cannot be empty for transaction view
**COBOL Source Location**: COTRN01C.cbl, lines 147-152, PROCESS-ENTER-KEY paragraph
**Field(s) Involved**: TRNIDINI
**Validation Condition**: Must not equal SPACES or LOW-VALUES
**Trigger Conditions**: When transaction ID is required for transaction view

### RULE-VAL-045
**Rule Description**: Transaction selection code must be valid
**COBOL Source Location**: COTRN00C.cbl, lines 185-200, PROCESS-ENTER-KEY paragraph
**Field(s) Involved**: CDEMO-CT00-TRN-SEL-FLG
**Validation Condition**: Must be 'S' or 's' for select
**Trigger Conditions**: When transaction selection is made from transaction list

---

## Bill Payment

### RULE-VAL-046
**Rule Description**: Bill payment confirmation must be Y or N
**COBOL Source Location**: COBIL00C.cbl, lines 182-190, PROCESS-ENTER-KEY paragraph
**Field(s) Involved**: CONFIRMI
**Validation Condition**: Must be 'Y' or 'N'
**Trigger Conditions**: When bill payment confirmation is requested

### RULE-VAL-047
**Rule Description**: Account must have positive balance for bill payment
**COBOL Source Location**: COBIL00C.cbl, lines 198-205, PROCESS-ENTER-KEY paragraph
**Field(s) Involved**: ACCT-CURR-BAL
**Validation Condition**: Current balance must be greater than zero
**Trigger Conditions**: When bill payment is being processed

---

## Date/Time Validation

### RULE-VAL-048
**Rule Description**: Date format validation using CEEDAYS API
**COBOL Source Location**: CSUTLDTC.cbl, lines 128-149, A000-MAIN paragraph
**Field(s) Involved**: LS-DATE, LS-DATE-FORMAT
**Validation Condition**: Date must be valid according to specified format using CEEDAYS API
**Trigger Conditions**: When date validation is requested through CSUTLDTC utility

### RULE-VAL-049
**Rule Description**: Invalid date feedback codes validation
**COBOL Source Location**: CSUTLDTC.cbl, lines 62-70, FEEDBACK-CODE section
**Field(s) Involved**: FEEDBACK-TOKEN-VALUE
**Validation Condition**: Various invalid date conditions (88-level: FC-INVALID-DATE, FC-BAD-DATE-VALUE, FC-INVALID-MONTH, etc.)
**Trigger Conditions**: When CEEDAYS API returns error feedback codes

---

## Reporting

### RULE-VAL-050
**Rule Description**: Report date parameters validation
**COBOL Source Location**: CORPT00C.cbl, lines 60-72, WS-START-DATE and WS-END-DATE sections
**Field(s) Involved**: WS-START-DATE-YYYY, WS-START-DATE-MM, WS-START-DATE-DD, WS-END-DATE-YYYY, WS-END-DATE-MM, WS-END-DATE-DD
**Validation Condition**: Date components must follow YYYY-MM-DD format
**Trigger Conditions**: When report parameters are being validated for transaction reports

---

## Summary

This document contains **50 business validation rules** extracted from **18 COBOL programs** in the CardDemo application. The rules are categorized into:

- **User Authentication**: 2 rules
- **Menu Navigation**: 3 rules  
- **User Management**: 11 rules
- **Account Management**: 6 rules
- **Credit Card Management**: 10 rules
- **Transaction Processing**: 14 rules
- **Bill Payment**: 2 rules
- **Date/Time Validation**: 2 rules
- **Reporting**: 1 rule

These validation rules represent the core business logic that should be preserved when modernizing the CardDemo application to contemporary web-based architectures with frontend and backend API validation layers.

## Key Validation Patterns Identified

1. **Mandatory Field Validation**: Consistent use of `SPACES OR LOW-VALUES` checks
2. **Numeric Validation**: `IS NOT NUMERIC` condition pattern
3. **Range Validation**: 88-level conditions for valid value ranges
4. **Format Validation**: Specific patterns for SSN, phone numbers, dates
5. **Confirmation Validation**: Y/N validation pattern for user confirmations
6. **Selection Code Validation**: S/U/D codes for Select/Update/Delete operations
7. **Cross-Field Validation**: Either/or requirements between related fields
8. **Domain-Specific Validation**: Business rules specific to banking/credit card domain
