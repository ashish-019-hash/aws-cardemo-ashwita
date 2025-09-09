# COBOL Business Validation Rules - CardDemo Application

## Overview
This document contains business-level validation rules extracted from the CardDemo COBOL application. These rules represent the core business logic that should be preserved during modernization to frontend and backend API validation layers.

## Validation Rules by Domain

### 1. Authentication Domain

#### RULE-VAL-001
- **Rule Description**: User ID is required for authentication
- **COBOL Source Location**: COSGN00C.cbl, lines 118-122, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: USERIDI
- **Validation Condition**: Field must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When ENTER key is pressed on sign-on screen

#### RULE-VAL-002
- **Rule Description**: Password is required for authentication
- **COBOL Source Location**: COSGN00C.cbl, lines 123-127, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: PASSWDI
- **Validation Condition**: Field must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When ENTER key is pressed on sign-on screen

#### RULE-VAL-003
- **Rule Description**: User must exist in security file
- **COBOL Source Location**: COSGN00C.cbl, lines 247-251, paragraph READ-USER-SEC-FILE
- **Field(s) Involved**: SEC-USR-ID
- **Validation Condition**: User record must be found in USRSEC file
- **Trigger Conditions**: After user ID validation passes

### 2. Account Management Domain

#### RULE-VAL-004
- **Rule Description**: Account status must be valid value
- **COBOL Source Location**: COACTUPC.cbl, lines 192-195, paragraph VALIDATE-ACCOUNT-STATUS
- **Field(s) Involved**: ACCT-ACTIVE-STATUS
- **Validation Condition**: Must be 'Y' (Active) or 'N' (Inactive)
- **Trigger Conditions**: When account update is processed

#### RULE-VAL-005
- **Rule Description**: Social Security Number must be 9 digits
- **COBOL Source Location**: COACTUPC.cbl, lines 121-123, paragraph VALIDATE-SSN
- **Field(s) Involved**: CUST-SSN
- **Validation Condition**: Must be exactly 9 numeric digits
- **Trigger Conditions**: When customer information is updated

#### RULE-VAL-006
- **Rule Description**: Phone number must be 10 digits
- **COBOL Source Location**: COACTUPC.cbl, lines 124-126, paragraph VALIDATE-PHONE
- **Field(s) Involved**: CUST-PHONE
- **Validation Condition**: Must be exactly 10 numeric digits
- **Trigger Conditions**: When customer information is updated

#### RULE-VAL-007
- **Rule Description**: Account ID must be numeric and not empty
- **COBOL Source Location**: COACTVWC.cbl, lines 125-128, validation messages
- **Field(s) Involved**: Account ID input field
- **Validation Condition**: Must be non-zero 11-digit number
- **Trigger Conditions**: When account view is requested

### 3. Credit Card Domain

#### RULE-VAL-008
- **Rule Description**: Card name must contain only alphabetic characters
- **COBOL Source Location**: COCRDUPC.cbl, lines 88-90, paragraph VALIDATE-CARD-NAME
- **Field(s) Involved**: CARD-EMBOSSED-NAME
- **Validation Condition**: Must contain only letters A-Z, a-z, and spaces
- **Trigger Conditions**: When credit card details are updated

#### RULE-VAL-009
- **Rule Description**: Card status must be valid value
- **COBOL Source Location**: COCRDUPC.cbl, lines 195-196, paragraph VALIDATE-CARD-STATUS
- **Field(s) Involved**: CARD-ACTIVE-STATUS
- **Validation Condition**: Must be 'Y' (Active) or 'N' (Inactive)
- **Trigger Conditions**: When credit card is updated

#### RULE-VAL-010
- **Rule Description**: Card expiry month must be valid
- **COBOL Source Location**: COCRDUPC.cbl, line 95, paragraph VALIDATE-EXPIRY-MONTH
- **Field(s) Involved**: CARD-EXPIRY-MONTH
- **Validation Condition**: Must be between 01 and 12
- **Trigger Conditions**: When credit card expiry date is updated

#### RULE-VAL-011
- **Rule Description**: Card expiry year must be within valid range
- **COBOL Source Location**: COCRDUPC.cbl, line 99, paragraph VALIDATE-EXPIRY-YEAR
- **Field(s) Involved**: CARD-EXPIRY-YEAR
- **Validation Condition**: Must be between 1950 and 2099
- **Trigger Conditions**: When credit card expiry date is updated

#### RULE-VAL-012
- **Rule Description**: Account number must be 11-digit numeric
- **COBOL Source Location**: COCRDSLC.cbl, lines 145-147, validation messages
- **Field(s) Involved**: Account number input
- **Validation Condition**: Must be non-zero 11-digit number
- **Trigger Conditions**: When card details are searched by account

#### RULE-VAL-013
- **Rule Description**: Card number must be 16-digit numeric
- **COBOL Source Location**: COCRDSLC.cbl, lines 148-149, validation messages
- **Field(s) Involved**: Card number input
- **Validation Condition**: Must be 16-digit number if provided
- **Trigger Conditions**: When card details are searched by card number

### 4. Transaction Domain

#### RULE-VAL-014
- **Rule Description**: Transaction ID must be numeric
- **COBOL Source Location**: COTRN00C.cbl, lines 209-218, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: TRNIDINI
- **Validation Condition**: Must be numeric value
- **Trigger Conditions**: When transaction search is performed

#### RULE-VAL-015
- **Rule Description**: Transaction ID is required for transaction view
- **COBOL Source Location**: COTRN01C.cbl, lines 147-152, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: TRNIDINI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When transaction details are requested

#### RULE-VAL-016
- **Rule Description**: Account ID must be numeric for transaction creation
- **COBOL Source Location**: COTRN02C.cbl, lines 197-202, paragraph VALIDATE-INPUT-KEY-FIELDS
- **Field(s) Involved**: ACTIDINI
- **Validation Condition**: Must be numeric value
- **Trigger Conditions**: When new transaction is created

#### RULE-VAL-017
- **Rule Description**: Card number must be numeric for transaction creation
- **COBOL Source Location**: COTRN02C.cbl, lines 211-216, paragraph VALIDATE-INPUT-KEY-FIELDS
- **Field(s) Involved**: CARDNINI
- **Validation Condition**: Must be numeric value
- **Trigger Conditions**: When new transaction is created

#### RULE-VAL-018
- **Rule Description**: Either account ID or card number must be provided
- **COBOL Source Location**: COTRN02C.cbl, lines 224-229, paragraph VALIDATE-INPUT-KEY-FIELDS
- **Field(s) Involved**: ACTIDINI, CARDNINI
- **Validation Condition**: At least one field must be provided
- **Trigger Conditions**: When new transaction is created

#### RULE-VAL-019
- **Rule Description**: Transaction type code cannot be empty
- **COBOL Source Location**: COTRN02C.cbl, lines 252-257, paragraph VALIDATE-INPUT-DATA-FIELDS
- **Field(s) Involved**: TTYPCDI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When transaction data is validated

#### RULE-VAL-020
- **Rule Description**: Transaction category code cannot be empty
- **COBOL Source Location**: COTRN02C.cbl, lines 258-263, paragraph VALIDATE-INPUT-DATA-FIELDS
- **Field(s) Involved**: TCATCDI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When transaction data is validated

#### RULE-VAL-021
- **Rule Description**: Transaction source cannot be empty
- **COBOL Source Location**: COTRN02C.cbl, lines 264-269, paragraph VALIDATE-INPUT-DATA-FIELDS
- **Field(s) Involved**: TRNSRCI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When transaction data is validated

#### RULE-VAL-022
- **Rule Description**: Transaction description cannot be empty
- **COBOL Source Location**: COTRN02C.cbl, lines 270-275, paragraph VALIDATE-INPUT-DATA-FIELDS
- **Field(s) Involved**: TDESCI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When transaction data is validated

#### RULE-VAL-023
- **Rule Description**: Transaction amount cannot be empty
- **COBOL Source Location**: COTRN02C.cbl, lines 276-281, paragraph VALIDATE-INPUT-DATA-FIELDS
- **Field(s) Involved**: TRNAMTI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When transaction data is validated

#### RULE-VAL-024
- **Rule Description**: Transaction type code must be numeric
- **COBOL Source Location**: COTRN02C.cbl, lines 323-328, paragraph VALIDATE-INPUT-DATA-FIELDS
- **Field(s) Involved**: TTYPCDI
- **Validation Condition**: Must be numeric value
- **Trigger Conditions**: When transaction data is validated

#### RULE-VAL-025
- **Rule Description**: Transaction category code must be numeric
- **COBOL Source Location**: COTRN02C.cbl, lines 329-334, paragraph VALIDATE-INPUT-DATA-FIELDS
- **Field(s) Involved**: TCATCDI
- **Validation Condition**: Must be numeric value
- **Trigger Conditions**: When transaction data is validated

#### RULE-VAL-026
- **Rule Description**: Transaction amount must be in correct format
- **COBOL Source Location**: COTRN02C.cbl, lines 340-351, paragraph VALIDATE-INPUT-DATA-FIELDS
- **Field(s) Involved**: TRNAMTI
- **Validation Condition**: Must be in format -99999999.99 (sign, 8 digits, decimal point, 2 digits)
- **Trigger Conditions**: When transaction data is validated

#### RULE-VAL-027
- **Rule Description**: Transaction origin date must be in YYYY-MM-DD format
- **COBOL Source Location**: COTRN02C.cbl, lines 354-366, paragraph VALIDATE-INPUT-DATA-FIELDS
- **Field(s) Involved**: TORIGDTI
- **Validation Condition**: Must match YYYY-MM-DD pattern with numeric year, month, day
- **Trigger Conditions**: When transaction data is validated

#### RULE-VAL-028
- **Rule Description**: Transaction process date must be in YYYY-MM-DD format
- **COBOL Source Location**: COTRN02C.cbl, lines 368-373, paragraph VALIDATE-INPUT-DATA-FIELDS
- **Field(s) Involved**: TPROCDTI
- **Validation Condition**: Must match YYYY-MM-DD pattern with numeric year, month, day
- **Trigger Conditions**: When transaction data is validated

### 5. User Management Domain

#### RULE-VAL-029
- **Rule Description**: First name is required for user creation
- **COBOL Source Location**: COUSR01C.cbl, lines 118-123, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: FNAMEI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When new user is created

#### RULE-VAL-030
- **Rule Description**: Last name is required for user creation
- **COBOL Source Location**: COUSR01C.cbl, lines 124-129, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: LNAMEI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When new user is created

#### RULE-VAL-031
- **Rule Description**: User ID is required for user creation
- **COBOL Source Location**: COUSR01C.cbl, lines 130-135, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: USERIDI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When new user is created

#### RULE-VAL-032
- **Rule Description**: Password is required for user creation
- **COBOL Source Location**: COUSR01C.cbl, lines 136-141, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: PASSWDI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When new user is created

#### RULE-VAL-033
- **Rule Description**: User type is required for user creation
- **COBOL Source Location**: COUSR01C.cbl, lines 142-147, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: USRTYPEI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When new user is created

#### RULE-VAL-034
- **Rule Description**: User ID is required for user update
- **COBOL Source Location**: COUSR02C.cbl, lines 146-151, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: USRIDINI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When user update is initiated

#### RULE-VAL-035
- **Rule Description**: First name is required for user update
- **COBOL Source Location**: COUSR02C.cbl, lines 186-191, paragraph UPDATE-USER-INFO
- **Field(s) Involved**: FNAMEI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When user information is updated

#### RULE-VAL-036
- **Rule Description**: Last name is required for user update
- **COBOL Source Location**: COUSR02C.cbl, lines 192-197, paragraph UPDATE-USER-INFO
- **Field(s) Involved**: LNAMEI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When user information is updated

#### RULE-VAL-037
- **Rule Description**: Password is required for user update
- **COBOL Source Location**: COUSR02C.cbl, lines 198-203, paragraph UPDATE-USER-INFO
- **Field(s) Involved**: PASSWDI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When user information is updated

#### RULE-VAL-038
- **Rule Description**: User type is required for user update
- **COBOL Source Location**: COUSR02C.cbl, lines 204-209, paragraph UPDATE-USER-INFO
- **Field(s) Involved**: USRTYPEI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When user information is updated

#### RULE-VAL-039
- **Rule Description**: User ID is required for user deletion
- **COBOL Source Location**: COUSR03C.cbl, lines 145-150, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: USRIDINI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When user deletion is initiated

#### RULE-VAL-040
- **Rule Description**: Valid selection action required for user list
- **COBOL Source Location**: COUSR00C.cbl, lines 210-215, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: Selection flags (SEL0001I-SEL0010I)
- **Validation Condition**: Must be 'U' (Update) or 'D' (Delete)
- **Trigger Conditions**: When user is selected from user list

### 6. Bill Payment Domain

#### RULE-VAL-041
- **Rule Description**: Account ID is required for bill payment
- **COBOL Source Location**: COBIL00C.cbl, lines 159-164, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: ACTIDINI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When bill payment is initiated

#### RULE-VAL-042
- **Rule Description**: Confirmation must be valid value for bill payment
- **COBOL Source Location**: COBIL00C.cbl, lines 173-191, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: CONFIRMI
- **Validation Condition**: Must be 'Y', 'y', 'N', 'n', SPACES, or LOW-VALUES
- **Trigger Conditions**: When bill payment confirmation is processed

#### RULE-VAL-043
- **Rule Description**: Account must have positive balance for bill payment
- **COBOL Source Location**: COBIL00C.cbl, lines 198-205, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: ACCT-CURR-BAL
- **Validation Condition**: Current balance must be greater than zero
- **Trigger Conditions**: When bill payment amount is validated

### 7. Reporting Domain

#### RULE-VAL-044
- **Rule Description**: Start date month is required for custom reports
- **COBOL Source Location**: CORPT00C.cbl, lines 259-265, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: SDTMMI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When custom report is requested

#### RULE-VAL-045
- **Rule Description**: Start date day is required for custom reports
- **COBOL Source Location**: CORPT00C.cbl, lines 266-272, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: SDTDDI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When custom report is requested

#### RULE-VAL-046
- **Rule Description**: Start date year is required for custom reports
- **COBOL Source Location**: CORPT00C.cbl, lines 273-279, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: SDTYYYYI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When custom report is requested

#### RULE-VAL-047
- **Rule Description**: End date month is required for custom reports
- **COBOL Source Location**: CORPT00C.cbl, lines 280-286, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: EDTMMI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When custom report is requested

#### RULE-VAL-048
- **Rule Description**: End date day is required for custom reports
- **COBOL Source Location**: CORPT00C.cbl, lines 287-293, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: EDTDDI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When custom report is requested

#### RULE-VAL-049
- **Rule Description**: End date year is required for custom reports
- **COBOL Source Location**: CORPT00C.cbl, lines 294-300, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: EDTYYYYI
- **Validation Condition**: Must not be SPACES or LOW-VALUES
- **Trigger Conditions**: When custom report is requested

#### RULE-VAL-050
- **Rule Description**: Start date month must be valid range
- **COBOL Source Location**: CORPT00C.cbl, lines 329-336, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: SDTMMI
- **Validation Condition**: Must be numeric and between 01 and 12
- **Trigger Conditions**: When custom report date validation is performed

#### RULE-VAL-051
- **Rule Description**: Start date day must be valid range
- **COBOL Source Location**: CORPT00C.cbl, lines 338-345, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: SDTDDI
- **Validation Condition**: Must be numeric and between 01 and 31
- **Trigger Conditions**: When custom report date validation is performed

#### RULE-VAL-052
- **Rule Description**: Start date year must be numeric
- **COBOL Source Location**: CORPT00C.cbl, lines 347-353, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: SDTYYYYI
- **Validation Condition**: Must be numeric value
- **Trigger Conditions**: When custom report date validation is performed

#### RULE-VAL-053
- **Rule Description**: End date month must be valid range
- **COBOL Source Location**: CORPT00C.cbl, lines 355-362, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: EDTMMI
- **Validation Condition**: Must be numeric and between 01 and 12
- **Trigger Conditions**: When custom report date validation is performed

#### RULE-VAL-054
- **Rule Description**: End date day must be valid range
- **COBOL Source Location**: CORPT00C.cbl, lines 364-371, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: EDTDDI
- **Validation Condition**: Must be numeric and between 01 and 31
- **Trigger Conditions**: When custom report date validation is performed

#### RULE-VAL-055
- **Rule Description**: End date year must be numeric
- **COBOL Source Location**: CORPT00C.cbl, lines 373-379, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: EDTYYYYI
- **Validation Condition**: Must be numeric value
- **Trigger Conditions**: When custom report date validation is performed

### 8. Menu and Navigation Domain

#### RULE-VAL-056
- **Rule Description**: Menu option must be valid number
- **COBOL Source Location**: COMEN01C.cbl, lines 127-134, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: WS-OPTION
- **Validation Condition**: Must be numeric, greater than zero, and not exceed maximum option count
- **Trigger Conditions**: When menu option is selected

#### RULE-VAL-057
- **Rule Description**: User must have appropriate access level for menu option
- **COBOL Source Location**: COMEN01C.cbl, lines 136-143, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: CDEMO-MENU-OPT-USRTYPE, CDEMO-USRTYP-USER
- **Validation Condition**: Regular users cannot access admin-only options (marked with 'A')
- **Trigger Conditions**: When menu option is selected by regular user

#### RULE-VAL-058
- **Rule Description**: Admin menu option must be valid number
- **COBOL Source Location**: COADM01C.cbl, lines 127-134, paragraph PROCESS-ENTER-KEY
- **Field(s) Involved**: WS-OPTION
- **Validation Condition**: Must be numeric, greater than zero, and not exceed maximum admin option count
- **Trigger Conditions**: When admin menu option is selected

### 9. Date Utility Domain

#### RULE-VAL-059
- **Rule Description**: Date must be in valid format for date utility
- **COBOL Source Location**: CSUTLDTC.cbl, lines 128-149, paragraph MAIN-PARA
- **Field(s) Involved**: CSUTLDTC-DATE
- **Validation Condition**: Date must pass CEEDAYS API validation
- **Trigger Conditions**: When date validation utility is called

#### RULE-VAL-060
- **Rule Description**: Date must not be invalid according to calendar rules
- **COBOL Source Location**: CSUTLDTC.cbl, lines 128-149, error handling
- **Field(s) Involved**: CSUTLDTC-DATE
- **Validation Condition**: Must be valid calendar date (no Feb 30, etc.)
- **Trigger Conditions**: When date validation utility processes date

## Summary

This document contains **60 business validation rules** extracted from **18 COBOL programs** in the CardDemo application. These rules cover:

- **Authentication**: 3 rules (User ID, Password, User existence)
- **Account Management**: 4 rules (Status, SSN, Phone, Account ID format)
- **Credit Card**: 6 rules (Name format, Status, Expiry validation, Number formats)
- **Transaction**: 15 rules (ID validation, Required fields, Format validation, Date validation)
- **User Management**: 12 rules (Required fields for CRUD operations, Selection validation)
- **Bill Payment**: 3 rules (Account ID, Confirmation, Balance validation)
- **Reporting**: 12 rules (Date field requirements and range validation)
- **Menu/Navigation**: 3 rules (Option validation, Access control)
- **Date Utility**: 2 rules (Format and calendar validation)

These validation rules represent the core business logic that must be preserved when modernizing the application to use frontend validation and backend API validation layers.
