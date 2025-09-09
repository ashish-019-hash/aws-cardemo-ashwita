# COBOL Business Validation Rules Extraction

## Overview
This document contains business-level validation rules extracted from the CardDemo COBOL application. These rules are intended for migration to modern validation layers (frontend + backend APIs).

## Validation Rules

### RULE-VAL-001: SSN Part 1 Invalid Values
- **Rule Description**: Social Security Number first part cannot be 0, 666, or 900-999
- **COBOL Source Location**: COACTUPC.cbl, lines 121-123
- **Field(s) Involved**: WS-EDIT-US-SSN-PART1-N
- **Validation Condition**: SSN first 3 digits must not equal 0, 666, or be in range 900-999
- **Trigger Conditions**: When SSN is being validated during account update

### RULE-VAL-002: Yes/No Field Validation
- **Rule Description**: Fields accepting Y/N values must contain only 'Y' or 'N'
- **COBOL Source Location**: COACTUPC.cbl, lines 78-80
- **Field(s) Involved**: WS-EDIT-YES-NO
- **Validation Condition**: Field value must be 'Y' or 'N'
- **Trigger Conditions**: When Y/N fields are being validated

### RULE-VAL-003: Account Status Validation
- **Rule Description**: Account status must be 'Y' (active) or 'N' (inactive)
- **COBOL Source Location**: COACTUPC.cbl, lines 193-195
- **Field(s) Involved**: WS-EDIT-ACCT-STATUS
- **Validation Condition**: Account status must be 'Y' or 'N'
- **Trigger Conditions**: When account status is being updated

### RULE-VAL-004: Primary Cardholder Indicator Validation
- **Rule Description**: Primary cardholder indicator must be 'Y' or 'N'
- **COBOL Source Location**: COACTUPC.cbl, lines 350-352
- **Field(s) Involved**: WS-EDIT-PRI-CARDHOLDER
- **Validation Condition**: Primary cardholder flag must be 'Y' or 'N'
- **Trigger Conditions**: When cardholder information is being validated

### RULE-VAL-005: Card Expiry Month Range Validation
- **Rule Description**: Card expiry month must be between 1 and 12
- **COBOL Source Location**: COCRDUPC.cbl, line 95
- **Field(s) Involved**: CARD-MONTH-CHECK-N
- **Validation Condition**: Month value must be in range 1-12
- **Trigger Conditions**: When card expiry date is being validated

### RULE-VAL-006: Card Expiry Year Range Validation
- **Rule Description**: Card expiry year must be between 1950 and 2099
- **COBOL Source Location**: COCRDUPC.cbl, line 99
- **Field(s) Involved**: CARD-YEAR-CHECK-N
- **Validation Condition**: Year value must be in range 1950-2099
- **Trigger Conditions**: When card expiry date is being validated

### RULE-VAL-007: Card Status Y/N Validation
- **Rule Description**: Card active status must be 'Y' or 'N'
- **COBOL Source Location**: COCRDUPC.cbl, lines 91-91
- **Field(s) Involved**: FLG-YES-NO-CHECK
- **Validation Condition**: Card status must be 'Y' or 'N'
- **Trigger Conditions**: When card status is being updated

### RULE-VAL-008: User ID Mandatory Field
- **Rule Description**: User ID cannot be empty or spaces
- **COBOL Source Location**: COSGN00C.cbl, lines 118-122
- **Field(s) Involved**: USERIDI OF COSGN0AI
- **Validation Condition**: User ID must not be spaces or low-values
- **Trigger Conditions**: During user sign-on process

### RULE-VAL-009: Password Mandatory Field
- **Rule Description**: Password cannot be empty or spaces
- **COBOL Source Location**: COSGN00C.cbl, lines 123-127
- **Field(s) Involved**: PASSWDI OF COSGN0AI
- **Validation Condition**: Password must not be spaces or low-values
- **Trigger Conditions**: During user sign-on process

### RULE-VAL-010: First Name Mandatory Field
- **Rule Description**: First name cannot be empty
- **COBOL Source Location**: COUSR01C.cbl, lines 118-123
- **Field(s) Involved**: FNAMEI OF COUSR1AI
- **Validation Condition**: First name must not be spaces or low-values
- **Trigger Conditions**: When creating new user

### RULE-VAL-011: Last Name Mandatory Field
- **Rule Description**: Last name cannot be empty
- **COBOL Source Location**: COUSR01C.cbl, lines 124-129
- **Field(s) Involved**: LNAMEI OF COUSR1AI
- **Validation Condition**: Last name must not be spaces or low-values
- **Trigger Conditions**: When creating new user

### RULE-VAL-012: User ID Mandatory for User Creation
- **Rule Description**: User ID cannot be empty during user creation
- **COBOL Source Location**: COUSR01C.cbl, lines 130-135
- **Field(s) Involved**: USERIDI OF COUSR1AI
- **Validation Condition**: User ID must not be spaces or low-values
- **Trigger Conditions**: When creating new user

### RULE-VAL-013: Password Mandatory for User Creation
- **Rule Description**: Password cannot be empty during user creation
- **COBOL Source Location**: COUSR01C.cbl, lines 136-141
- **Field(s) Involved**: PASSWDI OF COUSR1AI
- **Validation Condition**: Password must not be spaces or low-values
- **Trigger Conditions**: When creating new user

### RULE-VAL-014: User Type Mandatory Field
- **Rule Description**: User type cannot be empty
- **COBOL Source Location**: COUSR01C.cbl, lines 142-147
- **Field(s) Involved**: USRTYPEI OF COUSR1AI
- **Validation Condition**: User type must not be spaces or low-values
- **Trigger Conditions**: When creating new user

### RULE-VAL-015: Transaction ID Mandatory Field
- **Rule Description**: Transaction ID cannot be empty
- **COBOL Source Location**: COTRN02C.cbl, lines 147-152 (inferred from pattern)
- **Field(s) Involved**: Transaction ID input field
- **Validation Condition**: Transaction ID must not be spaces or low-values
- **Trigger Conditions**: When creating new transaction

### RULE-VAL-016: Phone Number Format Validation
- **Rule Description**: US phone number must follow (XXX)XXX-XXXX format with numeric parts
- **COBOL Source Location**: COACTUPC.cbl, lines 82-100
- **Field(s) Involved**: WS-EDIT-US-PHONE-NUM components
- **Validation Condition**: Area code, exchange, and number must be numeric
- **Trigger Conditions**: When phone number is being validated

### RULE-VAL-017: Date Format Validation
- **Rule Description**: Dates must be valid according to CEEDAYS API validation
- **COBOL Source Location**: CSUTLDTC.cbl, lines 128-149
- **Field(s) Involved**: LS-DATE, LS-DATE-FORMAT
- **Validation Condition**: Date must pass CEEDAYS validation checks
- **Trigger Conditions**: When date fields are being validated

### RULE-VAL-018: Account Number Non-Zero Validation
- **Rule Description**: Account number must be a non-zero 11-digit number
- **COBOL Source Location**: COACTUPC.cbl, lines 494-496
- **Field(s) Involved**: Account ID fields
- **Validation Condition**: Account number must be numeric and non-zero
- **Trigger Conditions**: When account number is being validated

### RULE-VAL-019: Card Number 16-Digit Validation
- **Rule Description**: Card number must be a 16-digit number if supplied
- **COBOL Source Location**: COCRDUPC.cbl, lines 193-194
- **Field(s) Involved**: Card number fields
- **Validation Condition**: Card number must be 16 digits if provided
- **Trigger Conditions**: When card number is being validated

### RULE-VAL-020: Name Alphabetic Validation
- **Rule Description**: Names can only contain alphabets and spaces
- **COBOL Source Location**: COACTUPC.cbl, lines 487-488
- **Field(s) Involved**: Name fields (first, middle, last)
- **Validation Condition**: Name fields must contain only alphabetic characters and spaces
- **Trigger Conditions**: When name fields are being validated

### RULE-VAL-021: Card Name Alphabetic Validation
- **Rule Description**: Card name can only contain alphabets and spaces
- **COBOL Source Location**: COCRDUPC.cbl, lines 183-184
- **Field(s) Involved**: Card name embossed field
- **Validation Condition**: Card name must contain only alphabetic characters and spaces
- **Trigger Conditions**: When card name is being validated

### RULE-VAL-022: User Type Admin/Regular Validation
- **Rule Description**: User type determines access to admin vs regular functions
- **COBOL Source Location**: COSGN00C.cbl, lines 230-240
- **Field(s) Involved**: SEC-USR-TYPE, CDEMO-USER-TYPE
- **Validation Condition**: User type controls program transfer to admin or regular menu
- **Trigger Conditions**: After successful user authentication

### RULE-VAL-023: Duplicate User ID Prevention
- **Rule Description**: User ID must be unique in the system
- **COBOL Source Location**: COUSR01C.cbl, lines 260-266
- **Field(s) Involved**: SEC-USR-ID
- **Validation Condition**: User ID must not already exist in USRSEC file
- **Trigger Conditions**: When creating new user account

### RULE-VAL-024: Password Authentication Validation
- **Rule Description**: Password must match stored password for user
- **COBOL Source Location**: COSGN00C.cbl, lines 223-246
- **Field(s) Involved**: SEC-USR-PWD, WS-USER-PWD
- **Validation Condition**: Entered password must match stored password
- **Trigger Conditions**: During user sign-on authentication

### RULE-VAL-025: Valid Menu Option Selection
- **Rule Description**: Menu option must be within valid range
- **COBOL Source Location**: COADM01C.cbl, lines 128-134
- **Field(s) Involved**: WS-OPTION, CDEMO-ADMIN-OPT-COUNT
- **Validation Condition**: Option number must be between 1 and maximum option count
- **Trigger Conditions**: When processing menu selection

## Validation Rule Categories

### Field-Level Validations
- Mandatory field checks (RULE-VAL-008 through RULE-VAL-015)
- Format validations (RULE-VAL-016, RULE-VAL-017)
- Length validations (RULE-VAL-018, RULE-VAL-019)
- Character type validations (RULE-VAL-020, RULE-VAL-021)

### Range Validations
- Month range 1-12 (RULE-VAL-005)
- Year range 1950-2099 (RULE-VAL-006)
- SSN part restrictions (RULE-VAL-001)
- Menu option range (RULE-VAL-025)

### Code/Value Checks
- Y/N field validations (RULE-VAL-002, RULE-VAL-003, RULE-VAL-004, RULE-VAL-007)
- User type validation (RULE-VAL-022)

### Domain-Specific Validations
- SSN format and restrictions (RULE-VAL-001)
- Phone number format (RULE-VAL-016)
- Date format validation (RULE-VAL-017)
- Account/Card number formats (RULE-VAL-018, RULE-VAL-019)

### Conditional Validations
- User type-based access control (RULE-VAL-022)
- Authentication-dependent validations (RULE-VAL-024)
- Duplicate prevention (RULE-VAL-023)

## Migration Notes

These validation rules should be implemented in modern validation layers as follows:

1. **Frontend Validations**: Field-level validations (mandatory, format, length, character type)
2. **Backend API Validations**: All validations with additional business logic checks
3. **Database Constraints**: Unique constraints, range checks, and referential integrity
4. **Business Logic Layer**: Complex conditional validations and cross-field dependencies

The extracted rules provide a comprehensive foundation for modernizing the CardDemo application's validation architecture while maintaining business rule integrity.
