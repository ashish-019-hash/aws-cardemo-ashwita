# COBOL Business Validation Rules Analysis

## Overview

This document contains the comprehensive analysis of business-level validation rules extracted from 18 COBOL programs in the CardDemo application. These validation rules are candidates for migration to modern validation layers (frontend + backend APIs).

## Validation Rules Summary

**Total Rules Identified:** 25  
**Programs Analyzed:** 18  
**Functional Categories:** 6

## Validation Rules by Category

### 1. Authentication & User Management Validations

#### RULE-VAL-001: User ID Mandatory Validation (Sign-on)
- **Rule Description:** User ID field cannot be empty or contain low-values during sign-on
- **COBOL Source Location:** COSGN00C.cbl, lines 118-122
- **Field(s) Involved:** USERIDI (User ID input field)
- **Validation Condition:** USERIDI = SPACES OR LOW-VALUES
- **Trigger Conditions:** When DFHENTER is pressed and user attempts to sign on

#### RULE-VAL-002: Password Mandatory Validation (Sign-on)
- **Rule Description:** Password field cannot be empty or contain low-values during sign-on
- **COBOL Source Location:** COSGN00C.cbl, lines 123-127
- **Field(s) Involved:** PASSWDI (Password input field)
- **Validation Condition:** PASSWDI = SPACES OR LOW-VALUES
- **Trigger Conditions:** When DFHENTER is pressed and user attempts to sign on

#### RULE-VAL-003: First Name Mandatory Validation (User Add)
- **Rule Description:** First name field cannot be empty when adding a new user
- **COBOL Source Location:** COUSR01C.cbl, lines 118-123
- **Field(s) Involved:** FNAMEI (First name input field)
- **Validation Condition:** FNAMEI = SPACES OR LOW-VALUES
- **Trigger Conditions:** When DFHENTER is pressed during user addition process

#### RULE-VAL-004: Last Name Mandatory Validation (User Add)
- **Rule Description:** Last name field cannot be empty when adding a new user
- **COBOL Source Location:** COUSR01C.cbl, lines 124-129
- **Field(s) Involved:** LNAMEI (Last name input field)
- **Validation Condition:** LNAMEI = SPACES OR LOW-VALUES
- **Trigger Conditions:** When DFHENTER is pressed during user addition process

#### RULE-VAL-005: User ID Mandatory Validation (User Add)
- **Rule Description:** User ID field cannot be empty when adding a new user
- **COBOL Source Location:** COUSR01C.cbl, lines 130-135
- **Field(s) Involved:** USERIDI (User ID input field)
- **Validation Condition:** USERIDI = SPACES OR LOW-VALUES
- **Trigger Conditions:** When DFHENTER is pressed during user addition process

#### RULE-VAL-006: Password Mandatory Validation (User Add)
- **Rule Description:** Password field cannot be empty when adding a new user
- **COBOL Source Location:** COUSR01C.cbl, lines 136-141
- **Field(s) Involved:** PASSWDI (Password input field)
- **Validation Condition:** PASSWDI = SPACES OR LOW-VALUES
- **Trigger Conditions:** When DFHENTER is pressed during user addition process

#### RULE-VAL-007: User Type Mandatory Validation
- **Rule Description:** User type field cannot be empty when adding a new user
- **COBOL Source Location:** COUSR01C.cbl, lines 142-147
- **Field(s) Involved:** USRTYPEI (User type input field)
- **Validation Condition:** USRTYPEI = SPACES OR LOW-VALUES
- **Trigger Conditions:** When DFHENTER is pressed during user addition process

#### RULE-VAL-008: Duplicate User ID Validation
- **Rule Description:** User ID must be unique - cannot add user with existing ID
- **COBOL Source Location:** COUSR01C.cbl, lines 260-266
- **Field(s) Involved:** SEC-USR-ID (User ID key field)
- **Validation Condition:** DFHRESP(DUPKEY) OR DFHRESP(DUPREC) returned from CICS WRITE
- **Trigger Conditions:** When attempting to write new user record to USRSEC file

#### RULE-VAL-009: User ID Mandatory Validation (User Update)
- **Rule Description:** User ID field cannot be empty when updating user information
- **COBOL Source Location:** COUSR02C.cbl, lines 146-150
- **Field(s) Involved:** USRIDINI (User ID input field)
- **Validation Condition:** USRIDINI = SPACES OR LOW-VALUES
- **Trigger Conditions:** When DFHENTER is pressed during user update process

#### RULE-VAL-010: User ID Mandatory Validation (User Delete)
- **Rule Description:** User ID field cannot be empty when deleting a user
- **COBOL Source Location:** COUSR03C.cbl, lines 145-150, 177-182
- **Field(s) Involved:** USRIDINI (User ID input field)
- **Validation Condition:** USRIDINI = SPACES OR LOW-VALUES
- **Trigger Conditions:** When DFHENTER is pressed or PF5 (delete) is pressed during user deletion process

### 2. Credit Card Validations

#### RULE-VAL-011: Card Expiration Month Range Validation
- **Rule Description:** Card expiration month must be between 1 and 12
- **COBOL Source Location:** COCRDUPC.cbl, line 95
- **Field(s) Involved:** CARD-MONTH-CHECK-N (Card expiration month)
- **Validation Condition:** 88 VALID-MONTH VALUES 1 THRU 12
- **Trigger Conditions:** When validating card expiration month input

#### RULE-VAL-012: Card Expiration Year Range Validation
- **Rule Description:** Card expiration year must be between 1950 and 2099
- **COBOL Source Location:** COCRDUPC.cbl, line 99
- **Field(s) Involved:** CARD-YEAR-CHECK-N (Card expiration year)
- **Validation Condition:** 88 VALID-YEAR VALUES 1950 THRU 2099
- **Trigger Conditions:** When validating card expiration year input

#### RULE-VAL-013: Yes/No Field Validation
- **Rule Description:** Yes/No fields must contain only 'Y' or 'N' values
- **COBOL Source Location:** COCRDUPC.cbl, line 91
- **Field(s) Involved:** FLG-YES-NO-CHECK (Yes/No flag field)
- **Validation Condition:** 88 FLG-YES-NO-VALID VALUES 'Y', 'N'
- **Trigger Conditions:** When validating yes/no input fields

#### RULE-VAL-014: Account Filter Validation Flag
- **Rule Description:** Account filter must be marked as valid when account data passes validation
- **COBOL Source Location:** COCRDUPC.cbl, lines 59, 63, 67, 71, 75, 79
- **Field(s) Involved:** Various filter flags (ACCTFILTER, CARDFILTER, CARDNAME, CARDSTATUS, CARDEXPMON, CARDEXPYEAR)
- **Validation Condition:** 88 FLG-*-ISVALID VALUE '1'
- **Trigger Conditions:** When processing and validating card-related input fields

### 3. Account Management Validations

#### RULE-VAL-015: Account ID Mandatory Validation (Bill Payment)
- **Rule Description:** Account ID field cannot be empty when processing bill payment
- **COBOL Source Location:** COBIL00C.cbl, lines 159-164
- **Field(s) Involved:** ACTIDINI (Account ID input field)
- **Validation Condition:** ACTIDINI = SPACES OR LOW-VALUES
- **Trigger Conditions:** When DFHENTER is pressed during bill payment process

#### RULE-VAL-016: Phone Number Format Validation
- **Rule Description:** Phone number must follow US format (XXX-XXX-XXXX) with numeric digits only
- **COBOL Source Location:** COACTUPC.cbl, lines 82-100 (inferred from phone validation patterns)
- **Field(s) Involved:** Phone number input fields
- **Validation Condition:** Must be 10 numeric digits in XXX-XXX-XXXX format
- **Trigger Conditions:** When validating customer phone number input

#### RULE-VAL-017: Account Number Numeric Validation
- **Rule Description:** Account number must be a non-zero 11-digit numeric value
- **COBOL Source Location:** COACTVWC.cbl, lines 125-128
- **Field(s) Involved:** Account number input field
- **Validation Condition:** Account number must be numeric and non-zero with 11 digits
- **Trigger Conditions:** When searching for or validating account information

### 4. Transaction Processing Validations

#### RULE-VAL-018: Transaction ID Mandatory Validation (Transaction View)
- **Rule Description:** Transaction ID field cannot be empty when viewing transaction details
- **COBOL Source Location:** COTRN01C.cbl, lines 147-152
- **Field(s) Involved:** TRNIDINI (Transaction ID input field)
- **Validation Condition:** TRNIDINI = SPACES OR LOW-VALUES
- **Trigger Conditions:** When DFHENTER is pressed during transaction view process

#### RULE-VAL-019: Transaction Type Code Mandatory Validation
- **Rule Description:** Transaction type code field cannot be empty when adding a transaction
- **COBOL Source Location:** COTRN02C.cbl, lines 252-257
- **Field(s) Involved:** TTYPCDI (Transaction type code input field)
- **Validation Condition:** TTYPCDI = SPACES OR LOW-VALUES
- **Trigger Conditions:** When DFHENTER is pressed during transaction addition process

#### RULE-VAL-020: Transaction Category Code Mandatory Validation
- **Rule Description:** Transaction category code field cannot be empty when adding a transaction
- **COBOL Source Location:** COTRN02C.cbl, lines 258-263
- **Field(s) Involved:** TCATCDI (Transaction category code input field)
- **Validation Condition:** TCATCDI = SPACES OR LOW-VALUES
- **Trigger Conditions:** When DFHENTER is pressed during transaction addition process

### 5. Menu Navigation Validations

#### RULE-VAL-021: Menu Option Numeric Validation (Regular User)
- **Rule Description:** Menu option must be numeric and within valid range for regular users
- **COBOL Source Location:** COMEN01C.cbl, lines 127-134
- **Field(s) Involved:** WS-OPTION (Menu option selection)
- **Validation Condition:** WS-OPTION IS NOT NUMERIC OR WS-OPTION > CDEMO-MENU-OPT-COUNT OR WS-OPTION = ZEROS
- **Trigger Conditions:** When DFHENTER is pressed on main menu

#### RULE-VAL-022: Admin-Only Option Access Validation
- **Rule Description:** Regular users cannot access admin-only menu options
- **COBOL Source Location:** COMEN01C.cbl, lines 136-143
- **Field(s) Involved:** CDEMO-MENU-OPT-USRTYPE, WS-OPTION
- **Validation Condition:** CDEMO-USRTYP-USER AND CDEMO-MENU-OPT-USRTYPE(WS-OPTION) = 'A'
- **Trigger Conditions:** When regular user attempts to access admin-only menu option

#### RULE-VAL-023: Menu Option Numeric Validation (Admin User)
- **Rule Description:** Menu option must be numeric and within valid range for admin users
- **COBOL Source Location:** COADM01C.cbl, lines 127-134
- **Field(s) Involved:** WS-OPTION (Menu option selection)
- **Validation Condition:** WS-OPTION IS NOT NUMERIC OR WS-OPTION > CDEMO-ADMIN-OPT-COUNT OR WS-OPTION = ZEROS
- **Trigger Conditions:** When DFHENTER is pressed on admin menu

### 6. Date Validation Utility

#### RULE-VAL-024: Date Format Validation (CEEDAYS API)
- **Rule Description:** Date must be in valid format and represent a real date
- **COBOL Source Location:** CSUTLDTC.cbl, lines 128-149
- **Field(s) Involved:** LS-DATE, LS-DATE-FORMAT
- **Validation Condition:** Multiple conditions checked via CEEDAYS API feedback codes
- **Trigger Conditions:** When date validation utility is called

**Sub-validations include:**
- FC-INVALID-DATE: Date is valid
- FC-INSUFFICIENT-DATA: Insufficient data provided
- FC-BAD-DATE-VALUE: Invalid date value
- FC-INVALID-ERA: Invalid era specification
- FC-UNSUPP-RANGE: Unsupported date range
- FC-INVALID-MONTH: Invalid month value
- FC-BAD-PIC-STRING: Bad picture string format
- FC-NON-NUMERIC-DATA: Non-numeric data in date field
- FC-YEAR-IN-ERA-ZERO: Year in era is zero

#### RULE-VAL-025: Report Date Range Validation
- **Rule Description:** Start and end dates for reports must be in valid YYYY-MM-DD format
- **COBOL Source Location:** CORPT00C.cbl, lines 60-72
- **Field(s) Involved:** WS-START-DATE, WS-END-DATE
- **Validation Condition:** Dates must conform to YYYY-MM-DD format structure
- **Trigger Conditions:** When generating transaction reports with date parameters

## Validation Relationships and Dependencies

### Shared Validation Patterns

1. **Mandatory Field Pattern**: Used across all user input forms
   - Common trigger: DFHENTER key press
   - Common condition: Field = SPACES OR LOW-VALUES
   - Common response: Set error flag, display message, position cursor

2. **Numeric Range Pattern**: Used for dates, amounts, and option selections
   - Common validation: Value within specified range
   - Common implementation: 88-level conditions

3. **User Type Authorization Pattern**: Controls access to functionality
   - Validates user type before allowing access to admin functions
   - Implemented in menu navigation and transaction processing

### Field Dependencies

1. **User Management Flow**:
   - RULE-VAL-003 → RULE-VAL-004 → RULE-VAL-005 → RULE-VAL-006 → RULE-VAL-007 → RULE-VAL-008
   - Sequential validation of all required fields before user creation

2. **Card Validation Flow**:
   - RULE-VAL-011 → RULE-VAL-012 → RULE-VAL-013
   - Card expiration validation requires both month and year to be valid

3. **Transaction Processing Flow**:
   - RULE-VAL-019 → RULE-VAL-020
   - Both type code and category code must be provided for transaction creation

## Migration Recommendations

### Frontend Validation Layer
- Implement client-side validation for mandatory fields (RULE-VAL-001 through RULE-VAL-010, RULE-VAL-015, RULE-VAL-018 through RULE-VAL-020)
- Add real-time format validation for dates, phone numbers, and account numbers
- Implement range validation for numeric fields (months, years, menu options)

### Backend API Validation Layer
- Implement server-side validation for all business rules as final validation
- Add database constraint validation for unique fields (RULE-VAL-008)
- Implement authorization validation for user type restrictions (RULE-VAL-022)
- Add comprehensive date validation using modern date libraries

### Shared Validation Components
- Create reusable validation components for common patterns:
  - Mandatory field validator
  - Numeric range validator  
  - Date format validator
  - User authorization validator

## Technical Notes

- All validation rules follow consistent COBOL patterns using EVALUATE TRUE statements
- Error handling uses standardized error flag and message mechanisms
- Cursor positioning (-1 values) provides user experience guidance
- 88-level conditions provide readable validation logic

## Files Analyzed

1. COSGN00C.cbl - Sign-on screen validation
2. COACTUPC.cbl - Account update validation  
3. COCRDUPC.cbl - Credit card update validation
4. COUSR01C.cbl - User addition validation
5. COUSR02C.cbl - User update validation
6. COUSR03C.cbl - User deletion validation
7. COUSR00C.cbl - User listing (no business validations)
8. COTRN01C.cbl - Transaction view validation
9. COTRN02C.cbl - Transaction addition validation
10. COTRN00C.cbl - Transaction listing (no business validations)
11. COBIL00C.cbl - Bill payment validation
12. COACTVWC.cbl - Account view validation
13. COCRDLIC.cbl - Credit card listing (no business validations)
14. COCRDSLC.cbl - Credit card selection (no business validations)
15. COMEN01C.cbl - Regular user menu validation
16. COADM01C.cbl - Admin menu validation
17. CORPT00C.cbl - Report generation validation
18. CSUTLDTC.cbl - Date validation utility

---

*Generated by COBOL Business Validation Analysis Tool*  
*Analysis Date: September 2025*  
*Total Programs Analyzed: 18*  
*Total Validation Rules Extracted: 25*
