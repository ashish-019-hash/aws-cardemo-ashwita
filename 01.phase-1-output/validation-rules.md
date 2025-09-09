# COBOL Business Validation Rules - CardDemo Application

This document contains business-level validation rules extracted from the CardDemo COBOL application for modernization planning. These rules will be migrated to modern validation layers (frontend + backend APIs).

## Overview

The CardDemo application contains comprehensive validation rules across 18 COBOL programs covering user management, account operations, credit card management, transaction processing, and administrative functions.

## Validation Rule Categories

### Field-Level Input Validations
- Required field checks (non-empty validations)
- Format constraints and data type validations
- Length validations

### Range Validations
- Numeric range checks
- Date range validations
- Value boundary constraints

### Code/Value Validations
- Status code validations
- User type restrictions
- Enumerated value checks

### Domain-Specific Validations
- Account number format validations
- Credit card number validations
- Date format and validity checks
- Amount format validations

### Conditional Validations
- User type-based access controls
- Field dependency validations
- Context-sensitive validations

---

## Validation Rules

### User Authentication & Management

## RULE-VAL-001
**Rule Description:** User ID cannot be empty during sign-on
**COBOL Source Location:** COSGN00C.cbl, lines 118-122, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** USERIDI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** When ENTER key is pressed on sign-on screen

## RULE-VAL-002
**Rule Description:** Password cannot be empty during sign-on
**COBOL Source Location:** COSGN00C.cbl, lines 123-127, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** PASSWDI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** When ENTER key is pressed on sign-on screen

## RULE-VAL-003
**Rule Description:** First Name is required for user creation
**COBOL Source Location:** COUSR01C.cbl, lines 118-123, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** FNAMEI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** When ENTER key is pressed on user add screen

## RULE-VAL-004
**Rule Description:** Last Name is required for user creation
**COBOL Source Location:** COUSR01C.cbl, lines 124-129, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** LNAMEI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** When ENTER key is pressed on user add screen

## RULE-VAL-005
**Rule Description:** User ID is required for user creation
**COBOL Source Location:** COUSR01C.cbl, lines 130-135, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** USERIDI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** When ENTER key is pressed on user add screen

## RULE-VAL-006
**Rule Description:** Password is required for user creation
**COBOL Source Location:** COUSR01C.cbl, lines 136-141, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** PASSWDI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** When ENTER key is pressed on user add screen

## RULE-VAL-007
**Rule Description:** User Type is required for user creation
**COBOL Source Location:** COUSR01C.cbl, lines 142-147, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** USRTYPEI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** When ENTER key is pressed on user add screen

## RULE-VAL-008
**Rule Description:** User ID is required for user update
**COBOL Source Location:** COUSR02C.cbl, lines 146-150, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** USRIDINI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** When ENTER key is pressed on user update screen

## RULE-VAL-009
**Rule Description:** User ID is required for user deletion
**COBOL Source Location:** COUSR03C.cbl, lines 145-150, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** USRIDINI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** When ENTER key is pressed on user delete screen

### Menu and Navigation Validations

## RULE-VAL-010
**Rule Description:** Menu option must be numeric and within valid range
**COBOL Source Location:** COMEN01C.cbl, lines 127-134, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** WS-OPTION
**Validation Condition:** Must be numeric, greater than zero, and not exceed CDEMO-MENU-OPT-COUNT
**Trigger Conditions:** When ENTER key is pressed on main menu screen

## RULE-VAL-011
**Rule Description:** Admin-only menu options restricted for regular users
**COBOL Source Location:** COMEN01C.cbl, lines 136-143, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** CDEMO-MENU-OPT-USRTYPE, CDEMO-USRTYP-USER
**Validation Condition:** If user type is 'USER' and menu option type is 'A' (Admin), access denied
**Trigger Conditions:** When regular user attempts to access admin-only menu option

## RULE-VAL-012
**Rule Description:** Admin menu option must be numeric and within valid range
**COBOL Source Location:** COADM01C.cbl, lines 127-134, PROCESS-ENTER-KEY paragraph
**Field(s) Involved:** WS-OPTION
**Validation Condition:** Must be numeric, greater than zero, and not exceed CDEMO-ADMIN-OPT-COUNT
**Trigger Conditions:** When ENTER key is pressed on admin menu screen

### Account Management Validations

## RULE-VAL-013
**Rule Description:** Account status must be Y (Yes) or N (No)
**COBOL Source Location:** COACTUPC.cbl, lines 192-195, WS-NON-KEY-FLAGS section
**Field(s) Involved:** WS-EDIT-ACCT-STATUS
**Validation Condition:** Field value must be 'Y' or 'N'
**Trigger Conditions:** During account update processing

## RULE-VAL-014
**Rule Description:** Primary cardholder flag must be Y (Yes) or N (No)
**COBOL Source Location:** COACTUPC.cbl, lines 349-352, WS-NON-KEY-FLAGS section
**Field(s) Involved:** WS-EDIT-PRI-CARDHOLDER
**Validation Condition:** Field value must be 'Y' or 'N'
**Trigger Conditions:** During account update processing

## RULE-VAL-015
**Rule Description:** First name cannot be empty for account updates
**COBOL Source Location:** COACTUPC.cbl, lines 278-281, WS-EDIT-NAME-FLAGS section
**Field(s) Involved:** WS-EDIT-FIRST-NAME-FLGS
**Validation Condition:** Field must not be blank or contain invalid characters
**Trigger Conditions:** During account name validation

## RULE-VAL-016
**Rule Description:** Last name cannot be empty for account updates
**COBOL Source Location:** COACTUPC.cbl, lines 286-289, WS-EDIT-NAME-FLAGS section
**Field(s) Involved:** WS-EDIT-LAST-NAME-FLGS
**Validation Condition:** Field must not be blank or contain invalid characters
**Trigger Conditions:** During account name validation

## RULE-VAL-017
**Rule Description:** Address line 1 is required for account updates
**COBOL Source Location:** COACTUPC.cbl, lines 291-294, WS-EDIT-ADDRESS-FLAGS section
**Field(s) Involved:** WS-EDIT-ADDRESS-LINE-1-FLGS
**Validation Condition:** Field must not be blank
**Trigger Conditions:** During account address validation

## RULE-VAL-018
**Rule Description:** City is required for account updates
**COBOL Source Location:** COACTUPC.cbl, lines 299-302, WS-EDIT-ADDRESS-FLAGS section
**Field(s) Involved:** WS-EDIT-CITY-FLGS
**Validation Condition:** Field must not be blank
**Trigger Conditions:** During account address validation

## RULE-VAL-019
**Rule Description:** State is required for account updates
**COBOL Source Location:** COACTUPC.cbl, lines 303-306, WS-EDIT-ADDRESS-FLAGS section
**Field(s) Involved:** WS-EDIT-STATE-FLGS
**Validation Condition:** Field must not be blank
**Trigger Conditions:** During account address validation

## RULE-VAL-020
**Rule Description:** ZIP code is required for account updates
**COBOL Source Location:** COACTUPC.cbl, lines 307-310, WS-EDIT-ADDRESS-FLAGS section
**Field(s) Involved:** WS-EDIT-ZIPCODE-FLGS
**Validation Condition:** Field must not be blank
**Trigger Conditions:** During account address validation

### Credit Card Management Validations

## RULE-VAL-021
**Rule Description:** Account number is required for card operations
**COBOL Source Location:** COCRDUPC.cbl, lines 177-179, WS-RETURN-MSG section
**Field(s) Involved:** Account number field
**Validation Condition:** Field must not be empty
**Trigger Conditions:** When performing card search or update operations

## RULE-VAL-022
**Rule Description:** Card number is required for card operations
**COBOL Source Location:** COCRDUPC.cbl, lines 179-181, WS-RETURN-MSG section
**Field(s) Involved:** Card number field
**Validation Condition:** Field must not be empty
**Trigger Conditions:** When performing card search or update operations

## RULE-VAL-023
**Rule Description:** Card name is required for card operations
**COBOL Source Location:** COCRDUPC.cbl, lines 181-183, WS-RETURN-MSG section
**Field(s) Involved:** Card name field
**Validation Condition:** Field must not be empty
**Trigger Conditions:** When performing card update operations

## RULE-VAL-024
**Rule Description:** Card name must contain only alphabets and spaces
**COBOL Source Location:** COCRDUPC.cbl, lines 183-185, WS-RETURN-MSG section
**Field(s) Involved:** Card name field
**Validation Condition:** Field must contain only alphabetic characters and spaces
**Trigger Conditions:** During card name validation

## RULE-VAL-025
**Rule Description:** Account number must be non-zero 11-digit number
**COBOL Source Location:** COCRDUPC.cbl, lines 189-193, WS-RETURN-MSG section
**Field(s) Involved:** Account number field
**Validation Condition:** Must be numeric, 11 digits, and not zero
**Trigger Conditions:** During account number validation

## RULE-VAL-026
**Rule Description:** Card number must be 16-digit number if provided
**COBOL Source Location:** COCRDUPC.cbl, lines 193-195, WS-RETURN-MSG section
**Field(s) Involved:** Card number field
**Validation Condition:** If provided, must be numeric and exactly 16 digits
**Trigger Conditions:** During card number validation

## RULE-VAL-027
**Rule Description:** Card active status must be Y or N
**COBOL Source Location:** COCRDUPC.cbl, lines 195-197, WS-RETURN-MSG section
**Field(s) Involved:** Card status field
**Validation Condition:** Field value must be 'Y' (active) or 'N' (inactive)
**Trigger Conditions:** During card status validation

## RULE-VAL-028
**Rule Description:** Card expiry month must be between 1 and 12
**COBOL Source Location:** COCRDUPC.cbl, lines 197-199, WS-RETURN-MSG section
**Field(s) Involved:** Card expiry month field
**Validation Condition:** Numeric value must be between 1 and 12 inclusive
**Trigger Conditions:** During card expiry date validation

## RULE-VAL-029
**Rule Description:** Card expiry year must be valid
**COBOL Source Location:** COCRDUPC.cbl, lines 199-201, WS-RETURN-MSG section
**Field(s) Involved:** Card expiry year field
**Validation Condition:** Must be a valid year (typically current year or future)
**Trigger Conditions:** During card expiry date validation

## RULE-VAL-030
**Rule Description:** Account number is required for card detail view
**COBOL Source Location:** COCRDSLC.cbl, lines 138-140, WS-RETURN-MSG section
**Field(s) Involved:** Account number field
**Validation Condition:** Field must not be empty
**Trigger Conditions:** When requesting card details

## RULE-VAL-031
**Rule Description:** Card number is required for card detail view
**COBOL Source Location:** COCRDSLC.cbl, lines 140-142, WS-RETURN-MSG section
**Field(s) Involved:** Card number field
**Validation Condition:** Field must not be empty
**Trigger Conditions:** When requesting card details

## RULE-VAL-032
**Rule Description:** Account number must be non-zero 11-digit number for card details
**COBOL Source Location:** COCRDSLC.cbl, lines 144-147, WS-RETURN-MSG section
**Field(s) Involved:** Account number field
**Validation Condition:** Must be numeric, 11 digits, and not zero
**Trigger Conditions:** During account number validation for card details

## RULE-VAL-033
**Rule Description:** Card number must be 16-digit number for card details
**COBOL Source Location:** COCRDSLC.cbl, lines 147-149, WS-RETURN-MSG section
**Field(s) Involved:** Card number field
**Validation Condition:** Must be numeric and exactly 16 digits
**Trigger Conditions:** During card number validation for card details

## RULE-VAL-034
**Rule Description:** Card list selection must be valid action code
**COBOL Source Location:** COCRDLIC.cbl, lines 77-82, WS-EDIT-SELECT-ARRAY section
**Field(s) Involved:** WS-EDIT-SELECT
**Validation Condition:** Must be 'S' (Select) or 'U' (Update)
**Trigger Conditions:** When selecting cards from card list

### Transaction Processing Validations

## RULE-VAL-035
**Rule Description:** Account ID is required for transaction processing
**COBOL Source Location:** COTRN02C.cbl, lines 195-202, VALIDATE-ACCOUNT-CARD-DATA paragraph
**Field(s) Involved:** ACTIDINI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction data validation

## RULE-VAL-036
**Rule Description:** Account ID must be numeric for transaction processing
**COBOL Source Location:** COTRN02C.cbl, lines 195-202, VALIDATE-ACCOUNT-CARD-DATA paragraph
**Field(s) Involved:** ACTIDINI
**Validation Condition:** Field must contain only numeric characters
**Trigger Conditions:** During account ID validation for transactions

## RULE-VAL-037
**Rule Description:** Card number must be numeric for transaction processing
**COBOL Source Location:** COTRN02C.cbl, lines 211-217, VALIDATE-ACCOUNT-CARD-DATA paragraph
**Field(s) Involved:** CARDNINI
**Validation Condition:** Field must contain only numeric characters
**Trigger Conditions:** During card number validation for transactions

## RULE-VAL-038
**Rule Description:** Either Account ID or Card Number must be provided
**COBOL Source Location:** COTRN02C.cbl, lines 224-230, VALIDATE-ACCOUNT-CARD-DATA paragraph
**Field(s) Involved:** ACTIDINI, CARDNINI
**Validation Condition:** At least one of Account ID or Card Number must be provided
**Trigger Conditions:** During transaction search criteria validation

## RULE-VAL-039
**Rule Description:** Transaction Type Code cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 252-257, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TTYPCDI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction data validation

## RULE-VAL-040
**Rule Description:** Transaction Category Code cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 258-263, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TCATCDI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction data validation

## RULE-VAL-041
**Rule Description:** Transaction Source cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 264-269, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TRNSRCI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction data validation

## RULE-VAL-042
**Rule Description:** Transaction Description cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 270-275, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TDESCI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction data validation

## RULE-VAL-043
**Rule Description:** Transaction Amount cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 276-281, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TRNAMTI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction data validation

## RULE-VAL-044
**Rule Description:** Original Date cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 282-287, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TORIGDTI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction data validation

## RULE-VAL-045
**Rule Description:** Process Date cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 288-293, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TPROCDTI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction data validation

## RULE-VAL-046
**Rule Description:** Merchant ID cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 294-299, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** MIDI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction data validation

## RULE-VAL-047
**Rule Description:** Merchant Name cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 300-305, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** MNAMEI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction data validation

## RULE-VAL-048
**Rule Description:** Merchant City cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 306-311, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** MCITYI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction data validation

## RULE-VAL-049
**Rule Description:** Merchant ZIP cannot be empty
**COBOL Source Location:** COTRN02C.cbl, lines 312-317, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** MZIPI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** During transaction data validation

## RULE-VAL-050
**Rule Description:** Transaction Type Code must be numeric
**COBOL Source Location:** COTRN02C.cbl, lines 323-328, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TTYPCDI
**Validation Condition:** Field must contain only numeric characters
**Trigger Conditions:** During transaction type validation

## RULE-VAL-051
**Rule Description:** Transaction Category Code must be numeric
**COBOL Source Location:** COTRN02C.cbl, lines 329-334, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TCATCDI
**Validation Condition:** Field must contain only numeric characters
**Trigger Conditions:** During transaction category validation

## RULE-VAL-052
**Rule Description:** Transaction Amount must be in format -99999999.99
**COBOL Source Location:** COTRN02C.cbl, lines 340-351, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TRNAMTI
**Validation Condition:** Must have sign (+ or -), 8 digits, decimal point, and 2 decimal places
**Trigger Conditions:** During transaction amount validation

## RULE-VAL-053
**Rule Description:** Original Date must be in format YYYY-MM-DD
**COBOL Source Location:** COTRN02C.cbl, lines 354-366, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TORIGDTI
**Validation Condition:** Must be 4-digit year, hyphen, 2-digit month, hyphen, 2-digit day
**Trigger Conditions:** During original date validation

## RULE-VAL-054
**Rule Description:** Process Date must be in format YYYY-MM-DD
**COBOL Source Location:** COTRN02C.cbl, lines 368-381, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TPROCDTI
**Validation Condition:** Must be 4-digit year, hyphen, 2-digit month, hyphen, 2-digit day
**Trigger Conditions:** During process date validation

## RULE-VAL-055
**Rule Description:** Original Date must be a valid calendar date
**COBOL Source Location:** COTRN02C.cbl, lines 393-400, VALIDATE-INPUT-DATA-FIELDS paragraph
**Field(s) Involved:** TORIGDTI
**Validation Condition:** Date must exist in calendar and pass CSUTLDTC validation
**Trigger Conditions:** During date validity check using CSUTLDTC utility

### Date Validation Utility Rules

## RULE-VAL-056
**Rule Description:** Date format validation using CEEDAYS API
**COBOL Source Location:** CSUTLDTC.cbl, lines 128-149, A000-MAIN paragraph
**Field(s) Involved:** LS-DATE, LS-DATE-FORMAT
**Validation Condition:** Date must conform to specified format and be valid per CEEDAYS API
**Trigger Conditions:** When CSUTLDTC utility is called for date validation

## RULE-VAL-057
**Rule Description:** Date must not have insufficient data
**COBOL Source Location:** CSUTLDTC.cbl, lines 131-132, A000-MAIN paragraph
**Field(s) Involved:** Date input
**Validation Condition:** Date must have complete day, month, year information
**Trigger Conditions:** During CEEDAYS API validation

## RULE-VAL-058
**Rule Description:** Date must not have invalid month
**COBOL Source Location:** CSUTLDTC.cbl, lines 139-140, A000-MAIN paragraph
**Field(s) Involved:** Month component of date
**Validation Condition:** Month must be valid (1-12)
**Trigger Conditions:** During CEEDAYS API validation

## RULE-VAL-059
**Rule Description:** Date must not contain non-numeric data in numeric positions
**COBOL Source Location:** CSUTLDTC.cbl, lines 143-144, A000-MAIN paragraph
**Field(s) Involved:** Numeric portions of date
**Validation Condition:** Year, month, day components must be numeric
**Trigger Conditions:** During CEEDAYS API validation

### Bill Payment Validations

## RULE-VAL-060
**Rule Description:** Account ID is required for bill payment
**COBOL Source Location:** COBIL00C.cbl, lines 115-122, main processing logic
**Field(s) Involved:** ACTIDINI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** When initiating bill payment process

---

## Validation Rule Summary

**Total Rules Extracted:** 60
**Programs Analyzed:** 18
**Rule Categories:**
- Field-Level Input Validations: 35 rules
- Range Validations: 8 rules  
- Code/Value Validations: 7 rules
- Domain-Specific Validations: 6 rules
- Conditional Validations: 4 rules

## Modernization Notes

These validation rules represent the core business logic that must be preserved during the migration to modern validation layers. Each rule should be implemented in both frontend (for immediate user feedback) and backend (for data integrity) validation systems.

Key patterns identified:
1. **Required Field Validations** - Most common pattern across all programs
2. **Format Validations** - Especially important for dates, amounts, and ID numbers
3. **Business Rule Validations** - Status codes, user type restrictions, value ranges
4. **Cross-Field Dependencies** - Conditional validations based on user type or context
5. **Data Integrity Checks** - Numeric validations, length constraints, character set restrictions

## Implementation Priority

**High Priority:**
- User authentication validations (RULE-VAL-001, RULE-VAL-002)
- Required field validations for core entities
- Data format validations for critical fields

**Medium Priority:**
- Business rule validations for status codes
- Range validations for dates and amounts
- Cross-field dependency validations

**Lower Priority:**
- Menu navigation validations
- Administrative function validations
- Display-related validations
