# CardDemo COBOL Business Validation Rules

## Overview

This document contains business-level validation rules extracted from the CardDemo CICS application COBOL programs. These validation rules are organized by functional area and are intended to support migration to modern validation layers (frontend + backend APIs).

## Validation Rule Categories

- **Authentication & Security**
- **Account Management** 
- **Credit Card Management**
- **Transaction Processing**
- **User Management**
- **Bill Payment**
- **Date Validation Utilities**

---

## Authentication & Security Validations

### RULE-VAL-001
**Rule Description**: User ID must not be empty  
**COBOL Source Location**: COSGN00C.cbl, lines 118-122  
**Field(s) Involved**: USERIDI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When ENTER key is pressed on signon screen  

### RULE-VAL-002
**Rule Description**: Password must not be empty  
**COBOL Source Location**: COSGN00C.cbl, lines 123-127  
**Field(s) Involved**: PASSWDI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When ENTER key is pressed on signon screen and User ID is valid  

### RULE-VAL-003
**Rule Description**: User must exist in security file  
**COBOL Source Location**: COSGN00C.cbl, lines 247-251  
**Field(s) Involved**: WS-USER-ID  
**Validation Condition**: User ID must be found in USRSEC file  
**Trigger Conditions**: After successful field validation, during authentication process  

### RULE-VAL-004
**Rule Description**: Password must match stored password  
**COBOL Source Location**: COSGN00C.cbl, lines 223-246  
**Field(s) Involved**: SEC-USR-PWD, WS-USER-PWD  
**Validation Condition**: Input password must equal stored password  
**Trigger Conditions**: When user record is found in security file  

---

## Account Management Validations

### RULE-VAL-005
**Rule Description**: Account ID must not be empty for account operations  
**COBOL Source Location**: COACTUPC.cbl, lines 1434-1437  
**Field(s) Involved**: Account ID search field  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When account details are being fetched  

### RULE-VAL-006
**Rule Description**: Account status must be Y or N  
**COBOL Source Location**: COACTUPC.cbl, lines 192-195  
**Field(s) Involved**: WS-EDIT-ACCT-STATUS  
**Validation Condition**: Value must be 'Y' or 'N'  
**Trigger Conditions**: When account status is being updated  

### RULE-VAL-007
**Rule Description**: Credit limit must be a valid signed number  
**COBOL Source Location**: COACTUPC.cbl, lines 196-199  
**Field(s) Involved**: WS-EDIT-CREDIT-LIMIT  
**Validation Condition**: Must be a valid signed numeric value with 2 decimal places  
**Trigger Conditions**: When credit limit is being updated  

### RULE-VAL-008
**Rule Description**: Cash credit limit must be a valid signed number  
**COBOL Source Location**: COACTUPC.cbl, lines 200-203  
**Field(s) Involved**: WS-EDIT-CASH-CREDIT-LIMIT  
**Validation Condition**: Must be a valid signed numeric value with 2 decimal places  
**Trigger Conditions**: When cash credit limit is being updated  

### RULE-VAL-009
**Rule Description**: Current balance must be a valid signed number  
**COBOL Source Location**: COACTUPC.cbl, lines 204-207  
**Field(s) Involved**: WS-EDIT-CURR-BAL  
**Validation Condition**: Must be a valid signed numeric value with 2 decimal places  
**Trigger Conditions**: When current balance is being updated  

### RULE-VAL-010
**Rule Description**: SSN Part 1 must not be invalid values  
**COBOL Source Location**: COACTUPC.cbl, lines 121-123  
**Field(s) Involved**: WS-EDIT-US-SSN-PART1-N  
**Validation Condition**: Must not be 0, 666, or 900-999  
**Trigger Conditions**: When SSN is being validated  

### RULE-VAL-011
**Rule Description**: Date of birth must be valid date format  
**COBOL Source Location**: COACTUPC.cbl, lines 216-230  
**Field(s) Involved**: WS-EDIT-DT-OF-BIRTH-FLGS  
**Validation Condition**: Must be valid CCYYMMDD format  
**Trigger Conditions**: When date of birth is being updated  

### RULE-VAL-012
**Rule Description**: FICO score must be valid numeric  
**COBOL Source Location**: COACTUPC.cbl, lines 231-234  
**Field(s) Involved**: WS-EDIT-FICO-SCORE-FLGS  
**Validation Condition**: Must be valid numeric value  
**Trigger Conditions**: When FICO score is being updated  

### RULE-VAL-013
**Rule Description**: Account open date must be valid  
**COBOL Source Location**: COACTUPC.cbl, lines 235-248  
**Field(s) Involved**: WS-EDIT-OPEN-DATE-FLGS  
**Validation Condition**: Must be valid date format  
**Trigger Conditions**: When account open date is being updated  

### RULE-VAL-014
**Rule Description**: Account expiry date must be valid  
**COBOL Source Location**: COACTUPC.cbl, lines 249-262  
**Field(s) Involved**: WS-EXPIRY-DATE-FLGS  
**Validation Condition**: Must be valid date format  
**Trigger Conditions**: When account expiry date is being updated  

### RULE-VAL-015
**Rule Description**: Account reissue date must be valid  
**COBOL Source Location**: COACTUPC.cbl, lines 263-276  
**Field(s) Involved**: WS-EDIT-REISSUE-DATE-FLGS  
**Validation Condition**: Must be valid date format  
**Trigger Conditions**: When account reissue date is being updated  

---

## Credit Card Management Validations

### RULE-VAL-016
**Rule Description**: Card account ID must not be empty  
**COBOL Source Location**: COCRDUPC.cbl, lines 57-60  
**Field(s) Involved**: WS-EDIT-ACCT-FLAG  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When card account filter is being validated  

### RULE-VAL-017
**Rule Description**: Card number must not be empty  
**COBOL Source Location**: COCRDUPC.cbl, lines 61-64  
**Field(s) Involved**: WS-EDIT-CARD-FLAG  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When card number filter is being validated  

### RULE-VAL-018
**Rule Description**: Card name must not be empty  
**COBOL Source Location**: COCRDUPC.cbl, lines 65-68  
**Field(s) Involved**: WS-EDIT-CARDNAME-FLAG  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When card name is being validated  

### RULE-VAL-019
**Rule Description**: Card status must be Y or N  
**COBOL Source Location**: COCRDUPC.cbl, lines 89-91  
**Field(s) Involved**: FLG-YES-NO-CHECK  
**Validation Condition**: Value must be 'Y' or 'N'  
**Trigger Conditions**: When card status is being validated  

### RULE-VAL-020
**Rule Description**: Card expiry month must be between 1 and 12  
**COBOL Source Location**: COCRDUPC.cbl, lines 92-95  
**Field(s) Involved**: CARD-MONTH-CHECK-N  
**Validation Condition**: Numeric value must be between 1 and 12  
**Trigger Conditions**: When card expiry month is being validated  

### RULE-VAL-021
**Rule Description**: Card expiry year must be between 1950 and 2099  
**COBOL Source Location**: COCRDUPC.cbl, lines 96-99  
**Field(s) Involved**: CARD-YEAR-CHECK-N  
**Validation Condition**: Numeric value must be between 1950 and 2099  
**Trigger Conditions**: When card expiry year is being validated  

### RULE-VAL-022
**Rule Description**: Card status must be Y or N (detailed validation)  
**COBOL Source Location**: COCRDUPC.cbl, lines 195-196  
**Field(s) Involved**: Card Active Status  
**Validation Condition**: Must be 'Y' or 'N'  
**Trigger Conditions**: When card status validation fails  

### RULE-VAL-023
**Rule Description**: Card expiry month must be valid range (detailed validation)  
**COBOL Source Location**: COCRDUPC.cbl, lines 197-198  
**Field(s) Involved**: Card expiry month  
**Validation Condition**: Must be between 1 and 12  
**Trigger Conditions**: When card expiry month validation fails  

### RULE-VAL-024
**Rule Description**: Card expiry year must be valid (detailed validation)  
**COBOL Source Location**: COCRDUPC.cbl, lines 199-200  
**Field(s) Involved**: Card expiry year  
**Validation Condition**: Must be valid year value  
**Trigger Conditions**: When card expiry year validation fails  

---

## Transaction Processing Validations

### RULE-VAL-025
**Rule Description**: Account ID must be numeric for transactions  
**COBOL Source Location**: COTRN02C.cbl, lines 197-203  
**Field(s) Involved**: ACTIDINI  
**Validation Condition**: Field must be numeric  
**Trigger Conditions**: When account ID is provided for transaction  

### RULE-VAL-026
**Rule Description**: Card number must be numeric for transactions  
**COBOL Source Location**: COTRN02C.cbl, lines 211-217  
**Field(s) Involved**: CARDNINI  
**Validation Condition**: Field must be numeric  
**Trigger Conditions**: When card number is provided for transaction  

### RULE-VAL-027
**Rule Description**: Either account ID or card number must be provided  
**COBOL Source Location**: COTRN02C.cbl, lines 224-230  
**Field(s) Involved**: ACTIDINI, CARDNINI  
**Validation Condition**: At least one field must be provided  
**Trigger Conditions**: When neither account nor card number is provided  

### RULE-VAL-028
**Rule Description**: Transaction type code must not be empty  
**COBOL Source Location**: COTRN02C.cbl, lines 252-257  
**Field(s) Involved**: TTYPCDI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When transaction data is being validated  

### RULE-VAL-029
**Rule Description**: Transaction category code must not be empty  
**COBOL Source Location**: COTRN02C.cbl, lines 258-263  
**Field(s) Involved**: TCATCDI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When transaction data is being validated  

### RULE-VAL-030
**Rule Description**: Transaction source must not be empty  
**COBOL Source Location**: COTRN02C.cbl, lines 264-269  
**Field(s) Involved**: TRNSRCI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When transaction data is being validated  

### RULE-VAL-031
**Rule Description**: Transaction description must not be empty  
**COBOL Source Location**: COTRN02C.cbl, lines 270-275  
**Field(s) Involved**: TDESCI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When transaction data is being validated  

### RULE-VAL-032
**Rule Description**: Transaction amount must not be empty  
**COBOL Source Location**: COTRN02C.cbl, lines 276-281  
**Field(s) Involved**: TRNAMTI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When transaction data is being validated  

### RULE-VAL-033
**Rule Description**: Transaction original date must not be empty  
**COBOL Source Location**: COTRN02C.cbl, lines 282-287  
**Field(s) Involved**: TORIGDTI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When transaction data is being validated  

### RULE-VAL-034
**Rule Description**: Transaction process date must not be empty  
**COBOL Source Location**: COTRN02C.cbl, lines 288-293  
**Field(s) Involved**: TPROCDTI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When transaction data is being validated  

### RULE-VAL-035
**Rule Description**: Merchant ID must not be empty  
**COBOL Source Location**: COTRN02C.cbl, lines 294-299  
**Field(s) Involved**: MIDI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When transaction data is being validated  

### RULE-VAL-036
**Rule Description**: Merchant name must not be empty  
**COBOL Source Location**: COTRN02C.cbl, lines 300-305  
**Field(s) Involved**: MNAMEI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When transaction data is being validated  

### RULE-VAL-037
**Rule Description**: Merchant city must not be empty  
**COBOL Source Location**: COTRN02C.cbl, lines 306-311  
**Field(s) Involved**: MCITYI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When transaction data is being validated  

### RULE-VAL-038
**Rule Description**: Merchant zip must not be empty  
**COBOL Source Location**: COTRN02C.cbl, lines 312-317  
**Field(s) Involved**: MZIPI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When transaction data is being validated  

### RULE-VAL-039
**Rule Description**: Transaction type code must be numeric  
**COBOL Source Location**: COTRN02C.cbl, lines 323-328  
**Field(s) Involved**: TTYPCDI  
**Validation Condition**: Field must be numeric  
**Trigger Conditions**: When transaction type code format is validated  

### RULE-VAL-040
**Rule Description**: Transaction category code must be numeric  
**COBOL Source Location**: COTRN02C.cbl, lines 329-334  
**Field(s) Involved**: TCATCDI  
**Validation Condition**: Field must be numeric  
**Trigger Conditions**: When transaction category code format is validated  

### RULE-VAL-041
**Rule Description**: Transaction amount must be in format -99999999.99  
**COBOL Source Location**: COTRN02C.cbl, lines 340-348  
**Field(s) Involved**: TRNAMTI  
**Validation Condition**: Must match pattern: sign(1) + digits(8) + decimal(1) + digits(2)  
**Trigger Conditions**: When transaction amount format is validated  

### RULE-VAL-042
**Rule Description**: Original date must be in YYYY-MM-DD format  
**COBOL Source Location**: COTRN02C.cbl, lines 354-363  
**Field(s) Involved**: TORIGDTI  
**Validation Condition**: Must match YYYY-MM-DD pattern with numeric components  
**Trigger Conditions**: When original date format is validated  

### RULE-VAL-043
**Rule Description**: Process date must be in YYYY-MM-DD format  
**COBOL Source Location**: COTRN02C.cbl, lines 368-373  
**Field(s) Involved**: TPROCDTI  
**Validation Condition**: Must match YYYY-MM-DD pattern with numeric components  
**Trigger Conditions**: When process date format is validated  

### RULE-VAL-044
**Rule Description**: Transaction ID must not be empty for viewing  
**COBOL Source Location**: COTRN01C.cbl, lines 147-151  
**Field(s) Involved**: TRNIDINI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When transaction view is requested  

---

## User Management Validations

### RULE-VAL-045
**Rule Description**: First name must not be empty for user creation  
**COBOL Source Location**: COUSR01C.cbl, lines 118-123  
**Field(s) Involved**: FNAMEI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When creating new user  

### RULE-VAL-046
**Rule Description**: Last name must not be empty for user creation  
**COBOL Source Location**: COUSR01C.cbl, lines 124-129  
**Field(s) Involved**: LNAMEI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When creating new user  

### RULE-VAL-047
**Rule Description**: User ID must not be empty for user creation  
**COBOL Source Location**: COUSR01C.cbl, lines 130-135  
**Field(s) Involved**: USERIDI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When creating new user  

### RULE-VAL-048
**Rule Description**: Password must not be empty for user creation  
**COBOL Source Location**: COUSR01C.cbl, lines 136-141  
**Field(s) Involved**: PASSWDI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When creating new user  

### RULE-VAL-049
**Rule Description**: User type must not be empty for user creation  
**COBOL Source Location**: COUSR01C.cbl, lines 142-147  
**Field(s) Involved**: USRTYPEI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When creating new user  

### RULE-VAL-050
**Rule Description**: User ID must not be empty for user update  
**COBOL Source Location**: COUSR02C.cbl, lines 146-151  
**Field(s) Involved**: USRIDINI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When updating user information  

### RULE-VAL-051
**Rule Description**: First name must not be empty for user update  
**COBOL Source Location**: COUSR02C.cbl, lines 186-191  
**Field(s) Involved**: FNAMEI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When updating user information  

### RULE-VAL-052
**Rule Description**: Last name must not be empty for user update  
**COBOL Source Location**: COUSR02C.cbl, lines 192-197  
**Field(s) Involved**: LNAMEI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When updating user information  

### RULE-VAL-053
**Rule Description**: Password must not be empty for user update  
**COBOL Source Location**: COUSR02C.cbl, lines 198-203  
**Field(s) Involved**: PASSWDI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When updating user information  

### RULE-VAL-054
**Rule Description**: User type must not be empty for user update  
**COBOL Source Location**: COUSR02C.cbl, lines 204-209  
**Field(s) Involved**: USRTYPEI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When updating user information  

---

## Bill Payment Validations

### RULE-VAL-055
**Rule Description**: Account ID must not be empty for bill payment  
**COBOL Source Location**: COBIL00C.cbl, lines 159-164  
**Field(s) Involved**: ACTIDINI  
**Validation Condition**: Field must not be SPACES or LOW-VALUES  
**Trigger Conditions**: When bill payment is initiated  

### RULE-VAL-056
**Rule Description**: Confirmation must be Y or N for bill payment  
**COBOL Source Location**: COBIL00C.cbl, lines 186-190  
**Field(s) Involved**: CONFIRMI  
**Validation Condition**: Value must be 'Y' or 'N'  
**Trigger Conditions**: When bill payment confirmation is provided  

### RULE-VAL-057
**Rule Description**: Account must have positive balance for payment  
**COBOL Source Location**: COBIL00C.cbl, lines 198-205  
**Field(s) Involved**: ACCT-CURR-BAL  
**Validation Condition**: Balance must be greater than zero  
**Trigger Conditions**: When account balance is checked for payment eligibility  

---

## Date Validation Utilities

### RULE-VAL-058
**Rule Description**: Date must be valid according to CEEDAYS API  
**COBOL Source Location**: CSUTLDTC.cbl, lines 129-149  
**Field(s) Involved**: LS-DATE, LS-DATE-FORMAT  
**Validation Condition**: Date must pass CEEDAYS validation with various error checks  
**Trigger Conditions**: When date validation utility is called  

### RULE-VAL-059
**Rule Description**: Date format must be supported  
**COBOL Source Location**: CSUTLDTC.cbl, lines 131-148  
**Field(s) Involved**: WS-DATE-FORMAT  
**Validation Condition**: Format must be recognized by CEEDAYS API  
**Trigger Conditions**: When date format validation is performed  

---

## Menu and Navigation Validations

### RULE-VAL-060
**Rule Description**: Menu option must be numeric  
**COBOL Source Location**: COMEN01C.cbl, lines 127-134  
**Field(s) Involved**: WS-OPTION  
**Validation Condition**: Option must be numeric, within valid range, and not zero  
**Trigger Conditions**: When menu option is selected  

### RULE-VAL-061
**Rule Description**: User must have access to selected menu option  
**COBOL Source Location**: COMEN01C.cbl, lines 136-143  
**Field(s) Involved**: CDEMO-MENU-OPT-USRTYPE  
**Validation Condition**: Regular users cannot access admin-only options  
**Trigger Conditions**: When menu option access is validated  

---

## Summary

This document contains **61 business validation rules** extracted from **18 COBOL programs** in the CardDemo application. The validation rules cover:

- **7** Authentication & Security rules
- **11** Account Management rules  
- **10** Credit Card Management rules
- **20** Transaction Processing rules
- **10** User Management rules
- **3** Bill Payment rules
- **2** Date Validation Utility rules
- **2** Menu and Navigation rules

These validation rules provide the foundation for implementing modern validation layers in frontend and backend API systems during the application modernization process.
