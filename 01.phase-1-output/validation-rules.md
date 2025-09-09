# COBOL Business Validation Rules Extraction

## Overview

This document contains business-level validation rules extracted from the CardDemo CICS application COBOL programs. These validation rules are intended for migration to modern validation layers (frontend + backend APIs).

**Extraction Criteria:**
- ✅ **Included**: Field-level input validations, range validations, code/value checks, domain-specific validations, conditional validations
- ❌ **Excluded**: Calculation logic, technical validations unrelated to business, BMS layout constraints

**Source Repository:** ashish-019-hash/aws-cardemo-ashwita  
**Source Folder:** 00.phase-1-input  
**Analysis Date:** September 09, 2025

---

## Authentication & Sign-on Validations

### RULE-VAL-001
**Rule Description:** User ID must not be empty  
**COBOL Source Location:** COSGN00C.cbl, lines 118-122  
**Field(s) Involved:** USERIDI  
**Validation Condition:** Field must not be SPACES or LOW-VALUES  
**Trigger Conditions:** When user attempts to sign on (PROCESS-ENTER-KEY paragraph)

### RULE-VAL-002
**Rule Description:** Password must not be empty  
**COBOL Source Location:** COSGN00C.cbl, lines 123-127  
**Field(s) Involved:** PASSWDI  
**Validation Condition:** Field must not be SPACES or LOW-VALUES  
**Trigger Conditions:** When user attempts to sign on (PROCESS-ENTER-KEY paragraph)

### RULE-VAL-003
**Rule Description:** User credentials must match security file  
**COBOL Source Location:** COSGN00C.cbl, lines 223-246  
**Field(s) Involved:** SEC-USR-PWD, WS-USER-PWD  
**Validation Condition:** Password from security file must equal entered password  
**Trigger Conditions:** After successful user lookup in USRSEC file

### RULE-VAL-004
**Rule Description:** User must exist in security file  
**COBOL Source Location:** COSGN00C.cbl, lines 247-251  
**Field(s) Involved:** WS-USER-ID  
**Validation Condition:** User ID must be found in USRSEC file (RESP-CD not equal 13)  
**Trigger Conditions:** During user authentication process

---

## User Management Validations

### RULE-VAL-005
**Rule Description:** First name is mandatory for user creation  
**COBOL Source Location:** COUSR01C.cbl, lines 118-123  
**Field(s) Involved:** FNAMEI  
**Validation Condition:** Field must not be SPACES or LOW-VALUES  
**Trigger Conditions:** When creating new user (PROCESS-ENTER-KEY paragraph)

### RULE-VAL-006
**Rule Description:** Last name is mandatory for user creation  
**COBOL Source Location:** COUSR01C.cbl, lines 124-129  
**Field(s) Involved:** LNAMEI  
**Validation Condition:** Field must not be SPACES or LOW-VALUES  
**Trigger Conditions:** When creating new user (PROCESS-ENTER-KEY paragraph)

### RULE-VAL-007
**Rule Description:** User ID is mandatory for user creation  
**COBOL Source Location:** COUSR01C.cbl, lines 130-135  
**Field(s) Involved:** USERIDI  
**Validation Condition:** Field must not be SPACES or LOW-VALUES  
**Trigger Conditions:** When creating new user (PROCESS-ENTER-KEY paragraph)

### RULE-VAL-008
**Rule Description:** Password is mandatory for user creation  
**COBOL Source Location:** COUSR01C.cbl, lines 136-141  
**Field(s) Involved:** PASSWDI  
**Validation Condition:** Field must not be SPACES or LOW-VALUES  
**Trigger Conditions:** When creating new user (PROCESS-ENTER-KEY paragraph)

### RULE-VAL-009
**Rule Description:** User type is mandatory for user creation  
**COBOL Source Location:** COUSR01C.cbl, lines 142-147  
**Field(s) Involved:** USRTYPEI  
**Validation Condition:** Field must not be SPACES or LOW-VALUES  
**Trigger Conditions:** When creating new user (PROCESS-ENTER-KEY paragraph)

### RULE-VAL-010
**Rule Description:** User ID must be unique  
**COBOL Source Location:** COUSR01C.cbl, lines 260-266  
**Field(s) Involved:** SEC-USR-ID  
**Validation Condition:** User ID must not already exist in USRSEC file (RESP-CD not DUPKEY/DUPREC)  
**Trigger Conditions:** When writing new user record to security file

### RULE-VAL-011
**Rule Description:** User ID is mandatory for user deletion  
**COBOL Source Location:** COUSR03C.cbl, lines 145-149  
**Field(s) Involved:** USRIDINI  
**Validation Condition:** Field must not be SPACES or LOW-VALUES  
**Trigger Conditions:** When deleting user (PROCESS-ENTER-KEY paragraph)

---

## Account Management Validations

### RULE-VAL-012
**Rule Description:** Account ID must be numeric  
**COBOL Source Location:** COACTUPC.cbl, lines 1802-1817  
**Field(s) Involved:** CC-ACCT-ID, CC-ACCT-ID-N  
**Validation Condition:** Field must be numeric and not equal to zeros  
**Trigger Conditions:** During account ID validation (1210-EDIT-ACCOUNT paragraph)

### RULE-VAL-013
**Rule Description:** Account ID must be 11 digits  
**COBOL Source Location:** COACTUPC.cbl, lines 1807-1808  
**Field(s) Involved:** CC-ACCT-ID  
**Validation Condition:** Must be a 11 digit non-zero number  
**Trigger Conditions:** During account ID validation (1210-EDIT-ACCOUNT paragraph)

### RULE-VAL-014
**Rule Description:** Mandatory fields must be supplied  
**COBOL Source Location:** COACTUPC.cbl, lines 1829-1851  
**Field(s) Involved:** WS-EDIT-ALPHANUM-ONLY  
**Validation Condition:** Field must not be LOW-VALUES, SPACES, or have zero length after trimming  
**Trigger Conditions:** During mandatory field validation (1215-EDIT-MANDATORY paragraph)

### RULE-VAL-015
**Rule Description:** Yes/No fields must be Y or N  
**COBOL Source Location:** COACTUPC.cbl, lines 1878-1892  
**Field(s) Involved:** WS-EDIT-YES-NO  
**Validation Condition:** Field must be 'Y' or 'N' (FLG-YES-NO-ISVALID)  
**Trigger Conditions:** During Yes/No field validation (1220-EDIT-YESNO paragraph)

### RULE-VAL-016
**Rule Description:** US phone area code must be numeric and non-zero  
**COBOL Source Location:** COACTUPC.cbl, lines 2264-2294  
**Field(s) Involved:** WS-EDIT-US-PHONE-NUMA, WS-EDIT-US-PHONE-NUMA-N  
**Validation Condition:** Must be numeric 3-digit number and not equal to zero  
**Trigger Conditions:** During US phone number validation (EDIT-AREA-CODE paragraph)

### RULE-VAL-017
**Rule Description:** US phone area code must be valid North America general purpose code  
**COBOL Source Location:** COACTUPC.cbl, lines 2298-2312  
**Field(s) Involved:** WS-US-PHONE-AREA-CODE-TO-EDIT  
**Validation Condition:** Must pass VALID-GENERAL-PURP-CODE validation  
**Trigger Conditions:** During US phone number validation (EDIT-AREA-CODE paragraph)

### RULE-VAL-018
**Rule Description:** US phone prefix must be numeric and non-zero  
**COBOL Source Location:** COACTUPC.cbl, lines 2335-2365  
**Field(s) Involved:** WS-EDIT-US-PHONE-NUMB, WS-EDIT-US-PHONE-NUMB-N  
**Validation Condition:** Must be numeric 3-digit number and not equal to zero  
**Trigger Conditions:** During US phone number validation (EDIT-US-PHONE-PREFIX paragraph)

### RULE-VAL-019
**Rule Description:** US phone line number must be numeric  
**COBOL Source Location:** COACTUPC.cbl, lines 2388-2400  
**Field(s) Involved:** WS-EDIT-US-PHONE-NUMC  
**Validation Condition:** Must be numeric 4-digit number  
**Trigger Conditions:** During US phone number validation (EDIT-US-PHONE-LINENUM paragraph)

### RULE-VAL-020
**Rule Description:** Signed numeric fields must be valid  
**COBOL Source Location:** COACTUPC.cbl, lines 2201-2218  
**Field(s) Involved:** WS-EDIT-SIGNED-NUMBER-9V2-X  
**Validation Condition:** Must pass FUNCTION TEST-NUMVAL-C validation (result = 0)  
**Trigger Conditions:** During signed numeric validation (1250-EDIT-SIGNED-9V2 paragraph)

---

## Transaction Management Validations

### RULE-VAL-021
**Rule Description:** Account ID must be numeric for transactions  
**COBOL Source Location:** COTRN02C.cbl, lines 197-203  
**Field(s) Involved:** ACTIDINI  
**Validation Condition:** Field must be numeric (IS NUMERIC test)  
**Trigger Conditions:** When validating input key fields (VALIDATE-INPUT-KEY-FIELDS paragraph)

### RULE-VAL-022
**Rule Description:** Card number must be numeric for transactions  
**COBOL Source Location:** COTRN02C.cbl, lines 211-217  
**Field(s) Involved:** CARDNINI  
**Validation Condition:** Field must be numeric (IS NOT NUMERIC fails)  
**Trigger Conditions:** When validating input key fields (VALIDATE-INPUT-KEY-FIELDS paragraph)

### RULE-VAL-023
**Rule Description:** Either Account ID or Card Number must be provided  
**COBOL Source Location:** COTRN02C.cbl, lines 224-230  
**Field(s) Involved:** ACTIDINI, CARDNINI  
**Validation Condition:** At least one of Account ID or Card Number must be entered  
**Trigger Conditions:** When validating input key fields (VALIDATE-INPUT-KEY-FIELDS paragraph)

### RULE-VAL-024
**Rule Description:** Merchant ID must be numeric  
**COBOL Source Location:** COTRN02C.cbl, lines 430-436  
**Field(s) Involved:** MIDI  
**Validation Condition:** Field must be numeric (IS NOT NUMERIC fails)  
**Trigger Conditions:** During input data field validation (VALIDATE-INPUT-DATA-FIELDS paragraph)

### RULE-VAL-025
**Rule Description:** Transaction origin date must be valid  
**COBOL Source Location:** COTRN02C.cbl, lines 400-406  
**Field(s) Involved:** TORIGDTI, CSUTLDTC-RESULT-MSG-NUM  
**Validation Condition:** Date must pass CSUTLDTC validation (result message not '2513')  
**Trigger Conditions:** During input data field validation using CSUTLDTC utility

### RULE-VAL-026
**Rule Description:** Transaction process date must be valid  
**COBOL Source Location:** COTRN02C.cbl, lines 420-427  
**Field(s) Involved:** TPROCDTI, CSUTLDTC-RESULT-MSG-NUM  
**Validation Condition:** Date must pass CSUTLDTC validation (result message not '2513')  
**Trigger Conditions:** During input data field validation using CSUTLDTC utility

### RULE-VAL-027
**Rule Description:** Transaction ID must not be empty for viewing  
**COBOL Source Location:** COTRN01C.cbl, lines 147-152  
**Field(s) Involved:** TRNIDINI  
**Validation Condition:** Field must not be SPACES or LOW-VALUES  
**Trigger Conditions:** When processing transaction view request (PROCESS-ENTER-KEY paragraph)

### RULE-VAL-028
**Rule Description:** Transaction ID must be numeric for browsing  
**COBOL Source Location:** COTRN00C.cbl, lines 209-214  
**Field(s) Involved:** TRNIDINI  
**Validation Condition:** Field must be numeric (IS NUMERIC test)  
**Trigger Conditions:** When validating transaction browse input (VALIDATE-INPUT paragraph)

---

## Card Management Validations

### RULE-VAL-029
**Rule Description:** Card account filter must be 11 digit number  
**COBOL Source Location:** COCRDUPC.cbl, lines 740-750  
**Field(s) Involved:** CC-ACCT-ID  
**Validation Condition:** Must be numeric and 11 characters long  
**Trigger Conditions:** During account filter validation (1210-EDIT-ACCOUNT paragraph)

### RULE-VAL-030
**Rule Description:** Card ID filter must be 16 digit number  
**COBOL Source Location:** COCRDUPC.cbl, lines 784-794  
**Field(s) Involved:** CC-CARD-NUM  
**Validation Condition:** Must be numeric and 16 characters long  
**Trigger Conditions:** During card filter validation (1220-EDIT-CARD paragraph)

### RULE-VAL-031
**Rule Description:** Card number must not be empty or zero  
**COBOL Source Location:** COCRDUPC.cbl, lines 768-779  
**Field(s) Involved:** CC-CARD-NUM, CC-CARD-NUM-N  
**Validation Condition:** Must not be LOW-VALUES, SPACES, or equal to zeros  
**Trigger Conditions:** During card filter validation (1220-EDIT-CARD paragraph)

---

## Menu & Navigation Validations

### RULE-VAL-032
**Rule Description:** Menu option must be numeric  
**COBOL Source Location:** COMEN01C.cbl, lines 127-134  
**Field(s) Involved:** WS-OPTION  
**Validation Condition:** Must be numeric, within valid range (1 to CDEMO-MENU-OPT-COUNT), and not zero  
**Trigger Conditions:** When processing menu selection (PROCESS-ENTER-KEY paragraph)

### RULE-VAL-033
**Rule Description:** User access control for admin-only options  
**COBOL Source Location:** COMEN01C.cbl, lines 136-143  
**Field(s) Involved:** CDEMO-USRTYP-USER, CDEMO-MENU-OPT-USRTYPE  
**Validation Condition:** Regular users cannot access admin-only menu options (type 'A')  
**Trigger Conditions:** When processing menu selection after option validation

---

## Bill Payment Validations

### RULE-VAL-034
**Rule Description:** Account ID is mandatory for bill payment  
**COBOL Source Location:** COBIL00C.cbl, lines 159-167  
**Field(s) Involved:** ACTIDINI  
**Validation Condition:** Field must not be SPACES or LOW-VALUES  
**Trigger Conditions:** When processing bill payment request (PROCESS-ENTER-KEY paragraph)

### RULE-VAL-035
**Rule Description:** Payment confirmation must be Y or N  
**COBOL Source Location:** COBIL00C.cbl, lines 173-191  
**Field(s) Involved:** CONFIRMI  
**Validation Condition:** Must be 'Y', 'y', 'N', 'n', SPACES, or LOW-VALUES  
**Trigger Conditions:** When processing bill payment confirmation

### RULE-VAL-036
**Rule Description:** Account must have positive balance for payment  
**COBOL Source Location:** COBIL00C.cbl, lines 198-206  
**Field(s) Involved:** ACCT-CURR-BAL  
**Validation Condition:** Current balance must be greater than zero  
**Trigger Conditions:** After account data retrieval and before payment processing

---

## Date Validation Utility Rules

### RULE-VAL-037
**Rule Description:** Date format validation using CEEDAYS API  
**COBOL Source Location:** CSUTLDTC.cbl, lines 128-149  
**Field(s) Involved:** FEEDBACK-CODE conditions  
**Validation Condition:** Date must pass CEEDAYS API validation with various error conditions checked  
**Trigger Conditions:** When CSUTLDTC utility is called for date validation

### RULE-VAL-038
**Rule Description:** Date must not have insufficient data  
**COBOL Source Location:** CSUTLDTC.cbl, lines 131-132  
**Field(s) Involved:** FC-INSUFFICIENT-DATA  
**Validation Condition:** Date input must have sufficient data for validation  
**Trigger Conditions:** During CEEDAYS API date validation

### RULE-VAL-039
**Rule Description:** Date value must be valid  
**COBOL Source Location:** CSUTLDTC.cbl, lines 133-134  
**Field(s) Involved:** FC-BAD-DATE-VALUE  
**Validation Condition:** Date value must be a valid date  
**Trigger Conditions:** During CEEDAYS API date validation

### RULE-VAL-040
**Rule Description:** Date month must be valid  
**COBOL Source Location:** CSUTLDTC.cbl, lines 139-140  
**Field(s) Involved:** FC-INVALID-MONTH  
**Validation Condition:** Month component of date must be valid (1-12)  
**Trigger Conditions:** During CEEDAYS API date validation

---

## Summary

**Total Validation Rules Extracted:** 40  
**Programs Analyzed:** 18  
**Validation Categories:**
- Authentication & Sign-on: 4 rules
- User Management: 7 rules  
- Account Management: 9 rules
- Transaction Management: 8 rules
- Card Management: 3 rules
- Menu & Navigation: 2 rules
- Bill Payment: 3 rules
- Date Validation Utility: 4 rules

**Common Validation Patterns:**
1. **Mandatory Field Validation**: Fields must not be SPACES or LOW-VALUES
2. **Numeric Validation**: Fields must pass IS NUMERIC test
3. **Length Validation**: Fields must meet specific length requirements
4. **Range Validation**: Values must be within acceptable ranges
5. **Code Validation**: Fields must match predefined valid values
6. **Cross-Field Validation**: Multiple fields validated together
7. **Business Rule Validation**: Domain-specific business logic validation

These validation rules provide a comprehensive foundation for implementing modern validation layers in frontend and backend systems during the CardDemo application modernization process.
