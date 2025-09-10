# CardDemo COBOL Business Validation Rules

This document contains business-level validation rules extracted from the CardDemo COBOL application. These rules represent field-level, range, code/value, domain-specific, and conditional validations that should be migrated to modern validation layers.

## Authentication & Sign-on Validations

### RULE-VAL-001
**Rule Description:** User ID must not be empty  
**COBOL Source Location:** COSGN00C.cbl, lines 118-122, PROCESS-ENTER-KEY paragraph  
**Field(s) Involved:** USERIDI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When ENTER key is pressed on signon screen  

### RULE-VAL-002
**Rule Description:** Password must not be empty  
**COBOL Source Location:** COSGN00C.cbl, lines 123-126, PROCESS-ENTER-KEY paragraph  
**Field(s) Involved:** PASSWDI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When ENTER key is pressed on signon screen  

## Transaction Processing Validations

### RULE-VAL-003
**Rule Description:** Account ID must be numeric for transaction processing  
**COBOL Source Location:** COTRN02C.cbl, lines 197-203, VALIDATE-INPUT-KEY-FIELDS paragraph  
**Field(s) Involved:** ACTIDINI  
**Validation Condition:** Field must be numeric when not SPACES or LOW-VALUES  
**Trigger Conditions:** When processing transaction entry with account ID provided  

### RULE-VAL-004
**Rule Description:** Card Number must be numeric for transaction processing  
**COBOL Source Location:** COTRN02C.cbl, lines 211-217, VALIDATE-INPUT-KEY-FIELDS paragraph  
**Field(s) Involved:** CARDNINI  
**Validation Condition:** Field must be numeric when not SPACES or LOW-VALUES  
**Trigger Conditions:** When processing transaction entry with card number provided  

### RULE-VAL-005
**Rule Description:** Either Account ID or Card Number must be provided  
**COBOL Source Location:** COTRN02C.cbl, lines 224-230, VALIDATE-INPUT-KEY-FIELDS paragraph  
**Field(s) Involved:** ACTIDINI, CARDNINI  
**Validation Condition:** At least one field must be provided (not SPACES or LOW-VALUES)  
**Trigger Conditions:** When processing transaction entry  

### RULE-VAL-006
**Rule Description:** Transaction Type Code cannot be empty  
**COBOL Source Location:** COTRN02C.cbl, lines 252-257, VALIDATE-INPUT-DATA-FIELDS paragraph  
**Field(s) Involved:** TTYPCDI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When validating transaction data fields  

### RULE-VAL-007
**Rule Description:** Transaction Category Code cannot be empty  
**COBOL Source Location:** COTRN02C.cbl, lines 258-263, VALIDATE-INPUT-DATA-FIELDS paragraph  
**Field(s) Involved:** TCATCDI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When validating transaction data fields  

### RULE-VAL-008
**Rule Description:** Transaction Source cannot be empty  
**COBOL Source Location:** COTRN02C.cbl, lines 264-269, VALIDATE-INPUT-DATA-FIELDS paragraph  
**Field(s) Involved:** TRNSRCI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When validating transaction data fields  

### RULE-VAL-009
**Rule Description:** Transaction Description cannot be empty  
**COBOL Source Location:** COTRN02C.cbl, lines 270-275, VALIDATE-INPUT-DATA-FIELDS paragraph  
**Field(s) Involved:** TDESCI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When validating transaction data fields  

### RULE-VAL-010
**Rule Description:** Transaction Amount cannot be empty  
**COBOL Source Location:** COTRN02C.cbl, lines 276-281, VALIDATE-INPUT-DATA-FIELDS paragraph  
**Field(s) Involved:** TRNAMTI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When validating transaction data fields  

### RULE-VAL-011
**Rule Description:** Transaction Original Date cannot be empty  
**COBOL Source Location:** COTRN02C.cbl, lines 282-287, VALIDATE-INPUT-DATA-FIELDS paragraph  
**Field(s) Involved:** TORIGDTI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When validating transaction data fields  

### RULE-VAL-012
**Rule Description:** Transaction Process Date cannot be empty  
**COBOL Source Location:** COTRN02C.cbl, lines 288-293, VALIDATE-INPUT-DATA-FIELDS paragraph  
**Field(s) Involved:** TPROCDTI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When validating transaction data fields  

### RULE-VAL-013
**Rule Description:** Merchant ID cannot be empty  
**COBOL Source Location:** COTRN02C.cbl, lines 294-299, VALIDATE-INPUT-DATA-FIELDS paragraph  
**Field(s) Involved:** MIDI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When validating transaction data fields  

### RULE-VAL-014
**Rule Description:** Merchant Name cannot be empty  
**COBOL Source Location:** COTRN02C.cbl, lines 300+, VALIDATE-INPUT-DATA-FIELDS paragraph  
**Field(s) Involved:** MNAMEI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When validating transaction data fields  

### RULE-VAL-015
**Rule Description:** Transaction confirmation must be Y or N  
**COBOL Source Location:** COTRN02C.cbl, lines 169-188, PROCESS-ENTER-KEY paragraph  
**Field(s) Involved:** CONFIRMI  
**Validation Condition:** Field must be 'Y', 'y', 'N', 'n', SPACES, or LOW-VALUES  
**Trigger Conditions:** When confirming transaction addition  

### RULE-VAL-016
**Rule Description:** Transaction ID must not be empty for transaction view  
**COBOL Source Location:** COTRN01C.cbl, lines 147-150, PROCESS-ENTER-KEY paragraph  
**Field(s) Involved:** TRNIDINI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When viewing transaction details  

## Credit Card Validations

### RULE-VAL-017
**Rule Description:** Account ID must be 11-digit numeric for card operations  
**COBOL Source Location:** COCRDUPC.cbl, lines 740-750, 1210-EDIT-ACCOUNT paragraph  
**Field(s) Involved:** CC-ACCT-ID  
**Validation Condition:** Field must be numeric and 11 digits  
**Trigger Conditions:** When validating account filter for card operations  

### RULE-VAL-018
**Rule Description:** Card Number must be 16-digit numeric  
**COBOL Source Location:** COCRDUPC.cbl, lines 784-794, 1220-EDIT-CARD paragraph  
**Field(s) Involved:** CC-CARD-NUM  
**Validation Condition:** Field must be numeric and 16 digits  
**Trigger Conditions:** When validating card filter for card operations  

### RULE-VAL-019
**Rule Description:** Card Name must contain only alphabetic characters and spaces  
**COBOL Source Location:** COCRDUPC.cbl, lines 822-837, 1230-EDIT-NAME paragraph  
**Field(s) Involved:** CCUP-NEW-CRDNAME  
**Validation Condition:** Field must contain only letters and spaces after conversion test  
**Trigger Conditions:** When validating card name during update  

### RULE-VAL-020
**Rule Description:** Card Status must be Y or N  
**COBOL Source Location:** COCRDUPC.cbl, lines 861-872, 1240-EDIT-CARDSTATUS paragraph  
**Field(s) Involved:** CCUP-NEW-CRDSTCD  
**Validation Condition:** Field must be valid Y/N value using FLG-YES-NO-VALID condition  
**Trigger Conditions:** When validating card status during update  

### RULE-VAL-021
**Rule Description:** Card Expiry Month must be between 1 and 12  
**COBOL Source Location:** COCRDUPC.cbl, lines 95, 896-907, VALID-MONTH condition and 1250-EDIT-EXPIRY-MON paragraph  
**Field(s) Involved:** CCUP-NEW-EXPMON  
**Validation Condition:** Field must be numeric and have value 1 THRU 12  
**Trigger Conditions:** When validating card expiry month during update  

### RULE-VAL-022
**Rule Description:** Card Expiry Year must be between 1950 and 2099  
**COBOL Source Location:** COCRDUPC.cbl, lines 99, 932-943, VALID-YEAR condition and 1260-EDIT-EXPIRY-YEAR paragraph  
**Field(s) Involved:** CCUP-NEW-EXPYEAR  
**Validation Condition:** Field must be numeric and have value 1950 THRU 2099  
**Trigger Conditions:** When validating card expiry year during update  

## Account Management Validations

### RULE-VAL-023
**Rule Description:** Account Status must be Y or N  
**COBOL Source Location:** COACTUPC.cbl, lines 1472-1476, 1200-EDIT-MAP-INPUTS paragraph  
**Field(s) Involved:** ACUP-NEW-ACTIVE-STATUS  
**Validation Condition:** Field must be valid Y/N value using 1220-EDIT-YESNO routine  
**Trigger Conditions:** When validating account status during update  

### RULE-VAL-024
**Rule Description:** Account Open Date must be valid date format  
**COBOL Source Location:** COACTUPC.cbl, lines 1478-1482, 1200-EDIT-MAP-INPUTS paragraph  
**Field(s) Involved:** ACUP-NEW-OPEN-DATE  
**Validation Condition:** Field must pass EDIT-DATE-CCYYMMDD validation routine  
**Trigger Conditions:** When validating account open date during update  

### RULE-VAL-025
**Rule Description:** Credit Limit must be valid signed numeric value  
**COBOL Source Location:** COACTUPC.cbl, lines 1484-1488, 1200-EDIT-MAP-INPUTS paragraph  
**Field(s) Involved:** ACUP-NEW-CREDIT-LIMIT-X  
**Validation Condition:** Field must pass 1250-EDIT-SIGNED-9V2 validation routine  
**Trigger Conditions:** When validating credit limit during update  

### RULE-VAL-026
**Rule Description:** Cash Credit Limit must be valid signed numeric value  
**COBOL Source Location:** COACTUPC.cbl, lines 1496-1501, 1200-EDIT-MAP-INPUTS paragraph  
**Field(s) Involved:** ACUP-NEW-CASH-CREDIT-LIMIT-X  
**Validation Condition:** Field must pass 1250-EDIT-SIGNED-9V2 validation routine  
**Trigger Conditions:** When validating cash credit limit during update  

### RULE-VAL-027
**Rule Description:** Current Balance must be valid signed numeric value  
**COBOL Source Location:** COACTUPC.cbl, lines 1509-1513, 1200-EDIT-MAP-INPUTS paragraph  
**Field(s) Involved:** ACUP-NEW-CURR-BAL-X  
**Validation Condition:** Field must pass 1250-EDIT-SIGNED-9V2 validation routine  
**Trigger Conditions:** When validating current balance during update  

### RULE-VAL-028
**Rule Description:** SSN must be valid US Social Security Number format  
**COBOL Source Location:** COACTUPC.cbl, lines 1529-1531, 1200-EDIT-MAP-INPUTS paragraph  
**Field(s) Involved:** ACUP-NEW-CUST-SSN-X (parts 1, 2, 3)  
**Validation Condition:** Field must pass 1265-EDIT-US-SSN validation routine  
**Trigger Conditions:** When validating SSN during account update  

### RULE-VAL-029
**Rule Description:** Date of Birth must be valid date and reasonable age  
**COBOL Source Location:** COACTUPC.cbl, lines 1533-1543, 1200-EDIT-MAP-INPUTS paragraph  
**Field(s) Involved:** ACUP-NEW-CUST-DOB-YYYY-MM-DD  
**Validation Condition:** Field must pass EDIT-DATE-CCYYMMDD and EDIT-DATE-OF-BIRTH validation routines  
**Trigger Conditions:** When validating date of birth during account update  

### RULE-VAL-030
**Rule Description:** FICO Score must be 3-digit numeric value  
**COBOL Source Location:** COACTUPC.cbl, lines 1545-1556, 1200-EDIT-MAP-INPUTS paragraph  
**Field(s) Involved:** ACUP-NEW-CUST-FICO-SCORE-X  
**Validation Condition:** Field must be 3-character numeric using 1245-EDIT-NUM-REQD routine  
**Trigger Conditions:** When validating FICO score during account update  

### RULE-VAL-031
**Rule Description:** First Name must be alphabetic and required  
**COBOL Source Location:** COACTUPC.cbl, lines 1560-1566, 1200-EDIT-MAP-INPUTS paragraph  
**Field(s) Involved:** ACUP-NEW-CUST-FIRST-NAME  
**Validation Condition:** Field must be 25-character alphabetic using 1225-EDIT-ALPHA-REQD routine  
**Trigger Conditions:** When validating first name during account update  

### RULE-VAL-032
**Rule Description:** Middle Name must be alphabetic (optional)  
**COBOL Source Location:** COACTUPC.cbl, lines 1568-1574, 1200-EDIT-MAP-INPUTS paragraph  
**Field(s) Involved:** ACUP-NEW-CUST-MIDDLE-NAME  
**Validation Condition:** Field must be 25-character alphabetic using 1235-EDIT-ALPHA-OPT routine  
**Trigger Conditions:** When validating middle name during account update  

### RULE-VAL-033
**Rule Description:** Last Name must be alphabetic and required  
**COBOL Source Location:** COACTUPC.cbl, lines 1576-1582, 1200-EDIT-MAP-INPUTS paragraph  
**Field(s) Involved:** ACUP-NEW-CUST-LAST-NAME  
**Validation Condition:** Field must be 25-character alphabetic using 1225-EDIT-ALPHA-REQD routine  
**Trigger Conditions:** When validating last name during account update  

### RULE-VAL-034
**Rule Description:** Address Line 1 is mandatory  
**COBOL Source Location:** COACTUPC.cbl, lines 1584-1590, 1200-EDIT-MAP-INPUTS paragraph  
**Field(s) Involved:** ACUP-NEW-CUST-ADDR-LINE-1  
**Validation Condition:** Field must not be empty using 1215-EDIT-MANDATORY routine  
**Trigger Conditions:** When validating address during account update  

### RULE-VAL-035
**Rule Description:** State Code must be 2-character alphabetic  
**COBOL Source Location:** COACTUPC.cbl, lines 1592-1602, 1200-EDIT-MAP-INPUTS paragraph  
**Field(s) Involved:** ACUP-NEW-CUST-ADDR-STATE-CD  
**Validation Condition:** Field must be 2-character alphabetic and valid US state using 1225-EDIT-ALPHA-REQD and 1270-EDIT-US-STATE-CD routines  
**Trigger Conditions:** When validating state code during account update  

### RULE-VAL-036
**Rule Description:** ZIP Code must be 5-digit numeric  
**COBOL Source Location:** COACTUPC.cbl, lines 1605-1611, 1200-EDIT-MAP-INPUTS paragraph  
**Field(s) Involved:** ACUP-NEW-CUST-ADDR-ZIP  
**Validation Condition:** Field must be 5-character numeric using 1245-EDIT-NUM-REQD routine  
**Trigger Conditions:** When validating ZIP code during account update  

### RULE-VAL-037
**Rule Description:** City must be alphabetic and required  
**COBOL Source Location:** COACTUPC.cbl, lines 1615-1621, 1200-EDIT-MAP-INPUTS paragraph  
**Field(s) Involved:** ACUP-NEW-CUST-ADDR-LINE-3  
**Validation Condition:** Field must be 50-character alphabetic using 1225-EDIT-ALPHA-REQD routine  
**Trigger Conditions:** When validating city during account update  

### RULE-VAL-038
**Rule Description:** Country Code must be 3-character alphabetic  
**COBOL Source Location:** COACTUPC.cbl, lines 1623-1630, 1200-EDIT-MAP-INPUTS paragraph  
**Field(s) Involved:** ACUP-NEW-CUST-ADDR-COUNTRY-CD  
**Validation Condition:** Field must be 3-character alphabetic using 1225-EDIT-ALPHA-REQD routine  
**Trigger Conditions:** When validating country code during account update  

### RULE-VAL-039
**Rule Description:** Phone Number 1 must be valid US phone format  
**COBOL Source Location:** COACTUPC.cbl, lines 1632-1638, 1200-EDIT-MAP-INPUTS paragraph  
**Field(s) Involved:** ACUP-NEW-CUST-PHONE-NUM-1  
**Validation Condition:** Field must pass 1260-EDIT-US-PHONE-NUM validation routine  
**Trigger Conditions:** When validating phone number 1 during account update  

### RULE-VAL-040
**Rule Description:** Phone Number 2 must be valid US phone format  
**COBOL Source Location:** COACTUPC.cbl, lines 1640-1646, 1200-EDIT-MAP-INPUTS paragraph  
**Field(s) Involved:** ACUP-NEW-CUST-PHONE-NUM-2  
**Validation Condition:** Field must pass 1260-EDIT-US-PHONE-NUM validation routine  
**Trigger Conditions:** When validating phone number 2 during account update  

### RULE-VAL-041
**Rule Description:** EFT Account ID must be 10-digit numeric  
**COBOL Source Location:** COACTUPC.cbl, lines 1648-1655, 1200-EDIT-MAP-INPUTS paragraph  
**Field(s) Involved:** ACUP-NEW-CUST-EFT-ACCOUNT-ID  
**Validation Condition:** Field must be 10-character numeric using 1245-EDIT-NUM-REQD routine  
**Trigger Conditions:** When validating EFT account ID during account update  

### RULE-VAL-042
**Rule Description:** Primary Card Holder indicator must be Y or N  
**COBOL Source Location:** COACTUPC.cbl, lines 1657-1662, 1200-EDIT-MAP-INPUTS paragraph  
**Field(s) Involved:** ACUP-NEW-CUST-PRI-HOLDER-IND  
**Validation Condition:** Field must be valid Y/N value using 1220-EDIT-YESNO routine  
**Trigger Conditions:** When validating primary cardholder indicator during account update  

### RULE-VAL-043
**Rule Description:** State and ZIP Code combination must be valid  
**COBOL Source Location:** COACTUPC.cbl, lines 1665-1669, 1200-EDIT-MAP-INPUTS paragraph  
**Field(s) Involved:** ACUP-NEW-CUST-ADDR-STATE-CD, ACUP-NEW-CUST-ADDR-ZIP  
**Validation Condition:** Cross-field validation using 1280-EDIT-US-STATE-ZIP-CD routine  
**Trigger Conditions:** When both state and ZIP code are valid during account update  

## Bill Payment Validations

### RULE-VAL-044
**Rule Description:** Account ID cannot be empty for bill payment  
**COBOL Source Location:** COBIL00C.cbl, lines 159-164, PROCESS-ENTER-KEY paragraph  
**Field(s) Involved:** ACTIDINI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When processing bill payment  

### RULE-VAL-045
**Rule Description:** Bill payment confirmation must be Y or N  
**COBOL Source Location:** COBIL00C.cbl, lines 173-191, PROCESS-ENTER-KEY paragraph  
**Field(s) Involved:** CONFIRMI  
**Validation Condition:** Field must be 'Y', 'y', 'N', 'n', SPACES, or LOW-VALUES  
**Trigger Conditions:** When confirming bill payment  

### RULE-VAL-046
**Rule Description:** Account balance must be greater than zero for payment  
**COBOL Source Location:** COBIL00C.cbl, lines 198-200, PROCESS-ENTER-KEY paragraph  
**Field(s) Involved:** ACCT-CURR-BAL  
**Validation Condition:** Balance must be greater than ZEROS  
**Trigger Conditions:** When processing bill payment after account validation  

## User Management Validations

### RULE-VAL-047
**Rule Description:** First Name cannot be empty for user creation  
**COBOL Source Location:** COUSR01C.cbl, lines 118-123, PROCESS-ENTER-KEY paragraph  
**Field(s) Involved:** FNAMEI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When creating new user  

### RULE-VAL-048
**Rule Description:** Last Name cannot be empty for user creation  
**COBOL Source Location:** COUSR01C.cbl, lines 124-129, PROCESS-ENTER-KEY paragraph  
**Field(s) Involved:** LNAMEI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When creating new user  

### RULE-VAL-049
**Rule Description:** User ID cannot be empty for user creation  
**COBOL Source Location:** COUSR01C.cbl, lines 130-135, PROCESS-ENTER-KEY paragraph  
**Field(s) Involved:** USERIDI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When creating new user  

### RULE-VAL-050
**Rule Description:** Password cannot be empty for user creation  
**COBOL Source Location:** COUSR01C.cbl, lines 136-141, PROCESS-ENTER-KEY paragraph  
**Field(s) Involved:** PASSWDI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When creating new user  

### RULE-VAL-051
**Rule Description:** User Type cannot be empty for user creation  
**COBOL Source Location:** COUSR01C.cbl, lines 142-147, PROCESS-ENTER-KEY paragraph  
**Field(s) Involved:** USRTYPEI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When creating new user  

### RULE-VAL-052
**Rule Description:** User ID cannot be empty for user update  
**COBOL Source Location:** COUSR02C.cbl, lines 146-151, PROCESS-ENTER-KEY paragraph  
**Field(s) Involved:** USRIDINI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When updating user information  

### RULE-VAL-053
**Rule Description:** First Name cannot be empty for user update  
**COBOL Source Location:** COUSR02C.cbl, lines 186-191, UPDATE-USER-INFO paragraph  
**Field(s) Involved:** FNAMEI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When updating user information  

### RULE-VAL-054
**Rule Description:** Last Name cannot be empty for user update  
**COBOL Source Location:** COUSR02C.cbl, lines 192-197, UPDATE-USER-INFO paragraph  
**Field(s) Involved:** LNAMEI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When updating user information  

### RULE-VAL-055
**Rule Description:** Password cannot be empty for user update  
**COBOL Source Location:** COUSR02C.cbl, lines 198-200+, UPDATE-USER-INFO paragraph  
**Field(s) Involved:** PASSWDI  
**Validation Condition:** Field must not equal SPACES or LOW-VALUES  
**Trigger Conditions:** When updating user information  

## Date Validation Utility

### RULE-VAL-056
**Rule Description:** Date format validation using CEEDAYS API  
**COBOL Source Location:** CSUTLDTC.cbl, lines 116-149, A000-MAIN paragraph  
**Field(s) Involved:** LS-DATE, LS-DATE-FORMAT  
**Validation Condition:** Date must pass CEEDAYS API validation with various feedback codes  
**Trigger Conditions:** When called by other programs for date validation  

## Summary

This document contains 56 business validation rules extracted from 18 COBOL programs in the CardDemo application. The validations cover:

- **Authentication**: User credentials validation
- **Transaction Processing**: Field requirements and data integrity
- **Credit Card Management**: Format, range, and business rule validations  
- **Account Management**: Comprehensive customer and account data validation
- **Bill Payment**: Account and confirmation validations
- **User Management**: User creation and update validations
- **Date Validation**: Centralized date validation utility

These rules should be implemented in modern validation frameworks while maintaining the same business logic and error handling patterns.
