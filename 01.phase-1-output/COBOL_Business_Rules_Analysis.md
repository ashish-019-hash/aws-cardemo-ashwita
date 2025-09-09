# COBOL Business Rules Analysis - CardDemo Application

## Executive Summary

This document presents the comprehensive analysis of business calculation rules extracted from 18 COBOL programs in the CardDemo application. The analysis focused exclusively on business-level calculations while excluding technical validation and infrastructure logic.

**Key Findings:**
- **Total COBOL Files Analyzed:** 18
- **Business Calculation Rules Identified:** 4
- **Programs with Business Logic:** 3 (COBIL00C, COTRN02C, CORPT00C)
- **Primary Business Areas:** Payment Processing, Transaction Management, Financial Reporting

## Business Rules Inventory

### RULE-CALC-001: Bill Payment Balance Calculation

**Rule ID:** RULE-CALC-001

**Rule Description:** Updates account balance by subtracting payment amount when processing bill payments

**COBOL Source Location:** COBIL00C.cbl, Line 234

**Involved Variables:**
- `ACCT-CURR-BAL` - Current account balance (PIC S9(10)V99)
- `TRAN-AMT` - Transaction amount being paid (PIC S9(10)V99)

**Input Conditions:**
- Payment transaction must be validated
- Account must exist and be accessible
- Transaction amount must be positive

**Calculation Logic/Formula:**
```
New Account Balance = Current Account Balance - Payment Amount
COMPUTE ACCT-CURR-BAL = ACCT-CURR-BAL - TRAN-AMT
```

**Business Impact:** Critical for maintaining accurate account balances during payment processing

---

### RULE-CALC-002: Transaction ID Generation

**Rule ID:** RULE-CALC-002

**Rule Description:** Generates sequential transaction IDs for new transactions across multiple business processes

**COBOL Source Location:** 
- COBIL00C.cbl, Lines 216-217 (Bill Payment)
- COTRN02C.cbl, Lines 448-449 (Transaction Addition)

**Involved Variables:**
- `WS-TRAN-ID-NUM` - Working storage transaction ID counter (PIC 9(16))
- `TRAN-ID` - Generated transaction identifier (PIC X(16))

**Input Conditions:**
- New transaction being created
- System must be able to read last transaction ID

**Calculation Logic/Formula:**
```
Next Transaction ID = Last Transaction ID + 1
ADD 1 TO WS-TRAN-ID-NUM
MOVE WS-TRAN-ID-NUM TO TRAN-ID
```

**Business Impact:** Ensures unique transaction identification across all business processes

---

### RULE-CALC-003: Daily Report Date Range Calculation

**Rule ID:** RULE-CALC-003

**Rule Description:** Calculates previous day's date for daily transaction reports

**COBOL Source Location:** CORPT00C.cbl, Lines 229-230

**Involved Variables:**
- `WS-CURDATE-N` - Current date in numeric format (PIC 9(8))
- `WS-START-DATE` - Report start date (PIC X(8))
- `WS-END-DATE` - Report end date (PIC X(8))

**Input Conditions:**
- Daily report option selected
- Current system date available

**Calculation Logic/Formula:**
```
Previous Day = Current Date - 1 Day
COMPUTE WS-CURDATE-N = FUNCTION DATE-OF-INTEGER(FUNCTION INTEGER-OF-DATE(WS-CURDATE-N) - 1)
```

**Business Impact:** Enables accurate daily transaction reporting for business analysis

---

### RULE-CALC-004: Monthly Report Date Range Calculation

**Rule ID:** RULE-CALC-004

**Rule Description:** Calculates first and last day of previous month for monthly transaction reports

**COBOL Source Location:** CORPT00C.cbl, Lines 224-228

**Involved Variables:**
- `WS-CURDATE-YYYY` - Current year (PIC 9(4))
- `WS-CURDATE-MM` - Current month (PIC 9(2))
- `WS-START-DATE` - Report start date (first day of previous month)
- `WS-END-DATE` - Report end date (last day of previous month)

**Input Conditions:**
- Monthly report option selected
- Current system date available

**Calculation Logic/Formula:**
```
If current month = 01 (January):
    Previous month = 12, Previous year = Current year - 1
Else:
    Previous month = Current month - 1, Previous year = Current year

Start Date = YYYY-MM-01 (First day of previous month)
End Date = YYYY-MM-DD (Last day of previous month)
```

**Business Impact:** Provides accurate monthly financial reporting capabilities

## Rule Dependencies and Relationships

### Primary Dependencies

1. **RULE-CALC-002 → RULE-CALC-001**: Transaction ID generation is used by bill payment processing
2. **RULE-CALC-002 → Transaction Addition**: Transaction ID generation is used by new transaction creation
3. **RULE-CALC-003 ↔ RULE-CALC-004**: Both rules support the reporting framework with different time periods

### Business Process Integration

- **Payment Processing Flow**: RULE-CALC-002 → RULE-CALC-001
- **Transaction Management Flow**: RULE-CALC-002 → Transaction Creation
- **Reporting Flow**: RULE-CALC-003 OR RULE-CALC-004 → Report Generation

## Analysis Methodology

### Inclusion Criteria
- Financial calculations (balance updates, payment processing)
- Business logic formulas (date calculations for reports)
- Conditional logic for business decisions
- Aggregation rules related to financial domain

### Exclusion Criteria
- Input validation rules
- Technical/infrastructure logic
- File handling operations
- Cursor processing
- Screen layout logic
- Error handling routines
- Database connection logic
- I/O operations

### Files Analyzed
1. COBIL00C.cbl - Bill Payment Processing ✓ (Contains RULE-CALC-001, RULE-CALC-002)
2. COACTUPC.cbl - Account Update Processing (No business calculations)
3. COACTVWC.cbl - Account View Processing (No business calculations)
4. COADM01C.cbl - Admin Menu (No business calculations)
5. COCRDLIC.cbl - Credit Card List (No business calculations)
6. COCRDSLC.cbl - Credit Card Detail View (No business calculations)
7. COCRDUPC.cbl - Credit Card Update (No business calculations)
8. COMEN01C.cbl - Main Menu (No business calculations)
9. CORPT00C.cbl - Report Generation ✓ (Contains RULE-CALC-003, RULE-CALC-004)
10. COSGN00C.cbl - Sign-on Processing (No business calculations)
11. COTRN00C.cbl - Transaction List (No business calculations)
12. COTRN01C.cbl - Transaction View (No business calculations)
13. COTRN02C.cbl - Transaction Addition ✓ (Contains RULE-CALC-002)
14. COUSR00C.cbl - User List (No business calculations)
15. COUSR01C.cbl - User Addition (No business calculations)
16. COUSR02C.cbl - User Update (No business calculations)
17. COUSR03C.cbl - User Deletion (No business calculations)
18. CSUTLDTC.cbl - Date Utility (Technical utility, no business calculations)

## Modernization Recommendations

### Critical Business Rules for Migration
1. **RULE-CALC-001**: Essential for payment processing accuracy
2. **RULE-CALC-002**: Required for transaction integrity across systems
3. **RULE-CALC-003 & RULE-CALC-004**: Important for business reporting continuity

### Implementation Priority
1. **High Priority**: RULE-CALC-001, RULE-CALC-002 (Core transaction processing)
2. **Medium Priority**: RULE-CALC-003, RULE-CALC-004 (Reporting functionality)

### Dependencies to Consider
- Transaction ID generation (RULE-CALC-002) is a shared dependency used by multiple business processes
- Date calculation rules support the reporting framework and should be implemented as reusable components
- Balance calculation logic should include proper error handling and audit trails in the modernized system

---

*Analysis completed on: September 09, 2025*  
*Total COBOL lines analyzed: ~15,000+*  
*Business rules extraction focused on financial and calculation logic only*
