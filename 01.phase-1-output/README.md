# COBOL Validation Rules Extraction - Phase 1 Output

## Overview
This folder contains the extracted business validation rules from the CardDemo COBOL application, organized for modernization to frontend and backend validation layers.

## Files

### validation-rules.md
Comprehensive documentation of all 60 business validation rules extracted from 18 COBOL programs, organized by business domain:
- Authentication (3 rules)
- Account Management (4 rules) 
- Credit Card (6 rules)
- Transaction (15 rules)
- User Management (12 rules)
- Bill Payment (3 rules)
- Reporting (12 rules)
- Menu/Navigation (3 rules)
- Date Utility (2 rules)

Each rule includes:
- Unique Rule ID (RULE-VAL-001 through RULE-VAL-060)
- Business-readable description
- COBOL source location (program name, line numbers, paragraph)
- Field(s) involved
- Validation condition in plain English
- Trigger conditions (when the validation is applied)

### validation-relationships.svg
Visual diagram showing:
- Business domain groupings
- Validation rule relationships and dependencies
- Cross-domain data flows
- Shared utility dependencies
- Modernization considerations

## Extraction Criteria

### Included Validations
- Field-level input validations (required fields, format checks)
- Range validations (dates, numeric ranges)
- Code/value checks (status codes, selection values)
- Domain-specific validations (business rules)
- Conditional validations (dependent field requirements)

### Excluded Items
- Calculation logic (interest computation, amount derivation)
- Technical validations (system errors, database exceptions)
- BMS layout constraints (screen formatting)
- Display-only checks (presentation logic)

## Source Analysis
All 18 COBOL programs in the 00.phase-1-input folder were analyzed:
- COSGN00C.cbl - Sign-on authentication
- COACTUPC.cbl - Account updates
- COACTVWC.cbl - Account view
- COCRDUPC.cbl - Credit card updates
- COCRDLIC.cbl - Credit card listing
- COCRDSLC.cbl - Credit card selection
- COTRN00C.cbl - Transaction listing
- COTRN01C.cbl - Transaction view
- COTRN02C.cbl - Transaction creation
- COUSR00C.cbl - User listing
- COUSR01C.cbl - User creation
- COUSR02C.cbl - User updates
- COUSR03C.cbl - User deletion
- COBIL00C.cbl - Bill payment
- CORPT00C.cbl - Report generation
- COMEN01C.cbl - Main menu
- COADM01C.cbl - Admin menu
- CSUTLDTC.cbl - Date utility

## Usage for Modernization
These validation rules serve as the specification for implementing:
1. **Frontend Validation**: Client-side validation for immediate user feedback
2. **Backend API Validation**: Server-side validation for data integrity
3. **Shared Validation Libraries**: Reusable validation components
4. **Error Message Standardization**: Consistent validation messaging

The rules maintain the original business logic while being technology-agnostic for implementation in modern frameworks.
