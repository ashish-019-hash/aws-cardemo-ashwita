# CardDemo COBOL Application Screen Flow Analysis

## Executive Summary

The CardDemo application is a comprehensive credit card management system built as a CICS (Customer Information Control System) application using COBOL. This document provides a detailed analysis of the business-level screen flows extracted from 18 COBOL programs, focusing on user navigation paths, screen transitions, and menu selections that drive the production user experience.

The application follows a pseudo-conversational CICS design pattern with role-based authentication, hierarchical menu structures, and comprehensive transaction processing capabilities. Navigation is primarily driven by CICS XCTL commands for program-to-program transfers and EIBAID evaluations for PF key handling.

## Application Architecture Overview

### Core Design Patterns
- **Pseudo-Conversational Flow**: Programs send screens to users, return control to CICS, and are re-invoked when users respond
- **State Management**: Context preserved through CARDDEMO-COMMAREA communication area
- **Role-Based Access**: Separate navigation paths for regular users and administrators
- **Program-to-Program Communication**: CICS XCTL commands for seamless navigation
- **BMS Integration**: Basic Mapping Support for screen interaction and data presentation

### User Types and Access Patterns
- **Regular Users**: Access to account management, bill payment, credit card operations, and transaction viewing
- **Administrative Users**: Full user management capabilities plus reporting functions
- **Authentication**: Centralized sign-on with role-based routing to appropriate menu systems

## Complete Screen Flow Hierarchy

### 1. Authentication Entry Point

#### COSGN00C - Sign-on Screen (SGON)
**Function**: User authentication and role-based routing  
**Navigation**:
- **ENTER**: Validates credentials and routes based on user type
  - Admin users → COADM01C (Admin Menu)
  - Regular users → COMEN01C (Main Menu)
- **PF3**: Exit application
- **Error Handling**: Invalid credentials display error messages

---

### 2. Administrative Menu System

#### COADM01C - Administrative Menu (CA00)
**Function**: Central hub for administrative operations  
**Menu Options**:

| Option | Description | Target Program | Transaction Code |
|--------|-------------|----------------|------------------|
| 1 | User List and Management | COUSR00C | CU00 |
| 2 | *Reserved* | - | - |
| 3 | *Reserved* | - | - |
| 4 | Transaction Reports | CORPT00C | CR00 |

**PF Key Navigation**:
- **PF3**: Return to sign-on screen
- **PF12**: Return to sign-on screen

#### COUSR00C - User List Screen (CU00)
**Function**: Display paginated list of all users from USRSEC file  
**Navigation**:
- **ENTER**: Process user selection
- **Selection Actions**:
  - **S** (Select) + User ID → COUSR02C (User Update)
  - **D** (Delete) + User ID → COUSR03C (User Delete)
- **PF3**: Return to Admin Menu (COADM01C)
- **PF7**: Page up through user list
- **PF8**: Page down through user list
- **PF12**: Return to Admin Menu

#### COUSR01C - User Add Screen (CU01)
**Function**: Create new regular or admin users  
**Navigation**:
- **ENTER**: Validate and create new user
- **PF3**: Return to Admin Menu (COADM01C)
- **PF4**: Clear current screen
- **Validation**: All fields mandatory, user ID uniqueness enforced

#### COUSR02C - User Update Screen (CU02)
**Function**: Modify existing user information  
**Navigation**:
- **ENTER**: Load user data for editing
- **PF3**: Save changes and return to previous screen
- **PF4**: Clear current screen
- **PF5**: Save user modifications
- **PF12**: Return to Admin Menu (COADM01C)

#### COUSR03C - User Delete Screen (CU03)
**Function**: Remove users from system  
**Navigation**:
- **ENTER**: Load user data for confirmation
- **PF3**: Return to previous screen
- **PF4**: Clear current screen
- **PF5**: Confirm and delete user
- **PF12**: Return to Admin Menu (COADM01C)

#### CORPT00C - Transaction Reports (CR00)
**Function**: Generate transaction reports via batch job submission  
**Report Types**:
- **Monthly Reports**: Current month transaction summary
- **Yearly Reports**: Annual transaction summary
- **Custom Date Range**: User-specified date parameters
**Navigation**:
- **ENTER**: Submit batch job for report generation
- **PF3**: Return to Main Menu (COMEN01C)
- **Validation**: Date format validation via CSUTLDTC utility

---

### 3. Regular User Menu System

#### COMEN01C - Main Menu (CM00)
**Function**: Central navigation hub for regular users  
**Menu Options**:

| Option | Description | Target Program | Transaction Code |
|--------|-------------|----------------|------------------|
| 1 | View Account Details | COACTVWC | CAVW |
| 2 | Bill Payment | COBIL00C | CB00 |
| 3 | Credit Card Services | COCRDLIC | CCLI |
| 4 | Transaction History | COTRN00C | CT00 |
| 5 | *Reserved* | - | - |
| 6 | *Reserved* | - | - |
| 7 | *Reserved* | - | - |
| 8 | *Reserved* | - | - |
| 9 | *Reserved* | - | - |
| 10 | *Reserved* | - | - |

**PF Key Navigation**:
- **PF3**: Return to sign-on screen
- **PF12**: Return to sign-on screen

---

### 4. Account Management Flow

#### COACTVWC - Account View Screen (CAVW)
**Function**: Display comprehensive account information  
**Data Displayed**:
- Account details from ACCTDAT file
- Customer information from CUSTDAT file
- Cross-reference data from CXACAIX file
**Navigation**:
- **ENTER**: Refresh account data display
- **PF3**: Return to calling program or Main Menu
- **Account Selection**: Navigate to COACTUPC for updates

#### COACTUPC - Account Update Screen (CAUP)
**Function**: Modify account information with comprehensive validation  
**Editable Fields**:
- Account status (Y/N)
- Credit limits and balances
- Customer demographic information
- Account dates and FICO scores
**Navigation**:
- **ENTER**: Validate input and process changes
- **PF3**: Return to previous screen
- **PF5**: Save account modifications
- **Validation**: Multi-layered field validation including date validation via CSUTLDTC

---

### 5. Bill Payment Flow

#### COBIL00C - Bill Payment Screen (CB00)
**Function**: Process online bill payments for account balances  
**Process Flow**:
1. Enter account ID
2. Display current balance
3. Confirm payment (Y/N)
4. Generate payment transaction
5. Update account balance
**Navigation**:
- **ENTER**: Process payment request
- **PF3**: Return to Main Menu (COMEN01C)
- **PF4**: Clear current screen
- **Validation**: Account existence, positive balance verification

---

### 6. Credit Card Management Flow

#### COCRDLIC - Credit Card List Screen (CCLI)
**Function**: Display paginated list of credit cards  
**Display Options**:
- All cards (admin users)
- Account-specific cards (regular users)
**Navigation**:
- **ENTER**: Process card selection
- **Selection Actions**:
  - **S** (Select) + Card → COCRDSLC (Card Detail)
  - **U** (Update) + Card → COCRDUPC (Card Update)
- **PF3**: Return to Main Menu (COMEN01C)
- **PF7**: Page up through card list
- **PF8**: Page down through card list

#### COCRDSLC - Credit Card Detail Screen (CCDL)
**Function**: Display comprehensive credit card information  
**Data Sources**:
- Card details from CARDDAT file
- Account cross-references
- Customer information
**Navigation**:
- **ENTER**: Refresh card data display
- **PF3**: Return to calling program (COCRDLIC or Main Menu)

#### COCRDUPC - Credit Card Update Screen (CCUP)
**Function**: Modify credit card information with validation  
**Editable Fields**:
- Card name embossed
- Card status (Y/N)
- Expiration date (month/year validation)
**Update Workflow**:
1. **Details Not Fetched** → **Show Details** → **Changes Made** → **Validation** → **Confirmation** → **Update Complete**
**Navigation**:
- **ENTER**: Process card modifications
- **PF3**: Return to previous screen
- **PF5**: Save card changes
- **Validation**: Card name alphabetic validation, date validation, status validation

---

### 7. Transaction Management Flow

#### COTRN00C - Transaction List Screen (CT00)
**Function**: Display paginated transaction history from TRANSACT file  
**Features**:
- Chronological transaction listing
- Pagination support with PF7/PF8
- Transaction selection for detailed view
**Navigation**:
- **ENTER**: Process transaction selection
- **Selection**: Transaction ID → COTRN01C (Transaction Detail)
- **PF3**: Return to Main Menu (COMEN01C)
- **PF5**: Navigate to COTRN00C (refresh)
- **PF7**: Page up through transactions
- **PF8**: Page down through transactions

#### COTRN01C - Transaction Detail Screen (CT01)
**Function**: Display comprehensive transaction information  
**Data Displayed**:
- Complete transaction record details
- Merchant information
- Transaction amounts and dates
**Navigation**:
- **ENTER**: Refresh transaction display
- **PF3**: Return to previous screen
- **PF5**: Navigate to COTRN00C (Transaction List)

#### COTRN02C - Transaction Add Screen (CT02)
**Function**: Create new transactions with comprehensive validation  
**Required Fields**:
- Account ID or Card Number (cross-validated)
- Transaction type and category codes
- Amount (format: ±99999999.99)
- Description and merchant information
- Origin and processing dates (YYYY-MM-DD format)
**Validation Process**:
1. Key field validation (Account/Card cross-reference)
2. Data field validation (all fields mandatory)
3. Format validation (amounts, dates)
4. Confirmation requirement (Y/N)
**Navigation**:
- **ENTER**: Process transaction creation
- **PF3**: Return to Main Menu (COMEN01C)
- **PF4**: Clear current screen
- **PF5**: Copy data from last transaction

---

### 8. Utility Programs

#### CSUTLDTC - Date Validation Utility
**Function**: Centralized date validation service using CEEDAYS API  
**Usage**: Called by multiple programs for date format and validity checking  
**Validation Types**:
- Date format compliance (YYYY-MM-DD)
- Date value validity
- Era and range validation
**Return Codes**: Severity codes indicating validation results

---

## PF Key Navigation Standards

### Universal PF Key Functions
| PF Key | Function | Usage Context |
|--------|----------|---------------|
| **PF3** | Exit/Return | Return to previous screen or main menu |
| **PF4** | Clear | Clear current screen fields |
| **PF5** | Save/Action | Save changes, copy data, or perform primary action |
| **PF7** | Page Up | Navigate backward through paginated lists |
| **PF8** | Page Down | Navigate forward through paginated lists |
| **PF12** | Cancel | Return to main menu or cancel operation |
| **ENTER** | Process | Submit data, validate input, or navigate |

### Selection Actions in List Screens
- **S** (Select): Choose item for viewing
- **U** (Update): Choose item for modification
- **D** (Delete): Choose item for deletion (admin only)

---

## Screen Transition Patterns

### Authentication Flow
```
COSGN00C (Sign-on) 
    ├── Admin User → COADM01C (Admin Menu)
    └── Regular User → COMEN01C (Main Menu)
```

### Administrative Flow
```
COADM01C (Admin Menu)
    ├── Option 1 → COUSR00C (User List)
    │   ├── Select → COUSR02C (User Update)
    │   ├── Delete → COUSR03C (User Delete)
    │   └── Add → COUSR01C (User Add)
    └── Option 4 → CORPT00C (Reports)
```

### User Flow - Account Management
```
COMEN01C (Main Menu)
    └── Option 1 → COACTVWC (Account View)
        └── Update → COACTUPC (Account Update)
```

### User Flow - Credit Card Management
```
COMEN01C (Main Menu)
    └── Option 3 → COCRDLIC (Card List)
        ├── Select → COCRDSLC (Card Detail)
        └── Update → COCRDUPC (Card Update)
```

### User Flow - Transaction Management
```
COMEN01C (Main Menu)
    └── Option 4 → COTRN00C (Transaction List)
        ├── Select → COTRN01C (Transaction Detail)
        └── Add → COTRN02C (Transaction Add)
```

### User Flow - Bill Payment
```
COMEN01C (Main Menu)
    └── Option 2 → COBIL00C (Bill Payment)
```

---

## Error Handling and Validation Patterns

### Common Validation Patterns
1. **Mandatory Field Validation**: All required fields checked for presence
2. **Format Validation**: Numeric fields, date formats, phone numbers, SSN
3. **Cross-Reference Validation**: Account/Card relationships verified
4. **Business Rule Validation**: Credit limits, balances, status codes
5. **Database Validation**: Record existence and integrity checks

### Error Message Display
- Error messages displayed in dedicated screen areas
- Color coding for different message types (error, warning, success)
- Cursor positioning to error fields for user convenience
- Comprehensive error descriptions for user guidance

### State Management
- **CARDDEMO-COMMAREA**: Primary communication area for inter-program data
- **Program-Specific Context**: Each program maintains operational state
- **Navigation Context**: Previous program information for return navigation
- **User Session Data**: Authentication and authorization information

---

## Technical Implementation Notes

### CICS Commands Used
- **XCTL**: Program-to-program transfer with communication area
- **SEND MAP**: Display formatted screens to terminal
- **RECEIVE MAP**: Capture user input from terminal
- **READ/WRITE/REWRITE**: File operations for data persistence
- **STARTBR/READNEXT/ENDBR**: File browsing for pagination

### File Access Patterns
- **ACCTDAT**: Account master file operations
- **CUSTDAT**: Customer information management
- **CARDDAT**: Credit card data operations
- **TRANSACT**: Transaction history and creation
- **USRSEC**: User security and authentication
- **CXACAIX**: Cross-reference index for navigation

### Communication Area Structure
- **CDEMO-TO-PROGRAM**: Target program for navigation
- **CDEMO-FROM-PROGRAM**: Source program for return navigation
- **CDEMO-USER-ID**: Current user identification
- **CDEMO-USER-TYPE**: User role (admin/regular)
- **Program-Specific Sections**: Context data for each functional area

---

## Conclusion

The CardDemo COBOL application demonstrates a well-structured, hierarchical screen flow design that effectively separates administrative and user functions while maintaining consistent navigation patterns throughout the system. The pseudo-conversational CICS architecture ensures efficient resource utilization while providing a responsive user experience.

Key strengths of the screen flow design include:
- Clear role-based access control
- Consistent PF key navigation standards
- Comprehensive validation and error handling
- Efficient pagination for large data sets
- Logical grouping of related functions
- Robust state management across program boundaries

This analysis covers all 18 COBOL programs in the input folder, focusing exclusively on production navigation paths and business-level screen transitions that drive the user experience in the CardDemo credit card management system.
