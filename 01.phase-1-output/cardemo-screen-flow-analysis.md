# CardDemo COBOL Application - Screen Flow Analysis

## Executive Summary

The CardDemo application is a comprehensive credit card management system built using CICS COBOL with a pseudo-conversational design pattern. The application provides role-based access control with distinct navigation paths for administrators and regular users. All screen flows follow a hierarchical structure starting from a central authentication screen and branching into functional modules based on user privileges.

### Key Architecture Features
- **Authentication-First Design**: All access begins through COSGN00C sign-on screen
- **Role-Based Navigation**: Admin users access COADM01C, regular users access COMEN01C
- **Pseudo-Conversational Pattern**: Programs use CICS XCTL for seamless navigation
- **Standardized PF Key Navigation**: Consistent function key patterns across all screens
- **BMS Screen Management**: All user interfaces use Basic Mapping Support

## Complete Screen Flow Hierarchy

```
COSGN00C (SGON) - Sign-on Authentication
├── Admin Users → COADM01C (CADM) - Administrative Menu
│   ├── User Management Module
│   │   ├── COUSR00C (CU00) - User List with pagination
│   │   ├── COUSR01C (CU01) - Add New User
│   │   ├── COUSR02C (CU02) - Update User Details
│   │   └── COUSR03C (CU03) - Delete User
│   └── All Regular User Functions (inherited access)
└── Regular Users → COMEN01C (OMEN) - Main User Menu
    ├── Account Management Module
    │   ├── COACTVWC (CAVW) - Account View/Details
    │   └── COACTUPC (CAUP) - Account Update
    ├── Credit Card Management Module
    │   ├── COCRDLIC (CCLI) - Card List with pagination
    │   ├── COCRDSLC (CCDL) - Card Details View
    │   └── COCRDUPC (CCUP) - Card Update/Maintenance
    ├── Transaction Management Module
    │   ├── COTRN00C (CT00) - Transaction List with pagination
    │   ├── COTRN01C (CT01) - Transaction Details View
    │   └── COTRN02C (CT02) - Add New Transaction
    ├── Bill Payment Module
    │   └── COBIL00C (CB00) - Online Bill Payment
    └── Reporting Module
        └── CORPT00C (CR00) - Transaction Reports
```

## Detailed Screen-by-Screen Navigation

### 1. Authentication Layer

#### COSGN00C - Sign-on Screen (Transaction: SGON)
**Purpose**: User authentication and role-based routing
**Navigation Options**:
- **ENTER**: Validate credentials and route based on user type
  - Admin users → COADM01C (Administrative Menu)
  - Regular users → COMEN01C (Main Menu)
- **PF3**: Exit application with thank you message

**Screen Flow Logic**:
```cobol
IF CDEMO-USRTYP-ADMIN
    EXEC CICS XCTL PROGRAM ('COADM01C') COMMAREA(CARDDEMO-COMMAREA)
ELSE
    EXEC CICS XCTL PROGRAM ('COMEN01C') COMMAREA(CARDDEMO-COMMAREA)
```

### 2. Menu Layer

#### COADM01C - Administrative Menu (Transaction: CADM)
**Purpose**: Central navigation hub for administrative functions
**Navigation Options**:
- **Menu Selection**: Navigate to specific administrative functions
- **PF3**: Exit to sign-on screen
- **Inherits**: All regular user menu options

#### COMEN01C - Main User Menu (Transaction: OMEN)
**Purpose**: Central navigation hub for regular user functions
**Navigation Options**:
- **Menu Selection**: Navigate to functional modules
- **PF3**: Exit to sign-on screen

### 3. User Management Module (Admin Only)

#### COUSR00C - User List (Transaction: CU00)
**Purpose**: Display paginated list of system users
**Navigation Options**:
- **Selection + 'U'**: Navigate to COUSR02C for user update
- **Selection + 'D'**: Navigate to COUSR03C for user deletion
- **PF3**: Return to COADM01C (Admin Menu)
- **PF7**: Previous page
- **PF8**: Next page
- **ENTER**: Process user selection

**Selection Processing**:
```cobol
WHEN 'U' OR 'u'
    MOVE 'COUSR02C' TO CDEMO-TO-PROGRAM
    EXEC CICS XCTL PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA)
WHEN 'D' OR 'd'
    MOVE 'COUSR03C' TO CDEMO-TO-PROGRAM
    EXEC CICS XCTL PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA)
```

#### COUSR01C - Add User (Transaction: CU01)
**Purpose**: Create new system users
**Navigation Options**:
- **ENTER**: Validate and create new user
- **PF3**: Return to COADM01C (Admin Menu)
- **PF4**: Clear current screen

#### COUSR02C - Update User (Transaction: CU02)
**Purpose**: Modify existing user details
**Navigation Options**:
- **ENTER**: Load user details for editing
- **PF3**: Save changes and return to previous screen
- **PF4**: Clear current screen
- **PF5**: Save user updates
- **PF12**: Return to COADM01C (Admin Menu)

#### COUSR03C - Delete User (Transaction: CU03)
**Purpose**: Remove users from the system
**Navigation Options**:
- **ENTER**: Load user details for confirmation
- **PF3**: Return to previous screen
- **PF4**: Clear current screen
- **PF5**: Confirm and delete user
- **PF12**: Return to COADM01C (Admin Menu)

### 4. Account Management Module

#### COACTVWC - Account View (Transaction: CAVW)
**Purpose**: Display account details and balance information
**Navigation Options**:
- **ENTER**: Process account lookup
- **PF3**: Return to COMEN01C (Main Menu)
- **PF4**: Clear current screen
- **PF5**: Navigate to account update (COACTUPC)

#### COACTUPC - Account Update (Transaction: CAUP)
**Purpose**: Modify account information and settings
**Navigation Options**:
- **ENTER**: Load account for editing
- **PF3**: Return to previous screen
- **PF4**: Clear current screen
- **PF5**: Save account changes

**State Management**:
```cobol
88 ACUP-DETAILS-NOT-FETCHED
88 ACUP-SHOW-DETAILS
88 ACUP-CHANGES-MADE
88 ACUP-CHANGES-OK-NOT-CONFIRMED
88 ACUP-CHANGES-OKAYED-AND-DONE
```

### 5. Credit Card Management Module

#### COCRDLIC - Card List (Transaction: CCLI)
**Purpose**: Display paginated list of credit cards
**Navigation Options**:
- **Selection + 'S'**: Navigate to COCRDSLC for card details
- **Selection + 'U'**: Navigate to COCRDUPC for card update
- **PF3**: Return to COMEN01C (Main Menu)
- **PF7**: Previous page
- **PF8**: Next page
- **ENTER**: Process card selection

**Card Selection Logic**:
```cobol
WHEN 'S' OR 's'
    MOVE 'COCRDSLC' TO CDEMO-TO-PROGRAM
    EXEC CICS XCTL PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA)
WHEN 'U' OR 'u'
    MOVE 'COCRDUPC' TO CDEMO-TO-PROGRAM
    EXEC CICS XCTL PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA)
```

#### COCRDSLC - Card Details (Transaction: CCDL)
**Purpose**: Display detailed credit card information
**Navigation Options**:
- **ENTER**: Process card detail request
- **PF3**: Return to previous screen
- **PF4**: Clear current screen

#### COCRDUPC - Card Update (Transaction: CCUP)
**Purpose**: Modify credit card details and status
**Navigation Options**:
- **ENTER**: Load card for editing or validate changes
- **PF3**: Return to previous screen
- **PF4**: Clear current screen
- **PF5**: Save card changes

**Update State Management**:
```cobol
88 CCUP-DETAILS-NOT-FETCHED
88 CCUP-SHOW-DETAILS
88 CCUP-CHANGES-MADE
88 CCUP-CHANGES-NOT-OK
88 CCUP-CHANGES-OK-NOT-CONFIRMED
88 CCUP-CHANGES-OKAYED-AND-DONE
```

### 6. Transaction Management Module

#### COTRN00C - Transaction List (Transaction: CT00)
**Purpose**: Display paginated list of card transactions
**Navigation Options**:
- **Selection + 'S'**: Navigate to COTRN01C for transaction details
- **PF3**: Return to COMEN01C (Main Menu)
- **PF7**: Previous page
- **PF8**: Next page
- **ENTER**: Process transaction selection

**Transaction Selection**:
```cobol
WHEN 'S' OR 's'
    MOVE 'COTRN01C' TO CDEMO-TO-PROGRAM
    EXEC CICS XCTL PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA)
```

#### COTRN01C - Transaction View (Transaction: CT01)
**Purpose**: Display detailed transaction information
**Navigation Options**:
- **ENTER**: Load transaction details
- **PF3**: Return to previous screen
- **PF4**: Clear current screen
- **PF5**: Return to COTRN00C (Transaction List)

#### COTRN02C - Add Transaction (Transaction: CT02)
**Purpose**: Create new card transactions
**Navigation Options**:
- **ENTER**: Validate and create transaction
- **PF3**: Return to previous screen
- **PF4**: Clear current screen
- **PF5**: Copy data from last transaction

### 7. Bill Payment Module

#### COBIL00C - Bill Payment (Transaction: CB00)
**Purpose**: Process online bill payments
**Navigation Options**:
- **ENTER**: Process payment request
- **PF3**: Return to COMEN01C (Main Menu)
- **PF4**: Clear current screen

**Payment Confirmation Flow**:
```cobol
EVALUATE CONFIRMI OF COBIL0AI
    WHEN 'Y' OR 'y'
        SET CONF-PAY-YES TO TRUE
        PERFORM READ-ACCTDAT-FILE
    WHEN 'N' OR 'n'
        PERFORM CLEAR-CURRENT-SCREEN
```

### 8. Reporting Module

#### CORPT00C - Transaction Reports (Transaction: CR00)
**Purpose**: Generate transaction reports via batch job submission
**Navigation Options**:
- **ENTER**: Submit report generation job
- **PF3**: Return to COMEN01C (Main Menu)

**Report Types**:
- **Monthly Reports**: Current month transaction summary
- **Yearly Reports**: Annual transaction summary
- **Custom Reports**: User-defined date range

## Standard PF Key Navigation Patterns

### Universal PF Keys (All Screens)
- **PF3**: Exit/Return to previous screen or main menu
- **PF4**: Clear current screen and reset input fields

### List Screen PF Keys (Pagination)
- **PF7**: Navigate to previous page
- **PF8**: Navigate to next page

### Update Screen PF Keys
- **PF5**: Save changes or perform special functions
- **PF12**: Return to administrative menu (admin screens only)

### Selection Options (List Screens)
- **'S' or 's'**: Select item for viewing details
- **'U' or 'u'**: Select item for updating
- **'D' or 'd'**: Select item for deletion (admin only)

## Program-to-Program XCTL Relationships

### Authentication Flow
```
COSGN00C → COADM01C (if admin user)
COSGN00C → COMEN01C (if regular user)
```

### Admin Navigation
```
COADM01C → COUSR00C → COUSR02C (update)
COADM01C → COUSR00C → COUSR03C (delete)
COADM01C → COUSR01C (add user)
```

### User Navigation
```
COMEN01C → COACTVWC → COACTUPC
COMEN01C → COCRDLIC → COCRDSLC
COMEN01C → COCRDLIC → COCRDUPC
COMEN01C → COTRN00C → COTRN01C
COMEN01C → COTRN02C
COMEN01C → COBIL00C
COMEN01C → CORPT00C
```

## Role-Based Access Control

### Administrator Privileges
- Full access to user management functions (COUSR00C, COUSR01C, COUSR02C, COUSR03C)
- Access to all regular user functions
- Special PF12 key for quick return to admin menu

### Regular User Restrictions
- No access to user management functions
- Limited to account, card, transaction, bill payment, and reporting functions
- Cannot create, update, or delete other users

## Error Handling and Validation

### Common Validation Patterns
- **Empty Field Validation**: All required fields checked for spaces or low-values
- **Numeric Field Validation**: Account IDs, card numbers validated as numeric
- **Date Format Validation**: Date fields validated using CSUTLDTC utility
- **File Access Validation**: CICS response codes checked for NORMAL, NOTFND, and error conditions

### Error Message Display
- Error messages displayed in dedicated screen areas
- Color coding used for different message types (red for errors, green for success)
- Cursor positioning to invalid fields for user correction

## Technical Implementation Notes

### Communication Area Management
- All programs use CARDDEMO-COMMAREA for state preservation
- Program-specific context areas maintain workflow state
- CDEMO-PGM-REENTER flag manages pseudo-conversational flow

### File Access Patterns
- VSAM files accessed via CICS READ, WRITE, REWRITE, DELETE
- Cross-reference files (CXACAIX) used for account-card relationships
- UPDATE option used for record locking during modifications

### Screen Management
- BMS maps used for all screen layouts
- SEND MAP and RECEIVE MAP commands for screen interaction
- ERASE option used for screen clearing
- CURSOR positioning for user guidance

This comprehensive screen flow analysis provides a complete understanding of the CardDemo application's user interface navigation patterns, enabling effective system maintenance, user training, and future enhancements.
