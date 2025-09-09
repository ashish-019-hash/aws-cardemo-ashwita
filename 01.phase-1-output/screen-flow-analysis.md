# CardDemo COBOL Application Screen Flow Analysis

## Executive Summary

The CardDemo application is a comprehensive credit card management system built using CICS COBOL with a pseudo-conversational design pattern. The application provides role-based navigation flows supporting both regular users and administrators through a hierarchical menu structure with standardized PF key navigation patterns.

### Key Navigation Characteristics
- **Entry Point**: COSGN00C (Sign-on Screen) with role-based authentication
- **Role-Based Routing**: Automatic redirection to appropriate menu based on user type
- **Consistent PF Key Patterns**: PF3 (Exit), PF4 (Clear), PF5 (Action), PF7/PF8 (Pagination)
- **Program Transfer Mechanism**: EXEC CICS XCTL for seamless screen transitions
- **Selection-Based Navigation**: 'S' (Select/View), 'U' (Update), 'D' (Delete) operations

## Complete Screen Flow Mappings

### Authentication & Main Navigation

| Screen ID | Screen Title | Trigger/Option | Target Screen | Notes |
|-----------|--------------|----------------|---------------|-------|
| COSGN0A | Sign-on Screen | Valid User Credentials | COMEN1A or COADM1A | Role-based routing |
| COSGN0A | Sign-on Screen | Invalid Credentials | COSGN0A | Error message displayed |
| COSGN0A | Sign-on Screen | PF3 | Application Exit | Session termination |
| COMEN1A | Main Menu (Regular Users) | Option 1 | CACTVWA | Account View |
| COMEN1A | Main Menu (Regular Users) | Option 2 | CCRDLIA | Credit Card List |
| COMEN1A | Main Menu (Regular Users) | Option 3 | COBIL0A | Bill Payment |
| COMEN1A | Main Menu (Regular Users) | Option 4 | COTRN0A | Transaction List |
| COMEN1A | Main Menu (Regular Users) | Option 5 | CORPT0A | Transaction Reports |
| COMEN1A | Main Menu (Regular Users) | PF3 | COSGN0A | Return to Sign-on |
| COADM1A | Admin Menu | Option 1 | COUSR0A | User List |
| COADM1A | Admin Menu | Option 2 | CACTVWA | Account View |
| COADM1A | Admin Menu | Option 3 | CCRDLIA | Credit Card List |
| COADM1A | Admin Menu | Option 4 | COBIL0A | Bill Payment |
| COADM1A | Admin Menu | Option 5 | COTRN0A | Transaction List |
| COADM1A | Admin Menu | Option 6 | CORPT0A | Transaction Reports |
| COADM1A | Admin Menu | PF3 | COSGN0A | Return to Sign-on |

### User Management (Admin Only)

| Screen ID | Screen Title | Trigger/Option | Target Screen | Notes |
|-----------|--------------|----------------|---------------|-------|
| COUSR0A | User List | Selection 'U' + User ID | COUSR2A | User Update |
| COUSR0A | User List | Selection 'D' + User ID | COUSR3A | User Delete |
| COUSR0A | User List | PF3 | COADM1A | Return to Admin Menu |
| COUSR0A | User List | PF7 | COUSR0A | Previous Page |
| COUSR0A | User List | PF8 | COUSR0A | Next Page |
| COUSR1A | User Add | ENTER (Valid Data) | COUSR1A | Success message |
| COUSR1A | User Add | ENTER (Invalid Data) | COUSR1A | Error message |
| COUSR1A | User Add | PF3 | COADM1A | Return to Admin Menu |
| COUSR1A | User Add | PF4 | COUSR1A | Clear Screen |
| COUSR2A | User Update | ENTER | COUSR2A | Fetch user data |
| COUSR2A | User Update | PF3 | COUSR0A | Return to User List |
| COUSR2A | User Update | PF4 | COUSR2A | Clear Screen |
| COUSR2A | User Update | PF5 | COUSR2A | Save Changes |
| COUSR2A | User Update | PF12 | COADM1A | Return to Admin Menu |
| COUSR3A | User Delete | ENTER | COUSR3A | Fetch user data |
| COUSR3A | User Delete | PF3 | COUSR0A | Return to User List |
| COUSR3A | User Delete | PF4 | COUSR3A | Clear Screen |
| COUSR3A | User Delete | PF5 | COUSR3A | Confirm Delete |
| COUSR3A | User Delete | PF12 | COADM1A | Return to Admin Menu |

### Transaction Management

| Screen ID | Screen Title | Trigger/Option | Target Screen | Notes |
|-----------|--------------|----------------|---------------|-------|
| COTRN0A | Transaction List | Selection + Transaction ID | COTRN1A | Transaction Detail |
| COTRN0A | Transaction List | PF3 | COMEN1A or COADM1A | Return to Menu |
| COTRN0A | Transaction List | PF5 | COTRN2A | Add New Transaction |
| COTRN0A | Transaction List | PF7 | COTRN0A | Previous Page |
| COTRN0A | Transaction List | PF8 | COTRN0A | Next Page |
| COTRN1A | Transaction Detail | ENTER | COTRN1A | Display transaction |
| COTRN1A | Transaction Detail | PF3 | COMEN1A or COADM1A | Return to Menu |
| COTRN1A | Transaction Detail | PF4 | COTRN1A | Clear Screen |
| COTRN1A | Transaction Detail | PF5 | COTRN0A | Return to List |
| COTRN2A | Add Transaction | ENTER (Valid Data) | COTRN2A | Success message |
| COTRN2A | Add Transaction | ENTER (Invalid Data) | COTRN2A | Error message |
| COTRN2A | Add Transaction | PF3 | COMEN1A or COADM1A | Return to Menu |
| COTRN2A | Add Transaction | PF4 | COTRN2A | Clear Screen |
| COTRN2A | Add Transaction | PF5 | COTRN0A | Return to List |

### Account Management

| Screen ID | Screen Title | Trigger/Option | Target Screen | Notes |
|-----------|--------------|----------------|---------------|-------|
| CACTVWA | Account View | ENTER (Account ID) | CACTVWA | Display account details |
| CACTVWA | Account View | PF3 | COMEN1A or COADM1A | Return to Menu |
| CACTUPC | Account Update | ENTER (Account ID) | CACTUPC | Fetch account data |
| CACTUPC | Account Update | PF3 | COMEN1A or COADM1A | Return to Menu |
| CACTUPC | Account Update | PF4 | CACTUPC | Clear Screen |
| CACTUPC | Account Update | PF5 | CACTUPC | Save Changes |

### Credit Card Management

| Screen ID | Screen Title | Trigger/Option | Target Screen | Notes |
|-----------|--------------|----------------|---------------|-------|
| CCRDLIA | Credit Card List | Selection 'S' + Card | CCRDSLA | Card Detail View |
| CCRDLIA | Credit Card List | Selection 'U' + Card | CCRDUPA | Card Update |
| CCRDLIA | Credit Card List | PF3 | COMEN1A or COADM1A | Return to Menu |
| CCRDLIA | Credit Card List | PF7 | CCRDLIA | Previous Page |
| CCRDLIA | Credit Card List | PF8 | CCRDLIA | Next Page |
| CCRDSLA | Credit Card Detail | ENTER | CCRDSLA | Display card details |
| CCRDSLA | Credit Card Detail | PF3 | CCRDLIA | Return to List |
| CCRDUPA | Credit Card Update | ENTER | CCRDUPA | Fetch card data |
| CCRDUPA | Credit Card Update | PF3 | CCRDLIA | Return to List |
| CCRDUPA | Credit Card Update | PF4 | CCRDUPA | Clear Screen |
| CCRDUPA | Credit Card Update | PF5 | CCRDUPA | Save Changes |

### Bill Payment

| Screen ID | Screen Title | Trigger/Option | Target Screen | Notes |
|-----------|--------------|----------------|---------------|-------|
| COBIL0A | Bill Payment | ENTER (Account ID) | COBIL0A | Display balance |
| COBIL0A | Bill Payment | ENTER + Confirm 'Y' | COBIL0A | Process payment |
| COBIL0A | Bill Payment | ENTER + Confirm 'N' | COBIL0A | Clear screen |
| COBIL0A | Bill Payment | PF3 | COMEN1A or COADM1A | Return to Menu |
| COBIL0A | Bill Payment | PF4 | COBIL0A | Clear Screen |

### Reporting

| Screen ID | Screen Title | Trigger/Option | Target Screen | Notes |
|-----------|--------------|----------------|---------------|-------|
| CORPT0A | Transaction Reports | Monthly Report | CORPT0A | Submit batch job |
| CORPT0A | Transaction Reports | Yearly Report | CORPT0A | Submit batch job |
| CORPT0A | Transaction Reports | Custom Report | CORPT0A | Submit batch job |
| CORPT0A | Transaction Reports | PF3 | COMEN1A or COADM1A | Return to Menu |

## BMS Map and Transaction ID Reference

### Screen Identifiers
| Program | Transaction ID | BMS Mapset | BMS Map | Screen Purpose |
|---------|----------------|------------|---------|----------------|
| COSGN00C | CSSN | COSGN00 | COSGN0A | Sign-on Screen |
| COMEN01C | CM00 | COMEN01 | COMEN1A | Main Menu (Regular Users) |
| COADM01C | CA00 | COADM01 | COADM1A | Admin Menu |
| COUSR00C | CU00 | COUSR00 | COUSR0A | User List |
| COUSR01C | CU01 | COUSR01 | COUSR1A | User Add |
| COUSR02C | CU02 | COUSR02 | COUSR2A | User Update |
| COUSR03C | CU03 | COUSR03 | COUSR3A | User Delete |
| COTRN00C | CT00 | COTRN00 | COTRN0A | Transaction List |
| COTRN01C | CT01 | COTRN01 | COTRN1A | Transaction Detail |
| COTRN02C | CT02 | COTRN02 | COTRN2A | Add Transaction |
| COACTVWC | CAVW | COACTVW | CACTVWA | Account View |
| COACTUPC | CAUP | COACTUPC | CACTUPC | Account Update |
| COCRDLIC | CCLI | COCRDLI | CCRDLIA | Credit Card List |
| COCRDSLC | CCDL | COCRDSL | CCRDSLA | Credit Card Detail |
| COCRDUPC | CCUP | COCRDUP | CCRDUPA | Credit Card Update |
| COBIL00C | CB00 | COBIL00 | COBIL0A | Bill Payment |
| CORPT00C | CR00 | CORPT00 | CORPT0A | Transaction Reports |

## Navigation Patterns and Business Rules

### Role-Based Access Control
- **Regular Users**: Access to account view, card management, bill payment, transactions, and reports
- **Admin Users**: Full access including user management (CRUD operations)
- **Authentication**: Centralized through COSGN00C with automatic role-based routing

### Standard PF Key Functions
- **PF3**: Exit/Return to previous screen or main menu
- **PF4**: Clear current screen fields
- **PF5**: Execute primary action (save, add transaction, etc.)
- **PF7**: Previous page (pagination)
- **PF8**: Next page (pagination)
- **PF12**: Return to main menu (bypass intermediate screens)

### Selection Mechanisms
- **List Screens**: Use selection codes ('S', 'U', 'D') with entity identifiers
- **Detail Screens**: Direct ENTER key processing with validation
- **Update Screens**: Multi-step workflow (fetch → modify → confirm → save)

### Error Handling
- **Field Validation**: Real-time validation with cursor positioning
- **Business Rules**: Account balance checks, user existence validation
- **User Feedback**: Color-coded messages (green for success, red for errors)

### State Management
- **Communication Areas**: CARDDEMO-COMMAREA maintains session context
- **Program Context**: Tracks calling program for proper return navigation
- **Pagination State**: Maintains current page and navigation keys for list screens

## Technical Implementation Notes

### Program Transfer Mechanism
All screen transitions use `EXEC CICS XCTL` commands with communication area passing:
```cobol
EXEC CICS XCTL
    PROGRAM(CDEMO-TO-PROGRAM)
    COMMAREA(CARDDEMO-COMMAREA)
END-EXEC
```

### Screen Handling Pattern
Standard BMS screen interaction follows the pattern:
```cobol
EXEC CICS SEND MAP('mapname') MAPSET('mapset') FROM(output-area)
EXEC CICS RECEIVE MAP('mapname') MAPSET('mapset') INTO(input-area)
```

### Pseudo-Conversational Design
Programs return control to CICS between user interactions, maintaining state through communication areas and re-entering on subsequent user actions.

This analysis covers all 18 COBOL programs in the CardDemo application, focusing exclusively on user-facing navigation flows and screen transitions that drive the business-level user experience.
