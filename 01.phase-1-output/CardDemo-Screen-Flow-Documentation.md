# CardDemo Application - Screen Flow Analysis

## Overview

This document provides a comprehensive analysis of business-level screen navigation flows in the CardDemo CICS application. The analysis covers all 18 COBOL programs in the system, focusing on user-facing navigation paths, screen transitions, and menu selections that represent real-time production workflows.

## Screen Flow Summary

The CardDemo application follows a hierarchical navigation structure starting from user authentication and branching into role-based menu systems. The application supports two user types:

- **Admin Users**: Access to all functions including user management
- **Regular Users**: Access to account, card, transaction, and bill payment functions

## Authentication Flow

### Sign-on Screen (COSGN00C)
**Screen ID**: COSGN00C  
**Transaction ID**: CC00  
**Function**: User authentication and routing

| Trigger/Condition | Action | Navigates To |
|------------------|--------|--------------|
| Valid Admin User Credentials | EXEC CICS XCTL | → COADM01C (Admin Menu) |
| Valid Regular User Credentials | EXEC CICS XCTL | → COMEN01C (Main Menu) |
| Invalid Credentials | Display Error | → COSGN00C (Stay on sign-on) |
| PF3 | Exit Application | → Application Termination |

## Menu Systems

### Admin Menu (COADM01C)
**Screen ID**: COADM01C  
**Transaction ID**: CA00  
**Function**: Administrative menu for admin users

| Option | Description | Navigates To |
|--------|-------------|--------------|
| Menu Option 1-10 | Dynamic option processing | → CDEMO-ADMIN-OPT-PGMNAME(option) |
| PF3 | Return to Sign-on | → COSGN00C |
| Invalid Option | Error Message | → COADM01C (Redisplay) |

### Main Menu (COMEN01C)
**Screen ID**: COMEN01C  
**Transaction ID**: CM00  
**Function**: Main menu for regular users

| Option | Description | Navigates To |
|--------|-------------|--------------|
| Menu Option 1-12 | Dynamic option processing | → CDEMO-MENU-OPT-PGMNAME(option) |
| Admin-Only Option (Regular User) | Access Denied | → COMEN01C (Error message) |
| PF3 | Return to Sign-on | → COSGN00C |
| Invalid Option | Error Message | → COMEN01C (Redisplay) |

## Business Function Flows

### Account Management

#### Account View (COACTVWC)
**Screen ID**: COACTVWC  
**Transaction ID**: CAVW  
**Function**: Display account details

| Trigger | Action | Navigates To |
|---------|--------|--------------|
| PF3 | Return to calling program | → CDEMO-FROM-PROGRAM or COMEN01C |
| ENTER | Process account lookup | → COACTVWC (Redisplay with data) |

#### Account Update (COACTUPC)
**Screen ID**: COACTUPC  
**Transaction ID**: CAUP  
**Function**: Update account information

| Trigger | Action | Navigates To |
|---------|--------|--------------|
| PF3 | Return to previous screen | → CDEMO-TO-PROGRAM |
| ENTER | Process account update | → COACTUPC (Confirmation/Error) |
| Update Confirmation | EXEC CICS XCTL | → CDEMO-TO-PROGRAM |

### Card Management

#### Card List (COCRDLIC)
**Screen ID**: COCRDLIC  
**Transaction ID**: CCLI  
**Function**: List credit cards with pagination

| Trigger | Action | Navigates To |
|---------|--------|--------------|
| PF3 | Return to menu | → LIT-MENUPGM (COMEN01C) |
| PF7 | Page up | → COCRDLIC (Previous page) |
| PF8 | Page down | → COCRDLIC (Next page) |
| Selection 'S' | View card details | → COCRDSLC (Card Detail) |
| Selection 'U' | Update card | → COCRDUPC (Card Update) |
| ENTER | Process selection | → Target program based on selection |

#### Card Detail (COCRDSLC)
**Screen ID**: COCRDSLC  
**Transaction ID**: CCDL  
**Function**: Display card details

| Trigger | Action | Navigates To |
|---------|--------|--------------|
| PF3 | Return to previous screen | → CDEMO-TO-PROGRAM |
| ENTER | Refresh display | → COCRDSLC (Redisplay) |

#### Card Update (COCRDUPC)
**Screen ID**: COCRDUPC  
**Transaction ID**: CCUP  
**Function**: Update card information

| Trigger | Action | Navigates To |
|---------|--------|--------------|
| PF3 | Return to previous screen | → CDEMO-TO-PROGRAM |
| ENTER | Process card update | → COCRDUPC (Confirmation/Error) |
| Update Confirmation | EXEC CICS XCTL | → CDEMO-TO-PROGRAM |

### Transaction Management

#### Transaction List (COTRN00C)
**Screen ID**: COTRN00C  
**Transaction ID**: CT00  
**Function**: List transactions with pagination

| Trigger | Action | Navigates To |
|---------|--------|--------------|
| PF3 | Return to main menu | → COMEN01C |
| PF7 | Page up | → COTRN00C (Previous page) |
| PF8 | Page down | → COTRN00C (Next page) |
| Selection 'S' | View transaction details | → COTRN01C (Transaction View) |
| ENTER | Process selection/filter | → COTRN00C (Filtered results) |

#### Transaction View (COTRN01C)
**Screen ID**: COTRN01C  
**Transaction ID**: CT01  
**Function**: Display transaction details

| Trigger | Action | Navigates To |
|---------|--------|--------------|
| PF3 | Return to previous screen | → CDEMO-FROM-PROGRAM or COMEN01C |
| PF4 | Clear screen | → COTRN01C (Cleared) |
| PF5 | Return to transaction list | → COTRN00C |
| ENTER | Refresh display | → COTRN01C (Redisplay) |

#### Add Transaction (COTRN02C)
**Screen ID**: COTRN02C  
**Transaction ID**: CT02  
**Function**: Add new transaction

| Trigger | Action | Navigates To |
|---------|--------|--------------|
| PF3 | Return to previous screen | → CDEMO-FROM-PROGRAM or COMEN01C |
| PF4 | Clear screen | → COTRN02C (Cleared) |
| PF5 | Return to transaction list | → COTRN00C |
| ENTER | Process new transaction | → COTRN02C (Confirmation/Error) |

### User Administration (Admin Only)

#### User List (COUSR00C)
**Screen ID**: COUSR00C  
**Transaction ID**: CU00  
**Function**: List users with pagination

| Trigger | Action | Navigates To |
|---------|--------|--------------|
| PF3 | Return to admin menu | → COADM01C |
| PF7 | Page up | → COUSR00C (Previous page) |
| PF8 | Page down | → COUSR00C (Next page) |
| Selection 'S' | View user details | → COUSR02C (User Update) |
| Selection 'U' | Update user | → COUSR02C (User Update) |
| ENTER | Process selection | → Target program based on selection |

#### Add User (COUSR01C)
**Screen ID**: COUSR01C  
**Transaction ID**: CU01  
**Function**: Add new user

| Trigger | Action | Navigates To |
|---------|--------|--------------|
| PF3 | Return to admin menu | → COADM01C |
| PF4 | Clear screen | → COUSR01C (Cleared) |
| ENTER | Process new user | → COUSR01C (Confirmation/Error) |

#### Update User (COUSR02C)
**Screen ID**: COUSR02C  
**Transaction ID**: CU02  
**Function**: Update user information

| Trigger | Action | Navigates To |
|---------|--------|--------------|
| PF3 | Return to previous screen | → CDEMO-FROM-PROGRAM or COADM01C |
| PF4 | Clear screen | → COUSR02C (Cleared) |
| PF5 | Return to user list | → COUSR00C |
| PF12 | Return to admin menu | → COADM01C |
| ENTER | Process user update | → COUSR02C (Confirmation/Error) |

#### Delete User (COUSR03C)
**Screen ID**: COUSR03C  
**Transaction ID**: CU03  
**Function**: Delete user

| Trigger | Action | Navigates To |
|---------|--------|--------------|
| PF3 | Return to previous screen | → CDEMO-FROM-PROGRAM or COADM01C |
| PF4 | Clear screen | → COUSR03C (Cleared) |
| PF5 | Return to user list | → COUSR00C |
| ENTER | Process user deletion | → COUSR03C (Confirmation/Error) |

### Bill Payment and Reporting

#### Bill Payment (COBIL00C)
**Screen ID**: COBIL00C  
**Transaction ID**: CB00  
**Function**: Process bill payments

| Trigger | Action | Navigates To |
|---------|--------|--------------|
| PF3 | Return to previous screen | → CDEMO-FROM-PROGRAM or COMEN01C |
| PF4 | Clear screen | → COBIL00C (Cleared) |
| ENTER | Process payment | → COBIL00C (Confirmation/Error) |

#### Reports (CORPT00C)
**Screen ID**: CORPT00C  
**Transaction ID**: CR00  
**Function**: Generate reports

| Trigger | Action | Navigates To |
|---------|--------|--------------|
| PF3 | Return to previous screen | → CDEMO-FROM-PROGRAM or COMEN01C |
| PF4 | Clear screen | → CORPT00C (Cleared) |
| ENTER | Submit report request | → CORPT00C (Confirmation/Error) |

## Standard PF Key Navigation

The following PF key functions are consistent across all screens in the application:

| PF Key | Function | Behavior |
|--------|----------|----------|
| PF3 | Exit/Return | Returns to previous screen or main menu |
| PF4 | Clear | Clears current screen input fields |
| PF5 | Return to List | Returns to associated list screen |
| PF7 | Page Up | Navigate to previous page (list screens) |
| PF8 | Page Down | Navigate to next page (list screens) |
| PF12 | Return to Menu | Returns to main menu (specific screens) |

## Return Navigation Patterns

All programs implement a consistent return navigation pattern using the `RETURN-TO-PREV-SCREEN` subroutine:

1. **CDEMO-TO-PROGRAM** field determines target program
2. **Default fallback** to sign-on screen (COSGN00C) if no target specified
3. **EXEC CICS XCTL** transfers control with COMMAREA preservation
4. **Context preservation** through CARDDEMO-COMMAREA structure

## Selection Processing

List screens (COCRDLIC, COTRN00C, COUSR00C) use standardized selection processing:

- **'S' or 's'**: View/Select record for detail display
- **'U' or 'u'**: Update record (navigates to update screen)
- **Invalid selections**: Display error message and remain on current screen

## State Management

The application maintains state through the **CARDDEMO-COMMAREA** structure:

- **CDEMO-FROM-PROGRAM**: Source program for return navigation
- **CDEMO-TO-PROGRAM**: Target program for forward navigation
- **CDEMO-PGM-CONTEXT**: Program-specific context information
- **CDEMO-USER-TYPE**: User type (Admin/Regular) for access control
- **Program-specific sections**: Each program maintains its own context area

## Technical Implementation Notes

### Pseudo-Conversational Pattern
- Programs use `EXEC CICS RETURN` with TRANSID and COMMAREA
- State preserved between user interactions
- Efficient resource utilization

### Program Transfer Mechanism
- `EXEC CICS XCTL` for seamless program-to-program navigation
- COMMAREA passed to maintain context
- No return to calling program (transfer control)

### Error Handling
- Validation errors redisplay current screen with error messages
- File operation errors handled with appropriate user feedback
- Invalid key presses result in error messages

## Conclusion

The CardDemo application implements a well-structured, hierarchical navigation system with consistent patterns across all business functions. The pseudo-conversational design ensures efficient resource usage while maintaining user context throughout the application workflow. The role-based access control and standardized PF key navigation provide a consistent user experience across all application modules.
