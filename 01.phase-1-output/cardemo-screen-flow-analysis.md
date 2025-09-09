# CardDemo COBOL Application - Screen Flow Analysis

## Executive Summary

This document provides a comprehensive analysis of the CardDemo COBOL application's screen flow architecture, focusing on business-level user navigation paths and screen transitions. The analysis covers all 18 COBOL programs in the application, documenting the complete user interaction flows from authentication through functional operations.

### Key Findings

- **Authentication-Driven Architecture**: Single entry point (COSGN00C) with role-based routing to admin or user menus
- **Dynamic Menu System**: Menu options loaded via copybook arrays (CDEMO-MENU-OPT-PGMNAME, CDEMO-ADMIN-OPT-PGMNAME)
- **Standardized Navigation**: Consistent PF key patterns across all screens for user experience uniformity
- **Pseudo-Conversational Design**: CICS programs maintain state through CARDDEMO-COMMAREA between interactions
- **Comprehensive Coverage**: 18 programs covering authentication, menus, account management, card operations, transaction processing, user administration, and reporting

## Complete Screen Flow Mapping

### Authentication Flow

| Screen ID | Screen Title | Trigger/Option | Target Screen ID | Navigation Type |
|-----------|--------------|----------------|------------------|-----------------|
| COSGN00C | Sign-On Screen | Valid Admin Credentials | COADM01C | CICS XCTL |
| COSGN00C | Sign-On Screen | Valid User Credentials | COMEN01C | CICS XCTL |
| COSGN00C | Sign-On Screen | PF3 | Exit Application | CICS RETURN |

### Admin Menu Navigation (COADM01C)

| Screen ID | Screen Title | Trigger/Option | Target Screen ID | Navigation Type |
|-----------|--------------|----------------|------------------|-----------------|
| COADM01C | Admin Menu | Option 1 | COUSR00C | CICS XCTL via CDEMO-ADMIN-OPT-PGMNAME |
| COADM01C | Admin Menu | Option 2 | COUSR01C | CICS XCTL via CDEMO-ADMIN-OPT-PGMNAME |
| COADM01C | Admin Menu | Option 3 | CORPT00C | CICS XCTL via CDEMO-ADMIN-OPT-PGMNAME |
| COADM01C | Admin Menu | Option 4 | Additional Admin Function | CICS XCTL via CDEMO-ADMIN-OPT-PGMNAME |
| COADM01C | Admin Menu | PF3 | COSGN00C | CICS XCTL |

### User Main Menu Navigation (COMEN01C)

| Screen ID | Screen Title | Trigger/Option | Target Screen ID | Navigation Type |
|-----------|--------------|----------------|------------------|-----------------|
| COMEN01C | Main Menu | Option 1 | COACTVWC | CICS XCTL via CDEMO-MENU-OPT-PGMNAME |
| COMEN01C | Main Menu | Option 2 | COACTUPC | CICS XCTL via CDEMO-MENU-OPT-PGMNAME |
| COMEN01C | Main Menu | Option 3 | COCRDLIC | CICS XCTL via CDEMO-MENU-OPT-PGMNAME |
| COMEN01C | Main Menu | Option 4 | COCRDSLC | CICS XCTL via CDEMO-MENU-OPT-PGMNAME |
| COMEN01C | Main Menu | Option 5 | COCRDUPC | CICS XCTL via CDEMO-MENU-OPT-PGMNAME |
| COMEN01C | Main Menu | Option 6 | COTRN00C | CICS XCTL via CDEMO-MENU-OPT-PGMNAME |
| COMEN01C | Main Menu | Option 7 | COTRN01C | CICS XCTL via CDEMO-MENU-OPT-PGMNAME |
| COMEN01C | Main Menu | Option 8 | COTRN02C | CICS XCTL via CDEMO-MENU-OPT-PGMNAME |
| COMEN01C | Main Menu | Option 9 | COBIL00C | CICS XCTL via CDEMO-MENU-OPT-PGMNAME |
| COMEN01C | Main Menu | Option 10 | Additional Function | CICS XCTL via CDEMO-MENU-OPT-PGMNAME |
| COMEN01C | Main Menu | PF3 | COSGN00C | CICS XCTL |

## Functional Screen Flow Details

### Account Management Flow

| Screen ID | Screen Title | Trigger/Option | Target Screen ID | Navigation Type |
|-----------|--------------|----------------|------------------|-----------------|
| COACTVWC | Account View | Account ID Entry + ENTER | Display Account Details | Screen Refresh |
| COACTVWC | Account View | PF3 | COMEN01C or Previous Screen | CICS XCTL |
| COACTUPC | Account Update | Account ID Entry + ENTER | Load Account for Update | Screen Refresh |
| COACTUPC | Account Update | Data Changes + ENTER | Validate and Update | Screen Refresh |
| COACTUPC | Account Update | PF3 | COMEN01C or Previous Screen | CICS XCTL |
| COACTUPC | Account Update | PF4 | Clear Screen | Screen Refresh |
| COBIL00C | Bill Payment | Account Selection + ENTER | Process Payment | Screen Refresh |
| COBIL00C | Bill Payment | PF3 | COMEN01C or Previous Screen | CICS XCTL |
| COBIL00C | Bill Payment | PF4 | Clear Screen | Screen Refresh |

### Card Management Flow

| Screen ID | Screen Title | Trigger/Option | Target Screen ID | Navigation Type |
|-----------|--------------|----------------|------------------|-----------------|
| COCRDLIC | Card List | Account Filter + ENTER | Display Card List | Screen Refresh |
| COCRDLIC | Card List | 'S' Selection + ENTER | COCRDSLC | CICS XCTL |
| COCRDLIC | Card List | 'U' Selection + ENTER | COCRDUPC | CICS XCTL |
| COCRDLIC | Card List | PF3 | COMEN01C or Previous Screen | CICS XCTL |
| COCRDLIC | Card List | PF7/PF8 | Previous/Next Page | Screen Refresh |
| COCRDSLC | Card Detail | Card/Account Entry + ENTER | Display Card Details | Screen Refresh |
| COCRDSLC | Card Detail | PF3 | COCRDLIC or Previous Screen | CICS XCTL |
| COCRDUPC | Card Update | Card Selection + ENTER | Load Card for Update | Screen Refresh |
| COCRDUPC | Card Update | Data Changes + ENTER | Validate and Update | Screen Refresh |
| COCRDUPC | Card Update | PF3 | COCRDLIC or Previous Screen | CICS XCTL |
| COCRDUPC | Card Update | PF4 | Clear Screen | Screen Refresh |

### Transaction Processing Flow

| Screen ID | Screen Title | Trigger/Option | Target Screen ID | Navigation Type |
|-----------|--------------|----------------|------------------|-----------------|
| COTRN00C | Transaction List | Transaction ID Filter + ENTER | Display Transaction List | Screen Refresh |
| COTRN00C | Transaction List | 'S' Selection + ENTER | COTRN01C | CICS XCTL |
| COTRN00C | Transaction List | PF3 | COMEN01C | CICS XCTL |
| COTRN00C | Transaction List | PF7/PF8 | Previous/Next Page | Screen Refresh |
| COTRN01C | Transaction View | Transaction ID + ENTER | Display Transaction Details | Screen Refresh |
| COTRN01C | Transaction View | PF3 | COTRN00C or Previous Screen | CICS XCTL |
| COTRN01C | Transaction View | PF4 | Clear Screen | Screen Refresh |
| COTRN01C | Transaction View | PF5 | COTRN00C | CICS XCTL |
| COTRN02C | Add Transaction | Account/Card + Transaction Data + ENTER | Validate Transaction | Screen Refresh |
| COTRN02C | Add Transaction | Confirmation 'Y' + ENTER | Add Transaction | Screen Refresh |
| COTRN02C | Add Transaction | PF3 | COMEN01C or Previous Screen | CICS XCTL |
| COTRN02C | Add Transaction | PF4 | Clear Screen | Screen Refresh |
| COTRN02C | Add Transaction | PF5 | Copy Last Transaction | Screen Refresh |

### User Administration Flow (Admin Only)

| Screen ID | Screen Title | Trigger/Option | Target Screen ID | Navigation Type |
|-----------|--------------|----------------|------------------|-----------------|
| COUSR00C | User List | User ID Filter + ENTER | Display User List | Screen Refresh |
| COUSR00C | User List | 'U' Selection + ENTER | COUSR02C | CICS XCTL |
| COUSR00C | User List | 'D' Selection + ENTER | COUSR03C | CICS XCTL |
| COUSR00C | User List | PF3 | COADM01C | CICS XCTL |
| COUSR00C | User List | PF7/PF8 | Previous/Next Page | Screen Refresh |
| COUSR01C | Add User | User Data + ENTER | Validate and Add User | Screen Refresh |
| COUSR01C | Add User | PF3 | COADM01C | CICS XCTL |
| COUSR01C | Add User | PF4 | Clear Screen | Screen Refresh |
| COUSR02C | Update User | User ID + ENTER | Load User for Update | Screen Refresh |
| COUSR02C | Update User | Data Changes + PF5 | Update User | Screen Refresh |
| COUSR02C | Update User | PF3 | COADM01C or Previous Screen | CICS XCTL |
| COUSR02C | Update User | PF4 | Clear Screen | Screen Refresh |
| COUSR02C | Update User | PF12 | COADM01C | CICS XCTL |
| COUSR03C | Delete User | User ID + ENTER | Load User for Deletion | Screen Refresh |
| COUSR03C | Delete User | PF5 | Delete User | Screen Refresh |
| COUSR03C | Delete User | PF3 | COADM01C or Previous Screen | CICS XCTL |
| COUSR03C | Delete User | PF4 | Clear Screen | Screen Refresh |
| COUSR03C | Delete User | PF12 | COADM01C | CICS XCTL |

### Reporting Flow (Admin Only)

| Screen ID | Screen Title | Trigger/Option | Target Screen ID | Navigation Type |
|-----------|--------------|----------------|------------------|-----------------|
| CORPT00C | Reports | Report Parameters + ENTER | Submit Batch Job | Screen Refresh |
| CORPT00C | Reports | PF3 | COADM01C | CICS XCTL |

## Standard PF Key Navigation Patterns

### Universal PF Key Functions

| PF Key | Function | Behavior | Available On |
|--------|----------|----------|--------------|
| PF3 | Exit/Return | Return to previous screen or main menu | All screens |
| PF4 | Clear | Clear current screen fields | Most input screens |
| PF5 | Special Action | Save, Delete, Copy, or specific function | Context-dependent |
| PF7 | Page Up | Navigate to previous page in lists | List screens |
| PF8 | Page Down | Navigate to next page in lists | List screens |
| PF12 | Cancel | Cancel operation and return to main menu | Update/Delete screens |

### Context-Specific PF Key Usage

**Transaction Screens:**
- PF5 in COTRN01C: Return to Transaction List (COTRN00C)
- PF5 in COTRN02C: Copy last transaction data

**User Management Screens:**
- PF5 in COUSR02C: Save user updates
- PF5 in COUSR03C: Confirm user deletion

**Account/Card Screens:**
- PF4: Clear screen for new entry
- PF3: Return to calling program or main menu

## Technical Architecture Patterns

### Communication Area (CARDDEMO-COMMAREA)

The application uses a shared communication area structure to maintain context between pseudo-conversational interactions:

- **CDEMO-FROM-PROGRAM**: Source program identifier
- **CDEMO-TO-PROGRAM**: Target program for XCTL
- **CDEMO-PGM-CONTEXT**: Program state flags (ENTER, REENTER)
- **CDEMO-USRTYP**: User type (ADMIN, USER) for access control
- **Program-specific sections**: CT00, CT01, CT02, CU00, etc. for maintaining operational state

### Dynamic Menu Loading

Menu programs use copybook-defined arrays for flexible option management:

```cobol
CDEMO-MENU-OPT-PGMNAME(WS-OPTION)     // User menu options
CDEMO-ADMIN-OPT-PGMNAME(WS-OPTION)    // Admin menu options
```

This allows for easy modification of menu structures without program changes.

### Screen Interaction Patterns

**BMS Map Processing:**
1. SEND MAP with screen data
2. RECEIVE MAP for user input
3. Validate input and process
4. XCTL to target program or refresh screen

**File Operations:**
- VSAM file access for data persistence
- STARTBR/READNEXT/ENDBR for list processing
- READ/WRITE/REWRITE/DELETE for CRUD operations

### Error Handling and Validation

**Multi-layered Validation:**
1. Field-level validation (numeric, required fields)
2. Cross-field validation (business rules)
3. Database validation (record existence)
4. User feedback through error messages

**State Management:**
- Error flags (WS-ERR-FLG) control screen flow
- Input validation prevents invalid state transitions
- Confirmation screens for destructive operations

## Business Process Flows

### User Journey: Account Management

1. **Authentication**: User logs in via COSGN00C
2. **Menu Selection**: Chooses account option from COMEN01C
3. **Account View**: Views account details in COACTVWC
4. **Account Update**: Modifies account information in COACTUPC
5. **Bill Payment**: Processes payment through COBIL00C
6. **Return**: Navigates back to main menu via PF3

### Admin Journey: User Management

1. **Authentication**: Admin logs in via COSGN00C
2. **Admin Menu**: Accesses admin functions via COADM01C
3. **User List**: Views user list in COUSR00C
4. **User Operations**: 
   - Add new user via COUSR01C
   - Update existing user via COUSR02C
   - Delete user via COUSR03C
5. **Return**: Returns to admin menu via PF3/PF12

### Transaction Processing Journey

1. **Menu Access**: User selects transaction option from COMEN01C
2. **Transaction List**: Views transactions in COTRN00C with pagination
3. **Transaction Detail**: Examines specific transaction in COTRN01C
4. **Add Transaction**: Creates new transaction in COTRN02C with validation
5. **Navigation**: Uses PF keys for efficient screen traversal

## Security and Access Control

### Role-Based Access

- **Admin Users**: Access to COADM01C and user management functions
- **Regular Users**: Access to COMEN01C and account/card/transaction functions
- **Authentication**: Single sign-on point with credential validation

### Navigation Security

- Programs validate user type before allowing access
- Admin-only functions protected by CDEMO-USRTYP-ADMIN flag
- Consistent return paths prevent unauthorized access

## Performance Considerations

### Pseudo-Conversational Design

- Programs release control between user interactions
- State maintained through communication areas
- Efficient resource utilization in CICS environment

### Pagination Support

- List screens implement STARTBR/READNEXT patterns
- PF7/PF8 navigation for large datasets
- Page state maintained in program-specific communication areas

## Conclusion

The CardDemo COBOL application demonstrates a well-structured, business-focused screen flow architecture with:

- **Clear separation of concerns** between authentication, menu navigation, and functional operations
- **Consistent user experience** through standardized PF key navigation
- **Flexible menu system** supporting easy maintenance and updates
- **Comprehensive business coverage** across account management, card operations, transaction processing, and user administration
- **Robust error handling** and validation throughout all user interactions

This analysis provides the foundation for understanding user navigation patterns and supports future enhancements or modernization efforts while maintaining the integrity of existing business processes.

---

**Analysis Coverage**: 18 COBOL programs analyzed  
**Focus**: Production user-facing screen flows  
**Exclusions**: Internal technical logic, test/debug screens, unused flows  
**Documentation Date**: September 2025
