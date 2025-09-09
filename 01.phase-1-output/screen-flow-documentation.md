# CardDemo COBOL Application Screen Flow Documentation

## Overview

The CardDemo application is a comprehensive credit card management system built using CICS COBOL with a pseudo-conversational architecture. This document provides a detailed analysis of the business-level screen flows extracted from all 18 COBOL programs in the application.

## Architecture Summary

- **Authentication Model**: Role-based access control with regular users and administrators
- **Navigation Pattern**: EXEC CICS XCTL transfers with CARDDEMO-COMMAREA state management
- **Screen Technology**: BMS (Basic Mapping Support) with SEND MAP/RECEIVE MAP operations
- **Transaction Model**: Pseudo-conversational CICS transactions preserving state between interactions

## Main Screen Flow Hierarchy

### 1. Authentication Entry Point

| Screen ID | Title | Function | Navigation |
|-----------|-------|----------|------------|
| COSGN00C | Sign-on Screen | User authentication and role validation | → COMEN01C (Regular Users)<br>→ COADM01C (Admin Users) |

**Key Navigation Logic:**
```cobol
IF CDEMO-USRTYP-ADMIN
    EXEC CICS XCTL PROGRAM ('COADM01C') COMMAREA(CARDDEMO-COMMAREA)
ELSE
    EXEC CICS XCTL PROGRAM ('COMEN01C') COMMAREA(CARDDEMO-COMMAREA)
END-IF
```

### 2. Regular User Menu System

| Screen ID | Title | Function | Available Options |
|-----------|-------|----------|-------------------|
| COMEN01C | Main Menu | Regular user function selection | Dynamic menu options via CDEMO-MENU-OPT-PGMNAME array |

**Menu Option Processing:**
```cobol
EXEC CICS XCTL PROGRAM(CDEMO-MENU-OPT-PGMNAME(WS-OPTION))
     COMMAREA(CARDDEMO-COMMAREA)
END-EXEC
```

**Regular User Functions:**
- Account Management (COACTVWC, COACTUPC)
- Credit Card Operations (COCRDLIC)
- Transaction Processing (COTRN00C)
- Bill Payment (COBIL00C)
- Reports (CORPT00C)

### 3. Administrator Menu System

| Screen ID | Title | Function | Available Options |
|-----------|-------|----------|-------------------|
| COADM01C | Admin Menu | Administrator function selection | Dynamic admin options via CDEMO-ADMIN-OPT-PGMNAME array |

**Admin Option Processing:**
```cobol
EXEC CICS XCTL PROGRAM(CDEMO-ADMIN-OPT-PGMNAME(WS-OPTION))
     COMMAREA(CARDDEMO-COMMAREA)
END-EXEC
```

**Administrator Functions:**
- User Management (COUSR00C, COUSR01C, COUSR02C, COUSR03C)
- All regular user functions with elevated privileges

## Detailed Screen Navigation Flows

### Account Management Flow

| Screen ID | Title | Trigger/Option | Navigates To |
|-----------|-------|----------------|--------------|
| COACTVWC | Account View | Menu Option | Display account details |
| COACTUPC | Account Update | Menu Option | Account modification form |
| COACTUPC | Account Update | Update Complete | → CDEMO-TO-PROGRAM (Previous screen) |

**Navigation Pattern:**
- Entry from main menu
- F3 returns to previous screen via RETURN-TO-PREV-SCREEN
- Update completion triggers SYNCPOINT and XCTL return

### Credit Card Management Flow

| Screen ID | Title | Trigger/Option | Navigates To |
|-----------|-------|----------------|--------------|
| COCRDLIC | Credit Card List | Menu Option | Display paginated card list |
| COCRDLIC | Credit Card List | S=Select | → COCRDSLC (Card Detail) |
| COCRDLIC | Credit Card List | U=Update | → COCRDUPC (Card Update) |
| COCRDSLC | Credit Card Detail | From Card List | Display card information |
| COCRDUPC | Credit Card Update | From Card List | Card modification form |

**Selection Processing Logic:**
```cobol
EVALUATE WS-EDIT-SELECT(I)
    WHEN 'S' - VIEW-REQUESTED-ON
        EXEC CICS XCTL PROGRAM(LIT-CARDDTLPGM) COMMAREA(CARDDEMO-COMMAREA)
    WHEN 'U' - UPDATE-REQUESTED-ON  
        EXEC CICS XCTL PROGRAM(LIT-CARDUPDPGM) COMMAREA(CARDDEMO-COMMAREA)
END-EVALUATE
```

### Transaction Management Flow

| Screen ID | Title | Trigger/Option | Navigates To |
|-----------|-------|----------------|--------------|
| COTRN00C | Transaction List | Menu Option | Display paginated transaction list |
| COTRN00C | Transaction List | S=Select | → COTRN01C (Transaction Detail) |
| COTRN01C | Transaction Detail | From Transaction List | Display transaction information |
| COTRN01C | Transaction Detail | F5 | → COTRN00C (Back to List) |
| COTRN02C | Transaction Add | Menu Option | New transaction entry form |

**Transaction Selection Logic:**
```cobol
EVALUATE CDEMO-CT00-TRN-SEL-FLG
    WHEN 'S' OR 's'
        MOVE 'COTRN01C' TO CDEMO-TO-PROGRAM
        EXEC CICS XCTL PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA)
END-EVALUATE
```

### User Management Flow (Admin Only)

| Screen ID | Title | Trigger/Option | Navigates To |
|-----------|-------|----------------|--------------|
| COUSR00C | User List | Admin Menu Option | Display paginated user list |
| COUSR00C | User List | U=Update | → COUSR02C (User Update) |
| COUSR00C | User List | D=Delete | → COUSR03C (User Delete) |
| COUSR01C | User Add | Admin Menu Option | New user creation form |
| COUSR02C | User Update | From User List | User modification form |
| COUSR03C | User Delete | From User List | User deletion confirmation |

**User Selection Processing:**
```cobol
EVALUATE CDEMO-CU00-USR-SEL-FLG
    WHEN 'U' OR 'u'
        MOVE 'COUSR02C' TO CDEMO-TO-PROGRAM
        EXEC CICS XCTL PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA)
    WHEN 'D' OR 'd'
        MOVE 'COUSR03C' TO CDEMO-TO-PROGRAM
        EXEC CICS XCTL PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA)
END-EVALUATE
```

### Additional Functions

| Screen ID | Title | Trigger/Option | Navigates To |
|-----------|-------|----------------|--------------|
| COBIL00C | Bill Payment | Menu Option | Bill payment processing form |
| CORPT00C | Reports | Menu Option | Report generation interface |
| CSUTLDTC | Date Utility | Called by other programs | Date validation service |

## PF Key Navigation Standards

All screens implement standardized PF key handling:

| PF Key | Function | Implementation |
|--------|----------|----------------|
| F3 | Exit to Previous Screen | RETURN-TO-PREV-SCREEN or RETURN-TO-SIGNON-SCREEN |
| F4 | Clear Current Screen | CLEAR-CURRENT-SCREEN |
| F5 | Special Functions | Program-specific actions |
| F7 | Page Up | PROCESS-PF7-KEY (List screens) |
| F8 | Page Down | PROCESS-PF8-KEY (List screens) |
| F12 | Cancel/Return to Menu | Return to calling program |
| ENTER | Process Input | PROCESS-ENTER-KEY |

## List Screen Pagination Patterns

List screens (COUSR00C, COTRN00C, COCRDLIC) implement consistent pagination:

- **Forward Navigation**: F8 key triggers PROCESS-PAGE-FORWARD
- **Backward Navigation**: F7 key triggers PROCESS-PAGE-BACKWARD  
- **CICS File Operations**: STARTBR → READNEXT/READPREV → ENDBR sequence
- **Page State Management**: Maintained in program-specific COMMAREA sections

## Selection Processing Standards

List screens use standardized selection codes:

- **S or s**: Select/View record (navigate to detail screen)
- **U or u**: Update record (navigate to update screen)  
- **D or d**: Delete record (navigate to delete confirmation)

Selection validation ensures only one record is selected per transaction.

## State Management Architecture

### CARDDEMO-COMMAREA Structure
- **CDEMO-FROM-PROGRAM**: Source program identifier
- **CDEMO-TO-PROGRAM**: Target program identifier  
- **CDEMO-FROM-TRANID**: Source transaction ID
- **CDEMO-USER-ID**: Authenticated user identifier
- **CDEMO-USER-TYPE**: User role (Regular/Admin)
- **CDEMO-PGM-CONTEXT**: Program-specific context data
- **CDEMO-PGM-REENTER**: Re-entry flag for pseudo-conversational flow

### Program-Specific Context Areas
- **CDEMO-CT00-INFO**: Transaction list context
- **CDEMO-CT01-INFO**: Transaction detail context  
- **CDEMO-CU00-INFO**: User list context
- **WS-THIS-PROGCOMMAREA**: Program-specific working storage

## Return Navigation Patterns

### RETURN-TO-PREV-SCREEN
```cobol
IF CDEMO-TO-PROGRAM = LOW-VALUES OR SPACES
    MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
END-IF
EXEC CICS XCTL PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA)
```

### RETURN-TO-SIGNON-SCREEN  
```cobol
IF CDEMO-TO-PROGRAM = LOW-VALUES OR SPACES
    MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
END-IF
EXEC CICS XCTL PROGRAM(CDEMO-TO-PROGRAM)
```

## BMS Map Integration

All screens use BMS (Basic Mapping Support) for terminal I/O:

- **SEND MAP**: Display screen to terminal with data
- **RECEIVE MAP**: Capture user input from terminal
- **Map Naming Convention**: Program name + map suffix (e.g., COSGN0A, COMEN1A)
- **Mapset Organization**: Each program has associated mapset for screen layouts

## Error Handling and Validation

Consistent error handling patterns across all programs:

- **WS-ERR-FLG**: Error state indicator ('Y'/'N')
- **WS-MESSAGE**: Error message display field
- **Field-level validation**: Required field checks with cursor positioning
- **CICS RESP codes**: File operation result evaluation
- **User feedback**: Error messages displayed on screen with appropriate field highlighting

## Technical Implementation Notes

1. **Pseudo-conversational Design**: Programs terminate after each user interaction, preserving state through COMMAREA
2. **Transaction Integrity**: SYNCPOINT operations ensure data consistency before program transfers
3. **Dynamic Menu Processing**: Menu options loaded from copybook arrays enabling flexible configuration
4. **Context Preservation**: Navigation history maintained for proper return path handling
5. **Role-based Security**: User type validation controls access to administrative functions

## Production Flow Verification

All documented flows represent live, production-ready navigation paths used in real-time user interactions. Test screens, debug functions, and obsolete flows have been excluded from this analysis.

The screen flow documentation covers all 18 COBOL programs analyzed:
- COSGN00C, COMEN01C, COADM01C (Core navigation)
- COACTVWC, COACTUPC (Account management)  
- COCRDLIC, COCRDSLC, COCRDUPC (Card management)
- COTRN00C, COTRN01C, COTRN02C (Transaction management)
- COUSR00C, COUSR01C, COUSR02C, COUSR03C (User management)
- COBIL00C (Bill payment)
- CORPT00C (Reports)
- CSUTLDTC (Utility functions)

This comprehensive analysis provides a complete picture of user navigation flows through the CardDemo application, enabling effective maintenance, enhancement, and user training activities.
