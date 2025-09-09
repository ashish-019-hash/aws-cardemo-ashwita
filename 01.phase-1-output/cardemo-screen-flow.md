# CardDemo COBOL Screen Flow Analysis

## Overview

This document provides a comprehensive analysis of the screen navigation flows in the CardDemo COBOL application. The analysis covers all user-facing screens and their navigation patterns, excluding utility programs and internal technical logic.

## Screen Flow Hierarchy

### 1. Authentication Layer

| Screen ID | Program | Transaction | Function | Navigation Trigger | Target Screen |
|-----------|---------|-------------|----------|-------------------|---------------|
| COSGN00C | COSGN00C | CC00 | Sign-on Screen | Admin User Type | COADM01C |
| COSGN00C | COSGN00C | CC00 | Sign-on Screen | Regular User Type | COMEN01C |
| COSGN00C | COSGN00C | CC00 | Sign-on Screen | PF3 | Exit Application |

**Key Navigation Logic:**
- User authentication determines routing path
- `CDEMO-USRTYP-ADMIN` → Admin Menu (COADM01C)
- `CDEMO-USRTYP-USER` → Regular Menu (COMEN01C)
- Uses EXEC CICS XCTL for program transfer

### 2. Main Menu Layer

#### Admin Menu (COADM01C)
| Screen ID | Program | Transaction | Function | Navigation Trigger | Target Screen |
|-----------|---------|-------------|----------|-------------------|---------------|
| COADM01C | COADM01C | CA00 | Admin Menu | Option 1-4 | Dynamic via CDEMO-ADMIN-OPT-PGMNAME |
| COADM01C | COADM01C | CA00 | Admin Menu | PF3 | COSGN00C |
| COADM01C | COADM01C | CA00 | Admin Menu | Invalid Option | Error Message |

**Key Navigation Logic:**
- Dynamic menu options via `CDEMO-ADMIN-OPT-PGMNAME(WS-OPTION)`
- Option validation: `WS-OPTION > CDEMO-ADMIN-OPT-COUNT`
- Menu options built dynamically in BUILD-MENU-OPTIONS

#### Regular User Menu (COMEN01C)
| Screen ID | Program | Transaction | Function | Navigation Trigger | Target Screen |
|-----------|---------|-------------|----------|-------------------|---------------|
| COMEN01C | COMEN01C | CM00 | Regular Menu | Valid Option | Dynamic via CDEMO-MENU-OPT-PGMNAME |
| COMEN01C | COMEN01C | CM00 | Regular Menu | Admin-Only Option | Access Denied |
| COMEN01C | COMEN01C | CM00 | Regular Menu | PF3 | COSGN00C |

**Key Navigation Logic:**
- Dynamic menu options via `CDEMO-MENU-OPT-PGMNAME(WS-OPTION)`
- Access control: `CDEMO-MENU-OPT-USRTYPE(WS-OPTION) = 'A'` blocks regular users
- Option validation: `WS-OPTION > CDEMO-MENU-OPT-COUNT`

### 3. Card Management Flow

#### Card List Screen (COCRDLIC)
| Screen ID | Program | Transaction | Function | Navigation Trigger | Target Screen |
|-----------|---------|-------------|----------|-------------------|---------------|
| COCRDLIC | COCRDLIC | CCLI | Card List | Selection 'S' | COCRDSLC (Card Detail) |
| COCRDLIC | COCRDLIC | CCLI | Card List | Selection 'U' | COCRDUPC (Card Update) |
| COCRDLIC | COCRDLIC | CCLI | Card List | PF3 | COMEN01C (Menu) |
| COCRDLIC | COCRDLIC | CCLI | Card List | PF7/PF8 | Page Up/Down |

**Key Navigation Logic:**
- Selection processing via `VIEW-REQUESTED-ON` and `UPDATE-REQUESTED-ON`
- Target program set via `CCARD-NEXT-PROG` variable
- Card detail: `LIT-CARDDTLPGM` (COCRDSLC)
- Card update: `LIT-CARDUPDPGM` (COCRDUPC)

#### Card Detail Screen (COCRDSLC)
| Screen ID | Program | Transaction | Function | Navigation Trigger | Target Screen |
|-----------|---------|-------------|----------|-------------------|---------------|
| COCRDSLC | COCRDSLC | CCDL | Card Detail | PF3 | Previous Screen |
| COCRDSLC | COCRDSLC | CCDL | Card Detail | PF4 | Clear Screen |
| COCRDSLC | COCRDSLC | CCDL | Card Detail | Update Action | COCRDUPC |

#### Card Update Screen (COCRDUPC)
| Screen ID | Program | Transaction | Function | Navigation Trigger | Target Screen |
|-----------|---------|-------------|----------|-------------------|---------------|
| COCRDUPC | COCRDUPC | CCUP | Card Update | PF3 | Previous Screen |
| COCRDUPC | COCRDUPC | CCUP | Card Update | PF4 | Clear Screen |
| COCRDUPC | COCRDUPC | CCUP | Card Update | Update Complete | Previous Screen |

### 4. Transaction Management Flow

#### Transaction List Screen (COTRN00C)
| Screen ID | Program | Transaction | Function | Navigation Trigger | Target Screen |
|-----------|---------|-------------|----------|-------------------|---------------|
| COTRN00C | COTRN00C | CT00 | Transaction List | Selection 'S' | COTRN01C |
| COTRN00C | COTRN00C | CT00 | Transaction List | PF3 | COMEN01C |
| COTRN00C | COTRN00C | CT00 | Transaction List | PF7/PF8 | Page Up/Down |

**Key Navigation Logic:**
- Selection validation: `CDEMO-CT00-TRN-SEL-FLG` = 'S' or 's'
- Target: `MOVE 'COTRN01C' TO CDEMO-TO-PROGRAM`
- Invalid selection shows error message

#### Transaction Detail Screen (COTRN01C)
| Screen ID | Program | Transaction | Function | Navigation Trigger | Target Screen |
|-----------|---------|-------------|----------|-------------------|---------------|
| COTRN01C | COTRN01C | CT01 | Transaction Detail | PF3 | Previous Screen or COMEN01C |
| COTRN01C | COTRN01C | CT01 | Transaction Detail | PF4 | Clear Screen |
| COTRN01C | COTRN01C | CT01 | Transaction Detail | PF5 | COTRN00C (List) |

#### Add Transaction Screen (COTRN02C)
| Screen ID | Program | Transaction | Function | Navigation Trigger | Target Screen |
|-----------|---------|-------------|----------|-------------------|---------------|
| COTRN02C | COTRN02C | CT02 | Add Transaction | PF3 | Previous Screen |
| COTRN02C | COTRN02C | CT02 | Add Transaction | PF4 | Clear Screen |
| COTRN02C | COTRN02C | CT02 | Add Transaction | PF5 | COTRN00C (List) |

### 5. User Administration Flow (Admin Only)

#### User List Screen (COUSR00C)
| Screen ID | Program | Transaction | Function | Navigation Trigger | Target Screen |
|-----------|---------|-------------|----------|-------------------|---------------|
| COUSR00C | COUSR00C | CU00 | User List | Selection 'U' | COUSR02C (Update) |
| COUSR00C | COUSR00C | CU00 | User List | Selection 'D' | COUSR03C (Delete) |
| COUSR00C | COUSR00C | CU00 | User List | PF3 | COADM01C |
| COUSR00C | COUSR00C | CU00 | User List | PF7/PF8 | Page Up/Down |

**Key Navigation Logic:**
- Selection validation: `CDEMO-CU00-USR-SEL-FLG` = 'U', 'u', 'D', or 'd'
- Update: `MOVE 'COUSR02C' TO CDEMO-TO-PROGRAM`
- Delete: `MOVE 'COUSR03C' TO CDEMO-TO-PROGRAM`

#### Add User Screen (COUSR01C)
| Screen ID | Program | Transaction | Function | Navigation Trigger | Target Screen |
|-----------|---------|-------------|----------|-------------------|---------------|
| COUSR01C | COUSR01C | CU01 | Add User | PF3 | COADM01C |
| COUSR01C | COUSR01C | CU01 | Add User | PF4 | Clear Screen |
| COUSR01C | COUSR01C | CU01 | Add User | PF12 | COADM01C |

#### Update User Screen (COUSR02C)
| Screen ID | Program | Transaction | Function | Navigation Trigger | Target Screen |
|-----------|---------|-------------|----------|-------------------|---------------|
| COUSR02C | COUSR02C | CU02 | Update User | PF3 | Previous Screen |
| COUSR02C | COUSR02C | CU02 | Update User | PF4 | Clear Screen |
| COUSR02C | COUSR02C | CU02 | Update User | PF12 | COADM01C |

#### Delete User Screen (COUSR03C)
| Screen ID | Program | Transaction | Function | Navigation Trigger | Target Screen |
|-----------|---------|-------------|----------|-------------------|---------------|
| COUSR03C | COUSR03C | CU03 | Delete User | PF3 | Previous Screen or COADM01C |
| COUSR03C | COUSR03C | CU03 | Delete User | PF4 | Clear Screen |
| COUSR03C | COUSR03C | CU03 | Delete User | PF5 | Delete Confirmation |
| COUSR03C | COUSR03C | CU03 | Delete User | PF12 | COADM01C |

### 6. Account Management Flow

#### Account View Screen (COACTVWC)
| Screen ID | Program | Transaction | Function | Navigation Trigger | Target Screen |
|-----------|---------|-------------|----------|-------------------|---------------|
| COACTVWC | COACTVWC | CAVW | Account View | Update Action | COACTUPC |
| COACTVWC | COACTVWC | CAVW | Account View | PF3 | Previous Screen |

#### Account Update Screen (COACTUPC)
| Screen ID | Program | Transaction | Function | Navigation Trigger | Target Screen |
|-----------|---------|-------------|----------|-------------------|---------------|
| COACTUPC | COACTUPC | CAUP | Account Update | Update Complete | Previous Screen |
| COACTUPC | COACTUPC | CAUP | Account Update | PF3 | Previous Screen |

#### Bill Payment Screen (COBIL00C)
| Screen ID | Program | Transaction | Function | Navigation Trigger | Target Screen |
|-----------|---------|-------------|----------|-------------------|---------------|
| COBIL00C | COBIL00C | CBIL | Bill Payment | PF3 | Previous Screen or COMEN01C |
| COBIL00C | COBIL00C | CBIL | Bill Payment | PF4 | Clear Screen |

### 7. Reports Flow

#### Report Generation Screen (CORPT00C)
| Screen ID | Program | Transaction | Function | Navigation Trigger | Target Screen |
|-----------|---------|-------------|----------|-------------------|---------------|
| CORPT00C | CORPT00C | CRPT | Report Generation | PF3 | Previous Screen |
| CORPT00C | CORPT00C | CRPT | Report Generation | Submit Job | Batch Processing |

## Standard Navigation Patterns

### PF Key Functions
- **PF3**: Exit/Return to previous screen (universal)
- **PF4**: Clear current screen (most screens)
- **PF5**: Special functions (varies by program)
- **PF7/PF8**: Page up/down navigation (list screens)
- **PF12**: Return to main menu (admin screens)

### Selection Codes
- **S**: Select/View detail record
- **U**: Update existing record
- **D**: Delete existing record
- **A**: Add new record (context-dependent)

### Return Path Logic
All screens implement return path logic via:
- `CDEMO-FROM-PROGRAM`: Tracks calling program
- `CDEMO-TO-PROGRAM`: Sets target program
- Default fallback: COSGN00C (sign-on screen)
- Admin screens default to COADM01C
- Regular screens default to COMEN01C

### Error Handling
- Invalid selections show error messages
- Invalid PF keys trigger error handling
- Field validation errors prevent navigation
- Access control blocks unauthorized functions

## Technical Implementation Notes

### CICS Commands Used
- `EXEC CICS XCTL`: Program-to-program transfer
- `EXEC CICS SEND MAP`: Display screen
- `EXEC CICS RECEIVE MAP`: Get user input
- `EXEC CICS RETURN`: Return control to CICS

### Communication Area (COMMAREA)
- `CARDDEMO-COMMAREA`: Main communication structure
- Preserves context across program transfers
- Contains user information, navigation history, and program state

### BMS Maps
Each screen has associated BMS maps:
- Input map (suffix 'I'): Receives user input
- Output map (suffix 'O'): Sends data to terminal
- Map names follow pattern: program prefix + map identifier

## Excluded Elements

The following were excluded from this analysis as they don't represent user navigation flows:
- **CSUTLDTC**: Date validation utility program
- Internal data validation routines
- File I/O operations not related to screen flow
- Technical error handling not affecting navigation
- Debug/test-only code paths

## Summary

The CardDemo application implements a hierarchical screen flow with:
- **18 total COBOL programs** (17 user-facing screens + 1 utility)
- **Role-based access control** (Admin vs Regular users)
- **Consistent navigation patterns** (PF keys, selection codes)
- **Pseudo-conversational design** (CICS best practice)
- **Comprehensive return path handling**

The screen flow supports complete credit card management operations including user authentication, account management, card operations, transaction processing, user administration, and reporting functionality.
