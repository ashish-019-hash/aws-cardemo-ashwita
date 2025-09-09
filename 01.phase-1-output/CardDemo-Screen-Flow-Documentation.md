# CardDemo Application - Screen Flow Documentation

## Overview
This document provides a comprehensive analysis of the business-level screen navigation flows in the CardDemo COBOL application. The analysis covers all 18 COBOL programs in the application, focusing on user-facing navigation paths, screen transitions, menu selections, and condition-based flows.

## Application Architecture
The CardDemo application follows a pseudo-conversational CICS design pattern with role-based access control and hierarchical screen navigation.

### Key Navigation Patterns
- **Authentication-based routing**: User type determines available menu options
- **XCTL program transfers**: Seamless navigation between functional modules
- **CICS SEND/RECEIVE**: Screen interaction using BMS maps
- **Context preservation**: CARDDEMO-COMMAREA maintains state across programs
- **Standardized PF key handling**: Consistent navigation controls across all screens

## Screen Flow Hierarchy

### 1. Authentication Layer

#### COSGN00C - Sign-on Screen
- **Screen ID**: COSGN00C
- **Transaction ID**: CC00
- **Map**: COSGN0A / COSGN00
- **Function**: User authentication and role-based routing

**Navigation Flows:**
| Trigger/Condition | Navigates To |
|------------------|--------------|
| Valid Admin User Credentials | → COADM01C (Admin Menu) |
| Valid Regular User Credentials | → COMEN01C (Regular User Menu) |
| PF3 | → Exit Application |
| Invalid Credentials | → COSGN00C (Redisplay with error) |

### 2. Menu Layer

#### COADM01C - Admin Menu
- **Screen ID**: COADM01C
- **Transaction ID**: CA00
- **Map**: COADM1A / COADM01
- **Function**: Administrative menu for admin users

**Navigation Flows:**
| Trigger/Option | Navigates To |
|---------------|--------------|
| Menu Option Selection | → CDEMO-ADMIN-OPT-PGMNAME(WS-OPTION) |
| PF3 | → COSGN00C (Sign-on Screen) |
| Invalid Option | → COADM01C (Redisplay with error) |

#### COMEN01C - Regular User Menu
- **Screen ID**: COMEN01C
- **Transaction ID**: CM00
- **Map**: COMEN1A / COMEN01
- **Function**: Main menu for regular users

**Navigation Flows:**
| Trigger/Option | Navigates To |
|---------------|--------------|
| Menu Option Selection | → CDEMO-MENU-OPT-PGMNAME(WS-OPTION) |
| PF3 | → COSGN00C (Sign-on Screen) |
| Admin-only Option (Regular User) | → COMEN01C (Access denied error) |
| Invalid Option | → COMEN01C (Redisplay with error) |

### 3. Account Management Module

#### COACTVWC - Account View
- **Screen ID**: COACTVWC
- **Transaction ID**: CAVW
- **Map**: CACTVWA / COACTVW
- **Function**: Display account details

**Navigation Flows:**
| Trigger/Action | Navigates To |
|---------------|--------------|
| PF3 | → CDEMO-FROM-PROGRAM (Previous screen) |
| ENTER with Account ID | → COACTVWC (Display account details) |
| Invalid Account ID | → COACTVWC (Redisplay with error) |

#### COACTUPC - Account Update
- **Screen ID**: COACTUPC
- **Transaction ID**: CAUP
- **Map**: CACTUPAO / COACTUPC
- **Function**: Update account information

**Navigation Flows:**
| Trigger/Action | Navigates To |
|---------------|--------------|
| PF3 | → CDEMO-FROM-PROGRAM (Previous screen) |
| ENTER with Updates | → COACTUPC (Process and confirm) |
| Update Confirmation | → CDEMO-FROM-PROGRAM (Return to caller) |
| Validation Error | → COACTUPC (Redisplay with error) |

### 4. Card Management Module

#### COCRDLIC - Credit Card List
- **Screen ID**: COCRDLIC
- **Transaction ID**: CCLI
- **Map**: CCRDLIA / COCRDLI
- **Function**: List credit cards with pagination

**Navigation Flows:**
| Trigger/Action | Navigates To |
|---------------|--------------|
| PF3 | → CDEMO-FROM-PROGRAM (Previous screen) |
| PF7 | → COCRDLIC (Previous page) |
| PF8 | → COCRDLIC (Next page) |
| Selection 'S' | → COCRDSLC (Card detail view) |
| Selection 'U' | → COCRDUPC (Card update) |
| ENTER | → COCRDLIC (Refresh list) |

#### COCRDSLC - Credit Card Detail
- **Screen ID**: COCRDSLC
- **Transaction ID**: CCDL
- **Map**: CCRDSLA / COCRDSL
- **Function**: Display credit card details

**Navigation Flows:**
| Trigger/Action | Navigates To |
|---------------|--------------|
| PF3 | → CDEMO-FROM-PROGRAM (Previous screen) |
| ENTER with Card/Account | → COCRDSLC (Display card details) |
| Invalid Card/Account | → COCRDSLC (Redisplay with error) |

#### COCRDUPC - Credit Card Update
- **Screen ID**: COCRDUPC
- **Transaction ID**: CCUP
- **Map**: CCRDUPA / COCRDUP
- **Function**: Update credit card information

**Navigation Flows:**
| Trigger/Action | Navigates To |
|---------------|--------------|
| PF3 | → CDEMO-FROM-PROGRAM (Previous screen) |
| ENTER with Updates | → COCRDUPC (Process and confirm) |
| Update Confirmation | → CDEMO-FROM-PROGRAM (Return to caller) |
| Validation Error | → COCRDUPC (Redisplay with error) |

### 5. Transaction Management Module

#### COTRN00C - Transaction List
- **Screen ID**: COTRN00C
- **Transaction ID**: CT00
- **Map**: COTRN0A / COTRN00
- **Function**: List transactions with pagination

**Navigation Flows:**
| Trigger/Action | Navigates To |
|---------------|--------------|
| PF3 | → COMEN01C (Main menu) |
| PF7 | → COTRN00C (Previous page) |
| PF8 | → COTRN00C (Next page) |
| Selection 'S' | → COTRN01C (Transaction detail view) |
| ENTER with Transaction ID | → COTRN00C (Filter by transaction) |
| Invalid Selection | → COTRN00C (Redisplay with error) |

#### COTRN01C - Transaction View
- **Screen ID**: COTRN01C
- **Transaction ID**: CT01
- **Map**: COTRN1A / COTRN01
- **Function**: Display transaction details

**Navigation Flows:**
| Trigger/Action | Navigates To |
|---------------|--------------|
| PF3 | → CDEMO-FROM-PROGRAM (Previous screen) |
| PF4 | → COTRN01C (Clear screen) |
| PF5 | → COTRN00C (Return to transaction list) |
| ENTER with Transaction ID | → COTRN01C (Display transaction details) |
| Invalid Transaction ID | → COTRN01C (Redisplay with error) |

#### COTRN02C - Add Transaction
- **Screen ID**: COTRN02C
- **Transaction ID**: CT02
- **Map**: COTRN2A / COTRN02
- **Function**: Add new transaction

**Navigation Flows:**
| Trigger/Action | Navigates To |
|---------------|--------------|
| PF3 | → CDEMO-FROM-PROGRAM (Previous screen) |
| PF4 | → COTRN02C (Clear screen) |
| ENTER with Transaction Data | → COTRN02C (Process and confirm) |
| Add Confirmation | → CDEMO-FROM-PROGRAM (Return to caller) |
| Validation Error | → COTRN02C (Redisplay with error) |

### 6. User Management Module (Admin Only)

#### COUSR00C - User List
- **Screen ID**: COUSR00C
- **Transaction ID**: CU00
- **Map**: COUSR0A / COUSR00
- **Function**: List users with pagination

**Navigation Flows:**
| Trigger/Action | Navigates To |
|---------------|--------------|
| PF3 | → COADM01C (Admin menu) |
| PF7 | → COUSR00C (Previous page) |
| PF8 | → COUSR00C (Next page) |
| Selection 'U' | → COUSR02C (Update user) |
| Selection 'D' | → COUSR03C (Delete user) |
| ENTER with User ID | → COUSR00C (Filter by user) |
| Invalid Selection | → COUSR00C (Redisplay with error) |

#### COUSR01C - Add User
- **Screen ID**: COUSR01C
- **Transaction ID**: CU01
- **Map**: COUSR1A / COUSR01
- **Function**: Add new user

**Navigation Flows:**
| Trigger/Action | Navigates To |
|---------------|--------------|
| PF3 | → COADM01C (Admin menu) |
| PF4 | → COUSR01C (Clear screen) |
| ENTER with User Data | → COUSR01C (Process and confirm) |
| Add Confirmation | → COUSR01C (Clear for next entry) |
| Validation Error | → COUSR01C (Redisplay with error) |
| Duplicate User ID | → COUSR01C (Redisplay with error) |

#### COUSR02C - Update User
- **Screen ID**: COUSR02C
- **Transaction ID**: CU02
- **Map**: COUSR2A / COUSR02
- **Function**: Update user information

**Navigation Flows:**
| Trigger/Action | Navigates To |
|---------------|--------------|
| PF3 | → CDEMO-FROM-PROGRAM (Previous screen) |
| PF4 | → COUSR02C (Clear screen) |
| PF5 | → COUSR00C (Return to user list) |
| ENTER with Updates | → COUSR02C (Process and confirm) |
| Update Confirmation | → CDEMO-FROM-PROGRAM (Return to caller) |
| Validation Error | → COUSR02C (Redisplay with error) |

#### COUSR03C - Delete User
- **Screen ID**: COUSR03C
- **Transaction ID**: CU03
- **Map**: COUSR3A / COUSR03
- **Function**: Delete user

**Navigation Flows:**
| Trigger/Action | Navigates To |
|---------------|--------------|
| PF3 | → CDEMO-FROM-PROGRAM (Previous screen) |
| PF4 | → COUSR03C (Clear screen) |
| PF5 | → COUSR00C (Return to user list) |
| ENTER with User ID | → COUSR03C (Display user for confirmation) |
| Delete Confirmation | → CDEMO-FROM-PROGRAM (Return to caller) |
| User Not Found | → COUSR03C (Redisplay with error) |

### 7. Bill Payment Module

#### COBIL00C - Bill Payment
- **Screen ID**: COBIL00C
- **Transaction ID**: CBIL
- **Map**: COBIL0A / COBIL00
- **Function**: Process bill payments

**Navigation Flows:**
| Trigger/Action | Navigates To |
|---------------|--------------|
| PF3 | → CDEMO-FROM-PROGRAM (Previous screen) |
| PF4 | → COBIL00C (Clear screen) |
| ENTER with Payment Data | → COBIL00C (Process payment) |
| Payment Confirmation | → CDEMO-FROM-PROGRAM (Return to caller) |
| Validation Error | → COBIL00C (Redisplay with error) |

### 8. Reports Module

#### CORPT00C - Reports
- **Screen ID**: CORPT00C
- **Transaction ID**: CRPT
- **Map**: CORPT0A / CORPT00
- **Function**: Generate and submit reports

**Navigation Flows:**
| Trigger/Action | Navigates To |
|---------------|--------------|
| PF3 | → CDEMO-FROM-PROGRAM (Previous screen) |
| ENTER with Report Parameters | → CORPT00C (Submit batch job) |
| Job Submission Confirmation | → CDEMO-FROM-PROGRAM (Return to caller) |
| Validation Error | → CORPT00C (Redisplay with error) |

## Standard PF Key Navigation

All screens in the CardDemo application follow consistent PF key navigation patterns:

| PF Key | Function | Navigation |
|--------|----------|------------|
| PF3 | Exit | Return to previous screen or main menu |
| PF4 | Clear | Clear current screen fields |
| PF5 | Return | Return to list screen (where applicable) |
| PF7 | Page Up | Previous page in paginated lists |
| PF8 | Page Down | Next page in paginated lists |
| ENTER | Process | Process current screen input |

## Selection Codes

List screens use standard selection codes for user actions:

| Code | Action | Description |
|------|--------|-------------|
| S | Select/View | Display detailed information |
| U | Update | Modify existing record |
| D | Delete | Remove record (admin only) |

## Technical Implementation Notes

### CICS Commands Used
- **EXEC CICS XCTL**: Program-to-program transfer with COMMAREA
- **EXEC CICS SEND MAP**: Display screen to user
- **EXEC CICS RECEIVE MAP**: Receive user input
- **EXEC CICS RETURN**: Return control to CICS with transaction continuation

### Context Preservation
- **CARDDEMO-COMMAREA**: Main communication area passed between programs
- **CDEMO-FROM-PROGRAM**: Tracks calling program for return navigation
- **CDEMO-TO-PROGRAM**: Specifies target program for navigation
- **CDEMO-PGM-CONTEXT**: Maintains program-specific state information

### Role-Based Access Control
- **CDEMO-USRTYP-ADMIN**: Admin user type flag
- **CDEMO-USRTYP-USER**: Regular user type flag
- Menu options filtered based on user type
- Admin-only functions restricted to admin users

## Conclusion

The CardDemo application implements a well-structured, hierarchical screen navigation system with consistent user interface patterns. The pseudo-conversational CICS design ensures efficient resource utilization while maintaining user context across screen transitions. Role-based access control provides appropriate security boundaries between administrative and regular user functions.

All navigation flows follow predictable patterns with standardized PF key handling, making the application intuitive for end users while maintaining robust business logic separation.
