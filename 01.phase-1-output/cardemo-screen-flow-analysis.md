# CardDemo COBOL Application - Screen Flow Analysis

## Overview

This document provides a comprehensive analysis of the business-level screen navigation flows in the CardDemo COBOL application. The analysis covers all 19 COBOL programs in the application, focusing on user-facing navigation paths and excluding internal technical validation logic.

## Application Architecture

The CardDemo application follows a **pseudo-conversational CICS design pattern** with:
- **Role-based authentication** routing users to appropriate menus
- **CICS XCTL commands** for seamless program-to-program navigation
- **BMS (Basic Mapping Support)** for screen handling
- **CARDDEMO-COMMAREA** for state management across screen transitions
- **Consistent PF key navigation patterns** across all screens

## Screen Flow Analysis

### Entry Point

#### COSGN00C - Sign-on Screen
**Function**: User authentication and role-based routing  
**Transaction ID**: CSGN  

**Navigation Flows**:
- **Valid Admin User Login** → COADM01C (Admin Menu)
- **Valid Regular User Login** → COMEN01C (Main Menu)  
- **PF3** → Exit Application (Thank you message)
- **Invalid Login** → Redisplay sign-on with error message

---

### Role-Based Menu System

#### COADM01C - Administrative Menu
**Function**: Main menu for administrative users  
**Transaction ID**: CADM  
**Access**: Admin users only

**Menu Options**:
- **Option 01** → COUSR00C (User List)
- **Option 02** → COUSR01C (Add User)
- **Option 03** → COACTVWC (Account View)
- **Option 04** → COCRDLIC (Card List)
- **PF3** → COSGN00C (Return to Sign-on)

#### COMEN01C - Main Menu  
**Function**: Main menu for regular users  
**Transaction ID**: CM00  
**Access**: Regular users

**Menu Options**:
- **Option 01** → COACTVWC (Account View)
- **Option 02** → COCRDLIC (Card List) 
- **Option 03** → COTRN00C (Transaction List)
- **Option 04** → COTRN02C (Add Transaction)
- **Option 05** → COBIL00C (Bill Payment)
- **Option 06** → CORPT00C (Reports)
- **Option 07** → COACTUPC (Account Update)
- **Option 08** → COCRDUPC (Card Update)
- **Option 09** → COTRN01C (Transaction View)
- **Option 10** → COTRN00C (Transaction List - alternate)
- **PF3** → COSGN00C (Return to Sign-on)

---

### Account Management Functions

#### COACTVWC - Account View
**Function**: Display account details and associated customer information  
**Transaction ID**: CAVW  

**Navigation Flows**:
- **ENTER** → Display account details for entered account number
- **PF3** → Return to calling program (COMEN01C or COADM01C)
- **Account Selection** → COCRDLIC (View associated cards)
- **Update Request** → COACTUPC (Account Update)

#### COACTUPC - Account Update  
**Function**: Update account information with validation  
**Transaction ID**: CAUP  

**Navigation Flows**:
- **ENTER** → Load account data for modification
- **PF3** → Return to calling program or main menu
- **PF4** → Clear current screen
- **Update Confirmation** → Save changes and return
- **Validation Error** → Redisplay with error messages

#### COBIL00C - Bill Payment
**Function**: Process bill payments for accounts  
**Transaction ID**: CBIL  

**Navigation Flows**:
- **ENTER** → Process payment transaction
- **PF3** → Return to calling program (COMEN01C)
- **PF4** → Clear current screen
- **Payment Confirmation** → Complete transaction and display confirmation

---

### Card Management Functions

#### COCRDLIC - Credit Card List
**Function**: Display paginated list of credit cards with selection options  
**Transaction ID**: CCLI  

**Navigation Flows**:
- **ENTER** → Display card list (filtered by account if specified)
- **S + Card Selection** → COCRDSLC (Card Detail View)
- **U + Card Selection** → COCRDUPC (Card Update)
- **PF3** → Return to calling program (COMEN01C or COADM01C)
- **PF7** → Previous page
- **PF8** → Next page

#### COCRDSLC - Credit Card Detail View
**Function**: Display detailed information for a specific credit card  
**Transaction ID**: CCDL  

**Navigation Flows**:
- **ENTER** → Display card details for entered card number
- **PF3** → Return to calling program (COCRDLIC)
- **PF4** → Clear current screen
- **Update Request** → COCRDUPC (Card Update)

#### COCRDUPC - Credit Card Update
**Function**: Update credit card information with comprehensive validation  
**Transaction ID**: CCUP  

**Navigation Flows**:
- **ENTER** → Load card data for modification
- **PF3** → Return to calling program or main menu
- **PF4** → Clear current screen
- **Update Confirmation** → Save changes and return
- **Validation Error** → Redisplay with error messages

---

### Transaction Management Functions

#### COTRN00C - Transaction List
**Function**: Display paginated list of transactions with selection options  
**Transaction ID**: CT00  

**Navigation Flows**:
- **ENTER** → Display transaction list (optionally filtered by transaction ID)
- **S + Transaction Selection** → COTRN01C (Transaction View)
- **PF3** → COMEN01C (Return to Main Menu)
- **PF7** → Previous page
- **PF8** → Next page

#### COTRN01C - Transaction View
**Function**: Display detailed information for a specific transaction  
**Transaction ID**: CT01  

**Navigation Flows**:
- **ENTER** → Display transaction details for entered transaction ID
- **PF3** → Return to calling program (COTRN00C or COMEN01C)
- **PF4** → Clear current screen
- **PF5** → COTRN00C (Return to Transaction List)

#### COTRN02C - Add Transaction
**Function**: Add new transaction with comprehensive validation  
**Transaction ID**: CT02  

**Navigation Flows**:
- **ENTER** → Validate and process new transaction
- **Account ID Entry** → Auto-populate card number from cross-reference
- **Card Number Entry** → Auto-populate account ID from cross-reference
- **Confirmation (Y)** → Save transaction and display success message
- **PF3** → Return to calling program (COMEN01C)
- **PF4** → Clear current screen
- **PF5** → Copy data from last transaction
- **Validation Error** → Redisplay with error messages

---

### User Administration Functions (Admin Only)

#### COUSR00C - User List
**Function**: Display paginated list of users with selection options  
**Transaction ID**: CU00  
**Access**: Admin users only

**Navigation Flows**:
- **ENTER** → Display user list (optionally filtered by user ID)
- **U + User Selection** → COUSR02C (User Update)
- **D + User Selection** → COUSR03C (User Delete)
- **PF3** → COADM01C (Return to Admin Menu)
- **PF7** → Previous page
- **PF8** → Next page

#### COUSR01C - Add User
**Function**: Add new user to the system  
**Transaction ID**: CU01  
**Access**: Admin users only

**Navigation Flows**:
- **ENTER** → Validate and create new user
- **PF3** → COADM01C (Return to Admin Menu)
- **PF4** → Clear current screen
- **User Creation Success** → Display confirmation and clear form
- **Validation Error** → Redisplay with error messages

#### COUSR02C - Update User
**Function**: Update existing user information  
**Transaction ID**: CU02  
**Access**: Admin users only

**Navigation Flows**:
- **ENTER** → Load user data for modification
- **PF3** → Save changes and return to calling program (COUSR00C or COADM01C)
- **PF4** → Clear current screen
- **PF5** → Save changes without exiting
- **PF12** → COADM01C (Return to Admin Menu)
- **Update Success** → Display confirmation message
- **Validation Error** → Redisplay with error messages

#### COUSR03C - Delete User
**Function**: Delete user from the system  
**Transaction ID**: CU03  
**Access**: Admin users only

**Navigation Flows**:
- **ENTER** → Load user data for deletion confirmation
- **PF3** → Return to calling program (COUSR00C or COADM01C)
- **PF4** → Clear current screen
- **PF5** → Confirm deletion and remove user
- **PF12** → COADM01C (Return to Admin Menu)
- **Deletion Success** → Display confirmation and clear form
- **User Not Found** → Display error message

---

### Reporting Functions

#### CORPT00C - Report Generation
**Function**: Generate transaction reports via batch job submission  
**Transaction ID**: CR00  

**Navigation Flows**:
- **Monthly Report Selection** → Submit monthly transaction report job
- **Yearly Report Selection** → Submit yearly transaction report job  
- **Custom Report Selection** → Validate date range and submit custom report job
- **PF3** → COMEN01C (Return to Main Menu)
- **Date Validation Error** → Redisplay with error messages
- **Job Submission Success** → Display confirmation message

---

## Common Navigation Patterns

### PF Key Functions (Consistent Across All Screens)

| PF Key | Function | Description |
|--------|----------|-------------|
| **PF3** | Exit/Return | Return to previous screen or calling program |
| **PF4** | Clear | Clear current screen and reset input fields |
| **PF5** | Special | Screen-specific functions (save, copy, etc.) |
| **PF7** | Page Up | Navigate to previous page (list screens) |
| **PF8** | Page Down | Navigate to next page (list screens) |
| **PF12** | Main Menu | Return directly to main menu (some screens) |

### List Screen Selection Codes

| Code | Function | Description |
|------|----------|-------------|
| **S** | Select/View | View detailed information for selected record |
| **U** | Update | Modify selected record |
| **D** | Delete | Remove selected record (admin functions only) |

### State Management

- **CARDDEMO-COMMAREA**: Primary communication area passed between programs
- **Program Context**: Each program maintains specific context sections (CT00, CT01, CU00, etc.)
- **Navigation History**: FROM-PROGRAM and TO-PROGRAM fields track navigation paths
- **Session State**: User type, authentication status, and current operation context

### Error Handling Patterns

- **Field Validation**: Real-time validation with cursor positioning on error fields
- **Error Messages**: Consistent error message display at screen bottom
- **Recovery Actions**: Clear error states and allow user correction
- **Graceful Degradation**: Return to previous screen on critical errors

## Technical Implementation Notes

### CICS Commands Used for Navigation
- **EXEC CICS XCTL**: Program-to-program control transfer
- **EXEC CICS RETURN**: Return control to CICS with transaction continuation
- **EXEC CICS SEND/RECEIVE**: BMS map handling for screen I/O

### File Operations Supporting Navigation
- **VSAM File Access**: Account, Customer, Card, Transaction, and User data
- **Cross-Reference Lookups**: CXACAIX for account-card relationships
- **State Persistence**: Communication area management across pseudo-conversations

### Security and Access Control
- **Role-Based Access**: Admin vs. regular user function segregation
- **Authentication Validation**: User credentials verified at sign-on
- **Function Authorization**: Admin-only screens protected by user type checks

## Conclusion

The CardDemo application implements a comprehensive screen navigation system with:
- **19 interactive programs** covering complete business functionality
- **Role-based menu structures** with appropriate access controls
- **Consistent navigation patterns** using PF keys and selection codes
- **Robust state management** through CICS communication areas
- **User-friendly error handling** with clear recovery paths

This analysis provides the foundation for understanding user interaction flows and can serve as documentation for system maintenance, enhancement, or migration activities.
