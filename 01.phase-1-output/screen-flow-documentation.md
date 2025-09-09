# CardDemo Application Screen Flow Documentation

## Overview

The CardDemo application is a comprehensive credit card management system built using CICS COBOL. It implements a pseudo-conversational design pattern with role-based access control, supporting both regular users and administrators through a structured menu-driven interface.

### Application Architecture

- **Entry Point**: COSGN00C (Sign-on Screen)
- **User Types**: Regular Users and Admin Users
- **Navigation Pattern**: Menu-driven with PF key navigation
- **State Management**: COMMAREA-based session persistence
- **Screen Communication**: CICS XCTL program transfers

## Screen Flow Mappings

### Authentication & Main Navigation

| Screen ID | Screen Title | Trigger/Option Value | Navigates To | PF Key Actions |
|-----------|--------------|---------------------|--------------|----------------|
| COSGN00C | Sign-on Screen | Valid Admin Credentials | COADM01C | PF3=Exit Application |
| COSGN00C | Sign-on Screen | Valid User Credentials | COMEN01C | PF3=Exit Application |
| COMEN01C | Main Menu (Regular Users) | Option 1-11 | Various Programs | PF3=Exit to Sign-on |
| COADM01C | Admin Menu | Option 1-6 | Various Programs | PF3=Exit to Sign-on |

### Transaction Management Flow

| Screen ID | Screen Title | Trigger/Option Value | Navigates To | PF Key Actions |
|-----------|--------------|---------------------|--------------|----------------|
| COTRN00C | Transaction List | Menu Option | Display List | PF3=Main Menu, PF7/8=Page Up/Down |
| COTRN00C | Transaction List | S + Transaction ID | COTRN01C | Enter=Select Transaction |
| COTRN01C | Transaction View | From Transaction List | Display Details | PF3=Return, PF4=Clear, PF5=Transaction List |
| COTRN02C | Add Transaction | Menu Option | Input Form | PF3=Return, PF4=Clear, PF5=Copy Last |

### User Management Flow (Admin Only)

| Screen ID | Screen Title | Trigger/Option Value | Navigates To | PF Key Actions |
|-----------|--------------|---------------------|--------------|----------------|
| COUSR00C | User List | Admin Menu Option | Display List | PF3=Admin Menu, PF7/8=Page Up/Down |
| COUSR00C | User List | U + User ID | COUSR02C | Enter=Select for Update |
| COUSR00C | User List | D + User ID | COUSR03C | Enter=Select for Delete |
| COUSR01C | Add User | Admin Menu Option | Input Form | PF3=Admin Menu, PF4=Clear |
| COUSR02C | Update User | From User List | Edit Form | PF3=Save & Return, PF4=Clear, PF5=Save, PF12=Admin Menu |
| COUSR03C | Delete User | From User List | Confirm Delete | PF3=Return, PF4=Clear, PF5=Delete, PF12=Admin Menu |

### Credit Card Management Flow

| Screen ID | Screen Title | Trigger/Option Value | Navigates To | PF Key Actions |
|-----------|--------------|---------------------|--------------|----------------|
| COCRDLIC | Credit Card List | Menu Option | Display List | PF3=Main Menu, PF7/8=Page Up/Down |
| COCRDLIC | Credit Card List | S + Card Selection | COCRDSLC | Enter=View Card Details |
| COCRDLIC | Credit Card List | U + Card Selection | COCRDUPC | Enter=Update Card |
| COCRDSLC | Card Details View | From Card List | Display Details | PF3=Return to List |
| COCRDUPC | Card Update | From Card List | Edit Form | PF3=Return, PF5=Save Changes |

### Account Management Flow

| Screen ID | Screen Title | Trigger/Option Value | Navigates To | PF Key Actions |
|-----------|--------------|---------------------|--------------|----------------|
| COACTVWC | Account View | Menu Option | Display Account | PF3=Main Menu |
| COACTUPC | Account Update | Menu Option | Edit Account | PF3=Return, PF5=Save Changes |

### Bill Payment Flow

| Screen ID | Screen Title | Trigger/Option Value | Navigates To | PF Key Actions |
|-----------|--------------|---------------------|--------------|----------------|
| COBIL00C | Bill Payment | Menu Option | Payment Form | PF3=Main Menu, PF4=Clear |
| COBIL00C | Bill Payment | Y (Confirm Payment) | Process Payment | Enter=Confirm Transaction |

### Reporting Flow

| Screen ID | Screen Title | Trigger/Option Value | Navigates To | PF Key Actions |
|-----------|--------------|---------------------|--------------|----------------|
| CORPT00C | Transaction Reports | Menu Option | Report Options | PF3=Main Menu |
| CORPT00C | Transaction Reports | Monthly/Yearly/Custom | Submit Batch Job | Enter=Generate Report |

## User Journey Paths

### Regular User Journey

1. **Authentication**: COSGN00C → Enter User ID/Password
2. **Main Menu**: COMEN01C → Select from 11 available options
3. **Functional Screens**: Navigate to specific functions based on menu selection
4. **Return Navigation**: Use PF3 to return to previous screen or PF12 for main menu
5. **Exit**: PF3 from main menu returns to sign-on screen

### Admin User Journey

1. **Authentication**: COSGN00C → Enter Admin credentials
2. **Admin Menu**: COADM01C → Select from 6 administrative options
3. **User Management**: Access CRUD operations for user accounts
4. **System Functions**: Access all regular user functions plus administrative features
5. **Return Navigation**: Use PF3 to return or PF12 for admin menu

## Navigation Patterns

### Standard PF Key Functions

- **PF3**: Exit/Return to previous screen or main menu
- **PF4**: Clear current screen fields
- **PF5**: Save/Action (context-dependent)
- **PF7**: Page up in list screens
- **PF8**: Page down in list screens
- **PF12**: Return to main menu (where applicable)

### Selection Processing

- **S**: Select record for viewing
- **U**: Select record for updating
- **D**: Select record for deletion (admin only)
- **Y/N**: Confirmation prompts for critical actions
- **Numeric Options**: Menu selection (1-11 for regular menu, 1-6 for admin menu)

### List Screen Navigation

All list screens (COTRN00C, COUSR00C, COCRDLIC) implement consistent pagination:
- Display 10 records per page
- PF7/PF8 for forward/backward navigation
- Selection fields for record-specific actions
- Automatic page boundary detection

## Technical Implementation Details

### Program Communication

- **CICS XCTL**: Used for all program-to-program transfers
- **COMMAREA**: Maintains session state and context between programs
- **Pseudo-conversational**: Programs terminate after sending screens, restart on user input

### State Management

Each program maintains context through specific COMMAREA sections:
- **CDEMO-CT00-INFO**: Transaction list context
- **CDEMO-CU00-INFO**: User management context  
- **CDEMO-CB00-INFO**: Bill payment context
- **Program-specific data**: Maintains screen state and selected records

### Error Handling

Consistent error handling patterns across all programs:
- Field-level validation with cursor positioning
- User-friendly error messages
- Screen redisplay with error indicators
- Graceful handling of file operation failures

### Security Implementation

- **User Type Validation**: Admin vs Regular user access control
- **Menu Option Filtering**: Options displayed based on user permissions
- **Program Access Control**: Admin-only programs protected at entry point
- **Session Management**: User context maintained throughout session

## Business Process Flows

### Transaction Processing

1. **List Transactions**: Browse paginated transaction history
2. **View Details**: Display complete transaction information
3. **Add New**: Create new transactions with validation
4. **Navigation**: Seamless movement between list and detail views

### User Administration

1. **User Listing**: Browse all system users with pagination
2. **Add Users**: Create new user accounts with role assignment
3. **Update Users**: Modify existing user information
4. **Delete Users**: Remove user accounts with confirmation
5. **Access Control**: All functions restricted to admin users

### Card Management

1. **Card Listing**: Display cards with account association
2. **View Details**: Show complete card information
3. **Update Cards**: Modify card details with validation
4. **Status Management**: Control card active/inactive status

### Account Operations

1. **Account View**: Display account details and balances
2. **Account Update**: Modify account information
3. **Bill Payment**: Process payments against account balances
4. **Transaction Integration**: Link with transaction processing

### Report Generation

1. **Report Selection**: Choose from predefined report types
2. **Parameter Entry**: Specify date ranges for custom reports
3. **Batch Submission**: Submit background jobs for report generation
4. **Job Management**: Integration with mainframe batch processing

## Screen Transition Matrix

| From Screen | To Screen | Trigger | Return Path |
|-------------|-----------|---------|-------------|
| COSGN00C | COMEN01C | User Login | PF3 |
| COSGN00C | COADM01C | Admin Login | PF3 |
| COMEN01C | COTRN00C | Menu Option | PF3 |
| COMEN01C | COCRDLIC | Menu Option | PF3 |
| COMEN01C | COACTVWC | Menu Option | PF3 |
| COMEN01C | COBIL00C | Menu Option | PF3 |
| COMEN01C | CORPT00C | Menu Option | PF3 |
| COADM01C | COUSR00C | Menu Option | PF3 |
| COTRN00C | COTRN01C | S + Selection | PF3, PF5 |
| COTRN00C | COTRN02C | Menu Option | PF3 |
| COUSR00C | COUSR01C | Menu Option | PF3 |
| COUSR00C | COUSR02C | U + Selection | PF3, PF12 |
| COUSR00C | COUSR03C | D + Selection | PF3, PF12 |
| COCRDLIC | COCRDSLC | S + Selection | PF3 |
| COCRDLIC | COCRDUPC | U + Selection | PF3 |

## Conclusion

The CardDemo application implements a well-structured screen flow architecture that provides intuitive navigation for both regular users and administrators. The consistent use of PF keys, menu-driven navigation, and role-based access control creates a user-friendly interface while maintaining security and data integrity. The pseudo-conversational design ensures efficient resource utilization while providing seamless user experience across all functional areas.
