# CardDemo Business User Stories

This document contains business-level user stories extracted from the CardDemo COBOL codebase. These stories focus on end-user interactions and business value, suitable for modernizing the system into a web or mobile application.

## Authentication & Access Management

### User Authentication
**As a** customer or administrator  
**I want to** log into the system with my username and password  
**So that** I can securely access my account information and perform authorized operations

**As a** customer or administrator  
**I want to** be automatically routed to the appropriate interface based on my user type  
**So that** I only see features and functions relevant to my role

**As a** user  
**I want to** receive clear error messages when I enter invalid credentials  
**So that** I understand what went wrong and can correct my login attempt

**As a** user  
**I want to** be able to exit the application securely  
**So that** my session is properly terminated and my data remains protected

## Account Management

### Account Information Access
**As a** customer  
**I want to** view my account details including balance, credit limit, and account status  
**So that** I can monitor my financial position and account standing

**As a** customer  
**I want to** see my current balance, available credit, and recent activity  
**So that** I can make informed decisions about my spending and payments

### Account Updates
**As a** customer  
**I want to** update my personal information such as name, address, and contact details  
**So that** my account records remain current and accurate

**As a** customer  
**I want to** modify my account preferences and settings  
**So that** the system works according to my needs and preferences

**As a** customer  
**I want to** receive confirmation when my account changes are successfully saved  
**So that** I know my updates have been processed

**As a** customer  
**I want to** see validation errors immediately when I enter invalid information  
**So that** I can correct mistakes before submitting my changes

## Card Management

### Card Portfolio Overview
**As a** customer  
**I want to** view a list of all my credit cards  
**So that** I can see my complete card portfolio at a glance

**As a** customer  
**I want to** see key information for each card including card number, status, and expiration date  
**So that** I can quickly identify and manage specific cards

**As a** customer  
**I want to** navigate through multiple pages of cards if I have many  
**So that** I can access all my cards even with a large portfolio

### Card Details and Management
**As a** customer  
**I want to** view detailed information for a specific card  
**So that** I can see all relevant card data including limits, balances, and terms

**As a** customer  
**I want to** select a card from my list to view or update  
**So that** I can manage individual cards efficiently

**As a** customer  
**I want to** update card information such as name on card and expiration date  
**So that** my card details remain accurate and current

**As a** customer  
**I want to** change my card status (activate/deactivate)  
**So that** I can control when my cards can be used

**As a** customer  
**I want to** receive validation feedback when updating card information  
**So that** I enter correct data and avoid processing errors

**As a** customer  
**I want to** confirm changes before they are saved  
**So that** I can review and approve modifications to my card details

## Transaction Processing

### Transaction History
**As a** customer  
**I want to** view a list of my recent transactions  
**So that** I can monitor my spending and account activity

**As a** customer  
**I want to** see transaction details including date, amount, merchant, and description  
**So that** I can understand each charge on my account

**As a** customer  
**I want to** navigate through multiple pages of transaction history  
**So that** I can review older transactions beyond the most recent ones

**As a** customer  
**I want to** search for specific transactions by transaction ID  
**So that** I can quickly locate particular transactions

### Transaction Details
**As a** customer  
**I want to** view complete details for a specific transaction  
**So that** I can see all information including merchant details, processing dates, and transaction codes

**As a** customer  
**I want to** select a transaction from my history to view its details  
**So that** I can investigate specific charges or payments

### Transaction Entry
**As a** customer or administrator  
**I want to** add new transactions to an account  
**So that** I can record payments, charges, or adjustments

**As a** customer or administrator  
**I want to** enter transaction details including amount, type, description, and merchant information  
**So that** the transaction is properly categorized and documented

**As a** customer or administrator  
**I want to** validate transaction data before it is saved  
**So that** I ensure accuracy and prevent processing errors

**As a** customer or administrator  
**I want to** confirm transaction details before final submission  
**So that** I can review and approve the transaction before it is processed

## Bill Payment

### Payment Processing
**As a** customer  
**I want to** pay my account balance online  
**So that** I can conveniently make payments without visiting a branch or mailing checks

**As a** customer  
**I want to** see my current balance before making a payment  
**So that** I know exactly how much I owe

**As a** customer  
**I want to** pay my full balance with a single action  
**So that** I can quickly clear my account without calculating amounts

**As a** customer  
**I want to** confirm my payment before it is processed  
**So that** I can review the payment details and approve the transaction

**As a** customer  
**I want to** receive confirmation when my payment is successfully processed  
**So that** I know my payment has been received and applied to my account

**As a** customer  
**I want to** be prevented from making payments when my balance is zero  
**So that** I don't accidentally create overpayments or processing issues

## Reporting

### Transaction Reports
**As a** customer or administrator  
**I want to** generate monthly transaction reports  
**So that** I can review my spending patterns and account activity for a specific month

**As a** customer or administrator  
**I want to** generate yearly transaction reports  
**So that** I can analyze my annual spending and prepare for tax or budgeting purposes

**As a** customer or administrator  
**I want to** create custom transaction reports for specific date ranges  
**So that** I can analyze activity for any time period that meets my needs

**As a** customer or administrator  
**I want to** specify start and end dates for custom reports  
**So that** I can control exactly which transactions are included in my analysis

**As a** customer or administrator  
**I want to** receive validation when entering report parameters  
**So that** I provide valid dates and generate meaningful reports

**As a** customer or administrator  
**I want to** submit report requests that are processed in the background  
**So that** I can continue using the system while my reports are being generated

## User Administration (Admin Only)

### User Management Overview
**As an** administrator  
**I want to** view a list of all system users  
**So that** I can manage user accounts and monitor system access

**As an** administrator  
**I want to** see user information including name, user ID, and user type  
**So that** I can quickly identify and manage specific users

**As an** administrator  
**I want to** navigate through multiple pages of users  
**So that** I can manage all users even in a large organization

### User Account Creation
**As an** administrator  
**I want to** add new users to the system  
**So that** I can provide access to new customers or staff members

**As an** administrator  
**I want to** specify user details including name, user ID, password, and user type  
**So that** new users have proper credentials and appropriate access levels

**As an** administrator  
**I want to** validate user information before creating accounts  
**So that** I ensure all required information is provided and properly formatted

**As an** administrator  
**I want to** receive confirmation when new users are successfully created  
**So that** I know the account setup is complete

**As an** administrator  
**I want to** be prevented from creating duplicate user IDs  
**So that** each user has a unique identifier in the system

### User Account Maintenance
**As an** administrator  
**I want to** update existing user information  
**So that** I can maintain accurate user records and modify access as needed

**As an** administrator  
**I want to** delete user accounts when they are no longer needed  
**So that** I can maintain system security and remove inactive accounts

**As an** administrator  
**I want to** select users from the list to update or delete  
**So that** I can efficiently manage specific user accounts

## Navigation and System Access

### Menu Navigation
**As a** customer  
**I want to** access a main menu with all available functions  
**So that** I can easily navigate to different areas of the system

**As an** administrator  
**I want to** access an administrative menu with management functions  
**So that** I can perform administrative tasks and system management

**As a** user  
**I want to** see only the menu options appropriate for my user type  
**So that** I am not confused by functions I cannot access

**As a** user  
**I want to** select menu options by number or name  
**So that** I can quickly navigate to the functions I need

**As a** user  
**I want to** return to previous screens or the main menu  
**So that** I can navigate back through the system hierarchy

### System Interaction
**As a** user  
**I want to** receive clear error messages when I make invalid selections  
**So that** I understand what went wrong and can correct my actions

**As a** user  
**I want to** clear screens and start over when needed  
**So that** I can reset forms and begin fresh data entry

**As a** user  
**I want to** have consistent navigation controls across all screens  
**So that** I can predict how to move through the system

---

## Summary

These user stories represent the core business functionality of the CardDemo credit card management system. They focus on the value delivered to end users - both customers managing their accounts and administrators managing the system. Each story describes what users want to accomplish and why, providing a foundation for modernizing the legacy COBOL system into a contemporary web or mobile application.

The stories are organized by functional area to support modular development and implementation planning. They emphasize self-service capabilities, real-time feedback, and user-friendly interactions that would be expected in a modern financial services application.
