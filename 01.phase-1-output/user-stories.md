# CardDemo Business User Stories

This document contains business-level user stories extracted from the CardDemo COBOL codebase. These stories focus on end-user functionality and business value, suitable for modernizing the system into a user-centric web or mobile application.

## Authentication & Access Control

### User Authentication
**As a** customer  
**I want to** log into the system with my user ID and password  
**So that** I can securely access my account information and perform banking operations

**As a** banking staff member  
**I want to** log into the system with administrative credentials  
**So that** I can access administrative functions and manage customer accounts

**As a** system user  
**I want to** be automatically redirected to the appropriate menu based on my user type  
**So that** I only see functions relevant to my role and responsibilities

**As a** user  
**I want to** receive clear error messages when I enter invalid credentials  
**So that** I understand what went wrong and can correct my login attempt

**As a** user  
**I want to** be able to exit the system securely  
**So that** my session is properly terminated and my account remains secure

## Account Management

### Account Information Access
**As a** customer  
**I want to** view my account details including balance and account limits  
**So that** I can monitor my financial status and available credit

**As a** customer  
**I want to** see my account ID, customer information, and current balance  
**So that** I can verify my account information is accurate

**As a** customer  
**I want to** filter and search for specific account information  
**So that** I can quickly find the details I need

### Account Information Updates
**As a** customer  
**I want to** update my account information such as contact details  
**So that** my records remain current and I receive important communications

**As a** customer  
**I want to** modify account settings and preferences  
**So that** my account reflects my current needs and circumstances

**As a** customer  
**I want to** receive confirmation when my account updates are successful  
**So that** I know my changes have been saved properly

**As a** customer  
**I want to** see validation errors if I enter invalid information  
**So that** I can correct mistakes before submitting my updates

## Credit Card Management

### Card Information Access
**As a** customer  
**I want to** view a list of all my credit cards  
**So that** I can see all cards associated with my account

**As a** customer  
**I want to** see detailed information for each credit card including card number, limits, and status  
**So that** I can manage my cards effectively

**As a** customer  
**I want to** filter my card list by account or card criteria  
**So that** I can quickly find specific cards

**As an** administrator  
**I want to** view all credit cards in the system  
**So that** I can monitor and manage the card portfolio

### Card Management Operations
**As a** customer  
**I want to** select a specific card to view or update  
**So that** I can manage individual card settings

**As a** customer  
**I want to** update card information such as limits or status  
**So that** my cards meet my current needs

**As a** customer  
**I want to** navigate between different cards easily  
**So that** I can efficiently manage multiple cards

**As a** customer  
**I want to** see card details organized clearly with account relationships  
**So that** I understand how my cards connect to my accounts

## Transaction Management

### Transaction History Access
**As a** customer  
**I want to** browse through my transaction history  
**So that** I can review my spending and account activity

**As a** customer  
**I want to** view transactions in a paginated list  
**So that** I can navigate through large numbers of transactions efficiently

**As a** customer  
**I want to** see transaction details including amount, date, merchant, and category  
**So that** I can understand each transaction completely

**As a** customer  
**I want to** navigate forward and backward through transaction pages  
**So that** I can review my complete transaction history

### Individual Transaction Details
**As a** customer  
**I want to** view detailed information for a specific transaction  
**So that** I can see all available details about a particular purchase or payment

**As a** customer  
**I want to** see transaction details including merchant information, location, and processing dates  
**So that** I can verify transaction accuracy and identify any discrepancies

**As a** customer  
**I want to** search for transactions by transaction ID  
**So that** I can quickly locate specific transactions

**As a** customer  
**I want to** return to the transaction list from transaction details  
**So that** I can continue browsing other transactions

### Transaction Entry
**As a** customer  
**I want to** add new transactions to my account  
**So that** I can record purchases or payments that may not have been automatically captured

**As a** customer  
**I want to** enter transaction details including amount, merchant, and category  
**So that** my transaction records are complete and accurate

**As a** customer  
**I want to** validate transaction information before saving  
**So that** I avoid errors in my transaction history

**As a** customer  
**I want to** receive confirmation when transactions are successfully added  
**So that** I know my transaction has been recorded

## Bill Payment

### Payment Processing
**As a** customer  
**I want to** pay my account balance in full  
**So that** I can clear my outstanding debt and avoid interest charges

**As a** customer  
**I want to** see my current account balance before making a payment  
**So that** I know exactly how much I owe

**As a** customer  
**I want to** confirm payment details before processing  
**So that** I can verify the payment amount and account information

**As a** customer  
**I want to** receive confirmation when my payment is processed  
**So that** I have proof of payment and know my balance has been updated

**As a** customer  
**I want to** see my updated account balance after payment  
**So that** I can verify the payment was applied correctly

**As a** customer  
**I want to** have a transaction record created for my payment  
**So that** I have a complete history of all account activity

## User Administration (Admin Only)

### User Management Overview
**As an** administrator  
**I want to** view a list of all system users  
**So that** I can monitor user accounts and manage access

**As an** administrator  
**I want to** see user information including user ID, name, and user type  
**So that** I can identify users and their roles in the system

**As an** administrator  
**I want to** navigate through user lists with pagination  
**So that** I can efficiently manage large numbers of users

### User Account Creation
**As an** administrator  
**I want to** add new users to the system  
**So that** I can provide access to new customers and staff members

**As an** administrator  
**I want to** specify user details including name, user ID, password, and user type  
**So that** new users have proper credentials and appropriate access levels

**As an** administrator  
**I want to** validate user information before creating accounts  
**So that** all user accounts have complete and accurate information

**As an** administrator  
**I want to** receive confirmation when user accounts are created successfully  
**So that** I know the new user can access the system

**As an** administrator  
**I want to** prevent duplicate user IDs  
**So that** each user has a unique identifier in the system

### User Account Maintenance
**As an** administrator  
**I want to** update existing user information  
**So that** user records remain current and accurate

**As an** administrator  
**I want to** modify user access levels and permissions  
**So that** users have appropriate access based on their roles

**As an** administrator  
**I want to** delete user accounts when necessary  
**So that** I can remove access for users who no longer need system access

**As an** administrator  
**I want to** select specific users for detailed operations  
**So that** I can perform targeted user management tasks

## Reporting & Analytics

### Transaction Reporting
**As an** administrator  
**I want to** generate transaction reports for specified date ranges  
**So that** I can analyze transaction patterns and business performance

**As an** administrator  
**I want to** submit report generation requests  
**So that** I can obtain detailed transaction analysis without impacting system performance

**As an** administrator  
**I want to** specify report parameters including start and end dates  
**So that** I can focus reports on specific time periods of interest

**As an** administrator  
**I want to** initiate batch report processing  
**So that** large reports can be generated efficiently without blocking other operations

**As an** administrator  
**I want to** validate report parameters before submission  
**So that** I ensure reports will contain meaningful and accurate data

**As an** administrator  
**I want to** receive confirmation when report jobs are submitted  
**So that** I know my report request is being processed

## Navigation & User Experience

### Menu Navigation
**As a** customer  
**I want to** access a main menu with available options  
**So that** I can navigate to different functions easily

**As a** customer  
**I want to** see only the menu options appropriate for my user type  
**So that** I'm not confused by functions I cannot access

**As an** administrator  
**I want to** access an administrative menu with management functions  
**So that** I can perform administrative tasks efficiently

**As a** user  
**I want to** receive clear error messages for invalid menu selections  
**So that** I understand what options are available to me

### System Navigation
**As a** user  
**I want to** return to previous screens easily  
**So that** I can navigate through the system intuitively

**As a** user  
**I want to** clear current screen information when needed  
**So that** I can start fresh with new data entry

**As a** user  
**I want to** exit functions and return to main menus  
**So that** I can move between different system areas efficiently

**As a** user  
**I want to** have consistent navigation options across all screens  
**So that** I can predict how to move through the system

## Data Validation & Error Handling

### Input Validation
**As a** user  
**I want to** receive immediate feedback on invalid data entry  
**So that** I can correct errors before submitting information

**As a** user  
**I want to** see specific error messages that explain what needs to be corrected  
**So that** I can fix problems quickly and accurately

**As a** user  
**I want to** have required fields clearly identified  
**So that** I know what information is mandatory

**As a** user  
**I want to** receive validation for date formats and numeric fields  
**So that** I enter information in the correct format

### System Reliability
**As a** user  
**I want to** receive clear messages when system operations are successful  
**So that** I know my actions have been completed properly

**As a** user  
**I want to** be notified if system errors occur  
**So that** I understand when operations cannot be completed

**As a** user  
**I want to** have the system handle errors gracefully  
**So that** I can continue working even when problems occur

**As a** user  
**I want to** receive guidance on how to resolve error conditions  
**So that** I can take appropriate action to complete my tasks
