# CardDemo COBOL Application - Business User Stories

This document contains business-level user stories extracted from the CardDemo COBOL credit card management system. These stories focus on end-user interactions and business value, suitable for modernizing the system into web or mobile interfaces.

## Authentication & Access Management

### User Authentication
**As a** customer  
**I want to** sign into the CardDemo system with my user ID and password  
**So that** I can securely access my account information and perform banking operations

**As a** customer  
**I want to** receive clear error messages when I enter invalid credentials  
**So that** I understand what went wrong and can correct my login attempt

**As a** customer  
**I want to** be automatically directed to the appropriate menu based on my user type  
**So that** I see only the functions relevant to my role

**As a** system administrator  
**I want to** be automatically directed to the admin menu when I sign in  
**So that** I can access administrative functions and user management tools

**As a** customer  
**I want to** be able to sign out of the system securely  
**So that** I can protect my account information when I'm finished

## Navigation & Menu Systems

### Main Menu Navigation
**As a** regular user  
**I want to** see a main menu with numbered options for different banking functions  
**So that** I can easily navigate to the services I need

**As a** regular user  
**I want to** select menu options by entering the corresponding number  
**So that** I can quickly access specific banking functions

**As a** regular user  
**I want to** receive feedback when I select an invalid menu option  
**So that** I understand what options are available to me

**As a** regular user  
**I want to** be prevented from accessing admin-only functions  
**So that** the system maintains proper security boundaries

### Administrative Menu Navigation
**As a** system administrator  
**I want to** see an administrative menu with options for user management and system functions  
**So that** I can perform administrative tasks efficiently

**As a** system administrator  
**I want to** navigate between different administrative functions using numbered menu options  
**So that** I can manage users, generate reports, and maintain the system

**As a** system administrator  
**I want to** return to the main menu or sign out from any administrative function  
**So that** I can efficiently move between different tasks

## Account Management

### Account Information Viewing
**As a** customer  
**I want to** view my account details including balance and account information  
**So that** I can monitor my financial status and account activity

**As a** customer  
**I want to** see my current account balance and available credit  
**So that** I can make informed decisions about my spending

**As a** customer  
**I want to** view my account holder information and contact details  
**So that** I can verify my personal information is current

**As a** customer  
**I want to** filter and search for specific account information  
**So that** I can quickly find the details I need

### Account Information Updates
**As a** customer  
**I want to** update my account information such as contact details  
**So that** I can keep my personal information current

**As a** customer  
**I want to** receive validation messages when updating account information  
**So that** I can ensure the information I enter is correct and complete

**As a** customer  
**I want to** confirm changes before they are saved to my account  
**So that** I can review and verify updates before they become permanent

**As a** customer  
**I want to** receive confirmation when my account updates are successfully saved  
**So that** I know my changes have been processed

## Credit Card Management

### Credit Card Portfolio View
**As a** customer  
**I want to** see a list of all my credit cards  
**So that** I can manage my entire card portfolio from one location

**As a** customer  
**I want to** view basic information for each card including card number and status  
**So that** I can quickly identify and select the card I want to manage

**As a** customer  
**I want to** navigate through multiple pages of cards if I have many  
**So that** I can access all my cards regardless of how many I have

**As a** system administrator  
**I want to** view all credit cards in the system  
**So that** I can monitor and manage the entire card portfolio

### Credit Card Details
**As a** customer  
**I want to** view detailed information for a specific credit card  
**So that** I can see all relevant details about that card

**As a** customer  
**I want to** see card details including expiration date, status, and associated account  
**So that** I can verify card information and understand its current state

**As a** customer  
**I want to** view the cardholder name and other identifying information  
**So that** I can confirm the card details are correct

### Credit Card Updates
**As a** customer  
**I want to** update my credit card information such as the name embossed on the card  
**So that** I can keep my card information current

**As a** customer  
**I want to** change my card status (activate/deactivate)  
**So that** I can control when my card can be used

**As a** customer  
**I want to** update card expiration dates when I receive a new card  
**So that** I can ensure my card information is accurate

**As a** customer  
**I want to** receive validation when updating card information  
**So that** I can ensure the changes I make are valid and properly formatted

**As a** customer  
**I want to** confirm card updates before they are saved  
**So that** I can review changes before they become permanent

## Transaction Processing

### Transaction History Viewing
**As a** customer  
**I want to** view a list of my recent transactions  
**So that** I can monitor my spending and account activity

**As a** customer  
**I want to** see transaction details including amount, date, and merchant information  
**So that** I can understand where and when I made purchases

**As a** customer  
**I want to** navigate through multiple pages of transaction history  
**So that** I can review all my transactions over time

**As a** customer  
**I want to** select specific transactions to view more details  
**So that** I can get complete information about individual purchases

### Transaction Detail View
**As a** customer  
**I want to** view complete details for a specific transaction  
**So that** I can see all information about a particular purchase

**As a** customer  
**I want to** see transaction details including merchant name, location, and transaction type  
**So that** I can verify the transaction and understand the purchase context

**As a** customer  
**I want to** view transaction processing dates and amounts  
**So that** I can track when transactions were processed and for how much

**As a** customer  
**I want to** see the card number used for each transaction  
**So that** I can verify which card was used for specific purchases

### Transaction Entry
**As a** authorized user  
**I want to** add new transactions to the system  
**So that** I can record purchases and payments

**As a** authorized user  
**I want to** enter transaction details including amount, merchant, and card information  
**So that** I can create complete transaction records

**As a** authorized user  
**I want to** validate transaction information before it's saved  
**So that** I can ensure transaction data is accurate and complete

**As a** authorized user  
**I want to** receive confirmation when transactions are successfully added  
**So that** I know the transaction has been properly recorded

## Bill Payment

### Bill Payment Processing
**As a** customer  
**I want to** pay my account balance in full  
**So that** I can clear my outstanding debt and avoid interest charges

**As a** customer  
**I want to** see my current balance before making a payment  
**So that** I can confirm the amount I need to pay

**As a** customer  
**I want to** confirm payment details before processing  
**So that** I can verify the payment amount and account information

**As a** customer  
**I want to** receive confirmation when my payment is processed  
**So that** I know my payment was successful and my balance has been updated

**As a** customer  
**I want to** have a transaction record created for my bill payment  
**So that** I can track my payment history and have proof of payment

**As a** customer  
**I want to** see my updated account balance after making a payment  
**So that** I can verify the payment was applied correctly

## User Administration

### User Management Overview
**As a** system administrator  
**I want to** view a list of all system users  
**So that** I can monitor user accounts and manage access

**As a** system administrator  
**I want to** see user information including user ID, name, and user type  
**So that** I can identify users and understand their access levels

**As a** system administrator  
**I want to** navigate through multiple pages of users  
**So that** I can manage all users regardless of how many are in the system

**As a** system administrator  
**I want to** select specific users for detailed management actions  
**So that** I can perform administrative tasks on individual accounts

### User Creation
**As a** system administrator  
**I want to** add new users to the system  
**So that** I can provide access to new customers and staff members

**As a** system administrator  
**I want to** enter user details including name, user ID, password, and user type  
**So that** I can create complete user profiles with appropriate access levels

**As a** system administrator  
**I want to** validate user information before creating accounts  
**So that** I can ensure user data is complete and properly formatted

**As a** system administrator  
**I want to** prevent duplicate user IDs when creating accounts  
**So that** I can maintain unique user identification in the system

**As a** system administrator  
**I want to** receive confirmation when users are successfully created  
**So that** I know new accounts have been properly established

### User Updates
**As a** system administrator  
**I want to** update existing user information  
**So that** I can maintain current and accurate user records

**As a** system administrator  
**I want to** modify user details such as name, password, and user type  
**So that** I can keep user accounts current and adjust access levels as needed

**As a** system administrator  
**I want to** validate changes before updating user records  
**So that** I can ensure modifications are appropriate and properly formatted

### User Deletion
**As a** system administrator  
**I want to** remove users from the system  
**So that** I can deactivate accounts for users who no longer need access

**As a** system administrator  
**I want to** confirm user deletion before removing accounts  
**So that** I can prevent accidental deletion of active user accounts

**As a** system administrator  
**I want to** receive confirmation when users are successfully deleted  
**So that** I know accounts have been properly removed from the system

## Reporting & Analytics

### Transaction Reporting
**As a** system administrator  
**I want to** generate transaction reports for specific date ranges  
**So that** I can analyze transaction patterns and business performance

**As a** system administrator  
**I want to** specify start and end dates for transaction reports  
**So that** I can focus on specific time periods for analysis

**As a** system administrator  
**I want to** submit report generation requests that run as background jobs  
**So that** I can generate large reports without impacting system performance

**As a** system administrator  
**I want to** receive confirmation when report generation jobs are submitted  
**So that** I know my report request has been queued for processing

**As a** system administrator  
**I want to** validate date ranges before submitting report requests  
**So that** I can ensure report parameters are correct and properly formatted

**As a** business analyst  
**I want to** access transaction reports to analyze spending patterns  
**So that** I can understand customer behavior and business trends

**As a** business analyst  
**I want to** review transaction data across different time periods  
**So that** I can identify trends and make informed business decisions

## Utility Functions

### Date Validation
**As a** user entering dates in any part of the system  
**I want to** receive validation feedback for date entries  
**So that** I can ensure dates are properly formatted and valid

**As a** user  
**I want to** be guided on proper date formats when entering dates  
**So that** I can successfully complete date-related transactions

**As a** user  
**I want to** receive clear error messages for invalid dates  
**So that** I can correct date entries and proceed with my tasks

---

## Summary

This CardDemo application provides comprehensive credit card management capabilities for both customers and administrators. The system supports:

- **Customer Self-Service**: Account viewing, card management, transaction history, and bill payments
- **Administrative Functions**: User management, system administration, and business reporting
- **Security & Access Control**: Role-based access with separate customer and admin interfaces
- **Transaction Processing**: Complete transaction lifecycle from entry to reporting
- **Data Management**: Comprehensive validation and error handling for all user inputs

These user stories represent the core business value of the CardDemo system and provide a foundation for modernizing the application into contemporary web or mobile interfaces while preserving the essential banking and credit card management functionality.
