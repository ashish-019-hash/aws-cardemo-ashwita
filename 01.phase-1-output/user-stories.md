# CardDemo Application - Business User Stories

This document contains business-level user stories extracted from the CardDemo COBOL codebase. These stories focus on end-user business actions and goals, excluding technical implementation details.

## User Types

- **Regular User**: Customers who can view their accounts, manage credit cards, review transactions, and make bill payments
- **Admin User**: Banking staff who can perform all regular user functions plus user administration and reporting

---

## Authentication & Access Management

### User Authentication
- As a **Regular User** I want to log into the system with my user ID and password So that I can securely access my account information
- As an **Admin User** I want to log into the system with my admin credentials So that I can access administrative functions and manage the system
- As a **User** I want the system to validate my credentials So that unauthorized users cannot access my account
- As a **User** I want to be automatically routed to the appropriate menu based on my user type So that I can quickly access the functions relevant to my role

---

## Account Management

### Account Information Access
- As a **Regular User** I want to view my account details including account number, balance, and limits So that I can monitor my account status
- As a **Regular User** I want to see my current account balance and available credit So that I can make informed financial decisions
- As a **Regular User** I want to view my account holder information So that I can verify my personal details are correct

### Account Information Updates
- As a **Regular User** I want to update my personal information such as name and address So that my account records remain current
- As a **Regular User** I want to modify my account preferences So that the system works according to my needs
- As a **Regular User** I want to receive confirmation when my account updates are successful So that I know my changes have been saved

---

## Credit Card Portfolio Management

### Card Information Access
- As a **Regular User** I want to view a list of all my credit cards So that I can see my complete card portfolio
- As a **Regular User** I want to see detailed information for each card including card number, expiration date, and status So that I can manage my cards effectively
- As a **Regular User** I want to browse through multiple pages of cards if I have many So that I can find specific cards easily
- As an **Admin User** I want to view all credit cards in the system So that I can monitor and manage the card portfolio

### Card Management Operations
- As a **Regular User** I want to view the status of my credit cards (active/inactive) So that I know which cards are available for use
- As a **Regular User** I want to see my card expiration dates So that I can request renewals before they expire
- As an **Admin User** I want to update credit card information including cardholder name, expiration date, and status So that I can maintain accurate card records
- As an **Admin User** I want to activate or deactivate credit cards So that I can control card usage for security purposes

---

## Transaction Management

### Transaction History Access
- As a **Regular User** I want to view a list of my recent transactions So that I can monitor my spending activity
- As a **Regular User** I want to see transaction details including amount, date, merchant, and description So that I can verify each transaction
- As a **Regular User** I want to browse through multiple pages of transactions So that I can review my complete transaction history
- As a **Regular User** I want to search for specific transactions by transaction ID So that I can quickly find particular transactions

### Transaction Details
- As a **Regular User** I want to view complete details of individual transactions including merchant information and processing dates So that I can understand each charge
- As a **Regular User** I want to see transaction amounts, types, and categories So that I can track my spending patterns
- As a **Regular User** I want to view transaction timestamps So that I can verify when purchases were made

### Transaction Creation
- As an **Admin User** I want to add new transactions to customer accounts So that I can record manual adjustments or corrections
- As an **Admin User** I want to specify transaction details including amount, merchant, date, and description So that transaction records are complete and accurate
- As an **Admin User** I want to validate transaction information before saving So that data integrity is maintained

---

## Bill Payment Services

### Payment Processing
- As a **Regular User** I want to pay my account balance in full So that I can clear my outstanding debt
- As a **Regular User** I want to see my current balance before making a payment So that I know exactly how much I owe
- As a **Regular User** I want to confirm my payment before it's processed So that I can avoid accidental payments
- As a **Regular User** I want to receive confirmation when my payment is successful So that I know my payment was processed

### Payment Validation
- As a **Regular User** I want the system to prevent me from making payments when my balance is zero So that I don't make unnecessary payments
- As a **Regular User** I want to be prompted to confirm payment amounts So that I can verify the payment details before processing
- As a **Regular User** I want to see updated account balances after payments So that I can confirm the payment was applied correctly

---

## User Administration (Admin Only)

### User Management Overview
- As an **Admin User** I want to view a list of all system users So that I can monitor user accounts
- As an **Admin User** I want to see user details including name, user ID, and user type So that I can identify and manage specific users
- As an **Admin User** I want to browse through multiple pages of users So that I can efficiently navigate large user lists

### User Account Creation
- As an **Admin User** I want to create new user accounts with user ID, name, password, and user type So that new customers and staff can access the system
- As an **Admin User** I want to specify whether new users are regular users or administrators So that appropriate access levels are assigned
- As an **Admin User** I want to receive confirmation when new users are successfully created So that I know the account setup is complete

### User Account Maintenance
- As an **Admin User** I want to update existing user information including names, passwords, and user types So that user records remain accurate
- As an **Admin User** I want to modify user access levels So that I can promote or demote users as needed
- As an **Admin User** I want to receive confirmation when user updates are successful So that I know changes have been applied

### User Account Removal
- As an **Admin User** I want to delete user accounts that are no longer needed So that I can maintain a clean user database
- As an **Admin User** I want to confirm user deletions before they are processed So that I can prevent accidental account removal
- As an **Admin User** I want to receive confirmation when users are successfully deleted So that I know the removal was completed

---

## Reporting & Analytics

### Report Generation
- As a **Regular User** I want to generate transaction reports for specific time periods So that I can analyze my spending patterns
- As a **Regular User** I want to request monthly transaction reports So that I can review my monthly activity
- As a **Regular User** I want to request yearly transaction reports So that I can see my annual spending summary
- As a **Regular User** I want to specify custom date ranges for reports So that I can analyze specific time periods

### Report Customization
- As a **Regular User** I want to generate reports with custom start and end dates So that I can focus on specific time periods of interest
- As a **Regular User** I want to validate date ranges before generating reports So that I receive accurate and meaningful data
- As a **Regular User** I want to submit report requests that will be processed in the background So that I can continue using the system while reports are generated

### Report Delivery
- As a **Regular User** I want to be notified when my requested reports are ready So that I can access them promptly
- As a **Regular User** I want to receive confirmation that my report request was submitted successfully So that I know the system is processing my request

---

## Navigation & User Experience

### Menu Navigation
- As a **Regular User** I want to access a main menu with options for account management, card operations, transactions, bill payment, and reports So that I can easily navigate to different functions
- As an **Admin User** I want to access an administrative menu with additional options for user management So that I can perform administrative tasks
- As a **User** I want to return to previous screens using function keys So that I can navigate efficiently through the application

### Screen Management
- As a **User** I want to clear screen data when needed So that I can start fresh with new information
- As a **User** I want to exit from any screen and return to the main menu So that I can change tasks easily
- As a **User** I want to receive clear error messages when I make input mistakes So that I can correct them and proceed

### Data Entry Support
- As a **User** I want the system to validate my input data and provide helpful error messages So that I can enter information correctly
- As a **User** I want to be guided through multi-step processes with clear instructions So that I can complete tasks successfully
- As a **User** I want to receive confirmation messages when operations complete successfully So that I know my actions were processed
