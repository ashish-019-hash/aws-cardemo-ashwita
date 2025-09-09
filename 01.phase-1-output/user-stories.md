# CardDemo Application - Business User Stories

## Overview
This document contains business-level user stories extracted from the CardDemo COBOL codebase. These stories focus on end-user value and business processes, designed to guide modernization into web or mobile interfaces.

## Authentication & Access Management

### User Authentication
- **As a customer** I want to log into the system with my user ID and password So that I can securely access my account information
- **As an administrator** I want to log into the system with elevated privileges So that I can manage users and perform administrative tasks
- **As a user** I want the system to redirect me to the appropriate menu based on my role So that I see only the functions I'm authorized to use

## Account Management

### Account Information Access
- **As a customer** I want to view my account details including current balance, credit limit, and account status So that I can monitor my financial position
- **As a customer** I want to see my account opening date, expiration date, and reissue information So that I can track my account lifecycle
- **As a customer** I want to view my customer information including name, SSN, and contact details So that I can verify my personal data is accurate

### Account Maintenance
- **As a customer** I want to update my account information including personal details and contact information So that my records remain current
- **As a customer** I want to modify account settings and preferences So that the system works according to my needs
- **As an administrator** I want to update customer account details and limits So that I can maintain accurate customer records

## Bill Payment & Financial Transactions

### Payment Processing
- **As a customer** I want to pay my account balance in full So that I can clear my outstanding debt
- **As a customer** I want to confirm payment details before processing So that I can avoid accidental payments
- **As a customer** I want to see my current balance before making a payment So that I know exactly how much I owe
- **As a customer** I want to receive confirmation after successful payment So that I have proof of my transaction

## Credit Card Management

### Card Portfolio Overview
- **As a customer** I want to view a list of all my credit cards So that I can see my complete card portfolio
- **As a customer** I want to see card details including card number, status, and expiration date So that I can manage my cards effectively
- **As an administrator** I want to view all credit cards in the system So that I can monitor card usage and status across all customers

### Card Information Access
- **As a customer** I want to view detailed information for a specific credit card So that I can see all relevant card data
- **As a customer** I want to see card limits, balances, and available credit So that I can make informed spending decisions
- **As a customer** I want to view card expiration dates and security codes So that I can use my cards for transactions

### Card Maintenance
- **As a customer** I want to update my card information including name embossed on card So that my card reflects current information
- **As a customer** I want to change card status (active/inactive) So that I can control card usage
- **As an administrator** I want to update card details and limits So that I can maintain accurate card records
- **As an administrator** I want to modify card expiration dates So that I can manage card lifecycle

## Transaction Management

### Transaction History
- **As a customer** I want to view a list of my recent transactions So that I can monitor my spending activity
- **As a customer** I want to browse through transaction history with pagination So that I can review older transactions
- **As a customer** I want to see transaction details including amount, date, merchant, and description So that I can understand each charge

### Transaction Details
- **As a customer** I want to view complete details of a specific transaction So that I can verify transaction accuracy
- **As a customer** I want to see merchant information including name, city, and location So that I can identify where transactions occurred
- **As a customer** I want to view transaction processing dates and original timestamps So that I can track transaction timing

### Transaction Entry
- **As an administrator** I want to add new transactions to customer accounts So that I can record manual adjustments or corrections
- **As an administrator** I want to enter transaction details including amount, merchant, and category So that transaction records are complete
- **As an administrator** I want to validate transaction data before saving So that I maintain data integrity

## User Administration (Admin Only)

### User Management
- **As an administrator** I want to view a list of all system users So that I can monitor user accounts
- **As an administrator** I want to create new user accounts So that I can provide system access to new customers or staff
- **As an administrator** I want to update existing user information So that I can maintain accurate user records
- **As an administrator** I want to delete user accounts So that I can remove access for inactive users

### User Account Details
- **As an administrator** I want to view user details including user type and permissions So that I can verify access levels
- **As an administrator** I want to modify user passwords and security settings So that I can maintain account security
- **As an administrator** I want to change user types between regular and admin So that I can adjust access privileges

## Reporting & Analytics

### Transaction Reporting
- **As an administrator** I want to generate transaction reports for specific date ranges So that I can analyze business activity
- **As an administrator** I want to submit batch jobs for report generation So that I can produce comprehensive reports without impacting online performance
- **As an administrator** I want to specify report parameters including start and end dates So that I can customize report content
- **As an administrator** I want to receive notification when reports are complete So that I know when results are available

## Navigation & User Experience

### Menu Navigation
- **As a customer** I want to access a main menu with available options So that I can navigate to different functions
- **As an administrator** I want to access an admin menu with administrative functions So that I can perform management tasks
- **As a user** I want to return to previous screens using function keys So that I can navigate efficiently through the application
- **As a user** I want to sign out of the system securely So that I can protect my account when finished

### Data Entry & Validation
- **As a user** I want to receive clear error messages when I enter invalid data So that I can correct mistakes quickly
- **As a user** I want field-level validation to guide proper data entry So that I can complete forms successfully
- **As a user** I want to clear form fields and start over So that I can correct multiple errors easily
