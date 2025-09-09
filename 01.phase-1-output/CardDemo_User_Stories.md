# CardDemo Application - Business User Stories

## Overview

This document contains business-level user stories extracted from the CardDemo COBOL codebase. These stories focus on end-user business actions and goals, relevant for modernizing the system into a user-centric web or mobile application.

## User Types

- **Regular User**: Customer who owns credit card accounts
- **Administrator**: Banking staff with elevated privileges for user and system management

---

## 1. Authentication & Access Control

### User Authentication
- **As a regular user** I want to log into the system with my user ID and password **So that** I can securely access my account information and perform banking operations
- **As an administrator** I want to log into the system with my admin credentials **So that** I can access administrative functions and manage the banking system
- **As a user** I want the system to validate my credentials **So that** unauthorized users cannot access my account
- **As a user** I want to be directed to the appropriate menu based on my user type **So that** I only see functions relevant to my role

### Navigation & Menu Access
- **As a regular user** I want to access a main menu after login **So that** I can navigate to different banking functions like account management, card operations, and transaction history
- **As an administrator** I want to access an admin menu after login **So that** I can perform administrative tasks like user management and system operations
- **As a user** I want to return to the sign-on screen when needed **So that** I can log out securely or switch users

---

## 2. Account Management

### Account Information Access
- **As a regular user** I want to view my account details **So that** I can see my current balance, account status, and personal information
- **As a regular user** I want to see my account balance and credit limit **So that** I can understand my available credit and spending capacity
- **As a regular user** I want to view my account holder information **So that** I can verify my personal details are correct

### Account Information Updates
- **As a regular user** I want to update my account information **So that** I can keep my personal details current and accurate
- **As a regular user** I want to modify my contact information **So that** the bank can reach me with important notifications
- **As a regular user** I want to update my address details **So that** statements and cards are sent to the correct location
- **As a regular user** I want validation of my input when updating account information **So that** I don't accidentally enter incorrect data

### Bill Payment
- **As a regular user** I want to pay my account balance **So that** I can reduce my outstanding debt and maintain good credit standing
- **As a regular user** I want to pay my full account balance **So that** I can avoid interest charges and clear my debt completely
- **As a regular user** I want confirmation of my payment **So that** I have proof the transaction was processed successfully
- **As a regular user** I want to see my updated balance after payment **So that** I can verify the payment was applied correctly

---

## 3. Credit Card Management

### Card Portfolio Overview
- **As a regular user** I want to see a list of all my credit cards **So that** I can manage my entire card portfolio from one place
- **As an administrator** I want to view all credit cards in the system **So that** I can monitor and manage the bank's card portfolio
- **As a regular user** I want to see basic card information in the list **So that** I can quickly identify each card and its status
- **As a user** I want to navigate through multiple pages of cards **So that** I can view all cards even when I have many

### Card Details Access
- **As a regular user** I want to view detailed information about a specific credit card **So that** I can see all relevant card data including limits, expiration, and status
- **As a regular user** I want to see my card number, expiration date, and CVV **So that** I can use the card for transactions
- **As a regular user** I want to view my card's credit limit and available credit **So that** I can understand my spending capacity
- **As a regular user** I want to see the cardholder name as it appears on the card **So that** I can verify the embossed information

### Card Information Updates
- **As a regular user** I want to update my card information **So that** I can keep my card details current and accurate
- **As a regular user** I want to update the name embossed on my card **So that** it reflects my current legal name
- **As a regular user** I want to change my card status if needed **So that** I can activate, deactivate, or report issues with my card
- **As a regular user** I want to update my card expiration date when renewed **So that** my card information remains current
- **As a regular user** I want validation when updating card information **So that** I don't enter invalid data that could cause issues

---

## 4. Transaction Processing

### Transaction History Browsing
- **As a regular user** I want to browse my transaction history **So that** I can review my spending patterns and verify charges
- **As a regular user** I want to see a list of recent transactions **So that** I can quickly check my latest activity
- **As a regular user** I want to navigate through multiple pages of transactions **So that** I can view my complete transaction history
- **As a regular user** I want to see transaction dates, amounts, and merchant information **So that** I can identify each transaction

### Transaction Details Access
- **As a regular user** I want to view detailed information about a specific transaction **So that** I can see all available details about a charge
- **As a regular user** I want to see transaction amounts, dates, and processing information **So that** I can verify transaction accuracy
- **As a regular user** I want to view merchant details including name, city, and location **So that** I can identify where the transaction occurred
- **As a regular user** I want to see transaction categories and types **So that** I can understand the nature of each charge

### Transaction Entry
- **As an administrator** I want to add new transactions to the system **So that** I can record charges, payments, and adjustments
- **As an administrator** I want to enter transaction details including amount, merchant, and category **So that** the transaction record is complete and accurate
- **As an administrator** I want validation when entering transaction data **So that** I don't create invalid or incorrect transaction records
- **As an administrator** I want to associate transactions with the correct card and account **So that** charges appear on the right customer's statement

---

## 5. User Administration (Administrator Only)

### User Management Overview
- **As an administrator** I want to view a list of all system users **So that** I can manage user accounts and monitor system access
- **As an administrator** I want to see user details including names, IDs, and user types **So that** I can identify and manage different users
- **As an administrator** I want to navigate through multiple pages of users **So that** I can manage large numbers of user accounts

### User Account Creation
- **As an administrator** I want to add new users to the system **So that** new customers and staff can access the banking system
- **As an administrator** I want to set user credentials including user ID and password **So that** new users can log into the system
- **As an administrator** I want to assign user types (regular or admin) **So that** users have appropriate access levels
- **As an administrator** I want to enter user personal information **So that** the system has complete user records
- **As an administrator** I want validation when creating user accounts **So that** I don't create duplicate or invalid user accounts

### User Account Updates
- **As an administrator** I want to update existing user information **So that** I can keep user records current and accurate
- **As an administrator** I want to modify user personal details **So that** the system reflects current user information
- **As an administrator** I want to change user passwords when needed **So that** users can regain access or improve security
- **As an administrator** I want to update user types and permissions **So that** I can adjust access levels as roles change

### User Account Deletion
- **As an administrator** I want to delete user accounts **So that** I can remove access for users who no longer need system access
- **As an administrator** I want to view user details before deletion **So that** I can confirm I'm deleting the correct account
- **As an administrator** I want confirmation before deleting users **So that** I don't accidentally remove active user accounts
- **As an administrator** I want to see confirmation after successful deletion **So that** I know the user account was properly removed

---

## 6. Reporting & Analytics

### Transaction Reporting
- **As an administrator** I want to generate transaction reports **So that** I can analyze transaction patterns and create business insights
- **As an administrator** I want to specify date ranges for reports **So that** I can analyze transactions for specific time periods
- **As an administrator** I want to submit batch jobs for report generation **So that** large reports don't impact system performance
- **As an administrator** I want to receive notification when reports are complete **So that** I know when the analysis is ready for review

### Report Parameters
- **As an administrator** I want to set start and end dates for transaction reports **So that** I can focus on specific time periods of interest
- **As an administrator** I want to validate date inputs **So that** I don't generate reports with invalid date ranges
- **As an administrator** I want to specify report formats and outputs **So that** I receive reports in the most useful format for analysis

---

## Summary

These user stories represent the core business functionality of the CardDemo application, focusing on:

1. **Security & Access**: Secure authentication and role-based access control
2. **Account Operations**: Comprehensive account management and bill payment capabilities
3. **Card Management**: Complete credit card lifecycle management
4. **Transaction Processing**: Full transaction history and management
5. **User Administration**: Complete user lifecycle management for administrators
6. **Business Intelligence**: Transaction reporting and analytics capabilities

Each story is designed to capture business value and user goals, making them suitable for modernization into contemporary web or mobile banking applications while maintaining the core banking functionality that customers and administrators need.
