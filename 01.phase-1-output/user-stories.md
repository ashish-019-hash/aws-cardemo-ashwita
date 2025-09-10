# CardDemo Application - Business User Stories

This document contains business-level user stories extracted from the CardDemo COBOL codebase, focusing on end-user interactions and business value for system modernization.

## Authentication & Access Management

### User Authentication
**As a** customer  
**I want to** log into the system with my user ID and password  
**So that** I can securely access my account information and perform banking operations

**As a** bank administrator  
**I want to** log into the system with my administrative credentials  
**So that** I can access administrative functions and manage the banking system

**As a** system user  
**I want to** be automatically directed to the appropriate menu based on my user role  
**So that** I only see functions relevant to my responsibilities and access level

## Account Management

### Account Information Access
**As a** customer  
**I want to** view my account details including balance and account information  
**So that** I can monitor my financial status and account activity

**As a** customer  
**I want to** update my account information such as personal details and contact information  
**So that** I can keep my banking records current and accurate

### Bill Payment Processing
**As a** customer  
**I want to** pay my account balance in full through the online system  
**So that** I can conveniently manage my payments without visiting a branch

**As a** customer  
**I want to** have my bill payment automatically recorded as a transaction  
**So that** I have a complete record of all my account activities

## Credit Card Management

### Card Portfolio Management
**As a** customer  
**I want to** view a list of all my credit cards with pagination support  
**So that** I can easily browse through my card portfolio when I have multiple cards

**As a** bank administrator  
**I want to** view all credit cards in the system with filtering capabilities  
**So that** I can manage the entire card portfolio and assist customers

### Card Information Access
**As a** customer  
**I want to** view detailed information about a specific credit card including card number, expiration date, and status  
**So that** I can verify card details and check card validity

**As a** customer  
**I want to** see my card's embossed name and current status  
**So that** I can confirm my card information is correct and active

### Card Information Updates
**As a** customer  
**I want to** update my credit card information such as the embossed name  
**So that** I can ensure my card displays the correct information

**As a** bank administrator  
**I want to** update credit card status and expiration dates  
**So that** I can manage card lifecycle and maintain accurate card records

**As a** bank administrator  
**I want to** validate card expiration dates and other card details during updates  
**So that** I can ensure data integrity and prevent invalid card information

## Transaction Management

### Transaction History Access
**As a** customer  
**I want to** browse through my transaction history with pagination  
**So that** I can review my spending patterns and account activity over time

**As a** customer  
**I want to** navigate forward and backward through my transaction pages  
**So that** I can efficiently find specific transactions without scrolling through everything

### Transaction Details
**As a** customer  
**I want to** view detailed information about a specific transaction including amount, date, merchant details, and transaction type  
**So that** I can verify transaction accuracy and understand my purchases

**As a** customer  
**I want to** see merchant information including name, city, and location for each transaction  
**So that** I can identify where and when I made purchases

### Transaction Entry
**As a** bank administrator  
**I want to** add new transactions to customer accounts with complete details  
**So that** I can record manual transactions or corrections when necessary

**As a** bank administrator  
**I want to** validate transaction information including amounts, dates, and merchant details  
**So that** I can ensure accurate transaction records and prevent data errors

## User Administration (Admin Only)

### User Management Overview
**As a** bank administrator  
**I want to** view a list of all system users with their roles and basic information  
**So that** I can monitor user access and manage system security

**As a** bank administrator  
**I want to** navigate through user lists with pagination  
**So that** I can efficiently manage large numbers of system users

### User Account Creation
**As a** bank administrator  
**I want to** create new user accounts with appropriate roles (regular user or administrator)  
**So that** I can provide system access to new employees or customers

**As a** bank administrator  
**I want to** set user credentials including user ID, password, and personal information during account creation  
**So that** I can establish secure access for new users

**As a** bank administrator  
**I want to** validate user information during account creation to prevent duplicate user IDs  
**So that** I can maintain unique user identities and system integrity

### User Account Updates
**As a** bank administrator  
**I want to** update existing user information including names, passwords, and user types  
**So that** I can maintain current user records and adjust access levels as needed

**As a** bank administrator  
**I want to** modify user roles between regular user and administrator  
**So that** I can adjust user permissions based on job responsibilities

### User Account Removal
**As a** bank administrator  
**I want to** delete user accounts that are no longer needed  
**So that** I can maintain system security and remove unauthorized access

**As a** bank administrator  
**I want to** confirm user details before deletion  
**So that** I can prevent accidental removal of active user accounts

## Reporting & Analytics

### Transaction Reporting
**As a** bank administrator  
**I want to** generate transaction reports for specific date ranges  
**So that** I can analyze transaction patterns and create business intelligence reports

**As a** bank administrator  
**I want to** submit batch jobs for report generation  
**So that** I can create comprehensive reports without impacting online system performance

**As a** bank administrator  
**I want to** specify start and end dates for transaction reports  
**So that** I can focus analysis on specific time periods relevant to business needs

**As a** bank administrator  
**I want to** validate date ranges before report generation  
**So that** I can ensure reports contain accurate and meaningful data

## Navigation & User Experience

### Menu Navigation
**As a** customer  
**I want to** access a main menu with options relevant to my account management needs  
**So that** I can easily navigate to the functions I need to use

**As a** bank administrator  
**I want to** access an administrative menu with management functions  
**So that** I can efficiently perform administrative tasks and system management

**As a** system user  
**I want to** navigate between different functions while maintaining my session context  
**So that** I can work efficiently without losing my place in the system

**As a** system user  
**I want to** return to previous screens or main menus using function keys  
**So that** I can navigate the system intuitively and efficiently

## Data Validation & Quality

### Input Validation
**As a** system user  
**I want to** receive immediate feedback when I enter invalid data  
**So that** I can correct errors quickly and complete my tasks successfully

**As a** system user  
**I want to** have date formats validated automatically  
**So that** I can ensure my date entries are accurate and properly formatted

**As a** system user  
**I want to** receive clear error messages when validation fails  
**So that** I understand what needs to be corrected and can fix issues efficiently

---

*This document represents the business functionality extracted from the CardDemo COBOL application, focusing on user goals and business value rather than technical implementation details. These user stories can guide the modernization of the system into a more user-centric web or mobile interface.*
