# Business Entities Analysis - CardDemo COBOL System

## Overview
This document presents the business entities extracted from the CardDemo COBOL legacy system for domain modeling and modernization purposes. The analysis focuses on real-world business entities essential for understanding the credit card management domain.

## Business Entities

### 1. Customer Entity
**Description**: Represents individual customers who hold accounts and credit cards.

**Attributes**:
- Customer ID (CUST-ID) - Unique identifier (9 digits)
- First Name (CUST-FNAME) - Customer's first name
- Middle Name (CUST-MNAME) - Customer's middle name  
- Last Name (CUST-LNAME) - Customer's last name
- Address Line 1 (CUST-ADDR-LINE-1) - Primary address
- Address Line 2 (CUST-ADDR-LINE-2) - Secondary address
- City (CUST-CITY) - Customer's city
- State (CUST-STATE) - Customer's state
- ZIP Code (CUST-ZIP) - Customer's postal code
- Phone Number (CUST-PHONE) - Contact phone number
- Government ID (CUST-GOVT-ID) - Social Security Number or similar
- Date of Birth (CUST-DOB) - Customer's birth date
- FICO Score (CUST-FICO-SCORE) - Credit score (300-850)

### 2. Account Entity
**Description**: Represents customer accounts that hold credit card balances and limits.

**Attributes**:
- Account ID (ACCT-ID) - Unique identifier (11 digits)
- Customer ID (ACCT-CUST-ID) - Foreign key to Customer (9 digits)
- Account Status (ACCT-STATUS) - Active/Inactive status
- Account Open Date (ACCT-OPEN-DATE) - Date account was opened
- Current Balance (ACCT-CURR-BAL) - Current account balance
- Credit Limit (ACCT-CREDIT-LIMIT) - Maximum credit allowed
- Cash Credit Limit (ACCT-CASH-CREDIT-LIMIT) - Cash advance limit
- Current Cycle Credit (ACCT-CURR-CYC-CREDIT) - Current cycle credits
- Current Cycle Debit (ACCT-CURR-CYC-DEBIT) - Current cycle debits
- Group ID (ACCT-GROUP-ID) - Account grouping identifier

### 3. Card Entity
**Description**: Represents credit cards issued to customers and linked to accounts.

**Attributes**:
- Card Number (CARD-NUM) - Unique card identifier (16 digits)
- Account ID (CARD-ACCT-ID) - Foreign key to Account (11 digits)
- CVV Code (CARD-CVV-CD) - Card verification value (3 digits)
- Embossed Name (CARD-NAME-EMBOSSED) - Name printed on card
- Card Status (CARD-STATUS) - Active/Inactive status (Y/N)
- Expiry Date (CARD-EXPIRY-DATE) - Card expiration date
- Issue Date (CARD-ISSUE-DATE) - Date card was issued
- Reissue Date (CARD-REISSUE-DATE) - Date card was reissued

### 4. Transaction Entity
**Description**: Represents financial transactions made using credit cards.

**Attributes**:
- Transaction ID (TRAN-ID) - Unique identifier (16 digits)
- Card Number (TRAN-CARD-NUM) - Foreign key to Card (16 digits)
- Transaction Type Code (TRAN-TYPE-CD) - Type of transaction
- Transaction Category Code (TRAN-CAT-CD) - Transaction category
- Transaction Source (TRAN-SOURCE) - Source of transaction
- Transaction Amount (TRAN-AMT) - Transaction amount (signed)
- Transaction Description (TRAN-DESC) - Description of transaction
- Original Timestamp (TRAN-ORIG-TS) - When transaction occurred
- Process Timestamp (TRAN-PROC-TS) - When transaction was processed
- Merchant ID (TRAN-MERCHANT-ID) - Merchant identifier
- Merchant Name (TRAN-MERCHANT-NAME) - Merchant business name
- Merchant City (TRAN-MERCHANT-CITY) - Merchant location city
- Merchant ZIP (TRAN-MERCHANT-ZIP) - Merchant postal code

### 5. User Entity
**Description**: Represents system users (both regular users and administrators) who access the CardDemo system.

**Attributes**:
- User ID (SEC-USR-ID) - Unique login identifier (8 characters)
- First Name (SEC-USR-FNAME) - User's first name
- Last Name (SEC-USR-LNAME) - User's last name
- Password (SEC-USR-PWD) - User's password (8 characters)
- User Type (SEC-USR-TYPE) - User role (Admin/Regular)

### 6. Card Cross-Reference Entity
**Description**: Provides cross-referencing between accounts, customers, and cards for efficient lookups.

**Attributes**:
- Card Number (XREF-CARD-NUM) - Foreign key to Card (16 digits)
- Customer ID (XREF-CUST-ID) - Foreign key to Customer (9 digits)
- Account ID (XREF-ACCT-ID) - Foreign key to Account (11 digits)

## Entity Relationships

### Customer ↔ Account
- **Relationship**: One-to-Many (1:N)
- **Description**: One customer can have multiple accounts
- **Foreign Key**: ACCT-CUST-ID references CUST-ID

### Account ↔ Card
- **Relationship**: One-to-Many (1:N)
- **Description**: One account can have multiple credit cards
- **Foreign Key**: CARD-ACCT-ID references ACCT-ID

### Card ↔ Transaction
- **Relationship**: One-to-Many (1:N)
- **Description**: One card can have multiple transactions
- **Foreign Key**: TRAN-CARD-NUM references CARD-NUM

### Customer ↔ Card (via Cross-Reference)
- **Relationship**: One-to-Many (1:N)
- **Description**: One customer can have multiple cards across different accounts
- **Cross-Reference**: XREF-CUST-ID and XREF-CARD-NUM

### Account ↔ Card (via Cross-Reference)
- **Relationship**: One-to-Many (1:N)
- **Description**: One account can have multiple cards
- **Cross-Reference**: XREF-ACCT-ID and XREF-CARD-NUM

## Entity Relationship Diagram

![Entity Relationship Diagram](entity_relationships.svg)

## Summary

The CardDemo system manages six core business entities:

1. **Customer** - Individual account holders with personal and demographic information
2. **Account** - Financial accounts with balances, limits, and status information
3. **Card** - Physical/virtual credit cards linked to accounts
4. **Transaction** - Financial transactions processed through cards
5. **User** - System users with authentication and authorization data
6. **Card Cross-Reference** - Linking table for efficient entity lookups

The relationships follow a hierarchical structure: Customer → Account → Card → Transaction, with additional cross-referencing for system efficiency. This structure supports typical credit card management operations including account management, card issuance, transaction processing, and user administration.

## Technical Notes

- All numeric identifiers use specific digit lengths for data integrity
- Date fields follow standard date formats for temporal operations
- Status fields use standardized codes (Y/N, Active/Inactive)
- Cross-reference entities enable efficient multi-table lookups
- Foreign key relationships maintain referential integrity across entities

This entity model provides the foundation for modernizing the CardDemo system while preserving essential business logic and data relationships.
