# CardDemo Business Entities Analysis

## Overview

This document presents the comprehensive analysis of business entities extracted from the CardDemo COBOL legacy system. The analysis covers all 18 COBOL files in the `00.phase-1-input` folder, focusing on real-world business-relevant entities essential for domain modeling and modernization.

## Business Entities Identified

### 1. Customer Entity

The Customer entity represents individual customers in the credit card system.

| Attribute | Data Type | Description |
|-----------|-----------|-------------|
| CUST-ID | PIC 9(09) | Unique customer identifier |
| CUST-FIRST-NAME | PIC X(25) | Customer's first name |
| CUST-MIDDLE-NAME | PIC X(25) | Customer's middle name |
| CUST-LAST-NAME | PIC X(25) | Customer's last name |
| CUST-SSN | PIC X(09) | Social Security Number |
| CUST-DOB-YYYY-MM-DD | PIC X(10) | Date of birth in YYYY-MM-DD format |
| CUST-ADDR-LINE-1 | PIC X(50) | Primary address line |
| CUST-ADDR-LINE-2 | PIC X(50) | Secondary address line |
| CUST-ADDR-LINE-3 | PIC X(50) | City information |
| CUST-ADDR-STATE-CD | PIC X(02) | State code |
| CUST-ADDR-ZIP | PIC X(10) | ZIP code |
| CUST-PHONE-NUM-1 | PIC X(15) | Primary phone number |
| CUST-PHONE-NUM-2 | PIC X(15) | Secondary phone number |
| CUST-FICO-CREDIT-SCORE | PIC 9(03) | FICO credit score |

### 2. Account Entity

The Account entity represents credit card accounts with financial details.

| Attribute | Data Type | Description |
|-----------|-----------|-------------|
| ACCT-ID | PIC 9(11) | Unique account identifier |
| ACCT-ACTIVE-STATUS | PIC X(01) | Account status (Active/Inactive) |
| ACCT-CURR-BAL | PIC S9(10)V99 | Current account balance |
| ACCT-CREDIT-LIMIT | PIC S9(10)V99 | Credit limit for the account |
| ACCT-CASH-CREDIT-LIMIT | PIC S9(10)V99 | Cash advance credit limit |
| ACCT-CURR-CYC-CREDIT | PIC S9(10)V99 | Current cycle credit amount |
| ACCT-CURR-CYC-DEBIT | PIC S9(10)V99 | Current cycle debit amount |
| ACCT-OPEN-DATE | PIC X(08) | Account opening date |
| ACCT-EXPIRAION-DATE | PIC X(08) | Account expiration date |
| ACCT-REISSUE-DATE | PIC X(08) | Account reissue date |
| ACCT-GROUP-ID | PIC X(10) | Account group identifier |

### 3. Card Entity

The Card entity represents physical credit cards linked to accounts.

| Attribute | Data Type | Description |
|-----------|-----------|-------------|
| CARD-NUM | PIC X(16) | Credit card number |
| CARD-ACCT-ID | PIC 9(11) | Associated account ID |
| CARD-CVV-CD | PIC 9(03) | Card verification value |
| CARD-EMBOSSED-NAME | PIC X(50) | Name embossed on card |
| CARD-EXPIRY-DATE | PIC X(08) | Card expiration date |
| CARD-ACTIVE-STATUS | PIC X(01) | Card status |

### 4. Transaction Entity

The Transaction entity represents all card transaction activities.

| Attribute | Data Type | Description |
|-----------|-----------|-------------|
| TRAN-ID | PIC X(16) | Unique transaction identifier |
| TRAN-CARD-NUM | PIC X(16) | Card number used for transaction |
| TRAN-TYPE-CD | PIC X(02) | Transaction type code |
| TRAN-CAT-CD | PIC X(04) | Transaction category code |
| TRAN-SOURCE | PIC X(10) | Transaction source |
| TRAN-AMT | PIC S9(08)V99 | Transaction amount |
| TRAN-DESC | PIC X(26) | Transaction description |
| TRAN-ORIG-TS | PIC X(26) | Original timestamp |
| TRAN-PROC-TS | PIC X(26) | Processing timestamp |
| TRAN-MERCHANT-ID | PIC X(15) | Merchant identifier |
| TRAN-MERCHANT-NAME | PIC X(50) | Merchant name |
| TRAN-MERCHANT-CITY | PIC X(50) | Merchant city |
| TRAN-MERCHANT-ZIP | PIC X(10) | Merchant ZIP code |

### 5. User Entity

The User entity represents system users (both regular users and administrators).

| Attribute | Data Type | Description |
|-----------|-----------|-------------|
| SEC-USR-ID | PIC X(08) | Unique user identifier |
| SEC-USR-FNAME | PIC X(25) | User's first name |
| SEC-USR-LNAME | PIC X(25) | User's last name |
| SEC-USR-PWD | PIC X(08) | User password |
| SEC-USR-TYPE | PIC X(01) | User type (Admin/Regular) |

### 6. Merchant Entity

The Merchant entity represents businesses that accept card payments.

| Attribute | Data Type | Description |
|-----------|-----------|-------------|
| MERCHANT-ID | PIC X(15) | Unique merchant identifier |
| MERCHANT-NAME | PIC X(50) | Merchant business name |
| MERCHANT-CITY | PIC X(50) | Merchant city location |
| MERCHANT-ZIP | PIC X(10) | Merchant ZIP code |

### 7. Cross-Reference Entity

The Cross-Reference entity links customers, accounts, and cards together.

| Attribute | Data Type | Description |
|-----------|-----------|-------------|
| XREF-CARD-NUM | PIC X(16) | Card number |
| XREF-CUST-ID | PIC 9(09) | Customer ID |
| XREF-ACCT-ID | PIC 9(11) | Account ID |

## Entity Relationships

The following relationships exist between the business entities:

### Customer ↔ Account (1:N)
- One customer can have multiple accounts
- Each account belongs to exactly one customer
- Linked via CUST-ID in both entities

### Account ↔ Card (1:N)
- One account can have multiple cards
- Each card belongs to exactly one account
- Linked via ACCT-ID in both entities

### Card ↔ Transaction (1:N)
- One card can have multiple transactions
- Each transaction is associated with exactly one card
- Linked via CARD-NUM in both entities

### Customer ↔ Card (1:N via Account)
- Customers access cards through their accounts
- Relationship maintained via Cross-Reference entity
- Enables customer to view all their cards across accounts

### Transaction ↔ Merchant (N:1)
- Multiple transactions can be with the same merchant
- Each transaction is with exactly one merchant
- Linked via MERCHANT-ID

### Account ↔ Bill Payment (1:N)
- One account can have multiple bill payments
- Each bill payment is for exactly one account
- Bill payments update account balances

## Entity Relationship Diagram

![Entity Relationship Diagram](entity_relationships.svg)

## Data Sources

The business entities were extracted from the following COBOL programs:

- **Account Management**: COACTVWC.cbl, COACTUPC.cbl
- **Card Operations**: COCRDLIC.cbl, COCRDSLC.cbl, COCRDUPC.cbl
- **Transaction Processing**: COTRN00C.cbl, COTRN01C.cbl, COTRN02C.cbl
- **User Management**: COUSR00C.cbl, COUSR01C.cbl, COUSR02C.cbl, COUSR03C.cbl
- **Bill Payment**: COBIL00C.cbl
- **Authentication**: COSGN00C.cbl
- **Menu Systems**: COMEN01C.cbl, COADM01C.cbl
- **Reporting**: CORPT00C.cbl
- **Utilities**: CSUTLDTC.cbl

## Technical Notes

### Excluded Elements
The following technical elements were excluded from the business entity analysis:
- Working storage variables (WS-*)
- Temporary variables (TEMP-*)
- Filler fields
- Flag variables for processing control
- Screen mapping variables
- Counter and index variables
- COBOL structure-only items without business context

### COPY Files Referenced
The analysis identified references to key COPY files containing business data structures:
- CVACT01Y: Account record layout
- CVACT02Y: Card record layout
- CVACT03Y: Card cross-reference layout
- CVCUS01Y: Customer record layout
- CVTRA05Y: Transaction record layout
- CSUSR01Y: User security record layout

## Modernization Implications

These business entities form the core domain model for modernizing the CardDemo application. The clear separation of concerns and well-defined relationships provide a solid foundation for:

1. **Microservices Architecture**: Each entity can potentially become a separate microservice
2. **API Design**: Entity attributes map directly to REST API data models
3. **Database Design**: Relationships guide normalized database schema design
4. **Business Logic**: Entity operations align with business capabilities

The comprehensive attribute definitions ensure that all business data requirements are captured for successful modernization efforts.
