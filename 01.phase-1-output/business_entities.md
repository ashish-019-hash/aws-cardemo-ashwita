# CardDemo Business Entities Analysis

## Overview

This document presents the business entities extracted from the CardDemo COBOL legacy system. The analysis focuses on real-world, business-relevant entities essential for domain modeling and modernization, excluding UI fields, technical variables, and COBOL structure-only items.

## Business Entities

### 1. Account Entity

**Primary Key:** ACCT-ID (PIC 9(11))

**Attributes:**
- ACCT-ID: Account identifier (PIC 9(11))
- ACCT-CURR-BAL: Current account balance (PIC S9(9)V99)
- ACCT-CREDIT-LIMIT: Credit limit for the account (PIC S9(9)V99)
- ACCT-CASH-CREDIT-LIMIT: Cash advance credit limit (PIC S9(9)V99)
- ACCT-OPEN-DATE: Account opening date
- ACCT-EXPIRY-DATE: Account expiration date
- ACCT-REISSUE-DATE: Account reissue date
- ACCT-CURR-CYC-CREDIT: Current cycle credit amount
- ACCT-CURR-CYC-DEBIT: Current cycle debit amount
- ACCT-GROUP-ID: Account group identifier

**Business Purpose:** Represents customer credit card accounts with financial limits, balances, and lifecycle dates.

### 2. Customer Entity

**Primary Key:** CUST-ID (PIC 9(09))

**Attributes:**
- CUST-ID: Customer identifier (PIC 9(09))
- CUST-FIRST-NAME: Customer first name (PIC X(25))
- CUST-MIDDLE-NAME: Customer middle name (PIC X(25))
- CUST-LAST-NAME: Customer last name (PIC X(25))
- CUST-ADDR-LINE-1: Address line 1 (PIC X(50))
- CUST-ADDR-LINE-2: Address line 2 (PIC X(50))
- CUST-ADDR-LINE-3: Address line 3 (PIC X(50))
- CUST-ADDR-STATE-CD: State code (PIC X(02))
- CUST-ADDR-COUNTRY-CD: Country code (PIC X(03))
- CUST-ADDR-ZIP: ZIP code (PIC X(10))
- CUST-PHONE-NUM-1: Primary phone number (PIC X(15))
- CUST-PHONE-NUM-2: Secondary phone number (PIC X(15))
- CUST-SSN: Social Security Number (PIC 9(09))
- CUST-GOVT-ISSUED-ID: Government issued ID
- CUST-DOB-YYYY-MM-DD: Date of birth (YYYY-MM-DD format)
- CUST-EFT-ACCOUNT-ID: Electronic funds transfer account ID
- CUST-PRI-CARD-IND: Primary card indicator
- CUST-FICO-CREDIT-SCORE: FICO credit score (PIC 9(03))

**Business Purpose:** Represents individual customers with personal information, contact details, and credit profile.

### 3. Card Entity

**Primary Key:** CARD-NUM (PIC X(16))

**Attributes:**
- CARD-NUM: Card number (PIC X(16))
- CARD-ACCT-ID: Associated account ID (PIC 9(11))
- CARD-CVV-CD: Card verification value (PIC 9(03))
- CARD-EMBOSSED-NAME: Name embossed on card (PIC X(50))
- CARD-EXPIRY-DATE: Card expiration date
- CARD-ACTIVE-STATUS: Card active status indicator

**Business Purpose:** Represents physical credit cards linked to customer accounts.

### 4. Transaction Entity

**Primary Key:** TRAN-ID (PIC X(16))

**Attributes:**
- TRAN-ID: Transaction identifier (PIC X(16))
- TRAN-CARD-NUM: Card number used for transaction (PIC X(16))
- TRAN-TYPE-CD: Transaction type code
- TRAN-CAT-CD: Transaction category code
- TRAN-SOURCE: Transaction source (e.g., 'POS TERM')
- TRAN-DESC: Transaction description
- TRAN-AMT: Transaction amount (PIC S9(9)V99)
- TRAN-MERCHANT-ID: Merchant identifier (PIC 9(09))
- TRAN-MERCHANT-NAME: Merchant name (PIC X(40))
- TRAN-MERCHANT-CITY: Merchant city (PIC X(40))
- TRAN-MERCHANT-ZIP: Merchant ZIP code (PIC X(10))
- TRAN-ORIG-TS: Original timestamp
- TRAN-PROC-TS: Processing timestamp

**Business Purpose:** Represents individual credit card transactions with merchant details and processing information.

### 5. User Entity

**Primary Key:** SEC-USR-ID (PIC X(08))

**Attributes:**
- SEC-USR-ID: User identifier (PIC X(08))
- SEC-USR-FNAME: User first name (PIC X(25))
- SEC-USR-LNAME: User last name (PIC X(25))
- SEC-USR-PWD: User password (PIC X(08))
- SEC-USR-TYPE: User type (Admin/Regular) (PIC X(01))

**Business Purpose:** Represents system users with authentication credentials and access levels.

### 6. Cross-Reference Entity

**Composite Key:** XREF-ACCT-ID, XREF-CARD-NUM

**Attributes:**
- XREF-ACCT-ID: Account identifier (PIC 9(11))
- XREF-CARD-NUM: Card number (PIC X(16))
- XREF-CUST-ID: Customer identifier (PIC 9(09))

**Business Purpose:** Enables navigation and relationships between accounts, customers, and cards.

## Entity Relationships

### Customer ↔ Account (1:N)
- **Relationship:** One customer can have multiple accounts
- **Cardinality:** 1:N
- **Implementation:** Customer ID links to Account records via cross-reference

### Account ↔ Card (1:N)
- **Relationship:** One account can have multiple cards
- **Cardinality:** 1:N
- **Implementation:** CARD-ACCT-ID references ACCT-ID

### Card ↔ Transaction (1:N)
- **Relationship:** One card can have multiple transactions
- **Cardinality:** 1:N
- **Implementation:** TRAN-CARD-NUM references CARD-NUM

### Customer ↔ Card (N:M via Cross-Reference)
- **Relationship:** Customers and cards are linked through cross-reference entity
- **Cardinality:** N:M
- **Implementation:** XREF entity maintains CUST-ID, ACCT-ID, and CARD-NUM relationships

### Account ↔ Transaction (1:N via Card)
- **Relationship:** Account transactions are accessed through associated cards
- **Cardinality:** 1:N (indirect)
- **Implementation:** Account → Card → Transaction relationship chain

## Entity Relationship Diagram

![Entity Relationship Diagram](entity_relationships.svg)

## Data Sources

The business entities were extracted from the following COBOL programs and copybooks:

### COBOL Programs Analyzed:
- **COACTVWC.cbl**: Account view functionality
- **COACTUPC.cbl**: Account update operations
- **COCRDSLC.cbl**: Credit card detail view
- **COCRDLIC.cbl**: Credit card listing
- **COCRDUPC.cbl**: Credit card update operations
- **COTRN00C.cbl**: Transaction listing
- **COTRN01C.cbl**: Transaction view
- **COTRN02C.cbl**: Transaction creation
- **COBIL00C.cbl**: Bill payment processing
- **COUSR00C.cbl**: User listing
- **COUSR01C.cbl**: User creation
- **COUSR02C.cbl**: User updates
- **COUSR03C.cbl**: User deletion
- **CORPT00C.cbl**: Report generation
- **COMEN01C.cbl**: Main menu (regular users)
- **COADM01C.cbl**: Admin menu
- **COSGN00C.cbl**: Sign-on authentication
- **CSUTLDTC.cbl**: Date validation utility

### Key COPY Files Referenced:
- **CVACT01Y**: Account record layout
- **CVCUS01Y**: Customer record layout
- **CVTRA05Y**: Transaction record layout
- **CVACT02Y**: Card record layout
- **CVACT03Y**: Card cross-reference layout
- **CSUSR01Y**: User security data layout

## Exclusions

The following technical elements were excluded from the business entity analysis as specified:

- UI fields and display-related variables
- Helper/technical variables (WS-, TEMP-, FILLER, FLAGS)
- Screen mapping variables
- Control fields, counters, and index variables
- COBOL structure-only items like 77-level values without business context
- Program control variables and error handling flags
- Date/time formatting variables
- Response codes and reason codes
- Working storage utility variables

## Summary

The CardDemo system contains 6 primary business entities that form the core domain model for a credit card management system. The entities are well-structured with clear relationships that support typical banking operations including customer management, account administration, card issuance, and transaction processing. The cross-reference entity provides flexibility for complex customer-account-card relationships while maintaining data integrity.
