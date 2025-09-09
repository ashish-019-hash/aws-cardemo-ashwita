# CardDemo COBOL Legacy System - Business Entity Analysis

## Overview

This document presents the comprehensive analysis of business entities extracted from the CardDemo COBOL legacy system. The analysis focuses on real-world, business-relevant entities essential for domain modeling and modernization, excluding UI fields, helper variables, and technical COBOL structures.

## Business Entities Identified

### 1. Customer Entity

**Source Files:** COACTUPC.cbl, CVCUS01Y (COPY file)
**Record Structure:** CUSTOMER-RECORD

**Attributes:**
- **CUST-ID** (PIC 9(09)) - Customer unique identifier
- **CUST-FNAME** (PIC X(25)) - Customer first name
- **CUST-LNAME** (PIC X(25)) - Customer last name
- **CUST-ADDR-LINE-1** (PIC X(50)) - Primary address line
- **CUST-ADDR-LINE-2** (PIC X(50)) - Secondary address line
- **CUST-ADDR-LINE-3** (PIC X(50)) - Additional address line
- **CUST-ADDR-STATE-CD** (PIC X(02)) - State code
- **CUST-ADDR-COUNTRY-CD** (PIC X(03)) - Country code
- **CUST-ADDR-ZIP** (PIC X(10)) - ZIP/postal code
- **CUST-PHONE-NUM-1** (PIC X(15)) - Primary phone number
- **CUST-PHONE-NUM-2** (PIC X(15)) - Secondary phone number
- **CUST-SSN** (PIC 9(09)) - Social Security Number
- **CUST-GOVT-ISSUED-ID** (PIC X(20)) - Government issued ID
- **CUST-DOB-YYYY-MM-DD** (PIC X(10)) - Date of birth
- **CUST-EFT-ACCOUNT-ID** (PIC X(15)) - Electronic funds transfer account
- **CUST-PRI-CARD-HOLDER-IND** (PIC X(01)) - Primary cardholder indicator
- **CUST-FICO-CREDIT-SCORE** (PIC 9(03)) - FICO credit score

### 2. Account Entity

**Source Files:** COBIL00C.cbl, COACTUPC.cbl, CVACT01Y (COPY file)
**Record Structure:** ACCOUNT-RECORD

**Attributes:**
- **ACCT-ID** (PIC 9(11)) - Account unique identifier
- **ACCT-ACTIVE-STATUS** (PIC X(01)) - Account status (Y/N)
- **ACCT-CURR-BAL** (PIC S9(10)V99) - Current account balance
- **ACCT-CREDIT-LIMIT** (PIC S9(10)V99) - Credit limit
- **ACCT-CASH-CREDIT-LIMIT** (PIC S9(10)V99) - Cash advance credit limit
- **ACCT-OPEN-DATE** (PIC X(08)) - Account opening date (YYYYMMDD)
- **ACCT-EXPIRAION-DATE** (PIC X(08)) - Account expiration date
- **ACCT-REISSUE-DATE** (PIC X(08)) - Account reissue date
- **ACCT-CURR-CYC-CREDIT** (PIC S9(10)V99) - Current cycle credit
- **ACCT-CURR-CYC-DEBIT** (PIC S9(10)V99) - Current cycle debit
- **ACCT-GROUP-ID** (PIC X(10)) - Account group identifier

### 3. Card Entity

**Source Files:** COCRDSLC.cbl, COCRDUPC.cbl, CVACT02Y (COPY file)
**Record Structure:** CARD-RECORD

**Attributes:**
- **CARD-NUM** (PIC 9(16)) - Card number
- **CARD-ACCT-ID** (PIC 9(11)) - Associated account ID
- **CARD-CVV-CD** (PIC 9(03)) - Card verification value
- **CARD-EMBOSSED-NAME** (PIC X(50)) - Name embossed on card
- **CARD-EXPIRAION-DATE** (PIC X(08)) - Card expiration date
- **CARD-ACTIVE-STATUS** (PIC X(01)) - Card status (Y/N)

### 4. Card Cross-Reference Entity

**Source Files:** COTRN02C.cbl, COBIL00C.cbl, CVACT03Y (COPY file)
**Record Structure:** CARD-XREF-RECORD

**Attributes:**
- **XREF-CARD-NUM** (PIC 9(16)) - Card number (primary key)
- **XREF-CUST-ID** (PIC 9(09)) - Customer ID reference
- **XREF-ACCT-ID** (PIC 9(11)) - Account ID reference

### 5. Transaction Entity

**Source Files:** COTRN01C.cbl, COTRN00C.cbl, COTRN02C.cbl, COBIL00C.cbl, CVTRA05Y (COPY file)
**Record Structure:** TRAN-RECORD

**Attributes:**
- **TRAN-ID** (PIC 9(16)) - Transaction unique identifier
- **TRAN-CARD-NUM** (PIC 9(16)) - Associated card number
- **TRAN-TYPE-CD** (PIC X(02)) - Transaction type code
- **TRAN-CAT-CD** (PIC 9(04)) - Transaction category code
- **TRAN-SOURCE** (PIC X(10)) - Transaction source
- **TRAN-DESC** (PIC X(26)) - Transaction description
- **TRAN-AMT** (PIC S9(9)V99) - Transaction amount
- **TRAN-MERCHANT-ID** (PIC 9(09)) - Merchant identifier
- **TRAN-MERCHANT-NAME** (PIC X(50)) - Merchant name
- **TRAN-MERCHANT-CITY** (PIC X(50)) - Merchant city
- **TRAN-MERCHANT-ZIP** (PIC X(10)) - Merchant ZIP code
- **TRAN-ORIG-TS** (PIC X(26)) - Original timestamp
- **TRAN-PROC-TS** (PIC X(26)) - Processing timestamp

### 6. User Security Entity

**Source Files:** COSGN00C.cbl, COUSR01C.cbl, COUSR02C.cbl, COUSR03C.cbl, CSUSR01Y (COPY file)
**Record Structure:** SEC-USER-DATA

**Attributes:**
- **SEC-USR-ID** (PIC X(08)) - User unique identifier
- **SEC-USR-FNAME** (PIC X(25)) - User first name
- **SEC-USR-LNAME** (PIC X(25)) - User last name
- **SEC-USR-PWD** (PIC X(08)) - User password
- **SEC-USR-TYPE** (PIC X(01)) - User type (A=Admin, R=Regular)

## Entity Relationships

### Primary Relationships

| Parent Entity | Child Entity | Relationship Type | Cardinality | Description |
|---------------|--------------|-------------------|-------------|-------------|
| Customer | Account | One-to-Many | 1:N | One customer can have multiple accounts |
| Account | Card | One-to-Many | 1:N | One account can have multiple cards |
| Card | Transaction | One-to-Many | 1:N | One card can have multiple transactions |

### Cross-Reference Relationships

| Entity 1 | Entity 2 | Relationship Type | Cardinality | Description |
|----------|----------|-------------------|-------------|-------------|
| Customer | Card | Many-to-Many | M:N | Customers linked to cards via cross-reference |
| Account | Card | One-to-Many | 1:N | Accounts linked to cards via cross-reference |

### Independent Entities

| Entity | Description |
|--------|-------------|
| User Security | Independent authentication entity for system access |

## Data Flow and Business Logic

### Account-Card-Transaction Hierarchy
1. **Customer** creates **Account** (1:N relationship)
2. **Account** issues **Card** (1:N relationship)
3. **Card** processes **Transaction** (1:N relationship)

### Cross-Reference Navigation
- **CARD-XREF-RECORD** enables bidirectional navigation:
  - From Account ID to Card Number(s)
  - From Card Number to Account ID and Customer ID
  - From Customer ID to associated Card Number(s)

### Transaction Processing Flow
1. Transaction initiated with Card Number
2. Card Number resolved to Account ID via cross-reference
3. Account balance updated based on transaction amount
4. Transaction record created with full merchant and timestamp details

## File Relationships

### VSAM Files and Entity Mapping

| VSAM File | Entity | Purpose |
|-----------|--------|---------|
| ACCTDAT | Account | Account master data storage |
| CUSTDAT | Customer | Customer master data storage |
| CARDDAT | Card | Card master data storage |
| TRANSACT | Transaction | Transaction history storage |
| USRSEC | User Security | User authentication data |
| CXACAIX | Card Cross-Reference | Account-to-Card index |
| CCXREF | Card Cross-Reference | Card-to-Account index |

## Business Rules Identified

### Account Management
- Account status must be 'Y' (active) for transactions
- Credit limit enforcement on transaction processing
- Current balance updated real-time with transactions

### Card Management
- Card expiration date validation
- Card status must be active for transaction processing
- CVV code validation for security

### Transaction Processing
- Transaction amount validation against account limits
- Merchant information capture for all transactions
- Dual timestamp tracking (original and processing)

### User Security
- User type determines system access level (Admin vs Regular)
- User authentication required for all system access

## Entity Relationship Diagram

![Entity Relationship Diagram](entity_relationships.svg)

The diagram above illustrates the complete business entity model with:
- Rectangular boxes representing each business entity
- Lines connecting related entities
- Cardinality indicators (1:1, 1:N, M:N)
- Clear visual representation of the hierarchical structure

## Modernization Considerations

### Key Business Entities for Migration
1. **Customer** - Core customer master data
2. **Account** - Financial account information
3. **Card** - Payment instrument data
4. **Transaction** - Transaction history and processing
5. **User Security** - System access and authentication

### Critical Relationships to Preserve
- Customer-Account hierarchy
- Account-Card associations
- Card-Transaction linkage
- Cross-reference navigation capabilities

### Data Integrity Requirements
- Referential integrity between Customer, Account, and Card
- Transaction audit trail preservation
- User authentication and authorization model
- Balance calculation consistency

This analysis provides the foundation for modernizing the CardDemo COBOL legacy system while preserving essential business logic and data relationships.
