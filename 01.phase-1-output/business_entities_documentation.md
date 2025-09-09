# COBOL Legacy System Business Entities Analysis

## Overview

This document presents the business entities extracted from the CardDemo COBOL legacy system for modernization purposes. The analysis focused on identifying real-world, business-relevant entities essential for domain modeling while filtering out UI fields, technical variables, and COBOL structure-only items.

## Business Entities

### 1. User Entity

**Purpose**: Manages system users with authentication and authorization capabilities

**Attributes**:
- `SEC-USR-ID` (PIC X(08)) - Unique user identifier (Primary Key)
- `SEC-USR-FNAME` (PIC X) - User first name
- `SEC-USR-LNAME` (PIC X) - User last name  
- `SEC-USR-PWD` (PIC X(08)) - User password
- `SEC-USR-TYPE` (PIC X) - User type (Admin/Regular user designation)

**Source Files**: COUSR01C.cbl, COUSR02C.cbl, COUSR03C.cbl, COSGN00C.cbl
**COPY File Reference**: CSUSR01Y

---

### 2. Customer Entity

**Purpose**: Stores customer personal and demographic information

**Attributes**:
- `CUST-ID` (PIC 9(09)) - Unique customer identifier (Primary Key)
- Customer personal information fields (detailed structure in CVCUS01Y)
- Customer contact information
- Customer demographic data

**Source Files**: COACTVWC.cbl, COACTUPC.cbl, COCRDUPC.cbl
**COPY File Reference**: CVCUS01Y

---

### 3. Account Entity

**Purpose**: Manages customer account information including balances and limits

**Attributes**:
- `ACCT-ID` (PIC 9(11)) - Unique account identifier (Primary Key)
- `ACCT-ACTIVE-STATUS` (PIC X(01)) - Account active status flag
- `ACCT-CURR-BAL` (PIC S9(10)V99) - Current account balance
- `ACCT-CREDIT-LIMIT` (PIC S9(10)V99) - Credit limit amount
- `ACCT-CASH-CREDIT-LIMIT` (PIC S9(10)V99) - Cash advance credit limit
- `ACCT-OPEN-DATE` (PIC X(08)) - Account opening date (YYYYMMDD)
- `ACCT-EXPIRAION-DATE` (PIC X(08)) - Account expiration date (YYYYMMDD)
- `ACCT-REISSUE-DATE` (PIC X(08)) - Account reissue date (YYYYMMDD)

**Source Files**: COACTVWC.cbl, COACTUPC.cbl, COBIL00C.cbl, COTRN02C.cbl
**COPY File Reference**: CVACT01Y

---

### 4. Card Entity

**Purpose**: Manages credit card details and card-specific information

**Attributes**:
- `CARD-NUM` (PIC 9(16)) - Credit card number (Primary Key)
- `CARD-ACCT-ID` (PIC 9(11)) - Associated account ID (Foreign Key → Account)
- `CARD-CVV-CD` (PIC 9(03)) - Card verification value code
- `CARD-NAME-EMBOSSED` (PIC X(50)) - Name embossed on card
- `CARD-STATUS` (PIC X) - Card status indicator
- `CARD-EXPIRAION-DATE` (PIC 9(10)) - Card expiration date
- Card expiry components:
  - `CARD-EXPIRY-YEAR` (PIC X(4)) - Expiration year
  - `CARD-EXPIRY-MONTH` (PIC X(2)) - Expiration month
  - `CARD-EXPIRY-DAY` (PIC X(2)) - Expiration day

**Source Files**: COCRDSLC.cbl, COCRDUPC.cbl, COCRDLIC.cbl
**COPY File Reference**: CVACT02Y

---

### 5. Transaction Entity

**Purpose**: Records all card transaction activities and payment processing

**Attributes**:
- `TRAN-ID` (PIC 9(16)) - Unique transaction identifier (Primary Key)
- `TRAN-CARD-NUM` (PIC 9(16)) - Card number used (Foreign Key → Card)
- `TRAN-TYPE-CD` (PIC X) - Transaction type code
- `TRAN-CAT-CD` (PIC X) - Transaction category code
- `TRAN-SOURCE` (PIC X) - Transaction source identifier
- `TRAN-AMT` (PIC S9(9)V99) - Transaction amount
- `TRAN-DESC` (PIC X) - Transaction description
- `TRAN-ORIG-TS` (PIC X) - Original transaction timestamp
- `TRAN-PROC-TS` (PIC X) - Processing timestamp
- `TRAN-MERCHANT-ID` (PIC 9(9)) - Merchant identifier
- `TRAN-MERCHANT-NAME` (PIC X) - Merchant name
- `TRAN-MERCHANT-CITY` (PIC X) - Merchant city

**Source Files**: COTRN00C.cbl, COTRN01C.cbl, COTRN02C.cbl, COBIL00C.cbl, CORPT00C.cbl
**COPY File Reference**: CVTRA05Y

---

### 6. Card Cross-Reference Entity

**Purpose**: Junction table linking accounts to cards for cross-referencing

**Attributes**:
- `XREF-ACCT-ID` (PIC 9(11)) - Account ID (Foreign Key → Account)
- `XREF-CARD-NUM` (PIC X(16)) - Card number (Foreign Key → Card)
- `XREF-CUST-ID` (PIC 9(09)) - Customer ID (Foreign Key → Customer)

**Source Files**: COBIL00C.cbl, COACTVWC.cbl, COACTUPC.cbl, COTRN02C.cbl
**COPY File Reference**: CVACT03Y

---

## Entity Relationships

### Primary Relationships

| Parent Entity | Child Entity | Relationship Type | Description |
|---------------|--------------|-------------------|-------------|
| Customer | Account | 1:N | One customer can have multiple accounts |
| Account | Card | 1:N | One account can have multiple cards |
| Card | Transaction | 1:N | One card can have multiple transactions |
| User | System Access | 1:1 | One user has one system access profile |

### Cross-Reference Relationships

| Entity 1 | Entity 2 | Via | Relationship Type | Description |
|----------|----------|-----|-------------------|-------------|
| Customer | Card | Card Cross-Reference | N:M | Customers linked to cards through accounts |
| Account | Card | Card Cross-Reference | 1:N | Direct account-to-card mapping |
| Account | Transaction | Card → Transaction | 1:N | Account transactions via associated cards |

### Relationship Details

- **Customer ↔ Account**: Customers own accounts that manage their financial relationships
- **Account ↔ Card**: Accounts can have multiple cards issued against them
- **Card ↔ Transaction**: All transactions are processed against specific cards
- **Card Cross-Reference**: Provides efficient lookup between customers, accounts, and cards
- **User**: Independent entity for system authentication and authorization

## Entity Relationship Diagram

![Entity Relationships](entity_relationships.svg)

The diagram shows the complete relationship structure with cardinalities and foreign key connections between all business entities.

## Data Files and Access Patterns

### VSAM Files Referenced
- **USRSEC**: User security and authentication data
- **CUSTDAT**: Customer master information
- **ACCTDAT**: Account master with balances and limits
- **CARDDAT**: Credit card master data
- **TRANSACT**: Transaction history and details
- **CXACAIX**: Card cross-reference index for lookups

### Key Access Patterns
- Customer lookup via CUST-ID
- Account access via ACCT-ID
- Card validation via CARD-NUM
- Transaction retrieval via TRAN-ID
- Cross-reference lookups via XREF structures

## Modernization Considerations

### Entity Normalization
- All entities follow proper normalization principles
- Clear primary and foreign key relationships
- Minimal data redundancy through cross-reference structures

### Business Rules Embedded
- Account balance and credit limit validations
- Card expiration date business logic
- Transaction processing workflows
- User authentication and authorization patterns

### Integration Points
- Card Cross-Reference enables efficient multi-entity queries
- Transaction entity captures complete audit trail
- Account entity centralizes financial data management
- User entity provides security framework

---

*Analysis completed on all 18 COBOL files in 00.phase-1-input folder*
*Technical variables (WS-, TEMP-, FILLER, FLAGS) and UI fields excluded as requested*
