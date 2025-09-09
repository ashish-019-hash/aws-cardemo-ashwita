# CardDemo Business Entities Analysis

## Overview

This document presents the comprehensive analysis of business entities extracted from the CardDemo COBOL legacy system. The analysis covers all 18 COBOL files in the `00.phase-1-input` folder, focusing on real-world business entities essential for domain modeling and modernization.

## Business Entities Identified

### 1. Account Entity

**Source**: `ACCT-UPDATE-RECORD` structure in `COACTUPC.cbl`

**Description**: Represents a credit card account with financial details and lifecycle information.

**Attributes**:
- **ACCT-UPDATE-ID** (PIC 9(11)) - Unique account identifier
- **ACCT-UPDATE-ACTIVE-STATUS** (PIC X(01)) - Account status (Y/N)
- **ACCT-UPDATE-CURR-BAL** (PIC S9(10)V99) - Current account balance
- **ACCT-UPDATE-CREDIT-LIMIT** (PIC S9(10)V99) - Credit limit amount
- **ACCT-UPDATE-CASH-CREDIT-LIMIT** (PIC S9(10)V99) - Cash advance limit
- **ACCT-UPDATE-OPEN-DATE** (PIC X(10)) - Account opening date
- **ACCT-UPDATE-EXPIRAION-DATE** (PIC X(10)) - Account expiration date
- **ACCT-UPDATE-REISSUE-DATE** (PIC X(10)) - Last reissue date
- **ACCT-UPDATE-CURR-CYC-CREDIT** (PIC S9(10)V99) - Current cycle credits
- **ACCT-UPDATE-CURR-CYC-DEBIT** (PIC S9(10)V99) - Current cycle debits
- **ACCT-UPDATE-GROUP-ID** (PIC X(10)) - Account group identifier

### 2. Customer Entity

**Source**: `CUST-UPDATE-RECORD` structure in `COACTUPC.cbl`

**Description**: Represents customer personal and demographic information.

**Attributes**:
- **CUST-UPDATE-ID** (PIC 9(09)) - Unique customer identifier
- **CUST-UPDATE-FIRST-NAME** (PIC X(25)) - Customer first name
- **CUST-UPDATE-MIDDLE-NAME** (PIC X(25)) - Customer middle name
- **CUST-UPDATE-LAST-NAME** (PIC X(25)) - Customer last name
- **CUST-UPDATE-ADDR-LINE-1** (PIC X(50)) - Primary address line
- **CUST-UPDATE-ADDR-LINE-2** (PIC X(50)) - Secondary address line
- **CUST-UPDATE-ADDR-LINE-3** (PIC X(50)) - Additional address line
- **CUST-UPDATE-ADDR-STATE-CD** (PIC X(02)) - State code
- **CUST-UPDATE-ADDR-COUNTRY-CD** (PIC X(03)) - Country code
- **CUST-UPDATE-ADDR-ZIP** (PIC X(10)) - ZIP/postal code
- **CUST-UPDATE-PHONE-NUM-1** (PIC X(15)) - Primary phone number
- **CUST-UPDATE-PHONE-NUM-2** (PIC X(15)) - Secondary phone number
- **CUST-UPDATE-SSN** (PIC 9(09)) - Social Security Number
- **CUST-UPDATE-GOVT-ISSUED-ID** (PIC X(20)) - Government issued ID
- **CUST-UPDATE-DOB-YYYY-MM-DD** (PIC X(10)) - Date of birth
- **CUST-UPDATE-EFT-ACCOUNT-ID** (PIC X(10)) - Electronic funds transfer account
- **CUST-UPDATE-PRI-CARD-IND** (PIC X(01)) - Primary cardholder indicator
- **CUST-UPDATE-FICO-CREDIT-SCORE** (PIC 9(03)) - FICO credit score

### 3. Card Entity

**Source**: `CARD-UPDATE-RECORD` structure in `COCRDUPC.cbl`

**Description**: Represents credit card details and status information.

**Attributes**:
- **CARD-UPDATE-NUM** (PIC X(16)) - Credit card number
- **CARD-UPDATE-ACCT-ID** (PIC 9(11)) - Associated account ID
- **CARD-UPDATE-CVV-CD** (PIC 9(03)) - Card verification value
- **CARD-UPDATE-EMBOSSED-NAME** (PIC X(50)) - Name embossed on card
- **CARD-UPDATE-EXPIRAION-DATE** (PIC X(10)) - Card expiration date
- **CARD-UPDATE-ACTIVE-STATUS** (PIC X(01)) - Card active status

### 4. Transaction Entity

**Source**: `TRAN-RECORD` referenced in `COTRN01C.cbl`, `COTRN02C.cbl`, `COBIL00C.cbl`

**Description**: Represents financial transactions processed through the system.

**Attributes**:
- **TRAN-ID** (PIC X(16)) - Unique transaction identifier
- **TRAN-CARD-NUM** (PIC X(16)) - Associated card number
- **TRAN-TYPE-CD** (PIC X(02)) - Transaction type code
- **TRAN-CAT-CD** (PIC 9(02)) - Transaction category code
- **TRAN-SOURCE** (PIC X(10)) - Transaction source
- **TRAN-AMT** (PIC S9(9)V99) - Transaction amount
- **TRAN-DESC** (PIC X(50)) - Transaction description
- **TRAN-ORIG-TS** (PIC X(26)) - Original timestamp
- **TRAN-PROC-TS** (PIC X(26)) - Processing timestamp
- **TRAN-MERCHANT-ID** (PIC X(15)) - Merchant identifier
- **TRAN-MERCHANT-NAME** (PIC X(50)) - Merchant name
- **TRAN-MERCHANT-CITY** (PIC X(50)) - Merchant city
- **TRAN-MERCHANT-ZIP** (PIC X(10)) - Merchant ZIP code

### 5. User Entity

**Source**: `SEC-USER-DATA` structure in `COUSR01C.cbl`

**Description**: Represents system users with authentication and authorization information.

**Attributes**:
- **SEC-USR-ID** (PIC X(08)) - Unique user identifier
- **SEC-USR-FNAME** (PIC X(25)) - User first name
- **SEC-USR-LNAME** (PIC X(25)) - User last name
- **SEC-USR-PWD** (PIC X(08)) - User password
- **SEC-USR-TYPE** (PIC X(01)) - User type (Admin/Regular)

### 6. Card Cross-Reference Entity

**Source**: `CARD-XREF-RECORD` operations in `COBIL00C.cbl`, `COTRN02C.cbl`

**Description**: Links accounts, customers, and cards for navigation and relationship management.

**Attributes**:
- **XREF-ACCT-ID** (PIC 9(11)) - Account identifier
- **XREF-CUST-ID** (PIC 9(09)) - Customer identifier
- **XREF-CARD-NUM** (PIC X(16)) - Card number

## Entity Relationships

### Primary Relationships

| Parent Entity | Child Entity | Relationship Type | Foreign Key | Description |
|---------------|--------------|-------------------|-------------|-------------|
| Customer | Account | 1:N | CUST-ID → ACCT-CUST-ID | One customer can have multiple accounts |
| Account | Card | 1:N | ACCT-ID → CARD-ACCT-ID | One account can have multiple cards |
| Card | Transaction | 1:N | CARD-NUM → TRAN-CARD-NUM | One card can have multiple transactions |
| User | Customer | 1:1 | USR-ID ↔ CUST-ID | Authentication relationship |

### Cross-Reference Relationships

The **Card Cross-Reference Entity** serves as a junction table that enables:
- **Account ↔ Customer**: Links accounts to their owning customers
- **Account ↔ Card**: Links accounts to their associated cards
- **Customer ↔ Card**: Indirect relationship through account linkage

### Relationship Details

1. **Customer to Account (1:N)**
   - A customer can own multiple credit card accounts
   - Each account belongs to exactly one customer
   - Managed through the cross-reference structure

2. **Account to Card (1:N)**
   - An account can have multiple cards (primary, secondary, etc.)
   - Each card is associated with exactly one account
   - Direct foreign key relationship via ACCT-ID

3. **Card to Transaction (1:N)**
   - A card can have multiple transactions over time
   - Each transaction is associated with exactly one card
   - Direct foreign key relationship via CARD-NUM

4. **User to Customer (1:1)**
   - Each system user corresponds to one customer
   - Authentication and authorization relationship
   - Enables role-based access control

## Business Rules Identified

### Account Business Rules
- Account status must be 'Y' (active) or 'N' (inactive)
- Credit limit must be positive
- Current balance can be negative (indicating credit available)
- Account must have valid open date

### Customer Business Rules
- Customer ID is a 9-digit unique identifier
- SSN must be 9 digits
- FICO score must be between 300-850
- Primary cardholder indicator must be 'Y' or 'N'

### Card Business Rules
- Card number is 16 digits
- CVV code is 3 digits
- Card must have valid expiration date
- Card status must be 'Y' (active) or 'N' (inactive)

### Transaction Business Rules
- Transaction ID is unique 16-character identifier
- Transaction amount can be positive (charges) or negative (credits)
- Transaction must have valid card number
- Transaction type and category codes define transaction classification

### User Business Rules
- User ID is 8-character unique identifier
- User type determines system access level
- Password required for authentication

## Files Analyzed

The following 18 COBOL files were analyzed for business entity extraction:

1. `COACTUPC.cbl` - Account update processing
2. `COACTVWC.cbl` - Account view processing
3. `COADM01C.cbl` - Admin menu
4. `COBIL00C.cbl` - Bill payment processing
5. `COCRDLIC.cbl` - Credit card listing
6. `COCRDSLC.cbl` - Credit card detail view
7. `COCRDUPC.cbl` - Credit card update processing
8. `COMEN01C.cbl` - Main menu
9. `CORPT00C.cbl` - Report generation
10. `COSGN00C.cbl` - Sign-on processing
11. `COTRN00C.cbl` - Transaction listing
12. `COTRN01C.cbl` - Transaction view
13. `COTRN02C.cbl` - Transaction add
14. `COUSR00C.cbl` - User listing
15. `COUSR01C.cbl` - User add
16. `COUSR02C.cbl` - User update
17. `COUSR03C.cbl` - User delete
18. `CSUTLDTC.cbl` - Date utility

## Entity Relationship Diagram

See the accompanying SVG diagram: [entity_relationships.svg](./entity_relationships.svg)

## Modernization Considerations

### Data Migration
- All entities have well-defined structures suitable for relational database migration
- Primary and foreign key relationships are clearly established
- Data types can be mapped to modern database systems

### API Design
- Each entity can be exposed as a REST resource
- Relationships support nested resource patterns
- Business rules can be implemented as validation logic

### Microservices Architecture
- Entities can be grouped into domain-bounded contexts:
  - **Customer Management**: Customer, User entities
  - **Account Management**: Account, Card entities
  - **Transaction Processing**: Transaction entity
  - **Cross-Reference Management**: Card Cross-Reference entity

### Security Considerations
- User entity provides foundation for authentication
- Customer-Account relationships support authorization
- Sensitive data (SSN, CVV) requires encryption in modern systems

## Conclusion

The CardDemo application contains a well-structured set of business entities that represent a complete credit card management domain. The entities are suitable for modernization efforts and provide a solid foundation for migrating to contemporary architectures while preserving business logic and data relationships.
