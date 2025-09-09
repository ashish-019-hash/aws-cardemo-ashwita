# CardDemo Business Entities Analysis

## Overview
This document contains the business entities extracted from the CardDemo COBOL legacy system. The analysis covers all files in the 00.phase-1-input folder and identifies real-world, business-relevant entities essential for domain modeling and modernization.

## Business Entities

### 1. Customer Entity
**Description**: Core customer information with demographics and credit data

**Attributes**:
- Customer ID (Primary Key) - PIC 9(09)
- First Name - PIC X(25)
- Last Name - PIC X(25)
- Date of Birth - PIC X(10)
- SSN (Social Security Number) - PIC 9(09)
- Phone Number - PIC X(15)
- Address Line 1 - PIC X(50)
- Address Line 2 - PIC X(50)
- City - PIC X(25)
- State - PIC X(02)
- ZIP Code - PIC X(10)
- FICO Credit Score - PIC 9(03)
- Status - PIC X(01)

**Source Files**: COACTUPC.cbl (CUST-UPDATE-RECORD), CVCUS01Y copybook references

### 2. Account Entity
**Description**: Credit account details with balances and limits

**Attributes**:
- Account ID (Primary Key) - PIC 9(11)
- Customer ID (Foreign Key) - PIC 9(09)
- Account Status - PIC X(01)
- Account Open Date - PIC X(10)
- Account Expiry Date - PIC X(10)
- Account Reissue Date - PIC X(10)
- Credit Limit - PIC S9(10)V99
- Cash Credit Limit - PIC S9(10)V99
- Current Balance - PIC S9(10)V99
- Current Cycle Credit - PIC S9(10)V99
- Current Cycle Debit - PIC S9(10)V99
- Group ID - PIC X(10)

**Source Files**: COACTUPC.cbl (ACCT-UPDATE-RECORD), COACTVWC.cbl, CVACT01Y copybook references

### 3. Card Entity
**Description**: Physical credit card information and status

**Attributes**:
- Card Number (Primary Key) - PIC X(16)
- Account ID (Foreign Key) - PIC 9(11)
- CVV Code - PIC 9(03)
- Embossed Name - PIC X(50)
- Expiry Date - PIC X(10)
- Card Status - PIC X(01)
- Card Active Status - PIC X(01)

**Source Files**: COCRDUPC.cbl (CARD-UPDATE-RECORD), COCRDSLC.cbl, COCRDLIC.cbl, CVACT02Y copybook references

### 4. Transaction Entity
**Description**: Payment and purchase transaction records

**Attributes**:
- Transaction ID (Primary Key) - PIC X(16)
- Card Number (Foreign Key) - PIC X(16)
- Transaction Type Code - PIC X(02)
- Transaction Category Code - PIC 9(04)
- Transaction Source - PIC X(10)
- Transaction Amount - PIC S9(10)V99
- Transaction Description - PIC X(50)
- Original Timestamp - PIC X(26)
- Processing Timestamp - PIC X(26)
- Merchant ID - PIC X(15)
- Merchant Name - PIC X(50)
- Merchant City - PIC X(50)
- Merchant ZIP - PIC X(10)

**Source Files**: COTRN01C.cbl, COTRN00C.cbl, COTRN02C.cbl (TRAN-RECORD), CVTRA05Y copybook references

### 5. Card Cross-Reference Entity
**Description**: Junction table linking customers, accounts, and cards

**Attributes**:
- Card Number (Primary Key) - PIC X(16)
- Account ID (Foreign Key) - PIC 9(11)
- Customer ID (Foreign Key) - PIC 9(09)

**Source Files**: COACTVWC.cbl, COACTUPC.cbl, COTRN02C.cbl (CARD-XREF-RECORD), CVACT03Y copybook references

### 6. User Security Entity
**Description**: System user authentication and authorization data

**Attributes**:
- User ID (Primary Key) - PIC X(08)
- Password - PIC X(08)
- User Type - PIC X(01)
- First Name - PIC X(25)
- Last Name - PIC X(25)

**Source Files**: COSGN00C.cbl, COUSR01C.cbl, COUSR02C.cbl, COUSR03C.cbl (SEC-USER-DATA), CSUSR01Y copybook references

## Entity Relationships

### Primary Relationships
1. **Customer ↔ Account** (1:N)
   - One customer can have multiple accounts
   - Each account belongs to exactly one customer
   - Linked via Customer ID

2. **Account ↔ Card** (1:N)
   - One account can have multiple cards
   - Each card belongs to exactly one account
   - Linked via Account ID

3. **Card ↔ Transaction** (1:N)
   - One card can have multiple transactions
   - Each transaction belongs to exactly one card
   - Linked via Card Number

4. **Account ↔ Card Cross-Reference** (1:N)
   - Cross-reference table enables efficient lookups
   - Links Account ID to Card Number and Customer ID
   - Used for navigation between related entities

5. **User ↔ Customer** (1:1)
   - Authentication relationship
   - One user corresponds to one customer for system access
   - Linked via User ID matching Customer ID patterns

### Relationship Summary Table

| Parent Entity | Child Entity | Relationship Type | Foreign Key |
|---------------|--------------|-------------------|-------------|
| Customer | Account | 1:N | Customer ID |
| Account | Card | 1:N | Account ID |
| Card | Transaction | 1:N | Card Number |
| Account | Card Cross-Reference | 1:N | Account ID |
| Customer | Card Cross-Reference | 1:N | Customer ID |
| User Security | Customer | 1:1 | User ID |

## Entity Relationship Diagram
![Entity Relationships](entity_relationships.svg)

## Technical Notes

### File Patterns Analyzed
- **COPY Statements**: CVACT01Y, CVACT02Y, CVACT03Y, CVCUS01Y, CVTRA05Y, CSUSR01Y
- **Record Structures**: ACCT-UPDATE-RECORD, CUST-UPDATE-RECORD, CARD-UPDATE-RECORD, TRAN-RECORD, CARD-XREF-RECORD, SEC-USER-DATA
- **File Operations**: CICS read/write operations across ACCTDAT, CUSTDAT, CARDDAT, TRANSACT, USRSEC, CXACAIX files

### Excluded Items
As per requirements, the following were excluded from entity extraction:
- UI fields and display-related variables
- Helper or technical variables (WS-, TEMP-, FILLER, FLAGS)
- Screen mapping variables
- Control fields, counters, index variables
- COBOL structure-only items like 77-level values with no business context

### Data Sources
All entities were extracted from analysis of 18 COBOL programs in the 00.phase-1-input folder:
- COACTVWC.cbl, COCRDSLC.cbl, COTRN01C.cbl, COACTUPC.cbl, COUSR01C.cbl
- COTRN00C.cbl, COCRDUPC.cbl, COTRN02C.cbl, CORPT00C.cbl, COUSR02C.cbl
- COSGN00C.cbl, COMEN01C.cbl, COCRDLIC.cbl, COUSR00C.cbl, COADM01C.cbl
- COBIL00C.cbl, COUSR03C.cbl, CSUTLDTC.cbl
