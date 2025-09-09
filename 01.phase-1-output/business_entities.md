# CardDemo COBOL Legacy System - Business Entities Analysis

## Overview
This document presents the business entities extracted from the CardDemo COBOL legacy system located in the `00.phase-1-input` folder. The analysis focused on identifying real-world, business-relevant entities essential for domain modeling and modernization.

## Business Entities

### 1. Account Entity
**Source**: COACTUPC.cbl (lines 418-433)

**Attributes**:
- Account ID (PIC 9(11)) - Primary key, 11-digit account identifier
- Active Status (PIC X(01)) - Account status indicator
- Current Balance (PIC S9(10)V99) - Current account balance with decimal precision
- Credit Limit (PIC S9(10)V99) - Maximum credit limit allowed
- Cash Credit Limit (PIC S9(10)V99) - Cash advance credit limit
- Open Date (PIC X(10)) - Account opening date
- Expiration Date (PIC X(10)) - Account expiration date
- Reissue Date (PIC X(10)) - Last reissue date
- Current Cycle Credit (PIC S9(10)V99) - Current billing cycle credits
- Current Cycle Debit (PIC S9(10)V99) - Current billing cycle debits
- Group ID (PIC X(10)) - Account group identifier

### 2. Customer Entity
**Source**: COACTUPC.cbl (lines 434-456)

**Attributes**:
- Customer ID (PIC 9(09)) - Primary key, 9-digit customer identifier
- First Name (PIC X(25)) - Customer first name
- Middle Name (PIC X(25)) - Customer middle name
- Last Name (PIC X(25)) - Customer last name
- Address Line 1 (PIC X(50)) - Primary address line
- Address Line 2 (PIC X(50)) - Secondary address line
- Address Line 3 (PIC X(50)) - Additional address line
- State Code (PIC X(02)) - State/province code
- Country Code (PIC X(03)) - Country code
- ZIP Code (PIC X(10)) - Postal/ZIP code
- Phone Number 1 (PIC X(15)) - Primary phone number
- Phone Number 2 (PIC X(15)) - Secondary phone number
- SSN (PIC 9(09)) - Social Security Number
- Government Issued ID (PIC X(20)) - Government identification
- Date of Birth (PIC X(10)) - Customer date of birth (YYYY-MM-DD)
- EFT Account ID (PIC X(10)) - Electronic funds transfer account
- Primary Card Indicator (PIC X(01)) - Primary cardholder flag
- FICO Credit Score (PIC 9(03)) - Credit score (300-850 range)

### 3. Card Entity
**Source**: COCRDUPC.cbl (lines 314-321)

**Attributes**:
- Card Number (PIC X(16)) - Primary key, 16-digit card number
- Account ID (PIC 9(11)) - Foreign key to Account entity
- CVV Code (PIC 9(03)) - Card verification value
- Embossed Name (PIC X(50)) - Name printed on card
- Expiration Date (PIC X(10)) - Card expiration date
- Active Status (PIC X(01)) - Card status indicator

### 4. Transaction Entity
**Source**: Referenced through CVTRA05Y copybook in multiple files
**Files**: COTRN00C.cbl, COTRN01C.cbl, COTRN02C.cbl, COBIL00C.cbl, CORPT00C.cbl

**Attributes**:
- Transaction ID (PIC X(16)) - Primary key, unique transaction identifier
- Card Number (PIC X(16)) - Foreign key to Card entity
- Account ID (PIC 9(11)) - Foreign key to Account entity
- Transaction Amount (PIC S9(9)V99) - Transaction amount with decimal precision
- Transaction Date (PIC X(08)) - Transaction date
- Transaction Type - Type of transaction (debit/credit)
- Merchant Information - Transaction merchant details
- Transaction Status - Current status of transaction

### 5. User Entity
**Source**: CSUSR01Y copybook referenced in COUSR01C.cbl, COUSR02C.cbl, COUSR03C.cbl, COSGN00C.cbl

**Attributes**:
- User ID (PIC X(08)) - Primary key, user identifier
- First Name (PIC X(25)) - User first name
- Last Name (PIC X(25)) - User last name
- Password (PIC X(08)) - User password
- User Type (PIC X(08)) - User type (Admin/Regular)

### 6. Card Cross-Reference Entity
**Source**: Referenced through CVACT03Y copybook
**Files**: COACTUPC.cbl, COTRN02C.cbl, COBIL00C.cbl

**Attributes**:
- Card Number (PIC X(16)) - Foreign key to Card entity
- Customer ID (PIC 9(09)) - Foreign key to Customer entity
- Account ID (PIC 9(11)) - Foreign key to Account entity

## Entity Relationships

### Primary Relationships

| Relationship | Cardinality | Description |
|--------------|-------------|-------------|
| Account ↔ Customer | 1:1 | Each account belongs to one customer, each customer can have one primary account |
| Account ↔ Card | 1:N | One account can have multiple cards |
| Card ↔ Transaction | 1:N | One card can have multiple transactions |
| Customer ↔ Card | 1:N | One customer can have multiple cards (through accounts) |
| User → All Entities | 1:N | Users (Admin/Regular) manage and access all business entities |

### Cross-Reference Relationships
- **Card Cross-Reference** serves as a junction table linking Account, Customer, and Card entities
- Enables efficient lookups between related entities
- Supports both forward and reverse navigation (Account→Card, Card→Account, Customer→Card)

## Entity Relationship Diagram

![Entity Relationships](entity_relationships.svg)

## Data Files and Sources

### COBOL Files Analyzed
- **Account Management**: COACTUPC.cbl, COACTVWC.cbl
- **Card Management**: COCRDLIC.cbl, COCRDSLC.cbl, COCRDUPC.cbl
- **Transaction Management**: COTRN00C.cbl, COTRN01C.cbl, COTRN02C.cbl
- **User Management**: COUSR00C.cbl, COUSR01C.cbl, COUSR02C.cbl, COUSR03C.cbl
- **Bill Payment**: COBIL00C.cbl
- **Reporting**: CORPT00C.cbl
- **Authentication**: COSGN00C.cbl
- **Menu Systems**: COMEN01C.cbl, COADM01C.cbl
- **Utilities**: CSUTLDTC.cbl

### Key Copybooks Referenced
- **CVACT01Y**: Account record layout
- **CVACT02Y**: Card record layout  
- **CVACT03Y**: Card cross-reference layout
- **CVCUS01Y**: Customer layout
- **CVTRA05Y**: Transaction record layout
- **CSUSR01Y**: User security layout

## Modernization Considerations

### Entity Normalization
- Entities are well-normalized with clear primary and foreign key relationships
- Customer and Account have 1:1 relationship but are separate entities for flexibility
- Card Cross-Reference provides efficient many-to-many resolution

### Data Integrity
- Strong typing with COBOL PIC clauses defines data constraints
- Numeric fields have appropriate precision for financial calculations
- Date fields use consistent YYYY-MM-DD format

### Security and Access
- User entity supports role-based access (Admin vs Regular users)
- Password management through User entity
- Audit trail capabilities through transaction history

## Analysis Summary

This analysis systematically examined all 18 COBOL files in the `00.phase-1-input` folder, focusing on business-relevant entities while excluding:

- UI fields and display-related variables
- Helper or technical variables (WS-, TEMP-, FILLER, FLAGS, screen mappings)
- Control fields, counters, index variables
- COBOL structure-only items (77-level values with no business context)

The identified entities represent the core business domain of the CardDemo credit card management system and provide a solid foundation for modernization efforts. The relationships between entities are well-defined through foreign key references and cross-reference tables, enabling comprehensive data modeling for modern application architectures.
