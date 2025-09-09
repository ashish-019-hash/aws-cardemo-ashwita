# CardDemo Business Entities Analysis

## Overview

This document presents the business entities extracted from the CardDemo COBOL legacy system. The analysis focuses on real-world, business-relevant entities essential for domain modeling and modernization, excluding UI fields, technical variables, and COBOL structure-only items.

## Business Entities

### 1. Customer Entity

The Customer entity represents individual customers in the credit card system.

| Attribute | Data Type | Length | Description |
|-----------|-----------|---------|-------------|
| Customer ID | Numeric | 9 digits | Unique customer identifier |
| First Name | Character | 25 chars | Customer's first name |
| Middle Name | Character | 25 chars | Customer's middle name |
| Last Name | Character | 25 chars | Customer's last name |
| Address Line 1 | Character | 50 chars | Primary address line |
| Address Line 2 | Character | 50 chars | Secondary address line |
| Address Line 3 | Character | 50 chars | Additional address line |
| State Code | Character | 2 chars | State abbreviation |
| Country Code | Character | 3 chars | Country code |
| ZIP Code | Character | 10 chars | Postal code |
| Phone Number 1 | Character | 15 chars | Primary phone number |
| Phone Number 2 | Character | 15 chars | Secondary phone number |
| SSN | Numeric | 9 digits | Social Security Number |
| Government Issued ID | Character | 20 chars | Government identification |
| Date of Birth | Date | 8 chars (YYYYMMDD) | Customer's birth date |
| EFT Account ID | Character | 10 chars | Electronic funds transfer account |
| Primary Holder Indicator | Character | 1 char | Primary account holder flag |
| FICO Score | Numeric | 3 digits | Credit score |

### 2. Account Entity

The Account entity represents credit card accounts associated with customers.

| Attribute | Data Type | Length | Description |
|-----------|-----------|---------|-------------|
| Account ID | Numeric | 11 digits | Unique account identifier |
| Active Status | Character | 1 char | Account status (Y/N) |
| Current Balance | Decimal | S9(10)V99 | Current account balance |
| Credit Limit | Decimal | S9(10)V99 | Maximum credit limit |
| Cash Credit Limit | Decimal | S9(10)V99 | Cash advance limit |
| Open Date | Date | 8 chars (YYYYMMDD) | Account opening date |
| Expiration Date | Date | 8 chars (YYYYMMDD) | Account expiration date |
| Reissue Date | Date | 8 chars (YYYYMMDD) | Last reissue date |
| Current Cycle Credit | Decimal | S9(10)V99 | Current billing cycle credits |
| Current Cycle Debit | Decimal | S9(10)V99 | Current billing cycle debits |
| Group ID | Character | 10 chars | Account group identifier |

### 3. Card Entity

The Card entity represents physical credit cards linked to accounts.

| Attribute | Data Type | Length | Description |
|-----------|-----------|---------|-------------|
| Account ID | Numeric | 11 digits | Associated account identifier |
| Card Number | Numeric | 16 digits | Unique card number |
| CVV Code | Numeric | 3 digits | Card verification value |
| Card Name Embossed | Character | 50 chars | Name printed on card |
| Card Status | Character | 1 char | Card status (Active/Inactive) |
| Expiration Year | Character | 4 chars | Card expiration year |
| Expiration Month | Character | 2 chars | Card expiration month |
| Expiration Day | Character | 2 chars | Card expiration day |

### 4. Transaction Entity

The Transaction entity represents all card transaction activities.

| Attribute | Data Type | Length | Description |
|-----------|-----------|---------|-------------|
| Transaction ID | Numeric | 16 digits | Unique transaction identifier |
| Card Number | Numeric | 16 digits | Associated card number |
| Account ID | Numeric | 11 digits | Associated account identifier |
| Transaction Amount | Decimal | S9(9)V99 | Transaction amount |
| Transaction Date | Date | 8 chars (YYYYMMDD) | Transaction date |
| Transaction Type Code | Character | 2 chars | Type of transaction |
| Category Code | Character | 4 chars | Transaction category |
| Transaction Source | Character | 10 chars | Source of transaction |
| Transaction Description | Character | 40 chars | Transaction description |
| Original Timestamp | Timestamp | 26 chars | Original transaction time |
| Processed Timestamp | Timestamp | 26 chars | Processing timestamp |
| Merchant ID | Character | 15 chars | Merchant identifier |
| Merchant Name | Character | 40 chars | Merchant business name |
| Merchant City | Character | 40 chars | Merchant location city |
| Merchant ZIP | Character | 10 chars | Merchant postal code |

### 5. User Entity

The User entity represents system users with authentication and authorization data.

| Attribute | Data Type | Length | Description |
|-----------|-----------|---------|-------------|
| User ID | Character | 8 chars | Unique user identifier |
| First Name | Character | 25 chars | User's first name |
| Last Name | Character | 25 chars | User's last name |
| Password | Character | 8 chars | User authentication password |
| User Type | Character | 8 chars | User role (ADMIN/REGULAR) |

## Entity Relationships

### Primary Relationships

1. **Customer ↔ Account (1:N)**
   - One customer can have multiple accounts
   - Each account belongs to exactly one customer
   - Relationship established through Customer ID

2. **Account ↔ Card (1:N)**
   - One account can have multiple cards
   - Each card belongs to exactly one account
   - Relationship established through Account ID

3. **Card ↔ Transaction (1:N)**
   - One card can have multiple transactions
   - Each transaction belongs to exactly one card
   - Relationship established through Card Number

4. **User Entity (Standalone)**
   - User entity is independent for authentication purposes
   - No direct relationship to customer/account entities
   - Used for system access control

### Cross-Reference Files

The system uses cross-reference files to maintain relationships:

- **CXACAIX**: Cross-reference index linking Account ID to Customer ID and Card Number
- **CARDAIX**: Card index for efficient card lookups by Account ID
- **CCXREF**: Card cross-reference file for card-to-account mappings

## Data Sources

The business entities were extracted from the following COBOL programs:

- **Customer Data**: COACTUPC.cbl (Account Update Program)
- **Account Data**: COACTUPC.cbl, COACTVWC.cbl (Account programs)
- **Card Data**: COCRDUPC.cbl, COCRDLIC.cbl, COCRDSLC.cbl (Card programs)
- **Transaction Data**: COTRN00C.cbl, COTRN01C.cbl, COTRN02C.cbl (Transaction programs)
- **User Data**: COUSR00C.cbl, COUSR01C.cbl, COUSR02C.cbl, COUSR03C.cbl (User programs)

## Entity Relationship Diagram

![Entity Relationship Diagram](entity_relationships.svg)

The diagram above illustrates the relationships between all business entities with proper cardinality notation.

## Excluded Items

The following technical and UI-related items were excluded from the business entity analysis:

- Working storage variables (WS-*)
- Temporary variables (TEMP-*)
- Filler fields
- Screen mapping variables
- Control flags and counters
- Index variables
- CICS technical variables
- Display formatting fields
- Error handling variables
- Program control structures

## Summary

The CardDemo system contains 5 core business entities:
- **Customer**: 18 business attributes
- **Account**: 11 business attributes  
- **Card**: 8 business attributes
- **Transaction**: 16 business attributes
- **User**: 5 business attributes

These entities form a comprehensive data model suitable for modernization efforts, capturing all essential business information while excluding technical implementation details.
