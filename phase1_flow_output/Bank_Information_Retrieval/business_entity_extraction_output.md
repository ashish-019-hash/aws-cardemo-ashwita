# Business Entity Extraction for Bank Information Retrieval

## Overview

This document contains the business entities extracted from the "Bank Information Retrieval" user story. All entities have been verified against the actual database tables in the Scala codebase (OBP-API).

## Extracted Business Entities

### Entity 1: MappedBank

**Database Table Name:** `mappedbank`

**Source File:** `obp-api/src/main/scala/code/model/dataAccess/MappedBank.scala`

**Description:** The primary entity representing a bank in the system. This entity stores core bank information including identification, branding (logo, website), and routing information.

**Relevance to User Story:** This entity directly supports the following acceptance criteria:
- AC-001: Retrieve a list of all banks supported on the platform
- AC-002: Retrieve detailed information for a specific bank by bank ID
- AC-003: Each bank record shall include the bank ID, short name, full name, logo URL, and website URL
- AC-004: The bank list endpoint shall return bank routing information for each bank

**Database Fields:**

| Field Name | Data Type | Description | Maps to API Response |
|------------|-----------|-------------|---------------------|
| id | Long | Primary key (auto-generated) | Internal use |
| permalink | String(255) | Unique bank identifier used in URLs | `id` |
| fullBankName | String(255) | Full name of the bank | `full_name` |
| shortBankName | String(100) | Short/abbreviated name of the bank | `short_name` |
| logoURL | String(255) | URL to the bank's logo image | `logo` |
| websiteURL | String(255) | URL to the bank's website | `website` |
| mBankRoutingScheme | String(255) | Bank routing scheme (e.g., BIC, SWIFT) | `bank_routings[].scheme` |
| mBankRoutingAddress | String(255) | Bank routing address value | `bank_routings[].address` |
| swiftBIC | String(255) | SWIFT BIC code (deprecated) | Deprecated |
| national_identifier | String(255) | National bank identifier (deprecated) | Deprecated |
| createdAt | DateTime | Record creation timestamp | Internal use |
| updatedAt | DateTime | Record last update timestamp | Internal use |

**Key Relationships:**
- One-to-Many with BankAttribute (a bank can have multiple attributes)

**Business Rules:**
- `permalink` must be unique across all banks
- `permalink` is used as the `BANK_ID` in API endpoints

---

### Entity 2: BankAttribute

**Database Table Name:** `bankattribute`

**Source File:** `obp-api/src/main/scala/code/bankattribute/MappedBankAttributeProvider.scala`

**Description:** Stores additional configurable attributes for banks. This entity allows flexible extension of bank information without modifying the core bank schema.

**Relevance to User Story:** This entity directly supports the following acceptance criteria:
- AC-005: The single bank retrieval endpoint shall return bank attributes in addition to core bank information

**Database Fields:**

| Field Name | Data Type | Description | Maps to API Response |
|------------|-----------|-------------|---------------------|
| id | Long | Primary key (auto-generated) | Internal use |
| BankId_ | String (UUID) | Foreign key reference to bank | `attributes[].bank_id` |
| BankAttributeId | String (UUID) | Unique identifier for the attribute | Internal use |
| Name | String(50) | Name/key of the attribute | `attributes[].name` |
| Type | String(50) | Data type of the attribute value | `attributes[].type` |
| Value | String(255) | The attribute value | `attributes[].value` |
| IsActive | Boolean | Whether the attribute is active (default: true) | `attributes[].is_active` |

**Key Relationships:**
- Many-to-One with MappedBank (multiple attributes belong to one bank)

**Business Rules:**
- `BankId_` must reference an existing bank
- `IsActive` defaults to `true` when not specified
- Attributes are only returned in single bank retrieval (GET /banks/{BANK_ID}), not in bank list (GET /banks)

---

## Entity Relationship Diagram

```
+------------------+          +------------------+
|   MappedBank     |          |  BankAttribute   |
+------------------+          +------------------+
| id (PK)          |          | id (PK)          |
| permalink        |<---------| BankId_ (FK)     |
| fullBankName     |    1:N   | BankAttributeId  |
| shortBankName    |          | Name             |
| logoURL          |          | Type             |
| websiteURL       |          | Value            |
| mBankRoutingScheme|         | IsActive         |
| mBankRoutingAddress|        +------------------+
| swiftBIC         |
| national_identifier|
| createdAt        |
| updatedAt        |
+------------------+
```

## Notes on Bank Routing

Bank routing information (`bank_routings` in the API response) is **not** stored in a separate database table. Instead, it is embedded within the `MappedBank` entity as two fields:
- `mBankRoutingScheme`: The routing scheme (e.g., "BIC", "SWIFT", "IBAN")
- `mBankRoutingAddress`: The routing address value

The API constructs the `bank_routings` array from these fields when returning bank information.

## Entities NOT Included

The following entities were considered but excluded as they are not directly relevant to the "Bank Information Retrieval" capability:

1. **BankAccountRouting** - This entity is for account-level routing, not bank-level routing. It stores routing information for individual bank accounts, not banks themselves.

2. **MappedBankAccount** - This entity represents bank accounts, which is outside the scope of bank information retrieval.

3. **MappedBankAccountData** - This entity stores additional account data, not bank data.

## Verification Summary

| Entity | Database Table | Verified in Codebase | Relevant to User Story |
|--------|---------------|---------------------|----------------------|
| MappedBank | mappedbank | Yes | Yes |
| BankAttribute | bankattribute | Yes | Yes |

## Source Code References

- **MappedBank Definition:** `obp-api/src/main/scala/code/model/dataAccess/MappedBank.scala`
- **BankAttribute Definition:** `obp-api/src/main/scala/code/bankattribute/MappedBankAttributeProvider.scala`
- **Bank Trait Interface:** `obp-commons/src/main/scala/com/openbankproject/commons/model/BankingModel.scala` (lines 36-61)
- **BankAttributeTrait Interface:** `obp-commons/src/main/scala/com/openbankproject/commons/model/CommonModelTrait.scala` (lines 553-560)
- **API Implementation:** `obp-api/src/main/scala/code/api/v4_0_0/APIMethods400.scala` (lines 239-301)
