# Business Entity Extraction for Bank Information Retrieval

## Overview

This document contains the business entities extracted from the "Bank Information Retrieval" user story. All entities have been verified against the actual database tables in the Scala codebase (OBP-API).

**Last Updated:** 2026-01-13

**Verification Method:** Entities verified against Boot.scala ToSchemify.models list (lines 1031-1151) and corresponding source files.

## Extracted Business Entities

### Entity 1: MappedBank

**Database Table Name:** `MappedBank`

**Source File:** `obp-api/src/main/scala/code/model/dataAccess/MappedBank.scala`

**Verified in Boot.scala:** Line 1042

**Description:** The primary entity representing a bank in the system. This entity stores core bank information including identification, branding (logo, website), and routing information.

**Relevance to User Story:** This entity directly supports the following acceptance criteria:
- AC-001: Retrieve a list of all banks supported on the platform
- AC-002: Retrieve detailed information for a specific bank by bank ID
- AC-003: Each bank record shall include the bank ID, short name, full name, logo URL, and website URL
- AC-004: The bank list endpoint shall return bank routing information for each bank

**Database Fields (Verified from MappedBank.scala lines 9-17):**

| Field Name | Data Type | Description | Maps to API Response |
|------------|-----------|-------------|---------------------|
| id | Long | Primary key (auto-generated via IdPK trait) | Internal use |
| permalink | MappedString(255) | Unique bank identifier used in URLs | `id` |
| fullBankName | MappedString(255) | Full name of the bank | `full_name` |
| shortBankName | MappedString(100) | Short/abbreviated name of the bank | `short_name` |
| logoURL | MappedString(255) | URL to the bank's logo image | `logo` |
| websiteURL | MappedString(255) | URL to the bank's website | `website` |
| mBankRoutingScheme | MappedString(255) | Bank routing scheme (e.g., BIC, SWIFT) | `bank_routings[].scheme` |
| mBankRoutingAddress | MappedString(255) | Bank routing address value | `bank_routings[].address` |
| swiftBIC | MappedString(255) | SWIFT BIC code (legacy field) | Legacy |
| national_identifier | MappedString(255) | National bank identifier (legacy field) | Legacy |
| createdAt | DateTime | Record creation timestamp (via CreatedUpdated trait) | Internal use |
| updatedAt | DateTime | Record last update timestamp (via CreatedUpdated trait) | Internal use |

**Key Relationships:**
- One-to-Many with BankAttribute (a bank can have multiple attributes)

**Business Rules:**
- `permalink` must be unique across all banks (enforced via dbIndexes)
- `permalink` is used as the `BANK_ID` in API endpoints (see bankId method line 20)

---

### Entity 2: BankAttribute

**Database Table Name:** `BankAttribute`

**Source File:** `obp-api/src/main/scala/code/bankattribute/MappedBankAttributeProvider.scala`

**Verified in Boot.scala:** Line 1143

**Description:** Stores additional configurable attributes for banks. This entity allows flexible extension of bank information without modifying the core bank schema.

**Relevance to User Story:** This entity directly supports the following acceptance criteria:
- AC-005: The single bank retrieval endpoint shall return bank attributes in addition to core bank information

**Database Fields (Verified from MappedBankAttributeProvider.scala lines 72-79):**

| Field Name | Data Type | Description | Maps to API Response |
|------------|-----------|-------------|---------------------|
| id | Long | Primary key (auto-generated via IdPK trait) | Internal use |
| BankId_ | UUIDString | Foreign key reference to bank permalink | `attributes[].bank_id` |
| BankAttributeId | MappedUUID | Unique identifier for the attribute | Internal use |
| Name | MappedString(50) | Name/key of the attribute | `attributes[].name` |
| Type | MappedString(50) | Data type of the attribute value (STRING, INTEGER, DOUBLE, DATE_WITH_DAY) | `attributes[].type` |
| Value | MappedString(255) | The attribute value | `attributes[].value` |
| IsActive | MappedBoolean | Whether the attribute is active (default: true) | `attributes[].is_active` |

**Key Relationships:**
- Many-to-One with MappedBank (multiple attributes belong to one bank via BankId_ field)

**Business Rules:**
- `BankId_` must reference an existing bank's permalink
- `IsActive` defaults to `true` when not specified (see line 78)
- Attributes are only returned in single bank retrieval (GET /banks/{BANK_ID}), not in bank list (GET /banks)
- Index exists on BankId_ for efficient lookups (see line 92)

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

1. **BankAccountRouting** (Boot.scala line 1044) - This entity is for account-level routing, not bank-level routing. It stores routing information for individual bank accounts, not banks themselves.

2. **MappedBankAccount** (Boot.scala line 1043) - This entity represents bank accounts, which is outside the scope of bank information retrieval.

3. **MappedBankAccountData** (Boot.scala line 1061) - This entity stores additional account data, not bank data.

## Verification Summary

| Entity | Database Table | Boot.scala Line | Source File Verified | Relevant to User Story |
|--------|---------------|-----------------|---------------------|----------------------|
| MappedBank | MappedBank | 1042 | Yes (lines 6-29) | Yes |
| BankAttribute | BankAttribute | 1143 | Yes (lines 68-93) | Yes |

## Source Code References

- **MappedBank Definition:** `obp-api/src/main/scala/code/model/dataAccess/MappedBank.scala` (lines 6-39)
- **BankAttribute Definition:** `obp-api/src/main/scala/code/bankattribute/MappedBankAttributeProvider.scala` (lines 68-93)
- **Boot.scala ToSchemify Models:** `obp-api/src/main/scala/bootstrap/liftweb/Boot.scala` (lines 1031-1151)
- **Bank Trait Interface:** `obp-commons/src/main/scala/com/openbankproject/commons/model/BankingModel.scala`
- **BankAttributeTrait Interface:** `obp-commons/src/main/scala/com/openbankproject/commons/model/CommonModelTrait.scala`
- **API Implementation:** `obp-api/src/main/scala/code/api/v4_0_0/APIMethods400.scala`
