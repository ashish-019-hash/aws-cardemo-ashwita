# Business Entity Extraction for Bank Information Retrieval

## Overview

This document contains the business entities extracted from the Bank Information Retrieval user stories that are present in the Scala codebase database. These entities are relevant to the migration from Scala to Go.

## User Story Reference

**Capability:** Bank Information Retrieval

**Description:** Retrieve information about banks supported on the platform including bank ID, name, logo, and website details.

**Endpoints:**
- `GET /obp/v5.1.0/banks` - Get all banks
- `GET /obp/v5.1.0/banks/{BANK_ID}` - Get bank by ID

---

## Extracted Business Entities

### Entity 1: MappedBank

**Scala Class:** `MappedBank` (extends `Bank` trait)

**Source File:** `obp-api/src/main/scala/code/model/dataAccess/MappedBank.scala`

**Trait Definition:** `obp-commons/src/main/scala/com/openbankproject/commons/model/BankingModel.scala`

**Description:** The MappedBank entity represents a financial institution registered on the platform. It contains identification, branding, and routing information for each bank. The class implements the `Bank` trait and uses Lift Mapper ORM for database persistence.

**Database Table:** `mappedbank` (Lift Mapper ORM - lowercase class name by default)

**Attributes:**

| Attribute Name | Data Type | Description | Required | Constraints |
|----------------|-----------|-------------|----------|-------------|
| bankId | BankId (String) | Unique identifier for the bank, used in URLs | Yes | Unique, Non-empty |
| shortName | String | Abbreviated name of the bank | Yes | Max 100 characters |
| fullName | String | Complete official name of the bank | Yes | Max 255 characters |
| logoUrl | String | URL to the bank's logo image | No | Valid URL, Max 255 characters |
| websiteUrl | String | URL to the bank's official website | No | Valid URL, Max 255 characters |
| bankRoutingScheme | String | Routing scheme identifier (e.g., BIC, SWIFT) | No | Max 255 characters |
| bankRoutingAddress | String | Routing address value | No | Max 255 characters |
| swiftBic | String | SWIFT BIC code (deprecated) | No | Max 255 characters |
| nationalIdentifier | String | National bank identifier (deprecated) | No | Max 255 characters |

**Relationships:**
- One-to-Many with BankAttribute (a bank can have multiple attributes)

**Relevance to User Story:**
- The MappedBank entity is the primary entity for the Bank Information Retrieval capability
- All fields (id, short_name, full_name, logo, website, bank_routings) in the API response are sourced from this entity
- Used in both `GET /banks` and `GET /banks/{BANK_ID}` endpoints

---

### Entity 2: BankAttribute

**Source File:** `obp-api/src/main/scala/code/bankattribute/MappedBankAttributeProvider.scala`

**Trait Definition:** `obp-commons/src/main/scala/com/openbankproject/commons/model/CommonModelTrait.scala`

**Description:** The BankAttribute entity stores additional configurable attributes for a bank. These are key-value pairs that extend the bank's metadata beyond the core fields.

**Database Table:** `bankattribute` (Lift Mapper ORM)

**Attributes:**

| Attribute Name | Data Type | Description | Required | Constraints |
|----------------|-----------|-------------|----------|-------------|
| bankId | BankId (String) | Foreign key reference to the MappedBank entity | Yes | Must exist in mappedbank table |
| bankAttributeId | String (UUID) | Unique identifier for the attribute | Yes | UUID format, Auto-generated |
| name | String | Name/key of the attribute | Yes | Max 50 characters |
| attributeType | BankAttributeType (Enum) | Type classification of the attribute | Yes | Valid enum value |
| value | String | Value of the attribute | Yes | Max 255 characters |
| isActive | Boolean | Flag indicating if the attribute is active | No | Default: true |

**Relationships:**
- Many-to-One with MappedBank (multiple attributes belong to one bank)

**Relevance to User Story:**
- BankAttribute is included in the response for single bank retrieval (`GET /banks/{BANK_ID}`)
- Excluded from list endpoint (`GET /banks`) for performance optimization
- Provides extensible metadata for banks beyond core fields

---

### Entity 3: BankId

**Source File:** `obp-commons/src/main/scala/com/openbankproject/commons/model/BankingModel.scala`

**Description:** BankId is a value object (case class) that encapsulates the bank identifier. It provides type safety and validation for bank identifiers throughout the application.

**Type:** Value Object / Case Class (not a separate database table)

**Attributes:**

| Attribute Name | Data Type | Description | Required | Constraints |
|----------------|-----------|-------------|----------|-------------|
| value | String | The actual bank identifier string | Yes | Non-empty string |

**Relevance to User Story:**
- Used as the path parameter in `GET /banks/{BANK_ID}` endpoint
- Provides type-safe bank identification across the application
- Includes permission checking for bank access control

---

## Entity Relationship Diagram

```
+------------------+          +-------------------+
|   MappedBank     |          |   BankAttribute   |
| (table:mappedbank)|         | (table:bankattribute)|
+------------------+          +-------------------+
| bankId (PK)      |<-------->| bankId (FK)       |
| shortName        |    1:N   | bankAttributeId   |
| fullName         |          | name              |
| logoUrl          |          | attributeType     |
| websiteUrl       |          | value             |
| bankRoutingScheme|          | isActive          |
| bankRoutingAddress|         +-------------------+
| swiftBic         |
| nationalIdentifier|
+------------------+
```

---

## API Response Mapping

### GET /banks Response Mapping

| API Field | Entity | Entity Attribute |
|-----------|--------|------------------|
| id | MappedBank | bankId |
| short_name | MappedBank | shortName |
| full_name | MappedBank | fullName |
| logo | MappedBank | logoUrl |
| website | MappedBank | websiteUrl |
| bank_routings[].scheme | MappedBank | bankRoutingScheme |
| bank_routings[].address | MappedBank | bankRoutingAddress |

### GET /banks/{BANK_ID} Response Mapping

| API Field | Entity | Entity Attribute |
|-----------|--------|------------------|
| id | MappedBank | bankId |
| short_name | MappedBank | shortName |
| full_name | MappedBank | fullName |
| logo | MappedBank | logoUrl |
| website | MappedBank | websiteUrl |
| bank_routings[].scheme | MappedBank | bankRoutingScheme |
| bank_routings[].address | MappedBank | bankRoutingAddress |
| attributes[].bank_id | BankAttribute | bankId |
| attributes[].name | BankAttribute | name |
| attributes[].type | BankAttribute | attributeType |
| attributes[].value | BankAttribute | value |
| attributes[].is_active | BankAttribute | isActive |

---

## Go Migration Considerations

### Struct Definitions

For the Go implementation, the following struct definitions are recommended:

**Bank struct:**
- Use string type for bankId, shortName, fullName, logoUrl, websiteUrl
- Use slice of BankRouting for routing information
- Use pointer or omitempty for optional fields

**BankAttribute struct:**
- Use string type for bankId, bankAttributeId, name, value
- Use custom type or string for attributeType
- Use pointer to bool for isActive to handle nil/null values

**BankRouting struct:**
- Use string type for scheme and address

### Database Considerations

- The Scala application uses Lift Mapper ORM with PostgreSQL/H2
- For Go, consider using GORM, sqlx, or standard database/sql package
- Maintain the same table structure for data compatibility
- Implement proper indexing on bankId fields for performance

---

## Summary

The Bank Information Retrieval capability requires two primary database entities:

1. **MappedBank** (table: `mappedbank`) - Core entity containing bank identification, branding, and routing information
2. **BankAttribute** (table: `bankattribute`) - Extension entity for additional bank metadata

These entities support the retrieval operations defined in the user stories and should be migrated to Go with equivalent struct definitions and database mappings.
