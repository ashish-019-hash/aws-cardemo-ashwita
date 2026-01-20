# Business Entity Extraction for Bank Creation

## Overview

This document contains the business entities extracted from the Bank Creation user story. All entities listed below have been verified against the actual database tables in the Scala codebase (OBP-API) and are directly relevant to the capability described in the user story.

## Extracted Business Entities

### 1. MappedBank

**Database Location:** `code/model/dataAccess/MappedBank.scala`

**Description:** The Bank entity that represents financial institutions in the Open Bank Project system. This is the primary entity created during the bank creation process. Banks are the top-level organizational units that contain accounts, customers, transactions, and other banking resources.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| id | Long (IdPK) | Internal database ID (auto-generated) |
| permalink | MappedString(255) | Bank identifier used in URLs (bankId) - unique |
| fullBankName | MappedString(255) | Full name of the bank |
| shortBankName | MappedString(100) | Short name/code of the bank |
| logoURL | MappedString(255) | URL to the bank's logo |
| websiteURL | MappedString(255) | Bank's website URL |
| swiftBIC | MappedString(255) | SWIFT/BIC code |
| national_identifier | MappedString(255) | National bank identifier |
| mBankRoutingScheme | MappedString(255) | Bank routing scheme (e.g., "BIC", "IBAN") |
| mBankRoutingAddress | MappedString(255) | Bank routing address value |
| createdAt | DateTime | Creation timestamp (from CreatedUpdated trait) |
| updatedAt | DateTime | Last update timestamp (from CreatedUpdated trait) |

**Relevance to User Story:**
- Primary entity created via endpoint: `POST /obp/v5.1.0/banks`
- Maps to request fields:
  - `id` -> permalink (Bank ID)
  - `short_name` -> shortBankName
  - `full_name` -> fullBankName
  - `logo` -> logoURL
  - `website` -> websiteURL
  - `bank_routings[].scheme` -> mBankRoutingScheme
  - `bank_routings[].address` -> mBankRoutingAddress
- Acceptance criteria: "The system shall allow authorized users to create a new bank entity with required metadata"
- Business rule: "Bank Entity Uniqueness: Each bank must have a unique identifier on the platform"
- Output data: "Created bank entity with generated ID"

---

### 2. BankAttribute

**Database Location:** `code/bankattribute/MappedBankAttributeProvider.scala`

**Description:** The Bank Attribute entity that stores additional custom attributes for bank entities. This entity allows for flexible configuration of bank-specific parameters beyond the core bank fields.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| id | Long (IdPK) | Internal database ID (auto-generated) |
| BankId_ | UUIDString | Bank identifier (foreign key to MappedBank) |
| BankAttributeId | MappedUUID | Unique attribute identifier |
| Name | MappedString(50) | Attribute name |
| Type | MappedString(50) | Attribute type (from BankAttributeType enum) |
| Value | MappedString(255) | Attribute value |
| IsActive | MappedBoolean | Whether the attribute is active (default: true) |

**Relevance to User Story:**
- Maps to request field: `attributes` array with name-value pairs
- Input data: "Associated metadata attributes"
- Request structure includes: name, value pairs for bank configuration
- Acceptance criteria: "The system shall associate metadata (name, logo, website, identifiers) with the bank entity during creation"
- Business rule: "Configuration Association: Bank entities must be created with associated configuration settings"

---

## Entity Relationships

```
MappedBank (1) ----< (N) BankAttribute
    |                         |
    |-- permalink (bankId)    |-- BankId_ (FK to MappedBank)
    |-- fullBankName          |-- BankAttributeId
    |-- shortBankName         |-- Name
    |-- logoURL               |-- Type
    |-- websiteURL            |-- Value
    |-- swiftBIC              |-- IsActive
    |-- national_identifier
    |-- mBankRoutingScheme
    |-- mBankRoutingAddress
    |-- createdAt
    |-- updatedAt
```

## Verification Summary

| Entity | Verified in Database | Relevant to User Story |
|--------|---------------------|------------------------|
| MappedBank | Yes - `code/model/dataAccess/MappedBank.scala` | Yes - Primary entity created during bank creation |
| BankAttribute | Yes - `code/bankattribute/MappedBankAttributeProvider.scala` | Yes - Stores bank metadata attributes |

## API Endpoint Mapping

Based on the user story, only the following endpoint is included:

### Endpoint: Create Bank
- **Endpoint**: `POST /obp/v5.1.0/banks`
- **Purpose**: Create a new bank entity on the platform with all associated metadata and configuration
- **Primary Entity Created**: MappedBank
- **Related Entities Created**: BankAttribute (for each attribute in the request)

**Note**: GET, PUT, DELETE endpoints are NOT included as they are not mentioned in the user story description which focuses on "Create new bank entities".

## Notes

1. All entity names are exact matches from the Scala codebase database tables using Lift Mapper ORM
2. The MappedBank entity is the primary entity created during bank creation
3. Bank routing information (scheme and address) is stored directly in the MappedBank entity, not as a separate entity
4. BankAttribute allows for flexible custom attributes on banks beyond the core fields
5. The bank creation process involves creating the MappedBank entity and optionally creating BankAttribute records for additional metadata
6. Authorization is handled through entitlements (e.g., CanCreateBank role) - this is managed separately from the business entities
7. The permalink field in MappedBank serves as the unique bank identifier used in URLs and API paths
8. Timestamps (createdAt, updatedAt) are automatically managed by the CreatedUpdated trait

## Data Validations (from User Story)

- Bank ID must be unique across the platform
- Bank name (full_name) is required and must not be empty
- Bank short_name must follow naming conventions (alphanumeric, limited length)
- Logo URL must be a valid URL format if provided
- Website URL must be a valid URL format if provided
- Bank routing schemes must be valid (e.g., BIC, IBAN, etc.)
- Metadata attributes must have valid name-value pairs
