# Business Entity Extraction for Bank Information Retrieval

## Overview

This document contains the business entities extracted from the Bank Information Retrieval user story. All entities listed below have been verified against the actual database tables in the Scala codebase (OBP-API) and are directly relevant to the capability described in the user story.

## Extracted Business Entities

### 1. MappedBank

**Database Location:** `code/model/dataAccess/MappedBank.scala`

**Description:** The Bank entity that represents financial institutions in the Open Bank Project system. This is the primary entity retrieved during bank information retrieval operations. Banks are the top-level organizational units that contain accounts, customers, transactions, and other banking resources.

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
- Primary entity retrieved via endpoints: `GET /banks` and `GET /banks/BANK_ID`
- Maps to response fields:
  - `id` <- permalink (Bank ID)
  - `short_name` <- shortBankName
  - `full_name` <- fullBankName
  - `logo` <- logoURL
  - `website` <- websiteURL
  - `bank_routings[].scheme` <- mBankRoutingScheme
  - `bank_routings[].address` <- mBankRoutingAddress
- Acceptance criteria: "System must allow retrieving details for all banks" and "System must allow retrieving bank details by bank identifier"
- Output data: "List of banks with details (name, logo, website, routing)" and "Single bank details with attributes"

---

### 2. BankAttribute

**Database Location:** `code/bankattribute/MappedBankAttributeProvider.scala`

**Description:** The Bank Attribute entity that stores additional custom attributes for bank entities. This entity allows for flexible configuration of bank-specific parameters beyond the core bank fields. Bank attributes are retrieved only for single bank retrieval (GET /banks/BANK_ID), not for the bank list endpoint.

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
- Retrieved via endpoint: `GET /banks/BANK_ID` (single bank retrieval only)
- Maps to response field: `attributes` array
- Response structure includes:
  - `bank_id` <- BankId_
  - `name` <- Name
  - `type` <- Type
  - `value` <- Value
  - `is_active` <- IsActive
- Acceptance criteria: "Retrieved single bank details must include bank attributes (operational parameters)"
- Business rule: "Complete Information for Single Bank: Retrieved single bank details must include all specified fields (name, logo, website, routing, attributes)"
- Note: Bank attributes are NOT included in the GET /banks (list all banks) response for performance reasons

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
| MappedBank | Yes - `code/model/dataAccess/MappedBank.scala` | Yes - Primary entity retrieved during bank information retrieval |
| BankAttribute | Yes - `code/bankattribute/MappedBankAttributeProvider.scala` | Yes - Retrieved for single bank details (operational parameters) |

## API Endpoint Mapping

Based on the user story, only the following retrieval endpoints are included:

### Endpoint 1: Retrieve All Banks
- **Endpoint**: `GET /banks`
- **Purpose**: Retrieve details for all banks including name, logo, website and routing information
- **Primary Entity Retrieved**: MappedBank (list)
- **Related Entities Retrieved**: None (attributes not included for performance)
- **Response Fields from MappedBank**:
  - id (from permalink)
  - short_name (from shortBankName)
  - full_name (from fullBankName)
  - logo (from logoURL)
  - website (from websiteURL)
  - bank_routings (from mBankRoutingScheme and mBankRoutingAddress)

### Endpoint 2: Retrieve Single Bank Details
- **Endpoint**: `GET /banks/BANK_ID`
- **Purpose**: Retrieve complete bank details including name, logo, website, routing and attributes for a specific bank
- **Primary Entity Retrieved**: MappedBank (single)
- **Related Entities Retrieved**: BankAttribute (list for the specific bank)
- **Response Fields from MappedBank**:
  - id (from permalink)
  - short_name (from shortBankName)
  - full_name (from fullBankName)
  - logo (from logoURL)
  - website (from websiteURL)
  - bank_routings (from mBankRoutingScheme and mBankRoutingAddress)
- **Response Fields from BankAttribute**:
  - attributes[].bank_id (from BankId_)
  - attributes[].name (from Name)
  - attributes[].type (from Type)
  - attributes[].value (from Value)
  - attributes[].is_active (from IsActive)

**Note**: POST, PUT, DELETE endpoints are NOT included as they are not mentioned in the user story description which focuses on "Bank Information Retrieval" (read-only operations).

## Notes

1. All entity names are exact matches from the Scala codebase database tables using Lift Mapper ORM
2. The MappedBank entity is the primary entity retrieved during bank information retrieval
3. Bank routing information (scheme and address) is stored directly in the MappedBank entity, not as a separate entity
4. BankAttribute allows for flexible custom attributes on banks beyond the core fields
5. The bank retrieval process involves querying the MappedBank entity and optionally querying BankAttribute records for single bank retrieval
6. Authorization is handled through entitlements - this is managed separately from the business entities
7. The permalink field in MappedBank serves as the unique bank identifier used in URLs and API paths
8. GET /banks returns a list of banks WITHOUT attributes (for performance reasons)
9. GET /banks/BANK_ID returns a single bank WITH attributes (complete information)
10. If a bank exists but has no attributes, the attributes array should be empty (not null)

## Data Validations (from User Story)

- For GET /banks/BANK_ID: Bank identifier must be provided and non-empty
- For GET /banks/BANK_ID: Bank identifier must exist in the system (return 404 if not found)
- For GET /banks: No validation required (returns empty list if no banks exist)

## Special Considerations

1. **Performance Optimization**: The GET /banks endpoint does not include attributes to optimize response time when listing multiple banks
2. **Error Handling**: Return appropriate HTTP status codes (200 for success, 404 for not found on single bank retrieval)
3. **Empty Results**: GET /banks should return empty list if no banks exist (not 404)
4. **Deprecated Fields**: The response may include deprecated fields (swiftBic, nationalIdentifier) for backward compatibility
