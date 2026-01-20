# Business Entity Extraction for Bank Attribute Management

## Overview

This document identifies the business entities extracted from the Bank Attribute Management user story that are verified against the actual database tables in the Scala codebase.

## Extracted Business Entities

### 1. BankAttribute

**Database Table/Class:** `BankAttribute` (Lift Mapper ORM)

**Source File:** `code/bankattribute/MappedBankAttributeProvider.scala`

**Description:** The primary entity for storing custom bank attributes. This entity enables extended metadata storage for banks, supporting regulatory reporting, custom identifiers, and business-specific requirements.

**Database Fields:**
| Field Name | Type | Max Length | Description |
|------------|------|------------|-------------|
| BankId_ | UUIDString | - | Foreign key reference to the Bank entity |
| BankAttributeId | MappedUUID | - | Unique identifier for the bank attribute |
| Name | MappedString | 50 | Name of the attribute (e.g., "ISIN", "TAX_NUMBER") |
| Type | MappedString | 50 | Attribute type: STRING, INTEGER, DOUBLE, DATE_WITH_DAY |
| Value | MappedString | 255 | The actual value of the attribute |
| IsActive | MappedBoolean | - | Active status flag (defaults to true) |

**Relevance to User Story:**
- Core entity for all CRUD operations on bank attributes
- Referenced in endpoints: POST /banks/BANK_ID/attribute, GET /banks/BANK_ID/attributes, GET /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID, PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID, DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID

---

### 2. MappedBank

**Database Table/Class:** `MappedBank` (Lift Mapper ORM)

**Source File:** `code/model/dataAccess/MappedBank.scala`

**Description:** The Bank entity that serves as the parent entity for bank attributes. A bank must exist before attributes can be created for it.

**Database Fields:**
| Field Name | Type | Max Length | Description |
|------------|------|------------|-------------|
| permalink | MappedString | 255 | Unique bank identifier used in URLs (bankId) |
| fullBankName | MappedString | 255 | Full name of the bank |
| shortBankName | MappedString | 100 | Short/abbreviated bank name |
| logoURL | MappedString | 255 | URL to bank's logo |
| websiteURL | MappedString | 255 | Bank's website URL |
| swiftBIC | MappedString | 255 | SWIFT/BIC code |
| national_identifier | MappedString | 255 | National identifier for the bank |
| mBankRoutingScheme | MappedString | 255 | Bank routing scheme |
| mBankRoutingAddress | MappedString | 255 | Bank routing address |

**Relevance to User Story:**
- Parent entity referenced by BankAttribute via BankId
- Business Rule: "Bank must exist in the system before attributes can be created for it"
- Validation: "Bank ID must correspond to an existing bank in the system"

---

### 3. AttributeDefinition

**Database Table/Class:** `AttributeDefinition` (Lift Mapper ORM)

**Source File:** `code/api/attributedefinition/MappedAttributeDefinition.scala`

**Description:** Defines the schema/definition for bank attributes, including type constraints, descriptions, and visibility settings. This entity supports structured metadata management.

**Database Fields:**
| Field Name | Type | Max Length | Description |
|------------|------|------------|-------------|
| AttributeDefinitionId | MappedUUID | - | Unique identifier for the attribute definition |
| BankId | MappedString | 50 | Bank identifier |
| Name | MappedString | 50 | Name of the attribute definition |
| Category | MappedString | 50 | Category (e.g., "Bank") |
| TypeOfValue | MappedString | 50 | Type of value: STRING, INTEGER, DOUBLE, DATE_WITH_DAY |
| Description | MappedString | 256 | Description of the attribute |
| Alias | MappedString | 50 | Alias name for the attribute |
| CanBeSeenOnViews | MappedString | 256 | Semicolon-separated list of views that can see this attribute |
| IsActive | MappedBoolean | - | Active status flag |

**Relevance to User Story:**
- Referenced in Endpoint 6: PUT /banks/BANK_ID/attribute-definitions/bank
- Supports "Create or Update Bank Attribute Definition" functionality
- Required entitlement: canCreateBankAttributeDefinitionAtOneBank

---

### 4. MappedEntitlement

**Database Table/Class:** `MappedEntitlement` (Lift Mapper ORM)

**Source File:** `code/entitlement/MappedEntitlements.scala`

**Description:** Stores user entitlements/roles that control access to bank attribute operations. This entity implements role-based access control (RBAC) for the Bank Attribute Management capability.

**Database Fields:**
| Field Name | Type | Max Length | Description |
|------------|------|------------|-------------|
| mEntitlementId | MappedUUID | - | Unique identifier for the entitlement |
| mBankId | UUIDString | - | Bank identifier (scope of entitlement) |
| mUserId | UUIDString | - | User identifier |
| mRoleName | MappedString | 64 | Role name (e.g., canCreateBankAttribute) |
| mCreatedByProcess | MappedString | 255 | Process that created the entitlement |

**Relevance to User Story:**
- Controls access to all bank attribute operations
- Required entitlements referenced in user story:
  - `canCreateBankAttribute` - Create bank attributes
  - `canGetBankAttribute` - Retrieve bank attributes
  - `canUpdateBankAttribute` - Update bank attributes
  - `canDeleteBankAttribute` - Delete bank attributes
  - `canCreateBankAttributeDefinitionAtOneBank` - Create/update attribute definitions

---

### 5. ResourceUser

**Database Table/Class:** `ResourceUser` (Lift Mapper ORM)

**Source File:** `code/model/dataAccess/ResourceUser.scala`

**Description:** The User entity that represents authenticated users in the system. Users must be authenticated and have appropriate entitlements to perform bank attribute operations.

**Relevance to User Story:**
- All endpoints require valid user authentication
- Users are linked to entitlements via MappedEntitlement.mUserId
- Error condition: "UserNotLoggedIn" when user is not authenticated

---

## Entity Relationships

```
ResourceUser (1) ----< (N) MappedEntitlement
     |
     | (authenticated user)
     v
MappedBank (1) ----< (N) BankAttribute
     |
     +----< (N) AttributeDefinition
```

**Relationship Descriptions:**
1. **ResourceUser to MappedEntitlement**: One user can have multiple entitlements (one-to-many)
2. **MappedBank to BankAttribute**: One bank can have multiple attributes (one-to-many)
3. **MappedBank to AttributeDefinition**: One bank can have multiple attribute definitions (one-to-many)

## Verification Summary

| Entity | Present in Scala DB | Relevant to User Story | Verified |
|--------|---------------------|------------------------|----------|
| BankAttribute | Yes | Yes - Primary entity for CRUD operations | Yes |
| MappedBank | Yes | Yes - Parent entity, existence validation | Yes |
| AttributeDefinition | Yes | Yes - Attribute definition endpoint | Yes |
| MappedEntitlement | Yes | Yes - RBAC for all operations | Yes |
| ResourceUser | Yes | Yes - Authentication requirement | Yes |

## Notes

- All entities listed above are verified against the actual Scala codebase database mappings
- Entity names match exactly as defined in the Lift Mapper ORM classes
- No irrelevant entities have been included - only those directly referenced in the user story and present in the database
