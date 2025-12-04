# Business Entity Extraction: Multi-Bank Support

## Source User Story
**User Story**: Multi-Bank Support

## Extraction Context
This document extracts business entities from the Multi-Bank Support user story that are present in the database of the Scala (OBP-API) codebase. Only entities with corresponding database mappings have been included. Entity names and field names are specified exactly as they appear in the database Mapper classes.

The Multi-Bank Support capability is a cross-cutting architectural concern that affects how all bank-specific resources are stored and accessed. The key database entity is `MappedBank`, which serves as the parent entity for all bank-scoped resources.

---

## Extracted Business Entities

### 1. MappedBank

**Description**: The foundational database entity for multi-bank support. Represents a banking institution in the system. All bank-scoped resources (accounts, transactions, attributes, entitlements, etc.) reference this entity through their bank identifier fields. This entity enables multiple banks to be hosted on a single API instance with complete data isolation.

**Database Class**: `MappedBank` (code/model/dataAccess/MappedBank.scala)

**Domain Trait**: `Bank` (com.openbankproject.commons.model.BankingModel.scala)

**ORM**: Lift Mapper (extends LongKeyedMapper[MappedBank] with IdPK with CreatedUpdated)

**Database Fields**:

| Database Field Name | Data Type | Description | API/JSON Mapping | Required |
|---------------------|-----------|-------------|------------------|----------|
| permalink | MappedString(255) | Unique identifier for the bank, used in URL paths as BANK_ID | id, bankId, BANK_ID | Yes |
| shortBankName | MappedString(100) | Short/abbreviated name of the bank | short_name | Yes |
| fullBankName | MappedString(255) | Full legal name of the bank | full_name | Yes |
| logoURL | MappedString(255) | URL to the bank's logo image | logo | No |
| websiteURL | MappedString(255) | URL to the bank's website | website | No |
| mBankRoutingScheme | MappedString(255) | Routing scheme identifier (e.g., BIC, SWIFT) | bank_routings.scheme | No |
| mBankRoutingAddress | MappedString(255) | Routing address value corresponding to the scheme | bank_routings.address | No |
| swiftBIC | MappedString(255) | SWIFT BIC code (deprecated) | - | No |
| national_identifier | MappedString(255) | National bank identifier (deprecated) | - | No |

**Role in Multi-Bank Support**:
- Serves as the parent entity for all bank-scoped resources
- The `permalink` field is used as BANK_ID in all bank-specific endpoint URLs
- Enables data isolation by providing the bank context for all queries
- All bank-scoped resources reference MappedBank through foreign key relationships

**Relationships**:
- One-to-Many with BankAttribute (bank attributes)
- One-to-Many with MappedBankAccount (bank accounts)
- One-to-Many with Entitlement (bank-scoped entitlements)
- One-to-Many with DynamicEntity (bank-scoped dynamic entities)

**Relevant Endpoints**:
- All endpoints with pattern `/banks/BANK_ID/...` use this entity for bank context
- GET /banks - List all banks
- GET /banks/BANK_ID - Get specific bank details

**Business Rules**:
- permalink must be unique across the system (indexed)
- permalink is used as the primary lookup key for all bank-scoped operations
- Bank must exist before any bank-scoped resources can be created or accessed

---

### 2. BankAttribute

**Description**: Database entity for bank-specific custom attributes. Demonstrates bank-scoped data isolation - each BankAttribute record belongs to exactly one bank and cannot be accessed through another bank's context.

**Database Class**: `BankAttribute` (code/bankattribute/MappedBankAttributeProvider.scala)

**Domain Trait**: `BankAttributeTrait` (com.openbankproject.commons.model.CommonModelTrait.scala)

**ORM**: Lift Mapper (extends LongKeyedMapper[BankAttribute] with IdPK)

**Database Fields**:

| Database Field Name | Data Type | Description | API/JSON Mapping | Required |
|---------------------|-----------|-------------|------------------|----------|
| BankId_ | UUIDString | Foreign key reference to MappedBank.permalink - enforces bank isolation | bank_id | Yes |
| BankAttributeId | MappedUUID | Unique identifier for the attribute | bank_attribute_id | Yes |
| Name | MappedString(50) | Name/key of the attribute | name | Yes |
| Type | MappedString(50) | Type classification (STRING, INTEGER, DOUBLE, DATE_WITH_DAY) | type | Yes |
| Value | MappedString(255) | The actual value of the attribute | value | Yes |
| IsActive | MappedBoolean | Active/inactive status flag | is_active | No |

**Role in Multi-Bank Support**:
- Demonstrates bank-scoped data isolation pattern
- BankId_ field ensures attributes are scoped to a specific bank
- Queries are filtered by BankId_ to prevent cross-bank data access

**Relevant Endpoints**:
- GET /banks/BANK_ID/attributes - Retrieve attributes for specific bank only

---

### 3. MappedBankAccount

**Description**: Database entity for bank accounts. Each account belongs to exactly one bank, demonstrating the multi-bank data isolation pattern for financial resources.

**Database Class**: `MappedBankAccount` (code/model/dataAccess/MappedBankAccount.scala)

**Domain Trait**: `BankAccount` (com.openbankproject.commons.model.BankingModel.scala)

**ORM**: Lift Mapper (extends LongKeyedMapper[MappedBankAccount] with IdPK with CreatedUpdated)

**Key Database Fields for Multi-Bank Support**:

| Database Field Name | Data Type | Description | API/JSON Mapping | Required |
|---------------------|-----------|-------------|------------------|----------|
| bank | MappedString(255) | Foreign key reference to MappedBank.permalink - enforces bank isolation | bank_id, BANK_ID | Yes |
| theAccountId | MappedString(255) | Unique account identifier within the bank | account_id, ACCOUNT_ID | Yes |
| accountCurrency | MappedString(255) | Currency code for the account | currency | Yes |
| accountBalance | MappedDecimal | Current balance of the account | balance | Yes |
| accountLabel | MappedString(255) | Display label for the account | label | No |
| kind | MappedString(255) | Account type classification | type | Yes |

**Role in Multi-Bank Support**:
- Primary example of bank-scoped financial resource
- `bank` field ensures accounts are scoped to a specific bank
- Account queries are always filtered by bank identifier
- Cross-bank account access is prevented at the data layer

**Relevant Endpoints**:
- GET /banks/BANK_ID/accounts - Retrieve accounts for specific bank only

---

### 4. MappedEntitlement

**Description**: Database entity for user entitlements/permissions. Entitlements are bank-scoped, meaning a user's permission at Bank A is independent of their permissions at Bank B.

**Database Class**: `MappedEntitlement` (code/entitlement/MappedEntitlementsProvider.scala)

**Domain Trait**: `Entitlement` (code/entitlement/Entitlement.scala)

**ORM**: Lift Mapper (extends LongKeyedMapper[MappedEntitlement] with IdPK with CreatedUpdated)

**Key Database Fields for Multi-Bank Support**:

| Database Field Name | Data Type | Description | API/JSON Mapping | Required |
|---------------------|-----------|-------------|------------------|----------|
| mBankId | MappedString(255) | Foreign key reference to MappedBank.permalink - scopes entitlement to specific bank | bank_id | Yes |
| mUserId | MappedString(255) | User identifier | user_id | Yes |
| mRoleName | MappedString(255) | Name of the role/permission | role_name | Yes |

**Role in Multi-Bank Support**:
- Enables bank-specific authorization
- User can have different entitlements at different banks
- mBankId field ensures permissions are scoped to specific bank
- canCreateAccount at Bank A ≠ canCreateAccount at Bank B

**Relevant Endpoints**:
- GET /banks/BANK_ID/entitlements - Retrieve entitlements for specific bank only

---

## Entity Relationship Diagram (Textual)

```
+------------------------+
|      MappedBank        |
+------------------------+
| permalink (PK, Index)  |<-------------------------------------------------+
| shortBankName          |                                                  |
| fullBankName           |                                                  |
| logoURL                |                                                  |
| websiteURL             |                                                  |
| mBankRoutingScheme     |                                                  |
| mBankRoutingAddress    |                                                  |
+------------------------+                                                  |
         |                                                                  |
         | 1:N                                                              |
         |                                                                  |
         +------------------+------------------+------------------+         |
         |                  |                  |                  |         |
         v                  v                  v                  v         |
+------------------+ +------------------+ +------------------+ +------------------+
|  BankAttribute   | | MappedBankAccount| | MappedEntitlement| |  DynamicEntity   |
+------------------+ +------------------+ +------------------+ +------------------+
| BankAttributeId  | | theAccountId     | | mUserId          | | dynamicEntityId  |
| BankId_ (FK)     | | bank (FK)        | | mBankId (FK)     | | bankId (FK)      |
| Name             | | accountCurrency  | | mRoleName        | | entityName       |
| Type             | | accountBalance   | +------------------+ | ...              |
| Value            | | accountLabel     |                      +------------------+
| IsActive         | | kind             |
+------------------+ +------------------+
```

---

## Multi-Bank Data Isolation Pattern

### How Data Isolation is Enforced

1. **URL Path Parameter**: All bank-specific endpoints include BANK_ID in the URL path (e.g., `/banks/BANK_ID/accounts`)

2. **Foreign Key Relationship**: All bank-scoped entities have a foreign key field referencing MappedBank.permalink:
   - BankAttribute.BankId_ -> MappedBank.permalink
   - MappedBankAccount.bank -> MappedBank.permalink
   - MappedEntitlement.mBankId -> MappedBank.permalink

3. **Query Scoping**: All database queries for bank-scoped resources include the bank identifier filter:
   ```scala
   // Example: Get bank attributes for specific bank
   BankAttribute.findAll(By(BankAttribute.BankId_, bankId.value))
   
   // Example: Get accounts for specific bank
   MappedBankAccount.findAll(By(MappedBankAccount.bank, bankId.value))
   ```

4. **Authorization Layer**: User entitlements are bank-specific, preventing cross-bank access even if a user has permissions at multiple banks

---

## Summary

| Database Entity | Database Class | Primary Key | Bank Isolation Field | Role in Multi-Bank Support |
|-----------------|----------------|-------------|---------------------|---------------------------|
| MappedBank | MappedBank | id (IdPK) | permalink (is the bank) | Parent entity - defines banks |
| BankAttribute | BankAttribute | id (IdPK) | BankId_ | Bank-scoped custom attributes |
| MappedBankAccount | MappedBankAccount | id (IdPK) | bank | Bank-scoped financial accounts |
| MappedEntitlement | MappedEntitlement | id (IdPK) | mBankId | Bank-scoped user permissions |

---

## Notes

1. **Cross-Cutting Concern**: Multi-bank support is implemented as an architectural pattern affecting all bank-specific entities, not a single entity.

2. **Data Isolation Enforcement**: Data isolation is enforced at multiple layers:
   - URL routing (BANK_ID path parameter)
   - Authorization (bank-scoped entitlements)
   - Data access layer (bank-filtered queries)

3. **Single Database Instance**: All banks share the same database instance with logical separation through foreign key relationships.

4. **Bank Identifier Validation**: The system validates that BANK_ID exists in MappedBank before allowing access to any bank-scoped resources.

5. **Source Files**:
   - MappedBank: `obp-api/src/main/scala/code/model/dataAccess/MappedBank.scala`
   - BankAttribute: `obp-api/src/main/scala/code/bankattribute/MappedBankAttributeProvider.scala`
   - MappedBankAccount: `obp-api/src/main/scala/code/model/dataAccess/MappedBankAccount.scala`
   - MappedEntitlement: `obp-api/src/main/scala/code/entitlement/MappedEntitlementsProvider.scala`

6. **Field Name Conventions**: 
   - Bank isolation fields use different naming conventions across entities:
     - BankAttribute: `BankId_` (PascalCase with underscore)
     - MappedBankAccount: `bank` (lowercase)
     - MappedEntitlement: `mBankId` (camelCase with 'm' prefix)
