# Business Entity Extraction for Transaction Attribute Management

## Overview

This document identifies the business entities extracted from the Transaction Attribute Management user story that are verified against the actual database tables in the Scala codebase.

## Extracted Business Entities

### 1. MappedTransactionAttribute

**Database Table/Class:** `MappedTransactionAttribute` (Lift Mapper ORM)

**Source File:** `code/transactionattribute/MappedTransactionAttributeProvider.scala`

**Description:** The primary entity for storing custom transaction attributes. This entity enables extended metadata storage for transactions, supporting flexible categorization, compliance tracking, and custom data storage for transactions.

**Database Fields:**
| Field Name | Type | Max Length | Description |
|------------|------|------------|-------------|
| mBankId | UUIDString | - | Foreign key reference to the Bank entity |
| mTransactionId | UUIDString | - | Foreign key reference to the Transaction entity |
| mTransactionAttributeId | MappedUUID | - | Unique identifier for the transaction attribute |
| mName | MappedString | 50 | Name of the attribute (e.g., "TAX_NUMBER", "CATEGORY") |
| mType | MappedString | 50 | Attribute type: STRING, INTEGER, DOUBLE, DATE_WITH_DAY |
| mValue | MappedString | 255 | The actual value of the attribute |

**Relevance to User Story:**
- Core entity for all CRUD operations on transaction attributes
- Referenced in endpoint: PUT /banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID/attributes/ATTRIBUTE_ID
- Supports createOrUpdate semantics for attribute management

---

### 2. MappedTransaction

**Database Table/Class:** `MappedTransaction` (Lift Mapper ORM)

**Source File:** `code/transaction/MappedTransaction.scala`

**Description:** The Transaction entity that serves as the parent entity for transaction attributes. A transaction must exist before attributes can be created for it.

**Database Fields:**
| Field Name | Type | Max Length | Description |
|------------|------|------------|-------------|
| bank | MappedString | 255 | Bank identifier |
| account | AccountIdString | - | Account identifier |
| transactionId | MappedString | 255 | Unique transaction identifier (UUID) |
| transactionUUID | MappedUUID | - | Transaction UUID (legacy) |
| transactionType | MappedString | 100 | Type of transaction |
| amount | MappedLong | - | Transaction amount in smallest currency unit |
| newAccountBalance | MappedLong | - | New account balance after transaction |
| currency | MappedString | 10 | Currency code |
| tStartDate | MappedDateTime | - | Transaction start date |
| tFinishDate | MappedDateTime | - | Transaction finish date |
| description | MappedString | 2000 | Transaction description |
| status | MappedString | 20 | Transaction status |

**Relevance to User Story:**
- Parent entity referenced by MappedTransactionAttribute via mTransactionId
- Business Rule: "The transaction must exist before attributes can be managed on it"
- Validation: "Transaction ID must be a valid transaction within the specified account"

---

### 3. MappedBankAccount

**Database Table/Class:** `MappedBankAccount` (Lift Mapper ORM)

**Source File:** `code/model/dataAccess/MappedBankAccount.scala`

**Description:** The Bank Account entity that contains transactions. An account must exist and be accessible before transaction attributes can be managed.

**Database Fields:**
| Field Name | Type | Max Length | Description |
|------------|------|------------|-------------|
| bank | UUIDString | - | Foreign key reference to the Bank entity |
| theAccountId | AccountIdString | - | Unique account identifier |
| accountCurrency | MappedString | 10 | Account currency code |
| accountNumber | MappedAccountNumber | - | Account number |
| accountBalance | MappedLong | - | Current account balance |
| accountName | MappedString | 255 | Account name |
| kind | MappedString | 255 | Account type/financial product name |
| accountLabel | MappedString | 255 | Account label |
| mBranchId | UUIDString | - | Branch identifier |

**Relevance to User Story:**
- Parent entity for transactions
- Validation: "Account ID must be a valid account belonging to the specified bank"
- Required for path parameter: ACCOUNT_ID

---

### 4. MappedBank

**Database Table/Class:** `MappedBank` (Lift Mapper ORM)

**Source File:** `code/model/dataAccess/MappedBank.scala`

**Description:** The Bank entity that serves as the top-level parent entity. A bank must exist before transaction attributes can be managed within its scope.

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
- Top-level parent entity for the attribute hierarchy
- Business Rule: "Attributes are scoped to a specific bank and must be managed within that bank's context"
- Validation: "Bank ID must be a valid, existing bank identifier"

---

### 5. AttributeDefinition

**Database Table/Class:** `AttributeDefinition` (Lift Mapper ORM)

**Source File:** `code/api/attributedefinition/MappedAttributeDefinition.scala`

**Description:** Defines the schema/definition for transaction attributes at the bank level, including type constraints, descriptions, and visibility settings. This entity supports standardized attribute schemas.

**Database Fields:**
| Field Name | Type | Max Length | Description |
|------------|------|------------|-------------|
| AttributeDefinitionId | MappedUUID | - | Unique identifier for the attribute definition |
| BankId | MappedString | 50 | Bank identifier |
| Name | MappedString | 50 | Name of the attribute definition |
| Category | MappedString | 50 | Category (e.g., "Transaction") |
| TypeOfValue | MappedString | 50 | Type of value: STRING, INTEGER, DOUBLE, DATE_WITH_DAY |
| Description | MappedString | 256 | Description of the attribute |
| Alias | MappedString | 50 | Alias name for the attribute |
| CanBeSeenOnViews | MappedString | 256 | Semicolon-separated list of views that can see this attribute |
| IsActive | MappedBoolean | - | Active status flag |

**Relevance to User Story:**
- Referenced in Endpoint: PUT /banks/BANK_ID/attribute-definitions/transaction
- Supports "Create or Update Transaction Attribute Definition" functionality
- Required entitlement: canCreateTransactionAttributeDefinitionAtOneBank
- Used for filtering attributes visible on specific views

---

### 6. MappedEntitlement

**Database Table/Class:** `MappedEntitlement` (Lift Mapper ORM)

**Source File:** `code/entitlement/MappedEntitlements.scala`

**Description:** Stores user entitlements/roles that control access to transaction attribute operations. This entity implements role-based access control (RBAC) for the Transaction Attribute Management capability.

**Database Fields:**
| Field Name | Type | Max Length | Description |
|------------|------|------------|-------------|
| mEntitlementId | MappedUUID | - | Unique identifier for the entitlement |
| mBankId | UUIDString | - | Bank identifier (scope of entitlement) |
| mUserId | UUIDString | - | User identifier |
| mRoleName | MappedString | 64 | Role name (e.g., canUpdateTransactionAttributeAtOneBank) |
| mCreatedByProcess | MappedString | 255 | Process that created the entitlement |

**Relevance to User Story:**
- Controls access to all transaction attribute operations
- Required entitlements referenced in user story:
  - `canUpdateTransactionAttributeAtOneBank` - Update transaction attributes
  - `canCreateTransactionAttributeDefinitionAtOneBank` - Create/update attribute definitions
- Business Rule: "Users must have the appropriate role/entitlement to manage transaction attributes"

---

### 7. ResourceUser

**Database Table/Class:** `ResourceUser` (Lift Mapper ORM)

**Source File:** `code/model/dataAccess/ResourceUser.scala`

**Description:** The User entity that represents authenticated users in the system. Users must be authenticated and have appropriate entitlements to perform transaction attribute operations.

**Database Fields:**
| Field Name | Type | Max Length | Description |
|------------|------|------------|-------------|
| id | MappedLongIndex | - | Primary key |
| userId_ | MappedUUID | - | Unique user identifier |
| email | MappedEmail | 100 | User email address |
| name_ | MappedString | 100 | User name |
| provider_ | MappedString | 100 | Identity provider |
| providerId | MappedString | 100 | Provider-specific user ID |
| Company | MappedString | 50 | User's company |
| IsDeleted | MappedBoolean | - | Soft delete flag |

**Relevance to User Story:**
- All endpoints require valid user authentication
- Users are linked to entitlements via MappedEntitlement.mUserId
- Dependency: "User must be authenticated and have valid session"

---

## Entity Relationships

```
ResourceUser (1) ----< (N) MappedEntitlement
     |
     | (authenticated user)
     v
MappedBank (1) ----< (N) MappedBankAccount (1) ----< (N) MappedTransaction (1) ----< (N) MappedTransactionAttribute
     |
     +----< (N) AttributeDefinition (Category = "Transaction")
```

**Relationship Descriptions:**
1. **ResourceUser to MappedEntitlement**: One user can have multiple entitlements (one-to-many)
2. **MappedBank to MappedBankAccount**: One bank can have multiple accounts (one-to-many)
3. **MappedBankAccount to MappedTransaction**: One account can have multiple transactions (one-to-many)
4. **MappedTransaction to MappedTransactionAttribute**: One transaction can have multiple attributes (one-to-many)
5. **MappedBank to AttributeDefinition**: One bank can have multiple attribute definitions (one-to-many)

## Business Rules Mapping to Entities

| Business Rule | Primary Entity | Related Entities |
|---------------|----------------|------------------|
| Attribute Type Validation | MappedTransactionAttribute | AttributeDefinition |
| Resource Existence | MappedTransaction | MappedBankAccount, MappedBank |
| Bank Scope | MappedTransactionAttribute | MappedBank |
| Attribute Identity | MappedTransactionAttribute | - |
| Entitlement Enforcement | MappedEntitlement | ResourceUser |

## Data Validations Mapping to Entities

| Validation | Primary Entity | Description |
|------------|----------------|-------------|
| Bank ID Validation | MappedBank | Must be a valid, existing bank identifier |
| Account ID Validation | MappedBankAccount | Must be a valid account belonging to the specified bank |
| Transaction ID Validation | MappedTransaction | Must be a valid transaction within the specified account |
| Attribute ID Validation | MappedTransactionAttribute | For updates, must reference an existing attribute |
| Type Field Validation | MappedTransactionAttribute | Must match one of the enumerated TransactionAttributeType values |
| Value Format Validation | MappedTransactionAttribute | Value must be compatible with the declared attribute type |

## Verification Summary

| Entity | Present in Scala DB | Relevant to User Story | Verified |
|--------|---------------------|------------------------|----------|
| MappedTransactionAttribute | Yes | Yes - Primary entity for CRUD operations | Yes |
| MappedTransaction | Yes | Yes - Parent entity, existence validation | Yes |
| MappedBankAccount | Yes | Yes - Parent entity for transactions | Yes |
| MappedBank | Yes | Yes - Top-level parent, bank scope | Yes |
| AttributeDefinition | Yes | Yes - Attribute definition endpoint | Yes |
| MappedEntitlement | Yes | Yes - RBAC for all operations | Yes |
| ResourceUser | Yes | Yes - Authentication requirement | Yes |

## Notes

- All entities listed above are verified against the actual Scala codebase database mappings
- Entity names match exactly as defined in the Lift Mapper ORM classes
- No irrelevant entities have been included - only those directly referenced in the user story and present in the database
- Field lengths and types are extracted from the actual Scala source code
- The TransactionAttributeType enumeration supports: STRING, INTEGER, DOUBLE, DATE_WITH_DAY
