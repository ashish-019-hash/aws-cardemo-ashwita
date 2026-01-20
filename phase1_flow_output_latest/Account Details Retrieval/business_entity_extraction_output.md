# Business Entity Extraction for Account Details Retrieval

## Overview

This document contains the business entities extracted from the Account Details Retrieval user story. All entities listed below have been verified against the actual database tables in the Scala codebase (OBP-API) and are directly relevant to the capability described in the user story.

## Extracted Business Entities

### 1. MappedBank

**Database Location:** `code/model/dataAccess/MappedBank.scala`

**Description:** The Bank entity that represents financial institutions in the system. Referenced in the user story through BANK_ID path parameter in all account details retrieval endpoints.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| permalink | MappedString(255) | Bank identifier used in URLs (bankId) |
| fullBankName | MappedString(255) | Full name of the bank |
| shortBankName | MappedString(100) | Short name of the bank |
| logoURL | MappedString(255) | URL to the bank's logo |
| websiteURL | MappedString(255) | Bank's website URL |
| swiftBIC | MappedString(255) | SWIFT/BIC code |
| national_identifier | MappedString(255) | National identifier |
| mBankRoutingScheme | MappedString(255) | Bank routing scheme |
| mBankRoutingAddress | MappedString(255) | Bank routing address |

**Relevance to User Story:**
- Referenced as BANK_ID in all endpoint paths: `GET /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/account`
- Validation requirement: "Bank identifier (BANK_ID) must be valid and exist in the system"
- Error response: "HTTP 404 Not Found / BankNotFound must be returned when BANK_ID does not exist"

---

### 2. MappedBankAccount

**Database Location:** `code/model/dataAccess/MappedBankAccount.scala`

**Description:** The Bank Account entity that represents individual accounts within a bank. This is the primary entity for account details retrieval, containing account information including balance, currency, type, and label.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| bank | UUIDString | Bank identifier (foreign key to MappedBank) |
| theAccountId | AccountIdString | Account identifier |
| accountCurrency | MappedString(10) | Currency code for the account |
| accountNumber | MappedAccountNumber | Account number |
| accountBalance | MappedLong | Account balance in smallest currency unit (e.g., cents) |
| accountName | MappedString(255) | Name of the account |
| kind | MappedString(255) | Account type/financial product name |
| accountLabel | MappedString(255) | Account label |
| accountLastUpdate | MappedDateTime | Last update timestamp |
| mBranchId | UUIDString | Branch identifier |
| accountRuleScheme1 | MappedString(10) | Account rule scheme 1 |
| accountRuleValue1 | MappedLong | Account rule value 1 |
| accountRuleScheme2 | MappedString(10) | Account rule scheme 2 |
| accountRuleValue2 | MappedLong | Account rule value 2 |

**Relevance to User Story:**
- Referenced as ACCOUNT_ID in all endpoint paths
- Contains the accountBalance field used for balance retrieval (from: "including balance")
- Contains accountCurrency field for currency information in response
- Contains accountLabel field for account label (from: "label" in output data)
- Contains kind field for account type (from: "type" in output data)
- Validation requirement: "Account identifier (ACCOUNT_ID) must be valid and exist within the specified bank"
- Error response: "HTTP 404 Not Found / AccountNotFound must be returned when ACCOUNT_ID does not exist"

---

### 3. ViewDefinition

**Database Location:** `code/views/system/ViewDefinition.scala`

**Description:** The View entity that defines access permissions for bank accounts. Referenced in the user story through VIEW_ID path parameter for view-based account access control. Controls what account data fields are visible to users.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| id_ | MappedLongIndex | Primary key identifier |
| name_ | MappedString(125) | View name |
| description_ | MappedString(255) | View description |
| bank_id | UUIDString | Bank identifier |
| account_id | AccountIdString | Account identifier |
| view_id | UUIDString | View identifier used in URLs |
| isSystem_ | MappedBoolean | Whether this is a system view |
| isPublic_ | MappedBoolean | Whether this view is public |
| canSeeBankAccountBalance_ | MappedBoolean | Permission to see account balance |
| canSeeBankAccountCurrency_ | MappedBoolean | Permission to see account currency |
| canSeeBankAccountLabel_ | MappedBoolean | Permission to see account label |
| canSeeBankAccountType_ | MappedBoolean | Permission to see account type |
| canSeeBankAccountNumber_ | MappedBoolean | Permission to see account number |
| canSeeBankAccountOwners_ | MappedBoolean | Permission to see account owners |
| canSeeBankAccountRoutingScheme_ | MappedBoolean | Permission to see account routing scheme |
| canSeeBankAccountRoutingAddress_ | MappedBoolean | Permission to see account routing address |
| canSeeAvailableViewsForBankAccount_ | MappedBoolean | Permission to see available views |

**Relevance to User Story:**
- Referenced as VIEW_ID in endpoint: `GET /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/account`
- Controls access permissions for account details retrieval (from: "View-Based Access Control")
- Validation requirement: "View identifier (VIEW_ID) must be valid and the user must have access to this view for the account"
- Error response: "HTTP 403 Forbidden / ViewNotFound when user lacks permission to access the specified view"
- The `canSeeBankAccountBalance_` field determines if balance can be viewed through this view
- The `canSeeBankAccountOwners_` field determines if owner information is visible

---

### 4. BankAccountRouting

**Database Location:** `code/model/dataAccess/BankAccountRouting.scala`

**Description:** The entity for storing account routing information such as IBAN, account numbers, and other routing schemes. Used to provide account routing details in the account details response.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| BankId | UUIDString | The identifier of the bank |
| AccountId | AccountIdString | The identifier of the account |
| AccountRoutingScheme | MappedString(32) | Routing scheme (e.g., IBAN, AccountNumber) |
| AccountRoutingAddress | MappedString(128) | Routing address value |

**Relevance to User Story:**
- Directly supports "account routing information (e.g., IBAN, account number)" in output data
- Maps to `account_routings` field in response JSON
- Acceptance criteria: "The system shall return account routing information (e.g., IBAN, account number) based on user permissions"
- Referenced in MappedBankAccount.accountRoutings method for retrieving routing information

---

### 5. MappedAccountAttribute

**Database Location:** `code/accountattribute/MappedAccountAttributeProvider.scala`

**Description:** The entity for storing custom metadata attributes associated with bank accounts. Used to provide account-specific attributes in the account details response.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| mBankIdId | UUIDString | The identifier of the bank |
| mAccountId | UUIDString | The identifier of the account |
| mCode | MappedString(50) | Product code |
| mAccountAttributeId | MappedUUID | Unique identifier for the attribute |
| mName | MappedString(50) | Attribute name |
| mType | MappedString(50) | Attribute type (STRING, INTEGER, etc.) |
| mValue | MappedString(255) | Attribute value |
| mProductInstanceCode | MappedString(255) | Product instance code |

**Relevance to User Story:**
- Directly supports "metadata" in capability description: "Get detailed information about a specific account including balance and metadata"
- Maps to `account_attributes` field in response JSON
- Output data includes: "account_attributes (List[AccountAttribute]) - Custom metadata attributes associated with the account"
- Business rule: "Metadata Inclusion: Account metadata/attributes must be included in the response"

---

### 6. AccountAccess

**Database Location:** `code/views/system/AccountAccess.scala`

**Description:** The join table that links Users to Views on specific Bank Accounts. This entity controls which users can access which accounts through which views.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| user_fk | MappedLongForeignKey | Foreign key to ResourceUser |
| bank_id | MappedString(255) | Bank identifier |
| account_id | MappedString(255) | Account identifier |
| view_id | UUIDString | View identifier |
| consumer_id | MappedString(255) | Consumer identifier (default: ALL_CONSUMERS) |

**Relevance to User Story:**
- Controls user access to accounts through views
- Acceptance criteria: "The system shall enforce access control to ensure users can only retrieve details for accounts they have been granted permission to view"
- Validation requirement: "User must have at least one view/permission granted on the account to retrieve its details"
- Error response: "HTTP 403 Forbidden / UserNoPermissionAccessView when user cannot access the account through the specified view"

---

## Entity Relationships

```
MappedBank (1) ----< (N) MappedBankAccount
    |                         |
    |-- permalink (bankId)    |-- theAccountId (accountId)
    |                         |-- bank (FK to MappedBank)
    |                         |-- accountCurrency
    |                         |-- accountBalance
    |                         |-- accountLabel
    |                         |-- kind (accountType)

MappedBankAccount (1) ----< (N) BankAccountRouting
    |                              |
    |-- theAccountId               |-- AccountId (FK to MappedBankAccount)
    |-- bank                       |-- BankId (FK to MappedBank)
                                   |-- AccountRoutingScheme
                                   |-- AccountRoutingAddress

MappedBankAccount (1) ----< (N) MappedAccountAttribute
    |                              |
    |-- theAccountId               |-- mAccountId (FK to MappedBankAccount)
    |-- bank                       |-- mBankIdId (FK to MappedBank)
                                   |-- mName
                                   |-- mValue
                                   |-- mType

ViewDefinition (N) ----< (1) MappedBankAccount
    |
    |-- bank_id (FK to MappedBank)
    |-- account_id (FK to MappedBankAccount)
    |-- view_id (viewId)
    |-- canSeeBankAccountBalance_ (permission)
    |-- canSeeBankAccountOwners_ (permission)

AccountAccess (N) ----< (1) ViewDefinition
    |
    |-- bank_id
    |-- account_id
    |-- view_id (FK to ViewDefinition)
    |-- user_fk (FK to ResourceUser)
```

## Verification Summary

| Entity | Verified in Database | Relevant to User Story |
|--------|---------------------|------------------------|
| MappedBank | Yes | Yes - Referenced via BANK_ID parameter |
| MappedBankAccount | Yes | Yes - Primary entity for account details, contains balance, type, label |
| ViewDefinition | Yes | Yes - Referenced via VIEW_ID parameter for access control |
| BankAccountRouting | Yes | Yes - Provides account routing information (IBAN, etc.) |
| MappedAccountAttribute | Yes | Yes - Provides account metadata/attributes |
| AccountAccess | Yes | Yes - Controls user access to accounts through views |

## Notes

1. All entity names are exact matches from the Scala codebase database tables
2. The MappedBankAccount entity uses Lift Mapper ORM with LongKeyedMapper pattern
3. Balance amounts are stored in smallest currency units (cents, pence, etc.) and converted for display using the Helper.smallestCurrencyUnitToBigDecimal method
4. The ViewDefinition entity controls which users can see specific account information through permission fields like canSeeBankAccountBalance_, canSeeBankAccountOwners_, etc.
5. Account routing information is retrieved from BankAccountRouting entity via the accountRoutings method in MappedBankAccount
6. Account attributes/metadata are retrieved from MappedAccountAttribute entity and filtered based on view permissions
7. The AccountAccess entity ensures users can only access accounts they have been granted permission to view
