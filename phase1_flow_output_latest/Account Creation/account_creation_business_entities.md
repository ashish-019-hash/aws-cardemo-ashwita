# Business Entity Extraction for Account Creation

## Overview

This document contains the business entities extracted from the Account Creation user story. All entities listed below have been verified against the actual database tables in the Scala codebase (OBP-API) and are directly relevant to the capability described in the user story.

## Extracted Business Entities

### 1. MappedBank

**Database Location:** `code/model/dataAccess/MappedBank.scala`

**Description:** The Bank entity that represents financial institutions in the system. Referenced in the user story through BANK_ID path parameter in account creation endpoints. Each account must be created under a specific bank entity.

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
- Referenced as BANK_ID in endpoint paths: `POST /obp/v5.1.0/banks/{BANK_ID}/accounts`
- Validation requirement: "Bank ID must reference an existing bank on the platform"
- Business rule: "Account-Bank Association: Each account must be created under a specific bank entity"
- Dependency: "The target bank must exist on the platform"

---

### 2. MappedBankAccount

**Database Location:** `code/model/dataAccess/MappedBankAccount.scala`

**Description:** The Bank Account entity that represents individual accounts within a bank. This is the primary entity created during the account creation process.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| bank | UUIDString | Bank identifier (foreign key to MappedBank) |
| theAccountId | AccountIdString | Account identifier |
| accountCurrency | MappedString(10) | Currency code for the account (e.g., EUR, USD, GBP) |
| accountNumber | MappedAccountNumber | Account number |
| accountBalance | MappedLong | Account balance in smallest currency unit (e.g., cents) |
| accountName | MappedString(255) | Name of the account |
| kind | MappedString(255) | Account type/financial product name |
| accountLabel | MappedString(255) | Account label |
| accountLastUpdate | MappedDateTime | Last update timestamp |
| mBranchId | UUIDString | Branch identifier |
| accountRuleScheme1 | MappedString(10) | First account rule scheme |
| accountRuleValue1 | MappedLong | First account rule value |
| accountRuleScheme2 | MappedString(10) | Second account rule scheme |
| accountRuleValue2 | MappedLong | Second account rule value |

**Relevance to User Story:**
- Primary entity created via endpoint: `POST /obp/v5.1.0/banks/{BANK_ID}/accounts`
- Contains fields for: account type, currency, label, balance as specified in user story input data
- Maps to request fields: label, product_code (kind), balance (currency, amount), branch_id
- Output data: "Created account entity with generated account ID"

---

### 3. MapperAccountHolders

**Database Location:** `code/accountholders/MapperAccountHolders.scala`

**Description:** The Account Holders entity that links users to bank accounts, establishing ownership relationships. This entity is created during account creation to assign ownership.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| user | MappedLongForeignKey | Foreign key to ResourceUser (owner) |
| accountBankPermalink | UUIDString | Bank identifier |
| accountPermalink | AccountIdString | Account identifier |
| source | MappedString(255) | Source of the account holder relationship |

**Relevance to User Story:**
- Implements "Ownership Assignment: Account creation must include ownership specification"
- Maps to request field: user_id
- Acceptance criteria: "The system shall assign ownership to the newly created account as specified in the request"
- Validation: "The system shall validate that the specified owner exists and is eligible for account ownership"

---

### 4. ResourceUser

**Database Location:** `code/model/dataAccess/ResourceUser.scala`

**Description:** The User entity that represents users in the system. Referenced during account creation for ownership assignment and authorization validation.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| id | MappedLongIndex | Primary key identifier |
| userId_ | MappedUUID | User UUID identifier |
| email | MappedEmail(100) | User email address |
| name_ | MappedString(100) | User name |
| provider_ | MappedString(100) | Identity provider |
| providerId | MappedString(100) | Provider-specific user ID |
| Company | MappedString(50) | User's company |
| CreatedByConsentId | MappedString(100) | Consent ID that created the user |
| CreatedByUserInvitationId | MappedString(100) | Invitation ID that created the user |
| IsDeleted | MappedBoolean | Soft delete flag |
| LastMarketingAgreementSignedDate | MappedDate | Marketing agreement date |
| LastUsedLocale | MappedString(10) | User's locale preference |

**Relevance to User Story:**
- Referenced as user_id in request: "Owner information (user ID, customer ID)"
- Validation: "User ID (owner) must reference an existing, valid user"
- Dependency: "The specified owner (user/customer) must exist in the system"
- Authorization: "User must have appropriate entitlements/roles for account creation"

---

### 5. BankAccountRouting

**Database Location:** `code/model/dataAccess/BankAccountRouting.scala`

**Description:** The Account Routing entity that stores routing information (IBAN, account numbers) for bank accounts. Created during account creation when routing information is provided.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| BankId | UUIDString | Bank identifier |
| AccountId | AccountIdString | Account identifier |
| AccountRoutingScheme | MappedString(32) | Routing scheme (e.g., IBAN, AccountNumber) |
| AccountRoutingAddress | MappedString(128) | Routing address value |

**Relevance to User Story:**
- Maps to request field: account_routings array with scheme and address
- Input data: "Account routing information (IBAN, account number scheme)"
- Validation: "Account routing schemes must be valid (e.g., IBAN, AccountNumber)"
- Validation: "Account routing addresses must conform to the specified scheme format"

---

### 6. MappedAccountAttribute

**Database Location:** `code/accountattribute/MappedAccountAttributeProvider.scala`

**Description:** The Account Attribute entity that stores additional custom attributes for bank accounts. Created during account creation when account attributes are provided.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| mBankIdId | UUIDString | Bank identifier |
| mAccountId | UUIDString | Account identifier |
| mCode | MappedString(50) | Product code |
| mAccountAttributeId | MappedUUID | Unique attribute identifier |
| mName | MappedString(50) | Attribute name |
| mType | MappedString(50) | Attribute type |
| mValue | MappedString(255) | Attribute value |
| mProductInstanceCode | MappedString(255) | Product instance code |

**Relevance to User Story:**
- Maps to request field: account_attributes array
- Input data: "Additional account parameters/attributes"
- Request structure includes: product_code, account_attribute_id, name, type, value

---

### 7. MappedBranch

**Database Location:** `code/branches/MappedBranchesProvider.scala`

**Description:** The Branch entity that represents bank branches. Referenced during account creation when a branch is specified for the account.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| mBankId | UUIDString | Bank identifier |
| mBranchId | UUIDString | Branch identifier |
| mName | MappedString(255) | Branch name |
| mLine1 | MappedString(255) | Address line 1 |
| mLine2 | MappedString(255) | Address line 2 |
| mLine3 | MappedString(255) | Address line 3 |
| mCity | MappedString(255) | City |
| mCounty | MappedString(255) | County |
| mState | MappedString(255) | State |
| mCountryCode | MappedString(2) | Country code |
| mPostCode | MappedString(20) | Postal code |
| mlocationLatitude | MappedDouble | Latitude |
| mlocationLongitude | MappedDouble | Longitude |
| mBranchRoutingScheme | MappedString(32) | Branch routing scheme |
| mBranchRoutingAddress | MappedString(64) | Branch routing address |
| mIsDeleted | MappedBoolean | Soft delete flag |

**Relevance to User Story:**
- Maps to request field: branch_id
- Input data: "Branch ID must reference a valid branch if specified"
- Validation: "Branch ID must reference a valid branch if specified"

---

### 8. MappedProduct

**Database Location:** `code/products/MappedProductsProvider.scala`

**Description:** The Product entity that represents banking products. Referenced during account creation when a product code is specified.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| mBankId | UUIDString | Bank identifier |
| mCode | MappedString(50) | Product code (unique with bankId) |
| mParentProductCode | MappedString(50) | Parent product code |
| mName | MappedString(125) | Product name |
| mCategory | MappedString(50) | Product category |
| mFamily | MappedString(50) | Product family |
| mSuperFamily | MappedString(50) | Product super family |
| mMoreInfoUrl | MappedString(2000) | More info URL |
| mTermsAndConditionsUrl | MappedString(2000) | Terms and conditions URL |
| mDetails | MappedString(2000) | Product details |
| mDescription | MappedString(2000) | Product description |
| mLicenseId | UUIDString | License identifier |
| mLicenseName | MappedString(255) | License name |

**Relevance to User Story:**
- Maps to request field: product_code
- Input data: "Account type (e.g., checking, savings, current)"
- Validation: "Product code must reference a valid banking product if specified"
- Dependency: "Banking products must be configured if product_code is required"

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
    |                         |-- kind (product type)
    |                         |-- mBranchId (FK to MappedBranch)

MappedBankAccount (1) ----< (N) BankAccountRouting
                    |              |
                    |              |-- AccountRoutingScheme
                    |              |-- AccountRoutingAddress
                    |
                    |----< (N) MappedAccountAttribute
                    |              |
                    |              |-- mName
                    |              |-- mType
                    |              |-- mValue
                    |
                    |----< (N) MapperAccountHolders
                                   |
                                   |-- user (FK to ResourceUser)

ResourceUser (1) ----< (N) MapperAccountHolders
    |
    |-- userId_ (userId)
    |-- name_
    |-- email

MappedBranch (1) ----< (N) MappedBankAccount
    |
    |-- mBranchId (branchId)
    |-- mBankId (FK to MappedBank)

MappedProduct (1) ----< (N) MappedBankAccount (via kind/product_code)
    |
    |-- mCode (productCode)
    |-- mBankId (FK to MappedBank)
```

## Verification Summary

| Entity | Verified in Database | Relevant to User Story |
|--------|---------------------|------------------------|
| MappedBank | Yes | Yes - Referenced via BANK_ID parameter, account-bank association |
| MappedBankAccount | Yes | Yes - Primary entity created during account creation |
| MapperAccountHolders | Yes | Yes - Ownership assignment for created accounts |
| ResourceUser | Yes | Yes - Owner validation via user_id parameter |
| BankAccountRouting | Yes | Yes - Account routing information storage |
| MappedAccountAttribute | Yes | Yes - Additional account attributes storage |
| MappedBranch | Yes | Yes - Branch reference via branch_id parameter |
| MappedProduct | Yes | Yes - Product reference via product_code parameter |

## Notes

1. All entity names are exact matches from the Scala codebase database tables using Lift Mapper ORM
2. The MappedBankAccount entity is the primary entity created during account creation
3. MapperAccountHolders establishes the ownership relationship between users and accounts
4. BankAccountRouting stores multiple routing schemes (IBAN, AccountNumber) for each account
5. MappedAccountAttribute allows for flexible custom attributes on accounts
6. Balance amounts are stored in smallest currency units (cents, pence, etc.) and converted for display
7. The account creation process involves creating/updating multiple related entities in a transactional manner
8. Authorization is handled through entitlements linked to ResourceUser (e.g., CanCreateAccount role)
