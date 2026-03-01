# Business Entity Extraction for Account Routing Lookup

## Overview

This document contains the business entities extracted from the Account Routing Lookup user story. All entities listed below have been verified against the actual database tables in the Scala codebase (OBP-API) and are directly relevant to the capability described in the user story.

## Extracted Business Entities

### 1. BankAccountRouting

**Database Location:** `code/model/dataAccess/BankAccountRouting.scala`

**Description:** The primary entity for storing account routing information such as IBAN, account numbers, and other routing schemes. This is the core entity for the Account Routing Lookup capability, enabling accounts to be found by their routing identifiers.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| BankId | UUIDString | The identifier of the bank |
| AccountId | AccountIdString | The identifier of the account |
| AccountRoutingScheme | MappedString(32) | Routing scheme (e.g., IBAN, ACCOUNT_NUMBER, BIC) |
| AccountRoutingAddress | MappedString(128) | Routing address value (e.g., the actual IBAN number) |

**Database Indexes:**
- UniqueIndex(BankId, AccountId, AccountRoutingScheme)
- UniqueIndex(BankId, AccountRoutingScheme, AccountRoutingAddress)

**Relevance to User Story:**
- Primary entity for "Find accounts by routing information such as IBAN or account number"
- Directly supports all three endpoints in the user story:
  - `GET /obp/v4.0.0/banks/{BANK_ID}/accounts/account-routing-query` - Uses scheme and address parameters
  - `GET /obp/v4.0.0/accounts/iban/{IBAN}` - Looks up by IBAN routing scheme
  - `GET /obp/v4.0.0/banks/{BANK_ID}/accounts/account-number/{ACCOUNT_NUMBER}` - Looks up by account number
- Maps to `account_routings` field in response JSON containing scheme and address pairs
- Acceptance criteria: "The system shall allow users to find bank accounts using IBAN" and "using account number"
- Business rule: "Routing Scheme Validation: The system must validate that the provided routing scheme is a supported type"

---

### 2. MappedBankAccount

**Database Location:** `code/model/dataAccess/MappedBankAccount.scala`

**Description:** The Bank Account entity that represents individual accounts within a bank. This entity is returned as the result of account routing lookups, containing account information including balance, currency, type, and label.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| bank | UUIDString | Bank identifier (foreign key to MappedBank) |
| theAccountId | AccountIdString | Account identifier |
| accountCurrency | MappedString(10) | Currency code for the account |
| accountNumber | MappedAccountNumber | Account number |
| accountBalance | MappedLong | Account balance in smallest currency unit |
| accountName | MappedString(255) | Name of the account |
| kind | MappedString(255) | Account type/financial product name |
| accountLabel | MappedString(255) | Account label |
| accountLastUpdate | MappedDateTime | Last update timestamp |
| mBranchId | UUIDString | Branch identifier |

**Relevance to User Story:**
- The entity returned when an account is found by routing information
- Output data includes: bank_id, account_id, label, currency, account_type
- Response payload: "The system shall return the matched account details when a valid routing identifier is provided"
- Contains the `accountRoutings` method that retrieves routing information from BankAccountRouting entity
- Acceptance criteria: "The system shall return the matched account details when a valid routing identifier is provided"
- Error response: "OBP-30018: Bank Account not found when no account matches the routing information"

---

### 3. MappedBank

**Database Location:** `code/model/dataAccess/MappedBank.scala`

**Description:** The Bank entity that represents financial institutions in the system. Referenced in the user story through BANK_ID path parameter for scoped account routing lookups.

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
- Referenced as BANK_ID in endpoint paths: `GET /obp/v4.0.0/banks/{BANK_ID}/accounts/account-routing-query`
- Input data: "bank_id (String, optional): Bank identifier to narrow down the search scope"
- Business rule: "Account Number Scope: When using account number for lookup, a bank_id should be provided to narrow the search scope"
- Dependency: "Bank Configuration: The bank must be configured and active on the platform"
- Response includes bank_id field identifying where the account is held

---

### 4. ViewDefinition

**Database Location:** `code/views/system/ViewDefinition.scala`

**Description:** The View entity that defines access permissions for bank accounts. Controls what account routing data fields are visible to users through permission flags.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| id_ | MappedLongIndex | Primary key identifier |
| name_ | MappedString(125) | View name |
| description_ | MappedString(255) | View description |
| bank_id | UUIDString | Bank identifier |
| account_id | AccountIdString | Account identifier |
| view_id | UUIDString | View identifier |
| isSystem_ | MappedBoolean | Whether this is a system view |
| isPublic_ | MappedBoolean | Whether this view is public |
| canSeeBankAccountRoutingScheme_ | MappedBoolean | Permission to see account routing scheme |
| canSeeBankAccountRoutingAddress_ | MappedBoolean | Permission to see account routing address |
| canSeeBankAccountIban_ | MappedBoolean | Permission to see account IBAN |
| canSeeBankAccountNumber_ | MappedBoolean | Permission to see account number |
| canSeeBankAccountBalance_ | MappedBoolean | Permission to see account balance |
| canSeeBankAccountCurrency_ | MappedBoolean | Permission to see account currency |
| canSeeBankAccountLabel_ | MappedBoolean | Permission to see account label |
| canSeeBankAccountType_ | MappedBoolean | Permission to see account type |

**Relevance to User Story:**
- Controls access permissions for account routing lookups
- Acceptance criteria: "The system shall require appropriate authentication and authorization to perform account routing lookups"
- Business rule: "Authorization Required: Users must have appropriate entitlements or view permissions to perform account routing lookups"
- The `canSeeBankAccountRoutingScheme_` and `canSeeBankAccountRoutingAddress_` fields determine if routing information is visible
- Error response: "OBP-20006: User is missing one or more roles for unauthorized requests"

---

### 5. AccountAccess

**Database Location:** `code/views/system/AccountAccess.scala`

**Description:** The join table that links Users to Views on specific Bank Accounts. This entity controls which users can access which accounts through which views for routing lookups.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| user_fk | MappedLongForeignKey | Foreign key to ResourceUser |
| bank_id | MappedString(255) | Bank identifier |
| account_id | MappedString(255) | Account identifier |
| view_id | UUIDString | View identifier |
| consumer_id | MappedString(255) | Consumer identifier (default: ALL_CONSUMERS) |

**Relevance to User Story:**
- Controls user access to accounts for routing lookups
- Dependency: "Authorization Service: User must have appropriate entitlements (e.g., CanGetAccountByRouting) or view permissions"
- Error response: "OBP-20001: User not logged in for unauthenticated requests"
- Ensures users can only look up accounts they have been granted permission to access

---

## Entity Relationships

```
MappedBank (1) ----< (N) MappedBankAccount
    |                         |
    |-- permalink (bankId)    |-- theAccountId (accountId)
    |                         |-- bank (FK to MappedBank)
    |                         |-- accountCurrency
    |                         |-- accountLabel
    |                         |-- kind (accountType)

MappedBankAccount (1) ----< (N) BankAccountRouting
    |                              |
    |-- theAccountId               |-- AccountId (FK to MappedBankAccount)
    |-- bank                       |-- BankId (FK to MappedBank)
                                   |-- AccountRoutingScheme (IBAN, ACCOUNT_NUMBER, etc.)
                                   |-- AccountRoutingAddress (actual routing value)

ViewDefinition (N) ----< (1) MappedBankAccount
    |
    |-- bank_id (FK to MappedBank)
    |-- account_id (FK to MappedBankAccount)
    |-- view_id (viewId)
    |-- canSeeBankAccountRoutingScheme_ (permission)
    |-- canSeeBankAccountRoutingAddress_ (permission)

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
| BankAccountRouting | Yes | Yes - Primary entity for routing lookups (IBAN, account number) |
| MappedBankAccount | Yes | Yes - Account entity returned from routing lookups |
| MappedBank | Yes | Yes - Referenced via BANK_ID parameter for scoped lookups |
| ViewDefinition | Yes | Yes - Controls routing information visibility permissions |
| AccountAccess | Yes | Yes - Controls user access to accounts for routing lookups |

## Notes

1. All entity names are exact matches from the Scala codebase database tables
2. BankAccountRouting is the PRIMARY entity for this capability, storing the routing scheme and address pairs
3. The BankAccountRouting entity has unique indexes on (BankId, AccountId, AccountRoutingScheme) and (BankId, AccountRoutingScheme, AccountRoutingAddress) to ensure efficient lookups
4. Multiple routing schemes can be associated with a single account (e.g., both IBAN and ACCOUNT_NUMBER)
5. The ViewDefinition entity controls which routing information fields are visible through permission flags like canSeeBankAccountRoutingScheme_ and canSeeBankAccountRoutingAddress_
6. Account routing lookups support multiple schemes including IBAN, ACCOUNT_NUMBER, BIC, and custom routing schemes
7. The MappedBankAccount.accountRoutings method retrieves all routing information from BankAccountRouting for a given account
8. IBAN lookups may search across all banks if bank_id is not provided, while account number lookups typically require bank_id for scoping
