# Business Entity Extraction for Account Balance Retrieval

## Overview

This document contains the business entities extracted from the Account Balance Retrieval user story. All entities listed below have been verified against the actual database tables in the Scala codebase (OBP-API) and are directly relevant to the capability described in the user story.

## Extracted Business Entities

### 1. MappedBank

**Database Location:** `code/model/dataAccess/MappedBank.scala`

**Description:** The Bank entity that represents financial institutions in the system. Referenced in the user story through BANK_ID path parameter in all balance retrieval endpoints.

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
- Referenced as BANK_ID in all endpoint paths: `GET /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances`
- Validation requirement: "Bank ID must be a valid identifier for a bank on the platform"
- Dependency: "Bank must be active on the platform"

---

### 2. MappedBankAccount

**Database Location:** `code/model/dataAccess/MappedBankAccount.scala`

**Description:** The Bank Account entity that represents individual accounts within a bank. Balance information is retrieved for specific accounts through the ACCOUNT_ID reference.

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

**Relevance to User Story:**
- Referenced as ACCOUNT_ID in all endpoint paths
- Contains the accountBalance field used for balance retrieval
- Contains accountCurrency field for currency information in response
- Validation requirement: "Account ID must be a valid identifier for an account at the specified bank"
- Dependency: "Account must exist in the system"

---

### 3. BankAccountBalance

**Database Location:** `code/bankaccountbalance/BankAccountBalance.scala`

**Description:** The entity for storing detailed balance records for bank accounts. This entity supports multiple balance types (available, booked, etc.) and is used by the getBankAccountBalances connector method for balance retrieval.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| BankId_ | MappedUUID | The identifier of the bank |
| AccountId_ | MappedUUID | The identifier of the account |
| BalanceId_ | MappedUUID | Unique identifier for the balance record (Primary Key) |
| BalanceType | MappedString(255) | Type of balance (e.g., available, booked) |
| BalanceAmount | MappedLong | Balance amount in smallest currency unit (e.g., cents) |
| ReferenceDate | MappedDate | Reference date for the balance |

**Relevance to User Story:**
- Directly supports "Get current balance information for accounts" capability
- Maps to endpoints: `GET /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances`
- Provides balance type information (available, booked, etc.) as mentioned in the user story output data
- Used by `getBankAccountBalances` and `getBankAccountBalancesByAccountId` connector methods

---

### 4. ViewDefinition

**Database Location:** `code/views/system/ViewDefinition.scala`

**Description:** The View entity that defines access permissions for bank accounts. Referenced in the user story through VIEW_ID path parameter for view-based balance access control.

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

**Relevance to User Story:**
- Referenced as VIEW_ID in endpoint: `GET /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/balances`
- Controls access permissions for balance retrieval
- Validation requirement: "View ID (when provided) must be a valid view that the user has access to"
- The `canSeeBankAccountBalance_` field determines if balance can be viewed through this view

---

## Entity Relationships

```
MappedBank (1) ----< (N) MappedBankAccount (1) ----< (N) BankAccountBalance
    |                         |                              |
    |-- permalink (bankId)    |-- theAccountId (accountId)   |-- BalanceId_ (balanceId)
    |                         |-- bank (FK to MappedBank)    |-- BankId_ (FK to MappedBank)
    |                         |-- accountCurrency            |-- AccountId_ (FK to MappedBankAccount)
    |                         |-- accountBalance             |-- BalanceType
                                                             |-- BalanceAmount

ViewDefinition (N) ----< (1) MappedBankAccount
    |
    |-- bank_id (FK to MappedBank)
    |-- account_id (FK to MappedBankAccount)
    |-- view_id (viewId)
    |-- canSeeBankAccountBalance_ (permission)
```

## Verification Summary

| Entity | Verified in Database | Relevant to User Story |
|--------|---------------------|------------------------|
| MappedBank | Yes | Yes - Referenced via BANK_ID parameter |
| MappedBankAccount | Yes | Yes - Referenced via ACCOUNT_ID parameter, contains balance data |
| BankAccountBalance | Yes | Yes - Primary entity for detailed balance records |
| ViewDefinition | Yes | Yes - Referenced via VIEW_ID parameter for access control |

## Notes

1. All entity names are exact matches from the Scala codebase database tables
2. The BankAccountBalance entity uses Lift Mapper ORM with KeyedMapper pattern
3. Balance amounts are stored in smallest currency units (cents, pence, etc.) and converted for display using the Helper.smallestCurrencyUnitToBigDecimal method
4. The ViewDefinition entity controls which users can see balance information through the canSeeBankAccountBalance_ permission field
5. Currency information is retrieved from the associated MappedBankAccount's accountCurrency field
6. The getBankAccountBalances connector method retrieves balance data from BankAccountBalance entity
