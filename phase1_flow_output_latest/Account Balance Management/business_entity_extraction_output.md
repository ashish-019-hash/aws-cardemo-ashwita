# Business Entity Extraction for Account Balance Management

## Overview

This document contains the business entities extracted from the Account Balance Management user story. All entities listed below have been verified against the actual database tables in the Scala codebase (OBP-API) and are directly relevant to the capability described in the user story.

## Extracted Business Entities

### 1. BankAccountBalance

**Database Location:** `code/bankaccountbalance/BankAccountBalance.scala`

**Description:** The primary entity for managing account balance records. This entity stores balance information for bank accounts, supporting create, update, and delete operations as described in the user story.

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
- Directly supports "Create, update, and delete account balance records" capability
- Maps to endpoints: POST /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances, PUT /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances/{BALANCE_ID}, DELETE /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances/{BALANCE_ID}

---

### 2. MappedBank

**Database Location:** `code/model/dataAccess/MappedBank.scala`

**Description:** The Bank entity that represents financial institutions in the system. Referenced in the user story through BANK_ID path parameter in all balance management endpoints.

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
- Referenced as BANK_ID in all endpoint paths
- Validation requirement: "Bank ID must be a valid identifier for a bank on the platform"
- Dependency: "Bank must exist and be active on the platform"

---

### 3. MappedBankAccount

**Database Location:** `code/model/dataAccess/MappedBankAccount.scala`

**Description:** The Bank Account entity that represents individual accounts within a bank. Balance records are associated with specific accounts through the ACCOUNT_ID reference.

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
- Referenced as ACCOUNT_ID in all endpoint paths
- Validation requirement: "Account ID must be a valid identifier for an account at the specified bank"
- Dependency: "Account must exist in the system"
- Currency conversion: Balance amounts are converted using the account's currency

---

## Entity Relationships

```
MappedBank (1) ----< (N) MappedBankAccount (1) ----< (N) BankAccountBalance
    |                         |                              |
    |-- permalink (bankId)    |-- theAccountId (accountId)   |-- BalanceId_ (balanceId)
    |                         |-- bank (FK to MappedBank)    |-- BankId_ (FK to MappedBank)
    |                         |-- accountCurrency            |-- AccountId_ (FK to MappedBankAccount)
                                                             |-- BalanceType
                                                             |-- BalanceAmount
```

## Verification Summary

| Entity | Verified in Database | Relevant to User Story |
|--------|---------------------|------------------------|
| BankAccountBalance | Yes | Yes - Primary entity for balance management |
| MappedBank | Yes | Yes - Referenced via BANK_ID parameter |
| MappedBankAccount | Yes | Yes - Referenced via ACCOUNT_ID parameter |

## Notes

1. All entity names are exact matches from the Scala codebase database tables
2. The BankAccountBalance entity uses Lift Mapper ORM with KeyedMapper pattern
3. Balance amounts are stored in smallest currency units (cents, pence, etc.) and converted for display
4. The BalanceId_ field serves as the primary key for BankAccountBalance records
5. Currency conversion relies on the associated MappedBankAccount's accountCurrency field
