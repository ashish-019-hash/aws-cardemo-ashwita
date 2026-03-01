# Business Entity Extraction for Balancing Transaction Retrieval

## Overview

This document contains the business entities extracted from the Balancing Transaction Retrieval user story. All entities listed below have been verified against the actual database tables in the Scala codebase (OBP-API) and are directly relevant to the capability described in the user story.

## Extracted Business Entities

### 1. MappedBank

**Database Location:** `code/model/dataAccess/MappedBank.scala`

**Description:** The Bank entity that represents financial institutions in the system. Referenced in the user story through BANK_ID path parameter in the balancing transaction retrieval endpoint.

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
- Referenced as BANK_ID in endpoint path: `GET /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/transactions/{TRANSACTION_ID}/balancing-transaction`
- Validation requirement: "Bank identifier (BANK_ID) must be valid and exist in the system"
- Error response: "HTTP 404 Not Found / BankNotFound must be returned when BANK_ID does not exist"

---

### 2. MappedBankAccount

**Database Location:** `code/model/dataAccess/MappedBankAccount.scala`

**Description:** The Bank Account entity that represents individual accounts within a bank. Referenced in the user story through ACCOUNT_ID path parameter. The balancing transaction involves accounts on both sides of the double-entry bookkeeping.

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
- Referenced as ACCOUNT_ID in endpoint path
- The balancing transaction response includes `this_account` and `other_account` information
- Acceptance criteria: "The system shall include account identifiers for the account involved in the balancing transaction"
- Validation requirement: "Account identifier (ACCOUNT_ID) must be valid and belong to the specified bank"
- Error response: "HTTP 404 Not Found / AccountNotFound must be returned when ACCOUNT_ID does not exist"

---

### 3. MappedTransaction

**Database Location:** `code/transaction/MappedTransaction.scala`

**Description:** The Transaction entity that stores all card/bank transaction records. This is the primary entity for transaction data, containing transaction details including amount, currency, type, and counterparty information. The balancing transaction retrieval returns the corresponding transaction in the double-entry bookkeeping system.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| bank | MappedString(255) | Bank identifier |
| account | AccountIdString | Account identifier |
| transactionId | MappedString(255) | Unique transaction identifier (UUID) |
| transactionUUID | MappedUUID | Transaction UUID (legacy field) |
| transactionType | MappedString(100) | Type of transaction |
| amount | MappedLong | Transaction amount in smallest currency unit |
| newAccountBalance | MappedLong | Account balance after transaction |
| currency | MappedString(10) | Currency code |
| tStartDate | MappedDateTime | Transaction start date |
| tFinishDate | MappedDateTime | Transaction finish/completion date |
| description | MappedString(2000) | Transaction description |
| chargePolicy | MappedString(32) | Charge policy |
| counterpartyAccountHolder | MappedString(255) | Counterparty account holder name |
| counterpartyAccountKind | MappedString(40) | Counterparty account type |
| counterpartyBankName | MappedString(100) | Counterparty bank name |
| counterpartyNationalId | MappedString(40) | Counterparty national identifier |
| CPCounterPartyId | UUIDString | Counterparty identifier |
| CPOtherAccountProvider | MappedString(36) | Other account provider |
| CPOtherAccountRoutingScheme | MappedString(255) | Other account routing scheme |
| CPOtherAccountRoutingAddress | MappedString(255) | Other account routing address |
| CPOtherBankRoutingScheme | MappedString(255) | Other bank routing scheme |
| CPOtherBankRoutingAddress | MappedString(255) | Other bank routing address |
| status | MappedString(20) | Transaction status |

**Relevance to User Story:**
- Referenced as TRANSACTION_ID in endpoint path: `GET /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/transactions/{TRANSACTION_ID}/balancing-transaction`
- The balancing transaction response includes transaction details: `transaction_id`, `amount`, `date`, `description`, `balance`, `type`
- Output data includes: "transaction_id (String) - Unique identifier of the balancing transaction"
- Validation requirement: "Transaction identifier (TRANSACTION_ID) must be valid and belong to the specified account"
- Error response: "HTTP 404 Not Found / TransactionNotFound must be returned when TRANSACTION_ID does not exist"

---

### 4. DoubleEntryBookTransaction

**Database Location:** `code/model/dataAccess/DoubleEntryBookTransaction.scala`

**Description:** The core entity for double-entry bookkeeping that links debit and credit transactions together. This entity is essential for the balancing transaction retrieval capability as it maintains the relationship between a transaction and its corresponding balancing entry in the accounting system.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| TransactionRequestBankId | MappedString(255) | Bank ID of the transaction request |
| TransactionRequestAccountId | AccountIdString | Account ID of the transaction request |
| TransactionRequestId | UUIDString | Transaction request identifier |
| DebitTransactionBankId | MappedString(255) | Bank ID of the debit transaction |
| DebitTransactionAccountId | AccountIdString | Account ID of the debit transaction |
| DebitTransactionId | UUIDString | Unique identifier of the debit transaction |
| CreditTransactionBankId | MappedString(255) | Bank ID of the credit transaction |
| CreditTransactionAccountId | AccountIdString | Account ID of the credit transaction |
| CreditTransactionId | UUIDString | Unique identifier of the credit transaction |

**Relevance to User Story:**
- This is the primary entity that enables balancing transaction retrieval
- Business rule: "Balancing Transaction Principle: Every transaction in a double-entry bookkeeping system has a corresponding balancing transaction that represents the other side of the entry"
- Business rule: "One-to-One Relationship: Each transaction has exactly one balancing transaction that corresponds to it in the accounting system"
- Acceptance criteria: "The system shall return the balancing transaction that corresponds to the given transaction in the double-entry bookkeeping system"
- The entity links debit transactions to credit transactions, enabling retrieval of the balancing entry
- Database indexes ensure unique mapping: `UniqueIndex(DebitTransactionBankId, DebitTransactionAccountId, DebitTransactionId)` and `UniqueIndex(CreditTransactionBankId, CreditTransactionAccountId, CreditTransactionId)`

---

### 5. ViewDefinition

**Database Location:** `code/views/system/ViewDefinition.scala`

**Description:** The View entity that defines access permissions for bank accounts and transactions. Referenced in the user story through VIEW_ID path parameter for view-based access control. Controls what transaction data fields are visible to users.

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
| canSeeTransactionThisBankAccount_ | MappedBoolean | Permission to see transactions for this bank account |
| canSeeTransactionOtherBankAccount_ | MappedBoolean | Permission to see transactions for other bank account |
| canSeeTransactionAmount_ | MappedBoolean | Permission to see transaction amount |
| canSeeTransactionType_ | MappedBoolean | Permission to see transaction type |
| canSeeTransactionCurrency_ | MappedBoolean | Permission to see transaction currency |
| canSeeTransactionStartDate_ | MappedBoolean | Permission to see transaction start date |
| canSeeTransactionFinishDate_ | MappedBoolean | Permission to see transaction finish date |
| canSeeTransactionBalance_ | MappedBoolean | Permission to see transaction balance |
| canSeeTransactionDescription_ | MappedBoolean | Permission to see transaction description |

**Relevance to User Story:**
- Referenced as VIEW_ID in endpoint: `GET /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/transactions/{TRANSACTION_ID}/balancing-transaction`
- Controls access permissions for balancing transaction retrieval
- Notes for Implementation: "The user's view permissions determine what level of detail they can see about the balancing transaction. Some views may show full details while others may mask certain information."
- Validation requirement: "View identifier (VIEW_ID) must be valid and the user must have access to it"
- Acceptance criteria: "The system shall enforce access control to ensure users can only retrieve balancing transactions for accounts they have been granted permission to access"

---

### 6. AccountAccess

**Database Location:** `code/views/system/AccountAccess.scala`

**Description:** The join table that links Users to Views on specific Bank Accounts. This entity controls which users can access which accounts through which views, essential for enforcing access control on balancing transaction retrieval.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| user_fk | MappedLongForeignKey | Foreign key to ResourceUser |
| bank_id | MappedString(255) | Bank identifier |
| account_id | MappedString(255) | Account identifier |
| view_id | UUIDString | View identifier |
| consumer_id | MappedString(255) | Consumer identifier (default: ALL_CONSUMERS) |

**Relevance to User Story:**
- Controls user access to accounts and transactions through views
- Acceptance criteria: "The system shall enforce access control to ensure users can only retrieve balancing transactions for accounts they have been granted permission to access"
- Validation requirement: "User must have at least one view/permission granted on the account to see transactions"
- Error response: "HTTP 403 Forbidden when user lacks permission to access the account's transactions"
- Notes for Implementation: "Not all transactions may have a visible balancing transaction to all users - internal settlement transactions or inter-bank transfers may have restricted visibility"

---

## Entity Relationships

```
MappedBank (1) ----< (N) MappedBankAccount
    |                         |
    |-- permalink (bankId)    |-- theAccountId (accountId)
    |                         |-- bank (FK to MappedBank)
    |                         |-- accountCurrency
    |                         |-- accountBalance

MappedBankAccount (1) ----< (N) MappedTransaction
    |                              |
    |-- theAccountId               |-- account (FK to MappedBankAccount)
    |-- bank                       |-- bank (FK to MappedBank)
                                   |-- transactionId
                                   |-- amount
                                   |-- currency
                                   |-- description

MappedTransaction (1) ----< (1) DoubleEntryBookTransaction (Debit Side)
    |                              |
    |-- transactionId              |-- DebitTransactionId (FK to MappedTransaction)
    |-- bank                       |-- DebitTransactionBankId
    |-- account                    |-- DebitTransactionAccountId

MappedTransaction (1) ----< (1) DoubleEntryBookTransaction (Credit Side)
    |                              |
    |-- transactionId              |-- CreditTransactionId (FK to MappedTransaction)
    |-- bank                       |-- CreditTransactionBankId
    |-- account                    |-- CreditTransactionAccountId

ViewDefinition (N) ----< (1) MappedBankAccount
    |
    |-- bank_id (FK to MappedBank)
    |-- account_id (FK to MappedBankAccount)
    |-- view_id (viewId)
    |-- canSeeTransactionAmount_ (permission)
    |-- canSeeTransactionBalance_ (permission)

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
| MappedBankAccount | Yes | Yes - Referenced via ACCOUNT_ID parameter, provides account info in response |
| MappedTransaction | Yes | Yes - Referenced via TRANSACTION_ID parameter, primary transaction entity |
| DoubleEntryBookTransaction | Yes | Yes - Core entity linking transactions to their balancing entries |
| ViewDefinition | Yes | Yes - Referenced via VIEW_ID parameter for access control |
| AccountAccess | Yes | Yes - Controls user access to accounts through views |

## Notes

1. All entity names are exact matches from the Scala codebase database tables
2. The DoubleEntryBookTransaction entity is the key entity for this capability, as it maintains the relationship between debit and credit transactions in the double-entry bookkeeping system
3. The MappedTransaction entity uses Lift Mapper ORM with LongKeyedMapper pattern
4. Transaction amounts are stored in smallest currency units (cents, pence, etc.) and converted for display using the Helper.smallestCurrencyUnitToBigDecimal method
5. The ViewDefinition entity controls which users can see specific transaction information through permission fields like canSeeTransactionAmount_, canSeeTransactionBalance_, etc.
6. The AccountAccess entity ensures users can only access transactions for accounts they have been granted permission to view
7. The balancing transaction retrieval works by looking up the DoubleEntryBookTransaction record for a given transaction and returning the corresponding transaction on the other side of the entry (debit returns credit, credit returns debit)
