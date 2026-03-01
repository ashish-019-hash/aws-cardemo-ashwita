# Business Entity Extraction for Double-Entry Transaction Retrieval

## Capability Information

- **Capability Name**: Double-Entry Transaction Retrieval
- **Description**: View double-entry bookkeeping transactions showing debit and credit sides
- **Source User Story**: double_entry_transaction_retrieval_user_story.md

---

## Extracted Business Entities

The following business entities have been extracted from the user story and verified against the actual database tables in the Scala codebase (OBP-API). Only entities that exist in the database and are relevant to the Double-Entry Transaction Retrieval capability are included.

---

### 1. DoubleEntryBookTransaction

**Database Table Name**: `DoubleEntryBookTransaction`

**Source File**: `code/model/dataAccess/DoubleEntryBookTransaction.scala`

**Description**: The primary entity for storing double-entry bookkeeping transaction records. This entity links the debit and credit sides of a transaction, maintaining the fundamental double-entry bookkeeping principle where every transaction has corresponding debit and credit entries.

**Relevance to User Story**: This is the core entity for the "View double-entry bookkeeping transactions showing debit and credit sides" capability. It stores the relationship between debit and credit transactions.

**Database Fields**:
| Field Name | Data Type | Description |
|------------|-----------|-------------|
| TransactionRequestBankId | MappedString(255) | Bank ID associated with the transaction request |
| TransactionRequestAccountId | AccountIdString | Account ID associated with the transaction request |
| TransactionRequestId | UUIDString | Unique identifier for the transaction request |
| DebitTransactionBankId | MappedString(255) | Bank ID for the debit side of the transaction |
| DebitTransactionAccountId | AccountIdString | Account ID for the debit side of the transaction |
| DebitTransactionId | UUIDString | Unique identifier for the debit transaction |
| CreditTransactionBankId | MappedString(255) | Bank ID for the credit side of the transaction |
| CreditTransactionAccountId | AccountIdString | Account ID for the credit side of the transaction |
| CreditTransactionId | UUIDString | Unique identifier for the credit transaction |

**Indexes**:
- UniqueIndex(DebitTransactionBankId, DebitTransactionAccountId, DebitTransactionId)
- UniqueIndex(CreditTransactionBankId, CreditTransactionAccountId, CreditTransactionId)

---

### 2. MappedTransaction

**Database Table Name**: `MappedTransaction`

**Source File**: `code/transaction/MappedTransaction.scala`

**Description**: The core transaction entity that stores individual financial transaction records. This entity contains the transaction details including amount, currency, dates, and counterparty information. Both debit and credit transactions in a double-entry are stored as MappedTransaction records.

**Relevance to User Story**: Referenced in the user story as the source of transaction data for both debit_transaction and credit_transaction in the double-entry view. The endpoint retrieves transaction details including amounts, accounts involved, and transaction metadata.

**Database Fields**:
| Field Name | Data Type | Description |
|------------|-----------|-------------|
| bank | MappedString(255) | Bank identifier |
| account | AccountIdString | Account identifier |
| transactionId | MappedString(255) | Unique transaction identifier (UUID) |
| transactionUUID | MappedUUID | Legacy UUID field |
| transactionType | MappedString(100) | Type of transaction |
| amount | MappedLong | Transaction amount in smallest currency unit |
| newAccountBalance | MappedLong | Account balance after transaction |
| currency | MappedString(10) | Currency code |
| tStartDate | MappedDateTime | Transaction start date |
| tFinishDate | MappedDateTime | Transaction finish date |
| description | MappedString(2000) | Transaction description |
| chargePolicy | MappedString(32) | Charge policy |
| counterpartyAccountHolder | MappedString(255) | Counterparty account holder name |
| counterpartyAccountKind | MappedString(40) | Counterparty account type |
| counterpartyBankName | MappedString(100) | Counterparty bank name |
| counterpartyNationalId | MappedString(40) | Counterparty national identifier |
| CPCounterPartyId | UUIDString | Counterparty unique identifier |
| CPOtherAccountRoutingScheme | MappedString(255) | Counterparty routing scheme |
| CPOtherAccountRoutingAddress | MappedString(255) | Counterparty routing address |
| CPOtherBankRoutingScheme | MappedString(255) | Counterparty bank routing scheme |
| CPOtherBankRoutingAddress | MappedString(255) | Counterparty bank routing address |
| status | MappedString(20) | Transaction status |

**Indexes**:
- UniqueIndex(transactionId, bank, account)
- Index(bank, account)

---

### 3. MappedBank

**Database Table Name**: `MappedBank`

**Source File**: `code/model/dataAccess/MappedBank.scala`

**Description**: The bank entity that stores bank information including identification, branding, and routing details. Banks are referenced in double-entry transactions to identify which bank holds the debited and credited accounts.

**Relevance to User Story**: Referenced in the endpoint path parameters (BANK_ID) and in the response structure (bank_id for both debit and credit transactions). The user story requires bank validation: "Bank identifier (BANK_ID) must be valid and exist in the system".

**Database Fields**:
| Field Name | Data Type | Description |
|------------|-----------|-------------|
| permalink | MappedString(255) | Bank ID used in URLs |
| fullBankName | MappedString(255) | Full name of the bank |
| shortBankName | MappedString(100) | Short name of the bank |
| logoURL | MappedString(255) | URL to bank logo |
| websiteURL | MappedString(255) | Bank website URL |
| swiftBIC | MappedString(255) | SWIFT/BIC code |
| national_identifier | MappedString(255) | National identifier |
| mBankRoutingScheme | MappedString(255) | Bank routing scheme |
| mBankRoutingAddress | MappedString(255) | Bank routing address |

**Indexes**:
- Index(permalink)

---

### 4. MappedBankAccount

**Database Table Name**: `MappedBankAccount`

**Source File**: `code/model/dataAccess/MappedBankAccount.scala`

**Description**: The bank account entity that stores account information including balance, currency, and account identifiers. Accounts are the fundamental units in double-entry bookkeeping - one account is debited and another is credited.

**Relevance to User Story**: Referenced in the endpoint path parameters (ACCOUNT_ID) and in the response structure (account_id for both debit and credit transactions). The user story states: "Account identifier (ACCOUNT_ID) must be valid and belong to the specified bank".

**Database Fields**:
| Field Name | Data Type | Description |
|------------|-----------|-------------|
| bank | UUIDString | Bank identifier |
| theAccountId | AccountIdString | Account identifier |
| accountCurrency | MappedString(10) | Account currency |
| accountNumber | MappedAccountNumber | Account number |
| accountBalance | MappedLong | Account balance in smallest currency unit |
| accountName | MappedString(255) | Account name |
| kind | MappedString(255) | Account type/financial product name |
| accountLabel | MappedString(255) | Account label |
| accountLastUpdate | MappedDateTime | Last update timestamp |
| mBranchId | UUIDString | Branch identifier |
| accountRuleScheme1 | MappedString(10) | Account rule scheme 1 |
| accountRuleValue1 | MappedLong | Account rule value 1 |
| accountRuleScheme2 | MappedString(10) | Account rule scheme 2 |
| accountRuleValue2 | MappedLong | Account rule value 2 |

**Indexes**:
- UniqueIndex(bank, theAccountId)

---

### 5. ViewDefinition

**Database Table Name**: `ViewDefinition`

**Source File**: `code/views/system/ViewDefinition.scala`

**Description**: The view/permission entity that defines access control for accounts and transactions. Views determine what level of detail users can see about transactions and accounts.

**Relevance to User Story**: Referenced in the endpoint path parameters (VIEW_ID) and in the access control requirements. The user story states: "View identifier (VIEW_ID) must be valid and the user must have access to it" and "The user's view permissions determine what level of detail they can see about the double-entry transaction."

**Database Fields**:
| Field Name | Data Type | Description |
|------------|-----------|-------------|
| id_ | MappedLongIndex | Primary key |
| name_ | MappedString(125) | View name |
| description_ | MappedString(255) | View description |
| bank_id | UUIDString | Bank identifier |
| account_id | AccountIdString | Account identifier |
| view_id | UUIDString | View identifier |
| metadataView_ | UUIDString | Metadata view reference |
| isSystem_ | MappedBoolean | Whether this is a system view |
| isPublic_ | MappedBoolean | Whether this view is public |
| isFirehose_ | MappedBoolean | Whether this is a firehose view |
| canSeeTransactionThisBankAccount_ | MappedBoolean | Permission to see transactions |
| canSeeTransactionOtherBankAccount_ | MappedBoolean | Permission to see other account transactions |
| canSeeTransactionAmount_ | MappedBoolean | Permission to see transaction amount |
| canSeeTransactionCurrency_ | MappedBoolean | Permission to see transaction currency |
| canSeeTransactionDescription_ | MappedBoolean | Permission to see transaction description |
| canSeeTransactionStartDate_ | MappedBoolean | Permission to see transaction start date |
| canSeeTransactionFinishDate_ | MappedBoolean | Permission to see transaction finish date |
| canSeeTransactionBalance_ | MappedBoolean | Permission to see transaction balance |

---

### 6. MappedCounterparty

**Database Table Name**: `MappedCounterparty`

**Source File**: `code/metadata/counterparties/MapperCounterparties.scala`

**Description**: The counterparty entity that stores information about the other party in a transaction. In double-entry bookkeeping, the counterparty represents the other side of the transaction.

**Relevance to User Story**: Referenced in the "Get Other Account of Transaction" endpoint which retrieves information about the other account involved in the transaction, representing the other side of the double-entry.

**Database Fields**:
| Field Name | Data Type | Description |
|------------|-----------|-------------|
| mCounterPartyId | String | Counterparty unique identifier |
| mName | MappedString(255) | Counterparty name |
| mCreatedByUserId | String | User who created the counterparty |
| mThisBankId | String | Bank ID of the account holder |
| mThisAccountId | String | Account ID of the account holder |
| mThisViewId | String | View ID |
| mOtherAccountRoutingScheme | MappedString(255) | Other account routing scheme |
| mOtherAccountRoutingAddress | MappedString(255) | Other account routing address |
| mOtherBankRoutingScheme | MappedString(255) | Other bank routing scheme |
| mOtherBankRoutingAddress | MappedString(255) | Other bank routing address |
| mOtherBranchRoutingScheme | MappedString(255) | Other branch routing scheme |
| mOtherBranchRoutingAddress | MappedString(255) | Other branch routing address |
| mIsBeneficiary | Boolean | Whether this is a beneficiary |
| mDescription | String | Description |
| mCurrency | String | Currency |
| mOtherAccountSecondaryRoutingScheme | String | Secondary routing scheme |
| mOtherAccountSecondaryRoutingAddress | String | Secondary routing address (e.g., IBAN) |

---

### 7. MappedCounterpartyMetadata

**Database Table Name**: `MappedCounterpartyMetadata`

**Source File**: `code/metadata/counterparties/MapperCounterparties.scala`

**Description**: Metadata entity for counterparties that stores additional information like aliases, URLs, and location data. This metadata enriches the counterparty information displayed in transaction views.

**Relevance to User Story**: Referenced in the "Get Other Account of Transaction" endpoint response which includes metadata fields like public_alias, private_alias, more_info, url, and image_url.

**Database Fields**:
| Field Name | Data Type | Description |
|------------|-----------|-------------|
| counterpartyId | UUIDString | Counterparty identifier |
| counterpartyName | MappedString(255) | Counterparty name |
| thisBankId | UUIDString | Bank ID of the account holder |
| thisAccountId | AccountIdString | Account ID of the account holder |
| publicAlias | MappedString(64) | Public alias for the counterparty |
| privateAlias | MappedString(64) | Private alias for the counterparty |
| moreInfo | MappedString(255) | Additional information |
| url | MappedString(2000) | URL |
| imageUrl | MappedString(2000) | Image URL |
| openCorporatesUrl | MappedString(2000) | Open Corporates URL |
| physicalLocation | MappedLongForeignKey | Physical location reference |
| corporateLocation | MappedLongForeignKey | Corporate location reference |

**Indexes**:
- Index(thisBankId, thisAccountId)
- Index(counterpartyId)

---

## Entity Relationships

The following diagram illustrates the relationships between the extracted business entities:

```
MappedBank (1) ----< (N) MappedBankAccount
     |                        |
     |                        |
     v                        v
DoubleEntryBookTransaction -----> MappedTransaction (Debit)
     |                                    |
     |                                    v
     +----------------------------> MappedTransaction (Credit)
                                          |
                                          v
                                   MappedCounterparty
                                          |
                                          v
                                   MappedCounterpartyMetadata

ViewDefinition -----> Controls access to all transaction views
```

**Key Relationships**:
1. **DoubleEntryBookTransaction** links two **MappedTransaction** records (debit and credit sides)
2. **MappedTransaction** belongs to a **MappedBankAccount** which belongs to a **MappedBank**
3. **MappedTransaction** references a **MappedCounterparty** for the other party in the transaction
4. **MappedCounterparty** has associated **MappedCounterpartyMetadata** for additional details
5. **ViewDefinition** controls access permissions for viewing transactions and accounts

---

## Verification Summary

| Entity Name | Verified in Database | Relevant to User Story |
|-------------|---------------------|------------------------|
| DoubleEntryBookTransaction | Yes | Yes - Core entity for double-entry transactions |
| MappedTransaction | Yes | Yes - Transaction details for debit/credit sides |
| MappedBank | Yes | Yes - Bank identification and validation |
| MappedBankAccount | Yes | Yes - Account identification and balance |
| ViewDefinition | Yes | Yes - Access control and permissions |
| MappedCounterparty | Yes | Yes - Other account in transaction |
| MappedCounterpartyMetadata | Yes | Yes - Counterparty metadata in response |

---

## Notes

1. All entities listed above have been verified to exist in the Scala codebase database layer (OBP-API).
2. Entity names match exactly as they appear in the database/ORM layer.
3. Only entities directly relevant to the Double-Entry Transaction Retrieval capability have been included.
4. The DoubleEntryBookTransaction entity is the primary entity that implements the double-entry bookkeeping pattern by linking debit and credit transactions.
