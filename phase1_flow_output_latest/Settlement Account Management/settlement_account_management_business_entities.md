# Business Entity Extraction for Settlement Account Management

## Overview

This document contains the business entities extracted from the Settlement Account Management user story. Each entity has been verified against the actual database tables in the Scala (OBP-API) codebase to ensure accuracy and relevance.

## Extracted Business Entities

### 1. MappedBankAccount

**Database Table Name:** MappedBankAccount

**Description:** The primary entity for storing bank accounts, including settlement accounts. Settlement accounts are stored in this table with the `kind` field set to "SETTLEMENT". This entity supports the core functionality of creating and managing settlement accounts for double-entry bookkeeping and payment processing.

**Relevance to User Story:**
- Settlement accounts are created and stored as MappedBankAccount records
- Supports account identification (account_id, bank_id)
- Stores balance information (accountBalance, accountCurrency)
- Contains account metadata (accountName, accountLabel, kind)
- Links to branch via mBranchId field

**Key Attributes (from Scala codebase):**
- `bank` (UUIDString) - Bank identifier
- `theAccountId` (AccountIdString) - Account identifier
- `accountCurrency` (MappedString) - Currency code
- `accountBalance` (MappedLong) - Account balance in smallest currency unit
- `accountName` (MappedString) - Account name
- `kind` (MappedString) - Account type (e.g., "SETTLEMENT")
- `accountLabel` (MappedString) - Account label
- `mBranchId` (UUIDString) - Branch identifier

---

### 2. MappedBank

**Database Table Name:** MappedBank

**Description:** The entity representing a bank in the system. Settlement accounts must be associated with a valid bank entity, making this a critical dependency for settlement account management.

**Relevance to User Story:**
- Settlement accounts must be created under a valid bank (BANK_ID validation)
- Bank entity provides context for settlement account operations
- Default settlement accounts are automatically created when a bank is created

**Key Attributes (from Scala codebase):**
- `permalink` (MappedString) - Bank identifier used in URLs
- `fullBankName` (MappedString) - Full name of the bank
- `shortBankName` (MappedString) - Short name of the bank
- `logoURL` (MappedString) - Bank logo URL
- `websiteURL` (MappedString) - Bank website URL
- `swiftBIC` (MappedString) - SWIFT/BIC code
- `national_identifier` (MappedString) - National identifier
- `mBankRoutingScheme` (MappedString) - Bank routing scheme
- `mBankRoutingAddress` (MappedString) - Bank routing address

---

### 3. ResourceUser

**Database Table Name:** ResourceUser

**Description:** The entity representing a user in the system. Settlement accounts can be owned by a specific user, and user authentication/authorization is required for settlement account operations.

**Relevance to User Story:**
- Settlement accounts can be associated with a user_id
- User ownership of settlement accounts
- Authorization validation for settlement account creation

**Key Attributes (from Scala codebase):**
- `id` (MappedLongIndex) - Primary key
- `userId_` (MappedUUID) - User identifier
- `email` (MappedEmail) - User email
- `name_` (MappedString) - User name
- `provider_` (MappedString) - Identity provider
- `providerId` (MappedString) - Provider-specific user ID

---

### 4. BankAccountRouting

**Database Table Name:** BankAccountRouting

**Description:** The entity for storing account routing information. Settlement accounts support account routings for integration with external payment systems like SEPA and CARD networks.

**Relevance to User Story:**
- Settlement accounts support account_routings configuration
- Enables integration with external payment systems
- Stores routing scheme (e.g., IBAN) and address information

**Key Attributes (from Scala codebase):**
- `BankId` (UUIDString) - Bank identifier
- `AccountId` (AccountIdString) - Account identifier
- `AccountRoutingScheme` (MappedString) - Routing scheme (e.g., "IBAN")
- `AccountRoutingAddress` (MappedString) - Routing address

---

### 5. MappedAccountAttribute

**Database Table Name:** MappedAccountAttribute

**Description:** The entity for storing account attributes. Settlement accounts can have associated attributes for additional configuration and metadata.

**Relevance to User Story:**
- Settlement accounts support account_attributes
- Enables flexible attribute storage for settlement account configuration
- Links attributes to specific accounts and banks

**Key Attributes (from Scala codebase):**
- `mBankIdId` (UUIDString) - Bank identifier
- `mAccountId` (UUIDString) - Account identifier
- `mCode` (MappedString) - Product code
- `mAccountAttributeId` (MappedUUID) - Attribute identifier
- `mName` (MappedString) - Attribute name
- `mType` (MappedString) - Attribute type
- `mValue` (MappedString) - Attribute value

---

### 6. MappedBranch

**Database Table Name:** MappedBranch

**Description:** The entity representing a bank branch. Settlement accounts can be associated with a specific branch via the branch_id field.

**Relevance to User Story:**
- Settlement accounts can specify a branch_id
- Branch association for organizational purposes
- Links settlement accounts to physical bank locations

**Key Attributes (from Scala codebase):**
- `mBankId` (UUIDString) - Bank identifier
- `mBranchId` (UUIDString) - Branch identifier
- `mName` (MappedString) - Branch name
- `mLine1`, `mLine2`, `mLine3` (MappedString) - Address lines
- `mCity`, `mState`, `mCountryCode`, `mPostCode` (MappedString) - Address components
- `mBranchRoutingScheme` (MappedString) - Branch routing scheme
- `mBranchRoutingAddress` (MappedString) - Branch routing address

---

### 7. DoubleEntryBookTransaction

**Database Table Name:** DoubleEntryBookTransaction

**Description:** The entity for storing double-entry bookkeeping transactions. Settlement accounts serve as counterparty accounts for transactions when no OBP account can be found for the actual counterparty.

**Relevance to User Story:**
- Settlement accounts are used for double-entry bookkeeping
- Links debit and credit transactions
- Supports transaction processing with settlement accounts as counterparty

**Key Attributes (from Scala codebase):**
- `TransactionRequestBankId` (MappedString) - Transaction request bank ID
- `TransactionRequestAccountId` (AccountIdString) - Transaction request account ID
- `TransactionRequestId` (UUIDString) - Transaction request ID
- `DebitTransactionBankId` (MappedString) - Debit transaction bank ID
- `DebitTransactionAccountId` (AccountIdString) - Debit transaction account ID
- `DebitTransactionId` (UUIDString) - Debit transaction ID
- `CreditTransactionBankId` (MappedString) - Credit transaction bank ID
- `CreditTransactionAccountId` (AccountIdString) - Credit transaction account ID
- `CreditTransactionId` (UUIDString) - Credit transaction ID

---

### 8. MappedEntitlement

**Database Table Name:** MappedEntitlement

**Description:** The entity for storing user entitlements/permissions. Users must have the CanCreateSettlementAccountAtOneBank role to create settlement accounts.

**Relevance to User Story:**
- Authorization validation for settlement account creation
- Stores user roles and permissions
- Enforces access control for settlement account operations

**Key Attributes (from Scala codebase):**
- `mEntitlementId` (MappedUUID) - Entitlement identifier
- `mBankId` (UUIDString) - Bank identifier (for bank-specific roles)
- `mUserId` (UUIDString) - User identifier
- `mRoleName` (MappedString) - Role name (e.g., "CanCreateSettlementAccountAtOneBank")
- `mCreatedByProcess` (MappedString) - Process that created the entitlement

---

## Entity Relationships

```
MappedBank (1) ----< (N) MappedBankAccount (Settlement Accounts)
MappedBankAccount (1) ----< (N) BankAccountRouting
MappedBankAccount (1) ----< (N) MappedAccountAttribute
MappedBranch (1) ----< (N) MappedBankAccount
ResourceUser (1) ----< (N) MappedBankAccount (ownership)
ResourceUser (1) ----< (N) MappedEntitlement
MappedBankAccount (Settlement) ----< (N) DoubleEntryBookTransaction
```

## Verification Notes

All entities listed above have been verified against the actual database model classes in the OBP-API Scala codebase located at `/home/ubuntu/repos/OBP-API-Cloned/obp-api/src/main/scala/code/`. The entity names match the exact class names used in the Scala codebase, which correspond to the database table names through Lift's ORM mapping.

## Summary

| Entity Name | Database Table | Relevance |
|-------------|----------------|-----------|
| MappedBankAccount | MappedBankAccount | Primary entity for settlement accounts |
| MappedBank | MappedBank | Bank entity (required dependency) |
| ResourceUser | ResourceUser | User entity (ownership and authorization) |
| BankAccountRouting | BankAccountRouting | Account routing for payment systems |
| MappedAccountAttribute | MappedAccountAttribute | Account attributes storage |
| MappedBranch | MappedBranch | Branch association |
| DoubleEntryBookTransaction | DoubleEntryBookTransaction | Double-entry bookkeeping support |
| MappedEntitlement | MappedEntitlement | User permissions/roles |
