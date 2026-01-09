# Business Entities for Bank Creation and Configuration

## Overview

This document contains the business entities extracted from the user story for Bank Creation and Configuration capability. Each entity has been verified against the actual database tables in the Scala codebase (OBP-API) to ensure accuracy and relevance.

## Extracted Business Entities

### 1. MappedBank

**Database Table Name:** MappedBank

**Description:** Represents a bank entity on the OBP platform with identification, branding, and routing information.

**Source File:** `code/model/dataAccess/MappedBank.scala`

**Attributes:**
| Attribute Name | Data Type | Description |
|----------------|-----------|-------------|
| permalink | String (255) | Unique bank identifier used in URLs (bank_id) |
| fullBankName | String (255) | Full name of the bank |
| shortBankName | String (100) | Short name/code of the bank |
| logoURL | String (255) | URL to the bank's logo |
| websiteURL | String (255) | URL to the bank's website |
| swiftBIC | String (255) | SWIFT/BIC code for the bank |
| national_identifier | String (255) | National identifier for the bank |
| mBankRoutingScheme | String (255) | Bank routing scheme (e.g., BIC, OBP) |
| mBankRoutingAddress | String (255) | Bank routing address |

**Relevance to User Story:** This entity directly corresponds to the Bank creation endpoint (POST /banks) mentioned in the user story. It stores the core bank identification and branding information.

---

### 2. BankAttribute

**Database Table Name:** BankAttribute

**Description:** Represents custom attributes for a bank to store extended metadata. Supports multiple data types for flexible configuration.

**Source File:** `code/bankattribute/MappedBankAttributeProvider.scala`

**Attributes:**
| Attribute Name | Data Type | Description |
|----------------|-----------|-------------|
| BankId_ | UUID String | Reference to the parent bank |
| BankAttributeId | UUID | Unique identifier for the attribute |
| Name | String (50) | Name of the attribute |
| Type | String (50) | Data type (STRING, INTEGER, DOUBLE, DATE_WITH_DAY) |
| Value | String (255) | Value of the attribute |
| IsActive | Boolean | Whether the attribute is active (default: true) |

**Relevance to User Story:** This entity corresponds to the Bank Attribute creation endpoint (POST /banks/BANK_ID/attribute) and update endpoint (PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID) mentioned in the user story. It enables custom metadata storage for banks.

---

### 3. MappedBankAccount

**Database Table Name:** MappedBankAccount

**Description:** Represents a bank account, including settlement accounts. Settlement accounts are automatically created when a new bank is created in SANDBOX mode.

**Source File:** `code/model/dataAccess/MappedBankAccount.scala`

**Attributes:**
| Attribute Name | Data Type | Description |
|----------------|-----------|-------------|
| bank | UUID String | Reference to the parent bank |
| theAccountId | Account ID String | Unique account identifier |
| accountCurrency | String (10) | Currency of the account (e.g., EUR) |
| accountNumber | Account Number | Account number |
| accountBalance | Long | Account balance in smallest currency unit |
| accountName | String (255) | Name of the account |
| kind | String (255) | Account type/financial product name |
| accountLabel | String (255) | Label for the account |
| accountLastUpdate | DateTime | Last update timestamp |
| mBranchId | UUID String | Reference to the branch |
| accountRuleScheme1 | String (10) | First account rule scheme |
| accountRuleValue1 | Long | First account rule value |
| accountRuleScheme2 | String (10) | Second account rule scheme |
| accountRuleValue2 | Long | Second account rule value |

**Relevance to User Story:** This entity corresponds to the Settlement Account creation endpoint (POST /banks/BANK_ID/settlement-accounts) mentioned in the user story. Default incoming and outgoing settlement accounts (OBP_DEFAULT_INCOMING_ACCOUNT_ID, OBP_DEFAULT_OUTGOING_ACCOUNT_ID) are created with EUR currency when a bank is created.

---

### 4. BankAccountRouting

**Database Table Name:** BankAccountRouting

**Description:** Represents routing information for bank accounts, enabling lookups and transfers using various routing schemes.

**Source File:** `code/model/dataAccess/BankAccountRouting.scala`

**Attributes:**
| Attribute Name | Data Type | Description |
|----------------|-----------|-------------|
| BankId | UUID String | Reference to the parent bank |
| AccountId | Account ID String | Reference to the account |
| AccountRoutingScheme | String (32) | Routing scheme (e.g., IBAN, AccountNumber) |
| AccountRoutingAddress | String (128) | Routing address value |

**Relevance to User Story:** This entity supports the account_routings field in the Settlement Account creation endpoint. It stores routing information for accounts to enable payment processing.

---

### 5. MappedEntitlement

**Database Table Name:** MappedEntitlement

**Description:** Represents role-based entitlements/permissions assigned to users for specific banks or globally.

**Source File:** `code/entitlement/MappedEntitlements.scala`

**Attributes:**
| Attribute Name | Data Type | Description |
|----------------|-----------|-------------|
| mEntitlementId | UUID | Unique identifier for the entitlement |
| mBankId | UUID String | Reference to the bank (empty for global entitlements) |
| mUserId | UUID String | Reference to the user |
| mRoleName | String (64) | Name of the role/entitlement |
| mCreatedByProcess | String (255) | Process that created the entitlement (default: "manual") |

**Relevance to User Story:** This entity is used for automatic role assignment when a bank is created. According to the user story, the user creating the bank is automatically assigned the CanCreateEntitlementAtOneBank role for that bank.

---

## Entity Relationships

```
MappedBank (1) ----< (N) BankAttribute
    |
    |----< (N) MappedBankAccount (Settlement Accounts)
    |              |
    |              |----< (N) BankAccountRouting
    |
    |----< (N) MappedEntitlement
```

## Verification Summary

| Entity Name | Present in Database | Verified in Boot.scala |
|-------------|---------------------|------------------------|
| MappedBank | Yes | Line 1042 |
| BankAttribute | Yes | Line 1143 |
| MappedBankAccount | Yes | Line 1043 |
| BankAccountRouting | Yes | Line 1044 |
| MappedEntitlement | Yes | Line 1125 |

## Notes

1. All entities listed above have been verified against the ToSchemify.models list in `bootstrap/liftweb/Boot.scala` which contains all database tables managed by the OBP-API Scala application.

2. The entity names used are the exact class names from the Scala codebase that correspond to database tables.

3. Settlement accounts in the user story are implemented using the MappedBankAccount entity with specific account IDs (OBP_DEFAULT_INCOMING_ACCOUNT_ID, OBP_DEFAULT_OUTGOING_ACCOUNT_ID).

4. Bank routing information mentioned in the user story (bank_routings with scheme and address) is stored within the MappedBank entity itself using mBankRoutingScheme and mBankRoutingAddress fields.
