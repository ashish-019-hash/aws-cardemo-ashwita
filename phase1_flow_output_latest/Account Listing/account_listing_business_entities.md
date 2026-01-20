# Business Entity Extraction for Account Listing

## Overview

This document contains the business entities extracted from the Account Listing user story. All entities listed below have been verified against the actual database tables in the Scala codebase (OBP-API) and are directly relevant to the capability described in the user story.

## Extracted Business Entities

### 1. MappedBank

**Database Location:** `code/model/dataAccess/MappedBank.scala`

**Description:** The Bank entity that represents financial institutions in the system. Referenced in the user story through BANK_ID path parameter in account listing endpoints. Account listing is scoped to a specific bank.

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
- Referenced as BANK_ID in endpoint paths: `GET /obp/v4.0.0/banks/BANK_ID/accounts`
- Business rule: "Bank Scope: Account listing is scoped to a specific bank - users must specify which bank's accounts to retrieve"
- Validation: "Bank identifier (BANK_ID) must be valid and exist in the system when retrieving accounts at a specific bank"
- Error handling: "Error response (HTTP 404 Not Found / BankNotFound) must be returned when BANK_ID does not exist"

---

### 2. MappedBankAccount

**Database Location:** `code/model/dataAccess/MappedBankAccount.scala`

**Description:** The Bank Account entity that represents individual accounts within a bank. This is the primary entity being retrieved and listed in the account listing capability.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| bank | UUIDString | Bank identifier (foreign key to MappedBank) |
| theAccountId | AccountIdString | Account identifier |
| accountCurrency | MappedString(10) | Currency code for the account (e.g., EUR, USD, GBP) |
| accountNumber | MappedAccountNumber | Account number |
| accountBalance | MappedLong | Account balance in smallest currency unit (e.g., cents) |
| accountName | MappedString(255) | Name of the account |
| kind | MappedString(255) | Account type/financial product name (e.g., CHECKING, SAVINGS) |
| accountLabel | MappedString(255) | User-friendly account label |
| accountLastUpdate | MappedDateTime | Last update timestamp |
| mBranchId | UUIDString | Branch identifier |
| accountRuleScheme1 | MappedString(10) | First account rule scheme |
| accountRuleValue1 | MappedLong | First account rule value |
| accountRuleScheme2 | MappedString(10) | Second account rule scheme |
| accountRuleValue2 | MappedLong | Second account rule value |

**Relevance to User Story:**
- Primary entity returned in account listing responses
- Output data includes: id, bank_id, label, number, type, balance
- Acceptance criteria: "The system shall return account identifiers for each account to enable subsequent API calls"
- Acceptance criteria: "The system shall return account metadata including account type, label, and balance information based on the requested detail level"
- Business rule: "Detail Level Flexibility: The system must support various detail levels - allowing applications to request minimal, basic, or detailed account information"

---

### 3. ResourceUser

**Database Location:** `code/model/dataAccess/ResourceUser.scala`

**Description:** The User entity that represents users in the system. Referenced during account listing for authentication and determining which accounts the user has access to view.

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
- Authentication: "User must be authenticated with a valid OAuth token or DirectLogin credentials"
- Access control: "Only accounts that the user has access to should be returned"
- Business rule: "User Context: Account retrieval is user-centric - the list is personalized based on the authenticated user's granted permissions"
- Error handling: "Error response (HTTP 401 Unauthorized) for missing or invalid authentication"

---

### 4. ViewDefinition

**Database Location:** `code/views/system/ViewDefinition.scala`

**Description:** The View entity that defines permissions and access levels for accounts. Views control what information users can see about accounts and what operations they can perform.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| id_ | MappedLongIndex | Primary key identifier |
| name_ | MappedString(125) | View name |
| description_ | MappedString(255) | View description |
| bank_id | UUIDString | Bank identifier |
| account_id | AccountIdString | Account identifier |
| view_id | UUIDString | View identifier |
| metadataView_ | UUIDString | Metadata view reference |
| isSystem_ | MappedBoolean | Whether this is a system view |
| isPublic_ | MappedBoolean | Whether this view is public |
| isFirehose_ | MappedBoolean | Whether this is a firehose view |
| canSeeBankAccountOwners_ | MappedBoolean | Permission to see account owners |
| canSeeBankAccountType_ | MappedBoolean | Permission to see account type |
| canSeeBankAccountBalance_ | MappedBoolean | Permission to see account balance |
| canSeeBankAccountCurrency_ | MappedBoolean | Permission to see account currency |
| canSeeBankAccountLabel_ | MappedBoolean | Permission to see account label |
| canSeeBankAccountNumber_ | MappedBoolean | Permission to see account number |
| canSeeBankAccountIban_ | MappedBoolean | Permission to see IBAN |
| canSeeAvailableViewsForBankAccount_ | MappedBoolean | Permission to see available views |

**Relevance to User Story:**
- Output data: "views_available (List[ViewBasic]) - Views the user has access to for this account"
- Technical context: "Views (code.views.Views) - View/permission management for account access control"
- Acceptance criteria: "The system shall return only accounts that the requesting user has been granted access to view"
- Validation: "User must have at least one view/permission granted on accounts to see them in the list"
- Validation: "Account numbers may be masked based on the user's view permissions"
- Validation: "Balance information is only included if the user's view permits balance access"

---

### 5. AccountAccess

**Database Location:** `code/views/system/AccountAccess.scala`

**Description:** The Account Access entity that links users to bank accounts and views, establishing which accounts a user can access and with what permissions.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| user_fk | MappedLongForeignKey | Foreign key to ResourceUser |
| bank_id | MappedString(255) | Bank identifier |
| account_id | MappedString(255) | Account identifier |
| view_id | UUIDString | View identifier |
| consumer_id | MappedString(255) | Consumer identifier (default: ALL-CONSUMERS) |

**Relevance to User Story:**
- Access control: "Only accounts that the user has access to should be returned"
- Business rule: "Access Control: Only accounts that the user has access to should be returned - the system must enforce view/permission-based access control"
- Dependency: "User must have been granted view/permission access to accounts (View & Permission Management capabilities)"
- Dependency: "Account-user access relationships must be established"
- Implementation note: "View-Based Filtering: Account visibility is determined by the views/permissions granted to the user"

---

### 6. BankAccountRouting

**Database Location:** `code/model/dataAccess/BankAccountRouting.scala`

**Description:** The Account Routing entity that stores routing information (IBAN, account numbers) for bank accounts. Included in account listing responses to provide routing details.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| BankId | UUIDString | Bank identifier |
| AccountId | AccountIdString | Account identifier |
| AccountRoutingScheme | MappedString(32) | Routing scheme (e.g., IBAN, AccountNumber) |
| AccountRoutingAddress | MappedString(128) | Routing address value |

**Relevance to User Story:**
- Output data: "account_routings (List[AccountRouting]) - Account routing information (IBAN, etc.)"
- Response structure includes: scheme and address for each routing entry
- Example response shows: `"account_routings": [{"scheme": "IBAN", "address": "US12345678901234567890"}]`

---

### 7. MapperAccountHolders

**Database Location:** `code/accountholders/MapperAccountHolders.scala`

**Description:** The Account Holders entity that links users to bank accounts, establishing ownership relationships. Used in account listing to return owner information.

**Database Fields:**
| Field Name | Type | Description |
|------------|------|-------------|
| user | MappedLongForeignKey | Foreign key to ResourceUser (owner) |
| accountBankPermalink | UUIDString | Bank identifier |
| accountPermalink | AccountIdString | Account identifier |
| source | MappedString(255) | Source of the account holder relationship |

**Relevance to User Story:**
- Output data: "owners (List[AccountOwner]) - Account owner information"
- Response structure includes: user_id, provider, display_name for each owner
- Endpoint 3 (Get Accounts Held): "Retrieve accounts held at a specific bank, including accounts where the user has been granted access but may not be the owner"
- Technical context: Methods like `getAccountsHeld()` and `getAccountsHeldByUser()` are used for account listing

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
    |                         |-- kind (account type)

MappedBankAccount (1) ----< (N) BankAccountRouting
                    |              |
                    |              |-- AccountRoutingScheme
                    |              |-- AccountRoutingAddress
                    |
                    |----< (N) MapperAccountHolders
                    |              |
                    |              |-- user (FK to ResourceUser)
                    |
                    |----< (N) AccountAccess
                                   |
                                   |-- user_fk (FK to ResourceUser)
                                   |-- view_id (FK to ViewDefinition)

ResourceUser (1) ----< (N) AccountAccess
    |                         |
    |-- userId_               |-- user_fk
    |-- name_                 |-- view_id
    |-- email

ViewDefinition (1) ----< (N) AccountAccess
    |                         |
    |-- view_id               |-- view_id
    |-- name_                 |-- bank_id
    |-- isPublic_             |-- account_id
    |-- canSeeBankAccountBalance_
    |-- canSeeBankAccountNumber_
```

## Verification Summary

| Entity | Verified in Database | Relevant to User Story |
|--------|---------------------|------------------------|
| MappedBank | Yes | Yes - Referenced via BANK_ID parameter, bank scope for listing |
| MappedBankAccount | Yes | Yes - Primary entity being listed/retrieved |
| ResourceUser | Yes | Yes - User authentication and access control |
| ViewDefinition | Yes | Yes - View/permission management for account access |
| AccountAccess | Yes | Yes - Links users to accounts they can access |
| BankAccountRouting | Yes | Yes - Account routing information in responses |
| MapperAccountHolders | Yes | Yes - Account ownership information in responses |

## Notes

1. All entity names are exact matches from the Scala codebase database tables using Lift Mapper ORM
2. The MappedBankAccount entity is the primary entity being retrieved during account listing
3. AccountAccess establishes which accounts a user can view based on granted permissions
4. ViewDefinition controls what information is visible (balance, account number, etc.) based on view permissions
5. Balance amounts are stored in smallest currency units (cents, pence, etc.) and converted for display
6. Account listing supports multiple detail levels (minimal, basic, detailed) controlled by endpoint selection
7. The system enforces access control through the combination of AccountAccess and ViewDefinition entities
8. Account routing information (IBAN, account numbers) is stored separately in BankAccountRouting for flexibility
9. Owner information is retrieved through MapperAccountHolders which links users to accounts they own
