# Business Entity Extraction for Account Label Update

**Last Verified:** January 20, 2026 14:50 UTC  
**Verification Status:** All entities verified against Scala codebase database tables in OBP-API  
**Scala Codebase:** /home/ubuntu/repos/OBP-API-Cloned  
**Source User Story:** User_story_files_latest/Account Label Update/account_label_update_user_story.md

## Story Overview

**As a** Bank Account Holder / Account Owner  
**I want to** update the display label for a bank account  
**So that** I can personalize and organize my accounts with meaningful names that help me easily identify and distinguish between multiple accounts (e.g., "Savings for Vacation", "Monthly Bills", "Emergency Fund")

## Extracted Business Entities

The following business entities have been extracted from the user story and verified against the Scala codebase database tables.

### 1. MappedBankAccount (Primary Entity)

**Database Table:** `mappedbankaccount`  
**Source File:** `obp-api/src/main/scala/code/model/dataAccess/MappedBankAccount.scala`  
**Trait:** `BankAccount`

**Description:** The primary entity representing a bank account. Contains the `accountLabel` field which is the target of the update operation described in the user story. The label allows users to personalize account identification with meaningful names.

**Entity Fields (verified from database):**

| Field Name | Type | Description |
|------------|------|-------------|
| bank | UUIDString | Reference to the bank that owns the account |
| theAccountId | AccountIdString | Unique account identifier |
| accountCurrency | MappedString(10) | Account currency code |
| accountNumber | MappedAccountNumber | Account number |
| holder | MappedString(100) | Account holder name (deprecated) |
| accountBalance | MappedLong | Account balance in smallest currency unit |
| accountName | MappedString(255) | Account name |
| kind | MappedString(255) | Account type / financial product name |
| **accountLabel** | **MappedString(255)** | **Account label - the field being updated** |
| accountLastUpdate | MappedDateTime | Last update timestamp |
| mBranchId | UUIDString | Branch identifier |
| accountRuleScheme1 | MappedString(10) | First account rule scheme |
| accountRuleValue1 | MappedLong | First account rule value |
| accountRuleScheme2 | MappedString(10) | Second account rule scheme |
| accountRuleValue2 | MappedLong | Second account rule value |

**Database Indexes:**
- UniqueIndex(bank, theAccountId)

**Key Methods:**
- `label: String` - Returns the account label value from `accountLabel.get`

### 2. MappedBank (Referenced Entity)

**Database Table:** `mappedbank`  
**Source File:** `obp-api/src/main/scala/code/model/dataAccess/MappedBank.scala`  
**Trait:** `Bank`

**Description:** Entity representing a bank. Referenced by the account label update operation through the bankId path parameter. The bank must exist for the account label update to proceed.

**Entity Fields (verified from database):**

| Field Name | Type | Description |
|------------|------|-------------|
| permalink | MappedString(255) | Unique bank identifier (used as bankId in URLs) |
| fullBankName | MappedString(255) | Full name of the bank |
| shortBankName | MappedString(100) | Short name of the bank |
| logoURL | MappedString(255) | URL to bank logo |
| websiteURL | MappedString(255) | Bank website URL |
| swiftBIC | MappedString(255) | SWIFT/BIC code |
| national_identifier | MappedString(255) | National identifier |
| mBankRoutingScheme | MappedString(255) | Bank routing scheme |
| mBankRoutingAddress | MappedString(255) | Bank routing address |

**Database Indexes:**
- Index(permalink)

### 3. ViewDefinition (Authorization Entity)

**Database Table:** `viewdefinition`  
**Source File:** `obp-api/src/main/scala/code/views/system/ViewDefinition.scala`  
**Trait:** `View`

**Description:** Entity representing a view that controls access permissions to account data and operations. Contains specific permission fields for viewing and updating account labels. Users must have appropriate view permissions to update account labels.

**Entity Fields relevant to Account Label Update (verified from database):**

| Field Name | Type | Description |
|------------|------|-------------|
| id_ | MappedLongIndex | Primary key |
| name_ | MappedString(125) | View name |
| description_ | MappedString(255) | View description |
| bank_id | UUIDString | Reference to the bank |
| account_id | AccountIdString | Reference to the account |
| view_id | UUIDString | Unique view identifier |
| isSystem_ | MappedBoolean | Whether this is a system view |
| isPublic_ | MappedBoolean | Whether this view is public |
| **canSeeBankAccountLabel_** | **MappedBoolean** | **Permission to see account label** |
| **canUpdateBankAccountLabel_** | **MappedBoolean** | **Permission to update account label** |

**Database Indexes:**
- Index on isSystem_, isPublic_, isFirehose_

**Key Permission Fields for Label Update:**
- `canSeeBankAccountLabel_` - Controls visibility of the account label
- `canUpdateBankAccountLabel_` - Controls ability to update the account label

### 4. AccountAccess (Authorization Entity)

**Database Table:** `accountaccess`  
**Source File:** `obp-api/src/main/scala/code/views/system/AccountAccess.scala`

**Description:** Entity that links users to views on specific bank accounts. Determines which users have access to which accounts through which views. Required for authorization checks before allowing label updates.

**Entity Fields (verified from database):**

| Field Name | Type | Description |
|------------|------|-------------|
| user_fk | MappedLongForeignKey | Reference to ResourceUser |
| bank_id | MappedString(255) | Bank identifier |
| account_id | MappedString(255) | Account identifier |
| view_id | UUIDString | View identifier |
| consumer_id | MappedString(255) | Consumer identifier (default: ALL_CONSUMERS) |

**Database Indexes:**
- UniqueIndex(bank_id, account_id, view_id, user_fk, consumer_id)

### 5. ResourceUser (Authorization Entity)

**Database Table:** `resourceuser`  
**Source File:** `obp-api/src/main/scala/code/model/dataAccess/ResourceUser.scala`  
**Trait:** `User`

**Description:** Entity representing a user in the system. All accounts, transactions, roles, views, and permissions are linked to ResourceUser. Required for authentication and authorization when updating account labels.

**Entity Fields (verified from database):**

| Field Name | Type | Description |
|------------|------|-------------|
| id | MappedLongIndex | Primary key |
| userId_ | MappedUUID | Unique user identifier |
| email | MappedEmail(100) | User email address |
| name_ | MappedString(100) | User name |
| provider_ | MappedString(100) | Identity provider |
| providerId | MappedString(100) | Provider-specific user ID |
| Company | MappedString(50) | User's company |
| IsDeleted | MappedBoolean | Soft delete flag |

**Database Indexes:**
- UniqueIndex(provider_, providerId)

### 6. ViewPermission (Authorization Entity)

**Database Table:** `viewpermission`  
**Source File:** `obp-api/src/main/scala/code/views/system/ViewPermission.scala`

**Description:** Normalized table storing individual permissions for a view. Used to check if a user has the `can_update_bank_account_label` permission required to update account labels.

**Entity Fields (verified from database):**

| Field Name | Type | Description |
|------------|------|-------------|
| bank_id | MappedString(255) | Bank identifier |
| account_id | MappedString(255) | Account identifier |
| view_id | UUIDString | View identifier |
| permission | MappedString(255) | Permission name (e.g., "can_update_bank_account_label") |
| extraData | MappedString(1024) | Additional permission data |

**Database Indexes:**
- UniqueIndex(bank_id, account_id, view_id, permission)

## Entity Relationships

```
MappedBank (1) ----< (N) MappedBankAccount
    |                      |
    |                      |
    +-- bankId (FK) -------+
                           |
MappedBankAccount (1) ----< (N) AccountAccess
    |                              |
    +-- bank_id, account_id (FK) --+
                                   |
ViewDefinition (1) ----< (N) AccountAccess
    |                              |
    +-- view_id (FK) --------------+
                                   |
ResourceUser (1) ----< (N) AccountAccess
    |                              |
    +-- user_fk (FK) --------------+

ViewDefinition (1) ----< (N) ViewPermission
    |                              |
    +-- bank_id, account_id, ------+
        view_id (FK)
```

**Relationship Details:**
1. **MappedBank to MappedBankAccount:** One-to-Many relationship. A bank can have multiple accounts, but each account belongs to exactly one bank.
2. **MappedBankAccount to AccountAccess:** One-to-Many relationship. An account can have multiple access grants for different users and views.
3. **ViewDefinition to AccountAccess:** One-to-Many relationship. A view can be granted to multiple users on multiple accounts.
4. **ResourceUser to AccountAccess:** One-to-Many relationship. A user can have access to multiple accounts through different views.
5. **ViewDefinition to ViewPermission:** One-to-Many relationship. A view can have multiple individual permissions.

## Relevant Endpoints

Based on the user story, the following operations are supported:

| Operation | Endpoint | Primary Entity |
|-----------|----------|----------------|
| Update Account Label | POST /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/label | MappedBankAccount |
| Update Account (including label) | PUT /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID} | MappedBankAccount |

## Authorization Flow for Label Update

1. **User Authentication:** Verify the user is authenticated (ResourceUser exists and is valid)
2. **Account Existence:** Verify the bank (MappedBank) and account (MappedBankAccount) exist
3. **View Access Check:** Verify the user has access to the account through AccountAccess
4. **Permission Check:** Verify the view has `canUpdateBankAccountLabel_` permission set to true (via ViewDefinition or ViewPermission)
5. **Label Update:** Update the `accountLabel` field in MappedBankAccount

## Notes

1. **Label Field Location:** The account label is stored in the `accountLabel` field of `MappedBankAccount` entity with a maximum length of 255 characters.
2. **Permission Requirements:** Users must have a view with `canUpdateBankAccountLabel_` set to true to update the label.
3. **View-Based Access Control:** The OBP-API uses a view-based permission system where users are granted access to accounts through views, and each view defines what operations are allowed.
4. **Audit Trail:** Label changes should be logged for compliance purposes (handled by the application layer, not a separate entity).
5. **Validation:** Label validation (length, character restrictions) is handled at the application layer before persisting to MappedBankAccount.
