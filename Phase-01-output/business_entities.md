# Database Table Entities - Bank Registration (POST /banks)

**Extracted From:** Open Bank Project (OBP) API - Scala Application
**Migration Target:** Go Application
**User Story:** Bank Registration and Configuration (POST /banks only)
**Analysis Date:** 2025-11-24
**Scope:** Database table entities only (Mapped* classes under obp-api/src/main)

---

## Terminology

**In this document, "entity" means database table entities only** - Lift `Mapped*` or similar persistent classes under `obp-api/src/main/scala/code`. JSON case classes like `PostBankJson400` and `BankJson400` are DTOs (Data Transfer Objects), not database table entities, and are not documented here.

---

## Summary

This analysis identifies the database table entities involved in the POST /obp/v4.0.0/banks (createBank) endpoint based on the bank_registration_configuration_user_story.md file and the Scala codebase.

**Database Table Entities:**
- **MappedBank** - Core bank entity (primary)
- **MappedBankAccount** - Settlement account entity (side effect: 2 accounts auto-created)
- **MappedEntitlement** - Role assignment entity (side effect: 2 entitlements auto-assigned)

**Source Code References:**
- Bank creation: `/obp-api/src/main/scala/code/bankconnectors/LocalMappedConnector.scala` (lines 3169-3247)
- API endpoint: `/obp-api/src/main/scala/code/api/v4_0_0/APIMethods400.scala` (lines 3621-3683)

---

## ENTITY-001: MappedBank

**Entity Type**: Database Table (Core Entity)
**Business Domain**: Bank Management / Multi-Tenancy
**Description**: The core bank entity representing a banking institution. This is the primary database table for storing bank information including identification, branding, and routing details.

**Source**:
- Package: `code.model.dataAccess`
- Class: `MappedBank`
- File: `/obp-api/src/main/scala/code/model/dataAccess/MappedBank.scala`
- Trait Implemented: `com.openbankproject.commons.model.Bank`
- Extends: `LongKeyedMapper[MappedBank] with IdPK with CreatedUpdated`

**Database Table Structure**:

| Field Name | Scala Type | Database Type | Optional/Required | Description | Key Type |
|------------|------------|---------------|-------------------|-------------|----------|
| id | Long | BIGINT | Required | Database primary key (auto-increment) | Primary Key (DB) |
| permalink | MappedString(255) | VARCHAR(255) | Required | Unique bank identifier (bankId) | Unique Index |
| fullBankName | MappedString(255) | VARCHAR(255) | Required | Full legal name of the bank | |
| shortBankName | MappedString(100) | VARCHAR(100) | Required | Short display name of the bank | |
| logoURL | MappedString(255) | VARCHAR(255) | Required | URL to the bank's logo image | |
| websiteURL | MappedString(255) | VARCHAR(255) | Required | Bank's website URL | |
| swiftBIC | MappedString(255) | VARCHAR(255) | Required | SWIFT/BIC code for international transfers | |
| national_identifier | MappedString(255) | VARCHAR(255) | Required | National bank identifier | |
| mBankRoutingScheme | MappedString(255) | VARCHAR(255) | Required | Routing scheme type (e.g., OBP, BIC) | |
| mBankRoutingAddress | MappedString(255) | VARCHAR(255) | Required | Routing address for the specified scheme | |
| createdAt | Date | TIMESTAMP | Required | Record creation timestamp | |
| updatedAt | Date | TIMESTAMP | Required | Record last update timestamp | |

**Go Struct Mapping**:
```go
type MappedBank struct {
    ID                  int64     `json:"-" db:"id"`
    BankID              string    `json:"id" db:"permalink"`
    FullBankName        string    `json:"full_name" db:"full_bank_name"`
    ShortBankName       string    `json:"short_name" db:"short_bank_name"`
    LogoURL             string    `json:"logo" db:"logo_url"`
    WebsiteURL          string    `json:"website" db:"website_url"`
    SwiftBIC            string    `json:"-" db:"swift_bic"`
    NationalIdentifier  string    `json:"-" db:"national_identifier"`
    BankRoutingScheme   string    `json:"-" db:"m_bank_routing_scheme"`
    BankRoutingAddress  string    `json:"-" db:"m_bank_routing_address"`
    CreatedAt           time.Time `json:"-" db:"created_at"`
    UpdatedAt           time.Time `json:"-" db:"updated_at"`
}
```

**Type Mapping Notes**:
- Scala `Long` → Go `int64`
- Scala `MappedString(n)` → Go `string`
- Scala `Date` → Go `time.Time`
- Database column names use snake_case
- `permalink` field is used as the business key (bankId)

**Creation Logic** (from LocalMappedConnector.scala lines 3198-3210):
```scala
MappedBank.create
  .permalink(bankId)
  .fullBankName(fullBankName)
  .shortBankName(shortBankName)
  .logoURL(logoURL)
  .websiteURL(websiteURL)
  .swiftBIC(swiftBIC)
  .national_identifier(national_identifier)
  .mBankRoutingScheme(bankRoutingScheme)
  .mBankRoutingAddress(bankRoutingAddress)
  .saveMe()
```

**Validation Rules** (from user story and APIMethods400.scala):
- BANK_ID (permalink) must be greater than 3 characters
- BANK_ID cannot contain space characters
- BANK_ID cannot contain `::::` characters
- BANK_ID must pass short string validation

**Relationships**:
- **Has Many**: MappedBankAccount (settlement accounts)
- **Has Many**: MappedEntitlement (role assignments)
- **Side Effects on Creation**:
  - 2 MappedBankAccount records created (incoming/outgoing settlement accounts)
  - 2 MappedEntitlement records created (CanCreateEntitlementAtOneBank, CanReadDynamicResourceDocsAtOneBank)

**Migration Considerations**:
- Implement all validation rules before persisting
- Use transactions to ensure atomicity with side effects
- Create settlement accounts and entitlements in same transaction
- Handle bank routing extraction (BIC vs non-BIC schemes)
- Preserve unique index on permalink field

---

## ENTITY-002: MappedBankAccount

**Entity Type**: Database Table (Side Effect Entity)
**Business Domain**: Bank Account Management / Settlement Accounts
**Description**: Bank account entity used for settlement accounts. Two settlement accounts are automatically created when a new bank is registered: one for incoming settlements and one for outgoing settlements, both in EUR currency.

**Source**:
- Package: `code.model.dataAccess`
- Class: `MappedBankAccount`
- File: `/obp-api/src/main/scala/code/model/dataAccess/MappedBankAccount.scala`
- Extends: `LongKeyedMapper[MappedBankAccount] with IdPK with CreatedUpdated`

**Database Table Structure** (relevant fields for settlement accounts):

| Field Name | Scala Type | Database Type | Optional/Required | Description | Key Type |
|------------|------------|---------------|-------------------|-------------|----------|
| id | Long | BIGINT | Required | Database primary key (auto-increment) | Primary Key (DB) |
| bank | MappedString | VARCHAR | Required | Bank ID (foreign key to MappedBank.permalink) | Foreign Key |
| theAccountId | MappedString | VARCHAR | Required | Account identifier | |
| accountCurrency | MappedString | VARCHAR | Required | Account currency code (EUR for settlement accounts) | |
| kind | MappedString | VARCHAR | Required | Account type (SETTLEMENT for settlement accounts) | |
| holder | MappedString | VARCHAR | Required | Account holder name (bank's full name) | |
| accountName | MappedString | VARCHAR | Required | Account name/description | |
| accountLabel | MappedString | VARCHAR | Required | Account label | |
| createdAt | Date | TIMESTAMP | Required | Record creation timestamp | |
| updatedAt | Date | TIMESTAMP | Required | Record last update timestamp | |

**Go Struct Mapping**:
```go
type MappedBankAccount struct {
    ID              int64     `json:"-" db:"id"`
    BankID          string    `json:"bank_id" db:"bank"`
    AccountID       string    `json:"account_id" db:"the_account_id"`
    AccountCurrency string    `json:"currency" db:"account_currency"`
    Kind            string    `json:"kind" db:"kind"`
    Holder          string    `json:"holder" db:"holder"`
    AccountName     string    `json:"name" db:"account_name"`
    AccountLabel    string    `json:"label" db:"account_label"`
    CreatedAt       time.Time `json:"created_at" db:"created_at"`
    UpdatedAt       time.Time `json:"updated_at" db:"updated_at"`
}
```

**Auto-Creation Logic** (from LocalMappedConnector.scala lines 3214-3244):

**Incoming Settlement Account** (lines 3218-3226):
```scala
MappedBankAccount.create
  .bank(bankId)
  .theAccountId(INCOMING_SETTLEMENT_ACCOUNT_ID)  // "OBP_DEFAULT_INCOMING_ACCOUNT_ID"
  .accountCurrency("EUR")
  .kind("SETTLEMENT")
  .holder(fullBankName)
  .accountName("Default incoming settlement account")
  .accountLabel("Settlement account: Do not delete!")
  .saveMe()
```

**Outgoing Settlement Account** (lines 3234-3242):
```scala
MappedBankAccount.create
  .bank(bankId)
  .theAccountId(OUTGOING_SETTLEMENT_ACCOUNT_ID)  // "OBP_DEFAULT_OUTGOING_ACCOUNT_ID"
  .accountCurrency("EUR")
  .kind("SETTLEMENT")
  .holder(fullBankName)
  .accountName("Default outgoing settlement account")
  .accountLabel("Settlement account: Do not delete!")
  .saveMe()
```

**Constants**:
- `INCOMING_SETTLEMENT_ACCOUNT_ID` = "OBP_DEFAULT_INCOMING_ACCOUNT_ID"
- `OUTGOING_SETTLEMENT_ACCOUNT_ID` = "OBP_DEFAULT_OUTGOING_ACCOUNT_ID"

**Relationships**:
- **Belongs To**: MappedBank (via bank field)
- **Created By**: createOrUpdateBank function (2 accounts per bank)

**Migration Considerations**:
- Create both settlement accounts in same transaction as bank creation
- Use exact account IDs: "OBP_DEFAULT_INCOMING_ACCOUNT_ID" and "OBP_DEFAULT_OUTGOING_ACCOUNT_ID"
- Always use EUR currency for settlement accounts
- Set kind to "SETTLEMENT"
- Use bank's full name as holder
- Check for existence before creating (idempotent operation)

---

## ENTITY-003: MappedEntitlement

**Entity Type**: Database Table (Side Effect Entity)
**Business Domain**: Authorization / Role Management
**Description**: Role assignment entity that grants permissions to users. Two entitlements are automatically assigned to the user who creates a new bank, allowing them to manage the bank and assign roles to other users.

**Source**:
- Package: `code.entitlement`
- Class: `MappedEntitlement`
- File: `/obp-api/src/main/scala/code/entitlement/MappedEntitlements.scala`
- Trait Implemented: `Entitlement`
- Extends: `LongKeyedMapper[MappedEntitlement] with IdPK with CreatedUpdated`

**Database Table Structure**:

| Field Name | Scala Type | Database Type | Optional/Required | Description | Key Type |
|------------|------------|---------------|-------------------|-------------|----------|
| id | Long | BIGINT | Required | Database primary key (auto-increment) | Primary Key (DB) |
| mEntitlementId | MappedUUID | UUID | Required | Unique entitlement identifier | Unique Index |
| mBankId | UUIDString | VARCHAR | Required | Bank ID (foreign key to MappedBank.permalink) | Foreign Key |
| mUserId | UUIDString | VARCHAR | Required | User ID (foreign key to ResourceUser.userId_) | Foreign Key |
| mRoleName | MappedString(64) | VARCHAR(64) | Required | Role name | |
| mCreatedByProcess | MappedString(255) | VARCHAR(255) | Required | Process that created the entitlement | |
| createdAt | Date | TIMESTAMP | Required | Record creation timestamp | |
| updatedAt | Date | TIMESTAMP | Required | Record last update timestamp | |

**Go Struct Mapping**:
```go
type MappedEntitlement struct {
    ID               int64     `json:"-" db:"id"`
    EntitlementID    string    `json:"entitlement_id" db:"m_entitlement_id"`
    BankID           string    `json:"bank_id" db:"m_bank_id"`
    UserID           string    `json:"user_id" db:"m_user_id"`
    RoleName         string    `json:"role_name" db:"m_role_name"`
    CreatedByProcess string    `json:"created_by_process" db:"m_created_by_process"`
    CreatedAt        time.Time `json:"created_at" db:"created_at"`
    UpdatedAt        time.Time `json:"updated_at" db:"updated_at"`
}
```

**Auto-Assignment Logic** (from APIMethods400.scala lines 3662-3677):

**Role 1: CanCreateEntitlementAtOneBank** (lines 3664-3669):
```scala
entitlementsByBank.filter(_.roleName == CanCreateEntitlementAtOneBank.toString()).size > 0 match {
  case true =>
    // Already has entitlement
    Future()
  case false =>
    Future(Entitlement.entitlement.vend.addEntitlement(bank.id, cc.userId, CanCreateEntitlementAtOneBank.toString()))
}
```

**Role 2: CanReadDynamicResourceDocsAtOneBank** (lines 3671-3676):
```scala
entitlementsByBank.filter(_.roleName == CanReadDynamicResourceDocsAtOneBank.toString()).size > 0 match {
  case true =>
    // Already has entitlement
    Future()
  case false =>
    Future(Entitlement.entitlement.vend.addEntitlement(bank.id, cc.userId, CanReadDynamicResourceDocsAtOneBank.toString()))
}
```

**Creation Logic** (from MappedEntitlements.scala lines 110-112):
```scala
MappedEntitlement.create
  .mBankId(bankId)
  .mUserId(userId)
  .mRoleName(roleName)
  .mCreatedByProcess(createdByProcess)
  .saveMe()
```

**Roles Assigned on Bank Creation**:
1. **CanCreateEntitlementAtOneBank** - Allows the user to manage entitlements for the bank they created
2. **CanReadDynamicResourceDocsAtOneBank** - Allows the user to read dynamic resource documentation for the bank

**Relationships**:
- **Belongs To**: MappedBank (via mBankId field)
- **Belongs To**: ResourceUser (via mUserId field)
- **Created By**: createBank API endpoint (2 entitlements per bank creation)

**Migration Considerations**:
- Create both entitlements in same transaction as bank creation
- Check for existence before creating (idempotent operation)
- Use exact role names: "CanCreateEntitlementAtOneBank" and "CanReadDynamicResourceDocsAtOneBank"
- Set createdByProcess to appropriate value (e.g., "createBank")
- Generate unique UUID for mEntitlementId
- Send notification email to user when entitlement is granted

---

## API Endpoint

### POST /obp/v4.0.0/banks

**Endpoint Name**: createBank
**Description**: Create a new bank with automatic settlement account and entitlement provisioning.

**Request**:
- Method: POST
- Path: /obp/v4.0.0/banks
- Body: PostBankJson400 (JSON DTO - not a database entity)
- Authentication: Required (OAuth)
- Authorization: Requires `canCreateBank` entitlement

**Response**:
- Status: HTTP 201 Created
- Body: BankJson400 (JSON DTO - not a database entity)

**Database Operations** (in order):
1. Create or update MappedBank record
2. Create 2 MappedBankAccount records (settlement accounts)
3. Create 2 MappedEntitlement records (role assignments)

**Transaction Management**:
All database operations must be performed in a single transaction to ensure atomicity. If any operation fails, all changes must be rolled back.

---

## Go Migration Guidelines

### Database Schema

**MappedBank Table**:
```sql
CREATE TABLE mapped_bank (
    id BIGSERIAL PRIMARY KEY,
    permalink VARCHAR(255) NOT NULL UNIQUE,
    full_bank_name VARCHAR(255) NOT NULL,
    short_bank_name VARCHAR(100) NOT NULL,
    logo_url VARCHAR(255),
    website_url VARCHAR(255),
    swift_bic VARCHAR(255),
    national_identifier VARCHAR(255),
    m_bank_routing_scheme VARCHAR(255),
    m_bank_routing_address VARCHAR(255),
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_mapped_bank_permalink ON mapped_bank(permalink);
```

**MappedBankAccount Table**:
```sql
CREATE TABLE mapped_bank_account (
    id BIGSERIAL PRIMARY KEY,
    bank VARCHAR(255) NOT NULL REFERENCES mapped_bank(permalink),
    the_account_id VARCHAR(255) NOT NULL,
    account_currency VARCHAR(3) NOT NULL,
    kind VARCHAR(50) NOT NULL,
    holder VARCHAR(255),
    account_name VARCHAR(255),
    account_label VARCHAR(255),
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(bank, the_account_id)
);
```

**MappedEntitlement Table**:
```sql
CREATE TABLE mapped_entitlement (
    id BIGSERIAL PRIMARY KEY,
    m_entitlement_id UUID NOT NULL UNIQUE,
    m_bank_id VARCHAR(255) NOT NULL,
    m_user_id VARCHAR(255) NOT NULL,
    m_role_name VARCHAR(64) NOT NULL,
    m_created_by_process VARCHAR(255) DEFAULT 'manual',
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(m_bank_id, m_user_id, m_role_name)
);
```

### Implementation Example

```go
func (s *BankService) CreateBank(ctx context.Context, req *PostBankJson400, userID string) (*BankJson400, error) {
    // Validate bank ID
    if err := ValidateBankID(req.ID); err != nil {
        return nil, err
    }
    
    // Extract routing information
    swiftBIC, bankRoutingScheme, bankRoutingAddress := extractRoutingInfo(req.BankRoutings)
    
    // Start transaction
    tx, err := s.db.BeginTx(ctx, nil)
    if err != nil {
        return nil, err
    }
    defer tx.Rollback()
    
    // 1. Create MappedBank
    bank := &MappedBank{
        BankID:             req.ID,
        FullBankName:       req.FullName,
        ShortBankName:      req.ShortName,
        LogoURL:            req.Logo,
        WebsiteURL:         req.Website,
        SwiftBIC:           swiftBIC,
        BankRoutingScheme:  bankRoutingScheme,
        BankRoutingAddress: bankRoutingAddress,
        CreatedAt:          time.Now(),
        UpdatedAt:          time.Now(),
    }
    
    if err := s.bankRepo.CreateWithTx(tx, bank); err != nil {
        return nil, err
    }
    
    // 2. Create settlement accounts
    incomingAccount := &MappedBankAccount{
        BankID:          req.ID,
        AccountID:       "OBP_DEFAULT_INCOMING_ACCOUNT_ID",
        AccountCurrency: "EUR",
        Kind:            "SETTLEMENT",
        Holder:          req.FullName,
        AccountName:     "Default incoming settlement account",
        AccountLabel:    "Settlement account: Do not delete!",
        CreatedAt:       time.Now(),
        UpdatedAt:       time.Now(),
    }
    
    outgoingAccount := &MappedBankAccount{
        BankID:          req.ID,
        AccountID:       "OBP_DEFAULT_OUTGOING_ACCOUNT_ID",
        AccountCurrency: "EUR",
        Kind:            "SETTLEMENT",
        Holder:          req.FullName,
        AccountName:     "Default outgoing settlement account",
        AccountLabel:    "Settlement account: Do not delete!",
        CreatedAt:       time.Now(),
        UpdatedAt:       time.Now(),
    }
    
    if err := s.accountRepo.CreateWithTx(tx, incomingAccount); err != nil {
        return nil, err
    }
    
    if err := s.accountRepo.CreateWithTx(tx, outgoingAccount); err != nil {
        return nil, err
    }
    
    // 3. Grant entitlements
    entitlement1 := &MappedEntitlement{
        EntitlementID:    uuid.New().String(),
        BankID:           req.ID,
        UserID:           userID,
        RoleName:         "CanCreateEntitlementAtOneBank",
        CreatedByProcess: "createBank",
        CreatedAt:        time.Now(),
        UpdatedAt:        time.Now(),
    }
    
    entitlement2 := &MappedEntitlement{
        EntitlementID:    uuid.New().String(),
        BankID:           req.ID,
        UserID:           userID,
        RoleName:         "CanReadDynamicResourceDocsAtOneBank",
        CreatedByProcess: "createBank",
        CreatedAt:        time.Now(),
        UpdatedAt:        time.Now(),
    }
    
    if err := s.entitlementRepo.CreateWithTx(tx, entitlement1); err != nil {
        return nil, err
    }
    
    if err := s.entitlementRepo.CreateWithTx(tx, entitlement2); err != nil {
        return nil, err
    }
    
    // Commit transaction
    if err := tx.Commit(); err != nil {
        return nil, err
    }
    
    // Build response
    return &BankJson400{
        ID:           req.ID,
        FullName:     req.FullName,
        ShortName:    req.ShortName,
        Logo:         req.Logo,
        Website:      req.Website,
        BankRoutings: req.BankRoutings,
        Attributes:   []interface{}{},
    }, nil
}
```

---

## Critical Success Factors

1. **Transaction Atomicity**: All three entity creations (MappedBank, 2x MappedBankAccount, 2x MappedEntitlement) must succeed or all must rollback
2. **Validation**: Bank ID validation rules must be implemented identically
3. **Settlement Accounts**: Exactly 2 accounts with exact IDs and EUR currency
4. **Entitlements**: Exactly 2 roles with exact role names
5. **Idempotency**: Check for existence before creating settlement accounts and entitlements
6. **Foreign Keys**: Maintain referential integrity between entities

---

## Type Conversion Summary

| Scala Type | Go Type | Notes |
|------------|---------|-------|
| Long | int64 | Database primary key |
| MappedString(n) | string | With DB constraints |
| MappedUUID | string (UUID) | Generate with uuid.New() |
| UUIDString | string | UUID as string |
| Date | time.Time | Timestamps |

---

## Notes

- This document includes **only database table entities** (Mapped* classes)
- JSON DTOs (PostBankJson400, BankJson400, BankRoutingJsonV121) are not database entities and are not documented here
- All entities are from `/obp-api/src/main/scala/code` package
- Settlement accounts are always created in EUR currency
- SANDBOX mode required (connector=mapped in properties file)
