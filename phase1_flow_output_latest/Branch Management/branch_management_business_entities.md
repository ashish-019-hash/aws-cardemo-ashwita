# Business Entity Extraction for Branch Management

## Overview

This document contains the business entities extracted from the Branch Management user story. All entities have been verified against the actual database tables in the Scala codebase (OBP-API) to ensure only existing and relevant entities are included.

## Extracted Business Entities

### 1. MappedBranch

**Database Table Name:** `MappedBranch`

**Description:** The primary entity representing a bank branch with complete location, service, and accessibility information. This is the core entity for the Branch Management capability.

**Source File:** `obp-api/src/main/scala/code/branches/MappedBranchesProvider.scala`

**Database Fields:**
| Field Name | Data Type | Description |
|------------|-----------|-------------|
| mBankId | UUIDString | Foreign key reference to the bank |
| mBranchId | UUIDString | Unique identifier for the branch |
| mName | MappedString(255) | Branch name |
| mLine1 | MappedString(255) | Address line 1 |
| mLine2 | MappedString(255) | Address line 2 |
| mLine3 | MappedString(255) | Address line 3 |
| mCity | MappedString(255) | City |
| mCounty | MappedString(255) | County |
| mState | MappedString(255) | State |
| mCountryCode | MappedString(2) | ISO country code |
| mPostCode | MappedString(20) | Postal code |
| mlocationLatitude | MappedDouble | Geographic latitude |
| mlocationLongitude | MappedDouble | Geographic longitude |
| mLicenseId | UUIDString | License identifier |
| mLicenseName | MappedString(255) | License name |
| mLobbyHours | MappedString(2000) | Lobby hours (deprecated string format) |
| mDriveUpHours | MappedString(2000) | Drive-up hours (deprecated string format) |
| mBranchRoutingScheme | MappedString(32) | Branch routing scheme |
| mBranchRoutingAddress | MappedString(64) | Branch routing address |
| mLobbyOpeningTimeOnMonday | TwentyFourHourClockString | Monday lobby opening time |
| mLobbyClosingTimeOnMonday | TwentyFourHourClockString | Monday lobby closing time |
| mLobbyOpeningTimeOnTuesday | TwentyFourHourClockString | Tuesday lobby opening time |
| mLobbyClosingTimeOnTuesday | TwentyFourHourClockString | Tuesday lobby closing time |
| mLobbyOpeningTimeOnWednesday | TwentyFourHourClockString | Wednesday lobby opening time |
| mLobbyClosingTimeOnWednesday | TwentyFourHourClockString | Wednesday lobby closing time |
| mLobbyOpeningTimeOnThursday | TwentyFourHourClockString | Thursday lobby opening time |
| mLobbyClosingTimeOnThursday | TwentyFourHourClockString | Thursday lobby closing time |
| mLobbyOpeningTimeOnFriday | TwentyFourHourClockString | Friday lobby opening time |
| mLobbyClosingTimeOnFriday | TwentyFourHourClockString | Friday lobby closing time |
| mLobbyOpeningTimeOnSaturday | TwentyFourHourClockString | Saturday lobby opening time |
| mLobbyClosingTimeOnSaturday | TwentyFourHourClockString | Saturday lobby closing time |
| mLobbyOpeningTimeOnSunday | TwentyFourHourClockString | Sunday lobby opening time |
| mLobbyClosingTimeOnSunday | TwentyFourHourClockString | Sunday lobby closing time |
| mDriveUpOpeningTimeOnMonday | TwentyFourHourClockString | Monday drive-up opening time |
| mDriveUpClosingTimeOnMonday | TwentyFourHourClockString | Monday drive-up closing time |
| mDriveUpOpeningTimeOnTuesday | TwentyFourHourClockString | Tuesday drive-up opening time |
| mDriveUpClosingTimeOnTuesday | TwentyFourHourClockString | Tuesday drive-up closing time |
| mDriveUpOpeningTimeOnWednesday | TwentyFourHourClockString | Wednesday drive-up opening time |
| mDriveUpClosingTimeOnWednesday | TwentyFourHourClockString | Wednesday drive-up closing time |
| mDriveUpOpeningTimeOnThursday | TwentyFourHourClockString | Thursday drive-up opening time |
| mDriveUpClosingTimeOnThursday | TwentyFourHourClockString | Thursday drive-up closing time |
| mDriveUpOpeningTimeOnFriday | TwentyFourHourClockString | Friday drive-up opening time |
| mDriveUpClosingTimeOnFriday | TwentyFourHourClockString | Friday drive-up closing time |
| mDriveUpOpeningTimeOnSaturday | TwentyFourHourClockString | Saturday drive-up opening time |
| mDriveUpClosingTimeOnSaturday | TwentyFourHourClockString | Saturday drive-up closing time |
| mDriveUpOpeningTimeOnSunday | TwentyFourHourClockString | Sunday drive-up opening time |
| mDriveUpClosingTimeOnSunday | TwentyFourHourClockString | Sunday drive-up closing time |
| mIsAccessible | MappedString(1) | Accessibility indicator (Y/N/empty) |
| mAccessibleFeatures | MappedString(250) | Accessible features description |
| mBranchType | MappedString(32) | Type of branch |
| mMoreInfo | MappedString(128) | Additional information |
| mPhoneNumber | MappedString(32) | Branch phone number |
| mIsDeleted | MappedBoolean | Soft delete flag |

**Database Indexes:**
- UniqueIndex(mBankId, mBranchId)
- Index(mBankId)

**Relationships:**
- References MappedBank via mBankId field

**Relevance to User Story:**
- Primary entity for all Branch Management CRUD operations
- Supports create, update, retrieve, and delete (soft delete) operations
- Contains all branch attributes mentioned in the user story including address, location, lobby hours, drive-up hours, accessibility features, and branch routing

---

### 2. MappedBank

**Database Table Name:** `MappedBank`

**Description:** The bank entity that branches belong to. A branch must be associated with an existing bank. This is a dependency entity for Branch Management.

**Source File:** `obp-api/src/main/scala/code/model/dataAccess/MappedBank.scala`

**Database Fields:**
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

**Database Indexes:**
- Index(permalink)

**Relationships:**
- One-to-Many relationship with MappedBranch (a bank can have multiple branches)

**Relevance to User Story:**
- Referenced in user story acceptance criteria: "A branch can only be created for an existing bank"
- Bank ID is required for all branch operations (create, update, retrieve, delete)
- Bank existence validation is performed before branch creation

---

### 3. MappedEntitlement

**Database Table Name:** `MappedEntitlement`

**Description:** The entitlement/permission entity that controls access to branch operations. Used for authorization checks on branch create, update, and delete operations.

**Source File:** `obp-api/src/main/scala/code/entitlement/MappedEntitlements.scala`

**Database Fields:**
| Field Name | Data Type | Description |
|------------|-----------|-------------|
| mEntitlementId | MappedUUID | Unique identifier for the entitlement |
| mBankId | UUIDString | Bank ID for bank-specific entitlements |
| mUserId | UUIDString | User ID who has the entitlement |
| mRoleName | MappedString(64) | Name of the role/permission |
| mCreatedByProcess | MappedString(255) | Process that created the entitlement |

**Database Indexes:**
- UniqueIndex(mEntitlementId)

**Relationships:**
- References users via mUserId
- References banks via mBankId for bank-specific entitlements

**Relevance to User Story:**
- User story specifies entitlement requirements for branch operations:
  - CanCreateBranch / CanCreateBranchAtAnyBank for branch creation
  - CanUpdateBranch for branch updates
  - CanDeleteBranch / CanDeleteBranchAtAnyBank for branch deletion
- All write operations (create, update, delete) require appropriate entitlements

---

## Entity Relationship Summary

```
MappedBank (1) -----> (*) MappedBranch
    |
    |--- permalink (Bank ID)
    |
    v
MappedEntitlement
    |
    |--- mBankId (references Bank for bank-specific permissions)
    |--- mRoleName (CanCreateBranch, CanUpdateBranch, CanDeleteBranch, etc.)
```

## Verification Notes

All entities listed above have been verified against:

1. **Database Schema Registration:** Confirmed in `bootstrap/liftweb/Boot.scala` under `ToSchemify.models` list
2. **ORM Mapping:** Each entity extends `LongKeyedMapper` with corresponding `LongKeyedMetaMapper` companion object
3. **User Story Relevance:** Each entity is directly referenced or required by the Branch Management capability as described in the user story

## Entities NOT Included (Embedded/Value Objects)

The following are NOT separate database tables but are embedded within MappedBranch or are value objects:

- **Address** - Embedded in MappedBranch (mLine1, mLine2, mLine3, mCity, mCounty, mState, mCountryCode, mPostCode)
- **Location** - Embedded in MappedBranch (mlocationLatitude, mlocationLongitude)
- **Lobby** - Embedded in MappedBranch (mLobbyOpeningTime*, mLobbyClosingTime*)
- **DriveUp** - Embedded in MappedBranch (mDriveUpOpeningTime*, mDriveUpClosingTime*)
- **License/Meta** - Embedded in MappedBranch (mLicenseId, mLicenseName)
- **Routing** - Embedded in MappedBranch (mBranchRoutingScheme, mBranchRoutingAddress)
- **LobbyString** - Deprecated string representation of lobby hours
- **DriveUpString** - Deprecated string representation of drive-up hours
