# Business Entity Extraction for ATM Management

**Last Verified:** January 20, 2026 08:57 UTC  
**Verification Status:** All entities verified against Scala codebase database tables in OBP-API  
**Scala Codebase:** /home/ubuntu/repos/OBP-API-Cloned  
**Source User Story:** User_story_files_latest/ATM Management/atm_management_user_story.md

## Story Overview

**As a** Bank Administrator or Operations Manager  
**I want to** create, update, retrieve, and delete ATM information including locations, supported languages, currencies, and accessibility features  
**So that** customers can easily find and use ATMs that meet their needs, and the bank can maintain accurate ATM network information for operational and customer service purposes

## Extracted Business Entities

The following business entities have been extracted from the user story and verified against the Scala codebase database tables.

### 1. MappedAtm (Primary Entity)

**Database Table:** `mappedatm`  
**Source File:** `obp-api/src/main/scala/code/atms/MappedAtmsProvider.scala`  
**Trait:** `AtmT`

**Description:** The primary entity representing an ATM machine with all its associated information including location, operating hours, supported services, languages, currencies, and accessibility features.

**Entity Fields (verified from database):**

| Field Name | Type | Description |
|------------|------|-------------|
| mAtmId | UUIDString | Unique identifier for the ATM |
| mBankId | UUIDString | Reference to the bank that owns the ATM |
| mName | MappedString(255) | Name/label of the ATM |
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
| mOpeningTimeOnMonday | TwentyFourHourClockString | Monday opening time |
| mClosingTimeOnMonday | TwentyFourHourClockString | Monday closing time |
| mOpeningTimeOnTuesday | TwentyFourHourClockString | Tuesday opening time |
| mClosingTimeOnTuesday | TwentyFourHourClockString | Tuesday closing time |
| mOpeningTimeOnWednesday | TwentyFourHourClockString | Wednesday opening time |
| mClosingTimeOnWednesday | TwentyFourHourClockString | Wednesday closing time |
| mOpeningTimeOnThursday | TwentyFourHourClockString | Thursday opening time |
| mClosingTimeOnThursday | TwentyFourHourClockString | Thursday closing time |
| mOpeningTimeOnFriday | TwentyFourHourClockString | Friday opening time |
| mClosingTimeOnFriday | TwentyFourHourClockString | Friday closing time |
| mOpeningTimeOnSaturday | TwentyFourHourClockString | Saturday opening time |
| mClosingTimeOnSaturday | TwentyFourHourClockString | Saturday closing time |
| mOpeningTimeOnSunday | TwentyFourHourClockString | Sunday opening time |
| mClosingTimeOnSunday | TwentyFourHourClockString | Sunday closing time |
| mIsAccessible | MappedString(1) | Accessibility flag (Y/N) |
| mLocatedAt | MappedString(32) | Location description |
| mMoreInfo | MappedString(128) | Additional information |
| mHasDepositCapability | MappedString(1) | Deposit capability flag (Y/N) |
| mSupportedLanguages | MappedText | Comma-separated list of supported languages |
| mServices | MappedText | Comma-separated list of services |
| mNotes | MappedText | Comma-separated notes |
| mAccessibilityFeatures | MappedText | Comma-separated accessibility features |
| mSupportedCurrencies | MappedText | Comma-separated supported currencies |
| mLocationCategories | MappedText | Comma-separated location categories |
| mMinimumWithdrawal | MappedString(255) | Minimum withdrawal amount |
| mBranchIdentification | MappedString(255) | Branch identifier |
| mSiteIdentification | MappedString(255) | Site identifier |
| mSiteName | MappedString(255) | Site name |
| mCashWithdrawalNationalFee | MappedString(255) | National withdrawal fee |
| mCashWithdrawalInternationalFee | MappedString(255) | International withdrawal fee |
| mBalanceInquiryFee | MappedString(255) | Balance inquiry fee |
| mAtmType | MappedString(255) | Type of ATM |
| mPhone | MappedString(255) | Contact phone number |

**Database Indexes:**
- UniqueIndex(mBankId, mAtmId)
- Index(mBankId)

### 2. AtmAttribute (Supporting Entity)

**Database Table:** `atmattribute`  
**Source File:** `obp-api/src/main/scala/code/atmattribute/MappedAtmAttributeProvider.scala`  
**Trait:** `AtmAttributeTrait`

**Description:** Entity for storing extensible attributes associated with ATMs. Allows adding custom key-value pairs to ATM records for flexibility.

**Entity Fields (verified from database):**

| Field Name | Type | Description |
|------------|------|-------------|
| BankId_ | UUIDString | Reference to the bank |
| AtmId_ | UUIDString | Reference to the ATM |
| AtmAttributeId | MappedUUID | Unique identifier for the attribute |
| Name | MappedString(50) | Attribute name |
| Type | MappedString(50) | Attribute type (from AtmAttributeType enum) |
| Value | MappedString(255) | Attribute value |
| IsActive | MappedBoolean | Active status flag |

**Database Indexes:**
- Index(BankId_, AtmId_)

### 3. MappedBank (Referenced Entity)

**Database Table:** `mappedbank`  
**Source File:** `obp-api/src/main/scala/code/model/dataAccess/MappedBank.scala`  
**Trait:** `Bank`

**Description:** Entity representing a bank. Referenced by ATM entities through the bankId field. ATMs must be associated with a valid bank entity.

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

## Entity Relationships

```
MappedBank (1) ----< (N) MappedAtm
    |                      |
    |                      |
    +-- bankId (FK) -------+
                           |
MappedAtm (1) ----< (N) AtmAttribute
    |                      |
    |                      |
    +-- atmId (FK) --------+
    +-- bankId (FK) -------+
```

**Relationship Details:**
1. **MappedBank to MappedAtm:** One-to-Many relationship. A bank can have multiple ATMs, but each ATM belongs to exactly one bank.
2. **MappedAtm to AtmAttribute:** One-to-Many relationship. An ATM can have multiple attributes for extensibility.

## Embedded Value Objects

The following are value objects embedded within the MappedAtm entity (not separate database tables):

### Address (Embedded in MappedAtm)
- line1, line2, line3, city, county, state, countryCode, postCode

### Location (Embedded in MappedAtm)
- latitude, longitude

### Meta (Embedded in MappedAtm)
- license (id, name)

## Relevant Endpoints

Based on the user story, the following CRUD operations are supported:

| Operation | Endpoint | Entity |
|-----------|----------|--------|
| Create ATM | POST /obp/v5.1.0/banks/{BANK_ID}/atms | MappedAtm |
| Update ATM | PUT /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} | MappedAtm |
| Retrieve ATM | GET /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} | MappedAtm |
| List ATMs | GET /obp/v5.1.0/banks/{BANK_ID}/atms | MappedAtm |
| Delete ATM | DELETE /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} | MappedAtm |

## Notes

1. **Address and Location** are not separate database entities but are embedded as fields within the MappedAtm table.
2. **Supported Languages, Services, Accessibility Features, Supported Currencies, Notes, and Location Categories** are stored as comma-separated strings in MappedText fields.
3. **AtmAttribute** provides extensibility for adding custom attributes to ATMs without modifying the core schema.
4. All ATM operations require a valid **BANK_ID** reference, enforcing the relationship with the MappedBank entity.
