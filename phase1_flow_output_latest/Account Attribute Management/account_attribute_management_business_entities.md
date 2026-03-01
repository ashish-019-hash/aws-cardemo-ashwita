# Business Entity Extraction for Account Attribute Management

**Last Verified:** January 20, 2026 14:16 UTC  
**Verification Status:** All entities verified against Scala codebase database tables in OBP-API  
**Scala Codebase:** /home/ubuntu/repos/OBP-API-Cloned  
**Source User Story:** User_story_files_latest/Account Attribute Management/account_attribute_management_user_story.md

## Story Overview

**As a** Bank Administrator or API Consumer  
**I want to** manage custom attributes associated with accounts  
**So that** I can extend account metadata with additional typed key-value pairs for business-specific requirements such as ISIN codes, loan identifiers, maturity dates, and other financial product attributes

## Extracted Business Entities

The following business entities have been extracted from the user story and verified against the Scala codebase database tables.

### 1. MappedAccountAttribute (Primary Entity)

**Database Table:** `mappedaccountattribute`  
**Source File:** `obp-api/src/main/scala/code/accountattribute/MappedAccountAttributeProvider.scala`  
**Trait:** `AccountAttribute`

**Description:** The primary entity representing a custom attribute associated with a bank account. Allows extending account metadata with typed key-value pairs for business-specific requirements like ISIN codes, loan identifiers, maturity dates, and other financial product attributes.

**Entity Fields (verified from database):**

| Field Name | Type | Description |
|------------|------|-------------|
| mBankIdId | UUIDString | Reference to the bank that owns the account |
| mAccountId | UUIDString | Reference to the account this attribute belongs to |
| mCode | MappedString(50) | Product code associated with the attribute |
| mAccountAttributeId | MappedUUID | Unique identifier for the account attribute |
| mName | MappedString(50) | Attribute name (e.g., "ISIN", "LOAN_ID", "MATURITY_DATE") |
| mType | MappedString(50) | Attribute type (STRING, INTEGER, DOUBLE, DATE_WITH_DAY) |
| mValue | MappedString(255) | Attribute value |
| mProductInstanceCode | MappedString(255) | Optional product instance identifier |

**Database Indexes:**
- Index(mAccountId)
- Index(mAccountAttributeId)

**Supported Attribute Types (from AccountAttributeType enum):**
- STRING - For text values (e.g., "TAX_NUMBER")
- INTEGER - For whole number values (e.g., "123")
- DOUBLE - For decimal values (e.g., "2012.04")
- DATE_WITH_DAY - For date values (e.g., "2012-04-23")

### 2. MappedBank (Referenced Entity)

**Database Table:** `mappedbank`  
**Source File:** `obp-api/src/main/scala/code/model/dataAccess/MappedBank.scala`  
**Trait:** `Bank`

**Description:** Entity representing a bank. Referenced by account attribute entities through the bankId field. Account attributes must be associated with a valid bank entity.

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

### 3. MappedBankAccount (Referenced Entity)

**Database Table:** `mappedbankaccount`  
**Source File:** `obp-api/src/main/scala/code/model/dataAccess/MappedBankAccount.scala`  
**Trait:** `BankAccount`

**Description:** Entity representing a bank account. Account attributes are linked to specific accounts via the accountId field. The account must exist before attributes can be created.

**Entity Fields (verified from database):**

| Field Name | Type | Description |
|------------|------|-------------|
| bank | UUIDString | Reference to the bank |
| theAccountId | AccountIdString | Unique account identifier |
| accountCurrency | MappedString(10) | Account currency code |
| accountNumber | MappedAccountNumber | Account number |
| holder | MappedString(100) | Account holder name (deprecated) |
| accountBalance | MappedLong | Account balance in smallest currency unit |
| accountName | MappedString(255) | Account name |
| kind | MappedString(255) | Account type / financial product name |
| accountLabel | MappedString(255) | Account label |
| accountLastUpdate | MappedDateTime | Last update timestamp |
| mBranchId | UUIDString | Branch identifier |
| accountRuleScheme1 | MappedString(10) | First account rule scheme |
| accountRuleValue1 | MappedLong | First account rule value |
| accountRuleScheme2 | MappedString(10) | Second account rule scheme |
| accountRuleValue2 | MappedLong | Second account rule value |

**Database Indexes:**
- UniqueIndex(bank, theAccountId)

### 4. MappedProduct (Referenced Entity)

**Database Table:** `mappedproduct`  
**Source File:** `obp-api/src/main/scala/code/products/MappedProductsProvider.scala`  
**Trait:** `Product`

**Description:** Entity representing a financial product. Account attributes are associated with products via the productCode field. The product must exist within the bank before attributes can be created.

**Entity Fields (verified from database):**

| Field Name | Type | Description |
|------------|------|-------------|
| mBankId | UUIDString | Reference to the bank |
| mCode | MappedString(50) | Unique product code |
| mParentProductCode | MappedString(50) | Parent product code for hierarchy |
| mName | MappedString(125) | Product name |
| mCategory | MappedString(50) | Product category |
| mFamily | MappedString(50) | Product family |
| mSuperFamily | MappedString(50) | Product super family |
| mMoreInfoUrl | MappedString(2000) | URL for more information |
| mTermsAndConditionsUrl | MappedString(2000) | URL for terms and conditions |
| mDetails | MappedString(2000) | Product details |
| mDescription | MappedString(2000) | Product description |
| mLicenseId | UUIDString | License identifier |
| mLicenseName | MappedString(255) | License name |

**Database Indexes:**
- UniqueIndex(mBankId, mCode)
- Index(mBankId)

## Entity Relationships

```
MappedBank (1) ----< (N) MappedBankAccount
    |                      |
    |                      |
    +-- bankId (FK) -------+
                           |
MappedBankAccount (1) ----< (N) MappedAccountAttribute
    |                              |
    |                              |
    +-- accountId (FK) ------------+
    +-- bankId (FK) ---------------+
                                   |
MappedProduct (1) ----< (N) MappedAccountAttribute
    |                              |
    |                              |
    +-- productCode (FK) ----------+
    +-- bankId (FK) ---------------+
```

**Relationship Details:**
1. **MappedBank to MappedBankAccount:** One-to-Many relationship. A bank can have multiple accounts, but each account belongs to exactly one bank.
2. **MappedBankAccount to MappedAccountAttribute:** One-to-Many relationship. An account can have multiple attributes for extensibility.
3. **MappedProduct to MappedAccountAttribute:** One-to-Many relationship. Account attributes are associated with a specific product, enabling product-specific metadata on accounts.
4. **MappedBank to MappedProduct:** One-to-Many relationship. A bank can have multiple products.

## Relevant Endpoints

Based on the user story, the following operations are supported:

| Operation | Endpoint | Entity |
|-----------|----------|--------|
| Create Account Attribute | POST /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/products/{PRODUCT_CODE}/attribute | MappedAccountAttribute |
| Update Account Attribute | PUT /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/products/{PRODUCT_CODE}/attributes/{ACCOUNT_ATTRIBUTE_ID} | MappedAccountAttribute |

## Notes

1. **Attribute Type Validation** is enforced using the `AccountAttributeType` enum which supports STRING, INTEGER, DOUBLE, and DATE_WITH_DAY types.
2. **Entity Existence Validation** is required before creating/updating attributes - the bank, account, and product must all exist.
3. **Product Association** enables product-specific metadata on accounts, allowing different attributes for different financial products.
4. **Typical Use Cases** for account attributes include:
   - ISIN (International Securities Identification Number) for bonds
   - VKN (German bond identifier)
   - REDCODE (Markit short code for credit derivatives)
   - LOAN_ID (for Anacredit reporting)
   - ISSUE_DATE (when a bond was issued)
   - MATURITY_DATE (end of product lifetime)
   - TRADABLE (whether the product can be traded)
5. **Authorization Requirements:**
   - Creating attributes requires `canCreateAccountAttributeAtOneBank` entitlement
   - Updating attributes requires `canUpdateAccountAttribute` entitlement
