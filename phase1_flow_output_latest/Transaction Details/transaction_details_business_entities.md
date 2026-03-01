# Business Entity Extraction for Transaction Details

## Capability Overview

- **Name**: Transaction Details
- **Description**: Get detailed information about a specific transaction
- **Frequency**: Real-time
- **Volume**: High

---

## Extracted Business Entities

The following business entities have been extracted from the Transaction Details user story and verified against the actual database tables in the Scala codebase (OBP-API).

### 1. MappedTransaction

**Database Table**: `mappedtransaction`

**Description**: Core transaction entity that stores all transaction data including amount, currency, dates, description, and counterparty information.

**Relevance to User Story**: This is the primary entity for the "Get detailed information about a specific transaction" capability. It contains the transaction ID, amount, currency, type, description, posted date, completion date, and balance information.

**Key Attributes** (from Scala codebase):
| Attribute | Type | Description |
|-----------|------|-------------|
| transactionId | String | Unique transaction identifier |
| bank | String | Bank identifier |
| account | String | Account identifier |
| transactionType | String | Type of transaction (SEPA, COUNTERPARTY, etc.) |
| amount | Long | Transaction amount in smallest currency unit |
| newAccountBalance | Long | Balance after transaction |
| currency | String | Currency code |
| tStartDate | DateTime | Transaction start/posted date |
| tFinishDate | DateTime | Transaction completion date |
| description | String | Transaction description |
| status | String | Transaction status |
| counterpartyAccountHolder | String | Counterparty name |
| CPCounterPartyId | String | Counterparty identifier |
| CPOtherAccountRoutingScheme | String | Counterparty account routing scheme |
| CPOtherAccountRoutingAddress | String | Counterparty account routing address |
| CPOtherBankRoutingScheme | String | Counterparty bank routing scheme |
| CPOtherBankRoutingAddress | String | Counterparty bank routing address |

**Source File**: `/obp-api/src/main/scala/code/transaction/MappedTransaction.scala`

---

### 2. MappedBank

**Database Table**: `mappedbank`

**Description**: Bank entity that stores bank identification, branding, and routing information.

**Relevance to User Story**: Required for validating the BANK_ID path parameter and retrieving bank routing information for the transaction's this_account and other_account.

**Key Attributes** (from Scala codebase):
| Attribute | Type | Description |
|-----------|------|-------------|
| permalink | String | Unique bank identifier (used as bankId in URLs) |
| fullBankName | String | Full name of the bank |
| shortBankName | String | Short name of the bank |
| logoURL | String | URL to bank logo |
| websiteURL | String | Bank website URL |
| swiftBIC | String | SWIFT/BIC code |
| national_identifier | String | National bank identifier |
| mBankRoutingScheme | String | Bank routing scheme |
| mBankRoutingAddress | String | Bank routing address |

**Source File**: `/obp-api/src/main/scala/code/model/dataAccess/MappedBank.scala`

---

### 3. MappedBankAccount

**Database Table**: `mappedbankaccount`

**Description**: Bank account entity that stores account details including balance, currency, and routing information.

**Relevance to User Story**: Required for validating the ACCOUNT_ID path parameter and retrieving account information for the this_account section of the transaction response.

**Key Attributes** (from Scala codebase):
| Attribute | Type | Description |
|-----------|------|-------------|
| bank | String | Bank identifier |
| theAccountId | String | Unique account identifier |
| accountCurrency | String | Account currency |
| accountNumber | String | Account number |
| accountBalance | Long | Current account balance |
| accountName | String | Account name |
| kind | String | Account type/financial product name |
| accountLabel | String | User-defined account label |
| mBranchId | String | Branch identifier |

**Source File**: `/obp-api/src/main/scala/code/model/dataAccess/MappedBankAccount.scala`

---

### 4. MappedCounterparty

**Database Table**: `mappedcounterparty`

**Description**: Counterparty entity that stores information about the other party in a transaction, created explicitly via the CreateCounterparty endpoint.

**Relevance to User Story**: Provides counterparty/other_account information in the transaction details response including holder name, bank routing, and account routing.

**Key Attributes** (from Scala codebase):
| Attribute | Type | Description |
|-----------|------|-------------|
| mCounterPartyId | String | Unique counterparty identifier |
| mName | String | Counterparty name |
| mThisBankId | String | This bank identifier |
| mThisAccountId | String | This account identifier |
| mThisViewId | String | View identifier |
| mOtherAccountRoutingScheme | String | Other account routing scheme |
| mOtherAccountRoutingAddress | String | Other account routing address |
| mOtherBankRoutingScheme | String | Other bank routing scheme |
| mOtherBankRoutingAddress | String | Other bank routing address |
| mOtherAccountSecondaryRoutingScheme | String | Secondary routing scheme (e.g., IBAN) |
| mOtherAccountSecondaryRoutingAddress | String | Secondary routing address |
| mIsBeneficiary | Boolean | Whether counterparty is a beneficiary |
| mDescription | String | Counterparty description |
| mCurrency | String | Currency |

**Source File**: `/obp-api/src/main/scala/code/metadata/counterparties/MapperCounterparties.scala`

---

### 5. MappedCounterpartyMetadata

**Database Table**: `mappedcounterpartymetadata`

**Description**: Metadata entity for counterparties that stores additional information like aliases, URLs, and location data.

**Relevance to User Story**: Provides the metadata section of the other_account in transaction details including public_alias, private_alias, more_info, url, image_url, open_corporates_url, corporate_location, and physical_location.

**Key Attributes** (from Scala codebase):
| Attribute | Type | Description |
|-----------|------|-------------|
| counterpartyId | String | Counterparty identifier |
| counterpartyName | String | Counterparty name |
| thisBankId | String | This bank identifier |
| thisAccountId | String | This account identifier |
| publicAlias | String | Public alias for counterparty |
| privateAlias | String | Private alias for counterparty |
| moreInfo | String | Additional information |
| url | String | Counterparty URL |
| imageUrl | String | Counterparty image URL |
| openCorporatesUrl | String | Open Corporates URL |
| physicalLocation | Long | Foreign key to physical location |
| corporateLocation | Long | Foreign key to corporate location |

**Source File**: `/obp-api/src/main/scala/code/metadata/counterparties/MapperCounterparties.scala`

---

### 6. MappedComment

**Database Table**: `mappedcomment`

**Description**: Transaction comment entity that stores user comments on transactions.

**Relevance to User Story**: Provides the comments array in the transaction metadata section, allowing users to add and view comments on transactions.

**Key Attributes** (from Scala codebase):
| Attribute | Type | Description |
|-----------|------|-------------|
| apiId | String | Unique comment identifier |
| text_ | String | Comment text content |
| poster | Long | Foreign key to user who posted |
| replyTo | String | ID of comment being replied to |
| view | String | View identifier |
| date | DateTime | Date comment was posted |
| bank | String | Bank identifier |
| account | String | Account identifier |
| transaction | String | Transaction identifier |

**Source File**: `/obp-api/src/main/scala/code/metadata/comments/MappedComment.scala`

---

### 7. MappedTag

**Database Table**: `mappedtag`

**Description**: Transaction tag entity that stores categorization tags for transactions.

**Relevance to User Story**: Provides the tags array in the transaction metadata section, allowing users to categorize transactions with custom tags.

**Key Attributes** (from Scala codebase):
| Attribute | Type | Description |
|-----------|------|-------------|
| tagId | String | Unique tag identifier |
| bank | String | Bank identifier |
| account | String | Account identifier |
| transaction | String | Transaction identifier |
| view | String | View identifier |
| user | Long | Foreign key to user who created tag |
| tag | String | Tag value/text |
| date | DateTime | Date tag was created |

**Source File**: `/obp-api/src/main/scala/code/metadata/tags/MappedTags.scala`

---

### 8. MappedTransactionImage

**Database Table**: `mappedtransactionimage`

**Description**: Transaction image entity that stores images associated with transactions (e.g., receipts).

**Relevance to User Story**: Provides the images array in the transaction metadata section, allowing users to attach and view images related to transactions.

**Key Attributes** (from Scala codebase):
| Attribute | Type | Description |
|-----------|------|-------------|
| imageId | String | Unique image identifier |
| bank | String | Bank identifier |
| account | String | Account identifier |
| transaction | String | Transaction identifier |
| view | String | View identifier |
| user | Long | Foreign key to user who uploaded |
| date | DateTime | Date image was uploaded |
| url | String | Image URL |
| imageDescription | String | Image description/label |

**Source File**: `/obp-api/src/main/scala/code/metadata/transactionimages/MapperTransactionImages.scala`

---

### 9. MappedNarrative

**Database Table**: `mappednarrative`

**Description**: Transaction narrative entity that stores user-defined narratives for transactions.

**Relevance to User Story**: Provides the narrative field in the transaction metadata section, allowing users to add custom descriptions to transactions.

**Key Attributes** (from Scala codebase):
| Attribute | Type | Description |
|-----------|------|-------------|
| bank | String | Bank identifier |
| account | String | Account identifier |
| transaction | String | Transaction identifier |
| narrative | String | User-defined narrative text |

**Source File**: `/obp-api/src/main/scala/code/metadata/narrative/MappedNarratives.scala`

---

### 10. ViewDefinition

**Database Table**: `viewdefinition`

**Description**: View entity that defines access permissions and controls what transaction data fields are visible to users.

**Relevance to User Story**: Required for the VIEW_ID path parameter validation and enforcing access control. The view determines which transaction fields are visible and what actions users can perform.

**Key Attributes** (from Scala codebase):
| Attribute | Type | Description |
|-----------|------|-------------|
| id_ | Long | Primary key |
| name_ | String | View name |
| description_ | String | View description |
| bank_id | String | Bank identifier |
| account_id | String | Account identifier |
| view_id | String | Unique view identifier |
| isSystem_ | Boolean | Whether this is a system view |
| isPublic_ | Boolean | Whether view is public |
| isFirehose_ | Boolean | Whether view has firehose access |
| metadataView_ | String | Metadata view reference |
| canSeeTransactionThisBankAccount_ | Boolean | Permission to see transaction |
| canSeeTransactionMetadata_ | Boolean | Permission to see metadata |
| canSeeComments_ | Boolean | Permission to see comments |
| canSeeTags_ | Boolean | Permission to see tags |
| canSeeImages_ | Boolean | Permission to see images |

**Source File**: `/obp-api/src/main/scala/code/views/system/ViewDefinition.scala`

---

### 11. ResourceUser

**Database Table**: `resourceuser`

**Description**: User entity that stores user authentication and identification information.

**Relevance to User Story**: Required for authentication validation and identifying users who posted comments, tags, and images in transaction metadata.

**Key Attributes** (from Scala codebase):
| Attribute | Type | Description |
|-----------|------|-------------|
| id | Long | Primary key |
| userId_ | String | Unique user identifier (UUID) |
| email | String | User email address |
| name_ | String | User display name |
| provider_ | String | Authentication provider |
| providerId | String | Provider-specific user ID |
| Company | String | User's company |
| IsDeleted | Boolean | Soft delete flag |

**Source File**: `/obp-api/src/main/scala/code/model/dataAccess/ResourceUser.scala`

---

### 12. MapperAccountHolders

**Database Table**: `mapperaccountholders`

**Description**: Account holder entity that links users to bank accounts they hold.

**Relevance to User Story**: Provides the holders array in the this_account section of transaction details, showing who owns the account.

**Key Attributes** (from Scala codebase):
| Attribute | Type | Description |
|-----------|------|-------------|
| user | Long | Foreign key to ResourceUser |
| accountBankPermalink | String | Bank identifier |
| accountPermalink | String | Account identifier |
| source | String | Source of account holder relationship |

**Source File**: `/obp-api/src/main/scala/code/accountholders/MapperAccountHolders.scala`

---

## Entity Relationships

```
MappedBank (1) ----< (N) MappedBankAccount
MappedBankAccount (1) ----< (N) MappedTransaction
MappedTransaction (1) ----< (N) MappedComment
MappedTransaction (1) ----< (N) MappedTag
MappedTransaction (1) ----< (N) MappedTransactionImage
MappedTransaction (1) ----< (1) MappedNarrative
MappedTransaction (N) >---- (1) MappedCounterparty
MappedCounterparty (1) ----< (1) MappedCounterpartyMetadata
MappedBankAccount (N) >----< (N) ResourceUser (via MapperAccountHolders)
MappedBankAccount (N) >----< (N) ViewDefinition (via AccountAccess)
ResourceUser (1) ----< (N) MappedComment
ResourceUser (1) ----< (N) MappedTag
ResourceUser (1) ----< (N) MappedTransactionImage
```

---

## Verification Summary

All entities listed above have been verified against the actual Scala codebase in the OBP-API repository:

| Entity | Verified | Source Location |
|--------|----------|-----------------|
| MappedTransaction | Yes | code/transaction/MappedTransaction.scala |
| MappedBank | Yes | code/model/dataAccess/MappedBank.scala |
| MappedBankAccount | Yes | code/model/dataAccess/MappedBankAccount.scala |
| MappedCounterparty | Yes | code/metadata/counterparties/MapperCounterparties.scala |
| MappedCounterpartyMetadata | Yes | code/metadata/counterparties/MapperCounterparties.scala |
| MappedComment | Yes | code/metadata/comments/MappedComment.scala |
| MappedTag | Yes | code/metadata/tags/MappedTags.scala |
| MappedTransactionImage | Yes | code/metadata/transactionimages/MapperTransactionImages.scala |
| MappedNarrative | Yes | code/metadata/narrative/MappedNarratives.scala |
| ViewDefinition | Yes | code/views/system/ViewDefinition.scala |
| ResourceUser | Yes | code/model/dataAccess/ResourceUser.scala |
| MapperAccountHolders | Yes | code/accountholders/MapperAccountHolders.scala |

---

## Notes

1. All entity names match the exact class names in the Scala codebase
2. Database table names are derived from the Lift Mapper convention (lowercase class name)
3. Only entities directly relevant to the Transaction Details capability are included
4. No irrelevant entities have been added - each entity maps to a specific part of the transaction detail response structure
