# Business Entity Extraction for Transaction Listing

## Capability Overview

- **Capability Name**: Transaction Listing
- **Description**: Retrieve transaction history for accounts with filtering and pagination
- **Source**: User Story for Transaction Listing

---

## Extracted Business Entities

The following business entities have been extracted from the user story and verified against the database tables in the Scala codebase (OBP-API). Only entities that exist in the database and are relevant to the Transaction Listing capability are included.

### 1. MappedTransaction

**Database Table**: `MappedTransaction`

**Source File**: `code/transaction/MappedTransaction.scala`

**Description**: Core transaction entity that stores all transaction records for accounts. This is the primary entity for the Transaction Listing capability.

**Key Attributes**:
| Attribute | Type | Description |
|-----------|------|-------------|
| transactionId | String | Unique transaction identifier |
| bank | String | Bank identifier |
| account | String | Account identifier |
| transactionType | String | Type of transaction |
| amount | Long | Transaction amount (smallest currency unit) |
| newAccountBalance | Long | Balance after transaction |
| currency | String | Transaction currency |
| tStartDate | DateTime | Transaction start date |
| tFinishDate | DateTime | Transaction finish date |
| description | String | Transaction description |
| status | String | Transaction status |
| counterpartyAccountHolder | String | Counterparty account holder name |
| CPOtherAccountRoutingScheme | String | Counterparty routing scheme |
| CPOtherAccountRoutingAddress | String | Counterparty routing address |
| CPOtherBankRoutingScheme | String | Counterparty bank routing scheme |
| CPOtherBankRoutingAddress | String | Counterparty bank routing address |

**Relevance to User Story**: This entity directly supports "Retrieve transaction history for accounts" - the core functionality of the Transaction Listing capability.

---

### 2. MappedBank

**Database Table**: `MappedBank`

**Source File**: `code/model/dataAccess/MappedBank.scala`

**Description**: Bank entity that stores bank information. Required for identifying which bank's transactions to retrieve.

**Key Attributes**:
| Attribute | Type | Description |
|-----------|------|-------------|
| permalink | String | Bank identifier used in URLs (BANK_ID) |
| fullBankName | String | Full name of the bank |
| shortBankName | String | Short name of the bank |
| logoURL | String | Bank logo URL |
| websiteURL | String | Bank website URL |
| swiftBIC | String | SWIFT/BIC code |
| national_identifier | String | National identifier |
| mBankRoutingScheme | String | Bank routing scheme |
| mBankRoutingAddress | String | Bank routing address |

**Relevance to User Story**: Required for the endpoint path parameter `BANK_ID` in transaction listing endpoints.

---

### 3. MappedBankAccount

**Database Table**: `MappedBankAccount`

**Source File**: `code/model/dataAccess/MappedBankAccount.scala`

**Description**: Bank account entity that stores account information. Required for identifying which account's transactions to retrieve.

**Key Attributes**:
| Attribute | Type | Description |
|-----------|------|-------------|
| bank | String | Bank identifier |
| theAccountId | String | Account identifier |
| accountCurrency | String | Account currency |
| accountNumber | String | Account number |
| accountBalance | Long | Current account balance |
| accountName | String | Account name |
| kind | String | Account type/financial product name |
| accountLabel | String | Account label |
| accountLastUpdate | DateTime | Last update timestamp |
| mBranchId | String | Branch identifier |

**Relevance to User Story**: Required for the endpoint path parameter `ACCOUNT_ID` in transaction listing endpoints.

---

### 4. ViewDefinition

**Database Table**: `ViewDefinition`

**Source File**: `code/views/system/ViewDefinition.scala`

**Description**: View definition entity that defines access permissions and what transaction data can be seen. Critical for access control in transaction listing.

**Key Attributes**:
| Attribute | Type | Description |
|-----------|------|-------------|
| id_ | Long | View identifier |
| name_ | String | View name |
| description_ | String | View description |
| bank_id | String | Bank identifier |
| account_id | String | Account identifier |
| view_id | String | View identifier used in URLs |
| isSystem_ | Boolean | Whether this is a system view |
| isPublic_ | Boolean | Whether this view is public |
| canSeeTransactionThisBankAccount_ | Boolean | Permission to see transactions |
| canSeeTransactionAmount_ | Boolean | Permission to see transaction amount |
| canSeeTransactionDescription_ | Boolean | Permission to see transaction description |
| canSeeTransactionMetadata_ | Boolean | Permission to see transaction metadata |
| canSeeTags_ | Boolean | Permission to see transaction tags |
| canSeeComments_ | Boolean | Permission to see transaction comments |
| canSeeImages_ | Boolean | Permission to see transaction images |

**Relevance to User Story**: Required for the endpoint path parameter `VIEW_ID` and enforces access control for transaction listing.

---

### 5. AccountAccess

**Database Table**: `AccountAccess`

**Source File**: `code/views/system/AccountAccess.scala`

**Description**: Account access entity that links users to views, controlling who can access which account's transactions.

**Key Attributes**:
| Attribute | Type | Description |
|-----------|------|-------------|
| user_fk | Long | User foreign key |
| bank_id | String | Bank identifier |
| account_id | String | Account identifier |
| view_id | String | View identifier |
| consumer_id | String | Consumer identifier |

**Relevance to User Story**: Supports the acceptance criteria "return only transactions for accounts that the requesting user has been granted access to view".

---

### 6. MappedCounterparty

**Database Table**: `MappedCounterparty`

**Source File**: `code/metadata/counterparties/MapperCounterparties.scala`

**Description**: Counterparty entity that stores information about the other party in a transaction.

**Key Attributes**:
| Attribute | Type | Description |
|-----------|------|-------------|
| mCounterPartyId | String | Counterparty identifier |
| mName | String | Counterparty name |
| mThisBankId | String | This bank identifier |
| mThisAccountId | String | This account identifier |
| mThisViewId | String | This view identifier |
| mOtherAccountRoutingScheme | String | Other account routing scheme |
| mOtherAccountRoutingAddress | String | Other account routing address |
| mOtherBankRoutingScheme | String | Other bank routing scheme |
| mOtherBankRoutingAddress | String | Other bank routing address |
| mIsBeneficiary | Boolean | Whether counterparty is beneficiary |
| mDescription | String | Counterparty description |
| mCurrency | String | Currency |

**Relevance to User Story**: Supports the output data requirement for "other_account" (counterparty account information) in transaction responses.

---

### 7. MappedCounterpartyMetadata

**Database Table**: `MappedCounterpartyMetadata`

**Source File**: `code/metadata/counterparties/MapperCounterparties.scala`

**Description**: Counterparty metadata entity that stores additional metadata about counterparties.

**Key Attributes**:
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
| openCorporatesUrl | String | OpenCorporates URL |

**Relevance to User Story**: Supports counterparty metadata display in transaction listing responses.

---

### 8. MappedTag

**Database Table**: `MappedTag`

**Source File**: `code/metadata/tags/MappedTags.scala`

**Description**: Transaction tag entity that stores tags associated with transactions.

**Key Attributes**:
| Attribute | Type | Description |
|-----------|------|-------------|
| tagId | String | Tag identifier |
| bank | String | Bank identifier |
| account | String | Account identifier |
| transaction | String | Transaction identifier |
| view | String | View identifier |
| user | Long | User who created the tag |
| tag | String | Tag value |
| date | DateTime | Date tag was posted |

**Relevance to User Story**: Supports the output data requirement for "metadata.tags" in transaction responses.

---

### 9. MappedComment

**Database Table**: `MappedComment`

**Source File**: `code/metadata/comments/MappedComment.scala`

**Description**: Transaction comment entity that stores comments associated with transactions.

**Key Attributes**:
| Attribute | Type | Description |
|-----------|------|-------------|
| apiId | String | Comment identifier |
| bank | String | Bank identifier |
| account | String | Account identifier |
| transaction | String | Transaction identifier |
| view | String | View identifier |
| poster | Long | User who posted the comment |
| text_ | String | Comment text |
| date | DateTime | Date comment was posted |
| replyTo | String | Reply to comment ID |

**Relevance to User Story**: Supports the output data requirement for "metadata.comments" in transaction responses.

---

### 10. MappedTransactionImage

**Database Table**: `MappedTransactionImage`

**Source File**: `code/metadata/transactionimages/MapperTransactionImages.scala`

**Description**: Transaction image entity that stores images associated with transactions.

**Key Attributes**:
| Attribute | Type | Description |
|-----------|------|-------------|
| imageId | String | Image identifier |
| bank | String | Bank identifier |
| account | String | Account identifier |
| transaction | String | Transaction identifier |
| view | String | View identifier |
| user | Long | User who uploaded the image |
| url | String | Image URL |
| imageDescription | String | Image description |
| date | DateTime | Date image was posted |

**Relevance to User Story**: Supports the output data requirement for "metadata.images" in transaction responses.

---

### 11. MappedTransactionAttribute

**Database Table**: `MappedTransactionAttribute`

**Source File**: `code/transactionattribute/MappedTransactionAttributeProvider.scala`

**Description**: Transaction attribute entity that stores custom attributes for transactions.

**Key Attributes**:
| Attribute | Type | Description |
|-----------|------|-------------|
| mTransactionAttributeId | String | Transaction attribute identifier |
| mBankId | String | Bank identifier |
| mTransactionId | String | Transaction identifier |
| mName | String | Attribute name |
| mType | String | Attribute type |
| mValue | String | Attribute value |

**Relevance to User Story**: Supports extended transaction data and filtering capabilities mentioned in the user story.

---

### 12. MappedNarrative

**Database Table**: `MappedNarrative`

**Source File**: `code/metadata/narrative/MappedNarratives.scala`

**Description**: Transaction narrative entity that stores narrative/description metadata for transactions.

**Key Attributes**:
| Attribute | Type | Description |
|-----------|------|-------------|
| bank | String | Bank identifier |
| account | String | Account identifier |
| transaction | String | Transaction identifier |
| narrative | String | Narrative text |

**Relevance to User Story**: Supports the output data requirement for "metadata.narrative" in transaction responses.

---

## Entity Relationships

```
MappedBank (1) ----< (N) MappedBankAccount
MappedBankAccount (1) ----< (N) MappedTransaction
MappedTransaction (1) ----< (N) MappedTag
MappedTransaction (1) ----< (N) MappedComment
MappedTransaction (1) ----< (N) MappedTransactionImage
MappedTransaction (1) ----< (N) MappedTransactionAttribute
MappedTransaction (1) ----< (1) MappedNarrative
MappedTransaction (N) >---- (1) MappedCounterparty
MappedCounterparty (1) ----< (1) MappedCounterpartyMetadata
MappedBankAccount (N) >----< (N) ViewDefinition (via AccountAccess)
```

---

## Summary

| Entity Name | Database Table | Primary Purpose |
|-------------|----------------|-----------------|
| MappedTransaction | MappedTransaction | Core transaction data storage |
| MappedBank | MappedBank | Bank identification |
| MappedBankAccount | MappedBankAccount | Account identification |
| ViewDefinition | ViewDefinition | Access control and permissions |
| AccountAccess | AccountAccess | User-view-account linking |
| MappedCounterparty | MappedCounterparty | Counterparty information |
| MappedCounterpartyMetadata | MappedCounterpartyMetadata | Counterparty metadata |
| MappedTag | MappedTag | Transaction tags |
| MappedComment | MappedComment | Transaction comments |
| MappedTransactionImage | MappedTransactionImage | Transaction images |
| MappedTransactionAttribute | MappedTransactionAttribute | Transaction custom attributes |
| MappedNarrative | MappedNarrative | Transaction narrative |

---

## Verification Notes

All entities listed above have been verified against the actual database tables in the Scala codebase (OBP-API repository at `/home/ubuntu/repos/OBP-API-Cloned`). The entity names used are the exact class names from the Scala source files that map to database tables using the Lift Mapper ORM framework.

---

*This business entity extraction was generated by analyzing the Transaction Listing user story and cross-referencing with the database entities in the Scala codebase.*
