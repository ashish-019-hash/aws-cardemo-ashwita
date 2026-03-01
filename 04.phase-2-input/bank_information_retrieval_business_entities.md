# Business Entity Extraction: Bank Information Retrieval

## Source User Story
**User Story**: Bank Information Retrieval

## Extraction Context
This document extracts business entities from the Bank Information Retrieval user story that are present in the database of the Scala (OBP-API) codebase. Only entities with corresponding database mappings have been included. Entity names and field names are specified exactly as they appear in the database Mapper classes.

---

## Extracted Business Entities

### 1. MappedBank

**Description**: The primary database entity representing a financial institution in the system. This entity stores core bank identification, branding, and routing information.

**Database Class**: `MappedBank` (code/model/dataAccess/MappedBank.scala)

**Domain Trait**: `Bank` (com.openbankproject.commons.model.BankingModel.scala)

**ORM**: Lift Mapper (extends LongKeyedMapper[MappedBank] with IdPK with CreatedUpdated)

**Database Fields**:

| Database Field Name | Data Type | Description | API/JSON Mapping | Required |
|---------------------|-----------|-------------|------------------|----------|
| permalink | MappedString(255) | Unique identifier for the bank, used in URLs | id, bankId | Yes |
| shortBankName | MappedString(100) | Short/abbreviated name of the bank | short_name | Yes |
| fullBankName | MappedString(255) | Full legal name of the bank | full_name | Yes |
| logoURL | MappedString(255) | URL to the bank's logo image | logo | No |
| websiteURL | MappedString(255) | URL to the bank's website | website | No |
| mBankRoutingScheme | MappedString(255) | Routing scheme identifier (e.g., BIC, SWIFT) | bank_routings.scheme | No |
| mBankRoutingAddress | MappedString(255) | Routing address value corresponding to the scheme | bank_routings.address | No |
| swiftBIC | MappedString(255) | SWIFT BIC code (deprecated) | - | No |
| national_identifier | MappedString(255) | National bank identifier (deprecated) | - | No |

**Relationships**:
- One-to-Many with BankAttribute (a MappedBank can have multiple BankAttribute records)

**Relevant Endpoints**:
- GET /banks - Retrieve all banks
- GET /banks/BANK_ID - Retrieve single bank details

**Business Rules**:
- permalink must be unique across the system (indexed)
- permalink is used as the primary lookup key for all bank operations

---

### 2. BankAttribute

**Description**: Database entity representing configurable attributes/parameters associated with a bank. These are operational parameters that can be dynamically added to banks without schema changes.

**Database Class**: `BankAttribute` (code/bankattribute/MappedBankAttributeProvider.scala)

**Domain Trait**: `BankAttributeTrait` (com.openbankproject.commons.model.CommonModelTrait.scala)

**ORM**: Lift Mapper (extends LongKeyedMapper[BankAttribute] with IdPK)

**Database Fields**:

| Database Field Name | Data Type | Description | API/JSON Mapping | Required |
|---------------------|-----------|-------------|------------------|----------|
| BankId_ | UUIDString | Foreign key reference to the parent MappedBank (permalink) | bank_id | Yes |
| BankAttributeId | MappedUUID | Unique identifier for the attribute (auto-generated) | - | Yes |
| Name | MappedString(50) | Name/key of the attribute | name | Yes |
| Type | MappedString(50) | Type classification of the attribute value (BankAttributeType enum) | type | Yes |
| Value | MappedString(255) | The actual value of the attribute | value | Yes |
| IsActive | MappedBoolean | Flag indicating if the attribute is currently active (default: true) | is_active | No |

**Relationships**:
- Many-to-One with MappedBank (multiple BankAttribute records belong to one MappedBank)

**Relevant Endpoints**:
- GET /banks/BANK_ID - Returns BankAttribute records as part of single bank retrieval

**Business Rules**:
- BankAttribute records are only returned for single bank retrieval (GET /banks/BANK_ID), not for list retrieval (GET /banks)
- If a bank has no attributes, an empty array should be returned
- IsActive defaults to true if not specified

---

## Entity Relationship Diagram (Textual)

```
+------------------------+          +------------------------+
|      MappedBank        |          |     BankAttribute      |
+------------------------+          +------------------------+
| permalink (PK, Index)  |<-------->| BankAttributeId (PK)   |
| shortBankName          |    1:N   | BankId_ (FK, Index)    |
| fullBankName           |          | Name                   |
| logoURL                |          | Type                   |
| websiteURL             |          | Value                  |
| mBankRoutingScheme     |          | IsActive               |
| mBankRoutingAddress    |          +------------------------+
| swiftBIC               |
| national_identifier    |
+------------------------+
```

---

## Response Data Structures

### Bank List Response (GET /banks)
Based on the user story, the response structure maps to the MappedBank entity:
```
{
  "banks": [
    {
      "id": MappedBank.permalink,
      "short_name": MappedBank.shortBankName,
      "full_name": MappedBank.fullBankName,
      "logo": MappedBank.logoURL,
      "website": MappedBank.websiteURL,
      "bank_routings": [
        {
          "scheme": MappedBank.mBankRoutingScheme,
          "address": MappedBank.mBankRoutingAddress
        }
      ]
    }
  ]
}
```

### Single Bank Response (GET /banks/BANK_ID)
Based on the user story, the response structure maps to MappedBank and BankAttribute entities:
```
{
  "id": MappedBank.permalink,
  "short_name": MappedBank.shortBankName,
  "full_name": MappedBank.fullBankName,
  "logo": MappedBank.logoURL,
  "website": MappedBank.websiteURL,
  "bank_routings": [
    {
      "scheme": MappedBank.mBankRoutingScheme,
      "address": MappedBank.mBankRoutingAddress
    }
  ],
  "attributes": [
    {
      "bank_id": BankAttribute.BankId_,
      "name": BankAttribute.Name,
      "type": BankAttribute.Type,
      "value": BankAttribute.Value,
      "is_active": BankAttribute.IsActive
    }
  ]
}
```

---

## Summary

| Database Entity | Database Class | Primary Key | Foreign Keys | Index Fields |
|-----------------|----------------|-------------|--------------|--------------|
| MappedBank | MappedBank | id (IdPK) | None | permalink |
| BankAttribute | BankAttribute | id (IdPK) | BankId_ -> MappedBank.permalink | BankId_ |

---

## Notes

1. **Deprecated Fields**: The `swiftBIC` and `national_identifier` fields in MappedBank are deprecated. The recommended approach is to use `mBankRoutingScheme` and `mBankRoutingAddress` instead.

2. **Performance Consideration**: BankAttribute records are intentionally excluded from the list retrieval endpoint (GET /banks) for performance reasons, as mentioned in the user story.

3. **Database Technology**: The Scala codebase uses Lift Web Framework's ORM (Mapper) for database persistence. Both entities extend `LongKeyedMapper` which provides standard CRUD operations and database mapping.

4. **Source Files**:
   - MappedBank database class: `obp-api/src/main/scala/code/model/dataAccess/MappedBank.scala`
   - Bank domain trait: `obp-commons/src/main/scala/com/openbankproject/commons/model/BankingModel.scala`
   - BankAttribute database class: `obp-api/src/main/scala/code/bankattribute/MappedBankAttributeProvider.scala`
   - BankAttributeTrait domain trait: `obp-api/src/main/scala/code/bankattribute/BankAttribute.scala`

5. **Field Name Conventions**: 
   - MappedBank uses camelCase for most fields (e.g., `shortBankName`, `fullBankName`)
   - MappedBank uses underscore for `national_identifier`
   - BankAttribute uses PascalCase for fields (e.g., `BankId_`, `Name`, `Type`, `Value`, `IsActive`)
