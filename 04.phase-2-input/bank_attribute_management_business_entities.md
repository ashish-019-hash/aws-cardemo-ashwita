# Business Entity Extraction: Bank Attribute Management

## Source User Story
**User Story**: Bank Attribute Management

## Extraction Context
This document extracts business entities from the Bank Attribute Management user story that are present in the database of the Scala (OBP-API) codebase. Only entities with corresponding database mappings have been included. Entity names and field names are specified exactly as they appear in the database Mapper classes.

---

## Extracted Business Entities

### 1. BankAttribute

**Description**: The primary database entity for this user story. Represents configurable custom attributes/parameters associated with a bank. These attributes provide extended metadata beyond standard bank fields, supporting multiple data types and active/inactive status.

**Database Class**: `BankAttribute` (code/bankattribute/MappedBankAttributeProvider.scala)

**Domain Trait**: `BankAttributeTrait` (com.openbankproject.commons.model.CommonModelTrait.scala)

**ORM**: Lift Mapper (extends LongKeyedMapper[BankAttribute] with IdPK)

**Database Fields**:

| Database Field Name | Data Type | Description | API/JSON Mapping | Required |
|---------------------|-----------|-------------|------------------|----------|
| BankId_ | UUIDString | Foreign key reference to the parent MappedBank (permalink) | bank_id | Yes |
| BankAttributeId | MappedUUID | Unique identifier for the attribute (auto-generated) | bank_attribute_id | Yes |
| Name | MappedString(50) | Name/key of the attribute | name | Yes |
| Type | MappedString(50) | Type classification of the attribute value (BankAttributeType enum: STRING, INTEGER, DOUBLE, DATE_WITH_DAY) | type | Yes |
| Value | MappedString(255) | The actual value of the attribute | value | Yes |
| IsActive | MappedBoolean | Flag indicating if the attribute is currently active (default: true) | is_active | No |

**Supported Attribute Types (BankAttributeType Enum)**:
- STRING: Any text value
- INTEGER: Whole number (e.g., 123)
- DOUBLE: Decimal number (e.g., 12.1234)
- DATE_WITH_DAY: Date format (e.g., 2012-04-23)

**Relationships**:
- Many-to-One with MappedBank (multiple BankAttribute records belong to one MappedBank via BankId_ -> MappedBank.permalink)

**Relevant Endpoints**:
- POST /banks/BANK_ID/attribute - Define (create) a new bank attribute
- PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Manage (update) an existing bank attribute
- GET /banks/BANK_ID/attributes - Retrieve all bank attributes for a specific bank
- GET /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Retrieve a single bank attribute
- DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Delete a bank attribute

**Business Rules**:
- BankId_ must reference an existing MappedBank.permalink
- Type must be one of the supported BankAttributeType values
- Value must match the specified Type (type-value consistency)
- Name should be unique within a bank's attribute set
- IsActive defaults to true if not specified
- BankAttributeId is auto-generated on creation

**Database Operations**:
- Create: `BankAttribute.create.BankId_(bankId).Name(name).Type(type).Value(value).IsActive(isActive).saveMe()`
- Read by ID: `BankAttribute.find(By(BankAttribute.BankAttributeId, id))`
- Read all by Bank: `BankAttribute.findAll(By(BankAttribute.BankId_, bankId))`
- Update: Find by BankAttributeId, then update fields and call `.saveMe()`
- Delete: `BankAttribute.bulkDelete_!!(By(BankAttribute.BankAttributeId, bankAttributeId))`

---

### 2. MappedBank (Reference Entity)

**Description**: Referenced entity representing the parent bank. BankAttribute records are associated with a specific bank through the BankId_ foreign key field. This entity is required for validating bank existence before attribute operations.

**Database Class**: `MappedBank` (code/model/dataAccess/MappedBank.scala)

**Domain Trait**: `Bank` (com.openbankproject.commons.model.BankingModel.scala)

**ORM**: Lift Mapper (extends LongKeyedMapper[MappedBank] with IdPK with CreatedUpdated)

**Relevant Database Fields for Bank Attribute Management**:

| Database Field Name | Data Type | Description | API/JSON Mapping | Required |
|---------------------|-----------|-------------|------------------|----------|
| permalink | MappedString(255) | Unique identifier for the bank, used as foreign key in BankAttribute.BankId_ | BANK_ID (path parameter) | Yes |

**Relationships**:
- One-to-Many with BankAttribute (a MappedBank can have multiple BankAttribute records)

**Role in Bank Attribute Management**:
- Validates bank existence before creating, retrieving, updating, or deleting attributes
- Provides the parent context for attribute operations
- Referenced via BankId_ field in BankAttribute

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

## Request/Response Data Structures

### Create Bank Attribute (POST /banks/BANK_ID/attribute)
**Request Body**:
```json
{
  "name": BankAttribute.Name,
  "type": BankAttribute.Type,
  "value": BankAttribute.Value,
  "is_active": BankAttribute.IsActive
}
```

**Response**:
```json
{
  "bank_id": BankAttribute.BankId_,
  "bank_attribute_id": BankAttribute.BankAttributeId,
  "name": BankAttribute.Name,
  "type": BankAttribute.Type,
  "value": BankAttribute.Value,
  "is_active": BankAttribute.IsActive
}
```

### Update Bank Attribute (PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID)
**Request Body**:
```json
{
  "name": BankAttribute.Name,
  "type": BankAttribute.Type,
  "value": BankAttribute.Value,
  "is_active": BankAttribute.IsActive
}
```

**Response**:
```json
{
  "bank_id": BankAttribute.BankId_,
  "bank_attribute_id": BankAttribute.BankAttributeId,
  "name": BankAttribute.Name,
  "type": BankAttribute.Type,
  "value": BankAttribute.Value,
  "is_active": BankAttribute.IsActive
}
```

### Retrieve All Bank Attributes (GET /banks/BANK_ID/attributes)
**Response**:
```json
{
  "bank_attributes": [
    {
      "bank_id": BankAttribute.BankId_,
      "bank_attribute_id": BankAttribute.BankAttributeId,
      "name": BankAttribute.Name,
      "type": BankAttribute.Type,
      "value": BankAttribute.Value,
      "is_active": BankAttribute.IsActive
    }
  ]
}
```

### Retrieve Single Bank Attribute (GET /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID)
**Response**:
```json
{
  "bank_id": BankAttribute.BankId_,
  "bank_attribute_id": BankAttribute.BankAttributeId,
  "name": BankAttribute.Name,
  "type": BankAttribute.Type,
  "value": BankAttribute.Value,
  "is_active": BankAttribute.IsActive
}
```

### Delete Bank Attribute (DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID)
**Response**: HTTP 204 No Content

---

## Summary

| Database Entity | Database Class | Primary Key | Foreign Keys | Index Fields | Role in User Story |
|-----------------|----------------|-------------|--------------|--------------|-------------------|
| BankAttribute | BankAttribute | id (IdPK) | BankId_ -> MappedBank.permalink | BankId_ | Primary entity - CRUD operations |
| MappedBank | MappedBank | id (IdPK) | None | permalink | Reference entity - validates bank existence |

---

## Notes

1. **Primary Entity**: BankAttribute is the primary entity for this user story, supporting full CRUD operations (Create, Read, Update, Delete).

2. **Type Validation**: The Type field must contain one of the BankAttributeType enum values: STRING, INTEGER, DOUBLE, DATE_WITH_DAY. The Value field must match the specified Type.

3. **Soft Deletion**: The IsActive field allows soft deactivation of attributes without permanent deletion. Use `is_active=false` for soft deletion, DELETE endpoint for permanent removal.

4. **Database Technology**: The Scala codebase uses Lift Web Framework's ORM (Mapper) for database persistence. BankAttribute extends `LongKeyedMapper` which provides standard CRUD operations.

5. **Source Files**:
   - BankAttribute database class: `obp-api/src/main/scala/code/bankattribute/MappedBankAttributeProvider.scala`
   - BankAttributeTrait domain trait: `obp-api/src/main/scala/code/bankattribute/BankAttribute.scala`
   - BankAttributeType enum: `obp-commons/src/main/scala/com/openbankproject/commons/model/enums/Enumerations.scala`
   - MappedBank database class: `obp-api/src/main/scala/code/model/dataAccess/MappedBank.scala`

6. **Field Name Conventions**: 
   - BankAttribute uses PascalCase for fields (e.g., `BankId_`, `Name`, `Type`, `Value`, `IsActive`)
   - The `BankId_` field has a trailing underscore to avoid conflict with the BankId case class

7. **Authorization Requirements**: All operations require appropriate entitlements:
   - canCreateBankAttribute (for POST)
   - canGetBankAttribute (for GET operations)
   - canUpdateBankAttribute (for PUT)
   - canDeleteBankAttribute (for DELETE)
