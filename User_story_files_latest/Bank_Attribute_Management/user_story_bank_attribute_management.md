# User Story for Bank Attribute Management

## Story Overview

**As a** Bank Administrator or API Consumer with appropriate entitlements
**I want to** manage custom attributes associated with banks for extended metadata storage
**So that** I can store and maintain additional bank-specific metadata beyond the standard bank information, enabling flexible data extension for regulatory reporting, custom identifiers, and business-specific requirements

## Acceptance Criteria

1. Authenticated users with `canCreateBankAttribute` entitlement can create new bank attributes with name, type, value, and active status
2. Authenticated users with `canGetBankAttribute` entitlement can retrieve all attributes for a specific bank
3. Authenticated users with `canGetBankAttribute` entitlement can retrieve a specific bank attribute by its ID
4. Authenticated users with `canUpdateBankAttribute` entitlement can update existing bank attributes
5. Authenticated users with `canDeleteBankAttribute` entitlement can delete bank attributes
6. Authenticated users with `canCreateBankAttributeDefinitionAtOneBank` entitlement can create or update bank attribute definitions
7. Bank attribute types must be one of: STRING, INTEGER, DOUBLE, or DATE_WITH_DAY
8. All operations require valid authentication and appropriate role-based entitlements
9. Bank must exist before attributes can be created for it
10. Attribute values are validated against their defined types

## Technical Context

### Classes/Services Involved
- **BankAttribute.scala**: Core trait and provider interface defining bank attribute operations
- **MappedBankAttributeProvider.scala**: Implementation of bank attribute persistence using Lift Mapper
- **BankAttributeX**: Dependency injection container for bank attribute provider
- **APIMethods400.scala**: REST API endpoint definitions for bank attribute operations
- **JSONFactory4.0.0.scala**: JSON serialization/deserialization for bank attribute data
- **NewStyle.function**: Service layer functions for bank attribute operations

### Input Data
- **Bank ID**: Unique identifier for the bank (path parameter)
- **Bank Attribute ID**: Unique identifier for the attribute (path parameter for specific operations)
- **BankAttributeJsonV400**: Request body containing:
  - `name`: String (max 50 characters) - Attribute name
  - `type`: String - One of STRING, INTEGER, DOUBLE, DATE_WITH_DAY
  - `value`: String (max 255 characters) - Attribute value
  - `is_active`: Boolean (optional, defaults to true) - Active status

### Output Data
- **BankAttributeResponseJsonV400**: Single attribute response containing:
  - `bank_id`: String - Bank identifier
  - `bank_attribute_id`: String - Unique attribute identifier
  - `name`: String - Attribute name
  - `type`: String - Attribute type
  - `value`: String - Attribute value
  - `is_active`: Boolean - Active status
- **BankAttributesResponseJsonV400**: List of bank attributes for bulk retrieval

### Processing Type
- API (REST/HTTP request-response)
- On-demand processing with low volume

## Relevant Endpoints

**Note**: The BRD description states "Manage custom attributes associated with banks for extended metadata storage". While "manage" strictly implies update/configure operations, the user has requested to consider all functions with exact endpoints present in the Scala application for migration purposes. Therefore, all existing endpoints are documented below.

### Endpoint 1: Create Bank Attribute
- **Endpoint**: `POST /banks/BANK_ID/attribute`
- **Justification (from description)**: "Manage custom attributes" - creating attributes is part of managing the attribute lifecycle for extended metadata storage
- **Purpose**: Create a new custom attribute for a bank to store extended metadata
- **Request**: 
  ```json
  {
    "name": "ISIN",
    "type": "STRING",
    "value": "XS1234567890",
    "is_active": true
  }
  ```
- **Response**: 
  ```json
  {
    "bank_id": "gh.29.uk",
    "bank_attribute_id": "7uy8a7e4-6d02-40e3-a129-0b2bf89de8uh",
    "name": "ISIN",
    "type": "STRING",
    "value": "XS1234567890",
    "is_active": true
  }
  ```
- **Required Entitlement**: `canCreateBankAttribute`
- **HTTP Status**: 201 Created

### Endpoint 2: Get Bank Attributes
- **Endpoint**: `GET /banks/BANK_ID/attributes`
- **Justification (from description)**: "Manage custom attributes" - retrieving attributes is necessary to manage and verify stored metadata
- **Purpose**: Retrieve all custom attributes associated with a specific bank
- **Request**: None (path parameter only)
- **Response**: 
  ```json
  {
    "bank_attributes": [
      {
        "bank_id": "gh.29.uk",
        "bank_attribute_id": "7uy8a7e4-6d02-40e3-a129-0b2bf89de8uh",
        "name": "ISIN",
        "type": "STRING",
        "value": "XS1234567890",
        "is_active": true
      }
    ]
  }
  ```
- **Required Entitlement**: `canGetBankAttribute`
- **HTTP Status**: 200 OK

### Endpoint 3: Get Bank Attribute By ID
- **Endpoint**: `GET /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID`
- **Justification (from description)**: "Manage custom attributes" - retrieving a specific attribute is necessary to manage individual metadata entries
- **Purpose**: Retrieve a specific bank attribute by its unique identifier
- **Request**: None (path parameters only)
- **Response**: 
  ```json
  {
    "bank_id": "gh.29.uk",
    "bank_attribute_id": "7uy8a7e4-6d02-40e3-a129-0b2bf89de8uh",
    "name": "ISIN",
    "type": "STRING",
    "value": "XS1234567890",
    "is_active": true
  }
  ```
- **Required Entitlement**: `canGetBankAttribute`
- **HTTP Status**: 200 OK

### Endpoint 4: Update Bank Attribute
- **Endpoint**: `PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID`
- **Justification (from description)**: "Manage custom attributes" - updating is a core management operation for maintaining metadata
- **Purpose**: Update an existing bank attribute's name, type, value, or active status
- **Request**: 
  ```json
  {
    "name": "ISIN",
    "type": "STRING",
    "value": "XS0987654321",
    "is_active": true
  }
  ```
- **Response**: 
  ```json
  {
    "bank_id": "gh.29.uk",
    "bank_attribute_id": "7uy8a7e4-6d02-40e3-a129-0b2bf89de8uh",
    "name": "ISIN",
    "type": "STRING",
    "value": "XS0987654321",
    "is_active": true
  }
  ```
- **Required Entitlement**: `canUpdateBankAttribute`
- **HTTP Status**: 200 OK

### Endpoint 5: Delete Bank Attribute
- **Endpoint**: `DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID`
- **Justification (from description)**: "Manage custom attributes" - deletion is part of the attribute lifecycle management for metadata storage
- **Purpose**: Remove a bank attribute from the system
- **Request**: None (path parameters only)
- **Response**: Empty body
- **Required Entitlement**: `canDeleteBankAttribute`
- **HTTP Status**: 204 No Content

### Endpoint 6: Create or Update Bank Attribute Definition
- **Endpoint**: `PUT /banks/BANK_ID/attribute-definitions/bank`
- **Justification (from description)**: "Manage custom attributes" - defining attribute schemas is essential for managing structured metadata storage
- **Purpose**: Create or update the definition/schema for bank attributes including type constraints and visibility settings
- **Request**: 
  ```json
  {
    "name": "ISIN",
    "category": "Bank",
    "type": "STRING",
    "description": "International Securities Identification Number",
    "alias": "isin_code",
    "can_be_seen_on_views": ["owner", "accountant"],
    "is_active": true
  }
  ```
- **Response**: 
  ```json
  {
    "attribute_definition_id": "9ca9a7e4-6d02-40e3-a129-0b2bf89de9b1",
    "bank_id": "gh.29.uk",
    "name": "ISIN",
    "category": "Bank",
    "type": "STRING",
    "description": "International Securities Identification Number",
    "alias": "isin_code",
    "can_be_seen_on_views": ["owner", "accountant"],
    "is_active": true
  }
  ```
- **Required Entitlement**: `canCreateBankAttributeDefinitionAtOneBank`
- **HTTP Status**: 201 Created

## Business Rules

1. **Bank Existence**: A bank must exist in the system before attributes can be created for it
2. **Attribute Type Validation**: The `type` field must be one of the following valid types:
   - `STRING`: Text values (e.g., "TAX_NUMBER")
   - `INTEGER`: Whole number values (e.g., 123)
   - `DOUBLE`: Decimal number values (e.g., 12.1234)
   - `DATE_WITH_DAY`: Date values in format (e.g., "2012-04-23")
3. **Unique Attribute ID**: Each bank attribute is assigned a unique UUID upon creation
4. **Active Status Default**: If `is_active` is not provided, it defaults to `true`
5. **Role-Based Access Control**: Each operation requires specific entitlements:
   - Create: `canCreateBankAttribute`
   - Read: `canGetBankAttribute`
   - Update: `canUpdateBankAttribute`
   - Delete: `canDeleteBankAttribute`
   - Definition Management: `canCreateBankAttributeDefinitionAtOneBank`
6. **Attribute Name Length**: Attribute names are limited to 50 characters
7. **Attribute Value Length**: Attribute values are limited to 255 characters

## Data Validations

- **Authentication Required**: All endpoints require valid user authentication
- **Bank ID Validation**: Bank ID must correspond to an existing bank in the system
- **Attribute ID Validation**: For update/delete operations, the attribute ID must exist
- **JSON Format Validation**: Request body must be valid JSON matching the expected schema
- **Type Field Validation**: Type must be one of STRING, INTEGER, DOUBLE, or DATE_WITH_DAY
- **Category Validation**: For attribute definitions, category must be "Bank"
- **Entitlement Check**: User must have the required entitlement for the specific operation

### Error Conditions
- `UserNotLoggedIn`: User is not authenticated
- `BankNotFound`: Specified bank does not exist
- `InvalidJsonFormat`: Request body is not valid JSON or doesn't match expected schema
- `UserHasMissingRoles`: User lacks required entitlement for the operation
- `UnknownError`: Unexpected system error

## Dependencies

### Upstream
- **Bank Management**: Bank entity must exist before attributes can be created
- **User Authentication**: Valid OAuth token or direct login credentials required
- **Entitlement System**: User must have appropriate roles/entitlements granted

### Downstream
- **Bank Information Retrieval**: Bank attributes are included when retrieving bank details via `GET /banks/BANK_ID`
- **Reporting Systems**: Bank attributes may be used in regulatory reporting and analytics
- **Third-Party Applications**: API consumers can use attributes for custom integrations

### External Systems
- **Authentication Provider**: OAuth 2.0 / OpenID Connect for user authentication
- **Database**: Persistent storage for bank attribute records (via Lift Mapper ORM)

## Notes for Implementation

### Special Considerations
1. **Attribute Type Enumeration**: The Go implementation should define a proper enum for BankAttributeType with values: STRING, INTEGER, DOUBLE, DATE_WITH_DAY
2. **UUID Generation**: Bank attribute IDs should be generated as UUIDs for uniqueness
3. **Soft Delete vs Hard Delete**: Current implementation uses hard delete (bulkDelete); consider if soft delete is needed for audit purposes
4. **Concurrent Updates**: The createOrUpdate pattern handles both create and update in a single method based on whether attributeId is provided
5. **Index Optimization**: Database index on BankId field for efficient attribute retrieval by bank

### Known Complexity
1. **Type Coercion**: Values are stored as strings but should be validated against their declared type
2. **Attribute Definition vs Attribute**: Two related but distinct concepts - definitions define the schema, attributes store actual values

### Missing or Unclear Requirements (Needs SME Input)
1. **Attribute Uniqueness**: Should attribute names be unique per bank? Current implementation allows duplicates
2. **Value Validation**: Should attribute values be validated against their type at creation/update time?
3. **Audit Trail**: Is there a requirement to track who created/modified attributes and when?
4. **Cascading Delete**: What happens to attributes when a bank is deleted?
5. **Attribute Versioning**: Is there a need to maintain historical versions of attribute values?

## Quality Checklist

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Bank Administrator / API Consumer)
- [x] Business value is stated (flexible metadata extension)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged for SME review
- [x] All relevant endpoints from Scala application are included
- [x] All details align with the capability description provided
- [x] Each endpoint is justified based on the "manage" verb in the description
