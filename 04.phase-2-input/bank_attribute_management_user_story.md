User story: Bank Attribute Management

## Story Overview
**As a** Bank Administrator  
**I want to** define and manage custom bank-level attributes for extended metadata  
**So that** I can capture and maintain additional bank-specific information beyond standard fields

## Acceptance Criteria
1. System must allow defining new custom bank-level attributes with name, type, and value
2. System must support multiple attribute types (STRING, INTEGER, DOUBLE, DATE_WITH_DAY)
3. System must allow managing (updating) existing bank-level attributes
4. System must allow retrieving all bank attributes for a specific bank
5. System must allow retrieving a single bank attribute by its identifier
6. System must allow deleting bank attributes
7. System must validate attribute type matches the value provided
8. System must associate attributes with specific banks
9. System must support active/inactive status for attributes

## Technical Context
- **Classes/Services Involved**: 
  - BankAttributeService: Handles creation, retrieval, update, and deletion of bank attributes
  - BankService: Validates bank existence
  - BankAttributeRepository: Data persistence for bank attributes
- **Input Data**: 
  - Bank identifier (bankId) in URL path
  - Attribute details (name, type, value, is_active) in request body (for create/update)
  - Attribute identifier (for retrieve single, update, delete)
- **Output Data**: 
  - Created/updated bank attribute details
  - Retrieved bank attribute(s)
  - Deletion confirmation
  - Validation error messages
- **Processing Type**: REST API

## Relevant Endpoints

### 1. Define Bank Attribute
- **Endpoint**: POST /banks/BANK_ID/attribute
  - **Purpose**: Define (create) a new custom bank-level attribute for extended metadata
  - **Request**: 
    - Path parameter: BANK_ID (bank identifier)
    - Body:
    ```json
    {
      "name": "string",
      "type": "STRING|INTEGER|DOUBLE|DATE_WITH_DAY",
      "value": "string",
      "is_active": "boolean"
    }
    ```
  - **Response**: 
    ```json
    {
      "bank_id": "string",
      "bank_attribute_id": "string",
      "name": "string",
      "type": "STRING|INTEGER|DOUBLE|DATE_WITH_DAY",
      "value": "string",
      "is_active": "boolean"
    }
    ```

### 2. Manage Bank Attribute
- **Endpoint**: PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID
  - **Purpose**: Manage (update) an existing custom bank-level attribute
  - **Request**: 
    - Path parameters: BANK_ID (bank identifier), BANK_ATTRIBUTE_ID (attribute identifier)
    - Body:
    ```json
    {
      "name": "string",
      "type": "STRING|INTEGER|DOUBLE|DATE_WITH_DAY",
      "value": "string",
      "is_active": "boolean"
    }
    ```
  - **Response**: 
    ```json
    {
      "bank_id": "string",
      "bank_attribute_id": "string",
      "name": "string",
      "type": "STRING|INTEGER|DOUBLE|DATE_WITH_DAY",
      "value": "string",
      "is_active": "boolean"
    }
    ```

### 3. Retrieve All Bank Attributes
- **Endpoint**: GET /banks/BANK_ID/attributes
  - **Purpose**: Retrieve all custom bank-level attributes for a specific bank
  - **Request**: 
    - Path parameter: BANK_ID (bank identifier)
  - **Response**: 
    ```json
    {
      "bank_attributes": [
        {
          "bank_id": "string",
          "bank_attribute_id": "string",
          "name": "string",
          "type": "STRING|INTEGER|DOUBLE|DATE_WITH_DAY",
          "value": "string",
          "is_active": "boolean"
        }
      ]
    }
    ```

### 4. Retrieve Single Bank Attribute
- **Endpoint**: GET /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID
  - **Purpose**: Retrieve a specific custom bank-level attribute by its identifier
  - **Request**: 
    - Path parameters: BANK_ID (bank identifier), BANK_ATTRIBUTE_ID (attribute identifier)
  - **Response**: 
    ```json
    {
      "bank_id": "string",
      "bank_attribute_id": "string",
      "name": "string",
      "type": "STRING|INTEGER|DOUBLE|DATE_WITH_DAY",
      "value": "string",
      "is_active": "boolean"
    }
    ```

### 5. Delete Bank Attribute
- **Endpoint**: DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID
  - **Purpose**: Delete a custom bank-level attribute
  - **Request**: 
    - Path parameters: BANK_ID (bank identifier), BANK_ATTRIBUTE_ID (attribute identifier)
  - **Response**: 
    - HTTP 204 No Content (successful deletion)

**Note**: All CRUD operations (Create, Read, Update, Delete) are included for complete bank attribute management. The POST endpoint defines new attributes, PUT manages/updates existing attributes, GET endpoints retrieve attributes, and DELETE removes attributes.

## Business Rules

1. **Valid Bank Identifier**: Bank identifier must exist in the system before defining, retrieving, or managing attributes
2. **Valid Attribute Type**: Attribute type must be one of: STRING, INTEGER, DOUBLE, DATE_WITH_DAY
3. **Type-Value Consistency**: Attribute value must match the specified type (e.g., INTEGER type must have numeric value)
4. **Unique Attribute Names**: Attribute names should be unique within a bank's attribute set
5. **Existing Attribute for Updates**: Bank attribute must exist before it can be managed/updated
6. **Existing Attribute for Retrieval**: Bank attribute must exist to retrieve single attribute details (return 404 if not found)
7. **Existing Attribute for Deletion**: Bank attribute must exist before it can be deleted
8. **Empty List for No Attributes**: Retrieving all attributes should return empty list if bank has no attributes (not 404)

## Data Validations

- Bank identifier must be provided and exist in the system (for all operations)
- For create/update operations:
  - Attribute name must be provided and non-empty
  - Attribute type must be one of the supported types: STRING, INTEGER, DOUBLE, DATE_WITH_DAY
  - Attribute value must match the specified type:
    - STRING: any text value
    - INTEGER: whole number (e.g., 123)
    - DOUBLE: decimal number (e.g., 12.1234)
    - DATE_WITH_DAY: date format (e.g., 2012-04-23)
- For update/retrieve single/delete operations: Bank attribute ID must be provided and exist
- For retrieve all operation: No additional validation required (returns empty list if no attributes)

## Dependencies

- Authentication service (to verify user permissions for creating, retrieving, updating, and deleting bank attributes)
- Bank service (to validate bank existence)
- Authorization service (to check entitlements: canCreateBankAttribute, canGetBankAttribute, canUpdateBankAttribute, canDeleteBankAttribute)

## Notes for Implementation

### Special Considerations
- Attributes provide extended metadata beyond standard bank fields
- Support for multiple data types enables flexible attribute definitions
- Active/inactive status allows soft deactivation without deletion
- Type validation ensures data integrity for typed attributes
- GET /banks/BANK_ID/attributes returns all attributes for a bank (empty list if none exist)
- GET /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID returns single attribute (404 if not found)
- DELETE operation permanently removes the attribute (use is_active=false for soft deletion)
- All operations require appropriate entitlements (canCreateBankAttribute, canGetBankAttribute, canUpdateBankAttribute, canDeleteBankAttribute)

### Questions for SME
1. Are there any restrictions on attribute names (e.g., reserved keywords, naming conventions)?
2. Should there be a limit on the number of attributes per bank?
3. Can multiple attributes have the same name for a single bank?
4. What are the specific validation rules for DATE_WITH_DAY format?
