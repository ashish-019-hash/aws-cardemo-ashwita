# Business Rules Extraction

**Extracted From**: Bank Attribute Management User Story
**Analysis Date**: December 01, 2025
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 7
- API Endpoints Analyzed: 5
- Rule Categories:
  - Calculations: 0
  - Decisions: 4
  - Thresholds: 0
  - Aggregations: 0
  - Workflows: 1
  - Transformations: 2

## Business Rules Catalog

### BR-001: Bank Existence Validation for Attribute Operations

**Category**: DECISION

**Description**: Before any bank attribute operation (create, retrieve, update, delete), the system must verify that the specified bank exists in the system. All attribute operations are scoped to a specific bank.

**Source**: 
- File: BankAttributeService.scala (inferred from user story)
- Class/Object: BankAttributeService, BankService
- Method: validateBankExists (called before all attribute operations)
- Lines: N/A (derived from user story)

**Business Logic**:
1. Receive bank identifier (BANK_ID) from the request path
2. Query BankService to verify bank exists in the system
3. If bank exists, proceed with the attribute operation
4. If bank does not exist, return appropriate error response (404 Not Found)

**Variables**:
- **Input**: bankId (string) - Unique identifier for the bank
- **Output**: Validation result (success/failure) determining if operation can proceed
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank exists in system | Bank is registered and available for attribute management | Bank record found in repository |
| Bank does not exist | Invalid or unknown bank identifier | No bank record found |

**Business Impact**: 
This rule ensures data integrity by preventing orphaned attributes. All bank attributes must be associated with a valid, existing bank. This maintains referential integrity in the system and prevents data corruption.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/attribute - Define new bank attribute
- GET /banks/BANK_ID/attributes - Retrieve all bank attributes
- GET /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Retrieve single bank attribute
- PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Update bank attribute
- DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Delete bank attribute

**Related Test Cases**:
- Test case for valid bank ID allowing attribute operations
- Test case for invalid bank ID returning 404 error

**Migration Notes for Go**:
- Implement bank validation as a reusable middleware or helper function
- Use Go's error handling pattern with explicit error returns
- Consider caching bank existence checks for performance in high-volume scenarios

**Example Scenarios**:
```
Scenario 1: Valid bank identifier
Input: bankId = "bank-001"
Processing: Query BankService for bank-001, bank exists
Output: Proceed with attribute operation

Scenario 2: Invalid bank identifier
Input: bankId = "non-existent-bank"
Processing: Query BankService for non-existent-bank, bank not found
Output: Return 404 Not Found error, attribute operation blocked
```

---

### BR-002: Attribute Type Validation and Enforcement

**Category**: DECISION

**Description**: When creating or updating a bank attribute, the system must validate that the attribute type is one of the supported types: STRING, INTEGER, DOUBLE, or DATE_WITH_DAY. This ensures consistent data typing across all bank attributes.

**Source**: 
- File: BankAttributeService.scala (inferred from user story)
- Class/Object: BankAttributeService
- Method: createAttribute, updateAttribute
- Lines: N/A (derived from user story)

**Business Logic**:
1. Receive attribute type from request body
2. Validate type against supported types enumeration
3. Supported types: STRING, INTEGER, DOUBLE, DATE_WITH_DAY
4. If type is valid, proceed with create/update operation
5. If type is invalid, return validation error

**Variables**:
- **Input**: type (string) - Attribute type specification
- **Output**: Validation result determining if operation can proceed
- **Constants**: Supported types = [STRING, INTEGER, DOUBLE, DATE_WITH_DAY]

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| type = STRING | Text-based attribute value | Any text value allowed |
| type = INTEGER | Whole number attribute value | Numeric whole numbers only |
| type = DOUBLE | Decimal number attribute value | Numeric decimal values |
| type = DATE_WITH_DAY | Date attribute value | Date format (e.g., 2012-04-23) |

**Business Impact**: 
This rule enables flexible yet controlled attribute definitions. By supporting multiple data types, banks can define various operational parameters while maintaining data integrity through type enforcement.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/attribute - Define new bank attribute
- PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Update bank attribute

**Related Test Cases**:
- Test case for each valid attribute type (STRING, INTEGER, DOUBLE, DATE_WITH_DAY)
- Test case for invalid attribute type returning validation error

**Migration Notes for Go**:
- Define a custom type or const block for attribute types
- Use Go's type system to enforce valid types at compile time where possible
- Implement validation function that checks against allowed types

**Example Scenarios**:
```
Scenario 1: Valid STRING type
Input: type = "STRING"
Processing: Validate against supported types, STRING is valid
Output: Proceed with attribute creation/update

Scenario 2: Valid INTEGER type
Input: type = "INTEGER"
Processing: Validate against supported types, INTEGER is valid
Output: Proceed with attribute creation/update

Scenario 3: Invalid type
Input: type = "BOOLEAN"
Processing: Validate against supported types, BOOLEAN not in list
Output: Return validation error - unsupported attribute type
```

---

### BR-003: Type-Value Consistency Enforcement

**Category**: DECISION

**Description**: When creating or updating a bank attribute, the system must validate that the provided value matches the specified attribute type. This ensures data integrity by preventing type mismatches.

**Source**: 
- File: BankAttributeService.scala (inferred from user story)
- Class/Object: BankAttributeService
- Method: validateTypeValueConsistency
- Lines: N/A (derived from user story)

**Business Logic**:
1. Receive attribute type and value from request body
2. Based on the type, validate the value format:
   - STRING: Accept any text value
   - INTEGER: Value must be a valid whole number (e.g., 123, -456)
   - DOUBLE: Value must be a valid decimal number (e.g., 12.1234, -0.5)
   - DATE_WITH_DAY: Value must be a valid date format (e.g., 2012-04-23)
3. If value matches type, proceed with operation
4. If value does not match type, return validation error

**Variables**:
- **Input**: 
  - type (string) - Attribute type specification
  - value (string) - Attribute value to validate
- **Output**: Validation result (success/failure)
- **Constants**: Type-specific validation patterns

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| STRING type | Any text value accepted | No format restrictions |
| INTEGER type | Whole number required | Pattern: ^-?\d+$ |
| DOUBLE type | Decimal number required | Pattern: ^-?\d+\.?\d*$ |
| DATE_WITH_DAY type | Date format required | Pattern: YYYY-MM-DD |

**Business Impact**: 
This rule ensures that attribute values are semantically correct for their declared type. This enables downstream systems to reliably process attribute values without type conversion errors, supporting accurate business operations and reporting.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/attribute - Define new bank attribute
- PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Update bank attribute

**Related Test Cases**:
- Test case for STRING type with any text value
- Test case for INTEGER type with valid whole number
- Test case for INTEGER type with invalid value (e.g., "abc") returning error
- Test case for DOUBLE type with valid decimal number
- Test case for DOUBLE type with invalid value returning error
- Test case for DATE_WITH_DAY type with valid date format
- Test case for DATE_WITH_DAY type with invalid date format returning error

**Migration Notes for Go**:
- Implement type-specific validation functions
- Use Go's strconv package for numeric validation
- Use time.Parse for date validation with appropriate format
- Consider using a validation library or custom validators

**Example Scenarios**:
```
Scenario 1: Valid INTEGER value
Input: type = "INTEGER", value = "12345"
Processing: Validate "12345" as integer, valid whole number
Output: Proceed with attribute creation/update

Scenario 2: Invalid INTEGER value
Input: type = "INTEGER", value = "12.5"
Processing: Validate "12.5" as integer, contains decimal point
Output: Return validation error - value does not match INTEGER type

Scenario 3: Valid DOUBLE value
Input: type = "DOUBLE", value = "12.1234"
Processing: Validate "12.1234" as double, valid decimal number
Output: Proceed with attribute creation/update

Scenario 4: Valid DATE_WITH_DAY value
Input: type = "DATE_WITH_DAY", value = "2012-04-23"
Processing: Validate "2012-04-23" as date, valid format
Output: Proceed with attribute creation/update

Scenario 5: Invalid DATE_WITH_DAY value
Input: type = "DATE_WITH_DAY", value = "23-04-2012"
Processing: Validate "23-04-2012" as date, wrong format
Output: Return validation error - value does not match DATE_WITH_DAY format
```

---

### BR-004: Attribute Existence Validation for Single Operations

**Category**: DECISION

**Description**: When retrieving, updating, or deleting a specific bank attribute, the system must verify that the attribute exists. If the attribute does not exist, an appropriate error response must be returned.

**Source**: 
- File: BankAttributeService.scala (inferred from user story)
- Class/Object: BankAttributeService
- Method: getAttributeById, updateAttribute, deleteAttribute
- Lines: N/A (derived from user story)

**Business Logic**:
1. Receive bank attribute identifier (BANK_ATTRIBUTE_ID) from the request path
2. Query the attribute repository to check if attribute exists for the specified bank
3. If attribute exists, proceed with the operation (retrieve/update/delete)
4. If attribute does not exist, return 404 Not Found error response

**Variables**:
- **Input**: 
  - bankId (string) - Bank identifier
  - bankAttributeId (string) - Attribute identifier
- **Output**: Attribute details or error response
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Attribute exists | Attribute is registered for the bank | Attribute record found |
| Attribute does not exist | Invalid or unknown attribute identifier | No attribute record found |

**Business Impact**: 
This rule ensures proper error handling when clients request operations on non-existent attributes. It provides clear feedback to API consumers and prevents silent failures in attribute management operations.

**API Endpoints Using This Rule**:
- GET /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Retrieve single bank attribute
- PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Update bank attribute
- DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Delete bank attribute

**Related Test Cases**:
- Test case for valid attribute ID returning attribute details
- Test case for invalid attribute ID returning 404 error

**Migration Notes for Go**:
- Implement using Go's error handling pattern
- Return appropriate HTTP status codes (200 for success, 404 for not found)
- Consider using a custom error type for attribute not found scenarios

**Example Scenarios**:
```
Scenario 1: Valid attribute identifier
Input: bankId = "bank-001", bankAttributeId = "attr-001"
Processing: Query repository for attr-001 under bank-001, attribute exists
Output: Return attribute details with status 200 OK

Scenario 2: Invalid attribute identifier
Input: bankId = "bank-001", bankAttributeId = "non-existent-attr"
Processing: Query repository for non-existent-attr, attribute not found
Output: Return error response with status 404 Not Found
```

---

### BR-005: Empty Result Handling for Attribute List Retrieval

**Category**: TRANSFORMATION

**Description**: When retrieving all attributes for a bank, if the bank has no attributes defined, the system must return an empty list with HTTP 200 status, not a 404 error. This distinguishes between "no attributes configured" (valid state) and "resource not found" (error state).

**Source**: 
- File: BankAttributeService.scala (inferred from user story)
- Class/Object: BankAttributeService
- Method: getAllAttributesForBank
- Lines: N/A (derived from user story)

**Business Logic**:
1. Query attribute repository for all attributes associated with the bank
2. If attributes found, return list of attributes with HTTP 200 OK
3. If no attributes found, return empty list with HTTP 200 OK
4. Do not return 404 Not Found for empty results

**Variables**:
- **Input**: bankId (string) - Bank identifier
- **Output**: List of bank attributes (may be empty)
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank has attributes | Bank has custom parameters defined | Non-empty attributes array |
| Bank has no attributes | Bank exists but no custom parameters | Empty attributes array [] |

**Business Impact**: 
This rule ensures consistent API behavior and proper semantic meaning of HTTP status codes. A bank with no attributes is a valid business state (e.g., newly registered bank), while 404 should be reserved for truly missing resources.

**API Endpoints Using This Rule**:
- GET /banks/BANK_ID/attributes - Retrieve all bank attributes

**Related Test Cases**:
- Test case for bank with attributes returning populated list
- Test case for bank with no attributes returning empty list with 200 status

**Migration Notes for Go**:
- Ensure the handler returns 200 OK even when the attribute slice is empty
- Initialize the response slice to avoid nil pointer issues
- Use json.Marshal with empty slice to produce "[]" not "null"

**Example Scenarios**:
```
Scenario 1: Bank with attributes
Input: bankId = "bank-001"
Processing: Query repository, find 3 attributes
Output: HTTP 200 OK with body {"bank_attributes": [{...}, {...}, {...}]}

Scenario 2: Bank with no attributes
Input: bankId = "bank-002"
Processing: Query repository, no attributes found
Output: HTTP 200 OK with body {"bank_attributes": []}
```

---

### BR-006: Attribute Active/Inactive Status Management

**Category**: WORKFLOW

**Description**: Bank attributes support an active/inactive status flag that allows soft deactivation of attributes without permanent deletion. This enables banks to temporarily disable attributes while preserving the data for potential reactivation.

**Source**: 
- File: BankAttributeService.scala (inferred from user story)
- Class/Object: BankAttributeService
- Method: createAttribute, updateAttribute
- Lines: N/A (derived from user story)

**Business Logic**:
1. When creating an attribute, accept is_active flag (boolean)
2. When updating an attribute, allow changing is_active flag
3. Active attributes (is_active = true) are operational and in use
4. Inactive attributes (is_active = false) are soft-deactivated but preserved
5. Both active and inactive attributes are returned in retrieval operations
6. DELETE operation permanently removes attribute (use is_active=false for soft deletion)

**Variables**:
- **Input**: is_active (boolean) - Attribute status flag
- **Output**: Attribute with updated status
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| is_active = true | Attribute is operational | Active status |
| is_active = false | Attribute is soft-deactivated | Inactive status |

**Business Impact**: 
This rule provides flexibility in attribute lifecycle management. Banks can deactivate attributes temporarily (e.g., during maintenance, policy changes) without losing historical data. This supports audit trails and enables easy reactivation when needed.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/attribute - Define new bank attribute with status
- PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Update attribute status
- GET /banks/BANK_ID/attributes - Returns all attributes regardless of status
- GET /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Returns attribute with status

**Related Test Cases**:
- Test case for creating attribute with is_active = true
- Test case for creating attribute with is_active = false
- Test case for updating attribute status from active to inactive
- Test case for updating attribute status from inactive to active

**Migration Notes for Go**:
- Use Go's bool type for is_active field
- Ensure JSON marshaling handles boolean correctly
- Consider adding filtering options for active-only retrieval in future iterations

**Example Scenarios**:
```
Scenario 1: Create active attribute
Input: name = "max_daily_limit", type = "INTEGER", value = "10000", is_active = true
Processing: Create attribute with active status
Output: Attribute created and operational

Scenario 2: Soft-deactivate attribute
Input: bankAttributeId = "attr-001", is_active = false
Processing: Update attribute status to inactive
Output: Attribute preserved but marked as inactive

Scenario 3: Reactivate attribute
Input: bankAttributeId = "attr-001", is_active = true
Processing: Update attribute status to active
Output: Attribute reactivated and operational
```

---

### BR-007: Complete Attribute Information Composition

**Category**: TRANSFORMATION

**Description**: When creating or updating a bank attribute, the system must compose and return complete attribute information including all fields: bank_id, bank_attribute_id, name, type, value, and is_active status.

**Source**: 
- File: BankAttributeService.scala (inferred from user story)
- Class/Object: BankAttributeService
- Method: createAttribute, updateAttribute, getAttributeById
- Lines: N/A (derived from user story)

**Business Logic**:
1. For create operations: Generate unique bank_attribute_id
2. Associate attribute with the specified bank_id
3. Store attribute details (name, type, value, is_active)
4. Return complete attribute object with all fields populated
5. For update operations: Preserve bank_id and bank_attribute_id, update other fields
6. For retrieval operations: Return complete attribute object

**Variables**:
- **Input**: 
  - bankId (string) - Bank identifier from path
  - name (string) - Attribute name
  - type (string) - Attribute type
  - value (string) - Attribute value
  - is_active (boolean) - Attribute status
- **Output**: Complete attribute object:
  - bank_id: string
  - bank_attribute_id: string (generated for create)
  - name: string
  - type: string
  - value: string
  - is_active: boolean
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Create operation | New attribute with generated ID | bank_attribute_id auto-generated |
| Update operation | Existing attribute modified | bank_attribute_id preserved |

**Business Impact**: 
This rule ensures API consumers receive complete attribute information in responses, enabling them to display and process attribute data without additional API calls. The consistent response structure supports reliable client-side processing.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/attribute - Returns created attribute with all fields
- PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Returns updated attribute with all fields
- GET /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Returns attribute with all fields
- GET /banks/BANK_ID/attributes - Returns list of attributes, each with all fields

**Related Test Cases**:
- Test case for create returning complete attribute object
- Test case for update returning complete attribute object
- Test case for retrieve returning complete attribute object

**Migration Notes for Go**:
- Use Go struct to model the attribute response with all fields
- Implement proper JSON marshaling for the response structure
- Use UUID or similar for generating unique bank_attribute_id values

**Example Scenarios**:
```
Scenario 1: Create new attribute
Input: bankId = "bank-001", name = "max_transaction_limit", type = "DOUBLE", value = "50000.00", is_active = true
Processing: Generate unique ID, associate with bank, store all fields
Output: {
  "bank_id": "bank-001",
  "bank_attribute_id": "attr-uuid-12345",
  "name": "max_transaction_limit",
  "type": "DOUBLE",
  "value": "50000.00",
  "is_active": true
}

Scenario 2: Update existing attribute
Input: bankId = "bank-001", bankAttributeId = "attr-uuid-12345", value = "75000.00"
Processing: Preserve IDs, update value field
Output: {
  "bank_id": "bank-001",
  "bank_attribute_id": "attr-uuid-12345",
  "name": "max_transaction_limit",
  "type": "DOUBLE",
  "value": "75000.00",
  "is_active": true
}
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /banks/BANK_ID/attribute | POST | Bank existence validation, Type validation, Type-value consistency, Status management, Complete info composition | BR-001, BR-002, BR-003, BR-006, BR-007 |
| /banks/BANK_ID/attributes | GET | Bank existence validation, Empty result handling | BR-001, BR-005 |
| /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID | GET | Bank existence validation, Attribute existence validation, Complete info composition | BR-001, BR-004, BR-007 |
| /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID | PUT | Bank existence validation, Attribute existence validation, Type validation, Type-value consistency, Status management, Complete info composition | BR-001, BR-002, BR-003, BR-004, BR-006, BR-007 |
| /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID | DELETE | Bank existence validation, Attribute existence validation | BR-001, BR-004 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestBankExistenceValidation | Pending | Pending |
| BR-002 | TestAttributeTypeValidation | Pending | Pending |
| BR-003 | TestTypeValueConsistency | Pending | Pending |
| BR-004 | TestAttributeExistenceValidation | Pending | Pending |
| BR-005 | TestEmptyAttributeListHandling | Pending | Pending |
| BR-006 | TestAttributeStatusManagement | Pending | Pending |
| BR-007 | TestCompleteAttributeComposition | Pending | Pending |

## Notes and Assumptions

1. **Source Code Assumption**: Since the user story does not include actual Scala source code, business rules were derived from the documented business logic, acceptance criteria, and technical context in the user story.

2. **Service Layer Inference**: The BankAttributeService and BankService classes are inferred from the user story's technical context section.

3. **Authentication/Authorization Exclusion**: Authentication and authorization logic (canCreateBankAttribute, canGetBankAttribute, canUpdateBankAttribute, canDeleteBankAttribute entitlements) are mentioned as dependencies but not extracted as business rules since they are cross-cutting security concerns handled separately.

4. **Validation Rules Exclusion**: Basic input validation rules (e.g., "attribute name must be non-empty") are excluded as they are technical validations, not business rules per the extraction guidelines. However, type-value consistency (BR-003) is included as it represents a business constraint on data integrity.

5. **Unique Attribute Names**: The user story mentions "Attribute names should be unique within a bank's attribute set" as a business rule. This is noted but not extracted as a separate rule since it's more of a data constraint that would be enforced at the database level.

6. **DELETE vs Soft Delete**: The user story clarifies that DELETE permanently removes attributes while is_active=false provides soft deletion. This distinction is captured in BR-006.

7. **Questions for SME**: The user story includes questions about attribute name restrictions, limits on attributes per bank, and DATE_WITH_DAY format specifics. These should be clarified before Go implementation.
