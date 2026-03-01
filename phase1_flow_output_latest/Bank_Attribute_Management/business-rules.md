# Business Rules Extraction

**Extracted From**: Bank Attribute Management Capability (Scala Application)
**Analysis Date**: 2026-01-20
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 12
- API Endpoints Analyzed: 6
- Rule Categories:
  - Calculations: 0
  - Decisions: 4
  - Thresholds: 2
  - Aggregations: 0
  - Workflows: 3
  - Transformations: 3

## Business Rules Catalog

### BR-001: Bank Existence Validation for Attribute Operations

**Category**: DECISION

**Description**: Before any bank attribute can be created, updated, or managed, the system must verify that the target bank exists in the system. This ensures data integrity by preventing orphaned attributes.

**Source**: 
- File: BankAttribute.scala, APIMethods400.scala
- Class/Object: BankAttribute trait, APIMethods400
- Method: createOrUpdateBankAttribute, getBankAttribute
- Lines: Referenced in user story acceptance criteria

**Business Logic**:
1. When a bank attribute operation is requested, extract the Bank ID from the request path
2. Query the bank repository to verify the bank exists
3. If bank does not exist, reject the operation with BankNotFound error
4. If bank exists, proceed with the attribute operation

**Variables**:
- **Input**: bankId (String) - Unique identifier for the bank
- **Output**: Boolean - Bank existence status
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank exists | Bank is registered in system | Valid bank ID in database |
| Bank not found | Bank is not registered | Invalid or non-existent bank ID |

**Business Impact**: 
Prevents creation of orphaned bank attributes that would have no parent bank entity, maintaining referential integrity in the banking metadata system.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/attribute - Create Bank Attribute
- GET /banks/BANK_ID/attributes - Get Bank Attributes
- GET /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Get Bank Attribute By ID
- PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Update Bank Attribute
- DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Delete Bank Attribute
- PUT /banks/BANK_ID/attribute-definitions/bank - Create or Update Bank Attribute Definition

**Related Test Cases**:
- Test bank attribute creation with valid bank ID
- Test bank attribute creation with invalid bank ID (should fail)

**Migration Notes for Go**:
- Implement bank existence check as a middleware or service layer validation
- Use Go's error handling pattern to return appropriate error types
- Consider caching bank existence checks for performance

**Example Scenarios**:
```
Scenario 1: Valid bank exists
Input: bankId = "gh.29.uk"
Processing: Query bank repository for "gh.29.uk"
Output: Bank found, proceed with attribute operation

Scenario 2: Bank does not exist
Input: bankId = "invalid.bank.id"
Processing: Query bank repository for "invalid.bank.id"
Output: BankNotFound error returned
```

---

### BR-002: Attribute Type Validation

**Category**: DECISION

**Description**: Bank attribute types must conform to a predefined set of valid types. This ensures consistent data typing across all bank attributes and enables proper value validation and storage.

**Source**: 
- File: BankAttribute.scala, JSONFactory4.0.0.scala
- Class/Object: BankAttributeType enum
- Method: createOrUpdateBankAttribute
- Lines: Referenced in user story acceptance criteria

**Business Logic**:
1. When creating or updating a bank attribute, extract the type field from the request
2. Validate that the type is one of the allowed values: STRING, INTEGER, DOUBLE, DATE_WITH_DAY
3. If type is invalid, reject the operation with validation error
4. If type is valid, proceed with attribute creation/update

**Variables**:
- **Input**: type (String) - Attribute type from request
- **Output**: Boolean - Type validation result
- **Constants**: Valid types = [STRING, INTEGER, DOUBLE, DATE_WITH_DAY]

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| type == "STRING" | Text-based attribute | Stores text values |
| type == "INTEGER" | Whole number attribute | Stores integer values |
| type == "DOUBLE" | Decimal number attribute | Stores floating-point values |
| type == "DATE_WITH_DAY" | Date attribute | Stores date values (YYYY-MM-DD) |

**Business Impact**: 
Ensures data consistency and enables type-specific validation and processing of attribute values. Supports regulatory reporting requirements that may need specific data types.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/attribute - Create Bank Attribute
- PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Update Bank Attribute
- PUT /banks/BANK_ID/attribute-definitions/bank - Create or Update Bank Attribute Definition

**Related Test Cases**:
- Test attribute creation with each valid type
- Test attribute creation with invalid type (should fail)

**Migration Notes for Go**:
- Define a Go enum (const with iota or string constants) for BankAttributeType
- Implement type validation in the request parsing layer
- Consider using Go's type system to enforce valid types at compile time

**Example Scenarios**:
```
Scenario 1: Valid STRING type
Input: type = "STRING"
Processing: Validate against allowed types
Output: Validation passes, attribute created

Scenario 2: Invalid type
Input: type = "BOOLEAN"
Processing: Validate against allowed types
Output: Validation fails, error returned
```

---

### BR-003: Role-Based Access Control for Attribute Operations

**Category**: DECISION

**Description**: Each bank attribute operation requires specific entitlements. Users must have the appropriate role-based entitlement to perform create, read, update, or delete operations on bank attributes.

**Source**: 
- File: APIMethods400.scala
- Class/Object: APIMethods400
- Method: Various endpoint handlers
- Lines: Referenced in user story acceptance criteria

**Business Logic**:
1. When a bank attribute operation is requested, identify the operation type
2. Check if the authenticated user has the required entitlement for that operation
3. If user lacks entitlement, reject with UserHasMissingRoles error
4. If user has entitlement, proceed with the operation

**Variables**:
- **Input**: userId (String), operationType (String)
- **Output**: Boolean - Authorization result
- **Constants**: 
  - Create: canCreateBankAttribute
  - Read: canGetBankAttribute
  - Update: canUpdateBankAttribute
  - Delete: canDeleteBankAttribute
  - Definition: canCreateBankAttributeDefinitionAtOneBank

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| hasEntitlement("canCreateBankAttribute") | User can create attributes | Required for POST |
| hasEntitlement("canGetBankAttribute") | User can read attributes | Required for GET |
| hasEntitlement("canUpdateBankAttribute") | User can update attributes | Required for PUT |
| hasEntitlement("canDeleteBankAttribute") | User can delete attributes | Required for DELETE |
| hasEntitlement("canCreateBankAttributeDefinitionAtOneBank") | User can manage definitions | Required for definition PUT |

**Business Impact**: 
Enforces security and compliance by ensuring only authorized users can manage bank metadata. Supports audit requirements and separation of duties in banking operations.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/attribute - Requires canCreateBankAttribute
- GET /banks/BANK_ID/attributes - Requires canGetBankAttribute
- GET /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Requires canGetBankAttribute
- PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Requires canUpdateBankAttribute
- DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Requires canDeleteBankAttribute
- PUT /banks/BANK_ID/attribute-definitions/bank - Requires canCreateBankAttributeDefinitionAtOneBank

**Related Test Cases**:
- Test each operation with authorized user
- Test each operation with unauthorized user (should fail)

**Migration Notes for Go**:
- Implement entitlement checking as middleware
- Use Go context to pass user entitlements through the request chain
- Consider using a centralized authorization service

**Example Scenarios**:
```
Scenario 1: Authorized user creates attribute
Input: userId = "admin1", operation = "CREATE"
Processing: Check user has canCreateBankAttribute
Output: Authorization passes, attribute created

Scenario 2: Unauthorized user attempts delete
Input: userId = "viewer1", operation = "DELETE"
Processing: Check user has canDeleteBankAttribute
Output: UserHasMissingRoles error returned
```

---

### BR-004: Unique Attribute ID Generation

**Category**: TRANSFORMATION

**Description**: Each bank attribute is assigned a unique UUID upon creation. This ensures global uniqueness of attribute identifiers across the system.

**Source**: 
- File: MappedBankAttributeProvider.scala
- Class/Object: MappedBankAttributeProvider
- Method: createOrUpdateBankAttribute
- Lines: Referenced in user story business rules

**Business Logic**:
1. When a new bank attribute is created (no existing attributeId provided)
2. Generate a new UUID for the bank_attribute_id
3. Associate the UUID with the new attribute record
4. Return the generated UUID in the response

**Variables**:
- **Input**: None (for new attributes)
- **Output**: bank_attribute_id (String) - UUID format
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| New attribute | No existing ID | Generate new UUID |
| Existing attribute | ID provided | Use existing ID |

**Business Impact**: 
Enables unique identification of bank attributes across the entire system, supporting audit trails, cross-referencing, and integration with external systems.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/attribute - Create Bank Attribute

**Related Test Cases**:
- Test that created attributes have unique UUIDs
- Test that UUIDs are in valid format

**Migration Notes for Go**:
- Use Go's uuid package (github.com/google/uuid) for UUID generation
- Ensure UUID format matches existing Scala implementation
- Consider using UUID v4 for random generation

**Example Scenarios**:
```
Scenario 1: New attribute creation
Input: New attribute request without ID
Processing: Generate UUID v4
Output: bank_attribute_id = "7uy8a7e4-6d02-40e3-a129-0b2bf89de8uh"
```

---

### BR-005: Active Status Default Value

**Category**: TRANSFORMATION

**Description**: When creating a bank attribute, if the is_active field is not provided in the request, it defaults to true. This ensures new attributes are active by default.

**Source**: 
- File: BankAttribute.scala, JSONFactory4.0.0.scala
- Class/Object: BankAttributeJsonV400
- Method: createOrUpdateBankAttribute
- Lines: Referenced in user story business rules

**Business Logic**:
1. When creating a bank attribute, check if is_active field is provided
2. If is_active is not provided or null, set it to true
3. If is_active is provided, use the provided value
4. Store the attribute with the determined active status

**Variables**:
- **Input**: is_active (Boolean, optional)
- **Output**: is_active (Boolean) - Final active status
- **Constants**: Default value = true

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| is_active not provided | Use default | Default to true |
| is_active = true | Attribute is active | Explicitly active |
| is_active = false | Attribute is inactive | Explicitly inactive |

**Business Impact**: 
Simplifies attribute creation by not requiring explicit active status, while ensuring new attributes are immediately usable in the system.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/attribute - Create Bank Attribute
- PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Update Bank Attribute

**Related Test Cases**:
- Test attribute creation without is_active (should default to true)
- Test attribute creation with is_active = false
- Test attribute creation with is_active = true

**Migration Notes for Go**:
- Use pointer type (*bool) for optional boolean fields in Go structs
- Implement default value logic in the service layer
- Consider using Go's omitempty tag for JSON serialization

**Example Scenarios**:
```
Scenario 1: No is_active provided
Input: {"name": "ISIN", "type": "STRING", "value": "XS123"}
Processing: is_active not provided, default to true
Output: is_active = true

Scenario 2: is_active explicitly false
Input: {"name": "ISIN", "type": "STRING", "value": "XS123", "is_active": false}
Processing: is_active provided as false
Output: is_active = false
```

---

### BR-006: Attribute Name Length Constraint

**Category**: THRESHOLD

**Description**: Bank attribute names are limited to a maximum of 50 characters. This ensures consistent data storage and prevents excessively long attribute names.

**Source**: 
- File: BankAttribute.scala, JSONFactory4.0.0.scala
- Class/Object: BankAttributeJsonV400
- Method: Validation layer
- Lines: Referenced in user story business rules

**Business Logic**:
1. When creating or updating a bank attribute, extract the name field
2. Check if the name length exceeds 50 characters
3. If length exceeds limit, reject with validation error
4. If length is within limit, proceed with operation

**Variables**:
- **Input**: name (String) - Attribute name
- **Output**: Boolean - Validation result
- **Constants**: Max length = 50 characters

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| name.length <= 50 | Valid name length | Max 50 characters |
| name.length > 50 | Name too long | Exceeds 50 characters |

**Business Impact**: 
Ensures database storage efficiency and consistent display of attribute names across user interfaces and reports.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/attribute - Create Bank Attribute
- PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Update Bank Attribute

**Related Test Cases**:
- Test attribute creation with name at 50 characters (should pass)
- Test attribute creation with name at 51 characters (should fail)

**Migration Notes for Go**:
- Implement length validation in request parsing or service layer
- Use Go's len() function for string length checking
- Return descriptive error message for validation failures

**Example Scenarios**:
```
Scenario 1: Valid name length
Input: name = "ISIN" (4 characters)
Processing: Check 4 <= 50
Output: Validation passes

Scenario 2: Name too long
Input: name = "A" * 51 (51 characters)
Processing: Check 51 <= 50
Output: Validation fails, error returned
```

---

### BR-007: Attribute Value Length Constraint

**Category**: THRESHOLD

**Description**: Bank attribute values are limited to a maximum of 255 characters. This ensures consistent data storage and prevents excessively long attribute values.

**Source**: 
- File: BankAttribute.scala, JSONFactory4.0.0.scala
- Class/Object: BankAttributeJsonV400
- Method: Validation layer
- Lines: Referenced in user story business rules

**Business Logic**:
1. When creating or updating a bank attribute, extract the value field
2. Check if the value length exceeds 255 characters
3. If length exceeds limit, reject with validation error
4. If length is within limit, proceed with operation

**Variables**:
- **Input**: value (String) - Attribute value
- **Output**: Boolean - Validation result
- **Constants**: Max length = 255 characters

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| value.length <= 255 | Valid value length | Max 255 characters |
| value.length > 255 | Value too long | Exceeds 255 characters |

**Business Impact**: 
Ensures database storage efficiency and consistent handling of attribute values across the system.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/attribute - Create Bank Attribute
- PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Update Bank Attribute

**Related Test Cases**:
- Test attribute creation with value at 255 characters (should pass)
- Test attribute creation with value at 256 characters (should fail)

**Migration Notes for Go**:
- Implement length validation in request parsing or service layer
- Use Go's len() function for string length checking
- Consider using database column constraints as additional safeguard

**Example Scenarios**:
```
Scenario 1: Valid value length
Input: value = "XS1234567890" (12 characters)
Processing: Check 12 <= 255
Output: Validation passes

Scenario 2: Value too long
Input: value = "A" * 256 (256 characters)
Processing: Check 256 <= 255
Output: Validation fails, error returned
```

---

### BR-008: Attribute Existence Validation for Update/Delete

**Category**: DECISION

**Description**: For update and delete operations, the specified bank attribute must exist in the system. This prevents operations on non-existent attributes.

**Source**: 
- File: MappedBankAttributeProvider.scala, APIMethods400.scala
- Class/Object: MappedBankAttributeProvider
- Method: getBankAttributeById, deleteBankAttribute
- Lines: Referenced in user story data validations

**Business Logic**:
1. When an update or delete operation is requested, extract the attribute ID
2. Query the attribute repository to verify the attribute exists
3. If attribute does not exist, reject with appropriate error
4. If attribute exists, proceed with the operation

**Variables**:
- **Input**: bankAttributeId (String) - Unique attribute identifier
- **Output**: Boolean - Attribute existence status
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Attribute exists | Attribute is in system | Valid attribute ID |
| Attribute not found | Attribute does not exist | Invalid attribute ID |

**Business Impact**: 
Prevents erroneous operations on non-existent data and provides clear feedback to API consumers about invalid requests.

**API Endpoints Using This Rule**:
- GET /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Get Bank Attribute By ID
- PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Update Bank Attribute
- DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Delete Bank Attribute

**Related Test Cases**:
- Test update with valid attribute ID
- Test update with invalid attribute ID (should fail)
- Test delete with valid attribute ID
- Test delete with invalid attribute ID (should fail)

**Migration Notes for Go**:
- Implement existence check before update/delete operations
- Return appropriate HTTP status codes (404 for not found)
- Consider using database transactions for atomic operations

**Example Scenarios**:
```
Scenario 1: Valid attribute exists
Input: bankAttributeId = "7uy8a7e4-6d02-40e3-a129-0b2bf89de8uh"
Processing: Query attribute repository
Output: Attribute found, proceed with operation

Scenario 2: Attribute does not exist
Input: bankAttributeId = "invalid-id"
Processing: Query attribute repository
Output: Attribute not found, error returned
```

---

### BR-009: Create or Update Pattern for Attribute Definitions

**Category**: WORKFLOW

**Description**: The attribute definition endpoint uses a create-or-update pattern. If an attribute definition with the same name exists for the bank, it is updated; otherwise, a new definition is created.

**Source**: 
- File: APIMethods400.scala, BankAttribute.scala
- Class/Object: APIMethods400
- Method: createOrUpdateBankAttributeDefinition
- Lines: Referenced in user story notes for implementation

**Business Logic**:
1. When a PUT request is received for attribute definition
2. Check if an attribute definition with the same name exists for the bank
3. If exists, update the existing definition with new values
4. If not exists, create a new definition with generated ID
5. Return the created or updated definition

**Variables**:
- **Input**: bankId (String), name (String), definition details
- **Output**: AttributeDefinition - Created or updated definition
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Definition exists | Update existing | Same name for bank |
| Definition not exists | Create new | New name for bank |

**Business Impact**: 
Provides idempotent behavior for definition management, simplifying client implementation and preventing duplicate definitions.

**API Endpoints Using This Rule**:
- PUT /banks/BANK_ID/attribute-definitions/bank - Create or Update Bank Attribute Definition

**Related Test Cases**:
- Test creating new definition
- Test updating existing definition with same name
- Test idempotent behavior with repeated requests

**Migration Notes for Go**:
- Implement upsert logic in the service layer
- Use database transactions to ensure atomicity
- Consider using database-level upsert if supported

**Example Scenarios**:
```
Scenario 1: Create new definition
Input: name = "ISIN" (not existing)
Processing: Check existence, create new
Output: New definition with generated ID

Scenario 2: Update existing definition
Input: name = "ISIN" (already exists)
Processing: Check existence, update existing
Output: Updated definition with same ID
```

---

### BR-010: Category Validation for Attribute Definitions

**Category**: DECISION

**Description**: For bank attribute definitions, the category field must be set to "Bank". This ensures proper categorization of attribute definitions.

**Source**: 
- File: APIMethods400.scala, JSONFactory4.0.0.scala
- Class/Object: AttributeDefinitionJsonV400
- Method: createOrUpdateBankAttributeDefinition
- Lines: Referenced in user story data validations

**Business Logic**:
1. When creating or updating an attribute definition, extract the category field
2. Validate that the category is "Bank"
3. If category is not "Bank", reject with validation error
4. If category is "Bank", proceed with operation

**Variables**:
- **Input**: category (String) - Definition category
- **Output**: Boolean - Validation result
- **Constants**: Valid category = "Bank"

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| category == "Bank" | Valid bank definition | Category is Bank |
| category != "Bank" | Invalid category | Category is not Bank |

**Business Impact**: 
Ensures attribute definitions are properly categorized for bank-level metadata, supporting organized attribute management.

**API Endpoints Using This Rule**:
- PUT /banks/BANK_ID/attribute-definitions/bank - Create or Update Bank Attribute Definition

**Related Test Cases**:
- Test definition creation with category = "Bank"
- Test definition creation with invalid category (should fail)

**Migration Notes for Go**:
- Implement category validation in request parsing
- Use Go constants for valid category values
- Return descriptive error for invalid categories

**Example Scenarios**:
```
Scenario 1: Valid category
Input: category = "Bank"
Processing: Validate category
Output: Validation passes

Scenario 2: Invalid category
Input: category = "Account"
Processing: Validate category
Output: Validation fails, error returned
```

---

### BR-011: Attribute Retrieval by Bank

**Category**: WORKFLOW

**Description**: When retrieving bank attributes, all attributes associated with a specific bank are returned as a collection. This enables bulk retrieval of bank metadata.

**Source**: 
- File: MappedBankAttributeProvider.scala, APIMethods400.scala
- Class/Object: MappedBankAttributeProvider
- Method: getBankAttributesByBank
- Lines: Referenced in user story endpoints

**Business Logic**:
1. When a GET request is received for bank attributes
2. Query the attribute repository for all attributes with the specified bank ID
3. Transform the results into the response format
4. Return the collection of attributes (may be empty if no attributes exist)

**Variables**:
- **Input**: bankId (String) - Bank identifier
- **Output**: List[BankAttribute] - Collection of bank attributes
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Attributes exist | Return collection | One or more attributes |
| No attributes | Return empty list | Zero attributes |

**Business Impact**: 
Enables efficient retrieval of all bank metadata in a single request, supporting reporting and integration use cases.

**API Endpoints Using This Rule**:
- GET /banks/BANK_ID/attributes - Get Bank Attributes

**Related Test Cases**:
- Test retrieval with multiple attributes
- Test retrieval with no attributes (should return empty list)

**Migration Notes for Go**:
- Use Go slices for attribute collections
- Return empty slice (not nil) when no attributes exist
- Consider pagination for banks with many attributes

**Example Scenarios**:
```
Scenario 1: Bank has attributes
Input: bankId = "gh.29.uk"
Processing: Query attributes for bank
Output: [attr1, attr2, attr3]

Scenario 2: Bank has no attributes
Input: bankId = "new.bank"
Processing: Query attributes for bank
Output: [] (empty list)
```

---

### BR-012: Hard Delete for Bank Attributes

**Category**: WORKFLOW

**Description**: When deleting a bank attribute, the record is permanently removed from the system (hard delete). This is a destructive operation that cannot be undone.

**Source**: 
- File: MappedBankAttributeProvider.scala
- Class/Object: MappedBankAttributeProvider
- Method: deleteBankAttribute (bulkDelete)
- Lines: Referenced in user story notes for implementation

**Business Logic**:
1. When a DELETE request is received for a bank attribute
2. Verify the attribute exists (see BR-008)
3. Permanently remove the attribute record from the database
4. Return success with no content (204)

**Variables**:
- **Input**: bankId (String), bankAttributeId (String)
- **Output**: None (204 No Content)
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Attribute exists | Delete permanently | Record removed |
| Attribute not found | Error returned | No deletion |

**Business Impact**: 
Provides ability to remove obsolete or incorrect bank metadata. Note: This is a permanent operation - consider audit requirements and whether soft delete might be needed.

**API Endpoints Using This Rule**:
- DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Delete Bank Attribute

**Related Test Cases**:
- Test successful deletion
- Test deletion of non-existent attribute (should fail)
- Test that deleted attribute cannot be retrieved

**Migration Notes for Go**:
- Implement hard delete using database DELETE statement
- Consider adding audit logging before deletion
- Evaluate if soft delete (is_deleted flag) is needed for compliance

**Example Scenarios**:
```
Scenario 1: Successful deletion
Input: bankAttributeId = "7uy8a7e4-6d02-40e3-a129-0b2bf89de8uh"
Processing: Delete record from database
Output: 204 No Content

Scenario 2: Attribute not found
Input: bankAttributeId = "invalid-id"
Processing: Check existence, not found
Output: Error returned
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /banks/BANK_ID/attribute | POST | Bank existence, type validation, RBAC, UUID generation, default active status, name/value length | BR-001, BR-002, BR-003, BR-004, BR-005, BR-006, BR-007 |
| /banks/BANK_ID/attributes | GET | Bank existence, RBAC, bulk retrieval | BR-001, BR-003, BR-011 |
| /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID | GET | Bank existence, RBAC, attribute existence | BR-001, BR-003, BR-008 |
| /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID | PUT | Bank existence, type validation, RBAC, attribute existence, name/value length | BR-001, BR-002, BR-003, BR-005, BR-006, BR-007, BR-008 |
| /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID | DELETE | Bank existence, RBAC, attribute existence, hard delete | BR-001, BR-003, BR-008, BR-012 |
| /banks/BANK_ID/attribute-definitions/bank | PUT | Bank existence, type validation, RBAC, category validation, create-or-update | BR-001, BR-002, BR-003, BR-009, BR-010 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestBankExistenceValidation | Pending | Pending |
| BR-002 | TestAttributeTypeValidation | Pending | Pending |
| BR-003 | TestRoleBasedAccessControl | Pending | Pending |
| BR-004 | TestUniqueAttributeIdGeneration | Pending | Pending |
| BR-005 | TestActiveStatusDefault | Pending | Pending |
| BR-006 | TestAttributeNameLengthConstraint | Pending | Pending |
| BR-007 | TestAttributeValueLengthConstraint | Pending | Pending |
| BR-008 | TestAttributeExistenceValidation | Pending | Pending |
| BR-009 | TestCreateOrUpdatePattern | Pending | Pending |
| BR-010 | TestCategoryValidation | Pending | Pending |
| BR-011 | TestAttributeRetrievalByBank | Pending | Pending |
| BR-012 | TestHardDeleteAttribute | Pending | Pending |

## Notes and Assumptions

1. **Attribute Uniqueness**: The current implementation allows duplicate attribute names per bank. This may need clarification from SME if uniqueness is required.

2. **Value Type Validation**: While attribute types are validated, it's unclear if attribute values are validated against their declared types at creation/update time. This should be confirmed.

3. **Audit Trail**: No explicit audit trail is documented. Consider if tracking who created/modified attributes and when is required for compliance.

4. **Cascading Delete**: The behavior when a bank is deleted is not specified. Attributes may become orphaned unless cascading delete is implemented.

5. **Attribute Versioning**: No versioning of attribute values is documented. Historical values are not maintained.

6. **Concurrent Access**: No explicit locking or optimistic concurrency control is documented for attribute updates.

7. **Error Response Format**: Error responses should follow a consistent format across all endpoints for API consumer convenience.
