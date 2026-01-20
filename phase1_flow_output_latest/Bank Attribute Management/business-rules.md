# Business Rules Extraction

**Extracted From**: Bank Attribute Management (Scala Application)
**Analysis Date**: 2026-01-20
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 8
- API Endpoints Analyzed: 6
- Rule Categories:
  - Calculations: 0
  - Decisions: 3
  - Thresholds: 2
  - Aggregations: 0
  - Workflows: 2
  - Transformations: 1

## Business Rules Catalog

### BR-001: Bank Existence Prerequisite

**Category**: DECISION

**Description**: A bank must exist in the system before any attributes can be created, updated, or managed for it. This ensures referential integrity and prevents orphaned attribute records.

**Source**: 
- File: BankAttribute.scala, MappedBankAttributeProvider.scala
- Class/Object: BankAttributeProvider
- Method: createOrUpdateBankAttribute
- Lines: Referenced in APIMethods400.scala endpoint definitions

**Business Logic**:
1. When a request is made to create or manage a bank attribute, the system first validates that the specified bank exists
2. If the bank does not exist, the operation is rejected with a BankNotFound error
3. Only after bank existence is confirmed can attribute operations proceed

**Variables**:
- **Input**: bankId (String) - The unique identifier of the bank
- **Output**: Boolean - Whether the bank exists and operation can proceed
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank exists in system | Bank is registered and active | Valid bank ID required |
| Bank not found | Cannot manage attributes for non-existent bank | Returns BankNotFound error |

**Business Impact**: 
Ensures data integrity by preventing creation of orphaned attributes. This rule maintains the parent-child relationship between banks and their attributes, which is critical for regulatory compliance and accurate reporting.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/attribute - Create Bank Attribute
- GET /banks/BANK_ID/attributes - Get Bank Attributes
- GET /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Get Bank Attribute By ID
- PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Update Bank Attribute
- DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Delete Bank Attribute
- PUT /banks/BANK_ID/attribute-definitions/bank - Create or Update Bank Attribute Definition

**Related Test Cases**:
- Test bank attribute creation with valid bank ID
- Test bank attribute creation with invalid bank ID (expect BankNotFound)

**Migration Notes for Go**:
- Implement bank existence check as a middleware or service layer validation
- Use early return pattern in Go for validation failures
- Consider caching bank existence checks for performance

**Example Scenarios**:
```
Scenario 1: Valid bank exists
Input: bankId = "gh.29.uk" (exists in system)
Processing: Check bank existence -> Bank found
Output: Proceed with attribute operation

Scenario 2: Bank does not exist
Input: bankId = "invalid.bank.id" (not in system)
Processing: Check bank existence -> Bank not found
Output: Return BankNotFound error, operation rejected
```

---

### BR-002: Attribute Type Validation

**Category**: DECISION

**Description**: Bank attribute types must be validated against a predefined set of allowed types. Only STRING, INTEGER, DOUBLE, or DATE_WITH_DAY types are permitted for bank attributes.

**Source**: 
- File: BankAttribute.scala, JSONFactory4.0.0.scala
- Class/Object: BankAttributeType enumeration
- Method: Type validation in createOrUpdateBankAttribute
- Lines: Type enum definition and validation logic

**Business Logic**:
1. When creating or updating a bank attribute, the type field is validated
2. The type must match one of the four allowed values: STRING, INTEGER, DOUBLE, DATE_WITH_DAY
3. If an invalid type is provided, the operation is rejected with a validation error
4. Type determines how the value should be interpreted and validated

**Variables**:
- **Input**: type (String) - The attribute type specified in the request
- **Output**: Boolean - Whether the type is valid
- **Constants**: 
  - STRING - Text values
  - INTEGER - Whole number values
  - DOUBLE - Decimal number values
  - DATE_WITH_DAY - Date values in format "YYYY-MM-DD"

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| type == "STRING" | Text-based attribute | Accepts any string value |
| type == "INTEGER" | Numeric whole number attribute | Accepts integer values (e.g., 123) |
| type == "DOUBLE" | Numeric decimal attribute | Accepts decimal values (e.g., 12.1234) |
| type == "DATE_WITH_DAY" | Date attribute | Accepts dates in "YYYY-MM-DD" format |

**Business Impact**: 
Ensures data consistency and enables proper type-based validation and processing of attribute values. This supports regulatory reporting requirements where specific data types are mandated.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/attribute - Create Bank Attribute
- PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Update Bank Attribute
- PUT /banks/BANK_ID/attribute-definitions/bank - Create or Update Bank Attribute Definition

**Related Test Cases**:
- Test attribute creation with each valid type (STRING, INTEGER, DOUBLE, DATE_WITH_DAY)
- Test attribute creation with invalid type (expect validation error)

**Migration Notes for Go**:
- Define a proper Go enum/const for BankAttributeType
- Use switch statement for type validation
- Consider using Go's type system for compile-time safety

**Example Scenarios**:
```
Scenario 1: Valid STRING type
Input: type = "STRING", value = "XS1234567890"
Processing: Validate type -> STRING is valid
Output: Attribute created with STRING type

Scenario 2: Valid INTEGER type
Input: type = "INTEGER", value = "123"
Processing: Validate type -> INTEGER is valid
Output: Attribute created with INTEGER type

Scenario 3: Invalid type
Input: type = "BOOLEAN", value = "true"
Processing: Validate type -> BOOLEAN not in allowed types
Output: Validation error returned
```

---

### BR-003: Role-Based Access Control for Attribute Operations

**Category**: DECISION

**Description**: Each bank attribute operation requires specific entitlements. Users must have the appropriate role-based entitlement to perform create, read, update, delete, or definition management operations.

**Source**: 
- File: APIMethods400.scala
- Class/Object: API endpoint definitions
- Method: Entitlement checks in each endpoint
- Lines: Entitlement annotations on endpoint definitions

**Business Logic**:
1. Before any attribute operation, the system checks if the authenticated user has the required entitlement
2. Each operation type maps to a specific entitlement:
   - Create attribute: canCreateBankAttribute
   - Read attribute(s): canGetBankAttribute
   - Update attribute: canUpdateBankAttribute
   - Delete attribute: canDeleteBankAttribute
   - Manage definitions: canCreateBankAttributeDefinitionAtOneBank
3. If the user lacks the required entitlement, the operation is rejected with UserHasMissingRoles error

**Variables**:
- **Input**: userId (String), operation (String), bankId (String)
- **Output**: Boolean - Whether user has required entitlement
- **Constants**: 
  - canCreateBankAttribute
  - canGetBankAttribute
  - canUpdateBankAttribute
  - canDeleteBankAttribute
  - canCreateBankAttributeDefinitionAtOneBank

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| User has canCreateBankAttribute | Authorized to create attributes | Required for POST /attribute |
| User has canGetBankAttribute | Authorized to view attributes | Required for GET /attributes |
| User has canUpdateBankAttribute | Authorized to modify attributes | Required for PUT /attributes |
| User has canDeleteBankAttribute | Authorized to remove attributes | Required for DELETE /attributes |
| User has canCreateBankAttributeDefinitionAtOneBank | Authorized to manage definitions | Required for PUT /attribute-definitions |

**Business Impact**: 
Enforces security and compliance by ensuring only authorized users can perform specific operations. This supports audit requirements and separation of duties in banking operations.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/attribute - Requires canCreateBankAttribute
- GET /banks/BANK_ID/attributes - Requires canGetBankAttribute
- GET /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Requires canGetBankAttribute
- PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Requires canUpdateBankAttribute
- DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Requires canDeleteBankAttribute
- PUT /banks/BANK_ID/attribute-definitions/bank - Requires canCreateBankAttributeDefinitionAtOneBank

**Related Test Cases**:
- Test each operation with user having correct entitlement (expect success)
- Test each operation with user lacking entitlement (expect UserHasMissingRoles)

**Migration Notes for Go**:
- Implement entitlement checking as middleware
- Use Go interfaces for entitlement provider abstraction
- Consider using context for passing user entitlements through request chain

**Example Scenarios**:
```
Scenario 1: User with create entitlement
Input: userId = "admin1", operation = "CREATE", entitlements = ["canCreateBankAttribute"]
Processing: Check entitlements -> canCreateBankAttribute found
Output: Operation allowed to proceed

Scenario 2: User without required entitlement
Input: userId = "viewer1", operation = "DELETE", entitlements = ["canGetBankAttribute"]
Processing: Check entitlements -> canDeleteBankAttribute not found
Output: UserHasMissingRoles error returned
```

---

### BR-004: Attribute Name Length Constraint

**Category**: THRESHOLD

**Description**: Bank attribute names are limited to a maximum of 50 characters to ensure consistency in storage and display across systems.

**Source**: 
- File: BankAttribute.scala, JSONFactory4.0.0.scala
- Class/Object: BankAttributeJsonV400
- Method: Input validation
- Lines: Field length constraints

**Business Logic**:
1. When creating or updating a bank attribute, the name field length is validated
2. If the name exceeds 50 characters, the operation is rejected
3. This constraint applies to both attribute creation and updates

**Variables**:
- **Input**: name (String) - The attribute name
- **Output**: Boolean - Whether the name length is valid
- **Constants**: MAX_NAME_LENGTH = 50

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| name.length <= 50 | Valid attribute name | Maximum 50 characters |
| name.length > 50 | Name too long | Rejected with validation error |

**Business Impact**: 
Ensures consistent data storage and prevents issues with database field limits, UI display truncation, and API response formatting.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/attribute - Create Bank Attribute
- PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Update Bank Attribute

**Related Test Cases**:
- Test attribute creation with name at exactly 50 characters (expect success)
- Test attribute creation with name exceeding 50 characters (expect validation error)

**Migration Notes for Go**:
- Use Go string length validation (len(name) <= 50)
- Consider using a validation library like go-playground/validator
- Define constant for max length to avoid magic numbers

**Example Scenarios**:
```
Scenario 1: Valid name length
Input: name = "ISIN" (4 characters)
Processing: Validate length -> 4 <= 50
Output: Name accepted

Scenario 2: Name at maximum length
Input: name = "A" * 50 (50 characters)
Processing: Validate length -> 50 <= 50
Output: Name accepted

Scenario 3: Name exceeds maximum
Input: name = "A" * 51 (51 characters)
Processing: Validate length -> 51 > 50
Output: Validation error returned
```

---

### BR-005: Attribute Value Length Constraint

**Category**: THRESHOLD

**Description**: Bank attribute values are limited to a maximum of 255 characters to ensure consistency in storage and prevent excessively large attribute values.

**Source**: 
- File: BankAttribute.scala, JSONFactory4.0.0.scala
- Class/Object: BankAttributeJsonV400
- Method: Input validation
- Lines: Field length constraints

**Business Logic**:
1. When creating or updating a bank attribute, the value field length is validated
2. If the value exceeds 255 characters, the operation is rejected
3. This constraint applies regardless of the attribute type

**Variables**:
- **Input**: value (String) - The attribute value
- **Output**: Boolean - Whether the value length is valid
- **Constants**: MAX_VALUE_LENGTH = 255

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| value.length <= 255 | Valid attribute value | Maximum 255 characters |
| value.length > 255 | Value too long | Rejected with validation error |

**Business Impact**: 
Ensures database storage efficiency and prevents potential issues with data transmission and processing of oversized values.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/attribute - Create Bank Attribute
- PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Update Bank Attribute

**Related Test Cases**:
- Test attribute creation with value at exactly 255 characters (expect success)
- Test attribute creation with value exceeding 255 characters (expect validation error)

**Migration Notes for Go**:
- Use Go string length validation (len(value) <= 255)
- Consider using a validation library like go-playground/validator
- Define constant for max length to avoid magic numbers

**Example Scenarios**:
```
Scenario 1: Valid value length
Input: value = "XS1234567890" (12 characters)
Processing: Validate length -> 12 <= 255
Output: Value accepted

Scenario 2: Value at maximum length
Input: value = "A" * 255 (255 characters)
Processing: Validate length -> 255 <= 255
Output: Value accepted

Scenario 3: Value exceeds maximum
Input: value = "A" * 256 (256 characters)
Processing: Validate length -> 256 > 255
Output: Validation error returned
```

---

### BR-006: Active Status Default Value

**Category**: TRANSFORMATION

**Description**: When creating a bank attribute, if the is_active field is not provided in the request, it defaults to true. This ensures new attributes are active by default.

**Source**: 
- File: BankAttribute.scala, MappedBankAttributeProvider.scala
- Class/Object: BankAttributeProvider
- Method: createOrUpdateBankAttribute
- Lines: Default value assignment logic

**Business Logic**:
1. When processing a create attribute request, check if is_active field is provided
2. If is_active is not provided (null/missing), set it to true
3. If is_active is explicitly provided, use the provided value
4. This default ensures newly created attributes are immediately usable

**Variables**:
- **Input**: is_active (Optional Boolean) - The active status from request
- **Output**: is_active (Boolean) - The final active status value
- **Constants**: DEFAULT_IS_ACTIVE = true

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| is_active not provided | Use default active status | Defaults to true |
| is_active = true | Attribute is active | Explicitly set to active |
| is_active = false | Attribute is inactive | Explicitly set to inactive |

**Business Impact**: 
Simplifies attribute creation by not requiring explicit active status for new attributes. Ensures new attributes are immediately available for use without additional activation steps.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/attribute - Create Bank Attribute

**Related Test Cases**:
- Test attribute creation without is_active field (expect is_active = true in response)
- Test attribute creation with is_active = true (expect is_active = true)
- Test attribute creation with is_active = false (expect is_active = false)

**Migration Notes for Go**:
- Use pointer type (*bool) for optional is_active field in request struct
- Apply default value when pointer is nil
- Consider using Go's omitempty JSON tag for optional fields

**Example Scenarios**:
```
Scenario 1: is_active not provided
Input: {"name": "ISIN", "type": "STRING", "value": "XS123"}
Processing: is_active not in request -> Apply default true
Output: {"is_active": true, ...}

Scenario 2: is_active explicitly true
Input: {"name": "ISIN", "type": "STRING", "value": "XS123", "is_active": true}
Processing: is_active provided as true -> Use provided value
Output: {"is_active": true, ...}

Scenario 3: is_active explicitly false
Input: {"name": "ISIN", "type": "STRING", "value": "XS123", "is_active": false}
Processing: is_active provided as false -> Use provided value
Output: {"is_active": false, ...}
```

---

### BR-007: Unique Attribute ID Generation

**Category**: WORKFLOW

**Description**: Each bank attribute is assigned a unique UUID upon creation. This ensures global uniqueness of attribute identifiers across the system.

**Source**: 
- File: MappedBankAttributeProvider.scala
- Class/Object: MappedBankAttributeProvider
- Method: createOrUpdateBankAttribute
- Lines: UUID generation logic

**Business Logic**:
1. When creating a new bank attribute, generate a new UUID
2. The UUID serves as the unique identifier (bank_attribute_id) for the attribute
3. This ID is used for all subsequent operations (get, update, delete) on the attribute
4. UUIDs ensure uniqueness without requiring centralized ID management

**Variables**:
- **Input**: None (generated internally)
- **Output**: bank_attribute_id (String) - UUID format identifier
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| New attribute creation | Generate new UUID | Format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx |
| Attribute update | Use existing UUID | No new ID generated |

**Business Impact**: 
Provides a reliable, globally unique identifier for each attribute that can be used across distributed systems and for audit trails.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/attribute - Create Bank Attribute (generates new UUID)

**Related Test Cases**:
- Test that created attribute has valid UUID format
- Test that multiple created attributes have unique IDs
- Test that update operation preserves original attribute ID

**Migration Notes for Go**:
- Use github.com/google/uuid package for UUID generation
- Generate UUID v4 for random unique identifiers
- Store as string in database for compatibility

**Example Scenarios**:
```
Scenario 1: Create new attribute
Input: {"name": "ISIN", "type": "STRING", "value": "XS123"}
Processing: Generate UUID -> "7uy8a7e4-6d02-40e3-a129-0b2bf89de8uh"
Output: {"bank_attribute_id": "7uy8a7e4-6d02-40e3-a129-0b2bf89de8uh", ...}

Scenario 2: Create another attribute
Input: {"name": "TAX_ID", "type": "STRING", "value": "TAX123"}
Processing: Generate UUID -> "9ca9a7e4-6d02-40e3-a129-0b2bf89de9b1"
Output: {"bank_attribute_id": "9ca9a7e4-6d02-40e3-a129-0b2bf89de9b1", ...}
```

---

### BR-008: Create or Update Pattern for Attribute Definitions

**Category**: WORKFLOW

**Description**: The attribute definition endpoint uses a create-or-update pattern where the same endpoint handles both creating new definitions and updating existing ones based on whether an attribute definition already exists.

**Source**: 
- File: APIMethods400.scala, MappedBankAttributeProvider.scala
- Class/Object: BankAttributeProvider
- Method: createOrUpdateAttributeDefinition
- Lines: Upsert logic in endpoint handler

**Business Logic**:
1. When a PUT request is made to the attribute-definitions endpoint, check if a definition with the same name exists for the bank
2. If no existing definition is found, create a new definition with a generated UUID
3. If an existing definition is found, update it with the new values
4. Return the created or updated definition in the response

**Variables**:
- **Input**: bankId (String), name (String), category (String), type (String), description (String), alias (String), can_be_seen_on_views (List[String]), is_active (Boolean)
- **Output**: AttributeDefinition object with attribute_definition_id
- **Constants**: category must be "Bank" for bank attribute definitions

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Definition does not exist | Create new definition | Generate new UUID |
| Definition exists | Update existing definition | Preserve existing UUID |
| category != "Bank" | Invalid category for bank definitions | Validation error |

**Business Impact**: 
Simplifies API usage by providing a single endpoint for both create and update operations. Reduces complexity for API consumers and ensures idempotent behavior.

**API Endpoints Using This Rule**:
- PUT /banks/BANK_ID/attribute-definitions/bank - Create or Update Bank Attribute Definition

**Related Test Cases**:
- Test creating new definition (expect 201 Created with new UUID)
- Test updating existing definition (expect 200 OK with same UUID)
- Test that update preserves attribute_definition_id

**Migration Notes for Go**:
- Implement upsert logic using database-specific features or check-then-insert/update pattern
- Use transactions to ensure atomicity of the check and create/update operations
- Consider using ON CONFLICT clause if using PostgreSQL

**Example Scenarios**:
```
Scenario 1: Create new definition
Input: {"name": "ISIN", "category": "Bank", "type": "STRING", ...}
Processing: Check if ISIN definition exists -> Not found -> Create new
Output: {"attribute_definition_id": "new-uuid", "name": "ISIN", ...}

Scenario 2: Update existing definition
Input: {"name": "ISIN", "category": "Bank", "type": "STRING", "description": "Updated description", ...}
Processing: Check if ISIN definition exists -> Found -> Update
Output: {"attribute_definition_id": "existing-uuid", "name": "ISIN", "description": "Updated description", ...}
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /banks/BANK_ID/attribute | POST | Bank existence, type validation, RBAC, name length, value length, active default, UUID generation | BR-001, BR-002, BR-003, BR-004, BR-005, BR-006, BR-007 |
| /banks/BANK_ID/attributes | GET | Bank existence, RBAC | BR-001, BR-003 |
| /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID | GET | Bank existence, RBAC | BR-001, BR-003 |
| /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID | PUT | Bank existence, type validation, RBAC, name length, value length | BR-001, BR-002, BR-003, BR-004, BR-005 |
| /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID | DELETE | Bank existence, RBAC | BR-001, BR-003 |
| /banks/BANK_ID/attribute-definitions/bank | PUT | Bank existence, type validation, RBAC, create-or-update pattern | BR-001, BR-002, BR-003, BR-008 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestBankExistenceValidation | Pending | Pending |
| BR-002 | TestAttributeTypeValidation | Pending | Pending |
| BR-003 | TestRoleBasedAccessControl | Pending | Pending |
| BR-004 | TestAttributeNameLength | Pending | Pending |
| BR-005 | TestAttributeValueLength | Pending | Pending |
| BR-006 | TestActiveStatusDefault | Pending | Pending |
| BR-007 | TestUniqueAttributeIdGeneration | Pending | Pending |
| BR-008 | TestCreateOrUpdateDefinition | Pending | Pending |

## Notes and Assumptions

1. **Assumption**: The Scala implementation uses Lift Mapper ORM for persistence. The Go implementation should use an appropriate ORM or database access pattern (e.g., GORM, sqlx).

2. **Assumption**: UUID generation in Scala uses standard Java UUID. Go implementation should use github.com/google/uuid package.

3. **Gap Identified**: The user story mentions potential need for attribute uniqueness per bank (should attribute names be unique?). Current implementation allows duplicates - this may need SME clarification.

4. **Gap Identified**: Value type validation (validating that INTEGER type values are actually integers) is mentioned as a complexity but not explicitly documented as a business rule. Consider adding explicit type-value validation in Go implementation.

5. **Gap Identified**: Cascading delete behavior when a bank is deleted is not explicitly defined. Go implementation should clarify this behavior.

6. **Gap Identified**: Audit trail requirements (who created/modified attributes and when) are mentioned as unclear. Consider adding audit fields in Go implementation if required.

7. **Technical Note**: The Scala implementation uses hard delete (bulkDelete). Consider if soft delete is needed for audit purposes in Go implementation.
