# Business Rules Extraction

**Extracted From**: Transaction Attribute Management Capability (Scala Application)
**Analysis Date**: 2026-01-20
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 6
- API Endpoints Analyzed: 2
- Rule Categories:
  - Calculations: 0
  - Decisions: 2
  - Thresholds: 0
  - Aggregations: 0
  - Workflows: 2
  - Transformations: 2

## Business Rules Catalog

### BR-001: Attribute Type Validation

**Category**: DECISION

**Description**: The system validates that transaction attribute types conform to one of the supported enumerated types before allowing any attribute management operation.

**Source**: 
- File: TransactionAttributeProvider.scala / APIMethods400.scala
- Class/Object: TransactionAttributeProvider, APIMethods400
- Method: updateTransactionAttribute, createOrUpdateTransactionAttribute
- Lines: Attribute type validation logic

**Business Logic**:
1. When a user submits an attribute for creation or update, the system extracts the `type` field from the request
2. The system validates that the type value matches one of the supported TransactionAttributeType enumeration values
3. If the type is valid, processing continues; if invalid, the operation is rejected with an appropriate error message

**Variables**:
- **Input**: type (STRING | INTEGER | DOUBLE | DATE_WITH_DAY)
- **Output**: Validation result (success/failure)
- **Constants**: Supported types - DOUBLE (e.g., 12.1234), STRING (e.g., TAX_NUMBER), INTEGER (e.g., 123), DATE_WITH_DAY (e.g., 2012-04-23)

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| type == "STRING" | Text-based attribute | Any string value |
| type == "INTEGER" | Whole number attribute | Numeric integer value |
| type == "DOUBLE" | Decimal number attribute | Numeric decimal value |
| type == "DATE_WITH_DAY" | Date attribute | Date in YYYY-MM-DD format |

**Business Impact**: 
Ensures data integrity and consistency across the transaction attribute system. Prevents invalid data types from being stored, which could cause downstream processing errors or reporting inconsistencies.

**API Endpoints Using This Rule**:
- PUT /banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID/attributes/ATTRIBUTE_ID - Update transaction attribute
- PUT /banks/BANK_ID/attribute-definitions/transaction - Create/update attribute definitions

**Related Test Cases**:
Test cases validating attribute type enumeration values and rejection of invalid types

**Migration Notes for Go**:
- Implement using Go constants or iota-based enums for type safety
- Create a validation function that checks against allowed type values
- Return appropriate HTTP error codes (400 Bad Request) for invalid types

**Example Scenarios**:
```
Scenario 1: Valid STRING type
Input: type = "STRING", value = "TAX_NUMBER_12345"
Processing: Validate type against enum, type is valid
Output: Validation passes, continue processing

Scenario 2: Invalid type
Input: type = "BOOLEAN", value = "true"
Processing: Validate type against enum, type not found
Output: Validation fails, return error "Invalid attribute type"
```

---

### BR-002: Resource Existence Validation

**Category**: DECISION

**Description**: The system enforces that all referenced resources (bank, account, transaction) must exist before any attribute management operation can be performed.

**Source**: 
- File: APIMethods400.scala / NewStyle.scala
- Class/Object: APIMethods400, NewStyle
- Method: updateTransactionAttribute
- Lines: Resource validation logic

**Business Logic**:
1. Extract BANK_ID, ACCOUNT_ID, and TRANSACTION_ID from the request path
2. Verify that the bank exists in the system
3. Verify that the account exists and belongs to the specified bank
4. Verify that the transaction exists within the specified account
5. For update operations, verify that the attribute with ATTRIBUTE_ID exists
6. If any resource is not found, reject the operation with a specific error message

**Variables**:
- **Input**: BANK_ID, ACCOUNT_ID, TRANSACTION_ID, ATTRIBUTE_ID (for updates)
- **Output**: Validation result with specific error if resource not found
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank exists | Bank is registered in the system | Valid bank identifier |
| Account belongs to bank | Account is scoped to the correct bank | Valid account-bank relationship |
| Transaction exists in account | Transaction is part of the account | Valid transaction-account relationship |
| Attribute exists (for updates) | Attribute was previously created | Valid attribute identifier |

**Business Impact**: 
Maintains referential integrity and prevents orphaned attributes. Ensures that attributes are always associated with valid, existing transactions within the correct organizational hierarchy.

**API Endpoints Using This Rule**:
- PUT /banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID/attributes/ATTRIBUTE_ID - Update transaction attribute

**Related Test Cases**:
Test cases for 404 Not Found responses when resources don't exist

**Migration Notes for Go**:
- Implement sequential validation checks with early return on failure
- Use appropriate HTTP status codes (404 Not Found) for missing resources
- Consider using middleware for common resource validation patterns

**Example Scenarios**:
```
Scenario 1: All resources exist
Input: BANK_ID = "bank-123", ACCOUNT_ID = "acc-456", TRANSACTION_ID = "txn-789"
Processing: Validate each resource exists in sequence
Output: All validations pass, continue to attribute operation

Scenario 2: Transaction not found
Input: BANK_ID = "bank-123", ACCOUNT_ID = "acc-456", TRANSACTION_ID = "txn-invalid"
Processing: Bank exists, account exists, transaction not found
Output: Return 404 error "Transaction not found"
```

---

### BR-003: Entitlement-Based Access Control

**Category**: DECISION

**Description**: The system enforces role-based access control requiring users to have specific entitlements before performing attribute management operations.

**Source**: 
- File: APIMethods400.scala
- Class/Object: APIMethods400
- Method: updateTransactionAttribute, createTransactionAttributeDefinition
- Lines: Entitlement check logic

**Business Logic**:
1. Authenticate the user and retrieve their session
2. Check if the user has the required entitlement for the requested operation
3. For attribute updates: require `canUpdateTransactionAttributeAtOneBank` entitlement
4. For attribute definition management: require `canCreateTransactionAttributeDefinitionAtOneBank` entitlement
5. If the user lacks the required entitlement, reject the operation with an authorization error

**Variables**:
- **Input**: User session, requested operation type
- **Output**: Authorization result (allowed/denied)
- **Constants**: Required entitlements - canUpdateTransactionAttributeAtOneBank, canCreateTransactionAttributeDefinitionAtOneBank

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| User has canUpdateTransactionAttributeAtOneBank | User can modify transaction attributes | Role-based entitlement |
| User has canCreateTransactionAttributeDefinitionAtOneBank | User can manage attribute schemas | Role-based entitlement |

**Business Impact**: 
Ensures that only authorized personnel (Bank Administrators or API Consumers with appropriate entitlements) can modify transaction metadata. This is critical for compliance, audit trails, and preventing unauthorized data modifications.

**API Endpoints Using This Rule**:
- PUT /banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID/attributes/ATTRIBUTE_ID - Requires canUpdateTransactionAttributeAtOneBank
- PUT /banks/BANK_ID/attribute-definitions/transaction - Requires canCreateTransactionAttributeDefinitionAtOneBank

**Related Test Cases**:
Test cases for 403 Forbidden responses when user lacks required entitlements

**Migration Notes for Go**:
- Implement middleware for entitlement checking
- Use context to pass user session and entitlements through the request chain
- Return 403 Forbidden for authorization failures

**Example Scenarios**:
```
Scenario 1: User has required entitlement
Input: User with canUpdateTransactionAttributeAtOneBank role, PUT attribute request
Processing: Check user entitlements, required entitlement found
Output: Authorization passes, continue to attribute update

Scenario 2: User lacks entitlement
Input: User without canUpdateTransactionAttributeAtOneBank role, PUT attribute request
Processing: Check user entitlements, required entitlement not found
Output: Return 403 Forbidden "User lacks required entitlement"
```

---

### BR-004: Attribute Value Type Compatibility

**Category**: TRANSFORMATION

**Description**: The system validates that attribute values are compatible with their declared types before persisting.

**Source**: 
- File: TransactionAttributeProvider.scala / MappedTransactionAttributeProvider.scala
- Class/Object: MappedTransactionAttributeProvider
- Method: createOrUpdateTransactionAttribute
- Lines: Value validation logic

**Business Logic**:
1. Receive the attribute with name, type, and value
2. Based on the declared type, validate that the value can be parsed/converted appropriately:
   - STRING: Any string value is valid
   - INTEGER: Value must be parseable as a whole number (e.g., "123")
   - DOUBLE: Value must be parseable as a decimal number (e.g., "12.1234")
   - DATE_WITH_DAY: Value must be in YYYY-MM-DD format (e.g., "2012-04-23")
3. If validation fails, reject the operation with a type mismatch error

**Variables**:
- **Input**: type (declared attribute type), value (string representation)
- **Output**: Validation result, parsed value if successful
- **Constants**: Date format pattern "YYYY-MM-DD"

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| type == INTEGER && value is numeric | Integer attribute with valid value | Whole numbers only |
| type == DOUBLE && value is decimal | Decimal attribute with valid value | Decimal numbers |
| type == DATE_WITH_DAY && value matches format | Date attribute with valid format | YYYY-MM-DD pattern |
| type == STRING | Text attribute | Any string accepted |

**Business Impact**: 
Ensures data quality and prevents storage of malformed data that could cause issues in downstream processing, reporting, or analytics. Maintains consistency in how attribute values are stored and retrieved.

**API Endpoints Using This Rule**:
- PUT /banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID/attributes/ATTRIBUTE_ID - Update transaction attribute

**Related Test Cases**:
Test cases for value format validation and type mismatch errors

**Migration Notes for Go**:
- Use strconv package for numeric parsing
- Use time.Parse for date validation with layout "2006-01-02"
- Return descriptive error messages indicating expected format

**Example Scenarios**:
```
Scenario 1: Valid INTEGER value
Input: type = "INTEGER", value = "12345"
Processing: Parse "12345" as integer, parsing succeeds
Output: Validation passes, value stored as "12345"

Scenario 2: Invalid INTEGER value
Input: type = "INTEGER", value = "12.34"
Processing: Parse "12.34" as integer, parsing fails
Output: Validation fails, return error "Value must be a valid integer"

Scenario 3: Valid DATE_WITH_DAY value
Input: type = "DATE_WITH_DAY", value = "2012-04-23"
Processing: Parse date against YYYY-MM-DD format, parsing succeeds
Output: Validation passes, value stored as "2012-04-23"
```

---

### BR-005: Bank-Scoped Attribute Management

**Category**: WORKFLOW

**Description**: Transaction attributes are scoped to a specific bank and must be managed within that bank's organizational context.

**Source**: 
- File: APIMethods400.scala / TransactionAttributeProvider.scala
- Class/Object: APIMethods400, TransactionAttributeProvider
- Method: updateTransactionAttribute
- Lines: Bank context handling

**Business Logic**:
1. Extract BANK_ID from the request path
2. Validate that the user has entitlements for the specified bank
3. Ensure that the account and transaction belong to the specified bank
4. Store the attribute with the bank context for proper scoping
5. Attribute definitions at the bank level standardize attribute schemas for all transactions within that bank

**Variables**:
- **Input**: BANK_ID, user entitlements, attribute data
- **Output**: Bank-scoped attribute record
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Attribute scoped to bank | Attribute belongs to specific bank's domain | Valid BANK_ID |
| User entitled for bank | User can operate within bank's context | Bank-specific entitlement |

**Business Impact**: 
Enables multi-tenant operation where different banks can have their own attribute schemas and data without interference. Supports regulatory compliance by ensuring data isolation between banking entities.

**API Endpoints Using This Rule**:
- PUT /banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID/attributes/ATTRIBUTE_ID - Bank-scoped attribute update
- PUT /banks/BANK_ID/attribute-definitions/transaction - Bank-level attribute definition management

**Related Test Cases**:
Test cases for bank isolation and cross-bank access prevention

**Migration Notes for Go**:
- Include bank_id in all attribute-related database queries
- Implement bank context validation in middleware
- Ensure attribute definitions are bank-specific

**Example Scenarios**:
```
Scenario 1: Attribute update within bank context
Input: BANK_ID = "bank-123", attribute update request
Processing: Validate bank exists, user has bank entitlement, update attribute with bank scope
Output: Attribute updated with bank_id = "bank-123"

Scenario 2: Cross-bank access attempt
Input: User entitled for "bank-123", attempts to update attribute in "bank-456"
Processing: Check user entitlements for bank-456, entitlement not found
Output: Return 403 Forbidden "User not entitled for this bank"
```

---

### BR-006: Idempotent Attribute Update

**Category**: WORKFLOW

**Description**: The attribute update operation uses createOrUpdate semantics, allowing the same attribute to be updated multiple times with the same or different values without creating duplicates.

**Source**: 
- File: MappedTransactionAttributeProvider.scala
- Class/Object: MappedTransactionAttributeProvider
- Method: createOrUpdateTransactionAttribute
- Lines: CreateOrUpdate logic

**Business Logic**:
1. Receive update request with ATTRIBUTE_ID
2. Check if an attribute with the given ID exists
3. If exists, update the existing record with new name, type, and value
4. If not exists (for create operations), create a new attribute record
5. Return the updated/created attribute with its transaction_attribute_id

**Variables**:
- **Input**: ATTRIBUTE_ID, name, type, value
- **Output**: Updated or created attribute record with transaction_attribute_id
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Attribute exists | Update existing attribute | Valid ATTRIBUTE_ID |
| Attribute not exists | Create new attribute | New ATTRIBUTE_ID generated |

**Business Impact**: 
Provides flexibility in attribute management by supporting both creation and update through a single operation pattern. Prevents duplicate attributes and ensures data consistency even when the same update request is sent multiple times.

**API Endpoints Using This Rule**:
- PUT /banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID/attributes/ATTRIBUTE_ID - CreateOrUpdate semantics

**Related Test Cases**:
Test cases for idempotent updates and duplicate prevention

**Migration Notes for Go**:
- Implement upsert pattern using database-specific syntax or check-then-insert/update logic
- Ensure atomic operations to prevent race conditions
- Return consistent response structure for both create and update cases

**Example Scenarios**:
```
Scenario 1: Update existing attribute
Input: ATTRIBUTE_ID = "attr-123", name = "tax_category", type = "STRING", value = "EXEMPT"
Processing: Find attribute by ID, attribute exists, update fields
Output: Return updated attribute with transaction_attribute_id = "attr-123"

Scenario 2: Multiple updates with same value
Input: Same request sent twice with ATTRIBUTE_ID = "attr-123"
Processing: First request updates attribute, second request updates to same values
Output: Both requests succeed, attribute remains consistent
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID/attributes/ATTRIBUTE_ID | PUT | Attribute type validation, resource existence, entitlement check, value type compatibility, bank scoping, idempotent update | BR-001, BR-002, BR-003, BR-004, BR-005, BR-006 |
| /banks/BANK_ID/attribute-definitions/transaction | PUT | Attribute type validation, entitlement check, bank scoping | BR-001, BR-003, BR-005 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestAttributeTypeValidation | Pending | Pending |
| BR-002 | TestResourceExistenceValidation | Pending | Pending |
| BR-003 | TestEntitlementBasedAccessControl | Pending | Pending |
| BR-004 | TestAttributeValueTypeCompatibility | Pending | Pending |
| BR-005 | TestBankScopedAttributeManagement | Pending | Pending |
| BR-006 | TestIdempotentAttributeUpdate | Pending | Pending |

## Notes and Assumptions

1. **Assumptions Made**:
   - The user story describes the primary business rules; actual Scala implementation may contain additional edge case handling
   - Attribute definitions at the bank level follow similar validation patterns as transaction attributes
   - The system uses standard HTTP status codes for error responses (400, 403, 404)

2. **Gaps Identified**:
   - Clarification needed on business-specific attribute naming conventions or reserved attribute names
   - Confirmation required on whether attribute history/audit trail is needed for compliance
   - Size limits on attribute values not specified in the user story

3. **SME Input Required**:
   - Maximum length for attribute names and values
   - Whether certain attribute names are reserved or have special meaning
   - Audit/compliance requirements for attribute changes
