# Business Rules Extraction

**Extracted From**: Account Attribute Management (Scala Application)
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

**Description**: The system must validate that account attribute types conform to a predefined set of valid types before allowing creation or update operations.

**Source**: 
- File: APIMethods310.scala / MappedAccountAttributeProvider.scala
- Class/Object: AccountAttributeProvider
- Method: createAccountAttribute / updateAccountAttribute
- Lines: Referenced in user story acceptance criteria

**Business Logic**:
1. When a user submits an account attribute creation or update request, the system extracts the `type` field from the request
2. The system validates that the type value matches one of the allowed enum values
3. If the type is valid, processing continues; if invalid, the request is rejected with an appropriate error

**Variables**:
- **Input**: `type` field from AccountAttributeJson request body
- **Output**: Validation result (pass/fail)
- **Constants**: Valid types = [STRING, INTEGER, DOUBLE, DATE_WITH_DAY]

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| type == "STRING" | Text-based attribute value | For identifiers like ISIN, TAX_NUMBER |
| type == "INTEGER" | Whole number attribute value | For numeric counts or IDs |
| type == "DOUBLE" | Decimal number attribute value | For monetary or percentage values |
| type == "DATE_WITH_DAY" | Date attribute value | For dates like MATURITY_DATE, ISSUE_DATE |

**Business Impact**: 
Ensures data integrity by enforcing consistent typing of account attributes across the system. This enables proper data processing, reporting, and analytics based on attribute types.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/accounts/ACCOUNT_ID/products/PRODUCT_CODE/attribute - Create Account Attribute
- PUT /banks/BANK_ID/accounts/ACCOUNT_ID/products/PRODUCT_CODE/attributes/ACCOUNT_ATTRIBUTE_ID - Update Account Attribute

**Related Test Cases**:
- Test cases validating attribute type enum values
- Test cases for invalid type rejection

**Migration Notes for Go**:
- Implement as a Go enum or const block with string values
- Use a validation function that checks against the allowed type set
- Return appropriate HTTP 400 error for invalid types

**Example Scenarios**:
```
Scenario 1: Valid STRING type
Input: type = "STRING", name = "ISIN", value = "GB0002634946"
Processing: Validate "STRING" is in [STRING, INTEGER, DOUBLE, DATE_WITH_DAY]
Output: Validation passes, continue processing

Scenario 2: Invalid type
Input: type = "BOOLEAN", name = "IS_ACTIVE", value = "true"
Processing: Validate "BOOLEAN" is in [STRING, INTEGER, DOUBLE, DATE_WITH_DAY]
Output: Validation fails, return error "Invalid attribute type"
```

---

### BR-002: Entity Existence Validation

**Category**: DECISION

**Description**: Before creating or updating an account attribute, the system must verify that all referenced entities (bank, account, product) exist in the system.

**Source**: 
- File: APIMethods310.scala / NewStyle.function
- Class/Object: NewStyle.function helpers
- Method: createAccountAttribute / updateAccountAttribute
- Lines: Referenced in user story acceptance criteria

**Business Logic**:
1. Extract BANK_ID, ACCOUNT_ID, and PRODUCT_CODE from the request path parameters
2. Verify the bank exists in the system
3. Verify the account exists within the specified bank
4. Verify the product exists within the specified bank
5. If any entity is missing, reject the request with an appropriate error
6. If all entities exist, proceed with attribute creation/update

**Variables**:
- **Input**: BANK_ID, ACCOUNT_ID, PRODUCT_CODE from URL path parameters
- **Output**: Validation result with specific error if entity not found
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank exists | Bank must be registered in the system | Valid BANK_ID required |
| Account exists in bank | Account must belong to the specified bank | Valid ACCOUNT_ID within BANK_ID |
| Product exists in bank | Product must be defined for the bank | Valid PRODUCT_CODE within BANK_ID |

**Business Impact**: 
Maintains referential integrity by ensuring account attributes are only created for valid, existing entities. Prevents orphaned attributes and ensures data consistency across the banking system.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/accounts/ACCOUNT_ID/products/PRODUCT_CODE/attribute - Create Account Attribute
- PUT /banks/BANK_ID/accounts/ACCOUNT_ID/products/PRODUCT_CODE/attributes/ACCOUNT_ATTRIBUTE_ID - Update Account Attribute

**Related Test Cases**:
- Test cases for non-existent bank handling
- Test cases for non-existent account handling
- Test cases for non-existent product handling

**Migration Notes for Go**:
- Implement as sequential existence checks with early return on failure
- Use appropriate HTTP 404 status codes for not found entities
- Consider using a validation middleware or helper function for reusability

**Example Scenarios**:
```
Scenario 1: All entities exist
Input: BANK_ID = "gh.29.uk", ACCOUNT_ID = "8ca8a7e4-...", PRODUCT_CODE = "SAVINGS"
Processing: Check bank exists -> Check account exists in bank -> Check product exists in bank
Output: All validations pass, continue with attribute creation

Scenario 2: Bank does not exist
Input: BANK_ID = "invalid.bank", ACCOUNT_ID = "8ca8a7e4-...", PRODUCT_CODE = "SAVINGS"
Processing: Check bank exists -> Bank not found
Output: Return error "Bank not found" with HTTP 404

Scenario 3: Account does not exist in bank
Input: BANK_ID = "gh.29.uk", ACCOUNT_ID = "invalid-account", PRODUCT_CODE = "SAVINGS"
Processing: Check bank exists -> Bank found -> Check account exists -> Account not found
Output: Return error "Account not found" with HTTP 404
```

---

### BR-003: Authorization Entitlement Enforcement

**Category**: DECISION

**Description**: The system must enforce role-based access control by verifying that users have the appropriate entitlements before allowing account attribute operations.

**Source**: 
- File: APIMethods310.scala
- Class/Object: APIMethods310
- Method: createAccountAttribute / updateAccountAttribute
- Lines: Referenced in user story acceptance criteria

**Business Logic**:
1. When a user attempts to create an account attribute, verify they have `canCreateAccountAttributeAtOneBank` entitlement
2. When a user attempts to update an account attribute, verify they have `canUpdateAccountAttribute` entitlement
3. If the user lacks the required entitlement, reject the request with an authorization error
4. If the user has the required entitlement, proceed with the operation

**Variables**:
- **Input**: User's entitlements/permissions, operation type (create/update)
- **Output**: Authorization decision (allowed/denied)
- **Constants**: 
  - Create entitlement: `canCreateAccountAttributeAtOneBank`
  - Update entitlement: `canUpdateAccountAttribute`

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Has create entitlement | User can create new attributes | canCreateAccountAttributeAtOneBank |
| Has update entitlement | User can modify existing attributes | canUpdateAccountAttribute |

**Business Impact**: 
Ensures proper access control and security by restricting attribute management operations to authorized users only. This protects sensitive account metadata from unauthorized modifications.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/accounts/ACCOUNT_ID/products/PRODUCT_CODE/attribute - Requires canCreateAccountAttributeAtOneBank
- PUT /banks/BANK_ID/accounts/ACCOUNT_ID/products/PRODUCT_CODE/attributes/ACCOUNT_ATTRIBUTE_ID - Requires canUpdateAccountAttribute

**Related Test Cases**:
- Test cases for unauthorized user rejection
- Test cases for authorized user access

**Migration Notes for Go**:
- Implement as middleware or decorator pattern for entitlement checks
- Use HTTP 403 Forbidden for authorization failures
- Consider implementing a generic entitlement checking function

**Example Scenarios**:
```
Scenario 1: User has create entitlement
Input: User with entitlements = ["canCreateAccountAttributeAtOneBank"], Operation = CREATE
Processing: Check if "canCreateAccountAttributeAtOneBank" in user entitlements
Output: Authorization passes, proceed with creation

Scenario 2: User lacks update entitlement
Input: User with entitlements = ["canViewAccount"], Operation = UPDATE
Processing: Check if "canUpdateAccountAttribute" in user entitlements
Output: Authorization fails, return HTTP 403 Forbidden
```

---

### BR-004: Account Attribute Creation Workflow

**Category**: WORKFLOW

**Description**: The complete business workflow for creating a new custom attribute on a bank account, including validation, persistence, and response generation.

**Source**: 
- File: APIMethods310.scala / MappedAccountAttributeProvider.scala
- Class/Object: AccountAttributeProvider
- Method: createAccountAttribute
- Lines: Referenced in user story endpoints

**Business Logic**:
1. Receive POST request with AccountAttributeJson body and path parameters (BANK_ID, ACCOUNT_ID, PRODUCT_CODE)
2. Authenticate the user (must be logged in)
3. Verify user has `canCreateAccountAttributeAtOneBank` entitlement
4. Validate that bank, account, and product exist
5. Validate the attribute type is valid (STRING, INTEGER, DOUBLE, DATE_WITH_DAY)
6. Generate a unique account_attribute_id for the new attribute
7. Persist the attribute with all provided fields (name, type, value, product_instance_code)
8. Return AccountAttributeResponseJson with HTTP 201 Created status

**Variables**:
- **Input**: 
  - Path: BANK_ID, ACCOUNT_ID, PRODUCT_CODE
  - Body: name, type, value, product_instance_code (optional)
- **Output**: AccountAttributeResponseJson containing account_attribute_id, name, type, value, product_instance_code, bank_id, account_id, product_code
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| User authenticated | User must be logged in | Valid session required |
| User authorized | User has create permission | canCreateAccountAttributeAtOneBank |
| Entities exist | Referenced entities are valid | Bank, Account, Product must exist |
| Type valid | Attribute type is supported | STRING, INTEGER, DOUBLE, DATE_WITH_DAY |

**Business Impact**: 
Enables banks to extend account metadata with custom attributes for business-specific requirements such as ISIN codes, loan identifiers, maturity dates, and other financial product attributes.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/accounts/ACCOUNT_ID/products/PRODUCT_CODE/attribute - Create Account Attribute

**Related Test Cases**:
- End-to-end test for successful attribute creation
- Test cases for each validation failure scenario

**Migration Notes for Go**:
- Implement as a handler function with clear separation of validation and persistence
- Use Go's error handling pattern for validation failures
- Consider using a transaction for atomic persistence

**Example Scenarios**:
```
Scenario 1: Successful attribute creation
Input: 
  Path: BANK_ID="gh.29.uk", ACCOUNT_ID="8ca8a7e4-...", PRODUCT_CODE="BOND"
  Body: name="ISIN", type="STRING", value="GB0002634946", product_instance_code="INST001"
Processing: 
  1. Authenticate user -> Success
  2. Check entitlement -> Has canCreateAccountAttributeAtOneBank
  3. Validate bank exists -> Found
  4. Validate account exists -> Found
  5. Validate product exists -> Found
  6. Validate type -> STRING is valid
  7. Generate ID -> "7uy8a7e4-6d02-40e3-a129-0b2bf89de8uh"
  8. Persist attribute -> Success
Output: HTTP 201 with AccountAttributeResponseJson
```

---

### BR-005: Account Attribute Update Workflow

**Category**: WORKFLOW

**Description**: The complete business workflow for updating an existing custom attribute on a bank account, including validation, persistence, and response generation.

**Source**: 
- File: APIMethods310.scala / MappedAccountAttributeProvider.scala
- Class/Object: AccountAttributeProvider
- Method: updateAccountAttribute
- Lines: Referenced in user story endpoints

**Business Logic**:
1. Receive PUT request with AccountAttributeJson body and path parameters (BANK_ID, ACCOUNT_ID, PRODUCT_CODE, ACCOUNT_ATTRIBUTE_ID)
2. Authenticate the user (must be logged in)
3. Verify user has `canUpdateAccountAttribute` entitlement
4. Validate that bank, account, and product exist
5. Validate that the attribute with ACCOUNT_ATTRIBUTE_ID exists
6. Validate the attribute type is valid (STRING, INTEGER, DOUBLE, DATE_WITH_DAY)
7. Update the attribute with new values (name, type, value, product_instance_code)
8. Return AccountAttributeResponseJson with HTTP 201 Created status

**Variables**:
- **Input**: 
  - Path: BANK_ID, ACCOUNT_ID, PRODUCT_CODE, ACCOUNT_ATTRIBUTE_ID
  - Body: name, type, value, product_instance_code (optional)
- **Output**: AccountAttributeResponseJson containing updated attribute details
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| User authenticated | User must be logged in | Valid session required |
| User authorized | User has update permission | canUpdateAccountAttribute |
| Entities exist | Referenced entities are valid | Bank, Account, Product must exist |
| Attribute exists | Attribute to update must exist | Valid ACCOUNT_ATTRIBUTE_ID |
| Type valid | Attribute type is supported | STRING, INTEGER, DOUBLE, DATE_WITH_DAY |

**Business Impact**: 
Enables banks to modify existing account attributes to reflect changes in business data, such as updated maturity dates, changed identifiers, or corrected values.

**API Endpoints Using This Rule**:
- PUT /banks/BANK_ID/accounts/ACCOUNT_ID/products/PRODUCT_CODE/attributes/ACCOUNT_ATTRIBUTE_ID - Update Account Attribute

**Related Test Cases**:
- End-to-end test for successful attribute update
- Test cases for non-existent attribute handling
- Test cases for each validation failure scenario

**Migration Notes for Go**:
- Implement as a handler function with clear separation of validation and persistence
- Use Go's error handling pattern for validation failures
- Ensure atomic update operation

**Example Scenarios**:
```
Scenario 1: Successful attribute update
Input: 
  Path: BANK_ID="gh.29.uk", ACCOUNT_ID="8ca8a7e4-...", PRODUCT_CODE="BOND", ACCOUNT_ATTRIBUTE_ID="7uy8a7e4-..."
  Body: name="ISIN", type="STRING", value="GB0002634947", product_instance_code="INST002"
Processing: 
  1. Authenticate user -> Success
  2. Check entitlement -> Has canUpdateAccountAttribute
  3. Validate bank exists -> Found
  4. Validate account exists -> Found
  5. Validate product exists -> Found
  6. Validate attribute exists -> Found
  7. Validate type -> STRING is valid
  8. Update attribute -> Success
Output: HTTP 201 with updated AccountAttributeResponseJson

Scenario 2: Attribute not found
Input: 
  Path: BANK_ID="gh.29.uk", ACCOUNT_ID="8ca8a7e4-...", PRODUCT_CODE="BOND", ACCOUNT_ATTRIBUTE_ID="invalid-id"
Processing: 
  1-5. All validations pass
  6. Validate attribute exists -> Not found
Output: HTTP 404 with error "Account attribute not found"
```

---

### BR-006: Product-Account Attribute Association

**Category**: TRANSFORMATION

**Description**: Account attributes must be linked to both an account and a product, enabling product-specific metadata on accounts.

**Source**: 
- File: MappedAccountAttributeProvider.scala
- Class/Object: AccountAttributeProvider
- Method: createAccountAttribute / updateAccountAttribute
- Lines: Referenced in user story acceptance criteria

**Business Logic**:
1. Each account attribute is associated with a specific ACCOUNT_ID
2. Each account attribute is associated with a specific PRODUCT_CODE
3. Optionally, an attribute can be associated with a specific product_instance_code
4. This triple association (account + product + optional instance) enables flexible metadata management
5. The attribute is uniquely identified by its account_attribute_id

**Variables**:
- **Input**: ACCOUNT_ID, PRODUCT_CODE, product_instance_code (optional)
- **Output**: Attribute with complete association metadata
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Account association | Attribute belongs to specific account | Required ACCOUNT_ID |
| Product association | Attribute is product-specific | Required PRODUCT_CODE |
| Instance association | Attribute may be instance-specific | Optional product_instance_code |

**Business Impact**: 
Enables banks to manage product-specific attributes on accounts, supporting use cases like tracking ISIN codes for bonds, loan identifiers for credit products, or maturity dates for term deposits. This flexible association model supports diverse financial product requirements.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/accounts/ACCOUNT_ID/products/PRODUCT_CODE/attribute - Create with associations
- PUT /banks/BANK_ID/accounts/ACCOUNT_ID/products/PRODUCT_CODE/attributes/ACCOUNT_ATTRIBUTE_ID - Update associations

**Related Test Cases**:
- Test cases verifying correct association storage
- Test cases for product_instance_code handling

**Migration Notes for Go**:
- Model the associations as foreign key relationships or embedded references
- Ensure the product_instance_code is properly handled as optional (pointer or nullable type in Go)
- Consider indexing on ACCOUNT_ID and PRODUCT_CODE for efficient queries

**Example Scenarios**:
```
Scenario 1: Attribute with product instance
Input: 
  ACCOUNT_ID = "8ca8a7e4-6d02-40e3-a129-0b2bf89de9f0"
  PRODUCT_CODE = "BOND"
  product_instance_code = "INST001"
  name = "ISIN", type = "STRING", value = "GB0002634946"
Processing: Create attribute linked to account, product, and specific instance
Output: Attribute stored with all three associations

Scenario 2: Attribute without product instance
Input: 
  ACCOUNT_ID = "8ca8a7e4-6d02-40e3-a129-0b2bf89de9f0"
  PRODUCT_CODE = "SAVINGS"
  product_instance_code = null
  name = "TAX_NUMBER", type = "STRING", value = "123456789"
Processing: Create attribute linked to account and product only
Output: Attribute stored with account and product associations, no instance
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /banks/BANK_ID/accounts/ACCOUNT_ID/products/PRODUCT_CODE/attribute | POST | Attribute type validation, Entity existence validation, Authorization enforcement, Creation workflow, Product-account association | BR-001, BR-002, BR-003, BR-004, BR-006 |
| /banks/BANK_ID/accounts/ACCOUNT_ID/products/PRODUCT_CODE/attributes/ACCOUNT_ATTRIBUTE_ID | PUT | Attribute type validation, Entity existence validation, Authorization enforcement, Update workflow, Product-account association | BR-001, BR-002, BR-003, BR-005, BR-006 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestAttributeTypeValidation | Pending | Pending |
| BR-002 | TestEntityExistenceValidation | Pending | Pending |
| BR-003 | TestAuthorizationEntitlement | Pending | Pending |
| BR-004 | TestCreateAccountAttribute | Pending | Pending |
| BR-005 | TestUpdateAccountAttribute | Pending | Pending |
| BR-006 | TestProductAccountAssociation | Pending | Pending |

## Notes and Assumptions

1. **Scope Limitation**: Per the user story, only CREATE and UPDATE operations are in scope for this capability. GET, DELETE, and LIST operations are explicitly excluded as they are not mentioned in the capability description.

2. **Attribute Type Enum**: The four valid attribute types (STRING, INTEGER, DOUBLE, DATE_WITH_DAY) are assumed to be the complete set based on the user story. Additional types may exist in the actual Scala implementation.

3. **HTTP Status Codes**: The user story indicates HTTP 201 Created for both create and update operations. This should be verified against the actual Scala implementation during migration.

4. **Product Instance Code**: This field is optional and may be null/empty. The Go implementation should handle this appropriately using pointers or nullable types.

5. **Entitlement Names**: The exact entitlement names (`canCreateAccountAttributeAtOneBank`, `canUpdateAccountAttribute`) are taken from the user story and should be verified against the actual entitlement system.

6. **Typical Use Cases**: Account attributes are commonly used for financial product identifiers such as ISIN, VKN, REDCODE, LOAN_ID, ISSUE_DATE, MATURITY_DATE, and TRADABLE flags.
