# Business Rules Extraction

**Extracted From**: Open Bank Project - Bank Creation and Configuration Capability
**Analysis Date**: 2026-01-09
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 6
- API Endpoints Analyzed: 4
- Rule Categories:
  - Calculations: 0
  - Decisions: 2
  - Thresholds: 1
  - Aggregations: 0
  - Workflows: 2
  - Transformations: 1

## Business Rules Catalog

### BR-001: Bank ID Uniqueness Validation

**Category**: DECISION

**Description**: Each bank created on the platform must have a unique identifier that does not conflict with any existing bank in the system.

**Source**: 
- File: APIMethods600.scala
- Class/Object: APIMethods600
- Method: createBank
- Lines: Bank creation endpoint handler

**Business Logic**:
1. When a new bank creation request is received, extract the bank_id from the request
2. Query the existing banks database to check if the bank_id already exists
3. If the bank_id exists, reject the creation request with an appropriate error
4. If the bank_id is unique, proceed with bank creation

**Variables**:
- **Input**: bank_id (String) - The proposed unique identifier for the new bank
- **Output**: Boolean - Whether the bank_id is unique and creation can proceed
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| bank_id exists in database | Bank ID already taken | Reject creation |
| bank_id not in database | Bank ID is available | Allow creation |

**Business Impact**: 
Ensures data integrity and prevents duplicate bank entries in the system. Critical for maintaining unique references to financial institutions across the platform.

**API Endpoints Using This Rule**:
- POST /banks - Bank creation

**Related Test Cases**:
- Test case for duplicate bank ID rejection
- Test case for successful unique bank ID creation

**Migration Notes for Go**:
- Implement database lookup before insert operation
- Use appropriate error handling for duplicate key scenarios
- Consider using database unique constraints as additional safeguard

**Example Scenarios**:
```
Scenario 1: Unique Bank ID
Input: bank_id = "new-bank-123"
Processing: Check database for existing bank with ID "new-bank-123"
Output: Not found - proceed with creation

Scenario 2: Duplicate Bank ID
Input: bank_id = "existing-bank-001"
Processing: Check database for existing bank with ID "existing-bank-001"
Output: Found - reject creation with error "Bank ID already exists"
```

---

### BR-002: Bank ID Format Validation

**Category**: THRESHOLD

**Description**: Bank ID must meet specific format requirements: greater than 3 characters in length, cannot contain space characters, and cannot contain the "::::" character sequence.

**Source**: 
- File: APIMethods600.scala
- Class/Object: APIMethods600
- Method: createBank
- Lines: Bank ID validation logic

**Business Logic**:
1. Receive bank_id from the creation request
2. Validate that bank_id length is greater than 3 characters
3. Validate that bank_id does not contain any space characters
4. Validate that bank_id does not contain the "::::" character sequence
5. If any validation fails, reject the request with specific error message
6. If all validations pass, proceed with bank creation

**Variables**:
- **Input**: bank_id (String) - The proposed bank identifier
- **Output**: Boolean - Whether the bank_id format is valid
- **Constants**: 
  - Minimum length: 3 characters (exclusive)
  - Forbidden characters: space (" ")
  - Forbidden sequence: "::::"

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| length <= 3 | Bank ID too short | Minimum 4 characters required |
| contains space | Invalid characters | Spaces not allowed |
| contains "::::" | Reserved delimiter | System delimiter not allowed |

**Business Impact**: 
Ensures bank IDs are meaningful identifiers and prevents conflicts with system-reserved character sequences used for internal data parsing and routing.

**API Endpoints Using This Rule**:
- POST /banks - Bank creation

**Related Test Cases**:
- Test case for bank ID length validation
- Test case for space character rejection
- Test case for "::::" sequence rejection

**Migration Notes for Go**:
- Use strings.Contains() for substring checks
- Use len() for length validation (note: consider UTF-8 rune count vs byte count)
- Implement as a reusable validation function

**Example Scenarios**:
```
Scenario 1: Valid Bank ID
Input: bank_id = "my-bank-123"
Processing: Length=11 (>3), no spaces, no "::::"
Output: Valid - proceed with creation

Scenario 2: Bank ID Too Short
Input: bank_id = "abc"
Processing: Length=3 (not >3)
Output: Invalid - "Bank ID must be greater than 3 characters"

Scenario 3: Bank ID With Space
Input: bank_id = "my bank"
Processing: Contains space character
Output: Invalid - "Bank ID cannot contain spaces"

Scenario 4: Bank ID With Reserved Sequence
Input: bank_id = "bank::::test"
Processing: Contains "::::" sequence
Output: Invalid - "Bank ID cannot contain '::::' characters"
```

---

### BR-003: Automatic Role Assignment on Bank Creation

**Category**: WORKFLOW

**Description**: When a user successfully creates a new bank, they are automatically granted the CanCreateEntitlementAtOneBank and CanReadDynamicResourceDocsAtOneBank roles for that specific bank.

**Source**: 
- File: APIMethods600.scala
- Class/Object: APIMethods600
- Method: createBank
- Lines: Post-creation role assignment logic

**Business Logic**:
1. After successful bank creation, identify the user who initiated the creation
2. Automatically create entitlement: CanCreateEntitlementAtOneBank for the new bank
3. Automatically create entitlement: CanReadDynamicResourceDocsAtOneBank for the new bank
4. Associate these entitlements with the creating user
5. Return success response with bank details

**Variables**:
- **Input**: 
  - user_id (String) - The ID of the user creating the bank
  - bank_id (String) - The ID of the newly created bank
- **Output**: 
  - Entitlements created for the user at the new bank
- **Constants**: 
  - Role names: "CanCreateEntitlementAtOneBank", "CanReadDynamicResourceDocsAtOneBank"

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank creation successful | Trigger role assignment | Automatic |
| User authenticated | Valid user for role assignment | Required |

**Business Impact**: 
Enables the bank creator to immediately manage entitlements and access documentation for their newly created bank, streamlining the onboarding process and reducing administrative overhead.

**API Endpoints Using This Rule**:
- POST /banks - Bank creation (triggers automatic role assignment)

**Related Test Cases**:
- Test case verifying role assignment after bank creation
- Test case verifying creator can create entitlements at new bank

**Migration Notes for Go**:
- Implement as a post-creation hook or within a transaction
- Ensure atomicity - if role assignment fails, consider rollback strategy
- Use dependency injection for entitlement service

**Example Scenarios**:
```
Scenario 1: Successful Bank Creation with Role Assignment
Input: user_id = "user-001", bank_id = "new-bank-123"
Processing: 
  1. Create bank "new-bank-123"
  2. Create entitlement CanCreateEntitlementAtOneBank for user-001 at new-bank-123
  3. Create entitlement CanReadDynamicResourceDocsAtOneBank for user-001 at new-bank-123
Output: Bank created, user has management roles for the bank
```

---

### BR-004: Automatic Settlement Account Creation in SANDBOX Mode

**Category**: WORKFLOW

**Description**: When a new bank is created in SANDBOX mode (connector=mapped), default incoming and outgoing settlement accounts are automatically created with EUR currency.

**Source**: 
- File: APIMethods600.scala / NewStyle.function
- Class/Object: NewStyle.function
- Method: createOrUpdateBank
- Lines: Settlement account creation logic

**Business Logic**:
1. Check if the system is running in SANDBOX mode (connector=mapped)
2. If in SANDBOX mode and bank creation is successful:
   a. Create default incoming settlement account with:
      - Account ID: OBP_DEFAULT_INCOMING_ACCOUNT_ID
      - Currency: EUR
   b. Create default outgoing settlement account with:
      - Account ID: OBP_DEFAULT_OUTGOING_ACCOUNT_ID
      - Currency: EUR
3. Associate both settlement accounts with the newly created bank
4. If not in SANDBOX mode, skip automatic settlement account creation

**Variables**:
- **Input**: 
  - bank_id (String) - The ID of the newly created bank
  - connector_mode (String) - System configuration for connector type
- **Output**: 
  - Two settlement accounts created for the bank
- **Constants**: 
  - Default incoming account ID: "OBP_DEFAULT_INCOMING_ACCOUNT_ID"
  - Default outgoing account ID: "OBP_DEFAULT_OUTGOING_ACCOUNT_ID"
  - Default currency: "EUR"
  - SANDBOX mode indicator: connector=mapped

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| connector=mapped | SANDBOX mode active | Create settlement accounts |
| connector!=mapped | Production mode | Skip automatic creation |

**Business Impact**: 
Enables immediate payment processing capabilities for newly created banks in sandbox/testing environments, facilitating rapid development and testing of banking integrations.

**API Endpoints Using This Rule**:
- POST /banks - Bank creation (triggers settlement account creation in SANDBOX)

**Related Test Cases**:
- Test case for settlement account creation in SANDBOX mode
- Test case verifying no automatic creation in production mode

**Migration Notes for Go**:
- Implement environment/configuration check for connector mode
- Create settlement accounts within the same transaction as bank creation
- Consider making default currency configurable for future multi-currency support

**Example Scenarios**:
```
Scenario 1: SANDBOX Mode Bank Creation
Input: bank_id = "test-bank-001", connector = "mapped"
Processing: 
  1. Create bank "test-bank-001"
  2. Create incoming settlement account (OBP_DEFAULT_INCOMING_ACCOUNT_ID, EUR)
  3. Create outgoing settlement account (OBP_DEFAULT_OUTGOING_ACCOUNT_ID, EUR)
Output: Bank created with two default settlement accounts

Scenario 2: Production Mode Bank Creation
Input: bank_id = "prod-bank-001", connector = "kafka" (or other)
Processing: 
  1. Create bank "prod-bank-001"
  2. Skip automatic settlement account creation
Output: Bank created without automatic settlement accounts
```

---

### BR-005: Bank Attribute Type Validation

**Category**: DECISION

**Description**: Bank attributes must specify a valid data type from the allowed set: STRING, INTEGER, DOUBLE, or DATE_WITH_DAY.

**Source**: 
- File: APIMethods400.scala
- Class/Object: APIMethods400
- Method: createBankAttribute, updateBankAttribute
- Lines: Attribute type validation logic

**Business Logic**:
1. Receive bank attribute creation/update request with type field
2. Validate that the type value matches one of the allowed types:
   - STRING: For text-based attribute values
   - INTEGER: For whole number attribute values
   - DOUBLE: For decimal number attribute values
   - DATE_WITH_DAY: For date attribute values
3. If type is not in the allowed list, reject the request
4. If type is valid, proceed with attribute creation/update

**Variables**:
- **Input**: 
  - type (String) - The data type for the bank attribute
  - name (String) - The attribute name
  - value (String) - The attribute value
  - is_active (Boolean) - Whether the attribute is active
- **Output**: 
  - Boolean - Whether the attribute type is valid
- **Constants**: 
  - Allowed types: ["STRING", "INTEGER", "DOUBLE", "DATE_WITH_DAY"]

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| type = "STRING" | Text attribute | Valid |
| type = "INTEGER" | Whole number attribute | Valid |
| type = "DOUBLE" | Decimal number attribute | Valid |
| type = "DATE_WITH_DAY" | Date attribute | Valid |
| type = other | Unknown type | Invalid - reject |

**Business Impact**: 
Ensures data consistency and enables proper type handling for bank metadata. Supports downstream processing and reporting that may depend on attribute data types.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/attribute - Create bank attribute
- PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID - Update bank attribute

**Related Test Cases**:
- Test case for each valid attribute type
- Test case for invalid attribute type rejection

**Migration Notes for Go**:
- Implement as an enum or constant set for type validation
- Use switch statement or map lookup for validation
- Consider using Go's type system to enforce attribute types at compile time

**Example Scenarios**:
```
Scenario 1: Valid STRING Type
Input: name = "region", type = "STRING", value = "Europe"
Processing: Type "STRING" is in allowed list
Output: Valid - create attribute

Scenario 2: Valid INTEGER Type
Input: name = "branch_count", type = "INTEGER", value = "150"
Processing: Type "INTEGER" is in allowed list
Output: Valid - create attribute

Scenario 3: Invalid Type
Input: name = "custom_field", type = "BOOLEAN", value = "true"
Processing: Type "BOOLEAN" is not in allowed list
Output: Invalid - "Attribute type must be one of: STRING, INTEGER, DOUBLE, DATE_WITH_DAY"
```

---

### BR-006: Consumer Credential Validation for Bank Creation

**Category**: TRANSFORMATION

**Description**: Valid consumer credentials are required to create a bank. The system must verify that the API consumer making the request has valid, active credentials.

**Source**: 
- File: APIMethods600.scala
- Class/Object: APIMethods600
- Method: createBank
- Lines: Consumer validation logic

**Business Logic**:
1. Extract consumer credentials from the API request
2. Validate that the consumer is registered in the system
3. Verify that the consumer credentials are active and not expired
4. If consumer validation fails, reject the bank creation request
5. If consumer is valid, proceed with authorization checks and bank creation

**Variables**:
- **Input**: 
  - consumer_key (String) - The API consumer key
  - consumer_secret (String) - The API consumer secret (if applicable)
- **Output**: 
  - Boolean - Whether the consumer credentials are valid
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Consumer registered | Known API consumer | Required |
| Consumer active | Consumer not disabled | Required |
| Consumer not expired | Credentials still valid | Required |

**Business Impact**: 
Ensures that only authorized API consumers can create banks on the platform, maintaining security and enabling audit trails for bank creation activities.

**API Endpoints Using This Rule**:
- POST /banks - Bank creation

**Related Test Cases**:
- Test case for valid consumer credentials
- Test case for invalid/missing consumer credentials
- Test case for expired consumer credentials

**Migration Notes for Go**:
- Implement as middleware or interceptor for API authentication
- Use secure credential storage and comparison
- Consider rate limiting for failed authentication attempts

**Example Scenarios**:
```
Scenario 1: Valid Consumer
Input: consumer_key = "valid-key-123", consumer registered and active
Processing: Lookup consumer, verify status is active
Output: Valid - proceed with bank creation

Scenario 2: Invalid Consumer
Input: consumer_key = "unknown-key-456"
Processing: Lookup consumer, not found in database
Output: Invalid - "Invalid consumer credentials"

Scenario 3: Expired Consumer
Input: consumer_key = "expired-key-789", consumer found but expired
Processing: Lookup consumer, found but expiry_date < current_date
Output: Invalid - "Consumer credentials have expired"
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /banks | POST | Bank ID uniqueness, Bank ID format, Role assignment, Settlement accounts, Consumer validation | BR-001, BR-002, BR-003, BR-004, BR-006 |
| /banks/BANK_ID/attribute | POST | Attribute type validation | BR-005 |
| /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID | PUT | Attribute type validation | BR-005 |
| /banks/BANK_ID/settlement-accounts | POST | Settlement account creation (manual) | N/A (manual creation) |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestBankIdUniqueness | Pending | Pending |
| BR-002 | TestBankIdFormat | Pending | Pending |
| BR-003 | TestAutoRoleAssignment | Pending | Pending |
| BR-004 | TestSettlementAccountCreation | Pending | Pending |
| BR-005 | TestAttributeTypeValidation | Pending | Pending |
| BR-006 | TestConsumerValidation | Pending | Pending |

## Notes and Assumptions

1. **Source Code References**: The source file references are based on the classes/services mentioned in the user story. Actual line numbers should be verified against the Scala codebase.

2. **SANDBOX Mode**: The automatic settlement account creation (BR-004) is specific to SANDBOX mode. Production deployments may require manual settlement account configuration.

3. **Currency Configuration**: Default settlement accounts use EUR currency. Future implementations may need to support configurable currencies for multi-currency platforms.

4. **Role Names**: The exact role names (CanCreateEntitlementAtOneBank, CanReadDynamicResourceDocsAtOneBank) should be verified against the actual Scala implementation.

5. **Attribute Type Extensibility**: The current attribute types (STRING, INTEGER, DOUBLE, DATE_WITH_DAY) may need extension for additional data types in future versions.

6. **Consumer Validation**: The specific mechanism for consumer credential validation (OAuth, API keys, etc.) should be verified against the actual implementation.
