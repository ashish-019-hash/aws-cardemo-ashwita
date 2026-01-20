# Business Rules Extraction

**Extracted From**: Account Creation Capability (User Story)
**Analysis Date**: 2026-01-20
**Analyst**: Expert Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 8
- API Endpoints Analyzed: 2
- Rule Categories:
  - Calculations: 0
  - Decisions: 3
  - Thresholds: 0
  - Aggregations: 0
  - Workflows: 2
  - Transformations: 3

## Business Rules Catalog

### BR-001: Account-Bank Association Rule

**Category**: DECISION

**Description**: Every bank account must be created under and associated with a specific, valid bank entity. The account cannot exist independently without a bank association.

**Source**: 
- File: Account Creation User Story
- Section: Business Rules, Acceptance Criteria
- Reference: "Each account must be created under a specific bank entity"

**Business Logic**:
1. When creating a new account, the system receives a Bank ID parameter
2. The system validates that the Bank ID references an existing, active bank on the platform
3. If the bank exists and is active, the account is associated with that bank
4. If the bank does not exist or is inactive, the account creation is rejected

**Variables**:
- **Input**: Bank ID (required) - the identifier of the bank under which the account is created
- **Output**: Bank association confirmation or rejection error
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank exists | Target bank is registered on platform | Bank ID must match existing bank |
| Bank is active | Bank is operational and accepting accounts | Bank status = active |

**Business Impact**: 
Ensures regulatory compliance and proper organizational structure by guaranteeing every account belongs to a licensed banking entity. Prevents orphaned accounts and maintains data integrity.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/accounts - Create new bank account
- PUT /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID} - Create account with specified ID

**Related Test Cases**:
- Test account creation with valid bank ID
- Test account creation with invalid/non-existent bank ID
- Test account creation with inactive bank

**Migration Notes for Go**:
- Implement bank validation as a separate service call before account creation
- Use proper error types for bank not found vs bank inactive scenarios
- Consider caching bank validation results for performance

**Example Scenarios**:
```
Scenario 1: Valid bank association
Input: Bank ID = "bank-123" (exists and active)
Processing: Validate bank exists -> Associate account with bank
Output: Account created with bank_id = "bank-123"

Scenario 2: Invalid bank
Input: Bank ID = "bank-invalid" (does not exist)
Processing: Validate bank exists -> Bank not found
Output: Error - "Bank not found"
```

---

### BR-002: Ownership Assignment Rule

**Category**: DECISION

**Description**: Account creation must include ownership specification, linking the account to a valid user or customer entity. The owner must exist and be eligible for account ownership.

**Source**: 
- File: Account Creation User Story
- Section: Business Rules, Acceptance Criteria
- Reference: "Account creation must include ownership specification"

**Business Logic**:
1. Account creation request includes owner information (user_id or customer_id)
2. System validates that the specified owner exists in the system
3. System verifies the owner is eligible to own accounts (not blocked, not suspended)
4. If validation passes, ownership is assigned to the account
5. If validation fails, account creation is rejected with appropriate error

**Variables**:
- **Input**: user_id (required) - identifier of the account owner
- **Output**: Ownership assignment confirmation or rejection error
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Owner exists | User/customer is registered in system | user_id must match existing user |
| Owner eligible | User is allowed to own accounts | User status not blocked/suspended |

**Business Impact**: 
Establishes clear accountability and access rights for each account. Enables proper authorization for account operations and ensures regulatory compliance for know-your-customer (KYC) requirements.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/accounts - Create new bank account
- PUT /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID} - Create account with specified ID

**Related Test Cases**:
- Test account creation with valid user ID
- Test account creation with non-existent user ID
- Test account creation with blocked/suspended user

**Migration Notes for Go**:
- Implement user validation service integration
- Handle different owner types (user vs customer) appropriately
- Consider ownership transfer scenarios in design

**Example Scenarios**:
```
Scenario 1: Valid ownership assignment
Input: user_id = "user-456" (exists and eligible)
Processing: Validate user exists -> Check eligibility -> Assign ownership
Output: Account created with owner = "user-456"

Scenario 2: Ineligible owner
Input: user_id = "user-blocked" (exists but suspended)
Processing: Validate user exists -> Check eligibility -> User suspended
Output: Error - "User not eligible for account ownership"
```

---

### BR-003: Authorization Requirement Rule

**Category**: DECISION

**Description**: Only authorized users with appropriate roles (Bank Administrator, Account Manager) and entitlements (CanCreateAccount) can create bank accounts.

**Source**: 
- File: Account Creation User Story
- Section: Business Rules, Dependencies
- Reference: "Only authorized users can create accounts"

**Business Logic**:
1. Before processing account creation, verify user authentication
2. Check if authenticated user has required role (Bank Administrator or Account Manager)
3. Verify user has CanCreateAccount entitlement for the target bank
4. If all authorization checks pass, proceed with account creation
5. If any check fails, reject with authorization error

**Variables**:
- **Input**: Authenticated user context, target bank ID
- **Output**: Authorization granted or denied
- **Constants**: Required roles: Bank Administrator, Account Manager; Required entitlement: CanCreateAccount

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| User authenticated | User has valid session | Valid authentication token |
| Has required role | User has admin/manager role | Role in [Bank Administrator, Account Manager] |
| Has entitlement | User can create accounts at bank | CanCreateAccount for bank_id |

**Business Impact**: 
Ensures proper access control and prevents unauthorized account creation. Maintains security and compliance with banking regulations requiring proper authorization for account operations.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/accounts - Create new bank account
- PUT /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID} - Create account with specified ID

**Related Test Cases**:
- Test account creation with authorized user
- Test account creation with unauthorized user
- Test account creation without CanCreateAccount entitlement

**Migration Notes for Go**:
- Implement middleware for authorization checks
- Use role-based access control (RBAC) pattern
- Cache entitlements for performance while ensuring freshness

**Example Scenarios**:
```
Scenario 1: Authorized user
Input: User with Bank Administrator role and CanCreateAccount entitlement
Processing: Verify authentication -> Check role -> Check entitlement -> Authorize
Output: Authorization granted, proceed with creation

Scenario 2: Unauthorized user
Input: User with Customer role (no admin privileges)
Processing: Verify authentication -> Check role -> Role insufficient
Output: Error - "Insufficient privileges to create accounts"
```

---

### BR-004: Account Identifier Generation Rule

**Category**: TRANSFORMATION

**Description**: The system must generate a unique account identifier for each newly created account when not explicitly specified, ensuring no duplicate account IDs exist.

**Source**: 
- File: Account Creation User Story
- Section: Acceptance Criteria
- Reference: "The system shall generate a unique account identifier for each newly created account"

**Business Logic**:
1. If account ID is not provided in request (POST endpoint), generate new unique ID
2. If account ID is provided (PUT endpoint), validate it doesn't already exist
3. Generated IDs must be unique across the entire system
4. ID format should follow platform conventions (alphanumeric, specific length)
5. Store the account with the assigned/generated ID

**Variables**:
- **Input**: Optional account_id from request
- **Output**: Unique account_id (generated or validated)
- **Constants**: ID format rules, uniqueness constraint

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| ID not provided | System generates ID | POST endpoint behavior |
| ID provided | System validates uniqueness | PUT endpoint behavior |
| ID unique | No duplicate accounts | Must not exist in system |

**Business Impact**: 
Ensures each account can be uniquely identified for all banking operations. Prevents data corruption from duplicate IDs and enables reliable account referencing across the platform.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/accounts - Generate new account ID
- PUT /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID} - Use specified account ID

**Related Test Cases**:
- Test automatic ID generation uniqueness
- Test creation with pre-specified valid ID
- Test creation with duplicate ID (should fail)

**Migration Notes for Go**:
- Use UUID or similar for ID generation
- Implement database-level uniqueness constraint
- Consider distributed ID generation if needed

**Example Scenarios**:
```
Scenario 1: Auto-generated ID
Input: No account_id provided (POST endpoint)
Processing: Generate unique ID -> Verify uniqueness -> Assign to account
Output: account_id = "acc-uuid-12345" (generated)

Scenario 2: Specified ID
Input: account_id = "custom-acc-001" (PUT endpoint)
Processing: Check if ID exists -> ID is unique -> Use specified ID
Output: account_id = "custom-acc-001" (as specified)
```

---

### BR-005: Account Initial State Rule

**Category**: WORKFLOW

**Description**: Newly created accounts must be initialized with a proper state (active, pending, etc.) based on business configuration and creation context.

**Source**: 
- File: Account Creation User Story
- Section: Acceptance Criteria
- Reference: "The system shall ensure the account is created with proper initial state"

**Business Logic**:
1. Determine initial account state based on creation context
2. If all validations pass and no approval required, set state to ACTIVE
3. If additional approval is required, set state to PENDING
4. Record the initial state with the account
5. Initial state determines what operations are available on the account

**Variables**:
- **Input**: Account creation context, approval requirements
- **Output**: Initial account state
- **Constants**: Possible states: ACTIVE, PENDING, PENDING_APPROVAL

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| No approval needed | Standard account creation | Initial state = ACTIVE |
| Approval required | Account needs review | Initial state = PENDING |
| High-value account | May require additional checks | Configurable threshold |

**Business Impact**: 
Controls when accounts become operational and ensures proper review processes are followed. Prevents premature use of accounts that require additional verification or approval.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/accounts - Create new bank account
- PUT /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID} - Create account with specified ID

**Related Test Cases**:
- Test account created with ACTIVE state
- Test account created with PENDING state when approval required
- Test operations blocked on PENDING accounts

**Migration Notes for Go**:
- Use enum/const for account states
- Implement state machine pattern for state transitions
- Make initial state configurable per bank/product

**Example Scenarios**:
```
Scenario 1: Standard account creation
Input: Regular account creation, no special requirements
Processing: All validations pass -> No approval needed -> Set ACTIVE
Output: Account created with status = ACTIVE

Scenario 2: Account requiring approval
Input: Account creation with approval workflow configured
Processing: Validations pass -> Approval required -> Set PENDING
Output: Account created with status = PENDING
```

---

### BR-006: Currency Validation Rule

**Category**: TRANSFORMATION

**Description**: Account currency must be a valid ISO 4217 currency code, and the account will operate in the specified currency for all balance and transaction operations.

**Source**: 
- File: Account Creation User Story
- Section: Data Validations, Input Data
- Reference: "Currency code must be a valid ISO 4217 currency code"

**Business Logic**:
1. Extract currency code from account creation request
2. Validate currency code against ISO 4217 standard
3. Verify the bank supports the specified currency
4. If valid and supported, assign currency to account
5. If invalid or unsupported, reject account creation

**Variables**:
- **Input**: currency (string) - ISO 4217 currency code (e.g., EUR, USD, GBP)
- **Output**: Validated currency assignment or error
- **Constants**: ISO 4217 currency codes, bank-supported currencies

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Valid ISO code | Currency is recognized standard | Must be in ISO 4217 list |
| Bank supports currency | Bank can handle this currency | Currency in bank's supported list |

**Business Impact**: 
Ensures accounts operate with valid, recognized currencies for proper financial operations. Enables correct balance calculations, transaction processing, and regulatory reporting.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/accounts - Create new bank account
- PUT /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID} - Create account with specified ID

**Related Test Cases**:
- Test account creation with valid currency (EUR, USD)
- Test account creation with invalid currency code
- Test account creation with unsupported currency for bank

**Migration Notes for Go**:
- Use a currency validation library or maintain ISO 4217 lookup
- Cache bank's supported currencies
- Consider multi-currency account scenarios

**Example Scenarios**:
```
Scenario 1: Valid currency
Input: currency = "EUR"
Processing: Validate ISO 4217 -> Check bank supports EUR -> Assign currency
Output: Account created with currency = EUR

Scenario 2: Invalid currency
Input: currency = "XYZ"
Processing: Validate ISO 4217 -> Code not found
Output: Error - "Invalid currency code: XYZ"
```

---

### BR-007: Account Routing Validation Rule

**Category**: TRANSFORMATION

**Description**: Account routing information (IBAN, account numbers) must conform to the specified scheme format and be valid for the routing type.

**Source**: 
- File: Account Creation User Story
- Section: Data Validations
- Reference: "Account routing schemes must be valid" and "Account routing addresses must conform to the specified scheme format"

**Business Logic**:
1. For each account routing entry, extract scheme and address
2. Validate scheme is a recognized routing type (IBAN, AccountNumber, etc.)
3. Validate address format matches the scheme requirements
4. For IBAN: validate checksum and country format
5. Store validated routing information with account

**Variables**:
- **Input**: account_routings array with scheme and address pairs
- **Output**: Validated routing information or validation errors
- **Constants**: Supported schemes: IBAN, AccountNumber, BBAN, etc.

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Valid scheme | Routing type is recognized | Scheme in supported list |
| Valid address format | Address matches scheme rules | Format per scheme specification |
| IBAN checksum valid | IBAN passes validation | Checksum algorithm passes |

**Business Impact**: 
Ensures accounts can be properly identified in payment networks and interbank transfers. Invalid routing information would prevent successful transactions and payments.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/accounts - Create new bank account
- PUT /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID} - Create account with specified ID

**Related Test Cases**:
- Test account creation with valid IBAN
- Test account creation with invalid IBAN checksum
- Test account creation with unsupported routing scheme

**Migration Notes for Go**:
- Use IBAN validation library for Go
- Implement scheme-specific validators
- Consider international routing requirements

**Example Scenarios**:
```
Scenario 1: Valid IBAN routing
Input: scheme = "IBAN", address = "DE89370400440532013000"
Processing: Validate scheme -> Validate IBAN format -> Validate checksum
Output: Routing information stored successfully

Scenario 2: Invalid IBAN
Input: scheme = "IBAN", address = "DE00000000000000000000"
Processing: Validate scheme -> Validate IBAN format -> Checksum fails
Output: Error - "Invalid IBAN checksum"
```

---

### BR-008: Account Creation Workflow Rule

**Category**: WORKFLOW

**Description**: Account creation follows a defined workflow: validate all inputs, create account entity, assign ownership, associate with bank, and return confirmation with created account details.

**Source**: 
- File: Account Creation User Story
- Section: Acceptance Criteria, Notes for Implementation
- Reference: "Validate all input data before attempting to persist to avoid partial creation states"

**Business Logic**:
1. Receive account creation request with all parameters
2. Validate authorization (BR-003)
3. Validate bank association (BR-001)
4. Validate ownership (BR-002)
5. Validate currency (BR-006)
6. Validate routing information (BR-007)
7. Generate or validate account ID (BR-004)
8. Create account entity with all validated data
9. Set initial state (BR-005)
10. Persist account to database
11. Return confirmation with complete account details

**Variables**:
- **Input**: Complete account creation request payload
- **Output**: Created account entity with all details or error response
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| All validations pass | Account can be created | No validation errors |
| Any validation fails | Account creation rejected | Return specific error |
| Persistence succeeds | Account stored successfully | Database write success |

**Business Impact**: 
Ensures atomic account creation with no partial states. Provides clear feedback on success or failure, enabling proper error handling and retry logic.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/accounts - Create new bank account
- PUT /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID} - Create account with specified ID

**Related Test Cases**:
- Test complete successful account creation workflow
- Test workflow stops at first validation failure
- Test rollback on persistence failure

**Migration Notes for Go**:
- Implement as a service method with clear step sequence
- Use transactions for database operations
- Return detailed error information for debugging

**Example Scenarios**:
```
Scenario 1: Successful creation
Input: Valid bank_id, user_id, currency, routing info
Processing: All validations pass -> Create entity -> Persist -> Return details
Output: {account_id, user_id, bank_id, currency, status: ACTIVE, created_at}

Scenario 2: Validation failure
Input: Invalid currency code
Processing: Bank validation passes -> Owner validation passes -> Currency validation fails
Output: Error - "Invalid currency code" (no account created)
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /obp/v5.1.0/banks/{BANK_ID}/accounts | POST | Bank association, ownership, authorization, ID generation, initial state, currency, routing, workflow | BR-001, BR-002, BR-003, BR-004, BR-005, BR-006, BR-007, BR-008 |
| /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID} | PUT | Bank association, ownership, authorization, ID validation, initial state, currency, routing, workflow | BR-001, BR-002, BR-003, BR-004, BR-005, BR-006, BR-007, BR-008 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestBankAssociation | Pending | Pending |
| BR-002 | TestOwnershipAssignment | Pending | Pending |
| BR-003 | TestAuthorizationCheck | Pending | Pending |
| BR-004 | TestAccountIdGeneration | Pending | Pending |
| BR-005 | TestInitialStateAssignment | Pending | Pending |
| BR-006 | TestCurrencyValidation | Pending | Pending |
| BR-007 | TestRoutingValidation | Pending | Pending |
| BR-008 | TestCreationWorkflow | Pending | Pending |

## Notes and Assumptions

1. **Assumptions Made**:
   - Account creation is an atomic operation (all-or-nothing)
   - Single ownership model assumed (one owner per account)
   - Initial state defaults to ACTIVE unless approval workflow is configured
   - Currency is required for account creation

2. **Gaps Identified**:
   - Specific account types and their validation rules not fully defined
   - Multi-owner account scenarios not addressed
   - Account limits and restrictions not specified
   - Notification/event triggers after creation not detailed

3. **Clarifications Needed**:
   - What are the mandatory vs optional parameters for account creation?
   - What account types are supported (checking, savings, current, etc.)?
   - Can an account have multiple owners?
   - What is the default initial balance for newly created accounts?
