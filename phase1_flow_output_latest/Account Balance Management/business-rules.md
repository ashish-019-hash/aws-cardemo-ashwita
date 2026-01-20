# Business Rules Extraction

**Extracted From**: Account Balance Management Capability
**Analysis Date**: 2026-01-20
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 7
- API Endpoints Analyzed: 3
- Rule Categories:
  - Calculations: 1
  - Decisions: 2
  - Thresholds: 0
  - Aggregations: 0
  - Workflows: 2
  - Transformations: 2

## Business Rules Catalog

### BR-001: Balance Record Creation

**Category**: WORKFLOW

**Description**: Create a new balance record for a specific bank account with auto-generated unique identifier

**Source**: 
- File: MappedBankAccountBalanceProvider.scala
- Class/Object: MappedBankAccountBalanceProvider
- Method: createOrUpdateBankAccountBalance
- Lines: N/A (to be identified during implementation)

**Business Logic**:
1. Validate that the bank exists and is active on the platform
2. Validate that the account exists within the specified bank
3. Generate a unique balance ID using UUID for the new record
4. Store the balance type and balance amount associated with the account
5. Automatically track reference date and last change date time for audit purposes
6. Return the created balance record with all identifiers

**Variables**:
- **Input**: 
  - BANK_ID: Identifier of the bank
  - ACCOUNT_ID: Identifier of the account
  - balance_type: Type of balance (e.g., available, booked)
  - balance_amount: Balance amount value as string
- **Output**: 
  - Complete balance record including bank_id, account_id, balance_id, balance_type, balance_amount
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank exists | Bank must be registered on platform | Valid BANK_ID |
| Account exists | Account must exist at specified bank | Valid ACCOUNT_ID |
| User authorized | User must have create permission | canCreateBankAccountBalance role |

**Business Impact**: 
Enables financial institutions to maintain accurate balance information for bank accounts, supporting double-entry bookkeeping and ensuring financial data integrity across the banking platform.

**API Endpoints Using This Rule**:
- POST /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances - Create new balance record

**Related Test Cases**:
- Test balance creation with valid bank and account
- Test balance creation with invalid bank ID
- Test balance creation with invalid account ID
- Test balance creation without required permissions

**Migration Notes for Go**:
- Use UUID package for generating unique balance IDs
- Implement proper error handling for validation failures
- Use Go structs for balance record representation
- Consider using Go's time package for audit timestamps

**Example Scenarios**:
```
Scenario 1: Successful balance creation
Input: BANK_ID = "bank-001", ACCOUNT_ID = "acc-001", balance_type = "available", balance_amount = "10000.00"
Processing: Validate bank, validate account, generate UUID, store record
Output: { bank_id: "bank-001", account_id: "acc-001", balance_id: "uuid-generated", balance_type: "available", balance_amount: "10000.00" }

Scenario 2: Balance creation with non-existent account
Input: BANK_ID = "bank-001", ACCOUNT_ID = "invalid-acc", balance_type = "available", balance_amount = "5000.00"
Processing: Validate bank (pass), validate account (fail)
Output: Error - Account not found
```

---

### BR-002: Balance Record Update

**Category**: WORKFLOW

**Description**: Update an existing balance record identified by its unique balance ID

**Source**: 
- File: MappedBankAccountBalanceProvider.scala
- Class/Object: MappedBankAccountBalanceProvider
- Method: createOrUpdateBankAccountBalance
- Lines: N/A (to be identified during implementation)

**Business Logic**:
1. Validate that the bank exists and is active on the platform
2. Validate that the account exists within the specified bank
3. Validate that the balance record exists with the specified balance ID
4. Update the balance type and/or balance amount as provided
5. Update the last change date time for audit trail
6. Return the updated balance record

**Variables**:
- **Input**: 
  - BANK_ID: Identifier of the bank
  - ACCOUNT_ID: Identifier of the account
  - BALANCE_ID: Identifier of the balance record to update
  - balance_type: Updated type of balance
  - balance_amount: Updated balance amount value
- **Output**: 
  - Updated balance record including bank_id, account_id, balance_id, balance_type, balance_amount
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank exists | Bank must be registered on platform | Valid BANK_ID |
| Account exists | Account must exist at specified bank | Valid ACCOUNT_ID |
| Balance exists | Balance record must exist | Valid BALANCE_ID |
| User authorized | User must have update permission | canUpdateBankAccountBalance role |

**Business Impact**: 
Allows financial institutions to correct or adjust balance information, ensuring accurate financial records and supporting reconciliation processes.

**API Endpoints Using This Rule**:
- PUT /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances/{BALANCE_ID} - Update existing balance record

**Related Test Cases**:
- Test balance update with valid identifiers
- Test balance update with non-existent balance ID
- Test balance update without required permissions
- Test partial update (only balance_type or only balance_amount)

**Migration Notes for Go**:
- Implement idempotent update operation
- Use Go's time package for updating audit timestamps
- Handle partial updates appropriately
- Return proper HTTP status codes (200 OK for success)

**Example Scenarios**:
```
Scenario 1: Successful balance update
Input: BANK_ID = "bank-001", ACCOUNT_ID = "acc-001", BALANCE_ID = "bal-001", balance_type = "booked", balance_amount = "15000.00"
Processing: Validate all identifiers, update record, update timestamp
Output: { bank_id: "bank-001", account_id: "acc-001", balance_id: "bal-001", balance_type: "booked", balance_amount: "15000.00" }

Scenario 2: Update non-existent balance
Input: BANK_ID = "bank-001", ACCOUNT_ID = "acc-001", BALANCE_ID = "invalid-bal", balance_type = "available", balance_amount = "5000.00"
Processing: Validate bank (pass), validate account (pass), validate balance (fail)
Output: Error - Balance record not found
```

---

### BR-003: Balance Record Deletion

**Category**: DECISION

**Description**: Delete an existing balance record by its unique balance ID

**Source**: 
- File: MappedBankAccountBalanceProvider.scala
- Class/Object: MappedBankAccountBalanceProvider
- Method: deleteBankAccountBalance
- Lines: N/A (to be identified during implementation)

**Business Logic**:
1. Validate that the bank exists and is active on the platform
2. Validate that the account exists within the specified bank
3. Validate that the balance record exists with the specified balance ID
4. Remove the balance record from the system
5. Return success confirmation (no content)

**Variables**:
- **Input**: 
  - BANK_ID: Identifier of the bank
  - ACCOUNT_ID: Identifier of the account
  - BALANCE_ID: Identifier of the balance record to delete
- **Output**: 
  - Empty response (HTTP 204 No Content)
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank exists | Bank must be registered on platform | Valid BANK_ID |
| Account exists | Account must exist at specified bank | Valid ACCOUNT_ID |
| Balance exists | Balance record must exist | Valid BALANCE_ID |
| User authorized | User must have delete permission | canDeleteBankAccountBalance role |

**Business Impact**: 
Enables removal of obsolete or erroneous balance records, maintaining data hygiene and supporting accurate financial reporting.

**API Endpoints Using This Rule**:
- DELETE /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances/{BALANCE_ID} - Delete balance record

**Related Test Cases**:
- Test balance deletion with valid identifiers
- Test balance deletion with non-existent balance ID
- Test balance deletion without required permissions
- Test idempotent deletion behavior

**Migration Notes for Go**:
- Implement soft delete vs hard delete based on business requirements
- Return HTTP 204 No Content on successful deletion
- Handle concurrent deletion attempts gracefully
- Consider audit logging for deleted records

**Example Scenarios**:
```
Scenario 1: Successful balance deletion
Input: BANK_ID = "bank-001", ACCOUNT_ID = "acc-001", BALANCE_ID = "bal-001"
Processing: Validate all identifiers, delete record
Output: HTTP 204 No Content

Scenario 2: Delete non-existent balance
Input: BANK_ID = "bank-001", ACCOUNT_ID = "acc-001", BALANCE_ID = "invalid-bal"
Processing: Validate bank (pass), validate account (pass), validate balance (fail)
Output: Error - Balance record not found
```

---

### BR-004: Balance Amount Currency Conversion

**Category**: TRANSFORMATION

**Description**: Convert balance amounts to smallest currency units for storage and back for display

**Source**: 
- File: BankAccountBalance.scala / JSONFactory510.scala
- Class/Object: BankAccountBalance, JSONFactory510
- Method: Various serialization/deserialization methods
- Lines: N/A (to be identified during implementation)

**Business Logic**:
1. When storing balance amount, convert from display format to smallest currency unit (e.g., dollars to cents)
2. When retrieving balance amount, convert from smallest currency unit back to display format
3. Use the account's associated currency for determining conversion factor
4. Maintain precision using BigDecimal for all calculations

**Variables**:
- **Input**: 
  - balance_amount: Balance amount in display format (e.g., "100.50")
  - account_currency: Currency associated with the account
- **Output**: 
  - Stored value in smallest currency unit (e.g., 10050 cents)
  - Display value in standard format (e.g., "100.50")
- **Constants**: 
  - Currency conversion factors (e.g., 100 for USD/EUR, 1 for JPY)

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Valid numeric | Amount must be parseable as BigDecimal | Valid numeric string |
| Currency known | Account must have associated currency | Valid currency code |

**Business Impact**: 
Ensures accurate financial calculations by avoiding floating-point precision issues, supporting international currencies with different decimal places.

**API Endpoints Using This Rule**:
- POST /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances - Balance amount conversion on create
- PUT /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances/{BALANCE_ID} - Balance amount conversion on update

**Related Test Cases**:
- Test conversion with standard currencies (USD, EUR)
- Test conversion with zero-decimal currencies (JPY)
- Test precision handling with many decimal places
- Test invalid numeric input handling

**Migration Notes for Go**:
- Use Go's math/big package or shopspring/decimal for precise decimal handling
- Implement currency-aware conversion functions
- Avoid float64 for monetary calculations
- Consider using int64 for smallest unit storage

**Example Scenarios**:
```
Scenario 1: USD balance storage
Input: balance_amount = "1234.56", currency = "USD"
Processing: 1234.56 * 100 = 123456 (cents)
Output: Stored as 123456, displayed as "1234.56"

Scenario 2: JPY balance storage (no decimals)
Input: balance_amount = "10000", currency = "JPY"
Processing: 10000 * 1 = 10000 (yen)
Output: Stored as 10000, displayed as "10000"
```

---

### BR-005: Role-Based Access Control for Balance Operations

**Category**: DECISION

**Description**: Enforce role-based permissions for balance management operations

**Source**: 
- File: API endpoint definitions
- Class/Object: Various API controllers
- Method: Authorization checks
- Lines: N/A (to be identified during implementation)

**Business Logic**:
1. Verify user is authenticated with valid token
2. Check user has appropriate role for the requested operation:
   - canCreateBankAccountBalance for POST operations
   - canUpdateBankAccountBalance for PUT operations
   - canDeleteBankAccountBalance for DELETE operations
3. Reject request with appropriate error if authorization fails
4. Proceed with operation if authorization succeeds

**Variables**:
- **Input**: 
  - User authentication token
  - Requested operation type (create/update/delete)
- **Output**: 
  - Authorization decision (allow/deny)
  - Error response if denied
- **Constants**: 
  - Role names: canCreateBankAccountBalance, canUpdateBankAccountBalance, canDeleteBankAccountBalance

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Token valid | User must be authenticated | Non-expired valid token |
| Role assigned | User must have required role | Specific role for operation |

**Business Impact**: 
Ensures only authorized personnel can modify financial balance records, maintaining security and compliance with financial regulations.

**API Endpoints Using This Rule**:
- POST /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances - Requires canCreateBankAccountBalance
- PUT /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances/{BALANCE_ID} - Requires canUpdateBankAccountBalance
- DELETE /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances/{BALANCE_ID} - Requires canDeleteBankAccountBalance

**Related Test Cases**:
- Test operation with valid role
- Test operation without required role
- Test operation with expired token
- Test operation without authentication

**Migration Notes for Go**:
- Implement middleware for authentication/authorization
- Use Go's context package for passing user information
- Consider using a dedicated authorization library
- Return HTTP 401 for authentication failures, 403 for authorization failures

**Example Scenarios**:
```
Scenario 1: Authorized balance creation
Input: User with canCreateBankAccountBalance role, POST request
Processing: Validate token, check role assignment
Output: Authorization granted, proceed with creation

Scenario 2: Unauthorized balance deletion
Input: User without canDeleteBankAccountBalance role, DELETE request
Processing: Validate token (pass), check role assignment (fail)
Output: HTTP 403 Forbidden - User missing required role
```

---

### BR-006: Balance Record Unique Identification

**Category**: TRANSFORMATION

**Description**: Generate and maintain unique identifiers for balance records using UUID

**Source**: 
- File: MappedBankAccountBalanceProvider.scala
- Class/Object: MappedBankAccountBalanceProvider
- Method: createOrUpdateBankAccountBalance
- Lines: N/A (to be identified during implementation)

**Business Logic**:
1. When creating a new balance record, generate a unique UUID as the balance ID
2. The balance ID serves as the primary identifier for update and delete operations
3. Balance IDs are immutable once assigned
4. The combination of BANK_ID, ACCOUNT_ID, and BALANCE_ID uniquely identifies a balance record

**Variables**:
- **Input**: 
  - None (auto-generated)
- **Output**: 
  - balance_id: UUID string (e.g., "550e8400-e29b-41d4-a716-446655440000")
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| New record | Only generate for new records | No existing BALANCE_ID |
| UUID format | Must be valid UUID | UUID v4 format |

**Business Impact**: 
Provides a reliable, collision-free identification system for balance records, enabling accurate tracking and management of financial data.

**API Endpoints Using This Rule**:
- POST /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances - UUID generation on create

**Related Test Cases**:
- Test UUID generation uniqueness
- Test UUID format validation
- Test balance ID immutability on update

**Migration Notes for Go**:
- Use github.com/google/uuid package for UUID generation
- Generate UUID v4 for random unique identifiers
- Store as string in database for compatibility
- Validate UUID format on input for update/delete operations

**Example Scenarios**:
```
Scenario 1: New balance record creation
Input: Create balance request (no balance_id provided)
Processing: Generate UUID v4
Output: balance_id = "550e8400-e29b-41d4-a716-446655440000"

Scenario 2: Balance update (ID preserved)
Input: Update balance request with balance_id = "550e8400-e29b-41d4-a716-446655440000"
Processing: Use existing balance_id, do not regenerate
Output: balance_id remains "550e8400-e29b-41d4-a716-446655440000"
```

---

### BR-007: Audit Trail Tracking

**Category**: CALC

**Description**: Automatically track reference date and last change date time for balance records

**Source**: 
- File: BankAccountBalance.scala
- Class/Object: BankAccountBalance
- Method: Various persistence methods
- Lines: N/A (to be identified during implementation)

**Business Logic**:
1. When creating a balance record, set reference date to current date
2. When creating or updating a balance record, set last change date time to current timestamp
3. These timestamps are automatically managed by the system, not user-provided
4. Timestamps support audit and compliance requirements

**Variables**:
- **Input**: 
  - System current date/time
- **Output**: 
  - reference_date: Date when balance was first recorded
  - last_change_date_time: Timestamp of most recent modification
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Create operation | Set both reference date and last change time | Current timestamp |
| Update operation | Update only last change time | Current timestamp |

**Business Impact**: 
Provides audit trail for financial records, supporting regulatory compliance, dispute resolution, and historical analysis of balance changes.

**API Endpoints Using This Rule**:
- POST /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances - Set reference date and last change time
- PUT /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances/{BALANCE_ID} - Update last change time

**Related Test Cases**:
- Test reference date set on creation
- Test last change time updated on modification
- Test timestamp format and timezone handling

**Migration Notes for Go**:
- Use time.Now() for current timestamp
- Consider using UTC for all stored timestamps
- Use time.Time type for Go representation
- Format as ISO 8601 for API responses

**Example Scenarios**:
```
Scenario 1: Balance creation audit trail
Input: Create balance at 2026-01-20 14:30:00 UTC
Processing: Set reference_date = 2026-01-20, last_change_date_time = 2026-01-20T14:30:00Z
Output: Both timestamps recorded

Scenario 2: Balance update audit trail
Input: Update balance at 2026-01-20 16:45:00 UTC (originally created 2026-01-15)
Processing: Keep reference_date = 2026-01-15, update last_change_date_time = 2026-01-20T16:45:00Z
Output: Reference date preserved, last change time updated
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances | POST | Balance creation, currency conversion, RBAC, UUID generation, audit trail | BR-001, BR-004, BR-005, BR-006, BR-007 |
| /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances/{BALANCE_ID} | PUT | Balance update, currency conversion, RBAC, audit trail | BR-002, BR-004, BR-005, BR-007 |
| /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances/{BALANCE_ID} | DELETE | Balance deletion, RBAC | BR-003, BR-005 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestBalanceCreation | Pending | Pending |
| BR-002 | TestBalanceUpdate | Pending | Pending |
| BR-003 | TestBalanceDeletion | Pending | Pending |
| BR-004 | TestCurrencyConversion | Pending | Pending |
| BR-005 | TestRoleBasedAccess | Pending | Pending |
| BR-006 | TestUUIDGeneration | Pending | Pending |
| BR-007 | TestAuditTrail | Pending | Pending |

## Notes and Assumptions

1. **Scope Limitation**: This extraction focuses only on Create, Update, and Delete operations as specified in the capability description. Retrieval operations (GET) are explicitly excluded per the user story.

2. **CreateOrUpdate Pattern**: The implementation uses a createOrUpdate pattern internally where the presence of a balance ID determines whether to create a new record or update an existing one.

3. **Currency Handling**: Balance amounts are assumed to be stored in smallest currency units internally, though the exact conversion factors depend on the account's associated currency.

4. **Authentication Dependency**: All operations assume a valid authentication token is provided. The specific authentication mechanism (OAuth, Bearer token) is handled by upstream services.

5. **Error Handling**: The business rules assume standard HTTP error codes will be used:
   - 400 Bad Request for invalid input
   - 401 Unauthorized for authentication failures
   - 403 Forbidden for authorization failures
   - 404 Not Found for missing resources
   - 201 Created for successful creation
   - 200 OK for successful update
   - 204 No Content for successful deletion

6. **Volume Classification**: The capability is classified as "Medium" volume, indicating moderate usage patterns that should be considered in Go implementation for performance optimization.
