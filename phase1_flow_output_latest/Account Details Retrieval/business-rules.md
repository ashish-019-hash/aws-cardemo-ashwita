# Business Rules Extraction

**Extracted From**: Account Details Retrieval Capability (Scala Application - Open Bank Project)
**Analysis Date**: 2026-01-20
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 7
- API Endpoints Analyzed: 3
- Rule Categories:
  - Calculations: 0
  - Decisions: 3
  - Thresholds: 0
  - Aggregations: 0
  - Workflows: 1
  - Transformations: 3

## Business Rules Catalog

### BR-001: View-Based Account Data Moderation

**Category**: DECISION

**Description**: The system determines what account information to expose based on the view through which the account is being accessed. Different views have different permissions that control visibility of account fields.

**Source**: 
- File: code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: getAccountById, getModeratedAccountById
- Lines: Account retrieval endpoint implementations

**Business Logic**:
1. When a user requests account details, the system identifies the view (VIEW_ID) being used
2. The system checks the view's permissions to determine which fields are visible
3. Account number may be masked (e.g., "****7890") if the view restricts full number visibility
4. Balance information is only included if the view permits balance access
5. Owner information is only included if the view permits owner visibility
6. Account routing information visibility is controlled by view permissions

**Variables**:
- **Input**: VIEW_ID (view identifier), user permissions, account data
- **Output**: Moderated account details with fields filtered based on view permissions
- **Constants**: View permission flags (can_see_balance, can_see_owner, can_see_account_number, etc.)

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| view.canSeeBalance == true | User can view account balance | Boolean flag |
| view.canSeeOwner == true | User can view account owner details | Boolean flag |
| view.canSeeAccountNumber == true | User can view full account number | Boolean flag |

**Business Impact**: 
This rule ensures data privacy and regulatory compliance by controlling what financial information is exposed to different users based on their access level. It enables multi-tenant access where different stakeholders (account owners, accountants, auditors) see appropriate levels of detail.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/account - Account retrieval with view moderation
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/account - Moderated account view

**Related Test Cases**:
- Test cases validating different view permissions return appropriate data subsets
- Test cases verifying masked vs unmasked account numbers based on view

**Migration Notes for Go**:
- Implement view permission checking as middleware or service layer logic
- Use Go interfaces to define view permission contracts
- Consider using struct tags for field-level visibility control
- Implement data masking utilities for sensitive fields

**Example Scenarios**:
```
Scenario 1: Owner view access
Input: VIEW_ID = "owner", user has owner permissions
Processing: All view permissions are true
Output: Full account details including balance, owner, full account number

Scenario 2: Limited view access
Input: VIEW_ID = "accountant", user has accountant permissions
Processing: canSeeBalance = true, canSeeOwner = false, canSeeAccountNumber = false
Output: Account details with balance, masked account number (****7890), no owner info
```

---

### BR-002: Account Access Authorization

**Category**: DECISION

**Description**: The system verifies that the requesting user has been granted permission to access the specified account before returning any account details.

**Source**: 
- File: code/api/util/NewStyle.scala
- Class/Object: NewStyle.function
- Method: getBankAccount, checkViewAccessAndReturnView
- Lines: Authorization check implementations

**Business Logic**:
1. User authentication is verified via OAuth token or DirectLogin credentials
2. System checks if the user has at least one view/permission granted on the requested account
3. If the user has no permissions on the account, access is denied with HTTP 403
4. If the user has permissions, the system proceeds with data retrieval using the appropriate view

**Variables**:
- **Input**: User authentication token, BANK_ID, ACCOUNT_ID, VIEW_ID
- **Output**: Authorization decision (allow/deny)
- **Constants**: HTTP status codes (403 Forbidden, 401 Unauthorized)

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| user.hasViewAccess(account, view) | User has permission to access account via specified view | Boolean check |
| user.isAuthenticated | User has valid authentication credentials | Valid token required |

**Business Impact**: 
This rule enforces the principle of least privilege, ensuring users can only access accounts they have been explicitly granted permission to view. This is critical for regulatory compliance (PSD2, GDPR) and preventing unauthorized data access.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/account
- GET /obp/v4.0.0/my/banks/BANK_ID/accounts/ACCOUNT_ID/account
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/account

**Related Test Cases**:
- Test cases verifying HTTP 403 when user lacks account permissions
- Test cases verifying HTTP 401 for unauthenticated requests
- Test cases verifying successful access with valid permissions

**Migration Notes for Go**:
- Implement authorization as middleware using Go's http.Handler pattern
- Use context.Context to pass user authentication information
- Consider using a dedicated authorization service or library
- Implement proper error responses with appropriate HTTP status codes

**Example Scenarios**:
```
Scenario 1: Authorized access
Input: User has "owner" view permission on account-001
Processing: checkViewAccessAndReturnView returns true
Output: Access granted, proceed to retrieve account details

Scenario 2: Unauthorized access
Input: User has no permissions on account-002
Processing: checkViewAccessAndReturnView returns false
Output: HTTP 403 Forbidden with UserNoPermissionAccessView error
```

---

### BR-003: Account Existence Validation

**Category**: DECISION

**Description**: The system validates that the requested bank and account exist in the system before attempting to retrieve account details.

**Source**: 
- File: code/bankconnectors/Connector.scala
- Class/Object: Connector
- Method: getBankAccount
- Lines: Account lookup implementations

**Business Logic**:
1. System validates that the BANK_ID corresponds to an existing bank in the system
2. If bank does not exist, return HTTP 404 with BankNotFound error
3. System validates that the ACCOUNT_ID exists within the specified bank
4. If account does not exist, return HTTP 404 with AccountNotFound error
5. Only if both validations pass, proceed with account data retrieval

**Variables**:
- **Input**: BANK_ID, ACCOUNT_ID
- **Output**: Validation result (exists/not found)
- **Constants**: Error codes (BankNotFound, AccountNotFound)

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| bank.exists(BANK_ID) | Bank is registered in the system | Valid bank identifier |
| account.exists(ACCOUNT_ID, BANK_ID) | Account exists within the bank | Valid account identifier |

**Business Impact**: 
This rule ensures data integrity and provides clear error feedback to API consumers. It prevents unnecessary processing and provides meaningful error messages that help developers debug integration issues.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/account
- GET /obp/v4.0.0/my/banks/BANK_ID/accounts/ACCOUNT_ID/account
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/account

**Related Test Cases**:
- Test cases verifying HTTP 404 for non-existent bank
- Test cases verifying HTTP 404 for non-existent account
- Test cases verifying successful retrieval for existing bank and account

**Migration Notes for Go**:
- Implement existence checks as early validation in request handlers
- Use custom error types for BankNotFound and AccountNotFound
- Return appropriate HTTP 404 responses with descriptive error messages
- Consider caching bank existence checks for performance

**Example Scenarios**:
```
Scenario 1: Valid bank and account
Input: BANK_ID = "bank-001", ACCOUNT_ID = "account-001"
Processing: Both bank and account exist
Output: Proceed to authorization and data retrieval

Scenario 2: Non-existent bank
Input: BANK_ID = "invalid-bank", ACCOUNT_ID = "account-001"
Processing: Bank lookup fails
Output: HTTP 404 Not Found with BankNotFound error

Scenario 3: Non-existent account
Input: BANK_ID = "bank-001", ACCOUNT_ID = "invalid-account"
Processing: Bank exists, account lookup fails
Output: HTTP 404 Not Found with AccountNotFound error
```

---

### BR-004: Account Details Composition

**Category**: TRANSFORMATION

**Description**: The system composes comprehensive account details by aggregating data from multiple sources including account master data, balance information, routing information, and custom attributes.

**Source**: 
- File: code/api/v4_0_0/JSONFactory4.0.0.scala
- Class/Object: JSONFactory400
- Method: createAccountJSON, createModeratedAccountJSON
- Lines: JSON response creation methods

**Business Logic**:
1. Retrieve core account data (id, bank_id, label, number, type)
2. Fetch current balance information with currency
3. Retrieve account owner information (user_id, provider, display_name)
4. Gather account routing information (IBAN, account number schemes)
5. Collect available views for the account
6. Retrieve custom account attributes (metadata key-value pairs)
7. Compose all data into a unified account response structure

**Variables**:
- **Input**: BankAccountCommons (account data), balance data, routing data, attributes
- **Output**: AccountJson400 or ModeratedAccountJSON400 response object
- **Constants**: Response field names, currency codes

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| includeBalance == true | Balance data should be included | Based on view permissions |
| includeOwners == true | Owner data should be included | Based on view permissions |
| includeAttributes == true | Custom attributes should be included | Based on view permissions |

**Business Impact**: 
This rule ensures that API consumers receive a complete, well-structured view of account information in a single request. It reduces the need for multiple API calls and provides a consistent data format for downstream applications.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/account
- GET /obp/v4.0.0/my/banks/BANK_ID/accounts/ACCOUNT_ID/account
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/account

**Related Test Cases**:
- Test cases verifying all expected fields are present in response
- Test cases verifying correct data types for each field
- Test cases verifying balance includes currency and amount

**Migration Notes for Go**:
- Define Go structs for AccountJson400 and ModeratedAccountJSON400
- Use JSON struct tags for proper serialization
- Implement composition using Go's struct embedding or explicit field mapping
- Consider using builder pattern for complex response construction

**Example Scenarios**:
```
Scenario 1: Full account details composition
Input: Account with balance, 2 owners, 2 routing schemes, 3 attributes
Processing: Aggregate all data sources
Output: Complete JSON response with all fields populated

Scenario 2: Minimal account details
Input: Account with balance only, no attributes
Processing: Aggregate available data, omit empty collections
Output: JSON response with core fields and balance, empty arrays for missing data
```

---

### BR-005: Balance Information Retrieval

**Category**: TRANSFORMATION

**Description**: The system retrieves and formats the current account balance including the currency code and properly formatted amount value.

**Source**: 
- File: code/bankconnectors/Connector.scala
- Class/Object: Connector
- Method: getBankAccount (balance retrieval)
- Lines: Balance data access implementations

**Business Logic**:
1. Retrieve the current balance value for the account
2. Identify the account's currency (e.g., USD, EUR, GBP)
3. Format the balance amount as a string with proper decimal precision
4. Combine currency and amount into AmountOfMoney structure
5. Return balance data as part of account details response

**Variables**:
- **Input**: ACCOUNT_ID, BANK_ID
- **Output**: AmountOfMoney object with currency and amount
- **Constants**: Currency codes (ISO 4217), decimal precision rules

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| balance.currency != null | Account has a defined currency | Valid ISO 4217 code |
| balance.amount is formatted | Amount is properly formatted | String with decimal precision |

**Business Impact**: 
Balance information is critical for financial planning, transaction verification, and reporting. Proper formatting ensures consistency across different currencies and prevents rounding errors in financial calculations.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/account
- GET /obp/v4.0.0/my/banks/BANK_ID/accounts/ACCOUNT_ID/account
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/account

**Related Test Cases**:
- Test cases verifying balance includes currency code
- Test cases verifying balance amount format
- Test cases verifying balance retrieval for different currencies

**Migration Notes for Go**:
- Use decimal library (e.g., shopspring/decimal) for precise monetary calculations
- Define AmountOfMoney struct with Currency and Amount fields
- Implement proper JSON marshaling for monetary values
- Consider using custom types for currency codes

**Example Scenarios**:
```
Scenario 1: USD balance
Input: Account balance = 1500.00, currency = USD
Processing: Format balance with 2 decimal places
Output: {"currency": "USD", "amount": "1500.00"}

Scenario 2: EUR balance with cents
Input: Account balance = 2345.67, currency = EUR
Processing: Format balance with 2 decimal places
Output: {"currency": "EUR", "amount": "2345.67"}
```

---

### BR-006: Account Routing Information Assembly

**Category**: TRANSFORMATION

**Description**: The system assembles account routing information including various routing schemes (IBAN, account number, sort code) based on the account's configuration and user permissions.

**Source**: 
- File: code/api/v4_0_0/JSONFactory4.0.0.scala
- Class/Object: JSONFactory400
- Method: createAccountRoutingJSON
- Lines: Routing information assembly

**Business Logic**:
1. Retrieve all routing schemes configured for the account
2. For each routing scheme, extract the scheme type and address value
3. Filter routing information based on view permissions
4. Format routing data as a list of scheme-address pairs
5. Include routing information in the account response

**Variables**:
- **Input**: Account routing data, view permissions
- **Output**: List of AccountRouting objects (scheme, address)
- **Constants**: Routing scheme types (IBAN, AccountNumber, SortCode, BIC, etc.)

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| routing.scheme in supported_schemes | Routing scheme is recognized | IBAN, AccountNumber, etc. |
| view.canSeeRouting == true | User can view routing information | Boolean flag |

**Business Impact**: 
Account routing information is essential for payment initiation and interbank transfers. Providing multiple routing schemes enables compatibility with different payment networks and international transfers.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/account
- GET /obp/v4.0.0/my/banks/BANK_ID/accounts/ACCOUNT_ID/account
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/account

**Related Test Cases**:
- Test cases verifying IBAN is included when available
- Test cases verifying multiple routing schemes are returned
- Test cases verifying routing visibility based on view permissions

**Migration Notes for Go**:
- Define AccountRouting struct with Scheme and Address fields
- Implement routing scheme validation
- Use slice of AccountRouting for multiple schemes
- Consider using constants or enums for routing scheme types

**Example Scenarios**:
```
Scenario 1: Account with multiple routing schemes
Input: Account has IBAN and AccountNumber configured
Processing: Assemble both routing schemes
Output: [{"scheme": "IBAN", "address": "US12345678901234567890"}, {"scheme": "AccountNumber", "address": "1234567890"}]

Scenario 2: Account with single routing scheme
Input: Account has only AccountNumber configured
Processing: Assemble single routing scheme
Output: [{"scheme": "AccountNumber", "address": "1234567890"}]
```

---

### BR-007: Account Details Retrieval Workflow

**Category**: WORKFLOW

**Description**: The complete workflow for retrieving account details orchestrates validation, authorization, data retrieval, and response composition in a specific sequence.

**Source**: 
- File: code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: getAccountById, getCoreAccountById, getModeratedAccountById
- Lines: Endpoint implementation methods

**Business Logic**:
1. Receive account details request with BANK_ID, ACCOUNT_ID, VIEW_ID
2. Validate user authentication (OAuth/DirectLogin)
3. Validate bank existence (return 404 if not found)
4. Validate account existence within bank (return 404 if not found)
5. Validate view existence and user access (return 403 if unauthorized)
6. Retrieve account data from backend connector
7. Apply view-based data moderation
8. Compose account details response
9. Return JSON response with appropriate HTTP status

**Variables**:
- **Input**: HTTP request with path parameters and authentication headers
- **Output**: JSON response with account details or error response
- **Constants**: HTTP status codes (200, 401, 403, 404)

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| All validations pass | Request is valid and authorized | Sequential checks |
| Any validation fails | Request cannot be processed | Return appropriate error |

**Business Impact**: 
This workflow ensures consistent, secure, and reliable account data retrieval. The ordered sequence of validations prevents unnecessary processing and provides clear error feedback at each stage.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/account
- GET /obp/v4.0.0/my/banks/BANK_ID/accounts/ACCOUNT_ID/account
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/account

**Related Test Cases**:
- Integration tests covering the complete workflow
- Test cases for each validation step
- Test cases for successful end-to-end retrieval

**Migration Notes for Go**:
- Implement workflow as a handler function with clear step separation
- Use early returns for validation failures
- Consider using middleware for authentication and common validations
- Implement proper error handling with custom error types
- Use Go's context for request-scoped data (user info, request ID)

**Example Scenarios**:
```
Scenario 1: Successful account retrieval
Input: Valid BANK_ID, ACCOUNT_ID, VIEW_ID, authenticated user with permissions
Processing: All validations pass, data retrieved and composed
Output: HTTP 200 with complete account details JSON

Scenario 2: Authentication failure
Input: Missing or invalid authentication token
Processing: Authentication validation fails at step 2
Output: HTTP 401 Unauthorized

Scenario 3: Authorization failure
Input: Valid authentication but no view access
Processing: View access validation fails at step 5
Output: HTTP 403 Forbidden with UserNoPermissionAccessView error
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/account | GET | View moderation, authorization, existence validation, details composition, balance retrieval, routing assembly, workflow | BR-001, BR-002, BR-003, BR-004, BR-005, BR-006, BR-007 |
| /obp/v4.0.0/my/banks/BANK_ID/accounts/ACCOUNT_ID/account | GET | Authorization, existence validation, details composition, balance retrieval, routing assembly, workflow | BR-002, BR-003, BR-004, BR-005, BR-006, BR-007 |
| /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/account | GET | View moderation, authorization, existence validation, details composition, balance retrieval, routing assembly, workflow | BR-001, BR-002, BR-003, BR-004, BR-005, BR-006, BR-007 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestViewBasedDataModeration | Pending | Pending |
| BR-002 | TestAccountAccessAuthorization | Pending | Pending |
| BR-003 | TestAccountExistenceValidation | Pending | Pending |
| BR-004 | TestAccountDetailsComposition | Pending | Pending |
| BR-005 | TestBalanceInformationRetrieval | Pending | Pending |
| BR-006 | TestAccountRoutingAssembly | Pending | Pending |
| BR-007 | TestAccountDetailsRetrievalWorkflow | Pending | Pending |

## Notes and Assumptions

1. **View Permission Model**: The business rules assume a view-based permission model where different views grant different levels of access to account data. The exact permission flags and their mappings should be verified with the existing Scala implementation.

2. **Real-time Balance**: The capability specifies real-time frequency, so balance information is assumed to be fetched fresh for each request rather than cached. Consider implementing caching with appropriate invalidation for performance optimization in Go.

3. **High Volume Support**: Given the high volume characteristic, the Go implementation should be designed with concurrency and performance in mind, leveraging Go's goroutines and efficient HTTP handling.

4. **Error Response Format**: The specific error response format (error codes, messages) should match the existing Scala API to ensure backward compatibility for existing API consumers.

5. **Account Attributes**: Custom account attributes are extensible key-value pairs. The Go implementation should support dynamic attributes without requiring code changes for new attribute types.

6. **Multi-Currency Support**: The system supports accounts in different currencies. The Go implementation should handle currency-specific formatting and validation appropriately.
