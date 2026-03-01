# Business Rules Extraction

**Extracted From**: Account Listing User Story
**Analysis Date**: 2026-01-20
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 6
- API Endpoints Analyzed: 5
- Rule Categories:
  - Calculations: 0
  - Decisions: 4
  - Thresholds: 0
  - Aggregations: 1
  - Workflows: 0
  - Transformations: 1

## Business Rules Catalog

### BR-001: Bank-Scoped Account Retrieval

**Category**: DECISION

**Description**: Account listing operations must be scoped to a specific bank - users must specify which bank's accounts to retrieve when accessing bank-specific endpoints.

**Source**: 
- File: User Story - Account Listing
- Class/Object: APIMethods400
- Method: getAccountsAtBank, getPrivateAccountIdsbyBankId, getAccountsHeld, getMyAccountsAtBank
- Lines: N/A (User Story Reference)

**Business Logic**:
1. When a user requests account listing, they must provide a valid BANK_ID parameter
2. The system retrieves only accounts associated with the specified bank
3. If the bank does not exist, the system returns HTTP 404 (BankNotFound) error
4. Each account in the response includes the bank_id field confirming the bank association

**Variables**:
- **Input**: BANK_ID (path parameter) - The unique identifier of the bank
- **Output**: List of accounts belonging to the specified bank
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| BANK_ID exists | Bank is valid and operational | Valid bank identifier |
| BANK_ID not found | Bank does not exist in system | HTTP 404 response |

**Business Impact**: 
Ensures users can access accounts at specific financial institutions, enabling multi-bank scenarios where users may have relationships with multiple banks. This is fundamental for Open Banking platforms that aggregate accounts across institutions.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts - Retrieve accounts at specific bank
- GET /obp/v4.0.0/banks/BANK_ID/accounts/account-ids/private - Retrieve account IDs at specific bank
- GET /obp/v4.0.0/banks/BANK_ID/accounts-held - Retrieve accounts held at specific bank
- GET /obp/v4.0.0/my/banks/BANK_ID/accounts - Retrieve user's accounts at specific bank

**Related Test Cases**:
- Test bank existence validation
- Test account retrieval with valid BANK_ID
- Test error response with invalid BANK_ID

**Migration Notes for Go**:
- Implement BANK_ID path parameter extraction using Go's HTTP router (e.g., gorilla/mux or chi)
- Create a bank validation middleware or service function
- Return appropriate HTTP status codes (404 for not found)

**Example Scenarios**:
```
Scenario 1: Valid bank request
Input: BANK_ID = "bank-id-001"
Processing: Validate bank exists, retrieve associated accounts
Output: List of accounts at bank-id-001

Scenario 2: Invalid bank request
Input: BANK_ID = "non-existent-bank"
Processing: Bank validation fails
Output: HTTP 404 BankNotFound error
```

---

### BR-002: View-Based Access Control for Account Visibility

**Category**: DECISION

**Description**: Users can only view accounts they have been granted explicit access to through the view/permission system. Account visibility is determined by the views and permissions granted to the authenticated user.

**Source**: 
- File: User Story - Account Listing
- Class/Object: Views, NewStyle.function
- Method: getBankAccountsForUser, getAccountsHeld
- Lines: N/A (User Story Reference)

**Business Logic**:
1. When a user requests account listing, the system identifies the authenticated user
2. The system queries the view/permission system to determine which accounts the user has access to
3. Only accounts where the user has at least one granted view are included in the response
4. Each account response includes the list of views available to the user for that account
5. If the user has no accessible accounts, an empty list is returned (not an error)

**Variables**:
- **Input**: Authenticated user identity (from OAuth/DirectLogin token)
- **Output**: List of accounts with views_available field populated
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| User has view on account | User can see the account | At least one view granted |
| User has no views | Account is hidden from user | Zero views = not visible |
| views_available populated | Shows what user can do | List of ViewBasic objects |

**Business Impact**: 
Enforces data privacy and access control in multi-user banking environments. Ensures users only see accounts they are authorized to access, supporting scenarios like shared business accounts, accountant access, and family account sharing.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts - Returns only accessible accounts
- GET /obp/v4.0.0/banks/BANK_ID/accounts/account-ids/private - Returns only private accessible account IDs
- GET /obp/v4.0.0/banks/BANK_ID/accounts-held - Returns accounts user has been granted access to
- GET /obp/v4.0.0/my/banks/BANK_ID/accounts - Returns user's own accounts
- GET /obp/v4.0.0/accounts - Returns all accessible accounts across banks

**Related Test Cases**:
- Test account visibility with granted view
- Test account hidden without view
- Test views_available field population
- Test empty list when no accessible accounts

**Migration Notes for Go**:
- Implement view/permission checking service
- Create user context extraction from authentication token
- Filter account results based on view permissions
- Populate views_available in response struct

**Example Scenarios**:
```
Scenario 1: User with multiple views
Input: User "user-001" with "owner" and "accountant" views on account-001
Processing: Check views, include account with both views listed
Output: Account-001 with views_available: ["owner", "accountant"]

Scenario 2: User with no access
Input: User "user-002" with no views on any accounts at bank
Processing: Check views, find no accessible accounts
Output: Empty accounts list []
```

---

### BR-003: Detail Level Selection for Account Information

**Category**: TRANSFORMATION

**Description**: The system supports various detail levels for account information retrieval, allowing applications to request minimal, basic, or detailed account information based on their needs.

**Source**: 
- File: User Story - Account Listing
- Class/Object: JSONFactory400
- Method: createAccountsJson, createAccountIdsJson, createAccountsHeldJson, createMyAccountsJson
- Lines: N/A (User Story Reference)

**Business Logic**:
1. Different endpoints provide different levels of account detail
2. Minimal detail: Only account IDs (for lightweight operations)
3. Basic detail: Account ID, bank_id, label, type, and views_available
4. Detailed: Includes balance, account_routings, owners, and account number
5. Full detail: Includes all above plus account_attributes (overdraft limit, interest rate)
6. Balance and account number visibility may be further restricted by view permissions

**Variables**:
- **Input**: Endpoint selection or detail level query parameter
- **Output**: Account data with fields appropriate to the detail level
- **Constants**: Detail levels: minimal, basic, detailed, full

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Minimal detail | Lightweight account listing | Only id field |
| Basic detail | Standard account info | id, bank_id, label, type, views |
| Detailed | Full account info | + balance, routings, owners, number |
| Full detail | Complete account data | + account_attributes |

**Business Impact**: 
Optimizes API performance and payload sizes by allowing applications to request only the data they need. Supports efficient account selection workflows (minimal), display purposes (basic), and detailed account management (full).

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/account-ids/private - Minimal detail (IDs only)
- GET /obp/v4.0.0/banks/BANK_ID/accounts-held - Basic detail
- GET /obp/v4.0.0/banks/BANK_ID/accounts - Detailed
- GET /obp/v4.0.0/my/banks/BANK_ID/accounts - Full detail with attributes

**Related Test Cases**:
- Test minimal response contains only IDs
- Test basic response contains expected fields
- Test detailed response includes balance and routings
- Test full response includes account_attributes

**Migration Notes for Go**:
- Create separate response structs for each detail level
- Implement field selection based on endpoint or query parameter
- Consider using struct tags for JSON serialization control
- Implement view-based field masking (e.g., account number masking)

**Example Scenarios**:
```
Scenario 1: Minimal detail request
Input: GET /banks/BANK_ID/accounts/account-ids/private
Processing: Retrieve accounts, return only IDs
Output: {"accounts": [{"id": "account-001"}, {"id": "account-002"}]}

Scenario 2: Full detail request
Input: GET /my/banks/BANK_ID/accounts
Processing: Retrieve accounts with all attributes
Output: Full account objects with balance, routings, owners, attributes
```

---

### BR-004: Empty List Response for No Accessible Accounts

**Category**: DECISION

**Description**: When a user has no accessible accounts at a specified bank, the system returns an empty list rather than an error response.

**Source**: 
- File: User Story - Account Listing
- Class/Object: APIMethods400
- Method: All account listing methods
- Lines: N/A (User Story Reference)

**Business Logic**:
1. After applying access control filters, if no accounts remain, return empty list
2. Empty list is a valid successful response (HTTP 200)
3. This is distinct from bank not found (HTTP 404) or unauthorized (HTTP 401)
4. Applications should handle empty lists gracefully in their UI

**Variables**:
- **Input**: User identity, BANK_ID
- **Output**: Empty accounts array: {"accounts": []}
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| No accessible accounts | User has no permissions at bank | Empty list, HTTP 200 |
| Bank not found | Bank doesn't exist | HTTP 404 error |
| Unauthorized | Invalid authentication | HTTP 401 error |

**Business Impact**: 
Provides clear distinction between "no accounts available" and error conditions. Enables applications to display appropriate messages to users (e.g., "You have no accounts at this bank" vs "Bank not found").

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts - Returns empty list if no access
- GET /obp/v4.0.0/banks/BANK_ID/accounts/account-ids/private - Returns empty list if no access
- GET /obp/v4.0.0/banks/BANK_ID/accounts-held - Returns empty list if no access
- GET /obp/v4.0.0/my/banks/BANK_ID/accounts - Returns empty list if no accounts
- GET /obp/v4.0.0/accounts - Returns empty list if no accounts anywhere

**Related Test Cases**:
- Test empty list response for user with no access
- Test HTTP 200 status with empty list
- Test distinction from HTTP 404 bank not found

**Migration Notes for Go**:
- Ensure empty slice is serialized as [] not null
- Return HTTP 200 with empty accounts array
- Implement proper error handling for distinct error cases

**Example Scenarios**:
```
Scenario 1: No accessible accounts
Input: User with no views at bank-001
Processing: Query accounts, apply access filter, no results
Output: HTTP 200, {"accounts": []}

Scenario 2: Bank not found (contrast)
Input: Invalid BANK_ID
Processing: Bank validation fails
Output: HTTP 404, BankNotFound error
```

---

### BR-005: Cross-Bank Account Aggregation

**Category**: AGGREGATION

**Description**: The system supports retrieval of accounts across all banks that the authenticated user has access to, enabling account aggregation use cases.

**Source**: 
- File: User Story - Account Listing
- Class/Object: APIMethods400, NewStyle.function
- Method: getAccounts, getAllAccountsForUser
- Lines: N/A (User Story Reference)

**Business Logic**:
1. User requests accounts without specifying a bank
2. System queries all banks where the user has account access
3. Accounts from all accessible banks are aggregated into a single response
4. Each account includes its bank_id to identify the source bank
5. Response provides a unified view across multiple financial institutions

**Variables**:
- **Input**: Authenticated user identity
- **Output**: Aggregated list of accounts from all banks with bank_id field
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Multiple banks | User has accounts at multiple institutions | Accounts aggregated |
| Single bank | User has accounts at one bank only | Single bank's accounts |
| No banks | User has no accounts anywhere | Empty list |

**Business Impact**: 
Enables Open Banking account aggregation scenarios where users can view all their accounts across multiple financial institutions in a single view. Critical for personal finance management applications and multi-bank dashboards.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/accounts - Retrieve all accounts across all banks

**Related Test Cases**:
- Test aggregation from multiple banks
- Test bank_id field correctly identifies source bank
- Test empty response when no accounts at any bank

**Migration Notes for Go**:
- Implement parallel queries to multiple banks for performance
- Aggregate results from multiple bank queries
- Ensure bank_id is populated in each account response
- Consider pagination for users with many accounts across banks

**Example Scenarios**:
```
Scenario 1: Multi-bank aggregation
Input: User with accounts at bank-001 and bank-002
Processing: Query both banks, aggregate results
Output: [
  {"id": "acc-001", "bank_id": "bank-001", "label": "Checking at Bank A"},
  {"id": "acc-002", "bank_id": "bank-002", "label": "Savings at Bank B"}
]

Scenario 2: Single bank user
Input: User with accounts only at bank-001
Processing: Query all banks, only bank-001 has accounts
Output: [{"id": "acc-001", "bank_id": "bank-001", "label": "Checking"}]
```

---

### BR-006: Account Number Masking Based on View Permissions

**Category**: DECISION

**Description**: Account numbers may be masked (partially hidden) based on the user's view permissions, protecting sensitive account information while still providing identification.

**Source**: 
- File: User Story - Account Listing
- Class/Object: JSONFactory400
- Method: createAccountsJson, createMyAccountsJson
- Lines: N/A (User Story Reference)

**Business Logic**:
1. When returning account information, check user's view permissions
2. If user has full account number permission, return complete number (e.g., "1234567890")
3. If user has limited permission, return masked number (e.g., "****1234")
4. Masking typically shows only last 4 digits
5. Balance information visibility is also controlled by view permissions

**Variables**:
- **Input**: User's view permissions, account number
- **Output**: Full or masked account number
- **Constants**: Masking pattern: "****" + last 4 digits

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Full number permission | User can see complete account number | Full number displayed |
| Limited permission | User has restricted view | Masked: ****1234 |
| Balance permission | User can see account balance | Balance included |
| No balance permission | Balance hidden from user | Balance field omitted |

**Business Impact**: 
Protects sensitive financial information while enabling account identification. Supports scenarios where users need to identify accounts without exposing full account numbers, such as shared account views or accountant access.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts - May return masked numbers
- GET /obp/v4.0.0/my/banks/BANK_ID/accounts - Returns full numbers for owner

**Related Test Cases**:
- Test masked account number for limited view
- Test full account number for owner view
- Test balance visibility based on permissions

**Migration Notes for Go**:
- Implement account number masking utility function
- Check view permissions before populating sensitive fields
- Create conditional field population based on permissions
- Consider using pointer types for optional fields (balance, number)

**Example Scenarios**:
```
Scenario 1: Owner view (full access)
Input: User with "owner" view on account
Processing: Check permissions, full access granted
Output: {"number": "1234567890", "balance": {"amount": "1500.00"}}

Scenario 2: Accountant view (limited access)
Input: User with "accountant" view on account
Processing: Check permissions, limited access
Output: {"number": "****7890", "balance": {"amount": "1500.00"}}
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /obp/v4.0.0/banks/BANK_ID/accounts | GET | Bank scope, access control, detail level, empty list, masking | BR-001, BR-002, BR-003, BR-004, BR-006 |
| /obp/v4.0.0/banks/BANK_ID/accounts/account-ids/private | GET | Bank scope, access control, minimal detail, empty list | BR-001, BR-002, BR-003, BR-004 |
| /obp/v4.0.0/banks/BANK_ID/accounts-held | GET | Bank scope, access control, basic detail, empty list | BR-001, BR-002, BR-003, BR-004 |
| /obp/v4.0.0/my/banks/BANK_ID/accounts | GET | Bank scope, access control, full detail, empty list, masking | BR-001, BR-002, BR-003, BR-004, BR-006 |
| /obp/v4.0.0/accounts | GET | Access control, aggregation, empty list | BR-002, BR-004, BR-005 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestBankScopeValidation | Pending | Pending |
| BR-002 | TestViewBasedAccessControl | Pending | Pending |
| BR-003 | TestDetailLevelSelection | Pending | Pending |
| BR-004 | TestEmptyListResponse | Pending | Pending |
| BR-005 | TestCrossBankAggregation | Pending | Pending |
| BR-006 | TestAccountNumberMasking | Pending | Pending |

## Notes and Assumptions

1. **Authentication Assumption**: All endpoints require valid OAuth or DirectLogin authentication. The user identity is extracted from the authentication token.

2. **View System**: The business rules assume a view/permission system exists that grants users access to specific accounts with specific capabilities (view balance, view number, etc.).

3. **Real-time Performance**: The user story specifies real-time frequency and high volume. The Go implementation should be optimized for performance with appropriate caching strategies.

4. **Pagination**: While not explicitly a business rule, the implementation notes suggest pagination should be considered for users with many accounts.

5. **Balance Freshness**: For accounts where balance is retrieved from external systems, consider including a timestamp indicating balance freshness.

6. **Error Handling**: Standard HTTP error codes should be used:
   - 200: Success (including empty list)
   - 401: Unauthorized (invalid/missing authentication)
   - 403: Forbidden (no permission to access any accounts)
   - 404: Bank not found

7. **Data Source**: Business rules are extracted solely from the Account Listing user story. No additional Scala source code was analyzed for this extraction.
