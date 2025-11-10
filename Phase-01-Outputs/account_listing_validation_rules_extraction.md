# Validation Rules Extraction: Account Listing

## Executive Summary

This document extracts and documents all validation rules for the Account Listing functionality in the OBP API system. The Account Listing feature allows banking application users to retrieve lists of bank accounts they have access to, either at a specific bank or across all banks. This analysis follows a multi-stakeholder approach to ensure comprehensive coverage of all validation requirements.

---

## User Story Context

**Feature**: Account Listing  
**User Story**: As a banking application user or API consumer, I want to retrieve a list of all bank accounts I have access to, so that I can view my accounts and select which one to perform operations on.

**API Endpoints**:
1. GET /obp/v5.1.0/users/{USER_ID}/banks/{BANK_ID}/accounts-held
2. GET /obp/v5.1.0/users/{USER_ID}/accounts-held

---

## Validation Rules by Category

### 1. Authentication and Authorization Validations

#### Rule 1.1: User Authentication Check
**What it checks**: Verifies that the requesting user is properly authenticated before retrieving account lists.

**Why it exists**: To ensure only legitimate users can access account information and prevent unauthorized access to sensitive banking data.

**When it applies**: At the beginning of every account listing request, before any account data is retrieved.

**Who it affects**: All users (customers, administrators, API consumers) attempting to retrieve account lists.

**What happens when it fails**: The system returns an authentication error (typically HTTP 401 Unauthorized) and no account data is returned.

**Where it is enforced**: At the API gateway level before the request reaches the account listing service.

**Example**:
- Valid: User provides valid authentication token → Request proceeds
- Invalid: User provides expired or missing token → Request rejected with "Authentication required" error

---

#### Rule 1.2: User ID Validation
**What it checks**: Confirms that the USER_ID parameter in the request corresponds to a valid, existing user in the system.

**Why it exists**: To prevent requests for non-existent users and ensure data integrity in the system.

**When it applies**: After authentication, when processing the USER_ID parameter from the URL path.

**Who it affects**: API consumers, banking applications, and any service making account listing requests.

**What happens when it fails**: Returns an error message indicating "Invalid user ID" or "User not found" (typically HTTP 404 Not Found).

**Where it is enforced**: In the account listing service during parameter validation phase.

**Example**:
- Valid: USER_ID "user123" exists in system → Validation passes
- Invalid: USER_ID "nonexistent999" does not exist → Error returned

---

#### Rule 1.3: Bank ID Validation
**What it checks**: Verifies that the BANK_ID parameter (when provided) corresponds to a valid, active bank in the system.

**Why it exists**: To ensure requests are made for legitimate banks and prevent errors when retrieving bank-specific account data.

**When it applies**: When using the bank-specific endpoint (accounts-held at one bank), during parameter validation.

**Who it affects**: Users requesting accounts at a specific bank, banking applications filtering by bank.

**What happens when it fails**: Returns "Invalid bank ID" error message (typically HTTP 404 Not Found or 400 Bad Request).

**Where it is enforced**: In the account listing service during parameter validation, before querying account data.

**Example**:
- Valid: BANK_ID "bank-001" exists and is active → Validation passes
- Invalid: BANK_ID "fake-bank" does not exist → Error returned

---

#### Rule 1.4: Entitlement Verification
**What it checks**: Confirms the user has the required entitlements (CanGetAccountsHeldAtOneBank OR CanGetAccountsHeldAtAnyBank) for the requested operation scope.

**Why it exists**: To enforce role-based access control and ensure users only access account lists they are authorized to view.

**When it applies**: After user authentication and before retrieving account data, based on which endpoint is called.

**Who it affects**: All users, with different entitlements for different user roles (customers, administrators, auditors).

**What happens when it fails**: Returns authorization error (HTTP 403 Forbidden) with message indicating insufficient permissions.

**Where it is enforced**: In the authorization layer after authentication but before data retrieval.

**Example**:
- Valid: User with CanGetAccountsHeldAtAnyBank requests all accounts → Access granted
- Invalid: User with only CanGetAccountsHeldAtOneBank requests all accounts across banks → Access denied

---

### 2. Input Parameter Validations

#### Rule 2.1: Account Type Filter Format Validation
**What it checks**: Validates that the account_type_filter parameter contains valid account type values when provided.

**Why it exists**: To ensure filtering logic works correctly and prevent errors from invalid account type specifications.

**When it applies**: When the optional account_type_filter query parameter is included in the request.

**Who it affects**: API consumers and applications that want to filter accounts by type (checking, savings, loan, etc.).

**What happens when it fails**: Returns validation error indicating invalid account type values with list of acceptable types.

**Where it is enforced**: In the query parameter validation phase before applying filters.

**Example**:
- Valid: account_type_filter="CHECKING,SAVINGS" → Validation passes
- Invalid: account_type_filter="INVALID_TYPE" → Error returned with valid types list

---

#### Rule 2.2: Account Type Filter Operation Validation
**What it checks**: Ensures the account_type_filter_operation parameter is either "INCLUDE" or "EXCLUDE" when provided.

**Why it exists**: To ensure the filtering logic can be applied correctly with a known operation mode.

**When it applies**: When the optional account_type_filter_operation query parameter is included in the request.

**Who it affects**: API consumers implementing account filtering functionality.

**What happens when it fails**: Returns validation error stating "Account type filter operation must be INCLUDE or EXCLUDE".

**Where it is enforced**: In the query parameter validation phase, specifically for the filter operation parameter.

**Example**:
- Valid: account_type_filter_operation="INCLUDE" → Validation passes
- Valid: account_type_filter_operation="EXCLUDE" → Validation passes
- Invalid: account_type_filter_operation="MAYBE" → Error returned

---

#### Rule 2.3: Filter Operation Default Handling
**What it checks**: Applies default value of "INCLUDE" when account_type_filter_operation is not specified but account_type_filter is provided.

**Why it exists**: To provide sensible default behavior and improve API usability by not requiring all parameters.

**When it applies**: When account_type_filter is provided but account_type_filter_operation is omitted.

**Who it affects**: API consumers who want simple filtering without specifying operation mode.

**What happens when it fails**: This is a default rule, so it doesn't fail; it automatically applies INCLUDE operation.

**Where it is enforced**: In the filter processing logic before applying account type filters.

**Example**:
- Scenario: account_type_filter="CHECKING" with no operation specified → System uses INCLUDE by default

---

#### Rule 2.4: Empty Filter List Handling
**What it checks**: Determines behavior when account_type_filter is empty or not provided.

**Why it exists**: To ensure all accounts are returned when no filtering is requested, providing complete account visibility.

**When it applies**: When the account_type_filter parameter is absent or contains an empty value.

**Who it affects**: All users who want to see all their accounts without filtering.

**What happens when it fails**: This is a permissive rule that doesn't fail; it returns all accounts regardless of type.

**Where it is enforced**: In the filter application logic, as a bypass condition.

**Example**:
- Scenario: No account_type_filter provided → All account types returned

---

### 3. Data Access and Permission Validations

#### Rule 3.1: View Permission Check
**What it checks**: Verifies that the user has at least one view permission for each account before including it in the results.

**Why it exists**: To enforce fine-grained access control and ensure users only see accounts they have explicit permission to view.

**When it applies**: For each account retrieved from the system, before adding it to the response list.

**Who it affects**: All users, as view permissions determine which accounts appear in their account list.

**What happens when it fails**: The account is silently excluded from the results (no error, just not included in the list).

**Where it is enforced**: In the account filtering logic after retrieving accounts from the core banking system.

**Example**:
- Valid: User has "owner" view on Account A → Account A included in results
- Invalid: User has no views on Account B → Account B excluded from results

---

#### Rule 3.2: Bank-Account Relationship Validation
**What it checks**: Ensures that when using the bank-specific endpoint, only accounts belonging to the specified bank are returned.

**Why it exists**: To maintain data integrity and ensure users receive accurate, bank-specific account information.

**When it applies**: When using the GET /users/{USER_ID}/banks/{BANK_ID}/accounts-held endpoint.

**Who it affects**: Users and applications requesting accounts at a specific bank.

**What happens when it fails**: Accounts from other banks are automatically excluded (not an error, but a filter).

**Where it is enforced**: In the account retrieval logic when querying the core banking system.

**Example**:
- Scenario: User requests accounts at Bank A → Only accounts at Bank A returned, even if user has accounts at Bank B

---

#### Rule 3.3: Cross-Bank Access Scope Validation
**What it checks**: Confirms that when using the all-banks endpoint, the user has CanGetAccountsHeldAtAnyBank entitlement.

**Why it exists**: To prevent unauthorized cross-bank account visibility and enforce proper authorization scope.

**When it applies**: When using the GET /users/{USER_ID}/accounts-held endpoint (without bank ID).

**Who it affects**: Users attempting to retrieve accounts across multiple banks.

**What happens when it fails**: Returns authorization error (HTTP 403 Forbidden) indicating insufficient permissions for cross-bank access.

**Where it is enforced**: In the authorization layer specific to the all-banks endpoint.

**Example**:
- Valid: User with CanGetAccountsHeldAtAnyBank entitlement → Can access all-banks endpoint
- Invalid: User with only CanGetAccountsHeldAtOneBank → Cannot access all-banks endpoint

---

### 4. Performance and Quality Validations

#### Rule 4.1: Response Time Validation
**What it checks**: Ensures the account listing response is returned within 2 seconds for typical user account lists.

**Why it exists**: To maintain acceptable user experience and system performance standards.

**When it applies**: For every account listing request, measured from request receipt to response delivery.

**Who it affects**: All users, particularly those with many accounts or during high-traffic periods.

**What happens when it fails**: System may trigger performance alerts; users experience slow response times.

**Where it is enforced**: At the service level with performance monitoring and potentially timeout mechanisms.

**Example**:
- Valid: Request returns in 1.5 seconds → Performance target met
- Invalid: Request takes 5 seconds → Performance issue flagged for investigation

---

#### Rule 4.2: Pagination Support Validation
**What it checks**: Ensures the system can handle pagination for users with large numbers of accounts.

**Why it exists**: To prevent performance degradation and timeout issues when users have many accounts.

**When it applies**: When a user has more accounts than can be efficiently returned in a single response.

**Who it affects**: Users with many accounts (business customers, administrators, aggregated views).

**What happens when it fails**: May result in slow responses, timeouts, or incomplete data if pagination is not properly implemented.

**Where it is enforced**: In the account retrieval and response formatting logic.

**Example**:
- Scenario: User has 500 accounts → System returns paginated results with page size of 50

---

### 5. Data Integrity Validations

#### Rule 5.1: Account Data Completeness Validation
**What it checks**: Ensures each account in the response includes required core information (account ID, bank ID, label, number, account_routings).

**Why it exists**: To guarantee API consumers receive complete, usable account information for downstream operations.

**When it applies**: When formatting each account object in the response JSON.

**Who it affects**: API consumers and applications that depend on complete account data.

**What happens when it fails**: May result in incomplete response data or internal errors if required fields are missing.

**Where it is enforced**: In the JSON response formatting layer (JSONFactory300.createCoreAccountsByCoreAccountsJSON).

**Example**:
- Valid: Account object includes id, bank_id, label, number, account_routings → Response formatted successfully
- Invalid: Account missing required field → Error or incomplete data

---

#### Rule 5.2: Account Status Validation
**What it checks**: Implicitly validates that only active, accessible accounts are included in the results.

**Why it exists**: To prevent users from seeing closed, suspended, or otherwise inaccessible accounts.

**When it applies**: During account retrieval from the core banking system.

**Who it affects**: All users, ensuring they only see accounts they can actually use.

**What happens when it fails**: Inactive accounts are excluded from results (not an error, but a filter).

**Where it is enforced**: In the account retrieval logic at the connector level.

**Example**:
- Valid: Active account with status "OPEN" → Included in results
- Invalid: Closed account with status "CLOSED" → Excluded from results

---

### 6. Filter Logic Validations

#### Rule 6.1: INCLUDE Filter Logic Validation
**What it checks**: When operation is INCLUDE, validates that only accounts matching the specified types are returned.

**Why it exists**: To provide precise filtering capability for users who want to see specific account types.

**When it applies**: When account_type_filter is provided with operation set to INCLUDE (or defaulted to INCLUDE).

**Who it affects**: Users filtering for specific account types (e.g., only checking accounts).

**What happens when it fails**: This is a logic rule; incorrect implementation would return wrong accounts.

**Where it is enforced**: In AccountsHelper.filterWithAccountType method.

**Example**:
- Scenario: Filter="CHECKING" with INCLUDE → Only checking accounts returned
- Scenario: Filter="CHECKING,SAVINGS" with INCLUDE → Only checking and savings accounts returned

---

#### Rule 6.2: EXCLUDE Filter Logic Validation
**What it checks**: When operation is EXCLUDE, validates that accounts matching the specified types are removed from results.

**Why it exists**: To allow users to hide specific account types they don't want to see.

**When it applies**: When account_type_filter is provided with operation explicitly set to EXCLUDE.

**Who it affects**: Users who want to see all accounts except certain types.

**What happens when it fails**: This is a logic rule; incorrect implementation would return wrong accounts.

**Where it is enforced**: In AccountsHelper.filterWithAccountType method.

**Example**:
- Scenario: Filter="LOAN" with EXCLUDE → All accounts except loan accounts returned
- Scenario: Filter="SAVINGS,INVESTMENT" with EXCLUDE → All accounts except savings and investment accounts returned

---

#### Rule 6.3: Multiple Account Type Filter Validation
**What it checks**: Validates that comma-separated account type values are properly parsed and applied.

**Why it exists**: To support filtering by multiple account types in a single request.

**When it applies**: When account_type_filter contains multiple comma-separated values.

**Who it affects**: API consumers implementing complex filtering scenarios.

**What happens when it fails**: May result in incorrect filtering or parsing errors if not handled properly.

**Where it is enforced**: In the filter parsing and application logic.

**Example**:
- Valid: "CHECKING,SAVINGS,MONEY_MARKET" → Three types parsed and applied
- Invalid: Malformed filter string → Parsing error

---

### 7. Security and Audit Validations

#### Rule 7.1: Audit Trail Requirement
**What it checks**: Ensures all account access requests are logged for audit purposes.

**Why it exists**: To maintain compliance with banking regulations and enable security monitoring.

**When it applies**: For every account listing request, regardless of success or failure.

**Who it affects**: Compliance officers, security teams, and auditors who review access logs.

**What happens when it fails**: Audit logging failure may trigger alerts but should not block the request.

**Where it is enforced**: In the audit logging layer, typically as a cross-cutting concern.

**Example**:
- Scenario: User requests account list → Log entry created with user ID, timestamp, accounts accessed

---

#### Rule 7.2: Sensitive Data Protection
**What it checks**: Ensures only non-sensitive core account information is returned in the listing (not full account details).

**Why it exists**: To minimize exposure of sensitive data and follow principle of least privilege.

**When it applies**: When formatting the response, ensuring only core fields are included.

**Who it affects**: All users, protecting their account privacy and security.

**What happens when it fails**: Over-exposure of data could violate privacy regulations and security policies.

**Where it is enforced**: In the response formatting logic (CoreAccountsHeldJsonV300 structure).

**Example**:
- Valid: Response includes account ID, label, number → Appropriate level of detail
- Invalid: Response includes full transaction history, balances → Too much information for listing

---

### 8. Error Handling Validations

#### Rule 8.1: Invalid User ID Error Response
**What it checks**: Validates that appropriate error message is returned when user ID is invalid.

**Why it exists**: To provide clear feedback to API consumers about what went wrong.

**When it applies**: When the USER_ID parameter does not match any existing user.

**Who it affects**: API consumers and developers integrating with the account listing API.

**What happens when it fails**: Generic or unclear error messages make troubleshooting difficult.

**Where it is enforced**: In the error handling and response formatting logic.

**Example**:
- Response: {"error": "User not found", "user_id": "invalid123", "status": 404}

---

#### Rule 8.2: Invalid Bank ID Error Response
**What it checks**: Validates that appropriate error message is returned when bank ID is invalid.

**Why it exists**: To provide clear feedback about bank-specific request failures.

**When it applies**: When the BANK_ID parameter does not match any existing bank.

**Who it affects**: API consumers making bank-specific account listing requests.

**What happens when it fails**: Unclear error messages lead to confusion and support requests.

**Where it is enforced**: In the error handling and response formatting logic.

**Example**:
- Response: {"error": "Bank not found", "bank_id": "invalid-bank", "status": 404}

---

#### Rule 8.3: Authorization Error Response
**What it checks**: Validates that clear authorization error is returned when user lacks required entitlements.

**Why it exists**: To inform users why their request was denied and what permissions they need.

**When it applies**: When entitlement check fails for the requested operation.

**Who it affects**: Users attempting operations beyond their authorization level.

**What happens when it fails**: Users don't understand why access is denied or how to resolve it.

**Where it is enforced**: In the authorization layer error handling.

**Example**:
- Response: {"error": "Insufficient permissions", "required_entitlement": "CanGetAccountsHeldAtAnyBank", "status": 403}

---

## Validation Rules Summary Table

| Rule ID | Category | Rule Name | Severity | Failure Impact |
|---------|----------|-----------|----------|----------------|
| 1.1 | Authentication | User Authentication Check | Critical | Request blocked |
| 1.2 | Authentication | User ID Validation | Critical | Error returned |
| 1.3 | Authentication | Bank ID Validation | High | Error returned |
| 1.4 | Authorization | Entitlement Verification | Critical | Access denied |
| 2.1 | Input | Account Type Filter Format | Medium | Validation error |
| 2.2 | Input | Filter Operation Validation | Medium | Validation error |
| 2.3 | Input | Filter Operation Default | Low | Default applied |
| 2.4 | Input | Empty Filter Handling | Low | All accounts returned |
| 3.1 | Access Control | View Permission Check | Critical | Account excluded |
| 3.2 | Access Control | Bank-Account Relationship | High | Account filtered |
| 3.3 | Access Control | Cross-Bank Access Scope | Critical | Access denied |
| 4.1 | Performance | Response Time Validation | Medium | Performance alert |
| 4.2 | Performance | Pagination Support | Medium | Slow response |
| 5.1 | Data Integrity | Account Data Completeness | High | Incomplete data |
| 5.2 | Data Integrity | Account Status Validation | High | Account excluded |
| 6.1 | Filter Logic | INCLUDE Filter Logic | Medium | Wrong results |
| 6.2 | Filter Logic | EXCLUDE Filter Logic | Medium | Wrong results |
| 6.3 | Filter Logic | Multiple Type Filter | Medium | Parsing error |
| 7.1 | Security | Audit Trail Requirement | High | Compliance issue |
| 7.2 | Security | Sensitive Data Protection | Critical | Privacy violation |
| 8.1 | Error Handling | Invalid User ID Error | Low | Poor UX |
| 8.2 | Error Handling | Invalid Bank ID Error | Low | Poor UX |
| 8.3 | Error Handling | Authorization Error | Medium | Poor UX |

---

## Stakeholder-Specific Insights

### For Business Analysts
The Account Listing feature enforces strict access control through view permissions and entitlements. Users can only see accounts they have explicit permission to access, and the system supports flexible filtering by account type. The business logic ensures that bank-specific requests are properly scoped and cross-bank access requires elevated permissions.

### For Compliance Officers
Key compliance validations include mandatory audit logging of all account access requests, enforcement of view-based permissions for data privacy, and proper authentication/authorization checks. The system follows the principle of least privilege by returning only core account information in listings, not full sensitive details.

### For Quality Assurance Teams
Test scenarios should cover: valid and invalid user/bank IDs, various entitlement combinations, account type filtering with INCLUDE/EXCLUDE operations, pagination with large account sets, view permission variations, and error message accuracy. Edge cases include users with no accounts, users with accounts at multiple banks, and empty filter lists.

### For Product Managers
Current limitations include the requirement for specific entitlements to access cross-bank account lists, which may create friction for some user types. The filtering capability is flexible but requires understanding of INCLUDE/EXCLUDE operations. Performance targets (2-second response time) may need adjustment for users with very large account portfolios.

### For Customer Support Teams
Common issues likely include: users not seeing expected accounts (due to view permissions), confusion about account type filtering, authorization errors when attempting cross-bank access, and performance concerns for users with many accounts. Clear error messages help users understand why requests fail.

### For System Administrators
Configuration considerations include entitlement assignments for different user roles, view permission setup for accounts, pagination settings for performance optimization, and audit logging configuration. Monitoring should track response times, authorization failures, and unusual access patterns.

### For Security Teams
Security validations include authentication verification, entitlement-based authorization, view permission enforcement, and audit trail maintenance. The system prevents unauthorized access through multiple layers of checks and follows the principle of least privilege in data exposure.

---

## Related Business Rules

The following business rules work together with these validation rules:

1. **View Permission Model**: Users must have at least one view on an account to see it in listings
2. **Entitlement Hierarchy**: CanGetAccountsHeldAtAnyBank is a superset of CanGetAccountsHeldAtOneBank
3. **Account Type Taxonomy**: System maintains a defined set of valid account types
4. **Bank-Account Ownership**: Accounts belong to exactly one bank
5. **Active Account Principle**: Only active, accessible accounts appear in listings
6. **Audit Requirement**: All account access must be logged for compliance
7. **Performance SLA**: Account listing responses must complete within 2 seconds
8. **Data Minimization**: Listings return only core account information, not full details
9. **Filter Default Behavior**: When no filter is specified, all account types are included
10. **Pagination Strategy**: Large result sets are paginated to maintain performance

---

## Implementation Checklist

- [x] User authentication validation implemented
- [x] User ID existence check implemented
- [x] Bank ID validation implemented
- [x] Entitlement verification implemented
- [x] Account type filter format validation implemented
- [x] Filter operation validation (INCLUDE/EXCLUDE) implemented
- [x] Default filter operation handling implemented
- [x] Empty filter list handling implemented
- [x] View permission check for each account implemented
- [x] Bank-account relationship filtering implemented
- [x] Cross-bank access scope validation implemented
- [x] Response time monitoring implemented
- [x] Pagination support implemented
- [x] Account data completeness validation implemented
- [x] Account status filtering implemented
- [x] INCLUDE filter logic implemented
- [x] EXCLUDE filter logic implemented
- [x] Multiple account type parsing implemented
- [x] Audit trail logging implemented
- [x] Sensitive data protection implemented
- [x] Error message clarity for invalid user ID implemented
- [x] Error message clarity for invalid bank ID implemented
- [x] Authorization error messaging implemented

---

## Testing Scenarios

### Positive Test Cases
1. Authenticated user with valid entitlements requests accounts at specific bank → Returns filtered account list
2. User with CanGetAccountsHeldAtAnyBank requests all accounts → Returns accounts across all banks
3. User applies INCLUDE filter for "CHECKING" accounts → Returns only checking accounts
4. User applies EXCLUDE filter for "LOAN" accounts → Returns all accounts except loans
5. User with view permissions on 5 accounts → All 5 accounts returned
6. Request with no filters → All accessible accounts returned

### Negative Test Cases
1. Unauthenticated request → Returns 401 Unauthorized
2. Invalid user ID → Returns 404 User Not Found
3. Invalid bank ID → Returns 404 Bank Not Found
4. User without required entitlement → Returns 403 Forbidden
5. Invalid account type filter value → Returns 400 Bad Request with validation error
6. Invalid filter operation (not INCLUDE/EXCLUDE) → Returns 400 Bad Request
7. User with no view permissions on any accounts → Returns empty list

### Edge Cases
1. User with 1000+ accounts → Pagination applied, response within 2 seconds
2. User with accounts at 10 different banks using all-banks endpoint → All accounts returned
3. Empty account type filter with INCLUDE operation → All accounts returned
4. Multiple comma-separated account types → All specified types filtered correctly
5. User with mix of active and closed accounts → Only active accounts returned
6. Concurrent requests from same user → Each request properly validated and logged

---

## Conclusion

The Account Listing feature implements 23 distinct validation rules across 8 categories to ensure secure, performant, and compliant account data access. These validations work together to enforce authentication, authorization, data integrity, performance standards, and regulatory compliance. The multi-layered approach ensures that users can only access accounts they have permission to view, while maintaining system performance and providing clear error feedback when validation fails.

The validation rules support the core business requirement of allowing users to retrieve their accessible accounts while protecting sensitive banking data and maintaining system integrity. Future enhancements should consider additional filtering options, improved pagination controls, and enhanced performance optimization for users with very large account portfolios.
