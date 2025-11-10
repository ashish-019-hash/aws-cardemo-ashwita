# Business Rules Extraction: Account Listing

## User Story Overview
**Feature:** Account Listing  
**API Endpoints:**
- GET /obp/v5.1.0/users/{USER_ID}/banks/{BANK_ID}/accounts-held
- GET /obp/v5.1.0/users/{USER_ID}/accounts-held

**Purpose:** Retrieve all bank accounts accessible to a user at one or multiple banks for account selection and overview

---

## Business Rules Extracted by Role

### Role 1: Bank Compliance Officer Perspective
**Focus:** Rules ensuring regulatory compliance and customer protection

#### Rule 1.1: User Authentication Requirement
**What it does:** All account listing requests must be made by authenticated users only

**When it applies:** Every time a user attempts to retrieve their account list through any account listing endpoint

**Who it affects:** All API consumers, banking application users, system administrators

**Example:** When a mobile banking app user opens the accounts screen, the system first verifies their authentication token before retrieving any account information. An unauthenticated request will be rejected immediately.

#### Rule 1.2: View Permission Verification
**What it does:** Only accounts where the user has at least one view permission are included in the results

**When it applies:** During the account filtering process after retrieving the raw account list from the core banking system

**Who it affects:** Users requesting account lists, account owners, users with delegated access

**Example:** If a user has been granted "owner" view on Account A and "public" view on Account B, but no view permissions on Account C, the system will return only Accounts A and B in the listing, even if all three accounts exist at the bank.

#### Rule 1.3: Audit Trail Maintenance
**What it does:** All account access requests must be logged for regulatory audit purposes

**When it applies:** Every successful and failed account listing request

**Who it affects:** Compliance officers, auditors, system administrators

**Example:** When a user retrieves their account list at 10:00 AM, the system records the user ID, timestamp, bank ID (if specified), number of accounts returned, and the request source in the audit log for future compliance reviews.

---

### Role 2: Customer Service Manager Perspective
**Focus:** Rules governing customer interactions with the bank

#### Rule 2.1: Self-Service Account Access
**What it does:** Customers can retrieve their own account lists without requiring bank staff assistance

**When it applies:** When authenticated users access the account listing endpoints with their own user ID

**Who it affects:** Banking customers, mobile app users, online banking users

**Example:** A customer logs into their mobile banking app and immediately sees a list of all their checking, savings, and credit card accounts without needing to call customer service or visit a branch.

#### Rule 2.2: Multi-Bank Account Aggregation
**What it does:** Users can retrieve accounts across all banks they have access to in a single request

**When it applies:** When using the all-banks endpoint (/users/{USER_ID}/accounts-held) instead of the bank-specific endpoint

**Who it affects:** Users with accounts at multiple banks within the Open Bank Project network

**Example:** A business owner who has accounts at three different banks can use the all-banks endpoint to see all their accounts (personal checking at Bank A, business account at Bank B, savings at Bank C) in one consolidated view.

#### Rule 2.3: Account Type Filtering
**What it does:** Customers can filter the account list by account type using optional query parameters

**When it applies:** When the account_type_filter parameter is provided in the request

**Who it affects:** Users who want to view specific types of accounts (e.g., only checking accounts, only savings accounts)

**Example:** A user who only wants to see their checking accounts can add "?account_type_filter=checking&account_type_filter_operation=INCLUDE" to the request, and the system will return only checking accounts, excluding savings, loans, and other account types.

---

### Role 3: Risk Management Specialist Perspective
**Focus:** Rules protecting the bank from fraud and financial risk

#### Rule 3.1: Entitlement-Based Access Control
**What it does:** Users must have specific entitlements (CanGetAccountsHeldAtOneBank or CanGetAccountsHeldAtAnyBank) to access account listing endpoints

**When it applies:** Before processing any account listing request

**Who it affects:** API consumers, third-party applications, bank staff, customers

**Example:** A third-party financial aggregation app attempting to retrieve a user's accounts must have the CanGetAccountsHeldAtAnyBank entitlement. Without this entitlement, the system rejects the request even if the user has authorized the app.

#### Rule 3.2: Scope-Based Entitlement Enforcement
**What it does:** Bank-specific endpoint requires CanGetAccountsHeldAtOneBank OR CanGetAccountsHeldAtAnyBank, while all-banks endpoint requires only CanGetAccountsHeldAtAnyBank

**When it applies:** During entitlement verification for each endpoint type

**Who it affects:** Users, applications, and services with different permission levels

**Example:** A customer service representative with CanGetAccountsHeldAtOneBank entitlement can help a customer view accounts at their specific bank branch, but cannot access the all-banks endpoint to see accounts across the entire banking network.

#### Rule 3.3: Response Time Performance Limit
**What it does:** Account listing requests must complete within 2 seconds for typical user account lists

**When it applies:** During all account listing operations

**Who it affects:** End users, system performance monitoring, infrastructure teams

**Example:** If a user has 15 accounts and the system takes 3 seconds to respond, this triggers a performance alert for the operations team to investigate potential database query optimization or caching improvements.

---

### Role 4: Product Manager Perspective
**Focus:** Rules defining banking products and services

#### Rule 4.1: Account Type Classification
**What it does:** Accounts are classified by type (checking, savings, loan, credit card, etc.) and can be filtered based on these classifications

**When it applies:** When accounts are stored, retrieved, and filtered

**Who it affects:** Product managers, customers viewing accounts, reporting systems

**Example:** The system categorizes accounts so that when a customer wants to transfer money, they can filter to show only "checking" and "savings" accounts as valid source accounts, excluding loan accounts which cannot be used as transfer sources.

#### Rule 4.2: Core Account Information Standard
**What it does:** All account listings must include core account information: account ID, bank ID, label, number, and account_routings

**When it applies:** When formatting the response for any account listing request

**Who it affects:** API consumers, frontend applications, integration partners

**Example:** When a mobile app displays the account list, it receives standardized data including the account number (for display), account ID (for subsequent API calls), and routing information (for payment setup), ensuring consistent user experience across all channels.

---

### Role 5: Operations Director Perspective
**Focus:** Rules governing internal bank processes and workflows

#### Rule 5.1: Pagination Support Requirement
**What it does:** The system must support pagination mechanisms for users with large numbers of accounts

**When it applies:** When processing account listing requests for users with many accounts

**Who it affects:** Users with numerous accounts, system performance, database load

**Example:** A corporate treasurer managing 200 company accounts receives results in pages of 50 accounts each, preventing system overload and ensuring responsive user interface performance.

#### Rule 5.2: Bank ID Validation
**What it does:** When using the bank-specific endpoint, the provided bank ID must be valid and exist in the system

**When it applies:** During request validation for the bank-specific account listing endpoint

**Who it affects:** API consumers, application developers, error handling systems

**Example:** If an application sends a request with bank ID "INVALID_BANK_123", the system returns an error message "Invalid bank ID" rather than attempting to retrieve accounts, preventing unnecessary database queries.

#### Rule 5.3: User ID Validation
**What it does:** The user ID in the request must be valid and exist in the system

**When it applies:** During request validation for all account listing endpoints

**Who it affects:** API consumers, authentication systems, error handling

**Example:** If a request includes a user ID that doesn't exist in the system, the API returns an appropriate error message rather than returning an empty account list, helping developers distinguish between "no accounts" and "invalid user".

#### Rule 5.4: Short TTL Caching Strategy
**What it does:** Frequently requested account lists should be cached with short time-to-live (TTL) to improve performance

**When it applies:** For repeated account listing requests from the same user within a short time period

**Who it affects:** System performance, user experience, infrastructure costs

**Example:** When a user refreshes their account list screen multiple times within 30 seconds, the system serves the cached response for the first few requests rather than querying the database each time, reducing load and improving response time.

---

### Role 6: Treasury and Payment Specialist Perspective
**Focus:** Rules controlling money movement and payment processing

#### Rule 6.1: Account Routing Information Provision
**What it does:** Account listings must include account routing information (IBAN, account number, routing codes) for payment processing

**When it applies:** When formatting account listing responses

**Who it affects:** Payment processors, users setting up transfers, third-party payment applications

**Example:** When a user wants to set up a direct deposit, the account listing provides the routing number and account number needed by their employer's payroll system to process the payment correctly.

---

### Role 7: Security and Access Control Manager Perspective
**Focus:** Rules protecting the system and controlling access

#### Rule 7.1: View-Based Access Control
**What it does:** Account visibility is controlled through view permissions, and only accounts with at least one view permission for the requesting user are returned

**When it applies:** During the account filtering process after retrieving accounts from the core system

**Who it affects:** Account owners, delegated users, auditors, compliance officers

**Example:** A financial advisor granted "public" view access to a client's investment account can see that account in their listing, but cannot see the client's personal checking account where they have no view permissions.

#### Rule 7.2: Account Type Filter Operation Validation
**What it does:** The account_type_filter_operation parameter must be either "INCLUDE" or "EXCLUDE" if specified

**When it applies:** During request parameter validation when account type filtering is requested

**Who it affects:** API consumers, application developers, validation systems

**Example:** If a developer sends a request with account_type_filter_operation set to "ONLY", the system rejects the request with a validation error explaining that only "INCLUDE" or "EXCLUDE" are valid operations.

#### Rule 7.3: Default Filter Behavior
**What it does:** When account type filter is not specified or the filter list is empty, all account types are returned

**When it applies:** When processing account listing requests without type filters

**Who it affects:** Users, applications using default behavior

**Example:** A user accessing their account list without specifying any filters sees all their accounts (checking, savings, credit cards, loans) in the response, providing a complete overview of their banking relationship.

#### Rule 7.4: Filter Operation Default
**What it does:** If account_type_filter is provided but account_type_filter_operation is not specified, the system defaults to "INCLUDE" operation

**When it applies:** During filter parameter processing when operation is omitted

**Who it affects:** API consumers, backward compatibility

**Example:** A legacy application that only sends account_type_filter=checking without specifying the operation will have the system automatically apply "INCLUDE" logic, showing only checking accounts.

#### Rule 7.5: Multiple Account Type Filtering
**What it does:** Account type filter supports multiple account types via comma-separated values

**When it applies:** When users want to filter for multiple specific account types

**Who it affects:** Users, applications requiring flexible filtering

**Example:** A user wanting to see only their deposit accounts can specify "account_type_filter=checking,savings&account_type_filter_operation=INCLUDE" to retrieve both checking and savings accounts while excluding loans and credit cards.

---

## Summary of Business Rules by Category

### Access and Permission Rules
- Rule 1.1: User Authentication Requirement
- Rule 1.2: View Permission Verification
- Rule 3.1: Entitlement-Based Access Control
- Rule 3.2: Scope-Based Entitlement Enforcement
- Rule 7.1: View-Based Access Control

### Validation and Verification Rules
- Rule 5.2: Bank ID Validation
- Rule 5.3: User ID Validation
- Rule 7.2: Account Type Filter Operation Validation

### Processing and Workflow Rules
- Rule 2.1: Self-Service Account Access
- Rule 2.2: Multi-Bank Account Aggregation
- Rule 5.1: Pagination Support Requirement
- Rule 5.4: Short TTL Caching Strategy

### Financial and Calculation Rules
- Rule 6.1: Account Routing Information Provision

### Compliance and Audit Rules
- Rule 1.3: Audit Trail Maintenance

### Customer and Account Rules
- Rule 2.3: Account Type Filtering
- Rule 4.1: Account Type Classification
- Rule 4.2: Core Account Information Standard

### Transaction and Payment Rules
- (No specific transaction rules for account listing, but Rule 6.1 supports payment setup)

### Security and Authentication Rules
- Rule 7.3: Default Filter Behavior
- Rule 7.4: Filter Operation Default
- Rule 7.5: Multiple Account Type Filtering

### Performance and Quality Rules
- Rule 3.3: Response Time Performance Limit

---

## Implementation Considerations

### Critical Business Rules for Migration
When migrating the Account Listing functionality to Go, the following business rules are absolutely critical and must be preserved:

1. **Authentication and Authorization** (Rules 1.1, 3.1, 3.2, 7.1): The security model must be replicated exactly to prevent unauthorized access
2. **View Permission Filtering** (Rules 1.2, 7.1): Only accounts with proper view permissions should be returned
3. **Validation Rules** (Rules 5.2, 5.3, 7.2): Input validation must match the original behavior to maintain API contract
4. **Filter Logic** (Rules 2.3, 7.3, 7.4, 7.5): Account type filtering must work identically to ensure backward compatibility
5. **Response Format** (Rule 4.2): The JSON response structure must match exactly for API consumers

### Testing Requirements
Each business rule should have corresponding test cases in the Go application:
- Test authenticated vs unauthenticated requests (Rule 1.1)
- Test view permission filtering with various permission combinations (Rule 1.2, 7.1)
- Test entitlement validation for both endpoints (Rule 3.1, 3.2)
- Test account type filtering with INCLUDE/EXCLUDE operations (Rules 2.3, 7.2, 7.5)
- Test default behavior when filters are omitted (Rules 7.3, 7.4)
- Test invalid bank ID and user ID handling (Rules 5.2, 5.3)
- Test response time under load (Rule 3.3)
- Test pagination with large account sets (Rule 5.1)

### Performance Considerations
- Implement caching strategy as per Rule 5.4
- Ensure response time meets the 2-second requirement (Rule 3.3)
- Optimize database queries for view permission checks (Rule 1.2)
- Implement efficient pagination (Rule 5.1)

### Audit and Compliance
- Implement comprehensive audit logging (Rule 1.3)
- Ensure all access attempts are recorded
- Maintain audit trail for regulatory compliance

---

## Endpoint-Specific Business Rules

### GET /obp/v5.1.0/users/{USER_ID}/banks/{BANK_ID}/accounts-held

**Required Entitlements:**
- CanGetAccountsHeldAtOneBank OR CanGetAccountsHeldAtAnyBank

**Validation Rules:**
- User ID must be valid (Rule 5.3)
- Bank ID must be valid (Rule 5.2)
- User must be authenticated (Rule 1.1)
- User must have required entitlement (Rule 3.1, 3.2)

**Processing Rules:**
- Filter by view permissions (Rule 1.2, 7.1)
- Apply account type filter if specified (Rule 2.3)
- Include core account information (Rule 4.2)
- Include account routing information (Rule 6.1)
- Support pagination (Rule 5.1)
- Complete within 2 seconds (Rule 3.3)
- Log access attempt (Rule 1.3)

### GET /obp/v5.1.0/users/{USER_ID}/accounts-held

**Required Entitlements:**
- CanGetAccountsHeldAtAnyBank (only)

**Validation Rules:**
- User ID must be valid (Rule 5.3)
- User must be authenticated (Rule 1.1)
- User must have CanGetAccountsHeldAtAnyBank entitlement (Rule 3.1, 3.2)

**Processing Rules:**
- Retrieve accounts across all banks (Rule 2.2)
- Filter by view permissions (Rule 1.2, 7.1)
- Apply account type filter if specified (Rule 2.3)
- Include core account information (Rule 4.2)
- Include account routing information (Rule 6.1)
- Support pagination (Rule 5.1)
- Complete within 2 seconds (Rule 3.3)
- Log access attempt (Rule 1.3)

---

## Query Parameter Business Rules

### account_type_filter
**Type:** String (comma-separated values)  
**Optional:** Yes  
**Default:** Empty (all types included)

**Business Rules:**
- Supports multiple account types via comma separation (Rule 7.5)
- Empty or omitted means no filtering (Rule 7.3)
- Values must match valid account types (Rule 4.1)

### account_type_filter_operation
**Type:** String  
**Optional:** Yes  
**Valid Values:** "INCLUDE", "EXCLUDE"  
**Default:** "INCLUDE"

**Business Rules:**
- Must be "INCLUDE" or "EXCLUDE" if specified (Rule 7.2)
- Defaults to "INCLUDE" if omitted (Rule 7.4)
- Controls whether filter list is inclusion or exclusion (Rule 2.3)

---

## Error Handling Business Rules

### Invalid User ID
**Response:** Error message indicating invalid user ID  
**HTTP Status:** 400 Bad Request or 404 Not Found  
**Business Rule:** Rule 5.3

### Invalid Bank ID
**Response:** Error message indicating invalid bank ID  
**HTTP Status:** 400 Bad Request or 404 Not Found  
**Business Rule:** Rule 5.2

### Missing Authentication
**Response:** Authentication required error  
**HTTP Status:** 401 Unauthorized  
**Business Rule:** Rule 1.1

### Insufficient Entitlements
**Response:** Permission denied error  
**HTTP Status:** 403 Forbidden  
**Business Rule:** Rule 3.1, 3.2

### Invalid Filter Operation
**Response:** Validation error explaining valid operations  
**HTTP Status:** 400 Bad Request  
**Business Rule:** Rule 7.2

---

## Data Privacy and Security Rules

### Personal Information Protection
- Only return accounts where user has view permissions (Rule 1.2, 7.1)
- Respect view-based access control for data visibility
- Log all access for audit purposes (Rule 1.3)

### Multi-Tenancy Rules
- Bank-specific endpoint only returns accounts from specified bank
- All-banks endpoint returns accounts across all accessible banks (Rule 2.2)
- View permissions are bank-specific and must be checked per bank

---

## Conclusion

This business rules extraction identifies 21 distinct business rules governing the Account Listing functionality across 7 different organizational perspectives. These rules cover authentication, authorization, validation, filtering, performance, compliance, and data formatting requirements. When migrating this functionality to Go, all these rules must be preserved to ensure the new implementation maintains functional equivalence with the Scala application and can be validated using the existing test cases.
