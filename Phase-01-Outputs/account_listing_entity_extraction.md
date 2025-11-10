# Business Entity Extraction - Account Listing User Story

Applied the entity extraction prompt to the Account Listing user story from the OBP-API Account Management documentation.

## Context

This entity extraction analysis is based on the **Open Bank Project (OBP-API)**, the open-source banking API platform.

- **Official Repository**: https://github.com/OpenBankProject/OBP-API.git
- **Technology Stack**: Scala-based REST API
- **Purpose**: Open-source API for banks that enables account holders to interact with their bank using a standard RESTful interface

All classes, methods, and services referenced in this document are from the Open Bank Project OBP-API codebase.

---

## Entity Catalog - Account Listing User Story

Based on the Account Listing user story (updated version), the following business entities have been identified:

### 1. **User** (Banking Application User / API Consumer)

**Business Description**: 
A person or system that uses the banking application and has authenticated access to the API. This represents the actor who wants to view their bank accounts and perform operations on them.

**Location in Codebase** (Open Bank Project OBP-API): 
- Referenced in API endpoints: APIMethods510.getAccountsHeldByUserAtBank, APIMethods510.getAccountsHeldByUser
- Used in authentication and authorization checks throughout the OBP-API authentication module
- Part of the user management system

**Key Fields**:
- User ID: Unique identifier for the user (required in API endpoint path)
- Authentication credentials: Validates user identity
- Entitlements: Permissions assigned to the user (CanGetAccountsHeldAtOneBank OR CanGetAccountsHeldAtAnyBank)
- User type: Distinguishes between different user roles

**Relationships to Other Entities**:
- A User can have access to multiple Bank Accounts
- A User has one or more Views on each accessible Account
- A User has specific Entitlements that control their access scope
- A User is associated with authentication context

**Business Rules**:
- User must be authenticated to retrieve account lists
- Only accounts where user has at least one view permission are returned
- User must have appropriate entitlements for the requested scope (single bank vs. all banks)
- Results respect user's entitlements (CanGetAccountsHeldAtOneBank or CanGetAccountsHeldAtAnyBank)
- View permission check is performed before including account in results

**Notes**:
- The user story focuses on what the user can do rather than the user entity structure itself
- Authentication and authorization are prerequisites for all operations
- User ID is a required path parameter in both API endpoints

---

### 2. **Bank Account** (Core Banking Entity)

**Business Description**: 
A financial account where money is stored and managed. This is the primary entity being listed in this user story. It represents a customer's relationship with a bank for holding and managing funds. The response includes core account information.

**Location in Codebase** (Open Bank Project OBP-API): 
- Classes: APIMethods510.getAccountsHeldByUserAtBank, APIMethods510.getAccountsHeldByUser
- Helper: AccountsHelper.filterWithAccountType (applies account type filtering)
- JSON Factory: JSONFactory300.createCoreAccountsByCoreAccountsJSON (formats response)
- Connector: Connector.getAccountsHeld, Connector.getAccountsHeldByUser (retrieves account data from core banking system)
- Repository: https://github.com/OpenBankProject/OBP-API.git

**Key Fields**:
- id: Unique identifier for the account
- bank_id: Identifier of the bank where the account is held
- label: Human-readable name or description of the account
- number: Account number
- account_routings: Routing information for the account (array/collection)
- account_type: Category of account (used for filtering)
- currency: The currency in which the account operates (implied from broader context)
- status: Active/inactive state (implied)

**Relationships to Other Entities**:
- A Bank Account belongs to one Bank
- A Bank Account is associated with one or more Users (account holders and authorized users)
- A Bank Account has one or more Views that control access permissions
- A Bank Account has a specific Account Type
- A Bank Account has Account Routings

**Business Rules**:
- User can only see accounts they have view permission for
- Account must exist in the system
- Results respect user's entitlements
- Account type filtering is optional and supports multiple types via comma-separated values
- Account type filter operation must be either INCLUDE or EXCLUDE
- Account type filter defaults to INCLUDE if not specified
- Empty filter list returns all accounts regardless of type
- Only accounts where user has at least one view permission are returned

**Notes**:
- This is the central entity in the Account Listing user story
- The user story focuses on the "core" account information subset (id, bank_id, label, number, account_routings)
- Full account details would include additional fields like balance, limits, etc.
- Response format must match CoreAccountsHeldJsonV300 structure exactly

---

### 3. **Bank** (Financial Institution)

**Business Description**: 
A financial institution that holds and manages customer accounts. The bank entity represents the organization providing banking services. Users can query accounts at a specific bank or across all banks.

**Location in Codebase** (Open Bank Project OBP-API): 
- Referenced in API methods: getAccountsHeldByUserAtBank (bank-specific endpoint)
- Used as a filter parameter in account retrieval
- Part of the core banking domain model in OBP-API
- Bank connector retrieves data from core banking system

**Key Fields**:
- Bank ID: Unique identifier for the bank (required in bank-specific endpoint)
- Bank name: Name of the financial institution (implied)
- Status: Active/inactive (implied)

**Relationships to Other Entities**:
- A Bank has multiple Bank Accounts
- A Bank has multiple Branches (implied from broader context)
- A Bank may have multiple Users accessing their accounts
- Bank-specific endpoint requires bank ID parameter

**Business Rules**:
- Bank ID must be valid if specified in the account listing request
- Users can retrieve accounts from a specific bank or across all banks depending on their entitlements
- Bank-specific endpoint requires bank ID parameter
- All-banks endpoint does not require bank ID parameter
- Invalid bank ID returns appropriate error message

**Notes**:
- The user story allows filtering by bank (single bank) or retrieving across all banks
- The distinction between single-bank and multi-bank access is controlled by user entitlements
- Two separate API endpoints handle single-bank vs. all-banks scenarios

---

### 4. **View / View Permission** (Access Control Entity)

**Business Description**: 
A view represents a specific level of access to account information. It controls what fields and operations a user can perform on an account. Views implement granular access control beyond simple yes/no account access. Only accounts where the user has at least one view permission are included in the listing results.

**Location in Codebase** (Open Bank Project OBP-API): 
- Class: ViewNewStyle (manages view permissions)
- Related to permission checking for account access
- Part of OBP-API's view-based access control system
- View permission check is performed before including account in results

**Key Fields**:
- View ID: Unique identifier for the view
- View name: Descriptive name (e.g., "owner", "public", "accountant")
- Permissions: Specific capabilities granted by this view
- Account reference: Which account this view applies to
- User reference: Which user has this view

**Relationships to Other Entities**:
- A View is associated with one Bank Account
- A View grants specific permissions to a User
- A User can have multiple Views on the same Account
- Only accounts where user has at least one View are returned in the listing

**Business Rules**:
- User must have at least one view permission on an account for it to appear in their account list
- Different views provide different levels of access to account information
- View permissions determine what account fields are visible to the user
- View permission check is performed before including account in results
- Only accounts the user has permission to view are returned

**Notes**:
- Views are a key security/access control mechanism in OBP-API
- The Account Listing operation filters results based on view permissions
- This is a sophisticated access control model beyond simple ownership
- Critical for fine-grained access control in multi-user scenarios

---

### 5. **Account Type** (Reference Data / Filter Criterion)

**Business Description**: 
A classification or category that defines the nature and purpose of a bank account. Common types include checking accounts, savings accounts, credit accounts, etc. Used as an optional filter criterion in account listing operations.

**Location in Codebase** (Open Bank Project OBP-API): 
- Referenced in query parameters for filtering (account_type_filter, account_type_filter_operation)
- Helper: AccountsHelper.filterWithAccountType (applies filtering logic)
- Part of core account information returned by OBP-API endpoints

**Key Fields**:
- Type code: Unique identifier for the account type
- Type name: Descriptive name (CURRENT, SAVINGS, CREDIT, etc.)
- Type description: Detailed explanation of the account type
- Filter operation: INCLUDE or EXCLUDE (determines filtering behavior)

**Relationships to Other Entities**:
- A Bank Account has one Account Type
- Account Type is used as a filter criterion in account listing
- Multiple account types can be specified in a single filter request

**Business Rules**:
- Account type filtering is optional
- Multiple account types can be specified via comma-separated values
- Account type values must match valid account types
- Account type filter operation must be "INCLUDE" or "EXCLUDE"
- Account type filter defaults to INCLUDE if not specified
- Empty filter list returns all accounts regardless of type
- Account type filter validation ensures operation is INCLUDE or EXCLUDE

**Notes**:
- Acts as reference/lookup data
- Used for categorization and filtering purposes
- The user story mentions this as an optional filter parameter with query parameters
- Supports both inclusion and exclusion logic for flexible filtering

---

### 6. **Entitlement** (Authorization Entity)

**Business Description**: 
A specific permission or right granted to a user that controls what operations they can perform in the system. Entitlements are broader than view permissions and control access to entire categories of operations. Two specific entitlements control account listing scope.

**Location in Codebase** (Open Bank Project OBP-API): 
- Referenced in business rules and entitlement management system
- Specific entitlements: CanGetAccountsHeldAtOneBank OR CanGetAccountsHeldAtAnyBank
- Part of OBP-API's role-based access control (RBAC) framework
- Checked before allowing API endpoint access

**Key Fields**:
- Entitlement ID: Unique identifier
- Entitlement name: Specific permission name (CanGetAccountsHeldAtOneBank, CanGetAccountsHeldAtAnyBank)
- User reference: Which user has this entitlement
- Scope: What the entitlement allows (single bank vs. all banks)

**Relationships to Other Entities**:
- Entitlements are granted to Users
- Entitlements control access scope (single bank vs. all banks)
- Works in conjunction with View permissions for complete access control
- Different entitlements required for different API endpoints

**Business Rules**:
- Results must respect user's entitlements (canGetAccountsHeldAtOneBank or canGetAccountsHeldAtAnyBank)
- CanGetAccountsHeldAtOneBank: Allows listing accounts at a specific bank
- CanGetAccountsHeldAtAnyBank: Allows listing accounts across all banks
- User must have appropriate entitlements for the requested scope
- Bank-specific endpoint requires CanGetAccountsHeldAtOneBank OR CanGetAccountsHeldAtAnyBank
- All-banks endpoint requires CanGetAccountsHeldAtAnyBank

**Notes**:
- Provides coarse-grained authorization (what APIs user can call)
- Complements the fine-grained View permissions (what fields user can see)
- Two-tier access control system: entitlements + views
- Entitlements determine API access, views determine data visibility

---

### 7. **Account Routing** (Banking Reference Data)

**Business Description**: 
Routing information associated with a bank account that enables external systems to identify and route transactions to the correct account. This includes information like IBAN, account numbers, routing numbers, and other identifiers used in payment processing.

**Location in Codebase** (Open Bank Project OBP-API): 
- Returned as part of core account information in the response
- Field name: account_routings (array/collection)
- Part of CoreAccountsHeldJsonV300 response structure

**Key Fields**:
- Routing scheme: Type of routing (e.g., IBAN, AccountNumber, RoutingNumber)
- Routing address: The actual routing value
- Account reference: Which account this routing belongs to

**Relationships to Other Entities**:
- Account Routings belong to one Bank Account
- A Bank Account can have multiple Account Routings (different routing schemes)

**Business Rules**:
- Account routing information is included in the core account response
- Multiple routing schemes can exist for a single account
- Routing information follows banking standards

**Notes**:
- Essential for payment processing and account identification
- Included in the account listing response for integration purposes
- Supports multiple routing schemes per account

---

## Entity Relationship Map

The Account Listing user story reveals the following key relationships:

1. **User ← → Bank Account**: A user can have access to multiple bank accounts, and a bank account can be accessible to multiple users (through different views)

2. **Bank → Bank Account**: A bank contains multiple bank accounts (one-to-many)

3. **Bank Account → Account Type**: Each bank account has one account type (many-to-one)

4. **Bank Account → Account Routing**: Each bank account has one or more account routings (one-to-many)

5. **User ← → View**: Users are granted specific views on accounts (many-to-many through view permissions)

6. **View → Bank Account**: Each view is associated with a specific bank account

7. **User → Entitlement**: Users are granted entitlements that control their overall access scope (one-to-many)

**Access Control Flow**:
- Entitlements determine if a user can make the API call (coarse-grained authorization)
- Views determine which specific accounts appear in results (fine-grained authorization)
- Only accounts where user has ≥1 view permission are included in the response
- Account type filters further refine the results (optional filtering)

**Data Flow**:
1. User makes API request with USER_ID and optional BANK_ID
2. System validates user authentication
3. System checks user entitlements (CanGetAccountsHeldAtOneBank or CanGetAccountsHeldAtAnyBank)
4. Connector retrieves accounts from core banking system
5. System filters accounts based on user's view permissions
6. System applies optional account type filtering (INCLUDE or EXCLUDE operation)
7. System formats response using JSONFactory300.createCoreAccountsByCoreAccountsJSON
8. Response includes: id, bank_id, label, number, account_routings for each account

---

## Business Domain Summary

The Account Listing user story covers the following business domains:

1. **Account Management**: Core functionality for retrieving and listing bank accounts with essential information
2. **Access Control & Security**: Multi-layered permission system using entitlements and views for granular access control
3. **Multi-tenancy**: Support for accounts across multiple banks with scope-based access
4. **Reference Data Management**: Account types as categorization/filtering dimension
5. **Banking Integration**: Account routing information for payment processing and external system integration

**Key Business Capabilities**:
- Single-bank account listing (GET /obp/v5.1.0/users/{USER_ID}/banks/{BANK_ID}/accounts-held)
- Cross-bank account listing (GET /obp/v5.1.0/users/{USER_ID}/accounts-held)
- Permission-based filtering (view-based access control)
- Account type filtering (INCLUDE/EXCLUDE operations with comma-separated values)
- Real-time account access with sub-2-second response time
- Pagination support for large account lists

**Technical Integration Points**:
- APIMethods510 (API endpoint handlers)
- AccountsHelper.filterWithAccountType (filtering logic)
- ViewNewStyle (permission management)
- JSONFactory300 (response formatting)
- Connector (core banking system integration)

---

## Questions & Uncertainties

1. **Account Ownership vs. Access**: The user story mentions "accounts held by user" - does this mean accounts owned by the user, or accounts the user has any access to? The business rule states "only accounts where user has at least one view permission" which suggests the latter, but this needs clarification for precise terminology.

2. **Pagination Details**: The user story mentions "system handles pagination for users with many accounts" but doesn't specify the pagination mechanism (page size, cursor-based vs. offset-based, default page size, maximum results per page, etc.). This is critical for implementation.

3. **Account Type Values**: What is the complete list of valid account type values? The user story mentions account type filtering but doesn't provide the full enumeration of valid types. Are these standardized or bank-specific?

4. **Default Filtering Behavior**: If no account type filters are specified, what is the default behavior? The business rule states "empty filter list returns all accounts regardless of type" but this should be explicitly confirmed.

5. **Performance Expectations**: The user story states "response time under 2 seconds for typical user account lists" - what defines a "typical" list? How many accounts? What is the expected maximum number of accounts a user might have?

6. **View Permission Hierarchy**: Are there different types of views with different permission levels? What's the minimal view required for an account to appear in the list? Can a user have multiple views on the same account?

7. **Entitlement Interaction**: The API endpoints specify "CanGetAccountsHeldAtOneBank OR CanGetAccountsHeldAtAnyBank" - how do these interact? If a user has both, which takes precedence? Or do they work together?

8. **Bank Connector Details**: The dependencies mention "Bank connector for retrieving actual account data" - is this a real-time call to external systems or is data cached/replicated locally? What is the latency impact?

9. **Account Routing Schemes**: What routing schemes are supported in the account_routings field? Are there standard schemes (IBAN, SWIFT, ACH, etc.) or is this extensible?

10. **Error Handling**: What specific error codes and messages are returned for invalid user ID, invalid bank ID, insufficient permissions, etc.? This is important for API consumer integration.

11. **Caching Strategy**: The implementation notes mention "short TTL caching should be implemented" - what is the recommended TTL? How is cache invalidation handled when account data changes?

12. **Audit Trail**: The implementation notes mention "audit trail should be maintained for all account access requests" - what specific information should be logged? Is this for compliance purposes?

---

## Additional Implementation Considerations

Based on the entity analysis, here are critical implementation points for the Go migration:

1. **Data Model Preservation**: All seven entities (User, Bank Account, Bank, View, Account Type, Entitlement, Account Routing) must be represented in the Go application with their complete field sets and relationships.

2. **Access Control Logic**: The two-tier permission system (entitlements + views) must be faithfully replicated in Go, as this is core to the system's security model:
   - Entitlement checking must happen before any data retrieval
   - View-based filtering must be applied to all account results
   - Both layers must be enforced for complete security

3. **API Endpoint Mapping**: The Scala endpoints must have exact equivalents in Go with identical behavior:
   - **Endpoint 1**: GET /obp/v5.1.0/users/{USER_ID}/banks/{BANK_ID}/accounts-held
     - Method: getAccountsHeldByUserAtBank
     - Entitlements: CanGetAccountsHeldAtOneBank OR CanGetAccountsHeldAtAnyBank
   - **Endpoint 2**: GET /obp/v5.1.0/users/{USER_ID}/accounts-held
     - Method: getAccountsHeldByUser
     - Entitlements: CanGetAccountsHeldAtAnyBank
   - URL patterns must match exactly
   - Query parameters: account_type_filter, account_type_filter_operation
   - Request/response formats must be identical (CoreAccountsHeldJsonV300)
   - Error codes and messages must match
   - Business rule enforcement must be consistent

4. **Filtering Logic Implementation**: The account type filtering system must support:
   - Comma-separated list of account types
   - INCLUDE operation (default): return only accounts matching the specified types
   - EXCLUDE operation: return all accounts except those matching the specified types
   - Empty filter list: return all accounts regardless of type
   - Validation: operation must be "INCLUDE" or "EXCLUDE"
   - Implementation reference: AccountsHelper.filterWithAccountType

5. **Performance Requirements**: The Go implementation must meet strict performance criteria:
   - Response time under 2 seconds for typical user account lists
   - Efficient pagination for users with many accounts
   - Short TTL caching for frequent requests
   - Optimized database queries for view permission checks
   - Efficient connector calls to core banking system

6. **Response Format**: The JSON response must exactly match CoreAccountsHeldJsonV300:
   - Array of core account objects
   - Each account includes: id, bank_id, label, number, account_routings
   - Account routings is an array/collection of routing information
   - Field names must match exactly (case-sensitive)

7. **Error Handling**: Implement comprehensive error handling for:
   - Invalid user ID (appropriate error message)
   - Invalid bank ID (appropriate error message)
   - Insufficient entitlements (authorization error)
   - No view permissions (empty result set, not error)
   - Invalid account type filter operation (validation error)
   - Connector failures (graceful degradation or error)

8. **Test Coverage**: Test cases should verify:
   - **Entitlement-based access**:
     - User with CanGetAccountsHeldAtOneBank can access single-bank endpoint
     - User with CanGetAccountsHeldAtAnyBank can access both endpoints
     - User without proper entitlements receives authorization error
   - **View-based filtering**:
     - Only accounts with at least one view permission are returned
     - Accounts without view permissions are excluded
     - Multiple views on same account work correctly
   - **Account type filtering**:
     - INCLUDE operation returns only matching types
     - EXCLUDE operation returns all except matching types
     - Comma-separated types work correctly
     - Empty filter returns all accounts
     - Invalid operation returns validation error
   - **Multi-bank vs. single-bank retrieval**:
     - Single-bank endpoint filters by bank ID
     - All-banks endpoint returns accounts across all banks
     - Invalid bank ID returns error
   - **Permission denial scenarios**:
     - Missing entitlements
     - No view permissions (returns empty list)
     - Invalid user ID
   - **Edge cases**:
     - User with no accounts (empty result)
     - User with many accounts (pagination)
     - Mixed permissions across different banks
     - Multiple account types
     - Multiple routing schemes per account

9. **Integration Points**: Ensure proper integration with:
   - Authentication system (user validation)
   - Authorization system (entitlement and view checks)
   - Core banking connector (account data retrieval)
   - JSON response formatter (CoreAccountsHeldJsonV300)
   - Pagination system (for large result sets)
   - Audit logging system (access tracking)

10. **Data Validation**: Implement validation for:
    - User ID format and existence
    - Bank ID format and existence (when provided)
    - Account type filter values (must match valid types)
    - Account type filter operation (must be INCLUDE or EXCLUDE)
    - Query parameter formats

---

## Source Documents

**Open Bank Project (OBP-API) Reference**:
- **Official Repository**: https://github.com/OpenBankProject/OBP-API.git
- **Description**: Open-source banking API platform that enables banks to provide secure, standardized API access
- **Technology**: Scala-based REST API
- **License**: AGPL v3
- **Documentation**: https://www.openbankproject.com/

**User Story Source**: 
- Repository: ashish-019-hash/aws-cardemo-ashwita
- Branch: devin/1762152678-copy-scala-prompt
- File: Playbooks/user_stories/obp_api_account_management_user_stories.md
- Section: Part 2: Detailed user stories - User Story 1: Account Listing (lines 74-147)
- Note: This user story documents the Account Listing functionality from the Open Bank Project OBP-API
- Last Updated: November 10, 2025 (updated version analyzed)

**Extraction Methodology**: 
- Based on entity extraction prompt methodology
- Systematic analysis of user story components
- Identification of entities, relationships, and business rules
- Focus on data model and business logic preservation for migration

**Key Classes/Services Referenced** (from OBP-API codebase):
- APIMethods510.getAccountsHeldByUserAtBank
- APIMethods510.getAccountsHeldByUser
- AccountsHelper.filterWithAccountType
- ViewNewStyle
- JSONFactory300.createCoreAccountsByCoreAccountsJSON
- Connector.getAccountsHeld
- Connector.getAccountsHeldByUser

---

## Summary

This entity extraction analysis identifies **seven core business entities** involved in the Account Listing user story:

1. **User** - The authenticated actor requesting account information
2. **Bank Account** - The primary entity being listed with core information
3. **Bank** - The financial institution holding accounts
4. **View/View Permission** - Fine-grained access control mechanism
5. **Account Type** - Classification and filtering dimension
6. **Entitlement** - Coarse-grained authorization control
7. **Account Routing** - Banking reference data for payment processing

The analysis reveals a sophisticated **two-tier access control system** (entitlements + views) that must be preserved in the Go migration, along with flexible **account type filtering** (INCLUDE/EXCLUDE operations) and support for both **single-bank and multi-bank** account retrieval scenarios.

The entity relationships, business rules, and data flows documented here provide the foundation for implementing the Account Listing functionality in Go while maintaining exact functional equivalence with the Scala-based OBP-API implementation.

---

This analysis extracts all business entities explicitly mentioned or implied in the Account Listing user story from the **Open Bank Project (OBP-API)**, following a systematic approach to identify entities, relationships, business rules, and implementation considerations. All classes, methods, and services referenced are from the official OBP-API repository at https://github.com/OpenBankProject/OBP-API.git.
