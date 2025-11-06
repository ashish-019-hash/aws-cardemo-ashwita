# Business Entity Extraction - Account Listing User Story

Applied the entity extraction prompt to the Account Listing user story from the OBP-API Account Management documentation.

---

## Entity Catalog - Account Listing User Story

Based on the Account Listing user story, the following business entities have been identified:

### 1. **User** (Banking Application User / API Consumer)

**Business Description**: 
A person or system that uses the banking application and has authenticated access to the API. This represents the actor who wants to view their bank accounts.

**Location in Codebase**: 
- Mentioned in technical context but not explicitly defined in the user story
- Referenced through authentication and authorization checks

**Key Fields**:
- User ID: Unique identifier for the user
- Authentication credentials: Validates user identity
- Entitlements: Permissions assigned to the user (canGetAccountsHeldAtOneBank, canGetAccountsHeldAtAnyBank)

**Relationships to Other Entities**:
- A User can have access to multiple Bank Accounts
- A User has one or more Views on each accessible Account
- A User has specific Entitlements that control their access scope

**Business Rules**:
- User must be authenticated to retrieve account lists
- Only accounts where user has at least one view permission are returned
- User must have appropriate entitlements for the requested scope (single bank vs. all banks)

**Notes**:
- The user story focuses on what the user can do rather than the user entity structure itself
- Authentication and authorization are prerequisites

---

### 2. **Bank Account** (Core Banking Entity)

**Business Description**: 
A financial account where money is stored and managed. This is the primary entity being listed in this user story. It represents a customer's relationship with a bank for holding and managing funds.

**Location in Codebase**: 
- Classes: APIMethods510.getAccountsHeldByUserAtBank, APIMethods510.getAccountsHeldByUser
- JSON Factory: JSONFactory300.createCoreAccountsByCoreAccountsJSON

**Key Fields**:
- Account ID: Unique identifier for the account
- Bank ID: Identifier of the bank where the account is held
- Label: Human-readable name or description of the account
- Account Type: Category of account (checking, savings, etc.)
- Currency: The currency in which the account operates (implied from other user stories)
- Status: Active/inactive state (implied)

**Relationships to Other Entities**:
- A Bank Account belongs to one Bank
- A Bank Account is associated with one or more Users (account holders and authorized users)
- A Bank Account has one or more Views that control access permissions
- A Bank Account has a specific Account Type

**Business Rules**:
- User can only see accounts they have view permission for
- Account must exist in the system
- Results respect user's entitlements
- Account type filtering is optional and supports multiple types

**Notes**:
- This is the central entity in the Account Listing user story
- The user story focuses on the "core" account information subset
- Full account details would include additional fields like balance, limits, etc.

---

### 3. **Bank** (Financial Institution)

**Business Description**: 
A financial institution that holds and manages customer accounts. The bank entity represents the organization providing banking services.

**Location in Codebase**: 
- Referenced in API methods: getAccountsHeldByUserAtBank
- Used as a filter parameter in account retrieval

**Key Fields**:
- Bank ID: Unique identifier for the bank
- Bank name: Name of the financial institution (implied)
- Status: Active/inactive (implied)

**Relationships to Other Entities**:
- A Bank has multiple Bank Accounts
- A Bank has multiple Branches (implied from broader context)
- A Bank may have multiple Users accessing their accounts

**Business Rules**:
- Bank ID must be valid if specified in the account listing request
- Users can retrieve accounts from a specific bank or across all banks depending on their entitlements

**Notes**:
- The user story allows filtering by bank (single bank) or retrieving across all banks
- The distinction between single-bank and multi-bank access is controlled by user entitlements

---

### 4. **View / View Permission** (Access Control Entity)

**Business Description**: 
A view represents a specific level of access to account information. It controls what fields and operations a user can perform on an account. Views implement granular access control beyond simple yes/no account access.

**Location in Codebase**: 
- Class: ViewNewStyle
- Related to permission checking for account access

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

**Notes**:
- Views are a key security/access control mechanism in OBP-API
- The Account Listing operation filters results based on view permissions
- This is a sophisticated access control model beyond simple ownership

---

### 5. **Account Type** (Reference Data)

**Business Description**: 
A classification or category that defines the nature and purpose of a bank account. Common types include checking accounts, savings accounts, credit accounts, etc.

**Location in Codebase**: 
- Referenced in query parameters for filtering
- Part of core account information returned

**Key Fields**:
- Type code: Unique identifier for the account type
- Type name: Descriptive name (CURRENT, SAVINGS, CREDIT, etc.)
- Type description: Detailed explanation of the account type

**Relationships to Other Entities**:
- A Bank Account has one Account Type
- Account Type is used as a filter criterion in account listing

**Business Rules**:
- Account type filtering is optional
- Multiple account types can be specified in a single request
- Account type values must match valid, predefined types
- Account type filter supports categories like checking, savings, etc.

**Notes**:
- Acts as reference/lookup data
- Used for categorization and filtering purposes
- The user story mentions this as an optional filter parameter

---

### 6. **Entitlement** (Authorization Entity)

**Business Description**: 
A specific permission or right granted to a user that controls what operations they can perform in the system. Entitlements are broader than view permissions and control access to entire categories of operations.

**Location in Codebase**: 
- Referenced in business rules
- Specific entitlements: canGetAccountsHeldAtOneBank, canGetAccountsHeldAtAnyBank

**Key Fields**:
- Entitlement ID: Unique identifier
- Entitlement name: Specific permission name
- User reference: Which user has this entitlement
- Scope: What the entitlement allows

**Relationships to Other Entities**:
- Entitlements are granted to Users
- Entitlements control access scope (single bank vs. all banks)
- Works in conjunction with View permissions for complete access control

**Business Rules**:
- Results must respect user's entitlements
- canGetAccountsHeldAtOneBank: Allows listing accounts at a specific bank
- canGetAccountsHeldAtAnyBank: Allows listing accounts across all banks
- User must have appropriate entitlements for the requested scope

**Notes**:
- Provides coarse-grained authorization (what APIs user can call)
- Complements the fine-grained View permissions (what fields user can see)
- Two-tier access control system: entitlements + views

---

## Entity Relationship Map

The Account Listing user story reveals the following key relationships:

1. **User ← → Bank Account**: A user can have access to multiple bank accounts, and a bank account can be accessible to multiple users (through different views)

2. **Bank → Bank Account**: A bank contains multiple bank accounts (one-to-many)

3. **Bank Account → Account Type**: Each bank account has one account type (many-to-one)

4. **User ← → View**: Users are granted specific views on accounts (many-to-many through view permissions)

5. **View → Bank Account**: Each view is associated with a specific bank account

6. **User → Entitlement**: Users are granted entitlements that control their overall access scope (one-to-many)

**Access Control Flow**:
- Entitlements determine if a user can make the API call (coarse-grained)
- Views determine which specific accounts appear in results (fine-grained)
- Only accounts where user has ≥1 view permission are included

---

## Business Domain Summary

The Account Listing user story covers the following business domains:

1. **Account Management**: Core functionality for retrieving and listing bank accounts
2. **Access Control & Security**: Multi-layered permission system using entitlements and views
3. **Multi-tenancy**: Support for accounts across multiple banks
4. **Reference Data Management**: Account types as categorization/filtering dimension

**Key Business Capabilities**:
- Single-bank account listing
- Cross-bank account listing
- Permission-based filtering
- Account type filtering
- Real-time account access

---

## Questions & Uncertainties

1. **Account Ownership vs. Access**: The user story mentions "accounts held by user" - does this mean accounts owned by the user, or accounts the user has any access to? The business rule states "only accounts where user has at least one view permission" which suggests the latter, but this needs clarification.

2. **Pagination Details**: The user story mentions "system handles pagination for users with many accounts" but doesn't specify the pagination mechanism (page size, cursor-based vs. offset-based, etc.)

3. **Account Type Values**: What is the complete list of valid account type values? The user story mentions "checking, savings" as examples but doesn't provide the full enumeration.

4. **Default Filtering**: If no filters are specified, what is the default behavior? All account types? All accessible accounts?

5. **Performance Expectations**: The user story states "response time under 2 seconds for typical user account lists" - what defines a "typical" list? How many accounts?

6. **View Permission Hierarchy**: Are there different types of views with different permission levels? What's the minimal view required for an account to appear in the list?

7. **Entitlement Interaction**: If a user has both canGetAccountsHeldAtOneBank and canGetAccountsHeldAtAnyBank, which takes precedence? Or do they work together?

8. **Bank Connector Details**: The dependencies mention "Bank connector for retrieving actual account data" - is this a real-time call to external systems or is data cached/replicated locally?

---

## Additional Implementation Considerations

Based on the entity analysis, here are critical implementation points for the Go migration:

1. **Data Model Preservation**: All six entities (User, Bank Account, Bank, View, Account Type, Entitlement) must be represented in the Go application with their complete field sets and relationships.

2. **Access Control Logic**: The two-tier permission system (entitlements + views) must be faithfully replicated in Go, as this is core to the system's security model.

3. **API Endpoint Mapping**: The Scala endpoints (getAccountsHeldByUserAtBank, getAccountsHeldByUser) must have exact equivalents in Go with identical:
   - URL patterns
   - Query parameters
   - Request/response formats
   - Error codes
   - Business rule enforcement

4. **Performance Requirements**: The Go implementation must meet the 2-second response time SLA and handle pagination efficiently.

5. **Test Coverage**: Test cases should verify:
   - Entitlement-based filtering
   - View-based filtering
   - Account type filtering
   - Multi-bank vs. single-bank retrieval
   - Permission denial scenarios
   - Edge cases (no accounts, many accounts, mixed permissions)

---

## Source Documents

**User Story Source**: 
- Repository: ashish-019-hash/aws-cardemo-ashwita
- Branch: devin/1762152678-copy-scala-prompt
- File: Playbooks/user_stories/obp_api_account_management_user_stories.md
- Section: Part 2: Detailed user stories - User Story 1: Account Listing (lines 72-120)

**Extraction Methodology**: 
- Repository: ashish-019-hash/obp-api
- Branch: devin/1760325583-business-entity-documentation
- File: Phase-01-Playbooks/entity_extraction_prompt.md

---

This analysis extracts all business entities explicitly mentioned or implied in the Account Listing user story, following the systematic approach outlined in the entity extraction prompt.
