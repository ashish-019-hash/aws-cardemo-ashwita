# User Stories for OBP-API Account Management

**System:** Open Bank Project API (OBP-API)  
**Focus Area:** Account Management Capabilities  
**API Versions Analyzed:** v5.0.0 (stable), v5.1.0 (latest stable)  
**Date:** November 2024  
**Methodology:** Two-Phase User Story Extraction from Scala Codebase

---

## Part 1: Account Management Capability Inventory

### Account Lifecycle Management

1. **Create Bank Account**
   - Classes/Services: APIMethods500.createAccount, BankAccountCreation, AccountAttributeX
   - Type: REST API (PUT endpoint)
   - Purpose: Enable authenticated users to create new bank accounts with proper validation, routing configuration, and initial attributes
   - Frequency: On-demand via API call

### Account Discovery & Retrieval

2. **Get Accounts Held by User at Specific Bank**
   - Classes/Services: APIMethods510.getAccountsHeldByUserAtBank
   - Type: REST API (GET endpoint)
   - Purpose: Retrieve all bank accounts that a specific user holds at a particular bank with optional filtering by account type and currency
   - Frequency: On-demand via API call

3. **Get All Accounts Held by User**
   - Classes/Services: APIMethods510.getAccountsHeldByUser
   - Type: REST API (GET endpoint)
   - Purpose: Retrieve all bank accounts that a user holds across all banks with optional filtering capabilities
   - Frequency: On-demand via API call

### Account Views & Permissions Management

4. **Get Available Views for Bank Account**
   - Classes/Services: APIMethods500.getViewsForBankAccount, Views.views.vend
   - Type: REST API (GET endpoint)
   - Purpose: List all available views (permission sets) for a specific bank account that determine what account data and operations are accessible
   - Frequency: On-demand via API call

5. **Create System View**
   - Classes/Services: APIMethods500.createSystemView, ViewNewStyle
   - Type: REST API (POST endpoint)
   - Purpose: Create system-wide view configurations that define standardized permission sets across the banking system
   - Frequency: On-demand via API call

6. **Update System View**
   - Classes/Services: APIMethods500.updateSystemView, ViewNewStyle
   - Type: REST API (PUT endpoint)
   - Purpose: Modify existing system view configurations to adjust permission sets
   - Frequency: On-demand via API call

7. **Delete System View**
   - Classes/Services: APIMethods500.deleteSystemView, ViewNewStyle
   - Type: REST API (DELETE endpoint)
   - Purpose: Remove system view configurations that are no longer needed
   - Frequency: On-demand via API call

8. **Get System View**
   - Classes/Services: APIMethods500.getSystemView, ViewNewStyle
   - Type: REST API (GET endpoint)
   - Purpose: Retrieve details of a specific system view configuration
   - Frequency: On-demand via API call

9. **Get System Views IDs**
   - Classes/Services: APIMethods500.getSystemViewsIds, ViewNewStyle
   - Type: REST API (GET endpoint)
   - Purpose: List all available system view identifiers for reference
   - Frequency: On-demand via API call

10. **Create Custom View**
    - Classes/Services: APIMethods510.createCustomView
    - Type: REST API (POST endpoint)
    - Purpose: Create account-specific custom views with tailored permission sets
    - Frequency: On-demand via API call

11. **Update Custom View**
    - Classes/Services: APIMethods510.updateCustomView
    - Type: REST API (PUT endpoint)
    - Purpose: Modify existing custom view configurations for specific accounts
    - Frequency: On-demand via API call

12. **Get Custom View**
    - Classes/Services: APIMethods510.getCustomView
    - Type: REST API (GET endpoint)
    - Purpose: Retrieve details of a specific custom view configuration
    - Frequency: On-demand via API call

### Account Access Control

13. **Get Account Access by User ID**
    - Classes/Services: APIMethods510.getAccountAccessByUserId, Views.views.vend.privateViewsUserCanAccess
    - Type: REST API (GET endpoint)
    - Purpose: List all accounts and views that a specific user has permission to access
    - Frequency: On-demand via API call

14. **Grant User Access to View**
    - Classes/Services: APIMethods510.grantUserAccessToViewById
    - Type: REST API (POST endpoint)
    - Purpose: Grant a user permission to access a specific view of a bank account
    - Frequency: On-demand via API call

15. **Revoke User Access to View**
    - Classes/Services: APIMethods510.revokeUserAccessToViewById
    - Type: REST API (DELETE endpoint)
    - Purpose: Remove a user's permission to access a specific view of a bank account
    - Frequency: On-demand via API call

16. **Create User with Account Access**
    - Classes/Services: APIMethods510.createUserWithAccountAccessById
    - Type: REST API (POST endpoint)
    - Purpose: Create a new user and immediately grant them access to specific account views
    - Frequency: On-demand via API call

### Account Balance Inquiry

17. **Get Bank Account Balances for Specific Account**
    - Classes/Services: APIMethods510.getBankAccountBalances, BalanceNewStyle
    - Type: REST API (GET endpoint)
    - Purpose: Retrieve balance information for a specific account through a specific view with permission validation
    - Frequency: On-demand via API call

18. **Get All Account Balances at Bank**
    - Classes/Services: APIMethods510.getBankAccountsBalances, BalanceNewStyle
    - Type: REST API (GET endpoint)
    - Purpose: Retrieve balances for all accounts the user can access at a specific bank
    - Frequency: On-demand via API call

19. **Get Account Balances Through View**
    - Classes/Services: APIMethods510.getBankAccountsBalancesThroughView, BalanceNewStyle
    - Type: REST API (GET endpoint)
    - Purpose: Retrieve account balances filtered by a specific view configuration at a bank
    - Frequency: On-demand via API call

### Customer-Account Relationship Management

20. **Create Customer Account Link**
    - Classes/Services: APIMethods500.createCustomerAccountLink, Connector.connector.vend
    - Type: REST API (POST endpoint)
    - Purpose: Establish a formal relationship between a customer and a bank account with a defined relationship type
    - Frequency: On-demand via API call

21. **Get Customer Account Links by Customer ID**
    - Classes/Services: APIMethods500.getCustomerAccountLinksByCustomerId
    - Type: REST API (GET endpoint)
    - Purpose: Retrieve all account links associated with a specific customer
    - Frequency: On-demand via API call

22. **Get Customer Account Links by Account ID**
    - Classes/Services: APIMethods500.getCustomerAccountLinksByBankIdAccountId
    - Type: REST API (GET endpoint)
    - Purpose: Retrieve all customer links associated with a specific bank account
    - Frequency: On-demand via API call

23. **Get Customer Account Link by ID**
    - Classes/Services: APIMethods500.getCustomerAccountLinkById
    - Type: REST API (GET endpoint)
    - Purpose: Retrieve details of a specific customer-account link relationship
    - Frequency: On-demand via API call

24. **Update Customer Account Link**
    - Classes/Services: APIMethods500.updateCustomerAccountLinkById
    - Type: REST API (PUT endpoint)
    - Purpose: Modify the relationship type between a customer and an account
    - Frequency: On-demand via API call

25. **Delete Customer Account Link**
    - Classes/Services: APIMethods500.deleteCustomerAccountLinkById
    - Type: REST API (DELETE endpoint)
    - Purpose: Remove the formal relationship between a customer and a bank account
    - Frequency: On-demand via API call

### Account Attributes Management

26. **Create Account Attributes**
    - Classes/Services: APIMethods500.createAccount (calls createAccountAttributes), AccountAttributeX
    - Type: REST API (automatic during account creation)
    - Purpose: Automatically generate account attributes based on product configuration during account creation
    - Frequency: Automatic during account creation process

### Account Integrity & Validation

27. **Account Access Unique Index Check**
    - Classes/Services: APIMethods510.accountAccessUniqueIndexCheck
    - Type: REST API (GET endpoint)
    - Purpose: Validate system integrity by checking for duplicate system view assignments that could cause access conflicts
    - Frequency: On-demand for system health checks

28. **Account Currency Check**
    - Classes/Services: APIMethods510.accountCurrencyCheck
    - Type: REST API (GET endpoint)
    - Purpose: Validate that all accounts have proper currency codes assigned
    - Frequency: On-demand for system health checks

29. **Orphaned Account Check**
    - Classes/Services: APIMethods510.orphanedAccountCheck
    - Type: REST API (GET endpoint)
    - Purpose: Identify accounts that exist without proper customer or user associations
    - Frequency: On-demand for system health checks

30. **Get Currencies at Bank**
    - Classes/Services: APIMethods510.getCurrenciesAtBank
    - Type: REST API (GET endpoint)
    - Purpose: List all currencies supported by accounts at a specific bank
    - Frequency: On-demand via API call

---

## Part 2: Detailed User Stories

### Priority: High (Core Banking Functions)

## User Story 1: Create Bank Account

### Story Overview
**As a** bank employee or authorized user  
**I want to** create a new bank account for a customer with proper validation and configuration  
**So that** customers can start using banking services with a properly configured account

### Acceptance Criteria
1. User must be authenticated before creating an account
2. User can create account for themselves OR for another user if they have the canCreateAccount role
3. Account ID must be provided and follow proper format (URL-safe characters)
4. Bank ID must be valid and exist in the system
5. Account type (product code) must be specified
6. Account label must be provided
7. Currency must be a valid ISO 4217 currency code
8. Initial balance must be zero (cannot create accounts with pre-loaded balances)
9. Account routing information must not duplicate existing routings
10. Account routing scheme and address must be provided
11. Account holder relationship is automatically established for the requesting user or specified user
12. Account attributes are automatically created based on product configuration
13. System returns HTTP 201 with account details on successful creation
14. System returns appropriate error messages for validation failures

### Technical Context
- **Classes/Services Involved**: 
  - APIMethods500.createAccount (main endpoint handler)
  - BankAccountCreation.setAccountHolderAndRefreshUserAccountAccess (account holder setup)
  - AccountAttributeX.createAccountAttributes (attribute generation)
  - Connector.connector.vend (backend integration)
- **Input Data**: 
  - Request body: PostPutProductJsonV500 containing user_id, account_id, account_type, label, balance (Amount with currency and value), branch_id, account_routings (list of scheme and address pairs)
  - Path parameters: BANK_ID, ACCOUNT_ID
- **Output Data**: 
  - Response body: ModeratedCoreAccountJsonV500 containing complete account details including ID, label, balance, account routings, and account attributes
  - HTTP Status: 201 Created on success
- **Processing Type**: REST API - Synchronous transaction processing

### Business Rules (from code)
1. Initial balance value must equal "0" - accounts cannot be created with pre-loaded funds
2. Currency code must be valid ISO 4217 format
3. Account routing combinations (scheme + address) must be unique across all accounts
4. User creating account must either own the account OR have canCreateAccount entitlement
5. Product (account_type) must exist and have associated product attributes
6. Account attributes are derived from product attributes automatically
7. Account holder is automatically assigned to the user (requestor or specified user_id)
8. Branch ID must be provided in the request
9. Multiple account routing schemes can be assigned to a single account

### Data Validations (if applicable)
- **Account ID validation**: Must be URL-safe, non-empty string
- **Bank ID validation**: Must exist in the system
- **Currency validation**: Must be valid ISO 4217 code (e.g., USD, EUR, GBP)
- **Balance validation**: Amount.value must equal "0"
- **User validation**: user_id must exist if specified, otherwise defaults to authenticated user
- **Product validation**: account_type must correspond to existing product with product code
- **Routing validation**: Each routing must have both scheme and address; combination must be unique
- **Permission validation**: User must have owner access or canCreateAccount role

### Dependencies
- **Upstream**: 
  - User authentication service (must have valid authenticated user)
  - Product catalog (must have valid product with product_code matching account_type)
  - Bank registry (must have valid bank)
  - User registry (if creating account for another user)
- **Downstream**: 
  - Account holder service (establishes account ownership)
  - Account attribute service (creates attributes based on product)
  - View permissions service (sets up default views and permissions)
- **External Systems**: 
  - Backend connector for account persistence
  - Possibly external core banking system through connector

### Notes for Implementation
- Account creation is a critical operation that requires multiple validations
- The initial balance restriction (must be zero) is a security measure to prevent unauthorized fund loading
- Account attributes are automatically populated from product configuration - developers need to ensure product attributes are properly configured
- Account routing is important for payment network integration (e.g., IBAN, account number schemes)
- Consider transaction management to ensure all related entities (account, attributes, holder, views) are created atomically
- **Needs SME Input**: What are the valid account routing schemes and their validation rules? Code mentions schemes but doesn't specify the complete list
- **Needs SME Input**: What are the default views and permissions created for new accounts?
- **Needs SME Input**: Are there any account type-specific validation rules beyond what's visible in the API layer?

---

## User Story 2: Retrieve User's Accounts at Specific Bank

### Story Overview
**As a** bank customer or employee  
**I want to** view all accounts that a specific user holds at a particular bank  
**So that** I can review account holdings and manage banking relationships

### Acceptance Criteria
1. User must be authenticated to retrieve account information
2. Must have canGetAccountsHeldAtOneBank or canGetAccountsHeldAtAnyBank entitlement
3. Target user ID must be valid and exist in the system
4. Bank ID must be valid and exist in the system
5. System returns list of accounts held by the specified user at the specified bank
6. Optional filtering by account_type (product code) is supported
7. Optional filtering by currency is supported
8. System returns HTTP 200 with account list on success
9. Account details include account ID, label, bank ID, account type, and other core attributes

### Technical Context
- **Classes/Services Involved**:
  - APIMethods510.getAccountsHeldByUserAtBank (main endpoint handler)
  - NewStyle.function.getUserByUserId (user lookup)
  - NewStyle.function.getAccountsHeld (account retrieval)
  - NewStyle.function.getBankAccountsHeldFuture (account details retrieval)
- **Input Data**:
  - Path parameters: USER_ID, BANK_ID
  - Query parameters: account_type (optional), currency (optional)
- **Output Data**:
  - Response body: CoreAccountsHeldJsonV400 (list of ModeratedCoreAccountJsonV400)
  - HTTP Status: 200 OK
- **Processing Type**: REST API - Synchronous query

### Business Rules (from code)
1. User must have appropriate entitlement to view accounts at bank
2. Only accounts actually held by the specified user are returned
3. Filtering by account type and currency is applied if parameters are provided
4. Account list includes full account details for accounts user has access to

### Data Validations (if applicable)
- **User ID validation**: Must be valid user in the system
- **Bank ID validation**: Must be valid bank in the system
- **Permission validation**: Must have canGetAccountsHeldAtOneBank or canGetAccountsHeldAtAnyBank
- **Account type filter validation**: If provided, must match valid product code
- **Currency filter validation**: If provided, must be valid currency code

### Dependencies
- **Upstream**:
  - User authentication service
  - User registry (for user_id lookup)
  - Bank registry (for bank_id validation)
  - Entitlement service (for permission checking)
- **Downstream**:
  - Account registry (for account retrieval)
- **External Systems**:
  - Backend connector for account data

### Notes for Implementation
- This endpoint supports filtering which improves usability for customers with many accounts
- Consider pagination for users with large numbers of accounts
- **Needs SME Input**: What is the complete set of account types that can be filtered on?
- **Needs SME Input**: Are there any data privacy rules that limit which users can view other users' accounts?

---

## User Story 3: Retrieve All User's Accounts Across Banks

### Story Overview
**As a** bank customer or system administrator  
**I want to** view all accounts that a user holds across all banks in the system  
**So that** I can get a complete picture of the user's banking relationships

### Acceptance Criteria
1. User must be authenticated to retrieve account information
2. Must have canGetAccountsHeldAtAnyBank entitlement
3. Target user ID must be valid and exist in the system
4. System returns list of all accounts held by user across all banks
5. Optional filtering by account_type and currency is supported
6. System returns HTTP 200 with account list on success
7. Account details include bank identification for each account

### Technical Context
- **Classes/Services Involved**:
  - APIMethods510.getAccountsHeldByUser (main endpoint handler)
  - NewStyle.function.getUserByUserId (user lookup)
  - NewStyle.function.getAccountsHeldByUser (cross-bank account retrieval)
  - NewStyle.function.getBankAccountsHeldFuture (account details retrieval)
- **Input Data**:
  - Path parameters: USER_ID
  - Query parameters: account_type (optional), currency (optional)
- **Output Data**:
  - Response body: CoreAccountsHeldJsonV400 (list of accounts across banks)
  - HTTP Status: 200 OK
- **Processing Type**: REST API - Synchronous query

### Business Rules (from code)
1. User must have elevated entitlement (canGetAccountsHeldAtAnyBank) to view cross-bank accounts
2. Returns accounts from all banks where user has holdings
3. Filtering applies across all banks

### Data Validations (if applicable)
- **User ID validation**: Must be valid user in the system
- **Permission validation**: Must have canGetAccountsHeldAtAnyBank entitlement
- **Filter validations**: Same as User Story 2

### Dependencies
- **Upstream**:
  - User authentication service
  - User registry
  - Entitlement service
- **Downstream**:
  - Multi-bank account registry
- **External Systems**:
  - Backend connector with cross-bank query capability

### Notes for Implementation
- This is a higher-privilege operation than bank-specific account retrieval
- Performance considerations for users with accounts at many banks
- **Needs SME Input**: Are there regulatory implications for cross-bank account viewing?
- **Needs SME Input**: Should there be audit logging for cross-bank account queries?

---

## User Story 4: View Account Balance

### Story Overview
**As a** bank customer  
**I want to** view the balance of my bank account  
**So that** I can monitor my available funds and financial position

### Acceptance Criteria
1. User must be authenticated to view account balances
2. User must have access to a view with CAN_SEE_BANK_ACCOUNT_BALANCE permission
3. Bank ID and Account ID must be valid
4. View ID must be valid and accessible to the user
5. System returns balance information including available balance and currency
6. System returns HTTP 200 with balance details on success
7. Balance information is view-dependent (different views may show different balance aspects)

### Technical Context
- **Classes/Services Involved**:
  - APIMethods510.getBankAccountBalances (main endpoint handler)
  - ViewNewStyle.checkViewAccessAndReturnView (permission validation)
  - BalanceNewStyle.getBankAccountBalances (balance retrieval)
- **Input Data**:
  - Path parameters: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Output Data**:
  - Response body: AccountBalancesV400Json containing balance details
  - HTTP Status: 200 OK
- **Processing Type**: REST API - Synchronous query

### Business Rules (from code)
1. Balance visibility is controlled by view permissions
2. User must have CAN_SEE_BANK_ACCOUNT_BALANCE permission on the specified view
3. Balance information is real-time from the backend system
4. Different views may expose different balance types (e.g., available, current, pending)

### Data Validations (if applicable)
- **Bank ID validation**: Must be valid bank
- **Account ID validation**: Must be valid account at the specified bank
- **View ID validation**: Must be valid view for the account
- **Permission validation**: View must have CAN_SEE_BANK_ACCOUNT_BALANCE permission
- **User access validation**: User must have access to the specified view

### Dependencies
- **Upstream**:
  - User authentication service
  - View permissions service
  - Account registry
- **Downstream**:
  - Balance service
- **External Systems**:
  - Core banking system (via connector) for real-time balance data

### Notes for Implementation
- Balance queries are frequent operations - consider caching strategies
- Balance information is sensitive - ensure proper audit logging
- View-based balance access enables fine-grained control (e.g., showing available vs. current balance)
- **Needs SME Input**: What are the different balance types that may be returned?
- **Needs SME Input**: Is there a difference between "available balance" and "current balance"?

---

## User Story 5: View All Account Balances at Bank

### Story Overview
**As a** bank customer  
**I want to** view balances for all my accounts at a specific bank  
**So that** I can get an overview of my financial position at that bank

### Acceptance Criteria
1. User must be authenticated
2. System retrieves all accounts user has access to at the specified bank
3. For each accessible account, balance information is returned
4. System returns HTTP 200 with list of account balances
5. User only sees balances for accounts they have permission to access

### Technical Context
- **Classes/Services Involved**:
  - APIMethods510.getBankAccountsBalances (main endpoint handler)
  - BalanceNewStyle.getAccountAccessAtBank (account access retrieval)
  - BalanceNewStyle.getBankAccountsBalances (bulk balance retrieval)
- **Input Data**:
  - Path parameters: BANK_ID
- **Output Data**:
  - Response body: AccountBalancesV400Json (list of balances)
  - HTTP Status: 200 OK
- **Processing Type**: REST API - Synchronous query

### Business Rules (from code)
1. System automatically determines which accounts user can access at the bank
2. Balances are returned only for accessible accounts
3. Efficient bulk retrieval for multiple accounts

### Data Validations (if applicable)
- **Bank ID validation**: Must be valid bank
- **Authentication validation**: User must be logged in
- **Access validation**: Automatically filters to accessible accounts only

### Dependencies
- **Upstream**:
  - User authentication service
  - View permissions service
- **Downstream**:
  - Balance service with bulk query capability
- **External Systems**:
  - Core banking system for balance data

### Notes for Implementation
- Useful for dashboard/summary views
- Performance optimization needed for users with many accounts
- **Needs SME Input**: Should there be a limit on number of accounts returned?

---

### Priority: Medium (Account Management & Relationships)

## User Story 6: Manage Account Views

### Story Overview
**As a** bank administrator  
**I want to** create, update, and delete account views  
**So that** I can control what account information is accessible to different users

### Acceptance Criteria
1. User must be authenticated and have appropriate entitlements
2. For system views: requires canCreateSystemView, canUpdateSystemView, or canDeleteSystemView
3. View definitions include view ID, name, description, and permission sets
4. System views apply across all accounts; custom views are account-specific
5. Permission sets define what data fields and operations are accessible
6. System returns HTTP 200/201 with view details on success
7. Deleting a view removes access for all users using that view

### Technical Context
- **Classes/Services Involved**:
  - APIMethods500.createSystemView, updateSystemView, deleteSystemView, getSystemView
  - APIMethods510.createCustomView, updateCustomView, getCustomView
  - ViewNewStyle (view management service)
- **Input Data**:
  - Request body: View configuration JSON with permissions, metadata
  - Path parameters: VIEW_ID (for updates/deletes)
- **Output Data**:
  - Response body: View configuration details
  - HTTP Status: 200 OK (update/delete), 201 Created (create)
- **Processing Type**: REST API - Synchronous configuration change

### Business Rules (from code)
1. System views are reusable across accounts
2. Custom views are specific to individual accounts
3. Views contain permission sets that control data visibility and operations
4. View names and IDs must be unique within their scope
5. Deleting a view affects all users who have access through that view

### Data Validations (if applicable)
- **View ID validation**: Must be unique and follow naming conventions
- **Permission validation**: User must have appropriate create/update/delete entitlements
- **View configuration validation**: Permission sets must be valid
- **Dependency validation**: Cannot delete views that are currently in use (needs confirmation)

### Dependencies
- **Upstream**:
  - User authentication and entitlement service
- **Downstream**:
  - View permissions service
  - Account access service (affected by view changes)
- **External Systems**:
  - None directly

### Notes for Implementation
- View management is critical for access control architecture
- Changes to views affect multiple users - consider notification mechanisms
- **Needs SME Input**: What is the complete set of available permissions for views?
- **Needs SME Input**: What are the standard system views that should be created?
- **Needs SME Input**: Can views be deleted if users currently have access through them?

---

## User Story 7: Get Available Views for Account

### Story Overview
**As a** bank customer or employee  
**I want to** see what views are available for a specific account  
**So that** I can understand my access levels and request additional permissions if needed

### Acceptance Criteria
1. User must be authenticated
2. User must have CAN_SEE_AVAILABLE_VIEWS_FOR_BANK_ACCOUNT permission on at least one view
3. System returns all views available for the specified account
4. Response includes view metadata (ID, name, description)
5. System returns HTTP 200 with list of views

### Technical Context
- **Classes/Services Involved**:
  - APIMethods500.getViewsForBankAccount (main endpoint handler)
  - Views.views.vend.availableViewsForAccount (view retrieval)
  - Permission checking logic
- **Input Data**:
  - Path parameters: BANK_ID, ACCOUNT_ID
- **Output Data**:
  - Response body: ViewsJsonV500 (list of available views)
  - HTTP Status: 200 OK
- **Processing Type**: REST API - Synchronous query

### Business Rules (from code)
1. User must have permission to see available views (CAN_SEE_AVAILABLE_VIEWS_FOR_BANK_ACCOUNT)
2. Returns all views configured for the account, not just those user has access to
3. Helps users understand the access control model

### Data Validations (if applicable)
- **Account validation**: BANK_ID and ACCOUNT_ID must be valid
- **Permission validation**: User must have CAN_SEE_AVAILABLE_VIEWS_FOR_BANK_ACCOUNT on any view

### Dependencies
- **Upstream**:
  - User authentication service
  - View configuration service
- **Downstream**:
  - None
- **External Systems**:
  - None

### Notes for Implementation
- Useful for transparency in access control
- Helps users understand why they can or cannot access certain data
- **Needs SME Input**: Should the response indicate which views the user actually has access to?

---

## User Story 8: Link Customer to Account

### Story Overview
**As a** bank employee  
**I want to** create a formal link between a customer and a bank account  
**So that** the customer's relationship with the account is properly documented with relationship type

### Acceptance Criteria
1. User must be authenticated and have canCreateCustomerAccountLink permission
2. Customer ID must be valid and belong to the specified bank
3. Account ID must be valid and exist at the specified bank
4. Relationship type must be specified (e.g., owner, authorized_user, beneficiary)
5. Customer-account link combination must be unique (no duplicate links)
6. System validates customer bank matches account bank
7. System returns HTTP 201 with link details on success

### Technical Context
- **Classes/Services Involved**:
  - APIMethods500.createCustomerAccountLink (main endpoint handler)
  - NewStyle.function.getCustomerByCustomerId (customer validation)
  - NewStyle.function.getBankAccount (account validation)
  - Connector.connector.vend.getCustomerAccountLink (duplicate check)
  - NewStyle.function.createCustomerAccountLink (link creation)
- **Input Data**:
  - Request body: CreateCustomerAccountLinkJson containing customer_id, bank_id, account_id, relationship_type
  - Path parameters: BANK_ID
- **Output Data**:
  - Response body: CustomerAccountLinkJson with link details
  - HTTP Status: 201 Created
- **Processing Type**: REST API - Synchronous transaction

### Business Rules (from code)
1. Customer must belong to the same bank as the account
2. Each customer-account combination can only have one link (no duplicates)
3. Relationship type must be specified to clarify the nature of the link
4. Both customer and account must exist before link can be created

### Data Validations (if applicable)
- **Customer ID validation**: Must exist and belong to specified bank
- **Account ID validation**: Must exist at specified bank
- **Bank matching validation**: Customer's bank must match account's bank
- **Duplicate validation**: Customer-account link must not already exist
- **Permission validation**: User must have canCreateCustomerAccountLink entitlement

### Dependencies
- **Upstream**:
  - User authentication and entitlement service
  - Customer registry
  - Account registry
- **Downstream**:
  - Customer-account relationship service
- **External Systems**:
  - Backend connector for persistence

### Notes for Implementation
- Critical for establishing formal customer-account relationships
- Relationship type is important for regulatory and reporting purposes
- **Needs SME Input**: What are the valid relationship_type values? (e.g., owner, joint_owner, authorized_user, beneficiary, etc.)
- **Needs SME Input**: Are there business rules about which relationship types require additional validation or documentation?

---

## User Story 9: Manage Customer-Account Links

### Story Overview
**As a** bank employee  
**I want to** view, update, and delete customer-account links  
**So that** I can maintain accurate customer-account relationships as they change over time

### Acceptance Criteria
1. User must be authenticated with appropriate permissions
2. **Get by Customer ID**: Returns all account links for a customer
3. **Get by Account ID**: Returns all customer links for an account
4. **Get by Link ID**: Returns specific link details
5. **Update**: Allows changing relationship_type of existing link
6. **Delete**: Removes customer-account link relationship
7. System validates all IDs before operations
8. System returns appropriate HTTP status (200 for get/update, 204 for delete)

### Technical Context
- **Classes/Services Involved**:
  - APIMethods500.getCustomerAccountLinksByCustomerId
  - APIMethods500.getCustomerAccountLinksByBankIdAccountId
  - APIMethods500.getCustomerAccountLinkById
  - APIMethods500.updateCustomerAccountLinkById
  - APIMethods500.deleteCustomerAccountLinkById
  - NewStyle.function for all operations
- **Input Data**:
  - Path parameters: BANK_ID, CUSTOMER_ID, ACCOUNT_ID, or CUSTOMER_ACCOUNT_LINK_ID
  - Request body (update): UpdateCustomerAccountLinkJson with new relationship_type
- **Output Data**:
  - Response body: CustomerAccountLinkJson or CustomerAccountLinksJson
  - HTTP Status: 200 OK (get/update), 204 No Content (delete)
- **Processing Type**: REST API - Synchronous operations

### Business Rules (from code)
1. Links can be retrieved by customer, account, or link ID for flexibility
2. Only relationship_type can be updated on existing links
3. Deleting a link removes the formal relationship but doesn't delete customer or account
4. User must have appropriate permissions for each operation

### Data Validations (if applicable)
- **ID validations**: All IDs (customer, account, link) must be valid
- **Permission validations**: Different operations require different entitlements
- **Link existence validation**: Link must exist before update/delete operations

### Dependencies
- **Upstream**:
  - User authentication and entitlement service
  - Customer and account registries
- **Downstream**:
  - Customer-account relationship service
- **External Systems**:
  - Backend connector

### Notes for Implementation
- Multiple retrieval methods provide flexibility for different use cases
- Consider audit logging for link deletions
- **Needs SME Input**: What should happen to account access when customer-account link is deleted?
- **Needs SME Input**: Are there restrictions on deleting links (e.g., primary account owner cannot be removed)?

---

## User Story 10: Manage User Access to Account Views

### Story Overview
**As a** bank administrator  
**I want to** grant or revoke user access to specific account views  
**So that** I can control who can access what information about each account

### Acceptance Criteria
1. User must be authenticated with appropriate permissions
2. **Grant Access**: Assigns view access to a user for an account
3. **Revoke Access**: Removes view access from a user for an account
4. Target user and view must exist and be valid
5. System validates account exists before granting access
6. System returns HTTP 200 on successful grant, 204 on successful revoke
7. Access changes take effect immediately

### Technical Context
- **Classes/Services Involved**:
  - APIMethods510.grantUserAccessToViewById (grant operation)
  - APIMethods510.revokeUserAccessToViewById (revoke operation)
- **Input Data**:
  - Path parameters: BANK_ID, ACCOUNT_ID, VIEW_ID, USER_ID (or provider/username)
- **Output Data**:
  - Response body: View access confirmation
  - HTTP Status: 200 OK (grant), 204 No Content (revoke)
- **Processing Type**: REST API - Synchronous access control change

### Business Rules (from code)
1. Only authorized administrators can grant or revoke access
2. Access is view-specific - granting access to one view doesn't affect others
3. Revoking access immediately removes user's ability to use that view
4. User can have access to multiple views for the same account

### Data Validations (if applicable)
- **User validation**: Target user must exist
- **View validation**: View must exist and be valid for the account
- **Account validation**: Account must exist
- **Permission validation**: Administrator must have appropriate entitlements

### Dependencies
- **Upstream**:
  - User authentication and entitlement service
  - User registry
  - View configuration service
  - Account registry
- **Downstream**:
  - Access control service
- **External Systems**:
  - None directly

### Notes for Implementation
- Critical for dynamic access control management
- Consider notification to users when access is granted or revoked
- **Needs SME Input**: Should there be an audit trail of access grants and revocations?
- **Needs SME Input**: Can access be revoked from account owners, or are there protected access rights?

---

## User Story 11: Get User's Account Access

### Story Overview
**As a** system administrator or compliance officer  
**I want to** view all accounts and views that a specific user has access to  
**So that** I can audit access rights and ensure proper access control

### Acceptance Criteria
1. User must be authenticated with canSeeAccountAccessForAnyUser permission
2. Target user ID must be valid
3. System returns complete list of accounts user can access
4. Response includes view information for each accessible account
5. System returns HTTP 200 with account access list

### Technical Context
- **Classes/Services Involved**:
  - APIMethods510.getAccountAccessByUserId (main endpoint handler)
  - NewStyle.function.getUserByUserId (user validation)
  - Views.views.vend.privateViewsUserCanAccess (access retrieval)
- **Input Data**:
  - Path parameters: USER_ID
- **Output Data**:
  - Response body: AccountsMinimalJson400 (list of accessible accounts)
  - HTTP Status: 200 OK
- **Processing Type**: REST API - Synchronous query

### Business Rules (from code)
1. Requires elevated permission to view other users' access
2. Returns comprehensive view of user's access across all accounts
3. Includes private views (views with restricted access)
4. Useful for access auditing and compliance

### Data Validations (if applicable)
- **User ID validation**: Must be valid user
- **Permission validation**: Must have canSeeAccountAccessForAnyUser entitlement

### Dependencies
- **Upstream**:
  - User authentication and entitlement service
  - User registry
- **Downstream**:
  - View access service
- **External Systems**:
  - None

### Notes for Implementation
- Important for security audits and compliance reporting
- Consider performance for users with extensive access
- **Needs SME Input**: Should there be date filtering to see historical access?
- **Needs SME Input**: Should this include access grant timestamps?

---

### Priority: Low (System Administration & Maintenance)

## User Story 12: Check Account System Integrity

### Story Overview
**As a** system administrator  
**I want to** run integrity checks on account data  
**So that** I can identify and fix data quality issues before they cause problems

### Acceptance Criteria
1. User must be authenticated with appropriate system administration permissions
2. **Account Access Unique Index Check**: Identifies duplicate system view assignments
3. **Account Currency Check**: Validates all accounts have proper currency codes
4. **Orphaned Account Check**: Finds accounts without proper customer/user associations
5. Each check returns pass/fail status with details of issues found
6. System returns HTTP 200 with check results
7. Results include debug information for failed checks

### Technical Context
- **Classes/Services Involved**:
  - APIMethods510.accountAccessUniqueIndexCheck (duplicate view check)
  - APIMethods510.accountCurrencyCheck (currency validation)
  - APIMethods510.orphanedAccountCheck (orphan detection)
- **Input Data**:
  - Path parameters: BANK_ID (for some checks)
- **Output Data**:
  - Response body: CheckSystemIntegrityJsonV510 with success status and debug info
  - HTTP Status: 200 OK
- **Processing Type**: REST API - Synchronous system check

### Business Rules (from code)
1. Integrity checks are non-destructive (read-only)
2. Checks identify issues but don't automatically fix them
3. Debug information helps administrators locate and fix issues
4. Some checks are bank-specific, others are system-wide

### Data Validations (if applicable)
- **Permission validation**: User must have system administration entitlements
- **Bank validation**: Bank ID must be valid when required

### Dependencies
- **Upstream**:
  - User authentication and entitlement service
- **Downstream**:
  - Account registry
  - View configuration service
- **External Systems**:
  - None

### Notes for Implementation
- Useful for scheduled maintenance and health checks
- Consider making these checks part of regular system monitoring
- **Needs SME Input**: Should integrity checks be automated on a schedule?
- **Needs SME Input**: What are the resolution procedures for each type of integrity issue?

---

## User Story 13: Get Supported Currencies at Bank

### Story Overview
**As a** bank employee or customer  
**I want to** view the list of currencies supported by accounts at a specific bank  
**So that** I know which currencies are available when creating accounts or making transactions

### Acceptance Criteria
1. User must be authenticated
2. Bank ID must be valid
3. System returns list of all currency codes used by accounts at the bank
4. Response includes ISO 4217 currency codes
5. System returns HTTP 200 with currency list

### Technical Context
- **Classes/Services Involved**:
  - APIMethods510.getCurrenciesAtBank (main endpoint handler)
- **Input Data**:
  - Path parameters: BANK_ID
- **Output Data**:
  - Response body: List of currency codes
  - HTTP Status: 200 OK
- **Processing Type**: REST API - Synchronous query

### Business Rules (from code)
1. Returns actual currencies in use, not theoretical supported currencies
2. Based on existing account data at the bank
3. Helps users understand available currency options

### Data Validations (if applicable)
- **Bank ID validation**: Must be valid bank
- **Authentication validation**: User must be logged in

### Dependencies
- **Upstream**:
  - User authentication service
  - Bank registry
- **Downstream**:
  - Account registry
- **External Systems**:
  - None

### Notes for Implementation
- Useful for UI dropdowns and form validation
- Consider caching as currency list changes infrequently
- **Needs SME Input**: Should this return all supported currencies or only those currently in use?

---

## User Story 14: Create User with Account Access

### Story Overview
**As a** bank administrator  
**I want to** create a new user and immediately grant them access to specific account views  
**So that** I can onboard new users with proper account access in a single operation

### Acceptance Criteria
1. User must be authenticated with user creation permissions
2. New user details must be provided (username, email, etc.)
3. Account and view IDs must be specified for initial access
4. System creates user account
5. System grants view access to specified accounts
6. System returns HTTP 201 with user details and access confirmation
7. Operation is atomic (either both succeed or both fail)

### Technical Context
- **Classes/Services Involved**:
  - APIMethods510.createUserWithAccountAccessById (main endpoint handler)
- **Input Data**:
  - Request body: User creation details plus account/view access specifications
- **Output Data**:
  - Response body: User details with access information
  - HTTP Status: 201 Created
- **Processing Type**: REST API - Synchronous transaction

### Business Rules (from code)
1. Combines user creation and access grant in single operation
2. Reduces administrative steps for user onboarding
3. Access is granted immediately upon user creation

### Data Validations (if applicable)
- **User details validation**: Username, email format, etc.
- **Account/View validation**: Must exist and be accessible
- **Permission validation**: Administrator must have user creation rights

### Dependencies
- **Upstream**:
  - User authentication and entitlement service
- **Downstream**:
  - User management service
  - Access control service
- **External Systems**:
  - Possibly external user directory

### Notes for Implementation
- Convenient for streamlined onboarding
- Consider transaction management for atomicity
- **Needs SME Input**: What user attributes are required vs. optional?
- **Needs SME Input**: Are there default views that should be granted to all new users?

---

## User Story 15: Manage Account Attributes

### Story Overview
**As a** system  
**I want to** automatically create and manage account attributes based on product configuration  
**So that** accounts have proper metadata for regulatory, reporting, and operational purposes

### Acceptance Criteria
1. Account attributes are automatically created during account creation
2. Attributes are derived from product attributes configuration
3. Each attribute has: account_attribute_id, product_code, contract_code (optional), name, type, value
4. Attribute types include: STRING, INTEGER, DOUBLE, DATE_WITH_DAY
5. Attributes support product instance codes for contract tracking
6. Attributes are included in account response payloads

### Technical Context
- **Classes/Services Involved**:
  - AccountAttributeX.createAccountAttributes (attribute creation)
  - JSONFactory500.createAccountAttributes (response formatting)
  - Product attribute services (source of attribute definitions)
- **Input Data**:
  - Product attributes from product catalog
  - Account ID and product code
- **Output Data**:
  - AccountAttributeResponseJson500 (list of attributes)
- **Processing Type**: Automatic during account creation

### Business Rules (from code)
1. Attributes are automatically populated from product configuration
2. No manual attribute specification during account creation
3. Attributes can have optional product instance codes for contract tracking
4. Attribute types must match predefined types
5. Attributes are immutable after account creation (based on observed behavior)

### Data Validations (if applicable)
- **Product validation**: Product must have defined attributes
- **Type validation**: Attribute type must be valid enum value
- **Value validation**: Value must match declared type

### Dependencies
- **Upstream**:
  - Product catalog with attribute definitions
- **Downstream**:
  - Account attribute storage
- **External Systems**:
  - None directly

### Notes for Implementation
- Attributes are important for regulatory compliance and reporting
- Product configuration must be complete before accounts can be created
- **Needs SME Input**: What are the standard attributes for each account type?
- **Needs SME Input**: Can attributes be updated after account creation?
- **Needs SME Input**: What is the purpose of product instance codes (contract codes)?

---

## Part 3: Open Questions

### Account Creation & Lifecycle
1. **Account Routing Schemes**: What are all the valid account routing schemes and their specific validation rules? The code references routing schemes but doesn't enumerate the complete set or their format requirements.

2. **Account Immutability**: Accounts appear to be immutable once created (no update endpoint found). Is this intentional? What is the business process for correcting account data errors?

3. **Account Deletion**: No account deletion endpoint was found. What is the account closure process? Are accounts soft-deleted or marked as inactive?

4. **Initial Balance Restriction**: Why must initial balance be zero? What is the subsequent process for funding new accounts? Is there a separate deposit operation?

### Account Views & Permissions
5. **Complete Permission Set**: What is the complete enumeration of all view permissions (e.g., CAN_SEE_BANK_ACCOUNT_BALANCE, CAN_SEE_AVAILABLE_VIEWS_FOR_BANK_ACCOUNT, etc.)? The code shows several but likely not all.

6. **Default Views**: What are the standard/default system views that should be created for a new OBP-API installation? What permissions does each have?

7. **View Hierarchy**: Is there a hierarchy or inheritance model for view permissions? Can views inherit permissions from other views?

8. **View Deletion Impact**: What happens when a view is deleted that users currently have access to? Are users notified? Is access automatically revoked?

### Customer-Account Relationships
9. **Relationship Types**: What are all the valid values for relationship_type in customer-account links? (e.g., owner, joint_owner, authorized_user, beneficiary, etc.)

10. **Relationship Rules**: Are there business rules about relationship types? For example, must every account have at least one "owner" relationship? Can an account have multiple owners?

11. **Link Deletion Impact**: What happens to account access when a customer-account link is deleted? Are view permissions automatically adjusted?

### Account Attributes
12. **Standard Attributes**: What are the standard attributes for each account type? What attributes are required vs. optional?

13. **Attribute Mutability**: Can account attributes be updated after account creation? If not, how are attribute errors corrected?

14. **Contract Codes**: What is the business purpose of product instance codes (contract_code) in account attributes? How are they used in practice?

### Access Control & Security
15. **Access Audit Trail**: Should there be an audit trail of access grants and revocations? Is this implemented elsewhere in the system?

16. **Protected Access**: Can access be revoked from account owners, or are there protected access rights that cannot be removed?

17. **Cross-Bank Access**: Are there regulatory or privacy implications for cross-bank account viewing (getAccountsHeldByUser)? What compliance requirements apply?

### Backend Integration
18. **Connector Variations**: The code uses a connector pattern for backend integration. Do different backend implementations have different business rules or validation requirements?

19. **Balance Types**: What are the different types of balances that may be returned (available, current, pending, etc.)? How do they differ?

20. **Real-time vs. Cached**: Are balances always retrieved in real-time from the core banking system, or is caching used? What is the data freshness guarantee?

### System Administration
21. **Integrity Check Automation**: Should the integrity checks be automated on a schedule? What is the recommended frequency?

22. **Issue Resolution**: What are the standard resolution procedures for each type of integrity issue (duplicate views, orphaned accounts, currency issues)?

23. **Supported Currencies**: Should getCurrenciesAtBank return all currencies the bank supports, or only those currently in use by existing accounts?

### Migration & Modernization
24. **Legacy Integration**: How do these accounts integrate with legacy core banking systems? Are there synchronization requirements?

25. **Account Data Migration**: What is the process for migrating existing accounts from legacy systems into OBP-API?

26. **Regional Standards**: The codebase mentions Berlin Group, UK Open Banking, etc. How do regional standards affect account management capabilities? Are there region-specific features not visible in the base API?

---

## Summary

This document provides a comprehensive analysis of Account Management capabilities in the OBP-API (v5.0.0 and v5.1.0), following the two-phase user story extraction methodology. The analysis identified:

- **30 distinct capabilities** across 8 major categories
- **15 detailed user stories** covering high, medium, and low priority capabilities
- **26 open questions** requiring SME input for complete understanding

### Key Insights

1. **View-Based Access Control**: The OBP-API uses a sophisticated view-based permission model where access to account data is controlled through views, each with specific permission sets. This enables fine-grained access control.

2. **Account Immutability**: Accounts appear to be immutable once created (no update or delete endpoints found in v5.0.0/v5.1.0), suggesting a security-first design approach.

3. **Automatic Attribute Management**: Account attributes are automatically derived from product configuration, reducing manual data entry and ensuring consistency.

4. **Comprehensive Customer Linking**: Full CRUD operations for customer-account relationships enable flexible relationship management with typed relationships.

5. **Multi-Level Balance Access**: Three different balance retrieval patterns (specific account, all at bank, through view) provide flexibility for different use cases.

6. **System Integrity Monitoring**: Built-in integrity checks enable proactive identification of data quality issues.

### Recommendations for Implementation

1. **Complete Permission Documentation**: Document all view permissions and their intended uses
2. **Standard View Templates**: Create and document standard system view configurations
3. **Relationship Type Taxonomy**: Define and document all valid customer-account relationship types
4. **Attribute Standards**: Document standard attributes for each account type
5. **Access Audit Trail**: Implement audit logging for access grants/revocations if not already present
6. **Integrity Check Automation**: Schedule regular integrity checks and define resolution procedures

---

**Document Version:** 1.0  
**Last Updated:** November 2024  
**Extraction Source:** OpenBankProject/OBP-API (GitHub)  
**Methodology Source:** user_story_extraction_prompt_scala.md
