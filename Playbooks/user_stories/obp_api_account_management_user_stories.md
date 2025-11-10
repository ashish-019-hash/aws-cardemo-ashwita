# User Stories for OBP API - Account Management

## Part 1: Capability Inventory

### Account Management
1. **Account Listing**
   - Classes/Services: APIMethods510.getAccountsHeldByUserAtBank, APIMethods510.getAccountsHeldByUser
   - Type: REST API
   - Purpose: Retrieve all accounts accessible to a user at one or multiple banks for account selection and overview
   - Frequency: Real-time, High volume

2. **Account Detail Retrieval**
   - Classes/Services: APIMethods510.getCoreAccountByIdThroughView, APIMethods400.getCoreAccountById, APIMethods400.getPrivateAccountByIdFull
   - Type: REST API
   - Purpose: View comprehensive account information including balances, limits, and account details
   - Frequency: Real-time, High volume

3. **Account Creation**
   - Classes/Services: APIMethods400.addAccount, APIMethods200.createAccount, APIMethods310.createAccount, APIMethods500.createAccount
   - Type: REST API
   - Purpose: Create new bank accounts with specified parameters and ownership
   - Frequency: On-demand, Medium volume

4. **Account Update**
   - Classes/Services: APIMethods400.updateAccountLabel, APIMethods121.updateAccountLabel, APIMethods310.updateAccount
   - Type: REST API
   - Purpose: Modify account attributes such as labels and descriptions
   - Frequency: On-demand, Medium volume

5. **Balance Inquiry**
   - Classes/Services: APIMethods510.getBankAccountBalances, APIMethods510.getBankAccountBalanceById, APIMethods510.getAllBankAccountBalances
   - Type: REST API
   - Purpose: Check current account balance and available funds
   - Frequency: Real-time, High volume

6. **Multi-Account Balance Check**
   - Classes/Services: APIMethods510.getBankAccountsBalances, APIMethods510.getBankAccountsBalancesThroughView, APIMethods400.getBankAccountsBalancesForCurrentUser
   - Type: REST API
   - Purpose: Retrieve balances for multiple accounts in single request for portfolio overview
   - Frequency: Real-time, Medium volume

7. **Account Access Management**
   - Classes/Services: APIMethods510.createUserWithAccountAccessById, APIMethods510.grantUserAccessToViewById, APIMethods510.revokeUserAccessToViewById, APIMethods510.getAccountAccessByUserId
   - Type: REST API
   - Purpose: Grant and revoke user access to specific account views for permission management
   - Frequency: On-demand, Medium volume

8. **Account Search by Routing**
   - Classes/Services: APIMethods400.getAccountByAccountRouting, APIMethods400.getAccountsByAccountRoutingRegex
   - Type: REST API
   - Purpose: Locate accounts using routing numbers or IBAN for payment processing
   - Frequency: Real-time, Medium volume

9. **Settlement Account Management**
   - Classes/Services: APIMethods400.createSettlementAccount, APIMethods400.getSettlementAccounts
   - Type: REST API
   - Purpose: Manage special settlement accounts for payment clearing operations
   - Frequency: On-demand, Low volume

10. **Account Attribute Management**
   - Classes/Services: APIMethods400.createOrUpdateAccountAttributeDefinition, APIMethods400.deleteAccountAttributeDefinition, APIMethods400.getAccountAttributeDefinition, APIMethods310.createAccountAttribute, APIMethods310.updateAccountAttribute
   - Type: REST API
   - Purpose: Add, update, and remove custom attributes on accounts for flexible metadata management
   - Frequency: On-demand, Low volume

---

## Part 2: Detailed User Stories

### Priority: High

---

## User Story 1: Account Listing

### Story Overview
**As a** banking application user or API consumer  
**I want to** retrieve a list of all bank accounts I have access to  
**So that** I can view my accounts and select which one to perform operations on

### API Endpoints
1. **GET /obp/v5.1.0/users/{USER_ID}/banks/{BANK_ID}/accounts-held**
   - Method: `getAccountsHeldByUserAtBank`
   - Purpose: Retrieve accounts held by user at a specific bank
   - Entitlements: CanGetAccountsHeldAtOneBank OR CanGetAccountsHeldAtAnyBank

2. **GET /obp/v5.1.0/users/{USER_ID}/accounts-held**
   - Method: `getAccountsHeldByUser`
   - Purpose: Retrieve accounts held by user across all banks
   - Entitlements: CanGetAccountsHeldAtAnyBank

### Acceptance Criteria
1. User can retrieve accounts held at a specific bank by providing bank ID
2. User can retrieve accounts held across all banks they have access to
3. Response includes core account information (account ID, bank ID, label, number, account_routings)
4. Results can be filtered by account type using query parameters (account_type_filter, account_type_filter_operation)
5. Only accounts the user has permission to view are returned
6. System handles pagination for users with many accounts
7. Response time is under 2 seconds for typical user account lists
8. Invalid user ID returns appropriate error message
9. Invalid bank ID returns appropriate error message
10. Account type filter validation ensures operation is INCLUDE or EXCLUDE

### Technical Context
- **Classes/Services Involved**: 
  - APIMethods510.getAccountsHeldByUserAtBank - retrieves accounts at specific bank
  - APIMethods510.getAccountsHeldByUser - retrieves accounts across all banks
  - AccountsHelper.filterWithAccountType - applies account type filtering
  - ViewNewStyle - manages view permissions
  - JSONFactory300.createCoreAccountsByCoreAccountsJSON - formats response
  - Connector.getAccountsHeld - retrieves account data from core banking system
  - Connector.getAccountsHeldByUser - retrieves account data across banks
- **Input Data**: User ID, Bank ID (optional), account type filters (query parameters: account_type_filter, account_type_filter_operation)
- **Output Data**: JSON array of core account objects with id, bank_id, label, number, account_routings
- **Processing Type**: Real-time REST API

### Business Rules (from code)
1. User must be authenticated to retrieve account lists
2. Only accounts where user has at least one view permission are returned
3. Account type filtering is optional and supports multiple types via comma-separated values
4. Account type filter operation must be either INCLUDE or EXCLUDE
5. Results must respect user's entitlements (canGetAccountsHeldAtOneBank or canGetAccountsHeldAtAnyBank)
6. Bank-specific endpoint requires bank ID parameter
7. All-banks endpoint does not require bank ID parameter
8. View permission check is performed before including account in results
9. Account type filter defaults to INCLUDE if not specified
10. Empty filter list returns all accounts regardless of type

### Data Validations
- User ID must be valid and exist in system
- Bank ID must be valid if specified
- Account type filter values must match valid account types
- Account type filter operation must be "INCLUDE" or "EXCLUDE"
- User must have appropriate entitlements for the requested scope

### Dependencies
- **Upstream**: User authentication and authorization
- **Downstream**: Account detail views, transaction retrieval, balance inquiries
- **External Systems**: Bank connector for retrieving actual account data

### Notes for Implementation
- Short TTL caching should be implemented for frequent requests to improve performance
- Pagination mechanism needed for users with large numbers of accounts
- Account type filter supports both inclusion and exclusion logic
- Response format must match CoreAccountsHeldJsonV300 structure exactly
- Audit trail should be maintained for all account access requests

---

## User Story 2: Account Detail Retrieval

### Story Overview
**As a** banking application user or account holder  
**I want to** view comprehensive information about a specific account  
**So that** I can see account details, balances, limits, and other relevant information

### API Endpoints
1. **GET /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views/{VIEW_ID}**
   - Method: `getCoreAccountByIdThroughView`
   - Purpose: Get core account details through a specific view
   - Entitlements: View-specific permissions

2. **GET /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}**
   - Method: `getCoreAccountById`
   - Purpose: Get core account information by ID
   - Entitlements: Account access permissions

3. **GET /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/account**
   - Method: `getPrivateAccountByIdFull`
   - Purpose: Get full private account details including sensitive information
   - Entitlements: Owner or specific view permissions

4. **GET /obp/v3.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/account**
   - Method: `getPrivateAccountById`
   - Purpose: Get private account details through view (v3.0.0)
   - Entitlements: View-specific permissions

5. **GET /obp/v3.0.0/my/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/account**
   - Method: `getPublicAccountById`
   - Purpose: Get public account information
   - Entitlements: Public view access

### Acceptance Criteria
1. User can retrieve account details by providing bank ID, account ID, and view ID
2. Response includes comprehensive account information based on view permissions
3. Account balance is included in the response
4. Account limits and restrictions are displayed
5. Account routing information (IBAN, account number) is provided
6. Account type and currency are included
7. Account label and description are returned
8. Only information permitted by the view is returned
9. Invalid account ID returns appropriate error
10. User without view permission receives authorization error
11. Response time is under 2 seconds

### Technical Context
- **Classes/Services Involved**:
  - APIMethods510.getCoreAccountByIdThroughView - retrieves account through view
  - APIMethods400.getCoreAccountById - retrieves core account data
  - APIMethods400.getPrivateAccountByIdFull - retrieves full account details
  - ViewNewStyle.checkViewAccessAndReturnView - validates view permissions
  - BankAccount - domain model for account data
  - JSONFactory - formats account response
- **Input Data**: Bank ID, Account ID, View ID, user authentication context
- **Output Data**: JSON object with account details (id, bank_id, label, number, balance, currency, account_type, account_routings, limits)
- **Processing Type**: Real-time REST API

### Business Rules (from code)
1. User must have view permission to access account details
2. Different views expose different levels of account information
3. Owner view provides full account access
4. Public view provides limited account information
5. Account must exist and be active
6. Bank ID and Account ID must match
7. View permissions are checked before returning data
8. Sensitive information is filtered based on view permissions
9. Account balance is current as of request time
10. Account routing information follows banking standards

### Data Validations
- Bank ID must be valid and exist
- Account ID must be valid and exist
- View ID must be valid for the account
- User must have permission for the specified view
- Account must belong to the specified bank

### Dependencies
- **Upstream**: User authentication, view permission management
- **Downstream**: Transaction viewing, balance inquiries, account operations
- **External Systems**: Core banking system via connector

### Notes for Implementation
- Different API versions provide different levels of detail
- View-based access control is critical for data privacy
- Response format varies by API version
- Caching strategy should consider data freshness requirements
- Audit logging required for account access

---

## User Story 3: Account Creation

### Story Overview
**As a** bank administrator or authorized user  
**I want to** create new bank accounts with specified parameters  
**So that** customers can have accounts to perform banking operations

### API Endpoints
1. **POST /obp/v4.0.0/banks/{BANK_ID}/accounts**
   - Method: `addAccount`
   - Purpose: Add new account with full parameters
   - Entitlements: CanCreateAccount

2. **PUT /obp/v2.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}**
   - Method: `createAccount`
   - Purpose: Create account (v2.0.0)
   - Entitlements: CanCreateAccount

3. **POST /obp/v2.2.0/banks/{BANK_ID}/accounts**
   - Method: `createAccount`
   - Purpose: Create account (v2.2.0)
   - Entitlements: CanCreateAccount

4. **POST /obp/v3.1.0/banks/{BANK_ID}/accounts**
   - Method: `createAccount`
   - Purpose: Create account (v3.1.0)
   - Entitlements: CanCreateAccount

5. **POST /obp/v5.0.0/banks/{BANK_ID}/accounts**
   - Method: `createAccount`
   - Purpose: Create account (v5.0.0)
   - Entitlements: CanCreateAccount

### Acceptance Criteria
1. Administrator can create new account by providing required parameters
2. Account is created with unique account ID
3. Account number is generated or validated
4. Account type is specified (checking, savings, etc.)
5. Account currency is set
6. Initial balance can be specified
7. Account owner/customer is linked
8. Account label and description can be set
9. Account routing information is configured
10. Account is created in active status
11. Appropriate entitlements are checked before creation
12. Validation errors return clear error messages
13. Duplicate account numbers are prevented

### Technical Context
- **Classes/Services Involved**:
  - APIMethods400.addAccount - creates account with full parameters
  - APIMethods200.createAccount - creates account (v2.0.0)
  - APIMethods220.createAccount - creates account (v2.2.0)
  - APIMethods310.createAccount - creates account (v3.1.0)
  - APIMethods500.createAccount - creates account (v5.0.0)
  - Connector.createBankAccount - creates account in core banking system
  - BankAccount - account domain model
  - Customer - customer domain model for linking
- **Input Data**: Bank ID, account parameters (user_id, label, type, balance, currency, account_routing, branch_id)
- **Output Data**: JSON object with created account details including generated account ID
- **Processing Type**: Real-time REST API

### Business Rules (from code)
1. User must have CanCreateAccount entitlement
2. Bank ID must be valid and active
3. Account type must be valid (checking, savings, loan, etc.)
4. Currency must be valid ISO currency code
5. Account number must be unique within bank
6. Customer/user must exist before account creation
7. Initial balance must be non-negative for most account types
8. Account routing information must follow banking standards
9. Branch ID must be valid if specified
10. Account label is required

### Data Validations
- Bank ID validation
- Account type validation against allowed types
- Currency code validation (ISO 4217)
- Account number uniqueness check
- Customer/user existence validation
- Balance format and range validation
- Routing information format validation
- Branch ID validation if provided

### Dependencies
- **Upstream**: User/customer registration, bank setup
- **Downstream**: Account access management, view creation, initial deposit
- **External Systems**: Core banking system for account creation

### Notes for Implementation
- Different API versions have different parameter requirements
- Account number generation strategy may vary by bank
- Initial balance handling varies by account type
- Audit trail required for account creation
- Transaction may need to be atomic with initial deposit
- Consider regulatory requirements for account opening

---

## User Story 4: Account Update

### Story Overview
**As a** account owner or authorized user  
**I want to** modify account attributes such as labels and descriptions  
**So that** I can keep account information current and organized

### API Endpoints
1. **PUT /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}**
   - Method: `updateAccountLabel`
   - Purpose: Update account label
   - Entitlements: CanUpdateAccountLabel OR account owner

2. **PUT /obp/v1.2.1/banks/{BANK_ID}/accounts/{ACCOUNT_ID}**
   - Method: `updateAccountLabel`
   - Purpose: Update account label (v1.2.1)
   - Entitlements: CanUpdateAccountLabel OR account owner

3. **PUT /obp/v3.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}**
   - Method: `updateAccount`
   - Purpose: Update account details
   - Entitlements: CanUpdateAccount OR account owner

### Acceptance Criteria
1. Account owner can update account label
2. Account owner can update account description
3. Authorized users with proper entitlements can update accounts
4. Account ID and bank ID must match
5. Updated information is immediately reflected
6. Invalid account ID returns error
7. Unauthorized users receive permission error
8. Label length is validated
9. Special characters in label are handled appropriately
10. Update history is maintained for audit purposes

### Technical Context
- **Classes/Services Involved**:
  - APIMethods400.updateAccountLabel - updates account label
  - APIMethods121.updateAccountLabel - updates account label (v1.2.1)
  - APIMethods310.updateAccount - updates account details
  - Connector.updateBankAccount - updates account in core system
  - BankAccount - account domain model
- **Input Data**: Bank ID, Account ID, updated label/description
- **Output Data**: JSON object with updated account information
- **Processing Type**: Real-time REST API

### Business Rules (from code)
1. User must be account owner or have CanUpdateAccountLabel entitlement
2. Account must exist and be active
3. Bank ID and Account ID must match
4. Label cannot be empty
5. Label length must be within limits
6. Description is optional
7. Core account attributes (number, type, currency) cannot be changed via label update
8. Update is atomic
9. Audit trail is maintained

### Data Validations
- Bank ID validation
- Account ID validation
- Label length validation (typically 1-255 characters)
- Label format validation (allowed characters)
- User permission validation
- Account existence validation

### Dependencies
- **Upstream**: Account creation, user authentication
- **Downstream**: Account display, account listing
- **External Systems**: Core banking system for persistence

### Notes for Implementation
- Different API versions support different update capabilities
- v3.1.0 updateAccount may support more fields than just label
- Consider rate limiting for update operations
- Audit logging required for all updates
- Optimistic locking may be needed for concurrent updates

---

## User Story 5: Balance Inquiry

### Story Overview
**As a** account holder or authorized user  
**I want to** check current account balance and available funds  
**So that** I can make informed financial decisions and track my account status

### API Endpoints
1. **GET /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views/{VIEW_ID}/balances**
   - Method: `getBankAccountBalances`
   - Purpose: Get account balances through view
   - Entitlements: View-specific permissions

2. **GET /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances/{BALANCE_ID}**
   - Method: `getBankAccountBalanceById`
   - Purpose: Get specific balance by ID
   - Entitlements: CanGetBankAccountBalance

3. **GET /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances**
   - Method: `getAllBankAccountBalances`
   - Purpose: Get all balances for account
   - Entitlements: CanGetBankAccountBalances

4. **GET /obp/v3.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances**
   - Method: `getBankAccountsBalances`
   - Purpose: Get account balances (v3.1.0)
   - Entitlements: View permissions

### Acceptance Criteria
1. User can retrieve current account balance
2. Available balance is displayed (balance minus holds/reserves)
3. Currency is included with balance
4. Balance timestamp is provided
5. Multiple balance types can be retrieved (current, available, pending)
6. Balance is accurate as of request time
7. View permissions control balance visibility
8. Invalid account ID returns error
9. Unauthorized access returns permission error
10. Response time is under 1 second for balance queries

### Technical Context
- **Classes/Services Involved**:
  - APIMethods510.getBankAccountBalances - retrieves balances through view
  - APIMethods510.getBankAccountBalanceById - retrieves specific balance
  - APIMethods510.getAllBankAccountBalances - retrieves all balances
  - APIMethods310.getBankAccountsBalances - retrieves balances (v3.1.0)
  - Connector.getBankAccountBalance - retrieves balance from core system
  - ViewNewStyle - manages view permissions
  - Balance - balance domain model
- **Input Data**: Bank ID, Account ID, View ID (optional), Balance ID (optional)
- **Output Data**: JSON object with balance information (amount, currency, type, timestamp)
- **Processing Type**: Real-time REST API

### Business Rules (from code)
1. User must have view permission to see balance
2. Balance is current as of request time
3. Different balance types may be available (current, available, pending)
4. Currency is always included with balance amount
5. Balance precision follows currency standards
6. Negative balances are allowed for certain account types
7. View permissions determine balance visibility
8. Balance timestamp indicates data freshness

### Data Validations
- Bank ID validation
- Account ID validation
- View ID validation (if provided)
- Balance ID validation (if provided)
- User permission validation
- Account existence validation

### Dependencies
- **Upstream**: Account creation, view permission setup
- **Downstream**: Transaction processing, payment authorization
- **External Systems**: Core banking system for real-time balance data

### Notes for Implementation
- Balance queries should be highly optimized for performance
- Caching strategy should balance freshness vs performance
- Consider real-time vs near-real-time balance requirements
- Audit logging for balance inquiries may be required
- Support for multiple balance types (current, available, pending, etc.)

---

## User Story 6: Multi-Account Balance Check

### Story Overview
**As a** banking application user with multiple accounts  
**I want to** retrieve balances for multiple accounts in a single request  
**So that** I can get a consolidated view of my financial position efficiently

### API Endpoints
1. **GET /obp/v5.1.0/banks/{BANK_ID}/accounts/balances**
   - Method: `getBankAccountsBalances`
   - Purpose: Get balances for multiple accounts at a bank
   - Entitlements: CanGetBankAccountsBalances

2. **GET /obp/v5.1.0/banks/{BANK_ID}/views/{VIEW_ID}/balances**
   - Method: `getBankAccountsBalancesThroughView`
   - Purpose: Get balances for multiple accounts through specific view
   - Entitlements: View-specific permissions

3. **GET /obp/v4.0.0/banks/{BANK_ID}/balances**
   - Method: `getBankAccountsBalancesForCurrentUser`
   - Purpose: Get all account balances for current user
   - Entitlements: Authenticated user

4. **GET /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/balances**
   - Method: `getBankAccountBalancesForCurrentUser`
   - Purpose: Get balances for specific account for current user
   - Entitlements: Account access permissions

### Acceptance Criteria
1. User can retrieve balances for all accessible accounts in one request
2. Response includes account ID with each balance
3. Balances are grouped by account
4. Currency is included for each balance
5. Only accounts user has permission to view are included
6. Response is paginated for users with many accounts
7. Total portfolio value can be calculated from response
8. Balances are current as of request time
9. Response time is under 3 seconds for typical number of accounts
10. Empty result is returned if user has no accessible accounts

### Technical Context
- **Classes/Services Involved**:
  - APIMethods510.getBankAccountsBalances - retrieves multi-account balances
  - APIMethods510.getBankAccountsBalancesThroughView - retrieves through view
  - APIMethods400.getBankAccountsBalancesForCurrentUser - retrieves for current user
  - APIMethods400.getBankAccountBalancesForCurrentUser - retrieves for specific account
  - Connector.getBankAccountsBalances - retrieves from core system
  - ViewNewStyle - manages permissions
- **Input Data**: Bank ID, View ID (optional), user authentication context
- **Output Data**: JSON array of account balances with account IDs
- **Processing Type**: Real-time REST API

### Business Rules (from code)
1. User must be authenticated
2. Only accounts with view permissions are included
3. Balances are retrieved in parallel for performance
4. Response includes all accessible accounts at specified bank
5. View-based filtering applies to account inclusion
6. Currency conversion is not performed (native currencies returned)
7. Pagination is supported for large account sets

### Data Validations
- Bank ID validation
- View ID validation (if provided)
- User authentication validation
- Permission validation for each account

### Dependencies
- **Upstream**: Account creation, view permissions, user authentication
- **Downstream**: Portfolio analysis, financial reporting
- **External Systems**: Core banking system for balance data

### Notes for Implementation
- Performance optimization critical for users with many accounts
- Consider parallel balance retrieval
- Caching strategy for frequently accessed data
- Pagination parameters should be configurable
- Consider aggregation options (total by currency, etc.)

---

## User Story 7: Account Access Management

### Story Overview
**As a** account owner or administrator  
**I want to** grant and revoke user access to specific account views  
**So that** I can control who can see and interact with account information

### API Endpoints
1. **POST /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views/{VIEW_ID}/user-account-access**
   - Method: `createUserWithAccountAccessById`
   - Purpose: Create user with account access
   - Entitlements: CanCreateUserWithAccountAccess

2. **POST /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views/{VIEW_ID}/account-access/grant**
   - Method: `grantUserAccessToViewById`
   - Purpose: Grant user access to view
   - Entitlements: CanGrantAccountAccess OR account owner

3. **POST /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views/{VIEW_ID}/account-access/revoke**
   - Method: `revokeUserAccessToViewById`
   - Purpose: Revoke user access to view
   - Entitlements: CanRevokeAccountAccess OR account owner

4. **GET /obp/v5.1.0/users/{USER_ID}/account-access**
   - Method: `getAccountAccessByUserId`
   - Purpose: Get account access for user
   - Entitlements: CanGetAccountAccess

5. **POST /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/user-account-access**
   - Method: `createUserWithAccountAccess`
   - Purpose: Create user with account access (v4.0.0)
   - Entitlements: CanCreateUserWithAccountAccess

6. **POST /obp/v1.2.1/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/permissions/{PROVIDER}/{PROVIDER_ID}/{VIEW_ID}**
   - Method: `addPermissionForUserForBankAccountForOneView`
   - Purpose: Add permission for user (v1.2.1)
   - Entitlements: Account owner

7. **DELETE /obp/v1.2.1/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/permissions/{PROVIDER}/{PROVIDER_ID}/{VIEW_ID}**
   - Method: `removePermissionForUserForBankAccountForOneView`
   - Purpose: Remove permission for user (v1.2.1)
   - Entitlements: Account owner

### Acceptance Criteria
1. Account owner can grant view access to other users
2. Account owner can revoke view access from users
3. Administrator with proper entitlements can manage access
4. Access can be granted for specific views (owner, public, accountant, etc.)
5. User receiving access is notified (if configured)
6. Access changes are immediately effective
7. Audit trail is maintained for access changes
8. Cannot revoke owner's own access
9. Invalid user ID returns error
10. Invalid view ID returns error
11. Duplicate access grant is handled gracefully

### Technical Context
- **Classes/Services Involved**:
  - APIMethods510.createUserWithAccountAccessById - creates user with access
  - APIMethods510.grantUserAccessToViewById - grants access
  - APIMethods510.revokeUserAccessToViewById - revokes access
  - APIMethods510.getAccountAccessByUserId - retrieves access info
  - ViewNewStyle.grantAccessToView - grants view access
  - ViewNewStyle.revokeAccessToView - revokes view access
  - ViewImpl - view permission management
- **Input Data**: Bank ID, Account ID, View ID, User ID, provider information
- **Output Data**: JSON confirmation of access change
- **Processing Type**: Real-time REST API

### Business Rules (from code)
1. User must be account owner or have appropriate entitlement
2. View must exist for the account
3. User receiving access must exist
4. Owner view access cannot be revoked from account owner
5. Access changes are atomic
6. Duplicate access grants are idempotent
7. Revoking non-existent access returns error
8. System views (owner, public) have special rules
9. Custom views can be created and managed separately

### Data Validations
- Bank ID validation
- Account ID validation
- View ID validation
- User ID validation
- Provider validation
- Permission validation for requester

### Dependencies
- **Upstream**: Account creation, user registration, view creation
- **Downstream**: Account access, transaction viewing, balance inquiries
- **External Systems**: User management system, notification system

### Notes for Implementation
- Access changes should be audited
- Consider notification mechanism for access grants
- Support for bulk access management may be needed
- Different API versions have different access management approaches
- View hierarchy and inheritance should be considered

---

## User Story 8: Account Search by Routing

### Story Overview
**As a** payment processor or banking application  
**I want to** locate accounts using routing numbers or IBAN  
**So that** I can process payments and transfers to the correct accounts

### API Endpoints
1. **POST /obp/v4.0.0/management/accounts/account-routing-query**
   - Method: `getAccountByAccountRouting`
   - Purpose: Get account by routing information (exact match)
   - Entitlements: CanGetAccountByAccountRouting

2. **POST /obp/v4.0.0/management/accounts/account-routing-regex-query**
   - Method: `getAccountsByAccountRoutingRegex`
   - Purpose: Search accounts by routing regex pattern
   - Entitlements: CanSearchAccountsByAccountRouting

### Acceptance Criteria
1. User can search for account using IBAN
2. User can search for account using account number
3. User can search for account using routing number
4. Search supports exact match
5. Search supports regex pattern matching
6. Response includes account details if found
7. Multiple matches return all matching accounts
8. No match returns empty result
9. Invalid routing scheme returns error
10. Response time is under 2 seconds
11. Only accounts user has permission to view are returned

### Technical Context
- **Classes/Services Involved**:
  - APIMethods400.getAccountByAccountRouting - searches by routing
  - APIMethods400.getAccountsByAccountRoutingRegex - searches by regex
  - Connector.getAccountByAccountRouting - retrieves from core system
  - AccountRouting - routing information model
  - BankAccount - account domain model
- **Input Data**: Bank ID, routing scheme (IBAN, AccountNumber, etc.), routing address/pattern
- **Output Data**: JSON object or array with matching account details
- **Processing Type**: Real-time REST API

### Business Rules (from code)
1. User must have appropriate entitlement to search
2. Routing scheme must be valid (IBAN, AccountNumber, etc.)
3. Routing address format must match scheme requirements
4. Search is case-insensitive
5. Regex search supports standard regex patterns
6. Only active accounts are returned
7. View permissions filter results
8. Bank ID scope limits search to specific bank

### Data Validations
- Bank ID validation
- Routing scheme validation (must be recognized scheme)
- Routing address format validation (IBAN checksum, etc.)
- Regex pattern validation (for regex search)
- User permission validation

### Dependencies
- **Upstream**: Account creation with routing information
- **Downstream**: Payment processing, transfer initiation
- **External Systems**: Core banking system, routing validation services

### Notes for Implementation
- IBAN validation should include checksum verification
- Routing information must be indexed for performance
- Consider caching for frequently searched routing numbers
- Support for international routing standards (IBAN, SWIFT, etc.)
- Regex search should have performance safeguards

---

## User Story 9: Settlement Account Management

### Story Overview
**As a** bank administrator or payment operations manager  
**I want to** manage special settlement accounts for payment clearing  
**So that** payment processing and clearing operations can be performed correctly

### API Endpoints
1. **POST /obp/v4.0.0/banks/{BANK_ID}/settlement-accounts**
   - Method: `createSettlementAccount`
   - Purpose: Create new settlement account
   - Entitlements: CanCreateSettlementAccount

2. **GET /obp/v4.0.0/banks/{BANK_ID}/settlement-accounts**
   - Method: `getSettlementAccounts`
   - Purpose: Retrieve all settlement accounts
   - Entitlements: CanGetSettlementAccounts

### Acceptance Criteria
1. Administrator can create settlement accounts
2. Settlement accounts have special attributes
3. Settlement accounts can be listed
4. Settlement accounts are distinguished from regular accounts
5. Settlement account creation includes currency specification
6. Settlement accounts support multiple currencies
7. Settlement accounts have appropriate access controls
8. Settlement account balance can be monitored
9. Invalid parameters return validation errors
10. Audit trail is maintained for settlement account operations

### Technical Context
- **Classes/Services Involved**:
  - APIMethods400.createSettlementAccount - creates settlement account
  - APIMethods400.getSettlementAccounts - retrieves settlement accounts
  - Connector.createSettlementAccount - creates in core system
  - SettlementAccount - settlement account model
  - BankAccount - base account model
- **Input Data**: Bank ID, settlement account parameters (currency, type, purpose)
- **Output Data**: JSON object with settlement account details
- **Processing Type**: Real-time REST API

### Business Rules (from code)
1. User must have CanCreateSettlementAccount entitlement
2. Settlement accounts are special-purpose accounts
3. Settlement accounts may have different rules than regular accounts
4. Currency must be specified for settlement accounts
5. Settlement accounts are used for clearing operations
6. Access to settlement accounts is restricted
7. Settlement accounts may not be visible to regular users

### Data Validations
- Bank ID validation
- Currency validation
- Settlement account type validation
- User entitlement validation
- Uniqueness validation for settlement account identifiers

### Dependencies
- **Upstream**: Bank setup, currency configuration
- **Downstream**: Payment clearing, settlement processing
- **External Systems**: Core banking system, clearing systems

### Notes for Implementation
- Settlement accounts have special regulatory requirements
- Consider multi-currency settlement account support
- Audit logging is critical for settlement operations
- Integration with clearing and settlement systems
- May require special reconciliation processes

---

## User Story 10: Account Attribute Management

### Story Overview
**As a** bank administrator or account manager  
**I want to** add, update, and remove custom attributes on accounts  
**So that** I can store flexible metadata and extend account information without schema changes

### API Endpoints
1. **POST /obp/v4.0.0/banks/{BANK_ID}/attribute-definitions/account**
   - Method: `createOrUpdateAccountAttributeDefinition`
   - Purpose: Create or update account attribute definition
   - Entitlements: CanCreateAccountAttributeDefinition

2. **DELETE /obp/v4.0.0/banks/{BANK_ID}/attribute-definitions/{ATTRIBUTE_DEFINITION_ID}**
   - Method: `deleteAccountAttributeDefinition`
   - Purpose: Delete account attribute definition
   - Entitlements: CanDeleteAccountAttributeDefinition

3. **GET /obp/v4.0.0/banks/{BANK_ID}/attribute-definitions/account**
   - Method: `getAccountAttributeDefinition`
   - Purpose: Get account attribute definitions
   - Entitlements: CanGetAccountAttributeDefinition

4. **POST /obp/v3.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/attributes**
   - Method: `createAccountAttribute`
   - Purpose: Create account attribute value
   - Entitlements: CanCreateAccountAttribute

5. **PUT /obp/v3.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/attributes/{ACCOUNT_ATTRIBUTE_ID}**
   - Method: `updateAccountAttribute`
   - Purpose: Update account attribute value
   - Entitlements: CanUpdateAccountAttribute

6. **POST /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views/{VIEW_ID}/tags**
   - Method: `addTagForViewOnAccount`
   - Purpose: Add tag to account view
   - Entitlements: CanAddTagForViewOnAccount

7. **DELETE /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views/{VIEW_ID}/tags/{TAG_ID}**
   - Method: `deleteTagForViewOnAccount`
   - Purpose: Delete tag from account view
   - Entitlements: CanDeleteTagForViewOnAccount

8. **GET /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views/{VIEW_ID}/tags**
   - Method: `getTagsForViewOnAccount`
   - Purpose: Get tags for account view
   - Entitlements: CanGetTagsForViewOnAccount

### Acceptance Criteria
1. Administrator can define custom attribute types
2. Attributes have name, type, and validation rules
3. Attribute definitions can be updated
4. Attribute definitions can be deleted if not in use
5. Attribute values can be set on accounts
6. Attribute values can be updated
7. Attribute values can be retrieved
8. Tags can be added to accounts for categorization
9. Tags can be removed from accounts
10. Attribute validation is enforced
11. Attribute history is maintained
12. Invalid attribute types return errors

### Technical Context
- **Classes/Services Involved**:
  - APIMethods400.createOrUpdateAccountAttributeDefinition - manages definitions
  - APIMethods400.deleteAccountAttributeDefinition - deletes definitions
  - APIMethods400.getAccountAttributeDefinition - retrieves definitions
  - APIMethods310.createAccountAttribute - creates attribute values
  - APIMethods310.updateAccountAttribute - updates attribute values
  - APIMethods400.addTagForViewOnAccount - adds tags
  - APIMethods400.deleteTagForViewOnAccount - deletes tags
  - APIMethods400.getTagsForViewOnAccount - retrieves tags
  - AccountAttribute - attribute model
  - AttributeDefinition - definition model
- **Input Data**: Bank ID, Account ID, attribute definitions, attribute values, tags
- **Output Data**: JSON objects with attribute/tag information
- **Processing Type**: Real-time REST API

### Business Rules (from code)
1. User must have appropriate entitlements for attribute operations
2. Attribute definitions must be created before values can be set
3. Attribute values must conform to definition type and validation
4. Attribute definitions cannot be deleted if in use
5. Tags are simpler key-value pairs for categorization
6. Attributes support complex types and validation
7. Attribute changes are audited
8. View-based access controls apply to attributes

### Data Validations
- Bank ID validation
- Account ID validation
- Attribute definition validation (name, type, rules)
- Attribute value validation against definition
- Tag format validation
- User permission validation

### Dependencies
- **Upstream**: Account creation, attribute definition setup
- **Downstream**: Account reporting, account filtering, custom business logic
- **External Systems**: Core banking system for persistence

### Notes for Implementation
- Attribute system provides flexibility for custom data
- Consider performance impact of many attributes
- Attribute definitions should be versioned
- Support for different attribute types (string, number, date, boolean, etc.)
- Tags provide simpler categorization mechanism
- Audit logging for all attribute changes
- Consider attribute inheritance or defaults

---

## Part 3: Open Questions

### Questions Requiring SME Clarification

1. **Account Listing**
   - What is the exact business definition of "held" accounts vs "accessible" accounts?
   - What are all valid account types for filtering?
   - What is the specific threshold for pagination trigger?
   - What is the acceptable TTL for cached account lists?
   - Is there a hard limit on accounts returned in multi-account queries?

2. **Account Detail Retrieval**
   - What specific fields should be included in each view type?
   - What is the data freshness requirement for account details?
   - Should balance be included in account details or separate endpoint?
   - What are the performance SLAs for account detail retrieval?

3. **Account Creation**
   - What are the complete validation rules for account numbers?
   - What is the account number generation strategy?
   - What are the regulatory requirements for account opening?
   - Should initial deposit be part of account creation or separate?
   - What are the allowed account types and their specific rules?

4. **Account Update**
   - What account fields can be updated after creation?
   - Are there any fields that require special approval to update?
   - What is the audit retention period for account updates?

5. **Balance Inquiry**
   - What is the acceptable latency for balance queries?
   - What are the different balance types (current, available, pending)?
   - How should holds and reserves be reflected in balances?
   - What is the caching strategy for balance data?

6. **Multi-Account Balance Check**
   - What is the maximum number of accounts in a single balance query?
   - Should currency conversion be supported?
   - What aggregation options are needed?

7. **Account Access Management**
   - What are all the standard view types and their permissions?
   - Can custom views be created by users or only administrators?
   - What is the notification mechanism for access grants?
   - Are there limits on number of users with access to an account?

8. **Account Search by Routing**
   - What routing schemes must be supported (IBAN, SWIFT, local)?
   - What validation is required for each routing scheme?
   - Should international routing be supported?
   - What are the performance requirements for routing searches?

9. **Settlement Account Management**
   - What are the specific types of settlement accounts?
   - What are the regulatory requirements for settlement accounts?
   - How do settlement accounts integrate with clearing systems?
   - What reconciliation processes are needed?

10. **Account Attribute Management**
    - What are the standard attribute types needed?
    - What validation rules should be supported?
    - Should attributes be versioned?
    - What is the maximum number of attributes per account?
    - How should attribute inheritance work?

### Technical Clarifications Needed

1. **API Version Strategy**
   - Which API version should be prioritized for migration?
   - Should all versions be supported or only latest?
   - What is the deprecation strategy for older versions?

2. **Performance Requirements**
   - What are the specific SLAs for each endpoint?
   - What are the expected transaction volumes?
   - What are the peak load requirements?

3. **Security & Compliance**
   - What are the specific regulatory requirements (PSD2, GDPR, etc.)?
   - What audit logging is required?
   - What data retention policies apply?

4. **Integration Requirements**
   - What core banking systems need to be supported?
   - What are the connector interface requirements?
   - What external systems need integration?

---

## Summary

This document provides comprehensive user stories for all 10 Account Management capabilities identified in the OBP API high-level requirements. Each user story includes:

- Clear business value proposition
- Complete list of API endpoints across all versions
- Detailed acceptance criteria
- Technical context and implementation details
- Business rules extracted from code
- Data validation requirements
- Dependencies and integration points
- Implementation notes and considerations

The user stories cover:
- **High Priority**: Account Listing, Account Detail Retrieval, Balance Inquiry (high-volume, real-time operations)
- **Medium Priority**: Account Creation, Account Update, Multi-Account Balance Check, Account Access Management, Account Search by Routing (medium-volume operations)
- **Low Priority**: Settlement Account Management, Account Attribute Management (low-volume, specialized operations)

All API endpoints from multiple versions (v1.2.1 through v6.0.0) are documented to ensure complete coverage for migration from Scala to Go application.
