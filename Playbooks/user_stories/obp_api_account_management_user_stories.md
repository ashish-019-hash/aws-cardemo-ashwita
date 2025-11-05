# User Stories for OBP-API Account Management

## Part 1: Capability Inventory

### Account Management
1. **Account Listing**
   - Classes/Services: APIMethods510.getAccountsHeldByUserAtBank, APIMethods510.getAccountsHeldByUser, various APIMethods versions
   - Type: REST API
   - Purpose: Enable users to retrieve all bank accounts they have access to
   - Frequency: Real-time, on-demand

2. **Account Detail Retrieval**
   - Classes/Services: APIMethods510.getCoreAccountByIdThroughView, multiple API versions
   - Type: REST API
   - Purpose: Provide comprehensive account information including balance, limits, and attributes
   - Frequency: Real-time, on-demand

3. **Account Creation**
   - Classes/Services: APIMethods400.addAccount, createAccount endpoints across versions
   - Type: REST API
   - Purpose: Create new bank accounts with specified attributes and configurations
   - Frequency: On-demand

4. **Account Update**
   - Classes/Services: APIMethods400.updateAccountLabel, account update endpoints
   - Type: REST API
   - Purpose: Modify account properties such as labels, descriptions, and settings
   - Frequency: On-demand

5. **Balance Inquiry**
   - Classes/Services: APIMethods510.getBankAccountBalances, BalanceNewStyle
   - Type: REST API
   - Purpose: Check current account balance and available funds
   - Frequency: Real-time, high volume

6. **Multi-Account Balance Check**
   - Classes/Services: APIMethods510.getBankAccountsBalances, APIMethods510.getBankAccountsBalancesThroughView
   - Type: REST API
   - Purpose: Retrieve balances for multiple accounts in a single request
   - Frequency: Real-time, medium volume

7. **Account Access Management**
   - Classes/Services: APIMethods510.grantUserAccessToViewById, APIMethods510.revokeUserAccessToViewById, ViewNewStyle
   - Type: REST API
   - Purpose: Grant or revoke user permissions to access specific account views
   - Frequency: On-demand

8. **Account Search by Routing**
   - Classes/Services: APIMethods400.getAccountByAccountRouting, APIMethods400.getAccountsByAccountRoutingRegex
   - Type: REST API
   - Purpose: Locate accounts using routing numbers (ABA, IBAN, etc.)
   - Frequency: Real-time, low-medium volume

9. **Settlement Account Management**
   - Classes/Services: APIMethods400.createSettlementAccount, APIMethods400.getSettlementAccounts
   - Type: REST API
   - Purpose: Manage special settlement accounts used for transaction processing
   - Frequency: On-demand, administrative

10. **Account Attribute Management**
    - Classes/Services: AccountAttributeX, APIMethods600.getAccountAttributesByAccount
    - Type: REST API
    - Purpose: Add, update, and remove custom attributes on accounts
    - Frequency: On-demand

---

## Part 2: Detailed User Stories

### Priority: High

## User Story 1: Account Listing

### Story Overview
**As a** banking application user or API consumer  
**I want to** retrieve a list of all bank accounts I have access to  
**So that** I can view my accounts and select which one to perform operations on

### Acceptance Criteria
1. User can retrieve accounts held at a specific bank
2. User can retrieve accounts held across all banks they have access to
3. Response includes core account information (account ID, bank ID, account type)
4. Results can be filtered by account type (e.g., checking, savings)
5. Only accounts the user has permission to view are returned
6. System handles pagination for users with many accounts
7. Response time is under 2 seconds for typical user account lists

### Technical Context
- **Classes/Services Involved**: 
  - APIMethods510.getAccountsHeldByUserAtBank - retrieves accounts at specific bank
  - APIMethods510.getAccountsHeldByUser - retrieves accounts across all banks
  - ViewNewStyle - manages view permissions
  - JSONFactory300.createCoreAccountsByCoreAccountsJSON - formats response
- **Input Data**: User ID, Bank ID (optional), account type filters (query parameters)
- **Output Data**: JSON array of core account objects with id, bank_id, label, account_type
- **Processing Type**: Real-time REST API

### Business Rules (from code)
1. User must be authenticated to retrieve account lists
2. Only accounts where user has at least one view permission are returned
3. Account type filtering is optional and supports multiple types
4. Results must respect user's entitlements (canGetAccountsHeldAtOneBank or canGetAccountsHeldAtAnyBank)

### Data Validations (if applicable)
- User ID must be valid and exist in system
- Bank ID must be valid if specified
- Account type filter values must match valid account types
- User must have appropriate entitlements for the requested scope

### Dependencies
- **Upstream**: User authentication and authorization
- **Downstream**: Account detail views, transaction retrieval, balance inquiries
- **External Systems**: Bank connector for retrieving actual account data

### Notes for Implementation
- Performance optimization needed for users with large numbers of accounts
- Consider caching account lists with short TTL for frequent requests
- Needs SME Input: Business rules for determining "held" vs "accessible" accounts
- Needs SME Input: Default account type values and filtering logic

---

## User Story 2: Account Detail Retrieval

### Story Overview
**As a** banking application user or API consumer  
**I want to** view comprehensive details about a specific account  
**So that** I can see balance, limits, attributes, and account configuration

### Acceptance Criteria
1. User can retrieve full account details by providing bank ID and account ID
2. Response includes balance information (current balance, available balance)
3. Response includes account metadata (label, account type, currency)
4. Response includes account limits (credit limit, debit limit)
5. Response includes custom attributes if any are defined
6. User must have appropriate view permission to see account details
7. System returns appropriate error if account does not exist or user lacks access

### Technical Context
- **Classes/Services Involved**:
  - APIMethods510.getCoreAccountByIdThroughView - retrieves account with view permission check
  - APIMethods400.getPrivateAccountByIdFull - retrieves private account details
  - NewStyle.function.getBankAccount - fetches account from bank connector
  - AccountAttributeX - retrieves custom attributes
- **Input Data**: Bank ID, Account ID, View ID
- **Output Data**: Complete account JSON with balance, limits, attributes, account holder information
- **Processing Type**: Real-time REST API

### Business Rules (from code)
1. User must have access to at least one view on the account
2. View permissions control which account fields are visible
3. Balance visibility depends on view permissions (CAN_SEE_AVAILABLE_BALANCE)
4. Private information requires elevated view permissions
5. Account attributes are returned only if user has permission to see them

### Data Validations (if applicable)
- Bank ID and Account ID must be valid
- View ID must be valid and exist for the account
- User must have access to the specified view
- Account must exist and be active

### Dependencies
- **Upstream**: User authentication, view permission assignment
- **Downstream**: Transaction retrieval, payment initiation, account updates
- **External Systems**: Bank connector for account data, attribute storage

### Notes for Implementation
- View-based field filtering adds complexity to response generation
- Consider separate endpoints for different detail levels to optimize performance
- Needs SME Input: Definition of "full" account details vs. summary
- Needs SME Input: Business rules for account holder information visibility

---

## User Story 3: Account Creation

### Story Overview
**As a** bank administrator or authorized system  
**I want to** create new bank accounts with specified attributes  
**So that** new customers can be onboarded and begin using banking services

### Acceptance Criteria
1. Authorized user can create account by providing bank ID and account details
2. Account creation requires account type, currency, initial balance, and account holder
3. System generates unique account ID for the new account
4. System creates default views for the account (owner, public, accountant, auditor)
5. Account holder (customer) is linked to the account
6. Custom attributes can be specified during account creation
7. System validates all required fields before creating account
8. Transaction record is created for initial balance deposit

### Technical Context
- **Classes/Services Involved**:
  - APIMethods400.addAccount - creates new account
  - Various API versions have createAccount endpoints
  - BankAccountCreation - data access layer for account creation
  - NewStyle.function.createBankAccount - bank connector call
  - Views - creates default view set
- **Input Data**: Bank ID, account type, currency, initial balance, user ID (account holder), label, branch ID, account routing
- **Output Data**: Complete account JSON with generated account ID and created views
- **Processing Type**: Real-time REST API

### Business Rules (from code)
1. User must have canCreateAccount entitlement
2. Account ID must be unique within the bank
3. Initial balance must be non-negative for most account types
4. Account holder must be a valid customer at the bank
5. Currency must be supported by the bank
6. Default view set is automatically created (owner view mandatory)
7. Account status is set to active by default

### Data Validations (if applicable)
- Bank ID must be valid and active
- Account type must be from approved list (CURRENT, SAVINGS, etc.)
- Currency code must be valid ISO 4217 code
- Initial balance format and precision validated
- Account holder must exist as customer
- Account routing information validated based on type (IBAN, ABA, etc.)

### Dependencies
- **Upstream**: Customer onboarding, KYC verification
- **Downstream**: Account access assignment, initial transactions
- **External Systems**: Bank connector, customer database, view management system

### Notes for Implementation
- Account creation is sensitive operation requiring audit logging
- Consider two-phase creation (pending review then activation) for regulatory compliance
- Needs SME Input: Complete list of required vs. optional fields
- Needs SME Input: Business rules for initial balance limits
- Needs SME Input: Account numbering scheme and format validation

---

## User Story 4: Account Update

### Story Overview
**As a** bank staff member or account owner  
**I want to** modify account properties such as labels and settings  
**So that** account information stays current and accurately reflects account purpose

### Acceptance Criteria
1. Authorized user can update account label/description
2. System validates user has permission to update the account
3. Update operations preserve account ID and core immutable fields
4. System maintains audit trail of account modifications
5. Updates are reflected immediately in subsequent queries
6. System rejects updates to prohibited fields (account number, creation date)

### Technical Context
- **Classes/Services Involved**:
  - APIMethods400.updateAccountLabel - updates account label
  - NewStyle.function.updateBankAccount - updates account in connector
  - MappedBankAccount - persistence layer
- **Input Data**: Bank ID, Account ID, updated fields (label, description, etc.)
- **Output Data**: Updated account JSON
- **Processing Type**: Real-time REST API

### Business Rules (from code)
1. User must have owner view or canUpdateAccount entitlement
2. Only specific fields are mutable (label, description, tags)
3. Core account fields (account ID, type, currency, creation date) are immutable
4. Account status changes require separate workflow
5. Updates to financial fields (balance, limits) require elevated permissions

### Data Validations (if applicable)
- Account must exist and be accessible
- Updated field values must meet format requirements
- Label length must be within limits (Needs SME Input: specific limits)
- User must have appropriate permissions

### Dependencies
- **Upstream**: Account access permissions, user authentication
- **Downstream**: Account display, reporting, search indexing
- **External Systems**: Bank connector, audit logging system

### Notes for Implementation
- Limited update scope suggests simple validation logic
- Consider supporting bulk update for efficiency
- Needs SME Input: Complete list of updatable fields and business rules for each
- Needs SME Input: Whether updates require approval workflow

---

## User Story 5: Balance Inquiry

### Story Overview
**As a** banking application user  
**I want to** check my current account balance  
**So that** I know how much money I have available for spending or transfer

### Acceptance Criteria
1. User can retrieve balance for specific account by providing bank ID and account ID
2. Response includes current balance (ledger balance)
3. Response includes available balance (current balance minus holds/reserves)
4. Balance amounts include currency code
5. Response time is under 1 second for typical requests
6. System supports high request volume during peak hours
7. User must have view permission that includes balance visibility

### Technical Context
- **Classes/Services Involved**:
  - APIMethods510.getBankAccountBalances - retrieves account balances
  - BalanceNewStyle.getBankAccountBalances - balance retrieval logic
  - JSONFactory400.createAccountBalancesJson - formats response
- **Input Data**: Bank ID, Account ID
- **Output Data**: JSON with current_balance, available_balance, currency
- **Processing Type**: Real-time REST API, high volume

### Business Rules (from code)
1. User must be authenticated
2. User must have view with CAN_SEE_AVAILABLE_BALANCE permission
3. Current balance = sum of all posted transactions
4. Available balance = current balance - holds - minimum balance requirement
5. Negative balances are allowed for accounts with overdraft
6. Balance precision matches currency decimal places

### Data Validations (if applicable)
- Bank ID and Account ID must be valid
- Account must exist and be active
- User must have view access with balance permission

### Dependencies
- **Upstream**: Transaction posting, hold management
- **Downstream**: Payment authorization, overdraft decisions
- **External Systems**: Bank connector for real-time balance calculation

### Notes for Implementation
- High-traffic endpoint requiring caching strategy
- Consider read replicas for balance queries
- Balance calculation must be consistent with transaction ledger
- Needs SME Input: Caching policy and staleness tolerance
- Needs SME Input: Handling of pending transactions in available balance

---

## User Story 6: Multi-Account Balance Check

### Story Overview
**As a** banking application user or aggregation service  
**I want to** retrieve balances for multiple accounts in a single request  
**So that** I can efficiently display summary information across all my accounts

### Acceptance Criteria
1. User can request balances for multiple accounts at once
2. Request can specify accounts across multiple banks
3. Response includes balance for each requested account
4. System handles partial failures (some accounts accessible, others not)
5. Response indicates which accounts were successfully retrieved
6. Performance scales efficiently with number of requested accounts
7. Each account balance respects individual view permissions

### Technical Context
- **Classes/Services Involved**:
  - APIMethods510.getBankAccountsBalances - multi-account balance retrieval
  - APIMethods510.getBankAccountsBalancesThroughView - with view filtering
  - BalanceNewStyle - balance calculation
- **Input Data**: List of bank ID and account ID pairs, view ID (optional)
- **Output Data**: JSON array of account balance objects
- **Processing Type**: Real-time REST API

### Business Rules (from code)
1. User must have access to requested accounts
2. Accounts without proper permissions are excluded from response
3. Response includes only successfully retrieved balances
4. Request is not atomic - partial success is acceptable
5. Maximum number of accounts per request may be limited (Needs SME Input)

### Data Validations (if applicable)
- Each bank ID and account ID pair must be well-formed
- User must have appropriate view access for each account
- Request size must be within acceptable limits

### Dependencies
- **Upstream**: Account access permissions, user authentication
- **Downstream**: Dashboard display, reporting, analytics
- **External Systems**: Bank connector for balance data

### Notes for Implementation
- Performance optimization critical for this endpoint
- Consider parallel processing for multiple account queries
- Needs SME Input: Maximum number of accounts per request
- Needs SME Input: Timeout handling for slow account queries
- Needs SME Input: Error handling strategy for partial failures

---

## User Story 7: Account Access Management

### Story Overview
**As a** bank administrator or account owner  
**I want to** grant or revoke user access to specific account views  
**So that** I can control who can see and operate on my accounts

### Acceptance Criteria
1. Account owner can grant view access to another user by specifying user ID and view ID
2. Account owner can revoke existing view access from a user
3. System validates user has authority to modify access (owner view or admin entitlement)
4. Access changes take effect immediately
5. System maintains audit trail of all access changes
6. Granting access to non-existent user or view returns appropriate error
7. Revoking non-existent access permission completes successfully

### Technical Context
- **Classes/Services Involved**:
  - APIMethods510.grantUserAccessToViewById - grants view access
  - APIMethods510.revokeUserAccessToViewById - revokes view access
  - APIMethods510.createUserWithAccountAccessById - creates user and grants access
  - ViewNewStyle - view permission management
  - Views.views.vend.addPermission - persistence
- **Input Data**: Bank ID, Account ID, View ID, User ID (for grant/revoke)
- **Output Data**: Success/failure status, updated view information
- **Processing Type**: Real-time REST API

### Business Rules (from code)
1. Granting user must have owner view or canCreateUserAccountAccess entitlement
2. View ID must be valid for the specified account
3. User being granted access must exist in the system
4. Cannot revoke owner's own owner view access
5. System view access may have additional restrictions
6. Some views (e.g., owner) may be restricted to account holder only

### Data Validations (if applicable)
- Bank ID, Account ID, View ID must all be valid
- User ID must correspond to existing user
- Requesting user must have appropriate authority
- View type must support the requested access operation

### Dependencies
- **Upstream**: User management, view definition, account ownership
- **Downstream**: All account and transaction operations that check permissions
- **External Systems**: User directory, audit logging

### Notes for Implementation
- Critical security operation requiring comprehensive audit logging
- Consider notification to user when they receive new account access
- Needs SME Input: Business rules for who can grant/revoke each view type
- Needs SME Input: Approval workflow requirements for sensitive views
- Needs SME Input: Maximum number of users per account view

---

## User Story 8: Account Search by Routing

### Story Overview
**As a** bank staff member or payment system  
**I want to** locate accounts using routing numbers (ABA, IBAN, sort code)  
**So that** I can identify the correct account for incoming payments or inquiries

### Acceptance Criteria
1. User can search for account by providing bank ID and account routing information
2. System supports multiple routing schemes (IBAN, ABA number, sort code, account number)
3. Exact match search returns single account if found
4. Regex pattern search returns multiple matching accounts
5. Search results respect user's view permissions
6. System returns appropriate error if no accounts match or user lacks access
7. Response includes enough information to identify the correct account

### Technical Context
- **Classes/Services Involved**:
  - APIMethods400.getAccountByAccountRouting - exact match search
  - APIMethods400.getAccountsByAccountRoutingRegex - pattern search
  - NewStyle.function.getBankAccountByRouting - bank connector query
- **Input Data**: Bank ID, routing scheme, routing address (or regex pattern)
- **Output Data**: Account JSON or array of accounts
- **Processing Type**: Real-time REST API

### Business Rules (from code)
1. User must have appropriate entitlement (canSearchAccountByRouting)
2. Routing information must include scheme (iban, account_number, etc.) and address
3. Routing information is validated against scheme-specific format
4. Search is case-insensitive for most routing schemes
5. Regex search requires elevated permissions
6. Results filtered by user's view access

### Data Validations (if applicable)
- Routing scheme must be recognized (IBAN, AccountNumber, etc.)
- Routing address must conform to scheme format (e.g., IBAN checksum)
- Bank ID must be valid
- User must have search entitlement
- Regex pattern must be valid regex syntax

### Dependencies
- **Upstream**: Account creation with routing information
- **Downstream**: Payment processing, account linking
- **External Systems**: Bank connector, routing validation services

### Notes for Implementation
- IBAN validation includes checksum verification
- Consider indexing strategy for efficient routing lookups
- Needs SME Input: Complete list of supported routing schemes
- Needs SME Input: Format validation rules for each routing scheme
- Needs SME Input: Security considerations for regex search

---

## User Story 9: Settlement Account Management

### Story Overview
**As a** bank operations staff or payment system administrator  
**I want to** create and manage settlement accounts  
**So that** the bank can process transaction settlements and reconciliation

### Acceptance Criteria
1. Authorized user can create settlement accounts with special attributes
2. Settlement accounts can be designated as RELEASER accounts or HOLDING accounts
3. System can retrieve all settlement accounts for a bank
4. Settlement accounts can be linked to other accounts via attributes
5. Holding accounts can be located by their associated releaser account ID
6. Settlement accounts support special processing rules different from customer accounts
7. Access to settlement accounts is restricted to authorized personnel

### Technical Context
- **Classes/Services Involved**:
  - APIMethods400.createSettlementAccount - creates settlement account
  - APIMethods400.getSettlementAccounts - retrieves all settlement accounts
  - APIMethods600 - manages holding account relationships
  - AccountAttributeX - stores account relationships (RELEASER_ACCOUNT_ID attribute)
- **Input Data**: Bank ID, account type (SETTLEMENT/HOLDING), associated account IDs, attributes
- **Output Data**: Settlement account JSON with account ID, type, links, attributes
- **Processing Type**: Real-time REST API

### Business Rules (from code)
1. User must have canGetSettlementAccountAtOneBank entitlement
2. Settlement accounts have account_type = "SETTLEMENT" or "HOLDING"
3. Holding accounts must be linked to a releaser account via RELEASER_ACCOUNT_ID attribute
4. Settlement accounts may have special balance and transaction rules
5. First matching holding account is returned when searching by releaser
6. Settlement accounts are not visible in regular account lists

### Data Validations (if applicable)
- Bank ID must be valid
- Account type must be SETTLEMENT or HOLDING
- For holding accounts, releaser account ID must be valid
- User must have settlement account entitlement
- Attribute values must be properly formatted

### Dependencies
- **Upstream**: Payment processing configuration, bank setup
- **Downstream**: Transaction settlement processing, reconciliation
- **External Systems**: Bank connector, payment clearing systems

### Notes for Implementation
- Settlement accounts have specialized accounting rules
- Integration with payment clearing and settlement systems
- Needs SME Input: Complete business rules for settlement vs. holding accounts
- Needs SME Input: Linking rules between releaser and holding accounts
- Needs SME Input: Settlement account lifecycle management

---

## User Story 10: Account Attribute Management

### Story Overview
**As a** bank administrator or system integrator  
**I want to** add, update, and remove custom attributes on accounts  
**So that** I can store additional metadata and configuration specific to my bank's needs

### Acceptance Criteria
1. Authorized user can add custom attributes to accounts
2. Attributes have name, value, and type (string, number, date)
3. User can update existing attribute values
4. User can remove attributes from accounts
5. System can retrieve all attributes for a specific account
6. System can search for accounts by attribute values
7. Attribute operations preserve data type integrity

### Technical Context
- **Classes/Services Involved**:
  - AccountAttributeX - account attribute management
  - APIMethods600.getAccountAttributesByAccount - retrieves attributes
  - APIMethods600.getAccountIdsByParams - searches by attributes
  - Multiple API versions have create/update/delete attribute endpoints
- **Input Data**: Bank ID, Account ID, attribute name, attribute value, attribute type
- **Output Data**: Attribute JSON with name, value, type, or list of attributes
- **Processing Type**: Real-time REST API

### Business Rules (from code)
1. User must have appropriate entitlement (varies by operation)
2. Attribute names must be unique per account
3. Attribute types include STRING, INTEGER, DOUBLE, DATE_WITH_DAY
4. Some attribute names may be reserved (e.g., RELEASER_ACCOUNT_ID)
5. Attributes can be used to link accounts (e.g., holding to releaser)
6. Attribute search returns list of matching account IDs

### Data Validations (if applicable)
- Attribute name must be non-empty string
- Attribute value must match specified type
- For dates, format must be YYYY-MM-DD
- For numbers, value must be parseable
- Attribute name length within limits

### Dependencies
- **Upstream**: Account creation, attribute schema definition
- **Downstream**: Account search, settlement account linking, custom business logic
- **External Systems**: Bank connector, attribute storage

### Notes for Implementation
- Flexible schema allows custom bank-specific extensions
- Consider schema registry for attribute definitions
- Needs SME Input: Naming conventions for custom attributes
- Needs SME Input: List of reserved attribute names
- Needs SME Input: Validation rules for attribute values
- Needs SME Input: Maximum number of attributes per account

---

## Part 3: Open Questions

### Business Logic Questions
1. What is the complete list of account types supported (CURRENT, SAVINGS, etc.) and their specific business rules?
2. What are the exact balance calculation rules, especially for available balance (holds, minimum balance, pending transactions)?
3. What workflow approvals are required for account creation, updates, and access changes?
4. What are the retention and archival policies for account data?
5. What are the business rules for account closure and reactivation?

### Data & Validation Questions
6. What are the exact field length limits for account labels, descriptions, and attributes?
7. What are the format requirements for each account routing scheme (beyond IBAN)?
8. What is the maximum number of accounts a user can hold?
9. What is the maximum number of custom attributes per account?
10. What are the account numbering schemes and how are they validated?

### Integration Questions
11. How do account operations integrate with core banking systems?
12. What real-time vs. batch processing is used for account data synchronization?
13. How are account balances calculated when distributed across multiple systems?
14. What external systems need to be notified of account changes?

### Security & Performance Questions
15. What are the specific role/entitlement requirements for each operation?
16. What audit logging is required for account operations?
17. What are the performance SLAs for high-volume endpoints (balance inquiry)?
18. What caching strategies are acceptable for account and balance data?
19. What rate limiting is applied to account operations?

---

## Document Metadata

**Generated**: November 5, 2025  
**Source System**: Open Bank Project (OBP) API  
**Scope**: Account Management capabilities (10 user stories)  
**Based on**: OBP-API Scala codebase analysis  
**Purpose**: Requirements elaboration and development planning

### Next Steps
1. Review user stories with business stakeholders to validate business rules
2. Confirm technical implementation details with development team
3. Resolve open questions through SME interviews
4. Break down user stories into implementable tasks
5. Define acceptance test scenarios for each story
6. Prioritize stories for development sprints

### Related Documentation
- OBP-API High-Level Requirements Document
- API Documentation (ResourceDoc annotations)
- View Permission Matrix
- Account Type Configuration Guide (Needs Creation)
- Routing Scheme Validation Rules (Needs Creation)
