# Business Entity Extraction - Account Detail Retrieval User Story

Applied the entity extraction prompt to the Account Detail Retrieval user story from the OBP-API Account Management documentation.

## Context

This entity extraction analysis is based on the **Open Bank Project (OBP-API)**, the open-source banking API platform.

- **Official Repository**: https://github.com/OpenBankProject/OBP-API.git
- **Technology Stack**: Scala-based REST API
- **Purpose**: Open-source API for banks that enables account holders to interact with their bank using a standard RESTful interface

All classes, methods, and services referenced in this document are from the Open Bank Project OBP-API codebase.

---

## Entity Catalog - Account Detail Retrieval User Story

Based on the Account Detail Retrieval user story, the following business entities have been identified:

### 1. **User** (Banking Application User / Account Holder)

**Business Description**: 
A person or system that uses the banking application and has authenticated access to the API. This represents the actor who wants to view comprehensive information about a specific account they have access to.

**Location in Codebase** (Open Bank Project OBP-API): 
- Referenced in API endpoints across multiple versions (v3.0.0, v4.0.0, v5.1.0)
- Used in authentication context for all account detail retrieval operations
- Part of the user management and authentication system

**Key Fields**:
- User ID: Unique identifier for the user (implicit in authentication context)
- Authentication credentials: Validates user identity
- View permissions: Specific views the user has on accounts
- User type: Distinguishes between account holders and other users

**Relationships to Other Entities**:
- A User can have access to multiple Bank Accounts
- A User has specific View permissions on each accessible Account
- A User's authentication context determines what account information they can access

**Business Rules**:
- User must be authenticated to retrieve account details
- User must have view permission to access account details
- Different views expose different levels of account information to the user
- View permissions are checked before returning data

**Notes**:
- The user story focuses on viewing comprehensive account information
- Authentication and view permissions are critical prerequisites
- User context determines the level of detail returned in the response

---

### 2. **Bank Account** (Core Banking Entity)

**Business Description**: 
A financial account where money is stored and managed. This is the primary entity being retrieved in this user story. The account contains comprehensive information including balances, limits, routing information, and other account details.

**Location in Codebase** (Open Bank Project OBP-API): 
- Classes: APIMethods510.getCoreAccountByIdThroughView, APIMethods400.getCoreAccountById, APIMethods400.getPrivateAccountByIdFull
- Domain Model: BankAccount (domain model for account data)
- JSON Factory: JSONFactory (formats account response)
- Multiple API versions provide different levels of account detail

**Key Fields**:
- id: Unique identifier for the account
- bank_id: Identifier of the bank where the account is held
- label: Human-readable name or description of the account
- number: Account number
- balance: Current account balance (current as of request time)
- currency: The currency in which the account operates
- account_type: Category of account (checking, savings, etc.)
- account_routings: Routing information (IBAN, account number, etc.)
- limits: Account limits and restrictions
- description: Additional account description
- status: Active/inactive state

**Relationships to Other Entities**:
- A Bank Account belongs to one Bank
- A Bank Account has one or more Views that control access permissions
- A Bank Account has a specific Account Type
- A Bank Account has Account Routings
- A Bank Account has Balance information
- A Bank Account has Limits

**Business Rules**:
- User must have view permission to access account details
- Different views expose different levels of account information
- Owner view provides full account access
- Public view provides limited account information
- Account must exist and be active
- Bank ID and Account ID must match
- Sensitive information is filtered based on view permissions
- Account balance is current as of request time
- Account routing information follows banking standards

**Notes**:
- This is the central entity in the Account Detail Retrieval user story
- Multiple API endpoints provide different levels of detail (core, private, public)
- Response format varies by API version
- Full account details include balance, limits, and comprehensive metadata

---

### 3. **Bank** (Financial Institution)

**Business Description**: 
A financial institution that holds and manages customer accounts. The bank entity represents the organization providing banking services. Bank ID is required to retrieve account details.

**Location in Codebase** (Open Bank Project OBP-API): 
- Referenced in all API endpoint paths as BANK_ID parameter
- Part of the core banking domain model in OBP-API
- Used for account validation and routing

**Key Fields**:
- Bank ID: Unique identifier for the bank (required in all API endpoints)
- Bank name: Name of the financial institution (implied)
- Status: Active/inactive (implied)

**Relationships to Other Entities**:
- A Bank has multiple Bank Accounts
- Bank ID is required to retrieve account details
- Account must belong to the specified bank

**Business Rules**:
- Bank ID must be valid and exist
- Account must belong to the specified bank
- Bank ID and Account ID must match

**Notes**:
- Bank ID is a required path parameter in all account detail retrieval endpoints
- Validation ensures account belongs to the specified bank
- Critical for multi-bank environments

---

### 4. **View / View Permission** (Access Control Entity)

**Business Description**: 
A view represents a specific level of access to account information. It controls what fields and operations a user can perform on an account. Views implement granular access control and determine what account details are visible to the user. Different views expose different levels of account information.

**Location in Codebase** (Open Bank Project OBP-API): 
- Class: ViewNewStyle.checkViewAccessAndReturnView (validates view permissions)
- Referenced in API endpoints as VIEW_ID parameter (in some endpoints)
- Part of OBP-API's view-based access control system
- Critical for data privacy and access control

**Key Fields**:
- View ID: Unique identifier for the view (required in some endpoints)
- View name: Descriptive name (e.g., "owner", "public", "accountant")
- Permissions: Specific capabilities and fields granted by this view
- Account reference: Which account this view applies to
- User reference: Which user has this view

**Relationships to Other Entities**:
- A View is associated with one Bank Account
- A View grants specific permissions to a User
- A User must have a specific View to access account details
- Different Views expose different levels of account information

**Business Rules**:
- User must have view permission to access account details
- Different views expose different levels of account information
- Owner view provides full account access
- Public view provides limited account information
- View permissions are checked before returning data
- Sensitive information is filtered based on view permissions
- View ID must be valid for the account (when required)
- User must have permission for the specified view

**Notes**:
- Views are a key security/access control mechanism in OBP-API
- Some endpoints require explicit VIEW_ID parameter, others infer from user permissions
- View-based access control is critical for data privacy
- Different API versions handle views differently

---

### 5. **Account Balance** (Financial Data)

**Business Description**: 
The current monetary balance of a bank account. This represents the amount of money currently available in the account and is a critical piece of information for account holders.

**Location in Codebase** (Open Bank Project OBP-API): 
- Included in account detail response
- Part of BankAccount domain model
- Retrieved from core banking system via connector

**Key Fields**:
- Amount: The monetary value of the balance
- Currency: The currency of the balance
- Timestamp: When the balance was calculated (current as of request time)

**Relationships to Other Entities**:
- Balance belongs to one Bank Account
- Balance is in a specific Currency
- Balance visibility is controlled by View permissions

**Business Rules**:
- Account balance is included in the response (when view permits)
- Account balance is current as of request time
- Balance visibility depends on view permissions
- Balance is returned in the account's currency

**Notes**:
- Critical financial information for account holders
- Real-time balance retrieval from core banking system
- View permissions control whether balance is visible

---

### 6. **Account Limits** (Account Constraints)

**Business Description**: 
Restrictions and limits placed on a bank account, such as withdrawal limits, overdraft limits, or transaction limits. These constraints govern what operations can be performed on the account.

**Location in Codebase** (Open Bank Project OBP-API): 
- Included in account detail response
- Part of comprehensive account information
- Defines account operational boundaries

**Key Fields**:
- Limit type: Type of limit (withdrawal, overdraft, transaction, etc.)
- Limit amount: The monetary or quantitative limit value
- Limit currency: Currency for monetary limits
- Limit period: Time period for the limit (daily, monthly, etc.)

**Relationships to Other Entities**:
- Limits belong to one Bank Account
- Limits are in a specific Currency (for monetary limits)
- Limits visibility is controlled by View permissions

**Business Rules**:
- Account limits and restrictions are displayed (when view permits)
- Limits define operational boundaries for the account
- Limits visibility depends on view permissions

**Notes**:
- Important for account holders to understand account constraints
- May include multiple types of limits
- View permissions control whether limits are visible

---

### 7. **Account Routing** (Banking Reference Data)

**Business Description**: 
Routing information associated with a bank account that enables external systems to identify and route transactions to the correct account. This includes information like IBAN, account numbers, routing numbers, and other identifiers used in payment processing.

**Location in Codebase** (Open Bank Project OBP-API): 
- Returned as part of account detail information
- Field name: account_routings
- Part of comprehensive account response

**Key Fields**:
- Routing scheme: Type of routing (e.g., IBAN, AccountNumber, RoutingNumber)
- Routing address: The actual routing value
- Account reference: Which account this routing belongs to

**Relationships to Other Entities**:
- Account Routings belong to one Bank Account
- A Bank Account can have multiple Account Routings (different routing schemes)

**Business Rules**:
- Account routing information (IBAN, account number) is provided (when view permits)
- Multiple routing schemes can exist for a single account
- Routing information follows banking standards
- Routing visibility depends on view permissions

**Notes**:
- Essential for payment processing and account identification
- Included in the account detail response for integration purposes
- Supports multiple routing schemes per account

---

### 8. **Account Type** (Reference Data)

**Business Description**: 
A classification or category that defines the nature and purpose of a bank account. Common types include checking accounts, savings accounts, credit accounts, loan accounts, etc.

**Location in Codebase** (Open Bank Project OBP-API): 
- Included in account detail response
- Part of core account information

**Key Fields**:
- Type code: Unique identifier for the account type
- Type name: Descriptive name (CURRENT, SAVINGS, CREDIT, LOAN, etc.)
- Type description: Detailed explanation of the account type

**Relationships to Other Entities**:
- A Bank Account has one Account Type
- Account Type defines the nature and purpose of the account

**Business Rules**:
- Account type is included in the response
- Account type defines the nature and purpose of the account

**Notes**:
- Acts as reference/lookup data
- Used for categorization and understanding account purpose
- Important for account holders to understand account nature

---

### 9. **Currency** (Reference Data)

**Business Description**: 
The monetary currency in which the account operates. This defines the unit of money used for all account transactions and balances.

**Location in Codebase** (Open Bank Project OBP-API): 
- Included in account detail response
- Part of core account information
- Used for balance and limit values

**Key Fields**:
- Currency code: ISO 4217 currency code (e.g., USD, EUR, GBP)
- Currency name: Full name of the currency
- Currency symbol: Symbol representation (e.g., $, €, £)

**Relationships to Other Entities**:
- A Bank Account operates in one Currency
- Account Balance is denominated in the account's Currency
- Account Limits (monetary) are in the account's Currency

**Business Rules**:
- Currency is included in the response
- Currency defines the unit of money for the account
- Currency follows ISO 4217 standard

**Notes**:
- Acts as reference/lookup data
- Critical for international banking and multi-currency support
- Determines how monetary values are interpreted

---

## Entity Relationship Map

The Account Detail Retrieval user story reveals the following key relationships:

1. **User ← → Bank Account**: A user can have access to multiple bank accounts through view permissions

2. **Bank → Bank Account**: A bank contains multiple bank accounts (one-to-many)

3. **Bank Account → Account Type**: Each bank account has one account type (many-to-one)

4. **Bank Account → Currency**: Each bank account operates in one currency (many-to-one)

5. **Bank Account → Account Balance**: Each bank account has one current balance (one-to-one)

6. **Bank Account → Account Limits**: Each bank account can have multiple limits (one-to-many)

7. **Bank Account → Account Routing**: Each bank account has one or more account routings (one-to-many)

8. **User ← → View**: Users are granted specific views on accounts (many-to-many through view permissions)

9. **View → Bank Account**: Each view is associated with a specific bank account

10. **Account Balance → Currency**: Balance is denominated in a specific currency

11. **Account Limits → Currency**: Monetary limits are denominated in a specific currency

**Access Control Flow**:
- User authentication validates user identity
- View permissions determine what account information is accessible
- Different views expose different levels of account information (owner, public, etc.)
- Sensitive information is filtered based on view permissions

**Data Flow**:
1. User makes API request with BANK_ID, ACCOUNT_ID, and optionally VIEW_ID
2. System validates user authentication
3. System checks user's view permissions on the account
4. System retrieves account data from core banking system via connector
5. System filters account information based on view permissions
6. System formats response using JSONFactory
7. Response includes: id, bank_id, label, number, balance, currency, account_type, account_routings, limits, description (based on view permissions)
8. Sensitive information is filtered based on view permissions

---

## Business Domain Summary

The Account Detail Retrieval user story covers the following business domains:

1. **Account Management**: Core functionality for retrieving comprehensive account information
2. **Access Control & Security**: View-based permission system for granular data access control
3. **Financial Data Management**: Balance and limit information for account holders
4. **Banking Integration**: Account routing information for payment processing and external system integration
5. **Reference Data Management**: Account types and currencies as categorization dimensions

**Key Business Capabilities**:
- Core account detail retrieval (GET /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views/{VIEW_ID})
- Core account retrieval by ID (GET /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID})
- Full private account detail retrieval (GET /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/account)
- Private account detail through view (GET /obp/v3.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/account)
- Public account information retrieval (GET /obp/v3.0.0/my/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/account)
- View-based access control for data privacy
- Real-time account detail access with sub-2-second response time
- Balance and limit information retrieval

**Technical Integration Points**:
- APIMethods510.getCoreAccountByIdThroughView (v5.1.0 endpoint)
- APIMethods400.getCoreAccountById (v4.0.0 core endpoint)
- APIMethods400.getPrivateAccountByIdFull (v4.0.0 full details endpoint)
- ViewNewStyle.checkViewAccessAndReturnView (permission validation)
- BankAccount (domain model)
- JSONFactory (response formatting)
- Core banking system connector (data retrieval)

---

## Questions & Uncertainties

1. **View Permission Hierarchy**: What is the complete hierarchy of views (owner, public, accountant, etc.)? What specific fields does each view expose? This is critical for implementing view-based filtering correctly.

2. **API Version Differences**: What are the exact differences between the five API endpoints in terms of response format and data included? The user story mentions different API versions provide different levels of detail, but specifics are needed.

3. **Balance Freshness**: The user story states "account balance is current as of request time" - is this a real-time call to the core banking system, or is there caching involved? What is the acceptable staleness for balance data?

4. **Limit Types**: What is the complete list of limit types supported (withdrawal, overdraft, transaction, etc.)? Are these standardized or bank-specific?

5. **Routing Schemes**: What routing schemes are supported in the account_routings field? Are there standard schemes (IBAN, SWIFT, ACH, etc.) or is this extensible?

6. **Sensitive Information Filtering**: What specific fields are considered "sensitive" and filtered based on view permissions? Is there a documented mapping of fields to view permissions?

7. **Error Handling**: What specific error codes and messages are returned for:
   - Invalid account ID
   - Invalid bank ID
   - Invalid view ID
   - User without view permission
   - Account doesn't belong to specified bank

8. **Response Format Variations**: How does the response format vary by API version? Are there breaking changes between versions that need to be handled?

9. **Caching Strategy**: The implementation notes mention "caching strategy should consider data freshness requirements" - what is the recommended caching approach for account details? What TTL is appropriate?

10. **Audit Logging**: The implementation notes mention "audit logging required for account access" - what specific information should be logged? Is this for compliance purposes (e.g., GDPR, PCI-DSS)?

11. **Account Status**: What are the valid account statuses (active, inactive, closed, frozen, etc.)? How does account status affect data retrieval?

12. **Public vs. Private Endpoints**: What is the difference between the "public" endpoint (GET /obp/v3.0.0/my/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/account) and the other endpoints? Does it have different authentication requirements?

---

## Additional Implementation Considerations

Based on the entity analysis, here are critical implementation points for the Go migration:

1. **Data Model Preservation**: All nine entities (User, Bank Account, Bank, View, Account Balance, Account Limits, Account Routing, Account Type, Currency) must be represented in the Go application with their complete field sets and relationships.

2. **View-Based Access Control**: The view-based permission system must be faithfully replicated in Go, as this is core to the system's security and data privacy model:
   - View permission checking must happen before any data retrieval
   - Different views must expose different levels of account information
   - Sensitive information filtering must be applied based on view permissions
   - Owner view provides full access, public view provides limited access

3. **API Endpoint Mapping**: The Scala endpoints must have exact equivalents in Go with identical behavior:
   - **Endpoint 1**: GET /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views/{VIEW_ID}
     - Method: getCoreAccountByIdThroughView
     - Entitlements: View-specific permissions
   - **Endpoint 2**: GET /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}
     - Method: getCoreAccountById
     - Entitlements: Account access permissions
   - **Endpoint 3**: GET /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/account
     - Method: getPrivateAccountByIdFull
     - Entitlements: Owner or specific view permissions
   - **Endpoint 4**: GET /obp/v3.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/account
     - Method: getPrivateAccountById
     - Entitlements: View-specific permissions
   - **Endpoint 5**: GET /obp/v3.0.0/my/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/account
     - Method: getPublicAccountById
     - Entitlements: Public view access
   - URL patterns must match exactly
   - Request/response formats must be identical
   - Error codes and messages must match
   - Business rule enforcement must be consistent

4. **Response Format Handling**: The Go implementation must support multiple API versions with different response formats:
   - Different API versions provide different levels of detail
   - Response format varies by API version
   - Field inclusion/exclusion based on view permissions
   - Proper JSON formatting using appropriate factories/serializers

5. **Performance Requirements**: The Go implementation must meet strict performance criteria:
   - Response time under 2 seconds for account detail retrieval
   - Efficient view permission checking
   - Optimized database queries for account data retrieval
   - Efficient connector calls to core banking system
   - Appropriate caching strategy considering data freshness requirements

6. **Balance and Limit Retrieval**: Implement real-time balance and limit retrieval:
   - Account balance is current as of request time
   - Balance retrieved from core banking system
   - Limits retrieved and included in response
   - Balance and limit visibility controlled by view permissions

7. **Error Handling**: Implement comprehensive error handling for:
   - Invalid account ID (appropriate error message)
   - Invalid bank ID (appropriate error message)
   - Invalid view ID (appropriate error message)
   - User without view permission (authorization error)
   - Account doesn't belong to specified bank (validation error)
   - Core banking system connector failures (graceful degradation or error)

8. **Test Coverage**: Test cases should verify:
   - **View-based access control**:
     - User with owner view can access full account details
     - User with public view can access limited account details
     - User without view permission receives authorization error
     - Sensitive information is filtered based on view permissions
   - **Multiple API endpoints**:
     - Each of the 5 endpoints returns appropriate data
     - Response format matches API version expectations
     - Different levels of detail are provided correctly
   - **Account validation**:
     - Invalid account ID returns error
     - Invalid bank ID returns error
     - Account must belong to specified bank
   - **Balance and limit retrieval**:
     - Balance is included in response (when view permits)
     - Limits are included in response (when view permits)
     - Balance is current as of request time
   - **Routing information**:
     - Account routing information is included (when view permits)
     - Multiple routing schemes are supported
   - **Edge cases**:
     - Account with no balance
     - Account with no limits
     - Account with multiple routing schemes
     - Different account types
     - Different currencies

9. **Integration Points**: Ensure proper integration with:
   - Authentication system (user validation)
   - Authorization system (view permission checks)
   - Core banking connector (account data retrieval, balance retrieval)
   - JSON response formatter (version-specific formatting)
   - Audit logging system (access tracking for compliance)

10. **Data Validation**: Implement validation for:
    - Bank ID format and existence
    - Account ID format and existence
    - View ID format and validity (when required)
    - User authentication context
    - Account belongs to specified bank
    - User has permission for specified view

11. **Sensitive Information Filtering**: Implement view-based filtering:
    - Define which fields are sensitive
    - Map fields to view permissions
    - Filter response based on user's view
    - Ensure no sensitive data leakage

12. **Audit Logging**: Implement comprehensive audit logging:
    - Log all account detail access requests
    - Include user ID, account ID, view ID, timestamp
    - Log for compliance purposes (GDPR, PCI-DSS, etc.)
    - Ensure audit trail is tamper-proof

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
- Section: Part 2: Detailed user stories - User Story 2: Account Detail Retrieval (lines 150-238)
- Note: This user story documents the Account Detail Retrieval functionality from the Open Bank Project OBP-API

**Extraction Methodology**: 
- Based on entity extraction prompt methodology
- Systematic analysis of user story components
- Identification of entities, relationships, and business rules
- Focus on data model and business logic preservation for migration

**Key Classes/Services Referenced** (from OBP-API codebase):
- APIMethods510.getCoreAccountByIdThroughView
- APIMethods400.getCoreAccountById
- APIMethods400.getPrivateAccountByIdFull
- ViewNewStyle.checkViewAccessAndReturnView
- BankAccount (domain model)
- JSONFactory (response formatting)
- Core banking system connector

---

## Summary

This entity extraction analysis identifies **nine core business entities** involved in the Account Detail Retrieval user story:

1. **User** - The authenticated actor requesting account information
2. **Bank Account** - The primary entity being retrieved with comprehensive details
3. **Bank** - The financial institution holding the account
4. **View/View Permission** - Fine-grained access control mechanism
5. **Account Balance** - Current monetary balance of the account
6. **Account Limits** - Restrictions and limits on the account
7. **Account Routing** - Banking reference data for payment processing
8. **Account Type** - Classification of the account
9. **Currency** - Monetary currency for the account

The analysis reveals a sophisticated **view-based access control system** that filters account information based on user permissions. Different views (owner, public, etc.) expose different levels of account information, with sensitive data filtered based on view permissions.

The user story involves **five different API endpoints** across three API versions (v3.0.0, v4.0.0, v5.1.0), each providing different levels of account detail. The Go migration must support all five endpoints with identical behavior and response formats.

Key implementation challenges include:
- View-based access control and sensitive information filtering
- Multiple API versions with different response formats
- Real-time balance retrieval from core banking system
- Comprehensive error handling for various validation scenarios
- Audit logging for compliance purposes

The entity relationships, business rules, and data flows documented here provide the foundation for implementing the Account Detail Retrieval functionality in Go while maintaining exact functional equivalence with the Scala-based OBP-API implementation.

---

This analysis extracts all business entities explicitly mentioned or implied in the Account Detail Retrieval user story from the **Open Bank Project (OBP-API)**, following a systematic approach to identify entities, relationships, business rules, and implementation considerations. All classes, methods, and services referenced are from the official OBP-API repository at https://github.com/OpenBankProject/OBP-API.git.
