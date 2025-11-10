# Business Entity Extraction - Account Creation User Story

Applied the entity extraction prompt to the Account Creation user story from the OBP-API Account Management documentation.

## Context

This entity extraction analysis is based on the **Open Bank Project (OBP-API)**, the open-source banking API platform.

- **Official Repository**: https://github.com/OpenBankProject/OBP-API.git
- **Technology Stack**: Scala-based REST API
- **Purpose**: Open-source API for banks that enables account holders to interact with their bank using a standard RESTful interface

All classes, methods, and services referenced in this document are from the Open Bank Project OBP-API codebase.

---

## Entity Catalog - Account Creation User Story

Based on the Account Creation user story, the following business entities have been identified:

### 1. **User / Administrator** (Bank Administrator or Authorized User)

**Business Description**: 
A bank administrator or authorized user who has the permission to create new bank accounts for customers. This represents the actor who initiates the account creation process on behalf of customers.

**Location in Codebase** (Open Bank Project OBP-API): 
- Referenced in all API endpoints for account creation
- Used in authentication and entitlement checking
- Part of the user management and authorization system

**Key Fields**:
- User ID: Unique identifier for the administrator/user
- Authentication credentials: Validates user identity
- Entitlements: Permissions assigned to the user (CanCreateAccount)
- User type: Distinguishes between administrators and regular users
- Role: Administrative or authorized role

**Relationships to Other Entities**:
- A User/Administrator can create multiple Bank Accounts
- A User/Administrator must have CanCreateAccount entitlement
- A User/Administrator links Customers to newly created Accounts

**Business Rules**:
- User must be authenticated to create accounts
- User must have CanCreateAccount entitlement
- Appropriate entitlements are checked before creation
- Only authorized users can perform account creation

**Notes**:
- The user story focuses on administrative account creation operations
- Authentication and entitlement checking are critical prerequisites
- This is different from account holders who use accounts - this is about creating accounts

---

### 2. **Bank Account** (Core Banking Entity - Being Created)

**Business Description**: 
A financial account where money is stored and managed. This is the primary entity being created in this user story. The account is created with specified parameters including type, currency, initial balance, and routing information.

**Location in Codebase** (Open Bank Project OBP-API): 
- Classes: APIMethods400.addAccount, APIMethods200.createAccount, APIMethods220.createAccount, APIMethods310.createAccount, APIMethods500.createAccount
- Domain Model: BankAccount (account domain model)
- Connector: Connector.createBankAccount (creates account in core banking system)
- Multiple API versions provide different account creation capabilities

**Key Fields**:
- Account ID: Unique identifier for the account (generated during creation)
- Bank ID: Identifier of the bank where the account is held (required input)
- User ID: Identifier of the account owner/customer (required input)
- Label: Human-readable name or description of the account (required)
- Account number: Unique account number (generated or validated)
- Account type: Category of account - checking, savings, loan, etc. (required)
- Currency: ISO currency code for the account (required)
- Balance: Initial balance (optional, must be non-negative for most account types)
- Account routing: Routing information for the account (configured during creation)
- Branch ID: Branch identifier if specified (optional)
- Description: Additional account description (optional)
- Status: Account status (created in active status)

**Relationships to Other Entities**:
- A Bank Account belongs to one Bank
- A Bank Account is owned by one Customer/User
- A Bank Account has one Account Type
- A Bank Account operates in one Currency
- A Bank Account may belong to one Branch
- A Bank Account has Account Routing information
- A Bank Account has an initial Balance

**Business Rules**:
- Account is created with unique account ID
- Account number is generated or validated
- Account number must be unique within bank
- Account type must be valid (checking, savings, loan, etc.)
- Currency must be valid ISO currency code
- Initial balance must be non-negative for most account types
- Account routing information must follow banking standards
- Account label is required
- Account is created in active status
- Bank ID must be valid and active
- Customer/user must exist before account creation
- Branch ID must be valid if specified

**Notes**:
- This is the central entity being created in the Account Creation user story
- Multiple API endpoints across different versions provide account creation
- Different API versions have different parameter requirements
- Account number generation strategy may vary by bank
- Initial balance handling varies by account type

---

### 3. **Bank** (Financial Institution)

**Business Description**: 
A financial institution that holds and manages customer accounts. The bank entity represents the organization where the new account will be created. Bank ID is required for account creation.

**Location in Codebase** (Open Bank Project OBP-API): 
- Referenced in all API endpoint paths as BANK_ID parameter
- Part of the core banking domain model in OBP-API
- Used for account validation and creation

**Key Fields**:
- Bank ID: Unique identifier for the bank (required in all API endpoints)
- Bank name: Name of the financial institution
- Status: Active/inactive (must be active for account creation)

**Relationships to Other Entities**:
- A Bank has multiple Bank Accounts
- A Bank has multiple Branches
- A Bank has multiple Customers
- Bank ID is required to create accounts

**Business Rules**:
- Bank ID must be valid and active
- Account number must be unique within bank
- Bank-specific account number generation strategies may apply

**Notes**:
- Bank ID is a required path parameter in all account creation endpoints
- Validation ensures bank exists and is active before account creation
- Critical for multi-bank environments

---

### 4. **Customer** (Account Owner)

**Business Description**: 
A customer of the bank who will own the newly created account. The customer entity represents the individual or organization that will hold and use the account for banking operations.

**Location in Codebase** (Open Bank Project OBP-API): 
- Domain Model: Customer (customer domain model for linking)
- Referenced in account creation parameters as user_id
- Part of customer management system

**Key Fields**:
- Customer ID / User ID: Unique identifier for the customer
- Customer name: Name of the account owner
- Customer type: Individual, business, etc.
- Customer status: Active/inactive

**Relationships to Other Entities**:
- A Customer can own multiple Bank Accounts
- A Customer belongs to one or more Banks
- A Customer is linked to newly created Account

**Business Rules**:
- Customer/user must exist before account creation
- Account owner/customer is linked during creation
- Customer existence validation is performed

**Notes**:
- Customer must be registered in the system before account creation
- User ID in account creation parameters refers to the customer
- Critical dependency for account creation

---

### 5. **Account Type** (Reference Data)

**Business Description**: 
A classification or category that defines the nature and purpose of a bank account. Common types include checking accounts, savings accounts, loan accounts, credit accounts, etc. Account type must be specified during account creation.

**Location in Codebase** (Open Bank Project OBP-API): 
- Referenced in account creation parameters as type
- Part of core account information
- Used for validation during account creation

**Key Fields**:
- Type code: Unique identifier for the account type
- Type name: Descriptive name (CHECKING, SAVINGS, LOAN, CREDIT, etc.)
- Type description: Detailed explanation of the account type
- Type rules: Business rules specific to the account type

**Relationships to Other Entities**:
- A Bank Account has one Account Type
- Account Type defines the nature and purpose of the account
- Account Type may have specific balance rules

**Business Rules**:
- Account type is specified during creation (required)
- Account type must be valid (checking, savings, loan, etc.)
- Account type validation against allowed types
- Initial balance handling varies by account type
- Initial balance must be non-negative for most account types

**Notes**:
- Acts as reference/lookup data
- Used for categorization and validation during account creation
- Different account types may have different business rules

---

### 6. **Currency** (Reference Data)

**Business Description**: 
The monetary currency in which the account will operate. This defines the unit of money used for all account transactions and balances. Currency must be specified during account creation.

**Location in Codebase** (Open Bank Project OBP-API): 
- Referenced in account creation parameters as currency
- Part of core account information
- Used for validation during account creation

**Key Fields**:
- Currency code: ISO 4217 currency code (e.g., USD, EUR, GBP)
- Currency name: Full name of the currency
- Currency symbol: Symbol representation (e.g., $, €, £)

**Relationships to Other Entities**:
- A Bank Account operates in one Currency
- Currency is set during account creation
- Initial Balance is denominated in the account's Currency

**Business Rules**:
- Account currency is set during creation (required)
- Currency must be valid ISO currency code
- Currency code validation (ISO 4217)
- Currency defines the unit of money for the account

**Notes**:
- Acts as reference/lookup data
- Critical for international banking and multi-currency support
- Determines how monetary values are interpreted

---

### 7. **Account Routing** (Banking Reference Data)

**Business Description**: 
Routing information associated with a bank account that enables external systems to identify and route transactions to the correct account. This includes information like IBAN, account numbers, routing numbers, and other identifiers. Routing information is configured during account creation.

**Location in Codebase** (Open Bank Project OBP-API): 
- Referenced in account creation parameters as account_routing
- Part of account configuration
- Used for payment processing setup

**Key Fields**:
- Routing scheme: Type of routing (e.g., IBAN, AccountNumber, RoutingNumber)
- Routing address: The actual routing value
- Account reference: Which account this routing belongs to

**Relationships to Other Entities**:
- Account Routings belong to one Bank Account
- A Bank Account can have multiple Account Routings (different routing schemes)
- Routing information is configured during account creation

**Business Rules**:
- Account routing information is configured during creation
- Account routing information must follow banking standards
- Routing information format validation
- Multiple routing schemes can exist for a single account

**Notes**:
- Essential for payment processing and account identification
- Configured during account creation for integration purposes
- Supports multiple routing schemes per account

---

### 8. **Branch** (Bank Branch - Optional)

**Business Description**: 
A physical or virtual branch of the bank where the account may be associated. Branch association is optional during account creation but may be required by some banks.

**Location in Codebase** (Open Bank Project OBP-API): 
- Referenced in account creation parameters as branch_id (optional)
- Part of bank organizational structure
- Used for account organization and management

**Key Fields**:
- Branch ID: Unique identifier for the branch
- Branch name: Name of the branch
- Branch location: Physical or virtual location
- Bank reference: Which bank this branch belongs to

**Relationships to Other Entities**:
- A Branch belongs to one Bank
- A Bank Account may belong to one Branch
- A Bank has multiple Branches

**Business Rules**:
- Branch ID must be valid if specified
- Branch ID validation if provided
- Branch association is optional during account creation

**Notes**:
- Optional parameter during account creation
- May be required by some banks for organizational purposes
- Used for account management and reporting

---

### 9. **Initial Balance** (Financial Data)

**Business Description**: 
The initial monetary amount deposited into the account at the time of creation. This represents the starting balance for the newly created account.

**Location in Codebase** (Open Bank Project OBP-API): 
- Referenced in account creation parameters as balance
- Part of account initialization
- May trigger initial deposit transaction

**Key Fields**:
- Amount: The monetary value of the initial balance
- Currency: The currency of the balance (matches account currency)
- Timestamp: When the balance was set (account creation time)

**Relationships to Other Entities**:
- Initial Balance belongs to one Bank Account
- Initial Balance is in the account's Currency
- Initial Balance may create an initial deposit transaction

**Business Rules**:
- Initial balance can be specified during creation (optional)
- Initial balance must be non-negative for most account types
- Balance format and range validation
- Initial balance handling varies by account type
- Transaction may need to be atomic with initial deposit

**Notes**:
- Optional parameter during account creation
- May require special handling for different account types
- May trigger creation of initial deposit transaction

---

### 10. **Entitlement** (Authorization Entity)

**Business Description**: 
A specific permission or right granted to a user that controls what operations they can perform in the system. For account creation, the CanCreateAccount entitlement is required.

**Location in Codebase** (Open Bank Project OBP-API): 
- Referenced in business rules and entitlement management system
- Specific entitlement: CanCreateAccount
- Part of OBP-API's role-based access control (RBAC) framework
- Checked before allowing account creation

**Key Fields**:
- Entitlement ID: Unique identifier
- Entitlement name: Specific permission name (CanCreateAccount)
- User reference: Which user has this entitlement
- Scope: What the entitlement allows (account creation)

**Relationships to Other Entities**:
- Entitlements are granted to Users/Administrators
- Entitlement controls access to account creation operations
- Required for all account creation endpoints

**Business Rules**:
- User must have CanCreateAccount entitlement
- Appropriate entitlements are checked before creation
- Entitlement validation is performed before account creation

**Notes**:
- Provides coarse-grained authorization (who can create accounts)
- Critical security control for account creation operations
- All five API endpoints require this entitlement

---

## Entity Relationship Map

The Account Creation user story reveals the following key relationships:

1. **User/Administrator → Bank Account**: An administrator creates new bank accounts (one-to-many)

2. **User/Administrator → Entitlement**: User must have CanCreateAccount entitlement (many-to-many)

3. **Bank → Bank Account**: A bank contains multiple bank accounts (one-to-many)

4. **Customer → Bank Account**: A customer owns one or more bank accounts (one-to-many)

5. **Bank Account → Account Type**: Each bank account has one account type (many-to-one)

6. **Bank Account → Currency**: Each bank account operates in one currency (many-to-one)

7. **Bank Account → Initial Balance**: Each bank account may have one initial balance (one-to-one, optional)

8. **Bank Account → Account Routing**: Each bank account has one or more account routings (one-to-many)

9. **Bank Account → Branch**: Each bank account may belong to one branch (many-to-one, optional)

10. **Bank → Branch**: A bank has multiple branches (one-to-many)

11. **Bank → Customer**: A bank has multiple customers (one-to-many)

12. **Initial Balance → Currency**: Initial balance is denominated in the account's currency

**Authorization Flow**:
- User authentication validates administrator identity
- Entitlement check validates user has CanCreateAccount permission
- Only authorized users can proceed with account creation

**Account Creation Flow**:
1. Administrator makes API request with BANK_ID and account parameters
2. System validates user authentication
3. System checks user has CanCreateAccount entitlement
4. System validates Bank ID exists and is active
5. System validates Customer/User ID exists
6. System validates Account Type is valid
7. System validates Currency code (ISO 4217)
8. System validates Branch ID if provided
9. System validates account routing information format
10. System checks account number uniqueness within bank
11. System validates initial balance (non-negative for most account types)
12. System generates unique Account ID
13. System generates or validates account number
14. Connector creates account in core banking system
15. System creates initial balance if specified (may be atomic transaction)
16. System configures account routing information
17. System sets account status to active
18. System returns created account details with generated Account ID

**Data Flow**:
- Input: Bank ID, user_id, label, type, balance, currency, account_routing, branch_id
- Processing: Validation, generation, creation in core banking system
- Output: JSON object with created account details including generated account ID

---

## Business Domain Summary

The Account Creation user story covers the following business domains:

1. **Account Management**: Core functionality for creating new bank accounts with specified parameters
2. **Authorization & Access Control**: Entitlement-based permission system for account creation operations
3. **Customer Onboarding**: Linking customers to newly created accounts
4. **Reference Data Management**: Account types, currencies, branches as configuration dimensions
5. **Banking Integration**: Account creation in core banking system via connector
6. **Regulatory Compliance**: Account opening requirements and audit trails

**Key Business Capabilities**:
- Add new account with full parameters (POST /obp/v4.0.0/banks/{BANK_ID}/accounts)
- Create account v2.0.0 (PUT /obp/v2.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID})
- Create account v2.2.0 (POST /obp/v2.2.0/banks/{BANK_ID}/accounts)
- Create account v3.1.0 (POST /obp/v3.1.0/banks/{BANK_ID}/accounts)
- Create account v5.0.0 (POST /obp/v5.0.0/banks/{BANK_ID}/accounts)
- Entitlement-based authorization for account creation
- Account number generation and uniqueness validation
- Initial balance setup with account creation
- Account routing configuration
- Multi-version API support for account creation

**Technical Integration Points**:
- APIMethods400.addAccount (v4.0.0 endpoint)
- APIMethods200.createAccount (v2.0.0 endpoint)
- APIMethods220.createAccount (v2.2.0 endpoint)
- APIMethods310.createAccount (v3.1.0 endpoint)
- APIMethods500.createAccount (v5.0.0 endpoint)
- Connector.createBankAccount (core banking system integration)
- BankAccount (domain model)
- Customer (domain model for linking)

---

## Questions & Uncertainties

1. **Account Number Generation**: What is the account number generation strategy? Is it bank-specific or system-wide? Is it sequential, random, or based on some algorithm? This is critical for implementation.

2. **API Version Differences**: What are the exact differences between the five API endpoints in terms of required parameters and response format? The user story mentions different API versions have different parameter requirements, but specifics are needed.

3. **Initial Balance Transaction**: When an initial balance is specified, is a separate deposit transaction created, or is it just a field on the account? Is this transaction atomic with account creation?

4. **Account Type Rules**: What are the specific business rules for each account type? For example, can loan accounts have positive initial balances? What are the constraints for each type?

5. **Duplicate Account Prevention**: How is duplicate account number prevention implemented? Is it a database constraint, application-level check, or both? What error is returned?

6. **Branch Requirement**: Under what circumstances is branch_id required vs optional? Is this bank-specific or account-type-specific?

7. **Routing Information Format**: What is the exact format and validation rules for account_routing parameter? What routing schemes are supported?

8. **Account Status**: The user story states "account is created in active status" - are there other possible statuses? Can accounts be created in inactive status?

9. **Customer Validation**: What specific validation is performed on the customer/user? Must they be an existing customer of the bank, or can they be any user in the system?

10. **Error Handling**: What specific error codes and messages are returned for:
    - Invalid bank ID
    - Invalid customer/user ID
    - Invalid account type
    - Invalid currency code
    - Duplicate account number
    - Invalid branch ID
    - Invalid routing information
    - Insufficient entitlements
    - Negative initial balance (when not allowed)

11. **Audit Trail**: The implementation notes mention "audit trail required for account creation" - what specific information should be logged? Is this for regulatory compliance (e.g., KYC, AML)?

12. **Regulatory Requirements**: The implementation notes mention "consider regulatory requirements for account opening" - what specific requirements apply? Are there KYC (Know Your Customer) or AML (Anti-Money Laundering) checks required?

13. **Transaction Atomicity**: If initial balance creation fails, should the account creation be rolled back? What is the transaction boundary?

14. **View Creation**: The dependencies mention "view creation" as downstream - are default views automatically created for new accounts? What views are created?

15. **Account Access Management**: How is initial access to the newly created account configured? Does the customer automatically get owner view?

---

## Additional Implementation Considerations

Based on the entity analysis, here are critical implementation points for the Go migration:

1. **Data Model Preservation**: All ten entities (User/Administrator, Bank Account, Bank, Customer, Account Type, Currency, Account Routing, Branch, Initial Balance, Entitlement) must be represented in the Go application with their complete field sets and relationships.

2. **Entitlement-Based Authorization**: The entitlement-based permission system must be faithfully replicated in Go:
   - CanCreateAccount entitlement checking must happen before any account creation
   - Only authorized users can create accounts
   - Entitlement validation must be enforced for all endpoints

3. **API Endpoint Mapping**: The Scala endpoints must have exact equivalents in Go with identical behavior:
   - **Endpoint 1**: POST /obp/v4.0.0/banks/{BANK_ID}/accounts
     - Method: addAccount
     - Entitlements: CanCreateAccount
   - **Endpoint 2**: PUT /obp/v2.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}
     - Method: createAccount
     - Entitlements: CanCreateAccount
   - **Endpoint 3**: POST /obp/v2.2.0/banks/{BANK_ID}/accounts
     - Method: createAccount
     - Entitlements: CanCreateAccount
   - **Endpoint 4**: POST /obp/v3.1.0/banks/{BANK_ID}/accounts
     - Method: createAccount
     - Entitlements: CanCreateAccount
   - **Endpoint 5**: POST /obp/v5.0.0/banks/{BANK_ID}/accounts
     - Method: createAccount
     - Entitlements: CanCreateAccount
   - URL patterns must match exactly
   - Request/response formats must be identical
   - Error codes and messages must match
   - Business rule enforcement must be consistent

4. **Multi-Version API Support**: The Go implementation must support five different API versions:
   - Different API versions have different parameter requirements
   - Response formats may vary by version
   - Backward compatibility must be maintained
   - Version-specific validation rules must be applied

5. **Validation Framework**: Implement comprehensive validation for account creation:
   - Bank ID validation (exists and active)
   - Account type validation against allowed types
   - Currency code validation (ISO 4217)
   - Account number uniqueness check within bank
   - Customer/user existence validation
   - Balance format and range validation (non-negative for most types)
   - Routing information format validation
   - Branch ID validation if provided
   - Account label required validation

6. **Account Number Generation**: Implement account number generation strategy:
   - Generate unique account numbers
   - Support bank-specific generation strategies
   - Validate account number uniqueness within bank
   - Prevent duplicate account numbers
   - Handle account number generation failures

7. **Initial Balance Handling**: Implement initial balance setup:
   - Support optional initial balance specification
   - Validate balance is non-negative for most account types
   - Handle account-type-specific balance rules
   - Consider atomic transaction with account creation
   - May need to create initial deposit transaction

8. **Core Banking System Integration**: Implement connector for account creation:
   - Connector.createBankAccount must create account in core banking system
   - Handle connector failures gracefully
   - Implement retry logic if appropriate
   - Ensure data consistency between OBP and core banking system

9. **Error Handling**: Implement comprehensive error handling for:
   - Invalid bank ID (appropriate error message)
   - Invalid customer/user ID (appropriate error message)
   - Invalid account type (validation error)
   - Invalid currency code (validation error)
   - Duplicate account number (uniqueness error)
   - Invalid branch ID (validation error)
   - Invalid routing information (format error)
   - Insufficient entitlements (authorization error)
   - Negative initial balance when not allowed (validation error)
   - Core banking system connector failures (system error)
   - Validation errors return clear error messages

10. **Test Coverage**: Test cases should verify:
    - **Entitlement-based authorization**:
      - User with CanCreateAccount can create accounts
      - User without CanCreateAccount receives authorization error
      - Entitlement check happens before any processing
    - **Account creation with valid parameters**:
      - Account is created with unique account ID
      - Account number is generated or validated
      - Account is created in active status
      - All required fields are populated
      - Response includes generated account ID
    - **Validation scenarios**:
      - Invalid bank ID returns error
      - Invalid customer/user ID returns error
      - Invalid account type returns error
      - Invalid currency code returns error
      - Duplicate account number returns error
      - Invalid branch ID returns error
      - Invalid routing information returns error
      - Negative initial balance (when not allowed) returns error
      - Missing required fields return errors
    - **Multi-version API support**:
      - Each of the 5 endpoints creates accounts correctly
      - Version-specific parameter requirements are enforced
      - Response formats match version expectations
    - **Initial balance handling**:
      - Account created with initial balance
      - Account created without initial balance
      - Initial balance validation for different account types
    - **Account number uniqueness**:
      - Duplicate account numbers are prevented
      - Uniqueness is enforced within bank
    - **Edge cases**:
      - Account creation with all optional parameters
      - Account creation with minimal parameters
      - Different account types (checking, savings, loan, credit)
      - Different currencies
      - With and without branch association

11. **Integration Points**: Ensure proper integration with:
    - Authentication system (user validation)
    - Authorization system (entitlement checks)
    - Core banking connector (account creation in core system)
    - Customer management system (customer validation)
    - Branch management system (branch validation if provided)
    - Account number generation service
    - Audit logging system (account creation tracking)

12. **Transaction Management**: Implement proper transaction handling:
    - Account creation should be atomic
    - Initial balance setup should be part of the transaction
    - Rollback on any failure
    - Ensure data consistency

13. **Audit Logging**: Implement comprehensive audit logging:
    - Log all account creation attempts
    - Include user ID, bank ID, customer ID, account parameters
    - Log success and failure outcomes
    - Include timestamps and request details
    - Ensure audit trail for regulatory compliance

14. **Regulatory Compliance**: Consider regulatory requirements:
    - KYC (Know Your Customer) requirements
    - AML (Anti-Money Laundering) checks
    - Account opening documentation requirements
    - Regulatory reporting requirements
    - Data retention requirements

15. **Downstream Operations**: Implement or trigger downstream operations:
    - View creation for new accounts (default views)
    - Account access management setup
    - Initial deposit transaction if balance specified
    - Notification to customer about new account
    - Account activation workflows if needed

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
- Section: Part 2: Detailed user stories - User Story 3: Account Creation (lines 241-337)
- Note: This user story documents the Account Creation functionality from the Open Bank Project OBP-API

**Extraction Methodology**: 
- Based on entity extraction prompt methodology
- Systematic analysis of user story components
- Identification of entities, relationships, and business rules
- Focus on data model and business logic preservation for migration

**Key Classes/Services Referenced** (from OBP-API codebase):
- APIMethods400.addAccount
- APIMethods200.createAccount
- APIMethods220.createAccount
- APIMethods310.createAccount
- APIMethods500.createAccount
- Connector.createBankAccount
- BankAccount (domain model)
- Customer (domain model)

---

## Summary

This entity extraction analysis identifies **ten core business entities** involved in the Account Creation user story:

1. **User/Administrator** - The authorized actor who creates accounts
2. **Bank Account** - The primary entity being created with specified parameters
3. **Bank** - The financial institution where the account is created
4. **Customer** - The account owner who will use the account
5. **Account Type** - Classification of the account (checking, savings, loan, etc.)
6. **Currency** - Monetary currency for the account
7. **Account Routing** - Banking reference data for payment processing
8. **Branch** - Optional bank branch association
9. **Initial Balance** - Optional starting balance for the account
10. **Entitlement** - Authorization control (CanCreateAccount)

The analysis reveals a **complex account creation process** involving multiple validations, entity relationships, and integration with the core banking system. The user story involves **five different API endpoints** across four API versions (v2.0.0, v2.2.0, v3.1.0, v4.0.0, v5.0.0), each potentially having different parameter requirements.

Key implementation challenges include:
- Multi-version API support with different parameter requirements
- Account number generation and uniqueness validation
- Initial balance handling with account-type-specific rules
- Entitlement-based authorization enforcement
- Core banking system integration via connector
- Transaction atomicity for account creation and initial balance
- Comprehensive validation framework
- Regulatory compliance requirements (KYC, AML)
- Audit trail for account creation operations

The entity relationships, business rules, and data flows documented here provide the foundation for implementing the Account Creation functionality in Go while maintaining exact functional equivalence with the Scala-based OBP-API implementation.

---

This analysis extracts all business entities explicitly mentioned or implied in the Account Creation user story from the **Open Bank Project (OBP-API)**, following a systematic approach to identify entities, relationships, business rules, and implementation considerations. All classes, methods, and services referenced are from the official OBP-API repository at https://github.com/OpenBankProject/OBP-API.git.
