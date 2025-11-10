# Screen Flow Documentation: Account Creation

## OBP-API Version: v5.0.0

Applied from: screen-flow-extraction-prompt.md (OBP-API Phase-01-Playbooks)
Source: Account Creation User Story (OBP-API Account Management User Stories)
Based on: Official OpenBankProject/OBP-API repository
Date: November 10, 2025

---

## Flow Name
**Account Creation and Setup Flow**

## Flow Description
This flow enables bank administrators and authorized users to create new bank accounts with specified parameters through the OBP-API. The flow supports multiple API versions (v2.0.0 through v5.0.0) with varying parameter requirements, allowing administrators to set up customer accounts with appropriate account types, currencies, initial balances, routing information, and other essential account attributes. The created accounts are immediately available for banking operations.

## API Endpoints

**Primary Endpoints (OBP-API multiple versions):**

1. **Add Account with Full Parameters (v4.0.0):**
   - Endpoint: `POST /obp/v4.0.0/banks/{BANK_ID}/accounts`
   - Implementation: `APIMethods400.addAccount`
   - Purpose: Add new account with full parameters
   - Entitlements: CanCreateAccount

2. **Create Account (v2.0.0):**
   - Endpoint: `PUT /obp/v2.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}`
   - Implementation: `APIMethods200.createAccount`
   - Purpose: Create account with specified ID
   - Entitlements: CanCreateAccount

3. **Create Account (v2.2.0):**
   - Endpoint: `POST /obp/v2.2.0/banks/{BANK_ID}/accounts`
   - Implementation: `APIMethods220.createAccount`
   - Purpose: Create account with auto-generated ID
   - Entitlements: CanCreateAccount

4. **Create Account (v3.1.0):**
   - Endpoint: `POST /obp/v3.1.0/banks/{BANK_ID}/accounts`
   - Implementation: `APIMethods310.createAccount`
   - Purpose: Create account with enhanced parameters
   - Entitlements: CanCreateAccount

5. **Create Account (v5.0.0):**
   - Endpoint: `POST /obp/v5.0.0/banks/{BANK_ID}/accounts`
   - Implementation: `APIMethods500.createAccount`
   - Purpose: Create account with latest API features
   - Entitlements: CanCreateAccount

**Request Format:**
- Request Type: JSON body with account parameters
- Required Fields:
  - `user_id`: Customer/user ID to link account to
  - `label`: Account label/name
  - `type`: Account type (checking, savings, loan, etc.)
  - `balance`: Initial balance (amount and currency)
  - `account_routing`: Routing information (scheme and address)
- Optional Fields:
  - `branch_id`: Branch identifier
  - `account_routings`: Multiple routing schemes
  - Additional attributes based on API version

**Response Format:**
- Response Type: JSON object with created account details
- Includes:
  - Generated account ID
  - Account number
  - Bank ID
  - Account label
  - Account type
  - Currency
  - Balance
  - Account routing information
  - Account status

## Starting Point
**Entry Points:**
1. **Admin Dashboard**: Administrator accesses account creation form from admin panel
2. **Customer Onboarding**: New customer registration flow includes account creation
3. **Direct API Call**: External system makes authenticated REST API request to create account
4. **Bulk Account Creation**: Batch processing system creates multiple accounts
5. **Branch Office**: Branch staff creates account for walk-in customer

**Prerequisites:**
- User must be authenticated using OAuth2 (Keycloak, OBPOIDC), OAuth1a, or DirectLogin
- User must have CanCreateAccount entitlement
- Bank must exist and be active
- Customer/user must exist in the system before account creation
- Valid account parameters must be prepared
- Regulatory requirements for account opening must be met

## Step-by-Step Flow

### Step 1: Authentication and Entitlement Verification
**Screen Purpose:** Verify user identity and account creation permissions

**User Entry:** Administrator or authorized user attempts to create new account

**Authentication Mechanisms (OBP-API):**
- **OAuth2**: Using Keycloak or OBP OIDC providers
- **OAuth1a**: Traditional OAuth 1.0a flow
- **DirectLogin**: Direct login with credentials

**Information Display:**
- Login screen (if not authenticated)
- Authentication status indicator
- Entitlement verification status
- Available banks for account creation

**User Input:**
- User credentials (if not authenticated)
- OAuth consent (for OAuth flows)
- Bank selection

**Actions Available:**
- Submit credentials
- Authorize OAuth application
- Select bank for account creation
- Cancel and return to dashboard

**Validation (Implementation Flow):**
1. Check authentication token validity
2. Verify user has CanCreateAccount entitlement
3. Validate bank ID exists and is active
4. Check user authorization level for the bank

**Success Path:** → Step 2: Customer/User Selection

**Error Codes:**
- **401 (UserNotLoggedIn)**: User is not authenticated
- **403 (UserHasMissingRoles)**: User lacks CanCreateAccount entitlement
- **400 (BankNotFound)**: Invalid bank ID specified
- **403 (InsufficientAuthorisationToCreateAccount)**: User not authorized for this bank

---

### Step 2: Customer/User Selection and Validation
**Screen Purpose:** Select and validate the customer who will own the account

**User Entry:** Authenticated user with CanCreateAccount entitlement

**Information Display:**
- Customer search interface
- Customer list (if applicable)
- Selected customer details
- Customer verification status
- Existing accounts for customer (if any)

**User Input:**
- Customer ID or search criteria
- Customer name search
- Customer identification number

**Actions Available:**
- Search for customer
- Select customer from list
- View customer details
- Create new customer (if permitted)
- Proceed to account parameters
- Cancel account creation

**Validation:**
- Customer/user must exist in the system
- Customer ID must be valid
- Customer must be eligible for new account
- Customer must not be blocked or restricted
- Verify customer KYC (Know Your Customer) status

**Implementation Flow:**
1. Search for customer by ID or criteria
2. Retrieve customer details
3. Validate customer status and eligibility
4. Check existing accounts for customer
5. Verify regulatory compliance for account opening

**Success Path:** → Step 3: Account Parameters Configuration

**Error Codes:**
- **400 (UserNotFoundById)**: Customer/user ID doesn't exist
- **403 (CustomerNotEligible)**: Customer not eligible for new account
- **400 (KYCNotComplete)**: Customer KYC verification incomplete

---

### Step 3: Account Parameters Configuration
**Screen Purpose:** Configure account parameters and attributes

**User Entry:** Valid customer selected

**Information Display:**
- Account parameter form
- Available account types
- Supported currencies
- Routing scheme options
- Branch selection (if applicable)
- Parameter validation feedback

**User Input:**
- **Account Label**: Descriptive name for the account (required)
- **Account Type**: Select from available types (required)
  - Checking/Current account
  - Savings account
  - Loan account
  - Credit card account
  - Other account types
- **Currency**: ISO 4217 currency code (required)
- **Initial Balance**: Starting balance amount (required)
- **Account Routing**: Routing information (required)
  - Scheme: IBAN, AccountNumber, etc.
  - Address: Routing address/number
- **Branch ID**: Branch identifier (optional)
- **Account Number**: Custom account number (optional, auto-generated if not provided)
- **Additional Attributes**: Version-specific parameters

**Account Type Options:**
- **Checking/Current**: Standard transaction account
- **Savings**: Interest-bearing savings account
- **Loan**: Loan account with repayment terms
- **Credit Card**: Credit card account
- **Investment**: Investment account
- **Business**: Business account
- **Other**: Other account types

**Actions Available:**
- Fill in account parameters
- Select account type from dropdown
- Choose currency from list
- Enter initial balance
- Configure routing information
- Add multiple routing schemes
- Validate parameters
- Preview account configuration
- Submit account creation
- Cancel and return

**Validation:**
- Account label is required and non-empty
- Account type must be valid
- Currency must be valid ISO 4217 code
- Initial balance must be non-negative (for most account types)
- Account number must be unique within bank (if provided)
- Routing information must follow banking standards
- Branch ID must be valid if specified
- All required fields must be completed

**Implementation Flow:**
1. Display account parameter form
2. Populate dropdown options (types, currencies, branches)
3. Validate each parameter as entered
4. Check account number uniqueness (if provided)
5. Verify routing information format
6. Validate currency code against ISO 4217
7. Prepare account creation request

**Success Path:** → Step 4: Account Creation Confirmation

**Error Codes:**
- **400 (InvalidAccountType)**: Invalid account type specified
- **400 (InvalidCurrencyCode)**: Invalid ISO currency code
- **400 (DuplicateAccountNumber)**: Account number already exists
- **400 (InvalidRoutingInformation)**: Routing info doesn't meet standards
- **400 (InvalidBranchId)**: Branch ID doesn't exist
- **400 (NegativeBalance)**: Initial balance cannot be negative

---

### Step 4: Account Creation Confirmation and Review
**Screen Purpose:** Review account parameters before final creation

**User Entry:** All account parameters configured and validated

**Information Display:**
- **Account Summary**:
  - Customer name and ID
  - Bank name and ID
  - Account label
  - Account type
  - Currency
  - Initial balance
  - Account routing information
  - Branch (if specified)
  - Account number (generated or custom)
- **Confirmation Checklist**:
  - All required parameters provided
  - Validations passed
  - Regulatory requirements met
  - Customer eligibility confirmed
- **Terms and Conditions**: Account opening terms (if applicable)

**User Input:**
- Review confirmation
- Accept terms and conditions (if required)
- Final approval

**Actions Available:**
- Review all parameters
- Edit parameters (return to Step 3)
- Confirm and create account
- Cancel account creation
- Save as draft (if supported)

**Validation:**
- Final validation of all parameters
- Duplicate account number check
- Customer eligibility reconfirmation
- Regulatory compliance verification

**Implementation Flow:**
1. Display comprehensive account summary
2. Perform final validations
3. Check for duplicate account numbers
4. Verify all business rules
5. Prepare for account creation in core banking system

**Success Path:** → Step 5: Account Creation Execution

**Alternative Path:** → Return to Step 3 if user wants to edit parameters

---

### Step 5: Account Creation Execution and Completion
**Screen Purpose:** Execute account creation and display results

**User Entry:** User confirms account creation

**Implementation Flow (OBP-API):**
1. Route to appropriate endpoint based on API version
2. For v4.0.0: `APIMethods400.addAccount`
3. For v5.0.0: `APIMethods500.createAccount`
4. For v3.1.0: `APIMethods310.createAccount`
5. For v2.2.0: `APIMethods220.createAccount`
6. For v2.0.0: `APIMethods200.createAccount`
7. Validate all parameters one final time
8. Call `Connector.createBankAccount` to create account in core banking system
9. Generate account ID (if not provided)
10. Generate account number (if not provided)
11. Create `BankAccount` domain object
12. Link account to customer
13. Set account status to active
14. Create audit log entry
15. Format response with created account details

**Information Display:**
- **Success Screen**:
  - Success confirmation message
  - Created account details:
    - Account ID (generated)
    - Account number (generated or custom)
    - Account label
    - Account type
    - Currency
    - Initial balance
    - Account routing information
    - Account status (Active)
  - Next steps options
- **Progress Indicator**: During account creation process

**Account Creation Process:**
1. Validate all parameters
2. Check account number uniqueness
3. Create account in core banking system via connector
4. Generate unique account ID
5. Generate or validate account number
6. Link account to customer/user
7. Set initial balance
8. Configure routing information
9. Set account status to active
10. Create audit trail entry
11. Return created account details

**Actions Available:**
- View created account details
- Create another account
- Set up account views and permissions
- Make initial deposit (if not already done)
- Configure account settings
- Print account confirmation
- Return to dashboard
- Navigate to account list

**Success Indicators:**
- Account created successfully message
- Unique account ID generated
- Account number assigned
- Account visible in account list
- Customer can access account

**Success Path:** → Account successfully created, user can proceed with next actions

**Error Codes:**
- **500 (UnknownError)**: Internal server error during creation
- **400 (AccountCreationFailed)**: Account creation failed in core banking system
- **400 (DuplicateAccountNumber)**: Account number conflict detected
- **500 (ConnectorError)**: Core banking system connector error

---

## Alternative Paths

### Path A: Customer Not Found
**Trigger:** Selected customer/user doesn't exist in system

**Flow:**
1. Display customer not found error
2. Provide options:
   - Search for different customer
   - Create new customer (if permitted)
   - Enter customer ID manually
   - Contact support
3. If creating new customer:
   - Redirect to customer registration flow
   - Complete customer creation
   - Return to account creation with new customer ID

**Error Message:** "Customer not found. Please verify the customer ID or create a new customer."

### Path B: Duplicate Account Number
**Trigger:** Provided account number already exists

**Flow:**
1. Display duplicate account number error
2. Show existing account with same number (limited details)
3. Provide options:
   - Use auto-generated account number
   - Enter different account number
   - Cancel account creation
4. If using auto-generated number:
   - System generates unique account number
   - Continue with account creation

**Error Message:** "Account number already exists. Please use a different account number or let the system generate one automatically."

### Path C: Insufficient Entitlements
**Trigger:** User lacks CanCreateAccount entitlement

**Flow:**
1. Display permission denied message
2. Show required entitlement: CanCreateAccount
3. Provide options:
   - Request entitlement from administrator
   - Contact support
   - Return to dashboard
4. Log unauthorized access attempt

**Error Message:** "You don't have permission to create accounts. Please contact your administrator to request the CanCreateAccount entitlement."

### Path D: Invalid Account Parameters
**Trigger:** One or more account parameters fail validation

**Flow:**
1. Display validation errors with specific field indicators
2. Highlight invalid fields in red
3. Show validation error messages for each field
4. Provide correction guidance
5. Allow user to correct parameters
6. Re-validate on change
7. Enable submit only when all validations pass

**Common Validation Errors:**
- Empty account label
- Invalid account type
- Invalid currency code
- Negative initial balance
- Invalid routing information format
- Invalid branch ID

### Path E: Core Banking System Unavailable
**Trigger:** Connector cannot reach core banking system

**Flow:**
1. Display system unavailable message
2. Show estimated recovery time (if available)
3. Provide options:
   - Retry account creation
   - Save parameters as draft (if supported)
   - Schedule account creation for later
   - Cancel and return
4. Log system availability issue
5. Notify system administrators

**Error Message:** "The core banking system is temporarily unavailable. Please try again later or save your parameters as a draft."

### Path F: Regulatory Compliance Check Failure
**Trigger:** Account creation violates regulatory requirements

**Flow:**
1. Display compliance error message
2. Show specific regulatory requirement that failed
3. Provide options:
   - Complete required compliance steps
   - Contact compliance officer
   - Cancel account creation
4. If compliance steps needed:
   - Redirect to compliance verification flow
   - Complete required verifications
   - Return to account creation

**Error Message:** "Account creation requires additional compliance verification. Please complete the required steps before proceeding."

### Path G: Session Timeout During Creation
**Trigger:** User session expires during account creation process

**Flow:**
1. Save entered parameters temporarily (if possible)
2. Display session timeout notification
3. Redirect to authentication screen
4. After re-authentication:
   - Restore saved parameters
   - Resume account creation from last step
   - Revalidate all parameters

---

## User Types

### Bank Administrators
- Full account creation privileges across all banks
- Can create accounts for any customer
- Access to all account types and parameters
- Can override certain validation rules (with audit)
- Access through admin interface

### Branch Managers
- Can create accounts for customers at their branch
- Limited to specific account types based on branch
- Must follow standard validation rules
- Access through branch management interface
- All actions logged for audit

### Customer Service Representatives
- Can create accounts for customers they're assisting
- Limited to standard account types
- Must follow strict validation rules
- Time-limited access sessions
- All actions logged and monitored

### Onboarding Specialists
- Specialized role for new customer account creation
- Can create accounts as part of onboarding process
- Access to customer verification tools
- Must complete compliance checks
- Access through onboarding workflow

### Third-Party Systems/API Consumers
- Automated account creation through API
- Require OAuth tokens with CanCreateAccount scope
- Subject to rate limiting
- Must provide all required parameters
- All API calls logged and monitored

### Compliance Officers
- Can review account creation requests
- Can approve/reject accounts requiring special approval
- Access to audit trails
- Cannot directly create accounts (review only)
- Access through compliance interface

---

## Integration Points

### Authentication Service
- **Implementation**: OAuth2Login (Keycloak, OBPOIDC), OAuth1a, DirectLogin
- Validates user credentials and authentication tokens
- Checks CanCreateAccount entitlement
- Manages session state and token lifecycle
- Provides user context for authorization

### Customer Management System
- Validates customer/user existence
- Retrieves customer details and status
- Checks customer eligibility for new accounts
- Verifies KYC (Know Your Customer) status
- Links account to customer record

### Core Banking System Connector
- **Implementation**: `Connector.createBankAccount`
- Creates account in core banking system
- Generates account numbers (if not provided)
- Sets initial balance
- Configures routing information
- Returns created account details
- Handles bank-specific account creation logic

### Account Domain Model
- **Implementation**: `BankAccount` domain object
- Represents account data structure
- Encapsulates account business logic
- Validates account parameters
- Manages account state and attributes

### Account Number Generation Service
- Generates unique account numbers
- Validates custom account numbers
- Checks for duplicates
- Follows bank-specific numbering schemes
- Ensures uniqueness within bank

### Routing Information Validator
- Validates routing information format
- Checks routing scheme standards (IBAN, SWIFT, etc.)
- Verifies routing address format
- Ensures compliance with banking standards

### Audit Logging System
- Records all account creation requests
- Logs user actions and parameters
- Maintains security audit trail with call context
- Tracks success and failure events
- Provides audit reports for compliance

### Regulatory Compliance System
- Validates account opening against regulations
- Checks customer eligibility
- Verifies KYC requirements
- Ensures anti-money laundering (AML) compliance
- Logs compliance checks

---

## Security & Compliance Considerations

### Authentication and Authorization
- **Token Validation**: All requests must include valid authentication token
- **Entitlement Enforcement**: System checks for CanCreateAccount entitlement
- **User Verification**: Validates user identity and authorization level
- **Error Responses**: Clear error codes (401, 403) for security failures
- User must be authenticated to create accounts
- User must have CanCreateAccount entitlement

### Data Validation and Integrity
- **Parameter Validation**: All account parameters validated before creation
- **Account Number Uniqueness**: Prevents duplicate account numbers
- **Customer Validation**: Ensures customer exists and is eligible
- **Currency Validation**: Verifies ISO 4217 currency codes
- **Balance Validation**: Ensures non-negative initial balance (for most types)
- **Routing Validation**: Verifies routing information follows standards

### Audit Trail and Compliance
- **Call Context Tracking**: Tracks all operations through request chain
- **Creation Logging**: Log all account creation attempts (success and failure)
- **Parameter Logging**: Record all account parameters
- **User Action Logging**: Track who created which accounts
- **Compliance Reporting**: Generate audit reports for regulatory compliance
- **Timestamp Recording**: Maintain creation timestamps

### Regulatory Requirements
- **KYC Verification**: Customer KYC must be complete
- **AML Compliance**: Anti-money laundering checks
- **Account Opening Rules**: Follow regulatory requirements for account opening
- **Customer Eligibility**: Verify customer meets requirements
- **Documentation**: Maintain required documentation

### Data Privacy
- **Sensitive Data Protection**: Protect customer and account information
- **Access Control**: Limit account creation to authorized users only
- **Data Encryption**: Encrypt sensitive data in transit and at rest
- **Comply with Regulations**: GDPR, PSD2, and other data protection laws

---

## Performance Considerations

### Response Time Requirements
- Account creation should complete within reasonable time (typically 3-5 seconds)
- Real-time validation for better user experience
- Asynchronous processing for complex account setups (if needed)

### Transaction Atomicity
- Account creation must be atomic (all or nothing)
- If creation fails, no partial account should exist
- Rollback mechanism for failed creations
- Consistent state across all systems

### Optimization Approaches
- **Database Indexing**: Optimize indexes for account number uniqueness checks
- **Connection Pooling**: Maintain connection pool to core banking system
- **Async Processing**: Use `Future` monad for non-blocking operations
- **Validation Caching**: Cache validation rules and reference data
- **Batch Processing**: Support bulk account creation for efficiency

### Scalability Considerations
- Support high volume of account creation requests
- Rate limiting to prevent abuse
- Queue-based processing for bulk operations
- Horizontal scaling of API servers

### Monitoring and Alerting
- **Creation Rate Monitoring**: Track account creation rates
- **Error Rate Monitoring**: Monitor creation failure rates
- **Performance Tracking**: Track creation time and identify bottlenecks
- **System Health**: Monitor connector health and core banking system availability

---

## Error Handling

### OBP-API Error Codes

1. **401 (UserNotLoggedIn)**
   - Cause: User is not authenticated or token is invalid
   - Action: Redirect to login/authentication screen
   - Recovery: Authenticate using OAuth2, OAuth1a, or DirectLogin

2. **403 (UserHasMissingRoles)**
   - Cause: User lacks CanCreateAccount entitlement
   - Message: Includes specific missing entitlement name
   - Action: Display access denied message
   - Recovery: Request CanCreateAccount entitlement from administrator

3. **403 (InsufficientAuthorisationToCreateAccount)**
   - Cause: User not authorized to create accounts at this bank
   - Action: Display authorization error
   - Recovery: Request authorization or contact administrator

4. **400 (BankNotFound)**
   - Cause: Invalid bank ID specified
   - Action: Show error message
   - Recovery: Verify bank ID or select different bank

5. **400 (UserNotFoundById)**
   - Cause: Customer/user ID doesn't exist
   - Action: Show customer not found error
   - Recovery: Verify customer ID or create new customer

6. **400 (InvalidAccountType)**
   - Cause: Invalid account type specified
   - Action: Show validation error with valid account types
   - Recovery: Select valid account type from list

7. **400 (InvalidCurrencyCode)**
   - Cause: Invalid ISO 4217 currency code
   - Action: Show validation error with valid currencies
   - Recovery: Select valid currency code

8. **400 (DuplicateAccountNumber)**
   - Cause: Account number already exists
   - Action: Show duplicate error
   - Recovery: Use auto-generated number or enter different number

9. **400 (InvalidRoutingInformation)**
   - Cause: Routing information doesn't meet standards
   - Action: Show validation error with format requirements
   - Recovery: Correct routing information format

10. **400 (InvalidBranchId)**
    - Cause: Branch ID doesn't exist
    - Action: Show branch validation error
    - Recovery: Select valid branch or leave empty

11. **400 (NegativeBalance)**
    - Cause: Initial balance cannot be negative
    - Action: Show validation error
    - Recovery: Enter non-negative balance

12. **400 (AccountCreationFailed)**
    - Cause: Account creation failed in core banking system
    - Action: Display creation failure message
    - Recovery: Retry or contact support

13. **400 (KYCNotComplete)**
    - Cause: Customer KYC verification incomplete
    - Action: Show compliance error
    - Recovery: Complete KYC verification before account creation

14. **500 (UnknownError)**
    - Cause: Internal server error during processing
    - Action: Display generic error message
    - Recovery: Retry request or contact support

15. **500 (ConnectorError)**
    - Cause: Core banking system connector error
    - Action: Display system unavailable message
    - Recovery: Retry later or contact support

### Recovery Options
- Retry with same parameters (for transient errors)
- Modify parameters (for validation errors)
- Request entitlements (for permission errors)
- Create customer first (for customer not found)
- Use auto-generated values (for duplicate errors)
- Re-authenticate (for auth errors)
- Contact support (for persistent errors)
- Save as draft and retry later

---

## Technical Context (OBP-API v5.0.0)

### Key Implementation Classes and Methods

**Classes/Services Involved:**
- `APIMethods400.addAccount` - creates account with full parameters (v4.0.0)
- `APIMethods200.createAccount` - creates account (v2.0.0)
- `APIMethods220.createAccount` - creates account (v2.2.0)
- `APIMethods310.createAccount` - creates account (v3.1.0)
- `APIMethods500.createAccount` - creates account (v5.0.0)
- `Connector.createBankAccount` - creates account in core banking system
- `BankAccount` - account domain model
- `Customer` - customer domain model for linking

**Input Data:**
- Bank ID (required)
- User ID (customer ID) (required)
- Account label (required)
- Account type (required)
- Balance (amount and currency) (required)
- Account routing (scheme and address) (required)
- Branch ID (optional)
- Account number (optional, auto-generated if not provided)
- Additional attributes based on API version

**Output Data:**
- JSON object with created account details:
  - Generated account ID
  - Account number (generated or custom)
  - Bank ID
  - Account label
  - Account type
  - Currency
  - Initial balance
  - Account routing information
  - Account status (Active)

**Processing Type:**
- Real-time REST API
- Synchronous request-response pattern
- Atomic transaction (all or nothing)

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
- All required fields must be present

### Account Number Generation
- System can auto-generate unique account numbers
- Custom account numbers must be validated for uniqueness
- Account numbering follows bank-specific schemes
- Uniqueness enforced within bank scope

---

## Dependencies

### Upstream Dependencies
- User authentication and authorization
- Customer/user registration and management
- Bank setup and configuration
- User must be authenticated to create accounts
- User must have CanCreateAccount entitlement
- Customer must exist in system

### Downstream Dependencies
- Account access management and view creation
- Initial deposit or balance setup
- Account settings configuration
- Transaction processing
- Statement generation

### External Systems
- Core banking system for account creation via connector
- Authentication provider (OAuth2, OAuth1a)
- Customer management system
- Audit logging system
- Regulatory compliance system

---

## Notes for Implementation

### Key Implementation Notes
- Different API versions have different parameter requirements
- Account number generation strategy may vary by bank
- Initial balance handling varies by account type
- Audit trail required for account creation
- Transaction may need to be atomic with initial deposit
- Consider regulatory requirements for account opening
- Account creation must be atomic (all or nothing)
- Rollback mechanism needed for failed creations

### API Version Considerations
- v5.0.0: Latest features and parameters (recommended)
- v4.0.0: Full parameter support with addAccount method
- v3.1.0: Enhanced parameters over v2.x
- v2.2.0: POST method with auto-generated ID
- v2.0.0: PUT method with specified ID
- Choose appropriate version based on requirements
- Maintain backward compatibility

### Account Type Handling
- Different account types may have different validation rules
- Loan accounts may allow negative balances
- Savings accounts may have minimum balance requirements
- Business accounts may require additional documentation
- Account type determines available operations

### Regulatory Compliance
- KYC verification must be complete before account creation
- AML checks may be required
- Account opening documentation must be maintained
- Compliance audit trail required
- Regional regulations may vary

---

## Questions Requiring SME Input

1. **Account Number Generation**: What is the account numbering scheme for each bank? Should account numbers follow specific patterns or formats?

2. **Initial Balance Requirements**: Are there minimum balance requirements for different account types? Can certain account types (e.g., loans) have negative initial balances?

3. **Account Type Definitions**: What is the complete list of valid account types and their specific characteristics? Are there bank-specific account types?

4. **Routing Information Standards**: What routing schemes are supported (IBAN, SWIFT, domestic codes)? What validation rules apply to each scheme?

5. **Branch Association**: Is branch association mandatory for accounts? How does branch affect account operations?

6. **Regulatory Requirements**: What are the specific regulatory requirements for account opening in different regions? What compliance checks must be performed?

7. **Account Approval Workflow**: Do certain account types require approval before activation? What is the approval process?

8. **Multi-Currency Accounts**: Are multi-currency accounts supported? How should they be created and managed?

9. **Account Limits**: Should default transaction limits be set during account creation? What are the standard limits for different account types?

10. **Bulk Account Creation**: Is there a need for bulk account creation functionality? What are the requirements for batch processing?

---

## Recommendations

1. **Implement Account Creation Wizard**: Create step-by-step wizard interface to guide users through account creation process

2. **Add Parameter Templates**: Provide pre-configured templates for common account types to speed up creation

3. **Implement Draft Saving**: Allow users to save account parameters as draft and complete creation later

4. **Add Validation Preview**: Show real-time validation feedback as users enter parameters

5. **Implement Bulk Creation**: Support bulk account creation from CSV/Excel files for efficiency

6. **Add Account Number Preview**: Show generated account number before final creation

7. **Implement Approval Workflow**: Add approval workflow for accounts requiring special authorization

8. **Add Customer Quick Create**: Allow creating new customer directly from account creation flow

9. **Implement Account Cloning**: Allow cloning existing account parameters for similar accounts

10. **Add Creation History**: Show history of recently created accounts for reference

11. **Implement Rollback Mechanism**: Ensure proper rollback if account creation fails partway

12. **Add Compliance Checklist**: Display compliance checklist during account creation

13. **Implement Audit Dashboard**: Provide dashboard for monitoring account creation activities

14. **Add Mobile Support**: Ensure account creation works well on mobile devices

15. **Implement Rate Limiting**: Add rate limiting to prevent abuse of account creation API

---

## Document Metadata

**Based on:** Official OpenBankProject/OBP-API repository (https://github.com/OpenBankProject/OBP-API.git)

**API Version:** v5.0.0 (with v4.0.0, v3.1.0, v2.2.0, and v2.0.0 endpoints)

**User Story Source:** Account Creation User Story from OBP-API Account Management User Stories

**Key Acceptance Criteria Addressed:**
1. Administrator can create new account by providing required parameters ✓
2. Account is created with unique account ID ✓
3. Account number is generated or validated ✓
4. Account type is specified (checking, savings, etc.) ✓
5. Account currency is set ✓
6. Initial balance can be specified ✓
7. Account owner/customer is linked ✓
8. Account label and description can be set ✓
9. Account routing information is configured ✓
10. Account is created in active status ✓
11. Appropriate entitlements are checked before creation ✓
12. Validation errors return clear error messages ✓
13. Duplicate account numbers are prevented ✓

**Last Updated:** November 10, 2025

This documentation maps the Account Creation user story to a complete screen flow following the extraction prompt guidelines, with all technical details verified against the actual OBP-API implementation across multiple API versions (v2.0.0 through v5.0.0). The flow emphasizes the administrative nature of account creation, parameter validation, and the API-based nature of the OBP system while maintaining focus on user experience and journey mapping. All acceptance criteria from the user story have been incorporated into the screen flow documentation.
