# Business Rules Extraction: Account Creation

## User Story Overview
**Feature:** Account Creation  
**API Endpoints:**
- POST /obp/v4.0.0/banks/{BANK_ID}/accounts
- PUT /obp/v2.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}
- POST /obp/v2.2.0/banks/{BANK_ID}/accounts
- POST /obp/v3.1.0/banks/{BANK_ID}/accounts
- POST /obp/v5.0.0/banks/{BANK_ID}/accounts

**Purpose:** Create new bank accounts with specified parameters so customers can perform banking operations

---

## Business Rules Extracted by Role

### Role 1: Bank Compliance Officer Perspective
**Focus:** Rules ensuring regulatory compliance and customer protection

#### Rule 1.1: CanCreateAccount Entitlement Requirement
**What it does:** Only users with CanCreateAccount entitlement can create new bank accounts

**When it applies:** Every time a user attempts to create a new account through any account creation endpoint

**Who it affects:** Bank administrators, authorized staff, API consumers

**Example:** When a bank branch manager tries to open a new checking account for a customer, the system first verifies they have the CanCreateAccount entitlement. A teller without this entitlement cannot create accounts even if they are authenticated.

#### Rule 1.2: Customer Pre-Existence Validation
**What it does:** The customer or user must exist in the system before an account can be created for them

**When it applies:** During account creation validation before the account is actually created

**Who it affects:** Bank staff creating accounts, customer onboarding processes

**Example:** Before creating a savings account for a new customer, the bank must first complete the customer registration process (KYC, identity verification) and create the customer record. Attempting to create an account with a non-existent customer ID will be rejected.

#### Rule 1.3: Account Creation Audit Trail
**What it does:** All account creation attempts must be logged for regulatory audit purposes

**When it applies:** Every successful and failed account creation request

**Who it affects:** Compliance officers, auditors, regulatory bodies

**Example:** When a bank employee creates a new business checking account, the system logs the employee ID, timestamp, customer ID, account type, initial balance, and all account parameters, creating an audit trail for compliance reviews and fraud detection.

#### Rule 1.4: Regulatory Compliance for Account Opening
**What it does:** Account creation must comply with regulatory requirements for account opening

**When it applies:** During the account creation process

**Who it affects:** Bank compliance teams, regulatory bodies, customers

**Example:** When opening a new account, the system may enforce requirements such as minimum age verification, tax identification number validation, and compliance with anti-money laundering regulations before allowing the account to be created.

---

### Role 2: Customer Service Manager Perspective
**Focus:** Rules governing customer interactions with the bank

#### Rule 2.1: Account Label Requirement
**What it does:** Every new account must have a label (name/description) specified during creation

**When it applies:** During account creation parameter validation

**Who it affects:** Bank staff creating accounts, customers, account management systems

**Example:** When creating a new account, the bank employee must provide a label like "Primary Checking" or "Emergency Savings" to help the customer identify the account purpose. An account creation request without a label will be rejected.

#### Rule 2.2: Account Type Specification
**What it does:** The account type must be specified and must be valid (checking, savings, loan, credit card, etc.)

**When it applies:** During account creation parameter validation

**Who it affects:** Bank staff, customers, product management

**Example:** When opening a new account, the staff member must select a valid account type from the available options. Attempting to create an account with type "INVALID_TYPE" will be rejected with an error listing the valid account types.

#### Rule 2.3: Initial Balance Specification
**What it does:** An initial balance can be specified when creating an account, and must be non-negative for most account types

**When it applies:** During account creation when initial balance is provided

**Who it affects:** Customers opening accounts, bank staff, accounting systems

**Example:** A customer opening a new savings account can make an initial deposit of $500, which is recorded as the initial balance. Attempting to create a checking account with an initial balance of -$100 will be rejected.

#### Rule 2.4: Account Description Optional
**What it does:** Account description is optional during account creation

**When it applies:** During account creation parameter processing

**Who it affects:** Bank staff, customers

**Example:** When creating a basic checking account, the staff member can provide just the required label "Checking Account" without adding an extended description, and the account will be created successfully.

---

### Role 3: Risk Management Specialist Perspective
**Focus:** Rules protecting the bank from fraud and financial risk

#### Rule 3.1: Account Number Uniqueness Enforcement
**What it does:** Account numbers must be unique within the bank to prevent conflicts and fraud

**When it applies:** During account creation when the account number is generated or validated

**Who it affects:** Core banking system, account management, fraud prevention

**Example:** When the system generates account number "1234567890" for a new checking account, it first checks if this number already exists at the bank. If it does, the system generates a different unique number to prevent account conflicts.

#### Rule 3.2: Duplicate Account Prevention
**What it does:** The system must prevent creation of duplicate accounts for the same customer with identical parameters

**When it applies:** During account creation validation

**Who it affects:** Customers, bank operations, data integrity

**Example:** If a customer already has a "Primary Checking" account, the system may warn or prevent creation of another account with the exact same label and type to avoid confusion and potential fraud.

#### Rule 3.3: Initial Balance Validation
**What it does:** Initial balance must be validated for format, range, and appropriateness for the account type

**When it applies:** During account creation parameter validation when initial balance is provided

**Who it affects:** Customers, accounting systems, fraud prevention

**Example:** When creating a savings account with an initial balance of "$1,000,000.00", the system validates the format is correct, the amount is non-negative, and may flag unusually large deposits for additional review per anti-money laundering rules.

---

### Role 4: Product Manager Perspective
**Focus:** Rules defining banking products and services

#### Rule 4.1: Account Type Product Definition
**What it does:** Each account type represents a specific banking product with defined features and capabilities

**When it applies:** During account creation and throughout the account lifecycle

**Who it affects:** Product managers, customers, bank operations

**Example:** Creating a "Premium Checking" account type automatically applies product features like no monthly fees, unlimited transactions, and overdraft protection, while a "Basic Savings" account type applies different features like transaction limits and interest accrual.

#### Rule 4.2: Currency Specification Requirement
**What it does:** Account currency must be specified and must be a valid ISO 4217 currency code

**When it applies:** During account creation parameter validation

**Who it affects:** International banking operations, customers, accounting systems

**Example:** When creating a new account for a customer with international transactions, the bank specifies "EUR" for a Euro account or "USD" for a US Dollar account. Attempting to create an account with currency "INVALID" will be rejected.

#### Rule 4.3: Account Routing Information Configuration
**What it does:** Account routing information must be configured according to banking standards (IBAN, routing numbers, SWIFT codes)

**When it applies:** During account creation when routing information is specified

**Who it affects:** Payment processors, customers, interbank transfers

**Example:** When creating a new checking account, the system configures routing information including the bank's routing number (e.g., "021000021") and generates an IBAN if required for international transactions, ensuring the account can receive payments.

#### Rule 4.4: Branch Association
**What it does:** Account can be associated with a specific bank branch if branch ID is provided

**When it applies:** During account creation when branch ID is specified

**Who it affects:** Branch operations, customer service, reporting

**Example:** When a customer opens an account at the downtown branch, the account is associated with branch ID "BRANCH_001", allowing the branch to track accounts opened at their location and provide localized customer service.

---

### Role 5: Operations Director Perspective
**Focus:** Rules governing internal bank processes and workflows

#### Rule 5.1: Bank ID Validation and Active Status
**What it does:** The bank ID must be valid and the bank must be active in the system

**When it applies:** During account creation request validation

**Who it affects:** Multi-bank operations, API consumers, error handling

**Example:** When creating an account at "BANK_A", the system verifies that BANK_A exists in the system and is active. Attempting to create an account at "INVALID_BANK" or an inactive bank will be rejected with an appropriate error message.

#### Rule 5.2: Unique Account ID Generation
**What it does:** Each new account must be assigned a unique account ID

**When it applies:** During the account creation process

**Who it affects:** Account management systems, database operations, API consumers

**Example:** When a new savings account is created, the system generates a unique account ID like "ACC_2024_001234" that will be used to identify this account in all subsequent operations. This ID is returned in the creation response.

#### Rule 5.3: Account Number Generation Strategy
**What it does:** Account number generation strategy may vary by bank based on their policies

**When it applies:** During account creation when account number needs to be generated

**Who it affects:** Bank operations, customers, payment processors

**Example:** Bank A may use sequential numbering (1000001, 1000002, etc.) while Bank B may use a combination of branch code, account type, and random digits (001-CHK-789456). The system respects each bank's configured strategy.

#### Rule 5.4: Active Status on Creation
**What it does:** Newly created accounts are set to active status by default

**When it applies:** During account creation completion

**Who it affects:** Account holders, transaction processing, account management

**Example:** When a new checking account is successfully created, it is immediately set to "ACTIVE" status, allowing the customer to start using it for deposits, withdrawals, and transfers without requiring additional activation steps.

#### Rule 5.5: Core Banking System Integration
**What it does:** Account creation must be performed through the connector to the core banking system

**When it applies:** During the actual account creation operation

**Who it affects:** External system integration, backend services, data consistency

**Example:** The API calls the bank connector's createBankAccount method to create the account in the core banking system, ensuring the account exists in the authoritative system and all downstream systems are properly updated.

#### Rule 5.6: Multi-Version API Support
**What it does:** Different API versions (v2.0.0, v2.2.0, v3.1.0, v4.0.0, v5.0.0) support account creation with varying parameter requirements

**When it applies:** Based on which API version endpoint is called

**Who it affects:** API consumers, legacy applications, modern applications

**Example:** An older application using API v2.0.0 may use PUT with account ID in the URL, while a newer application using v4.0.0 uses POST without account ID in the URL, allowing the system to generate the ID.

---

### Role 6: Treasury and Payment Specialist Perspective
**Focus:** Rules controlling money movement and payment processing

#### Rule 6.1: Initial Balance Atomic Transaction
**What it does:** If an initial balance is specified, the account creation and initial deposit should be atomic

**When it applies:** During account creation when initial balance is provided

**Who it affects:** Accounting systems, customers, transaction processing

**Example:** When creating a savings account with an initial deposit of $1,000, the system creates the account and records the initial deposit as a single atomic transaction. If either operation fails, both are rolled back to maintain data consistency.

#### Rule 6.2: Account Routing Standards Compliance
**What it does:** Account routing information must follow banking standards (ABA routing numbers, IBAN format, SWIFT codes)

**When it applies:** During account creation when routing information is configured

**Who it affects:** Payment processors, interbank transfers, international transactions

**Example:** When creating an account with IBAN, the system validates the IBAN format follows ISO 13616 standard (e.g., "DE89370400440532013000" for Germany), ensuring the account can participate in SEPA and international wire transfers.

---

### Role 7: Security and Access Control Manager Perspective
**Focus:** Rules protecting the system and controlling access

#### Rule 7.1: User Authentication Requirement
**What it does:** All account creation requests must be made by authenticated users

**When it applies:** Every time a user attempts to create an account

**Who it affects:** All API consumers, security systems

**Example:** When a bank employee attempts to create a new account through the web interface, the system first validates their authentication token. If the token is expired or invalid, the request is rejected with a 401 Unauthorized error.

#### Rule 7.2: Entitlement Verification Before Creation
**What it does:** System must verify user has CanCreateAccount entitlement before processing the creation request

**When it applies:** After authentication but before creating the account

**Who it affects:** Bank staff, API consumers, access control systems

**Example:** When a customer service representative tries to create an account, the system checks their role and entitlements. If they don't have CanCreateAccount entitlement, the request is denied with a 403 Forbidden error explaining the missing entitlement.

#### Rule 7.3: Customer/User Existence Security Check
**What it does:** System must validate the customer/user exists to prevent account creation for non-existent or unauthorized users

**When it applies:** During account creation validation

**Who it affects:** Security systems, fraud prevention, data integrity

**Example:** If someone attempts to create an account for user_id="FAKE_USER_999" that doesn't exist, the system rejects the request, preventing creation of orphaned accounts or potential fraud attempts.

#### Rule 7.4: Validation Error Clear Messaging
**What it does:** Validation errors must return clear, specific error messages to help identify the issue

**When it applies:** When any validation fails during account creation

**Who it affects:** API consumers, bank staff, error handling systems

**Example:** If account creation fails due to invalid currency code, the system returns "Invalid currency code 'XYZ'. Must be a valid ISO 4217 currency code such as USD, EUR, GBP" rather than a generic "Validation failed" message.

---

## Summary of Business Rules by Category

### Access and Permission Rules
- Rule 1.1: CanCreateAccount Entitlement Requirement
- Rule 7.1: User Authentication Requirement
- Rule 7.2: Entitlement Verification Before Creation

### Validation and Verification Rules
- Rule 1.2: Customer Pre-Existence Validation
- Rule 2.2: Account Type Specification
- Rule 4.2: Currency Specification Requirement
- Rule 5.1: Bank ID Validation and Active Status
- Rule 7.3: Customer/User Existence Security Check
- Rule 7.4: Validation Error Clear Messaging

### Processing and Workflow Rules
- Rule 5.2: Unique Account ID Generation
- Rule 5.3: Account Number Generation Strategy
- Rule 5.4: Active Status on Creation
- Rule 5.5: Core Banking System Integration
- Rule 5.6: Multi-Version API Support

### Financial and Calculation Rules
- Rule 2.3: Initial Balance Specification
- Rule 3.3: Initial Balance Validation
- Rule 6.1: Initial Balance Atomic Transaction

### Compliance and Audit Rules
- Rule 1.3: Account Creation Audit Trail
- Rule 1.4: Regulatory Compliance for Account Opening

### Customer and Account Rules
- Rule 2.1: Account Label Requirement
- Rule 2.4: Account Description Optional
- Rule 4.1: Account Type Product Definition
- Rule 4.3: Account Routing Information Configuration
- Rule 4.4: Branch Association

### Transaction and Payment Rules
- Rule 6.2: Account Routing Standards Compliance

### Security and Authentication Rules
- Rule 3.1: Account Number Uniqueness Enforcement
- Rule 3.2: Duplicate Account Prevention

---

## Implementation Considerations

### Critical Business Rules for Migration
When migrating the Account Creation functionality to Go, the following business rules are absolutely critical and must be preserved:

1. **Authentication and Authorization** (Rules 1.1, 7.1, 7.2): The entitlement system must be replicated exactly to prevent unauthorized account creation
2. **Validation Rules** (Rules 1.2, 2.2, 4.2, 5.1, 7.3): All input validation must match the original behavior to maintain API contract and data integrity
3. **Uniqueness Enforcement** (Rules 3.1, 5.2): Account numbers and IDs must be unique to prevent conflicts
4. **Atomic Operations** (Rule 6.1): Account creation with initial balance must be atomic to maintain data consistency
5. **Audit Trail** (Rule 1.3): All account creation attempts must be logged for compliance

### Testing Requirements
Each business rule should have corresponding test cases in the Go application:
- Test authenticated vs unauthenticated requests (Rule 7.1)
- Test entitlement validation (Rules 1.1, 7.2)
- Test customer existence validation (Rules 1.2, 7.3)
- Test account type validation (Rule 2.2)
- Test currency code validation (Rule 4.2)
- Test bank ID validation (Rule 5.1)
- Test account number uniqueness (Rule 3.1)
- Test duplicate account prevention (Rule 3.2)
- Test initial balance validation (Rules 2.3, 3.3)
- Test atomic transaction for initial balance (Rule 6.1)
- Test routing information validation (Rules 4.3, 6.2)
- Test branch ID validation (Rule 4.4)
- Test account label requirement (Rule 2.1)
- Test multi-version API compatibility (Rule 5.6)
- Test error message clarity (Rule 7.4)

### Performance Considerations
- Optimize account number generation to avoid collisions (Rule 5.3)
- Ensure atomic operations don't cause performance bottlenecks (Rule 6.1)
- Implement efficient uniqueness checks (Rule 3.1)
- Consider caching for validation lookups (bank ID, customer ID)

### Audit and Compliance
- Implement comprehensive audit logging (Rule 1.3)
- Ensure all creation attempts are recorded with all parameters
- Maintain audit trail for regulatory compliance (Rule 1.4)
- Log validation failures for security monitoring

---

## Endpoint-Specific Business Rules

### POST /obp/v4.0.0/banks/{BANK_ID}/accounts

**Required Entitlements:**
- CanCreateAccount

**Validation Rules:**
- User must be authenticated (Rule 7.1)
- User must have CanCreateAccount entitlement (Rules 1.1, 7.2)
- Bank ID must be valid and active (Rule 5.1)
- Customer/user must exist (Rules 1.2, 7.3)
- Account type must be valid (Rule 2.2)
- Currency must be valid ISO 4217 code (Rule 4.2)
- Account label is required (Rule 2.1)
- Initial balance must be non-negative if provided (Rules 2.3, 3.3)
- Routing information must follow standards if provided (Rules 4.3, 6.2)
- Branch ID must be valid if provided (Rule 4.4)

**Processing Rules:**
- Generate unique account ID (Rule 5.2)
- Generate or validate unique account number (Rules 3.1, 5.3)
- Create account in core banking system (Rule 5.5)
- Set account to active status (Rule 5.4)
- Process initial balance atomically if provided (Rule 6.1)
- Configure routing information (Rule 4.3)
- Associate with branch if specified (Rule 4.4)
- Log account creation (Rule 1.3)
- Return created account details with generated ID

### PUT /obp/v2.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}

**Required Entitlements:**
- CanCreateAccount

**Validation Rules:**
- Same as v4.0.0 endpoint
- Account ID provided in URL

**Processing Rules:**
- Use provided account ID or generate if not valid
- Same processing as v4.0.0 endpoint
- Support v2.0.0 request/response format (Rule 5.6)

### POST /obp/v2.2.0/banks/{BANK_ID}/accounts

**Required Entitlements:**
- CanCreateAccount

**Validation Rules:**
- Same as v4.0.0 endpoint

**Processing Rules:**
- Same as v4.0.0 endpoint
- Support v2.2.0 request/response format (Rule 5.6)

### POST /obp/v3.1.0/banks/{BANK_ID}/accounts

**Required Entitlements:**
- CanCreateAccount

**Validation Rules:**
- Same as v4.0.0 endpoint

**Processing Rules:**
- Same as v4.0.0 endpoint
- Support v3.1.0 request/response format (Rule 5.6)

### POST /obp/v5.0.0/banks/{BANK_ID}/accounts

**Required Entitlements:**
- CanCreateAccount

**Validation Rules:**
- Same as v4.0.0 endpoint

**Processing Rules:**
- Same as v4.0.0 endpoint
- Support v5.0.0 request/response format (Rule 5.6)
- May include additional fields or validation in v5.0.0

---

## Input Parameter Business Rules

### user_id (Required)
**Validation:**
- Must be valid and exist in system (Rules 1.2, 7.3)
- Customer must be registered and KYC complete

### label (Required)
**Validation:**
- Cannot be empty (Rule 2.1)
- Length must be within limits
- Special characters handling

### type (Required)
**Validation:**
- Must be valid account type (Rule 2.2)
- Must match available product types (Rule 4.1)
- Examples: checking, savings, loan, credit_card

### balance (Optional)
**Validation:**
- Must be non-negative for most account types (Rules 2.3, 3.3)
- Format validation (decimal places)
- Range validation (maximum limits)

### currency (Required)
**Validation:**
- Must be valid ISO 4217 currency code (Rule 4.2)
- Examples: USD, EUR, GBP, JPY

### account_routing (Optional)
**Validation:**
- Must follow banking standards if provided (Rules 4.3, 6.2)
- IBAN format validation
- Routing number format validation
- SWIFT code format validation

### branch_id (Optional)
**Validation:**
- Must be valid if provided (Rule 4.4)
- Branch must belong to the specified bank
- Branch must be active

---

## Error Handling Business Rules

### Invalid Bank ID
**Response:** Error message indicating invalid or inactive bank  
**HTTP Status:** 400 Bad Request or 404 Not Found  
**Business Rule:** Rule 5.1

### Missing CanCreateAccount Entitlement
**Response:** Permission denied error with specific entitlement name  
**HTTP Status:** 403 Forbidden  
**Business Rule:** Rules 1.1, 7.2

### Non-Existent Customer/User
**Response:** Error message indicating customer not found  
**HTTP Status:** 400 Bad Request or 404 Not Found  
**Business Rule:** Rules 1.2, 7.3

### Invalid Account Type
**Response:** Error message listing valid account types  
**HTTP Status:** 400 Bad Request  
**Business Rule:** Rule 2.2

### Invalid Currency Code
**Response:** Error message with valid ISO 4217 examples  
**HTTP Status:** 400 Bad Request  
**Business Rule:** Rule 4.2

### Missing Account Label
**Response:** Error message indicating label is required  
**HTTP Status:** 400 Bad Request  
**Business Rule:** Rule 2.1

### Negative Initial Balance
**Response:** Error message indicating balance must be non-negative  
**HTTP Status:** 400 Bad Request  
**Business Rule:** Rules 2.3, 3.3

### Duplicate Account Number
**Response:** Error message indicating account number already exists  
**HTTP Status:** 409 Conflict  
**Business Rule:** Rule 3.1

### Invalid Routing Information
**Response:** Error message with correct format examples  
**HTTP Status:** 400 Bad Request  
**Business Rule:** Rules 4.3, 6.2

### Invalid Branch ID
**Response:** Error message indicating branch not found  
**HTTP Status:** 400 Bad Request or 404 Not Found  
**Business Rule:** Rule 4.4

### Missing Authentication
**Response:** Authentication required error  
**HTTP Status:** 401 Unauthorized  
**Business Rule:** Rule 7.1

---

## Data Integrity and Consistency Rules

### Atomic Account Creation
- Account creation and initial balance must be atomic (Rule 6.1)
- If either operation fails, both must be rolled back
- Ensures no orphaned accounts or missing initial deposits

### Uniqueness Constraints
- Account ID must be unique across all accounts (Rule 5.2)
- Account number must be unique within bank (Rule 3.1)
- Prevents conflicts and fraud

### Referential Integrity
- Customer/user must exist before account creation (Rules 1.2, 7.3)
- Bank must exist and be active (Rule 5.1)
- Branch must exist and belong to bank if specified (Rule 4.4)

---

## Conclusion

This business rules extraction identifies 28 distinct business rules governing the Account Creation functionality across 7 different organizational perspectives. These rules cover authentication, authorization, validation, uniqueness enforcement, atomic operations, audit trail, and data integrity requirements. When migrating this functionality to Go, all these rules must be preserved to ensure the new implementation maintains functional equivalence with the Scala application and can be validated using the existing test cases.

The account creation process is particularly critical as it establishes the foundation for all subsequent banking operations. The Go implementation must ensure proper validation, uniqueness enforcement, atomic operations, and comprehensive audit logging to maintain data integrity and regulatory compliance.
