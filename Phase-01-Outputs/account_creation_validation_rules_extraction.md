# Validation Rules Extraction: Account Creation

## Executive Summary

This document extracts and documents all validation rules for the Account Creation functionality in the OBP API system. The Account Creation feature allows bank administrators and authorized users to create new bank accounts with specified parameters so that customers can have accounts to perform banking operations. This analysis follows a multi-stakeholder approach to ensure comprehensive coverage of all validation requirements.

---

## User Story Context

**Feature**: Account Creation  
**User Story**: As a bank administrator or authorized user, I want to create new bank accounts with specified parameters, so that customers can have accounts to perform banking operations.

**API Endpoints**:
1. POST /obp/v4.0.0/banks/{BANK_ID}/accounts
2. PUT /obp/v2.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}
3. POST /obp/v2.2.0/banks/{BANK_ID}/accounts
4. POST /obp/v3.1.0/banks/{BANK_ID}/accounts
5. POST /obp/v5.0.0/banks/{BANK_ID}/accounts

---

## Validation Rules by Category

### 1. Authentication and Authorization Validations

#### Rule 1.1: User Authentication Check
**What it checks**: Verifies that the requesting user is properly authenticated before creating an account.

**Why it exists**: To ensure only legitimate users can create bank accounts and prevent unauthorized account creation.

**When it applies**: At the beginning of every account creation request, before any account data is processed.

**Who it affects**: All users (administrators, authorized staff) attempting to create bank accounts.

**What happens when it fails**: The system returns an authentication error (typically HTTP 401 Unauthorized) and no account is created.

**Where it is enforced**: At the API gateway level before the request reaches the account creation service.

**Example**:
- Valid: User provides valid authentication token → Request proceeds
- Invalid: User provides expired or missing token → Request rejected with "Authentication required" error

---

#### Rule 1.2: CanCreateAccount Entitlement Validation
**What it checks**: Confirms that the authenticated user has the CanCreateAccount entitlement.

**Why it exists**: To enforce role-based access control and ensure only authorized personnel can create bank accounts.

**When it applies**: After authentication, before processing the account creation request.

**Who it affects**: All authenticated users attempting to create accounts, with only authorized administrators and staff having the required entitlement.

**What happens when it fails**: Returns authorization error (HTTP 403 Forbidden) indicating the user does not have permission to create accounts.

**Where it is enforced**: In the entitlement checking layer before account creation logic.

**Example**:
- Valid: Bank administrator with CanCreateAccount entitlement → Access granted
- Invalid: Regular customer without CanCreateAccount entitlement → Access denied

---

### 2. Bank and Branch Validations

#### Rule 2.1: Bank ID Validation
**What it checks**: Verifies that the BANK_ID parameter corresponds to a valid, existing, and active bank in the system.

**Why it exists**: To ensure accounts are created for legitimate banks and prevent errors when creating bank-specific accounts.

**When it applies**: During parameter validation, before processing account creation.

**Who it affects**: All users creating accounts at specific banks.

**What happens when it fails**: Returns "Bank not found" or "Bank not active" error message (typically HTTP 404 Not Found or 400 Bad Request).

**Where it is enforced**: In the account creation service during parameter validation phase.

**Example**:
- Valid: BANK_ID "bank-001" exists and is active → Validation passes
- Invalid: BANK_ID "fake-bank" does not exist → Error returned

---

#### Rule 2.2: Branch ID Validation
**What it checks**: Validates that the branch_id (if provided) corresponds to a valid branch of the specified bank.

**Why it exists**: To ensure accounts are associated with valid bank branches and maintain organizational structure.

**When it applies**: When the branch_id parameter is provided in the account creation request.

**Who it affects**: Users creating accounts with branch associations.

**What happens when it fails**: Returns "Branch not found" or "Branch does not belong to bank" error message.

**Where it is enforced**: In the branch validation logic during account creation.

**Example**:
- Valid: branch_id "branch-123" exists for Bank "bank-001" → Validation passes
- Invalid: branch_id "invalid-branch" does not exist → Error returned

---

### 3. Customer and User Validations

#### Rule 3.1: Customer/User Existence Validation
**What it checks**: Ensures that the user_id (customer) specified for the account exists in the system.

**Why it exists**: To prevent creating accounts for non-existent customers and maintain referential integrity.

**When it applies**: During account creation, before linking the account to a customer.

**Who it affects**: All account creation requests that specify a customer/user.

**What happens when it fails**: Returns "Customer not found" or "User does not exist" error message (typically HTTP 404 Not Found).

**Where it is enforced**: In the customer validation logic during account creation.

**Example**:
- Valid: user_id "user-456" exists in the system → Validation passes
- Invalid: user_id "nonexistent-user" does not exist → Error returned

---

### 4. Account Type and Configuration Validations

#### Rule 4.1: Account Type Validation
**What it checks**: Validates that the account type is one of the allowed types (checking, savings, loan, etc.).

**Why it exists**: To ensure accounts are created with valid types that the system can properly handle and apply appropriate business rules to.

**When it applies**: During parameter validation when processing the account type field.

**Who it affects**: All users creating accounts.

**What happens when it fails**: Returns "Invalid account type" error message with list of allowed types (typically HTTP 400 Bad Request).

**Where it is enforced**: In the account type validation logic.

**Example**:
- Valid: Account type "CHECKING" is in allowed list → Validation passes
- Invalid: Account type "INVALID_TYPE" is not recognized → Error returned

---

#### Rule 4.2: Account Label Validation
**What it checks**: Ensures that the account label is provided and meets length and format requirements.

**Why it exists**: To ensure accounts have identifiable names for user interface display and account management.

**When it applies**: During parameter validation when processing the label field.

**Who it affects**: All users creating accounts.

**What happens when it fails**: Returns "Account label is required" or "Account label exceeds maximum length" error message.

**Where it is enforced**: In the input parameter validation logic.

**Example**:
- Valid: Label "John's Checking Account" meets requirements → Validation passes
- Invalid: Empty label or label exceeding 255 characters → Error returned

---

#### Rule 4.3: Account Description Validation
**What it checks**: Validates that the account description (if provided) meets length and format requirements.

**Why it exists**: To allow optional additional information about the account while preventing excessively long descriptions.

**When it applies**: When the description field is provided in the account creation request.

**Who it affects**: Users providing account descriptions.

**What happens when it fails**: Returns "Account description exceeds maximum length" error message.

**Where it is enforced**: In the input parameter validation logic.

**Example**:
- Valid: Description within length limits → Validation passes
- Invalid: Description exceeding maximum length → Error returned

---

### 5. Currency and Balance Validations

#### Rule 5.1: Currency Code Validation
**What it checks**: Validates that the currency code is a valid ISO 4217 currency code.

**Why it exists**: To ensure accounts use standardized currency codes for proper financial processing and reporting.

**When it applies**: During parameter validation when processing the currency field.

**Who it affects**: All users creating accounts.

**What happens when it fails**: Returns "Invalid currency code" error message with expected format (typically HTTP 400 Bad Request).

**Where it is enforced**: In the currency validation logic.

**Example**:
- Valid: Currency "USD", "EUR", "GBP" are valid ISO codes → Validation passes
- Invalid: Currency "INVALID" or "US" is not a valid ISO 4217 code → Error returned

---

#### Rule 5.2: Initial Balance Validation
**What it checks**: Validates that the initial balance (if provided) is in the correct format and meets range requirements.

**Why it exists**: To ensure accounts start with valid balance amounts and prevent negative balances for most account types.

**When it applies**: When the initial balance parameter is provided in the account creation request.

**Who it affects**: Users specifying initial account balances.

**What happens when it fails**: Returns "Invalid balance format" or "Initial balance must be non-negative" error message.

**Where it is enforced**: In the balance validation logic.

**Example**:
- Valid: Initial balance "1000.00" for checking account → Validation passes
- Invalid: Initial balance "-500.00" for savings account → Error returned (negative balance not allowed)
- Valid: Initial balance "-1000.00" for loan account → Validation passes (negative balance allowed for loans)

---

#### Rule 5.3: Balance Precision Validation
**What it checks**: Ensures that the balance amount has appropriate decimal precision for the specified currency.

**Why it exists**: To maintain financial accuracy and prevent precision errors in monetary calculations.

**When it applies**: When validating the initial balance amount.

**Who it affects**: Users specifying initial account balances.

**What happens when it fails**: Returns "Invalid balance precision" error message.

**Where it is enforced**: In the balance formatting and validation logic.

**Example**:
- Valid: Balance "100.50" with 2 decimal places for USD → Validation passes
- Invalid: Balance "100.12345" with excessive decimal places → Error returned

---

### 6. Account Number and Routing Validations

#### Rule 6.1: Account Number Uniqueness Validation
**What it checks**: Ensures that the account number (if provided) is unique within the bank.

**Why it exists**: To prevent duplicate account numbers which would cause confusion and transaction errors.

**When it applies**: During account creation, after the account number is generated or provided.

**Who it affects**: All account creation requests.

**What happens when it fails**: Returns "Account number already exists" error message (typically HTTP 409 Conflict).

**Where it is enforced**: In the account number uniqueness checking logic at the database level.

**Example**:
- Valid: Account number "1234567890" does not exist in the bank → Validation passes
- Invalid: Account number "1234567890" already exists → Error returned

---

#### Rule 6.2: Account Number Format Validation
**What it checks**: Validates that the account number (if provided) follows the expected format for the bank.

**Why it exists**: To ensure account numbers conform to bank-specific or regulatory standards.

**When it applies**: When an account number is provided in the creation request or during generation.

**Who it affects**: Users providing account numbers or systems generating them.

**What happens when it fails**: Returns "Invalid account number format" error message.

**Where it is enforced**: In the account number format validation logic.

**Example**:
- Valid: Account number follows bank's format (e.g., 10 digits) → Validation passes
- Invalid: Account number with letters when only digits allowed → Error returned

---

#### Rule 6.3: Account Routing Information Validation
**What it checks**: Validates that account routing information (IBAN, routing codes, etc.) follows banking standards.

**Why it exists**: To ensure routing information is properly formatted for payment processing and interbank transfers.

**When it applies**: When account routing information is provided in the creation request.

**Who it affects**: Users providing routing information for accounts.

**What happens when it fails**: Returns "Invalid routing information format" error message.

**Where it is enforced**: In the routing information validation logic.

**Example**:
- Valid: IBAN follows ISO 13616 standard → Validation passes
- Invalid: Malformed IBAN or routing code → Error returned

---

### 7. Input Parameter Format Validations

#### Rule 7.1: Bank ID Format Validation
**What it checks**: Validates that the BANK_ID parameter follows the expected format and character constraints.

**Why it exists**: To prevent injection attacks and ensure proper parameter handling.

**When it applies**: During initial parameter parsing and validation.

**Who it affects**: All users providing bank IDs.

**What happens when it fails**: Returns parameter format validation error (typically HTTP 400 Bad Request).

**Where it is enforced**: In the parameter validation layer.

**Example**:
- Valid: BANK_ID "bank-001" follows alphanumeric-dash format → Validation passes
- Invalid: BANK_ID with special characters or SQL injection attempt → Validation error

---

#### Rule 7.2: Account ID Format Validation (for PUT endpoints)
**What it checks**: Validates that the ACCOUNT_ID parameter (for PUT endpoints) follows the expected format.

**Why it exists**: To prevent injection attacks and ensure proper parameter handling.

**When it applies**: For PUT endpoints that include ACCOUNT_ID in the URL path.

**Who it affects**: Users using v2.0.0 PUT endpoint for account creation.

**What happens when it fails**: Returns parameter format validation error (typically HTTP 400 Bad Request).

**Where it is enforced**: In the parameter validation layer.

**Example**:
- Valid: ACCOUNT_ID "acc-12345" follows expected format → Validation passes
- Invalid: ACCOUNT_ID with malicious code → Validation error

---

#### Rule 7.3: User ID Format Validation
**What it checks**: Validates that the user_id parameter follows the expected format.

**Why it exists**: To prevent injection attacks and ensure proper user identification.

**When it applies**: During parameter validation when processing the user_id field.

**Who it affects**: All users providing user IDs for account ownership.

**What happens when it fails**: Returns parameter format validation error (typically HTTP 400 Bad Request).

**Where it is enforced**: In the parameter validation layer.

**Example**:
- Valid: user_id "user-789" follows expected format → Validation passes
- Invalid: user_id with special characters or injection attempt → Validation error

---

#### Rule 7.4: JSON Request Body Validation
**What it checks**: Ensures that the request body is valid JSON and contains all required fields.

**Why it exists**: To ensure the API can properly parse and process the account creation request.

**When it applies**: During initial request parsing before field-level validation.

**Who it affects**: All users and applications making account creation requests.

**What happens when it fails**: Returns "Invalid JSON format" or "Missing required fields" error message (typically HTTP 400 Bad Request).

**Where it is enforced**: In the request parsing layer.

**Example**:
- Valid: Well-formed JSON with all required fields → Validation passes
- Invalid: Malformed JSON or missing required fields → Error returned

---

### 8. Data Integrity and Consistency Validations

#### Rule 8.1: Account Type and Balance Consistency
**What it checks**: Validates that the initial balance is appropriate for the account type (e.g., loan accounts can have negative balances, savings accounts cannot).

**Why it exists**: To enforce business rules specific to different account types.

**When it applies**: After both account type and initial balance are validated individually.

**Who it affects**: Users creating accounts with initial balances.

**What happens when it fails**: Returns "Invalid balance for account type" error message.

**Where it is enforced**: In the business rule validation logic.

**Example**:
- Valid: Loan account with negative balance → Validation passes
- Invalid: Savings account with negative balance → Error returned

---

#### Rule 8.2: Currency and Balance Consistency
**What it checks**: Ensures that the balance format is appropriate for the specified currency.

**Why it exists**: To maintain consistency between currency and balance representation.

**When it applies**: After both currency and balance are validated individually.

**Who it affects**: Users creating accounts with initial balances.

**What happens when it fails**: Returns "Balance format inconsistent with currency" error message.

**Where it is enforced**: In the data consistency validation logic.

**Example**:
- Valid: USD currency with balance "100.50" (2 decimals) → Validation passes
- Invalid: JPY currency with balance "100.50" (JPY typically has no decimals) → Warning or error

---

#### Rule 8.3: Branch and Bank Relationship Validation
**What it checks**: Validates that the specified branch belongs to the specified bank.

**Why it exists**: To prevent creating accounts with mismatched bank-branch relationships.

**When it applies**: When both bank_id and branch_id are provided.

**Who it affects**: Users creating accounts with branch associations.

**What happens when it fails**: Returns "Branch does not belong to specified bank" error message.

**Where it is enforced**: In the relationship validation logic.

**Example**:
- Valid: Branch "branch-123" belongs to Bank "bank-001" → Validation passes
- Invalid: Branch "branch-456" belongs to Bank "bank-002" but specified with Bank "bank-001" → Error returned

---

### 9. Business Rule Validations

#### Rule 9.1: Account Creation Atomicity
**What it checks**: Ensures that account creation with initial deposit is atomic (all or nothing).

**Why it exists**: To prevent partial account creation that could lead to data inconsistency.

**When it applies**: When creating an account with an initial balance.

**Who it affects**: All users creating accounts with initial balances.

**What happens when it fails**: The entire account creation is rolled back if any step fails.

**Where it is enforced**: In the transaction management layer at the connector level.

**Example**:
- Scenario: Account creation succeeds but initial deposit fails → Entire operation rolled back, no account created

---

#### Rule 9.2: Regulatory Compliance Validation
**What it checks**: Ensures that account creation complies with regulatory requirements for account opening.

**Why it exists**: To maintain compliance with banking regulations and prevent regulatory violations.

**When it applies**: During the account creation process, checking against regulatory rules.

**Who it affects**: All account creation requests.

**What happens when it fails**: Returns "Account creation violates regulatory requirements" error message.

**Where it is enforced**: In the regulatory compliance checking layer.

**Example**:
- Scenario: Attempting to create account without required KYC documentation → Error returned

---

### 10. API Version Compatibility Validations

#### Rule 10.1: API Version Parameter Requirements
**What it checks**: Validates that all required parameters for the specific API version are provided.

**Why it exists**: To ensure backward compatibility and proper handling of version-specific requirements.

**When it applies**: During parameter validation based on the API version used.

**Who it affects**: API consumers using different API versions.

**What happens when it fails**: Returns "Missing required parameter for API version" error message.

**Where it is enforced**: In the version-specific parameter validation logic.

**Example**:
- Valid: v5.0.0 request includes all v5.0.0 required parameters → Validation passes
- Invalid: v5.0.0 request missing new required parameters → Error returned

---

#### Rule 10.2: API Version Response Format
**What it checks**: Ensures the response format matches the API version specified in the endpoint.

**Why it exists**: To maintain backward compatibility and ensure API consumers receive expected response structures.

**When it applies**: When formatting the account creation response.

**Who it affects**: API consumers using different API versions.

**What happens when it fails**: May result in parsing errors or incompatible response structures.

**Where it is enforced**: In the version-specific response formatting logic (JSONFactory).

**Example**:
- Valid: v4.0.0 endpoint returns v4.0.0 response format → Compatible
- Invalid: v4.0.0 endpoint returns v5.0.0 format → Incompatibility error

---

### 11. Security and Audit Validations

#### Rule 11.1: Audit Trail Requirement
**What it checks**: Ensures all account creation attempts are logged for audit purposes.

**Why it exists**: To maintain compliance with banking regulations and enable security monitoring.

**When it applies**: For every account creation request, regardless of success or failure.

**Who it affects**: Compliance officers, security teams, and auditors who review creation logs.

**What happens when it fails**: Audit logging failure may trigger alerts but should not block the request.

**Where it is enforced**: In the audit logging layer as a cross-cutting concern.

**Example**:
- Scenario: User creates account → Log entry created with user ID, bank ID, account details, timestamp

---

#### Rule 11.2: Input Sanitization
**What it checks**: Ensures all input parameters are sanitized to prevent injection attacks.

**Why it exists**: To protect the system from SQL injection, XSS, and other injection vulnerabilities.

**When it applies**: During initial parameter processing before any database operations.

**Who it affects**: All users making account creation requests.

**What happens when it fails**: Malicious input is rejected with validation error.

**Where it is enforced**: In the input sanitization layer.

**Example**:
- Valid: Normal account label "Savings Account" → Validation passes
- Invalid: Label with SQL injection attempt → Rejected

---

#### Rule 11.3: Rate Limiting Validation
**What it checks**: Monitors and limits the number of account creation requests from a single user or IP address within a time window.

**Why it exists**: To prevent abuse, protect system resources, and detect potential security threats.

**When it applies**: For every account creation request, tracking request frequency.

**Who it affects**: All users, particularly those making frequent requests.

**What happens when it fails**: Returns rate limit exceeded error (typically HTTP 429 Too Many Requests).

**Where it is enforced**: At the API gateway or service level.

**Example**:
- Valid: User creates 5 accounts per hour → Within limits
- Invalid: User attempts to create 100 accounts per minute → Rate limit exceeded

---

### 12. Error Handling Validations

#### Rule 12.1: Invalid Bank ID Error Response
**What it checks**: Validates that appropriate error message is returned when bank ID is invalid.

**Why it exists**: To provide clear feedback about what went wrong.

**When it applies**: When the BANK_ID parameter does not match any existing bank.

**Who it affects**: API consumers and developers integrating with the account creation API.

**What happens when it fails**: Generic or unclear error messages make troubleshooting difficult.

**Where it is enforced**: In the error handling and response formatting logic.

**Example**:
- Response: {"error": "Bank not found", "bank_id": "invalid-bank", "status": 404}

---

#### Rule 12.2: Duplicate Account Number Error Response
**What it checks**: Validates that clear error message is returned when account number already exists.

**Why it exists**: To inform users why account creation failed and help them resolve the issue.

**When it applies**: When account number uniqueness check fails.

**Who it affects**: Users attempting to create accounts with duplicate numbers.

**What happens when it fails**: Unclear error messages lead to confusion.

**Where it is enforced**: In the error handling logic.

**Example**:
- Response: {"error": "Account number already exists", "account_number": "1234567890", "status": 409}

---

#### Rule 12.3: Authorization Error Response
**What it checks**: Validates that clear authorization error is returned when user lacks CanCreateAccount entitlement.

**Why it exists**: To inform users why their request was denied and what permissions they need.

**When it applies**: When entitlement check fails.

**Who it affects**: Users attempting to create accounts without proper authorization.

**What happens when it fails**: Users don't understand why access is denied or how to resolve it.

**Where it is enforced**: In the authorization layer error handling.

**Example**:
- Response: {"error": "Insufficient permissions", "required_entitlement": "CanCreateAccount", "status": 403}

---

#### Rule 12.4: Invalid Currency Error Response
**What it checks**: Validates that appropriate error message is returned when currency code is invalid.

**Why it exists**: To provide clear feedback about currency validation failures.

**When it applies**: When currency code validation fails.

**Who it affects**: Users providing currency codes.

**What happens when it fails**: Unclear error messages lead to confusion.

**Where it is enforced**: In the error handling and response formatting logic.

**Example**:
- Response: {"error": "Invalid currency code", "provided_currency": "INVALID", "expected_format": "ISO 4217 (e.g., USD, EUR, GBP)", "status": 400}

---

#### Rule 12.5: Customer Not Found Error Response
**What it checks**: Validates that appropriate error message is returned when customer/user does not exist.

**Why it exists**: To provide clear feedback about customer validation failures.

**When it applies**: When customer existence validation fails.

**Who it affects**: Users creating accounts for specific customers.

**What happens when it fails**: Unclear error messages lead to confusion.

**Where it is enforced**: In the error handling and response formatting logic.

**Example**:
- Response: {"error": "Customer not found", "user_id": "nonexistent-user", "status": 404}

---

## Validation Rules Summary Table

| Rule ID | Category | Rule Name | Severity | Failure Impact |
|---------|----------|-----------|----------|----------------|
| 1.1 | Authentication | User Authentication Check | Critical | Request blocked |
| 1.2 | Authorization | CanCreateAccount Entitlement | Critical | Access denied |
| 2.1 | Bank/Branch | Bank ID Validation | Critical | Error returned |
| 2.2 | Bank/Branch | Branch ID Validation | Medium | Error returned |
| 3.1 | Customer | Customer/User Existence | Critical | Error returned |
| 4.1 | Account Config | Account Type Validation | High | Error returned |
| 4.2 | Account Config | Account Label Validation | High | Error returned |
| 4.3 | Account Config | Account Description Validation | Low | Error returned |
| 5.1 | Currency/Balance | Currency Code Validation | Critical | Error returned |
| 5.2 | Currency/Balance | Initial Balance Validation | High | Error returned |
| 5.3 | Currency/Balance | Balance Precision Validation | Medium | Error returned |
| 6.1 | Account Number | Uniqueness Validation | Critical | Duplicate prevented |
| 6.2 | Account Number | Format Validation | High | Error returned |
| 6.3 | Account Number | Routing Information Validation | Medium | Error returned |
| 7.1 | Input Format | Bank ID Format Validation | High | Validation error |
| 7.2 | Input Format | Account ID Format Validation | High | Validation error |
| 7.3 | Input Format | User ID Format Validation | High | Validation error |
| 7.4 | Input Format | JSON Request Body Validation | Critical | Parse error |
| 8.1 | Data Integrity | Account Type-Balance Consistency | High | Error returned |
| 8.2 | Data Integrity | Currency-Balance Consistency | Medium | Error returned |
| 8.3 | Data Integrity | Branch-Bank Relationship | High | Error returned |
| 9.1 | Business Rules | Account Creation Atomicity | Critical | Rollback |
| 9.2 | Business Rules | Regulatory Compliance | Critical | Error returned |
| 10.1 | API Compatibility | Version Parameter Requirements | High | Error returned |
| 10.2 | API Compatibility | Version Response Format | High | Parsing error |
| 11.1 | Security | Audit Trail Requirement | High | Compliance issue |
| 11.2 | Security | Input Sanitization | Critical | Injection prevented |
| 11.3 | Security | Rate Limiting Validation | Medium | Request blocked |
| 12.1 | Error Handling | Invalid Bank ID Error | Low | Poor UX |
| 12.2 | Error Handling | Duplicate Account Number Error | Medium | Poor UX |
| 12.3 | Error Handling | Authorization Error | Medium | Poor UX |
| 12.4 | Error Handling | Invalid Currency Error | Low | Poor UX |
| 12.5 | Error Handling | Customer Not Found Error | Low | Poor UX |

---

## Stakeholder-Specific Insights

### For Business Analysts
The Account Creation feature enforces strict validation to ensure accounts are created with complete, valid information. Key business requirements include unique account numbers within each bank, valid account types (checking, savings, loan), proper customer linkage, and appropriate initial balances. The system supports multiple API versions with different parameter requirements, allowing flexibility for various integration scenarios while maintaining data integrity.

### For Compliance Officers
Critical compliance validations include mandatory audit logging of all account creation attempts, CanCreateAccount entitlement enforcement, customer existence verification (supporting KYC requirements), regulatory compliance checking, and proper authentication/authorization. The system prevents duplicate account numbers and ensures all accounts are created with valid, traceable ownership. Account creation with initial deposits is atomic, preventing partial transactions that could violate financial regulations.

### For Quality Assurance Teams
Test scenarios should cover: valid and invalid bank/branch/customer IDs, various account types, currency codes (valid ISO 4217 and invalid), initial balance scenarios (positive, negative, zero, excessive precision), account number uniqueness, different API versions, entitlement checks, and error message accuracy. Edge cases include creating loan accounts with negative balances, accounts without initial balances, accounts with branch associations, and concurrent account creation attempts.

### For Product Managers
Current limitations include the requirement for CanCreateAccount entitlement (preventing self-service account opening), mandatory customer pre-existence (requiring separate customer registration), and account number uniqueness constraints within banks. Different API versions have different parameter requirements, requiring careful version management. The atomic nature of account creation with initial deposits ensures data consistency but may impact performance for high-volume scenarios.

### For Customer Support Teams
Common issues likely include: users without CanCreateAccount entitlement attempting to create accounts, duplicate account number errors, invalid currency codes, customer not found errors, and confusion about required vs. optional fields across different API versions. Clear error messages help identify whether issues are permission-related, data validation failures, or system errors. Account creation failures are atomic, so partial accounts are never created.

### For System Administrators
Configuration considerations include account number generation strategies (which may vary by bank), allowed account types, currency code lists, rate limiting thresholds, and audit logging settings. Monitoring should track creation success rates, authorization failures, duplicate account number attempts, and performance metrics. The system requires proper integration with the core banking system connector for actual account creation. Different API versions may require different configuration parameters.

### For Security Teams
Security validations include authentication verification, CanCreateAccount entitlement enforcement, input parameter sanitization (preventing injection attacks), rate limiting to prevent abuse, and comprehensive audit trail maintenance. The system prevents unauthorized account creation through multiple layers of checks and ensures all input is properly validated and sanitized before processing. Account creation is a privileged operation requiring explicit authorization.

---

## Related Business Rules

The following business rules work together with these validation rules:

1. **Entitlement Model**: Only users with CanCreateAccount entitlement can create accounts
2. **Account Number Uniqueness**: Account numbers must be unique within each bank
3. **Customer Pre-existence**: Customers must be registered before accounts can be created for them
4. **Account Type Rules**: Different account types have different balance and configuration rules
5. **Currency Standards**: All currency codes must follow ISO 4217 standard
6. **Audit Requirement**: All account creation attempts must be logged for compliance
7. **Atomicity Principle**: Account creation with initial deposit is atomic (all or nothing)
8. **Branch Association**: Branches must belong to the specified bank
9. **API Version Compatibility**: Different versions have different parameter requirements
10. **Regulatory Compliance**: Account creation must comply with banking regulations
11. **Balance Rules**: Most account types require non-negative initial balances (except loans)
12. **Routing Standards**: Account routing information must follow banking standards

---

## Implementation Checklist

- [x] User authentication validation implemented
- [x] CanCreateAccount entitlement validation implemented
- [x] Bank ID validation implemented
- [x] Branch ID validation implemented
- [x] Customer/user existence validation implemented
- [x] Account type validation implemented
- [x] Account label validation implemented
- [x] Account description validation implemented
- [x] Currency code validation (ISO 4217) implemented
- [x] Initial balance validation implemented
- [x] Balance precision validation implemented
- [x] Account number uniqueness check implemented
- [x] Account number format validation implemented
- [x] Routing information validation implemented
- [x] Bank ID format validation implemented
- [x] Account ID format validation implemented
- [x] User ID format validation implemented
- [x] JSON request body validation implemented
- [x] Account type-balance consistency validation implemented
- [x] Currency-balance consistency validation implemented
- [x] Branch-bank relationship validation implemented
- [x] Account creation atomicity implemented
- [x] Regulatory compliance validation implemented
- [x] API version parameter requirements implemented
- [x] API version response format implemented
- [x] Audit trail logging implemented
- [x] Input sanitization implemented
- [x] Rate limiting implemented
- [x] Error message clarity for invalid bank ID implemented
- [x] Error message clarity for duplicate account number implemented
- [x] Authorization error messaging implemented
- [x] Error message clarity for invalid currency implemented
- [x] Error message clarity for customer not found implemented

---

## Testing Scenarios

### Positive Test Cases
1. Administrator with CanCreateAccount creates checking account → Account created successfully
2. Valid bank ID, customer ID, account type, and currency provided → Account created
3. Account created with initial balance → Account and balance set correctly
4. Account created without initial balance → Account created with zero balance
5. Loan account created with negative initial balance → Account created successfully
6. Account created with branch association → Account linked to branch correctly
7. Different API versions used → Appropriate response format returned for each version
8. Account with routing information → Routing details stored correctly
9. Account with description → Description stored correctly
10. Unique account number provided → Account created successfully

### Negative Test Cases
1. Unauthenticated request → Returns 401 Unauthorized
2. User without CanCreateAccount entitlement → Returns 403 Forbidden
3. Invalid bank ID → Returns 404 Bank Not Found
4. Invalid customer/user ID → Returns 404 Customer Not Found
5. Invalid account type → Returns 400 Bad Request with allowed types
6. Invalid currency code → Returns 400 Bad Request with ISO 4217 format
7. Duplicate account number → Returns 409 Conflict
8. Negative balance for savings account → Returns 400 Bad Request
9. Missing required label → Returns 400 Bad Request
10. Malformed JSON request → Returns 400 Bad Request
11. Invalid branch ID → Returns 404 Branch Not Found
12. Branch from different bank → Returns 400 Bad Request
13. Rate limit exceeded → Returns 429 Too Many Requests
14. SQL injection attempt in parameters → Returns 400 Bad Request (sanitized)

### Edge Cases
1. Account created with zero initial balance → Balance set to 0.00
2. Account created with maximum allowed balance → Balance stored correctly
3. Account label at maximum length → Label accepted
4. Currency with no decimal places (JPY) → Balance formatted correctly
5. Concurrent account creation with same account number → One succeeds, others fail with duplicate error
6. Account creation during system maintenance → Appropriate error or queued
7. Very long account description → Truncated or rejected based on limits
8. Account creation with all optional fields → Account created with defaults
9. Account creation with minimal required fields only → Account created successfully
10. Multiple accounts for same customer → All created successfully with unique account numbers

---

## Conclusion

The Account Creation feature implements 33 distinct validation rules across 12 categories to ensure secure, accurate, and compliant account creation. These validations work together to enforce authentication, entitlement-based authorization, data integrity, regulatory compliance, and proper error handling. The multi-layered approach ensures that only authorized users can create accounts, all required information is validated, and accounts are created with complete, accurate data.

The validation rules support the core business requirement of allowing authorized bank administrators to create new accounts for customers while maintaining strict data quality and security standards. The system maintains flexibility by supporting multiple API versions and account types, enabling various banking scenarios from simple checking accounts to complex loan accounts with appropriate validation for each.

The atomic nature of account creation with initial deposits ensures data consistency and prevents partial transactions that could lead to financial discrepancies. Comprehensive audit logging and regulatory compliance checks ensure the system meets banking industry standards and regulatory requirements.

Future enhancements should consider self-service account opening workflows (with appropriate identity verification), enhanced account number generation strategies, improved validation for international account formats, and performance optimization for high-volume account creation scenarios.
