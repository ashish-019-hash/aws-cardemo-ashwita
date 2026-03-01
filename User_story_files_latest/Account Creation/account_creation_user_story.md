# User Story for Account Creation

## Story Overview

**As a** Bank Administrator / Account Manager  
**I want to** create new bank accounts with specified parameters and ownership  
**So that** customers can have bank accounts provisioned on the platform with appropriate ownership assignments, enabling them to perform banking operations such as transactions, payments, and balance management

## Acceptance Criteria

1. The system shall allow authorized users to create a new bank account with specified parameters
2. The system shall accept and validate account parameters during creation (account type, currency, initial balance, etc.)
3. The system shall assign ownership to the newly created account as specified in the request
4. The system shall generate a unique account identifier for each newly created account
5. The system shall associate the account with the specified bank entity
6. The system shall validate that the specified owner exists and is eligible for account ownership
7. The system shall return confirmation of successful account creation with the created account details
8. The system shall reject creation requests with invalid or incomplete data with appropriate error messages
9. The system shall ensure the account is created with proper initial state (active, pending, etc.)

## Technical Context

- **Classes/Services Involved**: 
  - Account entity/model classes
  - Account creation service/handler
  - Ownership management service
  - Parameter validation service
  - Bank association service
  - Database/persistence layer for account storage

- **Input Data**: 
  - Bank ID (required) - the bank under which the account is created
  - Account type (e.g., checking, savings, current)
  - Currency code (e.g., EUR, USD, GBP)
  - Account label/name
  - Owner information (user ID, customer ID)
  - Initial balance (if applicable)
  - Account routing information (IBAN, account number scheme)
  - Additional account parameters/attributes

- **Output Data**: 
  - Created account entity with generated account ID
  - Account parameters as stored
  - Ownership assignment confirmation
  - Bank association details
  - Creation timestamp
  - Success/error response

- **Processing Type**: API / On-demand

## Relevant Endpoints

**IMPORTANT**: For every endpoint included, justification from the description is provided.

### Endpoint 1: Create Bank Account
- **Endpoint**: `POST /obp/v5.1.0/banks/{BANK_ID}/accounts`
  - **Justification (from description)**: "Create new bank accounts" - the word "Create" explicitly justifies a POST endpoint for account creation
  - **Purpose**: Create a new bank account under a specified bank with all required parameters and ownership assignment
  - **Request**: 
    ```json
    {
      "user_id": "string",
      "label": "string",
      "product_code": "string",
      "balance": {
        "currency": "string",
        "amount": "string"
      },
      "branch_id": "string",
      "account_routings": [
        {
          "scheme": "string",
          "address": "string"
        }
      ],
      "account_attributes": [
        {
          "product_code": "string",
          "account_attribute_id": "string",
          "name": "string",
          "type": "string",
          "value": "string"
        }
      ]
    }
    ```
  - **Response**: 
    ```json
    {
      "account_id": "string",
      "user_id": "string",
      "label": "string",
      "product_code": "string",
      "balance": {
        "currency": "string",
        "amount": "string"
      },
      "branch_id": "string",
      "account_routings": [...],
      "account_attributes": [...],
      "bank_id": "string",
      "created_at": "timestamp"
    }
    ```

### Endpoint 2: Create Account with Specified Account ID
- **Endpoint**: `PUT /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}`
  - **Justification (from description)**: "Create new bank accounts with specified parameters" - this endpoint allows creation with a specific account ID as a parameter, which is a form of account creation with specified parameters
  - **Purpose**: Create a new bank account with a pre-specified account ID, allowing for deterministic account identifiers
  - **Request**: 
    ```json
    {
      "user_id": "string",
      "label": "string",
      "product_code": "string",
      "balance": {
        "currency": "string",
        "amount": "string"
      },
      "branch_id": "string",
      "account_routings": [
        {
          "scheme": "string",
          "address": "string"
        }
      ]
    }
    ```
  - **Response**: 
    ```json
    {
      "account_id": "string",
      "user_id": "string",
      "label": "string",
      "product_code": "string",
      "balance": {...},
      "branch_id": "string",
      "account_routings": [...],
      "bank_id": "string",
      "created_at": "timestamp"
    }
    ```

**Note**: Based on the Operation Derivation Rules, the following endpoints are explicitly EXCLUDED because their operation types are not mentioned in the capability description:
- GET /banks/{BANK_ID}/accounts/{ACCOUNT_ID} - No "view", "retrieve", or "get" mentioned
- GET /banks/{BANK_ID}/accounts - No "list" or "search" mentioned
- PUT /banks/{BANK_ID}/accounts/{ACCOUNT_ID} (for updates) - No "update" or "modify" mentioned
- DELETE /banks/{BANK_ID}/accounts/{ACCOUNT_ID} - No "delete" or "remove" mentioned

## Business Rules (from capability description)

1. **Account-Bank Association**: Each account must be created under a specific bank entity
2. **Ownership Assignment**: Account creation must include ownership specification (user or customer association)
3. **Parameter Specification**: Accounts are created with specified parameters (type, currency, etc.)
4. **Authorization Required**: Only authorized users (bank administrators, account managers) can create accounts
5. **On-demand Processing**: Account creation is performed on-demand (not batch or scheduled)
6. **Medium Volume Operation**: Account creation is expected to be a medium-volume operation

## Data Validations (if applicable)

- Bank ID must reference an existing bank on the platform
- User ID (owner) must reference an existing, valid user
- Account label must not be empty and should follow naming conventions
- Currency code must be a valid ISO 4217 currency code
- Initial balance amount must be a valid numeric value
- Account routing schemes must be valid (e.g., IBAN, AccountNumber)
- Account routing addresses must conform to the specified scheme format
- Product code must reference a valid banking product if specified
- Branch ID must reference a valid branch if specified

## Dependencies

- **Upstream**: 
  - User authentication and authorization must be completed
  - User must have appropriate entitlements/roles for account creation (e.g., CanCreateAccount)
  - The target bank must exist on the platform
  - The specified owner (user/customer) must exist in the system
  - Banking products must be configured if product_code is required

- **Downstream**: 
  - After account creation, the account becomes available for:
    - Balance inquiries and management
    - Transaction processing
    - Payment initiation
    - View and permission assignments
    - Customer-account linking
    - Card association

- **External Systems**: 
  - Database/persistence layer for storing account entities
  - Bank entity service for bank validation
  - User/Customer service for ownership validation
  - Product catalog service for product validation (if applicable)

## Notes for Implementation

- **Authorization**: Ensure proper role-based access control - only users with CanCreateAccount or similar entitlement should be able to create accounts
- **Ownership Validation**: Verify that the specified owner exists and is eligible to own accounts at the specified bank
- **Bank Validation**: Confirm the target bank exists and is active before creating accounts
- **Idempotency**: Consider implementing idempotency for account creation to handle duplicate requests gracefully
- **Validation Order**: Validate all input data before attempting to persist to avoid partial creation states
- **Default Values**: Define sensible defaults for optional parameters not explicitly provided
- **Audit Trail**: Log account creation events for compliance and audit purposes
- **Error Handling**: Provide clear, actionable error messages for validation failures
- **Account Number Generation**: Implement secure, unique account number/ID generation if not specified

### Open Questions (Needs SME Input)

1. What are the mandatory vs optional parameters for account creation?
2. What account types are supported (checking, savings, current, etc.)?
3. What currencies are supported for account creation?
4. Can an account have multiple owners, or is single ownership enforced?
5. What is the default initial balance for newly created accounts?
6. Are there any restrictions on which users can own accounts at specific banks?
7. Should account creation trigger any downstream notifications or events (e.g., welcome emails)?
8. What validation rules apply to account routing information (IBAN format, etc.)?
9. Is there a limit on the number of accounts a single user can own?
10. What is the initial status of a newly created account (active, pending approval, etc.)?

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Bank Administrator / Account Manager)
- [x] Business value is stated (enabling customers to have bank accounts for banking operations)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (Open Questions section)
- [x] Only relevant endpoints are included (POST/PUT for creation only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific word from capability description justifies inclusion ("Create", "specified parameters", "ownership")
- [x] No endpoint type has been added unless its verb appears in the description
- [x] No CRUD operations inferred beyond what description explicitly states
