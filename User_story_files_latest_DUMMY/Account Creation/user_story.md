# User Story for Account Creation

## Story Overview

**As a** Bank Administrator or Account Manager  
**I want to** create new bank accounts with specified parameters and ownership  
**So that** customers can have properly configured bank accounts to conduct financial transactions and manage their funds

## Acceptance Criteria

1. The system shall allow authorized users to create new bank accounts under a specific bank entity
2. The system shall generate a unique account identifier for each newly created account when not explicitly specified
3. The system shall validate that the specified bank exists and is active before creating the account
4. The system shall assign ownership to the newly created account as specified in the request
5. The system shall validate that the specified owner (user) exists and is eligible for account ownership
6. The system shall accept and validate currency codes against ISO 4217 standards
7. The system shall accept and validate account routing information (IBAN, account numbers) when provided
8. The system shall ensure the account is created with proper initial state (active, pending)
9. The system shall return the created account details including the generated or specified account ID
10. The system shall reject account creation requests with invalid or missing required parameters

## Technical Context

**Classes/Services Involved:**
- `MappedBankAccount` - Primary entity for bank account storage
- `MapperAccountHolders` - Links users to bank accounts for ownership
- `BankAccountRouting` - Stores routing information (IBAN, account numbers)
- `MappedAccountAttribute` - Stores additional custom attributes
- `MappedBank` - Bank entity referenced via BANK_ID
- `ResourceUser` - User entity for ownership validation
- `MappedBranch` - Branch entity for branch association
- `MappedProduct` - Product entity for product code validation

**Input Data:**
- Bank ID (required) - path parameter identifying the target bank
- Account ID (optional for POST, required for PUT) - pre-specified account identifier
- Label (required) - human-readable account name
- Product code (optional) - banking product type (e.g., checking, savings)
- Balance object:
  - Currency (required) - ISO 4217 currency code (e.g., EUR, USD, GBP)
  - Amount (required) - initial balance amount
- User ID (required) - owner of the account
- Branch ID (optional) - associated branch
- Account routings (optional) - array of routing information:
  - Scheme (e.g., IBAN, AccountNumber)
  - Address (routing value)
- Account attributes (optional) - custom attributes array

**Output Data:**
- Created account entity with:
  - Account ID (generated or specified)
  - Bank ID
  - Label
  - Currency
  - Balance
  - Account type/product
  - Owner information
  - Routing information
  - Creation timestamp

**Processing Type:** API (HTTP request-response)

## Relevant Endpoints

**IMPORTANT**: Each endpoint is justified by specific words/phrases from the capability description.

### Endpoint 1: Create Bank Account (Auto-generated ID)

- **Endpoint**: `POST /obp/v5.1.0/banks/{BANK_ID}/accounts`
- **Justification (from description)**: "Create new bank accounts" - the word "Create" directly justifies this POST endpoint
- **Purpose**: Create a new bank account with a system-generated unique account ID
- **Request**:
  ```json
  {
    "user_id": "string",
    "label": "string",
    "product_code": "string",
    "balance": {
      "currency": "EUR",
      "amount": "0"
    },
    "branch_id": "string",
    "account_routings": [
      {
        "scheme": "IBAN",
        "address": "string"
      }
    ],
    "account_attributes": [
      {
        "product_code": "string",
        "account_attribute_id": "string",
        "name": "string",
        "type": "STRING",
        "value": "string"
      }
    ]
  }
  ```
- **Response**:
  ```json
  {
    "account_id": "string",
    "bank_id": "string",
    "label": "string",
    "currency": "EUR",
    "balance": {
      "currency": "EUR",
      "amount": "0"
    },
    "account_routings": [...],
    "account_attributes": [...]
  }
  ```

### Endpoint 2: Create Bank Account (Pre-specified ID)

- **Endpoint**: `PUT /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}`
- **Justification (from description)**: "Create new bank accounts with specified parameters" - the word "Create" and "specified parameters" justify this PUT endpoint for creating accounts with a pre-specified account ID
- **Purpose**: Create a new bank account with a user-specified account ID
- **Request**:
  ```json
  {
    "user_id": "string",
    "label": "string",
    "product_code": "string",
    "balance": {
      "currency": "EUR",
      "amount": "0"
    },
    "branch_id": "string",
    "account_routings": [
      {
        "scheme": "IBAN",
        "address": "string"
      }
    ],
    "account_attributes": [...]
  }
  ```
- **Response**:
  ```json
  {
    "account_id": "string",
    "bank_id": "string",
    "label": "string",
    "currency": "EUR",
    "balance": {...},
    "account_routings": [...],
    "account_attributes": [...]
  }
  ```

## Business Rules

1. **Account-Bank Association**: Each account must be created under a specific bank entity. The bank must exist and be active on the platform.

2. **Ownership Assignment**: Account creation must include ownership specification. The owner (user_id) must exist and be eligible for account ownership.

3. **Authorization Requirement**: Only authorized users with appropriate roles (Bank Administrator, Account Manager) and entitlements (CanCreateAccount) can create bank accounts.

4. **Account Identifier Generation**: The system must generate a unique account identifier for each newly created account when not explicitly specified (POST endpoint). When specified (PUT endpoint), the ID must be validated for uniqueness.

5. **Currency Compliance**: Account currency must be a valid ISO 4217 currency code. The bank must support the specified currency.

6. **Initial State Management**: Newly created accounts must be initialized with a proper state (active, pending) based on business configuration.

## Data Validations

### Required Field Validations
- Bank ID must be provided and reference an existing bank
- User ID (owner) must be provided and reference an existing, valid user
- Account label must not be empty and should follow naming conventions
- Currency code must be provided in the balance object
- Balance amount must be a valid numeric value

### Format Validations
- Currency code must be a valid ISO 4217 currency code (3-letter code)
- Account routing schemes must be valid (e.g., IBAN, AccountNumber)
- Account routing addresses must conform to the specified scheme format
- IBAN addresses must pass checksum validation
- Account ID (when specified) must contain only alphanumeric characters, hyphens, underscores, and periods (max 255 characters)

### Business Constraint Validations
- Bank must exist and be active in the system
- User must exist and be eligible for account ownership
- Product code must reference a valid banking product if specified
- Branch ID must reference a valid branch if specified
- Account ID must be unique (no duplicates allowed)

### Error Codes
- `OBP-30001`: Bank not found (404)
- `OBP-20005`: User not found (404)
- `OBP-10002`: Invalid value for required field (400)
- `OBP-10003`: Invalid currency value (400)
- `OBP-30005`: Invalid account ID format (400)
- `OBP-30110`: Invalid account routing scheme (400)
- `OBP-30111`: Invalid IBAN format (400)

## Dependencies

**Upstream:**
- User authentication must be completed before account creation
- Authorization/entitlement verification must pass
- The target bank must exist on the platform
- The specified owner (user/customer) must exist in the system
- Banking products must be configured if product_code is required

**Downstream:**
- Created account becomes available for transaction operations
- Account can be linked to customers
- Views and permissions can be assigned to the account
- Account routing information enables payment processing

**External Systems:**
- Backend connector (REST, Akka, or other configured connector) for account persistence
- User management system for owner validation
- Bank management system for bank validation

## Notes for Implementation

### Special Considerations
- Validate all input data before attempting to persist to avoid partial creation states
- Use transactional processing to ensure atomicity of account creation with ownership assignment
- Balance amounts are stored in smallest currency units (cents, pence, etc.) and converted for display
- Account creation involves creating/updating multiple related entities (account, ownership, routing, attributes)

### Known Complexity
- Multiple routing schemes may need different validation logic
- Currency validation requires ISO 4217 reference data
- IBAN validation requires country-specific format rules and checksum algorithm
- Authorization checks involve role and entitlement verification at bank level

### Missing or Unclear Requirements (Needs SME Input)
- What is the default initial state for newly created accounts (ACTIVE vs PENDING)?
- Are there any approval workflows required for certain account types?
- What are the specific product codes available and their validation rules?
- Are there limits on the number of accounts a user can own?
- What are the specific entitlement names required for account creation?

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Bank Administrator, Account Manager)
- [x] Business value is stated (customers can conduct financial transactions)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged for SME input
- [x] Only relevant endpoints are included (POST and PUT for creation)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words from the description justify inclusion
- [x] No CRUD operations inferred beyond what description explicitly states (only "Create" is mentioned)
