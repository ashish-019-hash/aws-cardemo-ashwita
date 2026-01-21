# User Story for Direct Debit Creation

## Story Overview

**As a** Bank Account Holder / Account Manager  
**I want to** create direct debit mandates for recurring collections  
**So that** I can authorize third parties (counterparties) to collect payments directly from my bank account on a recurring basis, enabling automated payment collection for subscriptions, utility bills, loan repayments, and other recurring financial obligations

## Acceptance Criteria

1. The system shall allow authorized users to create a direct debit mandate for a specific bank account
2. The system shall require a valid customer ID to associate the direct debit with a customer record
3. The system shall require a valid user ID to identify the authorizing user
4. The system shall require a valid counterparty ID to identify the payment collector
5. The system shall accept a date when the mandate was signed (optional, defaults to current date)
6. The system shall require a start date for when the direct debit collections can begin
7. The system shall accept an optional expiration date for the mandate
8. The system shall validate that the specified customer, user, and counterparty exist in the system
9. The system shall verify that the user has appropriate view permissions (can_create_direct_debit) on the account
10. The system shall generate a unique direct debit ID for each newly created mandate
11. The system shall return the created direct debit mandate details upon successful creation
12. The system shall reject creation requests with invalid or incomplete data with appropriate error messages

## Technical Context

- **Classes/Services Involved**: 
  - DirectDebitProvider - Interface for direct debit operations
  - MappedDirectDebitProvider - Implementation for direct debit persistence
  - DirectDebitTrait - Model trait defining direct debit properties
  - APIMethods400 - API endpoint definitions
  - JSONFactory400 - JSON serialization/deserialization
  - Customer validation service
  - User validation service
  - Counterparty validation service
  - View permission validation

- **Input Data**: 
  - Bank ID (path parameter) - the bank where the account is held
  - Account ID (path parameter) - the account to set up direct debit on
  - View ID (path parameter) - the view context for permission checking
  - customer_id (required) - ID of the customer authorizing the direct debit
  - user_id (required) - ID of the user creating the mandate
  - counterparty_id (required) - ID of the counterparty who will collect payments
  - date_signed (optional) - date when the mandate was signed (defaults to current date)
  - date_starts (required) - date when collections can begin
  - date_expires (optional) - date when the mandate expires

- **Output Data**: 
  - direct_debit_id - unique identifier for the created mandate
  - bank_id - the bank identifier
  - account_id - the account identifier
  - customer_id - the customer identifier
  - user_id - the user identifier
  - counterparty_id - the counterparty identifier
  - date_signed - the date the mandate was signed
  - date_starts - the start date for collections
  - date_expires - the expiration date (if set)
  - date_cancelled - cancellation date (null for new mandates)
  - active - boolean indicating if the mandate is active

- **Processing Type**: API / On-demand

## Relevant Endpoints

**IMPORTANT**: For every endpoint included, justification from the description is provided.

### Endpoint 1: Create Direct Debit
- **Endpoint**: `POST /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/direct-debit`
  - **Justification (from description)**: "Create direct debit mandates" - the word "Create" explicitly justifies a POST endpoint for direct debit creation
  - **Purpose**: Create a new direct debit mandate for a specific bank account, allowing a counterparty to collect recurring payments
  - **Request**: 
    ```json
    {
      "customer_id": "string",
      "user_id": "string",
      "counterparty_id": "string",
      "date_signed": "2025-01-15T00:00:00Z",
      "date_starts": "2025-02-01T00:00:00Z",
      "date_expires": "2026-02-01T00:00:00Z"
    }
    ```
  - **Response**: 
    ```json
    {
      "direct_debit_id": "string",
      "bank_id": "string",
      "account_id": "string",
      "customer_id": "string",
      "user_id": "string",
      "counterparty_id": "string",
      "date_signed": "2025-01-15T00:00:00Z",
      "date_starts": "2025-02-01T00:00:00Z",
      "date_expires": "2026-02-01T00:00:00Z",
      "date_cancelled": null,
      "active": true
    }
    ```

### Endpoint 2: Create Direct Debit (Management)
- **Endpoint**: `POST /obp/v4.0.0/management/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/direct-debit`
  - **Justification (from description)**: "Create direct debit mandates" - the word "Create" explicitly justifies a POST endpoint for direct debit creation. This management endpoint provides an alternative path for administrative creation without view-based permission checks.
  - **Purpose**: Create a new direct debit mandate through the management API, typically used by bank administrators with appropriate entitlements (canCreateDirectDebitAtOneBank)
  - **Request**: 
    ```json
    {
      "customer_id": "string",
      "user_id": "string",
      "counterparty_id": "string",
      "date_signed": "2025-01-15T00:00:00Z",
      "date_starts": "2025-02-01T00:00:00Z",
      "date_expires": "2026-02-01T00:00:00Z"
    }
    ```
  - **Response**: 
    ```json
    {
      "direct_debit_id": "string",
      "bank_id": "string",
      "account_id": "string",
      "customer_id": "string",
      "user_id": "string",
      "counterparty_id": "string",
      "date_signed": "2025-01-15T00:00:00Z",
      "date_starts": "2025-02-01T00:00:00Z",
      "date_expires": "2026-02-01T00:00:00Z",
      "date_cancelled": null,
      "active": true
    }
    ```

**Note**: Based on the Operation Derivation Rules, the following endpoints are explicitly EXCLUDED because their operation types are not mentioned in the capability description:
- GET /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/direct-debits - No "view", "retrieve", "list", or "get" mentioned
- GET /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/direct-debits/{DIRECT_DEBIT_ID} - No "view" or "retrieve" mentioned
- PUT /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/direct-debits/{DIRECT_DEBIT_ID} - No "update" or "modify" mentioned
- DELETE /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/direct-debits/{DIRECT_DEBIT_ID} - No "delete", "cancel", or "remove" mentioned

## Business Rules (from capability description)

1. **Mandate Authorization**: Direct debit mandates must be created by authorized users with appropriate permissions on the account
2. **Customer Association**: Each direct debit must be associated with a valid customer record
3. **Counterparty Requirement**: A valid counterparty must be specified as the payment collector
4. **Date Validation**: The start date must be provided; the mandate becomes active from this date
5. **Recurring Collections**: Direct debits are specifically designed for recurring payment collections
6. **On-demand Processing**: Direct debit creation is performed on-demand (not batch or scheduled)
7. **Medium Volume Operation**: Direct debit creation is expected to be a medium-volume operation

## Data Validations (if applicable)

- Bank ID must reference an existing bank on the platform
- Account ID must reference a valid account at the specified bank
- View ID must reference a valid view with can_create_direct_debit permission (for non-management endpoint)
- Customer ID must reference an existing customer record
- User ID must reference an existing, valid user
- Counterparty ID must reference an existing counterparty record
- date_starts must be a valid date (required field)
- date_signed, if provided, must be a valid date
- date_expires, if provided, must be a valid date and should be after date_starts
- All date fields must be in valid ISO 8601 format

## Dependencies

- **Upstream**: 
  - User authentication and authorization must be completed
  - User must have appropriate view permissions (can_create_direct_debit) or entitlements (canCreateDirectDebitAtOneBank for management endpoint)
  - The target bank must exist on the platform
  - The target account must exist at the specified bank
  - The specified customer must exist in the system
  - The specified user must exist in the system
  - The specified counterparty must be registered in the system

- **Downstream**: 
  - After direct debit creation, the mandate becomes available for:
    - Payment collection by the counterparty
    - Recurring transaction processing
    - Direct debit management operations (if that capability is enabled)
    - Reporting and audit purposes

- **External Systems**: 
  - Database/persistence layer for storing direct debit entities (MappedDirectDebit)
  - Customer service for customer validation
  - User service for user validation
  - Counterparty service for counterparty validation
  - View service for permission validation

## Notes for Implementation

- **Authorization**: Two authorization paths exist:
  1. View-based: User must have can_create_direct_debit permission on the specified view
  2. Management-based: User must have canCreateDirectDebitAtOneBank entitlement
- **Customer Validation**: Verify that the specified customer exists before creating the mandate
- **User Validation**: Verify that the specified user exists before creating the mandate
- **Counterparty Validation**: Verify that the specified counterparty exists and is valid for payment collection
- **Default Date Handling**: If date_signed is not provided, default to the current date
- **Active Status**: Newly created direct debits should be set to active=true by default
- **Idempotency**: Consider implementing idempotency for direct debit creation to handle duplicate requests gracefully
- **Audit Trail**: Log direct debit creation events for compliance and audit purposes
- **Error Handling**: Provide clear, actionable error messages for validation failures

### Open Questions (Needs SME Input)

1. What are the business rules for determining when a direct debit mandate becomes effective?
2. Are there any limits on the number of direct debit mandates per account?
3. What validation rules apply to the relationship between customer, user, and counterparty?
4. Should there be a minimum notice period between date_signed and date_starts?
5. Are there any restrictions on which counterparties can collect via direct debit?
6. What happens if a direct debit is created with a past date_starts?
7. Should direct debit creation trigger any downstream notifications (e.g., confirmation emails)?
8. What are the regulatory requirements for direct debit mandate creation (e.g., SEPA Direct Debit rules)?
9. Is there a maximum duration for a direct debit mandate (time between date_starts and date_expires)?
10. How should the system handle direct debit creation for accounts with insufficient permissions?

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Bank Account Holder / Account Manager)
- [x] Business value is stated (enabling automated recurring payment collection)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (Open Questions section)
- [x] Only relevant endpoints are included (POST for creation only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific word from capability description justifies inclusion ("Create")
- [x] No endpoint type has been added unless its verb appears in the description
- [x] Words like "manage" have been interpreted narrowly - no view/list/delete operations included since only "Create" is mentioned
- [x] No CRUD operations inferred beyond what description explicitly states
