# User Story for Standing Order Creation

## Story Overview

**As a** Bank Customer / Account Holder  
**I want to** create standing orders for recurring payments  
**So that** I can automate regular payment obligations (such as rent, subscriptions, loan repayments, or utility bills) without having to manually initiate each payment, ensuring timely and consistent payments to beneficiaries

## Acceptance Criteria

1. The system shall allow authorized users to create a new standing order for recurring payments
2. The system shall accept and validate standing order parameters during creation (amount, frequency, start date, end date, beneficiary details)
3. The system shall associate the standing order with a specific source account for debiting
4. The system shall generate a unique standing order identifier for each newly created standing order
5. The system shall validate that the source account exists and the user has authorization to create standing orders on it
6. The system shall validate beneficiary/counterparty information for the recurring payment destination
7. The system shall return confirmation of successful standing order creation with the created standing order details
8. The system shall reject creation requests with invalid or incomplete data with appropriate error messages
9. The system shall ensure the standing order is created with proper initial state (active, pending, etc.)
10. The system shall validate that the payment frequency and schedule are valid and supported

## Technical Context

- **Classes/Services Involved**: 
  - Standing Order entity/model classes
  - Standing Order creation service/handler
  - Account validation service
  - Beneficiary/Counterparty validation service
  - Payment scheduling service
  - Database/persistence layer for standing order storage

- **Input Data**: 
  - Bank ID (required) - the bank where the standing order is created
  - Account ID (required) - the source account for debiting
  - View ID (required) - the view context for the operation
  - Beneficiary/Counterparty details (account number, IBAN, bank details)
  - Payment amount and currency
  - Payment frequency (daily, weekly, monthly, quarterly, annually)
  - Start date for the standing order
  - End date (optional) - when the standing order should terminate
  - Payment reference/description
  - Additional standing order parameters/attributes

- **Output Data**: 
  - Created standing order entity with generated standing order ID
  - Standing order parameters as stored
  - Source account association confirmation
  - Beneficiary details confirmation
  - Schedule details (frequency, start date, end date)
  - Creation timestamp
  - Success/error response

- **Processing Type**: API / On-demand

## Relevant Endpoints

**IMPORTANT**: For every endpoint included, justification from the description is provided.

### Endpoint 1: Create Standing Order
- **Endpoint**: `POST /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/standing-order`
  - **Justification (from description)**: "Create standing orders" - the word "Create" explicitly justifies a POST endpoint for standing order creation
  - **Purpose**: Create a new standing order for recurring payments from a specified account
  - **Request**: 
    ```json
    {
      "counterparty_id": "string",
      "amount": {
        "currency": "string",
        "amount": "string"
      },
      "when": {
        "frequency": "string",
        "detail": "string"
      },
      "date_starts": "date",
      "date_expires": "date",
      "description": "string"
    }
    ```
  - **Response**: 
    ```json
    {
      "standing_order_id": "string",
      "bank_id": "string",
      "account_id": "string",
      "counterparty_id": "string",
      "amount": {
        "currency": "string",
        "amount": "string"
      },
      "when": {
        "frequency": "string",
        "detail": "string"
      },
      "date_starts": "date",
      "date_expires": "date",
      "date_signed": "date",
      "date_cancelled": "date",
      "date_active": "date",
      "active": "boolean"
    }
    ```

### Endpoint 2: Create Standing Order with IBAN
- **Endpoint**: `POST /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/standing-order/IBAN`
  - **Justification (from description)**: "Create standing orders for recurring payments" - this endpoint allows creation of standing orders using IBAN for the beneficiary, which is a form of standing order creation for recurring payments
  - **Purpose**: Create a new standing order for recurring payments using IBAN as the beneficiary identifier
  - **Request**: 
    ```json
    {
      "iban": "string",
      "amount": {
        "currency": "string",
        "amount": "string"
      },
      "when": {
        "frequency": "string",
        "detail": "string"
      },
      "date_starts": "date",
      "date_expires": "date",
      "description": "string"
    }
    ```
  - **Response**: 
    ```json
    {
      "standing_order_id": "string",
      "bank_id": "string",
      "account_id": "string",
      "iban": "string",
      "amount": {
        "currency": "string",
        "amount": "string"
      },
      "when": {
        "frequency": "string",
        "detail": "string"
      },
      "date_starts": "date",
      "date_expires": "date",
      "date_signed": "date",
      "date_cancelled": "date",
      "date_active": "date",
      "active": "boolean"
    }
    ```

**Note**: Based on the Operation Derivation Rules, the following endpoints are explicitly EXCLUDED because their operation types are not mentioned in the capability description:
- GET /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/standing-orders - No "view", "retrieve", "list", or "get" mentioned
- GET /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/standing-order/{STANDING_ORDER_ID} - No "view" or "retrieve" mentioned
- PUT /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/standing-order/{STANDING_ORDER_ID} - No "update", "modify", or "manage" mentioned
- DELETE /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/standing-order/{STANDING_ORDER_ID} - No "delete", "cancel", or "remove" mentioned

## Business Rules (from capability description)

1. **Account Association**: Each standing order must be created under a specific bank account
2. **Recurring Payment Purpose**: Standing orders are specifically for recurring payments (not one-time transfers)
3. **Authorization Required**: Only authorized users with appropriate view permissions can create standing orders
4. **On-demand Processing**: Standing order creation is performed on-demand (not batch or scheduled)
5. **Medium Volume Operation**: Standing order creation is expected to be a medium-volume operation
6. **Beneficiary Specification**: Standing orders must specify a valid beneficiary/counterparty for the recurring payments

## Data Validations (if applicable)

- Bank ID must reference an existing bank on the platform
- Account ID must reference an existing, valid account
- View ID must reference a valid view with appropriate permissions
- Counterparty ID or IBAN must reference a valid payment destination
- Currency code must be a valid ISO 4217 currency code
- Amount must be a valid positive numeric value
- Frequency must be a supported payment frequency (daily, weekly, monthly, quarterly, annually)
- Start date must be a valid future date or current date
- End date (if provided) must be after the start date
- Description/reference should follow any character length or format restrictions

## Dependencies

- **Upstream**: 
  - User authentication and authorization must be completed
  - User must have appropriate view permissions for standing order creation (e.g., can_add_standing_order)
  - The source account must exist and be active
  - The beneficiary/counterparty must be valid and reachable
  - Sufficient account balance or credit limit for the recurring payment amount

- **Downstream**: 
  - After standing order creation, the system will:
    - Execute recurring payments according to the specified schedule
    - Debit the source account at each payment interval
    - Credit the beneficiary account with each payment
    - Generate transaction records for each executed payment
    - Send notifications for payment execution (if configured)

- **External Systems**: 
  - Database/persistence layer for storing standing order entities
  - Account service for account validation and balance checks
  - Counterparty/Beneficiary service for destination validation
  - Payment scheduling/execution service for recurring payment processing
  - Notification service for payment confirmations (if applicable)

## Notes for Implementation

- **Authorization**: Ensure proper role-based access control - only users with can_add_standing_order or similar entitlement should be able to create standing orders
- **Account Validation**: Verify that the source account exists, is active, and the user has authorization to create standing orders on it
- **Beneficiary Validation**: Confirm the beneficiary/counterparty exists and is valid for receiving payments
- **Schedule Validation**: Validate that the payment schedule (frequency, start date, end date) is valid and supported
- **Idempotency**: Consider implementing idempotency for standing order creation to handle duplicate requests gracefully
- **Validation Order**: Validate all input data before attempting to persist to avoid partial creation states
- **Default Values**: Define sensible defaults for optional parameters not explicitly provided (e.g., no end date means indefinite)
- **Audit Trail**: Log standing order creation events for compliance and audit purposes
- **Error Handling**: Provide clear, actionable error messages for validation failures
- **Standing Order ID Generation**: Implement secure, unique standing order ID generation

### Open Questions (Needs SME Input)

1. What are the mandatory vs optional parameters for standing order creation?
2. What payment frequencies are supported (daily, weekly, monthly, quarterly, annually, custom)?
3. What currencies are supported for standing order payments?
4. Is there a minimum or maximum amount limit for standing order payments?
5. Can a standing order have multiple beneficiaries, or is single beneficiary enforced?
6. What is the default behavior if no end date is specified (indefinite or requires explicit end date)?
7. Should standing order creation trigger any downstream notifications or events (e.g., confirmation emails)?
8. What validation rules apply to beneficiary information (IBAN format, BIC validation, etc.)?
9. Is there a limit on the number of standing orders a single account can have?
10. What is the initial status of a newly created standing order (active immediately, pending approval, etc.)?
11. How are failed standing order payments handled (retry logic, notification, suspension)?
12. Can standing orders be created for cross-border/international payments?

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Bank Customer / Account Holder)
- [x] Business value is stated (automating regular payment obligations for timely and consistent payments)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (Open Questions section)
- [x] Only relevant endpoints are included (POST for creation only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific word from capability description justifies inclusion ("Create", "recurring payments")
- [x] No endpoint type has been added unless its verb appears in the description
- [x] No CRUD operations inferred beyond what description explicitly states
