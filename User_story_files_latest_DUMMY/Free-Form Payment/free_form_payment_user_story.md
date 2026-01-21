# User Story for Free-Form Payment

## Story Overview

**As a** Payment Initiator / Account Holder  
**I want to** initiate payments with flexible parameters  
**So that** I can make payment transactions with customizable payment details that may not fit standard payment templates, enabling greater flexibility in payment processing for various use cases

## Acceptance Criteria

1. The system shall allow authorized users to initiate payments with flexible parameters
2. The system shall accept and validate flexible payment parameters during initiation
3. The system shall process the payment request with the provided flexible parameters
4. The system shall validate the source account and ensure sufficient funds are available
5. The system shall validate the destination/beneficiary information provided in the flexible parameters
6. The system shall return confirmation of successful payment initiation with transaction details
7. The system shall reject payment requests with invalid or incomplete data with appropriate error messages
8. The system shall support real-time processing of free-form payment requests
9. The system shall generate a unique transaction identifier for each initiated payment

## Technical Context

- **Classes/Services Involved**: 
  - Payment initiation service/handler
  - Free-form payment processor
  - Account validation service
  - Transaction request service
  - Parameter validation service
  - Database/persistence layer for transaction storage

- **Input Data**: 
  - Bank ID (required) - the bank where the source account is held
  - Account ID (required) - the source account for the payment
  - View ID (required) - the view/permission context for the operation
  - Payment amount and currency
  - Beneficiary/recipient information (flexible format)
  - Payment description/reference
  - Additional flexible parameters as needed for the specific payment type
  - Transaction request type identifier

- **Output Data**: 
  - Transaction request ID
  - Payment status (initiated, pending, completed, etc.)
  - Transaction details as processed
  - Timestamp of initiation
  - Success/error response with details

- **Processing Type**: API / Real-time

## Relevant Endpoints

**IMPORTANT**: For every endpoint included, justification from the description is provided.

### Endpoint 1: Initiate Free-Form Payment
- **Endpoint**: `POST /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/transaction-request-types/FREE_FORM/transaction-requests`
  - **Justification (from description)**: "Initiate payments" - the word "Initiate" explicitly justifies a POST endpoint for payment initiation
  - **Purpose**: Initiate a free-form payment with flexible parameters from a specified account
  - **Request**: 
    ```json
    {
      "to": {
        "bank_id": "string",
        "account_id": "string"
      },
      "value": {
        "currency": "string",
        "amount": "string"
      },
      "description": "string",
      "charge_policy": "string",
      "future_date": "string"
    }
    ```
  - **Response**: 
    ```json
    {
      "id": "string",
      "type": "FREE_FORM",
      "from": {
        "bank_id": "string",
        "account_id": "string"
      },
      "details": {
        "to": {
          "bank_id": "string",
          "account_id": "string"
        },
        "value": {
          "currency": "string",
          "amount": "string"
        },
        "description": "string"
      },
      "transaction_ids": ["string"],
      "status": "string",
      "start_date": "timestamp",
      "end_date": "timestamp",
      "challenge": {
        "id": "string",
        "allowed_attempts": "integer",
        "challenge_type": "string"
      },
      "charge": {
        "summary": "string",
        "value": {
          "currency": "string",
          "amount": "string"
        }
      }
    }
    ```

**Note**: Based on the Operation Derivation Rules, the following endpoints are explicitly EXCLUDED because their operation types are not mentioned in the capability description:
- GET /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/transaction-request-types/FREE_FORM/transaction-requests - No "view", "retrieve", "get", or "list" mentioned
- PUT /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/transaction-request-types/FREE_FORM/transaction-requests/{REQUEST_ID} - No "update" or "modify" mentioned
- DELETE /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/transaction-request-types/FREE_FORM/transaction-requests/{REQUEST_ID} - No "delete", "cancel", or "remove" mentioned

## Business Rules (from capability description)

1. **Flexible Parameters**: The payment supports flexible parameters, allowing for customizable payment details
2. **Payment Initiation**: This is specifically for initiating payments, not for viewing or managing existing payments
3. **Authorization Required**: Only authorized users with appropriate permissions can initiate payments
4. **Real-time Processing**: Payment initiation is processed in real-time
5. **Medium Volume Operation**: Free-form payment is expected to be a medium-volume operation
6. **Account Context**: Payments are initiated from a specific account within a specific bank

## Data Validations (if applicable)

- Bank ID must reference an existing bank on the platform
- Account ID must reference an existing, valid account
- View ID must reference a valid view with payment initiation permissions
- Payment amount must be a positive numeric value
- Currency code must be a valid ISO 4217 currency code
- Beneficiary information must be provided and valid
- User must have appropriate entitlements for payment initiation
- Source account must have sufficient funds for the payment amount plus any applicable charges
- Payment description should not exceed maximum length limits

## Dependencies

- **Upstream**: 
  - User authentication and authorization must be completed
  - User must have appropriate entitlements/roles for payment initiation (e.g., CanCreateAnyTransactionRequest)
  - The source bank and account must exist on the platform
  - The source account must have sufficient balance
  - The FREE_FORM transaction request type must be enabled for the bank
  - View permissions must allow payment initiation

- **Downstream**: 
  - After payment initiation, the following may occur:
    - Strong Customer Authentication (SCA) challenge may be required
    - Transaction is created upon successful completion
    - Account balance is updated
    - Transaction history is updated
    - Notifications may be sent to relevant parties

- **External Systems**: 
  - Database/persistence layer for storing transaction requests
  - Account service for balance validation and updates
  - Challenge service for SCA if required
  - Backend connector for payment processing
  - Notification service for payment confirmations

## Notes for Implementation

- **Authorization**: Ensure proper role-based access control - only users with appropriate payment initiation entitlements should be able to initiate free-form payments
- **Flexible Parameters**: The "free-form" nature means the system should accept a variety of payment parameter combinations while still enforcing core validations
- **Balance Validation**: Verify sufficient funds before processing the payment initiation
- **SCA Compliance**: Implement Strong Customer Authentication challenges as required by PSD2 regulations
- **Idempotency**: Consider implementing idempotency for payment initiation to handle duplicate requests gracefully
- **Validation Order**: Validate all input data before attempting to process to avoid partial transaction states
- **Audit Trail**: Log all payment initiation events for compliance and audit purposes
- **Error Handling**: Provide clear, actionable error messages for validation failures
- **Charge Calculation**: Calculate and display any applicable charges before final confirmation

### Open Questions (Needs SME Input)

1. What specific flexible parameters are supported for free-form payments?
2. What are the mandatory vs optional parameters for free-form payment initiation?
3. What currencies are supported for free-form payments?
4. What are the transaction limits for free-form payments?
5. Is SCA always required for free-form payments, or are there exemptions?
6. What charge policies are available for free-form payments?
7. Can free-form payments be scheduled for future dates?
8. What validation rules apply to beneficiary information in free-form payments?
9. Are there any restrictions on which accounts can initiate free-form payments?
10. What is the expected processing time for free-form payment completion?

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Payment Initiator / Account Holder)
- [x] Business value is stated (enabling flexible payment processing for various use cases)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (Open Questions section)
- [x] Only relevant endpoints are included (POST for initiation only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific word from capability description justifies inclusion ("Initiate")
- [x] No endpoint type has been added unless its verb appears in the description
- [x] No CRUD operations inferred beyond what description explicitly states
