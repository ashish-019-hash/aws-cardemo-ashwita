# User Story for Counterparty Payment

## Story Overview

**As a** Bank Account Holder / Payment Initiator  
**I want to** initiate payments to registered counterparties  
**So that** I can make payments to pre-configured beneficiaries without needing to re-enter their payment details each time, streamlining the payment process and reducing errors

## Acceptance Criteria

1. The system shall allow authorized users to initiate a payment to a registered counterparty
2. The system shall validate that the specified counterparty exists and is registered for the source account
3. The system shall validate that the source account has sufficient funds for the payment amount
4. The system shall create a transaction request for the counterparty payment
5. The system shall use the pre-configured payment routing information from the counterparty record
6. The system shall support Strong Customer Authentication (SCA) challenges when required
7. The system shall return a transaction request ID upon successful payment initiation
8. The system shall reject payment requests with invalid counterparty references with appropriate error messages
9. The system shall process counterparty payments in real-time

## Technical Context

- **Classes/Services Involved**: 
  - Transaction Request service/handler
  - Counterparty validation service
  - Account balance verification service
  - Payment routing service
  - SCA/Challenge service
  - Database/persistence layer for transaction requests

- **Input Data**: 
  - Bank ID (required) - the bank where the source account is held
  - Account ID (required) - the source account for the payment
  - View ID (required) - the view through which the account is accessed
  - Counterparty ID (required) - the registered counterparty to pay
  - Payment amount and currency
  - Payment description/reference (optional)
  - Challenge type for SCA (if applicable)

- **Output Data**: 
  - Transaction request ID
  - Transaction request status (INITIATED, PENDING, COMPLETED, CHALLENGED)
  - Challenge information (if SCA is required)
  - Transaction details including amount, currency, and counterparty information
  - Timestamp of payment initiation
  - Success/error response

- **Processing Type**: API / Real-time

## Relevant Endpoints

**IMPORTANT**: For every endpoint included, justification from the description is provided.

### Endpoint 1: Create Transaction Request (Counterparty)
- **Endpoint**: `POST /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/transaction-request-types/COUNTERPARTY/transaction-requests`
  - **Justification (from description)**: "Initiate payments" - the word "Initiate" explicitly justifies a POST endpoint for creating/initiating a counterparty payment transaction request
  - **Purpose**: Initiate a payment to a registered counterparty using their pre-configured payment details
  - **Request**: 
    ```json
    {
      "to": {
        "counterparty_id": "string"
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
      "type": "COUNTERPARTY",
      "from": {
        "bank_id": "string",
        "account_id": "string"
      },
      "details": {
        "to_counterparty_id": "string",
        "to_counterparty_name": "string",
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
        "allowed_attempts": 3,
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

**Note**: Based on the Operation Derivation Rules, the following endpoints are explicitly EXCLUDED because their operation types are not mentioned in the capability description "Initiate payments to registered counterparties":
- GET /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/transaction-requests - No "view", "retrieve", "get", or "list" mentioned
- GET /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/transaction-requests/{TRANSACTION_REQUEST_ID} - No "view" or "retrieve" mentioned
- DELETE /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/transaction-requests/{TRANSACTION_REQUEST_ID} - No "delete", "cancel", or "remove" mentioned

## Business Rules (from capability description)

1. **Registered Counterparty Requirement**: Payments can only be initiated to counterparties that have been previously registered for the source account
2. **Real-time Processing**: Counterparty payments are processed in real-time (not batch or scheduled)
3. **High Volume Operation**: The system is expected to handle high volumes of counterparty payment requests
4. **Pre-configured Routing**: Payment routing information (IBAN, account number, etc.) is retrieved from the registered counterparty record
5. **Authorization Required**: Only authorized users with appropriate view access can initiate counterparty payments

## Data Validations (if applicable)

- Bank ID must reference an existing bank on the platform
- Account ID must reference a valid account at the specified bank
- View ID must be a valid view that the user has access to with payment initiation permissions
- Counterparty ID must reference a registered counterparty for the source account
- Payment amount must be a positive numeric value
- Currency code must be a valid ISO 4217 currency code
- Currency must be compatible with the source account and counterparty configuration
- Source account must have sufficient available balance for the payment amount plus any applicable charges
- Future date (if specified) must be a valid date in the future

## Dependencies

- **Upstream**: 
  - User authentication and authorization must be completed
  - User must have appropriate view access with payment initiation permissions (e.g., can_add_transaction_request_to_any_account)
  - The source account must exist and be active
  - The counterparty must be registered for the source account
  - Counterparty must have valid payment routing information configured

- **Downstream**: 
  - After payment initiation:
    - SCA challenge may be triggered requiring user response
    - Upon successful completion, a transaction record is created
    - Account balance is updated (debited)
    - Transaction appears in transaction history
    - Counterparty receives the payment (via banking network)

- **External Systems**: 
  - Database/persistence layer for storing transaction requests
  - Counterparty service for retrieving counterparty details
  - Account service for balance verification
  - SCA/Challenge service for Strong Customer Authentication
  - Payment gateway/connector for processing the actual payment
  - Banking network (SEPA, SWIFT, etc.) for external transfers

## Notes for Implementation

- **Authorization**: Ensure proper role-based access control - only users with appropriate view permissions can initiate payments
- **Counterparty Validation**: Verify that the counterparty is registered and active for the source account before processing
- **Balance Check**: Validate sufficient funds before creating the transaction request to avoid failed payments
- **SCA Handling**: Implement Strong Customer Authentication flow when payment amount exceeds thresholds or based on risk assessment
- **Idempotency**: Consider implementing idempotency for payment initiation to prevent duplicate payments
- **Charge Calculation**: Calculate and display any applicable charges before confirming the payment
- **Audit Trail**: Log all payment initiation attempts for compliance and audit purposes
- **Error Handling**: Provide clear, actionable error messages for validation failures (invalid counterparty, insufficient funds, etc.)
- **Currency Conversion**: Handle currency conversion if source account currency differs from payment currency

### Open Questions (Needs SME Input)

1. What are the mandatory vs optional fields for counterparty payment initiation?
2. What SCA challenge types are supported for counterparty payments?
3. Are there transaction limits for counterparty payments (per transaction, daily, monthly)?
4. Can future-dated counterparty payments be scheduled?
5. What charge policies are available (SHARED, OUR, BEN)?
6. How are failed payments handled and communicated to the user?
7. Is there a timeout for completing SCA challenges?
8. Can counterparty payments be cancelled after initiation but before completion?
9. What currencies are supported for counterparty payments?
10. Are there any restrictions based on counterparty type or location?

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Bank Account Holder / Payment Initiator)
- [x] Business value is stated (streamlined payments to pre-configured beneficiaries)
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
