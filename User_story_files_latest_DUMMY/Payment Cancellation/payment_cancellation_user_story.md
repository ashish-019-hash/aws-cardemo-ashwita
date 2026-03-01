# User Story for Payment Cancellation

## Story Overview
**As a** Payment Service Provider or Account Holder
**I want to** cancel pending payment transactions
**So that** I can stop unauthorized or erroneous payments before they are processed, maintain control over outgoing funds, and correct mistakes in payment initiation

## Acceptance Criteria
1. The system shall allow cancellation of payment transactions that are in a pending state
2. The system shall reject cancellation requests for payments that have already been processed or completed
3. The system shall validate that the requester has appropriate authorization to cancel the specified payment
4. The system shall return a confirmation when a payment is successfully cancelled
5. The system shall return an appropriate error response when cancellation fails (e.g., payment already processed, insufficient permissions)
6. The system shall update the payment status to "cancelled" upon successful cancellation

## Technical Context
- **Classes/Services Involved**: Payment cancellation service, Transaction status management, Authorization validation
- **Input Data**: Payment/Transaction identifier, Bank identifier, Account identifier, Cancellation reason (optional)
- **Output Data**: Cancellation confirmation, Updated payment status, Error details if cancellation fails
- **Processing Type**: API (Real-time request-response)

## Relevant Endpoints

**IMPORTANT**: Only endpoints justified by the capability description "Cancel pending payment transactions" are included.

- **Endpoint**: DELETE /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/transaction-requests/{TRANSACTION_REQUEST_ID}
  - **Justification (from description)**: "Cancel pending payment transactions" - the word "cancel" explicitly justifies a DELETE/cancellation endpoint
  - **Purpose**: Cancel a pending payment transaction request
  - **Request**: 
    - Path Parameters: BANK_ID (string), ACCOUNT_ID (string), TRANSACTION_REQUEST_ID (string)
    - Headers: Authorization token, Content-Type
  - **Response**: 
    - Success: HTTP 200/204 with cancellation confirmation
    - Error: HTTP 400 (invalid request), HTTP 403 (unauthorized), HTTP 404 (transaction not found), HTTP 409 (transaction already processed)

- **Endpoint**: POST /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/transaction-requests/{TRANSACTION_REQUEST_ID}/cancel
  - **Justification (from description)**: "Cancel pending payment transactions" - alternative REST pattern using POST with action verb for cancellation operations
  - **Purpose**: Cancel a pending payment transaction request using action-based endpoint
  - **Request**: 
    - Path Parameters: BANK_ID (string), ACCOUNT_ID (string), TRANSACTION_REQUEST_ID (string)
    - Body: Optional cancellation reason
  - **Response**: 
    - Success: HTTP 200 with updated transaction status showing "CANCELLED"
    - Error: HTTP 400 (invalid request), HTTP 403 (unauthorized), HTTP 404 (transaction not found), HTTP 409 (transaction already processed)

## Business Rules (from capability description)
1. Only pending payment transactions can be cancelled - completed or processed payments cannot be reversed through this capability
2. The requester must have appropriate authorization to cancel the payment (typically the payment initiator or an authorized administrator)
3. Cancellation must occur before the payment reaches a terminal processing state
4. The payment status must be updated to reflect the cancelled state

## Data Validations (if applicable)
- Transaction request ID must be valid and exist in the system
- Transaction must be in a "pending" or cancellable state
- Requester must have authorization to cancel the specified transaction
- Bank ID and Account ID must be valid and associated with the transaction

## Dependencies
- **Upstream**: Payment must have been previously initiated through one of the payment initiation capabilities (SEPA Credit Transfer, Account-to-Account Transfer, etc.)
- **Downstream**: Payment processing systems must be notified to halt processing; Transaction status must be updated; Audit logs must record the cancellation
- **External Systems**: Core banking system for payment status updates, Notification services for alerting relevant parties

## Notes for Implementation
- Consider implementing idempotency to handle duplicate cancellation requests gracefully
- Implement proper audit logging for all cancellation attempts (successful and failed)
- Consider time-based restrictions on cancellation (e.g., cut-off times for same-day payments)
- **Needs SME Input**: Specific business rules around cancellation windows and cut-off times
- **Needs SME Input**: Whether partial cancellation is supported for bulk payments
- **Needs SME Input**: Notification requirements when a payment is cancelled
- The capability description only mentions "cancel" - no view, list, or retrieval operations are included as they are not explicitly stated in the description
