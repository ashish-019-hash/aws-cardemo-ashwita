# User Story for Transaction Request Status

## Story Overview

**As a** third-party application developer or account holder
**I want to** check the status of payment/transaction requests
**So that** I can track the progress of initiated payments, verify successful completion, identify failed transactions, and provide real-time status updates to end users

## Acceptance Criteria

1. The system shall allow authorized users to check the current status of a previously submitted transaction request
2. The status check shall return the current state of the transaction request (e.g., pending, processing, completed, failed, cancelled)
3. The system shall provide real-time status information with high availability given the high volume nature of this capability
4. The status response shall include relevant details about the transaction request being queried
5. The system shall return appropriate error responses when querying non-existent or unauthorized transaction requests
6. The status check shall be available for all types of payment/transaction requests initiated through the platform

## Technical Context

- **Classes/Services Involved**: Transaction Request Service, Payment Status Handler, Transaction Request Repository
- **Input Data**: Transaction request identifier, bank identifier, account identifier (as path or query parameters)
- **Output Data**: Transaction request status object containing current state, timestamps, and relevant transaction details
- **Processing Type**: API / Real-time

## Relevant Endpoints

**IMPORTANT**: The capability description states "Check the status of payment/transaction requests". The verb "Check" is a synonym for "view/retrieve/get" operations.

- **Endpoint**: GET /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/transaction-requests/{TRANSACTION_REQUEST_ID}/status
  - **Justification (from description)**: "Check the status" - the word "check" justifies a GET/retrieval operation
  - **Purpose**: Retrieve the current status of a specific transaction request
  - **Request**: Path parameters: BANK_ID (string), ACCOUNT_ID (string), TRANSACTION_REQUEST_ID (string)
  - **Response**: JSON object containing transaction request status, state, timestamps, and related metadata

- **Endpoint**: GET /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/transaction-requests
  - **Justification (from description)**: "Check the status of payment/transaction requests" (plural) - implies ability to check multiple requests
  - **Purpose**: Retrieve a list of transaction requests with their statuses for an account
  - **Request**: Path parameters: BANK_ID (string), ACCOUNT_ID (string); Optional query parameters for filtering
  - **Response**: JSON array of transaction request objects with their current statuses

- **Endpoint**: GET /banks/{BANK_ID}/transaction-request-types
  - **Justification (from description)**: "Check the status of payment/transaction requests" - to check status, users need to know valid transaction request types
  - **Purpose**: Retrieve supported transaction request types for a bank (supporting capability for status checking)
  - **Request**: Path parameter: BANK_ID (string)
  - **Response**: JSON array of supported transaction request types

## Business Rules (from capability description)

1. Status checks must be performed in real-time to provide current transaction state
2. Only authorized users with appropriate permissions can check transaction request status
3. The capability supports high volume operations, indicating it must be optimized for performance
4. Status information must be accurate and reflect the actual state of the transaction in the banking system

## Data Validations

- Transaction request ID must be valid and exist in the system
- Bank ID must correspond to a valid bank on the platform
- Account ID must be valid and the user must have access to the account
- User must be authenticated and authorized to view the transaction request status

## Dependencies

- **Upstream**: 
  - Transaction request must have been previously initiated (depends on payment initiation capabilities)
  - User authentication and authorization must be completed
  - Account and bank must exist in the system
- **Downstream**: 
  - Status information may be used by third-party applications to update their users
  - May trigger notifications or webhooks based on status changes
- **External Systems**: 
  - Core banking system for actual transaction status
  - Payment processing systems for payment-related status updates

## Notes for Implementation

- **Performance Considerations**: Given the "High" volume and "Real-time" frequency, implement caching strategies and optimize database queries for status lookups
- **Status States**: Define clear status states (e.g., INITIATED, PENDING, PROCESSING, COMPLETED, FAILED, CANCELLED, REJECTED) - needs SME input for exact states
- **Audit Trail**: Consider logging all status check requests for audit purposes
- **Error Handling**: Implement comprehensive error responses for various failure scenarios (not found, unauthorized, system errors)
- **Needs SME Input**: 
  - Exact list of transaction request statuses and their meanings
  - Specific fields to include in status response
  - Any bank-specific status variations
  - Rate limiting requirements for high-volume status checks
