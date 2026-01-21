# User Story for Berlin Group Payment Cancellation

## Story Overview

**As a** Payment Service Provider (PSP) or Third-Party Provider (TPP) acting on behalf of an account holder

**I want to** cancel payments per Berlin Group specification

**So that** I can provide account holders with the ability to stop pending payment transactions before they are executed, ensuring compliance with PSD2 regulations and giving users control over their initiated payments

## Acceptance Criteria

1. The system shall allow cancellation of payment transactions that are in a pending or authorized state per Berlin Group NextGenPSD2 specification
2. The system shall return an appropriate HTTP status code and response body indicating the cancellation result
3. The system shall reject cancellation requests for payments that have already been executed or are in a non-cancellable state
4. The system shall validate that the requesting TPP has proper authorization to cancel the specified payment
5. The system shall update the payment status to "CANC" (Cancelled) upon successful cancellation
6. The system shall return Berlin Group compliant error responses when cancellation is not possible
7. The system shall support cancellation for all payment products (sepa-credit-transfers, instant-sepa-credit-transfers, target-2-payments, cross-border-credit-transfers)
8. The system shall support cancellation for all payment services (payments, bulk-payments, periodic-payments)

## Technical Context

- **Classes/Services Involved**: 
  - Berlin Group Payment Cancellation API endpoint handlers
  - Payment status management service
  - Authorization validation service
  - Backend connector for payment cancellation

- **Input Data**: 
  - Payment ID (path parameter)
  - Payment service type (path parameter: payments, bulk-payments, periodic-payments)
  - Payment product type (path parameter: sepa-credit-transfers, instant-sepa-credit-transfers, etc.)
  - Authorization headers (OAuth 2.0 Bearer token, X-Request-ID, PSU-ID, etc.)

- **Output Data**: 
  - Payment cancellation response with transaction status
  - HTTP status codes (204 No Content for successful cancellation, 202 Accepted for pending cancellation)
  - Error responses in Berlin Group format for failed cancellations

- **Processing Type**: API (Real-time request-response)

## Relevant Endpoints

**IMPORTANT**: The following endpoint is justified by the explicit use of "Cancel" in the capability description.

- **Endpoint**: DELETE /v1/{payment-service}/{payment-product}/{paymentId}
  - **Justification (from description)**: "Cancel payments per Berlin Group specification"
  - **Purpose**: Cancel a previously initiated payment transaction before execution
  - **Request**: 
    - Path Parameters:
      - `payment-service`: Type of payment service (payments, bulk-payments, periodic-payments)
      - `payment-product`: Type of payment product (sepa-credit-transfers, instant-sepa-credit-transfers, target-2-payments, cross-border-credit-transfers)
      - `paymentId`: Unique identifier of the payment to cancel
    - Headers:
      - `Authorization`: Bearer token for OAuth 2.0 authentication
      - `X-Request-ID`: Unique request identifier (UUID)
      - `PSU-ID`: Payment Service User identifier (optional)
      - `PSU-IP-Address`: IP address of the PSU (conditional)
  - **Response**: 
    - Success (204 No Content): Payment successfully cancelled
    - Success (202 Accepted): Cancellation request accepted, pending processing
    - Response Body (when applicable):
      ```json
      {
        "transactionStatus": "CANC",
        "_links": {
          "self": {
            "href": "/v1/payments/sepa-credit-transfers/{paymentId}"
          }
        }
      }
      ```
    - Error (400 Bad Request): Invalid request format
    - Error (401 Unauthorized): Authentication failed
    - Error (403 Forbidden): Not authorized to cancel this payment
    - Error (404 Not Found): Payment not found
    - Error (405 Method Not Allowed): Cancellation not supported for this payment type

## Business Rules (from capability description)

1. **Cancellation Eligibility**: Only payments in pending, authorized, or partially authorized states can be cancelled. Executed payments cannot be cancelled.

2. **Berlin Group Compliance**: All cancellation requests and responses must conform to the Berlin Group NextGenPSD2 specification format and semantics.

3. **Authorization Requirement**: The TPP requesting cancellation must have valid authorization and the payment must have been initiated by the same TPP or the PSU must have granted appropriate permissions.

4. **Payment Service Support**: Cancellation must be supported across all payment service types (single payments, bulk payments, periodic payments) as defined by Berlin Group.

5. **Payment Product Support**: Cancellation must be supported for all payment products offered by the bank (SEPA credit transfers, instant SEPA, TARGET2, cross-border transfers).

6. **Idempotency**: Repeated cancellation requests for an already cancelled payment should return success without side effects.

## Data Validations (if applicable)

- **Payment ID Validation**: The paymentId must be a valid identifier for an existing payment transaction
- **Payment Service Validation**: The payment-service path parameter must be one of: payments, bulk-payments, periodic-payments
- **Payment Product Validation**: The payment-product path parameter must be a valid payment product supported by the bank
- **Authorization Validation**: The Bearer token must be valid and not expired
- **X-Request-ID Validation**: Must be a valid UUID format
- **State Validation**: Payment must be in a cancellable state (not already executed or cancelled)
- **TPP Authorization Validation**: The requesting TPP must be authorized to cancel the specified payment

## Dependencies

- **Upstream**: 
  - Berlin Group Payment Initiation - A payment must have been previously initiated before it can be cancelled
  - Berlin Group Payment Authorisation - Payment may need to be in authorized state
  - OAuth 2.0 / OpenID Connect Authentication - Valid authentication required

- **Downstream**: 
  - Payment status is updated to CANC (Cancelled) in the system
  - Backend banking system processes the cancellation
  - Any scheduled execution of the payment is stopped

- **External Systems**: 
  - Backend banking/core banking system for actual payment cancellation
  - Payment processing infrastructure
  - Audit logging system for compliance tracking

## Notes for Implementation

- **SCA Consideration**: Depending on the bank's implementation and regulatory requirements, Strong Customer Authentication (SCA) may be required for payment cancellation. This should be clarified with SMEs.

- **Timing Constraints**: The ability to cancel a payment may be time-sensitive. Some payment products (e.g., instant payments) may have very short windows for cancellation or may not support cancellation at all once initiated.

- **Partial Cancellation**: For bulk payments, clarify whether partial cancellation (cancelling individual payments within a bulk) is supported or if only full bulk cancellation is allowed.

- **Cancellation Fees**: Determine if any fees apply to payment cancellation and how they should be communicated to the user.

- **Audit Trail**: Ensure all cancellation attempts (successful and failed) are logged for regulatory compliance and audit purposes.

- **Error Handling**: Implement comprehensive error handling with Berlin Group compliant error codes and messages for all failure scenarios.

- **Needs SME Input**: 
  - Specific cancellation windows for different payment products
  - SCA requirements for cancellation
  - Partial cancellation support for bulk payments
  - Fee structure for cancellations (if any)
  - Bank-specific restrictions on cancellation

---

*This user story was extracted based on the capability description: "Cancel payments per Berlin Group specification" from the BRD document.*
