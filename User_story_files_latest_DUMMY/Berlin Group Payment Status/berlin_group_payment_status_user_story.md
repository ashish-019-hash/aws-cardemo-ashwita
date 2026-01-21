# User Story for Berlin Group Payment Status

## Story Overview
**As a** Third-Party Payment Service Provider (TPP) or Payment Initiation Service Provider (PISP)
**I want to** check the payment status per Berlin Group specification
**So that** I can track the progress of initiated payments, verify transaction completion, and provide accurate status updates to end users in compliance with PSD2 regulations

## Acceptance Criteria
1. The system shall allow authorized TPPs to check the status of a previously initiated payment using the payment ID
2. The system shall return the current transaction status of the payment (e.g., RCVD, ACTC, ACCP, ACWC, ACSC, RJCT, CANC)
3. The system shall indicate whether sufficient funds are available for the payment when applicable
4. The system shall support status checks for different payment services (payments, periodic-payments, bulk-payments)
5. The system shall support status checks for different payment products (sepa-credit-transfers, instant-sepa-credit-transfers, target-2-payments, cross-border-credit-transfers)
6. The system shall validate the payment service type and payment product in the request URL
7. The system shall return appropriate error responses for invalid payment IDs or unauthorized access
8. The system shall comply with Berlin Group NextGenPSD2 Framework specifications for payment status responses

## Technical Context
- **Classes/Services Involved**: PaymentInitiationServicePISApi, TransactionRequest service, Account service, FX conversion service
- **Input Data**: Payment service type (path parameter), payment product (path parameter), payment ID (path parameter), authentication headers
- **Output Data**: Transaction status, funds availability indicator, optional PSU message, optional owner names, optional links for next steps
- **Processing Type**: API (HTTP request-response, Real-time)

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description.

- **Endpoint**: GET /{payment-service}/{payment-product}/{paymentId}/status
  - **Justification (from description)**: "Check payment status" - the word "Check" explicitly justifies a GET endpoint for retrieving/checking status information
  - **Purpose**: Check the transaction status of a payment initiation per Berlin Group specification
  - **Request**: 
    - Path Parameters:
      - `payment-service`: The payment service type (payments, periodic-payments, bulk-payments)
      - `payment-product`: The payment product (sepa-credit-transfers, instant-sepa-credit-transfers, target-2-payments, cross-border-credit-transfers)
      - `paymentId`: The unique identifier of the payment to check
    - Headers:
      - `Authorization`: Bearer token or other authentication method
      - `X-Request-ID`: Unique request identifier
      - `Consent-ID`: Consent identifier (if applicable)
  - **Response**: 
    ```json
    {
      "transactionStatus": "ACCP",
      "fundsAvailable": true,
      "psuMessage": "Optional message to the PSU",
      "ownerNames": [
        {
          "name": "Account Owner Name"
        }
      ],
      "_links": {
        "self": {
          "href": "/v1.3/payments/sepa-credit-transfers/{paymentId}/status"
        }
      },
      "tppMessage": []
    }
    ```
  - **HTTP Status Codes**:
    - 200: OK - Payment status retrieved successfully
    - 400: Bad Request - Invalid request parameters
    - 401: Unauthorized - Authentication failed
    - 403: Forbidden - Access denied
    - 404: Not Found - Payment not found

## Business Rules (from capability description)
1. Only authenticated and authorized TPPs with PISP role can check payment status
2. The payment ID must correspond to a valid, previously initiated payment
3. Transaction status values follow Berlin Group specification codes:
   - RCVD: Received - Payment initiation has been received
   - ACTC: AcceptedTechnicalValidation - Authentication and syntactical and semantical validation are successful
   - ACCP: AcceptedCustomerProfile - Preceding check of technical validation was successful and customer profile check was also successful
   - ACWC: AcceptedWithChange - Instruction is accepted but a change will be made
   - ACSC: AcceptedSettlementCompleted - Settlement on the debtor's account has been completed
   - RJCT: Rejected - Payment initiation or individual transaction included in the payment initiation has been rejected
   - CANC: Cancelled - Payment initiation has been cancelled before execution
4. Funds availability check is performed when the transaction status is ACTC, ACWC, or ACCP
5. Currency conversion is applied when checking funds availability if the payment currency differs from the account currency
6. The system must validate that the payment service type and payment product are supported

## Data Validations (if applicable)
- Payment service must be one of: payments, periodic-payments, bulk-payments
- Payment product must be one of: sepa-credit-transfers, instant-sepa-credit-transfers, target-2-payments, cross-border-credit-transfers
- Payment ID must be a valid UUID or transaction request identifier
- Authentication token must be valid and not expired
- TPP must have PISP authorization to access payment status
- Currency conversion rate must be available for funds availability check when currencies differ

## Dependencies
- **Upstream**: 
  - Payment must have been previously initiated via Berlin Group Payment Initiation endpoint
  - User must be authenticated with valid TPP credentials
  - TPP must have PISP role/entitlement
- **Downstream**: 
  - Status information can be used by TPP to update end-user interfaces
  - Status can trigger subsequent actions like payment authorisation or cancellation
  - Funds availability information can inform user decisions
- **External Systems**: 
  - Core banking system for account balance retrieval
  - FX service for currency conversion rates
  - Transaction request storage for payment details

## Notes for Implementation
- The capability description mentions only "Check payment status" - this limits the scope to status retrieval operations only
- No create, update, or delete operations are included as these are not mentioned in the capability description
- Payment initiation, cancellation, and authorisation endpoints are covered by separate capabilities (Berlin Group Payment Initiation, Berlin Group Payment Cancellation, Berlin Group Payment Authorisation)
- The response includes fundsAvailable field which requires real-time balance check and potential currency conversion
- Transaction status mapping from internal OBP status to Berlin Group status codes is handled by the mapTransactionStatus function
- The endpoint supports multiple payment services and products through path parameters, following Berlin Group URL structure
- Consider implementing caching for frequently checked payment statuses to reduce backend load (Needs SME Input on caching strategy)
- Error handling should follow Berlin Group error response format with appropriate HTTP status codes and error messages

## Quality Checklist Verification
- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (TPP/PISP)
- [x] Business value is stated (track payment progress, verify completion, provide status updates, PSD2 compliance)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (caching strategy)
- [x] Only relevant endpoints are included (GET for status check only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words from the capability description justify inclusion ("Check" justifies GET endpoint)
- [x] No endpoint type has been added unless its verb appears in the description
- [x] "Check" has been interpreted as a read/retrieval operation only - no create, update, or delete operations are included
