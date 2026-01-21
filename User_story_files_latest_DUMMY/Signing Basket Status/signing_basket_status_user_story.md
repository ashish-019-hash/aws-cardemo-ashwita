# User Story for Signing Basket Status

## Story Overview

**As a** Third-Party Provider (TPP) or Payment Service Provider (PSP)
**I want to** check the status of a signing basket
**So that** I can monitor the authorization progress of batch transactions and determine if the signing basket has been successfully authorized, is pending, or has failed

## Acceptance Criteria

1. The system shall allow authenticated users to retrieve the current status of a signing basket by providing the basket ID
2. The system shall return the transaction status of the signing basket (e.g., RCVD, ACTC, ACCP, ACSC, ACWC, RJCT)
3. The system shall validate that the user has appropriate PSD2 PISP (Payment Initiation Service Provider) permissions before returning the status
4. The system shall return an appropriate error response if the signing basket ID does not exist
5. The system shall return the status in real-time without caching delays

## Technical Context

- **Classes/Services Involved**:
  - `SigningBasketsApi` (APIMethods_SigningBasketsApi) - REST API endpoint handler
  - `SigningBasketProvider` - Data access layer for signing basket operations
  - `SigningBasketX` - Dependency injection container for signing basket provider
  - `JSONFactory_BERLIN_GROUP_1_3` - JSON response factory for Berlin Group compliant responses
  - `SigningBasketStatusResponse200` - Response model containing transaction status

- **Input Data**:
  - Path Parameter: `BASKETID` - The unique identifier of the signing basket
  - Headers: Authorization token for authenticated access

- **Output Data**:
  - Response Body: JSON object containing `transactionStatus` field with values like RCVD (Received), ACTC (AcceptedTechnicalValidation), ACCP (AcceptedCustomerProfile), ACSC (AcceptedSettlementCompleted), ACWC (AcceptedWithChange), RJCT (Rejected)

- **Processing Type**: Real-time API request/response

## Relevant Endpoints

- **Endpoint**: `GET /signing-baskets/{BASKETID}/status`
  - **Justification (from description)**: "Check signing basket status" - the word "check" directly maps to a retrieval/read operation
  - **Purpose**: Returns the current transaction status of a signing basket object
  - **Request**: 
    - Method: GET
    - Path: `/signing-baskets/{BASKETID}/status`
    - Path Parameters: `BASKETID` (string) - Unique identifier of the signing basket
    - Headers: Authorization (Bearer token)
  - **Response**: 
    ```json
    {
      "transactionStatus": "RCVD"
    }
    ```
    - HTTP 200: Success with status information
    - HTTP 401: User not logged in
    - HTTP 403: Insufficient permissions (not a PSD2 PISP)
    - HTTP 404: Signing basket not found

## Business Rules (from capability description)

1. Only authenticated users with valid PSD2 PISP credentials can check signing basket status
2. The signing basket must exist in the system before its status can be retrieved
3. The status reflects the current state of the batch authorization process
4. Status values follow the Berlin Group PSD2 standard transaction status codes:
   - RCVD: Received - Initial state when basket is created
   - ACTC: AcceptedTechnicalValidation - Technical validation passed
   - ACCP: AcceptedCustomerProfile - Customer profile validation passed
   - ACSC: AcceptedSettlementCompleted - All transactions in basket successfully authorized
   - ACWC: AcceptedWithChange - Accepted with modifications
   - RJCT: Rejected - Authorization failed

## Data Validations

- Basket ID must be a valid, non-empty string
- User must be authenticated with a valid access token
- User must have PSD2 PISP role/permissions
- The signing basket with the given ID must exist in the system

## Dependencies

- **Upstream**: 
  - Signing basket must be created first (via POST /signing-baskets)
  - User authentication must be completed
  - PSD2 PISP authorization must be granted

- **Downstream**: 
  - Status information can be used to determine next steps in the authorization flow
  - If status is pending, TPP may need to continue with authorization process
  - If status is completed, TPP can proceed with transaction execution

- **External Systems**: 
  - Authentication/Authorization service for user validation
  - PSD2 compliance verification service

## Notes for Implementation

- The endpoint follows Berlin Group PSD2 NextGenPSD2 Framework v1.3 specification
- Transaction status codes should be consistent with the Berlin Group standard
- The implementation should handle concurrent status checks efficiently
- Consider implementing caching with short TTL for high-frequency status polling scenarios
- Error responses should follow the Berlin Group standard error format with appropriate TPP messages
- The status endpoint is read-only and does not modify the signing basket state
- **Needs SME Input**: Confirm the complete list of valid transaction status values and their transitions
- **Needs SME Input**: Clarify rate limiting requirements for status polling
