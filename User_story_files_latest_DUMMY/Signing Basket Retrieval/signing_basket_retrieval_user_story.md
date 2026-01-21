# User Story for Signing Basket Retrieval

## Story Overview

**As a** Third-Party Provider (TPP) / Payment Initiation Service Provider (PISP)
**I want to** retrieve signing basket information
**So that** I can view the current state and contents of a signing basket that groups multiple transactions for batch authorization under PSD2 Berlin Group compliance

## Acceptance Criteria

1. The system shall allow authenticated TPPs with PISP role to retrieve signing basket information by basket ID
2. The system shall return the transaction status of the signing basket (e.g., ACCP, RCVD)
3. The system shall return the list of payment IDs associated with the signing basket
4. The system shall return the list of consent IDs associated with the signing basket (if applicable)
5. The system shall return HTTP 200 status code on successful retrieval
6. The system shall return appropriate error response if the basket ID does not exist
7. The system shall enforce PSD2 PISP authorization before allowing access to signing basket data

## Technical Context

- **Classes/Services Involved**: 
  - `SigningBasketsApi` - REST API endpoint handler for signing basket operations
  - `SigningBasketX.signingBasketProvider` - Provider for signing basket data access
  - `JSONFactory_BERLIN_GROUP_1_3` - JSON response factory for Berlin Group compliant responses
  - `SigningBasketNewStyle` - New style helper for signing basket operations

- **Input Data**: 
  - Path Parameter: `BASKETID` - The unique identifier of the signing basket to retrieve
  - Authentication: Valid user session with PISP authorization

- **Output Data**: 
  - `transactionStatus` - Current status of the signing basket (e.g., "ACCP", "RCVD")
  - `payments` - List of payment IDs included in the basket
  - `consents` - List of consent IDs included in the basket (if applicable)

- **Processing Type**: Real-time API request-response

## Relevant Endpoints

- **Endpoint**: GET /signing-baskets/{BASKETID}
  - **Justification (from description)**: "Retrieve signing basket information" - the word "Retrieve" explicitly justifies this GET endpoint
  - **Purpose**: Returns the content of a signing basket object including its transaction status and associated payment/consent IDs
  - **Request**: 
    - Method: GET
    - Path: `/signing-baskets/{BASKETID}`
    - Path Parameter: `BASKETID` (string) - Unique identifier of the signing basket
    - Headers: Authorization header with valid access token
  - **Response**: 
    ```json
    {
      "transactionStatus": "ACCP",
      "payments": ["payment-id-1", "payment-id-2"],
      "consents": ["consent-id-1"]
    }
    ```
    - HTTP 200: Successful retrieval
    - HTTP 401: User not logged in
    - HTTP 403: Insufficient permissions (not PISP authorized)
    - HTTP 404: Signing basket not found

## Business Rules (from capability description)

1. Only authenticated users with valid PISP (Payment Initiation Service Provider) authorization can retrieve signing basket information
2. The signing basket must exist in the system to be retrieved
3. The response must comply with Berlin Group PSD2 specification format
4. Transaction status reflects the current state of the batch authorization process

## Data Validations

- Basket ID must be a valid, non-empty string
- User must be authenticated with a valid session
- User must have PISP role/authorization to access signing basket data
- The requested signing basket must exist in the system

## Dependencies

- **Upstream**: 
  - Signing basket must have been previously created via POST /signing-baskets endpoint
  - User must be authenticated via OAuth 2.0 or other supported authentication method
  - User must have PISP entitlements granted

- **Downstream**: 
  - Retrieved basket information can be used to check status before initiating authorization
  - Information can be used to verify which payments/consents are grouped in the basket

- **External Systems**: 
  - Authentication/Authorization system for user validation
  - Signing basket data store for retrieving basket information

## Notes for Implementation

- The endpoint follows Berlin Group PSD2 specification for signing baskets
- The `passesPsd2Pisp` check ensures only authorized TPPs can access this endpoint
- The response format must maintain compatibility with Berlin Group v1.3 specification
- Error handling should provide meaningful error messages while not exposing sensitive system information
- Consider caching strategies for frequently accessed signing baskets to improve performance
- Logging should capture access attempts for audit trail purposes

---

*This user story was extracted from the OBP-API Scala codebase based on the "Signing Basket Retrieval" capability from the BRD document.*
