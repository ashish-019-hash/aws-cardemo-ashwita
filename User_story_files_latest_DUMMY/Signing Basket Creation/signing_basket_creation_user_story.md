# User Story for Signing Basket Creation

## Story Overview
**As a** Third-Party Provider (TPP) or Payment Service User (PSU)
**I want to** create signing baskets for batch authorisation
**So that** I can authorize multiple transactions with a single Strong Customer Authentication (SCA) process, improving efficiency and user experience in PSD2-compliant payment workflows

## Acceptance Criteria
1. The system shall allow creation of a signing basket containing multiple payment or consent references
2. The signing basket shall be created with a unique basket identifier for tracking
3. The system shall validate that all referenced transactions are eligible for batch authorisation
4. The signing basket shall be associated with the authenticated PSU
5. The system shall return appropriate confirmation upon successful basket creation
6. The system shall reject basket creation if any referenced transaction is invalid or already authorised
7. The signing basket creation shall comply with Berlin Group PSD2 specification requirements

## Technical Context
- **Classes/Services Involved**: SigningBasketService, PSD2ComplianceService, AuthorisationService
- **Input Data**: 
  - List of payment IDs or consent IDs to be included in the basket
  - PSU identification data
  - TPP identification headers (X-Request-ID, PSU-IP-Address, etc.)
- **Output Data**: 
  - Signing basket ID
  - Basket status
  - List of included transaction references
  - SCA methods available for authorisation
  - Links to authorisation endpoints
- **Processing Type**: API/Real-time (On-demand)

## Relevant Endpoints

**IMPORTANT**: For every endpoint included, justification from the description is provided.

- **Endpoint**: POST /v1/signing-baskets
  - **Justification (from description)**: "Create signing baskets" - the word "Create" explicitly justifies a POST endpoint
  - **Purpose**: Creates a new signing basket containing multiple transactions for batch authorisation
  - **Request**: 
    ```json
    {
      "paymentIds": ["payment-id-1", "payment-id-2"],
      "consentIds": ["consent-id-1"]
    }
    ```
    Headers:
    - X-Request-ID: Unique request identifier
    - PSU-IP-Address: IP address of the PSU
    - PSU-ID: PSU identifier (optional)
    - TPP-Redirect-URI: Redirect URI after SCA (optional)
  - **Response**: 
    ```json
    {
      "basketId": "signing-basket-123",
      "transactionStatus": "RCVD",
      "paymentIds": ["payment-id-1", "payment-id-2"],
      "consentIds": ["consent-id-1"],
      "scaMethods": [
        {
          "authenticationType": "SMS_OTP",
          "authenticationMethodId": "sms-otp-1"
        }
      ],
      "_links": {
        "self": {"href": "/v1/signing-baskets/signing-basket-123"},
        "startAuthorisation": {"href": "/v1/signing-baskets/signing-basket-123/authorisations"}
      }
    }
    ```

## Business Rules (from capability description)
1. A signing basket must contain at least one valid payment or consent reference
2. All transactions in the basket must belong to the same PSU
3. Transactions already authorised cannot be added to a new signing basket
4. The basket creation follows Berlin Group PSD2 specification for batch authorisation
5. The signing basket enables single SCA for multiple transactions (batch authorisation)
6. The basket must be created before authorisation can be initiated

## Data Validations (if applicable)
- Payment IDs must reference existing, valid payment initiation requests
- Consent IDs must reference existing, valid consent requests
- At least one payment ID or consent ID must be provided
- All referenced transactions must be in a state eligible for authorisation (not already authorised or cancelled)
- TPP must have appropriate permissions to access the referenced transactions
- X-Request-ID header must be unique for idempotency

## Dependencies
- **Upstream**: 
  - Payment initiation requests must exist before being added to a signing basket
  - Consent requests must exist before being added to a signing basket
  - PSU must be authenticated
- **Downstream**: 
  - Signing basket authorisation process (SCA) follows basket creation
  - Transaction status updates after successful basket authorisation
- **External Systems**: 
  - SCA provider for Strong Customer Authentication
  - Berlin Group PSD2 compliance validation

## Notes for Implementation
- The signing basket is a PSD2 Berlin Group specific feature for batch authorisation
- Implementation must follow Berlin Group NextGenPSD2 specification
- The basket ID should be generated as a unique identifier (UUID recommended)
- Consider implementing idempotency based on X-Request-ID header
- The basket should have a configurable expiration time
- **Needs SME Input**: Specific timeout/expiration policy for signing baskets
- **Needs SME Input**: Maximum number of transactions allowed in a single basket
- **Needs SME Input**: Whether mixed baskets (payments + consents) are supported
