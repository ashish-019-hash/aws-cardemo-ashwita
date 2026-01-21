# User Story for Berlin Group SCA Status

## Story Overview

**As a** Third-Party Provider (TPP) or Payment Service Provider (PSP)
**I want to** check Strong Customer Authentication status
**So that** I can verify the current state of SCA processes for consent and payment authorisations, ensuring PSD2 compliance and providing appropriate feedback to end users during authentication flows

## Acceptance Criteria

1. The system shall allow checking the SCA status of a consent authorisation by providing the consent ID and authorisation ID
2. The system shall allow checking the SCA status of a payment authorisation by providing the payment service, payment product, payment ID, and authorisation ID
3. The system shall allow checking the SCA status of a payment cancellation authorisation by providing the payment service, payment product, payment ID, and cancellation ID
4. The system shall return the current SCA status value (e.g., received, psuAuthenticated, finalised)
5. The system shall return appropriate error responses for invalid or non-existent authorisation IDs
6. The system shall require proper authentication (PSD2 AISP role for consent SCA status, PSD2 PISP role for payment SCA status)

## Technical Context

- **Classes/Services Involved**:
  - `AccountInformationServiceAISApi` - Handles consent SCA status retrieval
  - `PaymentInitiationServicePISApi` - Handles payment and payment cancellation SCA status retrieval
  - `JSONFactory_BERLIN_GROUP_1_3` - Creates SCA status response JSON
  - `ScaStatusResponse` - Response model for SCA status
  - `ChallengeProvider` - Provides challenge/authorisation data including SCA status
  - `NewStyle.function.getChallenge` - Retrieves challenge information
  - `NewStyle.function.getChallengesByConsentId` - Retrieves challenges for a consent

- **Input Data**:
  - For consent SCA status: `consentId` (path parameter), `authorisationId` (path parameter)
  - For payment SCA status: `paymentService` (path parameter), `paymentProduct` (path parameter), `paymentId` (path parameter), `authorisationId` (path parameter)
  - For payment cancellation SCA status: `paymentService` (path parameter), `paymentProduct` (path parameter), `paymentId` (path parameter), `cancellationId` (path parameter)

- **Output Data**:
  - `ScaStatusResponse` containing:
    - `scaStatus`: String (required) - Current SCA status value
    - `psuMessage`: Optional[String] - Text to be displayed to the PSU
    - `psuName`: Optional[String] - Name of the PSU
    - `trustedBeneficiaryFlag`: Optional[Boolean] - Whether creditor is in trusted beneficiary list
    - `_links`: Optional[LinksAll] - HATEOAS links
    - `tppMessage`: Optional[Seq[TppMessageGeneric]] - Messages to TPP on operational issues

- **Processing Type**: Real-time API

## Relevant Endpoints

**IMPORTANT**: Each endpoint is justified by the word "Check" in the capability description "Check Strong Customer Authentication status".

### Endpoint 1: Get Consent SCA Status
- **Endpoint**: GET /consents/{consentId}/authorisations/{authorisationId}
  - **Justification (from description)**: "Check" - justifies retrieval/read operation
  - **Purpose**: Read the SCA status of a consent initiation's authorisation sub-resource
  - **Request**: 
    - Path parameters: `consentId`, `authorisationId`
    - Headers: Authorization (Bearer token), X-Request-ID
  - **Response**: 
    ```json
    {
      "scaStatus": "received"
    }
    ```

### Endpoint 2: Get Payment Initiation SCA Status
- **Endpoint**: GET /{payment-service}/{payment-product}/{paymentId}/authorisations/{authorisationId}
  - **Justification (from description)**: "Check" - justifies retrieval/read operation
  - **Purpose**: Read the SCA status of a payment initiation's authorisation sub-resource
  - **Request**: 
    - Path parameters: `payment-service` (e.g., payments, periodic-payments, bulk-payments), `payment-product` (e.g., sepa-credit-transfers), `paymentId`, `authorisationId`
    - Headers: Authorization (Bearer token), X-Request-ID
  - **Response**: 
    ```json
    {
      "scaStatus": "psuAuthenticated"
    }
    ```

### Endpoint 3: Get Payment Cancellation SCA Status
- **Endpoint**: GET /{payment-service}/{payment-product}/{paymentId}/cancellation-authorisations/{cancellationId}
  - **Justification (from description)**: "Check" - justifies retrieval/read operation
  - **Purpose**: Read the SCA status of a payment cancellation's authorisation sub-resource
  - **Request**: 
    - Path parameters: `payment-service`, `payment-product`, `paymentId`, `cancellationId`
    - Headers: Authorization (Bearer token), X-Request-ID
  - **Response**: 
    ```json
    {
      "scaStatus": "finalised"
    }
    ```

## Business Rules (from capability description)

1. SCA status must be checked in real-time to reflect the current state of the authorisation process
2. Only authenticated TPPs with appropriate PSD2 roles (AISP for consent, PISP for payment) can check SCA status
3. The SCA status values follow the Berlin Group PSD2 specification (received, psuIdentified, psuAuthenticated, scaMethodSelected, started, finalised, failed, exempted)
4. SCA status checking is a high-volume operation supporting real-time authentication flows

## Data Validations (if applicable)

- `consentId` must be a valid existing consent identifier
- `authorisationId` must be a valid existing authorisation identifier associated with the consent or payment
- `paymentId` must be a valid existing payment identifier
- `payment-service` must be one of: payments, periodic-payments, bulk-payments
- `payment-product` must be a valid payment product (e.g., sepa-credit-transfers, instant-sepa-credit-transfers, target-2-payments, cross-border-credit-transfers)
- Request must include valid authentication credentials

## Dependencies

- **Upstream**: 
  - Consent must be created before checking consent SCA status
  - Payment must be initiated before checking payment SCA status
  - Authorisation sub-resource must exist for the consent or payment
  - User must be authenticated with appropriate PSD2 role

- **Downstream**: 
  - TPP uses SCA status to determine next steps in the authentication flow
  - SCA status of "finalised" indicates the authorisation process is complete
  - SCA status of "failed" indicates the authorisation process has failed

- **External Systems**: 
  - Authentication provider for TPP authentication
  - Challenge/authorisation storage system

## Notes for Implementation

- The SCA status is retrieved from the Challenge entity associated with the authorisation
- If no SCA status is found, the system returns "None" as the status value
- The implementation follows the Berlin Group NextGenPSD2 Framework Version 1.3
- Payment service types include: payments, periodic-payments, bulk-payments
- Payment products are validated against supported transaction request types
- The endpoint paths use kebab-case for payment service and payment product parameters
- HATEOAS links may be included in the response to guide the TPP to related resources
