# User Story for Signing Basket Authorisation

## Story Overview

**As a** Payment Service Provider (PSP) or Third-Party Provider (TPP)
**I want to** authorise signing baskets with Strong Customer Authentication (SCA)
**So that** I can securely authorise multiple transactions or consents bundled in a signing basket with a single SCA process, reducing friction for the Payment Service User (PSU) while maintaining PSD2 compliance

## Acceptance Criteria

1. The system shall allow starting an authorisation process for a signing basket by creating an authorisation sub-resource
2. The system shall support Strong Customer Authentication (SCA) methods for authorising signing baskets
3. The system shall allow updating PSU data during the authorisation process to complete the SCA flow
4. The system shall track and return the SCA status of the signing basket authorisation
5. The system shall validate challenge answers provided by the PSU during the SCA process
6. Upon successful authorisation (SCA finalised), the system shall mark all related payment transactions as COMPLETED and execute them
7. Upon failed authorisation (SCA failed), the system shall mark all related payment transactions as REJECTED
8. The system shall update the signing basket status to ACTC (AcceptedTechnicalValidation) upon successful authorisation
9. The system shall require PSD2 PISP (Payment Initiation Service Provider) role for authorisation operations

## Technical Context

- **Classes/Services Involved**:
  - `APIMethods_SigningBasketsApi` - REST API endpoint definitions for signing basket authorisation
  - `SigningBasketNewStyle` - Utility functions for signing basket payment validation
  - `SigningBasketProvider` - Provider interface for signing basket operations
  - `MappedSigningBasketProvider` - Implementation of signing basket persistence
  - `NewStyle.function` - Core utility functions for challenge creation and validation
  - `JSONFactory_BERLIN_GROUP_1_3` - JSON serialization for Berlin Group API responses

- **Input Data**:
  - `basketId` (path parameter) - Unique identifier of the signing basket to authorise
  - `authorisationId` (path parameter) - Unique identifier of the authorisation sub-resource
  - `UpdatePaymentPsuDataJson` - JSON body containing `scaAuthenticationData` (OTP or authentication response)

- **Output Data**:
  - Authorisation response with `challengeData`, `scaMethods`, `scaStatus`, `_links` for navigation
  - SCA status response indicating current authorisation state (received, psuAuthenticated, finalised, failed)
  - List of authorisation IDs for a signing basket

- **Processing Type**: Real-time API

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description "Authorise signing baskets with SCA".

---

- **Endpoint**: `POST /signing-baskets/{basketId}/authorisations`
  - **Justification (from description)**: "Authorise" - This endpoint starts the authorisation process for a signing basket
  - **Purpose**: Create an authorisation sub-resource and initiate the SCA process for authorising a signing basket. This creates a challenge for the PSU to complete.
  - **Request**: 
    - Path parameter: `basketId` (string) - The signing basket identifier
    - Body: Empty (authorisation is initiated based on the basket context)
  - **Response**: 
    ```json
    {
      "challengeData": {
        "otpMaxLength": 6,
        "additionalInformation": "string",
        "image": "string",
        "imageLink": "http://example.com/image",
        "otpFormat": "characters",
        "data": "string"
      },
      "scaMethods": [],
      "scaStatus": "received",
      "_links": {
        "scaStatus": "/v1.3/signing-baskets/{basketId}/authorisations/{authorisationId}",
        "authoriseTransaction": "/v1.3/signing-baskets/{basketId}/authorisations/{authorisationId}"
      },
      "chosenScaMethod": {},
      "psuMessage": "Please complete the authentication"
    }
    ```
    - HTTP Status: 201 Created

---

- **Endpoint**: `PUT /signing-baskets/{basketId}/authorisations/{authorisationId}`
  - **Justification (from description)**: "Authorise" and "SCA" - This endpoint completes the authorisation by validating the SCA challenge answer
  - **Purpose**: Update PSU data to complete the SCA authorisation process. This validates the challenge answer (e.g., OTP) and finalises or fails the authorisation.
  - **Request**: 
    - Path parameters: 
      - `basketId` (string) - The signing basket identifier
      - `authorisationId` (string) - The authorisation sub-resource identifier
    - Body:
      ```json
      {
        "scaAuthenticationData": "123456"
      }
      ```
  - **Response**: 
    ```json
    {
      "scaStatus": "finalised",
      "authorisationId": "4f4a8b7f-9968-4183-92ab-ca512b396bfc",
      "psuMessage": "Please check your SMS at a mobile device.",
      "_links": {
        "scaStatus": "/v1.3/signing-baskets/{basketId}/authorisations/{authorisationId}"
      }
    }
    ```
    - HTTP Status: 200 OK

---

- **Endpoint**: `GET /signing-baskets/{basketId}/authorisations/{authorisationId}`
  - **Justification (from description)**: "SCA" - Reading the SCA status is an integral part of the SCA authorisation flow to track progress
  - **Purpose**: Read the current SCA status of a signing basket's authorisation sub-resource to determine if authorisation is complete, pending, or failed.
  - **Request**: 
    - Path parameters:
      - `basketId` (string) - The signing basket identifier
      - `authorisationId` (string) - The authorisation sub-resource identifier
  - **Response**: 
    ```json
    {
      "scaStatus": "psuAuthenticated"
    }
    ```
    - HTTP Status: 200 OK

---

- **Endpoint**: `GET /signing-baskets/{basketId}/authorisations`
  - **Justification (from description)**: "Authorise" - Retrieving authorisation sub-resources is necessary to manage the authorisation process
  - **Purpose**: Read a list of all authorisation sub-resource IDs which have been created for a signing basket. Returns hyperlinks to all generated authorisation sub-resources.
  - **Request**: 
    - Path parameter: `basketId` (string) - The signing basket identifier
  - **Response**: 
    ```json
    {
      "authorisationIds": ["auth-id-1", "auth-id-2"]
    }
    ```
    - HTTP Status: 200 OK

## Business Rules (from capability description)

1. **PSD2 PISP Role Required**: Only users with PSD2 Payment Initiation Service Provider (PISP) role can perform authorisation operations on signing baskets
2. **SCA Challenge Validation**: The system must validate the challenge answer (scaAuthenticationData) against the created challenge before finalising authorisation
3. **Transaction Status Update on Success**: When SCA status becomes "finalised", all payment transactions in the signing basket must be marked as COMPLETED and executed
4. **Transaction Status Update on Failure**: When SCA status becomes "failed", all payment transactions in the signing basket must be marked as REJECTED
5. **Basket Status Update**: Upon successful authorisation, the signing basket status must be updated to ACTC (AcceptedTechnicalValidation)
6. **Payment Validation**: Before completing authorisation, all payment IDs in the signing basket must be validated to ensure they exist
7. **Multi-level SCA Support**: The authorisation process supports iterative SCA for corporate contexts where multiple PSUs need to authorise the same signing basket
8. **Challenge Type**: Signing basket authorisations use the `BERLIN_GROUP_SIGNING_BASKETS_CHALLENGE` challenge type

## Data Validations

- **Basket ID Validation**: The signing basket must exist in the system; returns 403 if not found
- **Authorisation ID Validation**: The authorisation sub-resource must exist for the given basket
- **Payment IDs Validation**: All payment IDs referenced in the signing basket must be valid transaction requests
- **SCA Authentication Data**: The `scaAuthenticationData` field must be provided and must match the expected challenge answer
- **User Authentication**: The user must be authenticated and have PSD2 PISP role
- **Challenge Status**: The challenge must be in an appropriate state for the requested operation

## Dependencies

- **Upstream**:
  - Signing basket must be created first via `POST /signing-baskets` (Signing Basket Creation capability)
  - Payment transactions must be initiated and included in the signing basket
  - User must be authenticated with valid PSD2 PISP credentials

- **Downstream**:
  - Upon successful authorisation, payment transactions are executed via `Connector.connector.vend.makePaymentV400`
  - Transaction request statuses are updated to COMPLETED or REJECTED
  - Signing basket status is updated to reflect authorisation outcome

- **External Systems**:
  - SCA method providers (SMS OTP, Email OTP, etc.) for challenge delivery
  - Payment processing backend for executing authorised payments
  - Connector layer for transaction request management

## Notes for Implementation

- **SCA Methods**: The system supports multiple SCA methods including SMS_OTP, EMAIL, CHIP_OTP, and IMPLICIT. The default method is determined by `getSuggestedDefaultScaMethod()`
- **Challenge Lifecycle**: Challenges progress through states: received -> psuAuthenticated -> finalised/failed
- **Corporate Multi-level SCA**: The `startSigningBasketAuthorisation` endpoint can be called multiple times for corporate contexts requiring multiple PSU authorisations
- **Embedded SCA Approach**: The implementation primarily supports the Embedded SCA approach where PSU credentials and OTP are submitted via API
- **Error Handling**: Invalid challenge answers result in SCA status "failed" and rejection of all related transactions
- **Needs SME Input**: Clarification needed on whether Redirect and Decoupled SCA approaches should be fully supported for signing basket authorisation
