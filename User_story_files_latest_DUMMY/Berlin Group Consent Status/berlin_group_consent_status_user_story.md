# User Story for Berlin Group Consent Status

## Story Overview

**As a** Third-Party Provider (TPP) / Payment Service Provider (PSP)
**I want to** check the status of an account information consent per Berlin Group specification
**So that** I can verify whether a consent is still valid before accessing account information, ensure regulatory compliance with PSD2 requirements, and provide appropriate feedback to end users about their consent state

## Acceptance Criteria

1. The system shall allow authorized TPPs to retrieve the current status of a consent by providing a valid consent ID
2. The system shall return the consent status in a format compliant with Berlin Group PSD2 specification (NextGenPSD2 XS2A Framework v1.3)
3. The system shall return appropriate error responses when:
   - The consent ID does not exist (403 Forbidden with ConsentNotFound error)
   - The user is not logged in or application access is not granted
   - The caller is not a valid PSD2 AISP (Account Information Service Provider)
4. The system shall support the following consent status values per Berlin Group specification:
   - `received` - Consent request has been received
   - `valid` - Consent is valid and can be used
   - `revokedByPsu` - Consent has been revoked by the Payment Service User
   - `expired` - Consent has expired
   - `terminatedByTpp` - Consent has been terminated by the TPP
5. The response shall be returned with HTTP 200 status code on successful retrieval

## Technical Context

- **Classes/Services Involved**:
  - `APIMethods_AccountInformationServiceAISApi` - REST endpoint handler for Berlin Group AIS API
  - `Consents.consentProvider` - Consent data access provider
  - `MappedConsent` / `ConsentTrait` - Consent data model
  - `JSONFactory_BERLIN_GROUP_1_3` - JSON response factory for Berlin Group v1.3
  - `ConsentStatusJsonV13` - Response case class for consent status
  - `ConsentStatus` - Enumeration of valid consent status values

- **Input Data**:
  - Path Parameter: `consentId` (String) - The unique identifier of the consent to check
  - Headers: Application authentication credentials (OAuth/API key)

- **Output Data**:
  - JSON response containing:
    - `consentStatus` (String) - Current status of the consent (e.g., "received", "valid", "expired", "revokedByPsu", "terminatedByTpp")

- **Processing Type**: Real-time API request-response

## Relevant Endpoints

**IMPORTANT**: The capability description states "Check consent status per Berlin Group specification". The verb "Check" justifies a retrieval/read operation.

- **Endpoint**: `GET /consents/{consentId}/status`
  - **Justification (from description)**: "Check consent status" - the word "check" is a synonym for retrieve/get/view operations
  - **Purpose**: Retrieve the current status of an account information consent
  - **Request**:
    - Method: GET
    - Path: `/v1.3/consents/{consentId}/status`
    - Path Parameter: `consentId` - Unique consent identifier
    - Headers: Authorization (application access credentials)
  - **Response**:
    ```json
    {
      "consentStatus": "received"
    }
    ```
    - HTTP 200: Successful retrieval with consent status
    - HTTP 403: Consent not found or access denied

## Business Rules (from capability description)

1. Only authorized Account Information Service Providers (AISPs) can check consent status - verified via `passesPsd2Aisp` validation
2. The consent must exist in the system to return its status; otherwise, a 403 Forbidden error is returned
3. The consent status reflects the current state of the consent lifecycle as defined by Berlin Group PSD2 specification
4. Application-level access is required (not necessarily user authentication) to check consent status
5. The consent status is constant throughout the lifecycle of a given consent and reflects any changes made through XS2A interface or PSU/ASPSP interface

## Data Validations

- **Consent ID Validation**: The provided consent ID must correspond to an existing consent in the system
- **Application Access Validation**: The calling application must have valid application-level access credentials
- **PSD2 AISP Validation**: The caller must be registered as a valid PSD2 Account Information Service Provider
- **Error Handling**:
  - `ConsentNotFound` error returned with 403 status if consent does not exist
  - `UserNotLoggedIn` error if application access validation fails

## Dependencies

- **Upstream**:
  - A consent must have been previously created via the `POST /consents` endpoint (Berlin Group Consent Creation capability)
  - The TPP must have valid application credentials registered in the system
  - The TPP must be registered as a PSD2 AISP

- **Downstream**:
  - The consent status information is used by TPPs to determine whether to proceed with account information requests
  - If status is "valid", TPP can proceed to access account list, balances, and transactions
  - If status is "expired", "revokedByPsu", or "terminatedByTpp", TPP should not attempt further account access

- **External Systems**:
  - Berlin Group PSD2 specification compliance (NextGenPSD2 XS2A Framework v1.3)
  - OAuth/application authentication system

## Notes for Implementation

- **Berlin Group Compliance**: The implementation must adhere to NextGenPSD2 XS2A Framework v1.3.12 specification for consent status responses
- **Status Values**: The consent status values follow Berlin Group enumeration: received, valid, revokedByPsu, expired, terminatedByTpp
- **Security Consideration**: The endpoint uses application-level access (`applicationAccess`) rather than full user authentication, allowing TPPs to check consent status without requiring the PSU to be actively logged in
- **Response Format**: The response uses `ConsentStatusJsonV13` case class which contains only the `consentStatus` field as a string
- **Error Codes**: Standard OBP error messages are used (ConsentNotFound, UserNotLoggedIn, UnknownError)
- **API Version**: This endpoint is part of Berlin Group v1.3 API (`/v1.3/consents/{consentId}/status`)

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (TPP/PSP)
- [x] Business value is stated (verify consent validity, ensure PSD2 compliance)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (none identified)
- [x] Only relevant endpoints are included (GET /consents/{consentId}/status)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific word from capability description justifies inclusion ("Check" -> retrieval operation)
- [x] No endpoint type added unless verb appears in description ("Check" justifies GET operation)
- [x] No CRUD operations inferred beyond what description explicitly states
