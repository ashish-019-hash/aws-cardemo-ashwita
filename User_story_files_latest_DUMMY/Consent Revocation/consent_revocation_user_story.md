# User Story for Consent Revocation

## Story Overview

**As a** Account Holder or Third-Party Application (TPP)
**I want to** revoke previously granted consents
**So that** I can terminate an application's access to my account data when I no longer wish to share it, or when the application is determined to be compromised or malicious

## Acceptance Criteria

1. An authenticated user can revoke their own consent by specifying the consent ID
2. The system validates that the consent exists before attempting revocation
3. The system validates that the requesting user owns the consent being revoked
4. Upon successful revocation, the consent status is changed to "REVOKED" (for OBP consents) or "terminatedByTpp" (for Berlin Group consents)
5. The revoked consent's last action date is updated to the current timestamp
6. Subsequent API requests using the revoked consent token fail validation
7. If a consent is already revoked, the system returns an appropriate error message (ConsentAlreadyRevoked)
8. If the consent is not found, the system returns a 404 error with ConsentNotFound message
9. The system supports multiple revocation methods: by consent ID at bank level, by consent ID for current user, and self-revocation using the current call's consent

## Technical Context

- **Classes/Services Involved**:
  - `ConsentProvider` - Interface defining consent operations including revoke methods
  - `MappedConsentProvider` - Implementation of consent provider with revoke and revokeBerlinGroupConsent methods
  - `MappedConsent` - Data model representing consent records with status management
  - `APIMethods510` - API endpoint definitions for v5.1.0 including revokeConsentAtBank, selfRevokeConsent, revokeMyConsent
  - `APIMethods310` - Legacy API endpoint for revokeConsent (deprecated in v5.1.0)
  - `AccountInformationServiceAISApi` - Berlin Group AIS API with deleteConsent endpoint

- **Input Data**:
  - `consentId` (path parameter) - The unique identifier of the consent to revoke
  - `bankId` (path parameter, optional) - Bank identifier for bank-scoped revocation
  - `Consent-Id` (request header, for self-revocation) - Consent ID from request header

- **Output Data**:
  - `ConsentJsonV310` - Response containing consentId, jsonWebToken, and updated status ("REVOKED")
  - HTTP 200 for successful OBP consent revocation
  - HTTP 204 (No Content) for successful Berlin Group consent deletion
  - Error responses with appropriate error codes and messages

- **Processing Type**: API / Real-time

## Relevant Endpoints

**IMPORTANT**: For every endpoint included, justification from the description is provided.

- **Endpoint**: DELETE /banks/BANK_ID/consents/CONSENT_ID
  - **Justification (from description)**: "Revoke previously granted consents" - the word "revoke" directly justifies this DELETE endpoint
  - **Purpose**: Revoke a specific consent at a bank level, requiring bank administrator or consent owner privileges
  - **Request**: 
    - Path parameters: `BANK_ID` (bank identifier), `CONSENT_ID` (consent identifier)
    - Authentication: Required (user must be logged in)
    - Authorization: User must own the consent or have `canRevokeConsentAtBank` entitlement
  - **Response**: 
    ```json
    {
      "consent_id": "string",
      "jwt": "string",
      "status": "REVOKED"
    }
    ```

- **Endpoint**: DELETE /my/consents/CONSENT_ID
  - **Justification (from description)**: "Revoke previously granted consents" - the word "revoke" directly justifies this DELETE endpoint for user's own consents
  - **Purpose**: Allow users to revoke their own consents without specifying a bank
  - **Request**:
    - Path parameters: `CONSENT_ID` (consent identifier)
    - Authentication: Required (user must be logged in)
    - Authorization: User must own the consent
  - **Response**:
    ```json
    {
      "consent_id": "string",
      "jwt": "string",
      "status": "REVOKED"
    }
    ```

- **Endpoint**: DELETE /my/consent/current
  - **Justification (from description)**: "Revoke previously granted consents" - the word "revoke" directly justifies this self-revocation endpoint
  - **Purpose**: Revoke the consent being used in the current API call (self-revocation)
  - **Request**:
    - Headers: `Consent-Id` (consent identifier from request header)
    - Authentication: Required (user must be logged in via consent)
  - **Response**:
    ```json
    {
      "consent_id": "string",
      "jwt": "string",
      "status": "REVOKED"
    }
    ```

- **Endpoint**: DELETE /consents/CONSENTID (Berlin Group PSD2)
  - **Justification (from description)**: "Revoke previously granted consents" - the word "revoke" directly justifies this Berlin Group consent deletion endpoint
  - **Purpose**: Allow TPPs to delete/revoke Berlin Group account information consents per PSD2 specification
  - **Request**:
    - Path parameters: `CONSENTID` (consent identifier)
    - Authentication: Application access with PSD2 AISP certificate
    - Authorization: Consumer must match the consumer that created the consent
  - **Response**: HTTP 204 No Content (empty body on success)

## Business Rules (from capability description)

1. **Ownership Validation**: Only the user who owns the consent (or an authorized administrator) can revoke it
2. **Consumer Validation**: For Berlin Group consents, the TPP (consumer) requesting deletion must match the TPP that created the consent
3. **Status Transition**: Upon revocation, consent status changes to "REVOKED" (OBP) or "terminatedByTpp" (Berlin Group)
4. **Idempotency Prevention**: Attempting to revoke an already-revoked consent returns an error (ConsentAlreadyRevoked)
5. **Token Invalidation**: After revocation, the consent's JWT token becomes invalid for subsequent API calls
6. **Audit Trail**: The last action date is updated to record when the revocation occurred

## Data Validations

- **Consent Existence**: System validates that the consent ID exists in the database
- **User Ownership**: System validates that the requesting user's ID matches the consent's user ID
- **Consumer Matching**: For Berlin Group, system validates that the consumer ID from the current call matches the consent's consumer ID
- **Status Check**: System checks if consent is already revoked before attempting revocation
- **Authentication**: All revocation endpoints require user authentication (except Berlin Group which uses application/certificate authentication)

## Dependencies

- **Upstream**:
  - User must be authenticated via OAuth, Direct Login, or Consent-based authentication
  - Consent must have been previously created and granted
  - For Berlin Group: TPP must have valid PSD2 AISP certificate

- **Downstream**:
  - Revoked consent tokens will fail validation on subsequent API calls
  - Account access granted through the consent is immediately terminated
  - Any scheduled or recurring operations using the consent will fail

- **External Systems**:
  - Database (MappedConsent table) for consent status updates
  - Authentication system for user/consumer validation

## Notes for Implementation

- **Multiple Revocation Paths**: The system supports multiple ways to revoke consents (bank-scoped, user-scoped, self-revocation, Berlin Group) to accommodate different use cases and API standards
- **PSD2 Compliance**: Berlin Group consent deletion follows PSD2 specification with terminatedByTpp status and 204 response
- **Backward Compatibility**: Legacy endpoint (v3.1.0 revokeConsent) is deprecated but may still be in use; v5.1.0 endpoints are the recommended approach
- **Error Handling**: Comprehensive error handling for not found, already revoked, and unauthorized scenarios
- **Needs SME Input**: Clarification needed on whether revocation should cascade to related authorizations or consent auth contexts
