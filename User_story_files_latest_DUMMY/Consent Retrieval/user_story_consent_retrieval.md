# User Story for Consent Retrieval

## Story Overview

**As a** Third-Party Application Developer or Account Information Service Provider (AISP)
**I want to** retrieve consent information and status
**So that** I can verify the current state of account access authorizations, ensure compliance with granted permissions, and provide accurate consent status information to end users

## Acceptance Criteria

1. The system shall allow authorized users to retrieve consent information by consent ID
2. The system shall return the current status of a consent (e.g., valid, expired, revoked)
3. The system shall return consent details including scope of access, validity period, and associated accounts
4. The system shall validate that the requesting party has appropriate permissions to view the consent
5. The system shall return appropriate error responses for invalid or non-existent consent IDs
6. The system shall support retrieval of consent information in real-time with high volume capacity

## Technical Context

- **Classes/Services Involved**: Consent service/repository for consent data retrieval, authentication service for access validation
- **Input Data**: Consent ID, Bank ID (optional), authentication credentials/tokens
- **Output Data**: Consent record containing consent ID, status, creation date, expiration date, scope of access, linked accounts, and associated user/customer information
- **Processing Type**: API / Real-time

## Relevant Endpoints

- **Endpoint**: GET /consents/{consent_id}
  - **Justification (from description)**: "Retrieve consent information and status" - the word "Retrieve" explicitly justifies this GET endpoint
  - **Purpose**: Retrieve detailed information about a specific consent including its current status
  - **Request**: 
    - Path Parameter: consent_id (required) - The unique identifier of the consent
    - Headers: Authorization token, Bank-Id (optional)
  - **Response**: 
    ```json
    {
      "consent_id": "string",
      "status": "string (VALID|EXPIRED|REVOKED|RECEIVED|REJECTED)",
      "created_date": "datetime",
      "expiration_date": "datetime",
      "last_action_date": "datetime",
      "accounts": [
        {
          "account_id": "string",
          "iban": "string",
          "access_type": "string"
        }
      ],
      "recurring_indicator": "boolean",
      "frequency_per_day": "integer",
      "combined_service_indicator": "boolean"
    }
    ```

- **Endpoint**: GET /banks/{bank_id}/consents/{consent_id}
  - **Justification (from description)**: "Retrieve consent information and status" - the word "Retrieve" explicitly justifies this GET endpoint for bank-scoped consent retrieval
  - **Purpose**: Retrieve consent information scoped to a specific bank
  - **Request**: 
    - Path Parameters: bank_id (required), consent_id (required)
    - Headers: Authorization token
  - **Response**: Same as above with bank-specific context

## Business Rules (from capability description)

1. Only authorized parties (the consent owner, the TPP that created the consent, or bank administrators) can retrieve consent information
2. Consent status must accurately reflect the current state based on validity period and any revocation actions
3. Retrieval operations must be performed in real-time to support high-volume access patterns
4. Consent information must include all relevant details for compliance verification

## Data Validations

- Consent ID must be a valid, existing identifier in the system
- Requesting user must have appropriate entitlements to view consent information
- Bank ID (if provided) must be valid and the consent must belong to that bank
- Authentication token must be valid and not expired

## Dependencies

- **Upstream**: 
  - User authentication must be completed before consent retrieval
  - Consent must have been previously created (Consent Creation capability)
- **Downstream**: 
  - Retrieved consent information may be used to determine access permissions for Account Information Services
  - Consent status may influence whether subsequent API calls are authorized
- **External Systems**: 
  - Authentication/Authorization service for access control
  - Backend banking systems for consent storage

## Notes for Implementation

- Consider caching frequently accessed consent records to improve performance given the high-volume nature
- Implement proper audit logging for consent retrieval operations for compliance purposes
- Ensure consent status is computed dynamically based on expiration dates and revocation status
- **Needs SME Input**: Specific consent status values and their business meanings may need clarification
- **Needs SME Input**: Whether consent retrieval should return different levels of detail based on the requester's role
