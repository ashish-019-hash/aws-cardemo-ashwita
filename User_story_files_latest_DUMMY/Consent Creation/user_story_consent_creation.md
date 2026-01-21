# User Story for Consent Creation

## Story Overview

**As a** Third-Party Application (TPP) or Account Information Service Provider (AISP)
**I want to** create consent records for account access authorization
**So that** I can obtain proper authorization from account holders to access their banking data in compliance with regulatory requirements (e.g., PSD2/Open Banking)

## Acceptance Criteria

1. The system shall allow authorized applications to create new consent records for account access
2. A consent record must be associated with a specific account holder and the requesting third-party application
3. The consent creation request must specify the scope of account access being requested (e.g., account information, balances, transactions)
4. Upon successful consent creation, the system shall return a unique consent identifier
5. The consent record must capture the authorization timestamp and validity period
6. The system shall validate that the requesting application has the necessary permissions to request consent
7. The consent creation must comply with regulatory requirements for account access authorization

## Technical Context

- **Classes/Services Involved**: ConsentService, ConsentController, ConsentRepository (inferred from typical Scala REST API patterns for consent management)
- **Input Data**: 
  - Request body containing:
    - Account holder identifier
    - Third-party application/consumer identifier
    - Requested access scope (accounts, balances, transactions)
    - Validity period/expiration date
    - Frequency of access (one-time, recurring)
- **Output Data**: 
  - Response payload containing:
    - Consent ID (unique identifier)
    - Consent status (e.g., RECEIVED, AWAITING_AUTHORIZATION)
    - Creation timestamp
    - Links to authorization resources
- **Processing Type**: Real-time API

## Relevant Endpoints

**IMPORTANT**: Only CREATE operations are included as the capability description explicitly states "Create consent records".

- **Endpoint**: POST /consents
  - **Justification (from description)**: "Create consent records for account access authorization" - the word "Create" explicitly justifies this endpoint
  - **Purpose**: Create a new consent record for account access authorization
  - **Request**: 
    ```json
    {
      "access": {
        "accounts": ["account-id-1", "account-id-2"],
        "balances": ["account-id-1"],
        "transactions": ["account-id-1"]
      },
      "recurringIndicator": true,
      "validUntil": "2026-12-31",
      "frequencyPerDay": 4,
      "combinedServiceIndicator": false
    }
    ```
  - **Response**: 
    ```json
    {
      "consentId": "consent-12345",
      "consentStatus": "received",
      "createdAt": "2026-01-21T05:53:00Z",
      "_links": {
        "self": { "href": "/consents/consent-12345" },
        "status": { "href": "/consents/consent-12345/status" },
        "scaRedirect": { "href": "/authorize?consent_id=consent-12345" }
      }
    }
    ```

- **Endpoint**: POST /banks/{bank_id}/consents
  - **Justification (from description)**: "Create consent records for account access authorization" - the word "Create" explicitly justifies this bank-specific consent creation endpoint
  - **Purpose**: Create a new consent record for account access authorization at a specific bank
  - **Request**: 
    ```json
    {
      "bank_id": "bank-123",
      "access": {
        "accounts": ["account-id-1"],
        "balances": ["account-id-1"],
        "transactions": ["account-id-1"]
      },
      "validUntil": "2026-12-31",
      "frequencyPerDay": 4
    }
    ```
  - **Response**: 
    ```json
    {
      "consent_id": "consent-67890",
      "status": "RECEIVED",
      "bank_id": "bank-123",
      "created_at": "2026-01-21T05:53:00Z"
    }
    ```

### Endpoints NOT Included (with justification)

The following endpoint types are **excluded** because their corresponding verbs are NOT present in the capability description:

- **GET /consents/{consent_id}** - Excluded because "view", "retrieve", "get", or "read" are not mentioned in the description
- **GET /consents** - Excluded because "list", "search", or "browse" are not mentioned in the description
- **PUT/PATCH /consents/{consent_id}** - Excluded because "update", "modify", or "manage" are not mentioned in the description
- **DELETE /consents/{consent_id}** - Excluded because "delete", "remove", "revoke", or "cancel" are not mentioned in the description

## Business Rules

1. **Authorization Requirement**: Only authorized third-party applications (registered consumers) can create consent requests
2. **Account Holder Association**: Each consent must be linked to a specific account holder who will authorize the access
3. **Scope Definition**: The consent must clearly define what account data can be accessed (accounts, balances, transactions)
4. **Validity Period**: Consents must have a defined validity period after which they expire
5. **Frequency Limits**: The consent may specify how frequently the TPP can access the account data per day
6. **Regulatory Compliance**: Consent creation must comply with PSD2/Open Banking regulatory requirements for account access authorization

## Data Validations

- Account holder identifier must be valid and exist in the system
- Third-party application must be registered and authorized to request consents
- Requested access scope must be within the application's permitted scope
- Validity period must be a future date and within regulatory limits (typically max 90 days for PSD2)
- Frequency per day must be a positive integer within allowed limits
- Bank ID (if provided) must reference a valid bank on the platform

## Dependencies

- **Upstream**: 
  - Third-party application must be registered as a consumer on the platform
  - Account holder must have accounts at the specified bank
  - Application must have appropriate entitlements to request consent
- **Downstream**: 
  - After consent creation, the account holder must authorize the consent (separate capability: Consent Status Update)
  - Once authorized, the consent enables account information access (separate capabilities: Account Listing, Account Details Retrieval, Transaction Listing)
- **External Systems**: 
  - Authentication/Authorization service for validating TPP credentials
  - Bank backend systems for account validation

## Notes for Implementation

- **Consent Lifecycle**: This capability covers ONLY the creation of consent records. Other lifecycle operations (retrieval, status update, revocation) are handled by separate capabilities (Consent Retrieval - ID 75, Consent Status Update - ID 77, Consent Revocation - ID 76)
- **PSD2 Compliance**: Implementation should align with Berlin Group NextGenPSD2 specification for consent creation
- **Idempotency**: Consider implementing idempotency keys to prevent duplicate consent creation
- **Audit Trail**: All consent creation events should be logged for regulatory audit purposes
- **Needs SME Input**: 
  - Exact consent status values and state machine transitions
  - Specific validation rules for access scope combinations
  - Integration details with Strong Customer Authentication (SCA) flow
  - Specific regulatory requirements for different jurisdictions

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Third-Party Application/AISP)
- [x] Business value is stated (regulatory compliance, proper authorization)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (Needs SME Input section)
- [x] Only relevant endpoints are included (CREATE only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific word "Create" from description justifies inclusion
- [x] No endpoint type (view, list, delete) has been added beyond what description explicitly states
