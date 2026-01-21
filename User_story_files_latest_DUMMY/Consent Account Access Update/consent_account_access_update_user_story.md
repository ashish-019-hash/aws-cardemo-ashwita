# User Story for Consent Account Access Update

## Story Overview

**As a** Bank Administrator or Third-Party Provider (TPP)
**I want to** modify account access permissions within an existing consent
**So that** I can update which accounts, balances, and transactions a consent grants access to without requiring the creation of a new consent

## Acceptance Criteria

1. The system shall allow authorized users to update the account access permissions of an existing consent by consent ID
2. The system shall validate that the consent exists before attempting to update account access
3. The system shall require at least one of accounts, balances, or transactions access to be specified in the update request
4. The system shall update the JWT token associated with the consent to reflect the new account access permissions
5. The system shall return the updated consent information including the new JWT token and consent status
6. The system shall reject requests with invalid JSON format with appropriate error messages
7. The system shall require proper authentication and authorization (canUpdateConsentAccountAccessAtOneBank or canUpdateConsentAccountAccessAtAnyBank entitlements)

## Technical Context

- **Classes/Services Involved**:
  - `APIMethods510` - Contains the endpoint implementation for updating consent account access
  - `Consents` / `MappedConsent` - Consent data provider and model for managing consent records
  - `Consent` - Utility for updating Berlin Group consent JWT tokens
  - `JSONFactory_BERLIN_GROUP_1_3` - JSON factory for consent access data structures
  - `ConsentAccessJson` - Data structure for account access permissions
  - `ConsentAccessAccountsJson` - Data structure for individual account references

- **Input Data**:
  - Path Parameters: `BANK_ID`, `CONSENT_ID`
  - Request Body: `PutConsentPayloadJsonV510` containing:
    - `access`: `ConsentAccessJson` with:
      - `accounts`: Optional list of account references (IBAN, BBAN, PAN, maskedPan, MSISDN, currency)
      - `balances`: Optional list of account references for balance access
      - `transactions`: Optional list of account references for transaction access
      - `availableAccounts`: Optional string for available accounts access level
      - `allPsd2`: Optional string for PSD2 all accounts access

- **Output Data**:
  - `ConsentJsonV310` containing:
    - `consent_id`: The consent identifier
    - `jwt`: Updated JSON Web Token with new account access permissions
    - `status`: Current consent status (e.g., "AUTHORISED")

- **Processing Type**: API (REST) / Real-time

## Relevant Endpoints

- **Endpoint**: PUT /management/banks/BANK_ID/consents/CONSENT_ID/account-access
  - **Justification (from description)**: "Modify account access permissions within a consent" - the word "Modify" directly justifies an UPDATE/PUT operation
  - **Purpose**: Updates the account access permissions associated with an existing consent, allowing administrators to change which accounts, balances, and transactions the consent grants access to
  - **Request**:
    ```json
    {
      "access": {
        "accounts": [
          {
            "iban": "DE91100000000123456789",
            "bban": null,
            "pan": null,
            "maskedPan": null,
            "msisdn": null,
            "currency": null
          }
        ],
        "balances": null,
        "transactions": null,
        "availableAccounts": null,
        "allPsd2": null
      }
    }
    ```
  - **Response**:
    ```json
    {
      "consent_id": "9d429899-24f5-42c8-8565-943ffa6a7945",
      "jwt": "eyJhbGciOiJIUzI1NiJ9...",
      "status": "AUTHORISED"
    }
    ```

## Business Rules (from capability description)

1. A consent must exist before its account access can be modified
2. At least one access type (accounts, balances, or transactions) must be specified in the update request - empty access is not permitted
3. Account access modifications update the JWT token embedded in the consent to reflect the new permissions
4. The consent status is preserved during account access updates
5. Users must have appropriate entitlements (canUpdateConsentAccountAccessAtOneBank or canUpdateConsentAccountAccessAtAnyBank) to perform this operation

## Data Validations

- Consent ID must reference an existing consent in the system (returns 404 if not found)
- Request body must conform to `PutConsentPayloadJsonV510` JSON schema (returns 400 if invalid)
- At least one of `accounts`, `balances`, or `transactions` must be non-empty in the access object (returns 400 if all are empty)
- User must be authenticated (returns 401 if not logged in)
- User must have required entitlements for the bank (returns 403 if unauthorized)
- Bank ID must reference a valid bank (returns 404 if not found)

## Dependencies

- **Upstream**:
  - A consent must have been previously created (via Consent Creation capability)
  - User must be authenticated with valid session
  - User must have appropriate entitlements granted

- **Downstream**:
  - Updated consent JWT is used for subsequent API calls requiring consent-based authorization
  - Account Information Service (AIS) operations will use the updated account access permissions

- **External Systems**:
  - JWT signing/verification infrastructure for consent token management
  - Berlin Group PSD2 compliance framework for consent access structures

## Notes for Implementation

- The endpoint follows Berlin Group PSD2 specifications for consent account access management
- Account references can be specified using multiple identifier types (IBAN, BBAN, PAN, maskedPan, MSISDN) to support various account identification schemes
- The JWT token is regenerated with updated claims when account access is modified
- This capability is part of the Consent Management category and supports PSD2 Account Information Service (AIS) compliance
- The endpoint is tagged with `apiTagConsent` and `apiTagPSD2AIS` indicating its role in consent and PSD2 compliance
- **Needs SME Input**: Clarification on whether partial updates are supported (e.g., adding accounts while preserving existing balance access) or if the entire access structure must be provided
