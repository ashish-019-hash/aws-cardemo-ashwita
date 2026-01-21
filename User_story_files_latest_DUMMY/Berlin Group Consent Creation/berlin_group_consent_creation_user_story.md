# User Story for Berlin Group Consent Creation

## Story Overview
**As a** Third-Party Provider (TPP) or Account Information Service Provider (AISP)
**I want to** create PSD2-compliant consent for account information access
**So that** I can obtain authorized access to a Payment Service User's (PSU) account information in compliance with PSD2 Berlin Group specifications

## Acceptance Criteria
1. The system shall allow authorized TPPs to create a consent resource for account information access
2. The system shall accept consent requests specifying which accounts, balances, and/or transactions the TPP wishes to access
3. The system shall validate that the consent request includes required fields: access permissions, recurringIndicator, validUntil date, and frequencyPerDay
4. The system shall enforce frequency limits for access without PSU involvement (default maximum 4 per day, configurable)
5. The system shall validate that for one-off access (recurringIndicator=false), frequencyPerDay must be set to 1
6. The system shall validate the validUntil date is in proper ISO date format and is a valid future date
7. The system shall return a consent ID and consent status upon successful creation
8. The system shall return appropriate error responses for invalid or missing required data
9. The system shall support the "availableAccounts" option for access to all available payment accounts (with restrictions)
10. The system shall automatically expire any existing recurring consent for the same PSU when a new recurring consent is authorized

## Technical Context
- **Classes/Services Involved**: Consent management service, Berlin Group API handler, JWT token service, account validation service
- **Input Data**: Consent request JSON containing access permissions (accounts, balances, transactions), recurringIndicator (boolean), validUntil (date), frequencyPerDay (integer), combinedServiceIndicator (optional boolean)
- **Output Data**: Consent response containing consentId, consentStatus, and hypermedia links for next steps (authorisation)
- **Processing Type**: API (HTTP request-response, Real-time)

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description.

- **Endpoint**: POST /v1.3/consents
  - **Justification (from description)**: "Create PSD2-compliant consent" - the word "Create" explicitly justifies a POST endpoint
  - **Purpose**: Create a new consent resource for account information access per Berlin Group PSD2 specification
  - **Request**: 
    ```json
    {
      "access": {
        "accounts": [
          {
            "iban": "DE2310010010123456789",
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
      "consentId": "1234-wertiq-983",
      "consentStatus": "received",
      "_links": {
        "startAuthorisation": {
          "href": "/v1.3/consents/1234-wertiq-983/authorisations"
        }
      }
    }
    ```

## Business Rules (from capability description)
1. Consent must be PSD2-compliant following Berlin Group NextGenPSD2 Framework specifications
2. The consent defines access rights to dedicated accounts of a given PSU
3. When a new recurring consent is authorized, any existing recurring consent for the same PSU automatically expires
4. frequencyPerDay indicates the maximum frequency for access without PSU involvement per day
5. For one-off access, frequencyPerDay must be set to 1
6. The frequency must be greater than or equal to 1 and less than or equal to the configured upper limit (default 4)
7. If availableAccounts is specified, it must be "allAccounts" and recurringIndicator must be false with frequencyPerDay of 1
8. At least one of accounts, balances, or transactions must be specified in the access object (unless availableAccounts is used)
9. The consent is created with initial status "received" pending PSU authorization

## Data Validations (if applicable)
- Access object must contain at least one of: accounts, balances, transactions, or availableAccounts
- recurringIndicator is a required boolean field
- validUntil must be a valid date in ISO format (YYYY-MM-DD)
- frequencyPerDay must be an integer greater than 0 and less than or equal to the configured upper limit
- If recurringIndicator is false, frequencyPerDay must equal 1
- If availableAccounts is specified, it must equal "allAccounts"
- Account identifiers (IBAN, BBAN, etc.) must be valid formats when provided
- combinedServiceIndicator is optional and defaults to false

## Dependencies
- **Upstream**: 
  - TPP must be authenticated and authorized as an AISP (Account Information Service Provider)
  - TPP must pass PSD2 AISP validation checks
  - Bank accounts referenced in the consent must exist and be accessible
- **Downstream**: 
  - Created consent requires PSU authorization through the authorisation sub-resource
  - Once authorized, consent enables access to account list, balances, and transactions endpoints
  - Consent JWT is generated and stored for subsequent API calls
- **External Systems**: 
  - Integration with Strong Customer Authentication (SCA) system for consent authorization
  - May integrate with external account validation services

## Notes for Implementation
- The capability description only mentions "Create" - therefore no retrieval (GET), update (PUT), or deletion (DELETE) endpoints are included in this user story as these would fall under separate capabilities like "Berlin Group Consent Status" or "Berlin Group Consent Deletion"
- The consent creation follows the Berlin Group NextGenPSD2 Framework Version 1.3 specification
- Multiple SCA approaches are supported: Redirect, OAuth, Decoupled, and Embedded
- The consent object includes hypermedia links (_links) to guide the TPP through the authorization flow
- The validUntil date may be adjusted by the ASPSP; use "9999-12-31" to request maximum available validity
- Consider implementing idempotency for consent creation to handle duplicate requests gracefully
- The combinedServiceIndicator field indicates if a payment initiation service will be addressed in the same session

## Quality Checklist Verification
- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (TPP/AISP)
- [x] Business value is stated (PSD2-compliant account information access)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (SCA method specifics need SME input)
- [x] Only relevant endpoints are included (POST for create only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words from the capability description justify inclusion
- [x] No endpoint type has been added unless its verb appears in the description
- [x] "Create" has been interpreted strictly - no view, list, or delete operations included
