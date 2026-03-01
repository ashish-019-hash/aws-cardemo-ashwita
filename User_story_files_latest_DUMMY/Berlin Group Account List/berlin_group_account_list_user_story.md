# User Story for Berlin Group Account List

## Story Overview
**As a** Third-Party Provider (TPP) or Account Information Service Provider (AISP)
**I want to** retrieve account list per Berlin Group specification
**So that** I can access the list of payment accounts that a Payment Service User (PSU) has granted consent for, enabling account aggregation and financial management services in compliance with PSD2 regulations

## Acceptance Criteria
1. The system shall return a list of accounts that the authenticated user has access to based on granted consent
2. The system shall return account identifiers (resourceId) for each account in the list
3. The system shall return account details including IBAN, currency, product type, and cash account type
4. The system shall provide hyperlinks to related resources (balances, transactions) based on consent permissions
5. The system shall support an optional "withBalance" query parameter to include balance information in the response
6. The system shall filter out card accounts from the payment account list (card accounts have separate endpoints)
7. The system shall validate that the requesting TPP has valid PSD2 AISP authorization
8. The system shall only return accounts for which a valid consent has been granted and stored
9. The system shall return account information consistent with the Berlin Group NextGenPSD2 v1.3 specification

## Technical Context
- **Classes/Services Involved**: 
  - `APIMethods_AccountInformationServiceAISApi` - Main API endpoint handler
  - `JSONFactory_BERLIN_GROUP_1_3` - JSON response factory for Berlin Group format
  - `NewStyle.function.getAccountListOfBerlinGroup` - Account list retrieval service
  - `NewStyle.function.getAccountCanReadBalancesOfBerlinGroup` - Balance permission checker
  - `NewStyle.function.getAccountCanReadTransactionsOfBerlinGroup` - Transaction permission checker
  - `Consents.consentProvider` - Consent management service
- **Input Data**: 
  - Authentication headers (OAuth2 access token or consent JWT)
  - Optional query parameter: `withBalance` (TRUE/FALSE)
  - Consent-Id header (implicit from authentication context)
- **Output Data**: 
  - JSON response containing array of account objects with resourceId, IBAN, currency, product, cashAccountType, name, and _links
- **Processing Type**: API (HTTP request-response, Real-time)

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description.

- **Endpoint**: GET /v1.3/accounts
  - **Justification (from description)**: "Retrieve account list" - the word "Retrieve" explicitly justifies a GET endpoint for listing accounts
  - **Purpose**: Read the identifiers of available payment accounts together with booking balance information, depending on the consent granted
  - **Request**: 
    - Headers:
      - `Authorization`: Bearer token or consent JWT
      - `X-Request-ID`: UUID for request tracking
      - `Consent-ID`: Consent identifier (optional if embedded in token)
      - `PSU-IP-Address`: IP address of the PSU (optional)
    - Query Parameters:
      - `withBalance`: Optional boolean (TRUE/FALSE) to include balance information
  - **Response**: 
    ```json
    {
      "accounts": [
        {
          "resourceId": "3dc3d5b3-7023-4848-9853-f5400a64e80f",
          "iban": "DE2310010010123456789",
          "currency": "EUR",
          "product": "Girokonto",
          "cashAccountType": "CACC",
          "name": "Main Account",
          "_links": {
            "balances": {
              "href": "/v1/accounts/3dc3d5b3-7023-4848-9853-f5400a64e80f/balances"
            },
            "transactions": {
              "href": "/v1/accounts/3dc3d5b3-7023-4848-9853-f5400a64e80f/transactions"
            }
          }
        }
      ]
    }
    ```

## Business Rules (from capability description)
1. Account list retrieval must comply with Berlin Group NextGenPSD2 v1.3 specification
2. Only accounts for which consent has been granted shall be returned
3. The TPP must have valid PSD2 AISP (Account Information Service Provider) authorization
4. Account identifiers (resourceId) must be constant throughout the lifecycle of the consent
5. Card accounts are excluded from the payment account list (separate Berlin Group Card Account Access capability)
6. The response includes hyperlinks to balances and transactions only if the consent grants access to those resources
7. If "withBalance" parameter is TRUE, balance information is included inline in the account response
8. The account list depends on the PSU ID and the stored consent addressed by consentId

## Data Validations (if applicable)
- The `withBalance` query parameter must be either "TRUE" or "FALSE" (case-insensitive) if provided
- Authentication token must be valid and not expired
- Consent must be in valid status (not revoked or expired)
- TPP must pass PSD2 AISP validation checks
- Request must include valid X-Request-ID header for tracking

## Dependencies
- **Upstream**: 
  - User must be authenticated via OAuth2 or consent JWT
  - Valid consent must exist and be stored on the ASPSP system (created via Berlin Group Consent Creation capability)
  - TPP must have valid PSD2 AISP registration/authorization
- **Downstream**: 
  - Account resourceIds can be used to access Berlin Group Balance Retrieval endpoint
  - Account resourceIds can be used to access Berlin Group Transaction List endpoint
  - Account information supports account aggregation services
- **External Systems**: 
  - Core banking system connector for account data retrieval
  - Consent management system for access validation
  - PSD2 TPP registry for AISP validation (if applicable)

## Notes for Implementation
- The capability description specifies "Retrieve account list per Berlin Group specification" - this is a read-only operation with no create, update, or delete functionality
- The implementation filters out accounts with CashAccountTypeCode="card" as these are handled by separate card account endpoints
- Balance information retrieval is conditional on both the withBalance parameter and the user having balance read permissions
- Transaction links are only included if the user has transaction read permissions for the account
- The resourceId returned must remain constant for the lifetime of the consent to ensure consistent account identification
- Consider implementing caching for frequently accessed account lists to improve performance (Needs SME Input on caching strategy)
- No POST, PUT, PATCH, or DELETE endpoints are included as the capability description only mentions "Retrieve" operations

## Quality Checklist Verification
- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (TPP/AISP)
- [x] Business value is stated (PSD2 compliant account aggregation)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (caching strategy)
- [x] Only relevant endpoints are included (GET for retrieve)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words from the capability description justify inclusion
- [x] No endpoint type has been added unless its verb appears in the description
- [x] "Retrieve" has been interpreted as read/GET operations only - no create, update, or delete endpoints included
