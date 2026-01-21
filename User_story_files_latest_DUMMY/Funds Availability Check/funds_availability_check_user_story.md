# User Story for Funds Availability Check

## Story Overview

**As a** Payment Initiation Service Provider (PISP) or third-party application  
**I want to** check if sufficient funds are available for a transaction  
**So that** I can verify account balance adequacy before initiating a payment, ensuring transaction success and providing a better user experience by preventing failed payment attempts due to insufficient funds

## Acceptance Criteria

1. The system shall check if sufficient funds are available in a specified account for a given transaction amount
2. The funds availability check shall be performed in real-time
3. The system shall return a clear indication of whether funds are available (true/false)
4. The check shall support verification against the account's available balance
5. The system shall handle cases where the account does not exist or is inaccessible
6. The funds check shall comply with PSD2 Berlin Group PIIS (Payment Instrument Issuer Service) specification
7. The system shall support high volume of requests as indicated by the "High" volume classification

## Technical Context

- **Classes/Services Involved**: Funds Confirmation Service, Account Balance Service, Consent Validation Service, PSD2 Compliance Service
- **Input Data**: 
  - Account identifier (IBAN or account reference)
  - Transaction amount to check
  - Currency code
  - Consent reference (for PSD2 compliance)
  - Card number (for card-based funds confirmation)
- **Output Data**: 
  - Funds availability indicator (boolean: true/false)
  - Timestamp of the check
  - Optional: Available balance amount (if permitted by consent)
- **Processing Type**: Real-time API request-response

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description.

### Endpoint 1: Check Funds Availability (Berlin Group PIIS)
- **Endpoint**: `POST /v1/funds-confirmations`
  - **Justification (from description)**: "Check if sufficient funds are available for a transaction"
  - **Purpose**: Verify whether sufficient funds are available in a payment account to cover a specific transaction amount, compliant with Berlin Group PSD2 PIIS specification
  - **Request**: 
    - Headers:
      - `Authorization`: Bearer token or OAuth credentials
      - `X-Request-ID`: Unique request identifier
      - `Consent-ID`: Reference to the valid consent for funds confirmation
    - Body:
      ```json
      {
        "cardNumber": "string (optional)",
        "account": {
          "iban": "string"
        },
        "instructedAmount": {
          "currency": "string",
          "amount": "string"
        }
      }
      ```
  - **Response**: 
    ```json
    {
      "fundsAvailable": true
    }
    ```

### Endpoint 2: Check Funds Availability for Account
- **Endpoint**: `POST /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/funds-available`
  - **Justification (from description)**: "Check if sufficient funds are available for a transaction"
  - **Purpose**: Check funds availability for a specific account at a bank for a given transaction amount
  - **Request**: 
    - Path Parameters:
      - `BANK_ID` (string, required): The identifier of the bank
      - `ACCOUNT_ID` (string, required): The identifier of the account
    - Headers:
      - `Authorization`: Bearer token or OAuth credentials
    - Body:
      ```json
      {
        "amount": "string",
        "currency": "string"
      }
      ```
  - **Response**: 
    ```json
    {
      "fundsAvailable": true,
      "timestamp": "string"
    }
    ```

## Business Rules (from capability description)

1. Funds availability must be checked in real-time to ensure accuracy at the moment of inquiry
2. The check is a read-only operation - no modifications to account data or balances are performed
3. The capability supports high volume of requests, requiring performance optimization
4. Funds confirmation requires valid consent from the account holder (PSD2 compliance)
5. The check compares the requested transaction amount against the available balance
6. Currency must match or be convertible for accurate comparison

## Data Validations (if applicable)

- Account identifier (IBAN or account reference) must be valid and properly formatted
- Transaction amount must be a positive numeric value
- Currency code must be a valid ISO 4217 currency code
- Consent ID must reference a valid, active consent for funds confirmation
- Card number (if provided) must be valid and associated with the account
- Authentication token must be valid and not expired
- The requesting party must have appropriate PIIS authorization

## Dependencies

- **Upstream**: 
  - User/TPP authentication must be completed
  - Valid consent for funds confirmation must exist
  - Account must exist and be active in the system
  - Bank must be active on the platform
- **Downstream**: 
  - Funds availability result can be used by payment initiation services
  - Result informs merchant/TPP whether to proceed with transaction
  - Can trigger alternative payment method selection if funds unavailable
- **External Systems**: 
  - Core banking system connector for retrieving actual balance data
  - Authentication/authorization service for validating access
  - Consent management service for validating PIIS consent

## Notes for Implementation

- This capability is part of PSD2 Berlin Group Compliance (Confirmation of Funds - PIIS) category
- Real-time frequency requirement means balance data should be fetched from the source system rather than cached stale data
- The "High" volume classification indicates the need for performance optimization and potentially caching strategies for repeated checks
- Error handling should provide clear messages for common failure scenarios (account not found, invalid consent, insufficient permissions)
- Consider implementing rate limiting to protect against abuse while supporting high volume
- The implementation should follow Berlin Group NextGenPSD2 specification for PIIS services
- Audit logging should capture all funds confirmation requests for regulatory compliance

## Operations NOT Included (per Operation Derivation Rules)

The following operations are explicitly NOT included because they are not mentioned in the capability description:

- **CREATE operations**: No "create", "add", or similar verbs in description - this is a check/query operation only
- **UPDATE operations**: No "manage", "update", "modify" or similar verbs in description
- **DELETE operations**: No "delete", "remove", "deactivate" or similar verbs in description
- **LIST/SEARCH operations**: The description says "Check" for a specific transaction, not "list" or "search" across multiple checks

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Payment Initiation Service Provider or third-party application)
- [x] Business value is stated (verify balance adequacy before payment initiation)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged
- [x] Only relevant endpoints are included (POST for check operation only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words from the capability description justify inclusion ("Check")
- [x] No CRUD operations are inferred beyond what the description explicitly states
- [x] Words like "manage" have been interpreted narrowly - not applicable as "manage" is not in description
