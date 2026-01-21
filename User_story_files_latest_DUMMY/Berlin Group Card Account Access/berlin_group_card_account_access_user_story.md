# User Story for Berlin Group Card Account Access

## Story Overview
**As a** Third-Party Provider (TPP) or Account Information Service Provider (AISP)
**I want to** access card account information per Berlin Group specification
**So that** I can retrieve and display card account details, balances, and transactions to account holders through my application in compliance with PSD2 regulations

## Acceptance Criteria
1. The system shall allow authorized TPPs to retrieve a list of card accounts accessible under a valid consent
2. The system shall provide detailed card account information including account identifiers, currency, and product type
3. The system shall return card account balances including available and current balance amounts
4. The system shall provide card account transaction history with filtering and pagination capabilities
5. The system shall validate that a valid Berlin Group consent exists before allowing card account access
6. The system shall return data in Berlin Group NextGenPSD2 compliant format
7. The system shall enforce consent scope limitations on card account data access
8. The system shall return appropriate error responses for invalid requests or insufficient consent permissions

## Technical Context
- **Classes/Services Involved**: Card Account Service, Berlin Group Consent Validation Service, Transaction Service, Balance Service
- **Input Data**: Consent ID, Card Account ID, Query parameters (date range, pagination)
- **Output Data**: Card account list, card account details, balance information, transaction records in Berlin Group format
- **Processing Type**: API (HTTP request-response, Real-time)

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description.

- **Endpoint**: GET /v1/card-accounts
  - **Justification (from description)**: "Access card account information" - the word "Access" explicitly justifies a GET endpoint to retrieve card account list
  - **Purpose**: Retrieve list of card accounts accessible under the current consent
  - **Request**: 
    ```
    Headers:
      - Consent-ID: {consent-id} (required)
      - X-Request-ID: {uuid} (required)
      - Authorization: Bearer {access-token}
    ```
  - **Response**: 
    ```json
    {
      "cardAccounts": [
        {
          "resourceId": "string",
          "maskedPan": "string",
          "currency": "string",
          "name": "string",
          "product": "string",
          "status": "enabled|deleted|blocked",
          "usage": "PRIV|ORGA",
          "_links": {
            "balances": {"href": "string"},
            "transactions": {"href": "string"}
          }
        }
      ]
    }
    ```

- **Endpoint**: GET /v1/card-accounts/{account-id}
  - **Justification (from description)**: "Access card account information" - the word "Access" and "information" justify retrieving detailed card account data
  - **Purpose**: Retrieve detailed information for a specific card account
  - **Request**: 
    ```
    Path Parameters:
      - account-id: {card-account-resource-id} (required)
    Headers:
      - Consent-ID: {consent-id} (required)
      - X-Request-ID: {uuid} (required)
      - Authorization: Bearer {access-token}
    ```
  - **Response**: 
    ```json
    {
      "resourceId": "string",
      "maskedPan": "string",
      "currency": "string",
      "name": "string",
      "product": "string",
      "status": "enabled|deleted|blocked",
      "usage": "PRIV|ORGA",
      "details": "string",
      "creditLimit": {
        "currency": "string",
        "amount": "string"
      },
      "_links": {
        "balances": {"href": "string"},
        "transactions": {"href": "string"}
      }
    }
    ```

- **Endpoint**: GET /v1/card-accounts/{account-id}/balances
  - **Justification (from description)**: "Access card account information" - balance is a core component of card account information
  - **Purpose**: Retrieve balance information for a specific card account
  - **Request**: 
    ```
    Path Parameters:
      - account-id: {card-account-resource-id} (required)
    Headers:
      - Consent-ID: {consent-id} (required)
      - X-Request-ID: {uuid} (required)
      - Authorization: Bearer {access-token}
    ```
  - **Response**: 
    ```json
    {
      "cardAccount": {
        "maskedPan": "string"
      },
      "balances": [
        {
          "balanceType": "closingBooked|expected|openingBooked|interimAvailable|interimBooked|forwardAvailable|nonInvoiced",
          "balanceAmount": {
            "currency": "string",
            "amount": "string"
          },
          "creditLimitIncluded": true|false,
          "lastChangeDateTime": "string (ISO 8601)",
          "referenceDate": "string (ISO 8601 date)"
        }
      ]
    }
    ```

- **Endpoint**: GET /v1/card-accounts/{account-id}/transactions
  - **Justification (from description)**: "Access card account information" - transaction history is a core component of card account information per Berlin Group specification
  - **Purpose**: Retrieve transaction history for a specific card account
  - **Request**: 
    ```
    Path Parameters:
      - account-id: {card-account-resource-id} (required)
    Query Parameters:
      - dateFrom: {date} (optional, ISO 8601 date)
      - dateTo: {date} (optional, ISO 8601 date)
      - bookingStatus: booked|pending|both (optional)
    Headers:
      - Consent-ID: {consent-id} (required)
      - X-Request-ID: {uuid} (required)
      - Authorization: Bearer {access-token}
    ```
  - **Response**: 
    ```json
    {
      "cardAccount": {
        "maskedPan": "string"
      },
      "transactions": {
        "booked": [
          {
            "cardTransactionId": "string",
            "terminalId": "string",
            "transactionDate": "string (ISO 8601 date)",
            "bookingDate": "string (ISO 8601 date)",
            "transactionAmount": {
              "currency": "string",
              "amount": "string"
            },
            "originalAmount": {
              "currency": "string",
              "amount": "string"
            },
            "markupFee": {
              "currency": "string",
              "amount": "string"
            },
            "markupFeePercentage": "string",
            "cardAcceptorId": "string",
            "cardAcceptorAddress": {
              "city": "string",
              "country": "string"
            },
            "merchantCategoryCode": "string",
            "maskedPan": "string",
            "transactionDetails": "string",
            "invoiced": true|false,
            "proprietaryBankTransactionCode": "string"
          }
        ],
        "pending": [
          {
            "cardTransactionId": "string",
            "transactionDate": "string (ISO 8601 date)",
            "transactionAmount": {
              "currency": "string",
              "amount": "string"
            },
            "cardAcceptorId": "string",
            "cardAcceptorAddress": {
              "city": "string",
              "country": "string"
            },
            "maskedPan": "string",
            "transactionDetails": "string"
          }
        ],
        "_links": {
          "cardAccount": {"href": "string"}
        }
      }
    }
    ```

## Business Rules (from capability description)
1. All card account access must be performed under a valid Berlin Group consent with appropriate scope
2. Card account data must be returned in NextGenPSD2 compliant format as per Berlin Group specification
3. Access is limited to card accounts explicitly included in the consent or all accounts if consent covers all accounts
4. Masked PAN (Primary Account Number) must be used instead of full card numbers for security
5. Transaction access may be limited by date range as specified in the consent
6. Balance types returned must conform to Berlin Group enumeration values
7. Real-time processing is required for card account information access

## Data Validations (if applicable)
- Consent-ID header must be present and reference a valid, active consent
- X-Request-ID must be a valid UUID for request tracking
- Account-ID must reference a card account accessible under the provided consent
- Date parameters (dateFrom, dateTo) must be valid ISO 8601 date format
- dateFrom must not be after dateTo when both are provided
- bookingStatus must be one of: booked, pending, both
- Authorization token must be valid and not expired

## Dependencies
- **Upstream**: 
  - Valid Berlin Group consent must be created and authorized (Berlin Group Consent Creation capability)
  - User must be authenticated via OAuth 2.0/OpenID Connect
  - Consent must include card account access scope
- **Downstream**: 
  - Retrieved card account information can be displayed to account holders in TPP applications
  - Transaction data may be used for account aggregation and financial analysis
- **External Systems**: 
  - Core banking system connector for card account data retrieval
  - Card processing system for real-time balance and transaction information

## Notes for Implementation
- The capability description specifies "per Berlin Group specification" - implementation must strictly follow NextGenPSD2 API specification version requirements
- Card account endpoints are separate from regular account endpoints in Berlin Group specification
- Masked PAN format should follow card scheme requirements (typically showing last 4 digits)
- Transaction pagination may be required for accounts with high transaction volumes - specific pagination mechanism needs SME input
- Some banks may not support all optional fields - graceful handling of missing data is required
- No CREATE, UPDATE, or DELETE endpoints are included as the capability description only mentions "Access" which maps to retrieval operations
- Rate limiting should be applied per Berlin Group recommendations for AIS endpoints

## Quality Checklist Verification
- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (TPP/AISP)
- [x] Business value is stated (PSD2 compliant card account access for account holders)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (pagination mechanism, optional field handling)
- [x] Only relevant endpoints are included (GET endpoints for access/retrieval only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words from the capability description justify inclusion ("Access" justifies GET operations)
- [x] No endpoint type has been added unless its verb appears in the description
- [x] "Access" has been interpreted as retrieval operations only - no create, update, or delete operations included
