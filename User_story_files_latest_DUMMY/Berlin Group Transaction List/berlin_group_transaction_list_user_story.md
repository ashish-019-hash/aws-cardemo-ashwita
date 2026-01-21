# User Story for Berlin Group Transaction List

## Story Overview

**As a** Third-Party Provider (TPP) / Account Information Service Provider (AISP)
**I want to** retrieve transactions per Berlin Group specification
**So that** I can provide account holders with a comprehensive view of their transaction history in compliance with PSD2 regulations

## Acceptance Criteria

1. The system shall allow authenticated TPPs to retrieve transaction lists for accounts they have consent to access
2. The system shall return transactions in the Berlin Group PSD2 v1.3 compliant format
3. The system shall support filtering transactions by booking status (booked, pending, or both)
4. The system shall support date range filtering using dateFrom and dateTo parameters
5. The system shall return transaction details including transactionId, creditor/debtor information, transaction amount, booking date, value date, and remittance information
6. The system shall validate that the TPP has a valid consent with transaction access rights before returning data
7. The system shall return transactions categorized into "booked" and "pending" arrays
8. The system shall include account IBAN information in the response
9. The system shall include HATEOAS links for navigation to related resources

## Technical Context

- **Classes/Services Involved**:
  - `APIMethods_AccountInformationServiceAISApi` - Contains the getTransactionList endpoint implementation
  - `JSONFactory_BERLIN_GROUP_1_3` - Handles JSON serialization for Berlin Group responses
  - `NewStyle.function` - Provides utility functions for bank account and transaction retrieval
  - `ViewNewStyle` - Handles view-based access control validation
  - `Consents` - Manages consent validation and retrieval

- **Input Data**:
  - Path Parameter: `account-id` (tokenised account identifier)
  - Query Parameters:
    - `bookingStatus` (required): "booked", "pending", or "both"
    - `dateFrom` (optional): Start date for transaction filtering
    - `dateTo` (optional): End date for transaction filtering
  - Headers: Authentication token, Consent-ID

- **Output Data**:
  - JSON response containing:
    - `account`: Object with IBAN
    - `transactions`: Object containing:
      - `booked`: Array of booked transactions
      - `pending`: Array of pending transactions
      - `_links`: Navigation links

- **Processing Type**: Real-time API request-response

## Relevant Endpoints

**IMPORTANT**: The capability description states "Retrieve transactions per Berlin Group specification". The verb "Retrieve" justifies a GET/read operation.

- **Endpoint**: `GET /accounts/{account-id}/transactions`
  - **Justification (from description)**: "Retrieve transactions" - the word "Retrieve" explicitly justifies this GET endpoint
  - **Purpose**: Read transaction list of an account per Berlin Group specification
  - **Request**:
    - Path: `/accounts/{account-id}/transactions`
    - Query Parameters:
      - `bookingStatus` (required): One of "booked", "pending", "both"
      - `dateFrom` (optional): ISO date format (YYYY-MM-DD)
      - `dateTo` (optional): ISO date format (YYYY-MM-DD)
    - Headers:
      - `Authorization`: Bearer token or OAuth credentials
      - `Consent-ID`: Valid consent identifier
      - `X-Request-ID`: Unique request identifier
  - **Response**:
    ```json
    {
      "account": {
        "iban": "DE2310010010123456788"
      },
      "transactions": {
        "booked": [
          {
            "transactionId": "1234567",
            "creditorName": "John Miles",
            "creditorAccount": {
              "iban": "DE67100100101306118605"
            },
            "transactionAmount": {
              "currency": "EUR",
              "amount": "256.67"
            },
            "bookingDate": "2017-10-25",
            "valueDate": "2017-10-26",
            "remittanceInformationUnstructured": "Example 1"
          }
        ],
        "pending": [
          {
            "transactionId": "1234569",
            "creditorName": "Claude Renault",
            "creditorAccount": {
              "iban": "FR7612345987650123456789014"
            },
            "transactionAmount": {
              "currency": "EUR",
              "amount": "-100.03"
            },
            "valueDate": "2017-10-26",
            "remittanceInformationUnstructured": "Example 3"
          }
        ],
        "_links": {
          "account": {
            "href": "/v1.3/accounts/3dc3d5b3-7023-4848-9853-f5400a64e80f"
          }
        }
      }
    }
    ```

## Business Rules (from capability description)

1. **Consent Requirement**: A valid PSD2 consent with transaction access rights must exist before transactions can be retrieved
2. **Account Access Validation**: The TPP must have been granted access to the specific account through the consent mechanism
3. **Berlin Group Compliance**: All responses must conform to the Berlin Group PSD2 v1.3 specification format
4. **Booking Status Filtering**: The bookingStatus parameter is mandatory and must be one of: "booked", "pending", or "both"
5. **Real-time Processing**: Transaction retrieval is a real-time operation with high volume expectations
6. **View-Based Access Control**: Access is controlled through the SYSTEM_READ_TRANSACTIONS_BERLIN_GROUP_VIEW_ID view

## Data Validations

- **bookingStatus Parameter**: Must be one of "booked", "pending", or "both" - returns 400 error if invalid
- **Account ID Validation**: The account-id must correspond to a valid account accessible through the consent
- **Authentication Validation**: User must be authenticated via valid OAuth/Bearer token
- **Consent Validation**: The consent must be valid, not expired, and include transaction access rights
- **Date Format Validation**: dateFrom and dateTo parameters must be in valid ISO date format (YYYY-MM-DD)
- **View Access Validation**: User must have access to the SYSTEM_READ_TRANSACTIONS_BERLIN_GROUP_VIEW_ID view for the account

## Dependencies

- **Upstream**:
  - Valid PSD2 consent must be created and authorized (Berlin Group Consent Creation capability)
  - User authentication must be completed
  - Account must exist and be accessible through the consent

- **Downstream**:
  - Transaction data is consumed by TPP applications for display to end users
  - May be used in conjunction with balance retrieval for complete account overview

- **External Systems**:
  - Core banking system (via Connector abstraction layer) for transaction data retrieval
  - Consent management system for access validation

## Notes for Implementation

- **Tokenised Account ID**: The account-id in the path can be a tokenised identification for data protection, as path information might be logged on intermediary servers
- **Pagination Consideration**: For accounts with large transaction volumes, pagination may need to be implemented
- **Balance Information**: The ASPSP might add balance information if transaction lists without balances are not supported
- **Multicurrency Support**: The endpoint should handle multicurrency accounts appropriately
- **Performance**: Given the "Very High" volume classification, caching and performance optimization should be considered
- **Error Handling**: Comprehensive error responses should be provided for various failure scenarios (invalid consent, expired consent, unauthorized access, etc.)

## Quality Checklist

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (TPP/AISP)
- [x] Business value is stated (PSD2 compliance, transaction history access)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged
- [x] Only relevant endpoints are included (GET /accounts/{account-id}/transactions)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, I can point to a specific word ("Retrieve") in the capability description that justifies this endpoint
- [x] No endpoint type has been added unless its verb appears in the description
- [x] The word "Retrieve" has been interpreted as a read/GET operation only
