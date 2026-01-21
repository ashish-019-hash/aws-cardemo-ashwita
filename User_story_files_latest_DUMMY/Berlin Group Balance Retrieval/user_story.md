# User Story for Berlin Group Balance Retrieval

## Story Overview

**As a** Third-Party Provider (TPP) or Account Information Service Provider (AISP)
**I want to** get account balances per Berlin Group specification
**So that** I can display current balance information to Payment Service Users (PSUs) in compliance with PSD2 regulations and the Berlin Group NextGenPSD2 standard

## Acceptance Criteria

1. The system shall retrieve balance information for a specific account when provided with a valid account-id
2. The account-id used in the request must be obtained from a prior "GET Account List" call
3. The user must be authenticated before accessing balance information
4. The user must have the appropriate view access (SYSTEM_READ_BALANCES_BERLIN_GROUP_VIEW_ID) to retrieve balances
5. The system shall validate that the requesting application passes PSD2 AISP authorization checks
6. The response shall include the account IBAN and a list of balance records
7. Each balance record shall contain balance amount (currency and amount), balance type, last change date/time, and reference date
8. The account-id shall remain constant throughout the lifecycle of a given consent
9. The system shall return appropriate error responses for unauthorized access (403) or invalid requests

## Technical Context

- **Classes/Services Involved**:
  - `APIMethods_AccountInformationServiceAISApi` - Main API endpoint handler for Account Information Service
  - `JSONFactory_BERLIN_GROUP_1_3` - JSON response factory for Berlin Group v1.3 format
  - `BankAccountBalanceNewStyle` - Service for retrieving bank account balances
  - `NewStyle.function` - Utility functions for account retrieval and validation
  - `Views` - View-based access control for account permissions

- **Input Data**:
  - Path parameter: `ACCOUNT_ID` - The tokenized account identifier
  - Authentication: User authentication token (OAuth/consent-based)
  - Headers: Standard Berlin Group headers (X-Request-ID, Consent-ID, etc.)

- **Output Data**:
  - `AccountBalancesV13` JSON response containing:
    - `account`: Object with IBAN
    - `balances`: Array of balance objects with:
      - `balanceAmount`: Object with `currency` (string) and `amount` (string)
      - `balanceType`: String indicating type (e.g., "AC" for available balance)
      - `lastChangeDateTime`: ISO 8601 timestamp of last balance change
      - `referenceDate`: Date reference for the balance

- **Processing Type**: Real-time API request-response

## Relevant Endpoints

**IMPORTANT**: The capability description states "Get account balances per Berlin Group specification" - the verb "Get" explicitly justifies a retrieval endpoint.

- **Endpoint**: `GET /accounts/{ACCOUNT_ID}/balances`
  - **Justification (from description)**: "Get account balances" - the word "Get" directly justifies this retrieval endpoint
  - **Purpose**: Retrieve balance information for a specific account identified by account-id
  - **Request**: 
    - Path parameter: `ACCOUNT_ID` (string) - The account identifier obtained from GET Account List
    - Headers: Authentication token, X-Request-ID, Consent-ID
  - **Response**: 
    ```json
    {
      "account": {
        "iban": "DE91 1000 0000 0123 4567 89"
      },
      "balances": [
        {
          "balanceAmount": {
            "currency": "EUR",
            "amount": "50.89"
          },
          "balanceType": "AC",
          "lastChangeDateTime": "yyyy-MM-dd'T'HH:mm:ss.SSSZ",
          "lastCommittedTransaction": "String",
          "referenceDate": "2018-03-08"
        }
      ]
    }
    ```

## Business Rules (from capability description)

1. **Berlin Group Specification Compliance**: The balance retrieval must conform to the Berlin Group NextGenPSD2 v1.3 specification for Account Information Services (AIS)
2. **Account-ID Tokenization**: The account-id can be a tokenized identification due to data protection reasons since path information might be logged on intermediary servers within the ASPSP sphere
3. **Account-ID Consistency**: The account-id must remain constant at least throughout the lifecycle of a given consent
4. **Prior Account List Retrieval**: The account-id must be retrieved by the "GET Account List" call before accessing balances
5. **Consent-Based Access**: Access to balance information is governed by the consent granted through the /consents endpoint
6. **PSD2 AISP Authorization**: The requesting application must pass PSD2 Account Information Service Provider (AISP) authorization checks
7. **View-Based Access Control**: Users must have the SYSTEM_READ_BALANCES_BERLIN_GROUP_VIEW_ID view access to retrieve balance information

## Data Validations

- **Account ID Validation**: The account-id must be a valid identifier that exists in the system
- **Authentication Validation**: User must be authenticated (authenticatedAccess check)
- **PSD2 AISP Validation**: Application must pass PSD2 AISP authorization (passesPsd2Aisp check)
- **View Access Validation**: User must have the appropriate view access for reading balances (checkAccountAccess with SYSTEM_READ_BALANCES_BERLIN_GROUP_VIEW_ID)
- **Error Handling**: 
  - 403 Forbidden: When user lacks view access to the account balances
  - 401 Unauthorized: When user is not logged in
  - 404 Not Found: When account-id does not exist

## Dependencies

- **Upstream**:
  - User must be authenticated via OAuth/OpenID Connect or Direct Login
  - A valid consent must exist granting access to account balances
  - Account list must be retrieved first to obtain valid account-id values
  - User must have been granted the SYSTEM_READ_BALANCES_BERLIN_GROUP_VIEW_ID view on the account

- **Downstream**:
  - Balance information can be used by TPPs to display account status to PSUs
  - Balance data may be aggregated across multiple accounts for personal finance management applications

- **External Systems**:
  - Bank connector (LocalMappedConnector or external backend via REST/Akka/Kafka connectors)
  - Consent management system for validating access permissions
  - View management system for access control

## Notes for Implementation

- **Data Protection**: The account-id may be tokenized for privacy reasons as path information could be logged on intermediary servers
- **Real-time Processing**: This is a high-volume, real-time capability requiring efficient database queries and caching strategies
- **Balance Types**: The system supports multiple balance types (e.g., available balance, booked balance) as per Berlin Group specification
- **Currency Handling**: Balance amounts are returned as strings to preserve precision for financial calculations
- **Date/Time Formatting**: lastChangeDateTime uses ISO 8601 format with milliseconds (yyyy-MM-dd'T'HH:mm:ss.SSSZ)
- **IBAN Extraction**: The system extracts IBAN from account routing information using the AccountRoutingScheme.IBAN scheme
- **Consent Lifecycle**: The account-id remains constant throughout the consent lifecycle, enabling consistent access patterns

## Quality Checklist

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (TPP/AISP)
- [x] Business value is stated (PSD2 compliance, balance display)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged
- [x] Only relevant endpoints are included (GET /accounts/{ACCOUNT_ID}/balances)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific word from capability description justifies inclusion ("Get")
- [x] No CRUD operations are inferred beyond what the description explicitly states
- [x] Words like "manage" have been interpreted narrowly - only retrieval operation included as per "Get" verb
