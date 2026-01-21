# User Story for Account-to-Account Transfer

## Capability Input

- **Name**: Account-to-Account Transfer
- **Description**: Initiate transfers between accounts within the platform
- **Frequency**: Real-time
- **Volume**: High

---

## Operation Verb Analysis

Based on the capability description, the following operation verbs are identified:

| Verb Found | Operation Type | Justification |
|------------|---------------|---------------|
| "Initiate" | CREATE | Explicitly stated: "Initiate transfers between accounts" |
| "transfers" | CREATE | Implies the action of creating/executing fund movements between accounts |

**Operations NOT included** (verbs not present in description):
- READ/RETRIEVAL operations: No "view", "retrieve", "get", "see", "display", "browse", "search", "list", "query", "lookup", "find", "show", "read", "access", or "fetch" mentioned
- UPDATE operations: No "manage", "configure", "maintain", "update", "modify", "change", "edit", "adjust", "set", or "customize" mentioned
- DELETE operations: No "delete", "remove", "deactivate", "close", "terminate", "disable", "archive", "retire", or "cancel" mentioned

---

## Story Overview

**As a** third-party developer, fintech application, account holder, or financial service provider integrating with the Open Bank Project platform
**I want to** initiate transfers between accounts within the platform
**So that** I can enable fund movements between accounts held on the same platform, support internal account transfers for end users, facilitate treasury operations between organizational accounts, enable automated fund sweeping between accounts, and provide seamless money movement capabilities within the banking ecosystem

---

## Acceptance Criteria

1. The system shall allow initiation of fund transfers from one account to another account within the platform
2. The system shall validate that the source account has sufficient funds before initiating the transfer
3. The system shall validate that both source and destination accounts exist and are active within the platform
4. The system shall require proper authentication and authorization before allowing transfer initiation
5. The system shall support specifying the transfer amount and currency
6. The system shall create a transaction request that can be tracked through its lifecycle
7. The system shall support real-time processing of transfer requests with appropriate performance characteristics for high-volume usage patterns
8. The system shall return a transaction request identifier upon successful initiation for tracking purposes
9. The system shall return appropriate error responses when source or destination account is not found
10. The system shall return appropriate error responses when insufficient funds are available
11. The system shall enforce access control to ensure users can only initiate transfers from accounts they have permission to operate
12. The system shall support optional description/narrative for the transfer
13. The system shall handle currency validation to ensure transfer currency matches account currency or is supported for conversion

---

## Technical Context

- **Classes/Services Involved** (from Scala source code):
  - `APIMethods400` (code.api.v4_0_0.APIMethods400) - REST endpoint definitions for transaction request creation
  - `JSONFactory400` (code.api.v4_0_0.JSONFactory4.0.0) - JSON response factory for transaction request responses
  - `NewStyle.function` (code.api.util.NewStyle) - Service layer with transaction request creation methods
  - `Connector` (code.bankconnectors.Connector) - Backend connector abstraction for transaction processing
  - `TransactionRequestCommonBodyJSON` - Request body structure for transfer initiation
  - `TransactionRequestWithChargeJSON400` - Response structure including transaction request details and charges
  - `MappedTransactionRequest` - Transaction request entity mapping
  - `Views` (code.views.Views) - View/permission management for account access control

- **Input Data**: 
  - For transfer initiation (`POST /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-request-types/TRANSFER_TO_ACCOUNT/transaction-requests`):
    - Bank identifier (BANK_ID) as path parameter
    - Source Account identifier (ACCOUNT_ID) as path parameter
    - View identifier (VIEW_ID) as path parameter
    - Transaction request type (TRANSFER_TO_ACCOUNT) as path parameter
    - Authentication token (OAuth/DirectLogin) to identify the requesting user
    - Request body containing:
      - `to` object with destination bank_id and account_id
      - `value` object with currency and amount
      - `description` (optional) - narrative for the transfer

- **Output Data** (based on transaction request response case classes):
  - `id` (String) - Unique transaction request identifier
  - `type` (String) - Transaction request type (TRANSFER_TO_ACCOUNT)
  - `from` (FromAccountTransfer) - Source account information
  - `details` (TransactionRequestBodyAllTypes) - Transfer details including destination and amount
  - `transaction_ids` (List[String]) - Associated transaction identifiers
  - `status` (String) - Current status of the transaction request
  - `start_date` (Date) - When the request was initiated
  - `end_date` (Date) - When the request was completed
  - `challenge` (ChallengeJson) - Security challenge if SCA is required
  - `charge` (TransactionRequestChargeJson) - Any charges associated with the transfer

- **Processing Type**: API / Real-time / Synchronous request-response

---

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description. Only CREATE operations are included as the description only contains the verb "Initiate".

### Endpoint 1: Initiate Account-to-Account Transfer

- **Endpoint**: `POST /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-request-types/TRANSFER_TO_ACCOUNT/transaction-requests`
  - **Justification (from description)**: "Initiate transfers between accounts within the platform"
  - **Purpose**: Initiate a fund transfer from the source account to another account within the same platform
  - **Scala Implementation**: `APIMethods400.createTransactionRequestTransferToAccount` -> `NewStyle.function.createTransactionRequestv400()` -> `Connector.makePaymentv400()`
  - **Request**: 
    ```
    POST /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-request-types/TRANSFER_TO_ACCOUNT/transaction-requests
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the source bank (required)
      ACCOUNT_ID: The unique identifier of the source account (required)
      VIEW_ID: The view identifier defining access permissions (required)
    Request Body:
    {
      "to": {
        "bank_id": "destination-bank-id",
        "account_id": "destination-account-id"
      },
      "value": {
        "currency": "EUR",
        "amount": "100.00"
      },
      "description": "Transfer for invoice payment"
    }
    ```
  - **Response** (based on `TransactionRequestWithChargeJSON400` case class): 
    ```json
    {
      "id": "transaction-request-id-001",
      "type": "TRANSFER_TO_ACCOUNT",
      "from": {
        "bank_id": "source-bank-id",
        "account_id": "source-account-id"
      },
      "details": {
        "to_transfer_to_account": {
          "bank_id": "destination-bank-id",
          "account_id": "destination-account-id"
        },
        "value": {
          "currency": "EUR",
          "amount": "100.00"
        },
        "description": "Transfer for invoice payment"
      },
      "transaction_ids": ["transaction-id-001"],
      "status": "COMPLETED",
      "start_date": "2024-01-15T10:30:00Z",
      "end_date": "2024-01-15T10:30:05Z",
      "challenge": null,
      "charge": {
        "summary": "Account Transfer Fee",
        "value": {
          "currency": "EUR",
          "amount": "0.00"
        }
      }
    }
    ```

### Endpoint 2: Initiate Account-to-Account Transfer (Alternative Path)

- **Endpoint**: `POST /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-request-types/TRANSFER_TO_ATMS/transaction-requests`
  - **Justification (from description)**: "Initiate transfers between accounts" - supports transfers to accounts accessible via ATM network
  - **Purpose**: Initiate a fund transfer to an account identified by ATM routing information within the platform
  - **Scala Implementation**: `APIMethods400.createTransactionRequestTransferToAtm` -> `NewStyle.function.createTransactionRequestv400()`
  - **Request**: 
    ```
    POST /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-request-types/TRANSFER_TO_ATMS/transaction-requests
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the source bank (required)
      ACCOUNT_ID: The unique identifier of the source account (required)
      VIEW_ID: The view identifier (required)
    Request Body:
    {
      "to": {
        "atm_id": "atm-identifier",
        "bank_id": "destination-bank-id"
      },
      "value": {
        "currency": "EUR",
        "amount": "50.00"
      },
      "description": "ATM transfer"
    }
    ```
  - **Response**: Similar structure to Endpoint 1 with type "TRANSFER_TO_ATMS"

### Endpoints NOT Included (with justification)

| Endpoint | Operation Type | Reason for Exclusion |
|----------|---------------|---------------------|
| GET /banks/{bank_id}/accounts/{account_id}/transaction-requests | READ | No "view", "retrieve", "list", or similar verb in description |
| GET /banks/{bank_id}/accounts/{account_id}/transaction-requests/{transaction_request_id} | READ | No "view", "get", "retrieve", or similar verb in description |
| PUT /banks/{bank_id}/accounts/{account_id}/transaction-requests/{transaction_request_id} | UPDATE | No "manage", "update", "modify", or similar verb in description |
| DELETE /banks/{bank_id}/accounts/{account_id}/transaction-requests/{transaction_request_id} | DELETE | No "delete", "cancel", "remove", or similar verb in description |

---

## Business Rules (from capability description)

1. **Platform Scope**: Transfers are limited to accounts "within the platform" - both source and destination accounts must exist on the Open Bank Project platform (from: "between accounts within the platform")
2. **Transfer Initiation**: The capability focuses on initiating/creating transfers - the system must support creating new transfer requests (from: "Initiate transfers")
3. **Account-to-Account**: Transfers are between accounts - requires valid source and destination account identifiers (from: "between accounts")
4. **Real-time Processing**: Transfer initiation must support real-time processing patterns with low latency (from: Frequency = Real-time)
5. **High Volume Support**: The system must be designed to handle high volume of transfer initiation requests efficiently (from: Volume = High)
6. **Internal Transfers**: The capability is specifically for internal platform transfers, not external/cross-platform transfers (from: "within the platform")

---

## Data Validations (if applicable)

- Bank identifier (BANK_ID) must be valid and exist in the system
- Source Account identifier (ACCOUNT_ID) must be valid and belong to the specified bank
- Destination bank_id and account_id must be valid and exist within the platform
- View identifier (VIEW_ID) must be valid and the user must have transaction initiation permissions
- User must be authenticated with a valid OAuth token or DirectLogin credentials
- User must have appropriate entitlements/permissions to initiate transfers from the source account
- Transfer amount must be a positive numeric value
- Transfer currency must be valid and supported by both source and destination accounts
- Source account must have sufficient available balance for the transfer amount plus any applicable fees
- Destination account must be active and able to receive funds
- Error response (HTTP 404 Not Found / `BankNotFound`) must be returned when BANK_ID does not exist
- Error response (HTTP 404 Not Found / `AccountNotFound`) must be returned when source or destination ACCOUNT_ID does not exist
- Error response (HTTP 401 Unauthorized) for missing or invalid authentication
- Error response (HTTP 403 Forbidden) when user lacks permission to initiate transfers from the account
- Error response (HTTP 400 Bad Request / `InsufficientFunds`) when source account has insufficient balance
- Error response (HTTP 400 Bad Request) for invalid transfer amount or currency
- All monetary values must include currency code and properly formatted amount

---

## Dependencies

- **Upstream**: 
  - Bank must exist in the system (Bank Creation capability)
  - Source account must be created and linked to the bank (Account Creation capability)
  - Destination account must be created and exist within the platform (Account Creation capability)
  - User must be authenticated (Authentication & Security capabilities)
  - User must have been granted appropriate view/permission access to initiate transfers (View & Permission Management capabilities)
  - Source account must have sufficient balance (Account Balance Management capability)
  - Transaction request types must be configured for the bank (Dynamic Configuration capability)

- **Downstream**: 
  - Transaction records are created for both source and destination accounts (Transaction Management capabilities)
  - Account balances are updated for both accounts (Account Balance Management capability)
  - Transaction request status can be checked (Transaction Request Status capability)
  - Challenge/SCA may be required for authorization (Challenge Response capability)
  - Notifications may be triggered for account holders (Webhook & Notification capabilities)

- **External Systems**: 
  - Backend banking connector for actual fund movement execution
  - Strong Customer Authentication (SCA) provider if challenge is required
  - Notification service for transfer confirmations

---

## Notes for Implementation

- **Security Considerations**: Transfer initiation is a sensitive operation requiring proper authentication, authorization, and potentially Strong Customer Authentication (SCA) for amounts above certain thresholds
- **Idempotency**: Consider implementing idempotency keys to prevent duplicate transfers in case of network issues or retries
- **Transaction Atomicity**: The transfer must be atomic - either both debit and credit succeed, or neither does (double-entry bookkeeping)
- **Rate Limiting**: High-volume capability may require rate limiting to prevent abuse
- **Audit Trail**: All transfer initiations should be logged for compliance and audit purposes
- **Currency Handling**: If source and destination accounts have different currencies, currency conversion logic may be required (FX Rate Management capability)
- **Needs SME Input**: 
  - What are the maximum transfer limits per transaction and per day?
  - Are there specific business hours restrictions for transfers?
  - What SCA methods are supported for transfer authorization?
  - Are there any specific compliance requirements (AML/KYC checks) before transfer execution?
  - What is the expected latency SLA for transfer completion?

---

## Quality Checklist

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified
- [x] Business value is stated
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged
- [x] Only relevant endpoints are included
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, I can point to a specific word or phrase in the capability description that justifies this endpoint
- [x] No endpoint type (create, update, view, list, delete) has been added unless its verb (or a clear synonym) appears in the description
- [x] Words like "manage" have been interpreted narrowly - only CREATE operations included as "Initiate" is the only verb present
