# User Story for Transaction Details

## Capability Input

- **Name**: Transaction Details
- **Description**: Get detailed information about a specific transaction
- **Frequency**: Real-time
- **Volume**: High

---

## Operation Verb Analysis

Based on the capability description, the following operation verbs are identified:

| Verb Found | Operation Type | Justification |
|------------|---------------|---------------|
| "Get" | READ/RETRIEVAL | Explicitly stated: "Get detailed information about a specific transaction" |
| "detailed information" | READ/RETRIEVAL | Implies comprehensive data retrieval for a single transaction |
| "specific transaction" | READ/RETRIEVAL | Implies retrieval by unique identifier |

**Operations NOT included** (verbs not present in description):
- CREATE operations: No "create", "register", "onboard", "set up", "add", "establish", "initialize", or "provision" mentioned
- UPDATE operations: No "manage", "configure", "maintain", "update", "modify", "change", "edit", "adjust", "set", or "customize" mentioned
- DELETE operations: No "delete", "remove", "deactivate", "close", "terminate", "disable", "archive", "retire", or "cancel" mentioned

---

## Story Overview

**As a** third-party developer, fintech application, account holder, or financial service provider integrating with the Open Bank Project platform
**I want to** get detailed information about a specific transaction
**So that** I can display comprehensive transaction details to end users, enable transaction verification and reconciliation workflows, support dispute resolution processes, provide detailed transaction receipts, and enable users to review complete information about individual financial activities

---

## Acceptance Criteria

1. The system shall allow retrieval of detailed information for a specific transaction identified by its unique transaction ID
2. The system shall return comprehensive transaction details including transaction ID, date/time, amount, currency, description, type, and status
3. The system shall return counterparty information associated with the transaction
4. The system shall return account information for both the source and destination of the transaction
5. The system shall return transaction metadata including any tags, comments, or images associated with the transaction
6. The system shall return only transaction details for accounts that the requesting user has been granted access to view
7. The system shall return responses in real-time with appropriate performance characteristics for high-volume usage patterns
8. The system shall return appropriate error responses (e.g., HTTP 404) when the specified transaction, bank, or account is not found
9. The system shall enforce access control to ensure users can only see details for transactions on accounts they have been granted permission to view
10. The system shall return the transaction's balance impact showing the account balance before and after the transaction
11. The system shall include double-entry bookkeeping information when available (debit and credit sides)

---

## Technical Context

- **Classes/Services Involved** (from Scala source code):
  - `APIMethods400` (code.api.v4_0_0.APIMethods400) - REST endpoint definitions for transaction detail retrieval
  - `JSONFactory400` (code.api.v4_0_0.JSONFactory4.0.0) - JSON response factory with transaction detail creation methods
  - `NewStyle.function` (code.api.util.NewStyle) - Service layer with transaction retrieval methods
  - `Connector` (code.bankconnectors.Connector) - Backend connector abstraction for transaction data access
  - `Views` (code.views.Views) - View/permission management for transaction access control
  - `TransactionJson400` - Case class defining single transaction response structure
  - `TransactionDetailsJson` - Case class for transaction detail fields
  - `MappedTransaction` - Transaction entity mapping

- **Input Data**: 
  - For transaction detail retrieval (`GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID`): Bank identifier, Account identifier, View identifier, and Transaction identifier as path parameters
  - Authentication token (OAuth/DirectLogin) to identify the requesting user

- **Output Data** (based on transaction response case classes):
  - `id` (String) - Unique transaction identifier
  - `this_account` (ThisAccountJson) - Account information for the transaction owner
    - `id` (String) - Account identifier
    - `bank_routing` (BankRoutingJson) - Bank routing information
    - `account_routings` (List[AccountRoutingJson]) - Account routing details (IBAN, etc.)
    - `holders` (List[AccountHolderJson]) - Account holder information
  - `other_account` (OtherAccountJson) - Counterparty account information
    - `id` (String) - Counterparty identifier
    - `holder` (AccountHolderJson) - Counterparty holder details
    - `bank_routing` (BankRoutingJson) - Counterparty bank routing
    - `account_routings` (List[AccountRoutingJson]) - Counterparty account routing
    - `metadata` (OtherAccountMetadataJson) - Counterparty metadata
  - `details` (TransactionDetailsJson) - Transaction details including:
    - `type` (String) - Transaction type (SEPA, COUNTERPARTY, etc.)
    - `description` (String) - Transaction description
    - `posted` (Date) - Posted date/time
    - `completed` (Date) - Completion date/time
    - `new_balance` (AmountOfMoney) - Balance after transaction
    - `value` (AmountOfMoney) - Transaction amount with currency
  - `metadata` (TransactionMetadataJson) - Transaction metadata
    - `narrative` (String) - User-defined narrative
    - `comments` (List[TransactionCommentJson]) - Comments on transaction
    - `tags` (List[TransactionTagJson]) - Tags for categorization
    - `images` (List[TransactionImageJson]) - Associated images

- **Processing Type**: API / Real-time / Synchronous request-response

---

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description. Only READ/RETRIEVAL operations are included as the description only contains the verb "Get" for retrieving detailed information.

### Endpoint 1: Get Transaction by ID (with View)

- **Endpoint**: `GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID`
  - **Justification (from description)**: "Get detailed information about a specific transaction"
  - **Purpose**: Retrieve comprehensive details for a specific transaction identified by its unique ID
  - **Scala Implementation**: `APIMethods400.getTransaction` -> `NewStyle.function.getTransaction()` -> `JSONFactory400.createTransactionJson()`
  - **Request**: 
    ```
    GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      ACCOUNT_ID: The unique identifier of the account (required)
      VIEW_ID: The view identifier defining access permissions (required)
      TRANSACTION_ID: The unique identifier of the transaction (required)
    ```
  - **Response** (based on `TransactionJson400` case class): 
    ```json
    {
      "id": "transaction-id-001",
      "this_account": {
        "id": "account-id-001",
        "bank_routing": {
          "scheme": "BIC",
          "address": "OKOYFIHH"
        },
        "account_routings": [
          {"scheme": "IBAN", "address": "FI1234567890123456"}
        ],
        "holders": [
          {"name": "John Doe", "is_alias": false}
        ]
      },
      "other_account": {
        "id": "counterparty-001",
        "holder": {
          "name": "Coffee Shop Ltd",
          "is_alias": false
        },
        "bank_routing": {
          "scheme": "BIC",
          "address": "NDEAFIHH"
        },
        "account_routings": [
          {"scheme": "IBAN", "address": "FI9876543210987654"}
        ],
        "metadata": {
          "public_alias": "Coffee Shop",
          "private_alias": "My favorite coffee place",
          "more_info": "Local coffee shop",
          "url": "https://coffeeshop.example.com",
          "image_url": "https://coffeeshop.example.com/logo.png",
          "open_corporates_url": "",
          "corporate_location": {
            "latitude": 60.1699,
            "longitude": 24.9384
          },
          "physical_location": {
            "latitude": 60.1699,
            "longitude": 24.9384
          }
        }
      },
      "details": {
        "type": "SEPA",
        "description": "Coffee purchase",
        "posted": "2024-01-15T10:30:00Z",
        "completed": "2024-01-15T10:30:00Z",
        "new_balance": {
          "currency": "EUR",
          "amount": "1495.50"
        },
        "value": {
          "currency": "EUR",
          "amount": "-4.50"
        }
      },
      "metadata": {
        "narrative": "Morning coffee at my favorite place",
        "comments": [
          {
            "id": "comment-001",
            "value": "Great coffee as always",
            "date": "2024-01-15T11:00:00Z",
            "user": {
              "id": "user-001",
              "provider": "OBP",
              "display_name": "John Doe"
            }
          }
        ],
        "tags": [
          {
            "id": "tag-001",
            "value": "food",
            "date": "2024-01-15T11:00:00Z",
            "user": {
              "id": "user-001",
              "provider": "OBP",
              "display_name": "John Doe"
            }
          },
          {
            "id": "tag-002",
            "value": "daily-expense",
            "date": "2024-01-15T11:00:00Z",
            "user": {
              "id": "user-001",
              "provider": "OBP",
              "display_name": "John Doe"
            }
          }
        ],
        "images": [
          {
            "id": "image-001",
            "label": "Receipt",
            "url": "https://example.com/receipts/receipt-001.jpg",
            "date": "2024-01-15T11:05:00Z",
            "user": {
              "id": "user-001",
              "provider": "OBP",
              "display_name": "John Doe"
            }
          }
        ]
      }
    }
    ```

### Endpoint 2: Get Transaction by ID (Core - User's Own Account)

- **Endpoint**: `GET /obp/v4.0.0/my/banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID`
  - **Justification (from description)**: "Get detailed information about a specific transaction" - supports retrieval for user's own accounts
  - **Purpose**: Retrieve detailed transaction information for the authenticated user's account with default view permissions
  - **Scala Implementation**: `APIMethods400.getMyTransaction` -> `NewStyle.function.getMyTransaction()` -> `JSONFactory400.createTransactionJson()`
  - **Request**: 
    ```
    GET /obp/v4.0.0/my/banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      ACCOUNT_ID: The unique identifier of the account (required)
      TRANSACTION_ID: The unique identifier of the transaction (required)
    ```
  - **Response**: Same structure as Endpoint 1

### Endpoint 3: Get Transaction Details (Firehose - Admin Access)

- **Endpoint**: `GET /obp/v4.0.0/banks/BANK_ID/firehose/transactions/TRANSACTION_ID`
  - **Justification (from description)**: "Get detailed information about a specific transaction" - supports administrative access to transaction details
  - **Purpose**: Retrieve detailed transaction information for authorized applications with firehose access (typically for compliance, audit, or administrative purposes)
  - **Scala Implementation**: `APIMethods400.getFirehoseTransaction` -> `NewStyle.function.getFirehoseTransaction()` -> `JSONFactory400.createTransactionJson()`
  - **Request**: 
    ```
    GET /obp/v4.0.0/banks/BANK_ID/firehose/transactions/TRANSACTION_ID
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      TRANSACTION_ID: The unique identifier of the transaction (required)
    ```
  - **Response**: Same structure as Endpoint 1, potentially with additional administrative fields

### Endpoints NOT Included (with justification)

| Endpoint | Operation Type | Reason for Exclusion |
|----------|---------------|---------------------|
| POST /banks/{bank_id}/accounts/{account_id}/transactions | CREATE | No "create", "add", "initiate", or similar verb in description |
| PUT /banks/{bank_id}/accounts/{account_id}/transactions/{transaction_id} | UPDATE | No "manage", "update", "modify", or similar verb in description |
| DELETE /banks/{bank_id}/accounts/{account_id}/transactions/{transaction_id} | DELETE | No "delete", "remove", "cancel", or similar verb in description |
| POST /banks/{bank_id}/accounts/{account_id}/transactions/{transaction_id}/metadata/tags | CREATE | No "add", "create" for tags mentioned in description |
| POST /banks/{bank_id}/accounts/{account_id}/transactions/{transaction_id}/metadata/comments | CREATE | No "add", "create" for comments mentioned in description |
| GET /banks/{bank_id}/accounts/{account_id}/transactions | LIST | This is transaction listing, not getting details of a specific transaction |

---

## Business Rules (from capability description)

1. **Single Transaction Scope**: Transaction details retrieval is scoped to a single, specific transaction - users must provide a unique transaction identifier (from: "a specific transaction")
2. **Detailed Information**: The system must return comprehensive, detailed information about the transaction, not just summary data (from: "detailed information")
3. **Access Control**: Only transaction details for accounts that the user has access to should be returned - the system must enforce view/permission-based access control (implied by accessing "a specific transaction")
4. **Real-time Access**: Transaction details must be available in real-time with low latency (from: Frequency = Real-time)
5. **High Volume Support**: The system must be designed to handle high volume of transaction detail requests efficiently (from: Volume = High)
6. **Complete Data Retrieval**: The "Get" operation implies a complete retrieval of all available information about the transaction, including metadata, counterparty details, and balance impact

---

## Data Validations (if applicable)

- Bank identifier (BANK_ID) must be valid and exist in the system
- Account identifier (ACCOUNT_ID) must be valid and belong to the specified bank
- Transaction identifier (TRANSACTION_ID) must be valid and belong to the specified account
- View identifier (VIEW_ID) must be valid and the user must have access to it (when using view-based endpoint)
- User must be authenticated with a valid OAuth token or DirectLogin credentials
- User must have at least one view/permission granted on the account to see transaction details
- Error response (HTTP 404 Not Found / `BankNotFound`) must be returned when BANK_ID does not exist
- Error response (HTTP 404 Not Found / `AccountNotFound`) must be returned when ACCOUNT_ID does not exist
- Error response (HTTP 404 Not Found / `TransactionNotFound`) must be returned when TRANSACTION_ID does not exist
- Error response (HTTP 401 Unauthorized) for missing or invalid authentication
- Error response (HTTP 403 Forbidden) when user lacks permission to access the transaction details
- All monetary values must include currency code and properly formatted amount
- All date/time values must be in ISO 8601 format

---

## Dependencies

- **Upstream**: 
  - Bank must exist in the system (Bank Creation capability)
  - Account must be created and linked to the bank (Account Creation capability)
  - Transaction must exist for the account (Payment Initiation or Transaction Processing capabilities)
  - User must be authenticated (Authentication & Security capabilities)
  - User must have been granted view/permission access to the account (View & Permission Management capabilities)
  - Account-user access relationships must be established

- **Downstream**: 
  - Transaction details may be used for dispute resolution workflows
  - Transaction details may be used for reconciliation processes
  - Transaction details may be displayed in user interfaces for review
  - Transaction details may be used for generating transaction receipts
  - Transaction details may be used for audit and compliance reporting

- **External Systems**: 
  - Backend banking systems via Connector abstraction (REST, Akka, Stored Procedure, Kafka, etc.)
  - Authentication providers (OAuth, OpenID Connect)

---

## Notes for Implementation

- **Performance Considerations**: As this is a real-time, high-volume capability, caching strategies should be considered for frequently accessed transaction details
- **Data Completeness**: Ensure all related data (metadata, counterparty info, balance impact) is retrieved in a single request to minimize round trips
- **View Permissions**: The view system controls which fields are visible - ensure proper field filtering based on view permissions
- **Audit Trail**: Consider logging access to transaction details for compliance and audit purposes
- **Currency Handling**: Ensure proper handling of multi-currency transactions with correct currency codes and formatting
- **Time Zone Handling**: Transaction timestamps should be stored and returned in UTC with proper ISO 8601 formatting
- **Needs SME Input**: 
  - Clarify if there are any transaction types that require special handling or additional fields
  - Confirm the exact set of metadata fields that should be returned for different view types
  - Determine if there are any regulatory requirements for transaction detail retention and access logging

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
- [x] For each endpoint, specific words in the capability description justify the endpoint
- [x] No endpoint type (create, update, view, list, delete) has been added unless its verb appears in the description
- [x] Words like "manage" have been interpreted narrowly - only "Get" (retrieval) operations are included
