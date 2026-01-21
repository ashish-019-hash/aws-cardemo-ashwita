# User Story for Transaction Listing

## Capability Input

- **Name**: Transaction Listing
- **Description**: Retrieve transaction history for accounts with filtering and pagination
- **Frequency**: Real-time
- **Volume**: Very High

---

## Operation Verb Analysis

Based on the capability description, the following operation verbs are identified:

| Verb Found | Operation Type | Justification |
|------------|---------------|---------------|
| "Retrieve" | READ/RETRIEVAL | Explicitly stated: "Retrieve transaction history for accounts" |
| "history" | READ/RETRIEVAL | Implies listing/browsing past transactions |
| "filtering" | READ/RETRIEVAL | Explicitly stated: "with filtering" - implies search/query capability |
| "pagination" | READ/RETRIEVAL | Explicitly stated: "and pagination" - implies listing with page navigation |

**Operations NOT included** (verbs not present in description):
- CREATE operations: No "create", "register", "onboard", "set up", "add", "establish", "initialize", or "provision" mentioned
- UPDATE operations: No "manage", "configure", "maintain", "update", "modify", "change", "edit", "adjust", "set", or "customize" mentioned
- DELETE operations: No "delete", "remove", "deactivate", "close", "terminate", "disable", "archive", "retire", or "cancel" mentioned

---

## Story Overview

**As a** third-party developer, fintech application, account holder, or financial service provider integrating with the Open Bank Project platform
**I want to** retrieve transaction history for accounts with filtering and pagination capabilities
**So that** I can display transaction records to end users, enable transaction search and analysis workflows in my application, support financial reconciliation processes, provide transaction categorization services, and enable users to review their spending patterns and account activity

---

## Acceptance Criteria

1. The system shall allow retrieval of transaction history for a specific account at a bank
2. The system shall support filtering transactions by various criteria (e.g., date range, amount range, transaction type, counterparty)
3. The system shall support pagination to efficiently retrieve large transaction sets in manageable chunks
4. The system shall return only transactions for accounts that the requesting user has been granted access to view
5. The system shall return transaction details including transaction ID, date, amount, currency, description, and counterparty information
6. The system shall support specifying the number of transactions per page and the page/offset for pagination
7. The system shall return transactions in chronological order (newest first or oldest first based on request parameters)
8. The system shall return responses in real-time with appropriate performance characteristics for very high-volume usage patterns
9. The system shall return appropriate error responses (e.g., HTTP 404) when the specified bank or account is not found
10. The system shall return an empty list when no transactions match the specified filter criteria
11. The system shall enforce access control to ensure users can only see transactions for accounts they have been granted permission to view
12. The system shall include pagination metadata in responses (total count, current page, has more pages)

---

## Technical Context

- **Classes/Services Involved** (from Scala source code):
  - `APIMethods400` (code.api.v4_0_0.APIMethods400) - REST endpoint definitions for transaction listing
  - `JSONFactory400` (code.api.v4_0_0.JSONFactory4.0.0) - JSON response factory with transaction list creation methods
  - `NewStyle.function` (code.api.util.NewStyle) - Service layer with transaction retrieval methods
  - `Connector` (code.bankconnectors.Connector) - Backend connector abstraction for transaction data access
  - `Views` (code.views.Views) - View/permission management for transaction access control
  - `TransactionJson400` / `TransactionsJson400` - Case classes defining response structure
  - `MappedTransaction` - Transaction entity mapping

- **Input Data**: 
  - For transaction list retrieval (`GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions`): Bank identifier, Account identifier, and View identifier as path parameters
  - Authentication token (OAuth/DirectLogin) to identify the requesting user
  - Query parameters for filtering:
    - `from_date` / `to_date` - Date range filter
    - `offset` - Pagination offset
    - `limit` - Number of transactions per page
    - `sort_direction` - ASC or DESC ordering

- **Output Data** (based on transaction response case classes):
  - `id` (String) - Unique transaction identifier
  - `this_account` (ThisAccountJson) - Account information for the transaction
  - `other_account` (OtherAccountJson) - Counterparty account information
  - `details` (TransactionDetailsJson) - Transaction details including:
    - `type` (String) - Transaction type
    - `description` (String) - Transaction description
    - `posted` (Date) - Posted date
    - `completed` (Date) - Completion date
    - `new_balance` (AmountOfMoney) - Balance after transaction
    - `value` (AmountOfMoney) - Transaction amount with currency
  - `metadata` (TransactionMetadataJson) - Transaction metadata (tags, comments, images)

- **Processing Type**: API / Real-time / Synchronous request-response

---

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description. Only READ/RETRIEVAL operations are included as the description only contains the verbs "Retrieve", "filtering", and "pagination".

### Endpoint 1: Get Transactions for Account (with View)

- **Endpoint**: `GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions`
  - **Justification (from description)**: "Retrieve transaction history for accounts with filtering and pagination"
  - **Purpose**: Retrieve transaction history for a specific account with a specific view, supporting filtering and pagination
  - **Scala Implementation**: `APIMethods400.getTransactionsForBankAccount` -> `NewStyle.function.getTransactions()` -> `JSONFactory400.createTransactionsJson()`
  - **Request**: 
    ```
    GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      ACCOUNT_ID: The unique identifier of the account (required)
      VIEW_ID: The view identifier defining access permissions (required)
    Query Parameters:
      from_date: Start date for filtering (optional, ISO 8601 format)
      to_date: End date for filtering (optional, ISO 8601 format)
      offset: Pagination offset (optional, default 0)
      limit: Number of transactions to return (optional, default 50)
      sort_direction: Sort order - ASC or DESC (optional, default DESC)
    ```
  - **Response** (based on `TransactionsJson400` case class): 
    ```json
    {
      "transactions": [
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
            ]
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
            "narrative": "Morning coffee",
            "comments": [],
            "tags": [
              {"id": "tag-001", "value": "food", "date": "2024-01-15T11:00:00Z"}
            ],
            "images": []
          }
        },
        {
          "id": "transaction-id-002",
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
            "id": "counterparty-002",
            "holder": {
              "name": "Employer Inc",
              "is_alias": false
            },
            "bank_routing": {
              "scheme": "BIC",
              "address": "HANDFIHH"
            },
            "account_routings": [
              {"scheme": "IBAN", "address": "FI1122334455667788"}
            ]
          },
          "details": {
            "type": "SEPA",
            "description": "Salary January 2024",
            "posted": "2024-01-14T08:00:00Z",
            "completed": "2024-01-14T08:00:00Z",
            "new_balance": {
              "currency": "EUR",
              "amount": "1500.00"
            },
            "value": {
              "currency": "EUR",
              "amount": "3000.00"
            }
          },
          "metadata": {
            "narrative": "Monthly salary",
            "comments": [],
            "tags": [
              {"id": "tag-002", "value": "income", "date": "2024-01-14T09:00:00Z"}
            ],
            "images": []
          }
        }
      ]
    }
    ```

### Endpoint 2: Get Transactions for Account (Core)

- **Endpoint**: `GET /obp/v4.0.0/my/banks/BANK_ID/accounts/ACCOUNT_ID/transactions`
  - **Justification (from description)**: "Retrieve transaction history for accounts" - supports retrieval for user's own accounts
  - **Purpose**: Retrieve transaction history for the authenticated user's account with default view permissions
  - **Scala Implementation**: `APIMethods400.getMyTransactions` -> `NewStyle.function.getMyTransactions()` -> `JSONFactory400.createTransactionsJson()`
  - **Request**: 
    ```
    GET /obp/v4.0.0/my/banks/BANK_ID/accounts/ACCOUNT_ID/transactions
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      ACCOUNT_ID: The unique identifier of the account (required)
    Query Parameters:
      from_date: Start date for filtering (optional)
      to_date: End date for filtering (optional)
      offset: Pagination offset (optional)
      limit: Number of transactions to return (optional)
    ```
  - **Response**: Same structure as Endpoint 1

### Endpoint 3: Get Transactions with Filtering Parameters

- **Endpoint**: `GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/filter`
  - **Justification (from description)**: "with filtering" - explicitly supports advanced filtering capabilities
  - **Purpose**: Retrieve transactions with advanced filtering options including amount range, transaction type, and counterparty filters
  - **Scala Implementation**: `APIMethods400.getTransactionsWithFilter` -> `NewStyle.function.getTransactionsWithFilter()` -> `JSONFactory400.createTransactionsJson()`
  - **Request**: 
    ```
    GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/filter
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      ACCOUNT_ID: The unique identifier of the account (required)
      VIEW_ID: The view identifier (required)
    Query Parameters:
      from_date: Start date for filtering (optional)
      to_date: End date for filtering (optional)
      min_amount: Minimum transaction amount (optional)
      max_amount: Maximum transaction amount (optional)
      transaction_type: Filter by transaction type (optional)
      counterparty_name: Filter by counterparty name (optional)
      offset: Pagination offset (optional)
      limit: Number of transactions to return (optional)
    ```
  - **Response**: Same structure as Endpoint 1 with filtered results

### Endpoint 4: Get Transaction Request Types (Supporting Endpoint)

- **Endpoint**: `GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-request-types`
  - **Justification (from description)**: "filtering" - supports filtering by transaction type by providing available types
  - **Purpose**: Retrieve available transaction request types to support filtering by transaction type
  - **Scala Implementation**: `APIMethods400.getTransactionRequestTypes` -> `NewStyle.function.getTransactionRequestTypes()`
  - **Request**: 
    ```
    GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-request-types
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      ACCOUNT_ID: The unique identifier of the account (required)
      VIEW_ID: The view identifier (required)
    ```
  - **Response**: 
    ```json
    {
      "transaction_request_types": [
        {"value": "SEPA", "charge": {"summary": "SEPA Transfer", "value": {"currency": "EUR", "amount": "0.00"}}},
        {"value": "COUNTERPARTY", "charge": {"summary": "Counterparty Transfer", "value": {"currency": "EUR", "amount": "0.00"}}},
        {"value": "TRANSFER_TO_ACCOUNT", "charge": {"summary": "Account Transfer", "value": {"currency": "EUR", "amount": "0.00"}}}
      ]
    }
    ```

### Endpoints NOT Included (with justification)

| Endpoint | Operation Type | Reason for Exclusion |
|----------|---------------|---------------------|
| POST /banks/{bank_id}/accounts/{account_id}/transactions | CREATE | No "create", "add", "initiate", or similar verb in description |
| PUT /banks/{bank_id}/accounts/{account_id}/transactions/{transaction_id} | UPDATE | No "manage", "update", "modify", or similar verb in description |
| DELETE /banks/{bank_id}/accounts/{account_id}/transactions/{transaction_id} | DELETE | No "delete", "remove", "cancel", or similar verb in description |
| POST /banks/{bank_id}/transaction-request-types/.../transaction-requests | CREATE | No "create", "initiate", or similar verb - this is payment initiation, not listing |

---

## Business Rules (from capability description)

1. **Account Scope**: Transaction listing is scoped to a specific account - users must specify which account's transactions to retrieve (from: "transaction history for accounts")
2. **Access Control**: Only transactions for accounts that the user has access to should be returned - the system must enforce view/permission-based access control (implied by "for accounts")
3. **Filtering Support**: The system must support filtering transactions by various criteria to enable users to find specific transactions (from: "with filtering")
4. **Pagination Support**: The system must support pagination to efficiently handle large transaction sets and enable incremental data loading (from: "and pagination")
5. **Real-time Access**: Transaction listing must support real-time access patterns with low latency (from: Frequency = Real-time)
6. **Very High Volume Support**: The system must be designed to handle very high volume of transaction listing requests efficiently (from: Volume = Very High)
7. **Historical Data**: The system provides access to transaction "history" - implying support for retrieving past transactions over time

---

## Data Validations (if applicable)

- Bank identifier (BANK_ID) must be valid and exist in the system
- Account identifier (ACCOUNT_ID) must be valid and belong to the specified bank
- View identifier (VIEW_ID) must be valid and the user must have access to it
- User must be authenticated with a valid OAuth token or DirectLogin credentials
- User must have at least one view/permission granted on the account to see transactions
- Date range filters must be valid ISO 8601 dates with from_date <= to_date
- Pagination offset must be a non-negative integer
- Pagination limit must be a positive integer within allowed bounds (e.g., 1-500)
- Amount filters must be valid numeric values with min_amount <= max_amount
- Error response (HTTP 404 Not Found / `BankNotFound`) must be returned when BANK_ID does not exist
- Error response (HTTP 404 Not Found / `AccountNotFound`) must be returned when ACCOUNT_ID does not exist
- Error response (HTTP 401 Unauthorized) for missing or invalid authentication
- Error response (HTTP 403 Forbidden) when user lacks permission to access the account's transactions
- Error response (HTTP 400 Bad Request) for invalid filter parameters
- Empty list returned (not error) when no transactions match the filter criteria
- All monetary values must include currency code and properly formatted amount

---

## Dependencies

- **Upstream**: 
  - Bank must exist in the system (Bank Creation capability)
  - Account must be created and linked to the bank (Account Creation capability)
  - Transactions must exist for the account (Payment Initiation or Transaction Processing capabilities)
  - User must be authenticated (Authentication & Security capabilities)
  - User must have been granted view/permission access to the account (View & Permission Management capabilities)
  - Account-user access relationships must be established

- **Downstream**: 
  - Retrieved transaction information is used for financial analysis and reporting
  - Transaction data is displayed in third-party application user interfaces for account activity review
  - Transaction identifiers are used in subsequent API calls for transaction details retrieval
  - Transaction data supports reconciliation processes in accounting applications
  - Transaction history enables spending pattern analysis and budgeting features

- **External Systems**: 
  - Backend banking connectors may retrieve transaction data from core banking systems
  - Transaction data may be fetched from external transaction processing systems
  - Real-time transaction feeds may be integrated for up-to-date transaction information

---

## Notes for Implementation

- **Performance Consideration**: Implement efficient database indexing on transaction date, account ID, and amount fields to support fast filtering and pagination queries
- **Caching Strategy**: Consider caching recent transaction lists with appropriate TTL, but ensure cache invalidation when new transactions are posted
- **Pagination Implementation**: Use cursor-based pagination for better performance with large datasets, or offset-based pagination for simpler implementation
- **Date Range Defaults**: Consider implementing sensible defaults for date range (e.g., last 90 days) when not specified to prevent unbounded queries
- **Filter Combinations**: Support combining multiple filters (AND logic) for flexible transaction search
- **Response Size Limits**: Implement maximum limit on transactions per request to prevent memory issues and ensure consistent response times
- **Sorting Options**: Support sorting by date (default), amount, or other relevant fields
- **Currency Handling**: Ensure proper handling of multi-currency accounts and transactions

### Needs SME Input
- Clarify the maximum number of transactions that can be returned in a single request
- Determine the default and maximum date range for transaction history retrieval
- Confirm which filter parameters are required vs optional
- Clarify if transaction listing should support full-text search on description
- Determine if there are any transaction types that should be excluded from listing (e.g., internal system transactions)
- Confirm the expected response time SLA for transaction listing given the "Very High" volume
- Clarify if transaction metadata (tags, comments) should always be included or be optional based on a parameter
- Determine if there should be support for exporting transactions in different formats (CSV, PDF)

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical implementation details)
- [x] User role is clearly identified (third-party developer, fintech application, account holder)
- [x] Business value is stated (display transactions, enable analysis, support reconciliation)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered (retrieve, filter, paginate)
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (SME input section)
- [x] Only relevant endpoints are included (READ/RETRIEVAL only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words/phrases from the capability description justify inclusion
- [x] No endpoint type (create, update, delete) has been added beyond what the description explicitly states
- [x] Words like "manage" have been interpreted narrowly - only retrieval operations included as per description

---

*This user story was generated by analyzing the capability description "Retrieve transaction history for accounts with filtering and pagination" and applying the Operation Derivation Rules to determine which endpoints to include.*
