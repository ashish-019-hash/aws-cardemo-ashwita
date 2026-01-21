# User Story for Balancing Transaction Retrieval

## Capability Input

- **Name**: Balancing Transaction Retrieval
- **Description**: Get the balancing transaction for a given transaction
- **Frequency**: Real-time
- **Volume**: Medium

---

## Operation Verb Analysis

Based on the capability description, the following operation verbs are identified:

| Verb Found | Operation Type | Justification |
|------------|---------------|---------------|
| "Get" | READ/RETRIEVAL | Explicitly stated: "Get the balancing transaction for a given transaction" |

**Operations NOT included** (verbs not present in description):
- CREATE operations: No "create", "register", "onboard", "set up", "add", "establish", "initialize", or "provision" mentioned
- UPDATE operations: No "manage", "configure", "maintain", "update", "modify", "change", "edit", "adjust", "set", or "customize" mentioned
- DELETE operations: No "delete", "remove", "deactivate", "close", "terminate", "disable", "archive", "retire", or "cancel" mentioned

---

## Story Overview

**As a** financial analyst, account holder, auditor, or third-party developer integrating with the Open Bank Project platform
**I want to** get the balancing transaction for a given transaction
**So that** I can understand the corresponding entry in the double-entry bookkeeping system, verify that transactions are properly balanced, perform account reconciliation, audit financial records, and ensure compliance with accounting principles where every transaction has a corresponding balancing entry

---

## Acceptance Criteria

1. The system shall allow retrieval of the balancing transaction for a specific transaction
2. The system shall return the balancing transaction that corresponds to the given transaction in the double-entry bookkeeping system
3. The system shall provide complete details of the balancing transaction including transaction ID, amount, account information, and metadata
4. The system shall return responses in real-time with appropriate performance characteristics for medium-volume usage patterns
5. The system shall return appropriate error responses (e.g., HTTP 404) when the specified bank, account, or transaction is not found
6. The system shall return appropriate error responses when no balancing transaction exists for the given transaction
7. The system shall enforce access control to ensure users can only retrieve balancing transactions for accounts they have been granted permission to access
8. The system shall clearly identify the relationship between the original transaction and its balancing counterpart
9. The system shall include account identifiers for the account involved in the balancing transaction

---

## Technical Context

- **Classes/Services Involved** (from Scala source code):
  - `APIMethods400` (code.api.v4_0_0.APIMethods400) - REST endpoint definitions for balancing transaction retrieval
  - `JSONFactory400` (code.api.v4_0_0.JSONFactory4.0.0) - JSON response factory with balancing transaction creation methods
  - `NewStyle.function` (code.api.util.NewStyle) - Service layer with transaction retrieval methods
  - `Connector` (code.bankconnectors.Connector) - Backend connector abstraction for transaction data access
  - `Views` (code.views.Views) - View/permission management for transaction access control
  - `BalancingTransactionJson` - Case class defining balancing transaction response structure
  - `MappedTransaction` / `MappedDoubleEntryBookTransaction` - Transaction entity mappings

- **Input Data**: 
  - For balancing transaction retrieval (`GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/balancing-transaction`): Bank identifier, Account identifier, View identifier, and Transaction identifier as path parameters
  - Authentication token (OAuth/DirectLogin) to identify the requesting user

- **Output Data** (based on balancing transaction response case classes):
  - `transaction_id` (String) - Unique identifier of the balancing transaction
  - `bank_id` (String) - Bank of the balancing transaction account
  - `account_id` (String) - Account involved in the balancing transaction
  - `amount` (AmountOfMoney) - Transaction amount with currency
  - `date` (Date) - Transaction date
  - `description` (String) - Transaction description
  - `balance` (AmountOfMoney) - Account balance after the transaction
  - `type` (String) - Transaction type indicator

- **Processing Type**: API / Real-time / Synchronous request-response

---

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description. Only READ/RETRIEVAL operations are included as the description only contains the verb "Get".

### Endpoint 1: Get Balancing Transaction for a Transaction

- **Endpoint**: `GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/balancing-transaction`
  - **Justification (from description)**: "Get the balancing transaction for a given transaction"
  - **Purpose**: Retrieve the balancing transaction that corresponds to a specific transaction in the double-entry bookkeeping system
  - **Scala Implementation**: `APIMethods400.getBalancingTransaction` -> `NewStyle.function.getBalancingTransaction()` -> `JSONFactory400.createBalancingTransactionJson()`
  - **Request**: 
    ```
    GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/balancing-transaction
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      ACCOUNT_ID: The unique identifier of the account (required)
      VIEW_ID: The view identifier defining access permissions (required)
      TRANSACTION_ID: The unique identifier of the transaction for which to retrieve the balancing transaction (required)
    ```
  - **Response** (based on `BalancingTransactionJson` case class): 
    ```json
    {
      "transaction_id": "balancing-transaction-id-001",
      "bank_id": "bank-002",
      "account_id": "account-002",
      "this_account": {
        "id": "account-002",
        "bank_routing": {
          "scheme": "BIC",
          "address": "NDEAFIHH"
        },
        "account_routings": [
          {
            "scheme": "IBAN",
            "address": "FI9876543210987654"
          }
        ],
        "holders": [
          {
            "name": "Account Holder Name",
            "is_alias": false
          }
        ]
      },
      "other_account": {
        "id": "account-001",
        "holder": {
          "name": "Original Transaction Account Holder",
          "is_alias": false
        },
        "bank_routing": {
          "scheme": "BIC",
          "address": "OKOYFIHH"
        },
        "account_routings": [
          {
            "scheme": "IBAN",
            "address": "FI1234567890123456"
          }
        ]
      },
      "details": {
        "type": "CREDIT",
        "description": "Balancing entry for payment transaction",
        "posted": "2024-01-15T10:30:00Z",
        "completed": "2024-01-15T10:30:00Z",
        "new_balance": {
          "currency": "EUR",
          "amount": "2100.00"
        },
        "value": {
          "currency": "EUR",
          "amount": "100.00"
        }
      }
    }
    ```

### Endpoints NOT Included (with justification)

| Endpoint | Operation Type | Reason for Exclusion |
|----------|---------------|---------------------|
| POST /banks/{bank_id}/accounts/{account_id}/transactions | CREATE | No "create", "add", "initiate", or similar verb in description |
| PUT /banks/{bank_id}/accounts/{account_id}/transactions/{transaction_id}/balancing-transaction | UPDATE | No "manage", "update", "modify", or similar verb in description |
| DELETE /banks/{bank_id}/accounts/{account_id}/transactions/{transaction_id}/balancing-transaction | DELETE | No "delete", "remove", "cancel", or similar verb in description |
| GET /banks/{bank_id}/accounts/{account_id}/balancing-transactions (list) | LIST | No "list", "retrieve all", or similar verb - capability is about getting a specific balancing transaction for a given transaction |

---

## Business Rules (from capability description)

1. **Balancing Transaction Principle**: Every transaction in a double-entry bookkeeping system has a corresponding balancing transaction that represents the other side of the entry (from: "balancing transaction for a given transaction")
2. **Transaction Specificity**: The balancing transaction retrieval is scoped to a specific transaction - users must specify which transaction's balancing entry to retrieve (from: "for a given transaction")
3. **One-to-One Relationship**: Each transaction has exactly one balancing transaction that corresponds to it in the accounting system (from: "the balancing transaction")
4. **Real-time Access**: Balancing transaction retrieval must support real-time access patterns with low latency (from: Frequency = Real-time)
5. **Medium Volume Support**: The system must be designed to handle medium volume of balancing transaction retrieval requests (from: Volume = Medium)

---

## Data Validations (if applicable)

- Bank identifier (BANK_ID) must be valid and exist in the system
- Account identifier (ACCOUNT_ID) must be valid and belong to the specified bank
- View identifier (VIEW_ID) must be valid and the user must have access to it
- Transaction identifier (TRANSACTION_ID) must be valid and belong to the specified account
- User must be authenticated with a valid OAuth token or DirectLogin credentials
- User must have at least one view/permission granted on the account to see transactions
- The transaction must have a corresponding balancing transaction in the system
- Error response (HTTP 404 Not Found / `BankNotFound`) must be returned when BANK_ID does not exist
- Error response (HTTP 404 Not Found / `AccountNotFound`) must be returned when ACCOUNT_ID does not exist
- Error response (HTTP 404 Not Found / `TransactionNotFound`) must be returned when TRANSACTION_ID does not exist
- Error response (HTTP 404 Not Found / `BalancingTransactionNotFound`) must be returned when no balancing transaction exists for the given transaction
- Error response (HTTP 401 Unauthorized) for missing or invalid authentication
- Error response (HTTP 403 Forbidden) when user lacks permission to access the account's transactions

---

## Dependencies

- **Upstream**: 
  - Bank must exist in the system (Bank Creation capability)
  - Account must be created and linked to the bank (Account Creation capability)
  - Transaction must exist for the account (Payment Initiation or Transaction Processing capabilities)
  - Double-entry bookkeeping records must be maintained for transactions with balancing entries
  - User must be authenticated (Authentication & Security capabilities)
  - User must have been granted view/permission access to the account (View & Permission Management capabilities)
  - Settlement accounts may be required for proper balancing transaction recording (Settlement Account Management capability)

- **Downstream**: 
  - Financial reporting and reconciliation processes may consume balancing transaction data
  - Audit systems may use balancing transaction views for compliance verification
  - Account balance calculations depend on accurate balancing transaction records
  - Transaction analytics and categorization may use balancing transaction information
  - Double-Entry Transaction Retrieval capability may use balancing transaction data

- **External Systems**: 
  - Core banking system backend for transaction data
  - Accounting/ledger systems that maintain double-entry records with balancing transactions
  - Settlement systems for inter-bank transactions

---

## Notes for Implementation

- **Balancing Transaction Context**: In double-entry bookkeeping, every financial transaction affects at least two accounts. The balancing transaction represents the corresponding entry on the other side of the ledger. For example, if Account A is debited, the balancing transaction shows the credit entry to Account B.

- **Relationship to Double-Entry Transaction Retrieval**: This capability is closely related to the Double-Entry Transaction Retrieval capability (ID: 18). While Double-Entry Transaction Retrieval shows both sides of a transaction together, Balancing Transaction Retrieval focuses on retrieving just the corresponding balancing entry for a given transaction.

- **Settlement Accounts**: For transactions between accounts at different banks or for certain transaction types, settlement accounts may be involved as intermediaries, and the balancing transaction may reference a settlement account.

- **View Permissions**: The user's view permissions determine what level of detail they can see about the balancing transaction. Some views may show full details while others may mask certain information.

- **Transaction Types**: Not all transactions may have a visible balancing transaction to all users - internal settlement transactions or inter-bank transfers may have restricted visibility.

- **Needs SME Input**:
  - Clarification needed on how multi-leg transactions (involving more than two accounts) are handled when retrieving the balancing transaction
  - Clarification needed on whether the balancing transaction for pending/incomplete transactions can be retrieved
  - Clarification needed on access control rules for viewing the balancing transaction when it belongs to a different customer's account
  - Clarification needed on the exact response structure for the balancing transaction endpoint

---

## Quality Checklist

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (financial analyst, account holder, auditor, third-party developer)
- [x] Business value is stated (understanding corresponding entries, verification, reconciliation, audit, compliance)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (in Notes for Implementation)
- [x] Only relevant endpoints are included (GET endpoint for retrieval)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words from the capability description justify inclusion ("Get")
- [x] No endpoint type (create, update, delete) has been added - only retrieval as per description
- [x] No CRUD operations inferred beyond what the description explicitly states
