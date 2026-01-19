# User Story for Double-Entry Transaction Retrieval

## Capability Input

- **Name**: Double-Entry Transaction Retrieval
- **Description**: View double-entry bookkeeping transactions showing debit and credit sides
- **Frequency**: Real-time
- **Volume**: Medium

---

## Operation Verb Analysis

Based on the capability description, the following operation verbs are identified:

| Verb Found | Operation Type | Justification |
|------------|---------------|---------------|
| "View" | READ/RETRIEVAL | Explicitly stated: "View double-entry bookkeeping transactions" |
| "showing" | READ/RETRIEVAL | Implies displaying/presenting data: "showing debit and credit sides" |

**Operations NOT included** (verbs not present in description):
- CREATE operations: No "create", "register", "onboard", "set up", "add", "establish", "initialize", or "provision" mentioned
- UPDATE operations: No "manage", "configure", "maintain", "update", "modify", "change", "edit", "adjust", "set", or "customize" mentioned
- DELETE operations: No "delete", "remove", "deactivate", "close", "terminate", "disable", "archive", "retire", or "cancel" mentioned

---

## Story Overview

**As a** financial analyst, auditor, account holder, or third-party developer integrating with the Open Bank Project platform
**I want to** view double-entry bookkeeping transactions showing debit and credit sides
**So that** I can understand the complete financial picture of transactions, verify accounting accuracy, perform reconciliation between accounts, audit financial records, and ensure compliance with double-entry bookkeeping principles where every transaction has corresponding debit and credit entries

---

## Acceptance Criteria

1. The system shall allow viewing of double-entry bookkeeping transactions for a specific transaction
2. The system shall display both the debit side and credit side of each transaction
3. The system shall show the relationship between the two sides of the double-entry transaction
4. The system shall return the transaction details including amounts, accounts involved, and transaction metadata for both sides
5. The system shall ensure that the debit and credit amounts balance (fundamental double-entry principle)
6. The system shall return responses in real-time with appropriate performance characteristics for medium-volume usage patterns
7. The system shall return appropriate error responses (e.g., HTTP 404) when the specified bank, account, or transaction is not found
8. The system shall enforce access control to ensure users can only view double-entry transactions for accounts they have been granted permission to access
9. The system shall clearly identify which side is the debit entry and which side is the credit entry
10. The system shall include account identifiers for both the source (debit) and destination (credit) accounts

---

## Technical Context

- **Classes/Services Involved** (from Scala source code):
  - `APIMethods400` (code.api.v4_0_0.APIMethods400) - REST endpoint definitions for double-entry transaction retrieval
  - `JSONFactory400` (code.api.v4_0_0.JSONFactory4.0.0) - JSON response factory with double-entry transaction creation methods
  - `NewStyle.function` (code.api.util.NewStyle) - Service layer with transaction retrieval methods
  - `Connector` (code.bankconnectors.Connector) - Backend connector abstraction for transaction data access
  - `Views` (code.views.Views) - View/permission management for transaction access control
  - `DoubleEntryTransactionJson` - Case class defining double-entry transaction response structure
  - `MappedTransaction` / `MappedDoubleEntryBookTransaction` - Transaction entity mappings

- **Input Data**: 
  - For double-entry transaction retrieval (`GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/double-entry-transaction`): Bank identifier, Account identifier, View identifier, and Transaction identifier as path parameters
  - Authentication token (OAuth/DirectLogin) to identify the requesting user

- **Output Data** (based on double-entry transaction response case classes):
  - `transaction_id` (String) - Unique transaction identifier
  - `debit_transaction` (TransactionJson) - The debit side of the transaction including:
    - `account_id` (String) - Account being debited
    - `bank_id` (String) - Bank of the debited account
    - `amount` (AmountOfMoney) - Debit amount with currency
    - `date` (Date) - Transaction date
    - `description` (String) - Transaction description
  - `credit_transaction` (TransactionJson) - The credit side of the transaction including:
    - `account_id` (String) - Account being credited
    - `bank_id` (String) - Bank of the credited account
    - `amount` (AmountOfMoney) - Credit amount with currency
    - `date` (Date) - Transaction date
    - `description` (String) - Transaction description

- **Processing Type**: API / Real-time / Synchronous request-response

---

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description. Only READ/RETRIEVAL operations are included as the description only contains the verb "View" and "showing".

### Endpoint 1: Get Double-Entry Transaction for a Transaction

- **Endpoint**: `GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/double-entry-transaction`
  - **Justification (from description)**: "View double-entry bookkeeping transactions showing debit and credit sides"
  - **Purpose**: Retrieve the double-entry bookkeeping representation of a specific transaction, showing both the debit and credit sides
  - **Scala Implementation**: `APIMethods400.getDoubleEntryTransaction` -> `NewStyle.function.getDoubleEntryTransaction()` -> `JSONFactory400.createDoubleEntryTransactionJson()`
  - **Request**: 
    ```
    GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/double-entry-transaction
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      ACCOUNT_ID: The unique identifier of the account (required)
      VIEW_ID: The view identifier defining access permissions (required)
      TRANSACTION_ID: The unique identifier of the transaction (required)
    ```
  - **Response** (based on `DoubleEntryTransactionJson` case class): 
    ```json
    {
      "transaction_id": "transaction-id-001",
      "debit_transaction": {
        "bank_id": "bank-001",
        "account_id": "account-001",
        "transaction_id": "transaction-id-001-debit",
        "amount": {
          "currency": "EUR",
          "amount": "100.00"
        },
        "date": "2024-01-15T10:30:00Z",
        "description": "Payment to supplier",
        "balance": {
          "currency": "EUR",
          "amount": "1400.00"
        }
      },
      "credit_transaction": {
        "bank_id": "bank-002",
        "account_id": "account-002",
        "transaction_id": "transaction-id-001-credit",
        "amount": {
          "currency": "EUR",
          "amount": "100.00"
        },
        "date": "2024-01-15T10:30:00Z",
        "description": "Payment received from customer",
        "balance": {
          "currency": "EUR",
          "amount": "2100.00"
        }
      }
    }
    ```

### Endpoint 2: Get Other Account of Transaction (Supporting Double-Entry View)

- **Endpoint**: `GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/other_account`
  - **Justification (from description)**: "showing debit and credit sides" - the other account represents the counterpart in the double-entry
  - **Purpose**: Retrieve information about the other account involved in the transaction, which represents the other side of the double-entry
  - **Scala Implementation**: `APIMethods400.getOtherAccountForTransaction` -> `NewStyle.function.getOtherAccount()` -> `JSONFactory400.createOtherAccountJson()`
  - **Request**: 
    ```
    GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/other_account
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      ACCOUNT_ID: The unique identifier of the account (required)
      VIEW_ID: The view identifier (required)
      TRANSACTION_ID: The unique identifier of the transaction (required)
    ```
  - **Response**: 
    ```json
    {
      "id": "counterparty-account-001",
      "holder": {
        "name": "Supplier Company Ltd",
        "is_alias": false
      },
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
      "metadata": {
        "public_alias": "Supplier",
        "private_alias": "Main Supplier",
        "more_info": "Primary supplier for office supplies",
        "url": "https://supplier.example.com",
        "image_url": "https://supplier.example.com/logo.png",
        "open_corporates_url": "",
        "corporate_location": null,
        "physical_location": null
      }
    }
    ```

### Endpoints NOT Included (with justification)

| Endpoint | Operation Type | Reason for Exclusion |
|----------|---------------|---------------------|
| POST /banks/{bank_id}/accounts/{account_id}/transactions | CREATE | No "create", "add", "initiate", or similar verb in description |
| PUT /banks/{bank_id}/accounts/{account_id}/transactions/{transaction_id}/double-entry | UPDATE | No "manage", "update", "modify", or similar verb in description |
| DELETE /banks/{bank_id}/accounts/{account_id}/transactions/{transaction_id}/double-entry | DELETE | No "delete", "remove", "cancel", or similar verb in description |
| GET /banks/{bank_id}/accounts/{account_id}/transactions (list) | LIST | No "list", "retrieve history", or similar verb - capability is about viewing specific double-entry transactions, not listing |

---

## Business Rules (from capability description)

1. **Double-Entry Principle**: Every transaction must have both a debit side and a credit side - this is the fundamental principle of double-entry bookkeeping (from: "double-entry bookkeeping transactions")
2. **Balance Requirement**: The debit amount must equal the credit amount for each transaction - debits must equal credits (from: "double-entry bookkeeping")
3. **Two-Sided View**: The system must show both sides of the transaction - the account being debited and the account being credited (from: "showing debit and credit sides")
4. **Transaction Scope**: Double-entry view is scoped to a specific transaction - users must specify which transaction's double-entry representation to view (from: "View double-entry bookkeeping transactions")
5. **Real-time Access**: Double-entry transaction retrieval must support real-time access patterns with low latency (from: Frequency = Real-time)
6. **Medium Volume Support**: The system must be designed to handle medium volume of double-entry transaction retrieval requests (from: Volume = Medium)

---

## Data Validations (if applicable)

- Bank identifier (BANK_ID) must be valid and exist in the system
- Account identifier (ACCOUNT_ID) must be valid and belong to the specified bank
- View identifier (VIEW_ID) must be valid and the user must have access to it
- Transaction identifier (TRANSACTION_ID) must be valid and belong to the specified account
- User must be authenticated with a valid OAuth token or DirectLogin credentials
- User must have at least one view/permission granted on the account to see transactions
- The transaction must have a corresponding double-entry representation in the system
- Error response (HTTP 404 Not Found / `BankNotFound`) must be returned when BANK_ID does not exist
- Error response (HTTP 404 Not Found / `AccountNotFound`) must be returned when ACCOUNT_ID does not exist
- Error response (HTTP 404 Not Found / `TransactionNotFound`) must be returned when TRANSACTION_ID does not exist
- Error response (HTTP 404 Not Found / `DoubleEntryTransactionNotFound`) must be returned when no double-entry representation exists for the transaction
- Error response (HTTP 401 Unauthorized) for missing or invalid authentication
- Error response (HTTP 403 Forbidden) when user lacks permission to access the account's transactions
- All monetary values must include currency code and properly formatted amount
- Debit and credit amounts must be equal (validation of double-entry integrity)

---

## Dependencies

- **Upstream**: 
  - Bank must exist in the system (Bank Creation capability)
  - Account must be created and linked to the bank (Account Creation capability)
  - Transaction must exist for the account (Payment Initiation or Transaction Processing capabilities)
  - Double-entry bookkeeping records must be maintained for transactions
  - User must be authenticated (Authentication & Security capabilities)
  - User must have been granted view/permission access to the account (View & Permission Management capabilities)
  - Settlement accounts may be required for proper double-entry recording (Settlement Account Management capability)

- **Downstream**: 
  - Financial reporting and reconciliation processes may consume double-entry transaction data
  - Audit systems may use double-entry views for compliance verification
  - Account balance calculations depend on accurate double-entry records
  - Transaction analytics and categorization may use double-entry information

- **External Systems**: 
  - Core banking system backend for transaction data
  - Accounting/ledger systems that maintain double-entry records
  - Settlement systems for inter-bank transactions

---

## Notes for Implementation

- **Double-Entry Bookkeeping Context**: In double-entry bookkeeping, every financial transaction affects at least two accounts - one account is debited (money flows out or liability increases) and another is credited (money flows in or asset increases). This capability exposes this fundamental accounting view of transactions.

- **Settlement Accounts**: For transactions between accounts at different banks or for certain transaction types, settlement accounts may be involved as intermediaries in the double-entry chain.

- **View Permissions**: The user's view permissions determine what level of detail they can see about the double-entry transaction. Some views may show full details while others may mask certain information.

- **Transaction Types**: Not all transactions may have a visible double-entry representation to all users - internal settlement transactions or inter-bank transfers may have restricted visibility.

- **Currency Handling**: For cross-currency transactions, the double-entry view should show the amounts in their respective currencies, and any currency conversion should be clearly indicated.

- **Needs SME Input**:
  - Clarification needed on how multi-leg transactions (involving more than two accounts) are represented in the double-entry view
  - Clarification needed on how pending/incomplete transactions are shown in double-entry format
  - Clarification needed on whether historical exchange rates are preserved for cross-currency double-entry views
  - Clarification needed on access control rules for viewing the "other side" of a transaction when it belongs to a different customer

---

## Quality Checklist

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (financial analyst, auditor, account holder, third-party developer)
- [x] Business value is stated (understanding complete financial picture, verification, reconciliation, audit, compliance)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (in Notes for Implementation)
- [x] Only relevant endpoints are included (GET endpoints for viewing)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words from the capability description justify inclusion ("View", "showing")
- [x] No endpoint type (create, update, delete) has been added - only view/retrieval as per description
- [x] No CRUD operations inferred beyond what the description explicitly states
