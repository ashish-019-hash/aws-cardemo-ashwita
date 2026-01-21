# User Story for Transaction Details

## Story Overview

**As a** bank customer or third-party application user  
**I want to** get detailed information about a specific transaction  
**So that** I can review transaction specifics, verify transaction accuracy, reconcile my accounts, and understand the full context of my financial activities

## Acceptance Criteria

1. Given a valid bank ID, account ID, view ID, and transaction ID, the system shall retrieve and return detailed information about the specific transaction
2. The transaction details shall include transaction amount, currency, date/time, and counterparty information
3. The system shall validate that the user has appropriate view permissions to access the transaction details
4. The system shall return an appropriate error response if the transaction ID does not exist
5. The system shall return an appropriate error response if the user lacks permission to view the transaction
6. The response shall include all relevant transaction metadata such as description, type, and status

## Technical Context

- **Classes/Services Involved**: Transaction service, Account service, View authorization service
- **Input Data**: Bank ID, Account ID, View ID, Transaction ID (path parameters); Authentication token (header)
- **Output Data**: Transaction details JSON object containing amount, currency, date, counterparty details, description, type, and metadata
- **Processing Type**: API (Real-time HTTP request-response)

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description.

### Endpoint 1: Get Transaction Details by ID

- **Endpoint**: `GET /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/transactions/{TRANSACTION_ID}/transaction`
  - **Justification (from description)**: "Get detailed information about a specific transaction" - the word "Get" justifies a GET endpoint for retrieval
  - **Purpose**: Retrieve comprehensive details about a single transaction identified by its transaction ID
  - **Request**: 
    - Path Parameters:
      - `BANK_ID` (string, required): The bank identifier
      - `ACCOUNT_ID` (string, required): The account identifier
      - `VIEW_ID` (string, required): The view identifier determining field visibility
      - `TRANSACTION_ID` (string, required): The unique transaction identifier
    - Headers:
      - `Authorization` (string, required): OAuth token or Direct Login token
  - **Response**: 
    ```json
    {
      "id": "string",
      "this_account": {
        "id": "string",
        "bank_routing": {
          "scheme": "string",
          "address": "string"
        },
        "account_routings": [
          {
            "scheme": "string",
            "address": "string"
          }
        ],
        "holders": [
          {
            "name": "string",
            "is_alias": "boolean"
          }
        ]
      },
      "other_account": {
        "id": "string",
        "holder": {
          "name": "string",
          "is_alias": "boolean"
        },
        "bank_routing": {
          "scheme": "string",
          "address": "string"
        },
        "account_routings": [
          {
            "scheme": "string",
            "address": "string"
          }
        ],
        "metadata": {
          "public_alias": "string",
          "private_alias": "string",
          "more_info": "string",
          "url": "string",
          "image_url": "string"
        }
      },
      "details": {
        "type": "string",
        "description": "string",
        "posted": "date",
        "completed": "date",
        "new_balance": {
          "currency": "string",
          "amount": "string"
        },
        "value": {
          "currency": "string",
          "amount": "string"
        }
      },
      "metadata": {
        "narrative": "string",
        "comments": [],
        "tags": [],
        "images": [],
        "where": {}
      }
    }
    ```

## Business Rules

1. **View-Based Access Control**: Transaction details visibility is controlled by the view permissions. Different views may expose different fields of the transaction.
2. **Authentication Required**: All requests must include valid authentication credentials (OAuth or Direct Login token).
3. **Bank-Account-Transaction Hierarchy**: The transaction must belong to the specified account, which must belong to the specified bank.
4. **Real-time Data**: Transaction details are retrieved in real-time from the underlying banking system.
5. **Counterparty Information**: The response includes details about both the account holder's account (this_account) and the counterparty's account (other_account).

## Data Validations

- **Transaction ID Validation**: The transaction ID must be a valid, non-empty string that exists in the system
- **Bank ID Validation**: The bank ID must correspond to a valid bank on the platform
- **Account ID Validation**: The account ID must be a valid account belonging to the specified bank
- **View ID Validation**: The view ID must be a valid view that the user has access to for the specified account
- **Authorization Validation**: The user must have appropriate permissions through the specified view to access transaction details
- **Error Conditions**:
  - 400 Bad Request: Invalid parameters provided
  - 401 Unauthorized: Missing or invalid authentication
  - 403 Forbidden: User lacks permission to access the transaction through the specified view
  - 404 Not Found: Transaction, account, or bank does not exist

## Dependencies

- **Upstream**: 
  - User must be authenticated (OAuth 2.0 / OpenID Connect or Direct Login)
  - User must have been granted access to a view on the account
  - Transaction must exist in the system
- **Downstream**: 
  - Transaction details can be used for reconciliation, reporting, and audit purposes
  - Metadata from transaction details may be used in other operations
- **External Systems**: 
  - Core banking system connector for retrieving transaction data
  - Authentication provider for validating user credentials

## Notes for Implementation

- **View Permissions**: The implementation must respect view-level permissions. Some views may hide certain fields (e.g., counterparty details, balance information).
- **Performance Consideration**: This is a high-volume, real-time endpoint. Caching strategies may be beneficial for frequently accessed transactions.
- **Audit Trail**: Consider logging access to transaction details for compliance and audit purposes.
- **Currency Handling**: Amounts should be returned with proper currency codes and precision.
- **Date/Time Format**: All dates should be returned in ISO 8601 format.
- **Needs SME Input**: Clarification needed on whether historical transactions have different retention policies or access patterns.
