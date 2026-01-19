# User Story for Settlement Account Management

## Story Overview

**As a** Bank Administrator or Financial Operations Manager  
**I want to** create and manage settlement accounts for double-entry bookkeeping and payment processing  
**So that** the bank can properly record and track financial transactions using double-entry accounting principles and facilitate payment processing across different payment systems and currencies

## Acceptance Criteria

1. The system shall allow authorized users to create new settlement accounts for a specific bank
2. The system shall allow authorized users to manage (update/configure) existing settlement account information
3. Settlement accounts shall support double-entry bookkeeping requirements by serving as counterparty accounts for transactions
4. Settlement accounts shall be configurable for different payment systems (e.g., SEPA, CARD, DEFAULT)
5. Settlement accounts shall support multiple currencies for payment processing
6. The settlement account ID shall be automatically generated based on the payment system and currency (e.g., SEPA_SETTLEMENT_ACCOUNT_EUR)
7. When a bank is created, default settlement accounts shall be automatically provisioned (OBP_DEFAULT_INCOMING_ACCOUNT_ID and OBP_DEFAULT_OUTGOING_ACCOUNT_ID)
8. Settlement accounts shall have an initial balance of zero upon creation
9. Settlement accounts shall support account routings for integration with external payment systems

## Technical Context

- **Classes/Services Involved**: Settlement Account Service, Bank Account Service, Account Attribute Service, Bank Service (for bank validation)
- **Input Data**: Settlement account creation/update requests containing user_id, payment_system, balance (must be zero), label, branch_id, and account_routings
- **Output Data**: Settlement account records with account_id, user_id, payment_system, balance, label, branch_id, account_routings, and account_attributes
- **Processing Type**: API (REST endpoints for create and manage operations)

## Relevant Endpoints

**IMPORTANT**: Each endpoint is justified by specific words in the capability description.

### Create Settlement Account

- **Endpoint**: POST /obp/v5.1.0/banks/{BANK_ID}/settlement-accounts
  - **Justification (from description)**: "Create" - explicitly stated in "Create and manage settlement accounts"
  - **Purpose**: Create a new settlement account for a specific bank to support double-entry bookkeeping and payment processing
  - **Request**: 
    ```json
    {
      "user_id": "user-id-123",
      "payment_system": "SEPA",
      "balance": {
        "currency": "EUR",
        "amount": "0"
      },
      "label": "SEPA Settlement Account EUR",
      "branch_id": "branch-001",
      "account_routings": [
        {
          "scheme": "IBAN",
          "address": "DE89370400440532013000"
        }
      ]
    }
    ```
  - **Response**: 
    ```json
    {
      "account_id": "SEPA_SETTLEMENT_ACCOUNT_EUR",
      "user_id": "user-id-123",
      "payment_system": "SEPA",
      "balance": {
        "currency": "EUR",
        "amount": "0"
      },
      "label": "SEPA Settlement Account EUR",
      "branch_id": "branch-001",
      "account_routings": [
        {
          "scheme": "IBAN",
          "address": "DE89370400440532013000"
        }
      ],
      "account_attributes": []
    }
    ```

### Update Settlement Account

- **Endpoint**: PUT /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}
  - **Justification (from description)**: "manage" - explicitly stated in "Create and manage settlement accounts" (manage implies update/configure operations)
  - **Purpose**: Update an existing settlement account's configuration and attributes
  - **Request**: Account update request with modified label, branch_id, or account_routings
  - **Response**: Updated settlement account record with all current details

**Note**: Per the Operation Derivation Rules, the word "manage" by itself means ONLY update/configure/maintain operations. Therefore:
- GET/retrieve/list endpoints are NOT included (no "view", "retrieve", "list", or "get" mentioned in description)
- DELETE endpoints are NOT included (no "delete", "remove", or "deactivate" mentioned in description)

## Business Rules (from capability description)

1. Settlement accounts must be associated with a valid bank entity (BANK_ID must exist)
2. Settlement accounts are used for double-entry bookkeeping - when no OBP account can be found for a counterparty, the double-entry transaction is saved on a settlement account
3. The account ID naming convention follows the pattern: {PAYMENT_SYSTEM}_SETTLEMENT_ACCOUNT_{CURRENCY} (e.g., SEPA_SETTLEMENT_ACCOUNT_EUR)
4. Settlement account lookup priority for transactions:
   - First, look for a settlement account specific to the payment system and currency
   - If not found, look for a default settlement account for the counterparty currency (DEFAULT_SETTLEMENT_ACCOUNT_{CURRENCY})
   - Else, use one of the two OBP default settlement accounts based on transaction direction
5. Initial balance for new settlement accounts MUST be zero
6. Settlement accounts can be owned by a specific user or the creating user if no user_id is specified
7. Default settlement accounts (OBP_DEFAULT_INCOMING_ACCOUNT_ID and OBP_DEFAULT_OUTGOING_ACCOUNT_ID) are automatically created when a bank is created, with EUR as default currency

## Data Validations (if applicable)

- **Bank ID Validation**: The specified BANK_ID must exist in the system
- **Initial Balance Validation**: The balance amount MUST be zero for new settlement accounts
- **Currency Validation**: The currency must be a valid ISO 4217 currency code
- **Payment System Validation**: The payment system must be a recognized value (e.g., SEPA, CARD, DEFAULT)
- **User ID Validation**: If specified, the user_id must correspond to an existing user
- **Account Routing Validation**: Account routings must have valid scheme and address values
- **Authorization Validation**: User must have the CanCreateSettlementAccountAtOneBank role to create settlement accounts

## Dependencies

- **Upstream**: 
  - Bank entity must exist before settlement accounts can be created for that bank
  - User must have appropriate entitlements/permissions (CanCreateSettlementAccountAtOneBank role) to create settlement accounts
  - For account ownership, the specified user must exist in the system
- **Downstream**: 
  - Settlement accounts are used by the transaction processing system for double-entry bookkeeping
  - When transactions are saved through the mapped connector, settlement accounts serve as counterparty accounts when no OBP account is found
  - Payment processing systems use settlement accounts for recording incoming and outgoing payments
- **External Systems**: 
  - Payment systems (SEPA, CARD networks) may reference settlement accounts for transaction recording
  - Accounting and reconciliation systems may consume settlement account data

## Notes for Implementation

- **Double-Entry Bookkeeping**: Settlement accounts are critical for maintaining proper double-entry bookkeeping. Every transaction must have a debit and credit side, and settlement accounts serve as the counterparty when the actual counterparty account is not in the OBP system
- **Payment System Integration**: Different payment systems (SEPA, CARD, etc.) should have their own dedicated settlement accounts to properly categorize and track transactions by payment method
- **Currency Handling**: Banks operating in multiple currencies should create settlement accounts for each currency they support to ensure proper currency segregation
- **Default Account Provisioning**: When implementing bank creation, ensure that the two default settlement accounts (incoming and outgoing) are automatically created with EUR currency
- **Account ID Generation**: The system must automatically generate the account ID based on the payment system and currency pattern to ensure consistency
- **Audit Trail**: Consider logging all changes to settlement accounts for audit and compliance purposes
- **Needs SME Input**: 
  - Clarify the complete list of supported payment systems
  - Determine if settlement accounts should support additional currencies beyond EUR by default
  - Confirm the reconciliation process between settlement accounts and external payment systems
