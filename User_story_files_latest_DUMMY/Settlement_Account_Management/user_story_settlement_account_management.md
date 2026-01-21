# User Story for Settlement Account Management

## Story Overview

**As a** Bank Administrator or Financial Operations Manager  
**I want to** create and manage settlement accounts for double-entry bookkeeping and payment processing  
**So that** the bank can maintain accurate financial records through proper double-entry accounting and facilitate seamless payment processing operations

## Acceptance Criteria

1. The system shall allow authorized users to create new settlement accounts with required parameters for double-entry bookkeeping
2. The system shall allow authorized users to manage (update/configure) existing settlement accounts
3. Settlement accounts shall support double-entry bookkeeping requirements (debit and credit entries)
4. Settlement accounts shall be usable for payment processing operations
5. The system shall validate all required fields before creating or updating a settlement account
6. The system shall return appropriate success/error responses for all operations
7. Only users with appropriate entitlements shall be able to create or manage settlement accounts

## Technical Context

- **Classes/Services Involved**: 
  - Settlement Account Service (handles creation and management of settlement accounts)
  - Double-Entry Bookkeeping Service (ensures proper accounting entries)
  - Payment Processing Service (uses settlement accounts for payment operations)
  - Authorization Service (validates user entitlements)

- **Input Data**: 
  - Settlement account creation: Bank ID, account type, currency, initial configuration, account holder information
  - Settlement account management: Account ID, updated configuration parameters, status changes

- **Output Data**: 
  - Created/updated settlement account details
  - Account identifiers for reference in transactions
  - Operation status and any validation errors

- **Processing Type**: API/Real-time (On-demand)

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description.

### Endpoint 1: Create Settlement Account

- **Endpoint**: `POST /obp/v4.0.0/banks/{BANK_ID}/settlement-accounts`
  - **Justification (from description)**: "Create" - explicitly mentioned in "Create and manage settlement accounts"
  - **Purpose**: Create a new settlement account for double-entry bookkeeping and payment processing
  - **Request**: 
    ```json
    {
      "account_type": "string",
      "currency": "string",
      "label": "string",
      "balance": {
        "amount": "string",
        "currency": "string"
      },
      "branch_id": "string",
      "user_id": "string",
      "account_routings": [
        {
          "scheme": "string",
          "address": "string"
        }
      ]
    }
    ```
  - **Response**: 
    ```json
    {
      "account_id": "string",
      "bank_id": "string",
      "account_type": "string",
      "currency": "string",
      "label": "string",
      "balance": {
        "amount": "string",
        "currency": "string"
      },
      "account_routings": [
        {
          "scheme": "string",
          "address": "string"
        }
      ],
      "created_at": "datetime"
    }
    ```

### Endpoint 2: Update Settlement Account

- **Endpoint**: `PUT /obp/v4.0.0/banks/{BANK_ID}/settlement-accounts/{ACCOUNT_ID}`
  - **Justification (from description)**: "manage" - explicitly mentioned in "Create and manage settlement accounts" (manage = update/configure operations per Operation Derivation Rules)
  - **Purpose**: Update/configure an existing settlement account's parameters
  - **Request**: 
    ```json
    {
      "label": "string",
      "account_type": "string",
      "branch_id": "string",
      "account_routings": [
        {
          "scheme": "string",
          "address": "string"
        }
      ]
    }
    ```
  - **Response**: 
    ```json
    {
      "account_id": "string",
      "bank_id": "string",
      "account_type": "string",
      "currency": "string",
      "label": "string",
      "balance": {
        "amount": "string",
        "currency": "string"
      },
      "account_routings": [
        {
          "scheme": "string",
          "address": "string"
        }
      ],
      "updated_at": "datetime"
    }
    ```

### Endpoint 3: Configure Settlement Account Settings

- **Endpoint**: `PATCH /obp/v4.0.0/banks/{BANK_ID}/settlement-accounts/{ACCOUNT_ID}/settings`
  - **Justification (from description)**: "manage" - explicitly mentioned in "Create and manage settlement accounts" (manage = configure operations per Operation Derivation Rules)
  - **Purpose**: Configure specific settings for a settlement account (partial update)
  - **Request**: 
    ```json
    {
      "payment_processing_enabled": "boolean",
      "double_entry_bookkeeping_config": {
        "default_debit_account": "string",
        "default_credit_account": "string"
      },
      "status": "string"
    }
    ```
  - **Response**: 
    ```json
    {
      "account_id": "string",
      "settings": {
        "payment_processing_enabled": "boolean",
        "double_entry_bookkeeping_config": {
          "default_debit_account": "string",
          "default_credit_account": "string"
        },
        "status": "string"
      },
      "updated_at": "datetime"
    }
    ```

## Endpoints NOT Included (with justification)

The following endpoint types are **NOT** included because they are not explicitly mentioned in the capability description:

- **GET /settlement-accounts** - NOT included because "list", "retrieve", "view", "search" are not mentioned in the description
- **GET /settlement-accounts/{id}** - NOT included because "view", "retrieve", "get" are not mentioned in the description
- **DELETE /settlement-accounts/{id}** - NOT included because "delete", "remove", "deactivate", "close" are not mentioned in the description

Per the Operation Derivation Rules, "manage" is interpreted narrowly as update/configure operations only.

## Business Rules (from capability description)

1. **Double-Entry Bookkeeping Compliance**: Settlement accounts must support double-entry bookkeeping principles where every transaction has corresponding debit and credit entries
2. **Payment Processing Integration**: Settlement accounts must be configured to facilitate payment processing operations
3. **Bank Association**: Each settlement account must be associated with a specific bank entity
4. **Currency Specification**: Settlement accounts must have a defined currency for proper financial tracking
5. **Authorization Required**: Only authorized users with appropriate entitlements can create or manage settlement accounts

## Data Validations (if applicable)

- Bank ID must be valid and exist in the system
- Currency must be a valid ISO 4217 currency code
- Account type must be a valid settlement account type
- Account routings must follow valid scheme formats (e.g., IBAN, SWIFT)
- Balance amounts must be valid numeric values
- User must have appropriate entitlements (e.g., CanCreateSettlementAccount, CanManageSettlementAccount)

## Dependencies

- **Upstream**: 
  - Bank entity must exist before creating settlement accounts
  - User authentication and authorization must be completed
  - Entitlement verification for settlement account operations

- **Downstream**: 
  - Payment processing operations can use created settlement accounts
  - Double-entry bookkeeping transactions reference settlement accounts
  - Transaction records are created when payments flow through settlement accounts

- **External Systems**: 
  - Core banking system connector for account synchronization
  - Payment processing gateway integration
  - Accounting/ledger system for double-entry bookkeeping

## Notes for Implementation

1. **Security Considerations**: Settlement accounts handle financial operations and require strict access control. Implement proper entitlement checks before allowing create/manage operations.

2. **Audit Trail**: All create and manage operations on settlement accounts should be logged for audit purposes.

3. **Idempotency**: Consider implementing idempotency keys for account creation to prevent duplicate accounts.

4. **Validation**: Implement comprehensive validation for all input fields, especially for financial data like currency and balance amounts.

5. **Double-Entry Integration**: Ensure the settlement account structure supports linking to debit and credit sides of transactions.

6. **Needs SME Input**:
   - Specific settlement account types supported by the platform
   - Required fields vs optional fields for settlement account creation
   - Business rules for settlement account status transitions
   - Integration requirements with specific payment processing systems

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Bank Administrator/Financial Operations Manager)
- [x] Business value is stated (accurate financial records, seamless payment processing)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (Needs SME Input section)
- [x] Only relevant endpoints are included (Create and Manage only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words from the capability description justify inclusion
- [x] No endpoint type (create, update, view, list, delete) has been added unless its verb appears in the description
- [x] "Manage" has been interpreted narrowly as update/configure only - view/list/delete operations are NOT included
