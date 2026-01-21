# User Story for Counterparty Limit Management

## Story Overview

**As a** bank administrator or account manager  
**I want to** set and manage transaction limits for counterparties  
**So that** I can control and regulate the maximum transaction amounts allowed with specific counterparties, ensuring risk management and compliance with internal policies

## Acceptance Criteria

1. The system shall allow setting new transaction limits for a specific counterparty
2. The system shall allow managing (updating/modifying) existing transaction limits for counterparties
3. Transaction limits shall be associated with a specific bank, account, and counterparty combination
4. The system shall validate that the counterparty exists before setting or managing limits
5. The system shall return appropriate error responses for invalid requests or unauthorized access
6. All limit management operations shall require user authentication
7. Limit values shall be validated to ensure they are positive numeric values
8. The system shall support different limit types (e.g., daily, monthly, per-transaction)

## Technical Context

- **Classes/Services Involved**: 
  - CounterpartyLimit (data model for limit records)
  - CounterpartyLimitProvider (provider trait for limit operations)
  - MappedCounterpartyLimitProvider (implementation of limit operations)
  - CounterpartyLimitNewStyle (new style API helper)
  - JSONFactory (JSON serialization/deserialization)
- **Input Data**: 
  - Bank identifier (BANK_ID)
  - Account identifier (ACCOUNT_ID)
  - Counterparty identifier (COUNTERPARTY_ID)
  - Limit type (string - e.g., "DAILY", "MONTHLY", "PER_TRANSACTION")
  - Limit amount (decimal/string representation)
  - Currency code (optional)
  - Authentication credentials/token
- **Output Data**: 
  - Limit record information including:
    - bank_id: Bank identifier
    - account_id: Account identifier
    - counterparty_id: Counterparty identifier
    - limit_id: Unique limit identifier
    - limit_type: Type of limit
    - limit_amount: Maximum transaction amount
    - currency: Currency code
- **Processing Type**: On-demand API request-response

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description.

### Endpoint 1: Set Counterparty Transaction Limit
- **Endpoint**: `POST /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/counterparties/{COUNTERPARTY_ID}/limits`
  - **Justification (from description)**: "Set" transaction limits for counterparties
  - **Purpose**: Create/set a new transaction limit for a specific counterparty
  - **Request**: 
    - Path Parameters:
      - `BANK_ID` (string, required): The identifier of the bank
      - `ACCOUNT_ID` (string, required): The identifier of the account
      - `COUNTERPARTY_ID` (string, required): The identifier of the counterparty
    - Headers:
      - `Authorization`: Bearer token or OAuth credentials
    - Body:
      ```json
      {
        "limit_type": "string",
        "limit_amount": "string",
        "currency": "string"
      }
      ```
  - **Response**: 
    ```json
    {
      "bank_id": "string",
      "account_id": "string",
      "counterparty_id": "string",
      "limit_id": "string",
      "limit_type": "string",
      "limit_amount": "string",
      "currency": "string"
    }
    ```
  - **HTTP Status**: 201 Created
  - **Required Role**: canSetCounterpartyLimit

### Endpoint 2: Manage (Update) Counterparty Transaction Limit
- **Endpoint**: `PUT /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/counterparties/{COUNTERPARTY_ID}/limits/{LIMIT_ID}`
  - **Justification (from description)**: "manage" transaction limits for counterparties
  - **Purpose**: Update/manage an existing transaction limit for a counterparty
  - **Request**: 
    - Path Parameters:
      - `BANK_ID` (string, required): The identifier of the bank
      - `ACCOUNT_ID` (string, required): The identifier of the account
      - `COUNTERPARTY_ID` (string, required): The identifier of the counterparty
      - `LIMIT_ID` (string, required): The identifier of the limit record to update
    - Headers:
      - `Authorization`: Bearer token or OAuth credentials
    - Body:
      ```json
      {
        "limit_type": "string",
        "limit_amount": "string",
        "currency": "string"
      }
      ```
  - **Response**: 
    ```json
    {
      "bank_id": "string",
      "account_id": "string",
      "counterparty_id": "string",
      "limit_id": "string",
      "limit_type": "string",
      "limit_amount": "string",
      "currency": "string"
    }
    ```
  - **HTTP Status**: 200 OK
  - **Required Role**: canManageCounterpartyLimit

## Business Rules (from capability description)

1. Transaction limits are managed on-demand as indicated by the "On-demand" frequency classification
2. The capability supports low volume of operations as indicated by the "Low" volume classification
3. Each limit record must be uniquely identified by a limit ID
4. Limits are associated with a specific bank, account, and counterparty combination
5. Limit amounts define the maximum transaction value allowed with a counterparty
6. The counterparty must exist in the system before limits can be set for it
7. Only authorized users with appropriate roles can perform limit management operations
8. Limits may be enforced during payment initiation to counterparties

## Data Validations (if applicable)

- Bank ID must be a valid identifier for a bank on the platform
- Account ID must be a valid identifier for an account at the specified bank
- Counterparty ID must be a valid identifier for an existing counterparty
- Limit ID must be a valid identifier for an existing limit record (for update operations)
- Limit type must be a non-empty string representing a valid limit category
- Limit amount must be a valid positive numeric value that can be parsed as BigDecimal
- Currency must be a valid ISO 4217 currency code (if provided)
- User must have appropriate permissions/entitlements (canSetCounterpartyLimit, canManageCounterpartyLimit)
- Authentication token must be valid and not expired

## Dependencies

- **Upstream**: 
  - User authentication must be completed
  - Bank must exist and be active on the platform
  - Account must exist in the system
  - Counterparty must be registered and associated with the account
  - User must have appropriate role-based permissions
- **Downstream**: 
  - Limit data is used during payment initiation to validate transaction amounts
  - Limit data feeds into risk management and compliance reporting
  - Limit enforcement during Counterparty Payment capability execution
- **External Systems**: 
  - Core banking system connector for synchronizing limit data
  - Authentication/authorization service for validating user access and roles
  - Risk management system for limit policy enforcement

## Notes for Implementation

- This capability is classified as "Low" volume, indicating infrequent usage patterns
- On-demand frequency means limit management operations are performed as needed rather than on a scheduled basis
- The implementation should support multiple limit types per counterparty (e.g., daily limit, monthly limit, per-transaction limit)
- Limit amounts should be stored in the smallest currency unit for precision
- Error handling should provide clear messages for common failure scenarios:
  - User not logged in
  - User missing required roles
  - Invalid JSON format
  - Invalid limit amount (not a valid positive number)
  - Limit record not found (for update operations)
  - Counterparty not found
  - Account not found
- The limit ID is auto-generated using UUID when setting new limits
- Consider implementing limit history tracking for audit purposes
- Limits should be checked during payment processing to counterparties (integration with Counterparty Payment capability)

## Operations NOT Included (per Operation Derivation Rules)

The following operations are explicitly NOT included because they are not mentioned in the capability description:

- **GET/Retrieve operations**: No "view", "retrieve", "get", "list", or "search" verbs in the capability description "Set and manage transaction limits for counterparties"
- **LIST operations**: No "list", "browse", or "query" verbs in the description
- **DELETE operations**: No "delete", "remove", "deactivate", or "terminate" verbs in the description

Note: While the system may contain GET or DELETE endpoints for counterparty limits, these are NOT included in this user story because the capability description specifically states "Set and manage" without mentioning retrieval or deletion operations. Retrieval operations would fall under a separate "Counterparty Limit Retrieval" capability, and deletion would require a "Counterparty Limit Deletion" capability.

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (bank administrator or account manager)
- [x] Business value is stated (control transaction amounts, risk management, compliance)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered (set, manage)
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged
- [x] Only relevant endpoints are included (POST for "set", PUT for "manage")
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words from the capability description justify inclusion ("Set", "manage")
- [x] No CRUD operations are inferred beyond what the description explicitly states
- [x] Words like "manage" have been interpreted narrowly as update/configure only - view/list/delete operations are NOT included
