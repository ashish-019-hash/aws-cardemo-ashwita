# User Story for Counterparty Retrieval

## Story Overview

**As a** banking application user or third-party developer  
**I want to** retrieve counterparty information by ID or name  
**So that** I can access details about registered payment recipients for transaction processing, payment initiation, and account management purposes

## Acceptance Criteria

1. The system shall allow retrieval of counterparty information using a unique counterparty ID
2. The system shall allow retrieval of counterparty information using the counterparty name
3. The system shall return complete counterparty details including identification, account information, and metadata when a valid ID or name is provided
4. The system shall return appropriate error responses when a counterparty is not found
5. The system shall enforce proper authorization before returning counterparty information
6. The system shall support real-time retrieval with high volume capacity as specified in the capability requirements

## Technical Context

- **Classes/Services Involved**: CounterpartyService, CounterpartyRepository, AuthorizationService
- **Input Data**: 
  - Counterparty ID (for ID-based retrieval)
  - Counterparty name (for name-based retrieval)
  - Bank ID (context for the counterparty lookup)
  - Account ID (context for the counterparty lookup)
  - Authentication/Authorization tokens
- **Output Data**: 
  - Counterparty details including: counterparty ID, name, account routing information, bank details, metadata, and creation timestamps
- **Processing Type**: Real-time API (synchronous request-response)

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description.

### Endpoint 1: Get Counterparty by ID
- **Endpoint**: `GET /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views/{VIEW_ID}/counterparties/{COUNTERPARTY_ID}`
  - **Justification (from description)**: "Retrieve counterparty information by ID"
  - **Purpose**: Retrieves detailed information about a specific counterparty using its unique identifier
  - **Request**: 
    - Path Parameters: BANK_ID, ACCOUNT_ID, VIEW_ID, COUNTERPARTY_ID
    - Headers: Authorization token
  - **Response**: 
    ```json
    {
      "counterparty_id": "string",
      "name": "string",
      "description": "string",
      "other_account_routing_scheme": "string",
      "other_account_routing_address": "string",
      "other_account_secondary_routing_scheme": "string",
      "other_account_secondary_routing_address": "string",
      "other_bank_routing_scheme": "string",
      "other_bank_routing_address": "string",
      "other_branch_routing_scheme": "string",
      "other_branch_routing_address": "string",
      "is_beneficiary": "boolean",
      "bespoke": []
    }
    ```

### Endpoint 2: Get Counterparty by Name
- **Endpoint**: `GET /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views/{VIEW_ID}/counterparties`
  - **Justification (from description)**: "Retrieve counterparty information by ... name"
  - **Purpose**: Retrieves counterparty information by searching/filtering using the counterparty name
  - **Request**: 
    - Path Parameters: BANK_ID, ACCOUNT_ID, VIEW_ID
    - Query Parameters: name (counterparty name to search for)
    - Headers: Authorization token
  - **Response**: 
    ```json
    {
      "counterparties": [
        {
          "counterparty_id": "string",
          "name": "string",
          "description": "string",
          "other_account_routing_scheme": "string",
          "other_account_routing_address": "string",
          "other_bank_routing_scheme": "string",
          "other_bank_routing_address": "string",
          "is_beneficiary": "boolean"
        }
      ]
    }
    ```

### Endpoint 3: Get Counterparty by ID (Explicit Named Endpoint)
- **Endpoint**: `GET /obp/v4.0.0/management/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views/{VIEW_ID}/counterparties/{COUNTERPARTY_ID}`
  - **Justification (from description)**: "Retrieve counterparty information by ID"
  - **Purpose**: Management-level endpoint for retrieving counterparty details by ID with elevated permissions
  - **Request**: 
    - Path Parameters: BANK_ID, ACCOUNT_ID, VIEW_ID, COUNTERPARTY_ID
    - Headers: Authorization token with management permissions
  - **Response**: Complete counterparty object with all associated metadata

## Business Rules (from capability description)

1. Counterparty retrieval must support lookup by unique identifier (ID)
2. Counterparty retrieval must support lookup by name
3. The capability operates in real-time with high volume expectations
4. Counterparty information is associated with specific bank accounts and views
5. Access to counterparty information requires proper authorization based on the view permissions

## Data Validations (if applicable)

- Counterparty ID must be a valid UUID format when provided
- Bank ID must reference an existing bank on the platform
- Account ID must reference a valid account within the specified bank
- View ID must reference a valid view that the user has access to
- Name search parameter should support partial matching for flexibility
- Authorization tokens must be valid and not expired

## Dependencies

- **Upstream**: 
  - User must be authenticated with valid credentials
  - Bank, Account, and View must exist in the system
  - Counterparty must have been previously created (via Counterparty Creation capability #50)
  - User must have appropriate view permissions to access counterparty data
- **Downstream**: 
  - Retrieved counterparty information can be used for Counterparty Payment initiation (capability #24)
  - Counterparty details support transaction processing and payment workflows
- **External Systems**: 
  - Authentication/Authorization service for access control
  - Core banking system connector for counterparty data storage

## Notes for Implementation

- The retrieval capability is designed for high-volume, real-time access patterns
- Consider implementing caching strategies for frequently accessed counterparties
- Name-based search should be case-insensitive for better user experience
- Ensure proper error handling for scenarios where counterparty is not found (404 responses)
- Consider pagination for name-based searches that may return multiple results
- **Needs SME Input**: Clarify if partial name matching is required or exact match only
- **Needs SME Input**: Determine if there are any rate limiting requirements for high-volume retrieval
- The capability explicitly excludes create, update, and delete operations - these are handled by separate capabilities (Counterparty Creation #50, Counterparty Deletion #52)
