# User Story for Transaction Attribute Management

## Story Overview

**As a** Bank Administrator or API Consumer with appropriate entitlements  
**I want to** manage custom attributes associated with transactions  
**So that** I can extend transaction metadata with additional business-specific information beyond the standard transaction fields, enabling flexible categorization, compliance tracking, and custom data storage for transactions

## Acceptance Criteria

1. The system shall allow authorized users to update existing custom attributes on transactions
2. The system shall support multiple attribute types including STRING, INTEGER, DOUBLE, and DATE_WITH_DAY
3. The system shall validate attribute type values against their declared types before persisting
4. The system shall require valid bank ID, account ID, and transaction ID for all attribute management operations
5. The system shall enforce role-based access control requiring appropriate entitlements for attribute management
6. The system shall return appropriate error messages when validation fails or required resources are not found
7. The system shall support attribute definitions at the bank level to standardize attribute schemas

## Technical Context

- **Classes/Services Involved**:
  - `TransactionAttributeProvider` - Core trait defining attribute management operations
  - `MappedTransactionAttributeProvider` - Implementation using Lift Mapper for persistence
  - `TransactionAttributeX` - Dependency injection container for the provider
  - `APIMethods400` - REST API endpoint definitions for v4.0.0
  - `JSONFactory400` - JSON serialization/deserialization for request/response payloads
  - `NewStyle` - Helper functions for async operations and error handling

- **Input Data**:
  - Request Body (JSON):
    ```json
    {
      "name": "string",
      "type": "STRING | INTEGER | DOUBLE | DATE_WITH_DAY",
      "value": "string"
    }
    ```
  - Path Parameters: BANK_ID, ACCOUNT_ID, TRANSACTION_ID, ATTRIBUTE_ID (for updates)

- **Output Data**:
  - Response Body (JSON):
    ```json
    {
      "transaction_attribute_id": "string",
      "name": "string",
      "type": "string",
      "value": "string"
    }
    ```

- **Processing Type**: API / Real-time (HTTP request-response)

## Relevant Endpoints

**IMPORTANT**: Based on the capability description "Manage custom attributes associated with transactions", only update/configure operations are justified. The word "manage" by itself means ONLY update/configure/maintain operations per the Operation Derivation Rules.

- **Endpoint**: PUT /banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID/attributes/ATTRIBUTE_ID
  - **Justification (from description)**: "Manage" - justifies update/configure operations
  - **Purpose**: Update an existing custom attribute on a specific transaction
  - **Request**: 
    ```json
    {
      "name": "attribute_name",
      "type": "STRING",
      "value": "attribute_value"
    }
    ```
  - **Response**: 
    ```json
    {
      "transaction_attribute_id": "uuid",
      "name": "attribute_name",
      "type": "STRING",
      "value": "attribute_value"
    }
    ```
  - **Required Role**: canUpdateTransactionAttributeAtOneBank

- **Endpoint**: PUT /banks/BANK_ID/attribute-definitions/transaction
  - **Justification (from description)**: "Manage" - justifies configuration of attribute definitions
  - **Purpose**: Create or update transaction attribute definitions at the bank level to standardize attribute schemas
  - **Request**: Attribute definition JSON with name, type, description, and validation rules
  - **Response**: Created/updated attribute definition details
  - **Required Role**: canCreateTransactionAttributeDefinitionAtOneBank

## Business Rules (from capability description)

1. **Attribute Type Validation**: The `type` field must be one of the supported types: DOUBLE (e.g., 12.1234), STRING (e.g., TAX_NUMBER), INTEGER (e.g., 123), or DATE_WITH_DAY (e.g., 2012-04-23)
2. **Resource Existence**: The transaction must exist before attributes can be managed on it
3. **Bank Scope**: Attributes are scoped to a specific bank and must be managed within that bank's context
4. **Attribute Identity**: Each attribute has a unique transaction_attribute_id for identification and updates
5. **Entitlement Enforcement**: Users must have the appropriate role/entitlement to manage transaction attributes

## Data Validations (if applicable)

- **Bank ID Validation**: Must be a valid, existing bank identifier
- **Account ID Validation**: Must be a valid account belonging to the specified bank
- **Transaction ID Validation**: Must be a valid transaction within the specified account
- **Attribute ID Validation**: For updates, must reference an existing attribute
- **Type Field Validation**: Must match one of the enumerated TransactionAttributeType values
- **Value Format Validation**: Value must be compatible with the declared attribute type (e.g., numeric string for INTEGER type)
- **JSON Format Validation**: Request body must conform to the expected JSON schema

## Dependencies

- **Upstream**:
  - Transaction must exist before attributes can be managed
  - User must be authenticated and have valid session
  - User must have appropriate entitlements (canUpdateTransactionAttributeAtOneBank)
  - Bank and account must exist and be accessible

- **Downstream**:
  - Updated attributes are persisted to the database
  - Attribute changes may be reflected in transaction views that include attribute data
  - Attribute definitions affect validation of future attribute operations

- **External Systems**:
  - Backend connector (LocalMappedConnector, REST, Akka, or other configured connector)
  - Database persistence layer (Lift Mapper / PostgreSQL or other configured database)

## Notes for Implementation

- **Attribute Type Handling**: The system uses Scala enumerations (TransactionAttributeType) for type safety. The Go implementation should use a similar type-safe approach with constants or enums.
- **Async Processing**: The Scala implementation uses Futures for async database operations. The Go implementation should use goroutines and channels or similar async patterns.
- **Error Handling**: Comprehensive error messages are returned for various failure scenarios including invalid JSON, missing resources, and authorization failures.
- **Idempotency**: The update operation uses createOrUpdate semantics, allowing the same attribute to be updated multiple times with the same or different values.
- **Needs SME Input**: 
  - Clarify if there are any business-specific attribute naming conventions or reserved attribute names
  - Confirm if attribute history/audit trail is required for compliance purposes
  - Determine if there are any size limits on attribute values
