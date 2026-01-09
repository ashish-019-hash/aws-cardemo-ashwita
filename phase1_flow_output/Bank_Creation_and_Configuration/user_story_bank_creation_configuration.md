# User Story for Bank Creation and Configuration

## Story Overview

**As a** Platform Administrator or Bank Onboarding Manager
**I want to** create and configure new banks on the platform with associated attributes and settlement accounts
**So that** new financial institutions can be onboarded to the Open Bank Project platform and begin offering their banking services through standardized APIs

## Acceptance Criteria

1. A new bank can be created on the platform with a unique bank ID, bank code, full name, logo, and website
2. Bank routing information (BIC, OBP routing) can be specified during bank creation
3. The bank ID must be greater than 3 characters and cannot contain spaces or "::::" characters
4. The bank ID must be unique and not conflict with existing banks on the platform
5. Settlement accounts are automatically created when a new bank is created:
   - Default incoming settlement account (Account ID: OBP_DEFAULT_INCOMING_ACCOUNT_ID, currency: EUR)
   - Default outgoing settlement account (Account ID: OBP_DEFAULT_OUTGOING_ACCOUNT_ID, currency: EUR)
6. The user creating the bank is automatically assigned the CanCreateEntitlementAtOneBank role
7. Custom bank attributes can be created to store extended metadata for the bank
8. Bank attributes support multiple data types: STRING, INTEGER, DOUBLE, DATE_WITH_DAY
9. Bank configuration can be updated after initial creation

## Technical Context

- **Classes/Services Involved**:
  - `APIMethods600.createBank` - Main bank creation endpoint handler
  - `APIMethods400.createBankAttribute` - Bank attribute creation handler
  - `APIMethods400.updateBankAttribute` - Bank attribute update handler
  - `NewStyle.function.createOrUpdateBank` - Core bank creation/update logic
  - `JSONFactory600.PostBankJson600` - Request body structure for bank creation
  - `JSONFactory400.BankAttributeRequestJsonV400` - Request body for bank attributes
  - `MappedBankAttributeProvider` - Bank attribute persistence layer

- **Input Data**:
  - Bank creation request: bank_id, bank_code, full_name (optional), logo (optional), website (optional), bank_routings (optional list of routing schemes and addresses)
  - Bank attribute request: name, type (STRING/INTEGER/DOUBLE/DATE_WITH_DAY), value, is_active

- **Output Data**:
  - Bank creation response: Complete bank JSON with id, short_name, full_name, logo, website, bank_routings, attributes
  - Bank attribute response: bank_id, bank_attribute_id, name, type, value, is_active

- **Processing Type**: API (REST) - On-demand, Real-time

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description.

### Endpoint 1: Create Bank
- **Endpoint**: `POST /banks`
- **Justification (from description)**: "Create... new banks on the platform"
- **Purpose**: Creates a new bank entity on the OBP platform with basic configuration
- **Request**:
  ```json
  {
    "bank_id": "string (required, >3 chars, no spaces)",
    "bank_code": "string (required)",
    "full_name": "string (optional)",
    "logo": "string (optional, URL)",
    "website": "string (optional, URL)",
    "bank_routings": [
      {
        "scheme": "string (e.g., BIC, OBP)",
        "address": "string"
      }
    ]
  }
  ```
- **Response**:
  ```json
  {
    "id": "string",
    "short_name": "string",
    "full_name": "string",
    "logo": "string",
    "website": "string",
    "bank_routings": [
      {
        "scheme": "string",
        "address": "string"
      }
    ],
    "attributes": []
  }
  ```

### Endpoint 2: Create Bank Attribute
- **Endpoint**: `POST /banks/BANK_ID/attribute`
- **Justification (from description)**: "Create... with associated attributes"
- **Purpose**: Creates custom attributes for a bank to store extended metadata
- **Request**:
  ```json
  {
    "name": "string",
    "type": "STRING | INTEGER | DOUBLE | DATE_WITH_DAY",
    "value": "string",
    "is_active": "boolean"
  }
  ```
- **Response**:
  ```json
  {
    "bank_id": "string",
    "bank_attribute_id": "string",
    "name": "string",
    "type": "string",
    "value": "string",
    "is_active": "boolean"
  }
  ```

### Endpoint 3: Update Bank Attribute
- **Endpoint**: `PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID`
- **Justification (from description)**: "configure... with associated attributes"
- **Purpose**: Updates existing bank attributes for configuration changes
- **Request**:
  ```json
  {
    "name": "string",
    "type": "STRING | INTEGER | DOUBLE | DATE_WITH_DAY",
    "value": "string",
    "is_active": "boolean"
  }
  ```
- **Response**:
  ```json
  {
    "bank_id": "string",
    "bank_attribute_id": "string",
    "name": "string",
    "type": "string",
    "value": "string",
    "is_active": "boolean"
  }
  ```

### Endpoint 4: Create Settlement Account
- **Endpoint**: `POST /banks/BANK_ID/settlement-accounts`
- **Justification (from description)**: "Create... with... settlement accounts"
- **Purpose**: Creates settlement accounts for payment processing at the bank
- **Request**:
  ```json
  {
    "user_id": "string",
    "payment_system": "string",
    "balance": {
      "currency": "string",
      "amount": "string"
    },
    "label": "string",
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
    "user_id": "string",
    "payment_system": "string",
    "balance": {
      "currency": "string",
      "amount": "string"
    },
    "label": "string",
    "branch_id": "string",
    "account_routings": [
      {
        "scheme": "string",
        "address": "string"
      }
    ]
  }
  ```

## Business Rules (from capability description)

1. **Bank ID Uniqueness**: Each bank must have a unique identifier that does not conflict with existing banks
2. **Bank ID Format**: Bank ID must be greater than 3 characters, cannot contain spaces, and cannot contain "::::" characters
3. **Automatic Role Assignment**: The user creating a bank is automatically granted CanCreateEntitlementAtOneBank and CanReadDynamicResourceDocsAtOneBank roles for that bank
4. **Automatic Settlement Account Creation**: When a bank is created in SANDBOX mode, default incoming and outgoing settlement accounts are automatically created with EUR currency
5. **Attribute Type Validation**: Bank attributes must specify a valid type (STRING, INTEGER, DOUBLE, or DATE_WITH_DAY)
6. **Consumer Validation**: Valid consumer credentials are required to create a bank

## Data Validations (if applicable)

- **Bank ID Validation**:
  - Must be a short string (length validation)
  - Must be greater than 3 characters
  - Cannot contain space characters
  - Cannot contain "::::" characters
  - Must be unique across all banks

- **Bank Attribute Validation**:
  - Type must be one of: STRING, INTEGER, DOUBLE, DATE_WITH_DAY
  - Name and value are required fields
  - is_active must be a boolean

- **Authentication Validation**:
  - User must be authenticated
  - User must have appropriate entitlements (canCreateBank, canCreateBankAttribute)
  - Valid consumer credentials required

## Dependencies

- **Upstream**:
  - User authentication and authorization must be completed
  - Consumer registration must be completed
  - User must have canCreateBank entitlement for bank creation
  - User must have canCreateBankAttribute entitlement for attribute creation

- **Downstream**:
  - Once a bank is created, accounts can be created under that bank
  - Bank attributes can be used for extended metadata storage
  - Settlement accounts enable payment processing capabilities
  - Other users can be granted entitlements at the newly created bank

- **External Systems**:
  - Database persistence layer (PostgreSQL, MySQL, H2, MS SQL Server, or Oracle)
  - Entitlement management system for role assignment

## Notes for Implementation

- **SANDBOX Mode Requirement**: The automatic settlement account creation feature only works in SANDBOX mode (when connector=mapped in properties file)
- **Entitlement Auto-Assignment**: Consider the security implications of automatically granting CanCreateEntitlementAtOneBank role to bank creators
- **Settlement Account Defaults**: Default settlement accounts use EUR currency - consider making this configurable for multi-currency support
- **Attribute Type Extensibility**: The attribute type system may need extension for additional data types in the future
- **Needs SME Input**: Clarification needed on whether bank configuration updates (PUT /banks/BANK_ID) should be included or if configuration is only done through attributes
- **Migration Consideration**: When migrating from Scala to Go, ensure the bank ID validation rules are preserved exactly to maintain data consistency
