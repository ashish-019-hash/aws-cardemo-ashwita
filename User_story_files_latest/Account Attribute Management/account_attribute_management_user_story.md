# User Story for Account Attribute Management

## Story Overview

**As a** Bank Administrator or API Consumer
**I want to** manage custom attributes associated with accounts
**So that** I can extend account metadata with additional typed key-value pairs for business-specific requirements such as ISIN codes, loan identifiers, maturity dates, and other financial product attributes

## Acceptance Criteria

1. The system shall allow authorized users to create new custom attributes for a specific bank account
2. The system shall allow authorized users to update existing custom attributes on a bank account
3. Account attributes must be linked to a specific account via ACCOUNT_ID
4. Account attributes must be associated with a product via PRODUCT_CODE
5. Each attribute must have a name, type, and value
6. The attribute type must be one of: STRING, INTEGER, DOUBLE, or DATE_WITH_DAY
7. The system shall validate the attribute type before creating or updating
8. The system shall verify that the bank, account, and product exist before creating/updating attributes
9. The system shall enforce proper authorization (entitlements) for attribute management operations
10. Optionally, a product_instance_code can be associated with the attribute

## Technical Context

- **Classes/Services Involved**:
  - `AccountAttributeProvider` - Trait defining the account attribute operations
  - `MappedAccountAttributeProvider` - Implementation of the account attribute provider
  - `AccountAttributeX` - Injector for account attribute provider
  - `APIMethods310` - API endpoint definitions for account attribute operations
  - `NewStyle.function` - Helper functions for async operations and validation

- **Input Data**:
  - `AccountAttributeJson` - Request body containing:
    - `name`: String - The attribute name (e.g., "ISIN", "LOAN_ID", "MATURITY_DATE")
    - `type`: String - The attribute type (STRING, INTEGER, DOUBLE, DATE_WITH_DAY)
    - `value`: String - The attribute value
    - `product_instance_code`: Optional[String] - Optional product instance identifier
  - Path parameters: BANK_ID, ACCOUNT_ID, PRODUCT_CODE, ACCOUNT_ATTRIBUTE_ID (for updates)

- **Output Data**:
  - `AccountAttributeResponseJson` - Response containing:
    - `account_attribute_id`: String - Unique identifier for the attribute
    - `name`: String - The attribute name
    - `type`: String - The attribute type
    - `value`: String - The attribute value
    - `product_instance_code`: Optional[String] - Product instance identifier
    - `bank_id`: String - The bank identifier
    - `account_id`: String - The account identifier
    - `product_code`: String - The product code

- **Processing Type**: API (REST request-response)

## Relevant Endpoints

**IMPORTANT**: The capability description states "Manage custom attributes associated with accounts". According to the Operation Derivation Rules, "manage" means ONLY update/configure/maintain operations.

### Endpoint 1: Create Account Attribute

- **Endpoint**: `POST /banks/BANK_ID/accounts/ACCOUNT_ID/products/PRODUCT_CODE/attribute`
- **Justification (from description)**: "Manage" - creating is part of managing/configuring account attributes
- **Purpose**: Create a new custom attribute for a specific bank account associated with a product
- **Request**:
  ```json
  {
    "name": "ISIN",
    "type": "STRING",
    "value": "GB0002634946",
    "product_instance_code": "INST001"
  }
  ```
- **Response**:
  ```json
  {
    "account_attribute_id": "7uy8a7e4-6d02-40e3-a129-0b2bf89de8uh",
    "name": "ISIN",
    "type": "STRING",
    "value": "GB0002634946",
    "product_instance_code": "INST001",
    "bank_id": "gh.29.uk",
    "account_id": "8ca8a7e4-6d02-40e3-a129-0b2bf89de9f0",
    "product_code": "PRODUCT_CODE"
  }
  ```
- **Required Entitlement**: `canCreateAccountAttributeAtOneBank`
- **HTTP Status**: 201 Created

### Endpoint 2: Update Account Attribute

- **Endpoint**: `PUT /banks/BANK_ID/accounts/ACCOUNT_ID/products/PRODUCT_CODE/attributes/ACCOUNT_ATTRIBUTE_ID`
- **Justification (from description)**: "Manage" - updating is the core meaning of managing/maintaining attributes
- **Purpose**: Update an existing custom attribute for a specific bank account
- **Request**:
  ```json
  {
    "name": "ISIN",
    "type": "STRING",
    "value": "GB0002634947",
    "product_instance_code": "INST002"
  }
  ```
- **Response**:
  ```json
  {
    "account_attribute_id": "7uy8a7e4-6d02-40e3-a129-0b2bf89de8uh",
    "name": "ISIN",
    "type": "STRING",
    "value": "GB0002634947",
    "product_instance_code": "INST002",
    "bank_id": "gh.29.uk",
    "account_id": "8ca8a7e4-6d02-40e3-a129-0b2bf89de9f0",
    "product_code": "PRODUCT_CODE"
  }
  ```
- **Required Entitlement**: `canUpdateAccountAttribute`
- **HTTP Status**: 201 Created

## Business Rules

1. **Attribute Type Validation**: The `type` field must be one of the following valid types:
   - `STRING` - For text values (e.g., "TAX_NUMBER")
   - `INTEGER` - For whole number values (e.g., "123")
   - `DOUBLE` - For decimal values (e.g., "2012.04")
   - `DATE_WITH_DAY` - For date values (e.g., "2012-04-23")

2. **Entity Existence Validation**: Before creating or updating an attribute:
   - The bank (BANK_ID) must exist in the system
   - The account (ACCOUNT_ID) must exist within the specified bank
   - The product (PRODUCT_CODE) must exist within the specified bank

3. **Authorization Requirements**:
   - Creating attributes requires `canCreateAccountAttributeAtOneBank` entitlement
   - Updating attributes requires `canUpdateAccountAttribute` entitlement

4. **Attribute Uniqueness**: Each account attribute is uniquely identified by its `account_attribute_id`

5. **Product Association**: Account attributes are linked to both an account and a product, enabling product-specific metadata on accounts

## Data Validations

- **Type Field Validation**: The system validates that the `type` field contains a valid `AccountAttributeType` enum value
- **JSON Format Validation**: The request body must be valid JSON matching the `AccountAttributeJson` schema
- **Entity Existence Checks**: 
  - Bank must exist (returns error if not found)
  - Account must exist within the bank (returns error if not found)
  - Product must exist within the bank (returns error if not found)
- **Attribute Existence Check (for updates)**: The attribute with the specified `ACCOUNT_ATTRIBUTE_ID` must exist before updating
- **Authentication Required**: User must be logged in to perform any attribute management operations

## Dependencies

- **Upstream**:
  - Bank must be created and exist in the system
  - Account must be created within the bank
  - Product must be defined for the bank
  - User must be authenticated and have appropriate entitlements

- **Downstream**:
  - Account attributes may be included in account detail responses
  - Account attributes may be used for filtering/searching accounts
  - Account attributes may be used in reporting and analytics

- **External Systems**:
  - Backend connector (LocalMappedConnector, AkkaConnector, RestConnector, etc.) for data persistence
  - Authentication system for user validation
  - Entitlement system for authorization checks

## Notes for Implementation

1. **Typical Use Cases**: Account attributes are commonly used for:
   - ISIN (International Securities Identification Number) for bonds
   - VKN (German bond identifier)
   - REDCODE (Markit short code for credit derivatives)
   - LOAN_ID (for Anacredit reporting)
   - ISSUE_DATE (when a bond was issued)
   - MATURITY_DATE (end of product lifetime)
   - TRADABLE (whether the product can be traded)

2. **Reference Standards**: See [FPML](http://www.fpml.org/) for additional examples of financial product attributes

3. **Attribute Definition Management**: There are separate endpoints for managing Account Attribute Definitions (schema/metadata for attributes) which are not part of this capability but may be relevant:
   - Create/Update Account Attribute Definition
   - Get Account Attribute Definition
   - Delete Account Attribute Definition

4. **Needs SME Input**:
   - What are the specific attribute names and types required for the target system?
   - Are there any constraints on attribute values beyond type validation?
   - Should there be a maximum number of attributes per account?
   - Are there any mandatory attributes that must be present on all accounts?

5. **Note on Excluded Operations**: The capability description states "Manage custom attributes" which, per the Operation Derivation Rules, includes only create and update operations. The following operations exist in the codebase but are NOT included in this capability scope as they are not explicitly mentioned in the description:
   - GET operations for retrieving/viewing attributes
   - DELETE operations for removing attributes
   - LIST operations for browsing attributes
   
   If these operations are needed, the capability description should be updated to explicitly include verbs like "view", "retrieve", "list", or "delete".
