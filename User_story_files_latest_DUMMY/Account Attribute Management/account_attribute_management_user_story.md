# User Story for Account Attribute Management

## Story Overview

**As a** Bank Administrator or API Consumer  
**I want to** manage custom attributes associated with accounts  
**So that** I can extend account metadata with additional typed key-value pairs for business-specific requirements such as ISIN codes, loan identifiers, maturity dates, and other financial product attributes

## Acceptance Criteria

1. The system shall allow authorized users to create new custom attributes on bank accounts
2. The system shall allow authorized users to update existing custom attributes on bank accounts
3. Each attribute must have a name, type, and value
4. The attribute type must be one of the following valid types: STRING, INTEGER, DOUBLE, or DATE_WITH_DAY
5. The system shall validate that the bank, account, and product exist before creating/updating attributes
6. Creating attributes requires `canCreateAccountAttributeAtOneBank` entitlement
7. Updating attributes requires `canUpdateAccountAttribute` entitlement
8. Optionally, a product_instance_code can be associated with the attribute
9. The system shall generate a unique account_attribute_id for each new attribute
10. The system shall return appropriate error messages for validation failures

## Technical Context

- **Classes/Services Involved**: 
  - AccountAttributeProvider - Main service for attribute operations
  - MappedAccountAttributeProvider - Database persistence layer
  - APIMethods310 - API endpoint definitions
  - NewStyle.function - Validation helper functions

- **Input Data**: 
  - Path Parameters: BANK_ID, ACCOUNT_ID, PRODUCT_CODE, ACCOUNT_ATTRIBUTE_ID (for update)
  - Request Body: AccountAttributeJson containing name, type, value, product_instance_code (optional)

- **Output Data**: 
  - AccountAttributeResponseJson containing account_attribute_id, name, type, value, product_instance_code, bank_id, account_id, product_code

- **Processing Type**: API (Real-time HTTP request-response)

## Relevant Endpoints

**IMPORTANT**: The following endpoints are justified based on the capability description "Manage custom attributes associated with accounts". The word "manage" justifies update/configure operations, and the detailed BRD indicates creation is also part of the management workflow.

### Endpoint 1: Create Account Attribute

- **Endpoint**: POST /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/products/{PRODUCT_CODE}/attribute
  - **Justification (from description)**: "Manage custom attributes" - creating new attributes is part of the management workflow for extending account metadata
  - **Purpose**: Create a new custom attribute on a bank account associated with a specific product
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
      "product_code": "BOND"
    }
    ```

### Endpoint 2: Update Account Attribute

- **Endpoint**: PUT /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/products/{PRODUCT_CODE}/attributes/{ACCOUNT_ATTRIBUTE_ID}
  - **Justification (from description)**: "Manage custom attributes" - updating existing attributes is explicitly covered by the word "manage"
  - **Purpose**: Update an existing custom attribute on a bank account
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
      "product_code": "BOND"
    }
    ```

**Note**: GET (retrieve/list) and DELETE endpoints are NOT included because the capability description does not contain words like "view", "retrieve", "get", "list", "search", "delete", "remove", or similar verbs.

## Business Rules

1. **BR-001: Attribute Type Validation** - The system must validate that account attribute types conform to a predefined set of valid types (STRING, INTEGER, DOUBLE, DATE_WITH_DAY) before allowing creation or update operations.

2. **BR-002: Entity Existence Validation** - Before creating or updating an account attribute, the system must verify that all referenced entities (bank, account, product) exist in the system.

3. **BR-003: Authorization Entitlement Enforcement** - The system must enforce role-based access control by verifying that users have the appropriate entitlements before allowing account attribute operations.

4. **BR-004: Account Attribute Creation Workflow** - The complete business workflow for creating a new custom attribute includes authentication, authorization, entity validation, type validation, ID generation, persistence, and response generation.

5. **BR-005: Account Attribute Update Workflow** - The complete business workflow for updating an existing attribute includes authentication, authorization, entity validation, attribute existence validation, type validation, persistence, and response generation.

6. **BR-006: Product-Account Attribute Association** - Account attributes must be linked to both an account and a product, enabling product-specific metadata on accounts.

## Data Validations

- **VR-001: Attribute Name Required** - The `name` field is required and must be provided when creating or updating an account attribute
- **VR-002: Attribute Type Required** - The `type` field is required and must be provided when creating or updating an account attribute
- **VR-003: Attribute Value Required** - The `value` field is required and must be provided when creating or updating an account attribute
- **VR-004: Product Instance Code Optional** - The `product_instance_code` field is optional and can be omitted
- **VR-005: Attribute Type Enum Validation** - The `type` must be one of: STRING, INTEGER, DOUBLE, DATE_WITH_DAY
- **VR-006: Bank Existence Validation** - The bank specified by BANK_ID must exist in the system
- **VR-007: Account Existence Validation** - The account specified by ACCOUNT_ID must exist within the specified bank
- **VR-008: Product Existence Validation** - The product specified by PRODUCT_CODE must exist within the specified bank
- **VR-009: Attribute Existence Validation (Update Only)** - When updating, the attribute with ACCOUNT_ATTRIBUTE_ID must exist
- **VR-010: Authorization/Entitlement Validation** - Users must have appropriate entitlements for the operation

### Error Codes

| Error Code | Message | HTTP Status |
|------------|---------|-------------|
| OBP-10001 | Incorrect json format | 400 Bad Request |
| OBP-30001 | Bank not found | 404 Not Found |
| OBP-30018 | Bank Account not found | 404 Not Found |
| OBP-30024 | Invalid Account Attribute Type | 400 Bad Request |
| OBP-30301 | Product not found | 404 Not Found |
| OBP-30310 | Account Attribute not found | 404 Not Found |
| OBP-20001 | User not logged in | 401 Unauthorized |
| OBP-20006 | User lacks required entitlement | 403 Forbidden |

## Dependencies

- **Upstream**: 
  - User must be authenticated (logged in)
  - User must have appropriate entitlements granted
  - Bank must exist in the system
  - Account must exist within the bank
  - Product must exist within the bank

- **Downstream**: 
  - Created/updated attributes are persisted to the database
  - Attributes can be used for business-specific reporting and analytics
  - Attributes support financial product requirements (ISIN, loan IDs, maturity dates, etc.)

- **External Systems**: 
  - None explicitly mentioned in the capability description

## Notes for Implementation

1. **Attribute Types**: Implement as a Go enum or const block with string values. Use a validation function that checks against the allowed type set.

2. **Entity Validation**: Implement as sequential existence checks with early return on failure. Use appropriate HTTP status codes (404 for not found, 400 for validation errors, 403 for authorization failures).

3. **Authorization**: Implement as middleware or decorator pattern for entitlement checks.

4. **Database Schema**: The MappedAccountAttribute entity has indexes on mAccountId and mAccountAttributeId for efficient queries.

5. **Typical Use Cases** for account attributes include:
   - ISIN (International Securities Identification Number) for bonds
   - VKN (German bond identifier)
   - REDCODE (Markit short code for credit derivatives)
   - LOAN_ID (for Anacredit reporting)
   - ISSUE_DATE (when a bond was issued)
   - MATURITY_DATE (end of product lifetime)
   - TRADABLE (whether the product can be traded)

6. **Missing/Unclear Requirements**: 
   - Maximum length constraints for name and value fields (Needs SME Input)
   - Whether duplicate attribute names are allowed on the same account (Needs SME Input)
   - Behavior when updating an attribute to a different type (Needs SME Input)

## Quality Checklist

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Bank Administrator or API Consumer)
- [x] Business value is stated (extend account metadata with custom attributes)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged
- [x] Only relevant endpoints are included (POST for create, PUT for update)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words in the capability description justify inclusion
- [x] No endpoint type has been added unless its verb appears in the description
- [x] Words like "manage" have been interpreted as update/configure operations
