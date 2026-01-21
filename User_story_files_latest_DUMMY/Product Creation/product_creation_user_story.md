# User Story for Product Creation

## Story Overview

**As a** Bank Product Manager / Platform Administrator  
**I want to** create banking product definitions  
**So that** the bank can offer standardized financial products (such as savings accounts, loans, credit cards) to customers through the platform

## Acceptance Criteria

1. The system shall allow authorized users to create new banking product definitions
2. The system shall accept and store product metadata during creation (name, type, category, etc.)
3. The system shall validate all required product definition fields before creation
4. The system shall generate a unique identifier for each newly created product
5. The system shall associate product attributes and configuration with the product entity during creation
6. The system shall return confirmation of successful product creation with the created product details
7. The system shall reject creation requests with invalid or incomplete data with appropriate error messages

## Technical Context

- **Classes/Services Involved**: 
  - Product entity/model classes
  - Product creation service/handler
  - Product validation service
  - Database/persistence layer for product storage

- **Input Data**: 
  - Product name (required)
  - Product code/identifier
  - Product type (e.g., savings, checking, loan, credit card)
  - Product category
  - Product description
  - Associated bank ID
  - Product attributes (interest rates, fees, terms, etc.)
  - Product configuration parameters

- **Output Data**: 
  - Created product entity with generated ID
  - Product metadata as stored
  - Configuration settings applied
  - Creation timestamp
  - Success/error response

- **Processing Type**: API / On-demand

## Relevant Endpoints

**IMPORTANT**: For every endpoint included, justification from the description is provided.

### Endpoint 1: Create Product
- **Endpoint**: `POST /obp/v5.1.0/banks/{BANK_ID}/products`
  - **Justification (from description)**: "Create banking product definitions" - the word "Create" explicitly justifies a POST endpoint for product creation
  - **Purpose**: Create a new banking product definition with all associated metadata and configuration
  - **Request**: 
    ```json
    {
      "product_code": "string",
      "name": "string",
      "category": "string",
      "family": "string",
      "super_family": "string",
      "more_info_url": "string (URL)",
      "details": "string",
      "description": "string",
      "meta": {
        "license": {
          "id": "string",
          "name": "string"
        }
      },
      "attributes": [
        {
          "name": "string",
          "type": "string",
          "value": "string"
        }
      ]
    }
    ```
  - **Response**: 
    ```json
    {
      "bank_id": "string",
      "product_code": "string",
      "name": "string",
      "category": "string",
      "family": "string",
      "super_family": "string",
      "more_info_url": "string",
      "details": "string",
      "description": "string",
      "meta": {
        "license": {
          "id": "string",
          "name": "string"
        }
      },
      "attributes": [...],
      "created_at": "timestamp"
    }
    ```

**Note**: Based on the Operation Derivation Rules, the following endpoints are explicitly EXCLUDED because their operation types are not mentioned in the capability description:
- GET /banks/{BANK_ID}/products/{PRODUCT_CODE} - No "view", "retrieve", or "get" mentioned
- GET /banks/{BANK_ID}/products - No "list" or "search" mentioned
- PUT /banks/{BANK_ID}/products/{PRODUCT_CODE} - No "update" or "modify" mentioned
- DELETE /banks/{BANK_ID}/products/{PRODUCT_CODE} - No "delete" or "remove" mentioned

## Business Rules (from capability description)

1. **Product Definition Uniqueness**: Each product must have a unique product code within a bank
2. **Bank Association**: Products must be created under a specific bank entity
3. **Definition Requirements**: Product creation must include essential definition attributes (name, type, category)
4. **Authorization Required**: Only authorized bank product managers or platform administrators can create new product definitions
5. **On-demand Processing**: Product creation is performed on-demand (not batch or scheduled)
6. **Low Volume Operation**: Product creation is expected to be a low-volume operation

## Data Validations (if applicable)

- Product code must be unique within the bank
- Product name is required and must not be empty
- Product code must follow naming conventions (alphanumeric, limited length)
- More info URL must be a valid URL format if provided
- Product category must be a valid category type
- Product attributes must have valid name-type-value combinations
- Bank ID must reference an existing bank entity

## Dependencies

- **Upstream**: 
  - User authentication and authorization must be completed
  - User must have appropriate entitlements/roles for product creation (e.g., CanCreateProduct)
  - Bank entity must exist before products can be created under it
  - Platform must be operational and accepting requests

- **Downstream**: 
  - After product creation, the product becomes available for:
    - Account creation with the product type
    - Product attribute management
    - Product fee configuration
    - Customer product offerings
    - API consumer access to product information

- **External Systems**: 
  - Database/persistence layer for storing product definitions
  - Bank entity service for validating bank existence

## Notes for Implementation

- **Authorization**: Ensure proper role-based access control - only users with CanCreateProduct or similar entitlement should be able to create products
- **Bank Validation**: Verify that the specified bank exists before creating a product under it
- **Idempotency**: Consider implementing idempotency for product creation to handle duplicate requests gracefully
- **Validation Order**: Validate all input data before attempting to persist to avoid partial creation states
- **Audit Trail**: Log product creation events for compliance and audit purposes
- **Error Handling**: Provide clear, actionable error messages for validation failures

### Open Questions (Needs SME Input)

1. What are the mandatory vs optional fields for product definition creation?
2. What product categories and types are supported by the platform?
3. Are there any naming conventions or restrictions for product codes?
4. Should product creation trigger any downstream notifications or events?
5. What validation rules apply to product attributes?
6. Is there a maximum number of attributes that can be associated with a product during creation?
7. Can products be created without being immediately active/available?

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Bank Product Manager / Platform Administrator)
- [x] Business value is stated (enabling banks to offer standardized financial products)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (Open Questions section)
- [x] Only relevant endpoints are included (POST for creation only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific word from capability description justifies inclusion ("Create")
- [x] No endpoint type has been added unless its verb appears in the description
- [x] No CRUD operations inferred beyond what description explicitly states
