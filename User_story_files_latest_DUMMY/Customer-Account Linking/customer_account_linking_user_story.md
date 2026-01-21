# User Story for Customer-Account Linking

## Story Overview

**As a** Bank Administrator / Customer Relationship Manager  
**I want to** link customers to bank accounts  
**So that** customers can be associated with their bank accounts, enabling proper account ownership tracking, customer relationship management, and allowing customers to access and manage their linked accounts through the banking platform

## Acceptance Criteria

1. The system shall allow authorized users to create a link between a customer and a bank account
2. The system shall validate that the specified customer exists in the system before creating the link
3. The system shall validate that the specified bank account exists before creating the link
4. The system shall create the customer-account association with the specified relationship type
5. The system shall return confirmation of successful linking with the created link details
6. The system shall reject linking requests with invalid customer ID or account ID with appropriate error messages
7. The system shall ensure that duplicate links are not created for the same customer-account pair
8. The system shall associate the link with the appropriate bank entity

## Technical Context

- **Classes/Services Involved**: 
  - Customer entity/model classes
  - Account entity/model classes
  - Customer-Account Link service/handler
  - Relationship type management service
  - Validation service for customer and account existence
  - Database/persistence layer for link storage

- **Input Data**: 
  - Bank ID (required) - the bank context for the operation
  - Customer ID (required) - the customer to be linked
  - Account ID (required) - the account to link the customer to
  - Relationship type (optional) - the type of relationship between customer and account (e.g., owner, authorized user, beneficiary)

- **Output Data**: 
  - Created customer-account link entity
  - Customer ID as stored
  - Account ID as stored
  - Relationship type
  - Bank ID association
  - Creation timestamp
  - Success/error response

- **Processing Type**: API / On-demand

## Relevant Endpoints

**IMPORTANT**: For every endpoint included, justification from the description is provided.

### Endpoint 1: Create Customer-Account Link
- **Endpoint**: `POST /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/customer-account-links`
  - **Justification (from description)**: "Link customers to bank accounts" - the word "Link" explicitly justifies a POST endpoint for creating the association between customer and account
  - **Purpose**: Create a new link between a customer and a bank account, establishing the relationship
  - **Request**: 
    ```json
    {
      "customer_id": "string",
      "relationship_type": "string"
    }
    ```
  - **Response**: 
    ```json
    {
      "customer_account_link_id": "string",
      "customer_id": "string",
      "account_id": "string",
      "bank_id": "string",
      "relationship_type": "string",
      "date_created": "timestamp"
    }
    ```

### Endpoint 2: Create Customer-Account Link (Alternative Path)
- **Endpoint**: `POST /obp/v5.1.0/banks/{BANK_ID}/customers/{CUSTOMER_ID}/account-links`
  - **Justification (from description)**: "Link customers to bank accounts" - the word "Link" explicitly justifies a POST endpoint for creating the association, with customer as the primary resource
  - **Purpose**: Create a new link from a customer to a bank account, establishing the relationship from the customer perspective
  - **Request**: 
    ```json
    {
      "account_id": "string",
      "relationship_type": "string"
    }
    ```
  - **Response**: 
    ```json
    {
      "customer_account_link_id": "string",
      "customer_id": "string",
      "account_id": "string",
      "bank_id": "string",
      "relationship_type": "string",
      "date_created": "timestamp"
    }
    ```

**Note**: Based on the Operation Derivation Rules, the following endpoints are explicitly EXCLUDED because their operation types are not mentioned in the capability description:
- GET /banks/{BANK_ID}/customers/{CUSTOMER_ID}/account-links - No "view", "retrieve", "get", or "list" mentioned
- GET /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/customer-links - No "view", "retrieve", "get", or "list" mentioned
- PUT /banks/{BANK_ID}/customer-account-links/{LINK_ID} - No "update", "modify", or "manage" mentioned
- DELETE /banks/{BANK_ID}/customer-account-links/{LINK_ID} - No "delete", "remove", or "unlink" mentioned

## Business Rules (from capability description)

1. **Customer-Account Association**: The capability specifically creates links between customers and bank accounts
2. **Bank Context**: All linking operations occur within the context of a specific bank
3. **Authorization Required**: Only authorized users (bank administrators, customer relationship managers) can create customer-account links
4. **On-demand Processing**: Customer-account linking is performed on-demand (not batch or scheduled)
5. **Medium Volume Operation**: Customer-account linking is expected to be a medium-volume operation
6. **Relationship Establishment**: The link establishes a formal relationship between the customer entity and the account entity

## Data Validations (if applicable)

- Bank ID must reference an existing bank on the platform
- Customer ID must reference an existing, valid customer within the specified bank
- Account ID must reference an existing, valid account within the specified bank
- Relationship type must be a valid relationship type if specified (e.g., owner, authorized_user, beneficiary)
- The customer-account link must not already exist (prevent duplicates)
- The customer must be eligible to be linked to the specified account type

## Dependencies

- **Upstream**: 
  - User authentication and authorization must be completed
  - User must have appropriate entitlements/roles for creating customer-account links (e.g., CanCreateCustomerAccountLink)
  - The target bank must exist on the platform
  - The specified customer must exist in the system under the specified bank
  - The specified account must exist in the system under the specified bank

- **Downstream**: 
  - After customer-account linking, the relationship enables:
    - Customer access to account information
    - Account ownership tracking and reporting
    - Customer relationship management
    - Compliance and regulatory reporting
    - Account access permissions based on relationship type

- **External Systems**: 
  - Database/persistence layer for storing customer-account link entities
  - Customer service for customer validation
  - Account service for account validation
  - Bank entity service for bank validation

## Notes for Implementation

- **Authorization**: Ensure proper role-based access control - only users with CanCreateCustomerAccountLink or similar entitlement should be able to create links
- **Customer Validation**: Verify that the specified customer exists and belongs to the specified bank
- **Account Validation**: Verify that the specified account exists and belongs to the specified bank
- **Duplicate Prevention**: Implement checks to prevent duplicate customer-account links for the same pair
- **Relationship Types**: Define and validate supported relationship types (owner, authorized_user, beneficiary, etc.)
- **Audit Trail**: Log customer-account linking events for compliance and audit purposes
- **Error Handling**: Provide clear, actionable error messages for validation failures
- **Idempotency**: Consider implementing idempotency for link creation to handle duplicate requests gracefully

### Open Questions (Needs SME Input)

1. What relationship types are supported for customer-account links (owner, authorized user, beneficiary, etc.)?
2. Can a customer be linked to multiple accounts?
3. Can an account have multiple customers linked to it?
4. Is there a limit on the number of customer-account links per customer or per account?
5. What is the default relationship type if not specified?
6. Are there any restrictions on which customers can be linked to which account types?
7. Should customer-account linking trigger any downstream notifications or events?
8. What happens to existing transactions/permissions when a customer-account link is created?
9. Is there a workflow or approval process required for certain relationship types?
10. How does customer-account linking interact with view/permission management?

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Bank Administrator / Customer Relationship Manager)
- [x] Business value is stated (enabling customer-account association for ownership tracking and account management)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (Open Questions section)
- [x] Only relevant endpoints are included (POST for linking only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific word from capability description justifies inclusion ("Link")
- [x] No endpoint type has been added unless its verb appears in the description
- [x] No CRUD operations inferred beyond what description explicitly states
