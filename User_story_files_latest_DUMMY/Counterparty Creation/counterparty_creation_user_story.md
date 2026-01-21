# User Story for Counterparty Creation

## Story Overview

**As a** Bank Account Owner / Account Manager  
**I want to** create counterparty records for payment recipients  
**So that** I can register and store information about external parties (beneficiaries) to whom I need to make payments, enabling streamlined payment initiation through pre-configured recipient details

## Acceptance Criteria

1. The system shall allow authorized users to create a new counterparty record for a specific bank account
2. The system shall accept and validate counterparty details including name, description, and routing information
3. The system shall associate the counterparty with a specific bank account and view
4. The system shall generate a unique counterparty identifier for each newly created counterparty
5. The system shall validate that the counterparty name is unique within the context of the bank/account/view combination
6. The system shall validate routing information when OBP routing schemes are used (bank and account must exist)
7. The system shall support custom bespoke key-value attributes for extended counterparty metadata
8. The system shall return confirmation of successful counterparty creation with the created counterparty details and metadata
9. The system shall reject creation requests with invalid or incomplete data with appropriate error messages
10. The system shall enforce view-level permissions (canAddCounterparty) for counterparty creation

## Technical Context

- **Classes/Services Involved**: 
  - Counterparty entity/model classes (CounterpartyTrait, MappedCounterparty)
  - Counterparty creation service (Counterparties.counterparties.vend.createCounterparty)
  - Counterparty metadata service (getOrCreateMetadata)
  - View permission validation service
  - Bank and account validation services
  - Database/persistence layer for counterparty storage

- **Input Data**: 
  - Bank ID (required) - the bank where the account resides
  - Account ID (required) - the account for which the counterparty is being created
  - View ID (required) - the view context for the counterparty
  - name (required) - human readable name for the counterparty (e.g., "Piano teacher", "Miss Nipa")
  - description (required) - description of the counterparty, max 36 characters
  - other_bank_routing_scheme - routing scheme for the counterparty's bank (e.g., "OBP", "bankId", "bankCode")
  - other_bank_routing_address - routing address for the counterparty's bank
  - other_account_routing_scheme - routing scheme for the counterparty's account (e.g., "OBP", "AccountId", "AccountNumber")
  - other_account_routing_address - routing address for the counterparty's account
  - other_account_secondary_routing_scheme - secondary routing scheme (e.g., "IBAN")
  - other_account_secondary_routing_address - secondary routing address (must be unique if IBAN)
  - other_branch_routing_scheme - branch routing scheme (optional)
  - other_branch_routing_address - branch routing address (optional)
  - is_beneficiary (required) - must be true to enable payments to this counterparty
  - bespoke - list of custom key-value pairs for extended metadata

- **Output Data**: 
  - Created counterparty entity with generated counterparty_id
  - Display information (name)
  - created_by_user_id
  - this_account reference
  - other_account_routing details
  - other_bank_routing details
  - Counterparty metadata (public_alias, private_alias, more_info, URL, image_URL, open_corporates_URL, corporate_location, physical_location)
  - Success/error response with HTTP 201 on success

- **Processing Type**: API / On-demand

## Relevant Endpoints

**IMPORTANT**: For every endpoint included, justification from the description is provided.

### Endpoint 1: Create Counterparty (Explicit)
- **Endpoint**: `POST /obp/v2.2.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/counterparties`
  - **Justification (from description)**: "Create counterparty records" - the word "Create" explicitly justifies a POST endpoint for counterparty creation
  - **Purpose**: Create a new explicit counterparty record for a specific bank account and view, enabling the account owner to register payment recipients for future transactions
  - **Request**: 
    ```json
    {
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
      "is_beneficiary": true,
      "bespoke": [
        {
          "key": "string",
          "value": "string"
        }
      ]
    }
    ```
  - **Response**: 
    ```json
    {
      "counterparty_id": "string",
      "display": {
        "name": "string"
      },
      "created_by_user_id": "string",
      "this_account": {
        "bank_id": "string",
        "account_id": "string"
      },
      "other_account_routing": {
        "scheme": "string",
        "address": "string"
      },
      "other_bank_routing": {
        "scheme": "string",
        "address": "string"
      },
      "metadata": {
        "public_alias": "string",
        "private_alias": "string",
        "more_info": "string",
        "URL": "string",
        "image_URL": "string",
        "open_corporates_URL": "string",
        "corporate_location": {
          "latitude": 0.0,
          "longitude": 0.0
        },
        "physical_location": {
          "latitude": 0.0,
          "longitude": 0.0
        }
      }
    }
    ```

**Note**: Based on the Operation Derivation Rules, the following endpoints are explicitly EXCLUDED because their operation types are not mentioned in the capability description:
- GET /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/counterparties - No "view", "retrieve", "list", or "get" mentioned
- GET /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/counterparties/{COUNTERPARTY_ID} - No "view" or "retrieve" mentioned
- PUT /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/counterparties/{COUNTERPARTY_ID} - No "update" or "modify" mentioned
- DELETE /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/counterparties/{COUNTERPARTY_ID} - No "delete" or "remove" mentioned

## Business Rules (from capability description)

1. **Account-View Association**: Each counterparty must be created under a specific bank account and view context
2. **Unique Naming**: Counterparty name must be unique within the combination of bank ID, account ID, and view ID
3. **Beneficiary Flag**: The is_beneficiary field must be set to true to enable payments to the counterparty
4. **Description Length**: Description field has a maximum length of 36 characters
5. **OBP Routing Validation**: When using OBP routing schemes, the referenced bank and account must exist in the system
6. **View Permission Required**: The view must have the canAddCounterparty permission for the user to create counterparties
7. **On-demand Processing**: Counterparty creation is performed on-demand (not batch or scheduled)
8. **Medium Volume Operation**: Counterparty creation is expected to be a medium-volume operation

## Data Validations (if applicable)

- Bank ID must reference an existing bank on the platform
- Account ID must reference an existing account at the specified bank
- View ID must reference a valid view with canAddCounterparty permission
- Counterparty name must not be empty and must be unique per bank/account/view
- Description must not exceed 36 characters
- If other_bank_routing_scheme is "OBP", other_bank_routing_address must be a valid bank ID
- If other_account_routing_scheme is "OBP", other_account_routing_address must be a valid account ID
- If other_account_secondary_routing_scheme is "IBAN", the IBAN should be unique for each counterparty
- is_beneficiary must be a valid boolean value
- Bespoke key-value pairs must have non-empty keys

## Dependencies

- **Upstream**: 
  - User authentication and authorization must be completed
  - User must have access to the specified view
  - The view must have canAddCounterparty permission enabled
  - The target bank and account must exist on the platform
  - For OBP routing schemes, the referenced counterparty bank/account must exist

- **Downstream**: 
  - After counterparty creation, the counterparty becomes available for:
    - COUNTERPARTY type transaction requests (payments)
    - SEPA payments (when IBAN is configured)
    - Counterparty metadata management (adding aliases, URLs, locations)
    - Payment initiation workflows

- **External Systems**: 
  - Database/persistence layer for storing counterparty entities
  - Bank entity service for bank validation (when OBP routing is used)
  - Account service for account validation (when OBP routing is used)
  - Metadata service for counterparty metadata creation

## Notes for Implementation

- **Authorization**: Ensure proper view-based access control - only users with access to views that have canAddCounterparty permission can create counterparties
- **Uniqueness Validation**: Verify that the counterparty name is unique within the bank/account/view context before creation
- **Routing Validation**: When OBP routing schemes are used, validate that the referenced bank and account exist
- **Metadata Creation**: Automatically create counterparty metadata upon successful counterparty creation
- **Explicit vs Implicit**: This capability creates "Explicit" counterparties (user-defined), as opposed to "Implicit" counterparties which are auto-generated from transaction history
- **Payment Enablement**: Ensure is_beneficiary is properly set to enable payment functionality
- **Error Handling**: Provide clear, actionable error messages for validation failures (CounterpartyAlreadyExists, InvalidAccountIdFormat, InvalidBankIdFormat, etc.)
- **Bespoke Attributes**: Support flexible key-value storage for custom counterparty attributes

### Open Questions (Needs SME Input)

1. What are the mandatory vs optional routing fields for counterparty creation?
2. Are there any restrictions on which routing schemes are supported beyond OBP?
3. Should counterparty creation trigger any downstream notifications or events?
4. What validation rules apply to IBAN format when used as secondary routing?
5. Is there a limit on the number of counterparties that can be created per account?
6. What is the expected behavior when creating a counterparty with the same routing information but different names?
7. Should bespoke attributes have any validation rules or size limits?
8. Are there any regulatory requirements for counterparty data storage (e.g., PSD2 compliance)?
9. How should the system handle counterparty creation for accounts with multiple views?
10. What audit trail requirements exist for counterparty creation events?

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Bank Account Owner / Account Manager)
- [x] Business value is stated (enabling streamlined payment initiation through pre-configured recipient details)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (Open Questions section)
- [x] Only relevant endpoints are included (POST for creation only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific word from capability description justifies inclusion ("Create")
- [x] No endpoint type has been added unless its verb appears in the description
- [x] Words like "manage" have been interpreted narrowly - no view/list/delete operations included since not explicitly mentioned
- [x] No CRUD operations inferred beyond what description explicitly states
