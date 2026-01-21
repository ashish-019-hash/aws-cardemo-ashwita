# User Story for View Retrieval

## Story Overview
**As a** system administrator or authorized user
**I want to** retrieve view definitions and permissions
**So that** I can understand what data access permissions are configured for accounts and ensure proper access control is in place

## Acceptance Criteria
1. The system shall allow authorized users to retrieve view definitions for a specific account
2. The system shall return view permission configurations including what data elements are accessible
3. The system shall support retrieving views by bank and account identifiers
4. The system shall return appropriate error responses when views are not found or access is denied
5. The system shall support retrieving both custom views and system-defined views
6. The system shall return view metadata including view name, description, and associated permissions

## Technical Context
- **Classes/Services Involved**: View service/controller handling view retrieval operations
- **Input Data**: Bank ID, Account ID, View ID (path parameters); authentication token (header)
- **Output Data**: View definition object containing view ID, name, description, and permission flags
- **Processing Type**: API/Real-time

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by the capability description which states "Retrieve view definitions and permissions".

- **Endpoint**: GET /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views
  - **Justification (from description)**: "Retrieve view definitions" - the word "retrieve" justifies this GET endpoint for listing views
  - **Purpose**: Retrieve all view definitions available for a specific account at a bank
  - **Request**: 
    - Path Parameters: BANK_ID (string), ACCOUNT_ID (string)
    - Headers: Authorization token
  - **Response**: 
    ```json
    {
      "views": [
        {
          "id": "string",
          "short_name": "string",
          "description": "string",
          "is_public": "boolean",
          "alias": "string",
          "hide_metadata_if_alias_used": "boolean",
          "can_see_transaction_this_bank_account": "boolean",
          "can_see_transaction_other_bank_account": "boolean",
          "can_see_transaction_metadata": "boolean",
          "can_see_transaction_label": "boolean",
          "can_see_transaction_amount": "boolean",
          "can_see_transaction_type": "boolean",
          "can_see_transaction_currency": "boolean",
          "can_see_transaction_start_date": "boolean",
          "can_see_transaction_finish_date": "boolean",
          "can_see_transaction_balance": "boolean",
          "can_see_comments": "boolean",
          "can_see_narrative": "boolean",
          "can_see_tags": "boolean",
          "can_see_images": "boolean",
          "can_see_bank_account_owners": "boolean",
          "can_see_bank_account_type": "boolean",
          "can_see_bank_account_balance": "boolean",
          "can_see_bank_account_currency": "boolean",
          "can_see_bank_account_label": "boolean",
          "can_see_bank_account_national_identifier": "boolean",
          "can_see_bank_account_swift_bic": "boolean",
          "can_see_bank_account_iban": "boolean",
          "can_see_bank_account_number": "boolean",
          "can_see_bank_account_bank_name": "boolean",
          "can_see_other_account_national_identifier": "boolean",
          "can_see_other_account_swift_bic": "boolean",
          "can_see_other_account_iban": "boolean",
          "can_see_other_account_bank_name": "boolean",
          "can_see_other_account_number": "boolean",
          "can_see_other_account_metadata": "boolean",
          "can_see_other_account_kind": "boolean",
          "can_see_more_info": "boolean",
          "can_see_url": "boolean",
          "can_see_image_url": "boolean",
          "can_see_open_corporates_url": "boolean",
          "can_see_corporate_location": "boolean",
          "can_see_physical_location": "boolean",
          "can_see_public_alias": "boolean",
          "can_see_private_alias": "boolean",
          "can_add_more_info": "boolean",
          "can_add_url": "boolean",
          "can_add_image_url": "boolean",
          "can_add_open_corporates_url": "boolean",
          "can_add_corporate_location": "boolean",
          "can_add_physical_location": "boolean",
          "can_add_public_alias": "boolean",
          "can_add_private_alias": "boolean",
          "can_delete_corporate_location": "boolean",
          "can_delete_physical_location": "boolean",
          "can_edit_narrative": "boolean",
          "can_add_comment": "boolean",
          "can_delete_comment": "boolean",
          "can_add_tag": "boolean",
          "can_delete_tag": "boolean",
          "can_add_image": "boolean",
          "can_delete_image": "boolean",
          "can_add_where_tag": "boolean",
          "can_see_where_tag": "boolean",
          "can_delete_where_tag": "boolean",
          "can_create_counterparty": "boolean",
          "can_see_bank_routing_scheme": "boolean",
          "can_see_bank_routing_address": "boolean",
          "can_see_bank_account_routing_scheme": "boolean",
          "can_see_bank_account_routing_address": "boolean",
          "can_see_other_bank_routing_scheme": "boolean",
          "can_see_other_bank_routing_address": "boolean",
          "can_see_other_account_routing_scheme": "boolean",
          "can_see_other_account_routing_address": "boolean"
        }
      ]
    }
    ```

- **Endpoint**: GET /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views/{VIEW_ID}
  - **Justification (from description)**: "Retrieve view definitions and permissions" - the word "retrieve" justifies this GET endpoint for fetching a specific view
  - **Purpose**: Retrieve a specific view definition and its permissions for an account
  - **Request**: 
    - Path Parameters: BANK_ID (string), ACCOUNT_ID (string), VIEW_ID (string)
    - Headers: Authorization token
  - **Response**: 
    ```json
    {
      "id": "string",
      "short_name": "string",
      "description": "string",
      "is_public": "boolean",
      "alias": "string",
      "hide_metadata_if_alias_used": "boolean",
      "can_see_transaction_this_bank_account": "boolean",
      "can_see_transaction_other_bank_account": "boolean",
      ... (all permission flags as shown above)
    }
    ```

- **Endpoint**: GET /banks/{BANK_ID}/views
  - **Justification (from description)**: "Retrieve view definitions" - the word "retrieve" justifies this GET endpoint for listing views at bank level
  - **Purpose**: Retrieve all view definitions available at a bank level
  - **Request**: 
    - Path Parameters: BANK_ID (string)
    - Headers: Authorization token
  - **Response**: 
    ```json
    {
      "views": [
        {
          "id": "string",
          "short_name": "string",
          "description": "string",
          "is_public": "boolean",
          ... (view definition fields)
        }
      ]
    }
    ```

## Business Rules
1. Only authenticated users with appropriate permissions can retrieve view definitions
2. View retrieval is scoped to specific banks and accounts based on user authorization
3. System views and custom views may have different retrieval permissions
4. View permissions define granular access to account and transaction data elements
5. Public views may be accessible to a broader set of users than private views

## Data Validations
- Bank ID must be a valid, existing bank identifier
- Account ID must be a valid account associated with the specified bank
- View ID must be a valid view identifier when retrieving a specific view
- User must have appropriate authorization to access the requested view
- Request must include valid authentication credentials

## Dependencies
- **Upstream**: User authentication and authorization must be completed before view retrieval
- **Downstream**: Retrieved view definitions are used to determine data access when fetching account/transaction details
- **External Systems**: None explicitly mentioned in the capability description

## Notes for Implementation
- Consider caching frequently accessed view definitions for performance optimization given the "High" volume and "Real-time" frequency
- Implement proper error handling for cases where views do not exist or user lacks access
- Ensure view permission flags are consistently applied across all data retrieval operations
- **Needs SME Input**: Clarify the exact relationship between system views and custom views
- **Needs SME Input**: Determine if there are any rate limiting requirements for view retrieval given the high volume
- **Needs SME Input**: Clarify if view retrieval should support filtering or pagination for accounts with many views
