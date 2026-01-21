# User Story for View Creation

## Story Overview
**As a** Bank Administrator or Account Access Manager
**I want to** create custom views defining data access permissions
**So that** I can control what data fields and operations are visible and accessible to different users or applications when they access account information

## Acceptance Criteria
1. The system shall allow authorized users to create a new custom view with specified data access permissions
2. The system shall accept view configuration parameters that define which data fields are accessible
3. The system shall accept permission settings that define what operations can be performed through the view
4. The system shall validate all required fields before creating the view
5. The system shall return a confirmation with the created view details upon successful creation
6. The system shall reject creation requests with invalid or missing required data with appropriate error messages
7. The system shall ensure view names are unique within the scope of the bank/account

## Technical Context
- **Classes/Services Involved**: View management service, permission configuration service, access control service
- **Input Data**: View name, description, permission flags (can_see_*, can_add_*, can_edit_*, can_delete_*), metadata fields configuration
- **Output Data**: Created view entity with assigned identifier, confirmation response with view details
- **Processing Type**: API (HTTP request-response)

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description.

- **Endpoint**: POST /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views
  - **Justification (from description)**: "Create custom views" - the word "Create" explicitly justifies a POST endpoint
  - **Purpose**: Create a new custom view for a specific account at a bank, defining data access permissions
  - **Request**: 
    ```json
    {
      "name": "string",
      "description": "string",
      "metadata_view": "string",
      "is_public": "boolean",
      "which_alias_to_use": "string",
      "hide_metadata_if_alias_used": "boolean",
      "allowed_actions": [
        "string"
      ],
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
      "can_add_counterparty": "boolean",
      "can_add_tag": "boolean",
      "can_add_image": "boolean",
      "can_add_narrative": "boolean",
      "can_add_comment": "boolean",
      "can_delete_corporate_location": "boolean",
      "can_delete_physical_location": "boolean",
      "can_edit_narrative": "boolean",
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
      "can_see_other_account_routing_address": "boolean",
      "can_query_available_funds": "boolean",
      "can_add_transaction_request_to_own_account": "boolean",
      "can_add_transaction_request_to_any_account": "boolean",
      "can_see_bank_account_credit_limit": "boolean"
    }
    ```
  - **Response**: 
    ```json
    {
      "id": "string",
      "short_name": "string",
      "description": "string",
      "metadata_view": "string",
      "is_public": "boolean",
      "alias": "string",
      "hide_metadata_if_alias_used": "boolean",
      "can_add_comment": "boolean",
      "can_add_corporate_location": "boolean",
      "can_add_image": "boolean",
      "can_add_image_url": "boolean",
      "can_add_more_info": "boolean",
      "can_add_open_corporates_url": "boolean",
      "can_add_physical_location": "boolean",
      "can_add_private_alias": "boolean",
      "can_add_public_alias": "boolean",
      "can_add_tag": "boolean",
      "can_add_url": "boolean",
      "can_add_where_tag": "boolean",
      "can_delete_comment": "boolean",
      "can_delete_corporate_location": "boolean",
      "can_delete_image": "boolean",
      "can_delete_physical_location": "boolean",
      "can_delete_tag": "boolean",
      "can_delete_where_tag": "boolean",
      "can_edit_owner_comment": "boolean",
      "can_see_bank_account_balance": "boolean",
      "can_see_bank_account_bank_name": "boolean",
      "can_see_bank_account_currency": "boolean",
      "can_see_bank_account_iban": "boolean",
      "can_see_bank_account_label": "boolean",
      "can_see_bank_account_national_identifier": "boolean",
      "can_see_bank_account_number": "boolean",
      "can_see_bank_account_owners": "boolean",
      "can_see_bank_account_swift_bic": "boolean",
      "can_see_bank_account_type": "boolean",
      "can_see_comments": "boolean",
      "can_see_corporate_location": "boolean",
      "can_see_image_url": "boolean",
      "can_see_images": "boolean",
      "can_see_more_info": "boolean",
      "can_see_open_corporates_url": "boolean",
      "can_see_other_account_bank_name": "boolean",
      "can_see_other_account_iban": "boolean",
      "can_see_other_account_kind": "boolean",
      "can_see_other_account_metadata": "boolean",
      "can_see_other_account_national_identifier": "boolean",
      "can_see_other_account_number": "boolean",
      "can_see_other_account_swift_bic": "boolean",
      "can_see_owner_comment": "boolean",
      "can_see_physical_location": "boolean",
      "can_see_private_alias": "boolean",
      "can_see_public_alias": "boolean",
      "can_see_tags": "boolean",
      "can_see_transaction_amount": "boolean",
      "can_see_transaction_balance": "boolean",
      "can_see_transaction_currency": "boolean",
      "can_see_transaction_description": "boolean",
      "can_see_transaction_finish_date": "boolean",
      "can_see_transaction_metadata": "boolean",
      "can_see_transaction_other_bank_account": "boolean",
      "can_see_transaction_start_date": "boolean",
      "can_see_transaction_this_bank_account": "boolean",
      "can_see_transaction_type": "boolean",
      "can_see_url": "boolean",
      "can_see_where_tag": "boolean"
    }
    ```

## Business Rules (from capability description)
1. Each custom view must have a unique name within the scope of the bank/account
2. Views define granular data access permissions through boolean flags for each data field
3. Views can be configured as public or private
4. Permission flags control visibility of transaction details, account information, and metadata
5. Permission flags control ability to add, edit, or delete various data elements
6. Only authorized bank administrators or account managers can create custom views
7. Views serve as the foundation for controlling what data third-party applications can access

## Data Validations (if applicable)
- View name is required and must be unique within the account scope
- View name must follow naming conventions (alphanumeric, limited special characters)
- Description field is optional but recommended for clarity
- All permission flags must be valid boolean values
- Alias settings must be valid if alias functionality is enabled
- Metadata view reference must be valid if specified

## Dependencies
- **Upstream**: 
  - User must be authenticated with appropriate bank administrator or account manager entitlements
  - Bank entity must exist in the system
  - Account must exist within the bank
  - Platform must be operational and accepting API requests
- **Downstream**: 
  - Once created, the view can be granted to users via View Access Grant capability
  - The view controls data visibility when users access account information through the granted view
  - Third-party applications use views to determine what data they can access
- **External Systems**: 
  - None explicitly mentioned in the capability description

## Notes for Implementation
- The capability description focuses specifically on "Create" operation - retrieval (GET), update (PUT), and deletion (DELETE) of views are covered by separate capabilities (View Retrieval, View Update, View Deletion)
- The extensive list of permission flags (can_see_*, can_add_*, can_edit_*, can_delete_*) provides fine-grained access control
- Consider implementing default permission templates to simplify view creation for common use cases (Needs SME Input)
- The relationship between views and the consent management system should be clarified (Needs SME Input)
- Consider implementing validation to prevent creation of views with no permissions enabled
- The "defining data access permissions" phrase in the description indicates the primary purpose is access control configuration

## Quality Checklist Verification
- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Bank Administrator/Account Access Manager)
- [x] Business value is stated (controlling data access for users and applications)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (default templates, consent relationship)
- [x] Only relevant endpoints are included (POST for create only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words from the capability description justify inclusion ("Create" justifies POST)
- [x] No endpoint type has been added unless its verb appears in the description
- [x] "Defining" has been interpreted as part of the creation process, not a separate operation
