# User Story for View Update

## Story Overview
**As a** Bank Account Owner or Platform Administrator
**I want to** modify view permissions and configurations
**So that** I can adjust data access controls and visibility settings for custom and system views to meet changing business requirements and security policies

## Acceptance Criteria
1. The system shall allow authorized users to modify the permissions and configurations of existing custom views on bank accounts
2. The system shall allow platform administrators to modify system view configurations
3. The system shall accept updates to view description, metadata view reference, public/private visibility, alias settings, and allowed actions
4. The system shall validate all configuration changes before applying them
5. The system shall return the updated view details upon successful modification
6. The system shall reject modification requests with invalid or missing required data with appropriate error messages
7. The system shall prevent modification of the view name (name is only set during creation)
8. The system shall prevent system views from being set to public (security constraint)

## Technical Context
- **Classes/Services Involved**: 
  - `ViewNewStyle` - New style service for view operations
  - `Views.views.vend` - View vendor interface for persistence operations
  - `MapperViews` - Mapper implementation for view storage
  - `JSONFactory300`, `JSONFactory310` - JSON factories for response formatting
- **Input Data**: 
  - `UpdateViewJSON` containing: description, metadata_view, is_public, is_firehose (optional), which_alias_to_use, hide_metadata_if_alias_used, allowed_actions, can_grant_access_to_views (optional), can_revoke_access_to_views (optional)
- **Output Data**: 
  - Updated view object with all current settings including viewId, name, description, is_public, is_firehose, which_alias_to_use, hide_metadata_if_alias_used, allowed_actions
- **Processing Type**: API (HTTP request-response)

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description.

- **Endpoint**: PUT /obp/v3.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views/{VIEW_ID}
  - **Justification (from description)**: "Modify view permissions and configurations" - the word "Modify" explicitly justifies a PUT endpoint for updating custom views
  - **Purpose**: Update an existing custom view on a bank account
  - **Request**: 
    ```json
    {
      "description": "string",
      "metadata_view": "string (existing view ID, e.g., owner)",
      "is_public": false,
      "which_alias_to_use": "string (public/private/none)",
      "hide_metadata_if_alias_used": true,
      "allowed_actions": [
        "can_see_transaction_this_bank_account",
        "can_see_transaction_other_bank_account",
        "can_see_transaction_metadata",
        "can_see_transaction_description",
        "can_see_transaction_amount",
        "can_see_transaction_type",
        "can_see_transaction_currency",
        "can_see_transaction_start_date",
        "can_see_transaction_finish_date",
        "can_see_transaction_balance",
        "can_see_comments",
        "can_see_tags",
        "can_see_images",
        "can_see_bank_account_owners",
        "can_see_bank_account_type",
        "can_see_bank_account_balance",
        "can_see_bank_account_currency",
        "can_see_bank_account_label",
        "can_see_bank_account_number",
        "can_see_other_account_national_identifier",
        "can_see_other_account_iban",
        "can_see_other_account_bank_name",
        "can_see_other_account_number",
        "can_see_other_account_metadata",
        "can_add_comment",
        "can_add_tag",
        "can_add_image",
        "can_add_where_tag",
        "can_update_custom_view"
      ],
      "can_grant_access_to_views": ["_view1", "_view2"],
      "can_revoke_access_to_views": ["_view1", "_view2"]
    }
    ```
  - **Response**: 
    ```json
    {
      "id": "string",
      "short_name": "string",
      "description": "string",
      "metadata_view": "string",
      "is_public": false,
      "is_firehose": false,
      "alias": "string",
      "hide_metadata_if_alias_used": true,
      "can_add_comment": true,
      "can_add_corporate_location": true,
      "can_add_image": true,
      "can_add_image_url": true,
      "can_add_more_info": true,
      "can_add_open_corporates_url": true,
      "can_add_physical_location": true,
      "can_add_private_alias": true,
      "can_add_public_alias": true,
      "can_add_tag": true,
      "can_add_url": true,
      "can_add_where_tag": true,
      "can_delete_comment": true,
      "can_delete_corporate_location": true,
      "can_delete_image": true,
      "can_delete_physical_location": true,
      "can_delete_tag": true,
      "can_delete_where_tag": true,
      "can_edit_owner_comment": true,
      "can_see_bank_account_balance": true,
      "can_see_bank_account_bank_name": true,
      "can_see_bank_account_currency": true,
      "can_see_bank_account_iban": true,
      "can_see_bank_account_label": true,
      "can_see_bank_account_national_identifier": true,
      "can_see_bank_account_number": true,
      "can_see_bank_account_owners": true,
      "can_see_bank_account_swift_bic": true,
      "can_see_bank_account_type": true,
      "can_see_comments": true,
      "can_see_corporate_location": true,
      "can_see_image_url": true,
      "can_see_images": true,
      "can_see_more_info": true,
      "can_see_open_corporates_url": true,
      "can_see_other_account_bank_name": true,
      "can_see_other_account_iban": true,
      "can_see_other_account_kind": true,
      "can_see_other_account_metadata": true,
      "can_see_other_account_national_identifier": true,
      "can_see_other_account_number": true,
      "can_see_other_account_swift_bic": true,
      "can_see_owner_comment": true,
      "can_see_physical_location": true,
      "can_see_private_alias": true,
      "can_see_public_alias": true,
      "can_see_tags": true,
      "can_see_transaction_amount": true,
      "can_see_transaction_balance": true,
      "can_see_transaction_currency": true,
      "can_see_transaction_description": true,
      "can_see_transaction_finish_date": true,
      "can_see_transaction_metadata": true,
      "can_see_transaction_other_bank_account": true,
      "can_see_transaction_start_date": true,
      "can_see_transaction_this_bank_account": true,
      "can_see_transaction_type": true,
      "can_see_url": true,
      "can_see_where_tag": true
    }
    ```

- **Endpoint**: PUT /obp/v3.1.0/system-views/{VIEW_ID}
  - **Justification (from description)**: "Modify view permissions and configurations" - the word "Modify" explicitly justifies a PUT endpoint for updating system views
  - **Purpose**: Update an existing system view's permissions and configurations
  - **Request**: 
    ```json
    {
      "description": "string",
      "metadata_view": "string",
      "is_public": false,
      "is_firehose": false,
      "which_alias_to_use": "string",
      "hide_metadata_if_alias_used": true,
      "allowed_actions": [
        "can_see_transaction_this_bank_account",
        "can_see_transaction_other_bank_account",
        "can_see_bank_account_balance",
        "can_see_bank_account_owners"
      ],
      "can_grant_access_to_views": ["owner", "accountant"],
      "can_revoke_access_to_views": ["owner", "accountant"]
    }
    ```
  - **Response**: 
    ```json
    {
      "id": "string",
      "short_name": "string",
      "description": "string",
      "metadata_view": "string",
      "is_public": false,
      "is_firehose": false,
      "alias": "string",
      "hide_metadata_if_alias_used": true,
      "can_add_comment": true,
      "can_see_bank_account_balance": true,
      "can_see_transaction_amount": true
    }
    ```

- **Endpoint**: PUT /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views/{VIEW_ID}
  - **Justification (from description)**: "Modify view permissions and configurations" - the word "Modify" explicitly justifies a PUT endpoint for updating custom views (latest API version)
  - **Purpose**: Update an existing custom view on a bank account using the latest API version with extended configuration options
  - **Request**: 
    ```json
    {
      "description": "string",
      "metadata_view": "string",
      "is_public": false,
      "which_alias_to_use": "string",
      "hide_metadata_if_alias_used": true,
      "allowed_actions": ["can_see_transaction_this_bank_account", "can_add_comment"],
      "can_grant_access_to_views": ["_custom_view1"],
      "can_revoke_access_to_views": ["_custom_view1"]
    }
    ```
  - **Response**: Updated view JSON with all permission flags

## Business Rules (from capability description)
1. Only the view owner or users with appropriate permissions (CAN_UPDATE_CUSTOM_VIEW) can modify custom views
2. Only platform administrators with canUpdateSystemView entitlement can modify system views
3. The view name cannot be changed after creation - only other configurations can be modified
4. System views cannot be set to public (is_public must be false for system views)
5. Custom views must have names starting with underscore (e.g., _myview) to distinguish from system views
6. The metadata_view field must reference an existing valid view ID
7. Allowed actions must be from the predefined list of valid action strings
8. Changes to view permissions take effect immediately upon successful update

## Data Validations (if applicable)
- View ID must exist and be accessible to the requesting user
- metadata_view must reference an existing view (e.g., owner, accountant, or custom view)
- is_public must be a boolean value (false required for system views)
- which_alias_to_use must be one of: "public", "private", or "none"
- hide_metadata_if_alias_used must be a boolean value
- allowed_actions must be a list of valid action strings from the predefined set
- can_grant_access_to_views and can_revoke_access_to_views must reference valid view IDs if provided
- Custom view names must start with underscore character
- Request JSON must conform to UpdateViewJSON schema

## Dependencies
- **Upstream**: 
  - User must be authenticated with valid OAuth credentials
  - For custom views: User must have access to the owner view or have CAN_UPDATE_CUSTOM_VIEW permission
  - For system views: User must have canUpdateSystemView entitlement
  - The view being updated must already exist
  - The bank account (for custom views) must exist and be accessible
- **Downstream**: 
  - Updated view permissions immediately affect data visibility for all users with access to that view
  - Changes may affect transaction metadata visibility based on metadata_view setting
  - Alias settings affect how counterparty information is displayed
  - Grant/revoke access permissions affect what other views users can manage
- **External Systems**: 
  - No direct external system dependencies for view update operations
  - View changes may affect data exposed through third-party applications using the API

## Notes for Implementation
- The capability description mentions "Modify view permissions and configurations" which maps directly to UPDATE operations only
- No retrieval (GET), creation (POST), or deletion (DELETE) endpoints are included as these operations are not mentioned in the capability description - these would fall under "View Retrieval", "View Creation", or "View Deletion" capabilities respectively
- The allowed_actions list is extensive and controls granular permissions for what data can be seen and what actions can be performed through the view
- Consider implementing optimistic locking or versioning to handle concurrent update scenarios
- View updates should be logged for audit purposes given their security implications
- The is_firehose flag is optional and controls whether the view can be used for bulk data access
- SME input may be needed to define the complete list of valid allowed_actions strings
- Consider implementing validation to ensure users cannot grant themselves permissions they don't already have

## Quality Checklist Verification
- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Bank Account Owner/Platform Administrator)
- [x] Business value is stated (adjust data access controls and visibility settings)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (complete allowed_actions list, concurrent update handling)
- [x] Only relevant endpoints are included (PUT for modify operations only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words from the capability description justify inclusion ("Modify" justifies PUT endpoints)
- [x] No endpoint type has been added unless its verb appears in the description
- [x] "Modify" has been interpreted as update/configure operations only - no view/list/delete operations included
