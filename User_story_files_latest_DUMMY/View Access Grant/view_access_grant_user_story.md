# User Story for View Access Grant

## Capability Input

- **Name**: View Access Grant
- **Description**: Grant user access to specific views on accounts
- **Frequency**: On-demand
- **Volume**: Medium

---

## Operation Verb Analysis

Based on the capability description, the following operation verbs are identified:

| Verb Found | Operation Type | Justification |
|------------|---------------|---------------|
| "Grant" | CREATE | Explicitly stated: "Grant user access to specific views on accounts" |

**Operations NOT included** (verbs not present in description):
- READ/RETRIEVAL operations: No "view", "retrieve", "get", "see", "display", "browse", "search", "list", "query", "lookup", "find", "show", "read", "access", or "fetch" mentioned in the context of retrieving access grants
- UPDATE operations: No "manage", "configure", "maintain", "update", "modify", "change", "edit", "adjust", "set", or "customize" mentioned
- DELETE operations: No "delete", "remove", "deactivate", "close", "terminate", "disable", "archive", "retire", "cancel", or "revoke" mentioned

---

## Story Overview

**As a** bank administrator or account owner on the Open Bank Project platform
**I want to** grant user access to specific views on accounts
**So that** I can control which users have permission to see specific data views on bank accounts, enabling fine-grained access control and data sharing with authorized third parties while maintaining security and compliance

---

## Acceptance Criteria

1. The system shall allow granting a user access to a specific view on a specific account
2. The system shall require valid user identification (user_id) to grant access
3. The system shall require valid bank identification (bank_id) to specify the bank context
4. The system shall require valid account identification (account_id) to specify which account the view access applies to
5. The system shall require valid view identification (view_id) to specify which view to grant access to
6. The system shall validate that the requesting user has permission to grant view access on the specified account
7. The system shall return a confirmation response upon successful access grant
8. The system shall return appropriate error responses when:
   - The specified user does not exist
   - The specified bank does not exist
   - The specified account does not exist
   - The specified view does not exist
   - The requesting user lacks permission to grant access
   - The target user already has access to the specified view
9. The system shall support on-demand access grant operations with medium volume capacity

---

## Technical Context

- **Classes/Services Involved** (from Scala source code):
  - `APIMethods400` / `APIMethods300` / `APIMethods210` - REST endpoint definitions for view access grant operations
  - `Views` (code.views.Views) - Service layer for view permission management
  - `NewStyle.function` (code.api.util.NewStyle) - Service layer with view access grant methods
  - `Connector` (code.bankconnectors.Connector) - Backend connector abstraction for permission data access
  - `ViewJsonV300` / `ViewsJsonV300` - Case classes defining response structure for view access

- **Input Data**: 
  - For granting view access (`POST /banks/BANK_ID/accounts/ACCOUNT_ID/permissions/PROVIDER/PROVIDER_ID/views/VIEW_ID`):
    - `BANK_ID` (path parameter) - Bank identifier
    - `ACCOUNT_ID` (path parameter) - Account identifier
    - `PROVIDER` (path parameter) - User provider (e.g., "obp")
    - `PROVIDER_ID` (path parameter) - User provider ID (user identifier)
    - `VIEW_ID` (path parameter) - View identifier to grant access to

- **Output Data** (based on view access response):
  - `view` object containing:
    - `id` (String) - View identifier
    - `short_name` (String) - Short display name of the view
    - `description` (String) - Description of what the view provides access to
    - `is_public` (Boolean) - Whether the view is public
    - `alias` (String) - View alias
    - `hide_metadata_if_alias_used` (Boolean) - Metadata hiding flag
    - `can_*` (Boolean fields) - Various permission flags indicating what the view allows

- **Processing Type**: API / On-demand / Synchronous request-response

---

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description. Only CREATE operations are included as the description only contains the verb "Grant".

### Endpoint 1: Grant User Access to View

- **Endpoint**: `POST /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/permissions/PROVIDER/PROVIDER_ID/views/VIEW_ID`
  - **Justification (from description)**: "Grant user access to specific views on accounts"
  - **Purpose**: Grant a specific user access to a specific view on a specific account, enabling them to see account data through that view's permissions
  - **Scala Implementation**: `APIMethods400.grantUserAccessToView` -> `Views.grantAccessToView()` or `NewStyle.function.grantAccessToView()`
  - **Request**: 
    ```
    POST /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/permissions/PROVIDER/PROVIDER_ID/views/VIEW_ID
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      ACCOUNT_ID: The unique identifier of the account (required)
      PROVIDER: The user provider, e.g., "obp" (required)
      PROVIDER_ID: The user's provider ID / user identifier (required)
      VIEW_ID: The view identifier to grant access to (required)
    Body: Empty or minimal JSON object
    ```
  - **Response** (based on ViewJsonV300 case class): 
    ```json
    {
      "view": {
        "id": "owner",
        "short_name": "Owner",
        "description": "Full access to account data",
        "metadata_view": "owner",
        "is_public": false,
        "is_system": true,
        "alias": "",
        "hide_metadata_if_alias_used": false,
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
        "can_add_counterparty": true,
        "can_add_transaction_request_to_own_account": true,
        "can_add_transaction_request_to_any_account": true,
        "can_delete_comment": true,
        "can_delete_corporate_location": true,
        "can_delete_image": true,
        "can_delete_physical_location": true,
        "can_delete_tag": true,
        "can_delete_where_tag": true,
        "can_edit_owner_comment": true,
        "can_see_bank_account_balance": true,
        "can_query_available_funds": true,
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
        "can_see_where_tag": true,
        "can_create_direct_debit": true,
        "can_create_standing_order": true
      }
    }
    ```

### Endpoints NOT Included (with justification)

| Endpoint | Operation Type | Reason for Exclusion |
|----------|---------------|---------------------|
| GET /banks/{bank_id}/accounts/{account_id}/permissions | READ | No "view", "retrieve", "list", "get", or similar verb in description |
| GET /banks/{bank_id}/accounts/{account_id}/permissions/{provider}/{provider_id}/views | READ | No "view", "retrieve", "list", "get", or similar verb in description |
| PUT /banks/{bank_id}/accounts/{account_id}/permissions/{provider}/{provider_id}/views/{view_id} | UPDATE | No "manage", "update", "configure", "modify", or similar verb in description |
| DELETE /banks/{bank_id}/accounts/{account_id}/permissions/{provider}/{provider_id}/views/{view_id} | DELETE | No "delete", "remove", "revoke", or similar verb in description (Note: View Access Revocation is a separate capability #68) |

---

## Business Rules (from capability description)

1. **User Targeting**: Access must be granted to a specific user identified by provider and provider_id (from: "Grant user access")
2. **View Specificity**: Access is granted to specific views, not blanket account access (from: "specific views")
3. **Account Scope**: View access is scoped to specific accounts, not bank-wide (from: "on accounts")
4. **On-demand Processing**: Access grants are processed on-demand as needed, not in batch (from: Frequency = On-demand)
5. **Medium Volume Support**: The system must handle medium volume of access grant requests (from: Volume = Medium)
6. **Authorization Required**: Only authorized users (account owners, administrators) can grant view access to others

---

## Data Validations (if applicable)

- Bank identifier (BANK_ID) must be valid and exist in the system
- Account identifier (ACCOUNT_ID) must be valid and exist within the specified bank
- User provider (PROVIDER) must be a valid provider type (e.g., "obp")
- User provider ID (PROVIDER_ID) must correspond to an existing user
- View identifier (VIEW_ID) must be valid and exist for the specified account
- The requesting user must have permission to grant access (typically account owner or administrator)
- Error response (HTTP 404 Not Found) must be returned when:
  - Bank does not exist (`BankNotFound`)
  - Account does not exist (`AccountNotFound`)
  - User does not exist (`UserNotFound`)
  - View does not exist (`ViewNotFound`)
- Error response (HTTP 403 Forbidden) must be returned when:
  - Requesting user lacks permission to grant access
- Error response (HTTP 400 Bad Request) for:
  - Malformed path parameters
  - User already has access to the specified view

---

## Dependencies

- **Upstream**: 
  - Bank must exist in the system (Bank Creation capability)
  - Account must exist within the bank (Account Creation capability)
  - View must be created and associated with the account (View Creation capability)
  - Target user must exist in the system (User Creation capability)
  - Requesting user must be authenticated and authorized (Authentication capabilities)

- **Downstream**: 
  - Once access is granted, the user can access account data through the specified view
  - The granted access can later be revoked through the View Access Revocation capability (#68)
  - View permissions determine what account data the user can see and what actions they can perform

- **External Systems**: 
  - None explicitly mentioned for access grant operations
  - May integrate with external identity providers for user validation

---

## Notes for Implementation

- **Idempotency Consideration**: Consider making the grant operation idempotent - if user already has access, return success rather than error
- **Audit Trail**: Implement logging/audit trail for all access grant operations for compliance and security monitoring
- **Notification**: Consider notifying the target user when they are granted access to a new view
- **Batch Operations**: Consider implementing batch grant operations for efficiency when granting access to multiple views or users
- **Permission Inheritance**: Clarify if granting access to a parent view automatically grants access to child views

### Needs SME Input
- Clarify if there are any restrictions on which views can be granted to which user types
- Determine if there should be an approval workflow for certain view access grants
- Confirm if access grants should have an expiration date or be permanent by default
- Clarify the behavior when attempting to grant access that already exists (error vs. idempotent success)
- Determine if there are any rate limits on access grant operations
- Clarify if access grants should be logged for audit/compliance purposes

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical implementation details)
- [x] User role is clearly identified (bank administrator/account owner)
- [x] Business value is stated (control access, enable data sharing, maintain security)
- [x] Acceptance criteria are testable and measurable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged for SME review
- [x] Only relevant endpoints are included (POST operation only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words from the capability description justify inclusion
- [x] No CRUD operations are inferred beyond what the description explicitly states (only "Grant" mentioned)
- [x] Words like "manage" have been interpreted narrowly - N/A (no "manage" in description)
