# User Story for System View Management

## Capability Input

- **Name**: System View Management
- **Description**: Manage predefined system views
- **Frequency**: On-demand
- **Volume**: Low

---

## Operation Verb Analysis

Based on the capability description, the following operation verbs are identified:

| Verb Found | Operation Type | Justification |
|------------|---------------|---------------|
| "Manage" | UPDATE/CONFIGURE | Explicitly stated: "Manage predefined system views" |

**Operations NOT included** (verbs not present in description):
- CREATE operations: No "create", "register", "onboard", "set up", "add", "establish", "initialize", or "provision" mentioned
- READ/RETRIEVAL operations: No "view", "retrieve", "get", "see", "display", "browse", "search", "list", "query", "lookup", "find", "show", "read", "access", or "fetch" mentioned
- DELETE operations: No "delete", "remove", "deactivate", "close", "terminate", "disable", "archive", "retire", or "cancel" mentioned

**Important Note on "Manage"**: Per the Operation Derivation Rules, the word "manage" by itself means ONLY update/configure/maintain operations. It does NOT imply view, list, or delete operations unless those verbs are separately mentioned in the description.

---

## Story Overview

**As a** bank administrator or system administrator on the Open Bank Project platform
**I want to** manage predefined system views by updating their configurations and permission settings
**So that** I can customize the behavior of standard system views (such as owner, accountant, auditor) to meet the specific access control requirements of my financial institution while maintaining consistent permission structures across accounts

---

## Acceptance Criteria

1. The system shall allow authorized administrators to update the configuration of predefined system views
2. The system shall allow modification of permission flags on system views (e.g., canSeeBankAccountBalance, canSeeBankAccountNumber, canSeeBankAccountOwners)
3. The system shall preserve the system view identifier and core structure when updating configurations
4. The system shall validate that only authorized users with appropriate entitlements can manage system views
5. The system shall return appropriate error responses (e.g., HTTP 403) when the user lacks permission to manage system views
6. The system shall return appropriate error responses (e.g., HTTP 404) when the specified system view does not exist
7. The system shall ensure that changes to system views are applied consistently across all accounts that use those views
8. The system shall maintain audit trail of changes made to system view configurations

---

## Technical Context

- **Classes/Services Involved** (from Scala source code):
  - `APIMethods400` / `APIMethods310` (code.api.v4_0_0.APIMethods400) - REST endpoint definitions for view management
  - `Views` (code.views.Views) - View management service layer
  - `ViewDefinition` (code.views.system.ViewDefinition) - Database entity for view definitions
  - `JSONFactory400` (code.api.v4_0_0.JSONFactory4.0.0) - JSON response factory for view responses
  - `NewStyle.function` (code.api.util.NewStyle) - Service layer with view management methods
  - `Connector` (code.bankconnectors.Connector) - Backend connector abstraction

- **Input Data**: 
  - For system view update (`PUT /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID`): Bank identifier, Account identifier, View identifier as path parameters
  - Authentication token (OAuth/DirectLogin) to identify the requesting user
  - Request body containing updated view configuration:
    - Permission flags (canSeeBankAccountBalance, canSeeBankAccountNumber, canSeeBankAccountOwners, etc.)
    - View metadata (description, alias)
    - Access settings (isPublic, isFirehose)

- **Output Data** (based on view response case classes):
  - `id` (String) - View identifier
  - `short_name` (String) - Short name of the view
  - `description` (String) - View description
  - `metadata_view` (String) - Metadata view reference
  - `is_public` (Boolean) - Whether the view is publicly accessible
  - `is_system` (Boolean) - Whether this is a system view (always true for system views)
  - `alias` (String) - View alias
  - `hide_metadata_if_alias_used` (Boolean) - Metadata hiding setting
  - `can_see_transaction_this_bank_account` (Boolean) - Permission flag
  - `can_see_transaction_other_bank_account` (Boolean) - Permission flag
  - `can_see_transaction_metadata` (Boolean) - Permission flag
  - `can_see_transaction_label` (Boolean) - Permission flag
  - `can_see_transaction_amount` (Boolean) - Permission flag
  - `can_see_transaction_type` (Boolean) - Permission flag
  - `can_see_transaction_currency` (Boolean) - Permission flag
  - `can_see_transaction_start_date` (Boolean) - Permission flag
  - `can_see_transaction_finish_date` (Boolean) - Permission flag
  - `can_see_transaction_balance` (Boolean) - Permission flag
  - `can_see_comments` (Boolean) - Permission flag
  - `can_see_narrative` (Boolean) - Permission flag
  - `can_see_tags` (Boolean) - Permission flag
  - `can_see_images` (Boolean) - Permission flag
  - `can_see_bank_account_owners` (Boolean) - Permission flag
  - `can_see_bank_account_type` (Boolean) - Permission flag
  - `can_see_bank_account_balance` (Boolean) - Permission flag
  - `can_see_bank_account_currency` (Boolean) - Permission flag
  - `can_see_bank_account_label` (Boolean) - Permission flag
  - `can_see_bank_account_national_identifier` (Boolean) - Permission flag
  - `can_see_bank_account_swift_bic` (Boolean) - Permission flag
  - `can_see_bank_account_iban` (Boolean) - Permission flag
  - `can_see_bank_account_number` (Boolean) - Permission flag
  - `can_see_bank_account_bank_name` (Boolean) - Permission flag
  - `can_see_other_account_national_identifier` (Boolean) - Permission flag
  - `can_see_other_account_swift_bic` (Boolean) - Permission flag
  - `can_see_other_account_iban` (Boolean) - Permission flag
  - `can_see_other_account_bank_name` (Boolean) - Permission flag
  - `can_see_other_account_number` (Boolean) - Permission flag
  - `can_see_other_account_metadata` (Boolean) - Permission flag
  - `can_see_other_account_kind` (Boolean) - Permission flag
  - `can_see_more_info` (Boolean) - Permission flag
  - `can_see_url` (Boolean) - Permission flag
  - `can_see_image_url` (Boolean) - Permission flag
  - `can_see_open_corporates_url` (Boolean) - Permission flag
  - `can_see_corporate_location` (Boolean) - Permission flag
  - `can_see_physical_location` (Boolean) - Permission flag
  - `can_see_public_alias` (Boolean) - Permission flag
  - `can_see_private_alias` (Boolean) - Permission flag
  - `can_add_more_info` (Boolean) - Permission flag
  - `can_add_url` (Boolean) - Permission flag
  - `can_add_image_url` (Boolean) - Permission flag
  - `can_add_open_corporates_url` (Boolean) - Permission flag
  - `can_add_corporate_location` (Boolean) - Permission flag
  - `can_add_physical_location` (Boolean) - Permission flag
  - `can_add_public_alias` (Boolean) - Permission flag
  - `can_add_private_alias` (Boolean) - Permission flag
  - `can_add_counterparty` (Boolean) - Permission flag
  - `can_delete_counterparty` (Boolean) - Permission flag
  - `can_add_comment` (Boolean) - Permission flag
  - `can_delete_comment` (Boolean) - Permission flag
  - `can_add_narrative` (Boolean) - Permission flag
  - `can_edit_narrative` (Boolean) - Permission flag
  - `can_delete_narrative` (Boolean) - Permission flag
  - `can_add_tag` (Boolean) - Permission flag
  - `can_delete_tag` (Boolean) - Permission flag
  - `can_add_image` (Boolean) - Permission flag
  - `can_delete_image` (Boolean) - Permission flag
  - `can_add_where_tag` (Boolean) - Permission flag
  - `can_see_where_tag` (Boolean) - Permission flag
  - `can_delete_where_tag` (Boolean) - Permission flag
  - `can_create_counterparty` (Boolean) - Permission flag
  - `can_see_bank_routing_scheme` (Boolean) - Permission flag
  - `can_see_bank_routing_address` (Boolean) - Permission flag
  - `can_see_bank_account_routing_scheme` (Boolean) - Permission flag
  - `can_see_bank_account_routing_address` (Boolean) - Permission flag
  - `can_see_other_bank_routing_scheme` (Boolean) - Permission flag
  - `can_see_other_bank_routing_address` (Boolean) - Permission flag
  - `can_see_other_account_routing_scheme` (Boolean) - Permission flag
  - `can_see_other_account_routing_address` (Boolean) - Permission flag
  - `can_add_transaction_request_to_own_account` (Boolean) - Permission flag
  - `can_add_transaction_request_to_any_account` (Boolean) - Permission flag
  - `can_see_bank_account_credit_limit` (Boolean) - Permission flag

- **Processing Type**: API / On-demand / Synchronous request-response

---

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description. Only UPDATE/CONFIGURE operations are included as the description only contains the verb "Manage".

### Endpoint 1: Update System View

- **Endpoint**: `PUT /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID`
  - **Justification (from description)**: "Manage predefined system views" - the word "manage" justifies update/configure operations
  - **Purpose**: Update the configuration and permission settings of a predefined system view
  - **Scala Implementation**: `APIMethods400.updateViewForBankAccount` -> `Views.updateSystemView()` -> `JSONFactory400.createViewJSON()`
  - **Request**: 
    ```
    PUT /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      ACCOUNT_ID: The unique identifier of the account (required)
      VIEW_ID: The unique identifier of the system view to update (required, e.g., "owner", "accountant", "auditor")
    Body:
    {
      "description": "Updated system view description",
      "metadata_view": "owner",
      "is_public": false,
      "which_alias_to_use": "public",
      "hide_metadata_if_alias_used": false,
      "allowed_actions": [
        "can_see_transaction_this_bank_account",
        "can_see_transaction_other_bank_account",
        "can_see_transaction_metadata",
        "can_see_transaction_label",
        "can_see_transaction_amount",
        "can_see_transaction_type",
        "can_see_transaction_currency",
        "can_see_transaction_start_date",
        "can_see_transaction_finish_date",
        "can_see_transaction_balance",
        "can_see_comments",
        "can_see_narrative",
        "can_see_tags",
        "can_see_images",
        "can_see_bank_account_owners",
        "can_see_bank_account_type",
        "can_see_bank_account_balance",
        "can_see_bank_account_currency",
        "can_see_bank_account_label",
        "can_see_bank_account_number",
        "can_see_bank_account_iban"
      ]
    }
    ```
  - **Response** (based on `ViewJsonV300` case class): 
    ```json
    {
      "id": "owner",
      "short_name": "Owner",
      "description": "Updated system view description",
      "metadata_view": "owner",
      "is_public": false,
      "is_system": true,
      "alias": "",
      "hide_metadata_if_alias_used": false,
      "can_see_transaction_this_bank_account": true,
      "can_see_transaction_other_bank_account": true,
      "can_see_transaction_metadata": true,
      "can_see_transaction_label": true,
      "can_see_transaction_amount": true,
      "can_see_transaction_type": true,
      "can_see_transaction_currency": true,
      "can_see_transaction_start_date": true,
      "can_see_transaction_finish_date": true,
      "can_see_transaction_balance": true,
      "can_see_comments": true,
      "can_see_narrative": true,
      "can_see_tags": true,
      "can_see_images": true,
      "can_see_bank_account_owners": true,
      "can_see_bank_account_type": true,
      "can_see_bank_account_balance": true,
      "can_see_bank_account_currency": true,
      "can_see_bank_account_label": true,
      "can_see_bank_account_national_identifier": true,
      "can_see_bank_account_swift_bic": true,
      "can_see_bank_account_iban": true,
      "can_see_bank_account_number": true,
      "can_see_bank_account_bank_name": true,
      "can_add_comment": true,
      "can_delete_comment": true,
      "can_add_narrative": true,
      "can_edit_narrative": true,
      "can_delete_narrative": true,
      "can_add_tag": true,
      "can_delete_tag": true,
      "can_add_image": true,
      "can_delete_image": true,
      "can_add_where_tag": true,
      "can_see_where_tag": true,
      "can_delete_where_tag": true,
      "can_add_transaction_request_to_own_account": true,
      "can_add_transaction_request_to_any_account": false,
      "can_see_bank_account_credit_limit": true
    }
    ```

### Endpoint 2: Update System View (Alternative - Bank-Level)

- **Endpoint**: `PUT /obp/v4.0.0/banks/BANK_ID/system-views/VIEW_ID`
  - **Justification (from description)**: "Manage predefined system views" - the word "manage" justifies update/configure operations at the bank level
  - **Purpose**: Update the configuration of a system view at the bank level, affecting all accounts that use this system view
  - **Scala Implementation**: `APIMethods400.updateSystemView` -> `Views.updateSystemViewForBank()` -> `JSONFactory400.createSystemViewJSON()`
  - **Request**: 
    ```
    PUT /obp/v4.0.0/banks/BANK_ID/system-views/VIEW_ID
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      VIEW_ID: The unique identifier of the system view to update (required, e.g., "owner", "accountant", "auditor")
    Body:
    {
      "description": "Updated bank-level system view configuration",
      "is_public": false,
      "which_alias_to_use": "public",
      "hide_metadata_if_alias_used": false,
      "allowed_actions": [
        "can_see_transaction_this_bank_account",
        "can_see_bank_account_balance",
        "can_see_bank_account_number",
        "can_see_bank_account_iban"
      ]
    }
    ```
  - **Response**: 
    ```json
    {
      "id": "accountant",
      "short_name": "Accountant",
      "description": "Updated bank-level system view configuration",
      "is_public": false,
      "is_system": true,
      "can_see_transaction_this_bank_account": true,
      "can_see_bank_account_balance": true,
      "can_see_bank_account_number": true,
      "can_see_bank_account_iban": true
    }
    ```

### Endpoints NOT Included (with justification)

| Endpoint | Operation Type | Reason for Exclusion |
|----------|---------------|---------------------|
| POST /banks/{bank_id}/system-views | CREATE | No "create", "register", "onboard", "add", or similar verb in description |
| GET /banks/{bank_id}/system-views | READ/LIST | No "view", "retrieve", "get", "list", "search", or similar verb in description |
| GET /banks/{bank_id}/system-views/{view_id} | READ | No "view", "retrieve", "get", "see", or similar verb in description |
| DELETE /banks/{bank_id}/system-views/{view_id} | DELETE | No "delete", "remove", "deactivate", or similar verb in description |

**Note**: Per the Operation Derivation Rules, the word "manage" by itself means ONLY update/configure/maintain operations. View, list, and delete operations are NOT included because those verbs are not present in the capability description.

---

## Business Rules (from capability description)

1. **System View Scope**: System views are predefined views with standard permission configurations (from: "predefined system views")
2. **Management Operations**: Only update/configure operations are supported for system views (from: "Manage" - interpreted narrowly as update/configure per Operation Derivation Rules)
3. **Predefined Nature**: System views have predefined identifiers and structures that cannot be changed - only their configurations can be updated (from: "predefined")
4. **On-demand Processing**: System view management is performed on-demand when administrators need to adjust configurations (from: Frequency = On-demand)
5. **Low Volume**: System view management is a low-volume operation typically performed during initial setup or periodic configuration changes (from: Volume = Low)
6. **Standard View Types**: Common predefined system views include "owner", "accountant", and "auditor" with different default permission sets

---

## Data Validations (if applicable)

- Bank identifier (BANK_ID) must be valid and exist in the system
- View identifier (VIEW_ID) must correspond to a valid predefined system view
- User must be authenticated with a valid OAuth token or DirectLogin credentials
- User must have appropriate entitlements to manage system views (e.g., CanUpdateSystemView role)
- Permission flags in the request must be valid boolean values
- The view being updated must have `isSystem_ = true` in the database
- Error response (HTTP 404 Not Found / `ViewNotFound`) must be returned when VIEW_ID does not exist
- Error response (HTTP 400 Bad Request) for invalid permission flag combinations
- Error response (HTTP 401 Unauthorized) for missing or invalid authentication
- Error response (HTTP 403 Forbidden) when user lacks permission to manage system views
- System view identifier cannot be changed during update operations
- All permission flags must be explicitly set in the request or retain their previous values

---

## Dependencies

- **Upstream**: 
  - Bank must exist in the system (Bank Creation capability)
  - System views must be initialized with predefined configurations
  - User must be authenticated (Authentication & Security capabilities)
  - User must have been granted appropriate entitlements to manage system views (Entitlement & Role Management capabilities)

- **Downstream**: 
  - Updated system view configurations affect all accounts that use those views
  - View Access Grant capability uses system views when granting access to users
  - Account Listing capability returns views_available based on system view configurations
  - Transaction Listing and Account Details Retrieval capabilities filter data based on system view permissions
  - All data access operations are affected by the permission flags configured in system views

- **External Systems**: 
  - Backend banking connectors may need to be notified of view configuration changes
  - Audit systems may record changes to system view configurations

---

## Notes for Implementation

- **Permission Flag Granularity**: System views have extensive permission flags controlling visibility of different data elements - ensure all flags are properly handled during updates
- **Consistency**: Changes to system views should be applied atomically to ensure consistency across all accounts using the view
- **Audit Trail**: Implement comprehensive audit logging for all changes to system view configurations for compliance purposes
- **Default Values**: When updating system views, consider whether unspecified permission flags should retain their previous values or be reset to defaults
- **Validation**: Implement validation to prevent invalid permission flag combinations that could lead to security issues
- **Caching**: If view configurations are cached, ensure cache invalidation when system views are updated

### Needs SME Input
- Clarify which specific system views are predefined (owner, accountant, auditor, or others)
- Determine if there are any permission flags that cannot be modified on system views
- Confirm if system view updates should be propagated immediately or require a refresh/reload
- Clarify the authorization model - which entitlements are required to manage system views
- Determine if there should be a mechanism to reset system views to their default configurations
- Confirm if system view changes should trigger notifications to affected users or applications

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical implementation details)
- [x] User role is clearly identified (bank administrator / system administrator)
- [x] Business value is stated (customize access control requirements)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged for SME input
- [x] Only relevant endpoints are included (UPDATE operations only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific word from capability description justifies inclusion ("Manage")
- [x] No endpoint type (create, view, list, delete) has been added beyond what "manage" implies
- [x] Word "manage" has been interpreted narrowly as update/configure only - view/list/delete operations are NOT included

---

## Capability Context

System View Management is part of the "View & Permission Management" category in the Open Bank Project API. System views are predefined views that come with standard permission configurations:

- **Owner View**: Full access to all account information and operations
- **Accountant View**: Access to financial data needed for accounting purposes
- **Auditor View**: Read-only access to transaction and account data for audit purposes

These system views provide a standardized way to grant access to accounts without requiring custom view creation for common use cases. The management capability allows administrators to customize the permission settings of these predefined views to meet their institution's specific requirements while maintaining the standard view structure.
