# User Story for Transaction Tagging

## Capability Input

- **Name**: Transaction Tagging
- **Description**: Add, retrieve, and delete tags on transactions for categorization
- **Frequency**: On-demand
- **Volume**: Medium

---

## Operation Verb Analysis

Based on the capability description, the following operation verbs are identified:

| Verb Found | Operation Type | Justification |
|------------|---------------|---------------|
| "Add" | CREATE | Explicitly stated: "Add...tags on transactions" |
| "retrieve" | READ/RETRIEVAL | Explicitly stated: "retrieve...tags on transactions" |
| "delete" | DELETE | Explicitly stated: "delete tags on transactions" |

**Operations NOT included** (verbs not present in description):
- UPDATE operations: No "manage", "configure", "maintain", "update", "modify", "change", "edit", "adjust", "set", or "customize" mentioned

---

## Story Overview

**As a** third-party developer, fintech application, account holder, or financial service provider integrating with the Open Bank Project platform
**I want to** add, retrieve, and delete tags on transactions for categorization
**So that** I can organize and categorize transactions for better financial tracking, enable users to label their spending patterns, support expense categorization workflows in my application, facilitate budgeting and financial analysis features, and provide transaction organization capabilities to end users

---

## Acceptance Criteria

1. The system shall allow authorized users to add tags to specific transactions for categorization purposes
2. The system shall allow authorized users to retrieve tags associated with a specific transaction
3. The system shall allow authorized users to delete tags from transactions when no longer needed
4. The system shall enforce access control to ensure users can only manage tags on transactions they have permission to access
5. The system shall support multiple tags per transaction to enable flexible categorization
6. The system shall return tag information including tag ID, tag value, and creation date
7. The system shall return appropriate error responses (e.g., HTTP 404) when the specified bank, account, or transaction is not found
8. The system shall return appropriate error responses (e.g., HTTP 403) when the user lacks permission to manage tags on the transaction
9. The system shall validate tag values to ensure they meet format requirements (e.g., non-empty, valid characters)
10. The system shall support on-demand access patterns with appropriate performance characteristics for medium-volume usage
11. The system shall return an empty list when no tags exist for a transaction
12. The system shall prevent duplicate tags on the same transaction

---

## Technical Context

- **Classes/Services Involved** (from Scala source code):
  - `APIMethods400` (code.api.v4_0_0.APIMethods400) - REST endpoint definitions for transaction tag operations
  - `JSONFactory400` (code.api.v4_0_0.JSONFactory4.0.0) - JSON response factory with tag creation methods
  - `NewStyle.function` (code.api.util.NewStyle) - Service layer with tag management methods
  - `Connector` (code.bankconnectors.Connector) - Backend connector abstraction for tag data access
  - `Views` (code.views.Views) - View/permission management for transaction access control
  - `TransactionTagJson` / `TransactionTagsJson` - Case classes defining tag response structure
  - `MappedTransactionTag` - Transaction tag entity mapping

- **Input Data**: 
  - For tag creation (`POST`): Bank identifier, Account identifier, View identifier, Transaction identifier as path parameters; Tag value in request body
  - For tag retrieval (`GET`): Bank identifier, Account identifier, View identifier, Transaction identifier as path parameters
  - For tag deletion (`DELETE`): Bank identifier, Account identifier, View identifier, Transaction identifier, Tag identifier as path parameters
  - Authentication token (OAuth/DirectLogin) to identify the requesting user

- **Output Data** (based on tag response case classes):
  - `id` (String) - Unique tag identifier
  - `value` (String) - Tag value/label
  - `date` (Date) - Date when the tag was created
  - For list operations: Array of tag objects

- **Processing Type**: API / On-demand / Synchronous request-response

---

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description. CREATE, READ/RETRIEVAL, and DELETE operations are included as the description explicitly contains the verbs "Add", "retrieve", and "delete".

### Endpoint 1: Add Tag to Transaction

- **Endpoint**: `POST /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/metadata/tags`
  - **Justification (from description)**: "Add...tags on transactions for categorization"
  - **Purpose**: Add a new tag to a specific transaction for categorization purposes
  - **Scala Implementation**: `APIMethods400.addTransactionTag` -> `NewStyle.function.createTransactionTag()` -> `JSONFactory400.createTransactionTagJson()`
  - **Request**: 
    ```
    POST /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/metadata/tags
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      ACCOUNT_ID: The unique identifier of the account (required)
      VIEW_ID: The view identifier defining access permissions (required)
      TRANSACTION_ID: The unique identifier of the transaction (required)
    Request Body:
    {
      "value": "groceries"
    }
    ```
  - **Response** (based on `TransactionTagJson` case class): 
    ```json
    {
      "id": "tag-id-001",
      "value": "groceries",
      "date": "2024-01-15T10:30:00Z",
      "user": {
        "id": "user-id-001",
        "provider": "OBP",
        "display_name": "John Doe"
      }
    }
    ```

### Endpoint 2: Get Tags for Transaction

- **Endpoint**: `GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/metadata/tags`
  - **Justification (from description)**: "retrieve...tags on transactions"
  - **Purpose**: Retrieve all tags associated with a specific transaction
  - **Scala Implementation**: `APIMethods400.getTransactionTags` -> `NewStyle.function.getTransactionTags()` -> `JSONFactory400.createTransactionTagsJson()`
  - **Request**: 
    ```
    GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/metadata/tags
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      ACCOUNT_ID: The unique identifier of the account (required)
      VIEW_ID: The view identifier defining access permissions (required)
      TRANSACTION_ID: The unique identifier of the transaction (required)
    ```
  - **Response** (based on `TransactionTagsJson` case class): 
    ```json
    {
      "tags": [
        {
          "id": "tag-id-001",
          "value": "groceries",
          "date": "2024-01-15T10:30:00Z",
          "user": {
            "id": "user-id-001",
            "provider": "OBP",
            "display_name": "John Doe"
          }
        },
        {
          "id": "tag-id-002",
          "value": "food",
          "date": "2024-01-15T11:00:00Z",
          "user": {
            "id": "user-id-001",
            "provider": "OBP",
            "display_name": "John Doe"
          }
        }
      ]
    }
    ```

### Endpoint 3: Delete Tag from Transaction

- **Endpoint**: `DELETE /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/metadata/tags/TAG_ID`
  - **Justification (from description)**: "delete tags on transactions"
  - **Purpose**: Delete a specific tag from a transaction
  - **Scala Implementation**: `APIMethods400.deleteTransactionTag` -> `NewStyle.function.deleteTransactionTag()`
  - **Request**: 
    ```
    DELETE /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/metadata/tags/TAG_ID
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      ACCOUNT_ID: The unique identifier of the account (required)
      VIEW_ID: The view identifier defining access permissions (required)
      TRANSACTION_ID: The unique identifier of the transaction (required)
      TAG_ID: The unique identifier of the tag to delete (required)
    ```
  - **Response**: 
    ```json
    {
      "deleted": true
    }
    ```
    Or HTTP 204 No Content on successful deletion

### Endpoints NOT Included (with justification)

| Endpoint | Operation Type | Reason for Exclusion |
|----------|---------------|---------------------|
| PUT /banks/{bank_id}/accounts/{account_id}/transactions/{transaction_id}/metadata/tags/{tag_id} | UPDATE | No "update", "modify", "edit", "manage", or similar verb in description |
| GET /banks/{bank_id}/accounts/{account_id}/transactions (list transactions) | READ | Not part of this capability - belongs to Transaction Listing capability |
| POST /banks/{bank_id}/accounts/{account_id}/transactions | CREATE | Not part of this capability - this is transaction creation, not tag creation |

---

## Business Rules (from capability description)

1. **Transaction Scope**: Tags are associated with specific transactions - users must specify which transaction to tag (from: "tags on transactions")
2. **Categorization Purpose**: Tags serve the purpose of categorizing transactions for organization and analysis (from: "for categorization")
3. **Add Operation**: Users can add new tags to transactions to label and categorize them (from: "Add")
4. **Retrieve Operation**: Users can retrieve existing tags on transactions to view categorization (from: "retrieve")
5. **Delete Operation**: Users can remove tags from transactions when categorization is no longer needed (from: "delete")
6. **Access Control**: Only users with appropriate view/permission access to the transaction should be able to manage its tags (implied by transaction access model)
7. **On-demand Access**: Tag operations are performed on-demand as users categorize their transactions (from: Frequency = On-demand)
8. **Medium Volume Support**: The system should handle medium volume of tag operations efficiently (from: Volume = Medium)

---

## Data Validations (if applicable)

- Bank identifier (BANK_ID) must be valid and exist in the system
- Account identifier (ACCOUNT_ID) must be valid and belong to the specified bank
- View identifier (VIEW_ID) must be valid and the user must have access to it
- Transaction identifier (TRANSACTION_ID) must be valid and belong to the specified account
- Tag identifier (TAG_ID) must be valid and belong to the specified transaction (for delete operations)
- User must be authenticated with a valid OAuth token or DirectLogin credentials
- User must have appropriate view/permission granted on the account to manage transaction tags
- Tag value must be non-empty and contain valid characters
- Tag value should have reasonable length limits (e.g., max 255 characters)
- Error response (HTTP 404 Not Found / `BankNotFound`) must be returned when BANK_ID does not exist
- Error response (HTTP 404 Not Found / `AccountNotFound`) must be returned when ACCOUNT_ID does not exist
- Error response (HTTP 404 Not Found / `TransactionNotFound`) must be returned when TRANSACTION_ID does not exist
- Error response (HTTP 404 Not Found / `TagNotFound`) must be returned when TAG_ID does not exist (for delete)
- Error response (HTTP 401 Unauthorized) for missing or invalid authentication
- Error response (HTTP 403 Forbidden) when user lacks permission to manage tags on the transaction
- Error response (HTTP 400 Bad Request) for invalid or empty tag value
- Duplicate tag values on the same transaction should be prevented or handled appropriately

---

## Dependencies

- **Upstream**: 
  - Bank must exist in the system (Bank Creation capability)
  - Account must be created and linked to the bank (Account Creation capability)
  - Transaction must exist for the account (Payment Initiation or Transaction Processing capabilities)
  - User must be authenticated (Authentication & Security capabilities)
  - User must have been granted view/permission access to the account (View & Permission Management capabilities)
  - Account-user access relationships must be established

- **Downstream**: 
  - Transaction Listing capability may display tags in transaction metadata
  - Transaction Details capability may include tag information
  - Reporting and analytics features may use tags for categorization and filtering
  - Personal finance management applications may use tags for expense tracking
  - Budgeting applications may aggregate transactions by tag for spending analysis

- **External Systems**: 
  - Backend banking connector for transaction data access
  - Database/storage system for persisting tag data
  - Authentication provider for user identity verification

---

## Notes for Implementation

- **Tag Storage**: Tags should be stored in a separate table/collection linked to transactions by transaction ID to support multiple tags per transaction
- **User Attribution**: Each tag should record which user created it and when, for audit purposes
- **View Permissions**: The view system should define which views allow tag creation, retrieval, and deletion - some views may be read-only
- **Tag Uniqueness**: Consider whether the same tag value can be added multiple times to the same transaction by different users
- **Tag Search**: While not in scope for this capability, consider indexing tags to support future search/filter by tag functionality
- **Cascading Delete**: When a transaction is deleted, associated tags should also be deleted
- **Performance**: For medium volume, standard database indexing on transaction_id should be sufficient
- **API Versioning**: Endpoints follow the v4.0.0 API versioning pattern
- **Needs SME Input**: 
  - What are the allowed characters and maximum length for tag values?
  - Should tags be case-sensitive or case-insensitive?
  - Can the same user add the same tag value multiple times to a transaction?
  - Are there any predefined/suggested tags that should be offered to users?
  - Should there be a limit on the number of tags per transaction?

---

## Quality Checklist

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified
- [x] Business value is stated
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered (Add, Retrieve, Delete)
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged
- [x] Only relevant endpoints are included
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, I can point to a specific word or phrase in the capability description that justifies this endpoint
- [x] No endpoint type (create, update, view, list, delete) has been added unless its verb (or a clear synonym) appears in the description
- [x] Words like "manage" have been interpreted narrowly - UPDATE operations are NOT included as "manage" is not in the description
