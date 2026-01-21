# User Story for Account Listing

## Capability Input

- **Name**: Account Listing
- **Description**: Retrieve list of accounts at a bank that a user has access to with various detail levels
- **Frequency**: Real-time
- **Volume**: High

---

## Operation Verb Analysis

Based on the capability description, the following operation verbs are identified:

| Verb Found | Operation Type | Justification |
|------------|---------------|---------------|
| "Retrieve" | READ/RETRIEVAL | Explicitly stated: "Retrieve list of accounts at a bank" |
| "list" | READ/RETRIEVAL | Explicitly stated: "Retrieve list of accounts" |

**Operations NOT included** (verbs not present in description):
- CREATE operations: No "create", "register", "onboard", "set up", "add", "establish", "initialize", or "provision" mentioned
- UPDATE operations: No "manage", "configure", "maintain", "update", "modify", "change", "edit", "adjust", "set", or "customize" mentioned
- DELETE operations: No "delete", "remove", "deactivate", "close", "terminate", "disable", "archive", "retire", or "cancel" mentioned

---

## Story Overview

**As a** third-party developer, fintech application, or account holder integrating with the Open Bank Project platform
**I want to** retrieve a list of accounts at a bank that I have access to with various detail levels
**So that** I can display account information to end users, enable account selection workflows in my application, aggregate account data across multiple banks, and provide personalized financial services based on the user's accessible accounts

---

## Acceptance Criteria

1. The system shall allow retrieval of a list of all accounts at a specific bank that the authenticated user has access to
2. The system shall support various detail levels for account information retrieval (e.g., minimal, basic, detailed)
3. The system shall return only accounts that the requesting user has been granted access to view
4. The system shall return account identifiers for each account to enable subsequent API calls
5. The system shall return account metadata including account type, label, and balance information based on the requested detail level
6. The system shall support retrieval of accounts across multiple banks when the user has access to accounts at different institutions
7. The system shall return responses in real-time with appropriate performance characteristics for high-volume usage patterns
8. The system shall return appropriate error responses (e.g., HTTP 404) when the specified bank is not found
9. The system shall return an empty list when the user has no accessible accounts at the specified bank
10. The system shall enforce access control to ensure users can only see accounts they have been granted permission to view

---

## Technical Context

- **Classes/Services Involved** (from Scala source code):
  - `APIMethods400` (code.api.v4_0_0.APIMethods400) - REST endpoint definitions for account listing
  - `JSONFactory400` (code.api.v4_0_0.JSONFactory4.0.0) - JSON response factory with account list creation methods
  - `NewStyle.function` (code.api.util.NewStyle) - Service layer with account retrieval methods
  - `Connector` (code.bankconnectors.Connector) - Backend connector abstraction for account data access
  - `Views` (code.views.Views) - View/permission management for account access control
  - `AccountJson400` / `AccountsJson400` - Case classes defining response structure

- **Input Data**: 
  - For account list retrieval (`GET /banks/BANK_ID/accounts`): Bank identifier (BANK_ID) as path parameter
  - For accounts with specific view (`GET /banks/BANK_ID/accounts/VIEW_ID/accounts`): Bank identifier and View identifier
  - Authentication token (OAuth/DirectLogin) to identify the requesting user
  - Optional query parameters for detail level selection

- **Output Data** (based on account response case classes):
  - `id` (String) - Unique account identifier
  - `bank_id` (String) - Bank identifier the account belongs to
  - `label` (String) - User-friendly account label/name
  - `number` (String) - Account number (masked or full based on permissions)
  - `owners` (List[AccountOwner]) - Account owner information
  - `type` (String) - Account type (e.g., checking, savings)
  - `balance` (AmountOfMoney) - Account balance (based on detail level and permissions)
  - `account_routings` (List[AccountRouting]) - Account routing information (IBAN, etc.)
  - `views_available` (List[ViewBasic]) - Views the user has access to for this account

- **Processing Type**: API / Real-time / Synchronous request-response

---

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description. Only READ/RETRIEVAL operations are included as the description only contains the verbs "Retrieve" and "list".

### Endpoint 1: Get Accounts at Bank (Basic)

- **Endpoint**: `GET /obp/v4.0.0/banks/BANK_ID/accounts`
  - **Justification (from description)**: "Retrieve list of accounts at a bank that a user has access to"
  - **Purpose**: Retrieve a list of all accounts at a specific bank that the authenticated user has access to
  - **Scala Implementation**: `APIMethods400.getAccountsAtBank` -> `NewStyle.function.getBankAccountsForUser()` -> `JSONFactory400.createAccountsJson()`
  - **Request**: 
    ```
    GET /obp/v4.0.0/banks/BANK_ID/accounts
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
    ```
  - **Response** (based on `AccountsJson400` case class): 
    ```json
    {
      "accounts": [
        {
          "id": "account-id-001",
          "bank_id": "bank-id-001",
          "label": "My Checking Account",
          "number": "****1234",
          "owners": [
            {"user_id": "user-001", "provider": "OBP", "display_name": "John Doe"}
          ],
          "type": "CHECKING",
          "balance": {
            "currency": "USD",
            "amount": "1500.00"
          },
          "account_routings": [
            {"scheme": "IBAN", "address": "US12345678901234567890"}
          ],
          "views_available": [
            {"id": "owner", "short_name": "Owner", "is_public": false}
          ]
        },
        {
          "id": "account-id-002",
          "bank_id": "bank-id-001",
          "label": "Savings Account",
          "number": "****5678",
          "owners": [
            {"user_id": "user-001", "provider": "OBP", "display_name": "John Doe"}
          ],
          "type": "SAVINGS",
          "balance": {
            "currency": "USD",
            "amount": "10000.00"
          },
          "account_routings": [
            {"scheme": "IBAN", "address": "US98765432109876543210"}
          ],
          "views_available": [
            {"id": "owner", "short_name": "Owner", "is_public": false}
          ]
        }
      ]
    }
    ```

### Endpoint 2: Get Accounts at Bank with Minimal Detail

- **Endpoint**: `GET /obp/v4.0.0/banks/BANK_ID/accounts/account-ids/private`
  - **Justification (from description)**: "Retrieve list of accounts" with "various detail levels" - supports minimal detail level retrieval
  - **Purpose**: Retrieve a minimal list of account IDs at a specific bank that the user has private access to
  - **Scala Implementation**: `APIMethods400.getPrivateAccountIdsbyBankId` -> `NewStyle.function.getAccountIdsForUser()` -> `JSONFactory400.createAccountIdsJson()`
  - **Request**: 
    ```
    GET /obp/v4.0.0/banks/BANK_ID/accounts/account-ids/private
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
    ```
  - **Response**: 
    ```json
    {
      "accounts": [
        {"id": "account-id-001"},
        {"id": "account-id-002"},
        {"id": "account-id-003"}
      ]
    }
    ```

### Endpoint 3: Get Accounts Held (Cross-Bank)

- **Endpoint**: `GET /obp/v4.0.0/banks/BANK_ID/accounts-held`
  - **Justification (from description)**: "Retrieve list of accounts at a bank that a user has access to" - supports accounts held at a bank
  - **Purpose**: Retrieve accounts held at a specific bank, including accounts where the user has been granted access but may not be the owner
  - **Scala Implementation**: `APIMethods400.getAccountsHeld` -> `NewStyle.function.getAccountsHeld()` -> `JSONFactory400.createAccountsHeldJson()`
  - **Request**: 
    ```
    GET /obp/v4.0.0/banks/BANK_ID/accounts-held
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
    ```
  - **Response**: 
    ```json
    {
      "accounts": [
        {
          "id": "account-id-001",
          "bank_id": "bank-id-001",
          "label": "Shared Business Account",
          "account_routings": [
            {"scheme": "IBAN", "address": "US12345678901234567890"}
          ]
        }
      ]
    }
    ```

### Endpoint 4: Get My Accounts at Bank (Full Detail)

- **Endpoint**: `GET /obp/v4.0.0/my/banks/BANK_ID/accounts`
  - **Justification (from description)**: "Retrieve list of accounts at a bank that a user has access to with various detail levels" - supports full detail level for user's own accounts
  - **Purpose**: Retrieve detailed information about the authenticated user's accounts at a specific bank
  - **Scala Implementation**: `APIMethods400.getMyAccountsAtBank` -> `NewStyle.function.getMyAccountsAtBank()` -> `JSONFactory400.createMyAccountsJson()`
  - **Request**: 
    ```
    GET /obp/v4.0.0/my/banks/BANK_ID/accounts
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
    ```
  - **Response**: 
    ```json
    {
      "accounts": [
        {
          "id": "account-id-001",
          "bank_id": "bank-id-001",
          "label": "My Checking Account",
          "number": "1234567890",
          "owners": [
            {"user_id": "user-001", "provider": "OBP", "display_name": "John Doe"}
          ],
          "type": "CHECKING",
          "balance": {
            "currency": "USD",
            "amount": "1500.00"
          },
          "account_routings": [
            {"scheme": "IBAN", "address": "US12345678901234567890"},
            {"scheme": "AccountNumber", "address": "1234567890"}
          ],
          "views_available": [
            {"id": "owner", "short_name": "Owner", "is_public": false},
            {"id": "accountant", "short_name": "Accountant", "is_public": false}
          ],
          "account_attributes": [
            {"name": "OVERDRAFT_LIMIT", "value": "500.00"},
            {"name": "INTEREST_RATE", "value": "0.01"}
          ]
        }
      ]
    }
    ```

### Endpoint 5: Get All Accounts at All Banks

- **Endpoint**: `GET /obp/v4.0.0/accounts`
  - **Justification (from description)**: "Retrieve list of accounts" - supports retrieval across all banks the user has access to
  - **Purpose**: Retrieve a list of all accounts across all banks that the authenticated user has access to
  - **Scala Implementation**: `APIMethods400.getAccounts` -> `NewStyle.function.getAllAccountsForUser()` -> `JSONFactory400.createAllAccountsJson()`
  - **Request**: 
    ```
    GET /obp/v4.0.0/accounts
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    ```
  - **Response**: 
    ```json
    {
      "accounts": [
        {
          "id": "account-id-001",
          "bank_id": "bank-id-001",
          "label": "Checking at Bank A",
          "views_available": [{"id": "owner", "short_name": "Owner", "is_public": false}]
        },
        {
          "id": "account-id-002",
          "bank_id": "bank-id-002",
          "label": "Savings at Bank B",
          "views_available": [{"id": "owner", "short_name": "Owner", "is_public": false}]
        }
      ]
    }
    ```

### Endpoints NOT Included (with justification)

| Endpoint | Operation Type | Reason for Exclusion |
|----------|---------------|---------------------|
| POST /banks/{bank_id}/accounts | CREATE | No "create", "register", "onboard", "add", or similar verb in description |
| PUT /banks/{bank_id}/accounts/{account_id} | UPDATE | No "manage", "update", "configure", "modify", or similar verb in description |
| PATCH /banks/{bank_id}/accounts/{account_id} | UPDATE | No "manage", "update", "configure", "modify", or similar verb in description |
| DELETE /banks/{bank_id}/accounts/{account_id} | DELETE | No "delete", "remove", "deactivate", "close", or similar verb in description |

---

## Business Rules (from capability description)

1. **Bank Scope**: Account listing is scoped to a specific bank - users must specify which bank's accounts to retrieve (from: "accounts at a bank")
2. **Access Control**: Only accounts that the user "has access to" should be returned - the system must enforce view/permission-based access control (from: "that a user has access to")
3. **Detail Level Flexibility**: The system must support "various detail levels" - allowing applications to request minimal, basic, or detailed account information based on their needs (from: "with various detail levels")
4. **Real-time Access**: Account listing must support real-time access patterns with low latency (from: Frequency = Real-time)
5. **High Volume Support**: The system must be designed to handle high volume of account listing requests efficiently (from: Volume = High)
6. **User Context**: Account retrieval is user-centric - the list is personalized based on the authenticated user's granted permissions

---

## Data Validations (if applicable)

- Bank identifier (BANK_ID) must be valid and exist in the system when retrieving accounts at a specific bank
- User must be authenticated with a valid OAuth token or DirectLogin credentials
- User must have at least one view/permission granted on accounts to see them in the list
- Response data must include required fields based on the requested detail level
- Account numbers may be masked based on the user's view permissions
- Balance information is only included if the user's view permits balance access
- Error response (HTTP 404 Not Found / `BankNotFound`) must be returned when BANK_ID does not exist
- Error response (HTTP 401 Unauthorized) for missing or invalid authentication
- Error response (HTTP 403 Forbidden) when user lacks permission to access any accounts
- Empty list returned (not error) when user has no accessible accounts at the specified bank
- All monetary values must include currency code and properly formatted amount

---

## Dependencies

- **Upstream**: 
  - Bank must exist in the system (Bank Creation capability)
  - Accounts must be created and linked to the bank (Account Creation capability)
  - User must be authenticated (Authentication & Security capabilities)
  - User must have been granted view/permission access to accounts (View & Permission Management capabilities)
  - Account-user access relationships must be established

- **Downstream**: 
  - Retrieved account information is used for account selection in payment initiation flows
  - Account identifiers are used in subsequent API calls for transaction listing, balance retrieval, and payment operations
  - Account information is displayed in third-party application user interfaces for account aggregation
  - Views available information enables applications to determine what operations the user can perform on each account

- **External Systems**: 
  - Backend banking connectors may retrieve account data from core banking systems
  - Account balance information may be fetched in real-time from external systems

---

## Notes for Implementation

- **Performance Consideration**: Implement caching strategies for account lists as they may be accessed frequently, but ensure cache invalidation when account access permissions change
- **Pagination**: Consider implementing pagination for users with many accounts to optimize response times and payload sizes
- **Detail Level Parameter**: Implement query parameter support for specifying detail level (e.g., `?detail=minimal`, `?detail=full`) to optimize response payloads
- **View-Based Filtering**: Account visibility is determined by the views/permissions granted to the user - ensure efficient view permission checking
- **Cross-Bank Aggregation**: Support efficient retrieval of accounts across multiple banks for account aggregation use cases
- **Balance Freshness**: Consider indicating balance freshness/timestamp for accounts where balance is retrieved from external systems

### Needs SME Input
- Clarify the exact detail levels supported and what fields are included in each level (minimal, basic, detailed, full)
- Determine if there should be a maximum number of accounts returned in a single request
- Confirm if account listing should support filtering by account type (checking, savings, etc.)
- Clarify caching strategy and acceptable staleness for account list information
- Determine if account listing should support sorting options (by name, balance, type, etc.)
- Confirm behavior when a user has access to accounts at a bank but the bank is temporarily unavailable

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical implementation details)
- [x] User role is clearly identified (third-party developer, fintech application, account holder)
- [x] Business value is stated (display account information, enable account selection, aggregate data)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (Needs SME Input section)
- [x] Only relevant endpoints are included (READ/RETRIEVAL only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific word or phrase from capability description justifies inclusion
- [x] No endpoint type (create, update, delete) added without explicit verb in description
- [x] Words like "manage" interpreted narrowly - view/list/delete operations only included if explicitly mentioned
