# User Story for Account Details Retrieval

## Capability Input

- **Name**: Account Details Retrieval
- **Description**: Get detailed information about a specific account including balance and metadata
- **Frequency**: Real-time
- **Volume**: High

---

## Operation Verb Analysis

Based on the capability description, the following operation verbs are identified:

| Verb Found | Operation Type | Justification |
|------------|---------------|---------------|
| "Get" | READ/RETRIEVAL | Explicitly stated: "Get detailed information about a specific account" |

**Operations NOT included** (verbs not present in description):
- CREATE operations: No "create", "register", "onboard", "set up", "add", "establish", "initialize", or "provision" mentioned
- UPDATE operations: No "manage", "configure", "maintain", "update", "modify", "change", "edit", "adjust", "set", or "customize" mentioned
- DELETE operations: No "delete", "remove", "deactivate", "close", "terminate", "disable", "archive", "retire", or "cancel" mentioned
- LIST operations: No "list", "browse", or "search" mentioned - this capability is for a "specific account"

---

## Story Overview

**As a** third-party developer, fintech application, or account holder integrating with the Open Bank Project platform
**I want to** get detailed information about a specific account including balance and metadata
**So that** I can display comprehensive account details to end users, verify account status before initiating transactions, access account metadata for personalized services, and retrieve current balance information for financial planning and reporting purposes

---

## Acceptance Criteria

1. The system shall allow retrieval of detailed information about a specific account identified by its unique account ID
2. The system shall return the current balance information for the specified account
3. The system shall return account metadata including account type, label, currency, and other descriptive attributes
4. The system shall return account routing information (e.g., IBAN, account number) based on user permissions
5. The system shall enforce access control to ensure users can only retrieve details for accounts they have been granted permission to view
6. The system shall return responses in real-time with appropriate performance characteristics for high-volume usage patterns
7. The system shall return appropriate error responses (e.g., HTTP 404) when the specified account is not found
8. The system shall return appropriate error responses (e.g., HTTP 403) when the user lacks permission to view the account
9. The system shall include view-specific information based on the view through which the account is being accessed
10. The system shall return account owner information when the user has permission to view ownership details

---

## Technical Context

- **Classes/Services Involved** (from Scala source code):
  - `APIMethods400` (code.api.v4_0_0.APIMethods400) - REST endpoint definitions for account details retrieval
  - `JSONFactory400` (code.api.v4_0_0.JSONFactory4.0.0) - JSON response factory with account detail creation methods
  - `NewStyle.function` (code.api.util.NewStyle) - Service layer with account retrieval methods
  - `Connector` (code.bankconnectors.Connector) - Backend connector abstraction for account data access
  - `Views` (code.views.Views) - View/permission management for account access control
  - `AccountJson400` / `ModeratedAccountJSON400` - Case classes defining response structure
  - `BankAccountCommons` - Common account data model

- **Input Data**: 
  - Bank identifier (BANK_ID) as path parameter
  - Account identifier (ACCOUNT_ID) as path parameter
  - View identifier (VIEW_ID) as path parameter (determines what data is visible)
  - Authentication token (OAuth/DirectLogin) to identify the requesting user

- **Output Data** (based on account response case classes):
  - `id` (String) - Unique account identifier
  - `bank_id` (String) - Bank identifier the account belongs to
  - `label` (String) - User-friendly account label/name
  - `number` (String) - Account number (masked or full based on permissions)
  - `owners` (List[AccountOwner]) - Account owner information (if permitted by view)
  - `type` (String) - Account type (e.g., checking, savings)
  - `balance` (AmountOfMoney) - Account balance with currency and amount
  - `account_routings` (List[AccountRouting]) - Account routing information (IBAN, etc.)
  - `views_available` (List[ViewBasic]) - Views the user has access to for this account
  - `account_attributes` (List[AccountAttribute]) - Custom metadata attributes associated with the account

- **Processing Type**: API / Real-time / Synchronous request-response

---

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description. Only READ/RETRIEVAL operations are included as the description only contains the verb "Get" for a "specific account".

### Endpoint 1: Get Account by ID (Core)

- **Endpoint**: `GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/account`
  - **Justification (from description)**: "Get detailed information about a specific account including balance and metadata"
  - **Purpose**: Retrieve detailed information about a specific account through a specific view
  - **Scala Implementation**: `APIMethods400.getAccountById` -> `NewStyle.function.getBankAccount()` -> `JSONFactory400.createAccountJSON()`
  - **Request**: 
    ```
    GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/account
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      ACCOUNT_ID: The unique identifier of the account (required)
      VIEW_ID: The view through which to access the account (required)
    ```
  - **Response** (based on `ModeratedAccountJSON400` case class): 
    ```json
    {
      "id": "account-id-001",
      "bank_id": "bank-id-001",
      "label": "My Checking Account",
      "number": "1234567890",
      "owners": [
        {
          "user_id": "user-001",
          "provider": "OBP",
          "display_name": "John Doe"
        }
      ],
      "type": "CHECKING",
      "balance": {
        "currency": "USD",
        "amount": "1500.00"
      },
      "account_routings": [
        {
          "scheme": "IBAN",
          "address": "US12345678901234567890"
        },
        {
          "scheme": "AccountNumber",
          "address": "1234567890"
        }
      ],
      "views_available": [
        {
          "id": "owner",
          "short_name": "Owner",
          "is_public": false
        },
        {
          "id": "accountant",
          "short_name": "Accountant",
          "is_public": false
        }
      ],
      "account_attributes": [
        {
          "product_code": "CHECKING_STANDARD",
          "account_attribute_id": "attr-001",
          "name": "OVERDRAFT_LIMIT",
          "type": "STRING",
          "value": "500.00"
        },
        {
          "product_code": "CHECKING_STANDARD",
          "account_attribute_id": "attr-002",
          "name": "INTEREST_RATE",
          "type": "STRING",
          "value": "0.01"
        }
      ]
    }
    ```

### Endpoint 2: Get Core Account by ID

- **Endpoint**: `GET /obp/v4.0.0/my/banks/BANK_ID/accounts/ACCOUNT_ID/account`
  - **Justification (from description)**: "Get detailed information about a specific account" - provides core account details for the authenticated user's own account
  - **Purpose**: Retrieve core account information for the authenticated user's specific account
  - **Scala Implementation**: `APIMethods400.getCoreAccountById` -> `NewStyle.function.getCoreBankAccount()` -> `JSONFactory400.createCoreAccountJSON()`
  - **Request**: 
    ```
    GET /obp/v4.0.0/my/banks/BANK_ID/accounts/ACCOUNT_ID/account
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      ACCOUNT_ID: The unique identifier of the account (required)
    ```
  - **Response**: 
    ```json
    {
      "id": "account-id-001",
      "bank_id": "bank-id-001",
      "label": "My Checking Account",
      "number": "1234567890",
      "owners": [
        {
          "user_id": "user-001",
          "provider": "OBP",
          "display_name": "John Doe"
        }
      ],
      "type": "CHECKING",
      "balance": {
        "currency": "USD",
        "amount": "1500.00"
      },
      "account_routings": [
        {
          "scheme": "IBAN",
          "address": "US12345678901234567890"
        }
      ],
      "account_attributes": [
        {
          "product_code": "CHECKING_STANDARD",
          "account_attribute_id": "attr-001",
          "name": "OVERDRAFT_LIMIT",
          "type": "STRING",
          "value": "500.00"
        }
      ]
    }
    ```

### Endpoint 3: Get Account by ID (Moderated View)

- **Endpoint**: `GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/account`
  - **Justification (from description)**: "Get detailed information about a specific account including balance and metadata" - provides view-moderated account details
  - **Purpose**: Retrieve account information moderated by a specific view's permissions
  - **Scala Implementation**: `APIMethods400.getModeratedAccountById` -> `NewStyle.function.getModeratedBankAccount()` -> `JSONFactory400.createModeratedAccountJSON()`
  - **Request**: 
    ```
    GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/account
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      ACCOUNT_ID: The unique identifier of the account (required)
      VIEW_ID: The view through which to access the account (required)
    ```
  - **Response**: 
    ```json
    {
      "id": "account-id-001",
      "bank_id": "bank-id-001",
      "label": "My Checking Account",
      "number": "****7890",
      "owners": [
        {
          "user_id": "user-001",
          "provider": "OBP",
          "display_name": "John Doe"
        }
      ],
      "type": "CHECKING",
      "balance": {
        "currency": "USD",
        "amount": "1500.00"
      },
      "account_routings": [
        {
          "scheme": "IBAN",
          "address": "US12345678901234567890"
        }
      ],
      "views_available": [
        {
          "id": "accountant",
          "short_name": "Accountant",
          "is_public": false
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
| GET /banks/{bank_id}/accounts | LIST | No "list", "browse", or "search" verb in description - this capability is for a "specific account" |

---

## Business Rules (from capability description)

1. **Specific Account Focus**: This capability retrieves information for a single, specific account identified by its unique ID (from: "a specific account")
2. **Detailed Information**: The system must return comprehensive account details, not just summary information (from: "detailed information")
3. **Balance Inclusion**: Account balance must be included in the response as a core requirement (from: "including balance")
4. **Metadata Inclusion**: Account metadata/attributes must be included in the response (from: "and metadata")
5. **Real-time Access**: Account details must be retrieved in real-time with low latency (from: Frequency = Real-time)
6. **High Volume Support**: The system must be designed to handle high volume of account detail requests efficiently (from: Volume = High)
7. **View-Based Access Control**: The information returned is moderated by the view through which the account is accessed

---

## Data Validations (if applicable)

- Bank identifier (BANK_ID) must be valid and exist in the system
- Account identifier (ACCOUNT_ID) must be valid and exist within the specified bank
- View identifier (VIEW_ID) must be valid and the user must have access to this view for the account
- User must be authenticated with a valid OAuth token or DirectLogin credentials
- User must have at least one view/permission granted on the account to retrieve its details
- Account number may be masked based on the user's view permissions
- Balance information is only included if the user's view permits balance access
- Owner information is only included if the user's view permits owner visibility
- Error response (HTTP 404 Not Found / `BankNotFound`) must be returned when BANK_ID does not exist
- Error response (HTTP 404 Not Found / `AccountNotFound`) must be returned when ACCOUNT_ID does not exist
- Error response (HTTP 401 Unauthorized) for missing or invalid authentication
- Error response (HTTP 403 Forbidden / `ViewNotFound`) when user lacks permission to access the specified view
- Error response (HTTP 403 Forbidden / `UserNoPermissionAccessView`) when user cannot access the account through the specified view
- All monetary values must include currency code and properly formatted amount

---

## Dependencies

- **Upstream**: 
  - Bank must exist in the system (Bank Creation capability)
  - Account must be created and linked to the bank (Account Creation capability)
  - User must be authenticated (Authentication & Security capabilities)
  - User must have been granted view/permission access to the account (View & Permission Management capabilities)
  - Views must be defined for the account (View Creation capability)

- **Downstream**: 
  - Retrieved account details are used to display account information in third-party applications
  - Balance information is used for financial planning, budgeting, and reporting features
  - Account metadata is used for personalized services and account categorization
  - Account routing information is used for payment initiation flows
  - Account details verification before initiating transactions

- **External Systems**: 
  - Backend banking connectors may retrieve account data from core banking systems
  - Account balance information may be fetched in real-time from external systems
  - Account metadata may be synchronized with external account management systems

---

## Notes for Implementation

- **Performance Consideration**: Implement caching strategies for account details as they may be accessed frequently, but ensure cache invalidation when account data changes
- **View Moderation**: Different views may expose different levels of detail - implement proper view-based data filtering
- **Balance Freshness**: Consider indicating balance freshness/timestamp for accounts where balance is retrieved from external systems
- **Metadata Extensibility**: Account attributes/metadata should support extensible key-value pairs for custom bank-specific information
- **Partial Data Handling**: Handle cases where some account data may be unavailable from backend systems gracefully
- **Concurrent Access**: Ensure thread-safe access to account data for high-volume concurrent requests

### Needs SME Input
- Clarify the exact metadata fields that should be included in the "detailed information"
- Determine if there are different detail levels for account retrieval (minimal vs. full)
- Confirm if balance should always be real-time or if cached values are acceptable
- Clarify handling of accounts with multiple currencies
- Determine if historical balance information should be included or is a separate capability
- Confirm behavior when account exists but balance is temporarily unavailable from backend

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical implementation details)
- [x] User role is clearly identified (third-party developer, fintech application, account holder)
- [x] Business value is stated (display account details, verify status, access metadata, retrieve balance)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (Needs SME Input section)
- [x] Only relevant endpoints are included (GET operations only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, I can point to a specific word or phrase in the capability description that justifies this endpoint ("Get detailed information about a specific account including balance and metadata")
- [x] No endpoint type (create, update, view, list, delete) has been added unless its verb (or a clear synonym) appears in the description
- [x] Words like "manage" have been interpreted narrowly - not applicable as "manage" is not in the description
