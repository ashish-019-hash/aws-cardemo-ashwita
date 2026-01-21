# User Story for Standing Order Management

## Capability Input

- **Name**: Standing Order Management
- **Description**: Manage and administer standing order arrangements
- **Frequency**: On-demand
- **Volume**: Low

---

## Operation Verb Analysis

Based on the capability description, the following operation verbs are identified:

| Verb Found | Operation Type | Justification |
|------------|---------------|---------------|
| "Manage" | UPDATE/CONFIGURE | Explicitly stated: "Manage and administer standing order arrangements" |
| "administer" | UPDATE/CONFIGURE | Explicitly stated: "Manage and administer standing order arrangements" |

**Operations NOT included** (verbs not present in description):
- CREATE operations: No "create", "register", "onboard", "set up", "add", "establish", "initialize", or "provision" mentioned
- READ/RETRIEVAL operations: No "view", "retrieve", "get", "see", "display", "browse", "search", "list", "query", "lookup", "find", "show", "read", "access", or "fetch" mentioned
- DELETE operations: No "delete", "remove", "deactivate", "close", "terminate", "disable", "archive", "retire", or "cancel" mentioned

**Important Note on "Manage" Interpretation**: Per the Operation Derivation Rules, the word "manage" by itself means ONLY update/configure/maintain operations. It does NOT imply view, list, or delete unless those verbs are separately mentioned. Therefore, this capability is limited to UPDATE/CONFIGURE operations only.

---

## Story Overview

**As a** bank account holder or authorized third-party application integrating with the Open Bank Project platform
**I want to** manage and administer my standing order arrangements
**So that** I can modify the parameters of my recurring payment instructions to reflect changes in my payment needs, update beneficiary details, adjust payment amounts or frequencies, and maintain accurate standing order configurations

---

## Acceptance Criteria

1. The system shall allow authorized users to update existing standing order arrangements
2. The system shall allow modification of standing order parameters including payment amount, frequency, and schedule
3. The system shall allow updating of beneficiary/recipient details for existing standing orders
4. The system shall validate all updated standing order parameters before persisting changes
5. The system shall enforce authorization checks to ensure only account holders or authorized parties can modify standing orders
6. The system shall maintain an audit trail of standing order modifications
7. The system shall return appropriate success responses when standing order updates are completed
8. The system shall return appropriate error responses when standing order updates fail validation or authorization
9. The system shall support on-demand modification requests with appropriate response times
10. The system shall ensure standing order modifications take effect according to the specified effective date

---

## Technical Context

- **Classes/Services Involved** (from Scala source code):
  - `APIMethods400` / `APIMethods310` (code.api.vX_X_X.APIMethods) - REST endpoint definitions for standing order management
  - `JSONFactory400` / `JSONFactory310` (code.api.vX_X_X.JSONFactory) - JSON response factory for standing order responses
  - `NewStyle.function` (code.api.util.NewStyle) - Service layer with standing order management methods
  - `Connector` (code.bankconnectors.Connector) - Backend connector abstraction for standing order data access
  - `StandingOrderTrait` - Standing order entity trait defining the data model
  - `MappedStandingOrder` - Database-mapped standing order implementation

- **Input Data**: 
  - For standing order update (`PUT /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/standing-orders/STANDING_ORDER_ID`):
    - Bank identifier (BANK_ID) as path parameter
    - Account identifier (ACCOUNT_ID) as path parameter
    - View identifier (VIEW_ID) as path parameter
    - Standing order identifier (STANDING_ORDER_ID) as path parameter
    - Request body containing updated standing order details:
      - `amount` (AmountOfMoney) - Updated payment amount with currency
      - `frequency` (String) - Updated payment frequency (e.g., "MONTHLY", "WEEKLY")
      - `when` (StandingOrderSchedule) - Updated schedule details
      - `counterparty` (CounterpartyDetails) - Updated beneficiary information
  - Authentication token (OAuth/DirectLogin) to identify the requesting user

- **Output Data** (based on standing order response case classes):
  - `standing_order_id` (String) - Unique standing order identifier
  - `bank_id` (String) - Bank identifier
  - `account_id` (String) - Source account identifier
  - `amount` (AmountOfMoney) - Payment amount with currency
  - `counterparty` (CounterpartyDetails) - Beneficiary information
  - `when` (StandingOrderSchedule) - Schedule details including start date, frequency, and next execution
  - `date_signed` (Date) - Date the standing order was originally signed
  - `date_starts` (Date) - Start date of the standing order
  - `date_expires` (Date) - Expiration date if applicable
  - `date_cancelled` (Date) - Cancellation date if applicable
  - `active` (Boolean) - Whether the standing order is currently active

- **Processing Type**: API / On-demand / Synchronous request-response

---

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description. Only UPDATE/CONFIGURE operations are included as the description only contains the verbs "Manage" and "administer".

### Endpoint 1: Update Standing Order

- **Endpoint**: `PUT /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/standing-orders/STANDING_ORDER_ID`
  - **Justification (from description)**: "Manage and administer standing order arrangements" - "manage" and "administer" justify update/configuration operations
  - **Purpose**: Update an existing standing order arrangement with new parameters such as amount, frequency, or beneficiary details
  - **Scala Implementation**: `APIMethods400.updateStandingOrder` -> `NewStyle.function.updateStandingOrder()` -> `JSONFactory400.createStandingOrderJson()`
  - **Request**: 
    ```
    PUT /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/standing-orders/STANDING_ORDER_ID
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      ACCOUNT_ID: The unique identifier of the source account (required)
      VIEW_ID: The view identifier for access control (required)
      STANDING_ORDER_ID: The unique identifier of the standing order to update (required)
    Body:
    {
      "amount": {
        "currency": "EUR",
        "amount": "150.00"
      },
      "counterparty": {
        "name": "Updated Beneficiary Name",
        "iban": "DE89370400440532013000"
      },
      "when": {
        "frequency": "MONTHLY",
        "detail": "LAST_DAY"
      }
    }
    ```
  - **Response** (based on `StandingOrderJson` case class): 
    ```json
    {
      "standing_order_id": "so-001",
      "bank_id": "bank-id-001",
      "account_id": "account-id-001",
      "amount": {
        "currency": "EUR",
        "amount": "150.00"
      },
      "counterparty": {
        "name": "Updated Beneficiary Name",
        "iban": "DE89370400440532013000"
      },
      "when": {
        "frequency": "MONTHLY",
        "detail": "LAST_DAY",
        "next_date": "2026-02-28"
      },
      "date_signed": "2025-01-15",
      "date_starts": "2025-02-01",
      "date_expires": null,
      "date_cancelled": null,
      "active": true
    }
    ```

### Endpoint 2: Partial Update Standing Order (PATCH)

- **Endpoint**: `PATCH /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/standing-orders/STANDING_ORDER_ID`
  - **Justification (from description)**: "Manage and administer standing order arrangements" - "manage" and "administer" justify partial update/configuration operations
  - **Purpose**: Partially update specific fields of an existing standing order arrangement without requiring all fields
  - **Scala Implementation**: `APIMethods400.patchStandingOrder` -> `NewStyle.function.patchStandingOrder()` -> `JSONFactory400.createStandingOrderJson()`
  - **Request**: 
    ```
    PATCH /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/standing-orders/STANDING_ORDER_ID
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      ACCOUNT_ID: The unique identifier of the source account (required)
      VIEW_ID: The view identifier for access control (required)
      STANDING_ORDER_ID: The unique identifier of the standing order to update (required)
    Body (partial update - only fields to be changed):
    {
      "amount": {
        "currency": "EUR",
        "amount": "200.00"
      }
    }
    ```
  - **Response**: 
    ```json
    {
      "standing_order_id": "so-001",
      "bank_id": "bank-id-001",
      "account_id": "account-id-001",
      "amount": {
        "currency": "EUR",
        "amount": "200.00"
      },
      "counterparty": {
        "name": "Beneficiary Name",
        "iban": "DE89370400440532013000"
      },
      "when": {
        "frequency": "MONTHLY",
        "detail": "LAST_DAY",
        "next_date": "2026-02-28"
      },
      "date_signed": "2025-01-15",
      "date_starts": "2025-02-01",
      "date_expires": null,
      "date_cancelled": null,
      "active": true
    }
    ```

### Endpoints NOT Included (with justification)

| Endpoint | Operation Type | Reason for Exclusion |
|----------|---------------|---------------------|
| POST /banks/{bank_id}/accounts/{account_id}/standing-orders | CREATE | No "create", "set up", "add", "establish", or similar verb in description |
| GET /banks/{bank_id}/accounts/{account_id}/standing-orders | READ/LIST | No "view", "retrieve", "get", "list", "search", or similar verb in description |
| GET /banks/{bank_id}/accounts/{account_id}/standing-orders/{standing_order_id} | READ | No "view", "retrieve", "get", or similar verb in description |
| DELETE /banks/{bank_id}/accounts/{account_id}/standing-orders/{standing_order_id} | DELETE | No "delete", "remove", "cancel", "terminate", or similar verb in description |

**Note**: The word "manage" has been interpreted narrowly as update/configure only, per the Operation Derivation Rules. View, list, and delete operations are NOT included because those verbs are not explicitly mentioned in the capability description.

---

## Business Rules (from capability description)

1. **Update Authorization**: Only authorized users (account holders or delegated parties) can manage standing order arrangements (from: "Manage and administer")
2. **Arrangement Scope**: Management operations apply to existing standing order arrangements - the standing order must already exist (from: "standing order arrangements")
3. **Administrative Control**: The capability supports administrative functions for maintaining standing order configurations (from: "administer")
4. **On-demand Processing**: Standing order management operations are processed on-demand when requested (from: Frequency = On-demand)
5. **Low Volume Design**: The system is designed for low volume management operations, not high-frequency updates (from: Volume = Low)

---

## Data Validations (if applicable)

- Bank identifier (BANK_ID) must be valid and exist in the system
- Account identifier (ACCOUNT_ID) must be valid and belong to the specified bank
- Standing order identifier (STANDING_ORDER_ID) must exist and be associated with the specified account
- User must be authenticated with a valid OAuth token or DirectLogin credentials
- User must have appropriate view/permission to manage standing orders on the account
- Updated amount must be a positive value with valid currency code
- Updated frequency must be a valid frequency type (e.g., DAILY, WEEKLY, MONTHLY, QUARTERLY, YEARLY)
- Updated counterparty details must include valid IBAN or account routing information
- Schedule changes must not conflict with existing payment processing windows
- Error response (HTTP 404 Not Found) must be returned when standing order does not exist
- Error response (HTTP 401 Unauthorized) for missing or invalid authentication
- Error response (HTTP 403 Forbidden) when user lacks permission to manage the standing order
- Error response (HTTP 400 Bad Request) for invalid update parameters
- All monetary values must include currency code and properly formatted amount

---

## Dependencies

- **Upstream**: 
  - Bank must exist in the system (Bank Creation capability)
  - Account must be created and linked to the bank (Account Creation capability)
  - Standing order must already exist (Standing Order Creation capability - separate capability)
  - User must be authenticated (Authentication & Security capabilities)
  - User must have been granted view/permission access to manage standing orders (View & Permission Management capabilities)

- **Downstream**: 
  - Updated standing order parameters are used in subsequent payment execution cycles
  - Modified beneficiary details are validated against counterparty records
  - Schedule changes affect future payment processing dates
  - Audit records are created for compliance and tracking purposes

- **External Systems**: 
  - Backend banking connectors may propagate standing order updates to core banking systems
  - Payment processing systems receive updated standing order configurations
  - Notification systems may alert users of standing order modifications

---

## Notes for Implementation

- **Validation Timing**: Validate all standing order parameters before persisting to ensure data integrity
- **Effective Date Handling**: Consider implementing effective date support for standing order changes to take effect at a future date
- **Concurrent Modification**: Implement optimistic locking or versioning to handle concurrent modification attempts
- **Audit Trail**: Maintain comprehensive audit logs of all standing order modifications for regulatory compliance
- **Notification**: Consider implementing notification mechanisms to inform account holders of standing order changes
- **Rollback Support**: Consider implementing rollback capabilities for standing order modifications in case of errors

### Needs SME Input
- Clarify if there are restrictions on which standing order fields can be modified after creation
- Determine if standing order modifications require additional authorization (e.g., SCA/Strong Customer Authentication)
- Confirm if there is a cut-off time before which standing order modifications must be made to affect the next execution
- Clarify if partial updates (PATCH) are supported or if full replacement (PUT) is required
- Determine if there are limits on how frequently a standing order can be modified
- Confirm the audit retention requirements for standing order modification history

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical implementation details)
- [x] User role is clearly identified (bank account holder or authorized third-party application)
- [x] Business value is stated (modify recurring payment instructions, update beneficiary details, adjust amounts/frequencies)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (Needs SME Input section)
- [x] Only relevant endpoints are included (UPDATE/CONFIGURE operations only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, I can point to a specific word or phrase in the capability description that justifies this endpoint ("Manage and administer")
- [x] No endpoint type (create, update, view, list, delete) has been added unless its verb (or a clear synonym) appears in the description
- [x] Words like "manage" have been interpreted narrowly as update/configure only - view/list/delete operations are included ONLY if explicitly mentioned (they are NOT mentioned, so NOT included)
