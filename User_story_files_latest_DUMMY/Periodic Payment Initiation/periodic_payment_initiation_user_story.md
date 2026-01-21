# User Story for Periodic Payment Initiation

## Story Overview

**As a** Bank Customer / Account Holder  
**I want to** set up recurring/periodic payment schedules  
**So that** I can automate regular payments (such as subscriptions, bills, rent, or loan repayments) without having to manually initiate each payment, ensuring timely payments and reducing the risk of missed payment deadlines

## Acceptance Criteria

1. The system shall allow authorized users to set up a new periodic/recurring payment schedule
2. The system shall accept and validate payment schedule parameters including frequency (daily, weekly, monthly, etc.), start date, end date (if applicable), and payment amount
3. The system shall validate the source account has sufficient permissions for periodic payment setup
4. The system shall validate the beneficiary/recipient account information
5. The system shall generate a unique identifier for each periodic payment schedule created
6. The system shall associate the periodic payment with the specified source account and bank
7. The system shall return confirmation of successful periodic payment setup with the created schedule details
8. The system shall reject setup requests with invalid or incomplete data with appropriate error messages
9. The system shall ensure the periodic payment schedule is created with proper initial state

## Technical Context

- **Classes/Services Involved**: 
  - Periodic Payment entity/model classes
  - Payment schedule service/handler
  - Account validation service
  - Beneficiary/Counterparty validation service
  - Payment frequency/recurrence calculation service
  - Database/persistence layer for periodic payment storage

- **Input Data**: 
  - Bank ID (required) - the bank under which the periodic payment is set up
  - Account ID (required) - the source account for the payments
  - Beneficiary/Counterparty information (account details, IBAN, etc.)
  - Payment amount and currency
  - Payment frequency (daily, weekly, monthly, quarterly, annually)
  - Start date for the recurring payments
  - End date (optional) - when the recurring payments should stop
  - Number of payments (optional) - alternative to end date
  - Payment description/reference
  - Execution day (e.g., day of month for monthly payments)

- **Output Data**: 
  - Created periodic payment entity with generated schedule ID
  - Payment schedule parameters as stored
  - Source account confirmation
  - Beneficiary details
  - Next execution date
  - Creation timestamp
  - Success/error response

- **Processing Type**: API / On-demand

## Relevant Endpoints

**IMPORTANT**: For every endpoint included, justification from the description is provided.

### Endpoint 1: Create Periodic Payment Schedule
- **Endpoint**: `POST /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/periodic-payments`
  - **Justification (from description)**: "Set up recurring/periodic payment schedules" - the phrase "Set up" explicitly justifies a POST endpoint for creating periodic payment schedules
  - **Purpose**: Create a new periodic/recurring payment schedule for automated payments from a specified account
  - **Request**: 
    ```json
    {
      "to": {
        "bank_id": "string",
        "account_id": "string"
      },
      "value": {
        "currency": "string",
        "amount": "string"
      },
      "description": "string",
      "start_date": "2024-01-15",
      "end_date": "2025-01-15",
      "frequency": "MONTHLY",
      "day_of_execution": "15",
      "creditor_account": {
        "iban": "string"
      },
      "creditor_name": "string"
    }
    ```
  - **Response**: 
    ```json
    {
      "periodic_payment_id": "string",
      "from": {
        "bank_id": "string",
        "account_id": "string"
      },
      "to": {
        "bank_id": "string",
        "account_id": "string"
      },
      "value": {
        "currency": "string",
        "amount": "string"
      },
      "description": "string",
      "start_date": "2024-01-15",
      "end_date": "2025-01-15",
      "frequency": "MONTHLY",
      "day_of_execution": "15",
      "next_execution_date": "2024-02-15",
      "status": "ACTIVE",
      "created_at": "timestamp"
    }
    ```

### Endpoint 2: Create Periodic Payment with Transaction Request
- **Endpoint**: `POST /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/transaction-request-types/PERIODIC/transaction-requests`
  - **Justification (from description)**: "Set up recurring/periodic payment schedules" - this is an alternative endpoint pattern for setting up periodic payments through the transaction request mechanism
  - **Purpose**: Create a periodic payment schedule using the transaction request workflow, which may include additional authorization steps
  - **Request**: 
    ```json
    {
      "to": {
        "bank_id": "string",
        "account_id": "string"
      },
      "value": {
        "currency": "string",
        "amount": "string"
      },
      "description": "string",
      "start_date": "2024-01-15",
      "frequency": "MONTHLY",
      "day_of_execution": "15"
    }
    ```
  - **Response**: 
    ```json
    {
      "id": "string",
      "type": "PERIODIC",
      "from": {
        "bank_id": "string",
        "account_id": "string"
      },
      "body": {
        "to": {...},
        "value": {...},
        "description": "string",
        "start_date": "2024-01-15",
        "frequency": "MONTHLY"
      },
      "status": "INITIATED",
      "challenge": {...},
      "created_at": "timestamp"
    }
    ```

**Note**: Based on the Operation Derivation Rules, the following endpoints are explicitly EXCLUDED because their operation types are not mentioned in the capability description:
- GET /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/periodic-payments - No "view", "retrieve", "list", or "get" mentioned
- GET /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/periodic-payments/{PERIODIC_PAYMENT_ID} - No "view" or "retrieve" mentioned
- PUT /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/periodic-payments/{PERIODIC_PAYMENT_ID} - No "update", "modify", or "manage" mentioned
- DELETE /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/periodic-payments/{PERIODIC_PAYMENT_ID} - No "delete", "cancel", or "remove" mentioned

## Business Rules (from capability description)

1. **Recurring Nature**: Periodic payments are recurring/scheduled payments that execute automatically based on defined frequency
2. **Schedule-Based**: Payments follow a defined schedule (frequency, start date, execution day)
3. **Account Association**: Each periodic payment must be associated with a source account at a specific bank
4. **On-demand Setup**: Periodic payment setup is performed on-demand (not batch or scheduled)
5. **Medium Volume Operation**: Periodic payment initiation is expected to be a medium-volume operation
6. **Authorization Required**: Only authorized users (account holders with appropriate permissions) can set up periodic payments

## Data Validations (if applicable)

- Bank ID must reference an existing bank on the platform
- Account ID must reference an existing, valid account owned by or accessible to the user
- Source account must have sufficient balance or credit limit for the payment amount
- Payment amount must be a valid positive numeric value
- Currency code must be a valid ISO 4217 currency code
- Start date must be a valid future date or current date
- End date (if provided) must be after the start date
- Frequency must be a valid recurrence pattern (DAILY, WEEKLY, MONTHLY, QUARTERLY, ANNUALLY)
- Day of execution must be valid for the specified frequency (e.g., 1-31 for monthly)
- Beneficiary account information must be valid (IBAN format, account number, etc.)
- Creditor name must not be empty if required

## Dependencies

- **Upstream**: 
  - User authentication and authorization must be completed
  - User must have appropriate entitlements/roles for periodic payment setup (e.g., CanCreatePeriodicPayment)
  - The source bank and account must exist on the platform
  - The source account must be accessible to the authenticated user
  - Beneficiary/counterparty information must be valid

- **Downstream**: 
  - After periodic payment setup, the system will:
    - Execute payments automatically according to the defined schedule
    - Create individual transaction records for each executed payment
    - Update account balances upon each payment execution
    - Generate notifications for payment execution (success/failure)
    - Track payment history for the periodic payment schedule

- **External Systems**: 
  - Database/persistence layer for storing periodic payment schedules
  - Payment execution engine for processing scheduled payments
  - Account service for balance validation and updates
  - Notification service for payment alerts
  - Scheduler/job service for triggering payments at defined times

## Notes for Implementation

- **Authorization**: Ensure proper role-based access control - only users with CanCreatePeriodicPayment or similar entitlement should be able to set up periodic payments
- **Account Validation**: Verify that the source account exists, is active, and is accessible to the user
- **Balance Consideration**: While balance validation at setup time is optional, consider warning users if current balance is insufficient
- **Frequency Handling**: Implement robust frequency calculation logic to handle edge cases (e.g., monthly payments on the 31st)
- **Time Zone Handling**: Define clear rules for execution timing across different time zones
- **Idempotency**: Consider implementing idempotency for periodic payment setup to handle duplicate requests gracefully
- **Validation Order**: Validate all input data before attempting to persist to avoid partial creation states
- **Audit Trail**: Log periodic payment setup events for compliance and audit purposes
- **Error Handling**: Provide clear, actionable error messages for validation failures
- **SCA Compliance**: Consider Strong Customer Authentication (SCA) requirements for periodic payment setup as per PSD2 regulations

### Open Questions (Needs SME Input)

1. What are the supported payment frequencies (daily, weekly, monthly, quarterly, annually)?
2. What is the maximum duration allowed for a periodic payment schedule?
3. Is there a maximum number of periodic payments that can be set up per account?
4. What happens if a scheduled payment fails due to insufficient funds?
5. Can periodic payments be set up for cross-border/international transfers?
6. What currencies are supported for periodic payments?
7. Is there a minimum or maximum payment amount for periodic payments?
8. Should periodic payment setup require Strong Customer Authentication (SCA)?
9. What notification mechanisms are available for payment execution status?
10. Can the end date be left open for indefinite recurring payments?
11. What is the cut-off time for same-day payment execution?
12. Are there any restrictions on beneficiary types (individuals, businesses, etc.)?

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Bank Customer / Account Holder)
- [x] Business value is stated (automating regular payments, ensuring timely payments)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (Open Questions section)
- [x] Only relevant endpoints are included (POST for setup only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific phrase from capability description justifies inclusion ("Set up")
- [x] No endpoint type has been added unless its verb appears in the description
- [x] Words like "manage" have been interpreted narrowly - no view/list/delete operations included since not mentioned
- [x] No CRUD operations inferred beyond what description explicitly states ("Set up" = CREATE only)
