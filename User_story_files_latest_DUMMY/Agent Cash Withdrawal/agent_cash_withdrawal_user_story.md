# User Story for Agent Cash Withdrawal

## Capability Input

- **Name**: Agent Cash Withdrawal
- **Description**: Process cash withdrawal requests through agents
- **Frequency**: On-demand
- **Volume**: Medium

---

## Operation Verb Analysis

Based on the capability description, the following operation verbs are identified:

| Verb Found | Operation Type | Justification |
|------------|---------------|---------------|
| "Process" | CREATE/INITIATE | Explicitly stated: "Process cash withdrawal requests through agents" |

**Operations NOT included** (verbs not present in description):
- READ/RETRIEVAL operations: No "view", "retrieve", "get", "list", "search", "browse", "query", "lookup", "find", "show", "read", "access", or "fetch" mentioned
- UPDATE operations: No "manage", "configure", "maintain", "update", "modify", "change", "edit", "adjust", "set", or "customize" mentioned
- DELETE operations: No "delete", "remove", "deactivate", "close", "terminate", "disable", "archive", "retire", or "cancel" mentioned

---

## Story Overview

**As a** bank account holder or authorized user of the Open Bank Project platform
**I want to** process cash withdrawal requests through registered agents
**So that** I can withdraw cash from my bank account via an agent location without visiting a physical bank branch or ATM, enabling convenient access to funds through the agent banking network

---

## Acceptance Criteria

1. The system shall allow authenticated users to initiate cash withdrawal requests through registered agents
2. The system shall validate that the specified agent exists and is identified by bank_id and agent_number
3. The system shall verify that the agent is confirmed and not in pending status before processing the withdrawal
4. The system shall validate that the withdrawal amount is positive and in a valid currency
5. The system shall validate the charge policy for the transaction
6. The system shall create a transaction request record with appropriate status tracking
7. The system shall support Strong Customer Authentication (SCA) challenges when required by the transaction type configuration
8. The system shall transfer funds from the user's account to the agent's linked account upon successful processing
9. The system shall return a transaction request response with charge information and status
10. The system shall return appropriate error responses for invalid agent numbers, insufficient funds, or unauthorized requests

---

## Technical Context

- **Classes/Services Involved** (from Scala source code):
  - `APIMethods400` (code.api.v4_0_0.APIMethods400) - REST endpoint definition for `createTransactionRequestAgentCashWithDrawal`
  - `JSONFactory400` (code.api.v4_0_0.JSONFactory4.0.0) - JSON factories including `AgentCashWithdrawalJson` and `TransactionRequestBodyAgentJsonV400`
  - `LocalMappedConnectorInternal` (code.bankconnectors.LocalMappedConnectorInternal) - Business logic for AGENT_CASH_WITHDRAWAL transaction type processing
  - `NewStyle.function` (code.api.util.NewStyle) - Service layer methods including `getAgentByAgentNumber`, `getAgentAccountLinksByAgentId`, `createTransactionRequestv400`
  - `TransactionRequestAgentCashWithdrawal` (com.openbankproject.commons.model.CommonModel) - Model class for agent cash withdrawal details
  - `TransactionRequestTypes.AGENT_CASH_WITHDRAWAL` (com.openbankproject.commons.model.enums.Enumerations) - Enum value for this transaction type

- **Input Data** (based on `TransactionRequestBodyAgentJsonV400` case class):
  - `to` (AgentCashWithdrawalJson) - Agent identification containing:
    - `bank_id` (String) - The bank identifier where the agent is registered
    - `agent_number` (String) - The unique agent number to identify the agent
  - `value` (AmountOfMoneyJsonV121) - Withdrawal amount containing:
    - `currency` (String) - Currency code (e.g., "EUR", "USD")
    - `amount` (String) - Withdrawal amount as string
  - `description` (String) - Description of the withdrawal transaction
  - `charge_policy` (String) - Charge policy for the transaction (e.g., "SHARED", "SENDER", "RECEIVER")
  - `future_date` (Option[String]) - Optional future date for scheduled withdrawals

- **Output Data** (based on `TransactionRequestWithChargeJSON400`):
  - `id` (String) - Unique transaction request identifier
  - `type` (String) - Transaction request type ("AGENT_CASH_WITHDRAWAL")
  - `from` (TransactionRequestAccountJsonV140) - Source account details
  - `details` (TransactionRequestBodyAllTypes) - Transaction details including agent information
  - `transaction_ids` (List[String]) - Associated transaction IDs
  - `status` (String) - Current status of the transaction request
  - `start_date` (Date) - When the request was initiated
  - `end_date` (Date) - When the request was completed
  - `challenge` (TransactionRequestChallengeJsonV140) - SCA challenge details if applicable
  - `charge` (TransactionRequestChargeJsonV200) - Charge information for the transaction

- **Processing Type**: API / On-demand / Synchronous request-response with potential SCA challenge flow

---

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description. Only CREATE/INITIATE operations are included as the description only contains the verb "Process".

### Endpoint 1: Create Transaction Request (AGENT_CASH_WITHDRAWAL)

- **Endpoint**: `POST /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-request-types/AGENT_CASH_WITHDRAWAL/transaction-requests`
  - **Justification (from description)**: "Process cash withdrawal requests through agents" - the verb "Process" indicates initiating/creating a transaction request for cash withdrawal
  - **Purpose**: Initiate a cash withdrawal transaction request that will transfer funds from the user's account to a registered agent's account, enabling the user to collect cash from the agent
  - **Scala Implementation**: `APIMethods400.createTransactionRequestAgentCashWithDrawal` -> `LocalMappedConnectorInternal.createTransactionRequest()` with `AGENT_CASH_WITHDRAWAL` type
  - **Request**: 
    ```
    POST /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-request-types/AGENT_CASH_WITHDRAWAL/transaction-requests
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The bank identifier of the source account (required)
      ACCOUNT_ID: The account identifier to withdraw from (required)
      VIEW_ID: The view identifier for account access (required)
    Body:
    {
      "to": {
        "bank_id": "agent-bank-id",
        "agent_number": "AGENT-001"
      },
      "value": {
        "currency": "EUR",
        "amount": "100.00"
      },
      "description": "Cash withdrawal via agent",
      "charge_policy": "SHARED",
      "future_date": "2024-12-31"
    }
    ```
  - **Response** (based on `TransactionRequestWithChargeJSON400`): 
    ```json
    {
      "id": "transaction-request-id-001",
      "type": "AGENT_CASH_WITHDRAWAL",
      "from": {
        "bank_id": "bank-id-001",
        "account_id": "account-id-001"
      },
      "details": {
        "to_agent": {
          "bank_id": "agent-bank-id",
          "agent_number": "AGENT-001"
        },
        "value": {
          "currency": "EUR",
          "amount": "100.00"
        },
        "description": "Cash withdrawal via agent"
      },
      "transaction_ids": ["txn-001"],
      "status": "INITIATED",
      "start_date": "2024-01-15T10:30:00Z",
      "end_date": null,
      "challenge": {
        "id": "challenge-id-001",
        "allowed_attempts": 3,
        "challenge_type": "OTP_VIA_API"
      },
      "charge": {
        "summary": "Transaction charge",
        "value": {
          "currency": "EUR",
          "amount": "1.00"
        }
      }
    }
    ```

### Endpoints NOT Included (with justification)

| Endpoint | Operation Type | Reason for Exclusion |
|----------|---------------|---------------------|
| GET /transaction-requests/{id} | READ | No "view", "retrieve", "get", "check", or "status" mentioned in description |
| GET /transaction-requests | LIST | No "list", "browse", "search", or similar verb in description |
| PUT /transaction-requests/{id} | UPDATE | No "update", "modify", "manage", or similar verb in description |
| DELETE /transaction-requests/{id} | DELETE | No "cancel", "delete", "remove", or similar verb in description |

---

## Business Rules (from capability description)

1. **Agent-Based Processing**: Cash withdrawal requests must be processed through registered agents (from: "through agents")
2. **Request Processing**: The system must support processing of cash withdrawal requests as a transaction type (from: "Process cash withdrawal requests")
3. **On-Demand Availability**: The capability must be available on-demand when users need to withdraw cash (from: Frequency = On-demand)
4. **Medium Volume Support**: The system should be designed to handle medium volume of withdrawal requests (from: Volume = Medium)

### Additional Business Rules (from Scala implementation):

5. **Agent Validation**: The agent must be found using the combination of `bank_id` and `agent_number`
6. **Agent Status Check**: The agent must be confirmed (`isConfirmedAgent = true`) and not pending (`isPendingAgent = false`) to process withdrawals
7. **Agent Account Link**: The agent must have a linked bank account to receive the transferred funds
8. **Charge Policy Validation**: The charge policy must be a valid value from the ChargePolicy enum (SHARED, SENDER, RECEIVER)
9. **SCA Challenge**: Strong Customer Authentication may be required based on the `AGENT_CASH_WITHDRAWAL_OTP_INSTRUCTION_TRANSPORT` configuration

---

## Data Validations (if applicable)

- Bank identifier (BANK_ID) must be valid and exist in the system
- Account identifier (ACCOUNT_ID) must be valid and accessible by the authenticated user
- View identifier (VIEW_ID) must grant appropriate permissions for transaction initiation
- Agent's `bank_id` must be a valid bank identifier in the system
- Agent's `agent_number` must correspond to a registered agent at the specified bank
- Withdrawal amount must be positive and non-zero (`NotPositiveAmount` error if invalid)
- Currency must be a valid ISO currency code (`InvalidTransactionRequestCurrency` error if invalid)
- Charge policy must be one of: "SHARED", "SENDER", "RECEIVER" (`InvalidChargePolicy` error if invalid)
- JSON request body must conform to `TransactionRequestBodyAgentJsonV400` format (`InvalidJsonFormat` error if malformed)
- User must have sufficient authorization to create transaction requests (`InsufficientAuthorisationToCreateTransactionRequest` error if unauthorized)
- Transaction requests must be enabled for the bank (`TransactionDisabled` error if disabled)
- Agent must not be in pending status (`AgentBeneficiaryPermit` error if agent is pending or not confirmed)
- Agent must have at least one linked account (`AgentAccountLinkNotFound` error if no account link exists)

---

## Dependencies

- **Upstream**: 
  - User must be authenticated via OAuth or DirectLogin
  - Source bank account must exist and be accessible to the user
  - Agent must be registered in the system with a valid agent_number
  - Agent must have a linked bank account for receiving funds
  - Agent must be in confirmed status (not pending)
  - Transaction request type AGENT_CASH_WITHDRAWAL must be enabled in system configuration (`transactionRequests_supported_types`)

- **Downstream**: 
  - Upon successful processing, funds are transferred from user's account to agent's linked account
  - Transaction records are created for audit and history purposes
  - If SCA is required, a challenge is created that must be answered via the Challenge Response capability
  - Transaction request status is updated as the request progresses through the workflow

- **External Systems**: 
  - OTP delivery system for Strong Customer Authentication (configured via `AGENT_CASH_WITHDRAWAL_OTP_INSTRUCTION_TRANSPORT`)
  - Backend banking connector for actual fund transfer execution
  - Agent management system for agent validation and account linking

---

## Notes for Implementation

- **SCA Configuration**: The system supports configurable SCA methods for agent cash withdrawals via the `AGENT_CASH_WITHDRAWAL_OTP_INSTRUCTION_TRANSPORT` property
- **Agent Lookup**: Agents are identified by the combination of `bank_id` and `agent_number`, not by a single unique identifier
- **Account Linking**: The agent's receiving account is determined through the `AgentAccountLink` relationship, taking the first linked account
- **Charge Policy**: The charge policy determines how transaction fees are allocated between sender and receiver
- **Future Dating**: The optional `future_date` field allows scheduling withdrawals for a future date

### Needs SME Input
- Clarify the exact workflow for physical cash collection after the transaction request is processed
- Determine if there are withdrawal limits per transaction or per day for agent cash withdrawals
- Confirm the supported charge policies and their fee structures for agent transactions
- Clarify the agent confirmation process and what criteria determine `isConfirmedAgent` status
- Determine if multiple agent account links are supported and how the system should handle selection
- Confirm the SCA challenge flow and timeout requirements for agent cash withdrawals

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical implementation details)
- [x] User role is clearly identified (bank account holder/authorized user)
- [x] Business value is stated (convenient cash access through agent network)
- [x] Acceptance criteria are testable and measurable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged for SME review
- [x] Only relevant endpoints are included (POST operation only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words from the capability description justify inclusion
- [x] No CRUD operations are inferred beyond what the description explicitly states (only "Process" mentioned)
- [x] Words like "manage" have been interpreted narrowly - N/A (no "manage" in description)
