# User Story for Direct Debit Management

## Story Overview

**As a** Bank Administrator or Account Manager  
**I want to** manage and administer direct debit arrangements  
**So that** I can maintain accurate direct debit configurations, update mandate details, and ensure proper administration of recurring collection agreements for customers

## Acceptance Criteria

1. The system shall allow authorized users to update existing direct debit arrangement details
2. The system shall allow administrators to modify direct debit mandate configurations
3. The system shall support administrative actions on direct debit arrangements such as status changes and parameter adjustments
4. The system shall validate all changes to direct debit arrangements before persisting them
5. The system shall maintain audit trails for all administrative changes to direct debit arrangements
6. The system shall enforce proper authorization before allowing management operations on direct debit arrangements

## Technical Context

- **Classes/Services Involved**: Direct Debit Management Service, Mandate Administration Service, Direct Debit Arrangement Handler
- **Input Data**: Direct debit arrangement identifiers, updated mandate parameters, administrative action requests, configuration changes
- **Output Data**: Updated direct debit arrangement records, confirmation of administrative actions, validation results
- **Processing Type**: API (On-demand, Real-time request-response)

## Relevant Endpoints

**IMPORTANT**: Based on the capability description "Manage and administer direct debit arrangements", only UPDATE/CONFIGURE operations are justified. The words "manage" and "administer" do NOT imply view, list, search, or delete operations per the Operation Derivation Rules.

### Endpoint 1: Update Direct Debit Arrangement

- **Endpoint**: `PUT /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/direct-debits/{DIRECT_DEBIT_ID}`
  - **Justification (from description)**: "Manage and administer" - the word "manage" justifies update/modify operations
  - **Purpose**: Update an existing direct debit arrangement's details and configuration
  - **Request**: 
    ```json
    {
      "mandate_reference": "string",
      "creditor_id": "string",
      "creditor_name": "string",
      "collection_amount": {
        "currency": "string",
        "amount": "string"
      },
      "collection_frequency": "string",
      "next_collection_date": "string",
      "status": "string"
    }
    ```
  - **Response**: 
    ```json
    {
      "direct_debit_id": "string",
      "bank_id": "string",
      "account_id": "string",
      "mandate_reference": "string",
      "creditor_id": "string",
      "creditor_name": "string",
      "collection_amount": {
        "currency": "string",
        "amount": "string"
      },
      "collection_frequency": "string",
      "next_collection_date": "string",
      "status": "string",
      "last_updated": "string"
    }
    ```

### Endpoint 2: Administer Direct Debit Status

- **Endpoint**: `PATCH /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/direct-debits/{DIRECT_DEBIT_ID}/status`
  - **Justification (from description)**: "administer" - justifies administrative configuration and status management operations
  - **Purpose**: Perform administrative actions on a direct debit arrangement such as suspending, resuming, or modifying status
  - **Request**: 
    ```json
    {
      "action": "string",
      "reason": "string",
      "effective_date": "string"
    }
    ```
  - **Response**: 
    ```json
    {
      "direct_debit_id": "string",
      "previous_status": "string",
      "new_status": "string",
      "action_performed": "string",
      "effective_date": "string",
      "administered_by": "string",
      "timestamp": "string"
    }
    ```

### Endpoint 3: Configure Direct Debit Parameters

- **Endpoint**: `PATCH /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/direct-debits/{DIRECT_DEBIT_ID}/configuration`
  - **Justification (from description)**: "Manage" - justifies configuration and parameter management operations
  - **Purpose**: Configure specific parameters of a direct debit arrangement without full update
  - **Request**: 
    ```json
    {
      "max_collection_amount": {
        "currency": "string",
        "amount": "string"
      },
      "notification_preferences": {
        "notify_before_collection": "boolean",
        "notification_days_before": "integer"
      },
      "retry_policy": {
        "max_retries": "integer",
        "retry_interval_days": "integer"
      }
    }
    ```
  - **Response**: 
    ```json
    {
      "direct_debit_id": "string",
      "configuration": {
        "max_collection_amount": {
          "currency": "string",
          "amount": "string"
        },
        "notification_preferences": {
          "notify_before_collection": "boolean",
          "notification_days_before": "integer"
        },
        "retry_policy": {
          "max_retries": "integer",
          "retry_interval_days": "integer"
        }
      },
      "last_configured": "string"
    }
    ```

## Endpoints NOT Included (with justification)

The following endpoint types are **NOT** included because they are not justified by the capability description:

- **GET /direct-debits** (List): NOT included - "manage" and "administer" do NOT imply "list", "view", or "retrieve"
- **GET /direct-debits/{id}** (View): NOT included - no "view", "retrieve", "get", or "read" mentioned in description
- **DELETE /direct-debits/{id}** (Delete): NOT included - no "delete", "remove", "cancel", or "terminate" mentioned in description
- **POST /direct-debits** (Create): NOT included - no "create", "add", or "establish" mentioned in description (Note: Direct Debit Creation is a separate capability #35)

## Business Rules (from capability description)

1. Direct debit arrangements must be properly managed to ensure accurate recurring collections
2. Administrative actions on direct debit arrangements require appropriate authorization
3. Changes to direct debit arrangements should be validated before being applied
4. Management operations should maintain the integrity of existing mandate agreements
5. Administrative changes should be traceable for audit and compliance purposes

## Data Validations (if applicable)

- Mandate reference must be valid and match existing records
- Collection amounts must be positive and within configured limits
- Collection frequency must be a valid frequency type (e.g., monthly, quarterly, annually)
- Dates must be valid and in the future for scheduled collections
- Status transitions must follow valid state machine rules
- Creditor ID must be a valid registered creditor

## Dependencies

- **Upstream**: 
  - Direct Debit Creation (Capability #35) - direct debit arrangements must exist before they can be managed
  - Account Management - accounts must exist and be active
  - Customer Management - customer must be linked to the account
  
- **Downstream**: 
  - Payment Processing - managed direct debits will be processed according to updated configurations
  - Notification Services - changes may trigger notifications to customers
  - Audit Logging - all management actions are logged

- **External Systems**: 
  - SEPA Direct Debit infrastructure (for European direct debits)
  - Creditor verification systems
  - Bank clearing systems

## Notes for Implementation

- **Needs SME Input**: Specific business rules for valid status transitions (e.g., can a suspended direct debit be directly terminated, or must it be resumed first?)
- **Needs SME Input**: Maximum limits for collection amounts and retry policies
- **Needs SME Input**: Required notification periods before collection date changes
- **Complexity Note**: Status management may require coordination with external clearing systems
- **Edge Case**: Handling management operations on direct debits that have pending collections
- **Security Consideration**: Ensure proper role-based access control for administrative operations
- **Audit Requirement**: All management and administrative actions must be logged with user identity and timestamp

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Bank Administrator or Account Manager)
- [x] Business value is stated (maintain accurate configurations, ensure proper administration)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (marked as "Needs SME Input")
- [x] Only relevant endpoints are included (UPDATE/CONFIGURE operations only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words from the capability description justify inclusion
- [x] No endpoint type (create, view, list, delete) has been added beyond what the description explicitly states
- [x] Words "manage" and "administer" have been interpreted narrowly as update/configure only

---

*This user story was extracted from the capability description: "Manage and administer direct debit arrangements" (Capability #36 from the BRD document)*
