# User Story for KYC Status Management

## Story Overview

**As a** Compliance Officer or Bank Administrator  
**I want to** manage customer KYC verification status  
**So that** I can maintain accurate records of customer KYC compliance status over time and ensure regulatory requirements are met

## Acceptance Criteria

1. The system shall allow authorized users to update the KYC verification status for a specific customer
2. The system shall record the KYC status with a timestamp indicating when the status was set
3. The system shall maintain a timeline of KYC status changes for each customer
4. The system shall validate that the customer exists before updating their KYC status
5. The system shall validate that the bank exists before processing the KYC status update
6. The system shall require appropriate entitlements (canAddKycStatus) to update KYC status
7. The system shall support both creating new KYC status records and updating existing ones (upsert behavior)
8. The system shall return the created/updated KYC status record upon successful operation

## Technical Context

### Classes/Services Involved
- **KycStatusProvider**: Trait defining the interface for KYC status operations
- **MappedKycStatusesProvider**: Implementation of KycStatusProvider using database mapping
- **MappedKycStatus**: Entity class representing a KYC status record with fields for bankId, customerId, customerNumber, ok (boolean status), and date
- **APIMethods200**: API endpoint definitions for KYC status management
- **JSONFactory200**: JSON serialization/deserialization for KYC status request/response
- **NewStyle.function**: Helper functions for entitlement checking and database operations
- **Connector**: Backend connector for database operations

### Input Data

**Request Body (PostKycStatusJSON)**:
```json
{
  "customer_number": "string",
  "ok": boolean,
  "date": "date"
}
```

**Path Parameters**:
- `BANK_ID`: The identifier of the bank
- `CUSTOMER_ID`: The identifier of the customer

### Output Data

**Response Body (KycStatusJSON)**:
```json
{
  "customer_id": "string",
  "customer_number": "string",
  "ok": boolean,
  "date": "date"
}
```

### Processing Type
- **API**: REST API / HTTP request-response
- **On-demand**: Triggered by user action
- **Volume**: Medium

## Relevant Endpoints

**IMPORTANT**: Based on the capability description "Manage customer KYC verification status", only the update/configure operation is justified.

### Endpoint 1: Add/Update KYC Status

- **Endpoint**: `PUT /banks/BANK_ID/customers/CUSTOMER_ID/kyc_statuses`
- **Justification (from description)**: "Manage" - The word "manage" justifies update/configure operations for KYC verification status
- **Purpose**: Create or update the KYC verification status for a specific customer at a specific bank
- **Request**: 
  - Method: PUT
  - Path Parameters: BANK_ID, CUSTOMER_ID
  - Body: PostKycStatusJSON containing customer_number, ok (boolean), and date
- **Response**: 
  - Success: HTTP 201 with KycStatusJSON containing customer_id, customer_number, ok, and date
  - Errors: 
    - 400: Invalid JSON format, Invalid Bank ID format
    - 401: User not logged in
    - 403: User lacks canAddKycStatus entitlement
    - 404: Bank not found, Customer not found
    - 500: Server error adding data

## Business Rules (from capability description)

1. **Upsert Behavior**: If a KYC status record already exists for the given bank and customer combination, it will be updated; otherwise, a new record will be created
2. **Timeline Tracking**: KYC status records are maintained as a timeline, ordered by update timestamp (descending), allowing tracking of status changes over time
3. **Bank-Customer Association**: Each KYC status is associated with both a bank and a customer, ensuring proper data isolation
4. **Boolean Status**: The KYC verification status is represented as a boolean (ok: true/false) indicating whether the customer has passed KYC verification
5. **Date Recording**: Each status update includes a date field to record when the KYC verification was performed

## Data Validations

- **Bank Validation**: The specified BANK_ID must correspond to an existing bank in the system
- **Customer Validation**: The specified CUSTOMER_ID must correspond to an existing customer in the system
- **JSON Format Validation**: The request body must be valid JSON conforming to the PostKycStatusJSON schema
- **Authentication**: User must be authenticated to access this endpoint
- **Authorization**: User must have the `canAddKycStatus` entitlement for the specified bank

## Dependencies

### Upstream
- **Customer Management**: Customer must exist before KYC status can be managed
- **Bank Management**: Bank must exist before KYC status can be managed
- **User Authentication**: User must be authenticated via OAuth or Direct Login
- **Entitlement Management**: User must have appropriate entitlements granted

### Downstream
- **Compliance Reporting**: KYC status data may be used for compliance reports
- **Account Operations**: KYC status may affect customer's ability to perform certain account operations
- **Audit Trail**: KYC status changes are tracked for audit purposes

### External Systems
- None explicitly mentioned in the capability description

## Notes for Implementation

### Special Considerations
1. The endpoint uses PUT method but performs an upsert operation (create or update)
2. The customer_number is included in both the URL path (via CUSTOMER_ID) and the request body, which may be redundant
3. The KYC status is stored with created/updated timestamps for audit purposes
4. The implementation uses Lift Mapper for database operations

### Known Complexity
1. The upsert logic requires checking for existing records before deciding to create or update
2. Entitlement checking is bank-specific, requiring the user to have permissions for the specific bank

### Missing or Unclear Requirements (Needs SME Input)
1. What are the valid values for the `ok` field beyond true/false?
2. Should there be validation on the date field (e.g., cannot be in the future)?
3. What is the relationship between customer_number and customer_id?
4. Are there any business rules about how often KYC status can be updated?
5. Should there be notifications when KYC status changes?
6. What happens to existing KYC status records when a customer is deleted?

---

## Quality Checklist

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Compliance Officer / Bank Administrator)
- [x] Business value is stated (maintain KYC compliance records)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged
- [x] Only relevant endpoints are included
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, I can point to a specific word or phrase in the capability description that justifies this endpoint ("Manage")
- [x] No endpoint type has been added unless its verb appears in the description
- [x] "Manage" has been interpreted narrowly as update/configure only - view/list/delete operations are NOT included since they are not explicitly mentioned in the description
