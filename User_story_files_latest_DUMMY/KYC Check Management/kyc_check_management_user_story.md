# User Story for KYC Check Management

## Story Overview

**As a** Compliance Officer or Bank Administrator  
**I want to** record and manage KYC verification checks  
**So that** the bank can maintain accurate records of customer identity verification activities and ensure regulatory compliance with Know Your Customer requirements

## Acceptance Criteria

1. The system shall allow authorized users to record new KYC verification checks for customers
2. The system shall allow authorized users to manage (update/modify) existing KYC check records
3. KYC check records shall capture verification details including check date, verification method, and outcome
4. KYC check records shall be associated with a specific customer and bank
5. The system shall support recording multiple KYC checks for the same customer over time
6. KYC check management shall include the ability to update check status and verification details

## Technical Context

- **Classes/Services Involved**: KYC Check Service, KYC Check Repository, Customer Service (for customer validation), Bank Service (for bank validation)
- **Input Data**: KYC check creation/update requests containing customer ID, check date, verification method, check outcome, and related metadata
- **Output Data**: KYC check records with full details including check ID, customer reference, verification status, and timestamps
- **Processing Type**: API (REST endpoints for record and manage operations)

## Relevant Endpoints

**IMPORTANT**: Each endpoint is justified by specific words in the capability description.

### Record KYC Check

- **Endpoint**: POST /obp/v5.1.0/banks/{BANK_ID}/customers/{CUSTOMER_ID}/kyc_checks
  - **Justification (from description)**: "Record" - explicitly stated in "Record and manage KYC verification checks"
  - **Purpose**: Record a new KYC verification check for a specific customer
  - **Request**: 
    ```json
    {
      "customer_number": "customer-123",
      "date": "2024-01-15T10:30:00Z",
      "how": "DOCUMENT_VERIFICATION",
      "staff_user_id": "staff-user-001",
      "staff_name": "John Smith",
      "satisfied": true,
      "comments": "All documents verified successfully"
    }
    ```
  - **Response**: 
    ```json
    {
      "bank_id": "bank-id-123",
      "customer_id": "customer-id-456",
      "id": "kyc-check-id-789",
      "customer_number": "customer-123",
      "date": "2024-01-15T10:30:00Z",
      "how": "DOCUMENT_VERIFICATION",
      "staff_user_id": "staff-user-001",
      "staff_name": "John Smith",
      "satisfied": true,
      "comments": "All documents verified successfully"
    }
    ```

### Manage (Update) KYC Check

- **Endpoint**: PUT /obp/v5.1.0/banks/{BANK_ID}/customers/{CUSTOMER_ID}/kyc_checks/{KYC_CHECK_ID}
  - **Justification (from description)**: "manage" - explicitly stated in "Record and manage KYC verification checks"
  - **Purpose**: Update/manage an existing KYC verification check record
  - **Request**: 
    ```json
    {
      "date": "2024-01-15T10:30:00Z",
      "how": "DOCUMENT_VERIFICATION",
      "staff_user_id": "staff-user-001",
      "staff_name": "John Smith",
      "satisfied": true,
      "comments": "Updated: Additional verification completed"
    }
    ```
  - **Response**: Updated KYC check record with all current details

## Business Rules (from capability description)

1. KYC checks must be associated with a valid customer entity (CUSTOMER_ID must exist)
2. KYC checks must be associated with a valid bank entity (BANK_ID must exist)
3. Each KYC check must record the verification method used (how the check was performed)
4. Each KYC check must record the outcome (satisfied or not satisfied)
5. Staff information should be captured for audit trail purposes
6. KYC checks should include timestamps for regulatory compliance tracking

## Data Validations (if applicable)

- **Bank ID Validation**: The specified BANK_ID must exist in the system
- **Customer ID Validation**: The specified CUSTOMER_ID must exist and be associated with the bank
- **Date Validation**: Check date must be a valid date format and not in the future
- **Verification Method Validation**: The "how" field must be a valid verification method (e.g., DOCUMENT_VERIFICATION, VIDEO_CALL, IN_PERSON)
- **Staff User Validation**: Staff user ID should reference a valid system user
- **Required Fields**: Customer ID, Bank ID, date, how, and satisfied status are required for recording a check

## Dependencies

- **Upstream**: 
  - Bank entity must exist before KYC checks can be recorded
  - Customer entity must exist and be linked to the bank
  - User must have appropriate entitlements/permissions to record or manage KYC checks
- **Downstream**: 
  - KYC check status may affect customer's ability to perform certain banking operations
  - KYC check records may be used for compliance reporting
- **External Systems**: 
  - Regulatory reporting systems may consume KYC check data
  - Compliance monitoring systems may track KYC verification status

## Notes for Implementation

- **Audit Trail**: All KYC check records and modifications should be logged for regulatory audit purposes
- **Compliance Requirements**: Ensure KYC check recording meets local regulatory requirements (e.g., AML directives, PSD2)
- **Data Retention**: Consider regulatory requirements for KYC data retention periods
- **Staff Attribution**: Ensure proper attribution of KYC checks to staff members for accountability
- **Verification Methods**: The system should support multiple verification methods as required by different jurisdictions
- **Needs SME Input**: Clarify the complete list of valid verification methods ("how" values) that should be supported
- **Needs SME Input**: Clarify specific regulatory requirements for KYC check data retention and reporting
- **Note**: This capability description does not include "view", "retrieve", "list", "get", "delete", or "remove" operations - only recording and managing (updating) KYC checks are in scope based on the description "Record and manage KYC verification checks"
