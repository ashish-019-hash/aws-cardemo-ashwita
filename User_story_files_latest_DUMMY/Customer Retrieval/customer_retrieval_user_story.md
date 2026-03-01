# User Story for Customer Retrieval

## Capability Input

- **Name**: Customer Retrieval
- **Description**: Retrieve customer information by various identifiers
- **Frequency**: Real-time
- **Volume**: High

---

## Operation Verb Analysis

Based on the capability description, the following operation verbs are identified:

| Verb Found | Operation Type | Justification |
|------------|---------------|---------------|
| "Retrieve" | READ/RETRIEVAL | Explicitly stated: "Retrieve customer information by various identifiers" |

**Operations NOT included** (verbs not present in description):
- CREATE operations: No "create", "register", "onboard", "set up", "add", "establish", "initialize", or "provision" mentioned
- UPDATE operations: No "manage", "configure", "maintain", "update", "modify", "change", "edit", "adjust", "set", or "customize" mentioned
- DELETE operations: No "delete", "remove", "deactivate", "close", "terminate", "disable", "archive", "retire", or "cancel" mentioned
- LIST operations: No "list", "browse", or "search" mentioned - this capability is specifically for retrieval by identifiers

---

## Story Overview

**As a** third-party developer, fintech application, bank staff member, or authorized system integrating with the Open Bank Project platform
**I want to** retrieve customer information by various identifiers
**So that** I can access customer details for account servicing, verify customer identity for transactions, display customer information in applications, support customer relationship management activities, and enable personalized banking services based on customer data

---

## Acceptance Criteria

1. The system shall allow retrieval of customer information using a unique customer identifier (Customer ID)
2. The system shall support retrieval of customer information using various identifier types (e.g., customer ID, linked account ID, bank-specific identifiers)
3. The system shall return comprehensive customer information including personal details, contact information, and relevant metadata
4. The system shall enforce access control to ensure only authorized users can retrieve customer information
5. The system shall return responses in real-time with appropriate performance characteristics for high-volume usage patterns
6. The system shall return appropriate error responses (e.g., HTTP 404) when the specified customer is not found
7. The system shall return appropriate error responses (e.g., HTTP 403) when the user lacks permission to view customer information
8. The system shall support retrieval of customers linked to specific accounts when the user has appropriate permissions
9. The system shall protect sensitive customer data based on user permissions and regulatory requirements
10. The system shall return customer information in a standardized format consistent with Open Banking specifications

---

## Technical Context

- **Classes/Services Involved** (from Scala source code):
  - APIMethods400 / APIMethods310 / APIMethods300 - REST endpoint definitions for customer retrieval
  - JSONFactory400 / JSONFactory310 - JSON response factory with customer detail creation methods
  - NewStyle.function (code.api.util.NewStyle) - Service layer with customer retrieval methods
  - Connector (code.bankconnectors.Connector) - Backend connector abstraction for customer data access
  - CustomerJson / CustomerJsonV310 - Case classes defining customer response structure
  - Customer / CustomerCommons - Common customer data model

- **Input Data**: 
  - Bank identifier (BANK_ID) as path parameter
  - Customer identifier (CUSTOMER_ID) as path parameter
  - Account identifier (ACCOUNT_ID) as path parameter (for account-linked customer retrieval)
  - Authentication token (OAuth/DirectLogin) to identify the requesting user

- **Output Data** (based on customer response case classes):
  - customer_id (String) - Unique customer identifier
  - bank_id (String) - Bank identifier the customer belongs to
  - customer_number (String) - Bank-assigned customer number
  - legal_name (String) - Customer's legal name
  - mobile_phone_number (String) - Customer's mobile phone
  - email (String) - Customer's email address
  - face_image (CustomerFaceImage) - Customer's face image information
  - date_of_birth (Date) - Customer's date of birth
  - relationship_status (String) - Customer's relationship status
  - dependants (Integer) - Number of dependants
  - dob_of_dependants (List[Date]) - Dates of birth of dependants
  - credit_rating (CreditRating) - Customer's credit rating information
  - credit_limit (AmountOfMoney) - Customer's credit limit
  - highest_education_attained (String) - Highest education level
  - employment_status (String) - Current employment status
  - kyc_status (Boolean) - KYC verification status
  - last_ok_date (Date) - Last KYC verification date
  - title (String) - Customer's title
  - branch_id (String) - Associated branch identifier
  - name_suffix (String) - Name suffix
  - customer_attributes (List[CustomerAttribute]) - Custom attributes

- **Processing Type**: API / Real-time / Synchronous request-response

---

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description. Only READ/RETRIEVAL operations are included as the description only contains the verb "Retrieve" with "various identifiers".

### Endpoint 1: Get Customer by Customer ID

- **Endpoint**: GET /obp/v4.0.0/banks/BANK_ID/customers/CUSTOMER_ID
  - **Justification (from description)**: "Retrieve customer information by various identifiers" - Customer ID is a primary identifier
  - **Purpose**: Retrieve detailed customer information using the unique customer identifier
  - **Scala Implementation**: APIMethods400.getCustomerByCustomerId -> NewStyle.function.getCustomerByCustomerId() -> JSONFactory400.createCustomerJson()
  - **Request**: 
    ```
    GET /obp/v4.0.0/banks/BANK_ID/customers/CUSTOMER_ID
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      CUSTOMER_ID: The unique identifier of the customer (required)
    ```
  - **Response**: 
    ```json
    {
      "customer_id": "customer-id-001",
      "bank_id": "bank-id-001",
      "customer_number": "CUST123456",
      "legal_name": "John Doe",
      "mobile_phone_number": "+1-555-123-4567",
      "email": "john.doe@example.com",
      "face_image": {
        "url": "https://example.com/faces/customer-001.jpg",
        "date": "2024-01-15"
      },
      "date_of_birth": "1985-06-15",
      "relationship_status": "Married",
      "dependants": 2,
      "dob_of_dependants": ["2010-03-20", "2013-07-10"],
      "credit_rating": {
        "rating": "A",
        "source": "CreditAgency"
      },
      "credit_limit": {
        "currency": "USD",
        "amount": "50000.00"
      },
      "highest_education_attained": "Bachelor's Degree",
      "employment_status": "Employed",
      "kyc_status": true,
      "last_ok_date": "2024-01-01",
      "title": "Mr",
      "branch_id": "branch-001",
      "name_suffix": "Jr"
    }
    ```

### Endpoint 2: Get Customer for Account

- **Endpoint**: GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/customer
  - **Justification (from description)**: "Retrieve customer information by various identifiers" - Account ID serves as an identifier to retrieve the linked customer
  - **Purpose**: Retrieve customer information linked to a specific account
  - **Scala Implementation**: APIMethods400.getCustomerForAccount -> NewStyle.function.getCustomerByAccountId() -> JSONFactory400.createCustomerJson()
  - **Request**: 
    ```
    GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/customer
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
      "customer_id": "customer-id-001",
      "bank_id": "bank-id-001",
      "customer_number": "CUST123456",
      "legal_name": "John Doe",
      "mobile_phone_number": "+1-555-123-4567",
      "email": "john.doe@example.com",
      "date_of_birth": "1985-06-15",
      "relationship_status": "Married",
      "dependants": 2,
      "credit_rating": {
        "rating": "A",
        "source": "CreditAgency"
      },
      "credit_limit": {
        "currency": "USD",
        "amount": "50000.00"
      },
      "highest_education_attained": "Bachelor's Degree",
      "employment_status": "Employed",
      "kyc_status": true,
      "last_ok_date": "2024-01-01"
    }
    ```

### Endpoint 3: Get My Customer at Bank

- **Endpoint**: GET /obp/v4.0.0/banks/BANK_ID/my/customer
  - **Justification (from description)**: "Retrieve customer information by various identifiers" - User's own identity serves as the identifier
  - **Purpose**: Retrieve the customer record for the currently authenticated user at a specific bank
  - **Scala Implementation**: APIMethods400.getMyCustomerAtBank -> NewStyle.function.getCustomerByUserId() -> JSONFactory400.createCustomerJson()
  - **Request**: 
    ```
    GET /obp/v4.0.0/banks/BANK_ID/my/customer
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
    ```
  - **Response**: 
    ```json
    {
      "customer_id": "customer-id-001",
      "bank_id": "bank-id-001",
      "customer_number": "CUST123456",
      "legal_name": "John Doe",
      "mobile_phone_number": "+1-555-123-4567",
      "email": "john.doe@example.com",
      "date_of_birth": "1985-06-15",
      "relationship_status": "Married",
      "dependants": 2,
      "credit_rating": {
        "rating": "A",
        "source": "CreditAgency"
      },
      "credit_limit": {
        "currency": "USD",
        "amount": "50000.00"
      },
      "highest_education_attained": "Bachelor's Degree",
      "employment_status": "Employed",
      "kyc_status": true,
      "last_ok_date": "2024-01-01"
    }
    ```

### Endpoint 4: Get Customer by Customer Number

- **Endpoint**: GET /obp/v4.0.0/banks/BANK_ID/customers/customer-number/CUSTOMER_NUMBER
  - **Justification (from description)**: "Retrieve customer information by various identifiers" - Customer Number is another identifier type
  - **Purpose**: Retrieve customer information using the bank-assigned customer number
  - **Scala Implementation**: APIMethods400.getCustomerByCustomerNumber -> NewStyle.function.getCustomerByCustomerNumber() -> JSONFactory400.createCustomerJson()
  - **Request**: 
    ```
    GET /obp/v4.0.0/banks/BANK_ID/customers/customer-number/CUSTOMER_NUMBER
    Headers:
      Authorization: Bearer {token} (or DirectLogin token)
      Content-Type: application/json
    Path Parameters:
      BANK_ID: The unique identifier of the bank (required)
      CUSTOMER_NUMBER: The bank-assigned customer number (required)
    ```
  - **Response**: 
    ```json
    {
      "customer_id": "customer-id-001",
      "bank_id": "bank-id-001",
      "customer_number": "CUST123456",
      "legal_name": "John Doe",
      "mobile_phone_number": "+1-555-123-4567",
      "email": "john.doe@example.com",
      "date_of_birth": "1985-06-15",
      "relationship_status": "Married",
      "dependants": 2,
      "credit_rating": {
        "rating": "A",
        "source": "CreditAgency"
      },
      "credit_limit": {
        "currency": "USD",
        "amount": "50000.00"
      },
      "highest_education_attained": "Bachelor's Degree",
      "employment_status": "Employed",
      "kyc_status": true,
      "last_ok_date": "2024-01-01"
    }
    ```

### Endpoints NOT Included (with justification)

| Endpoint | Operation Type | Reason for Exclusion |
|----------|---------------|---------------------|
| POST /banks/{bank_id}/customers | CREATE | No "create", "register", "onboard", "add", or similar verb in description |
| PUT /banks/{bank_id}/customers/{customer_id} | UPDATE | No "manage", "update", "configure", "modify", or similar verb in description |
| PATCH /banks/{bank_id}/customers/{customer_id} | UPDATE | No "manage", "update", "configure", "modify", or similar verb in description |
| DELETE /banks/{bank_id}/customers/{customer_id} | DELETE | No "delete", "remove", "deactivate", "close", or similar verb in description |
| GET /banks/{bank_id}/customers | LIST | No "list", "browse", or "search" verb in description - this capability is for retrieval "by various identifiers" |
| GET /banks/{bank_id}/customers?phone_number=... | SEARCH | No "search", "find", or "query" verb in description |

---

## Business Rules (from capability description)

1. **Identifier-Based Retrieval**: This capability retrieves customer information using specific identifiers, not bulk listing (from: "by various identifiers")
2. **Multiple Identifier Support**: The system must support retrieval using different types of identifiers (Customer ID, Customer Number, Account ID, User ID) (from: "various identifiers")
3. **Real-time Access**: Customer information must be retrieved in real-time with low latency (from: Frequency = Real-time)
4. **High Volume Support**: The system must be designed to handle high volume of customer retrieval requests efficiently (from: Volume = High)
5. **Single Customer Focus**: Each retrieval operation returns information for a single customer identified by the provided identifier
6. **Bank Scope**: Customer retrieval is scoped to a specific bank context

---

## Data Validations (if applicable)

- Bank identifier (BANK_ID) must be valid and exist in the system
- Customer identifier (CUSTOMER_ID) must be valid and exist within the specified bank
- Customer number (CUSTOMER_NUMBER) must be valid and exist within the specified bank
- Account identifier (ACCOUNT_ID) must be valid and have a linked customer
- User must be authenticated with a valid OAuth token or DirectLogin credentials
- User must have appropriate entitlements/permissions to retrieve customer information
- Error response (HTTP 404 Not Found / BankNotFound) must be returned when BANK_ID does not exist
- Error response (HTTP 404 Not Found / CustomerNotFound) must be returned when customer is not found by the provided identifier
- Error response (HTTP 401 Unauthorized) for missing or invalid authentication
- Error response (HTTP 403 Forbidden / UserHasMissingRoles) when user lacks permission to access customer information
- Sensitive customer data (e.g., date of birth, credit rating) may be masked or omitted based on user permissions
- All date fields must be returned in ISO 8601 format
- Phone numbers should be returned in international format

---

## Dependencies

- **Upstream**: 
  - Bank must exist in the system (Bank Creation capability)
  - Customer must be created and linked to the bank (Customer Creation capability)
  - User must be authenticated (Authentication & Security capabilities)
  - User must have appropriate entitlements to access customer data (Entitlement & Role Management capabilities)
  - For account-based retrieval, customer must be linked to the account (Customer-Account Linking capability)

- **Downstream**: 
  - Retrieved customer information is used to display customer details in third-party applications
  - Customer data is used for identity verification in transaction processing
  - Customer information supports KYC and compliance workflows
  - Customer details enable personalized banking services and recommendations
  - Customer data is used for customer relationship management activities

- **External Systems**: 
  - Backend banking connectors may retrieve customer data from core banking systems
  - Customer information may be synchronized with external CRM systems
  - KYC status may be verified against external identity verification services
  - Credit rating information may be fetched from external credit bureaus

---

## Notes for Implementation

- **Privacy Consideration**: Implement proper data masking and access controls for sensitive customer information (PII)
- **Performance Consideration**: Implement caching strategies for frequently accessed customer data while ensuring data freshness
- **Identifier Validation**: Validate identifier format before querying backend systems to fail fast on invalid inputs
- **Audit Logging**: Log all customer data access for compliance and audit purposes
- **Data Consistency**: Ensure customer data is consistent across different retrieval endpoints
- **Error Handling**: Provide clear error messages that don't expose sensitive information

### Needs SME Input
- Clarify the complete list of "various identifiers" that should be supported for customer retrieval
- Determine which customer fields are mandatory vs. optional in the response
- Confirm data masking requirements for sensitive fields based on user roles
- Clarify if there are different detail levels for customer retrieval (minimal vs. full)
- Determine if customer retrieval should include linked accounts information
- Confirm regulatory requirements for customer data access logging
- Clarify handling of customers with multiple relationships at the same bank

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical implementation details)
- [x] User role is clearly identified (third-party developer, fintech application, bank staff, authorized system)
- [x] Business value is stated (access customer details, verify identity, display information, support CRM, enable personalized services)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (Needs SME Input section)
- [x] Only relevant endpoints are included (GET operations only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, I can point to a specific word or phrase in the capability description that justifies this endpoint ("Retrieve customer information by various identifiers")
- [x] No endpoint type (create, update, view, list, delete) has been added unless its verb (or a clear synonym) appears in the description
- [x] Words like "manage" have been interpreted narrowly - not applicable as "manage" is not in the description
