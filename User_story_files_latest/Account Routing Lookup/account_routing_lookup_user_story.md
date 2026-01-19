# User Story for Account Routing Lookup

## Story Overview

**As a** Third-Party Application Developer or Payment Service Provider  
**I want to** find accounts by routing information such as IBAN or account number  
**So that** I can identify and retrieve account details for payment processing, account verification, and financial transaction initiation without needing to know the internal account identifiers

## Acceptance Criteria

1. The system shall allow users to find bank accounts using IBAN (International Bank Account Number) as the routing identifier
2. The system shall allow users to find bank accounts using account number as the routing identifier
3. The system shall support multiple routing schemes including IBAN, ACCOUNT_NUMBER, and other custom routing schemes
4. The system shall return the matched account details when a valid routing identifier is provided
5. The system shall return an appropriate error response when no account matches the provided routing information
6. The system shall validate the format of routing identifiers before performing the lookup
7. The system shall require appropriate authentication and authorization to perform account routing lookups
8. The system shall support optional bank ID filtering to narrow down the search scope
9. The system shall process lookup requests in real-time with high availability

## Technical Context

### Classes/Services Involved
- **Connector.scala**: Core connector interface defining `getBankAccountByRouting`, `getBankAccountByIban`, `getBankAccountByRoutings`, and `getAccountRoutingsByScheme` methods
- **LocalMappedConnector.scala**: Local implementation of account routing lookup functionality
- **BankAccountRouting.scala**: Data model for bank account routing information
- **NewStyle.scala**: New style API utility functions including `getBankAccountByRoutings`
- **APIUtil.scala**: API utility functions for request/response handling

### Input Data
- **Request Parameters**:
  - `scheme` (String, required): The routing scheme type (e.g., "IBAN", "ACCOUNT_NUMBER", "AccountNo", "BIC")
  - `address` (String, required): The routing address/value (e.g., the actual IBAN number or account number)
  - `bank_id` (String, optional): Bank identifier to narrow down the search scope

### Output Data
- **Response Payload**:
  - `bank_id`: The bank identifier where the account is held
  - `account_id`: The internal account identifier
  - `account_routings`: List of routing information associated with the account
    - `scheme`: The routing scheme (e.g., "IBAN", "ACCOUNT_NUMBER")
    - `address`: The routing address/value
  - `label`: Account label/name
  - `currency`: Account currency
  - `account_type`: Type of account
  - `balance`: Account balance information (if authorized)

### Processing Type
- **API/Real-time**: Synchronous HTTP request-response pattern with immediate account lookup and response

## Relevant Endpoints

### Endpoint 1: Get Account by Routing Information

- **Endpoint**: `GET /obp/v4.0.0/banks/{BANK_ID}/accounts/account-routing-query`
  - **Justification (from description)**: "Find accounts by routing information such as IBAN or account number" - the word "Find" justifies a GET/retrieval endpoint
  - **Purpose**: Find and retrieve a bank account using routing scheme and address
  - **Request**: 
    ```
    Query Parameters:
    - scheme: String (required) - The routing scheme (e.g., "IBAN", "ACCOUNT_NUMBER")
    - address: String (required) - The routing address value
    ```
  - **Response**:
    ```json
    {
      "bank_id": "gh.29.uk",
      "account_id": "8ca8a7e4-6d02-40e3-a129-0b2bf89de9f0",
      "label": "My Account",
      "currency": "EUR",
      "account_type": "CURRENT",
      "account_routings": [
        {
          "scheme": "IBAN",
          "address": "DE89370400440532013000"
        },
        {
          "scheme": "ACCOUNT_NUMBER",
          "address": "0532013000"
        }
      ]
    }
    ```

### Endpoint 2: Get Account by IBAN

- **Endpoint**: `GET /obp/v4.0.0/accounts/iban/{IBAN}`
  - **Justification (from description)**: "Find accounts by routing information such as IBAN" - explicitly mentions IBAN as a routing identifier for finding accounts
  - **Purpose**: Find and retrieve a bank account using IBAN directly
  - **Request**: 
    ```
    Path Parameters:
    - IBAN: String (required) - The International Bank Account Number
    ```
  - **Response**:
    ```json
    {
      "bank_id": "gh.29.uk",
      "account_id": "8ca8a7e4-6d02-40e3-a129-0b2bf89de9f0",
      "label": "My Account",
      "currency": "EUR",
      "account_type": "CURRENT",
      "account_routings": [
        {
          "scheme": "IBAN",
          "address": "DE89370400440532013000"
        }
      ]
    }
    ```

### Endpoint 3: Get Account by Account Number

- **Endpoint**: `GET /obp/v4.0.0/banks/{BANK_ID}/accounts/account-number/{ACCOUNT_NUMBER}`
  - **Justification (from description)**: "Find accounts by routing information such as ... account number" - explicitly mentions account number as a routing identifier for finding accounts
  - **Purpose**: Find and retrieve a bank account using account number within a specific bank
  - **Request**: 
    ```
    Path Parameters:
    - BANK_ID: String (required) - The bank identifier
    - ACCOUNT_NUMBER: String (required) - The account number
    ```
  - **Response**:
    ```json
    {
      "bank_id": "gh.29.uk",
      "account_id": "8ca8a7e4-6d02-40e3-a129-0b2bf89de9f0",
      "label": "My Account",
      "currency": "EUR",
      "account_type": "CURRENT",
      "account_routings": [
        {
          "scheme": "ACCOUNT_NUMBER",
          "address": "0532013000"
        }
      ]
    }
    ```

## Business Rules

1. **Routing Scheme Validation**: The system must validate that the provided routing scheme is a supported type (IBAN, ACCOUNT_NUMBER, BIC, or other configured schemes)
2. **IBAN Format Validation**: When IBAN is used as the routing scheme, the system must validate the IBAN format according to ISO 13616 standards
3. **Account Number Scope**: When using account number for lookup, a bank_id should be provided to narrow the search scope, as account numbers may not be globally unique
4. **Authorization Required**: Users must have appropriate entitlements or view permissions to perform account routing lookups
5. **Single Account Return**: The lookup should return a single account match; if multiple accounts match (which should be rare for unique identifiers like IBAN), the system should handle this appropriately
6. **Case Sensitivity**: Routing addresses should be handled in a case-insensitive manner where applicable (e.g., IBAN)

## Data Validations

- **Scheme Validation**: The routing scheme must be a non-empty string and must be a recognized scheme type
- **Address Validation**: The routing address must be a non-empty string
- **IBAN Validation**: When scheme is "IBAN", validate:
  - Length is between 15-34 characters
  - Starts with a valid country code (2 letters)
  - Contains valid check digits
  - Follows the country-specific IBAN format
- **Account Number Validation**: When scheme is "ACCOUNT_NUMBER", validate:
  - Non-empty alphanumeric string
  - Bank ID is provided for scoped lookup
- **Error Handling**:
  - Return `OBP-30018: Bank Account not found` when no account matches the routing information
  - Return `OBP-10001: Incorrect json format` for malformed requests
  - Return `OBP-20001: User not logged in` for unauthenticated requests
  - Return `OBP-20006: User is missing one or more roles` for unauthorized requests

## Dependencies

### Upstream
- **Authentication Service**: User must be authenticated via OAuth 1.0a, OAuth 2.0, OpenID Connect, or Direct Login before performing account routing lookups
- **Authorization Service**: User must have appropriate entitlements (e.g., `CanGetAccountByRouting`) or view permissions
- **Bank Configuration**: The bank must be configured and active on the platform

### Downstream
- **Payment Initiation**: Account routing lookup is often used as a prerequisite for payment initiation to verify recipient account details
- **Counterparty Creation**: The found account information can be used to create counterparty records
- **Account Verification**: Third-party applications use this to verify account ownership and details

### External Systems
- **Core Banking System**: The connector may need to query the underlying core banking system to retrieve account information based on routing details
- **IBAN Validation Service**: External IBAN validation services may be used to validate IBAN format and check digits

## Notes for Implementation

### Special Considerations
- **Performance**: Account routing lookup is a high-volume, real-time operation. Implement appropriate caching and indexing strategies for routing information
- **Multiple Routing Schemes**: An account may have multiple routing identifiers (e.g., both IBAN and account number). The lookup should work with any of the associated routing schemes
- **Scheme Normalization**: Handle variations in scheme naming (e.g., "AccountNo" vs "ACCOUNT_NUMBER") by normalizing to a standard format internally
- **Bank Scope**: When bank_id is not provided for IBAN lookups, the system should search across all banks; for account number lookups, bank_id should be required or strongly recommended

### Known Complexity
- **Cross-Bank Lookup**: IBAN lookups may need to search across multiple banks if the bank_id is not provided, which could impact performance
- **Routing Scheme Mapping**: Different banks may use different routing scheme names for the same type of identifier; implement mapping logic to handle these variations

### Missing or Unclear Requirements (Needs SME Input)
- What is the expected behavior when multiple accounts match the same routing information?
- Should the system support partial matching or wildcard searches for routing addresses?
- What are the rate limiting requirements for account routing lookup endpoints?
- Should the response include balance information by default, or should it be controlled by view permissions?
- Are there any specific audit logging requirements for account routing lookups?

## Quality Checklist

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Third-Party Application Developer, Payment Service Provider)
- [x] Business value is stated (identify accounts for payment processing and verification)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged for SME review
- [x] Only relevant endpoints are included (GET/find operations only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words in the capability description justify the endpoint ("Find")
- [x] No CRUD operations are inferred beyond what the description explicitly states (only retrieval/find operations)
- [x] Words like "manage" have been interpreted narrowly - this capability only uses "Find" which maps to retrieval operations
