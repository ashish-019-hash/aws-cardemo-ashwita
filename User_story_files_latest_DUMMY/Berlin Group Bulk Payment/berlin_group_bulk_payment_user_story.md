# User Story for Berlin Group Bulk Payment

## Story Overview

**As a** Payment Service Provider (PSP) or Third-Party Provider (TPP)
**I want to** initiate bulk payments per Berlin Group specification
**So that** I can submit multiple payment instructions in a single batch request, reducing operational overhead and enabling efficient processing of high-volume payment scenarios for my customers

## Acceptance Criteria

1. The system shall accept bulk payment initiation requests containing multiple individual payment instructions in a single API call
2. The system shall validate the bulk payment request structure according to Berlin Group PSD2 specification v1.3
3. The system shall support batch booking preference where the PSU can choose between consolidated or individual booking entries
4. The system shall validate the debtor account information (IBAN) for the bulk payment
5. The system shall validate each individual payment within the bulk request including creditor account, amount, and currency
6. The system shall support optional requested execution date and time for scheduled bulk payments
7. The system shall return a unique payment ID for tracking the bulk payment request
8. The system shall return appropriate transaction status (e.g., RCVD - Received) upon successful initiation
9. The system shall provide hypermedia links for subsequent operations (self, status, authorisation)
10. The system shall support Strong Customer Authentication (SCA) redirect flow when required
11. The system shall support multiple payment products: sepa-credit-transfers, instant-sepa-credit-transfers, target-2-payments, cross-border-credit-transfers

## Technical Context

**Classes/Services Involved**:
- `APIMethods_PaymentInitiationServicePISApi` - Main API handler for payment initiation endpoints
- `BulkPaymentInitiationJson` - Request model for bulk payment initiation
- `PaymentInitiationBulkElementJson` - Model for individual payment elements within bulk
- `AccountReference` - Model for debtor/creditor account information
- `JSONFactory_BERLIN_GROUP_1_3` - Factory for creating response JSON structures
- `TransactionRequestTypes` - Enum for supported payment products

**Input Data**:
- Request Body (JSON):
  - `batchBookingPreferred` (optional, Boolean): PSU preference for single or individual booking
  - `debtorAccount` (required, AccountReference): Debtor account with IBAN
  - `requestedExecutionDate` (optional, LocalDate): Requested date for payment execution
  - `requestedExecutionTime` (optional, OffsetDateTime): Requested time for payment execution
  - `payments` (required, Array): List of individual payment instructions containing:
    - `instructedAmount`: Currency and amount
    - `creditorName`: Name of the creditor
    - `creditorAccount`: Creditor account with IBAN
    - `remittanceInformationUnstructured`: Payment reference/description

**Output Data**:
- Response Body (JSON):
  - `transactionStatus`: Status of the payment initiation (e.g., "RCVD")
  - `paymentId`: Unique identifier for the bulk payment
  - `_links`: Hypermedia links including:
    - `scaRedirect`: URL for SCA redirect flow
    - `self`: Link to the payment resource
    - `status`: Link to check payment status
    - `scaStatus`: Link to check SCA authorisation status

**Processing Type**: Real-time API / On-demand

## Relevant Endpoints

**IMPORTANT**: The capability description states "Initiate bulk payments per Berlin Group specification". The word "Initiate" justifies CREATE operations only.

### Endpoint 1: Initiate Bulk Payment

- **Endpoint**: `POST /bulk-payments/{payment-product}`
  - **Justification (from description)**: "Initiate bulk payments" - the word "Initiate" explicitly justifies this POST endpoint for creating bulk payment requests
  - **Purpose**: Creates a new bulk payment initiation request containing multiple individual payments
  - **Request**:
    ```json
    {
      "batchBookingPreferred": true,
      "debtorAccount": {
        "iban": "DE40100100103307118608"
      },
      "paymentInformationId": "my-bulk-identification-1234",
      "requestedExecutionDate": "2018-08-01",
      "payments": [
        {
          "instructedAmount": {
            "currency": "EUR",
            "amount": "123.50"
          },
          "creditorName": "Merchant123",
          "creditorAccount": {
            "iban": "DE02100100109307118603"
          },
          "remittanceInformationUnstructured": "Ref Number Merchant 1"
        },
        {
          "instructedAmount": {
            "currency": "EUR",
            "amount": "34.10"
          },
          "creditorName": "Merchant456",
          "creditorAccount": {
            "iban": "FR7612345987650123456789014"
          },
          "remittanceInformationUnstructured": "Ref Number Merchant 2"
        }
      ]
    }
    ```
  - **Response** (HTTP 201 Created):
    ```json
    {
      "transactionStatus": "RCVD",
      "paymentId": "1234-wertiq-983",
      "_links": {
        "scaRedirect": {"href": "/otp?flow=payment&paymentService=bulk-payments&paymentProduct=sepa_credit_transfers&paymentId=..."},
        "self": {"href": "/v1.3/bulk-payments/sepa-credit-transfers/1234-wertiq-983"},
        "status": {"href": "/v1.3/bulk-payments/1234-wertiq-983/status"},
        "scaStatus": {"href": "/v1.3/bulk-payments/1234-wertiq-983/authorisations/123auth456"}
      }
    }
    ```

**Supported Payment Products** (path parameter):
- `sepa-credit-transfers`
- `instant-sepa-credit-transfers`
- `target-2-payments`
- `cross-border-credit-transfers`
- `pain.001-sepa-credit-transfers` (XML format)
- `pain.001-instant-sepa-credit-transfers` (XML format)
- `pain.001-target-2-payments` (XML format)
- `pain.001-cross-border-credit-transfers` (XML format)

## Business Rules (from capability description)

1. **Batch Booking Preference**: When `batchBookingPreferred` is true, the PSU prefers only one booking entry; when false, individual booking of all contained transactions is preferred. The ASPSP follows this preference according to contracts agreed with the PSU.

2. **Payment Product Validation**: The payment product in the URL path must be a valid Berlin Group payment product type (sepa-credit-transfers, instant-sepa-credit-transfers, target-2-payments, cross-border-credit-transfers, or their pain.001 XML equivalents).

3. **Berlin Group Compliance**: All bulk payment requests must conform to the NextGenPSD2 XS2A Framework Version 1.3 specification.

4. **SCA Requirement**: Strong Customer Authentication may be required for bulk payment initiation. The response will include appropriate links for SCA redirect or embedded flows.

5. **Multilevel SCA Support**: For payments requiring multiple authorisations (corporate context), explicit start of authorisation via the `startAuthorisation` link is required.

6. **Currency Validation**: All instructed amounts must use valid ISO 4217 currency codes.

7. **Amount Validation**: All payment amounts must be positive numbers.

8. **IBAN Validation**: Both debtor and creditor account IBANs must be valid and properly formatted.

## Data Validations

- **Debtor Account IBAN**: Must be a valid IBAN format
- **Creditor Account IBAN**: Must be a valid IBAN format for each payment in the bulk
- **Instructed Amount**: Must be a positive decimal number
- **Currency Code**: Must be a valid ISO 4217 currency code (e.g., EUR, USD, GBP)
- **Payment Product**: Must be one of the supported Berlin Group payment products
- **Payments Array**: Must contain at least one payment instruction
- **Requested Execution Date**: If provided, must be a valid future date in ISO 8601 format

**Error Conditions Handled**:
- `InvalidJsonFormat`: Request body does not match expected bulk payment structure
- `InvalidNumber`: Amount is not a valid number
- `NotPositiveAmount`: Amount is zero or negative
- `InvalidISOCurrencyCode`: Currency code is not valid
- `InvalidTransactionRequestType`: Payment product is not supported
- `UserNotLoggedIn`: Authentication required
- `invalidIban`: IBAN validation failed

## Dependencies

**Upstream**:
- User must be authenticated via OAuth 2.0 / OpenID Connect or other supported authentication method
- User must have PSD2 PISP (Payment Initiation Service Provider) role/entitlement
- Debtor account must exist and be accessible
- Valid consent may be required depending on ASPSP implementation

**Downstream**:
- Transaction request is created and stored in the system
- SCA challenge may be generated for authorisation
- Payment status can be queried via status endpoint
- Authorisation sub-resources are created for SCA flow

**External Systems**:
- Backend banking connectors (REST, Akka, Stored Procedure, Kafka, RabbitMQ)
- IBAN validation service
- Currency conversion service (for cross-currency payments)
- SCA/OTP service for Strong Customer Authentication

## Notes for Implementation

1. **Current Implementation Status**: The source code indicates bulk payments support is planned but the implementation currently throws a runtime exception for bulk-payments service type. The `initiatePaymentImplementation` function needs to be extended to handle `PaymentServiceTypes.bulk_payments`.

2. **JSON vs XML Support**: The Berlin Group specification supports both JSON and pain.001 XML formats for bulk payments. The current implementation focuses on JSON format.

3. **Batch Processing**: Consider implementing asynchronous processing for large bulk payment batches to avoid timeout issues.

4. **Idempotency**: Consider implementing idempotency keys to prevent duplicate bulk payment submissions.

5. **Rate Limiting**: Bulk payment endpoints may need specific rate limiting rules due to their potential for high resource consumption.

6. **Audit Trail**: All bulk payment initiations should be logged for regulatory compliance and audit purposes.

7. **Needs SME Input**:
   - Maximum number of individual payments allowed in a single bulk request
   - Specific timeout thresholds for bulk payment processing
   - Retry policies for failed individual payments within a bulk
   - Specific ASPSP requirements for batch booking implementation
