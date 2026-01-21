# User Story for Berlin Group Periodic Payment

## Story Overview

**As a** Third-Party Payment Service Provider (PISP)
**I want to** initiate periodic payments per Berlin Group specification
**So that** I can set up recurring/standing order payments on behalf of account holders in compliance with PSD2 regulations

## Acceptance Criteria

1. The system shall accept periodic payment initiation requests via POST endpoint with required payment details including debtor account, creditor account, instructed amount, and scheduling parameters
2. The system shall validate that the payment product is a supported type (sepa-credit-transfers, instant-sepa-credit-transfers, target-2-payments, cross-border-credit-transfers)
3. The system shall validate that the instructed amount is a positive number with valid ISO currency code
4. The system shall require mandatory scheduling parameters: startDate and frequency
5. The system shall support optional scheduling parameters: endDate, executionRule, and dayOfExecution
6. The system shall create a standing order initiation resource addressable under {paymentId}
7. The system shall return transaction status (RCVD - Received) upon successful initiation
8. The system shall provide HATEOAS links for subsequent operations (self, status, scaStatus, scaRedirect)
9. The system shall support Strong Customer Authentication (SCA) flow when required by the ASPSP
10. The system shall validate IBAN format for debtor and creditor accounts

## Technical Context

- **Classes/Services Involved**: 
  - `APIMethods_PaymentInitiationServicePISApi` - Main API endpoint handler for payment initiation
  - `PeriodicSepaCreditTransfersBerlinGroupV13` - Request body model for periodic payments
  - `PeriodicPaymentInitiationJson` - JSON model for periodic payment data
  - `JSONFactory_BERLIN_GROUP_1_3` - JSON response factory
  - `NewStyle.function.createTransactionRequestBGV1` - Transaction request creation service
  
- **Input Data**: 
  - Request Body (JSON):
    - `debtorAccount` (required): Account reference with IBAN
    - `instructedAmount` (required): Amount with currency and value
    - `creditorAccount` (required): Account reference with IBAN
    - `creditorName` (required): Name of the creditor (max 70 characters)
    - `startDate` (required): First applicable day of execution (ISO date format)
    - `frequency` (required): Frequency code (Daily, Weekly, EveryTwoWeeks, Monthly, EveryTwoMonths, Quarterly, SemiAnnual, Annual, MonthlyVariable)
    - `endDate` (optional): Last applicable day of execution
    - `executionRule` (optional): Execution rule (preceding/following)
    - `dayOfExecution` (optional): Day of month for execution (01-31)
    - `remittanceInformationUnstructured` (optional): Payment reference text
    - `endToEndIdentification` (optional): End-to-end identification
    - `creditorAgent` (optional): BIC of creditor's bank
    - `creditorAddress` (optional): Address of creditor

- **Output Data**: 
  - Response Body (JSON):
    - `transactionStatus`: Status of the payment initiation (RCVD, ACTC, etc.)
    - `paymentId`: Unique identifier for the created payment resource
    - `_links`: HATEOAS links containing:
      - `scaRedirect`: URL for SCA redirect flow
      - `self`: Link to the payment resource
      - `status`: Link to check payment status
      - `scaStatus`: Link to check SCA status

- **Processing Type**: Real-time API (synchronous request-response)

## Relevant Endpoints

**IMPORTANT**: For every endpoint included, justification from the description is provided.

- **Endpoint**: POST /periodic-payments/{payment-product}
  - **Justification (from description)**: "Initiate periodic payments per Berlin Group specification"
  - **Purpose**: Create a standing order initiation resource for recurrent/periodic payments
  - **Request**: 
    ```json
    {
      "instructedAmount": {
        "currency": "EUR",
        "amount": "123"
      },
      "debtorAccount": {
        "iban": "DE40100100103307118608"
      },
      "creditorName": "Merchant123",
      "creditorAccount": {
        "iban": "DE23100120020123456789"
      },
      "remittanceInformationUnstructured": "Ref Number Abonnement",
      "startDate": "2018-03-01",
      "executionRule": "preceding",
      "frequency": "Monthly",
      "dayOfExecution": "01"
    }
    ```
  - **Response**: 
    ```json
    {
      "transactionStatus": "RCVD",
      "paymentId": "1234-wertiq-983",
      "_links": {
        "scaRedirect": {"href": "/otp?flow=payment&paymentService=periodic-payments&paymentProduct=sepa_credit_transfers&paymentId=b0472c21-6cea-4ee0-b036-3e253adb3b0b"},
        "self": {"href": "/v1.3/periodic-payments/sepa-credit-transfers/1234-wertiq-983"},
        "status": {"href": "/v1.3/periodic-payments/1234-wertiq-983/status"},
        "scaStatus": {"href": "/v1.3/periodic-payments/1234-wertiq-983/authorisations/123auth456"}
      }
    }
    ```

## Business Rules (from capability description)

1. The payment service type must be "periodic-payments" for recurring payment initiation
2. The payment product must be one of the supported SEPA credit transfer types
3. The instructed amount must be a positive number greater than zero
4. The currency must be a valid ISO currency code
5. The startDate defines the first applicable day of execution for the standing order
6. If endDate is not provided, the standing order is considered infinite
7. The frequency parameter determines the recurrence pattern of the payment
8. The executionRule (preceding/following) determines how to handle non-business days
9. The dayOfExecution specifies which day of the month the payment should execute
10. Strong Customer Authentication (SCA) may be required depending on ASPSP configuration
11. The payment initiation creates a transaction request resource that can be tracked via paymentId

## Data Validations (if applicable)

- IBAN validation for debtor and creditor accounts
- ISO currency code validation for instructed amount currency
- Positive number validation for instructed amount value
- Date format validation for startDate and endDate (ISO 8601)
- Frequency code validation against allowed values
- Day of execution validation (01-31)
- Execution rule validation (preceding/following)
- Payment product validation against supported types
- Creditor name length validation (max 70 characters)

## Dependencies

- **Upstream**: 
  - User authentication via PSD2 PISP authorization
  - Valid consent for payment initiation
  - Debtor account must exist and be accessible
  
- **Downstream**: 
  - Transaction request is created and stored in the system
  - SCA challenge may be initiated if required
  - Payment authorisation sub-resources may be created
  - Scheduled payment execution based on frequency and dates
  
- **External Systems**: 
  - Backend banking connector (REST, Akka, Stored Procedure, or Kafka)
  - IBAN validation service
  - Currency conversion service (if cross-currency)

## Notes for Implementation

- The periodic payment endpoint shares implementation logic with single payments via `initiatePaymentImplementation` function
- The request body is extracted as `PeriodicSepaCreditTransfersBerlinGroupV13` for periodic payments
- The payment service type is validated to be "periodic_payments" 
- Transaction request type is derived from the payment product in the URL path
- The response includes HATEOAS links for navigating the payment lifecycle
- Multilevel SCA may be required for corporate contexts, requiring explicit authorisation start
- The implementation follows Berlin Group PSD2 API v1.3 specification
- Frequency values supported: Daily, Weekly, EveryTwoWeeks, Monthly, EveryTwoMonths, Quarterly, SemiAnnual, Annual, MonthlyVariable
- For MonthlyVariable frequency, the monthsOfExecution array specifies which months the payment executes
