# User Story for Berlin Group Payment Initiation

## Story Overview
**As a** Third-Party Payment Service Provider (TPP) or Fintech Application
**I want to** initiate payments per Berlin Group specification
**So that** I can execute PSD2-compliant payment transactions on behalf of account holders through a standardized API interface that ensures regulatory compliance and interoperability across European financial institutions

## Acceptance Criteria
1. The system shall allow authorized TPPs to initiate single payment transactions per Berlin Group NextGenPSD2 specification
2. The system shall accept payment initiation requests with required fields including debtor account, creditor account, instructed amount, and currency
3. The system shall validate payment initiation requests against Berlin Group schema requirements before processing
4. The system shall return a payment initiation response with a unique payment ID and transaction status
5. The system shall support multiple payment products as defined by Berlin Group (e.g., sepa-credit-transfers, instant-sepa-credit-transfers, target-2-payments, cross-border-credit-transfers)
6. The system shall enforce Strong Customer Authentication (SCA) requirements for payment initiation as per PSD2 regulations
7. The system shall return appropriate error responses with Berlin Group compliant error codes for invalid or rejected payment requests

## Technical Context
- **Classes/Services Involved**: Payment Initiation Service (PIS), Berlin Group API handler, Payment validation service, SCA orchestration service
- **Input Data**: Payment initiation request body containing debtor account (IBAN), creditor account (IBAN), creditor name, instructed amount, currency code, remittance information, requested execution date
- **Output Data**: Payment initiation response containing payment ID, transaction status (RCVD, PDNG, ACCP, ACTC, ACWC, ACSC, ACWP, ACFC, RJCT), SCA methods available, links to authorisation resources
- **Processing Type**: API (HTTP request-response, Real-time)

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description.

- **Endpoint**: POST /v1/payments/{payment-product}
  - **Justification (from description)**: "Initiate payments" - the word "Initiate" explicitly justifies a POST endpoint for creating payment transactions
  - **Purpose**: Initiate a single payment transaction per Berlin Group specification
  - **Request**: 
    ```json
    {
      "debtorAccount": {
        "iban": "string",
        "currency": "string"
      },
      "instructedAmount": {
        "currency": "string",
        "amount": "string"
      },
      "creditorAccount": {
        "iban": "string"
      },
      "creditorName": "string",
      "creditorAddress": {
        "streetName": "string",
        "buildingNumber": "string",
        "postCode": "string",
        "townName": "string",
        "country": "string"
      },
      "remittanceInformationUnstructured": "string",
      "requestedExecutionDate": "string (date)"
    }
    ```
  - **Response**: 
    ```json
    {
      "transactionStatus": "string (RCVD|PDNG|ACCP|ACTC|ACWC|ACSC|ACWP|ACFC|RJCT)",
      "paymentId": "string",
      "transactionFees": {
        "currency": "string",
        "amount": "string"
      },
      "transactionFeeIndicator": "boolean",
      "scaMethods": [
        {
          "authenticationType": "string",
          "authenticationMethodId": "string",
          "name": "string"
        }
      ],
      "_links": {
        "scaRedirect": {
          "href": "string"
        },
        "scaOAuth": {
          "href": "string"
        },
        "startAuthorisation": {
          "href": "string"
        },
        "startAuthorisationWithPsuIdentification": {
          "href": "string"
        },
        "startAuthorisationWithPsuAuthentication": {
          "href": "string"
        },
        "startAuthorisationWithEncryptedPsuAuthentication": {
          "href": "string"
        },
        "startAuthorisationWithAuthenticationMethodSelection": {
          "href": "string"
        },
        "self": {
          "href": "string"
        },
        "status": {
          "href": "string"
        }
      },
      "psuMessage": "string"
    }
    ```

## Business Rules (from capability description)
1. Payment initiation must comply with Berlin Group NextGenPSD2 specification standards
2. All payment requests must include valid IBAN format for debtor and creditor accounts
3. Instructed amount must be positive and currency must be a valid ISO 4217 currency code
4. Payment products supported include: sepa-credit-transfers, instant-sepa-credit-transfers, target-2-payments, cross-border-credit-transfers
5. Strong Customer Authentication (SCA) is required for payment initiation as per PSD2 Article 97
6. TPP must have valid PIS (Payment Initiation Service) authorization to initiate payments
7. Payment execution date cannot be in the past
8. Creditor name is mandatory for all payment types

## Data Validations (if applicable)
- Debtor IBAN must be valid and pass checksum validation
- Creditor IBAN must be valid and pass checksum validation
- Currency code must be a valid ISO 4217 three-letter code
- Amount must be a positive decimal value with maximum 2 decimal places for most currencies
- Requested execution date must be a valid date in ISO 8601 format (YYYY-MM-DD)
- Requested execution date must not be in the past
- Creditor name must not exceed maximum length (typically 70 characters)
- Remittance information must not exceed maximum length (typically 140 characters)
- TPP certificate must be valid and contain PIS role

## Dependencies
- **Upstream**: 
  - TPP must be authenticated with valid eIDAS certificate containing PIS role
  - Valid consent or implicit consent through SCA must be established
  - Debtor account must exist and be accessible
  - Sufficient funds must be available in debtor account (or overdraft limit)
- **Downstream**: 
  - Payment authorisation sub-resource is created for SCA flow
  - Payment status can be queried via Berlin Group Payment Status capability
  - Payment can be cancelled via Berlin Group Payment Cancellation capability (if supported)
  - Transaction appears in account transaction history after execution
- **External Systems**: 
  - Core banking system for account validation and payment execution
  - SEPA clearing network for SEPA credit transfers
  - TARGET2 system for high-value payments
  - SCA provider for Strong Customer Authentication

## Notes for Implementation
- The capability description mentions "Initiate payments" which only justifies the POST endpoint for payment initiation - retrieval (GET), status checking, and cancellation (DELETE) endpoints are covered by separate capabilities (Berlin Group Payment Status, Berlin Group Payment Cancellation)
- Berlin Group specification supports multiple payment products - implementation should be flexible to handle different payment types through the {payment-product} path parameter
- SCA flow complexity varies based on ASPSP (Account Servicing Payment Service Provider) implementation - may require redirect, decoupled, or embedded approach
- Idempotency should be implemented using X-Request-ID header to prevent duplicate payment submissions
- TPP-Redirect-URI header is required for redirect SCA approach
- PSU-IP-Address header is recommended for fraud detection
- Consider implementing rate limiting per TPP to prevent abuse
- Error responses must follow Berlin Group error format with tppMessages array

## Quality Checklist Verification
- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Third-Party Payment Service Provider/Fintech Application)
- [x] Business value is stated (PSD2-compliant payment execution through standardized API)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (SCA flow variations, payment product specifics)
- [x] Only relevant endpoints are included (POST for initiate)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words from the capability description justify inclusion
- [x] No endpoint type has been added unless its verb appears in the description
- [x] "Initiate" has been interpreted as create/POST operation only - status retrieval and cancellation are separate capabilities
