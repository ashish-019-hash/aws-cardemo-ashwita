# Validation Rules

**Extracted From:** Scala Application (OBP-API)  
**User Story:** Settlement Account Management  
**Analysis Date:** 2026-01-20  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 8

### Validation Categories
- Input Validation Rules: 3
- Format Validation Rules: 2
- Business Constraint Rules: 2
- Authorization Rules: 1

---

## Category: Input Validation

### Rule VR-001: Bank ID Existence Validation

**Field/Entity:** BANK_ID

**Validation Type:** Required Field / Entity Existence Validation

**Rule Description:**
The specified BANK_ID must exist in the system before any settlement account operations can be performed. This ensures that settlement accounts are always associated with a valid bank entity.

**Validation Logic:**

- **Condition:** When creating or updating a settlement account via POST /obp/v5.1.0/banks/{BANK_ID}/settlement-accounts or PUT /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}
- **Check:** Verify that the BANK_ID exists in the bank registry
- **Valid Criteria:** BANK_ID corresponds to an existing bank entity in the system
- **Invalid Criteria:** BANK_ID does not exist or is empty/null
- **Action on Success:** Proceed with settlement account creation/update operation
- **Action on Failure:** Return error response indicating bank not found

**Error Handling:**

- **Error Message:** `Bank not found. Please specify a valid BANK_ID.`
- **Error Code:** `OBP-30001` (Bank Not Found)
- **HTTP Status Code:** `404 Not Found`

**Related Entities:**
- Bank (bank_id field)
- Settlement Account (associated bank_id)

**User Story Context:**
This validation ensures that settlement accounts are only created for valid banks, maintaining referential integrity in the system. As stated in the acceptance criteria: "The system shall allow authorized users to create new settlement accounts for a specific bank."

**Dependencies:**
- Bank entity must be created before settlement accounts can be created

---

### Rule VR-002: User ID Existence Validation

**Field/Entity:** user_id

**Validation Type:** Optional Field / Entity Existence Validation

**Rule Description:**
If a user_id is specified in the settlement account creation request, it must correspond to an existing user in the system. If not specified, the creating user becomes the owner.

**Validation Logic:**

- **Condition:** When user_id is provided in the settlement account creation request
- **Check:** Verify that the user_id exists in the user registry
- **Valid Criteria:** 
  - user_id is not provided (optional field), OR
  - user_id corresponds to an existing user in the system
- **Invalid Criteria:** user_id is provided but does not correspond to any existing user
- **Action on Success:** Associate the settlement account with the specified user or the creating user
- **Action on Failure:** Return error response indicating user not found

**Error Handling:**

- **Error Message:** `User not found. Please specify a valid user_id.`
- **Error Code:** `OBP-20005` (User Not Found)
- **HTTP Status Code:** `404 Not Found`

**Related Entities:**
- User (user_id field)
- Settlement Account (owner user_id)

**User Story Context:**
This validation supports the business rule that "Settlement accounts can be owned by a specific user or the creating user if no user_id is specified."

**Dependencies:**
- User entity must exist if user_id is specified

---

### Rule VR-003: Payment System Validation

**Field/Entity:** payment_system

**Validation Type:** Input Validation / Enumeration Check

**Rule Description:**
The payment system must be a recognized value from the supported payment systems list. This ensures proper categorization of settlement accounts by payment method.

**Validation Logic:**

- **Condition:** When creating a settlement account with a payment_system value
- **Check:** Verify that payment_system is one of the recognized values
- **Valid Criteria:** payment_system is one of: SEPA, CARD, DEFAULT (or other recognized payment systems)
- **Invalid Criteria:** payment_system is empty, null, or not in the recognized values list
- **Action on Success:** Create settlement account with the specified payment system
- **Action on Failure:** Return error response indicating invalid payment system

**Error Handling:**

- **Error Message:** `Invalid payment system. Please specify a valid payment system (e.g., SEPA, CARD, DEFAULT).`
- **Error Code:** `OBP-30101` (Invalid Payment System)
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- Settlement Account (payment_system field)
- Account ID generation (uses payment_system in naming convention)

**User Story Context:**
This validation ensures that settlement accounts are properly configured for different payment systems as stated: "Settlement accounts shall be configurable for different payment systems (e.g., SEPA, CARD, DEFAULT)."

**Dependencies:**
- None (standalone validation)

---

## Category: Format Validation

### Rule VR-004: Currency ISO Code Validation

**Field/Entity:** balance.currency

**Validation Type:** Format Validation (ISO Standard)

**Rule Description:**
The currency must be a valid ISO 4217 currency code. This ensures proper currency handling and international compatibility.

**Validation Logic:**

- **Condition:** When creating a settlement account with a balance containing currency
- **Check:** Validate that currency is a valid 3-letter ISO 4217 currency code
- **Valid Criteria:** 
  - Currency is a 3-letter alphabetic code
  - Currency exists in the ISO 4217 currency code list (e.g., EUR, USD, GBP, XBT)
- **Invalid Criteria:** 
  - Currency is not 3 characters
  - Currency contains non-alphabetic characters
  - Currency is not in the ISO 4217 list
- **Action on Success:** Create settlement account with the specified currency
- **Action on Failure:** Return error response indicating invalid currency code

**Error Handling:**

- **Error Message:** `Invalid Currency Value. Expected a 3-letter ISO Currency Code.`
- **Error Code:** `OBP-10003` (Invalid ISO Currency Code)
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- Settlement Account (balance.currency field)
- Account ID generation (uses currency in naming convention: {PAYMENT_SYSTEM}_SETTLEMENT_ACCOUNT_{CURRENCY})

**User Story Context:**
This validation supports the requirement that "Settlement accounts shall support multiple currencies for payment processing" and ensures proper currency segregation for banks operating in multiple currencies.

**Dependencies:**
- ISO 4217 currency code reference data

---

### Rule VR-005: Account Routing Validation

**Field/Entity:** account_routings (scheme and address)

**Validation Type:** Format Validation / Structure Validation

**Rule Description:**
Account routings must have valid scheme and address values. The scheme identifies the routing type (e.g., IBAN, SWIFT) and the address contains the actual routing identifier.

**Validation Logic:**

- **Condition:** When account_routings are provided in the settlement account creation/update request
- **Check:** Validate that each routing has a non-empty scheme and address
- **Valid Criteria:** 
  - Each account_routing object has a non-empty scheme value
  - Each account_routing object has a non-empty address value
  - Scheme is a recognized routing scheme (e.g., IBAN, SWIFT, BIC)
  - Address format matches the expected format for the scheme (e.g., IBAN format for IBAN scheme)
- **Invalid Criteria:** 
  - scheme is empty or null
  - address is empty or null
  - scheme is not recognized
  - address format does not match scheme requirements
- **Action on Success:** Associate the account routings with the settlement account
- **Action on Failure:** Return error response indicating invalid account routing

**Error Handling:**

- **Error Message:** `Invalid account routing. Please provide valid scheme and address values.`
- **Error Code:** `OBP-30102` (Invalid Account Routing)
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- Settlement Account (account_routings array)
- External payment systems (use routings for integration)

**User Story Context:**
This validation ensures proper integration with external payment systems as stated: "Settlement accounts shall support account routings for integration with external payment systems."

**Dependencies:**
- Routing scheme reference data

---

## Category: Business Constraint Validation

### Rule VR-006: Initial Balance Zero Validation

**Field/Entity:** balance.amount

**Validation Type:** Business Constraint Validation

**Rule Description:**
The initial balance amount MUST be zero for new settlement accounts. This is a critical business rule to ensure proper accounting initialization.

**Validation Logic:**

- **Condition:** When creating a new settlement account
- **Check:** Verify that the balance amount is exactly zero
- **Valid Criteria:** balance.amount equals "0" or 0
- **Invalid Criteria:** balance.amount is any value other than zero (positive or negative)
- **Action on Success:** Create settlement account with zero balance
- **Action on Failure:** Return error response indicating balance must be zero

**Error Handling:**

- **Error Message:** `Initial balance for settlement accounts must be zero.`
- **Error Code:** `OBP-30103` (Invalid Initial Balance)
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- Settlement Account (balance.amount field)

**User Story Context:**
This validation enforces the business rule stated in acceptance criteria: "Settlement accounts shall have an initial balance of zero upon creation" and in business rules: "Initial balance for new settlement accounts MUST be zero."

**Dependencies:**
- None (standalone validation)

---

### Rule VR-007: Account ID Generation Pattern Validation

**Field/Entity:** account_id (auto-generated)

**Validation Type:** Business Constraint / Format Validation

**Rule Description:**
The settlement account ID shall be automatically generated based on the payment system and currency following the pattern: {PAYMENT_SYSTEM}_SETTLEMENT_ACCOUNT_{CURRENCY}.

**Validation Logic:**

- **Condition:** When creating a new settlement account
- **Check:** System generates account_id following the naming convention
- **Valid Criteria:** 
  - Account ID follows pattern: {PAYMENT_SYSTEM}_SETTLEMENT_ACCOUNT_{CURRENCY}
  - Example: SEPA_SETTLEMENT_ACCOUNT_EUR, CARD_SETTLEMENT_ACCOUNT_USD
  - For default accounts: OBP_DEFAULT_INCOMING_ACCOUNT_ID, OBP_DEFAULT_OUTGOING_ACCOUNT_ID
- **Invalid Criteria:** 
  - Account ID does not follow the expected pattern
  - Account ID contains invalid characters
  - Account ID already exists (uniqueness constraint)
- **Action on Success:** Create settlement account with the generated ID
- **Action on Failure:** Return error if ID generation fails or conflicts

**Error Handling:**

- **Error Message:** `Settlement account with this ID already exists.`
- **Error Code:** `OBP-30104` (Duplicate Settlement Account)
- **HTTP Status Code:** `409 Conflict`

**Related Entities:**
- Settlement Account (account_id field)
- Payment System (used in ID generation)
- Currency (used in ID generation)

**User Story Context:**
This validation ensures consistent account ID generation as stated: "The settlement account ID shall be automatically generated based on the payment system and currency (e.g., SEPA_SETTLEMENT_ACCOUNT_EUR)."

**Dependencies:**
- VR-003: Payment System Validation
- VR-004: Currency ISO Code Validation

---

## Category: Authorization Validation

### Rule VR-008: Settlement Account Creation Authorization

**Field/Entity:** User Role/Entitlement

**Validation Type:** Authorization Validation

**Rule Description:**
User must have the CanCreateSettlementAccountAtOneBank role to create settlement accounts. This ensures proper access control for sensitive financial operations.

**Validation Logic:**

- **Condition:** When a user attempts to create a settlement account via POST /obp/v5.1.0/banks/{BANK_ID}/settlement-accounts
- **Check:** Verify that the authenticated user has the CanCreateSettlementAccountAtOneBank entitlement for the specified bank
- **Valid Criteria:** 
  - User is authenticated
  - User has CanCreateSettlementAccountAtOneBank role for the target BANK_ID
- **Invalid Criteria:** 
  - User is not authenticated
  - User does not have the required role/entitlement
  - User has the role but for a different bank
- **Action on Success:** Allow settlement account creation to proceed
- **Action on Failure:** Return authorization error

**Error Handling:**

- **Error Message:** `User does not have the required entitlement: CanCreateSettlementAccountAtOneBank`
- **Error Code:** `OBP-20006` (Insufficient Entitlements)
- **HTTP Status Code:** `403 Forbidden`

**Related Entities:**
- User (authenticated user)
- Entitlement (CanCreateSettlementAccountAtOneBank role)
- Bank (bank_id for role scope)

**User Story Context:**
This validation enforces the authorization requirement stated: "User must have the CanCreateSettlementAccountAtOneBank role to create settlement accounts" and ensures that only authorized Bank Administrators or Financial Operations Managers can create settlement accounts.

**Dependencies:**
- User authentication must be completed first
- Entitlement/role management system

---

## Cross-Reference Summary

| Rule ID | Field/Entity | Validation Type | Error Code | HTTP Status |
|---------|--------------|-----------------|------------|-------------|
| VR-001 | BANK_ID | Entity Existence | OBP-30001 | 404 |
| VR-002 | user_id | Entity Existence | OBP-20005 | 404 |
| VR-003 | payment_system | Enumeration | OBP-30101 | 400 |
| VR-004 | balance.currency | ISO Format | OBP-10003 | 400 |
| VR-005 | account_routings | Structure | OBP-30102 | 400 |
| VR-006 | balance.amount | Business Constraint | OBP-30103 | 400 |
| VR-007 | account_id | Pattern/Uniqueness | OBP-30104 | 409 |
| VR-008 | User Role | Authorization | OBP-20006 | 403 |

---

## Validation Execution Order

For settlement account creation, validations should be executed in the following order:

1. **VR-008**: Authorization Validation (check user has required role)
2. **VR-001**: Bank ID Existence Validation (verify bank exists)
3. **VR-002**: User ID Existence Validation (if user_id provided)
4. **VR-003**: Payment System Validation (verify valid payment system)
5. **VR-004**: Currency ISO Code Validation (verify valid currency)
6. **VR-006**: Initial Balance Zero Validation (verify balance is zero)
7. **VR-005**: Account Routing Validation (if routings provided)
8. **VR-007**: Account ID Generation (generate and verify uniqueness)

---

## Notes

- All validation rules are derived from the Settlement Account Management user story
- Error codes follow the OBP-XXXXX format convention
- HTTP status codes follow REST API best practices
- Some validation rules may have dependencies on other rules as noted
- The complete list of supported payment systems should be confirmed with SME
- Additional currency support beyond EUR for default accounts should be clarified with SME
