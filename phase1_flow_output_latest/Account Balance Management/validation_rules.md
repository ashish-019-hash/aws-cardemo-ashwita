# Validation Rules

**Extracted From:** Account Balance Management Capability  
**User Story:** Account Balance Management User Story  
**Analysis Date:** 2026-01-20  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 12

### Validation Categories
- Input Validation Rules: 5
- Format Validation Rules: 2
- Business Constraint Rules: 3
- Length/Boundary Rules: 0
- Cross-Field Validation Rules: 2

---

## Category: Input Validation

### Rule VR-001: Bank ID Required Validation

**Field/Entity:** BANK_ID

**Validation Type:** Required Field Validation

**Rule Description:**
Bank ID must be provided as a path parameter for all balance management operations. The bank identifier must reference a valid, existing bank on the platform.

**Validation Logic:**

- **Condition:** When any balance management API endpoint is called (POST, PUT, DELETE)
- **Check:** Validate that BANK_ID path parameter is present and non-empty
- **Valid Criteria:** BANK_ID is provided and corresponds to an existing bank in the system
- **Invalid Criteria:** BANK_ID is missing, empty, or does not match any bank in the system
- **Action on Success:** Proceed with balance operation
- **Action on Failure:** Return error response indicating invalid or missing bank ID

**Error Handling:**

- **Error Message:** `Bank not found` or `Invalid Bank ID`
- **Error Code:** `OBP-30001` (Bank not found)
- **HTTP Status Code:** `400 Bad Request` or `404 Not Found`

**Scala Implementation:**

- **Location:** `BankAccountBalanceProvider` / API endpoint handlers
- **Method/Function:** Path parameter extraction and bank lookup
- **Line Reference:** API route definitions

**Related Entities:**
- Bank entity
- BankAccountBalance entity

**User Story Context:**
This validation ensures that balance records can only be created, updated, or deleted for accounts belonging to valid banks on the platform, as stated in the acceptance criteria: "Each balance record shall be associated with a specific bank and account."

**Dependencies:**
- Bank must exist in the system

---

### Rule VR-002: Account ID Required Validation

**Field/Entity:** ACCOUNT_ID

**Validation Type:** Required Field Validation

**Rule Description:**
Account ID must be provided as a path parameter for all balance management operations. The account identifier must reference a valid, existing account at the specified bank.

**Validation Logic:**

- **Condition:** When any balance management API endpoint is called (POST, PUT, DELETE)
- **Check:** Validate that ACCOUNT_ID path parameter is present and corresponds to an existing account
- **Valid Criteria:** ACCOUNT_ID is provided and corresponds to an existing account at the specified bank
- **Invalid Criteria:** ACCOUNT_ID is missing, empty, or does not match any account at the specified bank
- **Action on Success:** Proceed with balance operation
- **Action on Failure:** Return error response indicating invalid or missing account ID

**Error Handling:**

- **Error Message:** `Account not found` or `Invalid Account ID`
- **Error Code:** `OBP-30018` (Account not found)
- **HTTP Status Code:** `400 Bad Request` or `404 Not Found`

**Scala Implementation:**

- **Location:** `BankAccountBalanceProvider` / API endpoint handlers
- **Method/Function:** Path parameter extraction and account lookup
- **Line Reference:** API route definitions

**Related Entities:**
- BankAccount entity
- BankAccountBalance entity

**User Story Context:**
This validation ensures that balance records are only managed for valid accounts, as stated in the acceptance criteria: "The system shall validate that the account exists before creating or updating balance records."

**Dependencies:**
- VR-001: Bank ID must be valid first
- Account must exist at the specified bank

---

### Rule VR-003: Balance ID Required Validation (Update/Delete)

**Field/Entity:** BALANCE_ID

**Validation Type:** Required Field Validation

**Rule Description:**
Balance ID must be provided as a path parameter for update and delete operations. The balance identifier must reference an existing balance record.

**Validation Logic:**

- **Condition:** When PUT or DELETE balance management API endpoint is called
- **Check:** Validate that BALANCE_ID path parameter is present and corresponds to an existing balance record
- **Valid Criteria:** BALANCE_ID is provided and corresponds to an existing balance record for the specified account
- **Invalid Criteria:** BALANCE_ID is missing, empty, or does not match any balance record
- **Action on Success:** Proceed with update or delete operation
- **Action on Failure:** Return error response indicating balance record not found

**Error Handling:**

- **Error Message:** `Balance record not found`
- **Error Code:** `OBP-30XXX` (Balance not found)
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `MappedBankAccountBalanceProvider`
- **Method/Function:** Balance lookup by ID
- **Line Reference:** Update and delete method implementations

**Related Entities:**
- BankAccountBalance entity

**User Story Context:**
This validation ensures that update and delete operations target existing balance records, as stated in the data validations: "Balance ID must be a valid identifier for an existing balance record (for update and delete operations)."

**Dependencies:**
- VR-001: Bank ID must be valid
- VR-002: Account ID must be valid

---

### Rule VR-004: Balance Type Required Validation

**Field/Entity:** balance_type

**Validation Type:** Required Field Validation

**Rule Description:**
Balance type must be provided in the request body for create and update operations. It must be a non-empty string value.

**Validation Logic:**

- **Condition:** When POST or PUT balance management API endpoint is called
- **Check:** Validate that balance_type field is present in request body and is non-empty
- **Valid Criteria:** balance_type is a non-empty string (e.g., "available", "booked", "pending")
- **Invalid Criteria:** balance_type is missing, null, or empty string
- **Action on Success:** Proceed with balance creation or update
- **Action on Failure:** Return error response indicating missing or invalid balance type

**Error Handling:**

- **Error Message:** `Invalid balance type` or `Balance type is required`
- **Error Code:** `OBP-10001` (Invalid JSON format) or custom validation error
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `JSONFactory510` / Request body parsing
- **Method/Function:** JSON deserialization and validation
- **Line Reference:** JSON parsing methods

**Related Entities:**
- BankAccountBalance entity (balance_type field)

**User Story Context:**
This validation ensures that balance records have a defined type, as stated in the data validations: "Balance type must be a non-empty string."

**Dependencies:**
- Valid JSON request body

---

### Rule VR-005: Balance Amount Required Validation

**Field/Entity:** balance_amount

**Validation Type:** Required Field Validation

**Rule Description:**
Balance amount must be provided in the request body for create and update operations. It must be a valid numeric value.

**Validation Logic:**

- **Condition:** When POST or PUT balance management API endpoint is called
- **Check:** Validate that balance_amount field is present in request body
- **Valid Criteria:** balance_amount is provided in the request body
- **Invalid Criteria:** balance_amount is missing or null
- **Action on Success:** Proceed to format validation (VR-006)
- **Action on Failure:** Return error response indicating missing balance amount

**Error Handling:**

- **Error Message:** `Balance amount is required`
- **Error Code:** `OBP-10001` (Invalid JSON format)
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `JSONFactory510` / Request body parsing
- **Method/Function:** JSON deserialization
- **Line Reference:** JSON parsing methods

**Related Entities:**
- BankAccountBalance entity (balance_amount field)

**User Story Context:**
This validation ensures that balance records have an amount value, as stated in the acceptance criteria: "Balance records shall include balance type and balance amount information."

**Dependencies:**
- Valid JSON request body

---

## Category: Format Validation

### Rule VR-006: Balance Amount Numeric Format Validation

**Field/Entity:** balance_amount

**Validation Type:** Format Validation (Numeric)

**Rule Description:**
Balance amount must be a valid numeric value that can be parsed as BigDecimal. The value represents the balance in the smallest currency unit.

**Validation Logic:**

- **Condition:** When balance_amount is provided in POST or PUT request
- **Check:** Validate that balance_amount can be parsed as a valid BigDecimal number
- **Valid Criteria:** 
  - Value is a valid numeric string (e.g., "1000", "1000.50", "-500.25")
  - Can be successfully parsed as BigDecimal
- **Invalid Criteria:**
  - Value contains non-numeric characters (except decimal point and minus sign)
  - Value is not a valid number format (e.g., "abc", "12.34.56", "")
- **Action on Success:** Convert to BigDecimal and proceed with operation
- **Action on Failure:** Return error response indicating invalid balance amount format

**Error Handling:**

- **Error Message:** `Invalid balance amount (not a valid number)`
- **Error Code:** `OBP-10002` (Invalid Number)
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `JSONFactory510` / Balance parsing utilities
- **Method/Function:** BigDecimal parsing and conversion
- **Line Reference:** Amount conversion methods

**Code Snippet:**
```scala
// Example pattern for BigDecimal validation
def validateAmount(amountStr: String): Box[BigDecimal] = {
  tryo {
    BigDecimal(amountStr)
  } ?~! "Invalid balance amount (not a valid number)"
}
```

**Related Entities:**
- BankAccountBalance entity (balance_amount field)

**User Story Context:**
This validation ensures that balance amounts are valid numeric values, as stated in the data validations: "Balance amount must be a valid numeric value that can be parsed as BigDecimal."

**Dependencies:**
- VR-005: Balance amount must be present

---

### Rule VR-007: JSON Request Body Format Validation

**Field/Entity:** Request Body

**Validation Type:** Format Validation (JSON)

**Rule Description:**
The request body for create and update operations must be valid JSON format with the expected structure.

**Validation Logic:**

- **Condition:** When POST or PUT balance management API endpoint is called
- **Check:** Validate that request body is valid JSON with expected fields
- **Valid Criteria:** 
  - Request body is valid JSON
  - Contains required fields: balance_type, balance_amount
- **Invalid Criteria:**
  - Request body is not valid JSON
  - Missing required fields
  - Malformed JSON structure
- **Action on Success:** Parse JSON and proceed with field validations
- **Action on Failure:** Return error response indicating invalid JSON format

**Error Handling:**

- **Error Message:** `Invalid JSON format`
- **Error Code:** `OBP-10001` (Incorrect json format)
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** API endpoint handlers / JSON parsing layer
- **Method/Function:** JSON extraction and parsing
- **Line Reference:** Request body parsing

**Related Entities:**
- All balance management endpoints

**User Story Context:**
This validation ensures that API requests are properly formatted, as stated in the implementation notes: "Error handling should provide clear messages for common failure scenarios: Invalid JSON format."

**Dependencies:**
- None (first validation in request processing)

---

## Category: Business Constraint Validation

### Rule VR-008: User Authentication Validation

**Field/Entity:** Authorization Header / User Session

**Validation Type:** Business Constraint (Authentication)

**Rule Description:**
All balance management operations require user authentication. The user must be logged in with a valid authentication token.

**Validation Logic:**

- **Condition:** When any balance management API endpoint is called
- **Check:** Validate that user is authenticated with valid credentials/token
- **Valid Criteria:** 
  - Valid Bearer token or OAuth credentials provided in Authorization header
  - Token is not expired
  - User session is active
- **Invalid Criteria:**
  - No Authorization header provided
  - Invalid or expired token
  - User not logged in
- **Action on Success:** Proceed with authorization check (VR-009)
- **Action on Failure:** Return error response indicating user not logged in

**Error Handling:**

- **Error Message:** `User not logged in`
- **Error Code:** `OBP-20001` (User not logged in)
- **HTTP Status Code:** `401 Unauthorized`

**Scala Implementation:**

- **Location:** Authentication middleware / API security layer
- **Method/Function:** Token validation and user session check
- **Line Reference:** Authentication interceptors

**Related Entities:**
- User entity
- Authentication token

**User Story Context:**
This validation ensures secure access to balance management operations, as stated in the acceptance criteria: "All balance management operations shall require user authentication."

**Dependencies:**
- None (first business validation)

---

### Rule VR-009: User Authorization/Role Validation

**Field/Entity:** User Roles/Entitlements

**Validation Type:** Business Constraint (Authorization)

**Rule Description:**
Users must have appropriate role-based permissions to perform balance management operations. Different operations require different roles.

**Validation Logic:**

- **Condition:** After user authentication is verified
- **Check:** Validate that user has the required role for the specific operation
- **Valid Criteria:** 
  - For POST (create): User has `canCreateBankAccountBalance` role
  - For PUT (update): User has `canUpdateBankAccountBalance` role
  - For DELETE: User has `canDeleteBankAccountBalance` role
- **Invalid Criteria:**
  - User does not have the required role for the operation
  - User's role has been revoked
- **Action on Success:** Proceed with the balance operation
- **Action on Failure:** Return error response indicating missing required role

**Error Handling:**

- **Error Message:** `User missing required role` or `Insufficient permissions`
- **Error Code:** `OBP-20006` (User missing required role)
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** Authorization middleware / Entitlement checking
- **Method/Function:** Role/entitlement validation
- **Line Reference:** Authorization interceptors

**Related Entities:**
- User entity
- Entitlement/Role entities

**User Story Context:**
This validation ensures proper access control, as stated in the data validations: "User must have appropriate permissions/entitlements (canCreateBankAccountBalance, canUpdateBankAccountBalance, canDeleteBankAccountBalance)."

**Dependencies:**
- VR-008: User must be authenticated first

---

### Rule VR-010: Account Existence Validation

**Field/Entity:** ACCOUNT_ID + BANK_ID combination

**Validation Type:** Business Constraint (Entity Existence)

**Rule Description:**
The account must exist in the system before balance records can be created or updated for it. The account must belong to the specified bank.

**Validation Logic:**

- **Condition:** When creating or updating balance records
- **Check:** Validate that the account exists at the specified bank
- **Valid Criteria:** 
  - Account with given ACCOUNT_ID exists
  - Account belongs to the bank with given BANK_ID
  - Account is active (not closed or suspended)
- **Invalid Criteria:**
  - Account does not exist
  - Account exists but belongs to a different bank
  - Account is closed or suspended
- **Action on Success:** Proceed with balance operation
- **Action on Failure:** Return error response indicating account not found

**Error Handling:**

- **Error Message:** `Account not found`
- **Error Code:** `OBP-30018` (Account not found)
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `BankAccountBalanceNewStyle` / Account lookup
- **Method/Function:** Account existence check
- **Line Reference:** Pre-operation validation

**Related Entities:**
- BankAccount entity
- Bank entity

**User Story Context:**
This validation ensures data integrity, as stated in the acceptance criteria: "The system shall validate that the account exists before creating or updating balance records."

**Dependencies:**
- VR-001: Bank ID must be valid
- VR-002: Account ID must be provided

---

## Category: Cross-Field Validation

### Rule VR-011: Bank-Account Association Validation

**Field/Entity:** BANK_ID + ACCOUNT_ID

**Validation Type:** Cross-Field Validation

**Rule Description:**
The account specified by ACCOUNT_ID must belong to the bank specified by BANK_ID. This ensures balance records are created for the correct bank-account combination.

**Validation Logic:**

- **Condition:** When any balance management operation is performed
- **Check:** Validate that the account belongs to the specified bank
- **Valid Criteria:** 
  - Account's bank reference matches the provided BANK_ID
- **Invalid Criteria:**
  - Account exists but belongs to a different bank
- **Action on Success:** Proceed with balance operation
- **Action on Failure:** Return error response indicating account not found at specified bank

**Error Handling:**

- **Error Message:** `Account not found at specified bank`
- **Error Code:** `OBP-30018` (Account not found)
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `BankAccountBalanceProvider`
- **Method/Function:** Bank-account association check
- **Line Reference:** Lookup methods

**Related Entities:**
- Bank entity
- BankAccount entity
- BankAccountBalance entity

**User Story Context:**
This validation ensures proper association between banks and accounts, as stated in the business rules: "Balance records are associated with a specific bank and account combination."

**Dependencies:**
- VR-001: Bank ID must be valid
- VR-002: Account ID must be valid

---

### Rule VR-012: Balance-Account Association Validation (Update/Delete)

**Field/Entity:** BALANCE_ID + ACCOUNT_ID + BANK_ID

**Validation Type:** Cross-Field Validation

**Rule Description:**
For update and delete operations, the balance record must belong to the specified account at the specified bank. This ensures operations target the correct balance record.

**Validation Logic:**

- **Condition:** When PUT or DELETE balance management operation is performed
- **Check:** Validate that the balance record belongs to the specified account and bank
- **Valid Criteria:** 
  - Balance record exists with the given BALANCE_ID
  - Balance record's account reference matches ACCOUNT_ID
  - Balance record's bank reference matches BANK_ID
- **Invalid Criteria:**
  - Balance record does not exist
  - Balance record exists but belongs to a different account
  - Balance record exists but belongs to a different bank
- **Action on Success:** Proceed with update or delete operation
- **Action on Failure:** Return error response indicating balance record not found

**Error Handling:**

- **Error Message:** `Balance record not found for specified account`
- **Error Code:** `OBP-30XXX` (Balance not found)
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `MappedBankAccountBalanceProvider`
- **Method/Function:** Balance lookup with account/bank verification
- **Line Reference:** Update and delete method implementations

**Related Entities:**
- Bank entity
- BankAccount entity
- BankAccountBalance entity

**User Story Context:**
This validation ensures that update and delete operations target the correct balance records, as stated in the acceptance criteria: "The system shall allow updating existing balance records identified by a unique balance ID" and "The system shall allow deletion of balance records by their unique balance ID."

**Dependencies:**
- VR-001: Bank ID must be valid
- VR-002: Account ID must be valid
- VR-003: Balance ID must be valid

---

## Validation Execution Order

The validations should be executed in the following order for optimal error handling:

1. **VR-007**: JSON Request Body Format Validation (for POST/PUT)
2. **VR-008**: User Authentication Validation
3. **VR-009**: User Authorization/Role Validation
4. **VR-001**: Bank ID Required Validation
5. **VR-002**: Account ID Required Validation
6. **VR-010**: Account Existence Validation
7. **VR-011**: Bank-Account Association Validation
8. **VR-003**: Balance ID Required Validation (for PUT/DELETE)
9. **VR-012**: Balance-Account Association Validation (for PUT/DELETE)
10. **VR-004**: Balance Type Required Validation (for POST/PUT)
11. **VR-005**: Balance Amount Required Validation (for POST/PUT)
12. **VR-006**: Balance Amount Numeric Format Validation (for POST/PUT)

---

## Endpoint-Specific Validation Matrix

| Validation Rule | POST (Create) | PUT (Update) | DELETE |
|-----------------|---------------|--------------|--------|
| VR-001: Bank ID Required | Yes | Yes | Yes |
| VR-002: Account ID Required | Yes | Yes | Yes |
| VR-003: Balance ID Required | No | Yes | Yes |
| VR-004: Balance Type Required | Yes | Yes | No |
| VR-005: Balance Amount Required | Yes | Yes | No |
| VR-006: Balance Amount Format | Yes | Yes | No |
| VR-007: JSON Format | Yes | Yes | No |
| VR-008: Authentication | Yes | Yes | Yes |
| VR-009: Authorization | Yes | Yes | Yes |
| VR-010: Account Existence | Yes | Yes | Yes |
| VR-011: Bank-Account Association | Yes | Yes | Yes |
| VR-012: Balance-Account Association | No | Yes | Yes |

---

## Notes

- Balance IDs are auto-generated using UUID when creating new balance records, so no format validation is needed for balance_id on creation
- Balance amounts are stored in the smallest currency unit internally and converted for display using the account's currency
- Reference date and last change date time are automatically tracked for audit purposes and do not require user input validation
- The implementation uses a createOrUpdate pattern internally, where the presence of a balance ID determines whether to create a new record or update an existing one
