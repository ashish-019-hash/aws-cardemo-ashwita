# Validation Rules

**Extracted From:** Scala Application (OBP-API)  
**User Story:** Transaction Attribute Management  
**Analysis Date:** 2026-01-20  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 12

### Validation Categories
- Input Validation Rules: 4
- Format Validation Rules: 3
- Business Constraint Rules: 3
- Length/Boundary Rules: 1
- Cross-Field Validation Rules: 1

---

## Category: Input Validation

### Rule VR-001: Bank ID Required Validation

**Field/Entity:** BANK_ID (path parameter)

**Validation Type:** Required Field Validation

**Rule Description:**
Bank ID must be provided and must reference an existing bank in the system. This is a mandatory path parameter for all transaction attribute management operations.

**Validation Logic:**

- **Condition:** When any transaction attribute management API endpoint is called
- **Check:** Validate that BANK_ID is present in the URL path and corresponds to an existing bank
- **Valid Criteria:** 
  - BANK_ID is non-empty
  - BANK_ID references an existing bank in the database
- **Invalid Criteria:**
  - BANK_ID is empty or missing
  - BANK_ID does not match any existing bank
- **Action on Success:** Proceed with account validation
- **Action on Failure:** Return error response with appropriate error message

**Error Handling:**

- **Error Message:** `Bank not found. Please specify a valid value for BANK_ID.`
- **Error Code:** `OBP-30001`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `getBank(bankId: BankId, callContext: Option[CallContext])`
- **Line Reference:** API validation layer

**Code Snippet:**
```scala
for {
  (bank, callContext) <- NewStyle.function.getBank(bankId, callContext)
} yield {
  // proceed with operation
}
```

**Related Entities:**
- Bank entity
- All transaction attribute endpoints

**User Story Context:**
The user story specifies that "The system shall require valid bank ID, account ID, and transaction ID for all attribute management operations." Bank ID validation ensures operations are scoped to a valid bank context.

**Dependencies:**
- None (first validation in the chain)

---

### Rule VR-002: Account ID Required Validation

**Field/Entity:** ACCOUNT_ID (path parameter)

**Validation Type:** Required Field Validation

**Rule Description:**
Account ID must be provided and must reference an existing account belonging to the specified bank.

**Validation Logic:**

- **Condition:** When any transaction attribute management API endpoint is called
- **Check:** Validate that ACCOUNT_ID is present and corresponds to an existing account within the specified bank
- **Valid Criteria:** 
  - ACCOUNT_ID is non-empty
  - ACCOUNT_ID references an existing account
  - Account belongs to the specified BANK_ID
- **Invalid Criteria:**
  - ACCOUNT_ID is empty or missing
  - ACCOUNT_ID does not match any existing account
  - Account does not belong to the specified bank
- **Action on Success:** Proceed with transaction validation
- **Action on Failure:** Return error response with appropriate error message

**Error Handling:**

- **Error Message:** `Account not found. Please specify a valid value for ACCOUNT_ID.`
- **Error Code:** `OBP-30018`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `checkAccountAccess(bankId: BankId, accountId: AccountId, callContext: Option[CallContext])`
- **Line Reference:** API validation layer

**Code Snippet:**
```scala
for {
  (account, callContext) <- NewStyle.function.checkAccountAccess(bankId, accountId, callContext)
} yield {
  // proceed with operation
}
```

**Related Entities:**
- BankAccount entity
- Transaction attribute endpoints

**User Story Context:**
The user story specifies that "The system shall require valid bank ID, account ID, and transaction ID for all attribute management operations." Account validation ensures the transaction context is valid.

**Dependencies:**
- VR-001: Bank ID must be validated first

---

### Rule VR-003: Transaction ID Required Validation

**Field/Entity:** TRANSACTION_ID (path parameter)

**Validation Type:** Required Field Validation

**Rule Description:**
Transaction ID must be provided and must reference an existing transaction within the specified account.

**Validation Logic:**

- **Condition:** When any transaction attribute management API endpoint is called
- **Check:** Validate that TRANSACTION_ID is present and corresponds to an existing transaction within the specified account
- **Valid Criteria:** 
  - TRANSACTION_ID is non-empty
  - TRANSACTION_ID references an existing transaction
  - Transaction belongs to the specified account
- **Invalid Criteria:**
  - TRANSACTION_ID is empty or missing
  - TRANSACTION_ID does not match any existing transaction
  - Transaction does not belong to the specified account
- **Action on Success:** Proceed with attribute operation
- **Action on Failure:** Return error response with appropriate error message

**Error Handling:**

- **Error Message:** `Transaction not found. Please specify a valid value for TRANSACTION_ID.`
- **Error Code:** `OBP-30010`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `getTransaction(bankId: BankId, accountId: AccountId, transactionId: TransactionId, callContext: Option[CallContext])`
- **Line Reference:** API validation layer

**Code Snippet:**
```scala
for {
  (transaction, callContext) <- NewStyle.function.getTransaction(bankId, accountId, transactionId, callContext)
} yield {
  // proceed with attribute operation
}
```

**Related Entities:**
- Transaction entity
- TransactionAttribute entity

**User Story Context:**
The user story states "The transaction must exist before attributes can be managed on it" as a business rule. This validation ensures the target transaction exists.

**Dependencies:**
- VR-001: Bank ID must be validated first
- VR-002: Account ID must be validated first

---

### Rule VR-004: Attribute ID Required Validation (for Updates)

**Field/Entity:** ATTRIBUTE_ID (path parameter)

**Validation Type:** Required Field Validation

**Rule Description:**
For update operations, Attribute ID must be provided and must reference an existing attribute on the specified transaction.

**Validation Logic:**

- **Condition:** When PUT endpoint for updating transaction attributes is called
- **Check:** Validate that ATTRIBUTE_ID is present and corresponds to an existing attribute on the transaction
- **Valid Criteria:** 
  - ATTRIBUTE_ID is non-empty
  - ATTRIBUTE_ID references an existing transaction attribute
  - Attribute belongs to the specified transaction
- **Invalid Criteria:**
  - ATTRIBUTE_ID is empty or missing
  - ATTRIBUTE_ID does not match any existing attribute
  - Attribute does not belong to the specified transaction
- **Action on Success:** Proceed with attribute update
- **Action on Failure:** Return error response with appropriate error message

**Error Handling:**

- **Error Message:** `Transaction Attribute not found. Please specify a valid value for ATTRIBUTE_ID.`
- **Error Code:** `OBP-30024`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.model.TransactionAttributeProvider`
- **Method/Function:** `getTransactionAttributeById(transactionAttributeId: String)`
- **Line Reference:** Provider implementation

**Code Snippet:**
```scala
for {
  (attribute, callContext) <- NewStyle.function.getTransactionAttributeById(transactionAttributeId, callContext)
} yield {
  // proceed with update
}
```

**Related Entities:**
- TransactionAttribute entity

**User Story Context:**
The user story specifies "For updates, must reference an existing attribute" in the Data Validations section. This ensures update operations target valid attributes.

**Dependencies:**
- VR-001, VR-002, VR-003: Parent resource validations

---

## Category: Format Validation

### Rule VR-005: Attribute Type Enumeration Validation

**Field/Entity:** type (request body field)

**Validation Type:** Enumeration Validation

**Rule Description:**
The attribute type field must be one of the supported enumeration values: STRING, INTEGER, DOUBLE, or DATE_WITH_DAY.

**Validation Logic:**

- **Condition:** When creating or updating a transaction attribute
- **Check:** Validate that the type field matches one of the allowed TransactionAttributeType values
- **Valid Criteria:** 
  - type = "STRING"
  - type = "INTEGER"
  - type = "DOUBLE"
  - type = "DATE_WITH_DAY"
- **Invalid Criteria:**
  - type is empty or null
  - type does not match any of the enumerated values
  - type has incorrect casing (case-sensitive)
- **Action on Success:** Proceed with value validation based on type
- **Action on Failure:** Return error response with invalid type message

**Error Handling:**

- **Error Message:** `Invalid Transaction Attribute Type. Allowed values are: STRING, INTEGER, DOUBLE, DATE_WITH_DAY`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `TransactionAttributeType.withName(typeName: String)`
- **Line Reference:** Enumeration validation

**Code Snippet:**
```scala
sealed trait TransactionAttributeType
object TransactionAttributeType {
  case object STRING extends TransactionAttributeType
  case object INTEGER extends TransactionAttributeType
  case object DOUBLE extends TransactionAttributeType
  case object DATE_WITH_DAY extends TransactionAttributeType
}
```

**Related Entities:**
- TransactionAttribute entity
- Request body JSON

**User Story Context:**
The user story explicitly states: "The system shall support multiple attribute types including STRING, INTEGER, DOUBLE, and DATE_WITH_DAY" and "Attribute Type Validation: The type field must be one of the supported types."

**Dependencies:**
- VR-007: JSON format validation must pass first

---

### Rule VR-006: Attribute Value Type Compatibility Validation

**Field/Entity:** value (request body field)

**Validation Type:** Cross-Field Format Validation

**Rule Description:**
The attribute value must be compatible with the declared attribute type. For INTEGER type, value must be parseable as integer. For DOUBLE type, value must be parseable as decimal. For DATE_WITH_DAY, value must match date format.

**Validation Logic:**

- **Condition:** When creating or updating a transaction attribute with a specific type
- **Check:** Validate that the value can be parsed/converted to the declared type
- **Valid Criteria:** 
  - For STRING: Any string value is valid
  - For INTEGER: Value must be parseable as integer (e.g., "123")
  - For DOUBLE: Value must be parseable as decimal (e.g., "12.1234")
  - For DATE_WITH_DAY: Value must match format "YYYY-MM-DD" (e.g., "2012-04-23")
- **Invalid Criteria:**
  - For INTEGER: Value contains non-numeric characters or decimal point
  - For DOUBLE: Value cannot be parsed as a decimal number
  - For DATE_WITH_DAY: Value does not match the expected date format
- **Action on Success:** Proceed with attribute persistence
- **Action on Failure:** Return error response with type mismatch message

**Error Handling:**

- **Error Message:** `Invalid value for attribute type. Expected [TYPE] format but received incompatible value.`
- **Error Code:** `OBP-10002` (for numeric) / `OBP-10005` (for date)
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `validateAttributeValue(attributeType: TransactionAttributeType, value: String)`
- **Line Reference:** Value validation logic

**Code Snippet:**
```scala
def validateAttributeValue(attrType: TransactionAttributeType, value: String): Box[String] = {
  attrType match {
    case STRING => Full(value)
    case INTEGER => tryo(value.toInt).map(_ => value) ?~! InvalidNumber
    case DOUBLE => tryo(value.toDouble).map(_ => value) ?~! InvalidNumber
    case DATE_WITH_DAY => 
      if (value.matches("\\d{4}-\\d{2}-\\d{2}")) Full(value)
      else Failure(InvalidDateFormat)
  }
}
```

**Related Entities:**
- TransactionAttribute entity
- type field (cross-field dependency)

**User Story Context:**
The user story specifies "Value Format Validation: Value must be compatible with the declared attribute type (e.g., numeric string for INTEGER type)" in the Data Validations section.

**Dependencies:**
- VR-005: Type must be validated first

---

### Rule VR-007: JSON Request Body Format Validation

**Field/Entity:** Request Body

**Validation Type:** Format Validation

**Rule Description:**
The request body must be valid JSON conforming to the expected schema with required fields: name, type, and value.

**Validation Logic:**

- **Condition:** When any transaction attribute API endpoint receives a request body
- **Check:** Validate that the request body is valid JSON with required fields
- **Valid Criteria:** 
  - Request body is valid JSON
  - Contains "name" field (string)
  - Contains "type" field (string)
  - Contains "value" field (string)
- **Invalid Criteria:**
  - Request body is not valid JSON
  - Missing required fields
  - Fields have incorrect data types
- **Action on Success:** Proceed with field-level validations
- **Action on Failure:** Return error response with JSON format error

**Error Handling:**

- **Error Message:** `Incorrect json format.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.JSONFactory400`
- **Method/Function:** `extractTransactionAttributeJsonV400(json: JValue)`
- **Line Reference:** JSON extraction

**Code Snippet:**
```scala
case class TransactionAttributeJsonV400(
  name: String,
  `type`: String,
  value: String
)

def extractTransactionAttributeJsonV400(json: JValue): Box[TransactionAttributeJsonV400] = {
  tryo(json.extract[TransactionAttributeJsonV400]) ?~! InvalidJsonFormat
}
```

**Related Entities:**
- All transaction attribute endpoints
- Request body

**User Story Context:**
The user story specifies "JSON Format Validation: Request body must conform to the expected JSON schema" in the Data Validations section.

**Dependencies:**
- None (first validation for request body)

---

## Category: Business Constraint Validation

### Rule VR-008: User Entitlement Validation for Attribute Update

**Field/Entity:** User/Session

**Validation Type:** Authorization Validation

**Rule Description:**
Users must have the appropriate entitlement (canUpdateTransactionAttributeAtOneBank) to update transaction attributes.

**Validation Logic:**

- **Condition:** When PUT endpoint for updating transaction attributes is called
- **Check:** Validate that the authenticated user has the required entitlement for the specified bank
- **Valid Criteria:** 
  - User is authenticated with valid session
  - User has canUpdateTransactionAttributeAtOneBank entitlement for the specified bank
- **Invalid Criteria:**
  - User is not authenticated
  - User does not have the required entitlement
  - Entitlement is for a different bank
- **Action on Success:** Proceed with attribute update operation
- **Action on Failure:** Return authorization error

**Error Handling:**

- **Error Message:** `User does not have required entitlement: canUpdateTransactionAttributeAtOneBank`
- **Error Code:** `OBP-20001`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `hasEntitlement(bankId: String, userId: String, role: ApiRole)`
- **Line Reference:** Authorization check

**Code Snippet:**
```scala
for {
  _ <- NewStyle.function.hasEntitlement(bankId.value, u.userId, canUpdateTransactionAttributeAtOneBank, callContext)
} yield {
  // proceed with operation
}
```

**Related Entities:**
- User entity
- Entitlement entity
- ApiRole enumeration

**User Story Context:**
The user story states "The system shall enforce role-based access control requiring appropriate entitlements for attribute management" and specifies "Required Role: canUpdateTransactionAttributeAtOneBank" for the update endpoint.

**Dependencies:**
- User must be authenticated first

---

### Rule VR-009: User Entitlement Validation for Attribute Definition

**Field/Entity:** User/Session

**Validation Type:** Authorization Validation

**Rule Description:**
Users must have the appropriate entitlement (canCreateTransactionAttributeDefinitionAtOneBank) to create or update transaction attribute definitions.

**Validation Logic:**

- **Condition:** When PUT endpoint for attribute definitions is called
- **Check:** Validate that the authenticated user has the required entitlement for the specified bank
- **Valid Criteria:** 
  - User is authenticated with valid session
  - User has canCreateTransactionAttributeDefinitionAtOneBank entitlement for the specified bank
- **Invalid Criteria:**
  - User is not authenticated
  - User does not have the required entitlement
  - Entitlement is for a different bank
- **Action on Success:** Proceed with attribute definition operation
- **Action on Failure:** Return authorization error

**Error Handling:**

- **Error Message:** `User does not have required entitlement: canCreateTransactionAttributeDefinitionAtOneBank`
- **Error Code:** `OBP-20001`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `hasEntitlement(bankId: String, userId: String, role: ApiRole)`
- **Line Reference:** Authorization check

**Code Snippet:**
```scala
for {
  _ <- NewStyle.function.hasEntitlement(bankId.value, u.userId, canCreateTransactionAttributeDefinitionAtOneBank, callContext)
} yield {
  // proceed with definition operation
}
```

**Related Entities:**
- User entity
- Entitlement entity
- AttributeDefinition entity

**User Story Context:**
The user story specifies "Required Role: canCreateTransactionAttributeDefinitionAtOneBank" for the attribute definition endpoint.

**Dependencies:**
- User must be authenticated first

---

### Rule VR-010: Transaction Existence Prerequisite

**Field/Entity:** Transaction

**Validation Type:** Business Prerequisite Validation

**Rule Description:**
A transaction must exist before any attributes can be managed on it. This is a fundamental business constraint ensuring data integrity.

**Validation Logic:**

- **Condition:** Before any attribute create/update operation
- **Check:** Verify that the target transaction exists in the system
- **Valid Criteria:** 
  - Transaction with specified ID exists
  - Transaction is associated with the specified account and bank
- **Invalid Criteria:**
  - Transaction does not exist
  - Transaction exists but belongs to different account/bank
- **Action on Success:** Allow attribute management operations
- **Action on Failure:** Return resource not found error

**Error Handling:**

- **Error Message:** `Transaction not found. Please specify a valid value for TRANSACTION_ID.`
- **Error Code:** `OBP-30010`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `getTransaction(bankId, accountId, transactionId, callContext)`
- **Line Reference:** Transaction lookup

**Code Snippet:**
```scala
for {
  (transaction, callContext) <- NewStyle.function.getTransaction(bankId, accountId, transactionId, callContext)
  // Only proceed with attribute operations if transaction exists
} yield {
  // attribute operations
}
```

**Related Entities:**
- Transaction entity
- TransactionAttribute entity

**User Story Context:**
The user story explicitly states as a business rule: "Resource Existence: The transaction must exist before attributes can be managed on it."

**Dependencies:**
- VR-001, VR-002: Bank and Account must be validated first

---

## Category: Length/Boundary Validation

### Rule VR-011: Attribute Name Length Validation

**Field/Entity:** name (request body field)

**Validation Type:** Length Validation

**Rule Description:**
The attribute name must not exceed the maximum allowed length for string fields (typically 255 characters).

**Validation Logic:**

- **Condition:** When creating or updating a transaction attribute
- **Check:** Validate that the name field length is within acceptable bounds
- **Valid Criteria:** 
  - Name is non-empty
  - Name length <= 255 characters
- **Invalid Criteria:**
  - Name is empty or null
  - Name length > 255 characters
- **Action on Success:** Proceed with attribute operation
- **Action on Failure:** Return validation error

**Error Handling:**

- **Error Message:** `Value too long. Maximum length is 255 characters.`
- **Error Code:** `OBP-20010`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `checkMediumString(value: String)`
- **Line Reference:** String validation utility

**Code Snippet:**
```scala
def checkMediumString(value: String): String = {
  val valueLength = value.length
  val regex = """^([A-Za-z0-9\-._@]+)$""".r
  value match {
    case regex(e) if(valueLength <= 255) => SILENCE_IS_GOLDEN
    case regex(e) if(valueLength > 255) => ErrorMessages.InvalidValueLength
    case _ => ErrorMessages.InvalidValueCharacters
  }
}
```

**Related Entities:**
- TransactionAttribute entity
- name field

**User Story Context:**
While not explicitly stated in the user story, length validation is a standard practice for string fields to ensure database compatibility and prevent overflow errors.

**Dependencies:**
- VR-007: JSON format validation must pass first

---

## Category: Cross-Field Validation

### Rule VR-012: Bank Scope Validation

**Field/Entity:** BANK_ID, ACCOUNT_ID, TRANSACTION_ID (path parameters)

**Validation Type:** Cross-Field Relationship Validation

**Rule Description:**
All resources (account, transaction, attribute) must belong to the same bank scope. This ensures data isolation between banks.

**Validation Logic:**

- **Condition:** When any transaction attribute operation is performed
- **Check:** Validate that the account belongs to the specified bank, and the transaction belongs to the specified account
- **Valid Criteria:** 
  - Account.bankId == BANK_ID
  - Transaction.accountId == ACCOUNT_ID
  - Transaction.bankId == BANK_ID
- **Invalid Criteria:**
  - Account belongs to a different bank
  - Transaction belongs to a different account or bank
- **Action on Success:** Proceed with attribute operation
- **Action on Failure:** Return resource not found or access denied error

**Error Handling:**

- **Error Message:** `Resource not found or access denied. The requested resource does not belong to the specified bank.`
- **Error Code:** `OBP-30001`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** Resource lookup methods with bank scope filtering
- **Line Reference:** Multiple validation points

**Code Snippet:**
```scala
for {
  (bank, callContext) <- NewStyle.function.getBank(bankId, callContext)
  (account, callContext) <- NewStyle.function.checkAccountAccess(bankId, accountId, callContext)
  (transaction, callContext) <- NewStyle.function.getTransaction(bankId, accountId, transactionId, callContext)
  // All resources validated within bank scope
} yield {
  // proceed with operation
}
```

**Related Entities:**
- Bank entity
- BankAccount entity
- Transaction entity
- TransactionAttribute entity

**User Story Context:**
The user story states as a business rule: "Bank Scope: Attributes are scoped to a specific bank and must be managed within that bank's context."

**Dependencies:**
- VR-001, VR-002, VR-003: Individual resource validations

---

## Validation Flow Summary

The validation rules are applied in the following order for transaction attribute operations:

1. **Authentication Check** - User must have valid session
2. **VR-007** - JSON Request Body Format Validation
3. **VR-001** - Bank ID Required Validation
4. **VR-002** - Account ID Required Validation
5. **VR-003** - Transaction ID Required Validation
6. **VR-010** - Transaction Existence Prerequisite
7. **VR-004** - Attribute ID Required Validation (for updates only)
8. **VR-008/VR-009** - User Entitlement Validation
9. **VR-012** - Bank Scope Validation
10. **VR-011** - Attribute Name Length Validation
11. **VR-005** - Attribute Type Enumeration Validation
12. **VR-006** - Attribute Value Type Compatibility Validation

---

## Error Code Reference

| Error Code | HTTP Status | Description |
|------------|-------------|-------------|
| OBP-10001 | 400 | Invalid JSON format or invalid type |
| OBP-10002 | 400 | Invalid number format |
| OBP-10005 | 400 | Invalid date format |
| OBP-20001 | 403 | Missing required entitlement |
| OBP-20010 | 400 | Value too long |
| OBP-30001 | 404 | Bank not found |
| OBP-30010 | 404 | Transaction not found |
| OBP-30018 | 404 | Account not found |
| OBP-30024 | 404 | Transaction Attribute not found |

---

## Notes

- All validations follow the Box pattern (Full/Empty/Failure) for consistent error handling
- Error messages are centralized in the ErrorMessages object for maintainability
- Cross-field validations ensure data integrity across related entities
- Authorization validations are bank-scoped for multi-tenant support
- The validation order is designed to fail fast on common errors (authentication, JSON format) before performing expensive database lookups
