# Validation Rules

**Extracted From:** OBP-API Scala Application  
**User Story:** Account Attribute Management  
**Analysis Date:** 2026-01-20  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 10

### Validation Categories
- Input Validation Rules: 4
- Format Validation Rules: 1
- Business Constraint Rules: 3
- Length/Boundary Rules: 0
- Cross-Field Validation Rules: 2

---

## Category: Input Validation

### Rule VR-001: Attribute Name Required Validation

**Field/Entity:** name

**Validation Type:** Required Field Validation

**Rule Description:**
The attribute name field is required and must be provided when creating or updating an account attribute.

**Validation Logic:**

- **Condition:** When creating or updating an account attribute via POST or PUT request
- **Check:** Validate that the `name` field is present in the request body
- **Valid Criteria:** 
  - The `name` field is present and non-empty
  - Contains a valid string value (e.g., "ISIN", "LOAN_ID", "MATURITY_DATE")
- **Invalid Criteria:**
  - The `name` field is missing from the request
  - The `name` field is empty or null
- **Action on Success:** Proceed with attribute creation/update
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v3_1_0.APIMethods310`
- **Method/Function:** JSON extraction and validation
- **Line Reference:** Request body parsing

**Code Snippet:**
```scala
case class AccountAttributeJson(
  name: String,
  `type`: String,
  value: String,
  product_instance_code: Option[String]
)
```

**Related Entities:**
- AccountAttribute (name field)
- AccountAttributeJson (request body)

**User Story Context:**
Each attribute must have a name to identify the type of metadata being stored (e.g., ISIN codes, loan identifiers, maturity dates).

**Dependencies:**
- None (standalone validation)

---

### Rule VR-002: Attribute Type Required Validation

**Field/Entity:** type

**Validation Type:** Required Field Validation

**Rule Description:**
The attribute type field is required and must be provided when creating or updating an account attribute.

**Validation Logic:**

- **Condition:** When creating or updating an account attribute via POST or PUT request
- **Check:** Validate that the `type` field is present in the request body
- **Valid Criteria:** 
  - The `type` field is present and non-empty
  - Contains a valid string value
- **Invalid Criteria:**
  - The `type` field is missing from the request
  - The `type` field is empty or null
- **Action on Success:** Proceed with type enum validation (VR-003)
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v3_1_0.APIMethods310`
- **Method/Function:** JSON extraction and validation
- **Line Reference:** Request body parsing

**Code Snippet:**
```scala
case class AccountAttributeJson(
  name: String,
  `type`: String,
  value: String,
  product_instance_code: Option[String]
)
```

**Related Entities:**
- AccountAttribute (type field)
- AccountAttributeJson (request body)

**User Story Context:**
Each attribute must have a type to define the data type of the value being stored, enabling proper validation and processing.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-003: Attribute Value Required Validation

**Field/Entity:** value

**Validation Type:** Required Field Validation

**Rule Description:**
The attribute value field is required and must be provided when creating or updating an account attribute.

**Validation Logic:**

- **Condition:** When creating or updating an account attribute via POST or PUT request
- **Check:** Validate that the `value` field is present in the request body
- **Valid Criteria:** 
  - The `value` field is present and non-empty
  - Contains a valid string value
- **Invalid Criteria:**
  - The `value` field is missing from the request
  - The `value` field is empty or null
- **Action on Success:** Proceed with attribute creation/update
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v3_1_0.APIMethods310`
- **Method/Function:** JSON extraction and validation
- **Line Reference:** Request body parsing

**Code Snippet:**
```scala
case class AccountAttributeJson(
  name: String,
  `type`: String,
  value: String,
  product_instance_code: Option[String]
)
```

**Related Entities:**
- AccountAttribute (value field)
- AccountAttributeJson (request body)

**User Story Context:**
Each attribute must have a value to store the actual metadata content for the account.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-004: Product Instance Code Optional Validation

**Field/Entity:** product_instance_code

**Validation Type:** Optional Field Validation

**Rule Description:**
The product_instance_code field is optional and can be omitted when creating or updating an account attribute.

**Validation Logic:**

- **Condition:** When creating or updating an account attribute via POST or PUT request
- **Check:** If provided, validate that the `product_instance_code` field contains a valid string
- **Valid Criteria:** 
  - The `product_instance_code` field is absent (None)
  - The `product_instance_code` field contains a valid string value
- **Invalid Criteria:**
  - N/A (field is optional)
- **Action on Success:** Include product_instance_code in attribute if provided
- **Action on Failure:** N/A

**Error Handling:**

- **Error Message:** N/A (optional field)
- **Error Code:** N/A
- **HTTP Status Code:** N/A

**Scala Implementation:**

- **Location:** `code.api.v3_1_0.APIMethods310`
- **Method/Function:** JSON extraction
- **Line Reference:** Request body parsing

**Code Snippet:**
```scala
case class AccountAttributeJson(
  name: String,
  `type`: String,
  value: String,
  product_instance_code: Option[String]
)
```

**Related Entities:**
- AccountAttribute (product_instance_code field)
- AccountAttributeJson (request body)

**User Story Context:**
Optionally, a product_instance_code can be associated with the attribute to link it to a specific product instance.

**Dependencies:**
- None (standalone validation)

---

## Category: Format Validation

### Rule VR-005: Attribute Type Enum Validation

**Field/Entity:** type

**Validation Type:** Enum/Format Validation

**Rule Description:**
The attribute type must be one of the predefined valid types: STRING, INTEGER, DOUBLE, or DATE_WITH_DAY.

**Validation Logic:**

- **Condition:** When creating or updating an account attribute via POST or PUT request
- **Check:** Validate that the `type` field contains a valid `AccountAttributeType` enum value
- **Valid Criteria:** 
  - `type` = "STRING" - For text values (e.g., "TAX_NUMBER")
  - `type` = "INTEGER" - For whole number values (e.g., "123")
  - `type` = "DOUBLE" - For decimal values (e.g., "2012.04")
  - `type` = "DATE_WITH_DAY" - For date values (e.g., "2012-04-23")
- **Invalid Criteria:**
  - `type` contains any value other than STRING, INTEGER, DOUBLE, or DATE_WITH_DAY
  - `type` is case-sensitive mismatch (e.g., "string" instead of "STRING")
- **Action on Success:** Proceed with attribute creation/update
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-30024: Invalid Account Attribute Type.`
- **Error Code:** `OBP-30024`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle.function`
- **Method/Function:** `isValidAccountAttributeType`
- **Line Reference:** Type validation logic

**Code Snippet:**
```scala
object AccountAttributeType extends Enumeration {
  type AccountAttributeType = Value
  val STRING = Value("STRING")
  val INTEGER = Value("INTEGER")
  val DOUBLE = Value("DOUBLE")
  val DATE_WITH_DAY = Value("DATE_WITH_DAY")
}
```

**Related Entities:**
- AccountAttribute (type field)
- AccountAttributeType (enum)

**User Story Context:**
The attribute type must be one of the following valid types to ensure proper data handling and validation: STRING for text values, INTEGER for whole numbers, DOUBLE for decimal values, and DATE_WITH_DAY for date values.

**Dependencies:**
- VR-002: Attribute Type Required Validation

---

## Category: Business Constraint Validation

### Rule VR-006: Bank Existence Validation

**Field/Entity:** BANK_ID (path parameter)

**Validation Type:** Entity Existence Validation

**Rule Description:**
The bank specified by BANK_ID must exist in the system before creating or updating an account attribute.

**Validation Logic:**

- **Condition:** When creating or updating an account attribute via POST or PUT request
- **Check:** Validate that the bank with the specified BANK_ID exists in the system
- **Valid Criteria:** 
  - Bank with BANK_ID exists in the database
  - Bank is active and accessible
- **Invalid Criteria:**
  - No bank found with the specified BANK_ID
  - Bank has been deleted or deactivated
- **Action on Success:** Proceed with account validation (VR-007)
- **Action on Failure:** Return error response with 404 status code

**Error Handling:**

- **Error Message:** `OBP-30001: Bank not found. Please specify a valid value for BANK_ID.`
- **Error Code:** `OBP-30001`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle.function`
- **Method/Function:** `getBank`
- **Line Reference:** Bank lookup logic

**Code Snippet:**
```scala
for {
  (bank, callContext) <- NewStyle.function.getBank(bankId, callContext)
  // ... rest of validation chain
} yield {
  // success case
}
```

**Related Entities:**
- Bank (entity)
- BankId (identifier)

**User Story Context:**
The system shall verify that the bank exists before creating/updating attributes to ensure data integrity and proper association.

**Dependencies:**
- None (first entity validation in chain)

---

### Rule VR-007: Account Existence Validation

**Field/Entity:** ACCOUNT_ID (path parameter)

**Validation Type:** Entity Existence Validation

**Rule Description:**
The account specified by ACCOUNT_ID must exist within the specified bank before creating or updating an account attribute.

**Validation Logic:**

- **Condition:** When creating or updating an account attribute via POST or PUT request, after bank validation passes
- **Check:** Validate that the account with the specified ACCOUNT_ID exists within the bank
- **Valid Criteria:** 
  - Account with ACCOUNT_ID exists in the database
  - Account belongs to the specified bank (BANK_ID)
  - Account is active and accessible
- **Invalid Criteria:**
  - No account found with the specified ACCOUNT_ID
  - Account exists but belongs to a different bank
  - Account has been deleted or closed
- **Action on Success:** Proceed with product validation (VR-008)
- **Action on Failure:** Return error response with 404 status code

**Error Handling:**

- **Error Message:** `OBP-30018: Bank Account not found. Please specify valid values for BANK_ID and ACCOUNT_ID.`
- **Error Code:** `OBP-30018`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle.function`
- **Method/Function:** `checkBankAccountExists`
- **Line Reference:** Account lookup logic

**Code Snippet:**
```scala
for {
  (bank, callContext) <- NewStyle.function.getBank(bankId, callContext)
  (account, callContext) <- NewStyle.function.checkBankAccountExists(bankId, accountId, callContext)
  // ... rest of validation chain
} yield {
  // success case
}
```

**Related Entities:**
- BankAccount (entity)
- AccountId (identifier)
- Bank (parent entity)

**User Story Context:**
Account attributes must be linked to a specific account via ACCOUNT_ID. The system shall verify that the account exists within the specified bank before creating/updating attributes.

**Dependencies:**
- VR-006: Bank Existence Validation

---

### Rule VR-008: Product Existence Validation

**Field/Entity:** PRODUCT_CODE (path parameter)

**Validation Type:** Entity Existence Validation

**Rule Description:**
The product specified by PRODUCT_CODE must exist within the specified bank before creating or updating an account attribute.

**Validation Logic:**

- **Condition:** When creating or updating an account attribute via POST or PUT request, after account validation passes
- **Check:** Validate that the product with the specified PRODUCT_CODE exists within the bank
- **Valid Criteria:** 
  - Product with PRODUCT_CODE exists in the database
  - Product belongs to the specified bank (BANK_ID)
  - Product is active and accessible
- **Invalid Criteria:**
  - No product found with the specified PRODUCT_CODE
  - Product exists but belongs to a different bank
  - Product has been deleted or deactivated
- **Action on Success:** Proceed with attribute creation/update
- **Action on Failure:** Return error response with 404 status code

**Error Handling:**

- **Error Message:** `OBP-30301: Product not found. Please specify a valid value for PRODUCT_CODE.`
- **Error Code:** `OBP-30301`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle.function`
- **Method/Function:** `getProduct`
- **Line Reference:** Product lookup logic

**Code Snippet:**
```scala
for {
  (bank, callContext) <- NewStyle.function.getBank(bankId, callContext)
  (account, callContext) <- NewStyle.function.checkBankAccountExists(bankId, accountId, callContext)
  (product, callContext) <- NewStyle.function.getProduct(bankId, productCode, callContext)
  // ... rest of validation chain
} yield {
  // success case
}
```

**Related Entities:**
- Product (entity)
- ProductCode (identifier)
- Bank (parent entity)

**User Story Context:**
Account attributes must be associated with a product via PRODUCT_CODE. The system shall verify that the product exists within the specified bank before creating/updating attributes.

**Dependencies:**
- VR-006: Bank Existence Validation
- VR-007: Account Existence Validation

---

## Category: Cross-Field Validation

### Rule VR-009: Attribute Existence Validation (Update Only)

**Field/Entity:** ACCOUNT_ATTRIBUTE_ID (path parameter)

**Validation Type:** Entity Existence Validation (Cross-Field)

**Rule Description:**
When updating an account attribute, the attribute with the specified ACCOUNT_ATTRIBUTE_ID must exist before the update can proceed.

**Validation Logic:**

- **Condition:** When updating an account attribute via PUT request
- **Check:** Validate that the attribute with the specified ACCOUNT_ATTRIBUTE_ID exists
- **Valid Criteria:** 
  - Attribute with ACCOUNT_ATTRIBUTE_ID exists in the database
  - Attribute belongs to the specified account and bank
- **Invalid Criteria:**
  - No attribute found with the specified ACCOUNT_ATTRIBUTE_ID
  - Attribute exists but belongs to a different account or bank
- **Action on Success:** Proceed with attribute update
- **Action on Failure:** Return error response with 404 status code

**Error Handling:**

- **Error Message:** `OBP-30310: Account Attribute not found. Please specify a valid value for ACCOUNT_ATTRIBUTE_ID.`
- **Error Code:** `OBP-30310`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle.function`
- **Method/Function:** `getAccountAttributeById`
- **Line Reference:** Attribute lookup logic

**Code Snippet:**
```scala
for {
  // ... bank, account, product validations
  (accountAttribute, callContext) <- NewStyle.function.getAccountAttributeById(accountAttributeId, callContext)
  // ... update logic
} yield {
  // success case
}
```

**Related Entities:**
- AccountAttribute (entity)
- AccountAttributeId (identifier)
- BankAccount (parent entity)

**User Story Context:**
The attribute with the specified ACCOUNT_ATTRIBUTE_ID must exist before updating to ensure the update operation targets a valid existing attribute.

**Dependencies:**
- VR-006: Bank Existence Validation
- VR-007: Account Existence Validation
- VR-008: Product Existence Validation

---

### Rule VR-010: Authorization/Entitlement Validation

**Field/Entity:** User Entitlements

**Validation Type:** Authorization Validation (Cross-Field)

**Rule Description:**
Users must have the appropriate entitlements to perform account attribute management operations. Creating attributes requires `canCreateAccountAttributeAtOneBank` entitlement, and updating attributes requires `canUpdateAccountAttribute` entitlement.

**Validation Logic:**

- **Condition:** When creating or updating an account attribute via POST or PUT request
- **Check:** Validate that the authenticated user has the required entitlement for the operation
- **Valid Criteria:** 
  - For POST (create): User has `canCreateAccountAttributeAtOneBank` entitlement for the specified bank
  - For PUT (update): User has `canUpdateAccountAttribute` entitlement
  - User is authenticated and logged in
- **Invalid Criteria:**
  - User is not authenticated
  - User does not have the required entitlement for the operation
  - User's entitlement is for a different bank (for bank-specific entitlements)
- **Action on Success:** Proceed with attribute creation/update
- **Action on Failure:** Return error response with 403 status code

**Error Handling:**

- **Error Message (Create):** `OBP-20006: User is missing one or more roles: canCreateAccountAttributeAtOneBank`
- **Error Message (Update):** `OBP-20006: User is missing one or more roles: canUpdateAccountAttribute`
- **Error Code:** `OBP-20006`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.api.v3_1_0.APIMethods310`
- **Method/Function:** `hasEntitlement` check
- **Line Reference:** Entitlement validation in endpoint definition

**Code Snippet:**
```scala
// For Create
lazy val createAccountAttribute: OBPEndpoint = {
  case "banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "products" :: ProductCode(productCode) :: "attribute" :: Nil JsonPost json -> _ => {
    cc =>
      for {
        (Full(u), callContext) <- authenticatedAccess(cc)
        _ <- NewStyle.function.hasEntitlement(bankId.value, u.userId, canCreateAccountAttributeAtOneBank, callContext)
        // ... rest of logic
      } yield {
        // success case
      }
  }
}

// For Update
lazy val updateAccountAttribute: OBPEndpoint = {
  case "banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "products" :: ProductCode(productCode) :: "attributes" :: AccountAttributeId(accountAttributeId) :: Nil JsonPut json -> _ => {
    cc =>
      for {
        (Full(u), callContext) <- authenticatedAccess(cc)
        _ <- NewStyle.function.hasEntitlement(bankId.value, u.userId, canUpdateAccountAttribute, callContext)
        // ... rest of logic
      } yield {
        // success case
      }
  }
}
```

**Related Entities:**
- User (entity)
- Entitlement (entity)
- Bank (scope entity)

**User Story Context:**
The system shall enforce proper authorization (entitlements) for attribute management operations. Creating attributes requires `canCreateAccountAttributeAtOneBank` entitlement and updating attributes requires `canUpdateAccountAttribute` entitlement.

**Dependencies:**
- User must be authenticated (logged in)

---

## Quality Checklist

- [x] All validation functions in relevant code are documented
- [x] All error messages are captured with exact text
- [x] All error codes are documented
- [x] Required vs. optional fields are clearly marked
- [x] Cross-field validations are identified
- [x] Business constraint validations are included
- [x] Code references include file paths
- [x] User story context is explained for each rule
- [x] Valid and invalid criteria are clearly stated
- [x] HTTP status codes are documented where applicable
- [x] Dependencies between validations are noted

---

## Notes

- The validation rules are extracted based on the user story for Account Attribute Management capability
- The capability scope includes only CREATE and UPDATE operations as per the "manage" verb interpretation
- GET, DELETE, and LIST operations are not included in this capability scope
- Additional attribute-specific validations (e.g., value format based on type) may be implemented at the application level but are not explicitly documented in the user story
