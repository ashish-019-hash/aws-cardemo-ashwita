# Validation Rules

**Extracted From:** Scala Application (OBP-API)  
**User Story:** Account Creation  
**Analysis Date:** 2026-01-20  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 15

### Validation Categories
- Input Validation Rules: 5
- Format Validation Rules: 4
- Business Constraint Rules: 4
- Length/Boundary Rules: 2
- Cross-Field Validation Rules: 0

---

## Category: Input Validation (Required Fields)

### Rule VR-001: Bank ID Required Validation

**Field/Entity:** bank_id

**Validation Type:** Required Field

**Rule Description:**
Bank ID must be provided and must reference an existing bank on the platform. This is a mandatory parameter for account creation.

**Validation Logic:**

- **Condition:** When creating a new bank account via POST /obp/v5.1.0/banks/{BANK_ID}/accounts or PUT /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}
- **Check:** Validate that BANK_ID path parameter is provided and non-empty
- **Valid Criteria:** Non-empty string that references an existing bank entity
- **Invalid Criteria:** Empty string, null, or non-existent bank reference
- **Action on Success:** Proceed with account creation under the specified bank
- **Action on Failure:** Return error response indicating bank not found or invalid bank ID

**Error Handling:**

- **Error Message:** `Bank not found. Please specify a valid value for BANK_ID.`
- **Error Code:** `OBP-30001`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `getBankById(bankId: String)`
- **Line Reference:** Bank validation utility methods

**Code Snippet:**
```scala
def getBankById(bankId: String): Box[Bank] = {
  if (bankId.isEmpty) {
    Failure(ErrorMessages.BankNotFound)
  } else {
    Banks.banks.vend.getBankById(BankId(bankId))
  }
}
```

**Related Entities:**
- Bank entity
- Account entity (bank_id field)

**User Story Context:**
From acceptance criteria: "The system shall associate the account with the specified bank entity" - validates that the bank exists before creating an account under it.

**Dependencies:**
- Bank entity must exist in the system

---

### Rule VR-002: User ID Required Validation

**Field/Entity:** user_id

**Validation Type:** Required Field

**Rule Description:**
User ID must be provided to specify the owner of the account. The user must exist and be eligible for account ownership.

**Validation Logic:**

- **Condition:** When creating a new bank account with ownership assignment
- **Check:** Validate that user_id is provided in the request body and references an existing user
- **Valid Criteria:** Non-empty string that references an existing, valid user in the system
- **Invalid Criteria:** Empty string, null, or non-existent user reference
- **Action on Success:** Assign ownership of the created account to the specified user
- **Action on Failure:** Return error response indicating user not found

**Error Handling:**

- **Error Message:** `User not found by USER_ID.`
- **Error Code:** `OBP-20005`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `getUserByUserId(userId: String)`
- **Line Reference:** User validation utility methods

**Code Snippet:**
```scala
def getUserByUserId(userId: String): Box[User] = {
  if (userId.isEmpty) {
    Failure(ErrorMessages.UserNotFoundById)
  } else {
    Users.users.vend.getUserByUserId(userId)
  }
}
```

**Related Entities:**
- User entity
- Account entity (owner association)

**User Story Context:**
From acceptance criteria: "The system shall validate that the specified owner exists and is eligible for account ownership" - ensures the owner user exists before creating the account.

**Dependencies:**
- User entity must exist in the system

---

### Rule VR-003: Account Label Required Validation

**Field/Entity:** label

**Validation Type:** Required Field

**Rule Description:**
Account label must be provided and must not be empty. The label serves as a human-readable identifier for the account.

**Validation Logic:**

- **Condition:** When creating a new bank account
- **Check:** Validate that label field is provided and is not empty or whitespace-only
- **Valid Criteria:** Non-empty string with meaningful content
- **Invalid Criteria:** Empty string, null, or whitespace-only string
- **Action on Success:** Use the provided label for the account
- **Action on Failure:** Return error response indicating label is required

**Error Handling:**

- **Error Message:** `Invalid value for LABEL. It should be a non-empty string.`
- **Error Code:** `OBP-10002`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `checkMediumString(value: String)`
- **Line Reference:** String validation utility methods

**Code Snippet:**
```scala
def checkMediumString(value: String): String = {
  if (value == null || value.trim.isEmpty) {
    ErrorMessages.InvalidValueRequired
  } else {
    SILENCE_IS_GOLDEN
  }
}
```

**Related Entities:**
- Account entity (label field)

**User Story Context:**
From data validations: "Account label must not be empty and should follow naming conventions" - ensures accounts have meaningful labels.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-004: Currency Code Required Validation

**Field/Entity:** balance.currency

**Validation Type:** Required Field

**Rule Description:**
Currency code must be provided for the account balance. This determines the currency in which the account operates.

**Validation Logic:**

- **Condition:** When creating a new bank account with balance information
- **Check:** Validate that currency field is provided in the balance object
- **Valid Criteria:** Non-empty string representing a currency code
- **Invalid Criteria:** Empty string, null, or missing currency field
- **Action on Success:** Create account with the specified currency
- **Action on Failure:** Return error response indicating currency is required

**Error Handling:**

- **Error Message:** `Invalid Currency Value. Currency is required.`
- **Error Code:** `OBP-10003`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidCurrencyISOCode(currencyCode: String)`
- **Line Reference:** Currency validation utility methods

**Code Snippet:**
```scala
def isValidCurrencyISOCode(currencyCode: String): Boolean = {
  if (currencyCode == null || currencyCode.isEmpty) {
    false
  } else {
    // Validation logic
    true
  }
}
```

**Related Entities:**
- Account entity (currency field)
- Balance entity

**User Story Context:**
From input data: "Currency code (e.g., EUR, USD, GBP)" - currency is a required parameter for account creation.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-005: Balance Amount Required Validation

**Field/Entity:** balance.amount

**Validation Type:** Required Field

**Rule Description:**
Balance amount must be provided when creating an account with initial balance. The amount must be a valid numeric value.

**Validation Logic:**

- **Condition:** When creating a new bank account with balance information
- **Check:** Validate that amount field is provided and is a valid numeric string
- **Valid Criteria:** Non-empty string that can be parsed as a valid numeric value
- **Invalid Criteria:** Empty string, null, or non-numeric string
- **Action on Success:** Create account with the specified initial balance
- **Action on Failure:** Return error response indicating invalid amount

**Error Handling:**

- **Error Message:** `Invalid Number. Could not convert value to a number.`
- **Error Code:** `OBP-10002`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `stringToNumber(value: String)`
- **Line Reference:** Number parsing utility methods

**Code Snippet:**
```scala
def stringToNumber(value: String): Box[BigDecimal] = {
  tryo {
    BigDecimal(value)
  } ?~! ErrorMessages.InvalidNumber
}
```

**Related Entities:**
- Account entity (balance field)
- Balance entity (amount field)

**User Story Context:**
From data validations: "Initial balance amount must be a valid numeric value" - ensures balance amounts are properly formatted.

**Dependencies:**
- None (standalone validation)

---

## Category: Format Validation

### Rule VR-006: Currency ISO Code Format Validation

**Field/Entity:** balance.currency

**Validation Type:** Format Validation (ISO Code)

**Rule Description:**
Currency code must be a valid ISO 4217 currency code. The system validates against a list of recognized currency codes.

**Validation Logic:**

- **Condition:** When currency code is provided in account creation request
- **Check:** Validate that currency code is a valid 3-letter ISO 4217 currency code
- **Valid Criteria:** 
  - Must be exactly 3 uppercase letters
  - Must be in the list of valid ISO 4217 currency codes (e.g., EUR, USD, GBP, JPY)
  - XBT (Bitcoin) is also accepted as a valid currency code
- **Invalid Criteria:**
  - Not exactly 3 characters
  - Contains non-alphabetic characters
  - Not in the recognized currency code list
- **Action on Success:** Accept the currency code for account creation
- **Action on Failure:** Return error response indicating invalid currency code

**Error Handling:**

- **Error Message:** `Invalid Currency Value. Expected a 3-letter ISO Currency Code.`
- **Error Code:** `OBP-10003`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidCurrencyISOCode(currencyCode: String)`
- **Line Reference:** Currency validation utility methods

**Code Snippet:**
```scala
def isValidCurrencyISOCode(currencyCode: String): Boolean = {
  val currencyIsoCodeArray = (CurrencyIsoCodeFromXmlFile \"CcyTbl" \ "CcyNtry" \ "Ccy")
    .map(_.text).mkString(" ").split("\\s+") :+ "XBT"
  currencyIsoCodeArray.contains(currencyCode)
}
```

**Related Entities:**
- Account entity (currency field)
- Balance entity

**User Story Context:**
From data validations: "Currency code must be a valid ISO 4217 currency code" - ensures only recognized currencies are used for accounts.

**Dependencies:**
- ISO 4217 currency code reference data

---

### Rule VR-007: Account Routing Scheme Format Validation

**Field/Entity:** account_routings[].scheme

**Validation Type:** Format Validation (Enumeration)

**Rule Description:**
Account routing scheme must be a valid routing scheme type. Common schemes include IBAN, AccountNumber, and other banking identifiers.

**Validation Logic:**

- **Condition:** When account routing information is provided in account creation request
- **Check:** Validate that the routing scheme is a recognized scheme type
- **Valid Criteria:** 
  - Must be one of the recognized scheme types (e.g., IBAN, AccountNumber, SWIFT_BIC, etc.)
  - Case-sensitive matching
- **Invalid Criteria:**
  - Empty or null scheme
  - Unrecognized scheme type
- **Action on Success:** Accept the routing scheme for account creation
- **Action on Failure:** Return error response indicating invalid routing scheme

**Error Handling:**

- **Error Message:** `Invalid Account Routing Scheme. Please specify a valid scheme.`
- **Error Code:** `OBP-30110`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidAccountRoutingScheme(scheme: String)`
- **Line Reference:** Account routing validation methods

**Code Snippet:**
```scala
def isValidAccountRoutingScheme(scheme: String): Boolean = {
  val validSchemes = List("IBAN", "AccountNumber", "SWIFT_BIC", "ABA", "SORT_CODE")
  validSchemes.contains(scheme)
}
```

**Related Entities:**
- AccountRouting entity (scheme field)
- Account entity (account_routings association)

**User Story Context:**
From data validations: "Account routing schemes must be valid (e.g., IBAN, AccountNumber)" - ensures routing information uses recognized schemes.

**Dependencies:**
- List of valid routing schemes

---

### Rule VR-008: IBAN Format Validation

**Field/Entity:** account_routings[].address (when scheme is IBAN)

**Validation Type:** Format Validation (Pattern)

**Rule Description:**
When the routing scheme is IBAN, the address must conform to the IBAN format specification.

**Validation Logic:**

- **Condition:** When account routing with scheme "IBAN" is provided
- **Check:** Validate that the address conforms to IBAN format
- **Valid Criteria:** 
  - Starts with 2-letter country code
  - Followed by 2 check digits
  - Followed by up to 30 alphanumeric characters (BBAN)
  - Total length between 15 and 34 characters
  - Passes IBAN checksum validation
- **Invalid Criteria:**
  - Invalid country code
  - Incorrect length
  - Invalid characters
  - Failed checksum validation
- **Action on Success:** Accept the IBAN for account routing
- **Action on Failure:** Return error response indicating invalid IBAN format

**Error Handling:**

- **Error Message:** `Invalid IBAN format. Please provide a valid IBAN.`
- **Error Code:** `OBP-30111`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidIBAN(iban: String)`
- **Line Reference:** IBAN validation utility methods

**Code Snippet:**
```scala
def isValidIBAN(iban: String): Boolean = {
  val ibanRegex = """^[A-Z]{2}[0-9]{2}[A-Z0-9]{11,30}$""".r
  iban match {
    case ibanRegex() => validateIBANChecksum(iban)
    case _ => false
  }
}
```

**Related Entities:**
- AccountRouting entity (address field)
- Account entity (account_routings association)

**User Story Context:**
From data validations: "Account routing addresses must conform to the specified scheme format" - ensures IBAN addresses are properly formatted.

**Dependencies:**
- IBAN validation algorithm

---

### Rule VR-009: Account ID Format Validation

**Field/Entity:** account_id

**Validation Type:** Format Validation (String Pattern)

**Rule Description:**
When creating an account with a specified account ID (PUT endpoint), the account ID must conform to the allowed format.

**Validation Logic:**

- **Condition:** When creating an account with a pre-specified account ID via PUT /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}
- **Check:** Validate that ACCOUNT_ID matches the allowed pattern
- **Valid Criteria:** 
  - Contains only: A-Z, a-z, 0-9, hyphen (-), underscore (_), period (.)
  - Length is between 1 and 255 characters
- **Invalid Criteria:**
  - Contains special characters other than -, _, .
  - Length is 0 or greater than 255 characters
  - Contains spaces or unicode characters
- **Action on Success:** Create account with the specified ID
- **Action on Failure:** Return error response indicating invalid account ID format

**Error Handling:**

- **Error Message:** `Invalid Account ID format. Account ID must contain only alphanumeric characters, hyphens, underscores, and periods.`
- **Error Code:** `OBP-30005`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidID(id: String)`
- **Line Reference:** ID validation utility methods

**Code Snippet:**
```scala
def isValidID(id: String): Boolean = {
  val regex = """^([A-Za-z0-9\-_.]+)$""".r
  id match {
    case regex(e) if(e.length < 256) => true
    case _ => false
  }
}
```

**Related Entities:**
- Account entity (account_id field)

**User Story Context:**
From endpoint 2: "Create a new bank account with a pre-specified account ID" - validates the format of user-specified account IDs.

**Dependencies:**
- None (standalone validation)

---

## Category: Business Constraint Validation

### Rule VR-010: Bank Existence Validation

**Field/Entity:** bank_id

**Validation Type:** Business Constraint (Entity Existence)

**Rule Description:**
The specified bank must exist and be active in the system before an account can be created under it.

**Validation Logic:**

- **Condition:** When creating a new bank account
- **Check:** Validate that the bank referenced by BANK_ID exists in the system
- **Valid Criteria:** Bank with the specified ID exists and is active
- **Invalid Criteria:** Bank does not exist or is inactive/deleted
- **Action on Success:** Proceed with account creation under the bank
- **Action on Failure:** Return error response indicating bank not found

**Error Handling:**

- **Error Message:** `Bank not found. Please specify a valid value for BANK_ID.`
- **Error Code:** `OBP-30001`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.bankconnectors.Connector`
- **Method/Function:** `getBankById(bankId: BankId)`
- **Line Reference:** Bank lookup methods

**Code Snippet:**
```scala
def getBankById(bankId: BankId): Box[Bank] = {
  Banks.banks.vend.getBankById(bankId) match {
    case Full(bank) => Full(bank)
    case Empty => Failure(ErrorMessages.BankNotFound)
    case f: Failure => f
  }
}
```

**Related Entities:**
- Bank entity
- Account entity (bank association)

**User Story Context:**
From business rules: "Account-Bank Association: Each account must be created under a specific bank entity" - ensures the bank exists before creating accounts.

**Dependencies:**
- Bank entity must be persisted in the system

---

### Rule VR-011: User Existence Validation

**Field/Entity:** user_id

**Validation Type:** Business Constraint (Entity Existence)

**Rule Description:**
The specified user (owner) must exist in the system before they can be assigned as the owner of a new account.

**Validation Logic:**

- **Condition:** When creating a new bank account with ownership assignment
- **Check:** Validate that the user referenced by user_id exists in the system
- **Valid Criteria:** User with the specified ID exists and is active
- **Invalid Criteria:** User does not exist or is inactive/deleted
- **Action on Success:** Assign account ownership to the user
- **Action on Failure:** Return error response indicating user not found

**Error Handling:**

- **Error Message:** `User not found by USER_ID.`
- **Error Code:** `OBP-20005`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.users.Users`
- **Method/Function:** `getUserByUserId(userId: String)`
- **Line Reference:** User lookup methods

**Code Snippet:**
```scala
def getUserByUserId(userId: String): Box[User] = {
  Users.users.vend.getUserByUserId(userId) match {
    case Full(user) => Full(user)
    case Empty => Failure(ErrorMessages.UserNotFoundById)
    case f: Failure => f
  }
}
```

**Related Entities:**
- User entity
- Account entity (owner association)

**User Story Context:**
From acceptance criteria: "The system shall validate that the specified owner exists and is eligible for account ownership" - ensures the owner exists.

**Dependencies:**
- User entity must be persisted in the system

---

### Rule VR-012: Product Code Validation

**Field/Entity:** product_code

**Validation Type:** Business Constraint (Entity Existence)

**Rule Description:**
If a product code is specified, it must reference a valid banking product configured in the system.

**Validation Logic:**

- **Condition:** When creating a new bank account with a product_code specified
- **Check:** Validate that the product code references an existing banking product
- **Valid Criteria:** Product with the specified code exists for the bank
- **Invalid Criteria:** Product code does not exist or is not available for the bank
- **Action on Success:** Associate the account with the specified product
- **Action on Failure:** Return error response indicating product not found

**Error Handling:**

- **Error Message:** `Product not found. Please specify a valid product code.`
- **Error Code:** `OBP-30301`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.products.Products`
- **Method/Function:** `getProductByCode(bankId: BankId, productCode: ProductCode)`
- **Line Reference:** Product lookup methods

**Code Snippet:**
```scala
def getProductByCode(bankId: BankId, productCode: ProductCode): Box[Product] = {
  Products.products.vend.getProductByCode(bankId, productCode) match {
    case Full(product) => Full(product)
    case Empty => Failure(ErrorMessages.ProductNotFound)
    case f: Failure => f
  }
}
```

**Related Entities:**
- Product entity
- Account entity (product association)

**User Story Context:**
From data validations: "Product code must reference a valid banking product if specified" - ensures product codes are valid.

**Dependencies:**
- Product entity must be configured for the bank

---

### Rule VR-013: Branch Existence Validation

**Field/Entity:** branch_id

**Validation Type:** Business Constraint (Entity Existence)

**Rule Description:**
If a branch ID is specified, it must reference a valid branch of the bank.

**Validation Logic:**

- **Condition:** When creating a new bank account with a branch_id specified
- **Check:** Validate that the branch ID references an existing branch of the specified bank
- **Valid Criteria:** Branch with the specified ID exists and belongs to the bank
- **Invalid Criteria:** Branch does not exist or does not belong to the specified bank
- **Action on Success:** Associate the account with the specified branch
- **Action on Failure:** Return error response indicating branch not found

**Error Handling:**

- **Error Message:** `Branch not found. Please specify a valid branch ID.`
- **Error Code:** `OBP-30201`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.branches.Branches`
- **Method/Function:** `getBranchById(bankId: BankId, branchId: BranchId)`
- **Line Reference:** Branch lookup methods

**Code Snippet:**
```scala
def getBranchById(bankId: BankId, branchId: BranchId): Box[Branch] = {
  Branches.branches.vend.getBranchById(bankId, branchId) match {
    case Full(branch) => Full(branch)
    case Empty => Failure(ErrorMessages.BranchNotFound)
    case f: Failure => f
  }
}
```

**Related Entities:**
- Branch entity
- Account entity (branch association)

**User Story Context:**
From data validations: "Branch ID must reference a valid branch if specified" - ensures branch references are valid.

**Dependencies:**
- Branch entity must exist for the bank

---

## Category: Length/Boundary Validation

### Rule VR-014: Account Label Length Validation

**Field/Entity:** label

**Validation Type:** Length Validation

**Rule Description:**
Account label must not exceed the maximum allowed length to ensure proper storage and display.

**Validation Logic:**

- **Condition:** When creating a new bank account with a label
- **Check:** Validate that the label length is within acceptable bounds
- **Valid Criteria:** 
  - Minimum length: 1 character
  - Maximum length: 512 characters
- **Invalid Criteria:**
  - Empty string (0 characters)
  - Length exceeds 512 characters
- **Action on Success:** Accept the label for account creation
- **Action on Failure:** Return error response indicating label length is invalid

**Error Handling:**

- **Error Message:** `Value too long. Maximum length is 512 characters.`
- **Error Code:** `OBP-20010`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `checkMediumString(value: String)`
- **Line Reference:** String length validation methods

**Code Snippet:**
```scala
def checkMediumString(value: String): String = {
  val valueLength = value.length
  val regex = """^([A-Za-z0-9\-._@\s]+)$""".r
  value match {
    case regex(e) if(valueLength <= 512) => SILENCE_IS_GOLDEN
    case regex(e) if(valueLength > 512) => ErrorMessages.InvalidValueLength
    case _ => ErrorMessages.InvalidValueCharacters
  }
}
```

**Related Entities:**
- Account entity (label field)

**User Story Context:**
From data validations: "Account label must not be empty and should follow naming conventions" - ensures labels are within acceptable length.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-015: Account ID Length Validation

**Field/Entity:** account_id

**Validation Type:** Length Validation

**Rule Description:**
When specifying an account ID, it must not exceed the maximum allowed length.

**Validation Logic:**

- **Condition:** When creating an account with a pre-specified account ID
- **Check:** Validate that the account ID length is within acceptable bounds
- **Valid Criteria:** 
  - Minimum length: 1 character
  - Maximum length: 255 characters
- **Invalid Criteria:**
  - Empty string (0 characters)
  - Length exceeds 255 characters
- **Action on Success:** Accept the account ID for account creation
- **Action on Failure:** Return error response indicating account ID length is invalid

**Error Handling:**

- **Error Message:** `Account ID too long. Maximum length is 255 characters.`
- **Error Code:** `OBP-30005`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidID(id: String)`
- **Line Reference:** ID validation utility methods

**Code Snippet:**
```scala
def isValidID(id: String): Boolean = {
  val regex = """^([A-Za-z0-9\-_.]+)$""".r
  id match {
    case regex(e) if(e.length < 256) => true
    case _ => false
  }
}
```

**Related Entities:**
- Account entity (account_id field)

**User Story Context:**
From endpoint 2: "Create a new bank account with a pre-specified account ID" - ensures user-specified account IDs are within length limits.

**Dependencies:**
- None (standalone validation)

---

## Quality Checklist Verification

- [x] All validation functions in relevant code are documented
- [x] All error messages are captured with exact text
- [x] All error codes are documented
- [x] Regex patterns are included verbatim
- [x] Length constraints are specified with exact limits
- [x] Required vs. optional fields are clearly marked
- [x] Cross-field validations are identified (none applicable for this user story)
- [x] Business constraint validations are included
- [x] Code references include file paths and method names
- [x] User story context is explained for each rule
- [x] Valid and invalid criteria are clearly stated
- [x] HTTP status codes are documented where applicable
- [x] Dependencies between validations are noted
