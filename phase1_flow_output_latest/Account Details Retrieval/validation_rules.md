# Validation Rules

**Extracted From:** Open Bank Project (OBP) Scala Application  
**User Story:** Account Details Retrieval  
**Analysis Date:** 2026-01-20  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 14

### Validation Categories
- Input Validation Rules: 5
- Format Validation Rules: 2
- Business Constraint Rules: 4
- Length/Boundary Rules: 1
- Cross-Field Validation Rules: 2

---

## Category: Input Validation (Required Fields)

### Rule VR-001: Bank Identifier Required Validation

**Field/Entity:** BANK_ID

**Validation Type:** Required Field Validation

**Rule Description:**
Bank identifier must be provided as a path parameter and must exist in the system for account details retrieval.

**Validation Logic:**

- **Condition:** When any account details retrieval endpoint is called
- **Check:** Validate that BANK_ID path parameter is provided and references an existing bank
- **Valid Criteria:** BANK_ID is non-empty and corresponds to an existing bank in the system
- **Invalid Criteria:** BANK_ID is empty, null, or does not exist in the system
- **Action on Success:** Proceed with account lookup within the specified bank
- **Action on Failure:** Return error response with 404 status code

**Error Handling:**

- **Error Message:** `OBP-30001: Bank not found. Please specify a valid value for BANK_ID.`
- **Error Code:** `OBP-30001`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `getBank()` / `getBankFuture()`
- **Line Reference:** Bank validation in API methods

**Code Snippet:**
```scala
for {
  (bank, callContext) <- NewStyle.function.getBank(bankId, callContext)
  // ... rest of the logic
} yield {
  // success response
}
```

**Related Entities:**
- Bank (bank_id field)
- BankAccount (bank_id foreign key)

**User Story Context:**
This validation ensures that account details can only be retrieved from valid, existing banks. The user story specifies "Get detailed information about a specific account" which requires the account to belong to a valid bank.

**Dependencies:**
- Bank must exist in the system (Bank Creation capability)

---

### Rule VR-002: Account Identifier Required Validation

**Field/Entity:** ACCOUNT_ID

**Validation Type:** Required Field Validation

**Rule Description:**
Account identifier must be provided as a path parameter and must exist within the specified bank.

**Validation Logic:**

- **Condition:** When any account details retrieval endpoint is called
- **Check:** Validate that ACCOUNT_ID path parameter is provided and references an existing account within the specified bank
- **Valid Criteria:** ACCOUNT_ID is non-empty and corresponds to an existing account in the specified bank
- **Invalid Criteria:** ACCOUNT_ID is empty, null, or does not exist within the specified bank
- **Action on Success:** Proceed with account details retrieval
- **Action on Failure:** Return error response with 404 status code

**Error Handling:**

- **Error Message:** `OBP-30018: Account not found. Please specify a valid value for ACCOUNT_ID.`
- **Error Code:** `OBP-30018`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `getBankAccount()` / `checkBankAccountExists()`
- **Line Reference:** Account validation in API methods

**Code Snippet:**
```scala
for {
  (bankAccount, callContext) <- NewStyle.function.getBankAccount(bankId, accountId, callContext)
  // ... rest of the logic
} yield {
  // success response
}
```

**Related Entities:**
- BankAccount (account_id field)
- Transaction (from_account_id, to_account_id fields)

**User Story Context:**
This validation ensures that only existing accounts can have their details retrieved. The user story explicitly mentions "a specific account identified by its unique account ID" in acceptance criteria #1.

**Dependencies:**
- Account must be created and linked to the bank (Account Creation capability)
- VR-001: Bank Identifier Required Validation must pass first

---

### Rule VR-003: View Identifier Required Validation

**Field/Entity:** VIEW_ID

**Validation Type:** Required Field Validation

**Rule Description:**
View identifier must be provided as a path parameter for view-moderated account access endpoints and must be a valid view that the user has access to.

**Validation Logic:**

- **Condition:** When account details retrieval endpoints with VIEW_ID parameter are called (e.g., `/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/account`)
- **Check:** Validate that VIEW_ID path parameter is provided and references a valid view that the user has permission to access
- **Valid Criteria:** VIEW_ID is non-empty, exists for the account, and user has been granted access to this view
- **Invalid Criteria:** VIEW_ID is empty, null, does not exist, or user lacks permission to access it
- **Action on Success:** Proceed with view-moderated account details retrieval
- **Action on Failure:** Return error response with 403 status code

**Error Handling:**

- **Error Message:** `OBP-30005: View not found for Account. Please specify a valid value for VIEW_ID.`
- **Error Code:** `OBP-30005`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.views.Views`
- **Method/Function:** `checkViewAccessAndReturnView()` / `view()`
- **Line Reference:** View validation in API methods

**Code Snippet:**
```scala
for {
  view <- NewStyle.function.checkViewAccessAndReturnView(viewId, bankIdAccountId, user, callContext)
  // ... rest of the logic
} yield {
  // success response
}
```

**Related Entities:**
- View (view_id field)
- AccountAccess (view_id foreign key)
- ViewDefinition

**User Story Context:**
This validation supports acceptance criteria #9: "The system shall include view-specific information based on the view through which the account is being accessed." The view determines what data is visible to the user.

**Dependencies:**
- Views must be defined for the account (View Creation capability)
- User must have been granted view/permission access to the account

---

### Rule VR-004: Authentication Token Required Validation

**Field/Entity:** Authorization Header (OAuth/DirectLogin Token)

**Validation Type:** Required Field Validation

**Rule Description:**
User must be authenticated with a valid OAuth token or DirectLogin credentials to retrieve account details.

**Validation Logic:**

- **Condition:** When any account details retrieval endpoint is called
- **Check:** Validate that Authorization header contains a valid authentication token
- **Valid Criteria:** Authorization header is present and contains a valid, non-expired OAuth token or DirectLogin token
- **Invalid Criteria:** Authorization header is missing, token is invalid, expired, or malformed
- **Action on Success:** Identify the requesting user and proceed with authorization checks
- **Action on Failure:** Return error response with 401 status code

**Error Handling:**

- **Error Message:** `OBP-20001: User not logged in. Authentication is required.`
- **Error Code:** `OBP-20001`
- **HTTP Status Code:** `401 Unauthorized`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `getUser()` / `getUserFromAuthorizationHeader()`
- **Line Reference:** Authentication validation in API entry points

**Code Snippet:**
```scala
for {
  (Full(user), callContext) <- authenticatedAccess(cc)
  // ... rest of the logic
} yield {
  // success response
}
```

**Related Entities:**
- User (user_id field)
- Consumer (OAuth consumer)
- Token (OAuth/DirectLogin token)

**User Story Context:**
This validation supports acceptance criteria #5: "The system shall enforce access control to ensure users can only retrieve details for accounts they have been granted permission to view." Authentication is the first step in access control.

**Dependencies:**
- User must be authenticated (Authentication & Security capabilities)

---

### Rule VR-005: User Account Access Permission Validation

**Field/Entity:** User-Account-View Permission

**Validation Type:** Authorization Validation

**Rule Description:**
User must have at least one view/permission granted on the account to retrieve its details.

**Validation Logic:**

- **Condition:** After user authentication, when attempting to retrieve account details
- **Check:** Validate that the authenticated user has been granted access to at least one view for the specified account
- **Valid Criteria:** User has at least one view permission (owner, accountant, auditor, etc.) for the account
- **Invalid Criteria:** User has no view permissions for the account
- **Action on Success:** Proceed with account details retrieval based on permitted views
- **Action on Failure:** Return error response with 403 status code

**Error Handling:**

- **Error Message:** `OBP-20006: User does not have permission to access this view on the account.`
- **Error Code:** `OBP-20006`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.views.Views`
- **Method/Function:** `checkAccountAccessAndReturnView()` / `permission()`
- **Line Reference:** Permission validation in account access methods

**Code Snippet:**
```scala
for {
  view <- NewStyle.function.checkAccountAccessAndReturnView(viewId, bankIdAccountId, user, callContext)
  _ <- Helper.booleanToFuture(UserNoPermissionAccessView) {
    view.canSeeTransactionThisBankAccount
  }
  // ... rest of the logic
} yield {
  // success response
}
```

**Related Entities:**
- AccountAccess (user_id, account_id, view_id)
- Permission
- User

**User Story Context:**
This validation directly implements acceptance criteria #5 and #8: "The system shall enforce access control to ensure users can only retrieve details for accounts they have been granted permission to view" and "The system shall return appropriate error responses (e.g., HTTP 403) when the user lacks permission to view the account."

**Dependencies:**
- VR-004: Authentication Token Required Validation must pass first
- User must have been granted view/permission access to the account (View & Permission Management capabilities)

---

## Category: Format Validation

### Rule VR-006: Account ID Format Validation

**Field/Entity:** ACCOUNT_ID

**Validation Type:** Format Validation (String Pattern)

**Rule Description:**
Account ID must contain only alphanumeric characters, hyphens, underscores, and periods, with a maximum length of 255 characters.

**Validation Logic:**

- **Condition:** When ACCOUNT_ID is provided in any API request path parameter
- **Check:** Validate that ACCOUNT_ID matches the pattern `^([A-Za-z0-9\-_.]+)$` and length <= 255
- **Valid Criteria:** 
  - Contains only: A-Z, a-z, 0-9, hyphen (-), underscore (_), period (.)
  - Length is between 1 and 255 characters
- **Invalid Criteria:**
  - Contains special characters other than -, _, .
  - Length is 0 or greater than 255 characters
  - Contains spaces or unicode characters
- **Action on Success:** Proceed with account lookup
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Invalid Account ID format. Account ID must contain only alphanumeric characters, hyphens, underscores, and periods.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidID(id: String): Boolean`
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
- BankAccount (account_id field)
- All entities referencing account_id

**User Story Context:**
This validation ensures that account identifiers used in the account details retrieval are properly formatted and can be safely used in URLs and database queries. The user story mentions "unique account ID" in acceptance criteria #1.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-007: Bank ID Format Validation

**Field/Entity:** BANK_ID

**Validation Type:** Format Validation (String Pattern)

**Rule Description:**
Bank ID must contain only alphanumeric characters, hyphens, underscores, and periods, with a maximum length of 255 characters.

**Validation Logic:**

- **Condition:** When BANK_ID is provided in any API request path parameter
- **Check:** Validate that BANK_ID matches the pattern `^([A-Za-z0-9\-_.]+)$` and length <= 255
- **Valid Criteria:** 
  - Contains only: A-Z, a-z, 0-9, hyphen (-), underscore (_), period (.)
  - Length is between 1 and 255 characters
- **Invalid Criteria:**
  - Contains special characters other than -, _, .
  - Length is 0 or greater than 255 characters
  - Contains spaces or unicode characters
- **Action on Success:** Proceed with bank lookup
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Invalid Bank ID format. Bank ID must contain only alphanumeric characters, hyphens, underscores, and periods.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidID(id: String): Boolean`
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
- Bank (bank_id field)
- BankAccount (bank_id foreign key)

**User Story Context:**
This validation ensures that bank identifiers used in the account details retrieval are properly formatted. The user story requires specifying a bank to retrieve account details from.

**Dependencies:**
- None (standalone validation)

---

## Category: Business Constraint Validation

### Rule VR-008: View-Based Balance Visibility Validation

**Field/Entity:** balance (AmountOfMoney)

**Validation Type:** Business Constraint (View Permission)

**Rule Description:**
Balance information is only included in the response if the user's view permits balance access.

**Validation Logic:**

- **Condition:** When constructing account details response
- **Check:** Validate that the view through which the account is accessed has the `canSeeBalance` permission enabled
- **Valid Criteria:** View has `canSeeBalance = true`
- **Invalid Criteria:** View has `canSeeBalance = false`
- **Action on Success:** Include balance information in the response
- **Action on Failure:** Omit balance information from the response (no error, just filtered data)

**Error Handling:**

- **Error Message:** N/A (data is filtered, not rejected)
- **Error Code:** N/A
- **HTTP Status Code:** `200 OK` (with filtered response)

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.JSONFactory400`
- **Method/Function:** `createModeratedAccountJSON()` / `createAccountJSON()`
- **Line Reference:** Response construction methods

**Code Snippet:**
```scala
def createModeratedAccountJSON(account: ModeratedBankAccount, view: View): ModeratedAccountJSON400 = {
  ModeratedAccountJSON400(
    // ... other fields
    balance = if(view.canSeeBalance) Some(AmountOfMoneyJsonV121(account.currency, account.balance)) else None,
    // ... other fields
  )
}
```

**Related Entities:**
- View (canSeeBalance permission)
- BankAccount (balance field)
- AmountOfMoney (currency, amount)

**User Story Context:**
This validation implements the data validation requirement: "Balance information is only included if the user's view permits balance access." It also supports acceptance criteria #2: "The system shall return the current balance information for the specified account" - but only when permitted by the view.

**Dependencies:**
- VR-003: View Identifier Required Validation
- VR-005: User Account Access Permission Validation

---

### Rule VR-009: View-Based Owner Visibility Validation

**Field/Entity:** owners (List[AccountOwner])

**Validation Type:** Business Constraint (View Permission)

**Rule Description:**
Owner information is only included in the response if the user's view permits owner visibility.

**Validation Logic:**

- **Condition:** When constructing account details response
- **Check:** Validate that the view through which the account is accessed has the `canSeeOwners` permission enabled
- **Valid Criteria:** View has `canSeeOwners = true`
- **Invalid Criteria:** View has `canSeeOwners = false`
- **Action on Success:** Include owner information in the response
- **Action on Failure:** Omit owner information from the response (no error, just filtered data)

**Error Handling:**

- **Error Message:** N/A (data is filtered, not rejected)
- **Error Code:** N/A
- **HTTP Status Code:** `200 OK` (with filtered response)

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.JSONFactory400`
- **Method/Function:** `createModeratedAccountJSON()` / `createAccountJSON()`
- **Line Reference:** Response construction methods

**Code Snippet:**
```scala
def createModeratedAccountJSON(account: ModeratedBankAccount, view: View): ModeratedAccountJSON400 = {
  ModeratedAccountJSON400(
    // ... other fields
    owners = if(view.canSeeOwners) account.owners.map(createAccountOwnerJSON) else List.empty,
    // ... other fields
  )
}
```

**Related Entities:**
- View (canSeeOwners permission)
- AccountOwner (user_id, provider, display_name)
- BankAccount (owners relationship)

**User Story Context:**
This validation implements the data validation requirement: "Owner information is only included if the user's view permits owner visibility." It also supports acceptance criteria #10: "The system shall return account owner information when the user has permission to view ownership details."

**Dependencies:**
- VR-003: View Identifier Required Validation
- VR-005: User Account Access Permission Validation

---

### Rule VR-010: View-Based Account Number Masking Validation

**Field/Entity:** number (String)

**Validation Type:** Business Constraint (View Permission)

**Rule Description:**
Account number may be masked based on the user's view permissions to protect sensitive information.

**Validation Logic:**

- **Condition:** When constructing account details response
- **Check:** Validate that the view through which the account is accessed has the `canSeeAccountNumber` permission enabled
- **Valid Criteria:** View has `canSeeAccountNumber = true`
- **Invalid Criteria:** View has `canSeeAccountNumber = false`
- **Action on Success:** Include full account number in the response
- **Action on Failure:** Include masked account number (e.g., "****7890") or omit entirely

**Error Handling:**

- **Error Message:** N/A (data is masked/filtered, not rejected)
- **Error Code:** N/A
- **HTTP Status Code:** `200 OK` (with masked/filtered response)

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.JSONFactory400`
- **Method/Function:** `createModeratedAccountJSON()` / `createAccountJSON()`
- **Line Reference:** Response construction methods

**Code Snippet:**
```scala
def createModeratedAccountJSON(account: ModeratedBankAccount, view: View): ModeratedAccountJSON400 = {
  ModeratedAccountJSON400(
    // ... other fields
    number = if(view.canSeeAccountNumber) account.number else maskAccountNumber(account.number),
    // ... other fields
  )
}

def maskAccountNumber(number: String): String = {
  if(number.length > 4) "****" + number.takeRight(4) else "****"
}
```

**Related Entities:**
- View (canSeeAccountNumber permission)
- BankAccount (number field)

**User Story Context:**
This validation implements the data validation requirement: "Account number may be masked based on the user's view permissions." It supports acceptance criteria #4: "The system shall return account routing information (e.g., IBAN, account number) based on user permissions."

**Dependencies:**
- VR-003: View Identifier Required Validation
- VR-005: User Account Access Permission Validation

---

### Rule VR-011: Currency Code Format Validation

**Field/Entity:** currency (in balance.currency)

**Validation Type:** Business Constraint (ISO Standard)

**Rule Description:**
All monetary values must include a valid ISO 4217 currency code.

**Validation Logic:**

- **Condition:** When returning balance information in account details response
- **Check:** Validate that the currency code is a valid 3-letter ISO 4217 currency code
- **Valid Criteria:** Currency code is in the ISO 4217 currency code list (e.g., USD, EUR, GBP) or supported cryptocurrency codes (e.g., XBT)
- **Invalid Criteria:** Currency code is not a valid ISO 4217 code
- **Action on Success:** Include currency code in the balance response
- **Action on Failure:** Return error or use default currency handling

**Error Handling:**

- **Error Message:** `OBP-10003: Invalid Currency Value. Expected a 3-letter ISO Currency Code.`
- **Error Code:** `OBP-10003`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidCurrencyISOCode(currencyCode: String): Boolean`
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
- AmountOfMoney (currency field)
- BankAccount (currency field)

**User Story Context:**
This validation implements the data validation requirement: "All monetary values must include currency code and properly formatted amount." The user story mentions "balance" in the capability description which requires proper currency handling.

**Dependencies:**
- VR-008: View-Based Balance Visibility Validation (balance must be visible first)

---

## Category: Length/Boundary Validation

### Rule VR-012: ID Maximum Length Validation

**Field/Entity:** BANK_ID, ACCOUNT_ID, VIEW_ID

**Validation Type:** Length Validation

**Rule Description:**
All identifier fields (BANK_ID, ACCOUNT_ID, VIEW_ID) must not exceed 255 characters in length.

**Validation Logic:**

- **Condition:** When any ID parameter is provided in API requests
- **Check:** Validate that the ID length is between 1 and 255 characters
- **Valid Criteria:** 1 <= length <= 255
- **Invalid Criteria:** length = 0 or length > 255
- **Action on Success:** Proceed with ID validation and lookup
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-20010: Value too long. Maximum length is 255 characters.`
- **Error Code:** `OBP-20010`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidID(id: String): Boolean`
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
- All entities with ID fields

**User Story Context:**
This validation ensures that identifiers used in account details retrieval do not exceed database field limits and can be safely stored and processed.

**Dependencies:**
- None (standalone validation)

---

## Category: Cross-Field Validation

### Rule VR-013: Account-Bank Relationship Validation

**Field/Entity:** ACCOUNT_ID + BANK_ID

**Validation Type:** Cross-Field Validation (Relationship)

**Rule Description:**
The specified account must belong to the specified bank. An account cannot be retrieved from a bank it does not belong to.

**Validation Logic:**

- **Condition:** When retrieving account details with both BANK_ID and ACCOUNT_ID
- **Check:** Validate that the account identified by ACCOUNT_ID exists within the bank identified by BANK_ID
- **Valid Criteria:** Account exists and its bank_id matches the provided BANK_ID
- **Invalid Criteria:** Account does not exist in the specified bank (may exist in a different bank)
- **Action on Success:** Proceed with account details retrieval
- **Action on Failure:** Return error response with 404 status code

**Error Handling:**

- **Error Message:** `OBP-30018: Account not found. Please specify a valid value for ACCOUNT_ID.`
- **Error Code:** `OBP-30018`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `getBankAccount(bankId: BankId, accountId: AccountId)`
- **Line Reference:** Account retrieval methods

**Code Snippet:**
```scala
def getBankAccount(bankId: BankId, accountId: AccountId, callContext: Option[CallContext]): OBPReturnType[Box[BankAccount]] = {
  Connector.connector.vend.getBankAccount(bankId, accountId, callContext) map {
    case Full((account, cc)) if account.bankId == bankId => Full((account, cc))
    case _ => Failure(ErrorMessages.AccountNotFound)
  }
}
```

**Related Entities:**
- Bank (bank_id)
- BankAccount (account_id, bank_id)

**User Story Context:**
This validation ensures data integrity by verifying that the account belongs to the specified bank. The user story mentions retrieving "detailed information about a specific account" which requires proper bank-account relationship validation.

**Dependencies:**
- VR-001: Bank Identifier Required Validation
- VR-002: Account Identifier Required Validation

---

### Rule VR-014: View-Account Access Relationship Validation

**Field/Entity:** VIEW_ID + ACCOUNT_ID + User

**Validation Type:** Cross-Field Validation (Permission Relationship)

**Rule Description:**
The user must have been granted access to the specified view for the specified account. View access is account-specific.

**Validation Logic:**

- **Condition:** When retrieving account details through a specific view
- **Check:** Validate that the authenticated user has been granted access to VIEW_ID for the account identified by ACCOUNT_ID
- **Valid Criteria:** User has an AccountAccess record linking their user_id to the account_id with the specified view_id
- **Invalid Criteria:** No AccountAccess record exists for this user-account-view combination
- **Action on Success:** Proceed with view-moderated account details retrieval
- **Action on Failure:** Return error response with 403 status code

**Error Handling:**

- **Error Message:** `OBP-20006: User does not have permission to access this view on the account.`
- **Error Code:** `OBP-20006`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.views.Views`
- **Method/Function:** `checkAccountAccessAndReturnView(viewId, bankIdAccountId, user)`
- **Line Reference:** View access validation methods

**Code Snippet:**
```scala
def checkAccountAccessAndReturnView(
  viewId: ViewId, 
  bankIdAccountId: BankIdAccountId, 
  user: Option[User], 
  callContext: Option[CallContext]
): OBPReturnType[Box[View]] = {
  for {
    view <- Views.views.vend.view(viewId, bankIdAccountId)
    hasAccess <- Views.views.vend.permission(bankIdAccountId, user)
    _ <- Helper.booleanToFuture(UserNoPermissionAccessView) {
      hasAccess.views.contains(view)
    }
  } yield {
    Full(view)
  }
}
```

**Related Entities:**
- View (view_id)
- AccountAccess (user_id, account_id, view_id)
- User (user_id)
- BankAccount (account_id)

**User Story Context:**
This validation implements acceptance criteria #5: "The system shall enforce access control to ensure users can only retrieve details for accounts they have been granted permission to view" and acceptance criteria #9: "The system shall include view-specific information based on the view through which the account is being accessed."

**Dependencies:**
- VR-003: View Identifier Required Validation
- VR-004: Authentication Token Required Validation
- VR-005: User Account Access Permission Validation

---

## Quality Checklist Verification

- [x] All validation functions in relevant code are documented
- [x] All error messages are captured with exact text
- [x] All error codes are documented
- [x] Regex patterns are included verbatim
- [x] Length constraints are specified with exact limits
- [x] Required vs. optional fields are clearly marked
- [x] Cross-field validations are identified
- [x] Business constraint validations are included
- [x] Code references include file paths and method names
- [x] User story context is explained for each rule
- [x] Valid and invalid criteria are clearly stated
- [x] HTTP status codes are documented where applicable
- [x] Dependencies between validations are noted

---

## Notes

- All validation rules are derived from the Account Details Retrieval user story and its data validation requirements
- View-based data filtering (VR-008, VR-009, VR-010) does not return errors but filters the response data based on permissions
- The validation rules follow the OBP error code convention (OBP-XXXXX format)
- Cross-field validations (VR-013, VR-014) ensure data integrity and proper access control
- Performance considerations for high-volume access patterns should be addressed through caching strategies as noted in the user story
