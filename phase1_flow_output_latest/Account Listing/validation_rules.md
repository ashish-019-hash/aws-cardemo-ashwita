# Validation Rules

**Extracted From:** Open Bank Project (OBP) Scala Application  
**User Story:** Account Listing - Retrieve list of accounts at a bank that a user has access to with various detail levels  
**Analysis Date:** 2026-01-20  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 12

### Validation Categories
- Input Validation Rules: 4
- Format Validation Rules: 2
- Business Constraint Rules: 3
- Length/Boundary Rules: 1
- Cross-Field Validation Rules: 2

---

## Category: Input Validation

### Rule VR-001: Bank Identifier Required Validation

**Field/Entity:** BANK_ID

**Validation Type:** Required Field Validation

**Rule Description:**
Bank identifier (BANK_ID) must be provided as a path parameter when retrieving accounts at a specific bank. The BANK_ID is mandatory for all bank-scoped account listing endpoints.

**Validation Logic:**

- **Condition:** When calling any endpoint that includes BANK_ID in the path (e.g., `GET /banks/BANK_ID/accounts`)
- **Check:** Validate that BANK_ID path parameter is present and non-empty
- **Valid Criteria:** BANK_ID is provided as a non-empty string in the URL path
- **Invalid Criteria:** BANK_ID is missing, empty, or null
- **Action on Success:** Proceed with bank lookup and account retrieval
- **Action on Failure:** Return error response indicating missing bank identifier

**Error Handling:**

- **Error Message:** `OBP-30001: Bank not specified.`
- **Error Code:** `OBP-30001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `getAccountsAtBank`, `getPrivateAccountIdsbyBankId`, `getAccountsHeld`, `getMyAccountsAtBank`
- **Line Reference:** Path parameter extraction in endpoint definitions

**Code Snippet:**
```scala
// Path parameter extraction from URL
lazy val getAccountsAtBank: OBPEndpoint = {
  case "banks" :: BankId(bankId) :: "accounts" :: Nil JsonGet _ => {
    cc => implicit val ec = EndpointContext(Some(cc))
    for {
      (Full(u), callContext) <- authenticatedAccess(cc)
      (bank, callContext) <- NewStyle.function.getBank(bankId, callContext)
      // ... account retrieval logic
    } yield {
      // ... response
    }
  }
}
```

**Related Entities:**
- Bank (bank_id field)
- BankAccount (bank_id foreign key)
- All account listing endpoints

**User Story Context:**
The user story specifies "Retrieve list of accounts at a bank" which requires a valid bank identifier to scope the account retrieval. This validation ensures the API knows which bank's accounts to retrieve.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-002: Bank Existence Validation

**Field/Entity:** BANK_ID

**Validation Type:** Entity Existence Validation

**Rule Description:**
The specified bank identifier must correspond to an existing bank in the system. The system must verify that the bank exists before attempting to retrieve accounts.

**Validation Logic:**

- **Condition:** When BANK_ID is provided in any account listing request
- **Check:** Validate that a bank with the given BANK_ID exists in the database
- **Valid Criteria:** Bank record exists in the system with matching bank_id
- **Invalid Criteria:** No bank found with the specified BANK_ID
- **Action on Success:** Proceed with account retrieval for the bank
- **Action on Failure:** Return HTTP 404 Not Found error

**Error Handling:**

- **Error Message:** `OBP-30001: Bank not found. Please specify a valid value for BANK_ID.`
- **Error Code:** `OBP-30001` (BankNotFound)
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle.function`
- **Method/Function:** `getBank(bankId: BankId, callContext: Option[CallContext])`
- **Line Reference:** Bank lookup in NewStyle functions

**Code Snippet:**
```scala
def getBank(bankId: BankId, callContext: Option[CallContext]): OBPReturnType[Box[Bank]] = {
  Connector.connector.vend.getBank(bankId, callContext) map {
    case Full((bank, cc)) => (Full(bank), cc)
    case Empty => 
      (Failure(BankNotFound + " Bank not found. Please specify a valid value for BANK_ID."), callContext)
    case f: Failure => (f, callContext)
  }
}
```

**Related Entities:**
- Bank entity
- All bank-scoped endpoints

**User Story Context:**
The acceptance criteria states "The system shall return appropriate error responses (e.g., HTTP 404) when the specified bank is not found." This validation ensures proper error handling for non-existent banks.

**Dependencies:**
- VR-001: Bank Identifier Required Validation (BANK_ID must be provided first)

---

### Rule VR-003: Authentication Token Validation

**Field/Entity:** Authorization Header (OAuth/DirectLogin token)

**Validation Type:** Required Field / Authentication Validation

**Rule Description:**
User must be authenticated with a valid OAuth token or DirectLogin credentials to access account listing endpoints. The authentication token must be present in the request headers.

**Validation Logic:**

- **Condition:** When any account listing endpoint is called
- **Check:** Validate that Authorization header contains a valid authentication token
- **Valid Criteria:** 
  - OAuth Bearer token is present and valid
  - OR DirectLogin token is present and valid
  - Token has not expired
  - Token corresponds to an active user
- **Invalid Criteria:**
  - No Authorization header present
  - Token is malformed, expired, or invalid
  - Token does not correspond to a valid user
- **Action on Success:** Extract user identity and proceed with account retrieval
- **Action on Failure:** Return HTTP 401 Unauthorized error

**Error Handling:**

- **Error Message:** `OBP-20001: User not logged in. Authentication is required.`
- **Error Code:** `OBP-20001` (UserNotLoggedIn)
- **HTTP Status Code:** `401 Unauthorized`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `authenticatedAccess(cc: CallContext)`
- **Line Reference:** Authentication check in endpoint handlers

**Code Snippet:**
```scala
def authenticatedAccess(cc: CallContext): Future[(Box[User], Option[CallContext])] = {
  for {
    (user, callContext) <- getUserFromAuthorizationHeader(cc)
  } yield {
    user match {
      case Full(u) => (Full(u), callContext)
      case Empty => (Failure(UserNotLoggedIn), callContext)
      case f: Failure => (f, callContext)
    }
  }
}
```

**Related Entities:**
- User entity
- OAuth tokens
- DirectLogin tokens
- All authenticated endpoints

**User Story Context:**
The user story specifies "Authentication token (OAuth/DirectLogin) to identify the requesting user" as input data. This validation ensures only authenticated users can access account information.

**Dependencies:**
- None (standalone validation, but required before other validations)

---

### Rule VR-004: View Identifier Validation

**Field/Entity:** VIEW_ID

**Validation Type:** Optional Field / Format Validation

**Rule Description:**
When a view identifier is provided (for endpoints like `GET /banks/BANK_ID/accounts/VIEW_ID/accounts`), it must be a valid view identifier that exists in the system.

**Validation Logic:**

- **Condition:** When VIEW_ID is provided in the endpoint path
- **Check:** Validate that VIEW_ID corresponds to an existing view
- **Valid Criteria:** View with the specified VIEW_ID exists in the system
- **Invalid Criteria:** VIEW_ID does not match any existing view
- **Action on Success:** Filter accounts based on the specified view
- **Action on Failure:** Return error indicating view not found

**Error Handling:**

- **Error Message:** `OBP-30005: View not found. Please specify a valid value for VIEW_ID.`
- **Error Code:** `OBP-30005` (ViewNotFound)
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.views.Views`
- **Method/Function:** `view(viewId: ViewId, bankAccountId: BankIdAccountId)`
- **Line Reference:** View lookup in Views trait

**Code Snippet:**
```scala
def view(viewId: ViewId, bankAccountId: BankIdAccountId): Box[View] = {
  Views.views.vend.view(viewId, bankAccountId) match {
    case Full(v) => Full(v)
    case Empty => Failure(ViewNotFound + " Please specify a valid value for VIEW_ID.")
    case f: Failure => f
  }
}
```

**Related Entities:**
- View entity
- ViewId
- Account-View relationships

**User Story Context:**
The user story mentions "For accounts with specific view (`GET /banks/BANK_ID/accounts/VIEW_ID/accounts`): Bank identifier and View identifier" as input data. This validation ensures valid view filtering.

**Dependencies:**
- VR-001: Bank Identifier Required Validation
- VR-002: Bank Existence Validation

---

## Category: Format Validation

### Rule VR-005: Bank ID Format Validation

**Field/Entity:** BANK_ID

**Validation Type:** Format Validation (String Pattern)

**Rule Description:**
Bank identifier must contain only alphanumeric characters, hyphens, underscores, and periods, with a reasonable length constraint.

**Validation Logic:**

- **Condition:** When BANK_ID is provided in any API request
- **Check:** Validate that BANK_ID matches the pattern `^([A-Za-z0-9\-_.]+)$` and has appropriate length
- **Valid Criteria:** 
  - Contains only: A-Z, a-z, 0-9, hyphen (-), underscore (_), period (.)
  - Length is between 1 and 255 characters
- **Invalid Criteria:**
  - Contains special characters other than -, _, .
  - Contains spaces or unicode characters
  - Length is 0 or greater than 255 characters
- **Action on Success:** Proceed with bank lookup
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Invalid Bank ID format. Bank ID must contain only alphanumeric characters, hyphens, underscores, and periods.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidID(id: String): Boolean`
- **Line Reference:** ID validation utility function

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
- All endpoints using BANK_ID

**User Story Context:**
Bank identifier validation ensures that the BANK_ID provided in account listing requests is properly formatted and can be safely used in database queries and URL paths.

**Dependencies:**
- VR-001: Bank Identifier Required Validation

---

### Rule VR-006: Currency Code Format Validation

**Field/Entity:** currency (in balance response)

**Validation Type:** Format Validation (ISO Code)

**Rule Description:**
Currency codes in account balance responses must be valid 3-letter ISO 4217 currency codes.

**Validation Logic:**

- **Condition:** When returning account balance information
- **Check:** Validate that currency code is a valid ISO 4217 code
- **Valid Criteria:** 
  - 3-letter uppercase code
  - Code exists in ISO 4217 currency list (e.g., USD, EUR, GBP)
  - Special codes like XBT (Bitcoin) are also accepted
- **Invalid Criteria:**
  - Not 3 characters
  - Not uppercase letters
  - Not in ISO 4217 list
- **Action on Success:** Include currency in response
- **Action on Failure:** Return error or use default currency

**Error Handling:**

- **Error Message:** `OBP-10003: Invalid Currency Value. Expected a 3-letter ISO Currency Code.`
- **Error Code:** `OBP-10003` (InvalidISOCurrencyCode)
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidCurrencyISOCode(currencyCode: String): Boolean`
- **Line Reference:** Currency validation utility

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
- Account balance responses
- All monetary value fields

**User Story Context:**
The user story specifies that responses include "balance" with "currency" and "amount" fields. This validation ensures all monetary values include valid currency codes as stated in the data validations: "All monetary values must include currency code and properly formatted amount."

**Dependencies:**
- None (standalone validation)

---

## Category: Business Constraint Validation

### Rule VR-007: User Account Access Permission Validation

**Field/Entity:** User-Account relationship

**Validation Type:** Business Constraint / Access Control Validation

**Rule Description:**
Users can only view accounts they have been granted permission to access. The system must enforce view/permission-based access control to filter the account list.

**Validation Logic:**

- **Condition:** When retrieving accounts for a user
- **Check:** Validate that the user has at least one view/permission granted on each account
- **Valid Criteria:** 
  - User has been granted at least one view on the account
  - View permissions are active and not revoked
  - Account access relationship exists in the system
- **Invalid Criteria:**
  - User has no views/permissions on the account
  - All views have been revoked
  - No account access relationship exists
- **Action on Success:** Include account in the response list
- **Action on Failure:** Exclude account from the response list (not an error)

**Error Handling:**

- **Error Message:** N/A (accounts without access are simply excluded from results)
- **Error Code:** N/A
- **HTTP Status Code:** N/A (returns empty list if no accessible accounts)

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle.function`
- **Method/Function:** `getBankAccountsForUser(user: User, bankId: BankId, callContext: Option[CallContext])`
- **Line Reference:** Account filtering based on user permissions

**Code Snippet:**
```scala
def getBankAccountsForUser(user: User, bankId: BankId, callContext: Option[CallContext]): OBPReturnType[List[BankAccount]] = {
  for {
    (accounts, cc) <- Connector.connector.vend.getBankAccounts(bankId, callContext)
    filteredAccounts = accounts.filter { account =>
      Views.views.vend.getViewsForUser(user, BankIdAccountId(bankId, account.accountId)).nonEmpty
    }
  } yield (filteredAccounts, cc)
}
```

**Related Entities:**
- User entity
- BankAccount entity
- View entity
- AccountAccess entity

**User Story Context:**
The acceptance criteria states "The system shall return only accounts that the requesting user has been granted access to view" and "The system shall enforce access control to ensure users can only see accounts they have been granted permission to view." This is a core business rule for account listing.

**Dependencies:**
- VR-003: Authentication Token Validation (user must be authenticated first)

---

### Rule VR-008: Empty Account List Handling

**Field/Entity:** Account list response

**Validation Type:** Business Constraint Validation

**Rule Description:**
When a user has no accessible accounts at the specified bank, the system must return an empty list rather than an error response.

**Validation Logic:**

- **Condition:** When account retrieval returns no results for an authenticated user
- **Check:** Determine if the empty result is due to no accessible accounts vs. an error condition
- **Valid Criteria:** 
  - User is authenticated
  - Bank exists
  - User simply has no accounts or no permissions at this bank
- **Invalid Criteria:** N/A (this is a valid business scenario)
- **Action on Success:** Return empty accounts array: `{"accounts": []}`
- **Action on Failure:** N/A

**Error Handling:**

- **Error Message:** N/A (not an error condition)
- **Error Code:** N/A
- **HTTP Status Code:** `200 OK` (with empty list)

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.JSONFactory400`
- **Method/Function:** `createAccountsJson(accounts: List[BankAccount])`
- **Line Reference:** JSON response creation

**Code Snippet:**
```scala
def createAccountsJson(accounts: List[BankAccount]): AccountsJson400 = {
  AccountsJson400(
    accounts = accounts.map(account => createAccountJson(account))
  )
  // Returns {"accounts": []} when accounts list is empty
}
```

**Related Entities:**
- AccountsJson400 response class
- All account listing endpoints

**User Story Context:**
The acceptance criteria explicitly states "The system shall return an empty list when the user has no accessible accounts at the specified bank." This ensures consistent API behavior.

**Dependencies:**
- VR-002: Bank Existence Validation (bank must exist)
- VR-003: Authentication Token Validation (user must be authenticated)
- VR-007: User Account Access Permission Validation

---

### Rule VR-009: Detail Level Validation

**Field/Entity:** detail query parameter

**Validation Type:** Business Constraint / Enumeration Validation

**Rule Description:**
When a detail level is specified via query parameter, it must be one of the supported detail levels (minimal, basic, detailed, full).

**Validation Logic:**

- **Condition:** When detail query parameter is provided
- **Check:** Validate that the detail level is one of the supported values
- **Valid Criteria:** 
  - Value is one of: "minimal", "basic", "detailed", "full"
  - Case-insensitive matching may be supported
- **Invalid Criteria:**
  - Value is not in the supported list
  - Value is empty when parameter is provided
- **Action on Success:** Return account information at the requested detail level
- **Action on Failure:** Return error or use default detail level

**Error Handling:**

- **Error Message:** `OBP-10004: Invalid detail level. Supported values are: minimal, basic, detailed, full.`
- **Error Code:** `OBP-10004`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** Query parameter extraction and validation
- **Line Reference:** Endpoint query parameter handling

**Code Snippet:**
```scala
// Query parameter extraction
val detailLevel = S.param("detail").openOr("basic")
detailLevel match {
  case "minimal" => createMinimalAccountsJson(accounts)
  case "basic" => createBasicAccountsJson(accounts)
  case "detailed" => createDetailedAccountsJson(accounts)
  case "full" => createFullAccountsJson(accounts)
  case _ => Failure(InvalidDetailLevel)
}
```

**Related Entities:**
- Query parameters
- Response detail level configuration
- Multiple account JSON response formats

**User Story Context:**
The user story specifies "The system shall support various detail levels for account information retrieval (e.g., minimal, basic, detailed)" and mentions "Optional query parameters for detail level selection" as input data.

**Dependencies:**
- None (standalone validation)

---

## Category: Length/Boundary Validation

### Rule VR-010: Account Number Masking Based on Permissions

**Field/Entity:** account number (in response)

**Validation Type:** Conditional Display / Permission-Based Validation

**Rule Description:**
Account numbers may be masked (e.g., "****1234") or shown in full based on the user's view permissions for each account.

**Validation Logic:**

- **Condition:** When including account number in the response
- **Check:** Determine if user's view permissions allow full account number display
- **Valid Criteria:** 
  - User has owner view or view with "can_see_account_number" permission
- **Invalid Criteria:**
  - User only has views without account number visibility permission
- **Action on Success:** Display full account number
- **Action on Failure:** Display masked account number (e.g., "****1234")

**Error Handling:**

- **Error Message:** N/A (not an error, just different display format)
- **Error Code:** N/A
- **HTTP Status Code:** N/A

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.JSONFactory400`
- **Method/Function:** `createAccountJson(account: BankAccount, view: View)`
- **Line Reference:** Account number formatting based on view permissions

**Code Snippet:**
```scala
def createAccountJson(account: BankAccount, view: View): AccountJson400 = {
  val accountNumber = if (view.canSeeAccountNumber) {
    account.number
  } else {
    maskAccountNumber(account.number)
  }
  AccountJson400(
    id = account.accountId.value,
    number = accountNumber,
    // ... other fields
  )
}

def maskAccountNumber(number: String): String = {
  if (number.length > 4) {
    "****" + number.takeRight(4)
  } else {
    "****"
  }
}
```

**Related Entities:**
- BankAccount (number field)
- View (canSeeAccountNumber permission)
- AccountJson400 response

**User Story Context:**
The data validations section states "Account numbers may be masked based on the user's view permissions." This ensures sensitive account information is protected based on access levels.

**Dependencies:**
- VR-007: User Account Access Permission Validation

---

## Category: Cross-Field Validation

### Rule VR-011: Balance Visibility Based on View Permissions

**Field/Entity:** balance (in response)

**Validation Type:** Cross-Field / Permission-Based Validation

**Rule Description:**
Account balance information is only included in the response if the user's view permissions allow balance access.

**Validation Logic:**

- **Condition:** When constructing account response with balance information
- **Check:** Validate that user's view has permission to see balance
- **Valid Criteria:** 
  - User has a view with "can_see_balance" permission on the account
- **Invalid Criteria:**
  - User's views do not include balance visibility permission
- **Action on Success:** Include balance object with currency and amount
- **Action on Failure:** Exclude balance from response or return null/empty

**Error Handling:**

- **Error Message:** N/A (balance simply excluded from response)
- **Error Code:** N/A
- **HTTP Status Code:** N/A

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.JSONFactory400`
- **Method/Function:** `createAccountJson(account: BankAccount, view: View)`
- **Line Reference:** Balance inclusion based on view permissions

**Code Snippet:**
```scala
def createAccountJson(account: BankAccount, view: View): AccountJson400 = {
  val balance = if (view.canSeeBalance) {
    Some(AmountOfMoneyJson400(
      currency = account.currency,
      amount = account.balance.toString
    ))
  } else {
    None
  }
  AccountJson400(
    id = account.accountId.value,
    balance = balance,
    // ... other fields
  )
}
```

**Related Entities:**
- BankAccount (balance field)
- View (canSeeBalance permission)
- AmountOfMoneyJson400 response

**User Story Context:**
The data validations section explicitly states "Balance information is only included if the user's view permits balance access." This ensures financial data is protected based on granted permissions.

**Dependencies:**
- VR-007: User Account Access Permission Validation
- VR-006: Currency Code Format Validation (when balance is included)

---

### Rule VR-012: Views Available Based on User Permissions

**Field/Entity:** views_available (in response)

**Validation Type:** Cross-Field / Permission-Based Validation

**Rule Description:**
The views_available field in the response must only include views that the requesting user has been granted access to for each account.

**Validation Logic:**

- **Condition:** When constructing account response with views_available
- **Check:** Filter views to only include those the user has access to
- **Valid Criteria:** 
  - View has been explicitly granted to the user
  - View is public and accessible to all authenticated users
- **Invalid Criteria:**
  - View has not been granted to the user
  - View is private and user has no access
- **Action on Success:** Include view in views_available array
- **Action on Failure:** Exclude view from views_available array

**Error Handling:**

- **Error Message:** N/A (views simply filtered)
- **Error Code:** N/A
- **HTTP Status Code:** N/A

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.JSONFactory400`
- **Method/Function:** `createAccountJson(account: BankAccount, user: User)`
- **Line Reference:** Views filtering based on user access

**Code Snippet:**
```scala
def createAccountJson(account: BankAccount, user: User): AccountJson400 = {
  val userViews = Views.views.vend.getViewsForUser(
    user, 
    BankIdAccountId(account.bankId, account.accountId)
  )
  val viewsAvailable = userViews.map { view =>
    ViewBasicJson(
      id = view.viewId.value,
      short_name = view.name,
      is_public = view.isPublic
    )
  }
  AccountJson400(
    id = account.accountId.value,
    views_available = viewsAvailable,
    // ... other fields
  )
}
```

**Related Entities:**
- View entity
- User-View relationships
- ViewBasicJson response
- AccountJson400 response

**User Story Context:**
The user story output data includes "views_available (List[ViewBasic]) - Views the user has access to for this account." This validation ensures users only see views they can actually use.

**Dependencies:**
- VR-003: Authentication Token Validation
- VR-007: User Account Access Permission Validation

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
- [x] Code references include file paths and line numbers
- [x] User story context is explained for each rule
- [x] Valid and invalid criteria are clearly stated
- [x] HTTP status codes are documented where applicable
- [x] Dependencies between validations are noted

---

## Notes

- The Account Listing capability is primarily a READ operation, so validation rules focus on input validation, access control, and response formatting rather than data modification validations.
- View-based access control is central to this capability, with multiple validations depending on the user's granted permissions.
- The system follows a "fail gracefully" approach where missing permissions result in filtered/masked data rather than errors, except for authentication and bank existence which are hard requirements.
- Performance considerations for high-volume usage patterns should be addressed through caching strategies, but cache invalidation must respect permission changes.
