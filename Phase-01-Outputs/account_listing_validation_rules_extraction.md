# Validation Rules Extraction for Account Listing User Story
## Based on Actual Open Bank Project API Implementation

This document presents validation rules extracted from the actual Open Bank Project API source code (https://github.com/OpenBankProject/OBP-API.git), specifically analyzing the Account Listing endpoints in APIMethods510.scala.

---

## Executive Summary

The **Account Listing** functionality allows banking application users to retrieve a list of all bank accounts they have "held" at a specific bank or across all banks. This analysis documents the **actual validation rules implemented in the OBP-API Scala code**, not assumptions based on the user story alone.

**CRITICAL FINDING**: These endpoints do **NOT** check view permissions. The ResourceDoc explicitly states: *"Get Accounts held by the User if even the User has not been assigned the owner View yet."* This is by design to support account onboarding workflows.

---

## Actual Validation Rules from OBP-API Source Code

Based on analysis of the Scala implementation, there are **5 core validation rules** enforced by the Account Listing endpoints:

### 1. User Authentication Validation

**Implementation Location**: Framework level (implicit in CallContext)

**What it checks**: Whether the user is authenticated with a valid session/token before accessing account lists

**Why it exists**: To ensure only authenticated users can access any account information

**When it applies**: First step of every API request, before any processing

**Who it affects**: All API consumers, both authenticated and unauthenticated

**What happens when it fails**: 
- HTTP Status: 401 Unauthorized
- Error Message: "UserNotLoggedIn"

**Where it is enforced**: API authentication middleware/framework

**Source Code Evidence**:
- Test: AccountTest.scala lines 46-48 (getAccountsHeldByUserAtBank anonymous test)
- Test: AccountTest.scala lines 65-67 (getAccountsHeldByUser anonymous test)

```scala
// Test showing 401 for anonymous requests
val anonymousResponseGet = makeGetRequest(requestGet)
anonymousResponseGet.code should equal(401)
anonymousResponseGet.body.extract[ErrorMessage].message should equal(UserNotLoggedIn)
```

---

### 2. Entitlement Validation

**Implementation Location**: ResourceDoc entitlements specification in APIMethods510.scala

**What it checks**: User has the required entitlement(s) to access account listing

**Why it exists**: To control which users can view accounts at one bank vs. across all banks (feature-level access control)

**When it applies**: After authentication, before retrieving account data

**Who it affects**: Authenticated users without the required entitlements

**Entitlement Requirements**:
- **getAccountsHeldByUserAtBank** (GET /users/{userId}/banks/{bankId}/accounts-held):
  - Requires: `CanGetAccountsHeldAtOneBank` **OR** `CanGetAccountsHeldAtAnyBank`
- **getAccountsHeldByUser** (GET /users/{userId}/accounts-held):
  - Requires: `CanGetAccountsHeldAtAnyBank` only

**What happens when it fails**:
- HTTP Status: 403 Forbidden
- Error Message: "UserHasMissingRoles CanGetAccountsHeldAtOneBank or CanGetAccountsHeldAtAnyBank" (for bank-specific endpoint)
- Error Message: "UserHasMissingRoles CanGetAccountsHeldAtAnyBank" (for all-banks endpoint)

**Where it is enforced**: Entitlement checking layer before data retrieval

**Source Code Evidence**:
- Definition: ApiRole.scala lines 68-71
- ResourceDoc: APIMethods510.scala lines 813-814 (bank-specific) and line 860 (all-banks)
- Test: AccountTest.scala lines 54-57 (bank-specific test)
- Test: AccountTest.scala lines 73-76 (all-banks test)

```scala
// Entitlement definitions
case class CanGetAccountsHeldAtOneBank(requiresBankId: Boolean = true) extends ApiRole
case class CanGetAccountsHeldAtAnyBank(requiresBankId: Boolean = false) extends ApiRole

// Test showing 403 for users without entitlements
response.code should equal(403)
val errorMessage = UserHasMissingRoles + s"${CanGetAccountsHeldAtOneBank} or $CanGetAccountsHeldAtAnyBank"
response.body.extract[ErrorMessage].message contains errorMessage should be(true)
```

---

### 3. User ID Validation

**Implementation Location**: NewStyle.function.getUserByUserId() called in APIMethods510.scala lines 821 and 868

**What it checks**: The provided User ID exists in the system database

**Why it exists**: To prevent errors from non-existent or deleted users; ensures request references a valid user

**When it applies**: Early in the request processing, after authentication and entitlement checks

**Who it affects**: API consumers providing invalid or deleted user IDs

**What happens when it fails**:
- HTTP Status: 404 Not Found
- Error Message: "UserNotFoundByUserId"

**Where it is enforced**: User lookup service (Users.users.vend.getUserByUserId)

**Source Code Evidence**:
- Implementation: APIMethods510.scala lines 821, 868
- User lookup: LiftUsers.scala lines 80-82
- ResourceDoc: APIMethods510.scala lines 854-857

```scala
// getUserByUserId implementation
def getUserByUserId(userId : String) : Box[User] = {
  ResourceUser.find(By(ResourceUser.userId_, userId))
}

// Used in endpoint
(u, callContext) <- NewStyle.function.getUserByUserId(userId, cc.callContext)
```

---

### 4. Bank ID Validation (for getAccountsHeldByUserAtBank only)

**Implementation Location**: Implicit in NewStyle.function.getAccountsHeld(bankId, u, callContext)

**What it checks**: The provided Bank ID exists in the system

**Why it exists**: For the bank-specific endpoint, ensures the bank being queried actually exists

**When it applies**: Only for the bank-specific endpoint (GET /users/{userId}/banks/{bankId}/accounts-held)

**Who it affects**: API consumers querying accounts at a non-existent bank

**What happens when it fails**:
- HTTP Status: 404 Not Found
- Error Message: "BankNotFound"

**Where it is enforced**: Bank validation within getAccountsHeld function

**Source Code Evidence**:
- Implementation: APIMethods510.scala line 822
- ResourceDoc: APIMethods510.scala line 855

```scala
(availableAccounts, callContext) <- NewStyle.function.getAccountsHeld(bankId, u, callContext)
```

**Note**: This validation does NOT apply to getAccountsHeldByUser (all-banks endpoint) since no specific bank is specified.

---

### 5. Account Type Filter Validation

**Implementation Location**: AccountsHelper.scala lines 39-57 (filterWithAccountType function)

**What it checks**: 
1. The `account_type_filter_operation` parameter (if provided) must be either "INCLUDE" or "EXCLUDE"
2. Account types are filtered according to the operation and filter list

**Why it exists**: 
- To provide flexible filtering by account type (e.g., only savings accounts, exclude loan accounts)
- To prevent invalid filter operation values

**When it applies**: When optional query parameters `account_type_filter` and/or `account_type_filter_operation` are provided

**Who it affects**: API consumers using account type filtering

**Query Parameters**:
- `account_type_filter`: Comma-separated list of account types (e.g., "CURRENT,SAVINGS")
- `account_type_filter_operation`: Must be "INCLUDE" or "EXCLUDE" (default: "INCLUDE")

**What happens when it fails**:
- HTTP Status: 400 Bad Request
- Error Message: "InvalidFilterParameterFormat request parameter account_type_filter_operation must be either INCLUDE or EXCLUDE, current it is: {value}"

**Filter Logic**:
- **INCLUDE**: Returns only accounts with types in the filter list
- **EXCLUDE**: Returns accounts NOT in the filter list
- **No filter**: Returns all accounts

**Where it is enforced**: getFilteredCoreAccounts function in AccountsHelper.scala

**Source Code Evidence**:
- Implementation: AccountsHelper.scala lines 39-57
- Usage: APIMethods510.scala lines 825, 872
- Documentation: AccountsHelper.scala lines 21-30

```scala
private def filterWithAccountType(coreAccounts: List[CoreAccount], req: Req): List[CoreAccount] = {
  val filters = req.params.get("account_type_filter").map(_.flatMap(_.split(","))).getOrElse(Nil)
  val filtersOperation = req.params.get("account_type_filter_operation").flatMap(_.headOption).getOrElse("INCLUDE")
  
  val failMsg = s"""${InvalidFilterParameterFormat}request parameter account_type_filter_operation must be either INCLUDE or EXCLUDE, current it is: ${filtersOperation} """
  
  // validate account_type_filter_operation parameter
  unboxFullOrFail(tryo {
    assume(filtersOperation == "INCLUDE" || filtersOperation == "EXCLUDE")
  }, None, failMsg)
  
  coreAccounts.filter({ account =>
    (filters, filtersOperation) match {
      case (f, "INCLUDE") if f.nonEmpty => filters.contains(account.accountType)
      case (f, "EXCLUDE") if f.nonEmpty => !filters.contains(account.accountType)
      case _ => true
    }
  })
}
```

**Example URLs**:
- Include only CURRENT and SAVINGS: `?account_type_filter=CURRENT,SAVINGS&account_type_filter_operation=INCLUDE`
- Exclude LOAN accounts: `?account_type_filter=LOAN&account_type_filter_operation=EXCLUDE`

---

## Summary of All Validation Rules

| # | Validation Rule | HTTP Status on Failure | Error Message | Applies To |
|---|----------------|------------------------|---------------|------------|
| 1 | User Authentication | 401 | UserNotLoggedIn | Both endpoints |
| 2 | Entitlement (bank-specific) | 403 | UserHasMissingRoles CanGetAccountsHeldAtOneBank or CanGetAccountsHeldAtAnyBank | getAccountsHeldByUserAtBank |
| 2 | Entitlement (all-banks) | 403 | UserHasMissingRoles CanGetAccountsHeldAtAnyBank | getAccountsHeldByUser |
| 3 | User ID exists | 404 | UserNotFoundByUserId | Both endpoints |
| 4 | Bank ID exists | 404 | BankNotFound | getAccountsHeldByUserAtBank only |
| 5 | Account type filter operation | 400 | InvalidFilterParameterFormat | Both endpoints (when filter used) |

---

## Critical Validation Scenarios

### Scenario 1: Anonymous User Request
**Flow**: No authentication token provided
**Result**: 
- HTTP 401 Unauthorized
- Error: "UserNotLoggedIn"
**User Impact**: Must authenticate before accessing any account information

**Test Evidence**: AccountTest.scala lines 42-48, 65-67

---

### Scenario 2: Authenticated User Without Required Entitlement
**Flow**: User authenticated ✓ → Entitlement check ✗
**Result**:
- HTTP 403 Forbidden
- Error: "UserHasMissingRoles CanGetAccountsHeldAtOneBank or CanGetAccountsHeldAtAnyBank" (bank-specific)
- Error: "UserHasMissingRoles CanGetAccountsHeldAtAnyBank" (all-banks)
**User Impact**: Cannot access account listing; may need to request entitlement from administrator

**Test Evidence**: AccountTest.scala lines 50-58, 69-77

---

### Scenario 3: Invalid User ID
**Flow**: Authentication ✓ → Entitlement ✓ → User ID lookup ✗
**Result**:
- HTTP 404 Not Found
- Error: "UserNotFoundByUserId"
**User Impact**: Must provide valid User ID; may be trying to access deleted user's accounts

**Code Evidence**: APIMethods510.scala lines 821, 868; ResourceDoc line 856

---

### Scenario 4: Invalid Bank ID (Bank-Specific Endpoint Only)
**Flow**: Authentication ✓ → Entitlement ✓ → User ID ✓ → Bank ID lookup ✗
**Result**:
- HTTP 404 Not Found
- Error: "BankNotFound"
**User Impact**: Must provide valid Bank ID for bank-specific queries

**Code Evidence**: ResourceDoc line 855

---

### Scenario 5: Invalid Account Type Filter Operation
**Flow**: Authentication ✓ → Entitlement ✓ → User ID ✓ → Filter validation ✗
**Example**: `?account_type_filter_operation=INVALID`
**Result**:
- HTTP 400 Bad Request
- Error: "InvalidFilterParameterFormat request parameter account_type_filter_operation must be either INCLUDE or EXCLUDE, current it is: INVALID"
**User Impact**: Must use "INCLUDE" or "EXCLUDE" for account_type_filter_operation parameter

**Code Evidence**: AccountsHelper.scala lines 43-48

---

### Scenario 6: Successful Request With Filtering
**Flow**: Authentication ✓ → Entitlement ✓ → User ID ✓ → Bank ID ✓ → Filter applied ✓
**Example**: `?account_type_filter=CURRENT,SAVINGS&account_type_filter_operation=INCLUDE`
**Result**:
- HTTP 200 OK
- Returns only CURRENT and SAVINGS accounts held by the user
**User Impact**: Receives filtered list of accounts

---

### Scenario 7: User With No Accounts
**Flow**: All validations pass ✓ → No accounts found
**Result**:
- HTTP 200 OK
- Returns empty array `[]`
**User Impact**: Valid response; user simply has no accounts yet (may need onboarding)

---

## Important Notes

### NO View Permission Checks

**CRITICAL**: These Account Listing endpoints do **NOT** check view permissions. This is explicitly by design.

**Evidence**: ResourceDoc in APIMethods510.scala states:
> "Get Accounts held by the User if even the User has not been assigned the owner View yet.
> 
> Can be used to onboard the account to the API - since all other account and transaction endpoints require views to be assigned."

**Why**: These endpoints are specifically designed for account onboarding workflows where views may not yet be assigned. The "held" accounts represent accounts the user owns or has a relationship with, regardless of view assignments.

**Contrast**: Other OBP-API endpoints (like transaction history, balance inquiries) DO check view permissions, but Account Listing is intentionally exempt to support onboarding.

---

### Validation NOT Performed

Based on source code review, the following validations are **NOT** performed within these specific endpoints (though they may exist at framework/infrastructure level):

1. **Rate Limiting**: Not validated in endpoint code (may exist at API gateway level)
2. **Response Time Monitoring**: Not validated in endpoint (performance requirement, not validation rule)
3. **Caching TTL**: Not validated in endpoint (caching is transparent to validation logic)
4. **Pagination**: Not implemented in these endpoints (returns all accounts)
5. **View Permissions**: Explicitly NOT checked (by design for onboarding)
6. **Account Access Rights**: Not checked beyond entitlements (no granular per-account permissions)

---

## Error Messages Reference

| HTTP Status | Error Message | Meaning | User Action |
|-------------|---------------|---------|-------------|
| 401 | UserNotLoggedIn | No valid authentication token | Authenticate with valid credentials |
| 403 | UserHasMissingRoles CanGetAccountsHeldAtOneBank or CanGetAccountsHeldAtAnyBank | Missing required entitlement for bank-specific listing | Request entitlement from administrator |
| 403 | UserHasMissingRoles CanGetAccountsHeldAtAnyBank | Missing required entitlement for all-banks listing | Request entitlement from administrator |
| 404 | UserNotFoundByUserId | Provided User ID does not exist | Verify User ID is correct |
| 404 | BankNotFound | Provided Bank ID does not exist | Verify Bank ID is correct |
| 400 | InvalidFilterParameterFormat... | Invalid account_type_filter_operation value | Use "INCLUDE" or "EXCLUDE" only |
| 500 | UnknownError | Unexpected server error | Report to system administrator |

---

## Source Code Analysis References

This validation rules extraction is based on detailed analysis of the actual Open Bank Project API source code from https://github.com/OpenBankProject/OBP-API.git (commit 3b761070e on develop branch).

### Key Source Files Reviewed

1. **APIMethods510.scala** (lines 816-880)
   - Location: `obp-api/src/main/scala/code/api/v5_1_0/APIMethods510.scala`
   - Contains: Account Listing endpoint implementations (getAccountsHeldByUserAtBank, getAccountsHeldByUser)
   - Key findings: No view permission checks, entitlement requirements, user validation flow

2. **AccountTest.scala** (lines 1-100)
   - Location: `obp-api/src/test/scala/code/api/v5_1_0/AccountTest.scala`
   - Contains: Test scenarios showing validation behavior
   - Key findings: 401 for anonymous, 403 for missing entitlements, exact error messages

3. **AccountsHelper.scala** (lines 39-71)
   - Location: `obp-api/src/main/scala/code/api/v2_0_0/AccountsHelper.scala`
   - Contains: Account type filtering logic (filterWithAccountType, getFilteredCoreAccounts)
   - Key findings: INCLUDE/EXCLUDE validation, filter parameter handling

4. **ApiRole.scala** (lines 68-71)
   - Location: `obp-api/src/main/scala/code/api/util/ApiRole.scala`
   - Contains: Entitlement definitions
   - Key findings: CanGetAccountsHeldAtOneBank, CanGetAccountsHeldAtAnyBank definitions

5. **LiftUsers.scala** (lines 80-82)
   - Location: `obp-api/src/main/scala/code/users/LiftUsers.scala`
   - Contains: getUserByUserId implementation
   - Key findings: Simple database lookup by userId

### Validation Flow in Source Code

```
Request received
    ↓
1. Authentication Check (framework level)
   - If not authenticated → 401 UserNotLoggedIn
    ↓
2. Entitlement Check (ResourceDoc specification)
   - If missing required entitlement → 403 UserHasMissingRoles
    ↓
3. User ID Validation (getUserByUserId)
   - If user not found → 404 UserNotFoundByUserId
    ↓
4. Bank ID Validation (for bank-specific endpoint only)
   - If bank not found → 404 BankNotFound
    ↓
5. Retrieve Accounts (getAccountsHeld or getAccountsHeldByUser)
    ↓
6. Filter by Account Type (if filters provided)
   - If invalid operation → 400 InvalidFilterParameterFormat
    ↓
7. Return filtered account list (200 OK)
```

---

## Conclusion

The Account Listing functionality in OBP-API implements **5 core validation rules**:

1. **Authentication** - UserNotLoggedIn (401)
2. **Entitlements** - UserHasMissingRoles (403)
3. **User ID exists** - UserNotFoundByUserId (404)
4. **Bank ID exists** (bank-specific only) - BankNotFound (404)
5. **Account type filter operation** - InvalidFilterParameterFormat (400)

**Critical Distinction**: These endpoints are designed for account onboarding and do **NOT** check view permissions, unlike most other OBP-API endpoints. This allows users to discover their accounts before views are assigned.

All validation rules documented here are based on actual Scala source code analysis, not assumptions, ensuring accuracy and alignment with the real implementation.
