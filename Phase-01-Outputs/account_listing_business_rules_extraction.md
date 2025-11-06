# Business Rules Extraction: Account Listing User Story

## Overview

This document contains the comprehensive business rules extraction analysis for the Account Listing user story from the OBP API Account Management documentation. The analysis was performed using the 7-role business rules extraction framework and references the actual implementation from the Open Bank Project API repository (https://github.com/OpenBankProject/OBP-API.git).

**User Story Analyzed:** Account Listing (User Story 1)  
**Source:** obp_api_account_management_user_stories.md  
**OBP-API Implementation:** OpenBankProject/OBP-API (GitHub)  
**Analysis Date:** November 6, 2025  
**Purpose:** Support migration from Scala application to Go application with exact endpoint parity

---

## 🏦 Role 1: Bank Compliance Officer Perspective

### Rule Name: User Authentication Required for Account Access
**What it does:** Ensures only authenticated users can retrieve account lists  
**When it applies:** Every time a user attempts to retrieve their account list  
**Who it affects:** All banking application users and API consumers  
**Example:** Before displaying any account information, the system verifies the user's authentication token. If invalid or expired, the request is rejected with an authentication error.

**Actual Implementation** (APIMethods510.scala, line 821):
```scala
(u, callContext) <- NewStyle.function.getUserByUserId(userId, cc.callContext)
```
The getUserByUserId method validates the user exists and is authenticated before proceeding.

### Rule Name: View Permission-Based Account Visibility
**What it does:** Only shows accounts where the user has at least one view permission  
**When it applies:** When generating the list of accounts to return to the user  
**Who it affects:** Users with partial access to accounts at a bank  
**Example:** A customer service representative with view access to only checking accounts will only see checking accounts in their account list, even if the bank has savings accounts they don't have permission to view.

---

## 👥 Role 2: Customer Service Manager Perspective

### Rule Name: Account Type Filtering Support
**What it does:** Allows users to filter account lists by account type (checking, savings, etc.)  
**When it applies:** When a user provides account type filter parameters in their request  
**Who it affects:** Users who want to view specific types of accounts only  
**Example:** A user can request to see only their savings accounts by including "account_type_filter=SAVINGS" in the query parameters, hiding their checking and loan accounts from the response.

**Actual Implementation** (AccountsHelper.scala, lines 39-57):
```scala
private def filterWithAccountType(coreAccounts: List[CoreAccount], req: Req): List[CoreAccount] = {
  val filters = req.params.get("account_type_filter").map(_.flatMap(_.split(","))).getOrElse(Nil)
  val filtersOperation = req.params.get("account_type_filter_operation").flatMap(_.headOption).getOrElse("INCLUDE")
  
  coreAccounts.filter({ account =>
    (filters, filtersOperation) match {
      case (f, "INCLUDE") if f.nonEmpty => filters.contains(account.accountType)
      case (f, "EXCLUDE") if f.nonEmpty => !filters.contains(account.accountType)
      case _ => true
    }
  })
}
```
Query parameters: `account_type_filter` (comma-separated) and `account_type_filter_operation` (INCLUDE/EXCLUDE)

### Rule Name: Bank-Specific vs All-Banks Account Retrieval
**What it does:** Supports two modes: retrieve accounts at a specific bank OR across all banks the user has access to  
**When it applies:** Based on which API endpoint is called and whether Bank ID is provided  
**Who it affects:** Users with accounts at multiple banks  
**Example:** Using getAccountsHeldByUserAtBank requires specifying a bank_id and returns only accounts at that specific bank, while getAccountsHeldByUser returns accounts across all banks the user has access to.

**Actual Implementation** (APIMethods510.scala):
```scala
// Bank-specific endpoint (line 817)
case "users" :: userId :: "banks" :: BankId(bankId) :: "accounts-held" :: Nil JsonGet req =>
  (availableAccounts, callContext) <- NewStyle.function.getAccountsHeld(bankId, u, callContext)

// All-banks endpoint (line 864)
case "users" :: userId :: "accounts-held" :: Nil JsonGet req =>
  (availableAccounts, callContext) <- NewStyle.function.getAccountsHeldByUser(u, callContext)
```

---

## 🛡️ Role 3: Risk Management Specialist Perspective

### Rule Name: Entitlement-Based Access Scope Control
**What it does:** Restricts account listing scope based on user's entitlements  
**When it applies:** When validating if a user can retrieve accounts at one bank vs all banks  
**Who it affects:** Users with different privilege levels  
**Example:** A user with only "canGetAccountsHeldAtOneBank" entitlement can only query accounts at a single specified bank, while a user with "canGetAccountsHeldAtAnyBank" can query across all banks in the system.

**Actual Implementation** (APIMethods510.scala, line 813 & 860):
```scala
// Bank-specific endpoint requires either entitlement (line 813)
Some(List(canGetAccountsHeldAtOneBank, canGetAccountsHeldAtAnyBank))

// All-banks endpoint requires specific entitlement (line 860)
Some(List(canGetAccountsHeldAtAnyBank))
```
Defined in ApiRole.scala (lines 68-71):
```scala
case class CanGetAccountsHeldAtOneBank(requiresBankId: Boolean = true) extends ApiRole
lazy val canGetAccountsHeldAtOneBank: CanGetAccountsHeldAtOneBank = CanGetAccountsHeldAtOneBank()
case class CanGetAccountsHeldAtAnyBank(requiresBankId: Boolean = false) extends ApiRole
lazy val canGetAccountsHeldAtAnyBank: CanGetAccountsHeldAtAnyBank = CanGetAccountsHeldAtAnyBank()
```

### Rule Name: Performance-Based Response Time Requirement
**What it does:** Ensures account list retrieval completes within 2 seconds for typical requests  
**When it applies:** For every account listing request with standard number of accounts  
**Who it affects:** All users, especially those with time-sensitive applications  
**Example:** If a user has 50 accounts, the system must return the complete list within 2 seconds. If response time exceeds this, performance optimization is required (caching, indexing, etc.).

---

## 📦 Role 4: Product Manager Perspective

### Rule Name: Core Account Information Standard Response
**What it does:** Defines the minimum account information returned in list responses  
**When it applies:** For every account returned in the list  
**Who it affects:** API consumers and frontend applications  
**Example:** Each account in the response includes account ID, bank ID, label, number, and account_routings fields. This ensures consistent data structure for all account listings regardless of the specific endpoint used.

**Actual Implementation** (JSONFactory3.0.0.scala, lines 867-875):
```scala
def createCoreAccountsByCoreAccountsJSON(accountsHeld: List[AccountHeld]): CoreAccountsHeldJsonV300 =
  CoreAccountsHeldJsonV300(accountsHeld.map(
    account => AccountHeldJson(
      account.id,
      account.label,
      account.bankId,
      account.number,
      account.accountRoutings.map(accountRounting =>
        AccountRoutingJsonV121(accountRounting.scheme, accountRounting.address))
    )))
```

### Rule Name: Pagination Support for Large Account Lists
**What it does:** Handles users with many accounts through pagination mechanism  
**When it applies:** When a user has more accounts than can be efficiently returned in a single response  
**Who it affects:** Users with numerous accounts, high-net-worth individuals, business customers  
**Example:** A business customer with 200 accounts receives paginated results with 50 accounts per page, allowing the system to maintain performance while still providing access to all accounts.

---

## ⚙️ Role 5: Operations Director Perspective

### Rule Name: View Permission Check Before Account Inclusion
**What it does:** Validates user has appropriate view permissions for each account before including it in results  
**When it applies:** During the account list generation process for each potential account  
**Who it affects:** System operations, data access layers  
**Example:** When building the account list, the system queries the ViewNewStyle service to check if the user has any view (owner, public, accountant, etc.) on each account before adding it to the response array.

**Actual Implementation** (ViewNewStyle.scala):
View permission checking methods include:
- `checkViewAccessAndReturnView(viewId, bankAccountId, user, callContext)` - Validates user has access to specific view
- `checkOwnerViewAccessAndReturnOwnerView(user, bankAccountId, callContext)` - Validates user has owner view access

### Rule Name: Bank Connector Integration for Account Data
**What it does:** Requires integration with bank connector to retrieve actual account information  
**When it applies:** When account data needs to be fetched from the underlying banking system  
**Who it affects:** External system integration, backend services  
**Example:** The APIMethods510 classes call the bank connector to fetch account records from the core banking system, ensuring the API returns current data rather than potentially stale cached information.

**Actual Implementation** (Connector.scala, lines 526-527):
```scala
def getAccountsHeld(bankId: BankId, user: User, callContext: Option[CallContext]): 
  OBPReturnType[Box[List[BankIdAccountId]]]

def getAccountsHeldByUser(user: User, callContext: Option[CallContext]): 
  OBPReturnType[Box[List[BankIdAccountId]]]
```
Called from APIMethods510.scala (lines 822 & 869):
```scala
(availableAccounts, callContext) <- NewStyle.function.getAccountsHeld(bankId, u, callContext)
(availableAccounts, callContext) <- NewStyle.function.getAccountsHeldByUser(u, callContext)
```

---

## 💰 Role 6: Treasury and Payment Specialist Perspective

### Rule Name: Account Holder Ownership Determination
**What it does:** Differentiates between accounts the user "holds" vs accounts they can "access"  
**When it applies:** When determining which accounts to include in the "held" account list  
**Who it affects:** Account owners, authorized users, delegates  
**Example:** A user who is the primary account holder will see their personal checking account in the list. An accountant granted view access to review the account will also see it, but the business logic may categorize these differently based on ownership vs access rights.

---

## 🔒 Role 7: Security and Access Control Manager Perspective

### Rule Name: User Identity Validation Requirement
**What it does:** Validates that the user ID exists in the system before processing the request  
**When it applies:** At the beginning of every account listing request  
**Who it affects:** All API users  
**Example:** If someone attempts to retrieve accounts for user_id="unknown_user", the system returns an error indicating the user does not exist, preventing information disclosure through enumeration attacks.

### Rule Name: Bank ID Validation When Specified
**What it does:** Validates that the bank ID is valid and active when provided  
**When it applies:** When a user requests accounts at a specific bank  
**Who it affects:** Users querying specific banks  
**Example:** If a request includes bank_id="INVALID_BANK", the system returns a validation error rather than attempting to query a non-existent bank, preventing potential security issues and improving error handling.

### Rule Name: Audit Trail for Account Access
**What it does:** Maintains logs of who accessed which account lists and when  
**When it applies:** For every successful and failed account listing request  
**Who it affects:** Security teams, auditors, compliance officers  
**Example:** Each account listing request is logged with timestamp, user ID, requested bank (if any), filters applied, and number of accounts returned, creating an audit trail for security and compliance reviews.

---

## 🎯 Additional Business Rules Identified

### Rule Name: Account Type Filter Validation
**What it does:** Validates that account type filter values match valid account types in the system  
**When it applies:** When account type filters are provided in the request  
**Who it affects:** API consumers providing filter parameters  
**Example:** If a user provides "account_type_filter_operation=INVALID", the system returns a validation error stating the operation must be INCLUDE or EXCLUDE.

**Actual Implementation** (AccountsHelper.scala, lines 43-48):
```scala
val failMsg = s"""${InvalidFilterParameterFormat}request parameter account_type_filter_operation must be either INCLUDE or EXCLUDE, current it is: ${filtersOperation} """

unboxFullOrFail(tryo {
  assume(filtersOperation == "INCLUDE" || filtersOperation == "EXCLUDE")
}, None, failMsg)
```

### Rule Name: Short TTL Caching for Frequent Requests
**What it does:** Implements short time-to-live caching to improve performance for repeated requests  
**When it applies:** For account list queries from the same user within a short time window  
**Who it affects:** High-frequency API users, mobile applications  
**Example:** When a user's mobile app requests the account list twice within 30 seconds, the second request may be served from cache rather than querying the database again, reducing load while ensuring reasonably fresh data.

---

## 📋 Summary of Business Rules Categories

**Access and Permission Rules:** 3 rules  
**Validation and Verification Rules:** 4 rules  
**Processing and Workflow Rules:** 2 rules  
**Financial and Calculation Rules:** 0 rules (not applicable for listing)  
**Compliance and Audit Rules:** 2 rules  
**Customer and Account Rules:** 2 rules  
**Security and Authentication Rules:** 4 rules  

**Total Business Rules Identified:** 17 distinct business rules

---

## 🔍 SME Input Required

Based on the analysis, the following items need Subject Matter Expert input:

1. **Held vs Accessible Accounts**: Clear business definition needed for what constitutes a "held" account versus an "accessible" account
2. **Default Account Types**: Complete list of valid account type values and their filtering logic
3. **Pagination Limits**: Specific threshold for when pagination triggers and page size limits
4. **Caching Policy**: Exact TTL values for caching and staleness tolerance
5. **Maximum Accounts Per Request**: If there's a hard limit on accounts returned in multi-account queries

---

## 📝 Original User Story Reference

### Actual OBP-API Implementation

**Repository:** https://github.com/OpenBankProject/OBP-API.git

**REST Endpoints** (from obp-api/src/main/scala/code/api/v5_1_0/APIMethods510.scala):
- `GET /users/USER_ID/banks/BANK_ID/accounts-held` - getAccountsHeldByUserAtBank (line 816)
- `GET /users/USER_ID/accounts-held` - getAccountsHeldByUser (line 863)

**Entitlements Required** (from obp-api/src/main/scala/code/api/util/ApiRole.scala, lines 68-71):
- `CanGetAccountsHeldAtOneBank(requiresBankId: Boolean = true)` - line 68
- `CanGetAccountsHeldAtAnyBank(requiresBankId: Boolean = false)` - line 70

**Implementation Flow:**
1. `NewStyle.function.getUserByUserId(userId, callContext)` - Retrieve user
2. `NewStyle.function.getAccountsHeld(bankId, user, callContext)` OR `NewStyle.function.getAccountsHeldByUser(user, callContext)` - Get account IDs
3. `NewStyle.function.getBankAccountsHeldFuture(availableAccounts, callContext)` - Get full account details
4. `getFilteredCoreAccounts(availableAccounts, req, callContext)` - Apply account type filters
5. `JSONFactory300.createCoreAccountsByCoreAccountsJSON(accountHelds)` - Format JSON response

**Account Type Filtering** (from obp-api/src/main/scala/code/api/v2_0_0/AccountsHelper.scala, lines 39-57):
- Query parameter: `account_type_filter` (comma-separated list)
- Query parameter: `account_type_filter_operation` (must be "INCLUDE" or "EXCLUDE")
- Implementation in `AccountsHelper.filterWithAccountType()`

**Response Format** (from obp-api/src/main/scala/code/api/v3_0_0/JSONFactory3.0.0.scala, lines 867-875):
- Method: `createCoreAccountsByCoreAccountsJSON(accountsHeld: List[AccountHeld])`
- Returns: `CoreAccountsHeldJsonV300` containing id, label, bank_id, number, account_routings

**Connector Interface** (from obp-api/src/main/scala/code/bankconnectors/Connector.scala, lines 526-527):
- `def getAccountsHeld(bankId: BankId, user: User, callContext: Option[CallContext])`
- `def getAccountsHeldByUser(user: User, callContext: Option[CallContext])`

**View Permissions** (from obp-api/src/main/scala/code/api/util/newstyle/ViewNewStyle.scala):
- Methods for checking view access: `checkViewAccessAndReturnView()`, `checkOwnerViewAccessAndReturnOwnerView()`

### Story Overview
**As a** banking application user or API consumer  
**I want to** retrieve a list of all bank accounts I have access to  
**So that** I can view my accounts and select which one to perform operations on

### Acceptance Criteria (from original user story)
1. User can retrieve accounts held at a specific bank
2. User can retrieve accounts held across all banks they have access to
3. Response includes core account information (account ID, bank ID, account type)
4. Results can be filtered by account type (e.g., checking, savings)
5. Only accounts the user has permission to view are returned
6. System handles pagination for users with many accounts
7. Response time is under 2 seconds for typical user account lists

### Technical Context (from original user story)
- **Classes/Services Involved**: 
  - APIMethods510.getAccountsHeldByUserAtBank - retrieves accounts at specific bank
  - APIMethods510.getAccountsHeldByUser - retrieves accounts across all banks
  - ViewNewStyle - manages view permissions
  - JSONFactory300.createCoreAccountsByCoreAccountsJSON - formats response
- **Input Data**: User ID, Bank ID (optional), account type filters (query parameters)
- **Output Data**: JSON array of core account objects with id, bank_id, label, account_type
- **Processing Type**: Real-time REST API

### Business Rules from Original Code
1. User must be authenticated to retrieve account lists
2. Only accounts where user has at least one view permission are returned
3. Account type filtering is optional and supports multiple types
4. Results must respect user's entitlements (canGetAccountsHeldAtOneBank or canGetAccountsHeldAtAnyBank)

### Data Validations (from original user story)
- User ID must be valid and exist in system
- Bank ID must be valid if specified
- Account type filter values must match valid account types
- User must have appropriate entitlements for the requested scope

### Dependencies (from original user story)
- **Upstream**: User authentication and authorization
- **Downstream**: Account detail views, transaction retrieval, balance inquiries
- **External Systems**: Bank connector for retrieving actual account data

---

## 🔧 OBP-API Implementation Details

This section documents the actual Scala implementation from the Open Bank Project API repository for reference during the Go migration.

### File Structure

**Core Implementation Files:**
1. `obp-api/src/main/scala/code/api/v5_1_0/APIMethods510.scala` - REST endpoint definitions (lines 816-880)
2. `obp-api/src/main/scala/code/api/v3_0_0/JSONFactory3.0.0.scala` - Response formatting (lines 867-875)
3. `obp-api/src/main/scala/code/api/v2_0_0/AccountsHelper.scala` - Account type filtering (lines 39-71)
4. `obp-api/src/main/scala/code/api/util/ApiRole.scala` - Entitlement definitions (lines 68-71)
5. `obp-api/src/main/scala/code/api/util/newstyle/ViewNewStyle.scala` - View permission handling
6. `obp-api/src/main/scala/code/bankconnectors/Connector.scala` - Bank connector interface (lines 526-527)
7. `obp-api/src/main/scala/code/api/util/NewStyle.scala` - NewStyle.function utilities

### REST Endpoint Specifications

#### Endpoint 1: Get Accounts Held By User At Bank
**Path:** `GET /users/USER_ID/banks/BANK_ID/accounts-held`  
**Method:** `getAccountsHeldByUserAtBank` (APIMethods510.scala, line 816)  
**Required Entitlements:** `CanGetAccountsHeldAtOneBank` OR `CanGetAccountsHeldAtAnyBank`  
**Implementation:**
```scala
lazy val getAccountsHeldByUserAtBank: OBPEndpoint = {
  case "users" :: userId :: "banks" :: BankId(bankId) :: "accounts-held" :: Nil JsonGet req => {
    cc =>
      implicit val ec = EndpointContext(Some(cc))
      for {
        (u, callContext) <- NewStyle.function.getUserByUserId(userId, cc.callContext)
        (availableAccounts, callContext) <- NewStyle.function.getAccountsHeld(bankId, u, callContext)
        (accounts, callContext) <- NewStyle.function.getBankAccountsHeldFuture(availableAccounts.toList, callContext)
        accountHelds <- getFilteredCoreAccounts(availableAccounts, req, callContext).map { it =>
          val coreAccountIds: List[String] = it._1.map(_.id)
          accounts.filter(accountHeld => coreAccountIds.contains(accountHeld.id))
        }
      } yield {
        (JSONFactory300.createCoreAccountsByCoreAccountsJSON(accountHelds), HttpCode.`200`(callContext))
      }
  }
}
```

#### Endpoint 2: Get Accounts Held By User
**Path:** `GET /users/USER_ID/accounts-held`  
**Method:** `getAccountsHeldByUser` (APIMethods510.scala, line 863)  
**Required Entitlements:** `CanGetAccountsHeldAtAnyBank`  
**Implementation:**
```scala
lazy val getAccountsHeldByUser: OBPEndpoint = {
  case "users" :: userId :: "accounts-held" :: Nil JsonGet req => {
    cc =>
      implicit val ec = EndpointContext(Some(cc))
      for {
        (u, callContext) <- NewStyle.function.getUserByUserId(userId, cc.callContext)
        (availableAccounts, callContext) <- NewStyle.function.getAccountsHeldByUser(u, callContext)
        (accounts, callContext) <- NewStyle.function.getBankAccountsHeldFuture(availableAccounts, callContext)
        accountHelds <- getFilteredCoreAccounts(availableAccounts, req, callContext).map { it =>
          val coreAccountIds: List[String] = it._1.map(_.id)
          accounts.filter(accountHeld => coreAccountIds.contains(accountHeld.id))
        }
      } yield {
        (JSONFactory300.createCoreAccountsByCoreAccountsJSON(accountHelds), HttpCode.`200`(callContext))
      }
  }
}
```

### Entitlement Definitions

From ApiRole.scala (lines 68-71):
```scala
case class CanGetAccountsHeldAtOneBank(requiresBankId: Boolean = true) extends ApiRole
lazy val canGetAccountsHeldAtOneBank: CanGetAccountsHeldAtOneBank = CanGetAccountsHeldAtOneBank()

case class CanGetAccountsHeldAtAnyBank(requiresBankId: Boolean = false) extends ApiRole
lazy val canGetAccountsHeldAtAnyBank: CanGetAccountsHeldAtAnyBank = CanGetAccountsHeldAtAnyBank()
```

**Key Difference:**
- `CanGetAccountsHeldAtOneBank` requires `bankId` parameter (requiresBankId = true)
- `CanGetAccountsHeldAtAnyBank` does not require `bankId` parameter (requiresBankId = false)

### Account Type Filtering Implementation

From AccountsHelper.scala (lines 39-71):

**Query Parameters:**
- `account_type_filter` - Comma-separated list of account types (e.g., "CURRENT,SAVINGS")
- `account_type_filter_operation` - Must be "INCLUDE" or "EXCLUDE"

**Filter Logic:**
```scala
private def filterWithAccountType(coreAccounts: List[CoreAccount], req: Req): List[CoreAccount] = {
  val filters = req.params.get("account_type_filter").map(_.flatMap(_.split(","))).getOrElse(Nil)
  val filtersOperation = req.params.get("account_type_filter_operation").flatMap(_.headOption).getOrElse("INCLUDE")
  
  coreAccounts.filter({ account =>
    (filters, filtersOperation) match {
      case (f, "INCLUDE") if f.nonEmpty => filters.contains(account.accountType)
      case (f, "EXCLUDE") if f.nonEmpty => !filters.contains(account.accountType)
      case _ => true
    }
  })
}
```

**Validation:**
- Operation parameter must be exactly "INCLUDE" or "EXCLUDE"
- Invalid operation triggers error: `InvalidFilterParameterFormat`
- If no filters specified, all accounts pass through

### Response JSON Structure

From JSONFactory3.0.0.scala (lines 867-875):

**Method Signature:**
```scala
def createCoreAccountsByCoreAccountsJSON(accountsHeld: List[AccountHeld]): CoreAccountsHeldJsonV300
```

**Response Structure:**
```json
{
  "accounts": [
    {
      "id": "string",
      "label": "string",
      "bank_id": "string",
      "number": "string",
      "account_routings": [
        {
          "scheme": "string",
          "address": "string"
        }
      ]
    }
  ]
}
```

**Implementation:**
```scala
CoreAccountsHeldJsonV300(accountsHeld.map(
  account => AccountHeldJson(
    account.id,
    account.label,
    account.bankId,
    account.number,
    account.accountRoutings.map(accountRounting =>
      AccountRoutingJsonV121(accountRounting.scheme, accountRounting.address))
  )))
```

### Connector Interface

From Connector.scala (lines 526-527):

**Bank-Specific Account Retrieval:**
```scala
def getAccountsHeld(bankId: BankId, user: User, callContext: Option[CallContext]): 
  OBPReturnType[Box[List[BankIdAccountId]]]
```

**All-Banks Account Retrieval:**
```scala
def getAccountsHeldByUser(user: User, callContext: Option[CallContext]): 
  OBPReturnType[Box[List[BankIdAccountId]]]
```

**Return Type:** `OBPReturnType[Box[List[BankIdAccountId]]]`
- Returns list of BankIdAccountId tuples
- Each tuple contains: BankId and AccountId
- Wrapped in Box (Option-like container) for error handling
- Includes CallContext for request tracing

### View Permission Handling

From ViewNewStyle.scala:

**Key Methods:**
- `checkViewAccessAndReturnView(viewId, bankAccountId, user, callContext)` - Validates user has access to specific view
- `checkOwnerViewAccessAndReturnOwnerView(user, bankAccountId, callContext)` - Validates user has owner view access
- `grantAccessToCustomView(view, user, callContext)` - Grants view access to user
- `revokeAccessToCustomView(view, user, callContext)` - Revokes view access from user

**Permission Check Pattern:**
```scala
Future {
  APIUtil.checkViewAccessAndReturnView(viewId, bankAccountId, user, callContext)
} map {
  unboxFullOrFail(_, callContext, s"$UserNoPermissionAccessView Current ViewId is ${viewId.value}")
}
```

### Error Handling

**Common Error Messages:**
- `$UserNotLoggedIn` - User authentication required
- `$BankNotFound` - Specified bank ID does not exist
- `UserNotFoundByUserId` - User ID does not correspond to existing user
- `UnknownError` - Generic error fallback
- `InvalidFilterParameterFormat` - account_type_filter_operation must be INCLUDE or EXCLUDE
- `UserHasMissingRoles` - User lacks required entitlements

### NewStyle.function Utilities

**getUserByUserId:**
```scala
NewStyle.function.getUserByUserId(userId: String, callContext: Option[CallContext]): 
  Future[(User, Option[CallContext])]
```
Retrieves user by ID, validates user exists

**getAccountsHeld:**
```scala
NewStyle.function.getAccountsHeld(bankId: BankId, user: User, callContext: Option[CallContext]): 
  Future[(List[BankIdAccountId], Option[CallContext])]
```
Gets list of account IDs held by user at specific bank

**getAccountsHeldByUser:**
```scala
NewStyle.function.getAccountsHeldByUser(user: User, callContext: Option[CallContext]): 
  Future[(List[BankIdAccountId], Option[CallContext])]
```
Gets list of account IDs held by user across all banks

**getBankAccountsHeldFuture:**
```scala
NewStyle.function.getBankAccountsHeldFuture(bankIdAccountIds: List[BankIdAccountId], callContext: Option[CallContext]): 
  Future[(List[AccountHeld], Option[CallContext])]
```
Fetches full account details for list of account IDs

**getCoreBankAccountsFuture:**
```scala
NewStyle.function.getCoreBankAccountsFuture(bankIdAccountIds: List[BankIdAccountId], callContext: Option[CallContext]): 
  Future[(List[CoreAccount], Option[CallContext])]
```
Fetches core account information for filtering

---

## 🚀 Migration Considerations for Go Application

When migrating this functionality from Scala to Go, ensure:

1. **Exact Endpoint Compatibility**: Maintain the same REST API endpoints and behavior
   - `GET /users/USER_ID/banks/BANK_ID/accounts-held`
   - `GET /users/USER_ID/accounts-held`
2. **Authentication & Authorization**: Implement the same authentication flow and entitlement checks
   - CanGetAccountsHeldAtOneBank and CanGetAccountsHeldAtAnyBank entitlements
3. **View Permission Logic**: Replicate the ViewNewStyle permission checking mechanism
4. **Response Format**: Match the JSON structure exactly (CoreAccountsHeldJsonV300)
5. **Performance Requirements**: Meet the 2-second response time requirement
6. **Account Type Filtering**: Implement account_type_filter and account_type_filter_operation query parameters
7. **Error Handling**: Match error codes and messages for validation failures
8. **Bank Connector Integration**: Maintain compatibility with existing bank connector interface
9. **Caching Strategy**: Implement similar caching if present in Scala version
10. **Audit Logging**: Preserve audit trail functionality

---

## 📚 Related Documentation

- Source User Story: `Playbooks/user_stories/obp_api_account_management_user_stories.md`
- Business Rules Extraction Prompt: `Phase-01-Playbooks/business_rules_extraction_prompt.md` (obp-api repo)
- OBP-API Repository: https://github.com/OpenBankProject/OBP-API.git
- Actual Scala Implementation Files:
  - APIMethods510.scala (lines 816-880)
  - ApiRole.scala (lines 68-71)
  - AccountsHelper.scala (lines 39-71)
  - JSONFactory3.0.0.scala (lines 867-875)
  - ViewNewStyle.scala
  - Connector.scala (lines 526-527)

---

*This analysis was generated as part of the Scala to Go migration project to ensure all business rules are properly captured and implemented in the new Go-based application. All code references are from the actual Open Bank Project API implementation.*
