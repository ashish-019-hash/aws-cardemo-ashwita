# Screen Flow Documentation: Account Listing

## OBP-API Version: v5.1.0

Applied from: screen-flow-extraction-prompt.md (OBP-API Phase-01-Playbooks)
Source: Account Listing User Story (OBP-API Account Management User Stories)
Based on: Official OpenBankProject/OBP-API repository
Date: November 06, 2025

---

## Flow Name
**Account Listing and Selection Flow**

## Flow Description
This flow enables banking application users and API consumers to retrieve and view a comprehensive list of all bank accounts they have permission to access through the OBP-API v5.1.0. The flow supports both single-bank and multi-bank account retrieval, allowing users to identify and select accounts for subsequent banking operations.

## API Endpoints

**Primary Endpoints (OBP-API v5.1.0):**
1. **Single Bank Account Retrieval:**
   - Endpoint: `GET /obp/v5.1.0/users/USER_ID/banks/BANK_ID/accounts-held`
   - Implementation: `APIMethods510.getAccountsHeldByUserAtBank`
   - Purpose: Get accounts held by user at a specific bank

2. **Multi-Bank Account Retrieval:**
   - Endpoint: `GET /obp/v5.1.0/users/USER_ID/accounts-held`
   - Implementation: `APIMethods510.getAccountsHeldByUser`
   - Purpose: Get accounts held by user across all banks

**Response Format:**
- Response Type: `CoreAccountsHeldJsonV300`
- JSON Factory: `JSONFactory300.createCoreAccountsByCoreAccountsJSON`
- Structure:
```json
{
  "accounts": [
    {
      "id": "account_id",
      "label": "account_label",
      "bank_id": "bank_id",
      "number": "account_number",
      "account_routings": [
        {
          "scheme": "routing_scheme",
          "address": "routing_address"
        }
      ]
    }
  ]
}
```

## Starting Point
**Entry Points:**
1. **Direct API Call**: User or application makes authenticated REST API request to OBP-API v5.1.0 endpoints
2. **Dashboard Navigation**: User accesses account listing through main banking dashboard
3. **Post-Authentication**: System redirects to account listing after successful login
4. **Deep Link**: External system or email provides direct link to account listing

**Prerequisites:**
- User must be authenticated using one of the supported mechanisms: OAuth2 (Keycloak, OBPOIDC), OAuth1a, or DirectLogin
- User must have at least one account held at a bank
- User must possess the required entitlements:
  - For single bank endpoint: `canGetAccountsHeldAtOneBank` OR `canGetAccountsHeldAtAnyBank`
  - For all banks endpoint: `canGetAccountsHeldAtAnyBank`

## Step-by-Step Flow

### Step 1: Authentication Checkpoint
**Screen Purpose:** Verify user identity and authorization

**User Entry:** User attempts to access account listing feature

**Authentication Mechanisms (OBP-API):**
- **OAuth2**: Using Keycloak or OBP OIDC providers
- **OAuth1a**: Traditional OAuth 1.0a flow
- **DirectLogin**: Direct login with credentials

**Information Display:**
- Login screen (if not authenticated)
- Authentication status indicator
- OAuth authorization prompt (if using OAuth)

**User Input:**
- User credentials (username/password for DirectLogin)
- OAuth consent (for OAuth flows)
- Multi-factor authentication (if enabled)

**Actions Available:**
- Submit credentials
- Authorize OAuth application
- Request password reset
- Cancel and return to home

**Validation (Implementation Flow):**
1. Check authentication token validity
2. Retrieve user via `NewStyle.function.getUserByUserId`
3. Verify user has required entitlements:
   - For `/users/USER_ID/banks/BANK_ID/accounts-held`: `canGetAccountsHeldAtOneBank` OR `canGetAccountsHeldAtAnyBank`
   - For `/users/USER_ID/accounts-held`: `canGetAccountsHeldAtAnyBank`

**Success Path:** → Step 2: Account Scope Selection

**Error Codes:**
- **401 (UserNotLoggedIn)**: User is not authenticated
- **403 (UserHasMissingRoles)**: User lacks required entitlements
- **400 (UserNotFoundByUserId)**: User ID is invalid

---

### Step 2: Account Scope Selection
**Screen Purpose:** Allow user to specify the scope of account retrieval

**User Entry:** Successfully authenticated user

**Information Display:**
- Available banks user has access to
- Scope selection options (single bank vs. all banks)
- Account type filter options

**User Input:**
- Bank selection (optional - if requesting single bank)
- Account type filters (optional via query parameters)
- Filter operation selection (INCLUDE or EXCLUDE)

**Available Query Parameters:**
- `account_type_filter`: Comma-separated list of account types (e.g., "330,CURRENT+PLUS")
- `account_type_filter_operation`: Filter operation ("INCLUDE" or "EXCLUDE")
- Example: `?account_type_filter=330,CURRENT+PLUS&account_type_filter_operation=INCLUDE`

**Actions Available:**
- Select specific bank (calls `/users/USER_ID/banks/BANK_ID/accounts-held`)
- Choose "All Banks" option (calls `/users/USER_ID/accounts-held`)
- Apply account type filters
- Submit request

**Validation:**
- Bank ID must be valid if specified
- Account type filter operation must be either "INCLUDE" or "EXCLUDE"
- User must have entitlements matching requested scope

**Implementation Flow:**
1. Parse query parameters for filters
2. Call appropriate endpoint based on scope
3. Apply `AccountsHelper.getFilteredCoreAccounts` for filtering

**Success Path:** → Step 3: Account List Display

**Error Codes:**
- **400 (BankNotFound)**: Invalid bank ID specified
- **400 (InvalidFilterParameterFormat)**: Invalid filter operation value

---

### Step 3: Account List Display
**Screen Purpose:** Present comprehensive list of accessible accounts

**User Entry:** Valid account list request submitted

**Implementation Flow (OBP-API):**
1. `NewStyle.function.getUserByUserId` - Validate and retrieve user
2. `NewStyle.function.getAccountsHeld` (single bank) or `NewStyle.function.getAccountsHeldByUser` (all banks)
3. `NewStyle.function.getBankAccountsHeldFuture` - Retrieve full account details
4. `getFilteredCoreAccounts` - Apply account type filtering if specified
5. `JSONFactory300.createCoreAccountsByCoreAccountsJSON` - Format response as CoreAccountsHeldJsonV300

**Information Display (from CoreAccountsHeldJsonV300):**
- List of accounts with:
  - **id**: Account ID
  - **label**: Account label/name
  - **bank_id**: Bank ID
  - **number**: Account number
  - **account_routings**: Array of routing information
    - scheme: Routing scheme
    - address: Routing address
- Filter indicators (active filters shown)
- Result count

**Note on Pagination:**
- Current implementation returns all matching accounts in a single response
- No explicit pagination mechanism found in v5.1.0 implementation

**User Input:**
- Sort preferences (by name, type, bank)
- Account selection for details

**Actions Available:**
- Click account to view details
- Apply additional filters (modify query parameters)
- Refresh account list
- Export account list
- Return to dashboard

**Special Characteristics:**
- Accounts returned are "held" accounts - can be used for onboarding even if the user hasn't been assigned owner view yet
- This enables initial account setup and view assignment
- Results respect user's entitlements and filter settings

**Success Path:** → Step 4: Account Selection or Step 5: Return to Dashboard

**Error Codes:**
- **500 (UnknownError)**: Internal server error during retrieval

---

### Step 4: Account Selection (Optional)
**Screen Purpose:** Allow user to select specific account for operations

**User Entry:** User clicks on account from listing

**Information Display:**
- Selected account highlighted
- Quick actions menu for selected account
- Account summary information

**User Input:**
- Account selection
- Desired action choice

**Actions Available:**
- View full account details
- Check balance
- View transactions
- Initiate payment/transfer
- Update account settings
- Return to account list

**Validation:**
- User has appropriate view permissions for selected account
- Selected account is active and accessible

**Success Path:** → Navigate to selected operation (details, balance, etc.)

**Error Path:** → Access denied message with explanation

---

### Step 5: Completion - Return to Dashboard
**Screen Purpose:** Allow user to exit account listing flow

**User Entry:** User completes account viewing or selection

**Actions Available:**
- Return to main dashboard
- Log out
- Navigate to other banking features

---

## Alternative Paths

### Path A: No Accounts Available
**Trigger:** User has no accounts or no view permissions

**Flow:**
1. Display empty state screen
2. Show message: "No accounts available"
3. Provide options:
   - Contact support for account access
   - Request new account creation (if entitled)
   - Return to dashboard

### Path B: Filtered Results Empty
**Trigger:** Applied filters result in no matching accounts

**Flow:**
1. Display empty results with active filters shown
2. Show message: "No accounts match your criteria"
3. Provide options:
   - Clear filters
   - Modify filter criteria
   - View all accounts

### Path C: Session Timeout During Listing
**Trigger:** User session expires while viewing account list

**Flow:**
1. Display session timeout notification
2. Redirect to authentication screen
3. After re-authentication, return to account listing with previous filters preserved

### Path D: Large Account Lists
**Trigger:** User has a large number of accounts

**Flow:**
1. All matching accounts are returned in a single response (no server-side pagination in v5.1.0)
2. Show total account count
3. Client-side handling options:
   - Implement virtual scrolling for performance
   - Use account type filters to reduce result set
   - Display loading indicator during retrieval
4. Maintain filter state

### Path E: Performance Degradation
**Trigger:** Account retrieval exceeds 2-second response time threshold

**Flow:**
1. Display loading indicator
2. Show progress message
3. Options:
   - Continue waiting
   - Cancel request
   - Apply filters to reduce result set

---

## User Types

### Regular Users (Banking Customers)
- Can view accounts they own or have been granted access to
- Limited to personal and jointly-held accounts
- Access through consumer banking interface

### Business Customers
- Can view business accounts associated with their organization
- May have multiple signatories with different permission levels
- Access through business banking interface

### Bank Administrators
- Can view all accounts at their bank (with appropriate entitlements)
- Access for support and administrative purposes
- Enhanced filtering and search capabilities

### Third-Party Developers/API Consumers
- Access accounts through REST API endpoints
- Require OAuth tokens with appropriate scopes
- Subject to rate limiting and API quotas

### Customer Service Representatives
- Can view customer accounts to provide support
- Access logged for audit purposes
- Limited to view permissions only (no transaction capabilities)

---

## Integration Points

### Authentication Service
- **Implementation**: OAuth2Login (Keycloak, OBPOIDC), OAuth1a, DirectLogin
- Validates user credentials and authentication tokens
- Checks entitlements (`canGetAccountsHeldAtOneBank`, `canGetAccountsHeldAtAnyBank`)
- Manages session state and token lifecycle

### Bank Connector
- **Implementation**: `Connector` trait with various implementations (RabbitMQ, REST, Stored Procedure, etc.)
- Retrieves actual account data from core banking system via `getBankAccountsHeldFuture`
- Provides real-time account information
- Handles bank-specific data formats and transformations

### View Management System
- **Implementation**: `ViewNewStyle` for view permission management
- Determines which accounts user can access based on views
- Enforces view-based permissions for account data
- Manages granular access controls
- Note: Accounts-held endpoints return accounts even without assigned owner view (for onboarding)

### Account Filtering Helper
- **Implementation**: `AccountsHelper.getFilteredCoreAccounts`
- Applies account type filtering based on query parameters
- Supports INCLUDE and EXCLUDE operations
- Validates filter parameters

### JSON Response Factory
- **Implementation**: `JSONFactory300.createCoreAccountsByCoreAccountsJSON`
- Formats account data into `CoreAccountsHeldJsonV300` structure
- Transforms internal account representations to API response format
- Handles account routing information serialization

### Audit Logging System
- Records all account access requests via OBP-API audit trail
- Tracks user actions for compliance
- Maintains security audit trail with call context

---

## Security & Compliance Considerations

### Authentication Checkpoints
- **Token Validation**: All requests must include valid authentication token (OAuth2, OAuth1a, or DirectLogin)
- **Entitlement Enforcement**: System checks for required entitlements before processing request:
  - Single bank: `canGetAccountsHeldAtOneBank` OR `canGetAccountsHeldAtAnyBank`
  - All banks: `canGetAccountsHeldAtAnyBank`
- **User Verification**: `NewStyle.function.getUserByUserId` validates user existence and status
- **Error Responses**: Clear error codes (401, 403) for authentication/authorization failures

### Data Privacy
- Only display accounts user explicitly holds (not just has views on)
- Account data includes: ID, label, bank ID, number, and routing information
- Sensitive data exposure controlled by entitlements, not views
- Comply with data protection regulations (GDPR, PSD2, etc.)

### Audit Trail
- Call context (`callContext`) tracks all operations through the request chain
- Log all account listing requests with user ID and bank ID (if applicable)
- Record filters and search criteria used
- Track which accounts were displayed to which users
- Maintain security audit trail through OBP-API's built-in logging

---

## Performance Considerations

### Response Time Requirements
- Sub-2-second response time for typical account lists (per acceptance criteria)
- Performance optimization needed for users with large numbers of accounts

### Caching Strategy
- Consider caching account lists with short TTL (time-to-live)
- Invalidate cache on account updates
- Balance between data freshness and response time

### Optimization Approaches
- Use account type filtering to reduce result set size
- Implement client-side virtual scrolling for large lists
- Read replicas for high-volume account queries
- Index optimization for account lookups and filtering
- Async processing via `Future` monad for non-blocking operations

### Current Limitations
- No server-side pagination in v5.1.0 (all accounts returned in single response)
- May impact performance for users with hundreds of accounts
- Consider implementing pagination in future API versions

---

## Error Handling

### OBP-API Error Codes (from APIMethods510)

1. **401 (UserNotLoggedIn)**
   - Cause: User is not authenticated or token is invalid
   - Action: Redirect to login/authentication screen
   - Recovery: Authenticate using OAuth2, OAuth1a, or DirectLogin

2. **403 (UserHasMissingRoles)**
   - Cause: User lacks required entitlements
   - Message: Includes specific missing entitlement names (e.g., "canGetAccountsHeldAtOneBank or canGetAccountsHeldAtAnyBank")
   - Action: Display access denied message with explanation
   - Recovery: Request entitlements from administrator

3. **400 (BankNotFound)**
   - Cause: Invalid bank ID specified in single-bank endpoint
   - Action: Show error message
   - Recovery: Verify bank ID or try all-banks endpoint

4. **400 (UserNotFoundByUserId)**
   - Cause: User ID is invalid or user doesn't exist
   - Action: Show error message
   - Recovery: Verify user ID or contact support

5. **400 (InvalidFilterParameterFormat)**
   - Cause: Invalid account_type_filter_operation value (must be INCLUDE or EXCLUDE)
   - Action: Show validation error
   - Recovery: Correct filter parameter and retry

6. **500 (UnknownError)**
   - Cause: Internal server error during processing
   - Action: Display generic error message
   - Recovery: Retry request or contact support

### Recovery Options
- Retry with same parameters (for transient errors)
- Modify request parameters (for validation errors)
- Request proper entitlements (for permission errors)
- Re-authenticate (for auth errors)
- Contact support (for persistent errors)
- Return to previous screen

---

## Technical Context (OBP-API v5.1.0)

### Key Implementation Classes and Methods

**API Layer:**
- `Implementations5_1_0.getAccountsHeldByUserAtBank` (APIMethods510.scala, line 816)
- `Implementations5_1_0.getAccountsHeldByUser` (APIMethods510.scala, line 863)
- Endpoint definitions in `ResourceDoc` (APIMethods510.scala, lines 788-814, 835-861)

**Service Layer:**
- `NewStyle.function.getUserByUserId` - User validation
- `NewStyle.function.getAccountsHeld` - Single bank account retrieval
- `NewStyle.function.getAccountsHeldByUser` - Multi-bank account retrieval
- `NewStyle.function.getBankAccountsHeldFuture` - Fetch full account details

**Helper Layer:**
- `AccountsHelper.getFilteredCoreAccounts` (AccountsHelper.scala) - Account filtering
- `AccountsHelper.accountTypeFilterText` (AccountsHelper.scala) - Filter documentation

**Response Layer:**
- `JSONFactory300.createCoreAccountsByCoreAccountsJSON` (JSONFactory3.0.0.scala, line 867)
- Response type: `CoreAccountsHeldJsonV300`
- Element type: `AccountHeldJson`

**Authentication & Authorization:**
- `ViewNewStyle` - View permission management
- Entitlement roles: `canGetAccountsHeldAtOneBank`, `canGetAccountsHeldAtAnyBank`
- Auth mechanisms: OAuth2 (Keycloak, OBPOIDC), OAuth1a, DirectLogin

### "Held" Accounts Concept
Per API documentation: "Get Accounts held by the User if even the User has not been assigned the owner View yet. Can be used to onboard the account to the API - since all other account and transaction endpoints require views to be assigned."

This distinguishes "held" accounts (actual account ownership) from "accessible" accounts (view-based access), enabling account onboarding before view assignment.

## Questions Requiring SME Input

1. **Account Type Values**: Complete list of valid account type values for filtering - what are the standard account types in the target banking system?

2. **Pagination Strategy**: Should future versions implement server-side pagination? What should be the default page size?

3. **Caching Policy**: Acceptable staleness tolerance for cached account lists - how long can cached data be used before requiring refresh?

4. **Performance Thresholds**: Specific performance requirements for different user segments beyond the 2-second general requirement

5. **Filter Defaults**: Should certain account types be included/excluded by default, or should all accounts be shown?

---

## Recommendations

1. **Implement Server-Side Pagination**: Add pagination support in future API versions to handle users with large numbers of accounts more efficiently

2. **Add Search Functionality**: Enable search by account name, number, or type for users with large account portfolios (requires new endpoint or query parameter)

3. **Enhance Filtering**: Add more filter options beyond account type (e.g., by balance range, last activity date, account status)

4. **Provide Quick Actions**: Add inline quick actions (view balance, recent transactions) directly in account list to reduce navigation clicks

5. **Save Filter Preferences**: Remember user's filter and sort preferences for subsequent visits (client-side or user preferences API)

6. **Mobile Optimization**: Ensure responsive design for mobile devices where screen real estate is limited

7. **Accessibility**: Implement keyboard navigation and screen reader support for account list

8. **Caching**: Implement smart caching with appropriate TTL to improve response times while maintaining data freshness

9. **Batch Operations**: Consider adding batch endpoints for checking permissions across multiple accounts simultaneously

---

## Document Metadata

**Based on:** Official OpenBankProject/OBP-API repository (https://github.com/OpenBankProject/OBP-API.git)

**API Version:** v5.1.0

**Key Source Files:**
- `/obp-api/src/main/scala/code/api/v5_1_0/APIMethods510.scala` (lines 788-880)
- `/obp-api/src/main/scala/code/api/v3_0_0/JSONFactory3.0.0.scala` (lines 867-875)
- `/obp-api/src/main/scala/code/api/v2_0_0/AccountsHelper.scala` (lines 21-71)
- `/obp-api/src/test/scala/code/api/v5_1_0/AccountTest.scala` (lines 42-78)

**Last Updated:** November 6, 2025

This documentation maps the Account Listing user story to a complete screen flow following the extraction prompt guidelines, with all technical details verified against the actual OBP-API v5.1.0 implementation. The flow emphasizes the API-based nature of the OBP system while maintaining focus on user experience and journey mapping.
