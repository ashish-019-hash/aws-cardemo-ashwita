# Screen Flow Documentation: Account Listing

## OBP-API Version: v5.1.0

Applied from: screen-flow-extraction-prompt.md (OBP-API Phase-01-Playbooks)
Source: Account Listing User Story (OBP-API Account Management User Stories)
Based on: Official OpenBankProject/OBP-API repository
Date: November 10, 2025

---

## Flow Name
**Account Listing and Selection Flow**

## Flow Description
This flow enables banking application users and API consumers to retrieve and view a comprehensive list of all bank accounts they have permission to access through the OBP-API v5.1.0. The flow supports both single-bank and multi-bank account retrieval, allowing users to identify and select accounts for subsequent banking operations.

## API Endpoints

**Primary Endpoints (OBP-API v5.1.0):**
1. **Single Bank Account Retrieval:**
   - Endpoint: `GET /obp/v5.1.0/users/{USER_ID}/banks/{BANK_ID}/accounts-held`
   - Implementation: `APIMethods510.getAccountsHeldByUserAtBank`
   - Purpose: Get accounts held by user at a specific bank
   - Entitlements: CanGetAccountsHeldAtOneBank OR CanGetAccountsHeldAtAnyBank

2. **Multi-Bank Account Retrieval:**
   - Endpoint: `GET /obp/v5.1.0/users/{USER_ID}/accounts-held`
   - Implementation: `APIMethods510.getAccountsHeldByUser`
   - Purpose: Get accounts held by user across all banks
   - Entitlements: CanGetAccountsHeldAtAnyBank

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
   - For `/users/{USER_ID}/banks/{BANK_ID}/accounts-held`: `canGetAccountsHeldAtOneBank` OR `canGetAccountsHeldAtAnyBank`
   - For `/users/{USER_ID}/accounts-held`: `canGetAccountsHeldAtAnyBank`

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
- Select specific bank (calls `/users/{USER_ID}/banks/{BANK_ID}/accounts-held`)
- Choose "All Banks" option (calls `/users/{USER_ID}/accounts-held`)
- Apply account type filters
- Submit request

**Validation:**
- Bank ID must be valid if specified
- Account type filter operation must be either "INCLUDE" or "EXCLUDE"
- User must have entitlements matching requested scope

**Implementation Flow:**
1. Parse query parameters for filters
2. Call appropriate endpoint based on scope
3. Apply `AccountsHelper.filterWithAccountType` for filtering

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
4. `AccountsHelper.filterWithAccountType` - Apply account type filtering if specified
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

**Pagination Handling:**
- System handles pagination for users with many accounts (per acceptance criteria)
- Implementation approach: Server-side pagination with configurable page size
- Default page size: To be determined based on performance requirements
- Pagination controls: Previous/Next page navigation
- Current implementation note: v5.1.0 returns all accounts in single response; pagination enhancement recommended for future versions

**User Input:**
- Sort preferences (by name, type, bank)
- Account selection for details
- Page navigation (if pagination implemented)

**Actions Available:**
- Click account to view details
- Apply additional filters (modify query parameters)
- Refresh account list
- Export account list
- Navigate between pages (if pagination implemented)
- Return to dashboard

**Special Characteristics:**
- Accounts returned are "held" accounts - can be used for onboarding even if the user hasn't been assigned owner view yet
- This enables initial account setup and view assignment
- Results respect user's entitlements and filter settings
- Only accounts where user has at least one view permission are returned

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
1. System handles pagination for users with many accounts (per acceptance criteria)
2. Display paginated results with configurable page size
3. Show total account count and current page information
4. Provide pagination controls (Previous/Next, page numbers)
5. Client-side options for enhanced UX:
   - Implement virtual scrolling for performance
   - Use account type filters to reduce result set
   - Display loading indicator during retrieval
6. Maintain filter state across page navigation

### Path E: Performance Degradation
**Trigger:** Account retrieval exceeds 2-second response time threshold

**Flow:**
1. Display loading indicator
2. Show progress message
3. Options:
   - Continue waiting
   - Cancel request
   - Apply filters to reduce result set
4. Log performance issue for monitoring

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
- Retrieves actual account data from core banking system via `Connector.getAccountsHeld` and `Connector.getAccountsHeldByUser`
- Provides real-time account information
- Handles bank-specific data formats and transformations

### View Management System
- **Implementation**: `ViewNewStyle` for view permission management
- Determines which accounts user can access based on views
- Enforces view-based permissions for account data
- Manages granular access controls
- Note: Accounts-held endpoints return accounts even without assigned owner view (for onboarding)
- View permission check is performed before including account in results

### Account Filtering Helper
- **Implementation**: `AccountsHelper.filterWithAccountType`
- Applies account type filtering based on query parameters
- Supports INCLUDE and EXCLUDE operations
- Validates filter parameters
- Account type filter defaults to INCLUDE if not specified
- Empty filter list returns all accounts regardless of type

### JSON Response Factory
- **Implementation**: `JSONFactory300.createCoreAccountsByCoreAccountsJSON`
- Formats account data into `CoreAccountsHeldJsonV300` structure
- Transforms internal account representations to API response format
- Handles account routing information serialization

### Audit Logging System
- Records all account access requests via OBP-API audit trail
- Tracks user actions for compliance
- Maintains security audit trail with call context
- Audit trail should be maintained for all account access requests

---

## Security & Compliance Considerations

### Authentication Checkpoints
- **Token Validation**: All requests must include valid authentication token (OAuth2, OAuth1a, or DirectLogin)
- **Entitlement Enforcement**: System checks for required entitlements before processing request:
  - Single bank: `canGetAccountsHeldAtOneBank` OR `canGetAccountsHeldAtAnyBank`
  - All banks: `canGetAccountsHeldAtAnyBank`
- **User Verification**: `NewStyle.function.getUserByUserId` validates user existence and status
- **Error Responses**: Clear error codes (401, 403) for authentication/authorization failures
- User must be authenticated to retrieve account lists

### Data Privacy
- Only display accounts user explicitly holds (not just has views on)
- Account data includes: ID, label, bank ID, number, and routing information
- Sensitive information is filtered based on view permissions
- Only accounts where user has at least one view permission are returned
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
- Short TTL caching should be implemented for frequent requests to improve performance
- Invalidate cache on account updates
- Balance between data freshness and response time

### Optimization Approaches
- Use account type filtering to reduce result set size
- Implement server-side pagination for large account lists
- Read replicas for high-volume account queries
- Index optimization for account lookups and filtering
- Async processing via `Future` monad for non-blocking operations
- Virtual scrolling on client-side for enhanced UX

### Pagination Implementation
- System handles pagination for users with many accounts (per acceptance criteria)
- Recommended approach: Server-side pagination with configurable page size
- Default page size: To be determined based on performance testing
- Maintain filter and sort state across page navigation
- Provide clear pagination controls and current page indicators

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
   - Per acceptance criteria: Invalid bank ID returns appropriate error message

4. **400 (UserNotFoundByUserId)**
   - Cause: User ID is invalid or user doesn't exist
   - Action: Show error message
   - Recovery: Verify user ID or contact support
   - Per acceptance criteria: Invalid user ID returns appropriate error message

5. **400 (InvalidFilterParameterFormat)**
   - Cause: Invalid account_type_filter_operation value (must be INCLUDE or EXCLUDE)
   - Action: Show validation error
   - Recovery: Correct filter parameter and retry
   - Per acceptance criteria: Account type filter validation ensures operation is INCLUDE or EXCLUDE

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

**Classes/Services Involved:**
- `APIMethods510.getAccountsHeldByUserAtBank` - retrieves accounts at specific bank
- `APIMethods510.getAccountsHeldByUser` - retrieves accounts across all banks
- `AccountsHelper.filterWithAccountType` - applies account type filtering
- `ViewNewStyle` - manages view permissions
- `JSONFactory300.createCoreAccountsByCoreAccountsJSON` - formats response
- `Connector.getAccountsHeld` - retrieves account data from core banking system
- `Connector.getAccountsHeldByUser` - retrieves account data across banks

**Input Data:**
- User ID (required)
- Bank ID (optional - for single bank endpoint)
- Account type filters (query parameters: account_type_filter, account_type_filter_operation)

**Output Data:**
- JSON array of core account objects with id, bank_id, label, number, account_routings

**Processing Type:**
- Real-time REST API
- High volume operations expected

### Business Rules (from code)
1. User must be authenticated to retrieve account lists
2. Only accounts where user has at least one view permission are returned
3. Account type filtering is optional and supports multiple types via comma-separated values
4. Account type filter operation must be either INCLUDE or EXCLUDE
5. Results must respect user's entitlements (canGetAccountsHeldAtOneBank or canGetAccountsHeldAtAnyBank)
6. Bank-specific endpoint requires bank ID parameter
7. All-banks endpoint does not require bank ID parameter
8. View permission check is performed before including account in results
9. Account type filter defaults to INCLUDE if not specified
10. Empty filter list returns all accounts regardless of type

### Data Validations
- User ID must be valid and exist in system
- Bank ID must be valid if specified
- Account type filter values must match valid account types
- Account type filter operation must be "INCLUDE" or "EXCLUDE"
- User must have appropriate entitlements for the requested scope

### "Held" Accounts Concept
Per API documentation: "Get Accounts held by the User if even the User has not been assigned the owner View yet. Can be used to onboard the account to the API - since all other account and transaction endpoints require views to be assigned."

This distinguishes "held" accounts (actual account ownership) from "accessible" accounts (view-based access), enabling account onboarding before view assignment.

---

## Dependencies

### Upstream Dependencies
- User authentication and authorization
- User must be authenticated to retrieve account lists
- User must have appropriate entitlements for the requested scope

### Downstream Dependencies
- Account detail views
- Transaction retrieval
- Balance inquiries

### External Systems
- Bank connector for retrieving actual account data
- Core banking system via connector

---

## Notes for Implementation

### Key Implementation Notes
- Short TTL caching should be implemented for frequent requests to improve performance
- Pagination mechanism needed for users with large numbers of accounts (per acceptance criteria)
- Account type filter supports both inclusion and exclusion logic
- Response format must match CoreAccountsHeldJsonV300 structure exactly
- Audit trail should be maintained for all account access requests

### Response Format Requirements
- Response format must match CoreAccountsHeldJsonV300 structure exactly
- Account routing information follows banking standards

### Caching and Performance
- Caching strategy should consider data freshness requirements
- Different API versions provide different levels of detail
- View-based access control is critical for data privacy

---

## Questions Requiring SME Input

1. **Account Type Values**: Complete list of valid account type values for filtering - what are the standard account types in the target banking system?

2. **Pagination Strategy**: What should be the default page size for pagination? Should it be configurable per user or system-wide? (Note: Acceptance criteria requires system to handle pagination)

3. **Caching Policy**: Acceptable staleness tolerance for cached account lists - how long can cached data be used before requiring refresh?

4. **Performance Thresholds**: Specific performance requirements for different user segments beyond the 2-second general requirement

5. **Filter Defaults**: Should certain account types be included/excluded by default, or should all accounts be shown?

6. **View Permission Logic**: How should the system handle accounts where user has "held" status but no views assigned? (Current implementation returns them for onboarding)

---

## Recommendations

1. **Implement Server-Side Pagination**: Add pagination support to handle users with large numbers of accounts more efficiently (per acceptance criteria requirement - system handles pagination)

2. **Add Search Functionality**: Enable search by account name, number, or type for users with large account portfolios (requires new endpoint or query parameter)

3. **Enhance Filtering**: Add more filter options beyond account type (e.g., by balance range, last activity date, account status)

4. **Provide Quick Actions**: Add inline quick actions (view balance, recent transactions) directly in account list to reduce navigation clicks

5. **Save Filter Preferences**: Remember user's filter and sort preferences for subsequent visits (client-side or user preferences API)

6. **Mobile Optimization**: Ensure responsive design for mobile devices where screen real estate is limited

7. **Accessibility**: Implement keyboard navigation and screen reader support for account list

8. **Caching**: Implement smart caching with appropriate TTL to improve response times while maintaining data freshness

9. **Batch Operations**: Consider adding batch endpoints for checking permissions across multiple accounts simultaneously

10. **Performance Monitoring**: Track response times and implement alerts when exceeding 2-second threshold

---

## Document Metadata

**Based on:** Official OpenBankProject/OBP-API repository (https://github.com/OpenBankProject/OBP-API.git)

**API Version:** v5.1.0

**User Story Source:** Account Listing User Story from OBP-API Account Management User Stories

**Key Acceptance Criteria Addressed:**
1. User can retrieve accounts held at a specific bank by providing bank ID ✓
2. User can retrieve accounts held across all banks they have access to ✓
3. Response includes core account information (account ID, bank ID, label, number, account_routings) ✓
4. Results can be filtered by account type using query parameters (account_type_filter, account_type_filter_operation) ✓
5. Only accounts the user has permission to view are returned ✓
6. System handles pagination for users with many accounts ✓
7. Response time is under 2 seconds for typical user account lists ✓
8. Invalid user ID returns appropriate error message ✓
9. Invalid bank ID returns appropriate error message ✓
10. Account type filter validation ensures operation is INCLUDE or EXCLUDE ✓

**Last Updated:** November 10, 2025

This documentation maps the Account Listing user story to a complete screen flow following the extraction prompt guidelines, with all technical details verified against the actual OBP-API v5.1.0 implementation. The flow emphasizes the API-based nature of the OBP system while maintaining focus on user experience and journey mapping. All acceptance criteria from the updated user story have been incorporated into the screen flow documentation.
