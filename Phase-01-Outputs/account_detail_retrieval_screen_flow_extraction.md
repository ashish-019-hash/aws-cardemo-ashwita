# Screen Flow Documentation: Account Detail Retrieval

## OBP-API Version: v5.1.0

Applied from: screen-flow-extraction-prompt.md (OBP-API Phase-01-Playbooks)
Source: Account Detail Retrieval User Story (OBP-API Account Management User Stories)
Based on: Official OpenBankProject/OBP-API repository
Date: November 10, 2025

---

## Flow Name
**Account Detail Retrieval and Information Display Flow**

## Flow Description
This flow enables banking application users and account holders to view comprehensive information about a specific account through the OBP-API v5.1.0. The flow supports multiple API endpoints with varying levels of detail based on view permissions, allowing users to access account details, balances, limits, routing information, and other relevant account data based on their authorization level.

## API Endpoints

**Primary Endpoints (OBP-API v5.1.0 and earlier versions):**

1. **Core Account Details Through View (v5.1.0):**
   - Endpoint: `GET /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views/{VIEW_ID}`
   - Implementation: `APIMethods510.getCoreAccountByIdThroughView`
   - Purpose: Get core account details through a specific view
   - Entitlements: View-specific permissions

2. **Core Account by ID (v4.0.0):**
   - Endpoint: `GET /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}`
   - Implementation: `APIMethods400.getCoreAccountById`
   - Purpose: Get core account information by ID
   - Entitlements: Account access permissions

3. **Full Private Account Details (v4.0.0):**
   - Endpoint: `GET /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/account`
   - Implementation: `APIMethods400.getPrivateAccountByIdFull`
   - Purpose: Get full private account details including sensitive information
   - Entitlements: Owner or specific view permissions

4. **Private Account Through View (v3.0.0):**
   - Endpoint: `GET /obp/v3.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/account`
   - Implementation: `APIMethods300.getPrivateAccountById`
   - Purpose: Get private account details through view
   - Entitlements: View-specific permissions

5. **Public Account Information (v3.0.0):**
   - Endpoint: `GET /obp/v3.0.0/my/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/account`
   - Implementation: `APIMethods300.getPublicAccountById`
   - Purpose: Get public account information
   - Entitlements: Public view access

**Response Format:**
- Response Type: Varies by endpoint and API version
- JSON Factory: `JSONFactory` (version-specific)
- Structure includes:
  - Account ID, Bank ID, Label, Number
  - Balance (amount and currency)
  - Account type and currency
  - Account routing information (IBAN, account number, routing schemes)
  - Account limits and restrictions
  - View-specific information based on permissions

## Starting Point
**Entry Points:**
1. **Account Selection**: User selects an account from account listing
2. **Direct API Call**: Application makes authenticated REST API request with account identifiers
3. **Dashboard Widget**: User clicks on account summary widget to view details
4. **Deep Link**: External system or notification provides direct link to account details
5. **Search Result**: User finds account through search and clicks to view details

**Prerequisites:**
- User must be authenticated using OAuth2 (Keycloak, OBPOIDC), OAuth1a, or DirectLogin
- User must have view permission for the specified account
- Account must exist and be active
- Bank ID and Account ID must be valid and match
- View ID must be valid for the account (for view-based endpoints)

## Step-by-Step Flow

### Step 1: Authentication and Authorization Checkpoint
**Screen Purpose:** Verify user identity and view permissions

**User Entry:** User attempts to access account detail information

**Authentication Mechanisms (OBP-API):**
- **OAuth2**: Using Keycloak or OBP OIDC providers
- **OAuth1a**: Traditional OAuth 1.0a flow
- **DirectLogin**: Direct login with credentials

**Information Display:**
- Login screen (if not authenticated)
- Authentication status indicator
- OAuth authorization prompt (if using OAuth)

**User Input:**
- User credentials (if not authenticated)
- OAuth consent (for OAuth flows)
- Account identifiers (Bank ID, Account ID, View ID)

**Actions Available:**
- Submit credentials
- Authorize OAuth application
- Select view type (owner, public, custom)
- Cancel and return to account list

**Validation (Implementation Flow):**
1. Check authentication token validity
2. Validate Bank ID exists
3. Validate Account ID exists and belongs to specified bank
4. Check view permissions via `ViewNewStyle.checkViewAccessAndReturnView`
5. Verify user has permission for the specified view
6. Ensure account is active and accessible

**Success Path:** → Step 2: View Selection and Scope Determination

**Error Codes:**
- **401 (UserNotLoggedIn)**: User is not authenticated
- **403 (UserLacksPermissionCanSeeAvailableViewsForBankAccount)**: User lacks view permission
- **400 (BankNotFound)**: Invalid bank ID specified
- **400 (AccountNotFound)**: Invalid account ID or account doesn't exist
- **400 (ViewNotFound)**: Invalid view ID specified

---

### Step 2: View Selection and Scope Determination
**Screen Purpose:** Determine the level of account information to retrieve based on view permissions

**User Entry:** Successfully authenticated user with valid account identifiers

**Information Display:**
- Available views for the account (owner, public, custom views)
- View permission descriptions
- Information scope for each view type

**User Input:**
- View selection (if multiple views available)
- Preferred API version (if applicable)

**View Types:**
- **Owner View**: Full account access with all details
- **Public View**: Limited account information (public data only)
- **Custom Views**: Specific permission sets defined by bank

**Actions Available:**
- Select specific view
- Choose API endpoint version
- Request account details
- Return to account list

**Validation:**
- View ID must be valid for the account
- User must have permission for selected view
- API version must support requested view type

**Implementation Flow:**
1. Retrieve available views for user and account
2. Determine highest permission level available
3. Select appropriate API endpoint based on view and version
4. Prepare request parameters (Bank ID, Account ID, View ID)

**Success Path:** → Step 3: Account Detail Retrieval

**Error Codes:**
- **403 (InsufficientAuthorisationToCreateView)**: User cannot access any views
- **400 (ViewNotFound)**: Selected view doesn't exist

---

### Step 3: Account Detail Retrieval
**Screen Purpose:** Retrieve comprehensive account information from core banking system

**User Entry:** Valid account identifiers and view permissions confirmed

**Implementation Flow (OBP-API):**
1. Route to appropriate endpoint based on API version and view type
2. For v5.1.0: `APIMethods510.getCoreAccountByIdThroughView`
3. For v4.0.0: `APIMethods400.getCoreAccountById` or `getPrivateAccountByIdFull`
4. For v3.0.0: `APIMethods300.getPrivateAccountById` or `getPublicAccountById`
5. `ViewNewStyle.checkViewAccessAndReturnView` - Validate view permissions
6. Retrieve `BankAccount` domain object from connector
7. Filter account data based on view permissions
8. Format response using version-specific `JSONFactory`

**Information Retrieved:**
- **Core Account Information**:
  - Account ID
  - Bank ID
  - Account label/name
  - Account number
  - Account type (checking, savings, etc.)
  - Currency

- **Balance Information**:
  - Current balance
  - Available balance
  - Currency

- **Account Routing Information**:
  - IBAN
  - Account number
  - Routing scheme
  - Routing address

- **Account Limits and Restrictions**:
  - Daily transaction limits
  - Monthly limits
  - Withdrawal restrictions
  - Transfer limits

- **Additional Details** (based on view permissions):
  - Account description
  - Account holders
  - Account attributes
  - Tags and metadata

**Data Filtering:**
- Sensitive information filtered based on view permissions
- Owner view: All information accessible
- Public view: Limited to non-sensitive data
- Custom views: Specific fields based on view configuration

**Performance Considerations:**
- Response time target: Under 2 seconds
- Caching strategy for frequently accessed accounts
- Real-time balance retrieval from core banking system

**Success Path:** → Step 4: Account Information Display

**Error Codes:**
- **500 (UnknownError)**: Internal server error during retrieval
- **400 (InvalidAccountIdFormat)**: Malformed account ID
- **404 (BankAccountNotFound)**: Account not found in system

---

### Step 4: Account Information Display
**Screen Purpose:** Present comprehensive account details to user

**User Entry:** Account data successfully retrieved

**Information Display:**

**Primary Account Details Section:**
- Account label/name (prominent display)
- Account number (formatted)
- Account type badge
- Bank name and ID
- Account status indicator

**Balance Section:**
- Current balance (large, prominent display)
- Available balance
- Currency symbol and code
- Last updated timestamp

**Account Routing Section:**
- IBAN (if applicable)
- Account number
- Routing codes
- Sort code / routing number
- Copy-to-clipboard functionality

**Limits and Restrictions Section:**
- Daily transaction limit
- Monthly limit
- Withdrawal restrictions
- Transfer limits
- Remaining limits (if applicable)

**Additional Information Section** (view-dependent):
- Account description
- Account holders list
- Account opening date
- Account attributes
- Tags and categories

**User Input:**
- Scroll to view all sections
- Expand/collapse detail sections
- Copy account information

**Actions Available:**
- View transactions for this account
- Check detailed balance
- Initiate payment/transfer
- Update account settings (if permitted)
- Download account statement
- Share account details (secure)
- Refresh account information
- Return to account list

**Visual Elements:**
- Color-coded balance indicators (positive/negative)
- Icons for account type
- Status badges (active, frozen, closed)
- Responsive layout for mobile/desktop

**Success Path:** → Step 5: Additional Actions or Return to Dashboard

**Alternative Path:** → User selects action (view transactions, make payment, etc.)

---

### Step 5: Completion - Additional Actions or Return
**Screen Purpose:** Allow user to perform additional actions or exit the flow

**User Entry:** User has viewed account details

**Actions Available:**
- **Transaction History**: Navigate to transaction listing for this account
- **Make Payment**: Initiate payment from this account
- **Transfer Funds**: Transfer between accounts
- **Download Statement**: Generate and download account statement
- **Update Settings**: Modify account preferences (if permitted)
- **Refresh Data**: Reload account information
- **Return to Account List**: Go back to account listing
- **Return to Dashboard**: Navigate to main dashboard
- **Log Out**: End session

**Navigation Options:**
- Breadcrumb navigation
- Back button
- Main menu access
- Quick action buttons

---

## Alternative Paths

### Path A: Insufficient View Permissions
**Trigger:** User lacks permission for requested view

**Flow:**
1. Display permission denied message
2. Show available views user can access
3. Provide options:
   - Select different view with lower permissions
   - Request access from account owner
   - Return to account list
   - Contact support

**Error Message:** "You don't have permission to view this account with the selected view. Please select a different view or request access."

### Path B: Account Not Found
**Trigger:** Invalid account ID or account doesn't exist

**Flow:**
1. Display account not found error
2. Verify account identifiers
3. Provide options:
   - Return to account list
   - Search for account
   - Contact support
   - Verify account number

**Error Message:** "The requested account could not be found. Please verify the account number and try again."

### Path C: Session Timeout During Retrieval
**Trigger:** User session expires while retrieving account details

**Flow:**
1. Display session timeout notification
2. Save account identifiers for post-authentication redirect
3. Redirect to authentication screen
4. After re-authentication, automatically return to account details with same parameters

### Path D: Stale Data / Cache Refresh
**Trigger:** User requests fresh account data or cached data is stale

**Flow:**
1. Display loading indicator
2. Bypass cache and retrieve fresh data from core banking system
3. Update displayed information
4. Show last updated timestamp
5. Provide manual refresh option

### Path E: Multiple Views Available
**Trigger:** User has access to multiple views for the same account

**Flow:**
1. Display view selection screen
2. Show available views with permission descriptions:
   - Owner view: Full access to all account information
   - Public view: Limited public information only
   - Custom views: Specific permission sets
3. User selects preferred view
4. Retrieve account details with selected view
5. Display view indicator in account details screen
6. Allow view switching without re-authentication

### Path F: Performance Degradation
**Trigger:** Account retrieval exceeds 2-second response time threshold

**Flow:**
1. Display loading indicator with progress message
2. Show estimated wait time
3. Options:
   - Continue waiting
   - Cancel request
   - Return to account list
4. Log performance issue for monitoring
5. Consider caching strategy adjustment

---

## User Types

### Account Owners
- Full access to all account information through owner view
- Can view sensitive data (full balance, limits, account holders)
- Can perform all account operations
- Access through consumer or business banking interface

### Account Co-Holders
- Shared access to account information
- Permission level depends on account setup
- May have restricted access to certain operations
- View permissions defined by primary account holder

### Authorized Users
- Limited access based on granted permissions
- Can view account details through specific views
- Cannot modify account settings
- Common for business accounts with multiple signatories

### Bank Administrators
- Can view all accounts at their bank (with appropriate entitlements)
- Access for support and administrative purposes
- Enhanced information display with system metadata
- Audit logging for all access

### Third-Party Developers/API Consumers
- Access accounts through REST API endpoints
- Require OAuth tokens with appropriate scopes
- Subject to rate limiting and API quotas
- View permissions based on OAuth scope grants

### Customer Service Representatives
- Can view customer accounts to provide support
- Access logged for audit purposes
- Limited to view permissions only (no transaction capabilities)
- Time-limited access sessions

### Auditors and Compliance Officers
- Read-only access to account information
- Can view audit trails and access logs
- Special views with compliance-relevant information
- Access subject to strict logging and monitoring

---

## Integration Points

### Authentication Service
- **Implementation**: OAuth2Login (Keycloak, OBPOIDC), OAuth1a, DirectLogin
- Validates user credentials and authentication tokens
- Manages session state and token lifecycle
- Provides user context for authorization checks

### View Permission Management
- **Implementation**: `ViewNewStyle.checkViewAccessAndReturnView`
- Validates user has permission for requested view
- Determines information scope based on view type
- Enforces view-based access control
- Manages custom view configurations

### Bank Connector
- **Implementation**: `Connector` trait with various implementations
- Retrieves account data from core banking system
- Provides real-time balance information
- Handles bank-specific data formats and transformations
- Manages connection to core banking backend

### Account Domain Model
- **Implementation**: `BankAccount` domain object
- Represents account data structure
- Encapsulates account business logic
- Provides account validation methods
- Manages account state and attributes

### JSON Response Factory
- **Implementation**: Version-specific `JSONFactory`
- Formats account data into API response structure
- Transforms internal representations to external format
- Handles version-specific response schemas
- Filters data based on view permissions

### Audit Logging System
- Records all account access requests via OBP-API audit trail
- Tracks user actions for compliance
- Maintains security audit trail with call context
- Logs view permissions used for each access
- Provides audit reports for compliance officers

### Caching Layer
- Caches frequently accessed account information
- Implements TTL-based cache invalidation
- Balances data freshness with performance
- Bypasses cache for real-time balance requests
- Monitors cache hit rates and performance

---

## Security & Compliance Considerations

### Authentication Checkpoints
- **Token Validation**: All requests must include valid authentication token
- **View Permission Enforcement**: System checks view permissions before returning data
- **User Verification**: Validates user identity and account access rights
- **Error Responses**: Clear error codes (401, 403, 404) for security failures
- User must be authenticated to retrieve account details

### Data Privacy and View-Based Access Control
- **View Filtering**: Only information permitted by view is returned
- **Owner View**: Full access to all account information including sensitive data
- **Public View**: Limited to non-sensitive, public information
- **Custom Views**: Specific field-level permissions based on view configuration
- **Sensitive Data Protection**: Account holder details, full balance, limits filtered by view
- Comply with data protection regulations (GDPR, PSD2, etc.)

### Audit Trail
- **Call Context Tracking**: `callContext` tracks all operations through request chain
- **Access Logging**: Log all account detail requests with user ID, account ID, view ID
- **View Permission Logging**: Record which view was used for each access
- **Timestamp Recording**: Maintain access timestamps for audit purposes
- **Compliance Reporting**: Generate audit reports for regulatory compliance

### Data Integrity
- **Bank-Account Matching**: Verify account belongs to specified bank
- **Account Status Validation**: Ensure account is active and accessible
- **Balance Accuracy**: Real-time balance retrieval from authoritative source
- **Routing Information Validation**: Verify routing codes follow banking standards

### Rate Limiting and Abuse Prevention
- **API Rate Limits**: Enforce rate limits per user/application
- **Suspicious Activity Detection**: Monitor for unusual access patterns
- **Brute Force Protection**: Limit failed authentication attempts
- **Token Expiration**: Enforce token lifecycle and expiration

---

## Performance Considerations

### Response Time Requirements
- Sub-2-second response time for account detail retrieval (per acceptance criteria)
- Real-time balance information from core banking system
- Optimized database queries for account lookup

### Caching Strategy
- **Cache Frequently Accessed Accounts**: Implement short TTL caching for account details
- **Balance Freshness**: Always retrieve current balance in real-time (no caching)
- **Cache Invalidation**: Invalidate cache on account updates or transactions
- **Cache Key Strategy**: Use Bank ID + Account ID + View ID as cache key
- **TTL Configuration**: Configurable TTL based on data sensitivity

### Optimization Approaches
- **Database Indexing**: Optimize indexes for account lookup by ID
- **Connection Pooling**: Maintain connection pool to core banking system
- **Async Processing**: Use `Future` monad for non-blocking operations
- **Read Replicas**: Use read replicas for high-volume account queries
- **Response Compression**: Enable gzip compression for API responses
- **Lazy Loading**: Load additional details on-demand rather than upfront

### Monitoring and Alerting
- **Response Time Tracking**: Monitor and alert on response times exceeding 2 seconds
- **Error Rate Monitoring**: Track error rates by error type
- **Cache Performance**: Monitor cache hit rates and effectiveness
- **System Health**: Monitor connector health and core banking system availability

---

## Error Handling

### OBP-API Error Codes

1. **401 (UserNotLoggedIn)**
   - Cause: User is not authenticated or token is invalid
   - Action: Redirect to login/authentication screen
   - Recovery: Authenticate using OAuth2, OAuth1a, or DirectLogin

2. **403 (UserLacksPermissionCanSeeAvailableViewsForBankAccount)**
   - Cause: User lacks view permission for the account
   - Message: Includes specific view permission requirements
   - Action: Display access denied message with explanation
   - Recovery: Request view access from account owner or administrator

3. **400 (BankNotFound)**
   - Cause: Invalid bank ID specified
   - Action: Show error message
   - Recovery: Verify bank ID or return to account list
   - Per acceptance criteria: Invalid bank ID returns appropriate error

4. **400 (AccountNotFound) / 404 (BankAccountNotFound)**
   - Cause: Invalid account ID or account doesn't exist
   - Action: Show error message
   - Recovery: Verify account ID or return to account list
   - Per acceptance criteria: Invalid account ID returns appropriate error

5. **400 (ViewNotFound)**
   - Cause: Invalid view ID specified
   - Action: Show available views
   - Recovery: Select valid view or use default view

6. **400 (InvalidAccountIdFormat)**
   - Cause: Malformed account ID format
   - Action: Show validation error
   - Recovery: Correct account ID format and retry

7. **403 (InsufficientAuthorisationToCreateView)**
   - Cause: User cannot access any views for the account
   - Action: Display permission denied message
   - Recovery: Request account access or contact administrator

8. **500 (UnknownError)**
   - Cause: Internal server error during processing
   - Action: Display generic error message
   - Recovery: Retry request or contact support

### Recovery Options
- Retry with same parameters (for transient errors)
- Select different view (for permission errors)
- Verify account identifiers (for validation errors)
- Request view access (for authorization errors)
- Re-authenticate (for auth errors)
- Contact support (for persistent errors)
- Return to account list

---

## Technical Context (OBP-API v5.1.0)

### Key Implementation Classes and Methods

**Classes/Services Involved:**
- `APIMethods510.getCoreAccountByIdThroughView` - retrieves account through view (v5.1.0)
- `APIMethods400.getCoreAccountById` - retrieves core account data (v4.0.0)
- `APIMethods400.getPrivateAccountByIdFull` - retrieves full account details (v4.0.0)
- `APIMethods300.getPrivateAccountById` - retrieves private account through view (v3.0.0)
- `APIMethods300.getPublicAccountById` - retrieves public account information (v3.0.0)
- `ViewNewStyle.checkViewAccessAndReturnView` - validates view permissions
- `BankAccount` - domain model for account data
- `JSONFactory` - formats account response (version-specific)
- `Connector` - retrieves account data from core banking system

**Input Data:**
- Bank ID (required)
- Account ID (required)
- View ID (required for view-based endpoints)
- User authentication context

**Output Data:**
- JSON object with account details:
  - id, bank_id, label, number
  - balance (amount, currency)
  - account_type, currency
  - account_routings (IBAN, routing schemes)
  - limits and restrictions
  - view-specific additional information

**Processing Type:**
- Real-time REST API
- Synchronous request-response pattern
- High availability requirements

### Business Rules (from code)
1. User must have view permission to access account details
2. Different views expose different levels of account information
3. Owner view provides full account access
4. Public view provides limited account information
5. Account must exist and be active
6. Bank ID and Account ID must match
7. View permissions are checked before returning data
8. Sensitive information is filtered based on view permissions
9. Account balance is current as of request time
10. Account routing information follows banking standards

### Data Validations
- Bank ID must be valid and exist
- Account ID must be valid and exist
- View ID must be valid for the account
- User must have permission for the specified view
- Account must belong to the specified bank
- Account must be active and accessible

### View-Based Information Filtering
- **Owner View**: All account information including sensitive data
- **Public View**: Limited to public, non-sensitive information
- **Custom Views**: Field-level permissions based on view configuration
- **Filtering Logic**: Applied before response formatting
- **Permission Inheritance**: Views can inherit permissions from parent views

---

## Dependencies

### Upstream Dependencies
- User authentication and authorization
- View permission management system
- User must be authenticated to retrieve account details
- User must have view permission for the account

### Downstream Dependencies
- Transaction viewing and history
- Balance inquiries and statements
- Payment and transfer operations
- Account settings and preferences
- Account statement generation

### External Systems
- Core banking system via connector
- Authentication provider (OAuth2, OAuth1a)
- Audit logging system
- Caching infrastructure

---

## Notes for Implementation

### Key Implementation Notes
- Different API versions provide different levels of detail
- View-based access control is critical for data privacy
- Response format varies by API version
- Caching strategy should consider data freshness requirements
- Audit logging required for account access
- Balance information should be real-time (no caching)
- Routing information must follow banking standards

### API Version Considerations
- v5.1.0: Core account through view (recommended)
- v4.0.0: Core account by ID, full private account
- v3.0.0: Private and public account endpoints
- Choose appropriate version based on required detail level
- Maintain backward compatibility for older API versions

### View Permission Strategy
- Default to highest permission level available to user
- Provide view selection for users with multiple views
- Display view indicator in UI to show current permission level
- Allow view switching without re-authentication
- Cache view permissions with appropriate TTL

### Performance Optimization
- Implement caching for account details (not balance)
- Use database indexes for account lookup
- Optimize connector calls to core banking system
- Monitor and alert on response time thresholds
- Consider read replicas for high-volume queries

---

## Questions Requiring SME Input

1. **View Permission Hierarchy**: What is the complete hierarchy of view permissions and their specific field-level access rights?

2. **Balance Caching Policy**: Should account balance ever be cached, or should it always be retrieved in real-time? What is the acceptable staleness for non-balance account information?

3. **Account Limits**: What are the standard account limit types (daily, monthly, per-transaction) and how should they be displayed to users?

4. **Routing Information Standards**: What routing schemes are supported (IBAN, SWIFT, domestic routing codes) and what validation rules apply?

5. **Multi-Currency Accounts**: How should accounts with multiple currencies be handled? Should balance be displayed in all currencies or converted to a primary currency?

6. **Account Status Types**: What are all possible account statuses (active, frozen, closed, pending) and how should each be displayed and handled?

7. **Custom View Configuration**: How are custom views configured and what are the typical use cases for custom views beyond owner and public?

---

## Recommendations

1. **Implement View Switching**: Allow users to switch between available views without leaving the account detail screen

2. **Add Quick Actions**: Provide inline quick actions (make payment, view transactions) directly in account details to reduce navigation

3. **Real-Time Balance Updates**: Implement WebSocket or polling for real-time balance updates without page refresh

4. **Account Comparison**: Enable side-by-side comparison of multiple accounts for users with multiple accounts

5. **Export Functionality**: Add ability to export account details as PDF or CSV for record-keeping

6. **Favorite Accounts**: Allow users to mark accounts as favorites for quick access

7. **Account Alerts**: Display account alerts and notifications (low balance, limit approaching) in account details

8. **Transaction Preview**: Show recent transactions summary in account details without navigating to full transaction history

9. **Mobile Optimization**: Ensure responsive design with touch-friendly interface for mobile devices

10. **Accessibility**: Implement keyboard navigation, screen reader support, and high-contrast mode for account details

11. **Performance Monitoring**: Implement detailed performance monitoring with alerts for response times exceeding 2-second threshold

12. **Cache Warming**: Pre-load frequently accessed accounts into cache during off-peak hours

---

## Document Metadata

**Based on:** Official OpenBankProject/OBP-API repository (https://github.com/OpenBankProject/OBP-API.git)

**API Version:** v5.1.0 (with v4.0.0 and v3.0.0 endpoints)

**User Story Source:** Account Detail Retrieval User Story from OBP-API Account Management User Stories

**Key Acceptance Criteria Addressed:**
1. User can retrieve account details by providing bank ID, account ID, and view ID ✓
2. Response includes comprehensive account information based on view permissions ✓
3. Account balance is included in the response ✓
4. Account limits and restrictions are displayed ✓
5. Account routing information (IBAN, account number) is provided ✓
6. Account type and currency are included ✓
7. Account label and description are returned ✓
8. Only information permitted by the view is returned ✓
9. Invalid account ID returns appropriate error ✓
10. User without view permission receives authorization error ✓
11. Response time is under 2 seconds ✓

**Last Updated:** November 10, 2025

This documentation maps the Account Detail Retrieval user story to a complete screen flow following the extraction prompt guidelines, with all technical details verified against the actual OBP-API implementation across multiple API versions (v5.1.0, v4.0.0, v3.0.0). The flow emphasizes view-based access control and the API-based nature of the OBP system while maintaining focus on user experience and journey mapping. All acceptance criteria from the user story have been incorporated into the screen flow documentation.
