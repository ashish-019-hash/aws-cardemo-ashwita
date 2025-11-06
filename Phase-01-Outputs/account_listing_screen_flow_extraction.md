# Screen Flow Documentation: Account Listing

Applied from: screen-flow-extraction-prompt.md (OBP-API Phase-01-Playbooks)
Source: Account Listing User Story (OBP-API Account Management User Stories)
Date: November 06, 2025

---

## Flow Name
**Account Listing and Selection Flow**

## Flow Description
This flow enables banking application users and API consumers to retrieve and view a comprehensive list of all bank accounts they have permission to access. The flow supports both single-bank and multi-bank account retrieval, allowing users to identify and select accounts for subsequent banking operations.

## Starting Point
**Entry Points:**
1. **Direct API Call**: User or application makes authenticated REST API request to retrieve account list
2. **Dashboard Navigation**: User accesses account listing through main banking dashboard
3. **Post-Authentication**: System redirects to account listing after successful login
4. **Deep Link**: External system or email provides direct link to account listing

**Prerequisites:**
- User must be authenticated with valid credentials
- User must have at least one view permission on one or more accounts
- User must possess either `canGetAccountsHeldAtOneBank` or `canGetAccountsHeldAtAnyBank` entitlement

## Step-by-Step Flow

### Step 1: Authentication Checkpoint
**Screen Purpose:** Verify user identity and authorization

**User Entry:** User attempts to access account listing feature

**Information Display:**
- Login screen (if not authenticated)
- Authentication status indicator

**User Input:**
- User credentials (if required)
- Multi-factor authentication (if enabled)

**Actions Available:**
- Submit credentials
- Request password reset
- Cancel and return to home

**Validation:**
- User ID must be valid and exist in system
- User must have appropriate entitlements (canGetAccountsHeldAtOneBank or canGetAccountsHeldAtAnyBank)

**Success Path:** → Step 2: Account Scope Selection

**Error Path:** → Authentication failure screen with recovery options

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
- Account type filters (optional - checking, savings, etc.)
- Pagination preferences

**Actions Available:**
- Select specific bank
- Choose "All Banks" option
- Apply account type filters
- Set result limits
- Submit request

**Validation:**
- Bank ID must be valid if specified
- Account type filter values must match valid account types
- User must have entitlements matching requested scope

**Success Path:** → Step 3: Account List Display

**Error Path:** → Error message with invalid input details

---

### Step 3: Account List Display
**Screen Purpose:** Present comprehensive list of accessible accounts

**User Entry:** Valid account list request submitted

**Information Display:**
- List of accounts with:
  - Account ID
  - Bank ID
  - Account Label/Name
  - Account Type (checking, savings, etc.)
  - Quick access actions
- Pagination controls (if applicable)
- Filter indicators (active filters shown)
- Result count

**User Input:**
- Sort preferences (by name, type, bank)
- Page navigation
- Account selection for details

**Actions Available:**
- Click account to view details
- Apply additional filters
- Navigate pages (previous/next)
- Refresh account list
- Export account list
- Return to dashboard

**Validation:**
- Only accounts with at least one view permission are displayed
- Results respect user's entitlements
- Maximum 2-second response time for typical account lists

**Success Path:** → Step 4: Account Selection or Step 5: Return to Dashboard

**Error Path:** → Error screen with retry option

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

### Path D: Pagination for Large Account Lists
**Trigger:** User has more accounts than can be displayed on one page

**Flow:**
1. Display first page of results with pagination controls
2. Show total account count
3. Enable navigation:
   - Next/previous page buttons
   - Jump to specific page
   - Adjust results per page
4. Maintain filter state across pages

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
- Validates user credentials
- Checks entitlements and permissions
- Manages session state

### Bank Connector
- Retrieves actual account data from core banking system
- Provides real-time account information
- Handles bank-specific data formats

### View Management System (ViewNewStyle)
- Determines which accounts user can access
- Enforces view-based permissions
- Manages granular access controls

### Audit Logging System
- Records all account access requests
- Tracks user actions for compliance
- Maintains security audit trail

---

## Security & Compliance Considerations

### Authentication Checkpoints
- Initial authentication required before any account access
- Session validation on each request
- Re-authentication for sensitive operations

### Data Privacy
- Only display accounts user has explicit permission to view
- Mask sensitive information based on view permissions
- Comply with data protection regulations (GDPR, PSD2, etc.)

### Audit Trail
- Log all account listing requests
- Record filters and search criteria used
- Track which accounts were displayed to which users

---

## Performance Considerations

### Caching Strategy
- Consider caching account lists with short TTL (time-to-live)
- Invalidate cache on account updates
- Balance between data freshness and response time

### Optimization Needs
- Performance optimization for users with large numbers of accounts
- Read replicas for high-volume account queries
- Pagination to manage large result sets
- Index optimization for filtering and sorting

---

## Error Handling

### Common Errors
1. **Authentication Failure**: Redirect to login with error message
2. **Insufficient Permissions**: Display access denied with explanation
3. **Invalid Bank ID**: Show error message with valid bank options
4. **Network Timeout**: Provide retry option with progress indicator
5. **Service Unavailable**: Display maintenance message with estimated resolution time

### Recovery Options
- Retry with same parameters
- Modify request parameters
- Contact support
- Return to previous screen
- Log out and re-authenticate

---

## Questions Requiring SME Input

1. **Business Rules**: Clear definition of "held" vs "accessible" accounts - what distinguishes accounts the user holds from accounts they can merely access?

2. **Account Type Values**: Complete list of valid account type values and their filtering logic - which types are supported and how should they be categorized?

3. **Pagination Limits**: Maximum number of accounts per request and default page size for optimal performance

4. **Caching Policy**: Acceptable staleness tolerance for cached account lists - how long can cached data be used before requiring refresh?

5. **Available Balance Calculation**: How pending transactions should be reflected in available balance displays

6. **Performance Thresholds**: Specific performance requirements for different user segments (individual vs. business customers)

---

## Recommendations

1. **Implement Progressive Loading**: For users with many accounts, load initial set quickly and lazy-load additional accounts as user scrolls

2. **Add Search Functionality**: Enable search by account name, number, or type for users with large account portfolios

3. **Provide Quick Actions**: Add inline quick actions (view balance, recent transactions) directly in account list to reduce navigation clicks

4. **Save Filter Preferences**: Remember user's filter and sort preferences for subsequent visits

5. **Mobile Optimization**: Ensure responsive design for mobile devices where screen real estate is limited

6. **Accessibility**: Implement keyboard navigation and screen reader support for account list

---

## Technical Context (from User Story)

### Classes/Services Involved
- **APIMethods510.getAccountsHeldByUserAtBank** - retrieves accounts at specific bank
- **APIMethods510.getAccountsHeldByUser** - retrieves accounts across all banks
- **ViewNewStyle** - manages view permissions
- **JSONFactory300.createCoreAccountsByCoreAccountsJSON** - formats response

### Input Data
- User ID
- Bank ID (optional)
- Account type filters (query parameters)

### Output Data
- JSON array of core account objects with:
  - id
  - bank_id
  - label
  - account_type

### Processing Type
- Real-time REST API

---

## Business Rules (from Code)

1. User must be authenticated to retrieve account lists
2. Only accounts where user has at least one view permission are returned
3. Account type filtering is optional and supports multiple types
4. Results must respect user's entitlements (canGetAccountsHeldAtOneBank or canGetAccountsHeldAtAnyBank)

---

## Data Validations

- User ID must be valid and exist in system
- Bank ID must be valid if specified
- Account type filter values must match valid account types
- User must have appropriate entitlements for the requested scope

---

## Dependencies

### Upstream
- User authentication and authorization

### Downstream
- Account detail views
- Transaction retrieval
- Balance inquiries

### External Systems
- Bank connector for retrieving actual account data

---

## Notes for Implementation

- Performance optimization needed for users with large numbers of accounts
- Consider caching account lists with short TTL for frequent requests
- Needs SME Input: Business rules for determining "held" vs "accessible" accounts
- Needs SME Input: Default account type values and filtering logic

---

This documentation maps the Account Listing user story to a complete screen flow following the extraction prompt guidelines. The flow emphasizes the API-based nature of the OBP system while maintaining focus on user experience and journey mapping.
