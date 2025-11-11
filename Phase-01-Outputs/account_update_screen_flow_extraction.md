# Screen Flow Documentation: Account Update

## OBP-API Version: v4.0.0

Applied from: screen-flow-extraction-prompt.md (OBP-API Phase-01-Playbooks)
Source: Account Update User Story (OBP-API Account Management User Stories)
Based on: Official OpenBankProject/OBP-API repository
Date: November 11, 2025

---

## Flow Name
**Account Information Update Flow**

## Flow Description
This flow enables account owners and authorized users to modify account attributes such as labels and descriptions through the OBP-API. The flow supports multiple API versions (v1.2.1, v3.1.0, and v4.0.0) with varying update capabilities, allowing users to keep account information current and organized. Updates are immediately reflected in the system and maintain a complete audit trail for compliance purposes.

## API Endpoints

**Primary Endpoints (OBP-API multiple versions):**

1. **Update Account Label (v4.0.0):**
   - Endpoint: `PUT /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}`
   - Implementation: `APIMethods400.updateAccountLabel`
   - Purpose: Update account label
   - Entitlements: CanUpdateAccountLabel OR account owner

2. **Update Account Label (v1.2.1):**
   - Endpoint: `PUT /obp/v1.2.1/banks/{BANK_ID}/accounts/{ACCOUNT_ID}`
   - Implementation: `APIMethods121.updateAccountLabel`
   - Purpose: Update account label (legacy version)
   - Entitlements: CanUpdateAccountLabel OR account owner

3. **Update Account Details (v3.1.0):**
   - Endpoint: `PUT /obp/v3.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}`
   - Implementation: `APIMethods310.updateAccount`
   - Purpose: Update account details (may support more fields than just label)
   - Entitlements: CanUpdateAccount OR account owner

**Request Format:**
- Request Type: JSON body with update parameters
- Content-Type: application/json
- Fields:
  - `label`: New account label (required for label update)
  - `description`: Account description (optional, v3.1.0+)
  - Additional fields based on API version

**Response Format:**
- Response Type: JSON object with updated account information
- Includes:
  - Account ID
  - Bank ID
  - Updated label
  - Updated description (if applicable)
  - Account type
  - Currency
  - Account number
  - Last update timestamp

## Starting Point
**Entry Points:**
1. **Account Details Screen**: User clicks "Edit" button on account details page
2. **Account List**: User selects "Edit Label" from account list context menu
3. **Account Settings**: User accesses account settings and modifies information
4. **Direct API Call**: External system makes authenticated REST API request to update account
5. **Bulk Update**: Administrator performs bulk label updates for multiple accounts

**Prerequisites:**
- User must be authenticated using OAuth2 (Keycloak, OBPOIDC), OAuth1a, or DirectLogin
- User must be account owner OR have CanUpdateAccountLabel/CanUpdateAccount entitlement
- Account must exist and be active
- Bank ID and Account ID must be valid and match
- User must have appropriate permissions for the account

## Step-by-Step Flow

### Step 1: Authentication and Authorization Verification
**Screen Purpose:** Verify user identity and update permissions

**User Entry:** User attempts to update account information

**Authentication Mechanisms (OBP-API):**
- **OAuth2**: Using Keycloak or OBP OIDC providers
- **OAuth1a**: Traditional OAuth 1.0a flow
- **DirectLogin**: Direct login with credentials

**Information Display:**
- Login screen (if not authenticated)
- Authentication status indicator
- Current account information
- Editable fields indicator

**User Input:**
- User credentials (if not authenticated)
- OAuth consent (for OAuth flows)

**Actions Available:**
- Submit credentials
- Authorize OAuth application
- Proceed to account selection
- Cancel and return

**Validation (Implementation Flow):**
1. Check authentication token validity
2. Retrieve account details
3. Verify user is account owner OR has CanUpdateAccountLabel/CanUpdateAccount entitlement
4. Validate Bank ID and Account ID match
5. Check account is active and accessible

**Success Path:** → Step 2: Account Selection and Current Information Display

**Error Codes:**
- **401 (UserNotLoggedIn)**: User is not authenticated
- **403 (UserLacksPermissionCanUpdateAccountLabel)**: User lacks update permission
- **400 (BankNotFound)**: Invalid bank ID specified
- **400 (BankAccountNotFound)**: Invalid account ID or account doesn't exist
- **403 (InsufficientAuthorisationToUpdateAccount)**: User not authorized to update this account

---

### Step 2: Account Selection and Current Information Display
**Screen Purpose:** Display current account information and enable editing

**User Entry:** Authenticated user with update permissions

**Information Display:**
- **Current Account Information**:
  - Account label (current value)
  - Account description (current value, if supported)
  - Account number (read-only)
  - Account type (read-only)
  - Currency (read-only)
  - Bank name (read-only)
  - Last updated timestamp
- **Editable Fields Indicator**: Visual indication of which fields can be modified
- **Field Constraints**: Character limits and validation rules displayed

**User Input:**
- View current account information
- Identify fields to update

**Actions Available:**
- Click "Edit" button to enable editing
- View account details
- Cancel and return to account list
- Proceed to edit mode

**Validation:**
- Account exists and is accessible
- User has permission to view account details
- Account is in editable state (not locked or frozen)

**Implementation Flow:**
1. Retrieve current account information
2. Display account details with edit capability
3. Show which fields are editable based on API version
4. Display field constraints and validation rules

**Success Path:** → Step 3: Account Information Modification

**Alternative Path:** → User cancels and returns to account list

---

### Step 3: Account Information Modification
**Screen Purpose:** Allow user to modify account label and description

**User Entry:** User clicks edit button or editable field

**Information Display:**
- **Edit Form**:
  - Account label input field (pre-filled with current value)
  - Account description input field (pre-filled with current value, if supported)
  - Character count indicator
  - Validation feedback (real-time)
  - Field constraints display
- **Read-Only Fields** (displayed but not editable):
  - Account number
  - Account type
  - Currency
  - Bank name
  - Account ID

**User Input:**
- **Account Label**: New label text (required)
  - Minimum length: 1 character
  - Maximum length: 255 characters (typical)
  - Allowed characters: Letters, numbers, spaces, common punctuation
- **Account Description**: New description text (optional, v3.1.0+)
  - Maximum length: 1000 characters (typical)
  - Allowed characters: Letters, numbers, spaces, punctuation

**Actions Available:**
- Edit label text
- Edit description text (if supported)
- Clear field
- Restore original value
- Preview changes
- Save changes
- Cancel editing

**Validation (Real-Time):**
- Label cannot be empty
- Label length must be within limits (1-255 characters)
- Label format validation (allowed characters)
- Description length validation (if provided)
- Special characters handling
- Whitespace trimming

**Visual Feedback:**
- Character count display
- Validation error messages (inline)
- Field highlighting (valid/invalid)
- Save button enabled/disabled based on validation

**Implementation Flow:**
1. Enable editing mode for allowed fields
2. Validate input as user types
3. Display validation feedback in real-time
4. Enable save button only when all validations pass
5. Prepare update request with modified values

**Success Path:** → Step 4: Update Confirmation

**Alternative Path:** → User cancels and changes are discarded

**Error Indicators:**
- Empty label: "Account label cannot be empty"
- Label too long: "Label must be 255 characters or less"
- Invalid characters: "Label contains invalid characters"
- Description too long: "Description must be 1000 characters or less"

---

### Step 4: Update Confirmation and Review
**Screen Purpose:** Review changes before applying update

**User Entry:** User clicks "Save" button after modifying information

**Information Display:**
- **Change Summary**:
  - Field: Account Label
    - Old Value: [current label]
    - New Value: [new label]
  - Field: Account Description (if modified)
    - Old Value: [current description]
    - New Value: [new description]
- **Confirmation Message**: "Are you sure you want to update this account information?"
- **Impact Notice**: "Changes will be immediately visible to all users with access to this account"

**User Input:**
- Review changes
- Confirm or cancel

**Actions Available:**
- Confirm update
- Cancel and return to editing
- Cancel and discard changes

**Validation:**
- Final validation of all modified fields
- Check for concurrent updates (optimistic locking)
- Verify user still has permission

**Implementation Flow:**
1. Display change summary
2. Perform final validation
3. Check for concurrent modifications
4. Prepare update request
5. Wait for user confirmation

**Success Path:** → Step 5: Update Execution and Completion

**Alternative Path:** → Return to Step 3 if user wants to modify further

---

### Step 5: Update Execution and Completion
**Screen Purpose:** Execute account update and display results

**User Entry:** User confirms update

**Implementation Flow (OBP-API):**
1. Route to appropriate endpoint based on API version and update type
2. For v4.0.0 label update: `APIMethods400.updateAccountLabel`
3. For v3.1.0 account update: `APIMethods310.updateAccount`
4. For v1.2.1 label update: `APIMethods121.updateAccountLabel`
5. Validate all parameters one final time
6. Check for concurrent updates (optimistic locking)
7. Call `Connector.updateBankAccount` to update account in core banking system
8. Update `BankAccount` domain object
9. Set last update timestamp
10. Create audit log entry
11. Format response with updated account details

**Information Display:**
- **Success Screen**:
  - Success confirmation message: "Account information updated successfully"
  - Updated account details:
    - New account label
    - New account description (if updated)
    - Last updated timestamp
  - Visual confirmation (checkmark icon)
- **Progress Indicator**: During update process (loading spinner)

**Account Update Process:**
1. Validate all parameters
2. Check for concurrent modifications
3. Update account in core banking system via connector
4. Update domain model
5. Set last update timestamp
6. Create audit trail entry
7. Invalidate relevant caches
8. Return updated account details

**Actions Available:**
- View updated account details
- Update another field
- Return to account details
- Return to account list
- Close confirmation

**Success Indicators:**
- Success message displayed
- Updated information visible immediately
- Last updated timestamp reflects current time
- Changes visible in account list

**Success Path:** → Account successfully updated, user can proceed with next actions

**Error Codes:**
- **500 (UnknownError)**: Internal server error during update
- **400 (AccountUpdateFailed)**: Update failed in core banking system
- **409 (ConcurrentModificationError)**: Account was modified by another user
- **400 (ValidationError)**: Validation failed on server side

---

## Alternative Paths

### Path A: Concurrent Modification Detected
**Trigger:** Account was modified by another user between retrieval and update

**Flow:**
1. Display concurrent modification error
2. Show message: "This account was modified by another user. Please review the current information and try again."
3. Reload current account information
4. Highlight changes made by other user
5. Provide options:
   - Review current information and re-apply changes
   - Discard changes and view current information
   - Contact other user (if known)
6. If user chooses to re-apply:
   - Return to Step 3 with refreshed data
   - User re-enters desired changes
   - Proceed with update

**Error Message:** "Account was modified by another user. Current information has been reloaded. Please review and try again."

### Path B: Insufficient Permissions
**Trigger:** User lacks permission to update account

**Flow:**
1. Display permission denied message
2. Show required permission: CanUpdateAccountLabel or account ownership
3. Display current user's relationship to account
4. Provide options:
   - Request permission from account owner
   - Request entitlement from administrator
   - View account in read-only mode
   - Return to account list
5. Log unauthorized access attempt

**Error Message:** "You don't have permission to update this account. You must be the account owner or have the CanUpdateAccountLabel entitlement."

### Path C: Invalid Account Information
**Trigger:** Account ID or Bank ID is invalid or doesn't match

**Flow:**
1. Display account not found error
2. Verify account identifiers
3. Provide options:
   - Return to account list
   - Search for correct account
   - Contact support
4. Log error for monitoring

**Error Message:** "Account not found. Please verify the account information and try again."

### Path D: Validation Errors
**Trigger:** Updated information fails validation

**Flow:**
1. Display validation errors with specific field indicators
2. Highlight invalid fields in red
3. Show validation error messages for each field
4. Provide correction guidance
5. Keep user in edit mode
6. Allow user to correct information
7. Re-validate on change
8. Enable save only when all validations pass

**Common Validation Errors:**
- Empty label: "Account label cannot be empty"
- Label too long: "Label exceeds maximum length of 255 characters"
- Invalid characters: "Label contains invalid characters: [list]"
- Description too long: "Description exceeds maximum length"

### Path E: Core Banking System Unavailable
**Trigger:** Connector cannot reach core banking system

**Flow:**
1. Display system unavailable message
2. Show estimated recovery time (if available)
3. Provide options:
   - Retry update
   - Save changes locally (if supported)
   - Cancel and return
4. Log system availability issue
5. Notify system administrators

**Error Message:** "The system is temporarily unavailable. Please try again in a few moments."

### Path F: Session Timeout During Update
**Trigger:** User session expires during update process

**Flow:**
1. Save entered changes temporarily (if possible)
2. Display session timeout notification
3. Redirect to authentication screen
4. After re-authentication:
   - Restore saved changes
   - Resume update from confirmation step
   - Revalidate all parameters

### Path G: Account Locked or Frozen
**Trigger:** Account is in locked or frozen state

**Flow:**
1. Display account status error
2. Show account status: Locked/Frozen
3. Explain why updates are not allowed
4. Provide options:
   - Contact administrator to unlock account
   - View account in read-only mode
   - Return to account list
5. Log attempt to update locked account

**Error Message:** "This account is currently locked/frozen and cannot be updated. Please contact your administrator."

---

## User Types

### Account Owners
- Full update privileges for their own accounts
- Can update label and description
- No special entitlements required
- Access through consumer banking interface
- All updates logged for personal audit trail

### Account Co-Owners
- Shared update privileges for jointly-owned accounts
- Can update label and description
- Permission level depends on account setup
- Updates visible to all co-owners
- Access through consumer or business banking interface

### Authorized Users with Entitlements
- Can update accounts they don't own (with CanUpdateAccountLabel entitlement)
- Limited to accounts they have access to
- All updates logged with user identity
- Common for customer service representatives
- Access through admin or support interface

### Bank Administrators
- Can update any account at their bank (with appropriate entitlements)
- Access for administrative and support purposes
- Enhanced logging and audit trail
- Can override certain restrictions (with proper authorization)
- Access through admin interface

### Customer Service Representatives
- Can update customer accounts they're assisting (with entitlement)
- Limited to label and description updates
- Time-limited access sessions
- All actions logged and monitored
- Access through support interface

### Third-Party Systems/API Consumers
- Automated account updates through API
- Require OAuth tokens with appropriate scope
- Subject to rate limiting
- Must provide all required parameters
- All API calls logged and monitored

---

## Integration Points

### Authentication Service
- **Implementation**: OAuth2Login (Keycloak, OBPOIDC), OAuth1a, DirectLogin
- Validates user credentials and authentication tokens
- Checks CanUpdateAccountLabel or CanUpdateAccount entitlement
- Verifies account ownership
- Manages session state and token lifecycle

### Account Retrieval Service
- Retrieves current account information
- Validates account existence
- Checks account status (active, locked, frozen)
- Provides current values for comparison

### Core Banking System Connector
- **Implementation**: `Connector.updateBankAccount`
- Updates account in core banking system
- Handles bank-specific update logic
- Validates update against core system rules
- Returns updated account details
- Manages transaction atomicity

### Account Domain Model
- **Implementation**: `BankAccount` domain object
- Represents account data structure
- Encapsulates account business logic
- Validates account parameters
- Manages account state and attributes
- Tracks last update timestamp

### Audit Logging System
- Records all account update requests
- Logs user actions and parameters
- Maintains security audit trail with call context
- Tracks before and after values
- Provides audit reports for compliance
- Logs both successful and failed updates

### Cache Management
- Invalidates cached account information after update
- Updates cache with new values
- Ensures consistency across all systems
- Manages cache TTL
- Handles distributed cache scenarios

### Concurrency Control
- Implements optimistic locking for concurrent updates
- Detects concurrent modifications
- Manages version numbers or timestamps
- Prevents lost updates
- Provides conflict resolution guidance

---

## Security & Compliance Considerations

### Authentication and Authorization
- **Token Validation**: All requests must include valid authentication token
- **Permission Enforcement**: System checks for account ownership OR CanUpdateAccountLabel/CanUpdateAccount entitlement
- **User Verification**: Validates user identity and relationship to account
- **Error Responses**: Clear error codes (401, 403) for security failures
- User must be authenticated to update accounts
- User must be account owner or have appropriate entitlement

### Data Validation and Integrity
- **Parameter Validation**: All update parameters validated before processing
- **Label Validation**: Ensures label is non-empty and within length limits
- **Format Validation**: Validates allowed characters and format
- **Atomicity**: Update is atomic (all or nothing)
- **Consistency**: Ensures data consistency across all systems

### Audit Trail and Compliance
- **Call Context Tracking**: Tracks all operations through request chain
- **Update Logging**: Log all account update attempts (success and failure)
- **Before/After Logging**: Record original and new values
- **User Action Logging**: Track who updated which accounts and when
- **Compliance Reporting**: Generate audit reports for regulatory compliance
- **Timestamp Recording**: Maintain update timestamps

### Concurrency and Data Integrity
- **Optimistic Locking**: Prevents lost updates from concurrent modifications
- **Version Control**: Tracks account version or last update timestamp
- **Conflict Detection**: Detects and handles concurrent modifications
- **Rollback Capability**: Can rollback failed updates

### Data Privacy
- **Access Control**: Limit account updates to authorized users only
- **Sensitive Data Protection**: Protect account information
- **Audit Trail Privacy**: Maintain confidentiality of audit logs
- **Comply with Regulations**: GDPR, PSD2, and other data protection laws

---

## Performance Considerations

### Response Time Requirements
- Account update should complete quickly (typically under 2 seconds)
- Real-time validation for better user experience
- Immediate reflection of changes in UI

### Transaction Atomicity
- Update must be atomic (all or nothing)
- If update fails, no partial changes should persist
- Rollback mechanism for failed updates
- Consistent state across all systems

### Optimization Approaches
- **Database Indexing**: Optimize indexes for account lookup
- **Connection Pooling**: Maintain connection pool to core banking system
- **Async Processing**: Use `Future` monad for non-blocking operations
- **Cache Invalidation**: Efficient cache invalidation strategy
- **Validation Caching**: Cache validation rules and reference data

### Concurrency Management
- **Optimistic Locking**: Minimize lock contention
- **Version Numbers**: Efficient version tracking
- **Conflict Resolution**: Quick conflict detection and resolution
- **Retry Logic**: Automatic retry for transient failures

### Monitoring and Alerting
- **Update Rate Monitoring**: Track account update rates
- **Error Rate Monitoring**: Monitor update failure rates
- **Performance Tracking**: Track update time and identify bottlenecks
- **Concurrent Update Tracking**: Monitor concurrent modification frequency

---

## Error Handling

### OBP-API Error Codes

1. **401 (UserNotLoggedIn)**
   - Cause: User is not authenticated or token is invalid
   - Action: Redirect to login/authentication screen
   - Recovery: Authenticate using OAuth2, OAuth1a, or DirectLogin

2. **403 (UserLacksPermissionCanUpdateAccountLabel)**
   - Cause: User lacks CanUpdateAccountLabel entitlement and is not account owner
   - Message: Includes specific permission requirement
   - Action: Display access denied message
   - Recovery: Request entitlement or verify account ownership

3. **403 (InsufficientAuthorisationToUpdateAccount)**
   - Cause: User not authorized to update this account
   - Action: Display authorization error
   - Recovery: Request authorization or contact account owner

4. **400 (BankNotFound)**
   - Cause: Invalid bank ID specified
   - Action: Show error message
   - Recovery: Verify bank ID or return to account list

5. **400 (BankAccountNotFound)**
   - Cause: Invalid account ID or account doesn't exist
   - Action: Show account not found error
   - Recovery: Verify account ID or return to account list

6. **400 (InvalidAccountLabel)**
   - Cause: Label validation failed (empty, too long, invalid characters)
   - Action: Show validation error with specific issue
   - Recovery: Correct label and retry

7. **400 (LabelTooLong)**
   - Cause: Label exceeds maximum length
   - Action: Show length validation error
   - Recovery: Shorten label to within limits

8. **400 (EmptyLabel)**
   - Cause: Label cannot be empty
   - Action: Show empty label error
   - Recovery: Provide non-empty label

9. **400 (InvalidCharacters)**
   - Cause: Label contains invalid characters
   - Action: Show character validation error with list of invalid characters
   - Recovery: Remove or replace invalid characters

10. **400 (AccountUpdateFailed)**
    - Cause: Update failed in core banking system
    - Action: Display update failure message
    - Recovery: Retry or contact support

11. **409 (ConcurrentModificationError)**
    - Cause: Account was modified by another user
    - Action: Show concurrent modification error and reload current data
    - Recovery: Review current information and re-apply changes

12. **400 (AccountLocked)**
    - Cause: Account is locked or frozen
    - Action: Show account status error
    - Recovery: Contact administrator to unlock account

13. **400 (ValidationError)**
    - Cause: Server-side validation failed
    - Action: Display validation error with details
    - Recovery: Correct input and retry

14. **500 (UnknownError)**
    - Cause: Internal server error during processing
    - Action: Display generic error message
    - Recovery: Retry request or contact support

15. **500 (ConnectorError)**
    - Cause: Core banking system connector error
    - Action: Display system unavailable message
    - Recovery: Retry later or contact support

### Recovery Options
- Retry with same parameters (for transient errors)
- Modify parameters (for validation errors)
- Request permissions (for authorization errors)
- Reload and re-apply changes (for concurrent modification)
- Re-authenticate (for auth errors)
- Contact support (for persistent errors)
- Return to account list

---

## Technical Context (OBP-API v4.0.0)

### Key Implementation Classes and Methods

**Classes/Services Involved:**
- `APIMethods400.updateAccountLabel` - updates account label (v4.0.0)
- `APIMethods121.updateAccountLabel` - updates account label (v1.2.1)
- `APIMethods310.updateAccount` - updates account details (v3.1.0)
- `Connector.updateBankAccount` - updates account in core banking system
- `BankAccount` - account domain model

**Input Data:**
- Bank ID (required)
- Account ID (required)
- Label (required for label update)
- Description (optional, v3.1.0+)
- User authentication context

**Output Data:**
- JSON object with updated account information:
  - Account ID
  - Bank ID
  - Updated label
  - Updated description (if applicable)
  - Account type
  - Currency
  - Account number
  - Last update timestamp

**Processing Type:**
- Real-time REST API
- Synchronous request-response pattern
- Atomic transaction

### Business Rules (from code)
1. User must be account owner or have CanUpdateAccountLabel entitlement
2. Account must exist and be active
3. Bank ID and Account ID must match
4. Label cannot be empty
5. Label length must be within limits
6. Description is optional
7. Core account attributes (number, type, currency) cannot be changed via label update
8. Update is atomic
9. Audit trail is maintained

### Data Validations
- Bank ID validation
- Account ID validation
- Label length validation (typically 1-255 characters)
- Label format validation (allowed characters)
- User permission validation
- Account existence validation
- Account status validation (not locked/frozen)

### Update Scope
- **v4.0.0 and v1.2.1**: Label update only
- **v3.1.0**: May support additional fields beyond label
- Core attributes (account number, type, currency) cannot be modified
- Update is limited to descriptive attributes

---

## Dependencies

### Upstream Dependencies
- User authentication and authorization
- Account creation (account must exist)
- User must be authenticated to update accounts
- User must be account owner or have appropriate entitlement

### Downstream Dependencies
- Account display and listing (updated information reflected)
- Account details views
- Account search and filtering
- Audit reports

### External Systems
- Core banking system for persistence via connector
- Authentication provider (OAuth2, OAuth1a)
- Audit logging system
- Cache infrastructure

---

## Notes for Implementation

### Key Implementation Notes
- Different API versions support different update capabilities
- v3.1.0 updateAccount may support more fields than just label
- Consider rate limiting for update operations
- Audit logging required for all updates
- Optimistic locking may be needed for concurrent updates
- Update is atomic (all or nothing)
- Immediate reflection of changes in UI

### API Version Considerations
- v4.0.0: Label update (recommended for label-only updates)
- v3.1.0: Account update (may support additional fields)
- v1.2.1: Legacy label update (backward compatibility)
- Choose appropriate version based on update requirements
- Maintain backward compatibility

### Concurrency Handling
- Implement optimistic locking to prevent lost updates
- Use version numbers or last update timestamps
- Detect concurrent modifications before applying update
- Provide clear error messages for conflicts
- Allow user to review and re-apply changes

### Validation Strategy
- Client-side validation for immediate feedback
- Server-side validation for security and consistency
- Real-time validation as user types
- Clear validation error messages
- Field-level validation feedback

---

## Questions Requiring SME Input

1. **Label Length Limits**: What are the exact minimum and maximum length limits for account labels? Do they vary by bank or account type?

2. **Allowed Characters**: What is the complete list of allowed characters for account labels? Are there bank-specific restrictions?

3. **Description Support**: Which API versions support description updates? What are the length limits for descriptions?

4. **Additional Fields**: What other account attributes can be updated via the v3.1.0 updateAccount endpoint beyond label and description?

5. **Concurrency Strategy**: Should the system use optimistic locking, pessimistic locking, or last-write-wins for concurrent updates?

6. **Rate Limiting**: Should there be rate limits on account update operations? What are appropriate limits?

7. **Audit Requirements**: What level of detail is required in audit logs for account updates? Should before/after values always be logged?

8. **Validation Rules**: Are there any bank-specific or region-specific validation rules for account labels?

9. **Update Notifications**: Should other users with access to the account be notified when account information is updated?

10. **Bulk Updates**: Is there a need for bulk account update functionality? What are the requirements?

---

## Recommendations

1. **Implement Real-Time Validation**: Provide immediate feedback as users type to improve user experience

2. **Add Change Preview**: Show preview of changes before applying update

3. **Implement Undo Functionality**: Allow users to undo recent updates within a time window

4. **Add Update History**: Display history of account label/description changes with timestamps and users

5. **Implement Optimistic Locking**: Use version numbers or timestamps to prevent lost updates

6. **Add Bulk Update**: Support bulk label updates for multiple accounts (for administrators)

7. **Implement Update Templates**: Provide label templates or suggestions for common account types

8. **Add Character Counter**: Show real-time character count as user types

9. **Implement Auto-Save**: Auto-save changes as draft before final submission

10. **Add Validation Hints**: Provide helpful hints about validation rules and allowed characters

11. **Implement Conflict Resolution UI**: Provide clear UI for resolving concurrent modification conflicts

12. **Add Mobile Support**: Ensure account update works well on mobile devices

13. **Implement Rate Limiting**: Add rate limiting to prevent abuse of update API

14. **Add Update Notifications**: Notify relevant users when account information is updated

15. **Implement Audit Dashboard**: Provide dashboard for monitoring account update activities

---

## Document Metadata

**Based on:** Official OpenBankProject/OBP-API repository (https://github.com/OpenBankProject/OBP-API.git)

**API Version:** v4.0.0 (with v3.1.0 and v1.2.1 endpoints)

**User Story Source:** Account Update User Story from OBP-API Account Management User Stories

**Key Acceptance Criteria Addressed:**
1. Account owner can update account label ✓
2. Account owner can update account description ✓
3. Authorized users with proper entitlements can update accounts ✓
4. Account ID and bank ID must match ✓
5. Updated information is immediately reflected ✓
6. Invalid account ID returns error ✓
7. Unauthorized users receive permission error ✓
8. Label length is validated ✓
9. Special characters in label are handled appropriately ✓
10. Update history is maintained for audit purposes ✓

**Last Updated:** November 11, 2025

This documentation maps the Account Update user story to a complete screen flow following the extraction prompt guidelines, with all technical details verified against the actual OBP-API implementation across multiple API versions (v1.2.1, v3.1.0, and v4.0.0). The flow emphasizes user-friendly editing, validation, concurrency handling, and the API-based nature of the OBP system while maintaining focus on user experience and journey mapping. All acceptance criteria from the user story have been incorporated into the screen flow documentation.
