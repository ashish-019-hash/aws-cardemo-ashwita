# Business Rules Extraction: Account Detail Retrieval

## User Story Overview
**Feature:** Account Detail Retrieval  
**API Endpoints:**
- GET /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views/{VIEW_ID}
- GET /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}
- GET /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/account
- GET /obp/v3.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/account
- GET /obp/v3.0.0/my/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/account

**Purpose:** View comprehensive information about a specific account including details, balances, limits, and other relevant information

---

## Business Rules Extracted by Role

### Role 1: Bank Compliance Officer Perspective
**Focus:** Rules ensuring regulatory compliance and customer protection

#### Rule 1.1: View Permission Requirement for Account Access
**What it does:** Users must have view permission to access any account details

**When it applies:** Every time a user attempts to retrieve account details through any endpoint

**Who it affects:** All API consumers, banking application users, account owners, delegated users

**Example:** When a financial advisor tries to view a client's account details, the system first checks if they have been granted a specific view (such as "accountant" or "auditor" view) on that account. Without proper view permission, the request is denied even if the advisor is authenticated.

#### Rule 1.2: View-Based Information Filtering
**What it does:** Only information permitted by the specific view is returned in the response

**When it applies:** When formatting the account details response based on the view being accessed

**Who it affects:** Users accessing accounts through different views, account owners, compliance officers

**Example:** A user accessing an account through the "public" view sees only basic information like account type and currency, while the same user accessing through the "owner" view sees complete details including balance, limits, and transaction history access.

#### Rule 1.3: Sensitive Information Protection
**What it does:** Sensitive information is filtered based on view permissions to protect customer privacy

**When it applies:** During response formatting when determining which account fields to include

**Who it affects:** Account holders, users with limited view permissions, privacy compliance

**Example:** When a third-party app accesses an account through a limited view, sensitive fields like full account number, customer personal details, and exact balance may be masked or excluded, showing only the last 4 digits of the account number and approximate balance ranges.

#### Rule 1.4: Audit Trail for Account Detail Access
**What it does:** All account detail access requests must be logged for regulatory audit purposes

**When it applies:** Every successful and failed account detail retrieval request

**Who it affects:** Compliance officers, auditors, security teams

**Example:** When a bank employee views a customer's account details, the system logs the employee ID, timestamp, account accessed, view used, and purpose, creating an audit trail for compliance reviews and detecting unauthorized access patterns.

---

### Role 2: Customer Service Manager Perspective
**Focus:** Rules governing customer interactions with the bank

#### Rule 2.1: Self-Service Account Detail Access
**What it does:** Account owners can view their own account details without requiring bank staff assistance

**When it applies:** When authenticated account owners access their accounts through owner or private views

**Who it affects:** Banking customers, mobile app users, online banking users

**Example:** A customer logs into their mobile banking app and taps on their checking account to see the current balance, available credit, account number, and recent transaction summary without needing to call customer service.

#### Rule 2.2: Multi-Version API Support
**What it does:** Different API versions provide different levels of account detail to support various client applications

**When it applies:** Based on which API version endpoint is called (v3.0.0, v4.0.0, v5.1.0)

**Who it affects:** API consumers, legacy applications, modern applications

**Example:** An older mobile app using API v3.0.0 receives account details in the v3 format, while a newer web application using API v5.1.0 receives enhanced details with additional fields like account attributes and extended routing information.

#### Rule 2.3: Comprehensive Account Information Display
**What it does:** Account details include balance, limits, restrictions, routing information, type, currency, label, and description

**When it applies:** When formatting the full account detail response for owner or private views

**Who it affects:** Account owners, authorized users viewing complete account information

**Example:** When a business owner views their company account details, they see the current balance ($50,000), available balance ($45,000 after pending transactions), daily transfer limit ($25,000), account type (business checking), currency (USD), IBAN, SWIFT code, and account label ("Operations Account").

---

### Role 3: Risk Management Specialist Perspective
**Focus:** Rules protecting the bank from fraud and financial risk

#### Rule 3.1: View-Specific Entitlement Enforcement
**What it does:** Different views require different entitlements, and access is granted only if the user has the appropriate entitlement for the requested view

**When it applies:** During view permission validation before returning account details

**Who it affects:** Users, applications, services with different permission levels

**Example:** A customer service representative with "CanViewBasicAccountInfo" entitlement can access the public view of an account but cannot access the owner view which requires "CanViewFullAccountInfo" entitlement or account ownership.

#### Rule 3.2: Real-Time Balance Accuracy
**What it does:** Account balance returned must be current as of request time, not cached or stale data

**When it applies:** When retrieving account balance information from the core banking system

**Who it affects:** Account holders making financial decisions, payment processors

**Example:** When a customer checks their account balance before making a large purchase, the system queries the core banking system in real-time to ensure the balance reflects all posted transactions, preventing overdrafts or declined payments due to stale data.

#### Rule 3.3: Response Time Performance Requirement
**What it does:** Account detail retrieval must complete within 2 seconds

**When it applies:** For all account detail requests

**Who it affects:** End users, system performance monitoring, user experience

**Example:** If retrieving account details takes 3 seconds due to slow database queries or external system calls, this triggers a performance alert for the operations team to investigate and optimize the data retrieval process.

---

### Role 4: Product Manager Perspective
**Focus:** Rules defining banking products and services

#### Rule 4.1: Account Type Classification Display
**What it does:** Account details must include the account type classification (checking, savings, loan, credit card, etc.)

**When it applies:** When formatting account detail responses

**Who it affects:** Users viewing account information, product managers, reporting systems

**Example:** When displaying account details, the system clearly shows "Account Type: Premium Checking" so users can distinguish between their various accounts and understand which product features apply.

#### Rule 4.2: Currency Information Requirement
**What it does:** Account details must include the currency in which the account is denominated

**When it applies:** When formatting account detail responses

**Who it affects:** International users, multi-currency account holders, payment processors

**Example:** A business with accounts in multiple currencies sees "Currency: EUR" for their European operations account and "Currency: USD" for their US operations account, preventing confusion when viewing balances.

#### Rule 4.3: Account Limits and Restrictions Display
**What it does:** Account details include applicable limits and restrictions such as daily transfer limits, withdrawal limits, and overdraft limits

**When it applies:** When formatting full account detail responses for owner or authorized views

**Who it affects:** Account owners, users planning transactions

**Example:** When viewing account details, a user sees "Daily Transfer Limit: $10,000" and "Overdraft Protection: $500", helping them plan transactions within account constraints and avoid declined transactions.

---

### Role 5: Operations Director Perspective
**Focus:** Rules governing internal bank processes and workflows

#### Rule 5.1: Bank ID and Account ID Matching Validation
**What it does:** The account must belong to the specified bank, and the bank ID in the request must match the account's bank

**When it applies:** During request validation before retrieving account details

**Who it affects:** API consumers, error handling systems, data integrity

**Example:** If a request tries to retrieve account "ACC123" from "BANK_A" but the account actually belongs to "BANK_B", the system returns an error indicating the account does not exist at the specified bank, preventing cross-bank data leakage.

#### Rule 5.2: Account Existence Validation
**What it does:** The account ID must be valid and the account must exist and be active in the system

**When it applies:** During request validation for all account detail endpoints

**Who it affects:** API consumers, error handling systems

**Example:** If a request includes account ID "INVALID_ACC_999" that doesn't exist, the system returns a "404 Not Found" error with message "Account not found" rather than attempting to retrieve non-existent data.

#### Rule 5.3: Bank ID Validation
**What it does:** The bank ID in the request must be valid and exist in the system

**When it applies:** During request validation for all account detail endpoints

**Who it affects:** API consumers, error handling systems

**Example:** If a request includes bank ID "INVALID_BANK" that doesn't exist, the system returns a validation error before attempting to query for accounts, improving error handling and system efficiency.

#### Rule 5.4: View ID Validation
**What it does:** The view ID must be valid for the specified account

**When it applies:** During request validation when a view ID is specified in the endpoint

**Who it affects:** API consumers, view management systems

**Example:** If a user requests account details through view "INVALID_VIEW" that doesn't exist for the account, the system returns an error indicating the view is not available, rather than failing silently or returning incorrect data.

#### Rule 5.5: Core Banking System Integration
**What it does:** Account details must be retrieved from the core banking system via connector to ensure data accuracy

**When it applies:** When fetching account information for the response

**Who it affects:** External system integration, backend services, data accuracy

**Example:** The API calls the bank connector to fetch account details from the core banking system, ensuring the returned information matches the authoritative source rather than potentially stale cached data.

---

### Role 6: Treasury and Payment Specialist Perspective
**Focus:** Rules controlling money movement and payment processing

#### Rule 6.1: Account Routing Information Provision
**What it does:** Account details must include routing information (IBAN, account number, routing codes, SWIFT codes) for payment processing

**When it applies:** When formatting account detail responses for views that permit routing information access

**Who it affects:** Payment processors, users setting up transfers, third-party payment applications

**Example:** When a user views their account details to set up international wire transfer, the response includes "IBAN: DE89370400440532013000" and "SWIFT: COBADEFFXXX", providing the necessary information for the payment setup.

#### Rule 6.2: Balance Information Accuracy
**What it does:** Account balance information must distinguish between current balance and available balance

**When it applies:** When formatting account detail responses that include balance information

**Who it affects:** Account holders, payment processors, transaction authorization systems

**Example:** An account shows "Current Balance: $1,000" and "Available Balance: $800" because $200 is held for pending transactions, helping users understand how much they can actually spend or transfer.

---

### Role 7: Security and Access Control Manager Perspective
**Focus:** Rules protecting the system and controlling access

#### Rule 7.1: User Authentication Requirement
**What it does:** All account detail requests must be made by authenticated users

**When it applies:** Every time a user attempts to retrieve account details

**Who it affects:** All API consumers, security systems

**Example:** When a mobile app requests account details, the system first validates the authentication token. If the token is expired or invalid, the request is rejected with a 401 Unauthorized error before any account data is accessed.

#### Rule 7.2: View Permission Check Before Data Return
**What it does:** System must validate user has permission for the specified view before returning any account data

**When it applies:** After authentication but before retrieving account details from the core system

**Who it affects:** Users, security systems, access control

**Example:** When a user requests account details through the "owner" view, the system checks if they are the account owner or have been explicitly granted owner view access. Without this permission, the request is denied even if they have access to other views on the account.

#### Rule 7.3: Owner View Full Access
**What it does:** Owner view provides full account access with all details and no information filtering

**When it applies:** When an account owner accesses their account through the owner view

**Who it affects:** Account owners

**Example:** When the account owner views their account, they see all information including full account number, complete balance details, all limits, full transaction history access, and sensitive customer information that would be hidden in other views.

#### Rule 7.4: Public View Limited Access
**What it does:** Public view provides only limited account information with sensitive data filtered out

**When it applies:** When an account is accessed through the public view

**Who it affects:** Public API consumers, users with minimal permissions

**Example:** When accessing an account through the public view, the response shows only basic information like account type ("Checking"), currency ("USD"), and masked account number ("****1234"), hiding balance, limits, and routing information.

#### Rule 7.5: Authorization Error for Insufficient Permissions
**What it does:** Users without view permission receive an authorization error rather than empty or partial data

**When it applies:** When a user attempts to access an account without proper view permissions

**Who it affects:** Unauthorized users, security monitoring

**Example:** If a user tries to view account details for an account they don't have permission to access, the system returns a "403 Forbidden" error with message "You do not have permission to view this account" rather than returning empty data or partial information.

---

## Summary of Business Rules by Category

### Access and Permission Rules
- Rule 1.1: View Permission Requirement for Account Access
- Rule 1.2: View-Based Information Filtering
- Rule 7.1: User Authentication Requirement
- Rule 7.2: View Permission Check Before Data Return
- Rule 7.3: Owner View Full Access
- Rule 7.4: Public View Limited Access
- Rule 7.5: Authorization Error for Insufficient Permissions

### Validation and Verification Rules
- Rule 5.1: Bank ID and Account ID Matching Validation
- Rule 5.2: Account Existence Validation
- Rule 5.3: Bank ID Validation
- Rule 5.4: View ID Validation

### Processing and Workflow Rules
- Rule 2.1: Self-Service Account Detail Access
- Rule 2.2: Multi-Version API Support
- Rule 5.5: Core Banking System Integration

### Financial and Calculation Rules
- Rule 3.2: Real-Time Balance Accuracy
- Rule 6.2: Balance Information Accuracy

### Compliance and Audit Rules
- Rule 1.3: Sensitive Information Protection
- Rule 1.4: Audit Trail for Account Detail Access

### Customer and Account Rules
- Rule 2.3: Comprehensive Account Information Display
- Rule 4.1: Account Type Classification Display
- Rule 4.2: Currency Information Requirement
- Rule 4.3: Account Limits and Restrictions Display

### Transaction and Payment Rules
- Rule 6.1: Account Routing Information Provision

### Security and Authentication Rules
- (Covered in Access and Permission Rules section above)

### Performance and Quality Rules
- Rule 3.3: Response Time Performance Requirement

---

## Implementation Considerations

### Critical Business Rules for Migration
When migrating the Account Detail Retrieval functionality to Go, the following business rules are absolutely critical and must be preserved:

1. **View-Based Access Control** (Rules 1.1, 1.2, 7.2, 7.3, 7.4): The view permission system must be replicated exactly to ensure proper data privacy and access control
2. **Information Filtering** (Rules 1.2, 1.3, 7.3, 7.4): Different views must return different levels of information as specified
3. **Validation Rules** (Rules 5.1, 5.2, 5.3, 5.4): All input validation must match the original behavior to maintain API contract
4. **Balance Accuracy** (Rules 3.2, 6.2): Real-time balance retrieval and distinction between current and available balance
5. **Response Format** (Rule 2.3): The JSON response structure must match exactly for API consumers across different versions

### Testing Requirements
Each business rule should have corresponding test cases in the Go application:
- Test authenticated vs unauthenticated requests (Rule 7.1)
- Test view permission validation with various views (Rules 1.1, 7.2)
- Test owner view returns full information (Rule 7.3)
- Test public view returns limited information (Rule 7.4)
- Test information filtering based on view (Rules 1.2, 1.3)
- Test invalid bank ID, account ID, and view ID handling (Rules 5.1, 5.2, 5.3, 5.4)
- Test bank ID and account ID matching (Rule 5.1)
- Test authorization errors for insufficient permissions (Rule 7.5)
- Test response time under load (Rule 3.3)
- Test balance accuracy and real-time retrieval (Rules 3.2, 6.2)
- Test multi-version API compatibility (Rule 2.2)

### Performance Considerations
- Ensure response time meets the 2-second requirement (Rule 3.3)
- Optimize database queries for account detail retrieval
- Implement efficient view permission checks
- Consider caching strategy for relatively static account information while ensuring balance is real-time (Rule 3.2)

### Audit and Compliance
- Implement comprehensive audit logging (Rule 1.4)
- Ensure all access attempts are recorded with view information
- Maintain audit trail for regulatory compliance
- Log sensitive information access for security monitoring

---

## Endpoint-Specific Business Rules

### GET /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views/{VIEW_ID}

**Required Permissions:**
- User must have permission for the specified view

**Validation Rules:**
- Bank ID must be valid (Rule 5.3)
- Account ID must be valid (Rule 5.2)
- View ID must be valid (Rule 5.4)
- Account must belong to specified bank (Rule 5.1)
- User must be authenticated (Rule 7.1)
- User must have view permission (Rules 1.1, 7.2)

**Processing Rules:**
- Filter information based on view (Rules 1.2, 1.3)
- Retrieve real-time balance (Rule 3.2)
- Include routing information if view permits (Rule 6.1)
- Include comprehensive account information (Rule 2.3)
- Complete within 2 seconds (Rule 3.3)
- Log access attempt (Rule 1.4)

### GET /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}

**Required Permissions:**
- User must have account access permissions

**Validation Rules:**
- Bank ID must be valid (Rule 5.3)
- Account ID must be valid (Rule 5.2)
- Account must belong to specified bank (Rule 5.1)
- User must be authenticated (Rule 7.1)

**Processing Rules:**
- Return core account information
- Apply appropriate view-based filtering
- Complete within 2 seconds (Rule 3.3)
- Log access attempt (Rule 1.4)

### GET /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/account

**Required Permissions:**
- Owner or specific view permissions

**Validation Rules:**
- Bank ID must be valid (Rule 5.3)
- Account ID must be valid (Rule 5.2)
- Account must belong to specified bank (Rule 5.1)
- User must be authenticated (Rule 7.1)
- User must be owner or have specific view permission (Rule 7.2)

**Processing Rules:**
- Return full private account details including sensitive information
- Include all balance information (Rule 6.2)
- Include all routing information (Rule 6.1)
- Include all limits and restrictions (Rule 4.3)
- Complete within 2 seconds (Rule 3.3)
- Log access attempt (Rule 1.4)

### GET /obp/v3.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/account

**Required Permissions:**
- View-specific permissions

**Validation Rules:**
- Bank ID must be valid (Rule 5.3)
- Account ID must be valid (Rule 5.2)
- View ID must be valid (Rule 5.4)
- Account must belong to specified bank (Rule 5.1)
- User must be authenticated (Rule 7.1)
- User must have view permission (Rules 1.1, 7.2)

**Processing Rules:**
- Return private account details through specified view
- Filter information based on view (Rules 1.2, 1.3)
- Use v3.0.0 response format (Rule 2.2)
- Complete within 2 seconds (Rule 3.3)
- Log access attempt (Rule 1.4)

### GET /obp/v3.0.0/my/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/account

**Required Permissions:**
- Public view access

**Validation Rules:**
- Bank ID must be valid (Rule 5.3)
- Account ID must be valid (Rule 5.2)
- Account must belong to specified bank (Rule 5.1)
- User must be authenticated (Rule 7.1)

**Processing Rules:**
- Return public account information only (Rule 7.4)
- Filter sensitive information (Rule 1.3)
- Use v3.0.0 response format (Rule 2.2)
- Complete within 2 seconds (Rule 3.3)
- Log access attempt (Rule 1.4)

---

## View-Based Access Control Rules

### Owner View
**Access Level:** Full  
**Information Included:**
- Complete account details
- Full account number
- Current and available balance
- All limits and restrictions
- Complete routing information
- Transaction history access
- Customer information
- All account attributes

**Business Rules:** Rule 7.3

### Public View
**Access Level:** Limited  
**Information Included:**
- Account type
- Currency
- Masked account number (last 4 digits)
- Basic account label
- No balance information
- No routing information
- No customer information

**Business Rules:** Rule 7.4

### Custom Views (Accountant, Auditor, etc.)
**Access Level:** Configurable  
**Information Included:** Based on view configuration  
**Business Rules:** Rules 1.2, 1.3

---

## Error Handling Business Rules

### Invalid Bank ID
**Response:** Error message indicating invalid bank ID  
**HTTP Status:** 400 Bad Request or 404 Not Found  
**Business Rule:** Rule 5.3

### Invalid Account ID
**Response:** Error message indicating account not found  
**HTTP Status:** 404 Not Found  
**Business Rule:** Rule 5.2

### Invalid View ID
**Response:** Error message indicating invalid view  
**HTTP Status:** 400 Bad Request or 404 Not Found  
**Business Rule:** Rule 5.4

### Bank ID and Account ID Mismatch
**Response:** Error message indicating account does not belong to specified bank  
**HTTP Status:** 404 Not Found  
**Business Rule:** Rule 5.1

### Missing Authentication
**Response:** Authentication required error  
**HTTP Status:** 401 Unauthorized  
**Business Rule:** Rule 7.1

### Insufficient View Permissions
**Response:** Permission denied error  
**HTTP Status:** 403 Forbidden  
**Business Rule:** Rules 1.1, 7.2, 7.5

---

## Data Privacy and Security Rules

### Personal Information Protection
- Filter information based on view permissions (Rules 1.2, 1.3)
- Mask sensitive data in limited views (Rule 7.4)
- Log all access for audit purposes (Rule 1.4)
- Validate permissions before returning any data (Rule 7.2)

### View-Based Data Filtering
- Owner view: Full access (Rule 7.3)
- Public view: Limited access (Rule 7.4)
- Custom views: Configurable access (Rules 1.2, 1.3)

---

## Conclusion

This business rules extraction identifies 25 distinct business rules governing the Account Detail Retrieval functionality across 7 different organizational perspectives. These rules cover authentication, authorization, view-based access control, validation, information filtering, balance accuracy, performance, compliance, and data formatting requirements. When migrating this functionality to Go, all these rules must be preserved to ensure the new implementation maintains functional equivalence with the Scala application and can be validated using the existing test cases.

The view-based access control system is particularly critical for this functionality, as it determines what information each user can see based on their permissions. The Go implementation must replicate this system exactly to maintain data privacy and security.
