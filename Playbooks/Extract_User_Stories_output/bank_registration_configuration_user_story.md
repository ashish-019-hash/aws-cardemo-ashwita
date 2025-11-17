# User Story: Bank Registration and Configuration


## Story Overview


**As a** System Administrator or API Consumer with appropriate permissions  
**I want to** register a new bank in the Open Bank Project API platform with complete configuration details  
**So that** the bank can be made available for third-party applications to access banking services through standardized APIs while maintaining control over backend systems and data access policies


## Acceptance Criteria


1. **Bank Creation with Valid Data**: When I provide valid bank details (ID, full name, short name, logo URL, website URL, and bank routing information), the system successfully creates a new bank entity and returns HTTP 201 with complete bank details including all routing schemes.


2. **Bank ID Validation**: The system validates that the bank ID is greater than 3 characters, contains no spaces, does not contain `::::` characters, matches the short string pattern `^([A-Za-z0-9\-._]+)$`, and is 16 characters or less in length.


3. **Consumer Authentication Required**: The system verifies that the request is made by an authenticated OAuth consumer (not just a user) and rejects requests without valid consumer credentials with error "Invalid consumer credentials".


4. **Automatic Entitlement Assignment**: Upon successful bank creation, the system automatically grants the creating user two entitlements: `CanCreateEntitlementAtOneBank` and `CanReadDynamicResourceDocsAtOneBank` for the newly created bank, enabling them to manage the bank they created.


5. **Settlement Account Auto-Creation (Sandbox Mode)**: In sandbox/mapped connector mode, the system automatically creates two default settlement accounts for the new bank:
   - Incoming settlement account (ID: `OBP_DEFAULT_INCOMING_ACCOUNT_ID`, Name: "Default incoming settlement account", Currency: EUR, Balance: 0)
   - Outgoing settlement account (ID: `OBP_DEFAULT_OUTGOING_ACCOUNT_ID`, Name: "Default outgoing settlement account", Currency: EUR, Balance: 0)


6. **Bank Update Capability**: When a bank with the same ID already exists, the system updates the existing bank's details (full name, short name, logo, website, routing information) rather than creating a duplicate.


7. **Multiple Routing Schemes Support**: The system accepts and stores multiple bank routing schemes (BIC/SWIFT, national identifiers, custom schemes) and returns them in a structured format with scheme and address pairs.


8. **Bank Attributes Support**: After bank creation, the system allows adding custom bank-level attributes (metadata) with name, type, value, and active status for extended configuration beyond standard fields.


9. **Authorization Check**: The system verifies that the user has the `CanCreateBank` entitlement before allowing bank creation, returning appropriate error if the user lacks this permission.


10. **Idempotent Operation**: The create/update operation is idempotent - calling it multiple times with the same bank ID updates the existing record rather than failing or creating duplicates.


## Technical Context


### Classes/Services Involved


**API Layer:**
- `APIMethods400.scala` (line 3621): `createBank` OBPEndpoint - Main API endpoint handling POST /banks requests
- `JSONFactory400.scala`: `createBankJSON400()` - Serializes Bank domain object to BankJson400 response format
- `PostBankJson400` case class - Request body structure with id, short_name, full_name, logo, website, bank_routings
- `BankJson400` case class - Response body structure including attributes field


**Business Logic Layer:**
- `NewStyle.scala`: `createOrUpdateBank()` - Orchestrates bank creation/update with validation and connector invocation
- `APIUtil.scala`: `checkShortString()` - Validates bank ID format (alphanumeric, dash, dot, underscore, max 16 chars)
- `Helper.scala`: `booleanToFuture()` - Converts validation boolean checks to Future for error handling


**Data Access Layer:**
- `Connector.scala`: `createOrUpdateBank()` trait method - Abstract interface for backend bank persistence
- `LocalMappedConnector.scala` (line 294): `createOrUpdateBank()` implementation - Direct database access via Lift Mapper
- `MappedBank.scala`: Database entity mapping for bank table with fields: permalink (ID), fullBankName, shortBankName, logoURL, websiteURL, swiftBIC, national_identifier, mBankRoutingScheme, mBankRoutingAddress
- `MappedBankAccount.scala`: Settlement account creation for incoming/outgoing accounts


**Authorization:**
- `ApiRole.scala`: `CanCreateBank` role definition - Required entitlement for bank creation
- `Entitlement.scala`: `addEntitlement()` - Grants roles to users for bank management


### Input Data


**HTTP Request:**
- Method: POST
- Path: `/obp/v4.0.0/banks`
- Headers: OAuth 1.0a/2.0 authentication headers with consumer credentials
- Body (JSON):
```json
{
  "id": "bank-id-123",
  "short_name": "MyBank",
  "full_name": "My Banking Institution",
  "logo": "https://example.com/logo.png",
  "website": "https://www.mybank.com",
  "bank_routings": [
    {"scheme": "BIC", "address": "MYBANKXX"},
    {"scheme": "NATIONAL_ID", "address": "12345"}
  ]
}
```


**Extracted Parameters:**
- `bank.id`: Bank identifier (permalink)
- `bank.full_name`: Full legal name of the bank
- `bank.short_name`: Abbreviated display name
- `bank.logo`: URL to bank logo image
- `bank.website`: Bank's website URL
- `bank.bank_routings`: List of routing schemes with BIC extracted separately, others as custom routing


### Output Data


**Success Response (HTTP 201):**
```json
{
  "id": "bank-id-123",
  "short_name": "MyBank",
  "full_name": "My Banking Institution",
  "logo": "https://example.com/logo.png",
  "website": "https://www.mybank.com",
  "bank_routings": [
    {"scheme": "OBP", "address": "bank-id-123"},
    {"scheme": "BIC", "address": "MYBANKXX"},
    {"scheme": "NATIONAL_ID", "address": "12345"}
  ],
  "attributes": []
}
```


**Database Records Created:**
1. Bank record in `mappedbank` table with all provided fields
2. Two settlement account records in `mappedaccount` table (sandbox mode only)
3. Two entitlement records in `entitlement` table linking user to bank


**Processing Type:** Real-time REST API (synchronous request-response)


## Business Rules (from code)


1. **Bank ID Format Rule**: Bank ID must match regex `^([A-Za-z0-9\-._]+)$` and be maximum 16 characters. This ensures compatibility with URL paths and database constraints.


2. **Bank ID Length Rule**: Bank ID must be greater than 3 characters to ensure meaningful identifiers and avoid conflicts with reserved keywords.


3. **Bank ID Space Restriction**: Bank ID cannot contain space characters to maintain URL-safe identifiers.


4. **Bank ID Special Character Restriction**: Bank ID cannot contain `::::` character sequence (reserved for internal delimiter usage in the system).


5. **Consumer Authentication Rule**: Bank creation requires an authenticated OAuth consumer (application), not just a user. This ensures proper API client tracking and rate limiting.


6. **Authorization Rule**: User must have `CanCreateBank` entitlement to create banks. This role-based access control prevents unauthorized bank registration.


7. **Auto-Entitlement Rule**: Upon bank creation, the creating user automatically receives `CanCreateEntitlementAtOneBank` and `CanReadDynamicResourceDocsAtOneBank` roles for that specific bank, enabling them to manage permissions and documentation for their bank.


8. **Settlement Account Rule (Sandbox)**: In mapped connector mode (sandbox), two settlement accounts are automatically created with fixed IDs (`OBP_DEFAULT_INCOMING_ACCOUNT_ID`, `OBP_DEFAULT_OUTGOING_ACCOUNT_ID`) and EUR currency. These are used for payment processing reconciliation.


9. **Update-or-Insert Rule**: If a bank with the given ID already exists, the operation updates the existing record rather than failing. This enables configuration updates without separate update endpoints.


10. **BIC Routing Extraction Rule**: If bank_routings contains a scheme "BIC", it is extracted and stored in the dedicated swiftBIC field. Other routing schemes are stored in the generic bankRoutingScheme/Address fields.


11. **OBP Routing Auto-Addition Rule**: The system automatically adds an "OBP" routing scheme with the bank ID as the address to the response, even if not provided in the request. This ensures every bank has an OBP-specific identifier.


## Data Validations (if applicable)


### Input Validations


1. **JSON Format Validation**: Request body must be valid JSON matching PostBankJson400 structure. Invalid JSON returns error "Invalid JSON format. The Json body should be the PostBankJson400".


2. **Bank ID Short String Validation**: 
   - Pattern: `^([A-Za-z0-9\-._]+)$`
   - Max length: 16 characters
   - Returns: "Invalid value length" if > 16 chars, "Invalid value characters" if pattern doesn't match


3. **Bank ID Minimum Length Validation**: Bank ID length must be > 3 characters. Returns: "Invalid JSON format. Min length of BANK_ID should be greater than 3 characters."


4. **Bank ID Space Validation**: Bank ID must not contain spaces. Returns: "Invalid JSON format. BANK_ID can not contain space characters"


5. **Bank ID Special Character Validation**: Bank ID must not contain `::::`. Returns: "Invalid JSON format. BANK_ID can not contain `::::` characters"


6. **Consumer Credentials Validation**: Request must include valid OAuth consumer credentials. Returns: "Invalid consumer credentials" (OBP-20009) if consumer is not present.


7. **Entitlement Validation**: User must have `CanCreateBank` role. Returns: "Insufficient authorisation to create bank" if user lacks this entitlement.


### Data Integrity Validations


8. **Bank ID Uniqueness Check**: System checks if bank ID already exists via `getBankLegacy()`. If exists, performs update; if not, performs insert.


9. **Settlement Account Existence Check**: Before creating settlement accounts, system checks if they already exist to avoid duplicates.


### Error Handling


- All validations return appropriate HTTP status codes (400 for validation errors, 401 for authentication, 403 for authorization)
- Error responses include OBP error codes (e.g., OBP-20009) and descriptive messages
- Database operation failures return "Create bank error" or "Update bank error" messages


## Dependencies


### Upstream Dependencies


**Must Happen Before Bank Creation:**
1. **User Authentication**: User must be authenticated via OAuth 1.0a, OAuth 2.0, or Direct Login
2. **Consumer Registration**: OAuth consumer application must be registered in the system with valid key/secret
3. **Entitlement Grant**: User must have been granted `CanCreateBank` entitlement by a system administrator
4. **Database Availability**: PostgreSQL/H2/MySQL database must be accessible and schema initialized


### Downstream Dependencies


**Happens After Bank Creation:**
1. **Account Creation**: Users with appropriate entitlements can create bank accounts under the new bank
2. **Customer Registration**: Customer profiles can be associated with the new bank
3. **Product Configuration**: Banking products (loans, savings accounts, credit cards) can be defined for the bank
4. **View Configuration**: Custom views can be created to control data access for the bank's accounts
5. **Connector Configuration**: Method routing rules can be configured to route the bank's operations to specific backend connectors
6. **Bank Attribute Management**: Custom attributes can be added to extend bank metadata
7. **Branch/ATM Registration**: Physical locations can be registered under the bank
8. **API Documentation**: Bank-specific API documentation can be generated and customized


### External Systems


1. **Backend Banking System** (Optional): If using REST/Akka/StoredProcedure connectors, the backend system must be configured to recognize the new bank ID
2. **OAuth Provider** (Optional): If using external OAuth (e.g., Hydra), the provider must authenticate the consumer
3. **Database System**: PostgreSQL, MySQL, H2, or MS SQL Server for persistence
4. **Redis Cache** (Optional): For caching bank data if configured


### Internal System Dependencies


1. **Connector Layer**: `Connector.connector.vend.createOrUpdateBank()` must be available
2. **Entitlement System**: `Entitlement.entitlement.vend.addEntitlement()` must be functional
3. **Bank Attribute System**: `BankAttributeX.bankAttributeProvider` must be available for attribute operations
4. **Account System**: `MappedBankAccount` must be available for settlement account creation


## Notes for Implementation


### Special Considerations


1. **Sandbox vs Production Mode**: Settlement account auto-creation only occurs in sandbox mode (when connector=mapped in properties). In production with external connectors, settlement accounts must be created through the backend system.


2. **Idempotency Design**: The create/update operation is designed to be idempotent. The same request can be called multiple times safely - first call creates, subsequent calls update. This is important for retry logic in distributed systems.


3. **Entitlement Auto-Grant**: The automatic granting of `CanCreateEntitlementAtOneBank` and `CanReadDynamicResourceDocsAtOneBank` enables a self-service model where bank creators can manage their own banks without requiring system administrator intervention for every permission.


4. **Routing Scheme Flexibility**: The system supports multiple routing schemes beyond BIC/SWIFT. Custom schemes can be added for country-specific identifiers (e.g., UK Sort Code, US Routing Number, IBAN). The first non-BIC routing is stored in the generic routing fields.


5. **Settlement Account Purpose**: Settlement accounts are used by the payment processing system to track funds in transit. The incoming account receives funds from external sources, the outgoing account tracks funds sent to external destinations. These are reconciliation accounts, not customer-facing accounts.


### Known Complexity


1. **Multi-Connector Support**: In production deployments with StarConnector, the bank creation must be coordinated with backend systems. The OBP database stores the bank metadata, but the backend system must also recognize the bank ID for account/transaction operations.


2. **Concurrent Creation**: If multiple users attempt to create the same bank ID simultaneously, the database unique constraint on permalink will cause one to fail. The code handles this by checking existence first, but race conditions are possible.


3. **Entitlement Check Timing**: The entitlement check happens before bank creation, but entitlement grant happens after. If the grant fails, the bank is already created. This is acceptable as the bank can still be managed by system administrators.


4. **Bank Routing Scheme Mapping**: The code extracts BIC separately from other routings. If multiple BIC schemes are provided, only the first is stored in swiftBIC field. This limitation should be documented.


### Missing or Unclear Requirements Needing SME Input


1. **Bank Deletion**: The code does not include a bank deletion endpoint. Is bank deletion required? If so, what should happen to existing accounts, transactions, and customers associated with the bank?


2. **Bank ID Immutability**: Once created, can a bank ID be changed? The current implementation allows updates to all fields, but changing the ID would break foreign key relationships.


3. **Logo/Website Validation**: Should the system validate that logo and website URLs are accessible and return valid content? Currently, any string is accepted.


4. **National Identifier Format**: Different countries have different formats for national bank identifiers. Should the system validate these based on country codes?


5. **Settlement Account Currency**: Settlement accounts are hardcoded to EUR. Should this be configurable based on the bank's primary operating currency?


6. **Bank Approval Workflow**: Should new banks require approval by a system administrator before becoming active, or are they immediately available for use?


7. **Bank Metadata Requirements**: Are there mandatory fields beyond ID, full name, and short name? Can banks be created with minimal information and filled in later?


8. **Multi-Tenancy Isolation**: How should bank data be isolated in multi-tenant deployments? Should there be additional access controls beyond entitlements?


9. **Audit Trail**: Should bank creation/updates be logged in an audit trail for compliance purposes? Currently, only database timestamps (createdAt, updatedAt) are captured.


10. **Rate Limiting**: Should there be limits on how many banks a single user or consumer can create to prevent abuse?


## API Endpoint Reference


**Endpoint:** `POST /obp/v4.0.0/banks`


**Available Since:** API v4.0.0


**Authentication:** OAuth 1.0a, OAuth 2.0, or Direct Login (with consumer credentials required)


**Required Role:** `CanCreateBank`


**Request Example:**
```bash
curl -X POST https://api.example.com/obp/v4.0.0/banks \
  -H "Authorization: DirectLogin token=eyJhbGc..." \
  -H "Content-Type: application/json" \
  -d '{
    "id": "my-new-bank",
    "short_name": "MNB",
    "full_name": "My New Bank Ltd",
    "logo": "https://example.com/logo.png",
    "website": "https://www.mynewbank.com",
    "bank_routings": [
      {"scheme": "BIC", "address": "MNBKGB2L"}
    ]
  }'
```


**Success Response:** HTTP 201 Created with BankJson400 body


**Error Responses:**
- 400: Invalid JSON format, validation errors
- 401: User not logged in, invalid consumer credentials
- 403: Insufficient authorisation to create bank
- 500: Create bank error, update bank error


**Related Endpoints:**
- `GET /obp/v4.0.0/banks` - List all banks
- `GET /obp/v4.0.0/banks/BANK_ID` - Get specific bank details
- `POST /obp/v4.0.0/management/banks/BANK_ID/bank-attributes` - Add bank attributes
- `GET /obp/v4.0.0/banks/BANK_ID/attributes` - Get bank attributes
