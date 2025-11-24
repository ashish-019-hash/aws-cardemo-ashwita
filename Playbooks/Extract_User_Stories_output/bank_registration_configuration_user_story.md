# User Stories for Open Bank Project (OBP) API - Bank Registration and Configuration

## Part 1: Capability Inventory

### Bank Management
1. **Bank Registration and Configuration**
   - Classes/Services: Bank management services, bank entity controllers, bank configuration handlers
   - Type: REST API
   - Purpose: Enable creation and management of bank entities with identification details, branding elements (logos, colors), and operational parameters to support multi-bank deployments on a single API instance
   - Frequency: On-demand

## Part 2: Detailed User Stories

### Priority: High

## User Story: Bank Registration and Configuration

### Story Overview
**As a** Bank Administrator or System Administrator
**I want to** create and configure bank entities with complete identification, branding, and operational parameters
**So that** the banking institution can be properly registered in the OBP API platform, maintain its unique identity, and operate independently within a multi-bank environment while ensuring proper data isolation and brand consistency

### Acceptance Criteria
1. System shall allow creation of a new bank entity with unique bank identification code (BIC/SWIFT code or similar)
2. System shall accept and store bank branding information including:
   - Bank name (full legal name and display name)
   - Bank logo (URL or uploaded image)
   - Bank website URL
   - Brand colors and visual identity elements
3. System shall configure operational parameters for the bank including:
   - Supported currencies
   - Operating hours and timezone
   - Contact information (phone, email, address)
   - Regulatory compliance settings
4. System shall validate that bank identification codes are unique across the platform
5. System shall ensure data isolation between different banks on the same API instance
6. System shall allow retrieval of bank configuration details for display and operational purposes
7. System shall support updating bank configuration parameters after initial registration
8. System shall maintain audit trail of all bank configuration changes
9. System shall validate all required fields are provided during bank registration
10. System shall return appropriate error messages for invalid or duplicate bank registrations

### Technical Context
- **Classes/Services Involved**: 
  - Bank entity management service (handles CRUD operations for bank entities)
  - Bank configuration controller (REST API endpoints for bank management)
  - Bank validation service (validates bank data and ensures uniqueness)
  - Bank attribute management service (handles custom bank-level metadata)
  - Multi-tenancy isolation service (ensures data separation between banks)
- **Input Data**: 
  - Bank identification code (BIC/SWIFT or equivalent)
  - Bank name (legal and display names)
  - Branding information (logo URL, website, colors)
  - Operational parameters (currencies, timezone, contact details)
  - Custom attributes (extended metadata as key-value pairs)
- **Output Data**: 
  - Bank entity record with unique identifier
  - Confirmation of successful registration
  - Bank configuration details for retrieval
  - Error messages for validation failures
- **Processing Type**: REST API (synchronous request-response)

### Core Bank Entity Fields (from Scala Model)

Based on the `MappedBank.scala` model, the core bank entity consists of the following fields:

- **bankId** (permalink): Unique identifier used in URLs
- **fullName** (fullBankName): Full legal name of the bank
- **shortName** (shortBankName): Short display name of the bank
- **logoUrl** (logoURL): URL to the bank's logo image
- **websiteUrl** (websiteURL): Bank's website URL
- **swiftBic** (swiftBIC): SWIFT/BIC code for international transfers
- **nationalIdentifier**: National bank identifier
- **bankRoutingScheme**: Routing scheme type (e.g., OBP, BIC)
- **bankRoutingAddress**: Routing address for the specified scheme

These fields represent the identification, branding, and operational parameters that define a bank entity in the OBP API system.

### API Endpoints (Extracted from OBP-API Scala Codebase)

#### Bank Creation Endpoint

**POST /obp/v4.0.0/banks**
- **Endpoint Name**: createBank
- **Description**: Create a new bank (Authenticated access). The user creating this will be automatically assigned the Role CanCreateEntitlementAtOneBank, allowing them to manage the bank they create and assign roles to other users. Only available in SANDBOX mode (when connector=mapped in properties file). Settlement accounts are automatically created by the system when the bank is created.
- **Authentication**: Required (OAuth)
- **Authorization**: Requires `canCreateBank` entitlement
- **Request Parameters**: None
- **Request Body**: PostBankJson400
  ```json
  {
    "id": "string",
    "full_name": "string",
    "short_name": "string",
    "logo": "string",
    "website": "string",
    "bank_routings": [
      {
        "scheme": "string",
        "address": "string"
      }
    ]
  }
  ```
- **Validation Rules**:
  - BANK_ID must be greater than 3 characters
  - BANK_ID cannot contain space characters
  - BANK_ID cannot contain `::::` characters
  - BANK_ID must pass short string validation
- **Response**: BankJson400 (HTTP 201 Created)
  ```json
  {
    "id": "string",
    "full_name": "string",
    "short_name": "string",
    "logo": "string",
    "website": "string",
    "bank_routings": [
      {
        "scheme": "string",
        "address": "string"
      }
    ],
    "attributes": []
  }
  ```
- **Scala Implementation**: `code/api/v4_0_0/APIMethods400.scala` (line 3590-3683)
- **Use Case**: Register a new bank entity in the system with identification, branding, and routing information
- **Note**: Settlement accounts are automatically created (Default incoming settlement account with ID OBP_DEFAULT_INCOMING_ACCOUNT_ID and Default outgoing settlement account with ID OBP_DEFAULT_OUTGOING_ACCOUNT_ID, both in EUR currency)

#### Notes on Bank Management (Update) Endpoint
Based on the systematic search across all API versions (v1.2.1 through v6.0.0), there is **no explicit PUT /banks/BANK_ID endpoint** for updating existing bank entities. The core bank entity fields (name, logo, website, routing) appear to be immutable after creation, which aligns with the business rule that "Critical identification fields (like bank ID) cannot be modified after initial registration to maintain referential integrity."

Therefore, the "Bank Registration and Configuration" capability in the OBP-API Scala application consists of **only the creation endpoint (POST /banks)** for creating new bank entities. There is no REST API endpoint for updating/managing existing bank entities after creation.

#### API Version Notes
- The endpoints documented above are from **OBP API v4.0.0**
- The createBank endpoint also exists in **v6.0.0** with identical functionality
- Similar bank retrieval endpoints (GET /banks, GET /banks/BANK_ID) exist across all API versions (v1.2.1, v2.0.0, v2.1.0, v2.2.0, v3.0.0, v3.1.0, v5.0.0, v5.1.0, v6.0.0)
- The endpoint structure and functionality remain largely consistent across versions with minor variations in response formats
- For migration purposes, focus on v4.0.0 or later as they represent the most mature and feature-complete implementations

#### Bank Attributes (Out of Scope for This Capability)
The OBP API also provides bank attribute endpoints (POST/GET/PUT/DELETE /banks/BANK_ID/attributes/*) for managing custom key-value pairs that extend bank configuration beyond the core entity fields. However, per the user's clarification, bank attributes are considered a separate capability and are not included in "Bank Registration and Configuration" which focuses solely on creating and managing the core bank entity with identification, branding, and operational parameters.

### Business Rules (from code)
1. **Unique Bank Identification**: Each bank must have a unique identification code (BIC/SWIFT or equivalent) that cannot be duplicated across the platform
2. **Multi-Bank Isolation**: Data for different banks must be completely isolated to prevent cross-bank data access or contamination
3. **Required Fields Validation**: Bank name and identification code are mandatory fields that must be provided during registration
4. **Branding Consistency**: Bank branding information (logo, colors, website) must be consistently applied across all API responses and UI elements for that bank
5. **Operational Parameter Defaults**: If operational parameters are not provided, system shall apply sensible defaults (e.g., UTC timezone, standard business hours)
6. **Attribute Extensibility**: Banks can define custom attributes beyond standard fields to support institution-specific requirements
7. **Configuration Immutability**: Critical identification fields (like bank ID) cannot be modified after initial registration to maintain referential integrity
8. **Audit Trail Requirement**: All configuration changes must be logged with timestamp, user, and change details for compliance and audit purposes

### Data Validations (if applicable)
- Bank identification code format validation (BIC/SWIFT format compliance)
- Bank identification code uniqueness check across all registered banks
- URL format validation for bank website and logo URLs
- Email format validation for contact email addresses
- Phone number format validation for contact numbers
- Currency code validation against ISO 4217 standard
- Timezone validation against standard timezone database
- Required field presence validation (bank ID, bank name)
- String length validation for text fields (name, description, etc.)
- Logo image format and size validation (if uploaded directly)

### Dependencies
- **Upstream**: 
  - System administrator authentication and authorization
  - Valid OAuth consumer credentials for API access
  - Proper entitlements/roles for bank creation operations
- **Downstream**: 
  - Account creation services (require valid bank ID)
  - Customer management services (link customers to banks)
  - Product catalog services (associate products with banks)
  - Branch and ATM management (link locations to banks)
  - Transaction processing (all transactions linked to bank entities)
  - Consent management (consents are bank-specific)
  - User access control (users granted access to specific banks)
- **External Systems**: 
  - None directly, but bank configuration may reference external branding assets (logo images hosted externally)
  - May integrate with identity providers for bank-specific authentication

### Notes for Implementation
- **Multi-Tenancy Architecture**: The implementation must ensure complete data isolation between banks at the database level, likely using bank_id as a partition key or tenant identifier in all related tables
- **Caching Considerations**: Bank configuration data is read frequently but updated rarely, making it an excellent candidate for caching. Implement cache invalidation strategy when bank configuration is updated
- **API Versioning**: Bank registration endpoints may exist across multiple API versions (v1.2.1 through v6.0.0). Ensure backward compatibility when adding new configuration fields
- **Branding Asset Management**: Consider implementing a content delivery network (CDN) integration for bank logos and branding assets to improve performance
- **Configuration Validation**: Some operational parameters may have interdependencies (e.g., supported currencies must align with available FX rates). Implement comprehensive validation logic
- **Migration Support**: For existing deployments, provide migration tools to register legacy banks that may already exist in the system
- **Sandbox vs Production**: Bank registration may behave differently in sandbox mode (allowing test banks) versus production (requiring verified bank credentials)
- **Regulatory Compliance**: Bank registration may need to capture additional regulatory information depending on jurisdiction (e.g., regulatory license numbers, compliance certifications)
- **Missing or Unclear Requirements Needing SME Input**:
  - What is the approval workflow for bank registration? Is it automatic or requires manual approval?
  - Are there any restrictions on who can register a bank (e.g., only super admins)?
  - What happens to all related data (accounts, customers, transactions) if a bank is deactivated or deleted?
  - Are there any limits on the number of banks that can be registered on a single instance?
  - What are the specific regulatory requirements for bank registration in different jurisdictions?
  - How are bank mergers or acquisitions handled (combining two bank entities)?
  - Is there a bank verification process to confirm the legitimacy of the registering institution?

## Part 3: Open Questions

1. **Bank Approval Workflow**: Is there a manual approval process for bank registration, or is it automatically approved upon successful validation? Who has the authority to approve new bank registrations?

2. **Bank Deactivation/Deletion**: What is the process for deactivating or deleting a bank? How is related data (accounts, customers, transactions) handled? Is soft delete used to maintain historical records?

3. **Bank Verification**: Is there a verification process to confirm that the registering entity is a legitimate banking institution? What documentation or credentials are required?

4. **Regulatory Requirements**: What specific regulatory information must be captured during bank registration for different jurisdictions (e.g., EU, UK, Australia, US)?

5. **Bank Hierarchy**: Does the system support bank hierarchies (e.g., parent bank with multiple subsidiary banks)? How are relationships between related banking entities managed?

6. **Bank Limits**: Are there any limits on the number of banks that can be registered on a single API instance? What are the scalability considerations?

7. **Bank Migration**: How are existing banks migrated into the system if they were operating before the OBP API was implemented? Is there a bulk import capability?

8. **Bank Mergers**: How are bank mergers or acquisitions handled? Can two bank entities be merged, and what happens to their associated data?

9. **Bank Branding Updates**: How frequently can bank branding be updated? Are there any approval workflows for branding changes?

10. **Multi-Region Support**: Can a single bank entity operate across multiple regions/countries, or must separate bank entities be created for each jurisdiction?
