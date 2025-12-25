# High-Level Requirements Document: Open Bank Project API

## 1. System Overview & Purpose

### System Identification
- **System Name**: Open Bank Project API (OBP-API)
- **Application Code/ID**: OBP-API v1.10.1
- **Business Domain**: Financial Services / Open Banking Platform

### Business Purpose
The Open Bank Project API is an open-source RESTful API platform that abstracts core banking system complexities to enable financial applications to interact with multiple banks on behalf of account holders. The system serves as a standardized banking interface layer, allowing third-party developers, fintech companies, and payment service providers to build financial applications without needing to understand each bank's proprietary systems. The platform's mission is "Bank as a Platform, Transparency as an Asset," enabling banks to expose their services through standardized APIs while maintaining control over backend systems and data access.

### System Criticality
- **Criticality Level**: High
- **Business Impact if Unavailable**: Disruption to third-party financial applications, payment processing failures, inability for customers to access account information through connected applications, regulatory non-compliance for PSD2/Open Banking requirements, and potential financial losses for banks and their customers.

### System Type
- **Architecture**: REST API / Mixed System (API-first with Web UI components)
- **Processing Model**: Mixed - HTTP request-response for real-time API operations, scheduled background tasks for consent management and data cleanup, event-driven processing for webhooks and notifications

### Key Stakeholders
- Third-Party Developers building financial applications (fintech apps, personal finance managers, accounting software)
- Banks seeking to expose their services via standardized APIs
- Payment Service Providers (PSPs) requiring PSD2-compliant APIs for Account Information (AIS) and Payment Initiation (PIS) services
- Regulators and Auditors needing transparent access to financial data through controlled views
- Account Holders accessing their financial data through third-party applications
- Bank IT Operations teams managing API infrastructure and integrations

---

## 2. Core Capabilities Inventory

### Category: Account Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Bank Listing | Retrieve list of all banks supported on the API instance with basic information including ID, name, logo, and website | Real-time | High |
| 2 | Bank Details Retrieval | Get detailed information about a specific bank including attributes and configuration | Real-time | High |
| 3 | Account Listing | Retrieve all accounts for a specific bank that the user has access to | Real-time | High |
| 4 | Account Details Retrieval | Display detailed account information including balance, metadata, and account attributes | Real-time | High |
| 5 | Account Balance Retrieval | Get current balance information for a specific account | Real-time | Very High |
| 6 | Account Creation | Create new bank accounts with specified parameters and ownership | On-demand | Medium |
| 7 | Account Label Update | Update the display label for a bank account | On-demand | Low |
| 8 | Settlement Account Management | Create and manage settlement accounts for payment processing and double-entry bookkeeping | On-demand | Low |
| 9 | Account Routing Lookup | Find accounts by account routing information (IBAN, sort code, etc.) | Real-time | Medium |
| 10 | IBAN Validation | Validate and check IBAN numbers for correctness and errors | Real-time | Medium |
| 11 | Firehose Account Access | Bulk access to all accounts at a bank for authorized applications | Real-time | High |

### Category: Transaction Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Transaction Listing | Browse and retrieve transaction history for an account with filtering and pagination | Real-time | Very High |
| 2 | Transaction Details | View detailed information about a specific transaction | Real-time | High |
| 3 | Double-Entry Transaction View | View double-entry bookkeeping details showing debit and credit sides of transactions | Real-time | Medium |
| 4 | Balancing Transaction Retrieval | Get the balancing transaction for double-entry accounting purposes | Real-time | Medium |
| 5 | Transaction Tagging | Add, view, and delete tags on transactions for categorization and organization | On-demand | Medium |
| 6 | Transaction Metadata Management | Manage additional metadata associated with transactions | On-demand | Low |

### Category: Payment Initiation

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | SEPA Credit Transfer Initiation | Initiate SEPA credit transfer payments between accounts | Real-time | High |
| 2 | Periodic Payment Initiation | Set up recurring/periodic payment instructions | Real-time | Medium |
| 3 | Bulk Payment Initiation | Initiate multiple payments in a single batch request | Real-time | Medium |
| 4 | Transaction Request Creation | Create transaction requests for various payment types (account, counterparty, refund, card) | Real-time | High |
| 5 | Transaction Request Challenge | Answer security challenges for transaction request authorization | Real-time | High |
| 6 | Transaction Request Status | Check the status of pending transaction requests | Real-time | High |
| 7 | Payment Cancellation | Cancel pending payment requests before execution | On-demand | Low |
| 8 | Direct Debit Management | Create and manage direct debit instructions | On-demand | Medium |
| 9 | Standing Order Management | Create and manage standing orders for recurring payments | On-demand | Medium |
| 10 | Agent Cash Withdrawal | Process cash withdrawal requests through agents | Real-time | Medium |

### Category: Customer Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Customer Creation | Create new customer records with personal and contact information | On-demand | Medium |
| 2 | Customer Profile Retrieval | Get customer information by customer ID or other identifiers | Real-time | High |
| 3 | Customer Search | Search for customers by phone number, legal name, or other criteria | Real-time | Medium |
| 4 | Customer Attribute Management | Create, update, and retrieve custom attributes for customers | On-demand | Medium |
| 5 | Customer Address Management | Manage customer address information | On-demand | Low |
| 6 | Customer Messages | Retrieve and manage customer messages and notifications | Real-time | Medium |
| 7 | User-Customer Linking | Link user accounts to customer records | On-demand | Low |
| 8 | KYC Document Management | Manage Know Your Customer documents and verification status | On-demand | Medium |
| 9 | Tax Residence Management | Manage customer tax residence information | On-demand | Low |

### Category: User & Authentication Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | User Authentication | Authenticate users via multiple methods (OAuth, Direct Login, OpenID Connect, Gateway Login) | Real-time | Very High |
| 2 | User Registration | Create new user accounts with credentials and profile information | On-demand | Medium |
| 3 | User Profile Retrieval | Get user information by user ID, username, or email | Real-time | High |
| 4 | User Listing | List all users with filtering and pagination | Real-time | Medium |
| 5 | User Lock/Unlock | Lock or unlock user accounts for security purposes | On-demand | Low |
| 6 | Password Reset | Generate password reset links for users | On-demand | Medium |
| 7 | User Invitation | Create and manage user invitations for onboarding | On-demand | Low |
| 8 | User Deletion | Delete user accounts and associated data | On-demand | Low |
| 9 | Login Attempt Tracking | Track and manage login attempts for security monitoring | Real-time | High |
| 10 | Logout Link Generation | Generate logout links for user session termination | Real-time | Medium |

### Category: Authorization & Access Control

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | View Management | Create, update, and delete views that control data visibility and permissions | On-demand | Medium |
| 2 | View Permission Assignment | Grant and revoke user access to specific views on accounts | On-demand | Medium |
| 3 | Entitlement Management | Create, retrieve, and manage role-based entitlements for users | On-demand | Medium |
| 4 | Scope Management | Manage OAuth scopes for API access control | On-demand | Low |
| 5 | Account Access Grants | Grant users access to specific accounts through views | On-demand | Medium |
| 6 | Account Access Revocation | Revoke user access to accounts | On-demand | Low |
| 7 | Entitlement Request Processing | Process requests for new entitlements | On-demand | Low |

### Category: Consent Management (PSD2 Compliance)

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Consent Creation | Create consent records for third-party access to account information | Real-time | High |
| 2 | Consent Retrieval | Get consent information by consent ID | Real-time | High |
| 3 | Consent Status Check | Check the current status of a consent | Real-time | High |
| 4 | Consent Revocation | Revoke active consents | On-demand | Medium |
| 5 | Consent Listing | List all consents for a user or bank | Real-time | Medium |
| 6 | Consent Authorization | Process consent authorization flows with SCA | Real-time | High |
| 7 | Berlin Group Consent Management | Handle PSD2 Berlin Group compliant consent flows | Real-time | High |
| 8 | Consent Expiration Processing | Automatically expire outdated and invalid consents | Scheduled | Medium |

### Category: Consumer & Application Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Consumer Registration | Register new API consumer applications | On-demand | Low |
| 2 | Consumer Retrieval | Get consumer application details | Real-time | Medium |
| 3 | Consumer Listing | List all registered consumer applications | Real-time | Low |
| 4 | Consumer Update | Update consumer application details (redirect URL, logo, certificate, name) | On-demand | Low |
| 5 | Rate Limit Configuration | Set and manage API rate limits per consumer | On-demand | Low |
| 6 | Consumer Call Limits | Configure per-second, per-minute, per-hour, per-day, per-week, and per-month call limits | On-demand | Low |

### Category: Counterparty Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Counterparty Creation | Create explicit counterparty records for payment recipients | On-demand | Medium |
| 2 | Counterparty Retrieval | Get counterparty details by ID or name | Real-time | High |
| 3 | Counterparty Listing | List all counterparties for an account | Real-time | Medium |
| 4 | Counterparty Deletion | Delete counterparty records | On-demand | Low |
| 5 | Counterparty Limit Management | Set and manage transaction limits for specific counterparties | On-demand | Low |

### Category: Product & Catalog Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Product Creation | Create new banking product definitions | On-demand | Low |
| 2 | Product Retrieval | Get product details and attributes | Real-time | Medium |
| 3 | Product Listing | List all available banking products | Real-time | Medium |
| 4 | Product Fee Management | Create, update, and retrieve product fees | On-demand | Low |
| 5 | Product Collection Management | Organize products into collections | On-demand | Low |
| 6 | Product Attribute Management | Manage custom attributes for products | On-demand | Low |

### Category: Branch & ATM Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Branch Listing | List all branches for a bank | Real-time | Medium |
| 2 | Branch Details | Get detailed information about a specific branch | Real-time | Medium |
| 3 | ATM Listing | List all ATMs for a bank | Real-time | Medium |
| 4 | ATM Details | Get detailed information about a specific ATM | Real-time | Medium |
| 5 | ATM Creation | Create new ATM records | On-demand | Low |
| 6 | ATM Update | Update ATM information including supported languages, currencies, and services | On-demand | Low |
| 7 | ATM Deletion | Delete ATM records | On-demand | Low |
| 8 | ATM Attribute Management | Manage custom attributes for ATMs | On-demand | Low |

### Category: Card Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Card Creation | Create new physical card records | On-demand | Medium |
| 2 | Card Update | Update card information and status | On-demand | Medium |
| 3 | Card Account Listing | List card accounts for Berlin Group compliance | Real-time | Medium |
| 4 | Card Account Balance | Get balance information for card accounts | Real-time | Medium |
| 5 | Card Transaction Listing | List transactions for card accounts | Real-time | High |
| 6 | Card Attribute Management | Manage custom attributes for cards | On-demand | Low |

### Category: Berlin Group PSD2 Compliance

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Account Information Service (AIS) | Provide PSD2-compliant account information access | Real-time | Very High |
| 2 | Payment Initiation Service (PIS) | Provide PSD2-compliant payment initiation | Real-time | High |
| 3 | Confirmation of Funds (PIIS) | Check if sufficient funds are available for payments | Real-time | High |
| 4 | Signing Baskets | Authorize multiple transactions with single SCA process | Real-time | Medium |
| 5 | Strong Customer Authentication (SCA) | Process multi-factor authentication challenges | Real-time | High |
| 6 | SCA Status Tracking | Track the status of SCA authorization processes | Real-time | High |

### Category: Dynamic Configuration & Extensibility

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Dynamic Entity Management | Create, update, and delete runtime-defined data models | On-demand | Low |
| 2 | Dynamic Endpoint Management | Create and manage runtime-defined API endpoints | On-demand | Low |
| 3 | Dynamic Resource Documentation | Create and manage dynamic API documentation | On-demand | Low |
| 4 | Dynamic Message Documentation | Create and manage dynamic message documentation | On-demand | Low |
| 5 | Connector Method Management | Create and manage custom connector methods | On-demand | Low |
| 6 | Endpoint Mapping | Map endpoints to different backend implementations | On-demand | Low |
| 7 | Method Routing | Configure routing of connector methods to different backends | On-demand | Low |
| 8 | JSON Schema Validation | Create and manage JSON schema validations for API requests | On-demand | Low |
| 9 | Authentication Type Validation | Configure authentication requirements for endpoints | On-demand | Low |

### Category: API Collection & Documentation

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | API Collection Creation | Create personal API collections for organizing endpoints | On-demand | Low |
| 2 | API Collection Retrieval | Get API collection details and endpoints | Real-time | Medium |
| 3 | API Collection Sharing | Share API collections with other users | On-demand | Low |
| 4 | Featured Collections | Access featured/curated API collections | Real-time | Low |
| 5 | API Version Information | Get information about available API versions | Real-time | Medium |
| 6 | API Tags Retrieval | Get list of API tags for categorization | Real-time | Low |

### Category: Regulatory & Compliance

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Regulated Entity Management | Create, retrieve, and manage regulated entity records | On-demand | Low |
| 2 | Regulated Entity Attribute Management | Manage attributes for regulated entities | On-demand | Low |
| 3 | FX Rate Management | Create and manage foreign exchange rates | On-demand | Medium |
| 4 | Transaction Type Management | Manage transaction types and their configurations | On-demand | Low |

### Category: Webhook & Notification

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Account Webhook Management | Create and manage webhooks for account events | On-demand | Low |
| 2 | Bank Account Notification | Send notifications for bank account events | Event-driven | High |
| 3 | System Account Notification | Send system-level account notifications | Event-driven | Medium |
| 4 | Webhook Delivery | Deliver webhook payloads to registered endpoints | Event-driven | High |

### Category: Monitoring & Administration

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | API Metrics Collection | Collect and retrieve API usage metrics | Real-time | Very High |
| 2 | Aggregate Metrics | Get aggregated metrics for API usage analysis | Real-time | Medium |
| 3 | Database Information | Get information about the database configuration | On-demand | Low |
| 4 | System Health Check | Check system health and connectivity status | Real-time | Medium |
| 5 | Log Cache Access | Access cached log entries for debugging | On-demand | Low |
| 6 | Web UI Properties Management | Manage web UI configuration properties | On-demand | Low |
| 7 | Endpoint Tag Management | Create and manage tags for API endpoints | On-demand | Low |

### Category: Background Processing & Scheduled Tasks

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Consent Expiration Processing | Automatically expire Berlin Group consents past their valid date | Scheduled (Configurable) | Medium |
| 2 | Unfinished Consent Cleanup | Reject Berlin Group consents that remain in received status too long | Scheduled (Configurable) | Medium |
| 3 | OBP Consent Expiration | Expire OBP native consents past their valid date | Scheduled (Configurable) | Medium |
| 4 | Database Cleanup | Clean up old data and maintain database health | Scheduled | Low |
| 5 | Metrics Archival | Archive old metrics data for long-term storage | Scheduled | Medium |
| 6 | Transaction Processing | Process pending transactions in background | Scheduled | Medium |

### Category: Backend Integration

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Local Mapped Connector | Direct database access for OBP's local data store | Real-time | Very High |
| 2 | REST Connector | HTTP-based communication with external banking backends | Real-time | High |
| 3 | Akka Connector | Actor-based distributed system integration | Real-time | High |
| 4 | Stored Procedure Connector | Database stored procedure integration | Real-time | Medium |
| 5 | RabbitMQ Connector | Message queue integration for asynchronous processing | Event-driven | Medium |
| 6 | Cardano Connector | Blockchain integration for Cardano network | Real-time | Low |
| 7 | Ethereum Connector | Blockchain integration for Ethereum network | Real-time | Low |
| 8 | Dynamic Connector | Runtime-configurable connector for custom integrations | Real-time | Medium |

---

### Capability Summary

- **Total Capabilities Identified**: 145+
- **API Endpoints**: 700+ (across all API versions v1.2.1 through v6.0.0)
  - v1.2.1: 70 endpoints
  - v1.3.0: 3 endpoints
  - v1.4.0: 11 endpoints
  - v2.0.0: 40 endpoints
  - v2.1.0: 28 endpoints
  - v2.2.0: 19 endpoints
  - v3.0.0: 47 endpoints
  - v3.1.0: 103 endpoints
  - v4.0.0: 265 endpoints
  - v5.0.0: 39 endpoints
  - v5.1.0: 106 endpoints
  - v6.0.0: 14 endpoints
  - Berlin Group v1.3: 55 endpoints (AIS: 22, PIS: 24, PIIS: 1, Signing Baskets: 8)
- **Background Tasks**: 6 scheduled tasks
- **Event Consumers**: Webhook-based event processing
- **Batch Jobs**: Database cleanup, metrics archival, consent management
- **External Integrations**: 8 connector types for backend integration
- **Primary Business Functions**: Account Management, Transaction Processing, Payment Initiation, Customer Management, User Authentication, Authorization & Access Control, Consent Management, PSD2 Compliance, Dynamic Configuration, Monitoring & Administration

---

## Open Questions & Clarifications Needed

1. **Volume Estimates**: The volume classifications (High/Medium/Low) are inferred from typical banking API usage patterns. Actual volumes should be validated with operational data.

2. **Scheduled Task Intervals**: The exact intervals for scheduled tasks (consent expiration, database cleanup) are configurable via properties files. Default values and recommended settings should be confirmed with operations teams.

3. **Regional Compliance**: While Berlin Group PSD2 compliance is clearly implemented, the extent of UK Open Banking, Australian Open Banking, and other regional compliance implementations should be clarified.

4. **Blockchain Integration Usage**: The Cardano and Ethereum connectors are present in the codebase, but their production usage and business purpose should be confirmed.

5. **Dynamic Entity Scope**: The extent to which dynamic entities and endpoints are used in production versus sandbox/development environments should be clarified.

6. **Webhook Event Types**: The complete list of supported webhook event types and their business triggers should be documented.

7. **Rate Limiting Defaults**: Default rate limiting configurations and recommended values for different consumer types should be confirmed.

---

*Document generated from source code analysis of OBP-API codebase (obp-api/src/main/scala/code/)*
