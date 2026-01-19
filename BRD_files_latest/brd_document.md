# High-Level Requirements Document: Open Bank Project API

## 1. System Overview & Purpose

### System Identification
- **System Name**: Open Bank Project API (OBP-API)
- **Application Code/ID**: OBP-API
- **Business Domain**: Financial Services / Open Banking Platform

### Business Purpose
The Open Bank Project API is an open-source RESTful API platform that abstracts core banking system complexities to enable financial applications to interact with multiple banks on behalf of account holders. The system serves as a middleware layer between third-party applications and banking backends, providing standardized APIs for account information, payment initiation, and regulatory compliance. Its mission is "Bank as a Platform, Transparency as an Asset," enabling banks to expose their services through standardized APIs while maintaining control over backend systems and data access.

### System Criticality
- **Criticality Level**: High
- **Business Impact if Unavailable**: Third-party financial applications would lose access to banking data and payment capabilities. Banks would be unable to meet PSD2/Open Banking regulatory compliance requirements. Account holders would be unable to use fintech applications that depend on the platform for account aggregation and payment services.

### System Type
- **Architecture**: REST API / Full-Stack Web Application
- **Processing Model**: Mixed (HTTP request-response, scheduled tasks, event-driven processing)

### Key Stakeholders
- Third-Party Developers (fintech companies, personal finance managers, accounting software providers)
- Banks and Financial Institutions
- Payment Service Providers (PSPs)
- Regulators and Auditors
- Account Holders / End Users

---

## 2. Core Capabilities Inventory

### Category: Bank & Institution Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Bank Information Retrieval | Retrieve information about banks supported on the platform including identifiers, names, logos, and websites | Real-time | High |
| 2 | Bank Creation | Create new bank entities on the platform with associated metadata and configuration | On-demand | Low |
| 3 | Bank Attribute Management | Manage custom attributes associated with banks for extended metadata storage | On-demand | Low |
| 4 | Branch Management | Create, update, retrieve, and delete bank branch information including locations and services | On-demand | Medium |
| 5 | ATM Management | Create, update, retrieve, and delete ATM information including locations, supported languages, currencies, and accessibility features | On-demand | Medium |
| 6 | Settlement Account Management | Create and manage settlement accounts for double-entry bookkeeping and payment processing | On-demand | Low |

### Category: Account Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 7 | Account Listing | Retrieve list of accounts at a bank that a user has access to with various detail levels | Real-time | High |
| 8 | Account Details Retrieval | Get detailed information about a specific account including balance and metadata | Real-time | High |
| 9 | Account Creation | Create new bank accounts with specified parameters and ownership | On-demand | Medium |
| 10 | Account Label Update | Update the display label for a bank account | On-demand | Low |
| 11 | Account Balance Retrieval | Get current balance information for accounts | Real-time | Very High |
| 12 | Account Routing Lookup | Find accounts by routing information such as IBAN or account number | Real-time | High |
| 13 | Account Attribute Management | Manage custom attributes associated with accounts | On-demand | Medium |
| 14 | Firehose Account Access | Bulk access to all accounts at a bank for authorized applications | Real-time | High |
| 15 | Account Balance Management | Create, update, and delete account balance records | On-demand | Medium |

### Category: Transaction Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 16 | Transaction Listing | Retrieve transaction history for accounts with filtering and pagination | Real-time | Very High |
| 17 | Transaction Details | Get detailed information about a specific transaction | Real-time | High |
| 18 | Double-Entry Transaction Retrieval | View double-entry bookkeeping transactions showing debit and credit sides | Real-time | Medium |
| 19 | Balancing Transaction Retrieval | Get the balancing transaction for a given transaction | Real-time | Medium |
| 20 | Transaction Tagging | Add, retrieve, and delete tags on transactions for categorization | On-demand | Medium |
| 21 | Transaction Attribute Management | Manage custom attributes associated with transactions | On-demand | Medium |

### Category: Payment Initiation

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 22 | SEPA Credit Transfer Initiation | Initiate SEPA credit transfer payments between accounts | Real-time | High |
| 23 | Account-to-Account Transfer | Initiate transfers between accounts within the platform | Real-time | High |
| 24 | Counterparty Payment | Initiate payments to registered counterparties | Real-time | High |
| 25 | Card Payment Processing | Process card-based payment transactions | Real-time | High |
| 26 | Refund Processing | Process refund transactions for previous payments | On-demand | Medium |
| 27 | Free-Form Payment | Initiate payments with flexible parameters | Real-time | Medium |
| 28 | Simple Payment | Initiate basic payment transactions with minimal parameters | Real-time | High |
| 29 | Agent Cash Withdrawal | Process cash withdrawal requests through agents | On-demand | Medium |
| 30 | Periodic Payment Initiation | Set up recurring/periodic payment schedules | On-demand | Medium |
| 31 | Bulk Payment Initiation | Initiate multiple payments in a single batch | On-demand | Medium |
| 32 | Payment Cancellation | Cancel pending payment transactions | On-demand | Low |
| 33 | Transaction Request Status | Check the status of payment/transaction requests | Real-time | High |
| 34 | Challenge Response | Answer security challenges for payment authorization | Real-time | High |

### Category: Direct Debit & Standing Orders

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 35 | Direct Debit Creation | Create direct debit mandates for recurring collections | On-demand | Medium |
| 36 | Direct Debit Management | Manage and administer direct debit arrangements | On-demand | Low |
| 37 | Standing Order Creation | Create standing orders for recurring payments | On-demand | Medium |
| 38 | Standing Order Management | Manage and administer standing order arrangements | On-demand | Low |

### Category: Customer Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 39 | Customer Creation | Create new customer records with personal and contact information | On-demand | Medium |
| 40 | Customer Retrieval | Retrieve customer information by various identifiers | Real-time | High |
| 41 | Customer Search | Search for customers by phone number, legal name, or other criteria | Real-time | Medium |
| 42 | Customer Attribute Management | Manage custom attributes associated with customers | On-demand | Medium |
| 43 | Customer Address Management | Manage customer address information | On-demand | Low |
| 44 | Customer Messages | Send and retrieve messages to/from customers | On-demand | Medium |
| 45 | Customer-Account Linking | Link customers to bank accounts | On-demand | Medium |

### Category: KYC (Know Your Customer)

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 46 | KYC Status Management | Manage customer KYC verification status | On-demand | Medium |
| 47 | KYC Document Management | Store and manage KYC-related documents | On-demand | Medium |
| 48 | KYC Check Management | Record and manage KYC verification checks | On-demand | Medium |
| 49 | KYC Media Management | Manage media files related to KYC processes | On-demand | Low |

### Category: Counterparty Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 50 | Counterparty Creation | Create counterparty records for payment recipients | On-demand | Medium |
| 51 | Counterparty Retrieval | Retrieve counterparty information by ID or name | Real-time | High |
| 52 | Counterparty Deletion | Remove counterparty records | On-demand | Low |
| 53 | Counterparty Limit Management | Set and manage transaction limits for counterparties | On-demand | Low |

### Category: User & Access Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 54 | User Creation | Create new user accounts with specified roles | On-demand | Medium |
| 55 | User Retrieval | Retrieve user information by ID, username, or email | Real-time | High |
| 56 | User Listing | List all users with filtering and pagination | Real-time | Medium |
| 57 | User Deletion | Remove user accounts from the system | On-demand | Low |
| 58 | User Lock/Unlock | Lock or unlock user accounts for security purposes | On-demand | Low |
| 59 | User Invitation | Send invitations for new users to join the platform | On-demand | Low |
| 60 | User Attribute Management | Manage custom attributes associated with users | On-demand | Medium |
| 61 | Current User Information | Retrieve information about the currently authenticated user | Real-time | Very High |

### Category: View & Permission Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 62 | View Creation | Create custom views defining data access permissions | On-demand | Low |
| 63 | View Retrieval | Retrieve view definitions and permissions | Real-time | High |
| 64 | View Update | Modify view permissions and configurations | On-demand | Low |
| 65 | View Deletion | Remove custom views | On-demand | Low |
| 66 | System View Management | Manage predefined system views | On-demand | Low |
| 67 | View Access Grant | Grant user access to specific views on accounts | On-demand | Medium |
| 68 | View Access Revocation | Revoke user access to views | On-demand | Medium |

### Category: Entitlement & Role Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 69 | Entitlement Listing | List entitlements for users | Real-time | Medium |
| 70 | Entitlement Creation | Grant entitlements/roles to users | On-demand | Medium |
| 71 | Entitlement Deletion | Remove entitlements from users | On-demand | Low |
| 72 | Scope Management | Manage OAuth scopes for API access control | On-demand | Low |
| 73 | Entitlement Request Management | Process requests for new entitlements | On-demand | Low |

### Category: Consent Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 74 | Consent Creation | Create consent records for account access authorization | Real-time | High |
| 75 | Consent Retrieval | Retrieve consent information and status | Real-time | High |
| 76 | Consent Revocation | Revoke previously granted consents | On-demand | Medium |
| 77 | Consent Status Update | Update the status of consent records | On-demand | Medium |
| 78 | Consent Account Access Update | Modify account access permissions within a consent | On-demand | Low |

### Category: PSD2 Berlin Group Compliance (Account Information Service - AIS)

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 79 | Berlin Group Consent Creation | Create PSD2-compliant consent for account information access | Real-time | High |
| 80 | Berlin Group Consent Deletion | Delete/revoke Berlin Group consents | On-demand | Medium |
| 81 | Berlin Group Account List | Retrieve account list per Berlin Group specification | Real-time | High |
| 82 | Berlin Group Balance Retrieval | Get account balances per Berlin Group specification | Real-time | High |
| 83 | Berlin Group Transaction List | Retrieve transactions per Berlin Group specification | Real-time | High |
| 84 | Berlin Group Consent Status | Check consent status per Berlin Group specification | Real-time | High |
| 85 | Berlin Group Consent Authorisation | Manage consent authorisation sub-resources | Real-time | High |
| 86 | Berlin Group Card Account Access | Access card account information per Berlin Group specification | Real-time | Medium |

### Category: PSD2 Berlin Group Compliance (Payment Initiation Service - PIS)

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 87 | Berlin Group Payment Initiation | Initiate payments per Berlin Group specification | Real-time | High |
| 88 | Berlin Group Periodic Payment | Initiate periodic payments per Berlin Group specification | On-demand | Medium |
| 89 | Berlin Group Bulk Payment | Initiate bulk payments per Berlin Group specification | On-demand | Medium |
| 90 | Berlin Group Payment Status | Check payment status per Berlin Group specification | Real-time | High |
| 91 | Berlin Group Payment Cancellation | Cancel payments per Berlin Group specification | On-demand | Medium |
| 92 | Berlin Group Payment Authorisation | Manage payment authorisation sub-resources | Real-time | High |
| 93 | Berlin Group SCA Status | Check Strong Customer Authentication status | Real-time | High |

### Category: PSD2 Berlin Group Compliance (Confirmation of Funds - PIIS)

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 94 | Funds Availability Check | Check if sufficient funds are available for a transaction | Real-time | High |

### Category: PSD2 Berlin Group Compliance (Signing Baskets)

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 95 | Signing Basket Creation | Create signing baskets for batch authorisation | On-demand | Medium |
| 96 | Signing Basket Retrieval | Retrieve signing basket information | Real-time | Medium |
| 97 | Signing Basket Deletion | Delete signing baskets | On-demand | Low |
| 98 | Signing Basket Authorisation | Authorise signing baskets with SCA | Real-time | Medium |
| 99 | Signing Basket Status | Check signing basket status | Real-time | Medium |

### Category: Product & Service Catalog

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 100 | Product Creation | Create banking product definitions | On-demand | Low |
| 101 | Product Retrieval | Retrieve product information and details | Real-time | Medium |
| 102 | Product Tree Retrieval | Get hierarchical product structure | Real-time | Low |
| 103 | Product Collection Management | Manage collections of related products | On-demand | Low |
| 104 | Product Attribute Management | Manage custom attributes for products | On-demand | Low |
| 105 | Product Fee Management | Define and manage fees associated with products | On-demand | Low |

### Category: Card Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 106 | Card Creation | Create new card records for accounts | On-demand | Medium |
| 107 | Card Update | Update card information and status | On-demand | Medium |
| 108 | Card Deletion | Remove card records | On-demand | Low |
| 109 | Card Attribute Management | Manage custom attributes for cards | On-demand | Low |

### Category: Consumer/Application Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 110 | Consumer Registration | Register new API consumer applications | On-demand | Medium |
| 111 | Consumer Retrieval | Retrieve consumer application information | Real-time | Medium |
| 112 | Consumer Update | Update consumer application details | On-demand | Low |
| 113 | Consumer Enable/Disable | Activate or deactivate consumer applications | On-demand | Low |
| 114 | Consumer Rate Limiting | Set API call limits for consumer applications | On-demand | Low |
| 115 | Consumer Certificate Management | Manage client certificates for mTLS authentication | On-demand | Low |

### Category: Authentication & Security

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 116 | OAuth 1.0a Authentication | Authenticate users via OAuth 1.0a protocol | Real-time | High |
| 117 | OAuth 2.0 / OpenID Connect | Authenticate users via OAuth 2.0 and OIDC | Real-time | Very High |
| 118 | Direct Login | Authenticate users with direct credentials | Real-time | High |
| 119 | Gateway Login | Authenticate via external gateway systems | Real-time | Medium |
| 120 | Logout | Terminate user sessions | On-demand | High |
| 121 | Challenge Creation | Create security challenges for transaction authorisation | Real-time | High |
| 122 | Challenge Validation | Validate challenge responses for SCA | Real-time | High |
| 123 | mTLS Certificate Information | Retrieve client certificate information for mTLS | Real-time | Medium |
| 124 | Authentication Type Validation | Validate authentication methods for endpoints | Real-time | High |
| 125 | JSON Schema Validation | Validate request payloads against JSON schemas | Real-time | High |

### Category: Dynamic Configuration

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 126 | Dynamic Entity Management | Create, update, and delete runtime-defined data entities | On-demand | Low |
| 127 | Dynamic Endpoint Management | Create, update, and delete runtime-defined API endpoints | On-demand | Low |
| 128 | Dynamic Resource Documentation | Manage documentation for dynamic resources | On-demand | Low |
| 129 | Dynamic Message Documentation | Manage connector message documentation | On-demand | Low |
| 130 | Endpoint Mapping Management | Configure endpoint routing and mapping | On-demand | Low |
| 131 | Endpoint Tag Management | Manage tags for API endpoints | On-demand | Low |
| 132 | Method Routing Configuration | Configure connector method routing rules | On-demand | Low |
| 133 | Connector Method Management | Manage custom connector methods | On-demand | Low |
| 134 | Web UI Properties Management | Configure web interface properties | On-demand | Low |

### Category: API Collections & Documentation

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 135 | API Collection Creation | Create collections of API endpoints | On-demand | Low |
| 136 | API Collection Retrieval | Retrieve API collection information | Real-time | Medium |
| 137 | API Collection Endpoint Management | Add and remove endpoints from collections | On-demand | Low |
| 138 | Featured Collections | Retrieve featured/highlighted API collections | Real-time | Medium |
| 139 | Resource Documentation | Generate and retrieve API documentation | Real-time | Medium |
| 140 | Swagger/OpenAPI Generation | Generate OpenAPI specifications for APIs | Real-time | Medium |
| 141 | Message Documentation | Retrieve connector message documentation | Real-time | Low |

### Category: Metrics & Monitoring

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 142 | API Metrics Collection | Collect metrics on API usage and performance | Real-time | Very High |
| 143 | Aggregate Metrics Retrieval | Retrieve aggregated API usage statistics | Real-time | Medium |
| 144 | Detailed Metrics Retrieval | Retrieve detailed API call metrics | Real-time | Medium |
| 145 | Rate Limiting Information | Retrieve rate limiting status and configuration | Real-time | Medium |

### Category: Scheduled Background Tasks

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 146 | Consent Expiration Processing | Automatically expire outdated and invalid consents | Scheduled (Periodic) | Medium |
| 147 | Berlin Group Consent Cleanup | Process unfinished Berlin Group consents | Scheduled (Periodic) | Medium |
| 148 | Metrics Archival | Archive old metrics data to maintain performance | Scheduled (Daily) | High |
| 149 | Database Maintenance | Perform database cleanup and maintenance tasks | Scheduled (Periodic) | Low |
| 150 | Transaction Processing | Process pending transactions in background | Scheduled (Periodic) | Medium |

### Category: Webhook & Notification

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 151 | Account Webhook Management | Configure webhooks for account events | On-demand | Low |
| 152 | Bank Account Notification | Send notifications for bank account events | Event-driven | High |
| 153 | System Account Notification | Send system-level account notifications | Event-driven | Medium |

### Category: Foreign Exchange

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 154 | FX Rate Management | Create and manage foreign exchange rates | On-demand | Low |
| 155 | Currency Conversion | Convert amounts between currencies | Real-time | High |

### Category: Regulatory & Compliance

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 156 | Regulated Entity Management | Manage regulated entity registrations | On-demand | Low |
| 157 | Regulated Entity Attribute Management | Manage attributes for regulated entities | On-demand | Low |
| 158 | Tax Residence Management | Manage customer tax residence information | On-demand | Low |

### Category: CRM & Meetings

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 159 | Meeting Creation | Schedule meetings between bank staff and customers | On-demand | Low |
| 160 | Meeting Retrieval | Retrieve meeting information | Real-time | Low |
| 161 | CRM Event Management | Manage customer relationship events | On-demand | Low |

### Category: System Administration

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 162 | Database Information | Retrieve database configuration and status | On-demand | Low |
| 163 | Adapter Information | Retrieve backend adapter configuration | On-demand | Low |
| 164 | System Configuration | Retrieve and manage system configuration | On-demand | Low |
| 165 | API Version Information | Retrieve available API versions | Real-time | Medium |
| 166 | Health Check | Check system health and connectivity | Real-time | High |
| 167 | Call Context Retrieval | Retrieve request context information for debugging | On-demand | Low |

### Category: IBAN Validation

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 168 | IBAN Validation | Validate and check IBAN numbers for correctness | Real-time | High |

---

### Capability Summary

- **Total Capabilities Identified**: 168
- **API Endpoints**: ~150+
- **Background Tasks**: 5
- **Event Consumers/Webhooks**: 3
- **External Integrations**: Multiple backend connectors (REST, Akka, Stored Procedure, Kafka, RabbitMQ, Cardano, Ethereum)
- **Primary Business Functions**: 
  - Account Information Services
  - Payment Initiation Services
  - Consent Management
  - User & Access Management
  - PSD2/Open Banking Compliance
  - Customer Management
  - Product Catalog Management
  - API Management & Documentation
  - Metrics & Monitoring

---

## Open Questions & Clarifications Needed

1. **Backend Integration Specifics**: The system supports multiple backend connectors (REST, Akka, Stored Procedure, Kafka, RabbitMQ, Cardano, Ethereum). What are the specific backend systems currently in use and their integration requirements?

2. **Regional Compliance**: Beyond Berlin Group PSD2, are there other regional Open Banking standards (UK Open Banking, Australian CDR, etc.) that need to be supported in the target system?

3. **Transaction Volume Estimates**: What are the expected peak transaction volumes for payment initiation and account information services?

4. **SCA Methods**: Which Strong Customer Authentication methods (SMS OTP, Email, CHIP OTP, etc.) are required for the target system?

5. **Consent Lifecycle**: What are the specific business rules for consent expiration, renewal, and revocation beyond the standard PSD2 requirements?

6. **Webhook Destinations**: What external systems need to receive webhook notifications for account and transaction events?

7. **Metrics Retention**: What are the specific requirements for metrics data retention and archival periods?

8. **Dynamic Entity Usage**: Are there specific use cases for the dynamic entity and endpoint features that need to be preserved in the migration?

9. **Multi-tenancy**: Is the system deployed as a multi-tenant platform serving multiple banks, or as single-tenant instances per bank?

10. **Batch Processing Windows**: What are the acceptable processing windows for scheduled background tasks like consent cleanup and metrics archival?

---

*This document was generated by analyzing the OBP-API Scala codebase and extracting business capabilities from source code artifacts including API endpoint definitions, connector interfaces, scheduled tasks, and domain models.*
