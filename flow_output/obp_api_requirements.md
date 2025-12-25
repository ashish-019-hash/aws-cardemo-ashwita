# High-Level Requirements Document: Open Bank Project API

## 1. System Overview & Purpose

### System Identification
- **System Name**: Open Bank Project (OBP) API
- **Application Code/ID**: OBP-API
- **Business Domain**: Financial Services / Open Banking

### Business Purpose
The Open Bank Project API is an open-source RESTful API platform designed to abstract core banking system complexities and enable financial applications to interact with multiple banks on behalf of account holders. The system serves as a standardized banking interface layer that allows third-party developers, fintech companies, and payment service providers to build financial applications without needing to understand each bank's proprietary systems. The platform supports regulatory compliance requirements including PSD2 (Payment Services Directive 2) for European markets, enabling Account Information Services (AIS), Payment Initiation Services (PIS), and Confirmation of Funds Services (PIIS).

### System Criticality
- **Criticality Level**: High
- **Business Impact if Unavailable**: Disruption to all third-party financial applications relying on the API for banking operations, inability to process payments, view account information, or perform regulatory-compliant banking operations. Banks would lose their open banking connectivity, affecting customer experience and regulatory compliance.

### System Type
- **Architecture**: REST API / Full-Stack (with Web UI components)
- **Processing Model**: Mixed (HTTP request-response for real-time operations, event-driven for notifications, scheduled tasks for maintenance)

### Key Stakeholders
- Third-Party Developers and Fintech Companies
- Banks and Financial Institutions
- Payment Service Providers (PSPs)
- Regulatory Bodies and Auditors
- Account Holders / End Customers
- Bank Operations and IT Teams

---

## 2. Core Capabilities Inventory

### Category: Bank Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Bank Information Retrieval | Retrieve information about banks supported on the platform including identifiers, names, logos, and websites | Real-time | High |
| 2 | Bank Creation | Create new bank entities on the platform with associated settlement accounts | On-demand | Low |
| 3 | Bank Update | Update existing bank information including routing details and attributes | On-demand | Low |
| 4 | Bank Attribute Management | Create, update, and retrieve custom attributes associated with banks | On-demand | Low |
| 5 | Settlement Account Management | Create and manage settlement accounts for handling double-entry transactions | On-demand | Medium |

### Category: Account Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Account Creation | Create new bank accounts with specified attributes, routings, and initial balances | On-demand | Medium |
| 2 | Account Information Retrieval | Retrieve detailed account information including balances, metadata, and attributes | Real-time | High |
| 3 | Account Balance Inquiry | Query current and available balances for bank accounts | Real-time | Very High |
| 4 | Account Listing | List all accounts accessible to a user at a specific bank or across all banks | Real-time | High |
| 5 | Account Routing Management | Manage account routing information including IBAN, BIC, and other routing schemes | On-demand | Medium |
| 6 | Account Attribute Management | Create, update, and retrieve custom attributes for accounts | On-demand | Medium |
| 7 | Account Label Update | Update the display label for bank accounts | On-demand | Low |
| 8 | IBAN Validation | Validate and check IBAN numbers for correctness and errors | Real-time | High |
| 9 | Account Access Management | Grant and revoke user access to accounts through views | On-demand | Medium |
| 10 | Firehose Account Access | Bulk retrieval of account information for authorized applications | Real-time | High |

### Category: Transaction Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Transaction Retrieval | Retrieve transaction details and history for accounts | Real-time | Very High |
| 2 | Transaction Listing | List transactions with filtering, pagination, and date range support | Real-time | Very High |
| 3 | Historical Transaction Creation | Create historical transaction records for data migration or reconciliation | On-demand | Medium |
| 4 | Transaction Attribute Management | Add, update, and retrieve custom attributes for transactions | On-demand | Medium |
| 5 | Double-Entry Transaction View | View double-entry book transactions showing debit and credit sides | Real-time | Medium |
| 6 | Balancing Transaction Retrieval | Retrieve the balancing transaction for a given transaction | Real-time | Medium |
| 7 | Transaction Tagging | Add, retrieve, and delete tags for transactions | On-demand | Medium |

### Category: Payment Initiation

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | SEPA Credit Transfer | Initiate SEPA credit transfers using IBAN-based routing | Real-time | High |
| 2 | Account-to-Account Transfer | Transfer funds between accounts within the platform | Real-time | High |
| 3 | Counterparty Payment | Initiate payments to registered counterparties | Real-time | High |
| 4 | Simple Payment | Transfer money directly to bank account numbers or IBANs | Real-time | High |
| 5 | Refund Processing | Process refunds for previous transactions | On-demand | Medium |
| 6 | Free-Form Payment | Initiate payments with flexible parameters | On-demand | Low |
| 7 | Agent Cash Withdrawal | Process cash withdrawal requests through agents | On-demand | Medium |
| 8 | Card Payment | Process card-based payment transactions | Real-time | High |
| 9 | Transaction Request Challenge | Answer security challenges for transaction authorization | Real-time | High |
| 10 | Transaction Request Status | Check the status of pending transaction requests | Real-time | High |
| 11 | Transaction Request Attribute Management | Manage custom attributes for transaction requests | On-demand | Medium |

### Category: Customer Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Customer Creation | Create new customer records with personal and contact information | On-demand | Medium |
| 2 | Customer Information Retrieval | Retrieve customer details and overview information | Real-time | High |
| 3 | Customer Search | Search customers by various criteria including phone number, attributes, and legal name | Real-time | Medium |
| 4 | Customer Attribute Management | Create, update, and retrieve custom attributes for customers | On-demand | Medium |
| 5 | Customer Account Linking | Link customers to bank accounts with specified relationships | On-demand | Medium |
| 6 | Customer Message Management | Create and retrieve messages for customers | On-demand | Low |
| 7 | Customer Overview | Get comprehensive customer overview including accounts and relationships | Real-time | Medium |

### Category: User Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | User Creation | Create new users with specified roles and permissions | On-demand | Medium |
| 2 | User Information Retrieval | Retrieve user details by ID, username, or email | Real-time | High |
| 3 | User Listing | List all users with filtering and pagination | Real-time | Medium |
| 4 | User Invitation | Create and manage user invitations for onboarding | On-demand | Low |
| 5 | User Deletion | Delete user accounts and associated data | On-demand | Low |
| 6 | User Locking | Lock and unlock user accounts for security purposes | On-demand | Low |
| 7 | Password Reset | Generate password reset URLs for users | On-demand | Medium |
| 8 | User Authentication Context | Manage user authentication context for multi-factor authentication | Real-time | High |
| 9 | User Attribute Management | Create and manage personal and non-personal user attributes | On-demand | Medium |
| 10 | User-Customer Linking | Link users to customer records | On-demand | Medium |

### Category: Authorization & Access Control

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Entitlement Management | Grant and manage role-based entitlements for users | On-demand | Medium |
| 2 | Scope Management | Create and manage OAuth scopes for API access | On-demand | Low |
| 3 | View Management | Create, update, and delete views for account access control | On-demand | Medium |
| 4 | System View Management | Manage system-level views with predefined permissions | On-demand | Low |
| 5 | Custom View Management | Create and manage custom views for specific access patterns | On-demand | Medium |
| 6 | View Permission Management | Grant and revoke view permissions for users | On-demand | Medium |
| 7 | Account Access Grant | Grant user access to specific accounts through views | On-demand | Medium |
| 8 | Account Access Revocation | Revoke user access to accounts | On-demand | Medium |

### Category: Consent Management (PSD2 Compliance)

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Consent Creation | Create consent records for account access authorization | Real-time | High |
| 2 | Consent Retrieval | Retrieve consent details and status | Real-time | High |
| 3 | Consent Status Update | Update consent status (active, revoked, expired) | On-demand | Medium |
| 4 | Consent Revocation | Revoke existing consents | On-demand | Medium |
| 5 | Consent Request Management | Create and manage consent requests for user authorization | Real-time | High |
| 6 | VRP Consent Management | Manage Variable Recurring Payment consents | On-demand | Medium |
| 7 | Consent Authorization | Process consent authorization flows with SCA | Real-time | High |

### Category: Berlin Group PSD2 Services

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Account Information Service (AIS) | Provide PSD2-compliant account information access | Real-time | Very High |
| 2 | Payment Initiation Service (PIS) | Initiate PSD2-compliant payments | Real-time | High |
| 3 | Confirmation of Funds (PIIS) | Check if sufficient funds are available for payments | Real-time | High |
| 4 | Signing Baskets | Authorize multiple transactions with single SCA process | On-demand | Medium |
| 5 | SCA Status Management | Track Strong Customer Authentication status | Real-time | High |
| 6 | Payment Cancellation | Cancel pending payments with authorization | On-demand | Medium |
| 7 | Periodic Payments | Initiate recurring periodic payments | On-demand | Medium |
| 8 | Bulk Payments | Process bulk payment batches | On-demand | Medium |

### Category: Counterparty Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Counterparty Creation | Create counterparty records for payment recipients | On-demand | Medium |
| 2 | Counterparty Retrieval | Retrieve counterparty details by ID or name | Real-time | High |
| 3 | Counterparty Listing | List all counterparties for an account | Real-time | Medium |
| 4 | Counterparty Deletion | Delete counterparty records | On-demand | Low |
| 5 | Counterparty Limit Management | Set and manage payment limits for counterparties | On-demand | Low |

### Category: Product & Card Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Product Creation | Create banking product definitions | On-demand | Low |
| 2 | Product Retrieval | Retrieve product details and attributes | Real-time | Medium |
| 3 | Product Attribute Management | Manage custom attributes for products | On-demand | Low |
| 4 | Product Fee Management | Create and manage product fees | On-demand | Low |
| 5 | Physical Card Creation | Create physical card records for accounts | On-demand | Medium |
| 6 | Card Information Retrieval | Retrieve card details and attributes | Real-time | Medium |
| 7 | Card Account Management | Manage card accounts and balances | Real-time | Medium |
| 8 | Card Transaction Listing | List transactions for card accounts | Real-time | High |

### Category: Branch & ATM Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Branch Information Retrieval | Retrieve branch details and locations | Real-time | Medium |
| 2 | Branch Listing | List all branches for a bank | Real-time | Medium |
| 3 | ATM Creation | Create ATM records with location and service information | On-demand | Low |
| 4 | ATM Information Retrieval | Retrieve ATM details and attributes | Real-time | Medium |
| 5 | ATM Listing | List all ATMs for a bank | Real-time | Medium |
| 6 | ATM Attribute Management | Manage ATM attributes including services, languages, and currencies | On-demand | Low |
| 7 | ATM Update | Update ATM information and accessibility features | On-demand | Low |

### Category: Standing Orders & Direct Debits

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Standing Order Creation | Create recurring standing order instructions | On-demand | Medium |
| 2 | Standing Order Management | Manage and update standing orders | On-demand | Medium |
| 3 | Direct Debit Creation | Create direct debit mandates | On-demand | Medium |
| 4 | Direct Debit Management | Manage direct debit instructions | On-demand | Medium |

### Category: Dynamic Configuration

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Dynamic Entity Management | Create and manage runtime-defined data models | On-demand | Low |
| 2 | Dynamic Endpoint Management | Create and manage runtime-defined API endpoints | On-demand | Low |
| 3 | Dynamic Resource Documentation | Create and manage dynamic API documentation | On-demand | Low |
| 4 | Dynamic Message Documentation | Create and manage dynamic message documentation | On-demand | Low |
| 5 | Endpoint Mapping | Map and route API endpoints dynamically | On-demand | Low |
| 6 | Method Routing | Configure connector method routing per bank | On-demand | Low |
| 7 | Connector Method Management | Create and manage custom connector methods | On-demand | Low |

### Category: API Management & Consumer Services

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Consumer Registration | Register API consumers (third-party applications) | On-demand | Low |
| 2 | Consumer Management | Update consumer details and certificates | On-demand | Low |
| 3 | Rate Limiting | Set and manage API call limits per consumer | On-demand | Low |
| 4 | API Collection Management | Create and manage collections of API endpoints | On-demand | Low |
| 5 | API Information Retrieval | Retrieve API version and configuration information | Real-time | Medium |
| 6 | API Tags Management | Manage tags for API endpoint categorization | On-demand | Low |

### Category: Regulatory & Compliance

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | KYC Document Management | Manage Know Your Customer documents | On-demand | Medium |
| 2 | KYC Status Management | Track and update KYC verification status | On-demand | Medium |
| 3 | KYC Check Management | Manage KYC check records | On-demand | Medium |
| 4 | Tax Residence Management | Manage customer tax residence information | On-demand | Low |
| 5 | Regulated Entity Management | Manage regulated entity registrations | On-demand | Low |
| 6 | Authentication Type Validation | Validate and manage authentication types | On-demand | Low |
| 7 | JSON Schema Validation | Validate API requests against JSON schemas | Real-time | High |

### Category: Metrics & Monitoring

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | API Metrics Collection | Collect and store API usage metrics | Real-time | Very High |
| 2 | Metrics Retrieval | Retrieve API metrics for analysis | On-demand | Medium |
| 3 | Aggregate Metrics | Calculate aggregate metrics across endpoints | On-demand | Medium |
| 4 | Bank-Level Metrics | Retrieve metrics specific to a bank | On-demand | Medium |
| 5 | Adapter Information | Retrieve backend adapter status and information | Real-time | Low |

### Category: Webhook & Notification Services

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Bank Account Webhook Management | Create and manage webhooks for account events | On-demand | Low |
| 2 | System Webhook Management | Create and manage system-level webhooks | On-demand | Low |
| 3 | Notification Delivery | Deliver notifications for account and transaction events | Event-driven | High |

### Category: Administrative Functions

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Database Information | Retrieve database configuration and status | On-demand | Low |
| 2 | Cache Management | Manage API response caching | On-demand | Low |
| 3 | Web UI Properties | Manage web interface configuration properties | On-demand | Low |
| 4 | Endpoint Tag Management | Manage tags for API endpoint organization | On-demand | Low |
| 5 | User Lock Status | Check and manage user lock status | On-demand | Low |
| 6 | Cascade Deletion | Delete entities with cascading to related records | On-demand | Low |

### Category: Backend Integration

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Multi-Connector Support | Route requests to appropriate backend connectors | Real-time | Very High |
| 2 | REST Backend Integration | Connect to banking backends via REST APIs | Real-time | High |
| 3 | Akka Actor Integration | Connect to backends using Akka actor system | Real-time | High |
| 4 | Stored Procedure Integration | Execute banking operations via stored procedures | Real-time | High |
| 5 | Message Queue Integration | Process banking operations via message queues | Event-driven | High |
| 6 | Blockchain Integration | Support for Cardano and Ethereum blockchain backends | Real-time | Low |

---

### Capability Summary
- **Total Capabilities Identified**: 150+
- **API Endpoints**: 400+ (across all API versions v1.2.1 through v6.0.0)
- **Background Tasks**: Scheduled maintenance, cache refresh, metrics aggregation
- **Event Consumers**: Webhook delivery, notification processing
- **Batch Jobs**: Data migration, report generation
- **External Integrations**: Multiple backend connectors (REST, Akka, Stored Procedures, Message Queues, Blockchain)
- **Primary Business Functions**: Account Management, Payment Processing, Customer Management, Regulatory Compliance (PSD2), API Management, Authorization & Access Control

---

## Open Questions & Clarifications Needed

1. **Scheduled Task Frequency**: What are the exact schedules for background maintenance tasks such as cache refresh and metrics aggregation?

2. **Volume Estimates**: The volume estimates (High/Medium/Low) are based on typical open banking usage patterns. Actual volumes may vary significantly based on the number of connected banks and third-party applications.

3. **Regional Compliance**: While PSD2/Berlin Group compliance is clearly implemented, are there other regional compliance requirements (UK Open Banking, Australian CDR) that need to be documented?

4. **Blockchain Integration**: The Cardano and Ethereum connectors appear to be newer additions. What is the intended use case and current adoption status?

5. **Dynamic Endpoint Security**: What security controls are in place for dynamically created endpoints to prevent unauthorized API exposure?

These questions should be resolved during SME review to ensure complete understanding of the system's operational context.
