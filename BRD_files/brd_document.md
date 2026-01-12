# High-Level Requirements Document: Open Bank Project API

## 1. System Overview & Purpose

### System Identification
- **System Name**: Open Bank Project API (OBP-API)
- **Application Code/ID**: OBP-API
- **Business Domain**: Financial Services / Open Banking Platform

### Business Purpose
The Open Bank Project API is an open-source RESTful API platform that abstracts core banking system complexities to enable financial applications to interact with multiple banks on behalf of account holders. The system serves as a standardized interface layer between third-party developers, banks, and payment service providers, enabling secure access to banking data and payment initiation services while maintaining regulatory compliance with PSD2 and Open Banking standards.

### System Criticality
- **Criticality Level**: High
- **Business Impact if Unavailable**: Third-party financial applications would lose access to banking services, payment processing would be disrupted, and regulatory compliance for PSD2/Open Banking would be compromised. Banks would be unable to expose their services through standardized APIs.

### System Type
- **Architecture**: REST API / Full-Stack Web Application
- **Processing Model**: Mixed (HTTP request-response, Scheduled tasks, Event-driven processing)

### Key Stakeholders
- Third-Party Developers (fintech applications, personal finance managers, accounting software providers)
- Banks and Financial Institutions
- Payment Service Providers (PSPs)
- Regulators and Auditors
- Account Holders / End Users

---

## 2. Core Capabilities Inventory

### Category: Bank and Institution Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Bank Information Retrieval | Retrieve information about banks supported on the platform including identifiers, names, logos, and websites | Real-time | High |
| 2 | Bank Creation | Create new bank entities on the platform with associated metadata and configuration | On-demand | Low |
| 3 | Bank Attribute Management | Manage custom attributes associated with bank entities for extended metadata | On-demand | Low |
| 4 | Settlement Account Management | Create and manage settlement accounts for double-entry bookkeeping and payment processing | On-demand | Medium |
| 5 | Branch Management | Retrieve and manage bank branch information including locations and services | Real-time | Medium |
| 6 | ATM Management | Create, update, and retrieve ATM information including locations, supported currencies, languages, and accessibility features | Real-time | Medium |

### Category: Account Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Account Listing | Retrieve list of accounts accessible to users at specific banks | Real-time | High |
| 2 | Account Details Retrieval | Get detailed information about specific accounts including balances and metadata | Real-time | High |
| 3 | Account Creation | Create new bank accounts with specified parameters and ownership | On-demand | Medium |
| 4 | Account Balance Retrieval | Retrieve current and available balances for bank accounts | Real-time | Very High |
| 5 | Account Label Management | Update account labels and display names | On-demand | Low |
| 6 | Account Attribute Management | Create, update, and retrieve custom attributes for accounts | On-demand | Medium |
| 7 | Account Routing Lookup | Find accounts by routing information such as IBAN or account number | Real-time | High |
| 8 | IBAN Validation | Validate and check IBAN numbers for correctness and errors | Real-time | High |
| 9 | Firehose Account Access | Bulk retrieval of account data for authorized high-volume consumers | Real-time | Very High |

### Category: Transaction Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Transaction Listing | Retrieve transaction history for accounts with filtering and pagination | Real-time | Very High |
| 2 | Transaction Details | Get detailed information about specific transactions | Real-time | High |
| 3 | Transaction Attribute Management | Create and manage custom attributes for transactions | On-demand | Medium |
| 4 | Double-Entry Transaction Retrieval | Access double-entry bookkeeping records for transactions | Real-time | Medium |
| 5 | Balancing Transaction Retrieval | Get balancing transactions for double-entry accounting | Real-time | Medium |
| 6 | Historical Transaction Creation | Create historical transactions for data migration and reconciliation | On-demand | Medium |
| 7 | Transaction Tagging | Add, retrieve, and delete tags for transaction categorization | On-demand | Medium |

### Category: Payment Initiation

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | SEPA Credit Transfer Initiation | Initiate SEPA credit transfer payments between accounts | Real-time | High |
| 2 | Counterparty Payment Initiation | Initiate payments to registered counterparties | Real-time | High |
| 3 | Account-to-Account Transfer | Transfer funds between accounts at the same or different banks | Real-time | High |
| 4 | Refund Processing | Process refund transactions for previous payments | On-demand | Medium |
| 5 | Transaction Request Status | Track and retrieve status of payment initiation requests | Real-time | High |
| 6 | Transaction Request Challenge | Handle Strong Customer Authentication challenges for payments | Real-time | High |
| 7 | Direct Debit Management | Create and manage direct debit instructions | On-demand | Medium |
| 8 | Standing Order Management | Create and manage recurring payment standing orders | On-demand | Medium |
| 9 | Payment Cancellation | Cancel pending payment requests when permitted | On-demand | Low |
| 10 | Periodic Payment Initiation | Initiate recurring periodic payments with defined schedules | On-demand | Medium |

### Category: Customer Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Customer Information Retrieval | Retrieve customer profile information and demographics | Real-time | High |
| 2 | Customer Creation | Create new customer records with personal and contact information | On-demand | Medium |
| 3 | Customer Attribute Management | Create, update, and retrieve custom attributes for customers | On-demand | Medium |
| 4 | Customer Search | Search for customers by various criteria including phone number and legal name | Real-time | Medium |
| 5 | Customer-Account Linking | Manage relationships between customers and their accounts | On-demand | Medium |
| 6 | Agent Management | Create and manage agent records for customer service representatives | On-demand | Low |

### Category: User and Access Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | User Authentication | Authenticate users via multiple methods including OAuth, Direct Login, and OpenID Connect | Real-time | Very High |
| 2 | User Information Retrieval | Retrieve user profile information and identifiers | Real-time | High |
| 3 | User Creation | Create new user accounts with specified roles and permissions | On-demand | Medium |
| 4 | User Invitation | Send invitations for new users to join the platform | On-demand | Low |
| 5 | User Lock/Unlock | Lock and unlock user accounts for security management | On-demand | Low |
| 6 | Entitlement Management | Grant, revoke, and retrieve user entitlements and roles | On-demand | Medium |
| 7 | View Access Management | Grant and revoke user access to specific account views | On-demand | Medium |
| 8 | Password Reset | Generate and send password reset links for users | On-demand | Medium |
| 9 | User Deletion | Remove user accounts from the system | On-demand | Low |

### Category: View and Permission Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | View Creation | Create custom views with specific permission sets for account access | On-demand | Low |
| 2 | View Retrieval | Retrieve view definitions and associated permissions | Real-time | High |
| 3 | View Update | Modify view permissions and configurations | On-demand | Low |
| 4 | System View Management | Manage predefined system views (owner, accountant, auditor) | On-demand | Low |
| 5 | Account Access Grant | Grant users access to accounts through specific views | On-demand | Medium |
| 6 | Account Access Revocation | Remove user access to accounts | On-demand | Medium |

### Category: Consent Management (PSD2 Compliance)

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Consent Creation | Create consent records for third-party access to account information | Real-time | High |
| 2 | Consent Status Retrieval | Check the current status of consent records | Real-time | High |
| 3 | Consent Revocation | Revoke previously granted consents | On-demand | Medium |
| 4 | Consent Information Retrieval | Get detailed information about consent scope and validity | Real-time | High |
| 5 | Consent Authorisation | Manage authorisation sub-resources for consent flows | Real-time | High |
| 6 | SCA Status Tracking | Track Strong Customer Authentication status for consents | Real-time | High |

### Category: Berlin Group PSD2 Services

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Account Information Service (AIS) | Provide standardized access to account information per Berlin Group specification | Real-time | Very High |
| 2 | Payment Initiation Service (PIS) | Enable payment initiation per Berlin Group PSD2 specification | Real-time | High |
| 3 | Confirmation of Funds (PIIS) | Check if sufficient funds are available for card payments | Real-time | High |
| 4 | Signing Baskets | Authorize multiple transactions with single SCA process | Real-time | Medium |
| 5 | Card Account Information | Retrieve card account details and transactions | Real-time | High |

### Category: Product and Catalog Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Product Listing | Retrieve available banking products and their details | Real-time | Medium |
| 2 | Product Creation/Update | Create and update banking product definitions | On-demand | Low |
| 3 | Product Fee Management | Define and manage fees associated with banking products | On-demand | Low |
| 4 | Product Collection Management | Organize products into collections for categorization | On-demand | Low |
| 5 | Product Attribute Management | Manage custom attributes for products | On-demand | Low |

### Category: Counterparty Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Counterparty Creation | Register new counterparties for payment purposes | On-demand | Medium |
| 2 | Counterparty Retrieval | Get counterparty information and details | Real-time | High |
| 3 | Counterparty Limit Management | Set and manage transaction limits for counterparties | On-demand | Low |
| 4 | Counterparty Metadata | Manage additional metadata for counterparties | On-demand | Low |

### Category: Foreign Exchange

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | FX Rate Retrieval | Get current foreign exchange rates between currencies | Real-time | High |
| 2 | FX Rate Management | Create and update foreign exchange rate definitions | On-demand | Low |

### Category: Consumer and Application Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Consumer Registration | Register third-party applications as API consumers | On-demand | Low |
| 2 | Consumer Information Retrieval | Get consumer application details and credentials | Real-time | Medium |
| 3 | Consumer Update | Update consumer application settings and URLs | On-demand | Low |
| 4 | Rate Limiting Configuration | Set API call limits per consumer | On-demand | Low |
| 5 | Consumer Certificate Management | Manage client certificates for mutual TLS authentication | On-demand | Low |

### Category: Dynamic Configuration

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Dynamic Entity Management | Create and manage runtime-defined data models | On-demand | Low |
| 2 | Dynamic Endpoint Management | Create and manage runtime-defined API endpoints | On-demand | Low |
| 3 | Method Routing Configuration | Configure connector routing for different banks and methods | On-demand | Low |
| 4 | Endpoint Mapping | Map external endpoints to internal implementations | On-demand | Low |
| 5 | Web UI Properties | Configure web interface properties dynamically | On-demand | Low |

### Category: Regulatory and Compliance

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Regulated Entity Management | Register and manage regulated financial entities | On-demand | Low |
| 2 | KYC Document Management | Manage Know Your Customer documentation | On-demand | Medium |
| 3 | KYC Status Tracking | Track KYC verification status for customers | Real-time | Medium |
| 4 | Tax Residence Management | Manage customer tax residence information | On-demand | Low |

### Category: Metrics and Monitoring

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | API Metrics Collection | Collect and store API usage metrics | Real-time | Very High |
| 2 | Aggregate Metrics Retrieval | Get aggregated API usage statistics | On-demand | Medium |
| 3 | Metrics Archival | Archive historical metrics data | Scheduled (Daily) | High |
| 4 | Health Check | Monitor system health and connectivity status | Real-time | High |
| 5 | Database Information | Retrieve database connection and status information | On-demand | Low |

### Category: Scheduled Background Processing

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Consent Expiration Processing | Process and expire outdated consent records | Scheduled | Medium |
| 2 | Database Cleanup | Clean up expired and obsolete database records | Scheduled | Medium |
| 3 | Metrics Archival | Archive metrics data to historical storage | Scheduled (Daily) | High |
| 4 | Transaction Processing | Process pending and scheduled transactions | Scheduled | High |

### Category: Backend Integration

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | REST Connector | Connect to banking backends via REST APIs | Real-time | Very High |
| 2 | Akka Connector | Connect to distributed banking systems via Akka actors | Real-time | High |
| 3 | Stored Procedure Connector | Execute banking operations via database stored procedures | Real-time | High |
| 4 | RabbitMQ Connector | Connect to message queue-based banking systems | Event-driven | High |
| 5 | Cardano Blockchain Connector | Interface with Cardano blockchain for crypto operations | Real-time | Low |
| 6 | Ethereum Blockchain Connector | Interface with Ethereum blockchain for crypto operations | Real-time | Low |

### Category: API Documentation and Discovery

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Resource Documentation | Generate and serve API documentation | Real-time | High |
| 2 | Swagger/OpenAPI Generation | Generate OpenAPI specifications for API endpoints | On-demand | Medium |
| 3 | API Collection Management | Organize APIs into collections for discovery | On-demand | Low |
| 4 | Glossary Management | Maintain glossary of API terms and definitions | On-demand | Low |

### Category: Webhook and Notification

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Webhook Management | Configure and manage webhooks for event notifications | On-demand | Low |
| 2 | Account Notification Webhooks | Send notifications for account-related events | Event-driven | Medium |
| 3 | System Notification Webhooks | Send notifications for system-level events | Event-driven | Low |

### Capability Summary
- **Total Capabilities Identified**: 95+
- **API Endpoints**: 400+ (across all API versions v1.2.1 through v5.1.0)
- **Background Tasks**: 4 (Consent expiration, Database cleanup, Metrics archival, Transaction processing)
- **Event Consumers**: 2 (RabbitMQ, Webhook handlers)
- **Batch Jobs**: 3 (Metrics archival, Database cleanup, Consent processing)
- **External Integrations**: 6+ (REST, Akka, Stored Procedures, RabbitMQ, Cardano, Ethereum)
- **Primary Business Functions**: Bank Management, Account Management, Transaction Processing, Payment Initiation, Customer Management, User Access Control, Consent Management, PSD2 Compliance, Backend Integration

---

## Open Questions & Clarifications Needed

1. **Transaction Volume Estimates**: What are the expected peak transaction volumes for payment initiation and account information services?

2. **SLA Requirements**: What are the specific service level agreements for API response times and availability?

3. **Data Retention Policies**: What are the data retention requirements for transaction history, metrics, and consent records?

4. **Multi-tenancy Requirements**: Are there specific requirements for isolating data between different banks or tenants on the platform?

5. **Disaster Recovery**: What are the business continuity and disaster recovery requirements for the platform?

6. **Regional Compliance**: Beyond PSD2, are there other regional regulatory requirements (UK Open Banking, Australia CDR, etc.) that need to be supported?

7. **Blockchain Integration Scope**: What is the intended scope and use case for the Cardano and Ethereum blockchain connectors?

8. **Legacy System Migration**: Are there specific legacy banking systems that need to be supported through custom connectors?

These questions should be resolved during SME review to ensure complete understanding of business requirements.
