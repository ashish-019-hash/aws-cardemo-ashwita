# Open Bank Project (OBP) API - High-Level Requirements Document

## 1. System Overview & Purpose

### System Identification
- **System Name**: Open Bank Project (OBP) API
- **Application Code/ID**: obp-api (Version 1.10.1)
- **Business Domain**: Financial Services - Banking API Platform

### Business Purpose
The Open Bank Project API is a comprehensive banking API platform that enables financial institutions to expose their banking services through standardized RESTful APIs while abstracting core banking system complexities. The system serves as an intermediary layer between third-party applications and multiple banking backends, providing secure access to account information, payment initiation, customer management, and transaction processing capabilities. It supports multiple regulatory standards including PSD2 (Berlin Group), UK Open Banking, Australian Open Banking, and other international open banking frameworks, enabling banks to achieve regulatory compliance while maintaining control over their backend systems and data access policies.

### System Criticality
- **Criticality Level**: High
- **Business Impact if Unavailable**: Critical impact on third-party financial applications, payment service providers, and fintech integrations that depend on the API for real-time banking operations. Unavailability would prevent account access, payment processing, customer onboarding, and transaction inquiries across all connected applications and banking partners.

### System Type
- **Architecture**: REST API Platform with Web UI (Full-Stack)
- **Processing Model**: Mixed - HTTP request-response (primary), scheduled background tasks, event-driven processing, and batch operations

### Key Stakeholders
- Third-Party Application Developers and Fintech Companies
- Payment Service Providers (PSPs) requiring PSD2 compliance
- Banking Institutions and Financial Services Organizations
- Regulatory Bodies and Compliance Auditors
- Internal Bank IT Operations and Security Teams
- End-User Customers accessing banking services through third-party applications

## 2. Core Capabilities Inventory

### Category: Bank Management
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Bank Registration and Configuration | Create and manage bank entities with identification, branding, and operational parameters | On-demand | Low |
| 2 | Bank Information Retrieval | Retrieve bank details including name, logo, website, and attributes | Real-time | High |
| 3 | Bank Attribute Management | Define and manage custom bank-level attributes for extended metadata | On-demand | Medium |
| 4 | Multi-Bank Support | Support multiple banking institutions on a single API instance with isolated data | Real-time | High |

### Category: Account Management
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Account Creation | Create new bank accounts with initial balance, currency, and account type specifications | On-demand | Medium |
| 2 | Account Information Retrieval | Retrieve account details including balance, account holder, IBAN, and routing information | Real-time | Very High |
| 3 | Account Listing | List all accounts accessible to a user across single or multiple banks with filtering | Real-time | High |
| 4 | Account Balance Inquiry | Query current account balance and available funds in real-time | Real-time | Very High |
| 5 | Account Label Management | Update account labels and display names for user customization | On-demand | Medium |
| 6 | Account Attribute Management | Define and manage custom account-level attributes for extended metadata | On-demand | Medium |
| 7 | Account Routing Lookup | Find accounts by routing schemes (IBAN, account number, sort code) | Real-time | High |
| 8 | Settlement Account Management | Create and manage settlement accounts for payment system reconciliation | On-demand | Low |
| 9 | Account Application Processing | Handle account opening applications with approval workflows | On-demand | Medium |
| 10 | Firehose Account Access | Provide high-volume bulk account data access for authorized systems | Scheduled/Real-time | Very High |

### Category: Customer Management
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Customer Profile Creation | Create customer profiles with personal information, contact details, and demographics | On-demand | Medium |
| 2 | Customer Information Retrieval | Retrieve customer details including name, date of birth, address, and identification | Real-time | High |
| 3 | Customer Profile Updates | Update customer information including address, contact details, and personal data | On-demand | Medium |
| 4 | Customer Search | Search customers by various criteria including phone number, email, and customer ID | Real-time | Medium |
| 5 | Customer Attribute Management | Define and manage custom customer-level attributes for extended metadata | On-demand | Medium |
| 6 | Customer Address Management | Manage multiple addresses per customer with address types and validation | On-demand | Medium |
| 7 | Customer-Account Linking | Link customers to bank accounts with relationship types and permissions | On-demand | Medium |
| 8 | User-Customer Linking | Associate platform users with customer profiles for access control | On-demand | Medium |
| 9 | KYC Document Management | Upload, store, and retrieve Know Your Customer documents and media | On-demand | Medium |
| 10 | KYC Status Tracking | Track and update customer KYC verification status and compliance checks | On-demand | Medium |

### Category: Transaction Processing
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Transaction History Retrieval | Retrieve transaction history for accounts with filtering by date, amount, and type | Real-time | Very High |
| 2 | Transaction Detail Inquiry | Get detailed information for individual transactions including metadata and parties | Real-time | High |
| 3 | Transaction Attribute Management | Define and manage custom transaction-level attributes for extended metadata | On-demand | Medium |
| 4 | Double-Entry Transaction Tracking | Track double-entry bookkeeping for transactions showing debit and credit sides | Real-time | High |
| 5 | Balancing Transaction Retrieval | Retrieve the balancing transaction for any given transaction in the system | Real-time | Medium |
| 6 | Transaction Tagging | Add, update, and delete tags on transactions for categorization and search | On-demand | Medium |
| 7 | Transaction Metadata Management | Manage transaction comments, images, and additional metadata | On-demand | Low |
| 8 | Transaction Type Management | Define and manage transaction type classifications and categories | On-demand | Low |
| 9 | Transaction Status Tracking | Track transaction status changes and lifecycle events | Real-time | High |
| 10 | Transaction Search | Search transactions across accounts using various criteria and filters | Real-time | Medium |

### Category: Payment Initiation
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | SEPA Payment Initiation | Initiate SEPA credit transfers with beneficiary and amount details | Real-time | High |
| 2 | Domestic Payment Initiation | Create domestic payment instructions within the same country | Real-time | High |
| 3 | International Payment Initiation | Initiate cross-border payments with currency conversion and routing | Real-time | Medium |
| 4 | Counterparty Payment Initiation | Create payments to saved counterparties with stored beneficiary details | Real-time | High |
| 5 | Free-Form Payment Initiation | Process flexible payment requests with custom fields and parameters | Real-time | Medium |
| 6 | Simple Payment Initiation | Execute basic payment transfers between accounts | Real-time | High |
| 7 | Card Payment Initiation | Process card-based payment transactions | Real-time | High |
| 8 | Refund Processing | Initiate refund transactions for previous payments | On-demand | Medium |
| 9 | Transaction Request Attribute Management | Manage custom attributes on payment requests for extended data | On-demand | Low |
| 10 | Transaction Request Challenge | Handle Strong Customer Authentication challenges for payment authorization | Real-time | High |

### Category: Scheduled and Standing Payments
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Scheduled Payment Creation | Create future-dated payments with execution date specifications | On-demand | Medium |
| 2 | Scheduled Payment Retrieval | Retrieve scheduled payment details and status | Real-time | Medium |
| 3 | Domestic Scheduled Payment Management | Manage domestic scheduled payments with consent and authorization | On-demand | Medium |
| 4 | International Scheduled Payment Management | Manage international scheduled payments with currency and routing | On-demand | Low |
| 5 | Standing Order Creation | Create recurring payment instructions with frequency and end date | On-demand | Medium |
| 6 | Standing Order Retrieval | Retrieve standing order details and execution history | Real-time | Medium |
| 7 | Standing Order Management | Update and cancel standing orders with authorization | On-demand | Medium |
| 8 | Direct Debit Creation | Create direct debit mandates for recurring collections | On-demand | Medium |
| 9 | Direct Debit Management | Manage direct debit mandates and execution schedules | On-demand | Medium |

### Category: Card Management
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Card Information Retrieval | Retrieve card details including card number, type, and expiry date | Real-time | High |
| 2 | Card Listing | List all cards associated with accounts and customers | Real-time | Medium |
| 3 | Card Attribute Management | Define and manage custom card-level attributes for extended metadata | On-demand | Low |
| 4 | Card Status Management | Update card status (active, blocked, expired) and manage lifecycle | On-demand | Medium |

### Category: Counterparty Management
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Counterparty Creation | Create beneficiary records with account details and metadata | On-demand | Medium |
| 2 | Counterparty Retrieval | Retrieve counterparty information for payment initiation | Real-time | High |
| 3 | Counterparty Listing | List all counterparties associated with an account | Real-time | Medium |
| 4 | Counterparty Metadata Management | Manage counterparty names, logos, and additional information | On-demand | Low |
| 5 | Counterparty Limit Management | Set and manage transaction limits per counterparty | On-demand | Low |

### Category: Product and Service Management
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Product Catalog Management | Define and manage banking products with features and pricing | On-demand | Low |
| 2 | Product Information Retrieval | Retrieve product details including terms, fees, and eligibility | Real-time | Medium |
| 3 | Product Attribute Management | Define and manage custom product-level attributes | On-demand | Low |
| 4 | Product Collection Management | Group products into collections for marketing and display | On-demand | Low |
| 5 | Product Fee Management | Define and manage fee structures for banking products | On-demand | Low |
| 6 | Branch Information Management | Manage branch locations with address, hours, and services | On-demand | Low |
| 7 | ATM Information Management | Manage ATM locations with address, services, and availability | On-demand | Low |
| 8 | ATM Attribute Management | Define and manage custom ATM-level attributes | On-demand | Low |

### Category: User and Authentication Management
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | User Registration | Create new user accounts with credentials and profile information | On-demand | Medium |
| 2 | User Authentication | Authenticate users via OAuth 1.0a, OAuth 2.0, OpenID Connect, or direct login | Real-time | Very High |
| 3 | User Profile Management | Update user profile information including name, email, and preferences | On-demand | Medium |
| 4 | User Search and Retrieval | Search and retrieve user information by ID, username, or email | Real-time | Medium |
| 5 | User Invitation Management | Create and manage user invitation workflows with email notifications | On-demand | Low |
| 6 | User Deletion | Delete user accounts with data cleanup and audit trail | On-demand | Low |
| 7 | User Lock Management | Lock and unlock user accounts for security purposes | On-demand | Low |
| 8 | Password Reset | Handle password reset requests with secure token generation | On-demand | Medium |
| 9 | Logout Link Generation | Generate logout URLs for session termination | Real-time | High |
| 10 | Login Attempt Tracking | Track and monitor failed login attempts for security | Real-time | High |

### Category: Authorization and Access Control
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Entitlement Management | Grant and revoke role-based permissions to users | On-demand | Medium |
| 2 | Entitlement Retrieval | Retrieve user entitlements and permission lists | Real-time | High |
| 3 | Entitlement Request Processing | Handle entitlement requests with approval workflows | On-demand | Low |
| 4 | View-Based Access Control | Define and manage views that control field-level data visibility | On-demand | Medium |
| 5 | Account Access Granting | Grant users access to specific accounts through defined views | On-demand | Medium |
| 6 | Account Access Revocation | Revoke user access to accounts and views | On-demand | Medium |
| 7 | Scope Management | Define and manage OAuth scopes for API access control | On-demand | Low |
| 8 | Consumer Management | Manage OAuth consumer applications with keys and certificates | On-demand | Medium |
| 9 | Rate Limit Configuration | Set and manage API rate limits per consumer and endpoint | On-demand | Low |

### Category: Consent Management (PSD2 Compliance)
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Consent Creation | Create consent records for account access and payment initiation | Real-time | High |
| 2 | Consent Retrieval | Retrieve consent details including permissions and expiry | Real-time | High |
| 3 | Consent Revocation | Revoke consents and terminate authorized access | On-demand | Medium |
| 4 | Consent Status Management | Track consent lifecycle and status changes | Real-time | High |
| 5 | Consent JWT Generation | Generate JSON Web Tokens encoding consent permissions | Real-time | High |
| 6 | Consent Validation | Validate consent tokens and verify permissions | Real-time | Very High |
| 7 | Berlin Group Consent Management | Handle PSD2 Berlin Group specific consent flows | Real-time | High |
| 8 | UK Open Banking Consent Management | Handle UK Open Banking specific consent flows | Real-time | Medium |

### Category: Strong Customer Authentication (SCA)
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Challenge Creation | Create authentication challenges for sensitive operations | Real-time | High |
| 2 | Challenge Validation | Validate challenge responses (OTP, biometric, etc.) | Real-time | High |
| 3 | SMS OTP Generation | Generate and send one-time passwords via SMS | Real-time | High |
| 4 | Email OTP Generation | Generate and send one-time passwords via email | Real-time | High |
| 5 | Challenge Status Tracking | Track challenge lifecycle and validation status | Real-time | High |
| 6 | SCA Method Management | Configure and manage available SCA methods | On-demand | Low |

### Category: Regulatory API Standards
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Berlin Group PSD2 AIS | Account Information Service endpoints per Berlin Group specification | Real-time | High |
| 2 | Berlin Group PSD2 PIS | Payment Initiation Service endpoints per Berlin Group specification | Real-time | High |
| 3 | Berlin Group PSD2 PIIS | Confirmation of Funds Service endpoints per Berlin Group specification | Real-time | Medium |
| 4 | Berlin Group Signing Baskets | Bulk authorization mechanism for multiple transactions | Real-time | Low |
| 5 | UK Open Banking Account API | UK Open Banking account information endpoints | Real-time | Medium |
| 6 | UK Open Banking Payment API | UK Open Banking payment initiation endpoints | Real-time | Medium |
| 7 | Australian Open Banking API | Australian Consumer Data Right compliant endpoints | Real-time | Low |
| 8 | Bahrain OBF API | Bahrain Open Banking Framework compliant endpoints | Real-time | Low |
| 9 | Mexican Open Finance API | Mexican Open Finance standard compliant endpoints | Real-time | Low |
| 10 | Polish API Standard | Polish banking API standard compliant endpoints | Real-time | Low |
| 11 | STET API Standard | French STET standard compliant endpoints | Real-time | Low |

### Category: Dynamic System Configuration
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Dynamic Entity Creation | Define custom data entities at runtime without code deployment | On-demand | Low |
| 2 | Dynamic Entity CRUD Operations | Create, read, update, delete operations on dynamic entities | Real-time | Medium |
| 3 | Dynamic Endpoint Creation | Define custom API endpoints at runtime with business logic | On-demand | Low |
| 4 | Dynamic Endpoint Execution | Execute dynamically defined endpoints with custom code | Real-time | Low |
| 5 | Dynamic Resource Documentation | Generate API documentation for dynamic endpoints | On-demand | Low |
| 6 | Method Routing Configuration | Configure connector routing rules per bank and method | On-demand | Low |
| 7 | Connector Method Management | Define and manage connector method implementations | On-demand | Low |
| 8 | Endpoint Mapping Configuration | Map external endpoints to internal implementations | On-demand | Low |

### Category: Backend Integration
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Local Database Connector | Direct access to OBP database for sandbox and simple deployments | Real-time | High |
| 2 | REST Backend Connector | HTTP-based communication with external banking systems | Real-time | High |
| 3 | Akka Actor Connector | Actor-based distributed system integration | Real-time | Medium |
| 4 | Stored Procedure Connector | Database stored procedure invocation for legacy systems | Real-time | Medium |
| 5 | Internal Connector | Internal system integration for cross-module communication | Real-time | High |
| 6 | Dynamic Connector Routing | Runtime selection of connector based on bank and method | Real-time | High |
| 7 | Connector Health Monitoring | Monitor connector availability and performance | Real-time | Medium |

### Category: Webhook and Notification Management
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Account Notification Webhooks | Send webhook notifications for account events | Event-driven | High |
| 2 | System Notification Webhooks | Send webhook notifications for system events | Event-driven | Medium |
| 3 | Webhook Configuration | Configure webhook endpoints and event subscriptions | On-demand | Low |
| 4 | Email Notification Service | Send email notifications for various events and workflows | Event-driven | Medium |
| 5 | Event Notification API | Bahrain OBF event notification endpoints | Event-driven | Low |

### Category: Metadata and Tagging
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Transaction Tag Management | Add, update, delete tags on transactions | On-demand | Medium |
| 2 | Transaction Comment Management | Manage comments and notes on transactions | On-demand | Low |
| 3 | Transaction Image Management | Attach and manage images related to transactions | On-demand | Low |
| 4 | Counterparty Metadata | Manage metadata for counterparties including logos and descriptions | On-demand | Low |
| 5 | Account Metadata | Manage additional metadata for accounts | On-demand | Low |

### Category: Search and Query
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Transaction Search | Search transactions by various criteria across accounts | Real-time | Medium |
| 2 | Customer Search | Search customers by phone, email, name, and other attributes | Real-time | Medium |
| 3 | User Search | Search users by username, email, and user ID | Real-time | Medium |
| 4 | Account Search | Search accounts by routing information and attributes | Real-time | Medium |
| 5 | Elasticsearch Integration | Full-text search capabilities using Elasticsearch | Real-time | Medium |

### Category: Reporting and Analytics
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | API Metrics Collection | Collect and store API usage metrics and performance data | Real-time | Very High |
| 2 | Metrics Retrieval | Retrieve API usage statistics and analytics | On-demand | Medium |
| 3 | Connector Metrics | Track connector performance and availability metrics | Real-time | High |
| 4 | Rate Limiting Metrics | Monitor rate limit usage and violations | Real-time | High |

### Category: Foreign Exchange
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | FX Rate Management | Manage foreign exchange rates for currency conversion | On-demand | Low |
| 2 | FX Rate Retrieval | Retrieve current and historical exchange rates | Real-time | Medium |
| 3 | Currency Conversion | Convert amounts between currencies using current rates | Real-time | High |

### Category: Meeting and CRM
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Meeting Scheduling | Schedule meetings between customers and bank representatives | On-demand | Low |
| 2 | Meeting Management | Manage meeting details, participants, and status | On-demand | Low |
| 3 | CRM Event Management | Track customer relationship management events and interactions | On-demand | Low |

### Category: Tax and Compliance
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Tax Residence Management | Manage customer tax residence information for compliance | On-demand | Low |
| 2 | Regulated Entity Management | Track and manage regulated entities and compliance status | On-demand | Low |
| 3 | Yearly Customer Charges | Calculate and track annual customer charges and fees | Scheduled | Low |

### Category: API Documentation and Discovery
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Resource Documentation Generation | Auto-generate API documentation from endpoint definitions | On-demand | Low |
| 2 | Swagger/OpenAPI Export | Export API specifications in Swagger/OpenAPI format | On-demand | Low |
| 3 | API Explorer Interface | Provide interactive API testing and exploration interface | Real-time | Medium |
| 4 | Glossary Management | Manage API terminology and field definitions | On-demand | Low |
| 5 | API Collection Management | Group related endpoints into collections for organization | On-demand | Low |
| 6 | API Versioning | Support multiple concurrent API versions with backward compatibility | Real-time | High |

### Category: System Administration
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Database Information Retrieval | Get database connection and configuration information | On-demand | Low |
| 2 | System Configuration Management | Manage system-wide configuration parameters | On-demand | Low |
| 3 | Web UI Properties Management | Configure web interface properties and customization | On-demand | Low |
| 4 | Migration Management | Execute database migrations and schema updates | On-demand | Low |
| 5 | Cache Management | Manage Redis and Guava cache configurations | On-demand | Low |
| 6 | Sandbox Mode | Provide sandbox environment for testing and development | Real-time | Medium |

### Category: Security and Validation
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Request Signature Verification | Verify cryptographic signatures on API requests | Real-time | High |
| 2 | JSON Schema Validation | Validate request and response payloads against schemas | Real-time | High |
| 3 | IBAN Validation | Validate International Bank Account Numbers for correctness | Real-time | Medium |
| 4 | Input Validation | Validate all user inputs for security and data integrity | Real-time | Very High |
| 5 | Authentication Type Validation | Validate authentication methods and credentials | Real-time | Very High |
| 6 | Certificate Management | Manage client certificates for mutual TLS authentication | On-demand | Low |

### Category: Integration Services
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Payment Gateway Integration | Integrate with Stripe and other payment gateways | Real-time | Medium |
| 2 | SMS Service Integration | Integrate with Twilio for SMS notifications | Real-time | Medium |
| 3 | OpenTok Integration | Video conferencing integration for customer meetings | On-demand | Low |
| 4 | Hydra Integration | OAuth 2.0 server integration for authentication | Real-time | High |
| 5 | RabbitMQ Integration | Message queue integration for asynchronous processing | Event-driven | Medium |
| 6 | Kafka Integration | Event streaming integration for real-time data processing | Event-driven | High |

### Category: Background Processing
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Transaction Status Scheduler | Scheduled job to update transaction request statuses | Scheduled | Medium |
| 2 | Transaction Insertion | Background processing for bulk transaction insertion | Scheduled/Event-driven | High |
| 3 | User Refresh | Scheduled job to refresh user data from external systems | Scheduled | Low |
| 4 | Metrics Aggregation | Background aggregation of API metrics and statistics | Scheduled | Medium |

### Capability Summary
- **Total Capabilities Identified**: 200+
- **API Endpoints**: 400+ across all versions (v1.2.1 through v6.0.0)
- **Background Tasks**: 4+ scheduled jobs
- **Event Consumers**: Multiple webhook and notification handlers
- **Batch Jobs**: Transaction processing, metrics aggregation
- **External Integrations**: 10+ (Payment gateways, SMS, email, video, OAuth, messaging)
- **Primary Business Functions**: 
  - Account and Transaction Management
  - Payment Initiation and Processing
  - Customer and User Management
  - Authorization and Consent Management
  - Regulatory Compliance (PSD2, Open Banking)
  - Dynamic System Configuration
  - Backend Integration and Connectivity

## Open Questions & Clarifications Needed

1. **Scheduled Job Frequencies**: What are the exact schedules for background jobs like transaction status updates, metrics aggregation, and user refresh operations?

2. **Event Processing Volume**: What are the expected message volumes for Kafka and RabbitMQ integrations, and what specific events trigger webhook notifications?

3. **Batch Processing Windows**: Are there specific time windows for batch transaction processing and data synchronization operations?

4. **External System Dependencies**: What are the specific external banking systems that connect via the various connectors, and what are their availability requirements?

5. **Regulatory Scope**: Which regulatory standards (Berlin Group, UK Open Banking, etc.) are actively used in production versus available for future use?

6. **Dynamic Entity Usage**: How extensively are dynamic entities and dynamic endpoints used in production deployments?

7. **Multi-Tenancy Model**: How is data isolation achieved between different banks on the same API instance?

8. **Disaster Recovery**: What are the backup and disaster recovery requirements for the system?

9. **Performance SLAs**: What are the specific response time and throughput requirements for different API endpoint categories?

10. **Data Retention**: What are the data retention policies for transactions, audit logs, and customer information?
