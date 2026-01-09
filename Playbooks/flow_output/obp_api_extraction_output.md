# OBP-API High-Level Requirements Document

## Section 1: System Overview & Purpose

### System Identification

**System Name:** Open Bank Project API (OBP-API)

**Application Code/ID:** OBP-API

**Business Domain:** Financial Services / Open Banking

### Business Purpose

The Open Bank Project API is an open-source RESTful API platform designed to abstract core banking system complexities and enable financial applications to interact with multiple banks on behalf of account holders. The system serves as a middleware layer that standardizes banking operations, allowing third-party developers to build financial applications without needing to understand each bank's proprietary systems. The platform's mission is "Bank as a Platform, Transparency as an Asset," enabling banks to expose their services through standardized APIs while maintaining control over backend systems and data access.

### System Criticality

**Criticality Level:** High

**Business Impact if Unavailable:** If the OBP-API becomes unavailable, all third-party financial applications relying on it would lose access to banking services, payment initiation would fail, account information services would be disrupted, and regulatory compliance (PSD2/Open Banking) would be compromised. This would affect fintech applications, personal finance managers, accounting software, and payment service providers that depend on the platform.

### System Type

**Architecture:** REST API / Full-Stack (with Web UI components)

**Processing Model:** Mixed - HTTP request-response (primary), Event-driven (webhooks, notifications), Background processing (scheduled tasks, data synchronization)

### Technology Stack

The system is built using Scala with the Lift web framework, Akka for actor-based processing, and supports multiple database backends including PostgreSQL, H2, MySQL, and Oracle. It uses HikariCP for connection pooling and supports various authentication mechanisms including OAuth 1.0a, OAuth 2.0, OpenID Connect, and Direct Login.

### Key Stakeholders

The primary stakeholders include third-party developers building financial applications, banks seeking to expose their services via standardized APIs, payment service providers requiring PSD2-compliant APIs for Account Information Services (AIS) and Payment Initiation Services (PIS), regulators and auditors needing transparent access to financial data, and fintech companies building innovative financial products.

### Integration Patterns

The system supports multiple backend integration patterns through its connector architecture, including REST connectors for HTTP-based backends, Akka connectors for actor-based distributed systems, stored procedure connectors for database-level integration, Kafka connectors for message queue integration, and RabbitMQ connectors for event-driven architectures. The platform also supports dynamic connector routing, allowing different banks to use different backend implementations.

---

## Section 2: Core Capabilities Inventory

```json
{
  "coreCapabilities": [
    {
      "category": "Bank Management",
      "capabilities": [
        {
          "id": 1,
          "name": "Get Banks",
          "description": "Retrieve a list of all banks supported on the API instance with their identifiers, names, logos, and websites",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 2,
          "name": "Get Bank Details",
          "description": "Retrieve detailed information about a specific bank including attributes, short and full name, logo URL, and website",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 3,
          "name": "Create Bank",
          "description": "Create a new bank entity in the system with all required attributes and configuration",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 4,
          "name": "Bank Attribute Management",
          "description": "Create, update, retrieve, and delete custom attributes associated with banks for extended metadata",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 5,
          "name": "Settlement Account Management",
          "description": "Create and manage settlement accounts for banks to handle double-entry transactions and payment system reconciliation",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Account Management",
      "capabilities": [
        {
          "id": 6,
          "name": "Get Accounts List",
          "description": "Retrieve all accounts at a bank that the user has access to, with optional balance information",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 7,
          "name": "Get Account Details",
          "description": "Retrieve detailed information about a specific account including balance, metadata, and account attributes",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 8,
          "name": "Create Account",
          "description": "Create a new bank account with specified currency, type, and initial balance",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 9,
          "name": "Update Account Label",
          "description": "Update the display label or name of an existing bank account",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 10,
          "name": "Get Account Balances",
          "description": "Retrieve current balance information for a specific account or all accounts at a bank",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 11,
          "name": "Account Routing Query",
          "description": "Find accounts by their routing information such as IBAN, account number, or other identifiers",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 12,
          "name": "IBAN Validation",
          "description": "Validate and check IBAN numbers for errors and correctness",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 13,
          "name": "Account Attribute Management",
          "description": "Create, update, retrieve, and delete custom attributes associated with accounts",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 14,
          "name": "Firehose Account Access",
          "description": "Bulk access to all accounts at a bank for authorized firehose users with specific view permissions",
          "frequency": "Real-time",
          "volume": "High"
        }
      ]
    },
    {
      "category": "Transaction Management",
      "capabilities": [
        {
          "id": 15,
          "name": "Get Transactions List",
          "description": "Retrieve a list of transactions for a specific account with filtering and pagination options",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 16,
          "name": "Get Transaction Details",
          "description": "Retrieve detailed information about a specific transaction including metadata and attributes",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 17,
          "name": "Create Historical Transaction",
          "description": "Create historical transaction records for data migration or reconciliation purposes",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 18,
          "name": "Double Entry Transaction",
          "description": "Retrieve double-entry book transaction information showing debit and credit sides",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 19,
          "name": "Balancing Transaction",
          "description": "Retrieve the balancing transaction for a given transaction in double-entry bookkeeping",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 20,
          "name": "Transaction Attribute Management",
          "description": "Create, update, retrieve, and delete custom attributes associated with transactions",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 21,
          "name": "Transaction Tags",
          "description": "Add, retrieve, and delete tags for transactions to enable categorization and search",
          "frequency": "On-demand",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Payment Initiation",
      "capabilities": [
        {
          "id": 22,
          "name": "Create Transaction Request (SEPA)",
          "description": "Initiate a SEPA credit transfer payment request between accounts",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 23,
          "name": "Create Transaction Request (Account)",
          "description": "Initiate a payment request to another account within the same bank or system",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 24,
          "name": "Create Transaction Request (Counterparty)",
          "description": "Initiate a payment request to a predefined counterparty",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 25,
          "name": "Create Transaction Request (Card)",
          "description": "Initiate a card-based payment transaction request",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 26,
          "name": "Create Transaction Request (Refund)",
          "description": "Initiate a refund transaction request for a previous payment",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 27,
          "name": "Create Transaction Request (Free Form)",
          "description": "Initiate a free-form payment request with custom parameters",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 28,
          "name": "Create Transaction Request (Agent Cash Withdrawal)",
          "description": "Initiate a cash withdrawal request through an agent",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 29,
          "name": "Answer Transaction Request Challenge",
          "description": "Respond to a Strong Customer Authentication challenge for a transaction request",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 30,
          "name": "Get Transaction Request Status",
          "description": "Retrieve the current status of a transaction request",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 31,
          "name": "Transaction Request Attribute Management",
          "description": "Create, update, retrieve, and delete custom attributes for transaction requests",
          "frequency": "On-demand",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Customer Management",
      "capabilities": [
        {
          "id": 32,
          "name": "Create Customer",
          "description": "Create a new customer record with personal information, contact details, and KYC data",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 33,
          "name": "Get Customers",
          "description": "Retrieve a list of customers at a bank with filtering and pagination options",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 34,
          "name": "Get Customer by ID",
          "description": "Retrieve detailed information about a specific customer",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 35,
          "name": "Search Customers by Phone",
          "description": "Search for customers by their mobile phone number",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 36,
          "name": "Customer Attribute Management",
          "description": "Create, update, retrieve, and delete custom attributes associated with customers",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 37,
          "name": "Customer Messages",
          "description": "Send and retrieve messages to and from customers",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 38,
          "name": "Customer Account Links",
          "description": "Manage links between customers and their bank accounts",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 39,
          "name": "Get Correlated Users",
          "description": "Retrieve users correlated with a specific customer",
          "frequency": "Real-time",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "User Management",
      "capabilities": [
        {
          "id": 40,
          "name": "Get Current User",
          "description": "Retrieve information about the currently authenticated user",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 41,
          "name": "Get User by ID",
          "description": "Retrieve user information by user identifier",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 42,
          "name": "Get User by Username",
          "description": "Retrieve user information by username",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 43,
          "name": "Get Users by Email",
          "description": "Search for users by their email address",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 44,
          "name": "Get Users List",
          "description": "Retrieve a list of all users with filtering and pagination",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 45,
          "name": "Create User with Roles",
          "description": "Create a new user with specified entitlements and roles",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 46,
          "name": "Delete User",
          "description": "Delete a user from the system",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 47,
          "name": "Lock/Unlock User",
          "description": "Lock or unlock a user account to prevent or allow access",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 48,
          "name": "User Invitation",
          "description": "Create and manage user invitations for onboarding new users",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 49,
          "name": "Reset Password URL",
          "description": "Generate a password reset URL for a user",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 50,
          "name": "User Attribute Management",
          "description": "Create, update, retrieve, and delete custom attributes for users",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 51,
          "name": "User Customer Links",
          "description": "Manage links between users and customers",
          "frequency": "On-demand",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Authorization & Access Control",
      "capabilities": [
        {
          "id": 52,
          "name": "Get Entitlements",
          "description": "Retrieve entitlements (roles) assigned to a user",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 53,
          "name": "Create Entitlement",
          "description": "Assign a new entitlement (role) to a user",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 54,
          "name": "Delete Entitlement",
          "description": "Remove an entitlement from a user",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 55,
          "name": "Grant Account Access",
          "description": "Grant a user access to a specific account through a view",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 56,
          "name": "Revoke Account Access",
          "description": "Revoke a user's access to a specific account",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 57,
          "name": "Create User with Account Access",
          "description": "Create a new user and grant them access to specific accounts",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 58,
          "name": "Consumer Scope Management",
          "description": "Manage OAuth scopes for API consumers",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "View Management",
      "capabilities": [
        {
          "id": 59,
          "name": "Get Views",
          "description": "Retrieve available views for an account that define data access permissions",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 60,
          "name": "Create View",
          "description": "Create a custom view with specific data access permissions for an account",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 61,
          "name": "Update View",
          "description": "Update the permissions and settings of an existing view",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 62,
          "name": "Delete View",
          "description": "Delete a custom view from an account",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 63,
          "name": "System View Permissions",
          "description": "Manage permissions for system-level views",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Consent Management (PSD2)",
      "capabilities": [
        {
          "id": 64,
          "name": "Create Consent",
          "description": "Create a consent resource defining access rights to accounts for PSD2 compliance",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 65,
          "name": "Get Consent Status",
          "description": "Retrieve the current status of a consent",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 66,
          "name": "Get Consent Information",
          "description": "Retrieve detailed information about a consent including access rights and validity",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 67,
          "name": "Delete Consent",
          "description": "Revoke and delete a consent",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 68,
          "name": "Consent Authorisation",
          "description": "Start and manage the authorisation process for a consent",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 69,
          "name": "Update Consent PSU Data",
          "description": "Update Payment Service User data during consent authorisation",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 70,
          "name": "Get My Consents",
          "description": "Retrieve all consents for the current user",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 71,
          "name": "Consent User Update Request",
          "description": "Request to update the user associated with a consent",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Counterparty Management",
      "capabilities": [
        {
          "id": 72,
          "name": "Get Counterparties",
          "description": "Retrieve a list of counterparties for an account",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 73,
          "name": "Get Counterparty Details",
          "description": "Retrieve detailed information about a specific counterparty",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 74,
          "name": "Create Counterparty",
          "description": "Create a new counterparty for an account",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 75,
          "name": "Update Counterparty",
          "description": "Update information for an existing counterparty",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 76,
          "name": "Delete Counterparty",
          "description": "Delete a counterparty from an account",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 77,
          "name": "Counterparty Limits",
          "description": "Manage transaction limits for counterparties",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Product Management",
      "capabilities": [
        {
          "id": 78,
          "name": "Get Products",
          "description": "Retrieve a list of banking products offered by a bank",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 79,
          "name": "Get Product Details",
          "description": "Retrieve detailed information about a specific banking product",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 80,
          "name": "Create/Update Product",
          "description": "Create or update a banking product with its attributes and fees",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 81,
          "name": "Product Attribute Management",
          "description": "Create, update, retrieve, and delete custom attributes for products",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 82,
          "name": "Product Fee Management",
          "description": "Create, update, retrieve, and delete fees associated with products",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 83,
          "name": "Product Collection Management",
          "description": "Manage collections of related products",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Branch & ATM Management",
      "capabilities": [
        {
          "id": 84,
          "name": "Get Branches",
          "description": "Retrieve a list of bank branches with location and service information",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 85,
          "name": "Get Branch Details",
          "description": "Retrieve detailed information about a specific branch",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 86,
          "name": "Get ATMs",
          "description": "Retrieve a list of ATMs with location and service information",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 87,
          "name": "Get ATM Details",
          "description": "Retrieve detailed information about a specific ATM",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 88,
          "name": "Create/Update ATM",
          "description": "Create or update ATM information including location and services",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 89,
          "name": "ATM Attribute Management",
          "description": "Manage supported currencies, languages, accessibility features, and services for ATMs",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Direct Debit & Standing Orders",
      "capabilities": [
        {
          "id": 90,
          "name": "Create Direct Debit",
          "description": "Create a direct debit mandate for recurring payments",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 91,
          "name": "Get Direct Debits",
          "description": "Retrieve direct debit mandates for an account",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 92,
          "name": "Create Standing Order",
          "description": "Create a standing order for scheduled recurring payments",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 93,
          "name": "Get Standing Orders",
          "description": "Retrieve standing orders for an account",
          "frequency": "Real-time",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Card Management",
      "capabilities": [
        {
          "id": 94,
          "name": "Get Card Accounts",
          "description": "Retrieve a list of card accounts for a user",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 95,
          "name": "Get Card Account Details",
          "description": "Retrieve detailed information about a specific card account",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 96,
          "name": "Get Card Account Balances",
          "description": "Retrieve balance information for a card account",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 97,
          "name": "Get Card Account Transactions",
          "description": "Retrieve transaction history for a card account",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 98,
          "name": "Card Attribute Management",
          "description": "Manage custom attributes for cards",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "KYC & Compliance",
      "capabilities": [
        {
          "id": 99,
          "name": "KYC Check Management",
          "description": "Create and retrieve KYC check records for customers",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 100,
          "name": "KYC Document Management",
          "description": "Upload and manage KYC documents for customers",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 101,
          "name": "KYC Status Management",
          "description": "Track and update KYC status for customers",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 102,
          "name": "Tax Residence Management",
          "description": "Manage tax residence information for customers",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Consumer & API Management",
      "capabilities": [
        {
          "id": 103,
          "name": "Get Consumers",
          "description": "Retrieve a list of API consumers (third-party applications)",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 104,
          "name": "Create Consumer",
          "description": "Register a new API consumer application",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 105,
          "name": "Update Consumer",
          "description": "Update consumer application details including redirect URLs and certificates",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 106,
          "name": "Consumer Rate Limits",
          "description": "Set and manage API rate limits for consumers",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 107,
          "name": "Dynamic Registration",
          "description": "Support for dynamic client registration for OAuth consumers",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Dynamic Configuration",
      "capabilities": [
        {
          "id": 108,
          "name": "Dynamic Entity Management",
          "description": "Create, update, and delete runtime-defined data entities without code deployment",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 109,
          "name": "Dynamic Endpoint Management",
          "description": "Create, update, and delete runtime-defined API endpoints",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 110,
          "name": "Connector Method Management",
          "description": "Create and manage custom connector methods for backend integration",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 111,
          "name": "Endpoint Mapping",
          "description": "Configure mappings between API endpoints and backend operations",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 112,
          "name": "Dynamic Resource Documentation",
          "description": "Create and manage documentation for dynamic resources",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 113,
          "name": "Dynamic Message Documentation",
          "description": "Create and manage message documentation for connector communication",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Validation & Schema Management",
      "capabilities": [
        {
          "id": 114,
          "name": "JSON Schema Validation",
          "description": "Configure and manage JSON schema validations for API operations",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 115,
          "name": "Authentication Type Validation",
          "description": "Configure authentication requirements for specific API operations",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 116,
          "name": "Attribute Definition Management",
          "description": "Define and manage attribute schemas for various entity types",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "API Collections",
      "capabilities": [
        {
          "id": 117,
          "name": "Create API Collection",
          "description": "Create a collection of API endpoints for organization and sharing",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 118,
          "name": "Get API Collections",
          "description": "Retrieve API collections for the current user or featured collections",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 119,
          "name": "API Collection Endpoint Management",
          "description": "Add, remove, and manage endpoints within API collections",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 120,
          "name": "Share API Collection",
          "description": "Share API collections with other users",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Webhook & Notification",
      "capabilities": [
        {
          "id": 121,
          "name": "Account Notification Webhook",
          "description": "Configure webhooks to receive notifications on account events like new transactions",
          "frequency": "Event-driven",
          "volume": "High"
        },
        {
          "id": 122,
          "name": "System Notification Webhook",
          "description": "Configure system-level webhooks for various events",
          "frequency": "Event-driven",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Endpoint Tag Management",
      "capabilities": [
        {
          "id": 123,
          "name": "Create Endpoint Tag",
          "description": "Add tags to API endpoints for categorization and filtering",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 124,
          "name": "Get Endpoint Tags",
          "description": "Retrieve tags associated with API endpoints",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 125,
          "name": "Delete Endpoint Tag",
          "description": "Remove tags from API endpoints",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Foreign Exchange",
      "capabilities": [
        {
          "id": 126,
          "name": "Get FX Rates",
          "description": "Retrieve foreign exchange rates between currencies",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 127,
          "name": "Create/Update FX Rate",
          "description": "Create or update foreign exchange rates",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Metrics & Monitoring",
      "capabilities": [
        {
          "id": 128,
          "name": "Get API Metrics",
          "description": "Retrieve API usage metrics and statistics",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 129,
          "name": "Get Aggregate Metrics",
          "description": "Retrieve aggregated API metrics for analysis",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 130,
          "name": "Database Info",
          "description": "Retrieve basic information about the database",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "System Administration",
      "capabilities": [
        {
          "id": 131,
          "name": "Get API Versions",
          "description": "Retrieve available API versions supported by the instance",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 132,
          "name": "Root Endpoint",
          "description": "Get basic API information and available endpoints",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 133,
          "name": "WebUI Props Management",
          "description": "Manage web UI configuration properties",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 134,
          "name": "System Integrity Checks",
          "description": "Run integrity checks on system data including views and account access",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 135,
          "name": "Cascading Delete Operations",
          "description": "Perform cascading delete operations on banks, accounts, customers, and products",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 136,
          "name": "Log Cache Management",
          "description": "Manage log cache levels for debugging",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Berlin Group PSD2 - Account Information Service (AIS)",
      "capabilities": [
        {
          "id": 137,
          "name": "BG Create Consent",
          "description": "Create a Berlin Group consent for account information access",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 138,
          "name": "BG Get Account List",
          "description": "Retrieve account list according to Berlin Group specification",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 139,
          "name": "BG Get Account Details",
          "description": "Retrieve account details according to Berlin Group specification",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 140,
          "name": "BG Get Balances",
          "description": "Retrieve account balances according to Berlin Group specification",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 141,
          "name": "BG Get Transaction List",
          "description": "Retrieve transaction list according to Berlin Group specification",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 142,
          "name": "BG Get Transaction Details",
          "description": "Retrieve transaction details according to Berlin Group specification",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 143,
          "name": "BG Consent Authorisation",
          "description": "Manage consent authorisation flow according to Berlin Group specification",
          "frequency": "Real-time",
          "volume": "High"
        }
      ]
    },
    {
      "category": "Berlin Group PSD2 - Payment Initiation Service (PIS)",
      "capabilities": [
        {
          "id": 144,
          "name": "BG Create Payment",
          "description": "Initiate a payment according to Berlin Group specification",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 145,
          "name": "BG Get Payment Status",
          "description": "Retrieve payment status according to Berlin Group specification",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 146,
          "name": "BG Get Payment Details",
          "description": "Retrieve payment details according to Berlin Group specification",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 147,
          "name": "BG Cancel Payment",
          "description": "Cancel a payment according to Berlin Group specification",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 148,
          "name": "BG Payment Authorisation",
          "description": "Manage payment authorisation flow according to Berlin Group specification",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 149,
          "name": "BG Periodic Payment",
          "description": "Create periodic SEPA credit transfers according to Berlin Group specification",
          "frequency": "On-demand",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Berlin Group PSD2 - Confirmation of Funds (PIIS)",
      "capabilities": [
        {
          "id": 150,
          "name": "BG Funds Confirmation",
          "description": "Check if sufficient funds are available for a payment according to Berlin Group specification",
          "frequency": "Real-time",
          "volume": "High"
        }
      ]
    },
    {
      "category": "Berlin Group PSD2 - Signing Baskets",
      "capabilities": [
        {
          "id": 151,
          "name": "BG Create Signing Basket",
          "description": "Create a signing basket to authorize multiple transactions with single SCA",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 152,
          "name": "BG Get Signing Basket",
          "description": "Retrieve signing basket details",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 153,
          "name": "BG Signing Basket Authorisation",
          "description": "Manage signing basket authorisation flow",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 154,
          "name": "BG Delete Signing Basket",
          "description": "Delete a signing basket",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Regulated Entities",
      "capabilities": [
        {
          "id": 155,
          "name": "Get Regulated Entities",
          "description": "Retrieve a list of regulated entities (TPPs, banks)",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 156,
          "name": "Create Regulated Entity",
          "description": "Create a new regulated entity record",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 157,
          "name": "Update Regulated Entity",
          "description": "Update regulated entity information",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 158,
          "name": "Regulated Entity Attribute Management",
          "description": "Manage custom attributes for regulated entities",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Agent Management",
      "capabilities": [
        {
          "id": 159,
          "name": "Get Agents",
          "description": "Retrieve a list of agents for a bank",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 160,
          "name": "Create Agent",
          "description": "Create a new agent for cash withdrawal and other services",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 161,
          "name": "Update Agent",
          "description": "Update agent information and status",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    }
  ]
}
```

### Capability Summary

**Total Capabilities Identified:** 161

**API Endpoints:** 350+ (across all API versions v1.2.1 through v6.0.0)

**Background Tasks:** Webhook notifications, scheduled data synchronization

**Event Consumers:** Account notification webhooks, system notification webhooks

**Batch Jobs:** Data migration, cascading delete operations

**External Integrations:** Multiple connector types (REST, Akka, Stored Procedure, Kafka, RabbitMQ, Cardano, Ethereum)

**Primary Business Functions:**
- Bank Management
- Account Management
- Transaction Management
- Payment Initiation
- Customer Management
- User Management
- Authorization & Access Control
- View Management
- Consent Management (PSD2)
- Counterparty Management
- Product Management
- Branch & ATM Management
- Direct Debit & Standing Orders
- Card Management
- KYC & Compliance
- Consumer & API Management
- Dynamic Configuration
- Berlin Group PSD2 Compliance (AIS, PIS, PIIS, Signing Baskets)
- Metrics & Monitoring
- System Administration

---

## Open Questions & Clarifications Needed

1. **Connector Configuration:** What is the preferred connector type for the target Go application - REST, direct database, or message queue based?

2. **API Version Selection:** Which API versions should be prioritized for migration - the latest v6.0.0, the stable v4.0.0, or all versions?

3. **Berlin Group Compliance:** Is full Berlin Group PSD2 compliance required in the Go application, or only specific services (AIS, PIS, PIIS)?

4. **Dynamic Features:** Should the dynamic entity and dynamic endpoint features be migrated, or will the Go application use static configurations?

5. **Authentication Methods:** Which authentication methods should be supported in the Go application - OAuth 1.0a, OAuth 2.0, OpenID Connect, Direct Login, or all?

6. **Webhook Implementation:** What is the expected webhook delivery mechanism in the Go application?

7. **Database Support:** Which database backends should the Go application support - PostgreSQL only, or multiple databases like the Scala version?

8. **Connector Abstraction:** Should the Go application maintain the same connector abstraction layer for backend integration flexibility?

These questions should be resolved during SME review to ensure accurate migration planning.
