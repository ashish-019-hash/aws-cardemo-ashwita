# High-Level Requirements Document: Open Bank Project API

## 1. System Overview & Purpose

### System Identification
- **System Name**: Open Bank Project API (OBP-API)
- **Application Code/ID**: OBP-API
- **Business Domain**: Financial Services / Open Banking Platform

### Business Purpose
The Open Bank Project API is an open-source RESTful API platform that abstracts core banking system complexities to enable financial applications to interact with multiple banks on behalf of account holders. The system serves as a "Bank as a Platform" solution, providing standardized APIs for third-party developers, banks, and payment service providers to access banking services without requiring knowledge of each bank's proprietary systems. It enables regulatory compliance with PSD2/Open Banking requirements while maintaining control over backend systems and data access.

### System Criticality
- **Criticality Level**: High
- **Business Impact if Unavailable**: Complete disruption of third-party financial application integrations, inability to process payment initiations, account information services would be unavailable, and regulatory compliance (PSD2) would be compromised. Banks relying on OBP for Open Banking compliance would face regulatory penalties.

### System Type
- **Architecture**: REST API / Full-Stack Web Application
- **Processing Model**: Mixed (HTTP request-response, Scheduled tasks, Event-driven via webhooks)

### Key Stakeholders
- Third-Party Developers (Fintech applications, Personal Finance Managers, Accounting Software)
- Banks and Financial Institutions
- Payment Service Providers (PSPs)
- Regulators and Auditors
- Account Holders / End Users

---

## 2. Core Capabilities Inventory

```json
{
  "coreCapabilities": [
    {
      "category": "Bank Management",
      "capabilities": [
        {
          "id": 1,
          "name": "Get Banks",
          "description": "Retrieve list of all banks supported on the API instance with basic information including ID, name, logo, and website",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 2,
          "name": "Get Bank Details",
          "description": "Retrieve detailed information about a specific bank including attributes and configuration",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 3,
          "name": "Create Bank",
          "description": "Create a new bank entity in the system with required attributes and configuration",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 4,
          "name": "Update Bank",
          "description": "Update existing bank information and attributes",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 5,
          "name": "Get Bank Attributes",
          "description": "Retrieve custom attributes associated with a specific bank",
          "frequency": "Real-time",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Account Management",
      "capabilities": [
        {
          "id": 6,
          "name": "Get Accounts at Bank",
          "description": "Retrieve list of accounts at a specific bank that the user has access to",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 7,
          "name": "Get Account by ID",
          "description": "Retrieve detailed account information including balance and metadata for a specific account",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 8,
          "name": "Get Account by Routing",
          "description": "Retrieve account information using account routing details such as IBAN",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 9,
          "name": "Create Account",
          "description": "Create a new bank account with specified attributes and initial balance",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 10,
          "name": "Update Account Label",
          "description": "Update the display label for a bank account",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 11,
          "name": "Get Account Balances",
          "description": "Retrieve current balance information for a specific account",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 12,
          "name": "Get Firehose Accounts",
          "description": "Retrieve all accounts at a bank for administrative purposes",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 13,
          "name": "IBAN Validation",
          "description": "Validate and check IBAN numbers for correctness and errors",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 14,
          "name": "Create Settlement Account",
          "description": "Create settlement accounts for payment system processing",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 15,
          "name": "Get Settlement Accounts",
          "description": "Retrieve list of settlement accounts for a bank",
          "frequency": "Real-time",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Transaction Management",
      "capabilities": [
        {
          "id": 16,
          "name": "Get Transactions",
          "description": "Retrieve transaction history for a specific account with filtering options",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 17,
          "name": "Get Transaction Details",
          "description": "Retrieve detailed information about a specific transaction",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 18,
          "name": "Get Double Entry Transaction",
          "description": "Retrieve double-entry book transaction details showing debit and credit sides",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 19,
          "name": "Get Balancing Transaction",
          "description": "Retrieve the balancing transaction for double-entry accounting",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 20,
          "name": "Add Transaction Tag",
          "description": "Add metadata tags to transactions for categorization",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 21,
          "name": "Get Transaction Tags",
          "description": "Retrieve tags associated with a transaction",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 22,
          "name": "Delete Transaction Tag",
          "description": "Remove a tag from a transaction",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Transaction Request / Payment Initiation",
      "capabilities": [
        {
          "id": 23,
          "name": "Create Transaction Request (Account)",
          "description": "Initiate a payment transfer to another account within the same bank",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 24,
          "name": "Create Transaction Request (SEPA)",
          "description": "Initiate a SEPA credit transfer payment to external accounts",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 25,
          "name": "Create Transaction Request (Counterparty)",
          "description": "Initiate a payment to a registered counterparty",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 26,
          "name": "Create Transaction Request (Refund)",
          "description": "Initiate a refund transaction for a previous payment",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 27,
          "name": "Create Transaction Request (Card)",
          "description": "Initiate a card-based payment transaction",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 28,
          "name": "Create Transaction Request (Simple)",
          "description": "Initiate a simple payment transaction with minimal parameters",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 29,
          "name": "Create Transaction Request (Free Form)",
          "description": "Initiate a payment with custom free-form parameters",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 30,
          "name": "Answer Transaction Request Challenge",
          "description": "Respond to Strong Customer Authentication challenge for payment authorization",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 31,
          "name": "Get Transaction Request",
          "description": "Retrieve details of a specific transaction request",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 32,
          "name": "Get Transaction Requests",
          "description": "Retrieve list of transaction requests for an account",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 33,
          "name": "Create Transaction Request Attribute",
          "description": "Add custom attributes to a transaction request",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 34,
          "name": "Get Transaction Request Attributes",
          "description": "Retrieve custom attributes of a transaction request",
          "frequency": "Real-time",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Customer Management",
      "capabilities": [
        {
          "id": 35,
          "name": "Create Customer",
          "description": "Create a new customer record with personal and contact information",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 36,
          "name": "Get Customer by ID",
          "description": "Retrieve customer information by customer identifier",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 37,
          "name": "Get Customers by Phone Number",
          "description": "Search for customers using phone number",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 38,
          "name": "Get Customers by Legal Name",
          "description": "Search for customers using legal name",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 39,
          "name": "Create Customer Attribute",
          "description": "Add custom attributes to a customer record",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 40,
          "name": "Update Customer Attribute",
          "description": "Update existing customer attributes",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 41,
          "name": "Get Customer Attributes",
          "description": "Retrieve all custom attributes for a customer",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 42,
          "name": "Get Customer Messages",
          "description": "Retrieve messages sent to a customer",
          "frequency": "Real-time",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "User Management",
      "capabilities": [
        {
          "id": 43,
          "name": "Create User",
          "description": "Create a new user account in the system",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 44,
          "name": "Get Current User",
          "description": "Retrieve information about the currently authenticated user",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 45,
          "name": "Get User by ID",
          "description": "Retrieve user information by user identifier",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 46,
          "name": "Get User by Username",
          "description": "Retrieve user information by username",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 47,
          "name": "Get Users by Email",
          "description": "Search for users by email address",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 48,
          "name": "Get Users",
          "description": "Retrieve list of all users with filtering options",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 49,
          "name": "Delete User",
          "description": "Remove a user account from the system",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 50,
          "name": "Lock User",
          "description": "Lock a user account to prevent access",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 51,
          "name": "Unlock User",
          "description": "Unlock a previously locked user account",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 52,
          "name": "Create User Invitation",
          "description": "Send an invitation to a new user to join the platform",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 53,
          "name": "Get User Invitations",
          "description": "Retrieve list of pending user invitations",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 54,
          "name": "Reset Password URL",
          "description": "Generate a password reset URL for a user",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 55,
          "name": "Get Logout Link",
          "description": "Retrieve the logout URL for the current session",
          "frequency": "Real-time",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "View and Access Control",
      "capabilities": [
        {
          "id": 56,
          "name": "Create Custom View",
          "description": "Create a custom view defining access rights to account data",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 57,
          "name": "Update Custom View",
          "description": "Update permissions and settings of a custom view",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 58,
          "name": "Get Custom View",
          "description": "Retrieve details of a specific custom view",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 59,
          "name": "Delete Custom View",
          "description": "Remove a custom view from the system",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 60,
          "name": "Get System View",
          "description": "Retrieve details of a predefined system view",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 61,
          "name": "Delete System View",
          "description": "Remove a system view (administrative function)",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 62,
          "name": "Grant User Access to View",
          "description": "Grant a user permission to access an account through a specific view",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 63,
          "name": "Revoke User Access to View",
          "description": "Remove a user's permission to access an account through a view",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 64,
          "name": "Get Permissions for Account",
          "description": "Retrieve all permissions granted for a specific account",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 65,
          "name": "Create User with Account Access",
          "description": "Create a new user and grant them access to specific accounts",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Entitlement and Role Management",
      "capabilities": [
        {
          "id": 66,
          "name": "Get Entitlements",
          "description": "Retrieve entitlements (roles) assigned to a user",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 67,
          "name": "Get Entitlements for Bank",
          "description": "Retrieve all entitlements for a specific bank",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 68,
          "name": "Get All Entitlements",
          "description": "Retrieve all entitlements in the system",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 69,
          "name": "Create User with Roles",
          "description": "Create a new user with predefined roles and entitlements",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 70,
          "name": "Delete Entitlement",
          "description": "Remove an entitlement from a user",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 71,
          "name": "Get Scopes",
          "description": "Retrieve OAuth scopes available in the system",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 72,
          "name": "Delete Scope",
          "description": "Remove an OAuth scope from the system",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Consent Management",
      "capabilities": [
        {
          "id": 73,
          "name": "Create Consent",
          "description": "Create a consent granting time-limited access to accounts and services",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 74,
          "name": "Get Consents",
          "description": "Retrieve list of consents for a user or bank",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 75,
          "name": "Get Consent by ID",
          "description": "Retrieve details of a specific consent",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 76,
          "name": "Revoke Consent",
          "description": "Revoke an existing consent to remove access permissions",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 77,
          "name": "Update Consent Status",
          "description": "Update the status of a consent (e.g., from received to valid)",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 78,
          "name": "Get My Consents",
          "description": "Retrieve consents created by the current user",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 79,
          "name": "Self Revoke Consent",
          "description": "Allow users to revoke their own consents",
          "frequency": "On-demand",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Counterparty Management",
      "capabilities": [
        {
          "id": 80,
          "name": "Create Counterparty",
          "description": "Create a new counterparty (beneficiary) for payment purposes",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 81,
          "name": "Get Counterparty by ID",
          "description": "Retrieve counterparty details by identifier",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 82,
          "name": "Get Counterparty by Name",
          "description": "Search for counterparty by name",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 83,
          "name": "Delete Counterparty",
          "description": "Remove a counterparty from the system",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 84,
          "name": "Create Counterparty Limit",
          "description": "Set transaction limits for a specific counterparty",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 85,
          "name": "Get Counterparty Limit",
          "description": "Retrieve transaction limits for a counterparty",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 86,
          "name": "Update Counterparty Limit",
          "description": "Update transaction limits for a counterparty",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 87,
          "name": "Delete Counterparty Limit",
          "description": "Remove transaction limits for a counterparty",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Consumer (Application) Management",
      "capabilities": [
        {
          "id": 88,
          "name": "Create Consumer",
          "description": "Register a new third-party application (consumer) for API access",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 89,
          "name": "Get Consumer",
          "description": "Retrieve details of a registered consumer application",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 90,
          "name": "Get Consumers",
          "description": "Retrieve list of all registered consumer applications",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 91,
          "name": "Enable/Disable Consumer",
          "description": "Activate or deactivate a consumer application",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 92,
          "name": "Set Rate Limits",
          "description": "Configure API rate limits for a consumer application",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 93,
          "name": "Get Rate Limits",
          "description": "Retrieve current rate limit configuration for a consumer",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 94,
          "name": "Update Consumer Redirect URL",
          "description": "Update the OAuth redirect URL for a consumer",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 95,
          "name": "Update Consumer Certificate",
          "description": "Update the client certificate for a consumer",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Product Management",
      "capabilities": [
        {
          "id": 96,
          "name": "Create Product",
          "description": "Create a new banking product offering",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 97,
          "name": "Get Product",
          "description": "Retrieve details of a specific banking product",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 98,
          "name": "Get Products",
          "description": "Retrieve list of all banking products",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 99,
          "name": "Get Product Tree",
          "description": "Retrieve hierarchical product structure",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 100,
          "name": "Create Product Collection",
          "description": "Create a collection of related products",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Branch and ATM Management",
      "capabilities": [
        {
          "id": 101,
          "name": "Create Branch",
          "description": "Create a new bank branch location",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 102,
          "name": "Update Branch",
          "description": "Update branch information and details",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 103,
          "name": "Get Branch",
          "description": "Retrieve details of a specific branch",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 104,
          "name": "Get Branches",
          "description": "Retrieve list of all branches for a bank",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 105,
          "name": "Delete Branch",
          "description": "Remove a branch from the system",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 106,
          "name": "Create ATM",
          "description": "Create a new ATM location",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 107,
          "name": "Update ATM",
          "description": "Update ATM information including services and accessibility",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 108,
          "name": "Get ATM",
          "description": "Retrieve details of a specific ATM",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 109,
          "name": "Get ATMs",
          "description": "Retrieve list of all ATMs for a bank",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 110,
          "name": "Delete ATM",
          "description": "Remove an ATM from the system",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Card Management",
      "capabilities": [
        {
          "id": 111,
          "name": "Create Card",
          "description": "Create a new payment card for a bank account",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 112,
          "name": "Update Card",
          "description": "Update card information and status",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 113,
          "name": "Delete Card",
          "description": "Remove a card from the system",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 114,
          "name": "Get Card Accounts",
          "description": "Retrieve card accounts for a user (PSD2 AIS)",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 115,
          "name": "Get Card Account Balances",
          "description": "Retrieve balance information for card accounts",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 116,
          "name": "Get Card Account Transactions",
          "description": "Retrieve transaction history for card accounts",
          "frequency": "Real-time",
          "volume": "High"
        }
      ]
    },
    {
      "category": "Direct Debit and Standing Orders",
      "capabilities": [
        {
          "id": 117,
          "name": "Create Direct Debit",
          "description": "Set up a new direct debit mandate",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 118,
          "name": "Get Direct Debits",
          "description": "Retrieve list of direct debit mandates for an account",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 119,
          "name": "Create Standing Order",
          "description": "Set up a new recurring payment (standing order)",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 120,
          "name": "Get Standing Orders",
          "description": "Retrieve list of standing orders for an account",
          "frequency": "Real-time",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "PSD2 Berlin Group - Account Information Service (AIS)",
      "capabilities": [
        {
          "id": 121,
          "name": "Create AIS Consent",
          "description": "Create consent for account information access per PSD2 requirements",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 122,
          "name": "Delete AIS Consent",
          "description": "Revoke an account information consent",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 123,
          "name": "Get Account List (AIS)",
          "description": "Retrieve list of accounts accessible under a consent",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 124,
          "name": "Get Account Details (AIS)",
          "description": "Retrieve detailed account information under consent",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 125,
          "name": "Get Balances (AIS)",
          "description": "Retrieve account balances under consent",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 126,
          "name": "Get Transaction List (AIS)",
          "description": "Retrieve transaction history under consent",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 127,
          "name": "Get Consent Status",
          "description": "Retrieve current status of an AIS consent",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 128,
          "name": "Get Consent Information",
          "description": "Retrieve detailed information about a consent",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 129,
          "name": "Start Consent Authorisation",
          "description": "Initiate Strong Customer Authentication for consent",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 130,
          "name": "Update Consent PSU Data",
          "description": "Update Payment Service User data during consent authorisation",
          "frequency": "Real-time",
          "volume": "High"
        }
      ]
    },
    {
      "category": "PSD2 Berlin Group - Payment Initiation Service (PIS)",
      "capabilities": [
        {
          "id": 131,
          "name": "Initiate Payment",
          "description": "Initiate a single payment (SEPA credit transfer)",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 132,
          "name": "Initiate Periodic Payment",
          "description": "Initiate a recurring periodic payment",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 133,
          "name": "Initiate Bulk Payment",
          "description": "Initiate multiple payments in a single request",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 134,
          "name": "Get Payment Information",
          "description": "Retrieve details of an initiated payment",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 135,
          "name": "Get Payment Status",
          "description": "Retrieve current status of a payment",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 136,
          "name": "Cancel Payment",
          "description": "Request cancellation of an initiated payment",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 137,
          "name": "Start Payment Authorisation",
          "description": "Initiate Strong Customer Authentication for payment",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 138,
          "name": "Get Payment Authorisation Status",
          "description": "Retrieve SCA status for payment authorisation",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 139,
          "name": "Update Payment PSU Data",
          "description": "Update Payment Service User data during payment authorisation",
          "frequency": "Real-time",
          "volume": "High"
        }
      ]
    },
    {
      "category": "PSD2 Berlin Group - Confirmation of Funds (PIIS)",
      "capabilities": [
        {
          "id": 140,
          "name": "Confirm Funds Availability",
          "description": "Check if sufficient funds are available for a payment",
          "frequency": "Real-time",
          "volume": "High"
        }
      ]
    },
    {
      "category": "PSD2 Berlin Group - Signing Baskets",
      "capabilities": [
        {
          "id": 141,
          "name": "Create Signing Basket",
          "description": "Create a basket to authorize multiple transactions with single SCA",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 142,
          "name": "Get Signing Basket",
          "description": "Retrieve details of a signing basket",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 143,
          "name": "Delete Signing Basket",
          "description": "Cancel a signing basket",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 144,
          "name": "Start Signing Basket Authorisation",
          "description": "Initiate SCA for signing basket",
          "frequency": "Real-time",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Dynamic Entity Management",
      "capabilities": [
        {
          "id": 145,
          "name": "Create Dynamic Entity",
          "description": "Create runtime-defined data models without code deployment",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 146,
          "name": "Get Dynamic Entities",
          "description": "Retrieve list of dynamic entities",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 147,
          "name": "Update Dynamic Entity",
          "description": "Update a dynamic entity definition",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 148,
          "name": "Delete Dynamic Entity",
          "description": "Remove a dynamic entity from the system",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 149,
          "name": "Get My Dynamic Entities",
          "description": "Retrieve dynamic entities created by current user",
          "frequency": "Real-time",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Dynamic Endpoint Management",
      "capabilities": [
        {
          "id": 150,
          "name": "Create Dynamic Endpoint",
          "description": "Create runtime-defined API endpoints without code deployment",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 151,
          "name": "Get Dynamic Endpoints",
          "description": "Retrieve list of dynamic endpoints",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 152,
          "name": "Update Dynamic Endpoint Host",
          "description": "Update the host configuration for a dynamic endpoint",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 153,
          "name": "Delete Dynamic Endpoint",
          "description": "Remove a dynamic endpoint from the system",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 154,
          "name": "Get My Dynamic Endpoints",
          "description": "Retrieve dynamic endpoints created by current user",
          "frequency": "Real-time",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "API Collection Management",
      "capabilities": [
        {
          "id": 155,
          "name": "Create API Collection",
          "description": "Create a collection of API endpoints for organization",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 156,
          "name": "Get API Collections",
          "description": "Retrieve list of API collections",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 157,
          "name": "Get Featured API Collections",
          "description": "Retrieve featured/highlighted API collections",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 158,
          "name": "Add Endpoint to Collection",
          "description": "Add an API endpoint to a collection",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 159,
          "name": "Get Collection Endpoints",
          "description": "Retrieve endpoints in a specific collection",
          "frequency": "Real-time",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Validation and Schema Management",
      "capabilities": [
        {
          "id": 160,
          "name": "Create JSON Schema Validation",
          "description": "Create JSON schema validation rules for API requests",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 161,
          "name": "Get JSON Schema Validations",
          "description": "Retrieve all JSON schema validation rules",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 162,
          "name": "Update JSON Schema Validation",
          "description": "Update existing JSON schema validation rules",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 163,
          "name": "Delete JSON Schema Validation",
          "description": "Remove JSON schema validation rules",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 164,
          "name": "Create Authentication Type Validation",
          "description": "Create validation rules for authentication types",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 165,
          "name": "Get Authentication Type Validations",
          "description": "Retrieve authentication type validation rules",
          "frequency": "Real-time",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Connector and Method Routing",
      "capabilities": [
        {
          "id": 166,
          "name": "Create Connector Method",
          "description": "Create custom connector method implementation",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 167,
          "name": "Get Connector Methods",
          "description": "Retrieve list of connector methods",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 168,
          "name": "Update Connector Method",
          "description": "Update connector method implementation",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 169,
          "name": "Get Method Routings",
          "description": "Retrieve method routing configurations",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 170,
          "name": "Create Endpoint Mapping",
          "description": "Create mapping between endpoints and backend services",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 171,
          "name": "Get Endpoint Mappings",
          "description": "Retrieve endpoint mapping configurations",
          "frequency": "Real-time",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Resource Documentation",
      "capabilities": [
        {
          "id": 172,
          "name": "Create Dynamic Resource Doc",
          "description": "Create documentation for dynamic resources",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 173,
          "name": "Get Dynamic Resource Docs",
          "description": "Retrieve dynamic resource documentation",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 174,
          "name": "Create Dynamic Message Doc",
          "description": "Create documentation for dynamic messages",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 175,
          "name": "Get Dynamic Message Docs",
          "description": "Retrieve dynamic message documentation",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 176,
          "name": "Get Message Docs Swagger",
          "description": "Retrieve Swagger/OpenAPI documentation for messages",
          "frequency": "Real-time",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Endpoint Tag Management",
      "capabilities": [
        {
          "id": 177,
          "name": "Create Endpoint Tag",
          "description": "Create tags for categorizing API endpoints",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 178,
          "name": "Get Endpoint Tags",
          "description": "Retrieve endpoint tags",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 179,
          "name": "Update Endpoint Tag",
          "description": "Update endpoint tag information",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 180,
          "name": "Delete Endpoint Tag",
          "description": "Remove an endpoint tag",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Metrics and Monitoring",
      "capabilities": [
        {
          "id": 181,
          "name": "Get Metrics",
          "description": "Retrieve API usage metrics and statistics",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 182,
          "name": "Get Aggregate Metrics",
          "description": "Retrieve aggregated API metrics",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 183,
          "name": "Get Rate Limiting Info",
          "description": "Retrieve current rate limiting status",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 184,
          "name": "Get Adapter Info",
          "description": "Retrieve information about the backend adapter",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 185,
          "name": "Get Database Info",
          "description": "Retrieve database connection and status information",
          "frequency": "Real-time",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "System Configuration",
      "capabilities": [
        {
          "id": 186,
          "name": "Get Configuration",
          "description": "Retrieve system configuration settings",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 187,
          "name": "Get Web UI Props",
          "description": "Retrieve web UI configuration properties",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 188,
          "name": "Get API Versions",
          "description": "Retrieve list of supported API versions",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 189,
          "name": "Get Server JWK",
          "description": "Retrieve server JSON Web Key for OAuth",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 190,
          "name": "Get OAuth2 Server Well Known",
          "description": "Retrieve OAuth2 server discovery information",
          "frequency": "Real-time",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Webhook Management",
      "capabilities": [
        {
          "id": 191,
          "name": "Create Account Webhook",
          "description": "Create webhook for account-related events",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 192,
          "name": "Get Account Webhooks",
          "description": "Retrieve configured account webhooks",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 193,
          "name": "Bank Account Notification",
          "description": "Send notifications for bank account events",
          "frequency": "Event-driven",
          "volume": "Medium"
        },
        {
          "id": 194,
          "name": "System Account Notification",
          "description": "Send system-level account notifications",
          "frequency": "Event-driven",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Meeting Management",
      "capabilities": [
        {
          "id": 195,
          "name": "Create Meeting",
          "description": "Schedule a meeting between bank and customer",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 196,
          "name": "Get Meetings",
          "description": "Retrieve list of scheduled meetings",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 197,
          "name": "Get Meeting",
          "description": "Retrieve details of a specific meeting",
          "frequency": "Real-time",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "KYC (Know Your Customer)",
      "capabilities": [
        {
          "id": 198,
          "name": "Create KYC Check",
          "description": "Create a KYC verification check for a customer",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 199,
          "name": "Get KYC Checks",
          "description": "Retrieve KYC check results for a customer",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 200,
          "name": "Create KYC Document",
          "description": "Upload KYC documentation for a customer",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 201,
          "name": "Get KYC Documents",
          "description": "Retrieve KYC documents for a customer",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 202,
          "name": "Get KYC Status",
          "description": "Retrieve overall KYC status for a customer",
          "frequency": "Real-time",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Regulated Entity Management",
      "capabilities": [
        {
          "id": 203,
          "name": "Get Regulated Entities",
          "description": "Retrieve list of regulated entities (TPPs)",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 204,
          "name": "Get Regulated Entity by ID",
          "description": "Retrieve details of a specific regulated entity",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 205,
          "name": "Create Regulated Entity",
          "description": "Register a new regulated entity",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 206,
          "name": "Delete Regulated Entity",
          "description": "Remove a regulated entity from the system",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Agent Management",
      "capabilities": [
        {
          "id": 207,
          "name": "Create Agent",
          "description": "Create a new agent for cash withdrawal services",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 208,
          "name": "Get Agent",
          "description": "Retrieve details of a specific agent",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 209,
          "name": "Get Agents",
          "description": "Retrieve list of all agents",
          "frequency": "Real-time",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Scheduled Background Tasks",
      "capabilities": [
        {
          "id": 210,
          "name": "Consent Expiration Processing",
          "description": "Automatically expire consents that have passed their valid until date",
          "frequency": "Scheduled (Configurable interval)",
          "volume": "Medium"
        },
        {
          "id": 211,
          "name": "Unfinished Consent Cleanup",
          "description": "Reject consents that remain in received status beyond timeout period",
          "frequency": "Scheduled (Configurable interval)",
          "volume": "Medium"
        },
        {
          "id": 212,
          "name": "Metrics Archive",
          "description": "Archive API metrics data for historical analysis",
          "frequency": "Scheduled (Configurable interval)",
          "volume": "Medium"
        },
        {
          "id": 213,
          "name": "Database Cleanup",
          "description": "Clean up expired or obsolete database records",
          "frequency": "Scheduled (Configurable interval)",
          "volume": "Low"
        },
        {
          "id": 214,
          "name": "Transaction Processing",
          "description": "Process pending transactions in batch",
          "frequency": "Scheduled (Configurable interval)",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Authentication Services",
      "capabilities": [
        {
          "id": 215,
          "name": "OAuth 1.0a Authentication",
          "description": "Authenticate users and applications using OAuth 1.0a protocol",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 216,
          "name": "Direct Login",
          "description": "Authenticate users directly with username and password",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 217,
          "name": "OpenID Connect",
          "description": "Authenticate users using OpenID Connect protocol",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 218,
          "name": "Gateway Login",
          "description": "Authenticate through API gateway with JWT tokens",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 219,
          "name": "MTLS Client Certificate",
          "description": "Authenticate using mutual TLS client certificates",
          "frequency": "Real-time",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Data Warehouse and Search",
      "capabilities": [
        {
          "id": 220,
          "name": "Data Warehouse Search",
          "description": "Search data warehouse for historical data analysis",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 221,
          "name": "Data Warehouse Statistics",
          "description": "Retrieve statistical data from the data warehouse",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 222,
          "name": "Elasticsearch Metrics Search",
          "description": "Search API metrics using Elasticsearch",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Blockchain Integration",
      "capabilities": [
        {
          "id": 223,
          "name": "Cardano Transaction Request",
          "description": "Initiate transactions on Cardano blockchain",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 224,
          "name": "Ethereum Send Transaction",
          "description": "Send transactions on Ethereum blockchain",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 225,
          "name": "Ethereum Raw Transaction",
          "description": "Send raw transactions on Ethereum blockchain",
          "frequency": "Real-time",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "User Attribute Management",
      "capabilities": [
        {
          "id": 226,
          "name": "Get User Attributes",
          "description": "Retrieve custom attributes for a user",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 227,
          "name": "Create User Attribute",
          "description": "Add custom attributes to a user profile",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 228,
          "name": "Delete User Attribute",
          "description": "Remove custom attributes from a user profile",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 229,
          "name": "Get My Personal User Attributes",
          "description": "Retrieve personal attributes for current user",
          "frequency": "Real-time",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Account Balance Management",
      "capabilities": [
        {
          "id": 230,
          "name": "Create Bank Account Balance",
          "description": "Create balance record for a bank account",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 231,
          "name": "Get Bank Account Balance",
          "description": "Retrieve balance for a specific account",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 232,
          "name": "Update Bank Account Balance",
          "description": "Update balance information for an account",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 233,
          "name": "Delete Bank Account Balance",
          "description": "Remove balance record for an account",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    }
  ]
}
```

### Capability Summary
- **Total Capabilities Identified**: 233
- **API Endpoints**: 200+
- **Background Tasks**: 5
- **Event Consumers/Webhooks**: 4
- **Batch Jobs**: 5
- **External Integrations**: 10+ (Multiple backend connectors, blockchain integrations)
- **Primary Business Functions**: Bank Management, Account Management, Transaction Processing, Payment Initiation, Customer Management, User Management, Access Control, Consent Management, PSD2 Compliance (AIS/PIS/PIIS), Dynamic Configuration, Monitoring and Metrics

---

## Open Questions & Clarifications Needed

1. **Backend Connector Configuration**: The system supports multiple backend connectors (REST, Akka, Stored Procedure, Kafka, RabbitMQ, Cardano, Ethereum). Clarification needed on which connectors are actively used in production deployments.

2. **Regional API Implementations**: The codebase includes implementations for UK Open Banking, Australian Open Banking, Polish API, STET (French), and Bahrain OBF. Clarification needed on which regional implementations are required for the migration target.

3. **Dynamic Entity/Endpoint Usage**: The system supports runtime-defined entities and endpoints. Clarification needed on the extent of dynamic configuration usage in production.

4. **Blockchain Integration Scope**: Cardano and Ethereum connectors are present. Clarification needed on whether blockchain integration is required for the migration.

5. **Scheduled Task Configuration**: Background tasks for consent expiration, metrics archiving, and database cleanup are configurable. Clarification needed on specific scheduling requirements for the target system.

6. **Authentication Methods**: Multiple authentication methods are supported (OAuth 1.0a, Direct Login, OpenID Connect, Gateway Login, MTLS). Clarification needed on which authentication methods are required for the migration.

---

*Document generated from source code analysis of OBP-API codebase (obp-api/src/main)*
*Analysis Date: December 29, 2025*
