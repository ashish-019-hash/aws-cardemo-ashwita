# OBP-API Routing Reference

**Quick Reference Guide - HTTP Routes by Path Pattern**

## Total Routes: 769

## Route Table

| Method | Path | Endpoint Name | API Context | Auth Required |
|--------|------|---------------|-------------|---------------|
| DELETE | /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENTID | cancelPayment | Berlin Group PSD2 (v1_3) | No |
| DELETE | /account-access-consents/CONSENT_ID | deleteAccountAccessConsentsConsentId | UK Open Banking (v3_1_0) | No |
| DELETE | /banks/BANK_ID/CUSTOMER_ID/attributes/CUSTOMER_ATTRIBUTE_ID | deleteCustomerAttribute | v4_0_0 | Yes |
| DELETE | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/metadata/tags/TAG_ID | deleteTagForViewOnAccount | v4_0_0 | No |
| DELETE | /banks/BANK_ID/accounts/ACCOUNT_ID/balances/BALANCE_ID | deleteBankAccountBalance | v5_1_0 | Yes |
| DELETE | /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/counterparties/COUNTERPARTY_ID/limits | deleteCounterpartyLimit | v5_1_0 | No |
| DELETE | /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/target-views/TARGET_VIEW_ID | deleteCustomView | v5_1_0 | No |
| DELETE | /banks/BANK_ID/atms/ATM_ID | deleteAtm | v4_0_0 | Yes |
| DELETE | /banks/BANK_ID/atms/ATM_ID | deleteAtm | v5_1_0 | Yes |
| DELETE | /banks/BANK_ID/atms/ATM_ID/attributes/ATM_ATTRIBUTE_ID | deleteAtmAttribute | v5_1_0 | Yes |
| DELETE | /banks/BANK_ID/attribute-definitions/ATTRIBUTE_DEFINITION_ID/account | deleteAccountAttributeDefinition | v4_0_0 | Yes |
| DELETE | /banks/BANK_ID/attribute-definitions/ATTRIBUTE_DEFINITION_ID/card | deleteCardAttributeDefinition | v4_0_0 | Yes |
| DELETE | /banks/BANK_ID/attribute-definitions/ATTRIBUTE_DEFINITION_ID/customer | deleteCustomerAttributeDefinition | v4_0_0 | Yes |
| DELETE | /banks/BANK_ID/attribute-definitions/ATTRIBUTE_DEFINITION_ID/product | deleteProductAttributeDefinition | v4_0_0 | Yes |
| DELETE | /banks/BANK_ID/attribute-definitions/ATTRIBUTE_DEFINITION_ID/transaction | deleteTransactionAttributeDefinition | v4_0_0 | Yes |
| DELETE | /banks/BANK_ID/attribute-definitions/ATTRIBUTE_DEFINITION_ID/transaction-request | deleteTransactionRequestAttributeDefinition | v4_0_0 | Yes |
| DELETE | /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID | deleteBankAttribute | v4_0_0 | No |
| DELETE | /banks/BANK_ID/branches/BRANCH_ID | deleteBranch | v3_1_0 | Yes |
| DELETE | /banks/BANK_ID/consents/CONSENT_ID | revokeConsentAtBank | v5_1_0 | Yes |
| DELETE | /banks/BANK_ID/customer-account-links/CUSTOMER_ACCOUNT_LINK_ID | deleteCustomerAccountLinkById | v5_0_0 | Yes |
| DELETE | /banks/BANK_ID/customers/CUSTOMER_ID/addresses/CUSTOMER_ADDRESS_ID | deleteCustomerAddress | v3_1_0 | Yes |
| DELETE | /banks/BANK_ID/customers/CUSTOMER_ID/tax_residencies/TAX_RESIDENCE_ID | deleteTaxResidence | v3_1_0 | No |
| DELETE | /banks/BANK_ID/products/PRODUCT_CODE/attributes/PRODUCT_ATTRIBUTE_ID | deleteProductAttribute | v3_1_0 | Yes |
| DELETE | /banks/BANK_ID/products/PRODUCT_CODE/fees/PRODUCT_FEE_ID | deleteProductFee | v4_0_0 | Yes |
| DELETE | /banks/BANK_ID/user_customer_links/USER_CUSTOMER_LINK_ID | deleteUserCustomerLink | v4_0_0 | Yes |
| DELETE | /consents/CONSENTID | deleteConsent | Berlin Group PSD2 (v1_3) | No |
| DELETE | /consumers/CONSUMER_ID/scope/SCOPE_ID | deleteScope | v3_0_0 | No |
| DELETE | /entitlement-requests/ENTITLEMENT_REQUEST_ID | deleteEntitlementRequest | v3_0_0 | Yes |
| DELETE | /funds-confirmation-consents/CONSENTID | deleteFundsConfirmationConsentsConsentId | UK Open Banking (v3_1_0) | No |
| DELETE | /management/authentication-type-validations/OPERATION_ID | deleteAuthenticationTypeValidation | v4_0_0 | Yes |
| DELETE | /management/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/counterparties/COUNTERPARTY_ID | deleteCounterpartyForAnyAccount | v4_0_0 | Yes |
| DELETE | /management/banks/BANK_ID/cards/CARD_ID | deleteCardForBank | v3_1_0 | Yes |
| DELETE | /management/banks/BANK_ID/dynamic-endpoints/DYNAMIC_ENDPOINT_ID | deleteBankLevelDynamicEndpoint | v4_0_0 | Yes |
| DELETE | /management/banks/BANK_ID/dynamic-entities/DYNAMIC_ENTITY_ID | deleteBankLevelDynamicEntity | v4_0_0 | Yes |
| DELETE | /management/banks/BANK_ID/dynamic-message-docs/DYNAMIC_MESSAGE_DOC_ID | deleteBankLevelDynamicMessageDoc | v4_0_0 | Yes |
| DELETE | /management/banks/BANK_ID/dynamic-resource-docs/DYNAMIC-RESOURCE-DOC-ID | deleteBankLevelDynamicResourceDoc | v4_0_0 | Yes |
| DELETE | /management/banks/BANK_ID/endpoint-mappings/ENDPOINT_MAPPING_ID | deleteBankLevelEndpointMapping | v4_0_0 | Yes |
| DELETE | /management/banks/BANK_ID/endpoints/OPERATION_ID/tags/ENDPOINT_TAG_ID | deleteBankLevelEndpointTag | v4_0_0 | Yes |
| DELETE | /management/cascading/banks/BANK_ID | deleteBankCascade | v4_0_0 | Yes |
| DELETE | /management/cascading/banks/BANK_ID/accounts/ACCOUNT_ID | deleteAccountCascade | v4_0_0 | Yes |
| DELETE | /management/cascading/banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID | deleteTransactionCascade | v4_0_0 | Yes |
| DELETE | /management/cascading/banks/BANK_ID/customers/CUSTOMER_ID | deleteCustomerCascade | v4_0_0 | Yes |
| DELETE | /management/cascading/banks/BANK_ID/products/PRODUCT_CODE | deleteProductCascade | v4_0_0 | Yes |
| DELETE | /management/dynamic-endpoints/DYNAMIC_ENDPOINT_ID | deleteDynamicEndpoint | v4_0_0 | Yes |
| DELETE | /management/dynamic-message-docs/DYNAMIC_MESSAGE_DOC_ID | deleteDynamicMessageDoc | v4_0_0 | Yes |
| DELETE | /management/dynamic-resource-docs/DYNAMIC-RESOURCE-DOC-ID | deleteDynamicResourceDoc | v4_0_0 | Yes |
| DELETE | /management/endpoint-mappings/ENDPOINT_MAPPING_ID | deleteEndpointMapping | v4_0_0 | Yes |
| DELETE | /management/endpoints/OPERATION_ID/tags/ENDPOINT_TAG_ID | deleteSystemLevelEndpointTag | v4_0_0 | Yes |
| DELETE | /management/json-schema-validations/OPERATION_ID | deleteJsonSchemaValidation | v4_0_0 | Yes |
| DELETE | /management/method_routings/METHOD_ROUTING_ID | deleteMethodRouting | v3_1_0 | Yes |
| DELETE | /management/system-dynamic-entities/DYNAMIC_ENTITY_ID | deleteSystemDynamicEntity | v4_0_0 | Yes |
| DELETE | /management/webui_props/WEB_UI_PROPS_ID | deleteWebUiProps | v3_1_0 | Yes |
| DELETE | /my/api-collection-ids/API_COLLECTION_ID/api-collection-endpoint-ids/API_COLLECTION_ENDPOINT_ID | deleteMyApiCollectionEndpointById | v4_0_0 | No |
| DELETE | /my/api-collection-ids/API_COLLECTION_ID/api-collection-endpoints/OPERATION_ID | deleteMyApiCollectionEndpointByOperationId | v4_0_0 | No |
| DELETE | /my/api-collections/API_COLLECTION_ID | deleteMyApiCollection | v4_0_0 | No |
| DELETE | /my/api-collections/API_COLLECTION_NAME/api-collection-endpoints/OPERATION_ID | deleteMyApiCollectionEndpoint | v4_0_0 | No |
| DELETE | /my/consent/current | selfRevokeConsent | v5_1_0 | No |
| DELETE | /my/dynamic-endpoints/DYNAMIC_ENDPOINT_ID | deleteMyDynamicEndpoint | v4_0_0 | No |
| DELETE | /my/dynamic-entities/DYNAMIC_ENTITY_ID | deleteMyDynamicEntity | v4_0_0 | No |
| DELETE | /regulated-entities/REGULATED_ENTITY_ID | deleteRegulatedEntity | v5_1_0 | Yes |
| DELETE | /regulated-entities/REGULATED_ENTITY_ID/attributes/REGULATED_ENTITY_ATTRIBUTE_ID | deleteRegulatedEntityAttribute | v5_1_0 | Yes |
| DELETE | /signing-baskets/BASKETID | deleteSigningBasket | Berlin Group PSD2 (v1_3) | No |
| DELETE | /system-views/VIEW_ID/permissions/PERMISSION_NAME | deleteSystemViewPermission | v5_1_0 | Yes |
| DELETE | /users/USER_ID | deleteUser | v4_0_0 | Yes |
| DELETE | /users/USER_ID/auth-context | deleteUserAuthContexts | v3_1_0 | Yes |
| DELETE | /users/USER_ID/auth-context/USER_AUTH_CONTEXT_ID | deleteUserAuthContextById | v3_1_0 | Yes |
| DELETE | /users/USER_ID/non-personal/attributes/USER_ATTRIBUTE_ID | deleteNonPersonalUserAttribute | v5_1_0 | Yes |
| GET | /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENTID | getPaymentInformation | Berlin Group PSD2 (v1_3) | No |
| GET | /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENTID/authorisations | getPaymentInitiationAuthorisation | Berlin Group PSD2 (v1_3) | No |
| GET | /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENTID/cancellation-authorisations | getPaymentInitiationCancellationAuthorisationInformation | Berlin Group PSD2 (v1_3) | No |
| GET | /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENTID/cancellation-authorisations/CANCELLATIONID | getPaymentCancellationScaStatus | Berlin Group PSD2 (v1_3) | No |
| GET | /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENT_ID/authorisations/AUTHORISATION_ID | getPaymentInitiationScaStatus | Berlin Group PSD2 (v1_3) | No |
| GET | /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENT_ID/status | getPaymentInitiationStatus | Berlin Group PSD2 (v1_3) | No |
| GET | /account-access-consents/CONSENT_ID | getAccountAccessConsentsConsentId | UK Open Banking (v3_1_0) | No |
| GET | /account-access-consents/CONSENT_ID | accountAccessConsentsConsentIdGet | Bahrain OBF (v1_0_0) | No |
| GET | /accounts | getAccountList | UK Open Banking (v2_0_0) | No |
| GET | /accounts | getAccounts | UK Open Banking (v3_1_0) | No |
| GET | /accounts | getAccountList | Berlin Group PSD2 (v1_3) | No |
| GET | /accounts | accountsGet | STET (French) (v1_4) | No |
| GET | /accounts | accountsGet | Bahrain OBF (v1_0_0) | No |
| GET | /accounts/ACCOUNTID/beneficiaries | getAccountsAccountIdBeneficiaries | UK Open Banking (v3_1_0) | No |
| GET | /accounts/ACCOUNTID/direct-debits | getAccountsAccountIdDirectDebits | UK Open Banking (v3_1_0) | No |
| GET | /accounts/ACCOUNTID/offers | getAccountsAccountIdOffers | UK Open Banking (v3_1_0) | No |
| GET | /accounts/ACCOUNTID/party | getAccountsAccountIdParty | UK Open Banking (v3_1_0) | No |
| GET | /accounts/ACCOUNTID/product | getAccountsAccountIdProduct | UK Open Banking (v3_1_0) | No |
| GET | /accounts/ACCOUNTID/scheduled-payments | getAccountsAccountIdScheduledPayments | UK Open Banking (v3_1_0) | No |
| GET | /accounts/ACCOUNTID/standing-orders | getAccountsAccountIdStandingOrders | UK Open Banking (v3_1_0) | No |
| GET | /accounts/ACCOUNTID/statements | getAccountsAccountIdStatements | UK Open Banking (v3_1_0) | No |
| GET | /accounts/ACCOUNTID/statements/STATEMENTID | getAccountsAccountIdStatementsStatementId | UK Open Banking (v3_1_0) | No |
| GET | /accounts/ACCOUNTID/statements/STATEMENTID/file | getAccountsAccountIdStatementsStatementIdFile | UK Open Banking (v3_1_0) | No |
| GET | /accounts/ACCOUNTID/statements/STATEMENTID/transactions | getAccountsAccountIdStatementsStatementIdTransactions | UK Open Banking (v3_1_0) | No |
| GET | /accounts/ACCOUNTRESOURCEID/balances | accountsBalancesGet | STET (French) (v1_4) | No |
| GET | /accounts/ACCOUNTRESOURCEID/transactions | accountsTransactionsGet | STET (French) (v1_4) | No |
| GET | /accounts/ACCOUNT_ID | getAccount | UK Open Banking (v2_0_0) | No |
| GET | /accounts/ACCOUNT_ID | getAccountsAccountId | UK Open Banking (v3_1_0) | No |
| GET | /accounts/ACCOUNT_ID | getAccountDetails | Berlin Group PSD2 (v1_3) | No |
| GET | /accounts/ACCOUNT_ID | accountsAccountIdGet | Bahrain OBF (v1_0_0) | No |
| GET | /accounts/ACCOUNT_ID/balances | getAccountBalances | UK Open Banking (v2_0_0) | No |
| GET | /accounts/ACCOUNT_ID/balances | getAccountsAccountIdBalances | UK Open Banking (v3_1_0) | No |
| GET | /accounts/ACCOUNT_ID/balances | getBalances | Berlin Group PSD2 (v1_3) | No |
| GET | /accounts/ACCOUNT_ID/balances | accountsAccountIdBalancesGet | Bahrain OBF (v1_0_0) | No |
| GET | /accounts/ACCOUNT_ID/beneficiaries | accountsAccountIdBeneficiariesGet | Bahrain OBF (v1_0_0) | No |
| GET | /accounts/ACCOUNT_ID/direct-debits | accountsAccountIdDirectDebitsGet | Bahrain OBF (v1_0_0) | No |
| GET | /accounts/ACCOUNT_ID/future-dated-payments | accountsAccountIdFutureDatedPaymentsGet | Bahrain OBF (v1_0_0) | No |
| GET | /accounts/ACCOUNT_ID/offers | accountsAccountIdOffersGet | Bahrain OBF (v1_0_0) | No |
| GET | /accounts/ACCOUNT_ID/parties | accountsAccountIdPartiesGet | Bahrain OBF (v1_0_0) | No |
| GET | /accounts/ACCOUNT_ID/party | accountsAccountIdPartyGet | Bahrain OBF (v1_0_0) | No |
| GET | /accounts/ACCOUNT_ID/standing-orders | accountsAccountIdStandingOrdersGet | Bahrain OBF (v1_0_0) | No |
| GET | /accounts/ACCOUNT_ID/statements | accountsAccountIdStatementsGet | Bahrain OBF (v1_0_0) | No |
| GET | /accounts/ACCOUNT_ID/statements/STATEMENT_ID | accountsAccountIdStatementsStatementIdGet | Bahrain OBF (v1_0_0) | No |
| GET | /accounts/ACCOUNT_ID/statements/STATEMENT_ID/file | accountsAccountIdStatementsStatementIdFileGet | Bahrain OBF (v1_0_0) | No |
| GET | /accounts/ACCOUNT_ID/statements/STATEMENT_ID/transactions | accountsAccountIdStatementsStatementIdTransactionsGet | Bahrain OBF (v1_0_0) | No |
| GET | /accounts/ACCOUNT_ID/supplementary-account-info | accountsAccountIdSupplementaryAccountInfoGet | Bahrain OBF (v1_0_0) | No |
| GET | /accounts/ACCOUNT_ID/transactions | getAccountTransactions | UK Open Banking (v2_0_0) | No |
| GET | /accounts/ACCOUNT_ID/transactions | getAccountsAccountIdTransactions | UK Open Banking (v3_1_0) | No |
| GET | /accounts/ACCOUNT_ID/transactions | getTransactionList | Berlin Group PSD2 (v1_3) | No |
| GET | /accounts/ACCOUNT_ID/transactions | accountsAccountIdTransactionsGet | Bahrain OBF (v1_0_0) | No |
| GET | /accounts/ACCOUNT_ID/transactions/TRANSACTIONID | getTransactionDetails | Berlin Group PSD2 (v1_3) | No |
| GET | /adapter | getAdapterInfo | v3_1_0 | Yes |
| GET | /adapter | getAdapterInfo | v5_0_0 | Yes |
| GET | /api-collections/API_COLLECTION_ID/api-collection-endpoints | getApiCollectionEndpoints | v4_0_0 | No |
| GET | /api-collections/featured | getFeaturedApiCollections | v4_0_0 | No |
| GET | /api-collections/sharable/API_COLLECTION_ID | getSharableApiCollectionById | v4_0_0 | No |
| GET | /api/glossary | getApiGlossary | v3_0_0 | No |
| GET | /api/versions | getScannedApiVersions | v4_0_0 | No |
| GET | /atms | getMxAtms | Mexican Open Finance (MxOF) | No |
| GET | /balances | getBalances | UK Open Banking (v2_0_0) | No |
| GET | /balances | getBalances | UK Open Banking (v3_1_0) | No |
| GET | /balances | balancesGet | Bahrain OBF (v1_0_0) | No |
| GET | /banking/accounts | listAccounts | Australian Open Banking (v1_0_0) | No |
| GET | /banking/accounts/ACCOUNT_ID | getAccountDetail | Australian Open Banking (v1_0_0) | No |
| GET | /banking/accounts/ACCOUNT_ID/balance | listBalance | Australian Open Banking (v1_0_0) | No |
| GET | /banking/accounts/ACCOUNT_ID/direct-debits | listDirectDebits | Australian Open Banking (v1_0_0) | No |
| GET | /banking/accounts/ACCOUNT_ID/payments/scheduled | listScheduledPayments | Australian Open Banking (v1_0_0) | No |
| GET | /banking/accounts/ACCOUNT_ID/transactions | getTransactions | Australian Open Banking (v1_0_0) | No |
| GET | /banking/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID | getTransactionDetail | Australian Open Banking (v1_0_0) | No |
| GET | /banking/accounts/balances | listBalancesBulk | Australian Open Banking (v1_0_0) | No |
| GET | /banking/accounts/direct-debits | listDirectDebitsBulk | Australian Open Banking (v1_0_0) | No |
| GET | /banking/payees | listPayees | Australian Open Banking (v1_0_0) | No |
| GET | /banking/payees/PAYEE_ID | getPayeeDetail | Australian Open Banking (v1_0_0) | No |
| GET | /banking/payments/scheduled | listScheduledPaymentsBulk | Australian Open Banking (v1_0_0) | No |
| GET | /banking/products | listProducts | Australian Open Banking (v1_0_0) | No |
| GET | /banking/products/PRODUCT_ID | getProductDetail | Australian Open Banking (v1_0_0) | No |
| GET | /banks | getBanks | v3_0_0 | No |
| GET | /banks | getBanks | v4_0_0 | No |
| GET | /banks/BANK_ID | bankById | v3_0_0 | No |
| GET | /banks/BANK_ID | getBank | v4_0_0 | No |
| GET | /banks/BANK_ID | getBank | v5_0_0 | No |
| GET | /banks/BANK_ID/account-applications | getAccountApplications | v3_1_0 | No |
| GET | /banks/BANK_ID/account-applications/ACCOUNT_APPLICATION_ID | getAccountApplication | v3_1_0 | No |
| GET | /banks/BANK_ID/accounts | getPrivateAccountsAtOneBank | v4_0_0 | No |
| GET | /banks/BANK_ID/accounts-held | getAccountsHeld | v3_0_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/account | getPrivateAccountById | v3_0_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/account | getPrivateAccountByIdFull | v3_1_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/account | getPrivateAccountByIdFull | v4_0_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/checkbook/orders | getCheckbookOrders | v3_1_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/counterparties | getExplicitCounterpartiesForAccount | v2_2_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/counterparties/COUNTERPARTY_ID | getExplicitCounterpartyById | v2_2_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/credit_cards/orders | getStatusOfCreditCardOrder | v3_1_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/funds-available | checkFundsAvailable | v3_1_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/other_accounts | getOtherAccountsForBankAccount | v3_0_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/other_accounts/OTHER_ACCOUNT_ID | getOtherAccountByIdForBankAccount | v3_0_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-requests | getTransactionRequests | v3_1_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-requests | getTransactionRequests | v5_1_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-requests/TRANSACTION_REQUEST_ID | getTransactionRequest | v4_0_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions | getTransactionsForBankAccount | v3_0_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/double-entry-transaction | getDoubleEntryTransaction | v4_0_0 | Yes |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/transaction | getTransactionByIdForBankAccount | v3_1_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/balances | getBankAccountBalancesForCurrentUser | v4_0_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/balances | getAllBankAccountBalances | v5_1_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/balances/BALANCE_ID | getBankAccountBalanceById | v5_1_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/customer-account-links | getCustomerAccountLinksByBankIdAccountId | v5_0_0 | Yes |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/permissions/PROVIDER/PROVIDER_ID | getPermissionForUserForBankAccount | v3_0_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/transaction-requests/TRANSACTION_REQUEST_ID/attributes | getTransactionRequestAttributes | v4_0_0 | Yes |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/transaction-requests/TRANSACTION_REQUEST_ID/attributes/ATTRIBUTE_ID | getTransactionRequestAttributeById | v4_0_0 | Yes |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID/attributes | getTransactionAttributes | v4_0_0 | Yes |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID/attributes/ATTRIBUTE_ID | getTransactionAttributeById | v4_0_0 | Yes |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/views | getViewsForBankAccount | v2_2_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/views | getViewsForBankAccount | v3_0_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/views | getViewsForBankAccount | v5_0_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID | getCoreAccountByIdThroughView | v5_1_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/balances | getBankAccountBalances | v5_1_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/counterparties/COUNTERPARTY_ID/limit-status | getCounterpartyLimitStatus | v5_1_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/counterparties/COUNTERPARTY_ID/limits | getCounterpartyLimit | v5_1_0 | No |
| GET | /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/target-views/TARGET_VIEW_ID | getCustomView | v5_1_0 | No |
| GET | /banks/BANK_ID/accounts/account_ids/private | getPrivateAccountIdsbyBankId | v3_0_0 | No |
| GET | /banks/BANK_ID/accounts/private | privateAccountsAtOneBank | v3_0_0 | No |
| GET | /banks/BANK_ID/adapter | getAdapterInfoForBank | v3_0_0 | Yes |
| GET | /banks/BANK_ID/agents | getAgents | v5_1_0 | Yes |
| GET | /banks/BANK_ID/agents/AGENT_ID | getAgent | v5_1_0 | No |
| GET | /banks/BANK_ID/atms | getAtms | v3_0_0 | No |
| GET | /banks/BANK_ID/atms | getAtms | v4_0_0 | No |
| GET | /banks/BANK_ID/atms | getAtms | v5_1_0 | No |
| GET | /banks/BANK_ID/atms/ATM_ID | getAtm | v3_0_0 | No |
| GET | /banks/BANK_ID/atms/ATM_ID | getAtm | v4_0_0 | No |
| GET | /banks/BANK_ID/atms/ATM_ID | getAtm | v5_1_0 | No |
| GET | /banks/BANK_ID/atms/ATM_ID/attributes | getAtmAttributes | v5_1_0 | Yes |
| GET | /banks/BANK_ID/atms/ATM_ID/attributes/ATM_ATTRIBUTE_ID | getAtmAttribute | v5_1_0 | Yes |
| GET | /banks/BANK_ID/attribute-definitions/account | getAccountAttributeDefinition | v4_0_0 | Yes |
| GET | /banks/BANK_ID/attribute-definitions/card | getCardAttributeDefinition | v4_0_0 | Yes |
| GET | /banks/BANK_ID/attribute-definitions/customer | getCustomerAttributeDefinition | v4_0_0 | Yes |
| GET | /banks/BANK_ID/attribute-definitions/product | getProductAttributeDefinition | v4_0_0 | Yes |
| GET | /banks/BANK_ID/attribute-definitions/transaction | getTransactionAttributeDefinition | v4_0_0 | Yes |
| GET | /banks/BANK_ID/attribute-definitions/transaction-request | getTransactionRequestAttributeDefinition | v4_0_0 | Yes |
| GET | /banks/BANK_ID/attributes | getBankAttributes | v4_0_0 | Yes |
| GET | /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID | getBankAttribute | v4_0_0 | Yes |
| GET | /banks/BANK_ID/balances | getBankAccountsBalances | v3_1_0 | Yes |
| GET | /banks/BANK_ID/balances | getBankAccountsBalancesForCurrentUser | v4_0_0 | No |
| GET | /banks/BANK_ID/balances | getBankAccountsBalances | v5_1_0 | No |
| GET | /banks/BANK_ID/branches | getBranches | v3_0_0 | No |
| GET | /banks/BANK_ID/branches/BRANCH_ID | getBranch | v3_0_0 | No |
| GET | /banks/BANK_ID/currencies | getCurrenciesAtBank | v5_1_0 | No |
| GET | /banks/BANK_ID/customer-account-links/CUSTOMER_ACCOUNT_LINK_ID | getCustomerAccountLinkById | v5_0_0 | Yes |
| GET | /banks/BANK_ID/customers | getCustomersByAttributes | v4_0_0 | Yes |
| GET | /banks/BANK_ID/customers | getCustomersAtOneBank | v5_0_0 | Yes |
| GET | /banks/BANK_ID/customers-minimal | getCustomersMinimalAtOneBank | v5_0_0 | Yes |
| GET | /banks/BANK_ID/customers/CUSTOMER_ID | getCustomerByCustomerId | v3_1_0 | Yes |
| GET | /banks/BANK_ID/customers/CUSTOMER_ID/addresses | getCustomerAddresses | v3_1_0 | Yes |
| GET | /banks/BANK_ID/customers/CUSTOMER_ID/attributes | getCustomerAttributes | v4_0_0 | Yes |
| GET | /banks/BANK_ID/customers/CUSTOMER_ID/attributes/ATTRIBUTE_ID | getCustomerAttributeById | v4_0_0 | Yes |
| GET | /banks/BANK_ID/customers/CUSTOMER_ID/correlated-users | getCorrelatedUsersInfoByCustomerId | v4_0_0 | Yes |
| GET | /banks/BANK_ID/customers/CUSTOMER_ID/customer-account-links | getCustomerAccountLinksByCustomerId | v5_0_0 | Yes |
| GET | /banks/BANK_ID/customers/CUSTOMER_ID/messages | getCustomerMessages | v4_0_0 | Yes |
| GET | /banks/BANK_ID/customers/CUSTOMER_ID/tax-residences | getTaxResidence | v3_1_0 | No |
| GET | /banks/BANK_ID/entitlements | getEntitlementsForBank | v4_0_0 | Yes |
| GET | /banks/BANK_ID/firehose/accounts/ACCOUNT_ID/views/VIEW_ID/transactions | getFirehoseTransactionsForBankAccount | v3_0_0 | Yes |
| GET | /banks/BANK_ID/firehose/accounts/views/VIEW_ID | getFirehoseAccountsAtOneBank | v3_0_0 | Yes |
| GET | /banks/BANK_ID/firehose/accounts/views/VIEW_ID | getFirehoseAccountsAtOneBank | v4_0_0 | Yes |
| GET | /banks/BANK_ID/firehose/customers | getFirehoseCustomers | v3_1_0 | Yes |
| GET | /banks/BANK_ID/fx/FROM_CURRENCY_CODE/TO_CURRENCY_CODE | getCurrentFxRate | v2_2_0 | No |
| GET | /banks/BANK_ID/meetings | getMeetings | v3_1_0 | No |
| GET | /banks/BANK_ID/my/consent-infos | getConsentInfosByBank | v4_0_0 | No |
| GET | /banks/BANK_ID/my/consents | getConsents | v3_1_0 | No |
| GET | /banks/BANK_ID/my/consents | getConsents | v4_0_0 | No |
| GET | /banks/BANK_ID/my/consents | getMyConsentsByBank | v5_1_0 | No |
| GET | /banks/BANK_ID/my/customers | getMyCustomersAtBank | v5_0_0 | No |
| GET | /banks/BANK_ID/product-collections/COLLECTION_CODE | getProductCollection | v3_1_0 | No |
| GET | /banks/BANK_ID/product-tree/PRODUCT_CODE | getProductTree | v3_1_0 | No |
| GET | /banks/BANK_ID/products/PRODUCT_CODE | getProduct | v4_0_0 | No |
| GET | /banks/BANK_ID/products/PRODUCT_CODE/attributes/PRODUCT_ATTRIBUTE_ID | getProductAttribute | v3_1_0 | Yes |
| GET | /banks/BANK_ID/products/PRODUCT_CODE/attributes/PRODUCT_ATTRIBUTE_ID | getProductAttribute | v4_0_0 | Yes |
| GET | /banks/BANK_ID/products/PRODUCT_CODE/fees | getProductFees | v4_0_0 | No |
| GET | /banks/BANK_ID/products/PRODUCT_CODE/fees/PRODUCT_FEE_ID | getProductFee | v4_0_0 | No |
| GET | /banks/BANK_ID/public/accounts/ACCOUNT_ID/VIEW_ID/account | getPublicAccountById | v3_0_0 | No |
| GET | /banks/BANK_ID/settlement-accounts | getSettlementAccounts | v4_0_0 | Yes |
| GET | /banks/BANK_ID/user-invitations | getUserInvitations | v4_0_0 | Yes |
| GET | /banks/BANK_ID/user-invitations/SECRET_LINK | getUserInvitation | v4_0_0 | Yes |
| GET | /banks/BANK_ID/user_customer_links/customers/CUSTOMER_ID | getUserCustomerLinksByCustomerId | v4_0_0 | Yes |
| GET | /banks/BANK_ID/user_customer_links/users/USER_ID | getUserCustomerLinksByUserId | v4_0_0 | Yes |
| GET | /banks/BANK_ID/views/VIEW_ID/balances | getBankAccountsBalancesThroughView | v5_1_0 | No |
| GET | /beneficiaries | getBeneficiaries | UK Open Banking (v3_1_0) | No |
| GET | /beneficiaries | beneficiariesGet | Bahrain OBF (v1_0_0) | No |
| GET | /card-accounts | getCardAccounts | Berlin Group PSD2 (v1_3) | No |
| GET | /card-accounts/ACCOUNT_ID | readCardAccount | Berlin Group PSD2 (v1_3) | No |
| GET | /card-accounts/ACCOUNT_ID/balances | getCardAccountBalances | Berlin Group PSD2 (v1_3) | No |
| GET | /card-accounts/ACCOUNT_ID/transactions | getCardAccountTransactionList | Berlin Group PSD2 (v1_3) | No |
| GET | /common/customer | getCustomer | Australian Open Banking (v1_0_0) | No |
| GET | /common/customer/detail | getCustomerDetail | Australian Open Banking (v1_0_0) | No |
| GET | /config | config | v2_2_0 | Yes |
| GET | /config | config | v3_1_0 | Yes |
| GET | /connector/loopback | getObpConnectorLoopback | v3_1_0 | No |
| GET | /consents/CONSENTID | getConsentInformation | Berlin Group PSD2 (v1_3) | No |
| GET | /consents/CONSENTID/authorisations | getConsentAuthorisation | Berlin Group PSD2 (v1_3) | No |
| GET | /consents/CONSENTID/authorisations/AUTHORISATIONID | getConsentScaStatus | Berlin Group PSD2 (v1_3) | No |
| GET | /consents/CONSENTID/status | getConsentStatus | Berlin Group PSD2 (v1_3) | No |
| GET | /consumer/consent-requests/CONSENT_REQUEST_ID | getConsentRequest | v5_0_0 | No |
| GET | /consumer/consent-requests/CONSENT_REQUEST_ID/consents | getConsentByConsentRequestId | v5_0_0 | No |
| GET | /consumer/current/consents/CONSENT_ID | getConsentByConsentIdViaConsumer | v5_1_0 | No |
| GET | /consumers/CONSUMER_ID/scopes | getScopes | v3_0_0 | No |
| GET | /consumers/CONSUMER_ID/scopes | getScopes | v4_0_0 | No |
| GET | /customers | getCustomersAtAnyBank | v4_0_0 | Yes |
| GET | /customers-minimal | getCustomersMinimalAtAnyBank | v4_0_0 | Yes |
| GET | /customers/CUSTOMER_ID/accounts-minimal | getAccountsMinimalByCustomerId | v4_0_0 | Yes |
| GET | /database/info | getMapperDatabaseInfo | v4_0_0 | Yes |
| GET | /development/call_context | getCallContext | v4_0_0 | Yes |
| GET | /development/echo/jws-verified-request-jws-signed-response | verifyRequestSignResponse | v4_0_0 | No |
| GET | /direct-debits | getDirectDebits | UK Open Banking (v3_1_0) | No |
| GET | /direct-debits | directDebitsGet | Bahrain OBF (v1_0_0) | No |
| GET | /discovery/outages | getOutages | Australian Open Banking (v1_0_0) | No |
| GET | /discovery/status | getStatus | Australian Open Banking (v1_0_0) | No |
| GET | /domestic-future-dated-payment-cancellation-consents/CONSENT_ID | domesticFutureDatedPaymentCancellationConsentsConsentIdGet | Bahrain OBF (v1_0_0) | No |
| GET | /domestic-future-dated-payment-consents/CONSENT_ID | domesticFutureDatedPaymentConsentsConsentIdGet | Bahrain OBF (v1_0_0) | No |
| GET | /domestic-future-dated-payments/DOMESTIC_FUTURE_DATED_PAYMENT_ID | domesticFutureDatedPaymentsDomesticFutureDatedPaymentIdGet | Bahrain OBF (v1_0_0) | No |
| GET | /domestic-future-dated-payments/DOMESTIC_FUTURE_DATED_PAYMENT_ID/payment-details | domesticFutureDatedPaymentsDomesticFutureDatedPaymentIdPaymentDetailsGet | Bahrain OBF (v1_0_0) | No |
| GET | /domestic-payment-consents/CONSENTID | getDomesticPaymentConsentsConsentId | UK Open Banking (v3_1_0) | No |
| GET | /domestic-payment-consents/CONSENTID/funds-confirmation | getDomesticPaymentConsentsConsentIdFundsConfirmation | UK Open Banking (v3_1_0) | No |
| GET | /domestic-payment-consents/CONSENT_ID | domesticPaymentConsentsConsentIdGet | Bahrain OBF (v1_0_0) | No |
| GET | /domestic-payment-consents/CONSENT_ID/funds-confirmation | domesticPaymentConsentsConsentIdFundsConfirmationGet | Bahrain OBF (v1_0_0) | No |
| GET | /domestic-payments/DOMESTICPAYMENTID | getDomesticPaymentsDomesticPaymentId | UK Open Banking (v3_1_0) | No |
| GET | /domestic-payments/DOMESTIC_PAYMENT_ID | domesticPaymentsDomesticPaymentIdGet | Bahrain OBF (v1_0_0) | No |
| GET | /domestic-payments/DOMESTIC_PAYMENT_ID/payment-details | domesticPaymentsDomesticPaymentIdPaymentDetailsGet | Bahrain OBF (v1_0_0) | No |
| GET | /domestic-scheduled-payment-consents/CONSENTID | getDomesticScheduledPaymentConsentsConsentId | UK Open Banking (v3_1_0) | No |
| GET | /domestic-scheduled-payments/DOMESTICSCHEDULEDPAYMENTID | getDomesticScheduledPaymentsDomesticScheduledPaymentId | UK Open Banking (v3_1_0) | No |
| GET | /domestic-standing-order-consents/CONSENTID | getDomesticStandingOrderConsentsConsentId | UK Open Banking (v3_1_0) | No |
| GET | /domestic-standing-orders/DOMESTICSTANDINGORDERID | getDomesticStandingOrdersDomesticStandingOrderId | UK Open Banking (v3_1_0) | No |
| GET | /dummy | testResourceDoc | v1_4_0 | No |
| GET | /end-user-identity | endUserIdentityGet | STET (French) (v1_4) | No |
| GET | /endpoints/authentication-type-validations | getAllAuthenticationTypeValidationsPublic | v4_0_0 | Yes |
| GET | /endpoints/json-schema-validations | getAllJsonSchemaValidationsPublic | v4_0_0 | Yes |
| GET | /entitlement-requests | getAllEntitlementRequests | v3_0_0 | Yes |
| GET | /entitlements | getAllEntitlements | v3_1_0 | No |
| GET | /file-payment-consents/CONSENTID | getFilePaymentConsentsConsentId | UK Open Banking (v3_1_0) | No |
| GET | /file-payment-consents/CONSENTID/file | getFilePaymentConsentsConsentIdFile | UK Open Banking (v3_1_0) | No |
| GET | /file-payment-consents/CONSENT_ID | filePaymentConsentsConsentIdGet | Bahrain OBF (v1_0_0) | No |
| GET | /file-payment-consents/CONSENT_ID/file | filePaymentConsentsConsentIdFileGet | Bahrain OBF (v1_0_0) | No |
| GET | /file-payments/FILEPAYMENTID | getFilePaymentsFilePaymentId | UK Open Banking (v3_1_0) | No |
| GET | /file-payments/FILEPAYMENTID/report-file | getFilePaymentsFilePaymentIdReportFile | UK Open Banking (v3_1_0) | No |
| GET | /file-payments/FILE_PAYMENT_ID | filePaymentsFilePaymentIdGet | Bahrain OBF (v1_0_0) | No |
| GET | /file-payments/FILE_PAYMENT_ID/payment-details | filePaymentsFilePaymentIdPaymentDetailsGet | Bahrain OBF (v1_0_0) | No |
| GET | /file-payments/FILE_PAYMENT_ID/report-file | filePaymentsFilePaymentIdReportFileGet | Bahrain OBF (v1_0_0) | No |
| GET | /funds-confirmation-consents/CONSENTID | getFundsConfirmationConsentsConsentId | UK Open Banking (v3_1_0) | No |
| GET | /future-dated-payments | futureDatedPaymentsGet | Bahrain OBF (v1_0_0) | No |
| GET | /international-payment-consents/CONSENTID | getInternationalPaymentConsentsConsentId | UK Open Banking (v3_1_0) | No |
| GET | /international-payment-consents/CONSENTID/funds-confirmation | getInternationalPaymentConsentsConsentIdFundsConfirmation | UK Open Banking (v3_1_0) | No |
| GET | /international-payment-consents/CONSENT_ID | internationalPaymentConsentsConsentIdGet | Bahrain OBF (v1_0_0) | No |
| GET | /international-payment-consents/CONSENT_ID/funds-confirmation | internationalPaymentConsentsConsentIdFundsConfirmationGet | Bahrain OBF (v1_0_0) | No |
| GET | /international-payments/INTERNATIONALPAYMENTID | getInternationalPaymentsInternationalPaymentId | UK Open Banking (v3_1_0) | No |
| GET | /international-payments/INTERNATIONAL_PAYMENT_ID | internationalPaymentsInternationalPaymentIdGet | Bahrain OBF (v1_0_0) | No |
| GET | /international-payments/INTERNATIONAL_PAYMENT_ID/payment-details | internationalPaymentsInternationalPaymentIdPaymentDetailsGet | Bahrain OBF (v1_0_0) | No |
| GET | /international-scheduled-payment-consents/CONSENTID | getInternationalScheduledPaymentConsentsConsentId | UK Open Banking (v3_1_0) | No |
| GET | /international-scheduled-payment-consents/CONSENTID/funds-confirmation | getInternationalScheduledPaymentConsentsConsentIdFundsConfirmation | UK Open Banking (v3_1_0) | No |
| GET | /international-scheduled-payments/INTERNATIONALSCHEDULEDPAYMENTID | getInternationalScheduledPaymentsInternationalScheduledPaymentId | UK Open Banking (v3_1_0) | No |
| GET | /international-standing-order-consents/CONSENTID | getInternationalStandingOrderConsentsConsentId | UK Open Banking (v3_1_0) | No |
| GET | /international-standing-orders/INTERNATIONALSTANDINGORDERPAYMENTID | getInternationalStandingOrdersInternationalStandingOrderPaymentId | UK Open Banking (v3_1_0) | No |
| GET | /management/aggregate-metrics | getAggregateMetrics | v3_0_0 | No |
| GET | /management/aggregate-metrics | getAggregateMetrics | v5_1_0 | No |
| GET | /management/api-collections | getAllApiCollections | v5_1_0 | No |
| GET | /management/authentication-type-validations | getAllAuthenticationTypeValidations | v4_0_0 | Yes |
| GET | /management/authentication-type-validations/OPERATION_ID | getAuthenticationTypeValidation | v4_0_0 | Yes |
| GET | /management/banks/BANK_ID/account-web-hooks | getAccountWebhooks | v3_1_0 | Yes |
| GET | /management/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/counterparties | getCounterpartiesForAnyAccount | v4_0_0 | Yes |
| GET | /management/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/counterparties/COUNTERPARTY_ID | getCounterpartyByIdForAnyAccount | v4_0_0 | Yes |
| GET | /management/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/counterparty-names/COUNTERPARTY_NAME | getCounterpartyByNameForAnyAccount | v4_0_0 | Yes |
| GET | /management/banks/BANK_ID/cards | getCardsForBank | v3_1_0 | No |
| GET | /management/banks/BANK_ID/cards/CARD_ID | getCardForBank | v3_1_0 | Yes |
| GET | /management/banks/BANK_ID/dynamic-endpoints | getBankLevelDynamicEndpoints | v4_0_0 | Yes |
| GET | /management/banks/BANK_ID/dynamic-endpoints/DYNAMIC_ENDPOINT_ID | getBankLevelDynamicEndpoint | v4_0_0 | Yes |
| GET | /management/banks/BANK_ID/dynamic-entities | getBankLevelDynamicEntities | v4_0_0 | Yes |
| GET | /management/banks/BANK_ID/dynamic-message-docs | getAllBankLevelDynamicMessageDocs | v4_0_0 | Yes |
| GET | /management/banks/BANK_ID/dynamic-message-docs/DYNAMIC_MESSAGE_DOC_ID | getBankLevelDynamicMessageDoc | v4_0_0 | Yes |
| GET | /management/banks/BANK_ID/dynamic-resource-docs | getAllBankLevelDynamicResourceDocs | v4_0_0 | Yes |
| GET | /management/banks/BANK_ID/dynamic-resource-docs/DYNAMIC-RESOURCE-DOC-ID | getBankLevelDynamicResourceDoc | v4_0_0 | Yes |
| GET | /management/banks/BANK_ID/endpoint-mappings | getAllBankLevelEndpointMappings | v4_0_0 | Yes |
| GET | /management/banks/BANK_ID/endpoint-mappings/ENDPOINT_MAPPING_ID | getBankLevelEndpointMapping | v4_0_0 | Yes |
| GET | /management/banks/BANK_ID/endpoints/OPERATION_ID/tags | getBankLevelEndpointTags | v4_0_0 | Yes |
| GET | /management/banks/BANK_ID/fast-firehose/accounts | getFastFirehoseAccountsAtOneBank | v4_0_0 | Yes |
| GET | /management/connector-methods | getAllConnectorMethods | v4_0_0 | Yes |
| GET | /management/connector-methods/CONNECTOR_METHOD_ID | getConnectorMethod | v4_0_0 | Yes |
| GET | /management/connector/metrics | getConnectorMetrics | v2_2_0 | Yes |
| GET | /management/consents | getConsents | v5_1_0 | Yes |
| GET | /management/consents/banks/BANK_ID | getConsentsAtBank | v5_1_0 | Yes |
| GET | /management/consumers | getConsumers | v3_1_0 | Yes |
| GET | /management/consumers | getConsumers | v5_1_0 | Yes |
| GET | /management/consumers/CONSUMER_ID | getConsumer | v3_1_0 | Yes |
| GET | /management/consumers/CONSUMER_ID | getConsumer | v5_1_0 | Yes |
| GET | /management/consumers/CONSUMER_ID/consumer/call-limits | getCallsLimit | v3_1_0 | Yes |
| GET | /management/dynamic-endpoints | getDynamicEndpoints | v4_0_0 | Yes |
| GET | /management/dynamic-endpoints/DYNAMIC_ENDPOINT_ID | getDynamicEndpoint | v4_0_0 | Yes |
| GET | /management/dynamic-message-docs | getAllDynamicMessageDocs | v4_0_0 | Yes |
| GET | /management/dynamic-message-docs/DYNAMIC_MESSAGE_DOC_ID | getDynamicMessageDoc | v4_0_0 | Yes |
| GET | /management/dynamic-resource-docs | getAllDynamicResourceDocs | v4_0_0 | Yes |
| GET | /management/dynamic-resource-docs/DYNAMIC-RESOURCE-DOC-ID | getDynamicResourceDoc | v4_0_0 | Yes |
| GET | /management/endpoint-mappings | getAllEndpointMappings | v4_0_0 | Yes |
| GET | /management/endpoint-mappings/ENDPOINT_MAPPING_ID | getEndpointMapping | v4_0_0 | Yes |
| GET | /management/endpoints/OPERATION_ID/tags | getSystemLevelEndpointTags | v4_0_0 | Yes |
| GET | /management/json-schema-validations | getAllJsonSchemaValidations | v4_0_0 | Yes |
| GET | /management/json-schema-validations/OPERATION_ID | getJsonSchemaValidation | v4_0_0 | Yes |
| GET | /management/method_routings | getMethodRoutings | v3_1_0 | Yes |
| GET | /management/metrics | getMetrics | v5_1_0 | No |
| GET | /management/metrics/banks/BANK_ID | getMetricsAtBank | v5_0_0 | No |
| GET | /management/metrics/top-apis | getTopAPIs | v3_1_0 | No |
| GET | /management/metrics/top-consumers | getMetricsTopConsumers | v3_1_0 | No |
| GET | /management/system-dynamic-entities | getSystemDynamicEntities | v4_0_0 | Yes |
| GET | /management/system/integrity/account-access-unique-index-1-check | accountAccessUniqueIndexCheck | v5_1_0 | No |
| GET | /management/system/integrity/banks/BANK_ID/account-currency-check | accountCurrencyCheck | v5_1_0 | No |
| GET | /management/system/integrity/banks/BANK_ID/orphaned-account-check | orphanedAccountCheck | v5_1_0 | No |
| GET | /management/system/integrity/custom-view-names-check | customViewNamesCheck | v5_1_0 | No |
| GET | /management/system/integrity/system-view-names-check | systemViewNamesCheck | v5_1_0 | No |
| GET | /management/transaction-requests/TRANSACTION_REQUEST_ID | getTransactionRequestById | v5_1_0 | Yes |
| GET | /management/users/current/consumers | getConsumersForCurrentUser | v3_1_0 | Yes |
| GET | /management/webui_props | getWebUiProps | v3_1_0 | Yes |
| GET | /message-docs/CONNECTOR | getMessageDocs | v2_2_0 | No |
| GET | /message-docs/CONNECTOR/swagger2.0 | getMessageDocsSwagger | v3_1_0 | No |
| GET | /my/accounts | corePrivateAccountsAllBanks | v3_0_0 | No |
| GET | /my/api-collection-ids/API_COLLECTION_ID/api-collection-endpoints | getMyApiCollectionEndpointsById | v4_0_0 | No |
| GET | /my/api-collections | getMyApiCollections | v4_0_0 | No |
| GET | /my/api-collections/API_COLLECTION_ID | getMyApiCollectionById | v4_0_0 | No |
| GET | /my/api-collections/API_COLLECTION_NAME/api-collection-endpoints | getMyApiCollectionEndpoints | v4_0_0 | No |
| GET | /my/api-collections/API_COLLECTION_NAME/api-collection-endpoints/OPERATION_ID | getMyApiCollectionEndpoint | v4_0_0 | No |
| GET | /my/api-collections/name/API_COLLECTION_NAME | getMyApiCollectionByName | v4_0_0 | No |
| GET | /my/banks/BANK_ID/accounts/ACCOUNT_ID/account | getCoreAccountById | v3_0_0 | No |
| GET | /my/banks/BANK_ID/accounts/ACCOUNT_ID/account | getCoreAccountById | v4_0_0 | No |
| GET | /my/banks/BANK_ID/accounts/ACCOUNT_ID/transactions | getCoreTransactionsForBankAccount | v3_0_0 | No |
| GET | /my/consent-infos | getConsentInfos | v4_0_0 | No |
| GET | /my/consents | getMyConsents | v5_1_0 | No |
| GET | /my/correlated-entities | getMyCorrelatedEntities | v4_0_0 | No |
| GET | /my/customers | getMyCustomersAtAnyBank | v5_0_0 | No |
| GET | /my/dynamic-endpoints | getMyDynamicEndpoints | v4_0_0 | No |
| GET | /my/dynamic-entities | getMyDynamicEntities | v4_0_0 | No |
| GET | /my/entitlement-requests | getEntitlementRequestsForCurrentUser | v3_0_0 | No |
| GET | /my/entitlements | getEntitlementsForCurrentUser | v3_0_0 | No |
| GET | /my/mtls/certificate/current | mtlsClientCertificateInfo | v5_1_0 | No |
| GET | /my/spaces | getMySpaces | v4_0_0 | No |
| GET | /my/user/attributes | getMyPersonalUserAttributes | v4_0_0 | No |
| GET | /offers | getOffers | UK Open Banking (v3_1_0) | No |
| GET | /offers | offersGet | Bahrain OBF (v1_0_0) | No |
| GET | /party | getParty | UK Open Banking (v3_1_0) | No |
| GET | /party | partyGet | Bahrain OBF (v1_0_0) | No |
| GET | /payment-requests/PAYMENTREQUESTRESOURCEID | paymentRequestsGet | STET (French) (v1_4) | No |
| GET | /products | getProducts | UK Open Banking (v3_1_0) | No |
| GET | /rate-limiting | getRateLimitingInfo | v3_1_0 | No |
| GET | /regulated-entities | regulatedEntities | v5_1_0 | No |
| GET | /regulated-entities/REGULATED_ENTITY_ID | getRegulatedEntityById | v5_1_0 | Yes |
| GET | /regulated-entities/REGULATED_ENTITY_ID/attributes | getAllRegulatedEntityAttributes | v5_1_0 | Yes |
| GET | /regulated-entities/REGULATED_ENTITY_ID/attributes/REGULATED_ENTITY_ATTRIBUTE_ID | getRegulatedEntityAttributeById | v5_1_0 | Yes |
| GET | /root | root | v2_2_0 | No |
| GET | /root | root | v5_1_0 | No |
| GET | /scheduled-payments | getScheduledPayments | UK Open Banking (v3_1_0) | No |
| GET | /signing-baskets/BASKETID | getSigningBasket | Berlin Group PSD2 (v1_3) | No |
| GET | /signing-baskets/BASKETID/authorisations | getSigningBasketAuthorisation | Berlin Group PSD2 (v1_3) | No |
| GET | /signing-baskets/BASKETID/authorisations/AUTHORISATIONID | getSigningBasketScaStatus | Berlin Group PSD2 (v1_3) | No |
| GET | /signing-baskets/BASKETID/status | getSigningBasketStatus | Berlin Group PSD2 (v1_3) | No |
| GET | /standing-orders | getStandingOrders | UK Open Banking (v3_1_0) | No |
| GET | /standing-orders | standingOrdersGet | Bahrain OBF (v1_0_0) | No |
| GET | /statements | getStatements | UK Open Banking (v3_1_0) | No |
| GET | /statements | statementsGet | Bahrain OBF (v1_0_0) | No |
| GET | /system-views-ids | getSystemViewsIds | v5_0_0 | Yes |
| GET | /tags | getApiTags | v5_1_0 | No |
| GET | /transactions | getTransactions | UK Open Banking (v3_1_0) | No |
| GET | /transactions | transactionsGet | Bahrain OBF (v1_0_0) | No |
| GET | /transactions/TRANSACTION_ID/balancing-transaction | getBalancingTransaction | v4_0_0 | No |
| GET | /trusted-beneficiaries | trustedBeneficiariesGet | STET (French) (v1_4) | No |
| GET | /ui/suggested-session-timeout | suggestedSessionTimeout | v5_1_0 | No |
| GET | /user/current/consents/CONSENT_ID | getConsentByConsentId | v5_1_0 | No |
| GET | /users | getUsers | v3_0_0 | Yes |
| GET | /users | getUsers | v4_0_0 | Yes |
| GET | /users/PROVIDER/USERNAME/lock-status | getUserLockStatus | v5_1_0 | Yes |
| GET | /users/USERNAME/lock-status | getBadLoginStatus | v3_1_0 | Yes |
| GET | /users/USER_ID/account-access | getAccountAccessByUserId | v5_1_0 | Yes |
| GET | /users/USER_ID/accounts-held | getAccountsHeldByUser | v5_1_0 | Yes |
| GET | /users/USER_ID/api-collections | getApiCollectionsForUser | v4_0_0 | No |
| GET | /users/USER_ID/attributes | getUserWithAttributes | v4_0_0 | No |
| GET | /users/USER_ID/auth-context | getUserAuthContexts | v3_1_0 | No |
| GET | /users/USER_ID/auth-context | getUserAuthContexts | v5_0_0 | No |
| GET | /users/USER_ID/banks/BANK_ID/accounts-held | getAccountsHeldByUserAtBank | v5_1_0 | Yes |
| GET | /users/USER_ID/entitlement-requests | getEntitlementRequests | v3_0_0 | Yes |
| GET | /users/USER_ID/entitlements-and-permissions | getEntitlementsAndPermissions | v5_1_0 | Yes |
| GET | /users/USER_ID/non-personal/attributes | getNonPersonalUserAttributes | v5_1_0 | Yes |
| GET | /users/current/customers | getCustomersForUser | v3_0_0 | No |
| GET | /users/current/customers/customer_ids | getCustomersForUserIdsOnly | v5_1_0 | No |
| GET | /users/current/user_id | getCurrentUserId | v4_0_0 | Yes |
| GET | /users/email/EMAIL/terminator | getUser | v3_0_0 | Yes |
| GET | /users/email/EMAIL/terminator | getUsersByEmail | v4_0_0 | Yes |
| GET | /users/provider/PROVIDER/username/USERNAME | getUserByProviderAndUsername | v5_1_0 | Yes |
| GET | /users/user_id/USER_ID | getUserByUserId | v3_0_0 | Yes |
| GET | /users/user_id/USER_ID | getUserByUserId | v4_0_0 | Yes |
| GET | /users/username/USERNAME | getUserByUsername | v3_0_0 | Yes |
| GET | /users/username/USERNAME | getUserByUsername | v4_0_0 | Yes |
| GET | /waiting-for-godot | waitingForGodot | v5_1_0 | No |
| GET | /webui-props | getWebUiProps | v5_1_0 | No |
| PATCH | /account-access-consents/CONSENT_ID | accountAccessConsentsConsentIdPatch | Bahrain OBF (v1_0_0) | No |
| PATCH | /domestic-future-dated-payments/DOMESTIC_FUTURE_DATED_PAYMENT_ID | domesticFutureDatedPaymentsDomesticFutureDatedPaymentIdPatch | Bahrain OBF (v1_0_0) | No |
| POST | /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENT_ID/authorisations | startPaymentAuthorisationUpdatePsuAuthentication | Berlin Group PSD2 (v1_3) | No |
| POST | /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENT_ID/cancellation-authorisations | startPaymentInitiationCancellationAuthorisationTransactionAuthorisation | Berlin Group PSD2 (v1_3) | No |
| POST | /account-access-consents | createAccountAccessConsents | UK Open Banking (v3_1_0) | No |
| POST | /account-access-consents | accountAccessConsentsPost | Bahrain OBF (v1_0_0) | No |
| POST | /account/check/scheme/iban | ibanChecker | v4_0_0 | No |
| POST | /accounts/v2_1_1.1/deleteConsent | deleteConsent | Polish API (v2_1_1_1) | No |
| POST | /accounts/v2_1_1.1/getAccount | getAccount | Polish API (v2_1_1_1) | No |
| POST | /accounts/v2_1_1.1/getAccounts | getAccounts | Polish API (v2_1_1_1) | No |
| POST | /accounts/v2_1_1.1/getHolds | getHolds | Polish API (v2_1_1_1) | No |
| POST | /accounts/v2_1_1.1/getTransactionDetail | getTransactionDetail | Polish API (v2_1_1_1) | No |
| POST | /accounts/v2_1_1.1/getTransactionsCancelled | getTransactionsCancelled | Polish API (v2_1_1_1) | No |
| POST | /accounts/v2_1_1.1/getTransactionsDone | getTransactionsDone | Polish API (v2_1_1_1) | No |
| POST | /accounts/v2_1_1.1/getTransactionsPending | getTransactionsPending | Polish API (v2_1_1_1) | No |
| POST | /accounts/v2_1_1.1/getTransactionsRejected | getTransactionsRejected | Polish API (v2_1_1_1) | No |
| POST | /accounts/v2_1_1.1/getTransactionsScheduled | getTransactionsScheduled | Polish API (v2_1_1_1) | No |
| POST | /auth/v2_1_1.1/authorize | authorize | Polish API (v2_1_1_1) | No |
| POST | /auth/v2_1_1.1/authorizeExt | authorizeExt | Polish API (v2_1_1_1) | No |
| POST | /auth/v2_1_1.1/token | token | Polish API (v2_1_1_1) | No |
| POST | /banking/accounts/balances | listBalancesSpecificAccounts | Australian Open Banking (v1_0_0) | No |
| POST | /banking/accounts/direct-debits | listDirectDebitsSpecificAccounts | Australian Open Banking (v1_0_0) | No |
| POST | /banking/payments/scheduled | listScheduledPaymentsSpecificAccounts | Australian Open Banking (v1_0_0) | No |
| POST | /banks | createBank | v2_2_0 | Yes |
| POST | /banks | createBank | v4_0_0 | Yes |
| POST | /banks | createBank | v5_0_0 | Yes |
| POST | /banks/BANK_ID/account-applications | createAccountApplication | v3_1_0 | No |
| POST | /banks/BANK_ID/account-web-hooks | createAccountWebhook | v3_1_0 | Yes |
| POST | /banks/BANK_ID/accounts | addAccount | v4_0_0 | Yes |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/counterparties | createCounterparty | v2_2_0 | No |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/counterparties | createExplicitCounterparty | v4_0_0 | No |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/counterparties/COUNTERPARTY_ID | deleteExplicitCounterparty | v4_0_0 | No |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/direct-debit | createDirectDebit | v4_0_0 | No |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/metadata/tags | addTagForViewOnAccount | v4_0_0 | No |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/standing-order | createStandingOrder | v4_0_0 | No |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-request-types/ACCOUNT/transaction-requests | createTransactionRequestAccount | v4_0_0 | No |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-request-types/ACCOUNT_OTP/transaction-requests | createTransactionRequestAccountOtp | v4_0_0 | No |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-request-types/AGENT_CASH_WITHDRAWAL/transaction-requests | createTransactionRequestAgentCashWithDrawal | v4_0_0 | No |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-request-types/REFUND/transaction-requests | createTransactionRequestRefund | v4_0_0 | No |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-request-types/SIMPLE/transaction-requests | createTransactionRequestSimple | v4_0_0 | No |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-request-types/TRANSACTION_REQUEST_TYPE/transaction-requests/TRANSACTION_REQUEST_ID/challenge | answerTransactionRequestChallenge | v4_0_0 | No |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/account-access/revoke | revokeUserAccessToView | v4_0_0 | No |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/balances | createBankAccountBalance | v5_1_0 | Yes |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/products/PRODUCT_CODE/attribute | createAccountAttribute | v3_1_0 | Yes |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/transaction-requests/TRANSACTION_REQUEST_ID/attribute | createTransactionRequestAttribute | v4_0_0 | Yes |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID/attribute | createTransactionAttribute | v4_0_0 | Yes |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/user-account-access | createUserWithAccountAccess | v4_0_0 | No |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/views | createViewForBankAccount | v2_2_0 | No |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/views | createViewForBankAccount | v3_0_0 | No |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/account-access/grant | grantUserAccessToViewById | v5_1_0 | No |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/account-access/revoke | revokeUserAccessToViewById | v5_1_0 | No |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/counterparties/COUNTERPARTY_ID/limits | createCounterpartyLimit | v5_1_0 | No |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/target-views | createCustomView | v5_1_0 | No |
| POST | /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/user-account-access | createUserWithAccountAccessById | v5_1_0 | No |
| POST | /banks/BANK_ID/agents | createAgent | v5_1_0 | No |
| POST | /banks/BANK_ID/atms | createAtm | v2_2_0 | Yes |
| POST | /banks/BANK_ID/atms | createAtm | v3_0_0 | Yes |
| POST | /banks/BANK_ID/atms | createAtm | v4_0_0 | Yes |
| POST | /banks/BANK_ID/atms | createAtm | v5_1_0 | Yes |
| POST | /banks/BANK_ID/atms/ATM_ID/attributes | createAtmAttribute | v5_1_0 | Yes |
| POST | /banks/BANK_ID/attribute | createBankAttribute | v4_0_0 | Yes |
| POST | /banks/BANK_ID/branches | createBranch | v2_2_0 | Yes |
| POST | /banks/BANK_ID/branches | createBranch | v3_0_0 | Yes |
| POST | /banks/BANK_ID/consents/CONSENT_ID/challenge | answerConsentChallenge | v3_1_0 | No |
| POST | /banks/BANK_ID/customer-account-links | createCustomerAccountLink | v5_0_0 | Yes |
| POST | /banks/BANK_ID/customer/CUSTOMER_ID/messages | addCustomerMessage | v1_4_0 | No |
| POST | /banks/BANK_ID/customers | createCustomer | v3_1_0 | Yes |
| POST | /banks/BANK_ID/customers | createCustomer | v4_0_0 | Yes |
| POST | /banks/BANK_ID/customers | createCustomer | v5_0_0 | Yes |
| POST | /banks/BANK_ID/customers/CUSTOMER_ID/address | createCustomerAddress | v3_1_0 | Yes |
| POST | /banks/BANK_ID/customers/CUSTOMER_ID/attribute | createCustomerAttribute | v4_0_0 | Yes |
| POST | /banks/BANK_ID/customers/CUSTOMER_ID/messages | createCustomerMessage | v4_0_0 | Yes |
| POST | /banks/BANK_ID/customers/CUSTOMER_ID/tax-residence | createTaxResidence | v3_1_0 | Yes |
| POST | /banks/BANK_ID/customers/customer-number | getCustomerByCustomerNumber | v3_1_0 | Yes |
| POST | /banks/BANK_ID/customers/customer-number-query/overview | getCustomerOverview | v5_0_0 | Yes |
| POST | /banks/BANK_ID/customers/customer-number-query/overview-flat | getCustomerOverviewFlat | v5_0_0 | Yes |
| POST | /banks/BANK_ID/customers/legal-name | getCustomersByLegalName | v5_1_0 | Yes |
| POST | /banks/BANK_ID/management/historical/transactions | createHistoricalTransactionAtBank | v4_0_0 | Yes |
| POST | /banks/BANK_ID/meetings | createMeeting | v3_1_0 | No |
| POST | /banks/BANK_ID/my/consents/EMAIL | createConsentEmail | v3_1_0 | No |
| POST | /banks/BANK_ID/my/consents/IMPLICIT | createConsentImplicit | v3_1_0 | No |
| POST | /banks/BANK_ID/my/consents/SMS | createConsentSms | v3_1_0 | No |
| POST | /banks/BANK_ID/products/PRODUCT_CODE/attribute | createProductAttribute | v3_1_0 | Yes |
| POST | /banks/BANK_ID/products/PRODUCT_CODE/attribute | createProductAttribute | v4_0_0 | Yes |
| POST | /banks/BANK_ID/products/PRODUCT_CODE/fee | createProductFee | v4_0_0 | Yes |
| POST | /banks/BANK_ID/search/customers/mobile-phone-number | getCustomersByCustomerPhoneNumber | v4_0_0 | Yes |
| POST | /banks/BANK_ID/settlement-accounts | createSettlementAccount | v4_0_0 | No |
| POST | /banks/BANK_ID/user-invitation | createUserInvitation | v4_0_0 | No |
| POST | /banks/BANK_ID/user-invitations | getUserInvitationAnonymous | v4_0_0 | No |
| POST | /banks/BANK_ID/user_customer_links | createUserCustomerLinks | v4_0_0 | Yes |
| POST | /banks/BANK_ID/users/current/auth-context-updates/AUTH_CONTEXT_UPDATE_ID/challenge | answerUserAuthContextUpdateChallenge | v3_1_0 | No |
| POST | /banks/BANK_ID/users/current/auth-context-updates/AUTH_CONTEXT_UPDATE_ID/challenge | answerUserAuthContextUpdateChallenge | v5_0_0 | No |
| POST | /banks/BANK_ID/users/current/auth-context-updates/SCA_METHOD | createUserAuthContextUpdateRequest | v3_1_0 | No |
| POST | /banks/BANK_ID/users/current/auth-context-updates/SCA_METHOD | createUserAuthContextUpdateRequest | v5_0_0 | No |
| POST | /banks/BANK_ID/web-hooks/account/notifications/on-create-transaction | createBankAccountNotificationWebhook | v4_0_0 | Yes |
| POST | /bulk-payments/PAYMENT_PRODUCT | initiateBulkPayments | Berlin Group PSD2 (v1_3) | No |
| POST | /confirmation/v2_1_1.1/getConfirmationOfFunds | getConfirmationOfFunds | Polish API (v2_1_1_1) | No |
| POST | /consents | createConsent | Berlin Group PSD2 (v1_3) | No |
| POST | /consents/CONSENTID/authorisations | startConsentAuthorisationTransactionAuthorisation | Berlin Group PSD2 (v1_3) | No |
| POST | /consumer/consent-requests | createConsentRequest | v5_0_0 | No |
| POST | /consumer/consent-requests/CONSENT_REQUEST_ID/EMAIL/consents | createConsentByConsentRequestIdEmail | v5_0_0 | No |
| POST | /consumer/consent-requests/CONSENT_REQUEST_ID/IMPLICIT/consents | createConsentByConsentRequestIdImplicit | v5_0_0 | No |
| POST | /consumer/consent-requests/CONSENT_REQUEST_ID/SMS/consents | createConsentByConsentRequestIdSms | v5_0_0 | No |
| POST | /consumer/vrp-consent-requests | createVRPConsentRequest | v5_1_0 | No |
| POST | /consumers/CONSUMER_ID/scopes | addScope | v3_0_0 | Yes |
| POST | /consumers/CONSUMER_ID/scopes | addScope | v4_0_0 | Yes |
| POST | /domestic-future-dated-payment-cancellation-consents | domesticFutureDatedPaymentCancellationConsentsPost | Bahrain OBF (v1_0_0) | No |
| POST | /domestic-future-dated-payment-consents | domesticFutureDatedPaymentConsentsPost | Bahrain OBF (v1_0_0) | No |
| POST | /domestic-future-dated-payments | domesticFutureDatedPaymentsPost | Bahrain OBF (v1_0_0) | No |
| POST | /domestic-payment-consents | createDomesticPaymentConsents | UK Open Banking (v3_1_0) | No |
| POST | /domestic-payment-consents | domesticPaymentConsentsPost | Bahrain OBF (v1_0_0) | No |
| POST | /domestic-payments | createDomesticPayments | UK Open Banking (v3_1_0) | No |
| POST | /domestic-payments | domesticPaymentsPost | Bahrain OBF (v1_0_0) | No |
| POST | /domestic-scheduled-payment-consents | createDomesticScheduledPaymentConsents | UK Open Banking (v3_1_0) | No |
| POST | /domestic-scheduled-payments | createDomesticScheduledPayments | UK Open Banking (v3_1_0) | No |
| POST | /domestic-standing-order-consents | createDomesticStandingOrderConsents | UK Open Banking (v3_1_0) | No |
| POST | /domestic-standing-orders | createDomesticStandingOrders | UK Open Banking (v3_1_0) | No |
| POST | /dynamic-registration/consumers | createConsumerDynamicRegistration | v5_1_0 | No |
| POST | /entitlement-requests | addEntitlementRequest | v3_0_0 | No |
| POST | /event-notifications | eventNotificationsPost | Bahrain OBF (v1_0_0) | No |
| POST | /file-payment-consents | createFilePaymentConsents | UK Open Banking (v3_1_0) | No |
| POST | /file-payment-consents | filePaymentConsentsPost | Bahrain OBF (v1_0_0) | No |
| POST | /file-payment-consents/CONSENTID/file | createFilePaymentConsentsConsentIdFile | UK Open Banking (v3_1_0) | No |
| POST | /file-payment-consents/CONSENT_ID/file | filePaymentConsentsConsentIdFilePost | Bahrain OBF (v1_0_0) | No |
| POST | /file-payments | createFilePayments | UK Open Banking (v3_1_0) | No |
| POST | /file-payments | filePaymentsPost | Bahrain OBF (v1_0_0) | No |
| POST | /funds-confirmation-consents | createFundsConfirmationConsents | UK Open Banking (v3_1_0) | No |
| POST | /funds-confirmations | createFundsConfirmations | UK Open Banking (v3_1_0) | No |
| POST | /funds-confirmations | checkAvailabilityOfFunds | Berlin Group PSD2 (v1_3) | No |
| POST | /funds-confirmations | fundsConfirmationsPost | STET (French) (v1_4) | No |
| POST | /international-payment-consents | createInternationalPaymentConsents | UK Open Banking (v3_1_0) | No |
| POST | /international-payment-consents | internationalPaymentConsentsPost | Bahrain OBF (v1_0_0) | No |
| POST | /international-payments | createInternationalPayments | UK Open Banking (v3_1_0) | No |
| POST | /international-payments | internationalPaymentsPost | Bahrain OBF (v1_0_0) | No |
| POST | /international-scheduled-payment-consents | createInternationalScheduledPaymentConsents | UK Open Banking (v3_1_0) | No |
| POST | /international-scheduled-payments | createInternationalScheduledPayments | UK Open Banking (v3_1_0) | No |
| POST | /international-standing-order-consents | createInternationalStandingOrderConsents | UK Open Banking (v3_1_0) | No |
| POST | /international-standing-orders | createInternationalStandingOrders | UK Open Banking (v3_1_0) | No |
| POST | /management/accounts/account-routing-query | getAccountByAccountRouting | v4_0_0 | No |
| POST | /management/accounts/account-routing-regex-query | getAccountsByAccountRoutingRegex | v4_0_0 | No |
| POST | /management/authentication-type-validations/OPERATION_ID | createAuthenticationTypeValidation | v4_0_0 | Yes |
| POST | /management/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/counterparties | createCounterpartyForAnyAccount | v4_0_0 | Yes |
| POST | /management/banks/BANK_ID/accounts/ACCOUNT_ID/direct-debit | createDirectDebitManagement | v4_0_0 | Yes |
| POST | /management/banks/BANK_ID/accounts/ACCOUNT_ID/standing-order | createStandingOrderManagement | v4_0_0 | Yes |
| POST | /management/banks/BANK_ID/cards | addCardForBank | v3_1_0 | Yes |
| POST | /management/banks/BANK_ID/cards | addCardForBank | v5_0_0 | Yes |
| POST | /management/banks/BANK_ID/cards/CARD_ID/attribute | createCardAttribute | v3_1_0 | No |
| POST | /management/banks/BANK_ID/dynamic-endpoints | createBankLevelDynamicEndpoint | v4_0_0 | Yes |
| POST | /management/banks/BANK_ID/dynamic-entities | createBankLevelDynamicEntity | v4_0_0 | No |
| POST | /management/banks/BANK_ID/dynamic-message-docs | createBankLevelDynamicMessageDoc | v4_0_0 | Yes |
| POST | /management/banks/BANK_ID/dynamic-resource-docs | createBankLevelDynamicResourceDoc | v4_0_0 | Yes |
| POST | /management/banks/BANK_ID/endpoint-mappings | createBankLevelEndpointMapping | v4_0_0 | Yes |
| POST | /management/banks/BANK_ID/endpoints/OPERATION_ID/tags | createBankLevelEndpointTag | v4_0_0 | Yes |
| POST | /management/connector-methods | createConnectorMethod | v4_0_0 | Yes |
| POST | /management/consumers | createConsumer | v2_2_0 | Yes |
| POST | /management/consumers | createConsumer | v5_1_0 | Yes |
| POST | /management/dynamic-endpoints | createDynamicEndpoint | v4_0_0 | Yes |
| POST | /management/dynamic-message-docs | createDynamicMessageDoc | v4_0_0 | Yes |
| POST | /management/dynamic-resource-docs | createDynamicResourceDoc | v4_0_0 | Yes |
| POST | /management/dynamic-resource-docs/endpoint-code | buildDynamicEndpointTemplate | v4_0_0 | No |
| POST | /management/endpoint-mappings | createEndpointMapping | v4_0_0 | Yes |
| POST | /management/endpoints/OPERATION_ID/tags | createSystemLevelEndpointTag | v4_0_0 | Yes |
| POST | /management/historical/transactions  | saveHistoricalTransaction | v3_1_0 | No |
| POST | /management/json-schema-validations/OPERATION_ID | createJsonSchemaValidation | v4_0_0 | Yes |
| POST | /management/method_routings | createMethodRouting | v3_1_0 | No |
| POST | /management/system-dynamic-entities | createSystemDynamicEntity | v4_0_0 | No |
| POST | /management/user/reset-password-url | resetPasswordUrl | v4_0_0 | Yes |
| POST | /management/webui_props | createWebUiProps | v3_1_0 | No |
| POST | /my/api-collection-ids/API_COLLECTION_ID/api-collection-endpoints | createMyApiCollectionEndpointById | v4_0_0 | No |
| POST | /my/api-collections | createMyApiCollection | v4_0_0 | No |
| POST | /my/api-collections/API_COLLECTION_NAME/api-collection-endpoints | createMyApiCollectionEndpoint | v4_0_0 | No |
| POST | /my/consents/IMPLICIT | createConsentImplicit | v5_1_0 | No |
| POST | /my/consumers | createMyConsumer | v5_1_0 | No |
| POST | /my/user/attributes | createMyPersonalUserAttribute | v4_0_0 | No |
| POST | /payment-requests | paymentRequestsPost | STET (French) (v1_4) | No |
| POST | /payment-requests/PAYMENTREQUESTRESOURCEID/confirmation | paymentRequestConfirmationPost | STET (French) (v1_4) | No |
| POST | /payments/PAYMENT_PRODUCT | initiatePayments | Berlin Group PSD2 (v1_3) | No |
| POST | /payments/v2_1_1.1/EEA | eEA | Polish API (v2_1_1_1) | No |
| POST | /payments/v2_1_1.1/bundle | bundle | Polish API (v2_1_1_1) | No |
| POST | /payments/v2_1_1.1/cancelPayments | cancelPayments | Polish API (v2_1_1_1) | No |
| POST | /payments/v2_1_1.1/cancelRecurringPayment | cancelRecurringPayment | Polish API (v2_1_1_1) | No |
| POST | /payments/v2_1_1.1/domestic | domestic | Polish API (v2_1_1_1) | No |
| POST | /payments/v2_1_1.1/getBundle | getBundle | Polish API (v2_1_1_1) | No |
| POST | /payments/v2_1_1.1/getMultiplePayments | getMultiplePayments | Polish API (v2_1_1_1) | No |
| POST | /payments/v2_1_1.1/getPayment | getPayment | Polish API (v2_1_1_1) | No |
| POST | /payments/v2_1_1.1/getRecurringPayment | getRecurringPayment | Polish API (v2_1_1_1) | No |
| POST | /payments/v2_1_1.1/nonEEA | nonEEA | Polish API (v2_1_1_1) | No |
| POST | /payments/v2_1_1.1/recurring | recurring | Polish API (v2_1_1_1) | No |
| POST | /payments/v2_1_1.1/tax | tax | Polish API (v2_1_1_1) | No |
| POST | /periodic-payments/PAYMENT_PRODUCT | initiatePeriodicPayments | Berlin Group PSD2 (v1_3) | No |
| POST | /regulated-entities | createRegulatedEntity | v5_1_0 | Yes |
| POST | /regulated-entities/REGULATED_ENTITY_ID/attributes | createRegulatedEntityAttribute | v5_1_0 | Yes |
| POST | /search/warehouse/INDEX | dataWarehouseSearch | v3_0_0 | Yes |
| POST | /search/warehouse/statistics/INDEX/FIELD | dataWarehouseStatistics | v3_0_0 | Yes |
| POST | /signing-baskets | createSigningBasket | Berlin Group PSD2 (v1_3) | No |
| POST | /signing-baskets/BASKETID/authorisations | startSigningBasketAuthorisation | Berlin Group PSD2 (v1_3) | No |
| POST | /system-views | createSystemView | v3_1_0 | Yes |
| POST | /system-views | createSystemView | v5_0_0 | Yes |
| POST | /system-views/VIEW_ID/permissions | addSystemViewPermission | v5_1_0 | Yes |
| POST | /transaction-request-types/CARD/transaction-requests | createTransactionRequestCard | v4_0_0 | No |
| POST | /user-entitlements | createUserWithRoles | v4_0_0 | No |
| POST | /users/PROVIDER/PROVIDER_ID/sync | syncExternalUser | v5_1_0 | Yes |
| POST | /users/PROVIDER/USERNAME/locks | lockUserByProviderAndUsername | v5_1_0 | Yes |
| POST | /users/USERNAME/locks | lockUser | v4_0_0 | Yes |
| POST | /users/USER_ID/auth-context | createUserAuthContext | v3_1_0 | Yes |
| POST | /users/USER_ID/auth-context | createUserAuthContext | v5_0_0 | Yes |
| POST | /users/USER_ID/non-personal/attributes | createNonPersonalUserAttribute | v5_1_0 | Yes |
| POST | /users/USER_ID/refresh | refreshUser | v3_1_0 | Yes |
| POST | /web-hooks/account/notifications/on-create-transaction | createSystemAccountNotificationWebhook | v4_0_0 | Yes |
| PUT | /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENT_ID/authorisations/AUTHORISATION_ID | updatePaymentPsuDataTransactionAuthorisation | Berlin Group PSD2 (v1_3) | No |
| PUT | /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENT_ID/cancellation-authorisations/AUTHORISATION_ID | updatePaymentCancellationPsuDataTransactionAuthorisation | Berlin Group PSD2 (v1_3) | No |
| PUT | /banks | updateBank | v5_0_0 | Yes |
| PUT | /banks/BANK_ID/account-applications/ACCOUNT_APPLICATION_ID | updateAccountApplicationStatus | v3_1_0 | No |
| PUT | /banks/BANK_ID/account-web-hooks | enableDisableAccountWebhook | v3_1_0 | Yes |
| PUT | /banks/BANK_ID/accounts/ACCOUNT_ID | createAccount | v2_2_0 | Yes |
| PUT | /banks/BANK_ID/accounts/ACCOUNT_ID | createAccount | v3_1_0 | Yes |
| PUT | /banks/BANK_ID/accounts/ACCOUNT_ID | createAccount | v5_0_0 | Yes |
| PUT | /banks/BANK_ID/accounts/ACCOUNT_ID/balances/BALANCE_ID | updateBankAccountBalance | v5_1_0 | Yes |
| PUT | /banks/BANK_ID/accounts/ACCOUNT_ID/products/PRODUCT_CODE/attributes/ACCOUNT_ATTRIBUTE_ID | updateAccountAttribute | v3_1_0 | Yes |
| PUT | /banks/BANK_ID/accounts/ACCOUNT_ID/transaction-requests/TRANSACTION_REQUEST_ID/attributes/ATTRIBUTE_ID | updateTransactionRequestAttribute | v4_0_0 | Yes |
| PUT | /banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID/attributes/ACCOUNT_ATTRIBUTE_ID | updateTransactionAttribute | v4_0_0 | Yes |
| PUT | /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID | updateViewForBankAccount | v2_2_0 | No |
| PUT | /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID | updateViewForBankAccount | v3_0_0 | No |
| PUT | /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/counterparties/COUNTERPARTY_ID/limits | updateCounterpartyLimit | v5_1_0 | No |
| PUT | /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/target-views/TARGET_VIEW_ID | updateCustomView | v5_1_0 | No |
| PUT | /banks/BANK_ID/agents/AGENT_ID | updateAgentStatus | v5_1_0 | No |
| PUT | /banks/BANK_ID/atms/ATM_ID | updateAtm | v4_0_0 | Yes |
| PUT | /banks/BANK_ID/atms/ATM_ID | updateAtm | v5_1_0 | Yes |
| PUT | /banks/BANK_ID/atms/ATM_ID/accessibility-features | updateAtmAccessibilityFeatures | v4_0_0 | No |
| PUT | /banks/BANK_ID/atms/ATM_ID/attributes/ATM_ATTRIBUTE_ID | updateAtmAttribute | v5_1_0 | Yes |
| PUT | /banks/BANK_ID/atms/ATM_ID/location-categories | updateAtmLocationCategories | v4_0_0 | No |
| PUT | /banks/BANK_ID/atms/ATM_ID/notes | updateAtmNotes | v4_0_0 | No |
| PUT | /banks/BANK_ID/atms/ATM_ID/services | updateAtmServices | v4_0_0 | No |
| PUT | /banks/BANK_ID/atms/ATM_ID/supported-currencies | updateAtmSupportedCurrencies | v4_0_0 | No |
| PUT | /banks/BANK_ID/atms/ATM_ID/supported-languages | updateAtmSupportedLanguages | v4_0_0 | No |
| PUT | /banks/BANK_ID/attribute-definitions/account | createOrUpdateAccountAttributeDefinition | v4_0_0 | Yes |
| PUT | /banks/BANK_ID/attribute-definitions/bank | createOrUpdateBankAttributeDefinition | v4_0_0 | Yes |
| PUT | /banks/BANK_ID/attribute-definitions/card | createOrUpdateCardAttributeDefinition | v4_0_0 | Yes |
| PUT | /banks/BANK_ID/attribute-definitions/customer | createOrUpdateCustomerAttributeAttributeDefinition | v4_0_0 | Yes |
| PUT | /banks/BANK_ID/attribute-definitions/product | createOrUpdateProductAttributeDefinition | v4_0_0 | Yes |
| PUT | /banks/BANK_ID/attribute-definitions/transaction | createOrUpdateTransactionAttributeDefinition | v4_0_0 | Yes |
| PUT | /banks/BANK_ID/attribute-definitions/transaction-request | createOrUpdateTransactionRequestAttributeDefinition | v4_0_0 | Yes |
| PUT | /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID | updateBankAttribute | v4_0_0 | No |
| PUT | /banks/BANK_ID/branches/BRANCH_ID | updateBranch | v3_0_0 | Yes |
| PUT | /banks/BANK_ID/consents/CONSENT_ID | updateConsentStatus | v4_0_0 | No |
| PUT | /banks/BANK_ID/consents/CONSENT_ID/user-update-request | addConsentUser | v4_0_0 | No |
| PUT | /banks/BANK_ID/customer-account-links/CUSTOMER_ACCOUNT_LINK_ID | updateCustomerAccountLinkById | v5_0_0 | Yes |
| PUT | /banks/BANK_ID/customers/CUSTOMER_ID/addresses/CUSTOMER_ADDRESS_ID | updateCustomerAddress | v3_1_0 | No |
| PUT | /banks/BANK_ID/customers/CUSTOMER_ID/attributes/CUSTOMER_ATTRIBUTE_ID | updateCustomerAttribute | v4_0_0 | Yes |
| PUT | /banks/BANK_ID/customers/CUSTOMER_ID/branch | updateCustomerBranch | v3_1_0 | No |
| PUT | /banks/BANK_ID/customers/CUSTOMER_ID/credit-limit | updateCustomerCreditLimit | v3_1_0 | No |
| PUT | /banks/BANK_ID/customers/CUSTOMER_ID/credit-rating-and-source | updateCustomerCreditRatingAndSource | v3_1_0 | No |
| PUT | /banks/BANK_ID/customers/CUSTOMER_ID/data | updateCustomerData | v3_1_0 | No |
| PUT | /banks/BANK_ID/customers/CUSTOMER_ID/email | updateCustomerEmail | v3_1_0 | No |
| PUT | /banks/BANK_ID/customers/CUSTOMER_ID/identity | updateCustomerIdentity | v3_1_0 | No |
| PUT | /banks/BANK_ID/customers/CUSTOMER_ID/mobile-number | updateCustomerMobileNumber | v3_1_0 | No |
| PUT | /banks/BANK_ID/customers/CUSTOMER_ID/number | updateCustomerNumber | v3_1_0 | No |
| PUT | /banks/BANK_ID/fx | createFx | v2_2_0 | Yes |
| PUT | /banks/BANK_ID/product-collections/COLLECTION_CODE | createProductCollection | v3_1_0 | Yes |
| PUT | /banks/BANK_ID/products | createProduct | v2_2_0 | Yes |
| PUT | /banks/BANK_ID/products/PRODUCT_CODE | createProduct | v3_1_0 | Yes |
| PUT | /banks/BANK_ID/products/PRODUCT_CODE | createProduct | v4_0_0 | Yes |
| PUT | /banks/BANK_ID/products/PRODUCT_CODE | createProduct | v5_0_0 | Yes |
| PUT | /banks/BANK_ID/products/PRODUCT_CODE/attributes/PRODUCT_ATTRIBUTE_ID | updateProductAttribute | v3_1_0 | Yes |
| PUT | /banks/BANK_ID/products/PRODUCT_CODE/attributes/PRODUCT_ATTRIBUTE_ID | updateProductAttribute | v4_0_0 | Yes |
| PUT | /banks/BANK_ID/products/PRODUCT_CODE/fees/PRODUCT_FEE_ID | updateProductFee | v4_0_0 | Yes |
| PUT | /consents | consentsPut | STET (French) (v1_4) | No |
| PUT | /consents/CONSENTID/authorisations/AUTHORISATIONID | updateConsentsPsuDataTransactionAuthorisation | Berlin Group PSD2 (v1_3) | No |
| PUT | /management/authentication-type-validations/OPERATION_ID | updateAuthenticationTypeValidation | v4_0_0 | Yes |
| PUT | /management/banks/BANK_ID/accounts/ACCOUNT_ID | updateAccount | v3_1_0 | Yes |
| PUT | /management/banks/BANK_ID/cards/CARD_ID | updatedCardForBank | v3_1_0 | Yes |
| PUT | /management/banks/BANK_ID/cards/CARD_ID/attributes/CARD_ATTRIBUTE_ID | updateCardAttribute | v3_1_0 | No |
| PUT | /management/banks/BANK_ID/consents/CONSENT_ID | updateConsentStatusByConsent | v5_1_0 | No |
| PUT | /management/banks/BANK_ID/consents/CONSENT_ID/account-access | updateConsentAccountAccessByConsentId | v5_1_0 | No |
| PUT | /management/banks/BANK_ID/consents/CONSENT_ID/created-by-user | updateConsentUserIdByConsentId | v5_1_0 | No |
| PUT | /management/banks/BANK_ID/dynamic-endpoints/DYNAMIC_ENDPOINT_ID/host | updateBankLevelDynamicEndpointHost | v4_0_0 | Yes |
| PUT | /management/banks/BANK_ID/dynamic-entities/DYNAMIC_ENTITY_ID | updateBankLevelDynamicEntity | v4_0_0 | Yes |
| PUT | /management/banks/BANK_ID/dynamic-message-docs/DYNAMIC_MESSAGE_DOC_ID | updateBankLevelDynamicMessageDoc | v4_0_0 | Yes |
| PUT | /management/banks/BANK_ID/dynamic-resource-docs/DYNAMIC-RESOURCE-DOC-ID | updateBankLevelDynamicResourceDoc | v4_0_0 | Yes |
| PUT | /management/banks/BANK_ID/endpoint-mappings/ENDPOINT_MAPPING_ID | updateBankLevelEndpointMapping | v4_0_0 | Yes |
| PUT | /management/banks/BANK_ID/endpoints/OPERATION_ID/tags/ENDPOINT_TAG_ID | updateBankLevelEndpointTag | v4_0_0 | Yes |
| PUT | /management/connector-methods/CONNECTOR_METHOD_ID | updateConnectorMethod | v4_0_0 | Yes |
| PUT | /management/consumers/CONSUMER_ID | enableDisableConsumers | v3_1_0 | Yes |
| PUT | /management/consumers/CONSUMER_ID/consumer/call-limits | callsLimit | v3_1_0 | Yes |
| PUT | /management/consumers/CONSUMER_ID/consumer/call-limits | callsLimit | v4_0_0 | Yes |
| PUT | /management/consumers/CONSUMER_ID/consumer/certificate | updateConsumerCertificate | v5_1_0 | Yes |
| PUT | /management/consumers/CONSUMER_ID/consumer/logo_url | updateConsumerLogoURL | v5_1_0 | Yes |
| PUT | /management/consumers/CONSUMER_ID/consumer/name | updateConsumerName | v5_1_0 | Yes |
| PUT | /management/consumers/CONSUMER_ID/consumer/redirect_url | updateConsumerRedirectURL | v5_1_0 | Yes |
| PUT | /management/dynamic-endpoints/DYNAMIC_ENDPOINT_ID/host | updateDynamicEndpointHost | v4_0_0 | Yes |
| PUT | /management/dynamic-message-docs/DYNAMIC_MESSAGE_DOC_ID | updateDynamicMessageDoc | v4_0_0 | Yes |
| PUT | /management/dynamic-resource-docs/DYNAMIC-RESOURCE-DOC-ID | updateDynamicResourceDoc | v4_0_0 | Yes |
| PUT | /management/endpoint-mappings/ENDPOINT_MAPPING_ID | updateEndpointMapping | v4_0_0 | Yes |
| PUT | /management/endpoints/OPERATION_ID/tags/ENDPOINT_TAG_ID | updateSystemLevelEndpointTag | v4_0_0 | Yes |
| PUT | /management/json-schema-validations/OPERATION_ID | updateJsonSchemaValidation | v4_0_0 | Yes |
| PUT | /management/method_routings/METHOD_ROUTING_ID | updateMethodRouting | v3_1_0 | No |
| PUT | /management/system-dynamic-entities/DYNAMIC_ENTITY_ID | updateSystemDynamicEntity | v4_0_0 | Yes |
| PUT | /management/transaction-requests/TRANSACTION_REQUEST_ID | updateTransactionRequestStatus | v5_1_0 | Yes |
| PUT | /management/users/USER_ID | validateUserByUserId | v5_1_0 | Yes |
| PUT | /my/api-collections/API_COLLECTION_ID | updateMyApiCollection | v5_1_0 | No |
| PUT | /my/dynamic-entities/DYNAMIC_ENTITY_ID | updateMyDynamicEntity | v4_0_0 | No |
| PUT | /my/user/attributes/USER_ATTRIBUTE_ID | updateMyPersonalUserAttribute | v4_0_0 | No |
| PUT | /payment-requests/PAYMENTREQUESTRESOURCEID | paymentRequestPut | STET (French) (v1_4) | No |
| PUT | /regulated-entities/REGULATED_ENTITY_ID/attributes/REGULATED_ENTITY_ATTRIBUTE_ID | updateRegulatedEntityAttribute | v5_1_0 | Yes |
| PUT | /signing-baskets/BASKETID/authorisations/AUTHORISATIONID | updateSigningBasketPsuData | Berlin Group PSD2 (v1_3) | No |
| PUT | /system-views/VIEW_ID | updateSystemView | v3_1_0 | Yes |
| PUT | /system-views/VIEW_ID | updateSystemView | v5_0_0 | Yes |
| PUT | /users/PROVIDER/USERNAME/lock-status | unlockUserByProviderAndUsername | v5_1_0 | Yes |
| PUT | /users/USERNAME/lock-status | unlockUser | v3_1_0 | Yes |
