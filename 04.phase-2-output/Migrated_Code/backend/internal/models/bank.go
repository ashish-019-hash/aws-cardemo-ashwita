package models

import (
	"time"

	"github.com/google/uuid"
)

// MappedBank represents a Bank entity in the OBP system
// Source: code/model/dataAccess/MappedBank.scala
// User Story: Bank Registration and Configuration
type MappedBank struct {
	// Internal database identifier (auto-generated)
	ID int64 `json:"-" db:"id"`

	// Bank ID used in URLs - maps to user story "bankId"
	Permalink string `json:"id" db:"permalink" validate:"required,min=4,max=255"`

	// Full name of the bank - maps to user story "bankName"
	FullBankName string `json:"full_name" db:"fullbankname" validate:"required,max=255"`

	// Short name/code of the bank - maps to user story "bankCode"
	ShortBankName string `json:"short_name" db:"shortbankname" validate:"required,max=100"`

	// URL to bank logo - maps to user story "branding.logo"
	LogoURL string `json:"logo" db:"logourl" validate:"max=255"`

	// Bank website URL - maps to user story "branding"
	WebsiteURL string `json:"website" db:"websiteurl" validate:"max=255"`

	// SWIFT/BIC code - maps to user story "operationalParams"
	SwiftBIC string `json:"swift_bic,omitempty" db:"swiftbic" validate:"max=255"`

	// National bank identifier - maps to user story "operationalParams"
	NationalIdentifier string `json:"national_identifier,omitempty" db:"national_identifier" validate:"max=255"`

	// Bank routing scheme - maps to user story "operationalParams"
	BankRoutingScheme string `json:"bank_routing_scheme,omitempty" db:"mbankroutingscheme" validate:"max=255"`

	// Bank routing address - maps to user story "operationalParams"
	BankRoutingAddress string `json:"bank_routing_address,omitempty" db:"mbankroutingaddress" validate:"max=255"`

	// Timestamps from CreatedUpdated trait
	CreatedAt time.Time `json:"created_at,omitempty" db:"createdat"`
	UpdatedAt time.Time `json:"updated_at,omitempty" db:"updatedat"`
}

// BankId is a value type for bank identifiers
// Source: com/openbankproject/commons/model/BankingModel.scala:153
type BankId struct {
	Value string `json:"value"`
}

// Bank interface matching the Scala Bank trait
// Source: com/openbankproject/commons/model/BankingModel.scala:36
type Bank interface {
	GetBankId() BankId
	GetFullName() string
	GetShortName() string
	GetLogoUrl() string
	GetWebsiteUrl() string
	GetSwiftBic() string
	GetNationalIdentifier() string
	GetBankRoutingScheme() string
	GetBankRoutingAddress() string
}

// Implement Bank interface for MappedBank
func (b *MappedBank) GetBankId() BankId           { return BankId{Value: b.Permalink} }
func (b *MappedBank) GetFullName() string         { return b.FullBankName }
func (b *MappedBank) GetShortName() string        { return b.ShortBankName }
func (b *MappedBank) GetLogoUrl() string          { return b.LogoURL }
func (b *MappedBank) GetWebsiteUrl() string       { return b.WebsiteURL }
func (b *MappedBank) GetSwiftBic() string         { return b.SwiftBIC }
func (b *MappedBank) GetNationalIdentifier() string { return b.NationalIdentifier }
func (b *MappedBank) GetBankRoutingScheme() string  { return b.BankRoutingScheme }
func (b *MappedBank) GetBankRoutingAddress() string { return b.BankRoutingAddress }

// NewMappedBank creates a new MappedBank with default values
func NewMappedBank() *MappedBank {
	now := time.Now()
	return &MappedBank{
		CreatedAt: now,
		UpdatedAt: now,
	}
}

// BankAttribute represents configurable attributes/parameters associated with a bank
// Source: code/bankattribute/MappedBankAttributeProvider.scala
// User Story: Bank Information Retrieval
// Database Class: BankAttribute (extends LongKeyedMapper[BankAttribute] with IdPK)
type BankAttribute struct {
	// Internal database identifier (auto-generated)
	ID int64 `json:"-" db:"id"`

	// Foreign key reference to the parent MappedBank (permalink)
	BankID string `json:"bank_id" db:"bankid" validate:"required"`

	// Unique identifier for the attribute (auto-generated UUID)
	BankAttributeID string `json:"bank_attribute_id,omitempty" db:"bankattributeid"`

	// Name/key of the attribute
	Name string `json:"name" db:"name" validate:"required,max=50"`

	// Type classification of the attribute value (BankAttributeType enum)
	Type string `json:"type" db:"type" validate:"required,max=50"`

	// The actual value of the attribute
	Value string `json:"value" db:"value" validate:"required,max=255"`

	// Flag indicating if the attribute is currently active (default: true)
	IsActive bool `json:"is_active" db:"isactive"`
}

// BankAttributeTrait interface matching the Scala BankAttributeTrait
// Source: com/openbankproject/commons/model/CommonModelTrait.scala
type BankAttributeTrait interface {
	GetBankID() string
	GetName() string
	GetType() string
	GetValue() string
	GetIsActive() bool
}

// Implement BankAttributeTrait interface for BankAttribute
func (a *BankAttribute) GetBankID() string   { return a.BankID }
func (a *BankAttribute) GetName() string     { return a.Name }
func (a *BankAttribute) GetType() string     { return a.Type }
func (a *BankAttribute) GetValue() string    { return a.Value }
func (a *BankAttribute) GetIsActive() bool   { return a.IsActive }

// NewBankAttribute creates a new BankAttribute with default values
func NewBankAttribute(bankID, name, attrType, value string) *BankAttribute {
	return &BankAttribute{
		BankID:          bankID,
		BankAttributeID: uuid.New().String(),
		Name:            name,
		Type:            attrType,
		Value:           value,
		IsActive:        true, // Default to true as per user story
	}
}

// ============================================================================
// Multi-Bank Support Entities
// User Story: Multi-Bank Support
// ============================================================================

// MappedBankAccount represents a bank account entity
// Source: code/model/dataAccess/MappedBankAccount.scala
// User Story: Multi-Bank Support - Bank-scoped account access
// Database Class: MappedBankAccount (extends LongKeyedMapper[MappedBankAccount] with IdPK with CreatedUpdated)
type MappedBankAccount struct {
	// Internal database identifier (auto-generated)
	ID int64 `json:"-" db:"id"`

	// Foreign key reference to MappedBank.permalink - enforces bank isolation
	// BR-003: Data Isolation Enforcement - accounts are scoped to specific bank
	BankID string `json:"bank_id" db:"bank" validate:"required"`

	// Unique account identifier within the bank
	AccountID string `json:"account_id" db:"theaccountid" validate:"required,max=255"`

	// Currency code for the account (e.g., USD, EUR)
	Currency string `json:"currency" db:"accountcurrency" validate:"required,max=255"`

	// Current balance of the account
	Balance float64 `json:"balance" db:"accountbalance"`

	// Display label for the account
	Label string `json:"label,omitempty" db:"accountlabel" validate:"max=255"`

	// Account type classification (e.g., CURRENT, SAVINGS)
	Kind string `json:"type" db:"kind" validate:"required,max=255"`

	// Timestamps from CreatedUpdated trait
	CreatedAt time.Time `json:"created_at,omitempty" db:"createdat"`
	UpdatedAt time.Time `json:"updated_at,omitempty" db:"updatedat"`
}

// BankAccount interface matching the Scala BankAccount trait
// Source: com/openbankproject/commons/model/BankingModel.scala
type BankAccount interface {
	GetBankID() string
	GetAccountID() string
	GetCurrency() string
	GetBalance() float64
	GetLabel() string
	GetKind() string
}

// Implement BankAccount interface for MappedBankAccount
func (a *MappedBankAccount) GetBankID() string    { return a.BankID }
func (a *MappedBankAccount) GetAccountID() string { return a.AccountID }
func (a *MappedBankAccount) GetCurrency() string  { return a.Currency }
func (a *MappedBankAccount) GetBalance() float64  { return a.Balance }
func (a *MappedBankAccount) GetLabel() string     { return a.Label }
func (a *MappedBankAccount) GetKind() string      { return a.Kind }

// NewMappedBankAccount creates a new MappedBankAccount with default values
// BR-006: Bank-Scoped Resource Ownership - account is permanently associated with bank
func NewMappedBankAccount(bankID, accountID, currency, kind string) *MappedBankAccount {
	now := time.Now()
	return &MappedBankAccount{
		BankID:    bankID,
		AccountID: uuid.New().String(),
		Currency:  currency,
		Kind:      kind,
		Balance:   0.0,
		CreatedAt: now,
		UpdatedAt: now,
	}
}

// MappedEntitlement represents a user entitlement/permission entity
// Source: code/entitlement/MappedEntitlementsProvider.scala
// User Story: Multi-Bank Support - Bank-scoped entitlements
// Database Class: MappedEntitlement (extends LongKeyedMapper[MappedEntitlement] with IdPK with CreatedUpdated)
// BR-005: Bank-Scoped Entitlements - permissions are scoped to specific banks
type MappedEntitlement struct {
	// Internal database identifier (auto-generated)
	ID int64 `json:"-" db:"id"`

	// Unique identifier for the entitlement
	EntitlementID string `json:"entitlement_id" db:"entitlementid" validate:"required"`

	// Foreign key reference to MappedBank.permalink - scopes entitlement to specific bank
	// BR-005: User permissions are bank-specific
	BankID string `json:"bank_id" db:"mbankid" validate:"required"`

	// User identifier
	UserID string `json:"user_id" db:"muserid" validate:"required,max=255"`

	// Name of the role/permission (e.g., canCreateAccount, canViewTransactions)
	RoleName string `json:"role_name" db:"mrolename" validate:"required,max=255"`

	// Timestamps from CreatedUpdated trait
	CreatedAt time.Time `json:"created_at,omitempty" db:"createdat"`
	UpdatedAt time.Time `json:"updated_at,omitempty" db:"updatedat"`
}

// Entitlement interface matching the Scala Entitlement trait
// Source: code/entitlement/Entitlement.scala
type Entitlement interface {
	GetEntitlementID() string
	GetBankID() string
	GetUserID() string
	GetRoleName() string
}

// Implement Entitlement interface for MappedEntitlement
func (e *MappedEntitlement) GetEntitlementID() string { return e.EntitlementID }
func (e *MappedEntitlement) GetBankID() string        { return e.BankID }
func (e *MappedEntitlement) GetUserID() string        { return e.UserID }
func (e *MappedEntitlement) GetRoleName() string      { return e.RoleName }

// NewMappedEntitlement creates a new MappedEntitlement with default values
// BR-005: Bank-Scoped Entitlements - entitlement is associated with specific bank
func NewMappedEntitlement(bankID, userID, roleName string) *MappedEntitlement {
	now := time.Now()
	return &MappedEntitlement{
		EntitlementID: uuid.New().String(),
		BankID:        bankID,
		UserID:        userID,
		RoleName:      roleName,
		CreatedAt:     now,
		UpdatedAt:     now,
	}
}
