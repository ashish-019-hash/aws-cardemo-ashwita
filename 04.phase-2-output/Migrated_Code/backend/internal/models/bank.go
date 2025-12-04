package models

import "time"

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
