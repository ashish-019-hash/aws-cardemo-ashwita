package validators

import (
	"errors"
	"regexp"
	"strings"

	"github.com/obp-api/bank-registration/internal/models"
)

// ValidationError codes as defined in validation_rules.md
const (
	ErrCodeBankIDRequired         = "BANK-VAL-001"
	ErrCodeBankCodeRequired       = "BANK-VAL-002"
	ErrCodeBankNameRequired       = "BANK-VAL-003"
	ErrCodeBrandingRequired       = "BANK-VAL-004"
	ErrCodeOperationalParamsReq   = "BANK-VAL-005"
	ErrCodeBankIDPathRequired     = "BANK-VAL-006"
	ErrCodeBankIDUnique           = "BANK-VAL-007"
	ErrCodeBankCodeUnique         = "BANK-VAL-008"
	ErrCodeBankNotFound           = "BANK-VAL-009"
	ErrCodeBankIDFormat           = "BANK-VAL-010"
	ErrCodeBankCodeFormat         = "BANK-VAL-011"
	ErrCodeCurrencyFormat         = "BANK-VAL-012"
	ErrCodeBankNameLength         = "BANK-VAL-013"
	ErrCodeCurrenciesArraySize    = "BANK-VAL-014"
	ErrCodeUpdateFieldRequired    = "BANK-VAL-015"
)

// BankValidator handles all validation rules for Bank entities
type BankValidator struct{}

// NewBankValidator creates a new BankValidator instance
func NewBankValidator() *BankValidator {
	return &BankValidator{}
}

// ValidationResult represents the result of a validation
type ValidationResult struct {
	Valid   bool
	Code    string
	Message string
	Field   string
}

// ValidateCreateBankRequest validates a CreateBankRequest
// Implements: VR-001 to VR-005, VR-010 to VR-014
func (v *BankValidator) ValidateCreateBankRequest(req *models.CreateBankRequest) []ValidationResult {
	var results []ValidationResult

	// VR-001: Bank ID Required Validation
	if result := v.ValidateBankIDRequired(req.BankID); !result.Valid {
		results = append(results, result)
	}

	// VR-002: Bank Code Required Validation
	if result := v.ValidateBankCodeRequired(req.BankCode); !result.Valid {
		results = append(results, result)
	}

	// VR-003: Bank Name Required Validation
	if result := v.ValidateBankNameRequired(req.BankName); !result.Valid {
		results = append(results, result)
	}

	// VR-004: Branding Information Required Validation
	if result := v.ValidateBrandingRequired(&req.Branding); !result.Valid {
		results = append(results, result)
	}

	// VR-005: Operational Parameters Required Validation
	if result := v.ValidateOperationalParamsRequired(&req.OperationalParams); !result.Valid {
		results = append(results, result)
	}

	// Only proceed with format validations if required fields are present
	if len(results) == 0 {
		// VR-010: Bank ID Format Validation
		if result := v.ValidateBankIDFormat(req.BankID); !result.Valid {
			results = append(results, result)
		}

		// VR-011: Bank Code Format Validation
		if result := v.ValidateBankCodeFormat(req.BankCode); !result.Valid {
			results = append(results, result)
		}

		// VR-013: Bank Name Length Validation
		if result := v.ValidateBankNameLength(req.BankName); !result.Valid {
			results = append(results, result)
		}

		// VR-014: Currencies Array Size Validation
		if result := v.ValidateCurrenciesArraySize(req.OperationalParams.Currencies); !result.Valid {
			results = append(results, result)
		}

		// VR-012: Currency Code Format Validation
		for _, currency := range req.OperationalParams.Currencies {
			if result := v.ValidateCurrencyCodeFormat(currency); !result.Valid {
				results = append(results, result)
				break // Return first error
			}
		}
	}

	return results
}

// ValidateUpdateBankRequest validates an UpdateBankRequest
// Implements: VR-015, VR-010, VR-012, VR-013, VR-014
func (v *BankValidator) ValidateUpdateBankRequest(req *models.UpdateBankRequest) []ValidationResult {
	var results []ValidationResult

	// VR-015: Update Request Field Validation
	if result := v.ValidateUpdateRequestHasFields(req); !result.Valid {
		results = append(results, result)
		return results
	}

	// Validate optional fields if provided
	if req.BankName != nil {
		// VR-013: Bank Name Length Validation
		if result := v.ValidateBankNameLength(*req.BankName); !result.Valid {
			results = append(results, result)
		}
	}

	if req.OperationalParams != nil && req.OperationalParams.Currencies != nil {
		// VR-014: Currencies Array Size Validation
		if result := v.ValidateCurrenciesArraySize(req.OperationalParams.Currencies); !result.Valid {
			results = append(results, result)
		}

		// VR-012: Currency Code Format Validation
		for _, currency := range req.OperationalParams.Currencies {
			if result := v.ValidateCurrencyCodeFormat(currency); !result.Valid {
				results = append(results, result)
				break
			}
		}
	}

	return results
}

// VR-001: Bank ID Required Validation
func (v *BankValidator) ValidateBankIDRequired(bankID string) ValidationResult {
	if bankID == "" {
		return ValidationResult{
			Valid:   false,
			Code:    ErrCodeBankIDRequired,
			Message: "Bank ID is required and cannot be empty",
			Field:   "bankId",
		}
	}
	return ValidationResult{Valid: true}
}

// VR-002: Bank Code Required Validation
func (v *BankValidator) ValidateBankCodeRequired(bankCode string) ValidationResult {
	if bankCode == "" {
		return ValidationResult{
			Valid:   false,
			Code:    ErrCodeBankCodeRequired,
			Message: "Bank Code is required and cannot be empty",
			Field:   "bankCode",
		}
	}
	return ValidationResult{Valid: true}
}

// VR-003: Bank Name Required Validation
func (v *BankValidator) ValidateBankNameRequired(bankName string) ValidationResult {
	if bankName == "" {
		return ValidationResult{
			Valid:   false,
			Code:    ErrCodeBankNameRequired,
			Message: "Bank Name is required and cannot be empty",
			Field:   "bankName",
		}
	}
	return ValidationResult{Valid: true}
}

// VR-004: Branding Information Required Validation
func (v *BankValidator) ValidateBrandingRequired(branding *models.Branding) ValidationResult {
	if branding == nil || branding.Logo == "" || branding.Colors == "" {
		return ValidationResult{
			Valid:   false,
			Code:    ErrCodeBrandingRequired,
			Message: "Branding information (logo and colors) is required",
			Field:   "branding",
		}
	}
	return ValidationResult{Valid: true}
}

// VR-005: Operational Parameters Required Validation
func (v *BankValidator) ValidateOperationalParamsRequired(params *models.OperationalParams) ValidationResult {
	if params == nil || params.BusinessHours == "" || params.Limits == nil || len(params.Currencies) == 0 {
		return ValidationResult{
			Valid:   false,
			Code:    ErrCodeOperationalParamsReq,
			Message: "Operational parameters (businessHours, limits, currencies) are required",
			Field:   "operationalParams",
		}
	}
	return ValidationResult{Valid: true}
}

// VR-006: Bank ID Path Parameter Required for Update
func (v *BankValidator) ValidateBankIDPathRequired(bankID string) ValidationResult {
	if bankID == "" {
		return ValidationResult{
			Valid:   false,
			Code:    ErrCodeBankIDPathRequired,
			Message: "Bank ID path parameter is required",
			Field:   "bankId",
		}
	}
	return ValidationResult{Valid: true}
}

// VR-010: Bank ID Format Validation
// Bank ID must contain only alphanumeric characters, hyphens, and underscores
// Must be > 3 characters, no spaces, no ::::
func (v *BankValidator) ValidateBankIDFormat(bankID string) ValidationResult {
	// Check minimum length (must be > 3 characters)
	if len(bankID) <= 3 {
		return ValidationResult{
			Valid:   false,
			Code:    ErrCodeBankIDFormat,
			Message: "Bank ID must be greater than 3 characters",
			Field:   "bankId",
		}
	}

	// Check for spaces
	if strings.Contains(bankID, " ") {
		return ValidationResult{
			Valid:   false,
			Code:    ErrCodeBankIDFormat,
			Message: "Bank ID cannot contain space characters",
			Field:   "bankId",
		}
	}

	// Check for ::::
	if strings.Contains(bankID, "::::") {
		return ValidationResult{
			Valid:   false,
			Code:    ErrCodeBankIDFormat,
			Message: "Bank ID cannot contain '::::' characters",
			Field:   "bankId",
		}
	}

	// Check format: only alphanumeric, hyphens, and underscores
	regex := regexp.MustCompile(`^[A-Za-z0-9_-]+$`)
	if !regex.MatchString(bankID) {
		return ValidationResult{
			Valid:   false,
			Code:    ErrCodeBankIDFormat,
			Message: "Bank ID must contain only alphanumeric characters, hyphens, and underscores",
			Field:   "bankId",
		}
	}

	return ValidationResult{Valid: true}
}

// VR-011: Bank Code Format Validation
// Bank Code must be uppercase alphanumeric characters only
func (v *BankValidator) ValidateBankCodeFormat(bankCode string) ValidationResult {
	regex := regexp.MustCompile(`^[A-Z0-9]+$`)
	if !regex.MatchString(bankCode) {
		return ValidationResult{
			Valid:   false,
			Code:    ErrCodeBankCodeFormat,
			Message: "Bank Code must contain only uppercase alphanumeric characters",
			Field:   "bankCode",
		}
	}
	return ValidationResult{Valid: true}
}

// VR-012: Currency Code Format Validation
// Currency codes must be valid 3-letter ISO 4217 currency codes
func (v *BankValidator) ValidateCurrencyCodeFormat(currencyCode string) ValidationResult {
	// Check format: exactly 3 uppercase letters
	regex := regexp.MustCompile(`^[A-Z]{3}$`)
	if !regex.MatchString(currencyCode) {
		return ValidationResult{
			Valid:   false,
			Code:    ErrCodeCurrencyFormat,
			Message: "Invalid currency code '" + currencyCode + "'. Must be a valid 3-letter ISO 4217 currency code",
			Field:   "operationalParams.currencies",
		}
	}

	// Validate against known ISO 4217 currency codes
	validCurrencies := map[string]bool{
		"USD": true, "EUR": true, "GBP": true, "JPY": true, "CHF": true,
		"CAD": true, "AUD": true, "INR": true, "CNY": true, "HKD": true,
		"SGD": true, "NZD": true, "KRW": true, "MXN": true, "BRL": true,
		"ZAR": true, "RUB": true, "SEK": true, "NOK": true, "DKK": true,
		"PLN": true, "THB": true, "MYR": true, "IDR": true, "PHP": true,
		"TRY": true, "AED": true, "SAR": true, "ILS": true, "EGP": true,
	}

	if !validCurrencies[currencyCode] {
		return ValidationResult{
			Valid:   false,
			Code:    ErrCodeCurrencyFormat,
			Message: "Invalid ISO 4217 currency code: " + currencyCode,
			Field:   "operationalParams.currencies",
		}
	}

	return ValidationResult{Valid: true}
}

// VR-013: Bank Name Length Validation
// Bank Name must be between 1 and 255 characters
func (v *BankValidator) ValidateBankNameLength(bankName string) ValidationResult {
	if len(bankName) < 1 || len(bankName) > 255 {
		return ValidationResult{
			Valid:   false,
			Code:    ErrCodeBankNameLength,
			Message: "Bank Name must be between 1 and 255 characters",
			Field:   "bankName",
		}
	}
	return ValidationResult{Valid: true}
}

// VR-014: Currencies Array Size Validation
// Currencies array must contain between 1 and 50 currency codes
func (v *BankValidator) ValidateCurrenciesArraySize(currencies []string) ValidationResult {
	if len(currencies) < 1 {
		return ValidationResult{
			Valid:   false,
			Code:    ErrCodeCurrenciesArraySize,
			Message: "At least one currency code is required",
			Field:   "operationalParams.currencies",
		}
	}
	if len(currencies) > 50 {
		return ValidationResult{
			Valid:   false,
			Code:    ErrCodeCurrenciesArraySize,
			Message: "Maximum 50 currency codes allowed",
			Field:   "operationalParams.currencies",
		}
	}
	return ValidationResult{Valid: true}
}

// VR-015: Update Request Field Validation
// At least one field must be provided for update
func (v *BankValidator) ValidateUpdateRequestHasFields(req *models.UpdateBankRequest) ValidationResult {
	if req.BankName == nil && req.Branding == nil && req.OperationalParams == nil {
		return ValidationResult{
			Valid:   false,
			Code:    ErrCodeUpdateFieldRequired,
			Message: "At least one field must be provided for update",
			Field:   "",
		}
	}
	return ValidationResult{Valid: true}
}

// NewBankNotFoundError creates a bank not found validation result
// VR-009: Bank Existence Validation for Update
func NewBankNotFoundError(bankID string) ValidationResult {
	return ValidationResult{
		Valid:   false,
		Code:    ErrCodeBankNotFound,
		Message: "Bank with ID '" + bankID + "' not found",
		Field:   "bankId",
	}
}

// NewBankIDUniqueError creates a bank ID uniqueness validation result
// VR-007: Bank ID Uniqueness Validation
func NewBankIDUniqueError(bankID string) ValidationResult {
	return ValidationResult{
		Valid:   false,
		Code:    ErrCodeBankIDUnique,
		Message: "Bank with ID '" + bankID + "' already exists",
		Field:   "bankId",
	}
}

// NewBankCodeUniqueError creates a bank code uniqueness validation result
// VR-008: Bank Code Uniqueness Validation
func NewBankCodeUniqueError(bankCode string) ValidationResult {
	return ValidationResult{
		Valid:   false,
		Code:    ErrCodeBankCodeUnique,
		Message: "Bank with Code '" + bankCode + "' already exists",
		Field:   "bankCode",
	}
}

// ToError converts a ValidationResult to an error
func (r ValidationResult) ToError() error {
	if r.Valid {
		return nil
	}
	return errors.New(r.Message)
}
