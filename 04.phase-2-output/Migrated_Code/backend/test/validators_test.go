package test

import (
	"testing"

	"github.com/obp-api/bank-registration/internal/models"
	"github.com/obp-api/bank-registration/internal/validators"
	"github.com/stretchr/testify/assert"
)

// TestVR001_BankIDRequired tests VR-001: Bank ID Required Validation
func TestVR001_BankIDRequired(t *testing.T) {
	v := validators.NewBankValidator()

	tests := []struct {
		name    string
		bankID  string
		valid   bool
		errCode string
	}{
		{"valid bank ID", "valid-bank-id", true, ""},
		{"empty bank ID", "", false, validators.ErrCodeBankIDRequired},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := v.ValidateBankIDRequired(tt.bankID)
			assert.Equal(t, tt.valid, result.Valid)
			if !tt.valid {
				assert.Equal(t, tt.errCode, result.Code)
				assert.Equal(t, "bankId", result.Field)
			}
		})
	}
}

// TestVR002_BankCodeRequired tests VR-002: Bank Code Required Validation
func TestVR002_BankCodeRequired(t *testing.T) {
	v := validators.NewBankValidator()

	tests := []struct {
		name     string
		bankCode string
		valid    bool
		errCode  string
	}{
		{"valid bank code", "VALIDCODE", true, ""},
		{"empty bank code", "", false, validators.ErrCodeBankCodeRequired},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := v.ValidateBankCodeRequired(tt.bankCode)
			assert.Equal(t, tt.valid, result.Valid)
			if !tt.valid {
				assert.Equal(t, tt.errCode, result.Code)
				assert.Equal(t, "bankCode", result.Field)
			}
		})
	}
}

// TestVR003_BankNameRequired tests VR-003: Bank Name Required Validation
func TestVR003_BankNameRequired(t *testing.T) {
	v := validators.NewBankValidator()

	tests := []struct {
		name     string
		bankName string
		valid    bool
		errCode  string
	}{
		{"valid bank name", "Test Bank", true, ""},
		{"empty bank name", "", false, validators.ErrCodeBankNameRequired},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := v.ValidateBankNameRequired(tt.bankName)
			assert.Equal(t, tt.valid, result.Valid)
			if !tt.valid {
				assert.Equal(t, tt.errCode, result.Code)
				assert.Equal(t, "bankName", result.Field)
			}
		})
	}
}

// TestVR004_BrandingRequired tests VR-004: Branding Information Required Validation
func TestVR004_BrandingRequired(t *testing.T) {
	v := validators.NewBankValidator()

	tests := []struct {
		name     string
		branding *models.Branding
		valid    bool
		errCode  string
	}{
		{"valid branding", &models.Branding{Logo: "https://logo.url", Colors: "#FFFFFF"}, true, ""},
		{"nil branding", nil, false, validators.ErrCodeBrandingRequired},
		{"empty logo", &models.Branding{Logo: "", Colors: "#FFFFFF"}, false, validators.ErrCodeBrandingRequired},
		{"empty colors", &models.Branding{Logo: "https://logo.url", Colors: ""}, false, validators.ErrCodeBrandingRequired},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := v.ValidateBrandingRequired(tt.branding)
			assert.Equal(t, tt.valid, result.Valid)
			if !tt.valid {
				assert.Equal(t, tt.errCode, result.Code)
				assert.Equal(t, "branding", result.Field)
			}
		})
	}
}

// TestVR005_OperationalParamsRequired tests VR-005: Operational Parameters Required Validation
func TestVR005_OperationalParamsRequired(t *testing.T) {
	v := validators.NewBankValidator()

	tests := []struct {
		name    string
		params  *models.OperationalParams
		valid   bool
		errCode string
	}{
		{
			"valid params",
			&models.OperationalParams{
				BusinessHours: "9-5",
				Limits:        map[string]interface{}{"daily": 10000},
				Currencies:    []string{"USD"},
			},
			true,
			"",
		},
		{"nil params", nil, false, validators.ErrCodeOperationalParamsReq},
		{
			"empty business hours",
			&models.OperationalParams{
				BusinessHours: "",
				Limits:        map[string]interface{}{"daily": 10000},
				Currencies:    []string{"USD"},
			},
			false,
			validators.ErrCodeOperationalParamsReq,
		},
		{
			"nil limits",
			&models.OperationalParams{
				BusinessHours: "9-5",
				Limits:        nil,
				Currencies:    []string{"USD"},
			},
			false,
			validators.ErrCodeOperationalParamsReq,
		},
		{
			"empty currencies",
			&models.OperationalParams{
				BusinessHours: "9-5",
				Limits:        map[string]interface{}{"daily": 10000},
				Currencies:    []string{},
			},
			false,
			validators.ErrCodeOperationalParamsReq,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := v.ValidateOperationalParamsRequired(tt.params)
			assert.Equal(t, tt.valid, result.Valid)
			if !tt.valid {
				assert.Equal(t, tt.errCode, result.Code)
			}
		})
	}
}

// TestVR006_BankIDPathRequired tests VR-006: Bank ID Path Parameter Required for Update
func TestVR006_BankIDPathRequired(t *testing.T) {
	v := validators.NewBankValidator()

	tests := []struct {
		name    string
		bankID  string
		valid   bool
		errCode string
	}{
		{"valid path param", "bank-123", true, ""},
		{"empty path param", "", false, validators.ErrCodeBankIDPathRequired},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := v.ValidateBankIDPathRequired(tt.bankID)
			assert.Equal(t, tt.valid, result.Valid)
			if !tt.valid {
				assert.Equal(t, tt.errCode, result.Code)
			}
		})
	}
}

// TestVR010_BankIDFormat tests VR-010: Bank ID Format Validation
func TestVR010_BankIDFormat(t *testing.T) {
	v := validators.NewBankValidator()

	tests := []struct {
		name    string
		bankID  string
		valid   bool
		errCode string
	}{
		{"valid alphanumeric", "valid-bank-123", true, ""},
		{"valid with underscore", "valid_bank_123", true, ""},
		{"too short (3 chars)", "abc", false, validators.ErrCodeBankIDFormat},
		{"contains space", "bank id", false, validators.ErrCodeBankIDFormat},
		{"contains ::::", "bank::::id", false, validators.ErrCodeBankIDFormat},
		{"contains special chars", "bank@id!", false, validators.ErrCodeBankIDFormat},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := v.ValidateBankIDFormat(tt.bankID)
			assert.Equal(t, tt.valid, result.Valid)
			if !tt.valid {
				assert.Equal(t, tt.errCode, result.Code)
				assert.Equal(t, "bankId", result.Field)
			}
		})
	}
}

// TestVR011_BankCodeFormat tests VR-011: Bank Code Format Validation
func TestVR011_BankCodeFormat(t *testing.T) {
	v := validators.NewBankValidator()

	tests := []struct {
		name     string
		bankCode string
		valid    bool
		errCode  string
	}{
		{"valid uppercase", "BANKCODE", true, ""},
		{"valid with numbers", "BANK123", true, ""},
		{"lowercase letters", "bankcode", false, validators.ErrCodeBankCodeFormat},
		{"mixed case", "BankCode", false, validators.ErrCodeBankCodeFormat},
		{"contains special chars", "BANK-CODE", false, validators.ErrCodeBankCodeFormat},
		{"contains space", "BANK CODE", false, validators.ErrCodeBankCodeFormat},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := v.ValidateBankCodeFormat(tt.bankCode)
			assert.Equal(t, tt.valid, result.Valid)
			if !tt.valid {
				assert.Equal(t, tt.errCode, result.Code)
				assert.Equal(t, "bankCode", result.Field)
			}
		})
	}
}

// TestVR012_CurrencyCodeFormat tests VR-012: Currency Code Format Validation
func TestVR012_CurrencyCodeFormat(t *testing.T) {
	v := validators.NewBankValidator()

	tests := []struct {
		name     string
		currency string
		valid    bool
		errCode  string
	}{
		{"valid USD", "USD", true, ""},
		{"valid EUR", "EUR", true, ""},
		{"valid GBP", "GBP", true, ""},
		{"lowercase", "usd", false, validators.ErrCodeCurrencyFormat},
		{"too short", "US", false, validators.ErrCodeCurrencyFormat},
		{"too long", "USDD", false, validators.ErrCodeCurrencyFormat},
		{"invalid code", "XXX", false, validators.ErrCodeCurrencyFormat},
		{"contains numbers", "US1", false, validators.ErrCodeCurrencyFormat},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := v.ValidateCurrencyCodeFormat(tt.currency)
			assert.Equal(t, tt.valid, result.Valid)
			if !tt.valid {
				assert.Equal(t, tt.errCode, result.Code)
			}
		})
	}
}

// TestVR013_BankNameLength tests VR-013: Bank Name Length Validation
func TestVR013_BankNameLength(t *testing.T) {
	v := validators.NewBankValidator()

	// Create a string longer than 255 characters
	longName := ""
	for i := 0; i < 260; i++ {
		longName += "a"
	}

	tests := []struct {
		name     string
		bankName string
		valid    bool
		errCode  string
	}{
		{"valid name", "Test Bank", true, ""},
		{"minimum length (1 char)", "A", true, ""},
		{"maximum length (255 chars)", string(make([]byte, 255)), true, ""},
		{"empty name", "", false, validators.ErrCodeBankNameLength},
		{"too long (>255 chars)", longName, false, validators.ErrCodeBankNameLength},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := v.ValidateBankNameLength(tt.bankName)
			assert.Equal(t, tt.valid, result.Valid)
			if !tt.valid {
				assert.Equal(t, tt.errCode, result.Code)
			}
		})
	}
}

// TestVR014_CurrenciesArraySize tests VR-014: Currencies Array Size Validation
func TestVR014_CurrenciesArraySize(t *testing.T) {
	v := validators.NewBankValidator()

	// Create array with 51 currencies
	tooManyCurrencies := make([]string, 51)
	for i := 0; i < 51; i++ {
		tooManyCurrencies[i] = "USD"
	}

	tests := []struct {
		name       string
		currencies []string
		valid      bool
		errCode    string
	}{
		{"valid single currency", []string{"USD"}, true, ""},
		{"valid multiple currencies", []string{"USD", "EUR", "GBP"}, true, ""},
		{"valid max currencies (50)", make([]string, 50), true, ""},
		{"empty array", []string{}, false, validators.ErrCodeCurrenciesArraySize},
		{"too many currencies (>50)", tooManyCurrencies, false, validators.ErrCodeCurrenciesArraySize},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := v.ValidateCurrenciesArraySize(tt.currencies)
			assert.Equal(t, tt.valid, result.Valid)
			if !tt.valid {
				assert.Equal(t, tt.errCode, result.Code)
			}
		})
	}
}

// TestVR015_UpdateRequestFieldValidation tests VR-015: Update Request Field Validation
func TestVR015_UpdateRequestFieldValidation(t *testing.T) {
	v := validators.NewBankValidator()

	bankName := "Updated Name"
	branding := &models.Branding{Logo: "https://logo.url", Colors: "#FFFFFF"}

	tests := []struct {
		name    string
		request *models.UpdateBankRequest
		valid   bool
		errCode string
	}{
		{
			"valid with bank name",
			&models.UpdateBankRequest{BankName: &bankName},
			true,
			"",
		},
		{
			"valid with branding",
			&models.UpdateBankRequest{Branding: branding},
			true,
			"",
		},
		{
			"invalid - all fields nil",
			&models.UpdateBankRequest{},
			false,
			validators.ErrCodeUpdateFieldRequired,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := v.ValidateUpdateRequestHasFields(tt.request)
			assert.Equal(t, tt.valid, result.Valid)
			if !tt.valid {
				assert.Equal(t, tt.errCode, result.Code)
			}
		})
	}
}

// TestValidateCreateBankRequest_Full tests full CreateBankRequest validation
func TestValidateCreateBankRequest_Full(t *testing.T) {
	v := validators.NewBankValidator()

	tests := []struct {
		name       string
		request    *models.CreateBankRequest
		expectErrs int
	}{
		{
			"valid request",
			&models.CreateBankRequest{
				BankID:   "valid-bank-id",
				BankCode: "VALIDCODE",
				BankName: "Valid Bank Name",
				Branding: models.Branding{Logo: "https://logo.url", Colors: "#FFFFFF"},
				OperationalParams: models.OperationalParams{
					BusinessHours: "9-5",
					Limits:        map[string]interface{}{"daily": 10000},
					Currencies:    []string{"USD"},
				},
			},
			0,
		},
		{
			"missing bank ID",
			&models.CreateBankRequest{
				BankCode: "VALIDCODE",
				BankName: "Valid Bank Name",
				Branding: models.Branding{Logo: "https://logo.url", Colors: "#FFFFFF"},
				OperationalParams: models.OperationalParams{
					BusinessHours: "9-5",
					Limits:        map[string]interface{}{"daily": 10000},
					Currencies:    []string{"USD"},
				},
			},
			1,
		},
		{
			"all fields missing",
			&models.CreateBankRequest{},
			5, // bankId, bankCode, bankName, branding, operationalParams
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			results := v.ValidateCreateBankRequest(tt.request)
			assert.Equal(t, tt.expectErrs, len(results))
		})
	}
}

// TestValidateUpdateBankRequest_Full tests full UpdateBankRequest validation
func TestValidateUpdateBankRequest_Full(t *testing.T) {
	v := validators.NewBankValidator()

	bankName := "Updated Name"
	longName := string(make([]byte, 300))

	tests := []struct {
		name       string
		request    *models.UpdateBankRequest
		expectErrs int
	}{
		{
			"valid request with bank name",
			&models.UpdateBankRequest{BankName: &bankName},
			0,
		},
		{
			"invalid - no fields provided",
			&models.UpdateBankRequest{},
			1,
		},
		{
			"invalid - bank name too long",
			&models.UpdateBankRequest{BankName: &longName},
			1,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			results := v.ValidateUpdateBankRequest(tt.request)
			assert.Equal(t, tt.expectErrs, len(results))
		})
	}
}

// TestValidationResult_ToError tests ValidationResult.ToError method
func TestValidationResult_ToError(t *testing.T) {
	validResult := validators.ValidationResult{Valid: true}
	assert.Nil(t, validResult.ToError())

	invalidResult := validators.ValidationResult{
		Valid:   false,
		Message: "Test error message",
	}
	err := invalidResult.ToError()
	assert.NotNil(t, err)
	assert.Equal(t, "Test error message", err.Error())
}

// TestNewBankNotFoundError tests NewBankNotFoundError helper
func TestNewBankNotFoundError(t *testing.T) {
	result := validators.NewBankNotFoundError("test-bank-id")

	assert.False(t, result.Valid)
	assert.Equal(t, validators.ErrCodeBankNotFound, result.Code)
	assert.Contains(t, result.Message, "test-bank-id")
	assert.Equal(t, "bankId", result.Field)
}

// TestNewBankIDUniqueError tests NewBankIDUniqueError helper
func TestNewBankIDUniqueError(t *testing.T) {
	result := validators.NewBankIDUniqueError("duplicate-id")

	assert.False(t, result.Valid)
	assert.Equal(t, validators.ErrCodeBankIDUnique, result.Code)
	assert.Contains(t, result.Message, "duplicate-id")
	assert.Equal(t, "bankId", result.Field)
}

// TestNewBankCodeUniqueError tests NewBankCodeUniqueError helper
func TestNewBankCodeUniqueError(t *testing.T) {
	result := validators.NewBankCodeUniqueError("DUPCODE")

	assert.False(t, result.Valid)
	assert.Equal(t, validators.ErrCodeBankCodeUnique, result.Code)
	assert.Contains(t, result.Message, "DUPCODE")
	assert.Equal(t, "bankCode", result.Field)
}
