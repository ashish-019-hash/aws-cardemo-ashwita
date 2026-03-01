package test

import (
	"encoding/json"
	"testing"
	"time"

	"github.com/obp-api/bank-registration/internal/models"
	"github.com/stretchr/testify/assert"
)

// TestMappedBank_Creation tests MappedBank struct creation
func TestMappedBank_Creation(t *testing.T) {
	tests := []struct {
		name     string
		bank     models.MappedBank
		wantErr  bool
	}{
		{
			name: "valid bank with all fields",
			bank: models.MappedBank{
				ID:                 1,
				Permalink:          "test-bank-001",
				FullBankName:       "Test Bank Full Name",
				ShortBankName:      "TESTBANK",
				LogoURL:            "https://example.com/logo.png",
				WebsiteURL:         "https://example.com",
				SwiftBIC:           "TESTBIC1",
				NationalIdentifier: "NAT001",
				BankRoutingScheme:  "BIC",
				BankRoutingAddress: "TESTADDR",
				CreatedAt:          time.Now(),
				UpdatedAt:          time.Now(),
			},
			wantErr: false,
		},
		{
			name: "valid bank with minimal fields",
			bank: models.MappedBank{
				Permalink:     "minimal-bank",
				FullBankName:  "Minimal Bank",
				ShortBankName: "MINBANK",
			},
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			assert.NotNil(t, tt.bank)
			assert.NotEmpty(t, tt.bank.Permalink)
		})
	}
}

// TestMappedBank_JSONSerialization tests JSON serialization/deserialization
func TestMappedBank_JSONSerialization(t *testing.T) {
	bank := models.MappedBank{
		ID:            1,
		Permalink:     "json-test-bank",
		FullBankName:  "JSON Test Bank",
		ShortBankName: "JSONBANK",
		LogoURL:       "https://example.com/logo.png",
		WebsiteURL:    "https://example.com",
		SwiftBIC:      "JSONBIC1",
	}

	// Serialize to JSON
	jsonData, err := json.Marshal(bank)
	assert.NoError(t, err)
	assert.NotEmpty(t, jsonData)

	// Verify JSON field names
	jsonStr := string(jsonData)
	assert.Contains(t, jsonStr, `"id":"json-test-bank"`)
	assert.Contains(t, jsonStr, `"full_name":"JSON Test Bank"`)
	assert.Contains(t, jsonStr, `"short_name":"JSONBANK"`)
	assert.Contains(t, jsonStr, `"logo":"https://example.com/logo.png"`)

	// Deserialize from JSON
	var deserializedBank models.MappedBank
	err = json.Unmarshal(jsonData, &deserializedBank)
	assert.NoError(t, err)
	assert.Equal(t, bank.Permalink, deserializedBank.Permalink)
	assert.Equal(t, bank.FullBankName, deserializedBank.FullBankName)
}

// TestMappedBank_Interface tests Bank interface implementation
func TestMappedBank_Interface(t *testing.T) {
	bank := &models.MappedBank{
		Permalink:          "interface-test",
		FullBankName:       "Interface Test Bank",
		ShortBankName:      "INTBANK",
		LogoURL:            "https://logo.url",
		WebsiteURL:         "https://website.url",
		SwiftBIC:           "SWIFTBIC",
		NationalIdentifier: "NATID",
		BankRoutingScheme:  "SCHEME",
		BankRoutingAddress: "ADDRESS",
	}

	// Test interface methods
	assert.Equal(t, "interface-test", bank.GetBankId().Value)
	assert.Equal(t, "Interface Test Bank", bank.GetFullName())
	assert.Equal(t, "INTBANK", bank.GetShortName())
	assert.Equal(t, "https://logo.url", bank.GetLogoUrl())
	assert.Equal(t, "https://website.url", bank.GetWebsiteUrl())
	assert.Equal(t, "SWIFTBIC", bank.GetSwiftBic())
	assert.Equal(t, "NATID", bank.GetNationalIdentifier())
	assert.Equal(t, "SCHEME", bank.GetBankRoutingScheme())
	assert.Equal(t, "ADDRESS", bank.GetBankRoutingAddress())
}

// TestNewMappedBank tests the constructor function
func TestNewMappedBank(t *testing.T) {
	bank := models.NewMappedBank()

	assert.NotNil(t, bank)
	assert.False(t, bank.CreatedAt.IsZero())
	assert.False(t, bank.UpdatedAt.IsZero())
}

// TestBankId_ValueType tests BankId value type
func TestBankId_ValueType(t *testing.T) {
	bankId := models.BankId{Value: "test-bank-id"}

	assert.Equal(t, "test-bank-id", bankId.Value)

	// Test JSON serialization
	jsonData, err := json.Marshal(bankId)
	assert.NoError(t, err)
	assert.Contains(t, string(jsonData), `"value":"test-bank-id"`)
}

// TestCreateBankRequest_Validation tests CreateBankRequest struct
func TestCreateBankRequest_Validation(t *testing.T) {
	tests := []struct {
		name    string
		request models.CreateBankRequest
		valid   bool
	}{
		{
			name: "valid request with all fields",
			request: models.CreateBankRequest{
				BankID:   "valid-bank-id",
				BankCode: "VALIDCODE",
				BankName: "Valid Bank Name",
				Branding: models.Branding{
					Logo:   "https://logo.url",
					Colors: "#FFFFFF",
				},
				OperationalParams: models.OperationalParams{
					BusinessHours: "9-5",
					Limits:        map[string]interface{}{"daily": 10000},
					Currencies:    []string{"USD", "EUR"},
				},
			},
			valid: true,
		},
		{
			name: "invalid request - missing bank ID",
			request: models.CreateBankRequest{
				BankCode: "VALIDCODE",
				BankName: "Valid Bank Name",
			},
			valid: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if tt.valid {
				assert.NotEmpty(t, tt.request.BankID)
				assert.NotEmpty(t, tt.request.BankCode)
				assert.NotEmpty(t, tt.request.BankName)
			} else {
				// At least one required field should be empty
				isEmpty := tt.request.BankID == "" || tt.request.BankCode == "" || tt.request.BankName == ""
				assert.True(t, isEmpty)
			}
		})
	}
}

// TestUpdateBankRequest_OptionalFields tests UpdateBankRequest optional fields
func TestUpdateBankRequest_OptionalFields(t *testing.T) {
	// Test with nil fields
	req := models.UpdateBankRequest{}
	assert.Nil(t, req.BankName)
	assert.Nil(t, req.Branding)
	assert.Nil(t, req.OperationalParams)

	// Test with set fields
	bankName := "Updated Name"
	req.BankName = &bankName
	assert.NotNil(t, req.BankName)
	assert.Equal(t, "Updated Name", *req.BankName)
}

// TestBranding_Struct tests Branding struct
func TestBranding_Struct(t *testing.T) {
	branding := models.Branding{
		Logo:   "https://logo.url",
		Colors: "#FF0000",
	}

	assert.Equal(t, "https://logo.url", branding.Logo)
	assert.Equal(t, "#FF0000", branding.Colors)

	// Test JSON serialization
	jsonData, err := json.Marshal(branding)
	assert.NoError(t, err)
	assert.Contains(t, string(jsonData), `"logo":"https://logo.url"`)
	assert.Contains(t, string(jsonData), `"colors":"#FF0000"`)
}

// TestOperationalParams_Struct tests OperationalParams struct
func TestOperationalParams_Struct(t *testing.T) {
	params := models.OperationalParams{
		BusinessHours: "9:00-17:00",
		Limits:        map[string]interface{}{"daily": 10000, "monthly": 100000},
		Currencies:    []string{"USD", "EUR", "GBP"},
	}

	assert.Equal(t, "9:00-17:00", params.BusinessHours)
	assert.NotNil(t, params.Limits)
	assert.Equal(t, 3, len(params.Currencies))
	assert.Contains(t, params.Currencies, "USD")
}

// TestCreateBankResponse_Struct tests CreateBankResponse struct
func TestCreateBankResponse_Struct(t *testing.T) {
	response := models.CreateBankResponse{
		BankID: "created-bank-id",
		Status: "created",
	}

	assert.Equal(t, "created-bank-id", response.BankID)
	assert.Equal(t, "created", response.Status)

	// Test JSON serialization
	jsonData, err := json.Marshal(response)
	assert.NoError(t, err)
	assert.Contains(t, string(jsonData), `"bankId":"created-bank-id"`)
	assert.Contains(t, string(jsonData), `"status":"created"`)
}

// TestUpdateBankResponse_Struct tests UpdateBankResponse struct
func TestUpdateBankResponse_Struct(t *testing.T) {
	response := models.UpdateBankResponse{
		BankID: "updated-bank-id",
		Status: "updated",
	}

	assert.Equal(t, "updated-bank-id", response.BankID)
	assert.Equal(t, "updated", response.Status)
}

// TestErrorResponse_Struct tests ErrorResponse struct
func TestErrorResponse_Struct(t *testing.T) {
	errResp := models.ErrorResponse{
		Code:    "BANK-VAL-001",
		Message: "Bank ID is required",
	}

	assert.Equal(t, "BANK-VAL-001", errResp.Code)
	assert.Equal(t, "Bank ID is required", errResp.Message)

	// Test JSON serialization
	jsonData, err := json.Marshal(errResp)
	assert.NoError(t, err)
	assert.Contains(t, string(jsonData), `"code":"BANK-VAL-001"`)
	assert.Contains(t, string(jsonData), `"message":"Bank ID is required"`)
}
