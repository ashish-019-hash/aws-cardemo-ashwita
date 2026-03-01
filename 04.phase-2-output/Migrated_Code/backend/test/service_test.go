package test

import (
	"context"
	"testing"

	"github.com/obp-api/bank-registration/internal/models"
	"github.com/obp-api/bank-registration/internal/repositories"
	"github.com/obp-api/bank-registration/internal/services"
	"github.com/obp-api/bank-registration/pkg/db"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// setupServiceTest creates a new service with test database
func setupServiceTest(t *testing.T) (*services.BankService, func()) {
	testDB, err := db.NewTestDB()
	require.NoError(t, err)

	repo := repositories.NewBankRepository(testDB)
	service := services.NewBankService(repo)

	cleanup := func() {
		testDB.Close()
	}

	return service, cleanup
}

// createValidCreateRequest creates a valid CreateBankRequest for testing
func createValidCreateRequest(bankID, bankCode, bankName string) *models.CreateBankRequest {
	return &models.CreateBankRequest{
		BankID:   bankID,
		BankCode: bankCode,
		BankName: bankName,
		Branding: models.Branding{
			Logo:   "https://logo.url",
			Colors: "#FFFFFF",
		},
		OperationalParams: models.OperationalParams{
			BusinessHours: "9-5",
			Limits:        map[string]interface{}{"daily": 10000},
			Currencies:    []string{"USD"},
		},
	}
}

// TestBankService_CreateBank tests bank creation service
// Tests BR-001 (Unique Identification) and BR-002 (Required Fields)
func TestBankService_CreateBank(t *testing.T) {
	service, cleanup := setupServiceTest(t)
	defer cleanup()

	ctx := context.Background()

	tests := []struct {
		name       string
		request    *models.CreateBankRequest
		wantErr    bool
		errCode    string
		httpStatus int
	}{
		{
			name:    "valid bank creation",
			request: createValidCreateRequest("valid-bank-001", "VALIDBANK", "Valid Bank"),
			wantErr: false,
		},
		{
			name: "missing bank ID - BR-002",
			request: &models.CreateBankRequest{
				BankCode: "NOCODE",
				BankName: "No ID Bank",
				Branding: models.Branding{Logo: "https://logo.url", Colors: "#FFFFFF"},
				OperationalParams: models.OperationalParams{
					BusinessHours: "9-5",
					Limits:        map[string]interface{}{"daily": 10000},
					Currencies:    []string{"USD"},
				},
			},
			wantErr:    true,
			errCode:    "BANK-VAL-001",
			httpStatus: 400,
		},
		{
			name: "missing bank code - BR-002",
			request: &models.CreateBankRequest{
				BankID:   "no-code-bank",
				BankName: "No Code Bank",
				Branding: models.Branding{Logo: "https://logo.url", Colors: "#FFFFFF"},
				OperationalParams: models.OperationalParams{
					BusinessHours: "9-5",
					Limits:        map[string]interface{}{"daily": 10000},
					Currencies:    []string{"USD"},
				},
			},
			wantErr:    true,
			errCode:    "BANK-VAL-002",
			httpStatus: 400,
		},
		{
			name: "missing bank name - BR-002",
			request: &models.CreateBankRequest{
				BankID:   "no-name-bank",
				BankCode: "NONAME",
				Branding: models.Branding{Logo: "https://logo.url", Colors: "#FFFFFF"},
				OperationalParams: models.OperationalParams{
					BusinessHours: "9-5",
					Limits:        map[string]interface{}{"daily": 10000},
					Currencies:    []string{"USD"},
				},
			},
			wantErr:    true,
			errCode:    "BANK-VAL-003",
			httpStatus: 400,
		},
		{
			name: "missing branding - BR-002",
			request: &models.CreateBankRequest{
				BankID:   "no-branding-bank",
				BankCode: "NOBRAND",
				BankName: "No Branding Bank",
				OperationalParams: models.OperationalParams{
					BusinessHours: "9-5",
					Limits:        map[string]interface{}{"daily": 10000},
					Currencies:    []string{"USD"},
				},
			},
			wantErr:    true,
			errCode:    "BANK-VAL-004",
			httpStatus: 400,
		},
		{
			name: "missing operational params - BR-002",
			request: &models.CreateBankRequest{
				BankID:   "no-params-bank",
				BankCode: "NOPARAM",
				BankName: "No Params Bank",
				Branding: models.Branding{Logo: "https://logo.url", Colors: "#FFFFFF"},
			},
			wantErr:    true,
			errCode:    "BANK-VAL-005",
			httpStatus: 400,
		},
		{
			name: "invalid bank ID format - too short",
			request: &models.CreateBankRequest{
				BankID:   "abc",
				BankCode: "SHORTID",
				BankName: "Short ID Bank",
				Branding: models.Branding{Logo: "https://logo.url", Colors: "#FFFFFF"},
				OperationalParams: models.OperationalParams{
					BusinessHours: "9-5",
					Limits:        map[string]interface{}{"daily": 10000},
					Currencies:    []string{"USD"},
				},
			},
			wantErr:    true,
			errCode:    "BANK-VAL-010",
			httpStatus: 400,
		},
		{
			name: "invalid bank ID format - contains space",
			request: &models.CreateBankRequest{
				BankID:   "bank with space",
				BankCode: "SPACEID",
				BankName: "Space ID Bank",
				Branding: models.Branding{Logo: "https://logo.url", Colors: "#FFFFFF"},
				OperationalParams: models.OperationalParams{
					BusinessHours: "9-5",
					Limits:        map[string]interface{}{"daily": 10000},
					Currencies:    []string{"USD"},
				},
			},
			wantErr:    true,
			errCode:    "BANK-VAL-010",
			httpStatus: 400,
		},
		{
			name: "invalid bank code format - lowercase",
			request: &models.CreateBankRequest{
				BankID:   "lowercase-code-bank",
				BankCode: "lowercase",
				BankName: "Lowercase Code Bank",
				Branding: models.Branding{Logo: "https://logo.url", Colors: "#FFFFFF"},
				OperationalParams: models.OperationalParams{
					BusinessHours: "9-5",
					Limits:        map[string]interface{}{"daily": 10000},
					Currencies:    []string{"USD"},
				},
			},
			wantErr:    true,
			errCode:    "BANK-VAL-011",
			httpStatus: 400,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			response, err := service.CreateBank(ctx, tt.request)

			if tt.wantErr {
				assert.Error(t, err)
				assert.Nil(t, response)

				serviceErr, ok := err.(*services.ServiceError)
				assert.True(t, ok, "Expected ServiceError type")
				if ok {
					assert.Equal(t, tt.errCode, serviceErr.Code)
					assert.Equal(t, tt.httpStatus, serviceErr.HTTPStatus)
				}
			} else {
				assert.NoError(t, err)
				assert.NotNil(t, response)
				assert.Equal(t, tt.request.BankID, response.BankID)
				assert.Equal(t, "created", response.Status)
			}
		})
	}
}

// TestBankService_CreateBank_DuplicateID tests BR-001: Unique Bank ID
func TestBankService_CreateBank_DuplicateID(t *testing.T) {
	service, cleanup := setupServiceTest(t)
	defer cleanup()

	ctx := context.Background()

	// Create first bank
	req1 := createValidCreateRequest("duplicate-id-test", "BANK001", "Bank One")
	response, err := service.CreateBank(ctx, req1)
	require.NoError(t, err)
	assert.NotNil(t, response)

	// Try to create second bank with same ID
	req2 := createValidCreateRequest("duplicate-id-test", "BANK002", "Bank Two")
	response, err = service.CreateBank(ctx, req2)

	assert.Error(t, err)
	assert.Nil(t, response)

	serviceErr, ok := err.(*services.ServiceError)
	assert.True(t, ok)
	if ok {
		assert.Equal(t, "BANK-VAL-007", serviceErr.Code)
		assert.Equal(t, 409, serviceErr.HTTPStatus)
	}
}

// TestBankService_CreateBank_DuplicateCode tests BR-001: Unique Bank Code
func TestBankService_CreateBank_DuplicateCode(t *testing.T) {
	service, cleanup := setupServiceTest(t)
	defer cleanup()

	ctx := context.Background()

	// Create first bank
	req1 := createValidCreateRequest("bank-001", "DUPCODE", "Bank One")
	response, err := service.CreateBank(ctx, req1)
	require.NoError(t, err)
	assert.NotNil(t, response)

	// Try to create second bank with same code
	req2 := createValidCreateRequest("bank-002", "DUPCODE", "Bank Two")
	response, err = service.CreateBank(ctx, req2)

	assert.Error(t, err)
	assert.Nil(t, response)

	serviceErr, ok := err.(*services.ServiceError)
	assert.True(t, ok)
	if ok {
		assert.Equal(t, "BANK-VAL-008", serviceErr.Code)
		assert.Equal(t, 409, serviceErr.HTTPStatus)
	}
}

// TestBankService_UpdateBank tests bank update service
// Tests BR-003 (Valid Updates - only existing banks can be updated)
func TestBankService_UpdateBank(t *testing.T) {
	service, cleanup := setupServiceTest(t)
	defer cleanup()

	ctx := context.Background()

	// Create a bank first
	createReq := createValidCreateRequest("update-test-bank", "UPDATETEST", "Update Test Bank")
	_, err := service.CreateBank(ctx, createReq)
	require.NoError(t, err)

	newName := "Updated Bank Name"
	newBranding := &models.Branding{Logo: "https://new-logo.url", Colors: "#000000"}

	tests := []struct {
		name       string
		bankID     string
		request    *models.UpdateBankRequest
		wantErr    bool
		errCode    string
		httpStatus int
	}{
		{
			name:    "valid update with bank name",
			bankID:  "update-test-bank",
			request: &models.UpdateBankRequest{BankName: &newName},
			wantErr: false,
		},
		{
			name:    "valid update with branding",
			bankID:  "update-test-bank",
			request: &models.UpdateBankRequest{Branding: newBranding},
			wantErr: false,
		},
		{
			name:       "update non-existent bank - BR-003",
			bankID:     "non-existent-bank",
			request:    &models.UpdateBankRequest{BankName: &newName},
			wantErr:    true,
			errCode:    "BANK-VAL-009",
			httpStatus: 404,
		},
		{
			name:       "update with empty bank ID",
			bankID:     "",
			request:    &models.UpdateBankRequest{BankName: &newName},
			wantErr:    true,
			errCode:    "BANK-VAL-006",
			httpStatus: 400,
		},
		{
			name:       "update with no fields - VR-015",
			bankID:     "update-test-bank",
			request:    &models.UpdateBankRequest{},
			wantErr:    true,
			errCode:    "BANK-VAL-015",
			httpStatus: 400,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			response, err := service.UpdateBank(ctx, tt.bankID, tt.request)

			if tt.wantErr {
				assert.Error(t, err)
				assert.Nil(t, response)

				serviceErr, ok := err.(*services.ServiceError)
				assert.True(t, ok, "Expected ServiceError type")
				if ok {
					assert.Equal(t, tt.errCode, serviceErr.Code)
					assert.Equal(t, tt.httpStatus, serviceErr.HTTPStatus)
				}
			} else {
				assert.NoError(t, err)
				assert.NotNil(t, response)
				assert.Equal(t, tt.bankID, response.BankID)
				assert.Equal(t, "updated", response.Status)
			}
		})
	}
}

// TestBankService_UpdateBank_InvalidBankIDFormat tests invalid bank ID format in update
func TestBankService_UpdateBank_InvalidBankIDFormat(t *testing.T) {
	service, cleanup := setupServiceTest(t)
	defer cleanup()

	ctx := context.Background()

	newName := "Updated Name"

	tests := []struct {
		name   string
		bankID string
	}{
		{"too short", "abc"},
		{"contains space", "bank id"},
		{"contains ::::", "bank::::id"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			response, err := service.UpdateBank(ctx, tt.bankID, &models.UpdateBankRequest{BankName: &newName})

			assert.Error(t, err)
			assert.Nil(t, response)

			serviceErr, ok := err.(*services.ServiceError)
			assert.True(t, ok)
			if ok {
				assert.Equal(t, "BANK-VAL-010", serviceErr.Code)
				assert.Equal(t, 400, serviceErr.HTTPStatus)
			}
		})
	}
}

// TestBankService_GetBank tests bank retrieval
func TestBankService_GetBank(t *testing.T) {
	service, cleanup := setupServiceTest(t)
	defer cleanup()

	ctx := context.Background()

	// Create a bank first
	createReq := createValidCreateRequest("get-test-bank", "GETTEST", "Get Test Bank")
	_, err := service.CreateBank(ctx, createReq)
	require.NoError(t, err)

	tests := []struct {
		name       string
		bankID     string
		wantErr    bool
		httpStatus int
	}{
		{
			name:    "get existing bank",
			bankID:  "get-test-bank",
			wantErr: false,
		},
		{
			name:       "get non-existent bank",
			bankID:     "non-existent-bank",
			wantErr:    true,
			httpStatus: 404,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			bank, err := service.GetBank(ctx, tt.bankID)

			if tt.wantErr {
				assert.Error(t, err)
				assert.Nil(t, bank)

				serviceErr, ok := err.(*services.ServiceError)
				assert.True(t, ok)
				if ok {
					assert.Equal(t, tt.httpStatus, serviceErr.HTTPStatus)
				}
			} else {
				assert.NoError(t, err)
				assert.NotNil(t, bank)
				assert.Equal(t, tt.bankID, bank.Permalink)
			}
		})
	}
}

// TestBankService_CreateAndUpdate_Integration tests full create and update flow
func TestBankService_CreateAndUpdate_Integration(t *testing.T) {
	service, cleanup := setupServiceTest(t)
	defer cleanup()

	ctx := context.Background()

	// Step 1: Create bank
	createReq := createValidCreateRequest("integration-test-bank", "INTTEST", "Integration Test Bank")
	createResp, err := service.CreateBank(ctx, createReq)
	require.NoError(t, err)
	assert.Equal(t, "created", createResp.Status)

	// Step 2: Verify bank was created
	bank, err := service.GetBank(ctx, "integration-test-bank")
	require.NoError(t, err)
	assert.Equal(t, "Integration Test Bank", bank.FullBankName)

	// Step 3: Update bank
	newName := "Updated Integration Test Bank"
	updateReq := &models.UpdateBankRequest{BankName: &newName}
	updateResp, err := service.UpdateBank(ctx, "integration-test-bank", updateReq)
	require.NoError(t, err)
	assert.Equal(t, "updated", updateResp.Status)

	// Step 4: Verify bank was updated
	updatedBank, err := service.GetBank(ctx, "integration-test-bank")
	require.NoError(t, err)
	assert.Equal(t, "Updated Integration Test Bank", updatedBank.FullBankName)
}

// TestServiceError tests ServiceError struct
func TestServiceError(t *testing.T) {
	err := &services.ServiceError{
		Code:       "TEST-001",
		Message:    "Test error message",
		HTTPStatus: 400,
		Field:      "testField",
	}

	assert.Equal(t, "Test error message", err.Error())
	assert.Equal(t, "TEST-001", err.Code)
	assert.Equal(t, 400, err.HTTPStatus)
	assert.Equal(t, "testField", err.Field)
}
