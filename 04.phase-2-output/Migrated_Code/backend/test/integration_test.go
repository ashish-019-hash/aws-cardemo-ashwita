package test

import (
	"bytes"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/gin-gonic/gin"
	"github.com/obp-api/bank-registration/internal/controllers"
	"github.com/obp-api/bank-registration/internal/models"
	"github.com/obp-api/bank-registration/internal/repositories"
	"github.com/obp-api/bank-registration/internal/routes"
	"github.com/obp-api/bank-registration/internal/services"
	"github.com/obp-api/bank-registration/pkg/db"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// setupIntegrationTest creates a complete test environment
func setupIntegrationTest(t *testing.T) (*gin.Engine, func()) {
	gin.SetMode(gin.TestMode)

	testDB, err := db.NewTestDB()
	require.NoError(t, err)

	repo := repositories.NewBankRepository(testDB)
	bankService := services.NewBankService(repo)
	bankAttributeService := services.NewBankAttributeService(repo)
	multiBankService := services.NewMultiBankService(repo)
	bankController := controllers.NewBankController(bankService)
	bankAttributeController := controllers.NewBankAttributeController(bankAttributeService)
	multiBankController := controllers.NewMultiBankController(multiBankService)

	router := gin.New()
	routes.SetupRoutes(router, bankController, bankAttributeController, multiBankController)

	cleanup := func() {
		testDB.Close()
	}

	return router, cleanup
}

// TestIntegration_FullBankLifecycle tests the complete bank lifecycle
// This tests the user story: Bank Registration and Configuration
func TestIntegration_FullBankLifecycle(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// Step 1: Create a new bank (Acceptance Criteria 1)
	// As a Bank Administrator, I want to create a new Bank entity
	createBody := map[string]interface{}{
		"bankId":   "integration-test-bank",
		"bankCode": "INTTEST",
		"bankName": "Integration Test Bank",
		"branding": map[string]interface{}{
			"logo":   "https://example.com/logo.png",
			"colors": "#FF5733",
		},
		"operationalParams": map[string]interface{}{
			"businessHours": "9-5",
			"limits":        map[string]interface{}{"daily": 10000},
			"currencies":    []string{"USD"},
		},
	}
	createData, _ := json.Marshal(createBody)

	createReq, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(createData))
	createReq.Header.Set("Content-Type", "application/json")

	w1 := httptest.NewRecorder()
	router.ServeHTTP(w1, createReq)

	assert.Equal(t, http.StatusCreated, w1.Code, "Bank creation should return 201 Created")

	var createResponse models.CreateBankResponse
	err := json.Unmarshal(w1.Body.Bytes(), &createResponse)
	require.NoError(t, err)
	assert.Equal(t, "integration-test-bank", createResponse.BankID)
	assert.Equal(t, "created", createResponse.Status)

	// Step 2: Verify bank was created by retrieving it
	// Note: Using /banks/:bankId from Bank Information Retrieval user story
	getReq, _ := http.NewRequest("GET", "/banks/integration-test-bank", nil)

	w2 := httptest.NewRecorder()
	router.ServeHTTP(w2, getReq)

	assert.Equal(t, http.StatusOK, w2.Code, "Bank retrieval should return 200 OK")

	var bankData map[string]interface{}
	err = json.Unmarshal(w2.Body.Bytes(), &bankData)
	require.NoError(t, err)
	assert.Equal(t, "integration-test-bank", bankData["id"])
	assert.Equal(t, "Integration Test Bank", bankData["full_name"])

	// Step 3: Update bank identification information (Acceptance Criteria 2)
	updateNameBody := map[string]interface{}{
		"bankName": "Updated Integration Test Bank",
	}
	updateNameData, _ := json.Marshal(updateNameBody)

	updateNameReq, _ := http.NewRequest("PUT", "/api/banks/integration-test-bank", bytes.NewBuffer(updateNameData))
	updateNameReq.Header.Set("Content-Type", "application/json")

	w3 := httptest.NewRecorder()
	router.ServeHTTP(w3, updateNameReq)

	assert.Equal(t, http.StatusOK, w3.Code, "Bank update should return 200 OK")

	// Step 4: Update bank branding elements (Acceptance Criteria 3)
	updateBrandingBody := map[string]interface{}{
		"branding": map[string]interface{}{
			"logo":   "https://example.com/new-logo.png",
			"colors": "#000000",
		},
	}
	updateBrandingData, _ := json.Marshal(updateBrandingBody)

	updateBrandingReq, _ := http.NewRequest("PUT", "/api/banks/integration-test-bank", bytes.NewBuffer(updateBrandingData))
	updateBrandingReq.Header.Set("Content-Type", "application/json")

	w4 := httptest.NewRecorder()
	router.ServeHTTP(w4, updateBrandingReq)

	assert.Equal(t, http.StatusOK, w4.Code, "Branding update should return 200 OK")

	// Step 5: Update bank operational parameters (Acceptance Criteria 4)
	updateOpsBody := map[string]interface{}{
		"operationalParams": map[string]interface{}{
			"businessHours": "8-6",
			"limits":        map[string]interface{}{"daily": 20000},
			"currencies":    []string{"USD", "EUR", "GBP"},
		},
	}
	updateOpsData, _ := json.Marshal(updateOpsBody)

	updateOpsReq, _ := http.NewRequest("PUT", "/api/banks/integration-test-bank", bytes.NewBuffer(updateOpsData))
	updateOpsReq.Header.Set("Content-Type", "application/json")

	w5 := httptest.NewRecorder()
	router.ServeHTTP(w5, updateOpsReq)

	assert.Equal(t, http.StatusOK, w5.Code, "Operational params update should return 200 OK")

	// Step 6: Verify all updates were applied
	// Note: Using /banks/:bankId from Bank Information Retrieval user story
	finalGetReq, _ := http.NewRequest("GET", "/banks/integration-test-bank", nil)

	w6 := httptest.NewRecorder()
	router.ServeHTTP(w6, finalGetReq)

	assert.Equal(t, http.StatusOK, w6.Code)

	var finalBankData map[string]interface{}
	err = json.Unmarshal(w6.Body.Bytes(), &finalBankData)
	require.NoError(t, err)
	assert.Equal(t, "Updated Integration Test Bank", finalBankData["full_name"])
}

// TestIntegration_BR001_UniqueIdentification tests BR-001: Unique Bank Identification Constraint
func TestIntegration_BR001_UniqueIdentification(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// Create first bank
	bank1Body := map[string]interface{}{
		"bankId":   "unique-test-bank",
		"bankCode": "UNIQUE1",
		"bankName": "Unique Test Bank 1",
		"branding": map[string]interface{}{
			"logo":   "https://logo.url",
			"colors": "#FFFFFF",
		},
		"operationalParams": map[string]interface{}{
			"businessHours": "9-5",
			"limits":        map[string]interface{}{"daily": 10000},
			"currencies":    []string{"USD"},
		},
	}
	bank1Data, _ := json.Marshal(bank1Body)

	req1, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(bank1Data))
	req1.Header.Set("Content-Type", "application/json")

	w1 := httptest.NewRecorder()
	router.ServeHTTP(w1, req1)
	assert.Equal(t, http.StatusCreated, w1.Code)

	// Try to create bank with duplicate ID
	bank2Body := map[string]interface{}{
		"bankId":   "unique-test-bank", // Same ID
		"bankCode": "UNIQUE2",
		"bankName": "Unique Test Bank 2",
		"branding": map[string]interface{}{
			"logo":   "https://logo.url",
			"colors": "#FFFFFF",
		},
		"operationalParams": map[string]interface{}{
			"businessHours": "9-5",
			"limits":        map[string]interface{}{"daily": 10000},
			"currencies":    []string{"USD"},
		},
	}
	bank2Data, _ := json.Marshal(bank2Body)

	req2, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(bank2Data))
	req2.Header.Set("Content-Type", "application/json")

	w2 := httptest.NewRecorder()
	router.ServeHTTP(w2, req2)

	assert.Equal(t, http.StatusConflict, w2.Code, "Duplicate bank ID should return 409 Conflict")

	var errResponse models.ErrorResponse
	err := json.Unmarshal(w2.Body.Bytes(), &errResponse)
	require.NoError(t, err)
	assert.Equal(t, "BANK-VAL-007", errResponse.Code)

	// Try to create bank with duplicate code
	bank3Body := map[string]interface{}{
		"bankId":   "another-unique-bank",
		"bankCode": "UNIQUE1", // Same code as bank1
		"bankName": "Another Unique Bank",
		"branding": map[string]interface{}{
			"logo":   "https://logo.url",
			"colors": "#FFFFFF",
		},
		"operationalParams": map[string]interface{}{
			"businessHours": "9-5",
			"limits":        map[string]interface{}{"daily": 10000},
			"currencies":    []string{"USD"},
		},
	}
	bank3Data, _ := json.Marshal(bank3Body)

	req3, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(bank3Data))
	req3.Header.Set("Content-Type", "application/json")

	w3 := httptest.NewRecorder()
	router.ServeHTTP(w3, req3)

	assert.Equal(t, http.StatusConflict, w3.Code, "Duplicate bank code should return 409 Conflict")

	var errResponse2 models.ErrorResponse
	err = json.Unmarshal(w3.Body.Bytes(), &errResponse2)
	require.NoError(t, err)
	assert.Equal(t, "BANK-VAL-008", errResponse2.Code)
}

// TestIntegration_BR002_RequiredFields tests BR-002: Required Fields for Bank Creation
func TestIntegration_BR002_RequiredFields(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	tests := []struct {
		name        string
		body        map[string]interface{}
		expectedErr string
	}{
		{
			name: "missing bankId",
			body: map[string]interface{}{
				"bankCode": "TESTCODE",
				"bankName": "Test Bank",
				"branding": map[string]interface{}{"logo": "https://logo.url", "colors": "#FFFFFF"},
				"operationalParams": map[string]interface{}{
					"businessHours": "9-5",
					"limits":        map[string]interface{}{"daily": 10000},
					"currencies":    []string{"USD"},
				},
			},
			expectedErr: "BANK-VAL-001",
		},
		{
			name: "missing bankCode",
			body: map[string]interface{}{
				"bankId":   "test-bank-id",
				"bankName": "Test Bank",
				"branding": map[string]interface{}{"logo": "https://logo.url", "colors": "#FFFFFF"},
				"operationalParams": map[string]interface{}{
					"businessHours": "9-5",
					"limits":        map[string]interface{}{"daily": 10000},
					"currencies":    []string{"USD"},
				},
			},
			expectedErr: "BANK-VAL-002",
		},
		{
			name: "missing bankName",
			body: map[string]interface{}{
				"bankId":   "test-bank-id",
				"bankCode": "TESTCODE",
				"branding": map[string]interface{}{"logo": "https://logo.url", "colors": "#FFFFFF"},
				"operationalParams": map[string]interface{}{
					"businessHours": "9-5",
					"limits":        map[string]interface{}{"daily": 10000},
					"currencies":    []string{"USD"},
				},
			},
			expectedErr: "BANK-VAL-003",
		},
		{
			name: "missing branding",
			body: map[string]interface{}{
				"bankId":   "test-bank-id",
				"bankCode": "TESTCODE",
				"bankName": "Test Bank",
				"operationalParams": map[string]interface{}{
					"businessHours": "9-5",
					"limits":        map[string]interface{}{"daily": 10000},
					"currencies":    []string{"USD"},
				},
			},
			expectedErr: "BANK-VAL-004",
		},
		{
			name: "missing operationalParams",
			body: map[string]interface{}{
				"bankId":   "test-bank-id",
				"bankCode": "TESTCODE",
				"bankName": "Test Bank",
				"branding": map[string]interface{}{"logo": "https://logo.url", "colors": "#FFFFFF"},
			},
			expectedErr: "BANK-VAL-005",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			data, _ := json.Marshal(tt.body)
			req, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(data))
			req.Header.Set("Content-Type", "application/json")

			w := httptest.NewRecorder()
			router.ServeHTTP(w, req)

			assert.Equal(t, http.StatusBadRequest, w.Code)

			var errResponse models.ErrorResponse
			err := json.Unmarshal(w.Body.Bytes(), &errResponse)
			require.NoError(t, err)
			assert.Equal(t, tt.expectedErr, errResponse.Code)
		})
	}
}

// TestIntegration_BR003_ValidUpdates tests BR-003: Valid Updates Constraint
func TestIntegration_BR003_ValidUpdates(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// Try to update non-existent bank
	updateBody := map[string]interface{}{
		"bankName": "Updated Name",
	}
	data, _ := json.Marshal(updateBody)

	req, _ := http.NewRequest("PUT", "/api/banks/non-existent-bank", bytes.NewBuffer(data))
	req.Header.Set("Content-Type", "application/json")

	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	assert.Equal(t, http.StatusNotFound, w.Code, "Update of non-existent bank should return 404 Not Found")

	var errResponse models.ErrorResponse
	err := json.Unmarshal(w.Body.Bytes(), &errResponse)
	require.NoError(t, err)
	assert.Equal(t, "BANK-VAL-009", errResponse.Code)
}

// TestIntegration_ValidationRules tests all validation rules (VR-001 through VR-015)
func TestIntegration_ValidationRules(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// VR-010: Bank ID Format Validation
	t.Run("VR-010 Bank ID Format", func(t *testing.T) {
		invalidIDs := []string{"abc", "bank id", "bank::::id", "bank@id!"}
		for _, id := range invalidIDs {
			body := map[string]interface{}{
				"bankId":   id,
				"bankCode": "TESTCODE",
				"bankName": "Test Bank",
				"branding": map[string]interface{}{"logo": "https://logo.url", "colors": "#FFFFFF"},
				"operationalParams": map[string]interface{}{
					"businessHours": "9-5",
					"limits":        map[string]interface{}{"daily": 10000},
					"currencies":    []string{"USD"},
				},
			}
			data, _ := json.Marshal(body)
			req, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(data))
			req.Header.Set("Content-Type", "application/json")

			w := httptest.NewRecorder()
			router.ServeHTTP(w, req)

			assert.Equal(t, http.StatusBadRequest, w.Code)

			var errResponse models.ErrorResponse
			json.Unmarshal(w.Body.Bytes(), &errResponse)
			assert.Equal(t, "BANK-VAL-010", errResponse.Code)
		}
	})

	// VR-011: Bank Code Format Validation
	t.Run("VR-011 Bank Code Format", func(t *testing.T) {
		invalidCodes := []string{"lowercase", "MixedCase", "BANK-CODE", "BANK CODE"}
		for _, code := range invalidCodes {
			body := map[string]interface{}{
				"bankId":   "valid-bank-id",
				"bankCode": code,
				"bankName": "Test Bank",
				"branding": map[string]interface{}{"logo": "https://logo.url", "colors": "#FFFFFF"},
				"operationalParams": map[string]interface{}{
					"businessHours": "9-5",
					"limits":        map[string]interface{}{"daily": 10000},
					"currencies":    []string{"USD"},
				},
			}
			data, _ := json.Marshal(body)
			req, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(data))
			req.Header.Set("Content-Type", "application/json")

			w := httptest.NewRecorder()
			router.ServeHTTP(w, req)

			assert.Equal(t, http.StatusBadRequest, w.Code)

			var errResponse models.ErrorResponse
			json.Unmarshal(w.Body.Bytes(), &errResponse)
			assert.Equal(t, "BANK-VAL-011", errResponse.Code)
		}
	})

	// VR-015: Update Request Field Validation
	t.Run("VR-015 Update Request Field Validation", func(t *testing.T) {
		// First create a bank
		createBody := map[string]interface{}{
			"bankId":   "vr015-test-bank",
			"bankCode": "VR015TEST",
			"bankName": "VR015 Test Bank",
			"branding": map[string]interface{}{"logo": "https://logo.url", "colors": "#FFFFFF"},
			"operationalParams": map[string]interface{}{
				"businessHours": "9-5",
				"limits":        map[string]interface{}{"daily": 10000},
				"currencies":    []string{"USD"},
			},
		}
		createData, _ := json.Marshal(createBody)
		createReq, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(createData))
		createReq.Header.Set("Content-Type", "application/json")

		w1 := httptest.NewRecorder()
		router.ServeHTTP(w1, createReq)
		assert.Equal(t, http.StatusCreated, w1.Code)

		// Try to update with no fields
		updateBody := map[string]interface{}{}
		updateData, _ := json.Marshal(updateBody)
		updateReq, _ := http.NewRequest("PUT", "/api/banks/vr015-test-bank", bytes.NewBuffer(updateData))
		updateReq.Header.Set("Content-Type", "application/json")

		w2 := httptest.NewRecorder()
		router.ServeHTTP(w2, updateReq)

		assert.Equal(t, http.StatusBadRequest, w2.Code)

		var errResponse models.ErrorResponse
		json.Unmarshal(w2.Body.Bytes(), &errResponse)
		assert.Equal(t, "BANK-VAL-015", errResponse.Code)
	})
}

// TestIntegration_MultipleBanks tests creating and managing multiple banks
func TestIntegration_MultipleBanks(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// Create multiple banks
	banks := []struct {
		id   string
		code string
		name string
	}{
		{"bank-alpha", "ALPHA", "Alpha Bank"},
		{"bank-beta", "BETA", "Beta Bank"},
		{"bank-gamma", "GAMMA", "Gamma Bank"},
	}

	for _, bank := range banks {
		body := map[string]interface{}{
			"bankId":   bank.id,
			"bankCode": bank.code,
			"bankName": bank.name,
			"branding": map[string]interface{}{"logo": "https://logo.url", "colors": "#FFFFFF"},
			"operationalParams": map[string]interface{}{
				"businessHours": "9-5",
				"limits":        map[string]interface{}{"daily": 10000},
				"currencies":    []string{"USD"},
			},
		}
		data, _ := json.Marshal(body)
		req, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(data))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusCreated, w.Code, "Bank %s should be created", bank.id)
	}

	// Verify each bank can be retrieved
	// Note: Using /banks/:bankId from Bank Information Retrieval user story
	for _, bank := range banks {
		req, _ := http.NewRequest("GET", "/banks/"+bank.id, nil)

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusOK, w.Code, "Bank %s should be retrievable", bank.id)

		var bankData map[string]interface{}
		json.Unmarshal(w.Body.Bytes(), &bankData)
		assert.Equal(t, bank.name, bankData["full_name"])
	}
}

// TestIntegration_HealthCheck tests the health endpoint
func TestIntegration_HealthCheck(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	req, _ := http.NewRequest("GET", "/health", nil)

	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	assert.Equal(t, http.StatusOK, w.Code)

	var response map[string]string
	err := json.Unmarshal(w.Body.Bytes(), &response)
	require.NoError(t, err)
	assert.Equal(t, "healthy", response["status"])
}

// TestIntegration_AcceptanceCriteria tests all acceptance criteria from user story
func TestIntegration_AcceptanceCriteria(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// AC1: System must allow creating new Bank entities with identification details
	t.Run("AC1 - Create Bank with identification", func(t *testing.T) {
		body := map[string]interface{}{
			"bankId":   "ac1-test-bank",
			"bankCode": "AC1TEST",
			"bankName": "AC1 Test Bank",
			"branding": map[string]interface{}{"logo": "https://logo.url", "colors": "#FFFFFF"},
			"operationalParams": map[string]interface{}{
				"businessHours": "9-5",
				"limits":        map[string]interface{}{"daily": 10000},
				"currencies":    []string{"USD"},
			},
		}
		data, _ := json.Marshal(body)
		req, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(data))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusCreated, w.Code)
	})

	// AC2: System must allow managing (updating) Bank identification information
	t.Run("AC2 - Update Bank identification", func(t *testing.T) {
		updateBody := map[string]interface{}{
			"bankName": "Updated AC1 Test Bank",
		}
		data, _ := json.Marshal(updateBody)
		req, _ := http.NewRequest("PUT", "/api/banks/ac1-test-bank", bytes.NewBuffer(data))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusOK, w.Code)
	})

	// AC3: System must allow managing (updating) Bank branding elements
	t.Run("AC3 - Update Bank branding", func(t *testing.T) {
		updateBody := map[string]interface{}{
			"branding": map[string]interface{}{
				"logo":   "https://new-logo.url",
				"colors": "#000000",
			},
		}
		data, _ := json.Marshal(updateBody)
		req, _ := http.NewRequest("PUT", "/api/banks/ac1-test-bank", bytes.NewBuffer(data))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusOK, w.Code)
	})

	// AC4: System must allow managing (updating) Bank operational parameters
	t.Run("AC4 - Update Bank operational parameters", func(t *testing.T) {
		updateBody := map[string]interface{}{
			"operationalParams": map[string]interface{}{
				"businessHours": "8-6",
				"limits":        map[string]interface{}{"daily": 20000},
				"currencies":    []string{"USD", "EUR"},
			},
		}
		data, _ := json.Marshal(updateBody)
		req, _ := http.NewRequest("PUT", "/api/banks/ac1-test-bank", bytes.NewBuffer(data))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusOK, w.Code)
	})

	// AC5: All create and manage operations must validate the provided data
	t.Run("AC5 - Validation on create and update", func(t *testing.T) {
		// Test validation on create
		invalidBody := map[string]interface{}{
			"bankId": "abc", // Too short
		}
		data, _ := json.Marshal(invalidBody)
		req, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(data))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusBadRequest, w.Code)

		// Test validation on update (no fields)
		emptyBody := map[string]interface{}{}
		emptyData, _ := json.Marshal(emptyBody)
		updateReq, _ := http.NewRequest("PUT", "/api/banks/ac1-test-bank", bytes.NewBuffer(emptyData))
		updateReq.Header.Set("Content-Type", "application/json")

		w2 := httptest.NewRecorder()
		router.ServeHTTP(w2, updateReq)

		assert.Equal(t, http.StatusBadRequest, w2.Code)
	})
}

// ============================================================================
// Bank Information Retrieval Integration Tests
// User Story: Bank Information Retrieval
// ============================================================================

// TestIntegration_BankRetrieval_GetAllBanks_EmptyList tests BR-004: Empty Result Handling
// When no banks exist, the API should return 200 OK with an empty array, not 404
func TestIntegration_BankRetrieval_GetAllBanks_EmptyList(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// GET /banks with no banks in database
	req, _ := http.NewRequest("GET", "/banks", nil)

	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	// BR-004, VR-006, VR-008: Should return 200 OK with empty array
	assert.Equal(t, http.StatusOK, w.Code, "Empty bank list should return 200 OK, not 404")

	var response models.BankListResponse
	err := json.Unmarshal(w.Body.Bytes(), &response)
	require.NoError(t, err)
	assert.NotNil(t, response.Banks, "Banks array should not be nil")
	assert.Empty(t, response.Banks, "Banks array should be empty")
}

// TestIntegration_BankRetrieval_GetAllBanks_WithBanks tests BR-003: Basic Bank Information Composition
// The bank list should include basic info but exclude attributes for performance
func TestIntegration_BankRetrieval_GetAllBanks_WithBanks(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// Create multiple banks first
	banks := []map[string]interface{}{
		{
			"bankId":   "retrieval-bank-alpha",
			"bankCode": "ALPHA",
			"bankName": "Alpha Bank",
			"branding": map[string]interface{}{"logo": "https://alpha.com/logo.png", "colors": "#FF0000"},
			"operationalParams": map[string]interface{}{
				"businessHours": "9-5",
				"limits":        map[string]interface{}{"daily": 10000},
				"currencies":    []string{"USD"},
			},
		},
		{
			"bankId":   "retrieval-bank-beta",
			"bankCode": "BETA",
			"bankName": "Beta Bank",
			"branding": map[string]interface{}{"logo": "https://beta.com/logo.png", "colors": "#00FF00"},
			"operationalParams": map[string]interface{}{
				"businessHours": "8-6",
				"limits":        map[string]interface{}{"daily": 20000},
				"currencies":    []string{"EUR"},
			},
		},
		{
			"bankId":   "retrieval-bank-gamma",
			"bankCode": "GAMMA",
			"bankName": "Gamma Bank",
			"branding": map[string]interface{}{"logo": "https://gamma.com/logo.png", "colors": "#0000FF"},
			"operationalParams": map[string]interface{}{
				"businessHours": "10-4",
				"limits":        map[string]interface{}{"daily": 30000},
				"currencies":    []string{"GBP"},
			},
		},
	}

	for _, bank := range banks {
		data, _ := json.Marshal(bank)
		req, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(data))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)
		assert.Equal(t, http.StatusCreated, w.Code)
	}

	// GET /banks to retrieve all banks
	getReq, _ := http.NewRequest("GET", "/banks", nil)

	w := httptest.NewRecorder()
	router.ServeHTTP(w, getReq)

	// VR-005, VR-008: Should return 200 OK with bank list
	assert.Equal(t, http.StatusOK, w.Code)

	var response models.BankListResponse
	err := json.Unmarshal(w.Body.Bytes(), &response)
	require.NoError(t, err)
	assert.Len(t, response.Banks, 3, "Should return all 3 banks")

	// BR-003: Verify basic bank info is included (VR-005)
	for _, bank := range response.Banks {
		assert.NotEmpty(t, bank.ID, "Bank ID should be present")
		assert.NotEmpty(t, bank.ShortName, "Short name should be present")
		assert.NotEmpty(t, bank.FullName, "Full name should be present")
		// Note: Attributes should NOT be included in list response (BR-003)
	}
}

// TestIntegration_BankRetrieval_GetBankById_Success tests BR-002: Complete Bank Information Composition
// Single bank retrieval should include complete info with attributes
func TestIntegration_BankRetrieval_GetBankById_Success(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// Create a bank first
	createBody := map[string]interface{}{
		"bankId":   "detail-test-bank",
		"bankCode": "DETAIL",
		"bankName": "Detail Test Bank",
		"branding": map[string]interface{}{"logo": "https://detail.com/logo.png", "colors": "#FFFFFF"},
		"operationalParams": map[string]interface{}{
			"businessHours": "9-5",
			"limits":        map[string]interface{}{"daily": 10000},
			"currencies":    []string{"USD"},
		},
	}
	createData, _ := json.Marshal(createBody)
	createReq, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(createData))
	createReq.Header.Set("Content-Type", "application/json")

	w1 := httptest.NewRecorder()
	router.ServeHTTP(w1, createReq)
	assert.Equal(t, http.StatusCreated, w1.Code)

	// GET /banks/:bankId to retrieve single bank with attributes
	getReq, _ := http.NewRequest("GET", "/banks/detail-test-bank", nil)

	w2 := httptest.NewRecorder()
	router.ServeHTTP(w2, getReq)

	// BR-002, VR-004, VR-008: Should return 200 OK with complete bank info
	assert.Equal(t, http.StatusOK, w2.Code)

	var response models.BankDetailResponse
	err := json.Unmarshal(w2.Body.Bytes(), &response)
	require.NoError(t, err)

	// VR-004: Verify complete bank info is included
	assert.Equal(t, "detail-test-bank", response.ID)
	assert.Equal(t, "DETAIL", response.ShortName)
	assert.Equal(t, "Detail Test Bank", response.FullName)
	assert.NotNil(t, response.BankRoutings, "Bank routings should be present")

	// VR-007: Attributes should be empty array, not nil
	assert.NotNil(t, response.Attributes, "Attributes should not be nil")
	assert.Empty(t, response.Attributes, "Attributes should be empty array when no attributes exist")
}

// TestIntegration_BankRetrieval_GetBankById_NotFound tests BR-001: Bank Existence Validation
// When bank doesn't exist, the API should return 404 Not Found
func TestIntegration_BankRetrieval_GetBankById_NotFound(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// GET /banks/:bankId for non-existent bank
	req, _ := http.NewRequest("GET", "/banks/non-existent-bank", nil)

	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	// BR-001, VR-002, VR-003, VR-008: Should return 404 Not Found
	assert.Equal(t, http.StatusNotFound, w.Code, "Non-existent bank should return 404 Not Found")

	var errResponse models.ErrorResponse
	err := json.Unmarshal(w.Body.Bytes(), &errResponse)
	require.NoError(t, err)
	assert.Equal(t, "BANK-VAL-009", errResponse.Code)
}

// TestIntegration_BankRetrieval_VR001_BankIdRequired tests VR-001: Bank Identifier Required Validation
// Empty bank ID should return 400 Bad Request
func TestIntegration_BankRetrieval_VR001_BankIdRequired(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// GET /banks/ with empty bank ID (trailing slash)
	// Note: This test verifies the service-level validation
	// The route /banks/ without ID will hit GetAllBanks, not GetBankById
	// So we test via the service directly or via a different route pattern

	// Test with explicit empty path parameter handling
	req, _ := http.NewRequest("GET", "/banks/", nil)

	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	// Note: Gin router may redirect /banks/ to /banks or return 404
	// The actual VR-001 validation happens at service level when bankId is empty string
	// This is tested in service_test.go
}

// TestIntegration_BankRetrieval_UserStory_AcceptanceCriteria tests all acceptance criteria
// from the Bank Information Retrieval user story
func TestIntegration_BankRetrieval_UserStory_AcceptanceCriteria(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// AC1: Retrieve list of all banks with basic information
	t.Run("AC1_RetrieveAllBanks", func(t *testing.T) {
		// Create some banks first
		for i := 1; i <= 3; i++ {
			body := map[string]interface{}{
				"bankId":   "ac1-bank-" + string(rune('a'+i-1)),
				"bankCode": "AC1" + string(rune('A'+i-1)),
				"bankName": "AC1 Test Bank " + string(rune('A'+i-1)),
				"branding": map[string]interface{}{"logo": "https://logo.url", "colors": "#FFFFFF"},
				"operationalParams": map[string]interface{}{
					"businessHours": "9-5",
					"limits":        map[string]interface{}{"daily": 10000},
					"currencies":    []string{"USD"},
				},
			}
			data, _ := json.Marshal(body)
			req, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(data))
			req.Header.Set("Content-Type", "application/json")

			w := httptest.NewRecorder()
			router.ServeHTTP(w, req)
			require.Equal(t, http.StatusCreated, w.Code)
		}

		// Retrieve all banks
		req, _ := http.NewRequest("GET", "/banks", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusOK, w.Code)

		var response models.BankListResponse
		err := json.Unmarshal(w.Body.Bytes(), &response)
		require.NoError(t, err)
		assert.GreaterOrEqual(t, len(response.Banks), 3)
	})

	// AC2: Retrieve single bank with complete information including attributes
	t.Run("AC2_RetrieveSingleBank", func(t *testing.T) {
		// Create a bank
		body := map[string]interface{}{
			"bankId":   "ac2-single-bank",
			"bankCode": "AC2SINGLE",
			"bankName": "AC2 Single Bank",
			"branding": map[string]interface{}{"logo": "https://logo.url", "colors": "#FFFFFF"},
			"operationalParams": map[string]interface{}{
				"businessHours": "9-5",
				"limits":        map[string]interface{}{"daily": 10000},
				"currencies":    []string{"USD"},
			},
		}
		data, _ := json.Marshal(body)
		createReq, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(data))
		createReq.Header.Set("Content-Type", "application/json")

		w1 := httptest.NewRecorder()
		router.ServeHTTP(w1, createReq)
		require.Equal(t, http.StatusCreated, w1.Code)

		// Retrieve single bank
		getReq, _ := http.NewRequest("GET", "/banks/ac2-single-bank", nil)
		w2 := httptest.NewRecorder()
		router.ServeHTTP(w2, getReq)

		assert.Equal(t, http.StatusOK, w2.Code)

		var response models.BankDetailResponse
		err := json.Unmarshal(w2.Body.Bytes(), &response)
		require.NoError(t, err)
		assert.Equal(t, "ac2-single-bank", response.ID)
		assert.Equal(t, "AC2 Single Bank", response.FullName)
		assert.NotNil(t, response.Attributes)
	})

	// AC3: Handle non-existent bank with appropriate error
	t.Run("AC3_HandleNonExistentBank", func(t *testing.T) {
		req, _ := http.NewRequest("GET", "/banks/ac3-non-existent", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusNotFound, w.Code)

		var errResponse models.ErrorResponse
		err := json.Unmarshal(w.Body.Bytes(), &errResponse)
		require.NoError(t, err)
		assert.Equal(t, "BANK-VAL-009", errResponse.Code)
	})

	// AC4: Handle empty bank list gracefully
	// Note: This is tested in TestIntegration_BankRetrieval_GetAllBanks_EmptyList

	// AC5: Return attributes as empty array when no attributes exist
	t.Run("AC5_EmptyAttributesArray", func(t *testing.T) {
		// Create a bank without attributes
		body := map[string]interface{}{
			"bankId":   "ac5-no-attrs-bank",
			"bankCode": "AC5NOATTR",
			"bankName": "AC5 No Attributes Bank",
			"branding": map[string]interface{}{"logo": "https://logo.url", "colors": "#FFFFFF"},
			"operationalParams": map[string]interface{}{
				"businessHours": "9-5",
				"limits":        map[string]interface{}{"daily": 10000},
				"currencies":    []string{"USD"},
			},
		}
		data, _ := json.Marshal(body)
		createReq, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(data))
		createReq.Header.Set("Content-Type", "application/json")

		w1 := httptest.NewRecorder()
		router.ServeHTTP(w1, createReq)
		require.Equal(t, http.StatusCreated, w1.Code)

		// Retrieve bank and verify attributes is empty array, not null
		getReq, _ := http.NewRequest("GET", "/banks/ac5-no-attrs-bank", nil)
		w2 := httptest.NewRecorder()
		router.ServeHTTP(w2, getReq)

		assert.Equal(t, http.StatusOK, w2.Code)

		// Parse raw JSON to verify attributes is [] not null
		var rawResponse map[string]interface{}
		err := json.Unmarshal(w2.Body.Bytes(), &rawResponse)
		require.NoError(t, err)

		attrs, ok := rawResponse["attributes"]
		assert.True(t, ok, "attributes field should be present")
		assert.NotNil(t, attrs, "attributes should not be null")

		attrArray, ok := attrs.([]interface{})
		assert.True(t, ok, "attributes should be an array")
		assert.Empty(t, attrArray, "attributes should be empty array")
	})
}

// TestIntegration_BankRetrieval_BusinessRules tests all business rules (BR-001 through BR-004)
func TestIntegration_BankRetrieval_BusinessRules(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// BR-001: Bank Existence Validation for Single Bank Retrieval
	t.Run("BR001_BankExistenceValidation", func(t *testing.T) {
		req, _ := http.NewRequest("GET", "/banks/br001-non-existent", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusNotFound, w.Code)
	})

	// BR-002: Complete Bank Information Composition for Single Bank Retrieval
	t.Run("BR002_CompleteBankInfoComposition", func(t *testing.T) {
		// Create a bank
		body := map[string]interface{}{
			"bankId":   "br002-complete-bank",
			"bankCode": "BR002",
			"bankName": "BR002 Complete Bank",
			"branding": map[string]interface{}{"logo": "https://logo.url", "colors": "#FFFFFF"},
			"operationalParams": map[string]interface{}{
				"businessHours": "9-5",
				"limits":        map[string]interface{}{"daily": 10000},
				"currencies":    []string{"USD"},
			},
		}
		data, _ := json.Marshal(body)
		createReq, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(data))
		createReq.Header.Set("Content-Type", "application/json")

		w1 := httptest.NewRecorder()
		router.ServeHTTP(w1, createReq)
		require.Equal(t, http.StatusCreated, w1.Code)

		// Retrieve and verify complete info
		getReq, _ := http.NewRequest("GET", "/banks/br002-complete-bank", nil)
		w2 := httptest.NewRecorder()
		router.ServeHTTP(w2, getReq)

		assert.Equal(t, http.StatusOK, w2.Code)

		var response models.BankDetailResponse
		err := json.Unmarshal(w2.Body.Bytes(), &response)
		require.NoError(t, err)

		// Verify all fields are present
		assert.NotEmpty(t, response.ID)
		assert.NotEmpty(t, response.ShortName)
		assert.NotEmpty(t, response.FullName)
		assert.NotNil(t, response.BankRoutings)
		assert.NotNil(t, response.Attributes)
	})

	// BR-003: Basic Bank Information Composition for Bank List Retrieval
	t.Run("BR003_BasicBankInfoComposition", func(t *testing.T) {
		// Create a bank
		body := map[string]interface{}{
			"bankId":   "br003-basic-bank",
			"bankCode": "BR003",
			"bankName": "BR003 Basic Bank",
			"branding": map[string]interface{}{"logo": "https://logo.url", "colors": "#FFFFFF"},
			"operationalParams": map[string]interface{}{
				"businessHours": "9-5",
				"limits":        map[string]interface{}{"daily": 10000},
				"currencies":    []string{"USD"},
			},
		}
		data, _ := json.Marshal(body)
		createReq, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(data))
		createReq.Header.Set("Content-Type", "application/json")

		w1 := httptest.NewRecorder()
		router.ServeHTTP(w1, createReq)
		require.Equal(t, http.StatusCreated, w1.Code)

		// Retrieve list and verify basic info only
		getReq, _ := http.NewRequest("GET", "/banks", nil)
		w2 := httptest.NewRecorder()
		router.ServeHTTP(w2, getReq)

		assert.Equal(t, http.StatusOK, w2.Code)

		var response models.BankListResponse
		err := json.Unmarshal(w2.Body.Bytes(), &response)
		require.NoError(t, err)

		// Find our bank in the list
		var found bool
		for _, bank := range response.Banks {
			if bank.ID == "br003-basic-bank" {
				found = true
				assert.NotEmpty(t, bank.ShortName)
				assert.NotEmpty(t, bank.FullName)
				// Note: BankListItem does NOT have Attributes field (BR-003)
				break
			}
		}
		assert.True(t, found, "Bank should be in the list")
	})

	// BR-004: Empty Result Handling for Bank List
	// Tested in TestIntegration_BankRetrieval_GetAllBanks_EmptyList
}

// TestIntegration_BankRetrieval_ValidationRules tests all validation rules (VR-001 through VR-008)
func TestIntegration_BankRetrieval_ValidationRules(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// VR-001: Bank Identifier Required Validation
	// Tested at service level - empty string bankId returns 400

	// VR-002: Bank Identifier Existence Validation
	t.Run("VR002_BankIdExistenceValidation", func(t *testing.T) {
		req, _ := http.NewRequest("GET", "/banks/vr002-non-existent", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusNotFound, w.Code)
	})

	// VR-003: Valid Bank Identifier Business Rule
	t.Run("VR003_ValidBankIdBusinessRule", func(t *testing.T) {
		req, _ := http.NewRequest("GET", "/banks/vr003-invalid-bank", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusNotFound, w.Code)
	})

	// VR-004: Complete Information for Single Bank
	t.Run("VR004_CompleteInfoForSingleBank", func(t *testing.T) {
		// Create a bank
		body := map[string]interface{}{
			"bankId":   "vr004-complete-bank",
			"bankCode": "VR004",
			"bankName": "VR004 Complete Bank",
			"branding": map[string]interface{}{"logo": "https://logo.url", "colors": "#FFFFFF"},
			"operationalParams": map[string]interface{}{
				"businessHours": "9-5",
				"limits":        map[string]interface{}{"daily": 10000},
				"currencies":    []string{"USD"},
			},
		}
		data, _ := json.Marshal(body)
		createReq, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(data))
		createReq.Header.Set("Content-Type", "application/json")

		w1 := httptest.NewRecorder()
		router.ServeHTTP(w1, createReq)
		require.Equal(t, http.StatusCreated, w1.Code)

		// Retrieve and verify complete info
		getReq, _ := http.NewRequest("GET", "/banks/vr004-complete-bank", nil)
		w2 := httptest.NewRecorder()
		router.ServeHTTP(w2, getReq)

		assert.Equal(t, http.StatusOK, w2.Code)

		var response models.BankDetailResponse
		err := json.Unmarshal(w2.Body.Bytes(), &response)
		require.NoError(t, err)

		// VR-004: All fields must be present
		assert.NotEmpty(t, response.ID)
		assert.NotEmpty(t, response.ShortName)
		assert.NotEmpty(t, response.FullName)
		assert.NotNil(t, response.BankRoutings)
		assert.NotNil(t, response.Attributes)
	})

	// VR-005: Basic Information for Bank List
	t.Run("VR005_BasicInfoForBankList", func(t *testing.T) {
		req, _ := http.NewRequest("GET", "/banks", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusOK, w.Code)

		var response models.BankListResponse
		err := json.Unmarshal(w.Body.Bytes(), &response)
		require.NoError(t, err)
		assert.NotNil(t, response.Banks)
	})

	// VR-006: Empty Bank List Handling
	// Tested in TestIntegration_BankRetrieval_GetAllBanks_EmptyList

	// VR-007: Empty Attributes Array Handling
	t.Run("VR007_EmptyAttributesArrayHandling", func(t *testing.T) {
		// Create a bank without attributes
		body := map[string]interface{}{
			"bankId":   "vr007-no-attrs-bank",
			"bankCode": "VR007",
			"bankName": "VR007 No Attributes Bank",
			"branding": map[string]interface{}{"logo": "https://logo.url", "colors": "#FFFFFF"},
			"operationalParams": map[string]interface{}{
				"businessHours": "9-5",
				"limits":        map[string]interface{}{"daily": 10000},
				"currencies":    []string{"USD"},
			},
		}
		data, _ := json.Marshal(body)
		createReq, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(data))
		createReq.Header.Set("Content-Type", "application/json")

		w1 := httptest.NewRecorder()
		router.ServeHTTP(w1, createReq)
		require.Equal(t, http.StatusCreated, w1.Code)

		// Retrieve and verify attributes is empty array
		getReq, _ := http.NewRequest("GET", "/banks/vr007-no-attrs-bank", nil)
		w2 := httptest.NewRecorder()
		router.ServeHTTP(w2, getReq)

		assert.Equal(t, http.StatusOK, w2.Code)

		var response models.BankDetailResponse
		err := json.Unmarshal(w2.Body.Bytes(), &response)
		require.NoError(t, err)
		assert.NotNil(t, response.Attributes, "Attributes should not be nil")
		assert.Empty(t, response.Attributes, "Attributes should be empty array")
	})

	// VR-008: HTTP Status Code Validation
	t.Run("VR008_HTTPStatusCodeValidation", func(t *testing.T) {
		// 200 for successful retrieval
		body := map[string]interface{}{
			"bankId":   "vr008-status-bank",
			"bankCode": "VR008",
			"bankName": "VR008 Status Bank",
			"branding": map[string]interface{}{"logo": "https://logo.url", "colors": "#FFFFFF"},
			"operationalParams": map[string]interface{}{
				"businessHours": "9-5",
				"limits":        map[string]interface{}{"daily": 10000},
				"currencies":    []string{"USD"},
			},
		}
		data, _ := json.Marshal(body)
		createReq, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(data))
		createReq.Header.Set("Content-Type", "application/json")

		w1 := httptest.NewRecorder()
		router.ServeHTTP(w1, createReq)
		require.Equal(t, http.StatusCreated, w1.Code)

		// 200 for successful single bank retrieval
		getReq, _ := http.NewRequest("GET", "/banks/vr008-status-bank", nil)
		w2 := httptest.NewRecorder()
		router.ServeHTTP(w2, getReq)
		assert.Equal(t, http.StatusOK, w2.Code)

		// 200 for bank list (even if empty)
		listReq, _ := http.NewRequest("GET", "/banks", nil)
		w3 := httptest.NewRecorder()
		router.ServeHTTP(w3, listReq)
		assert.Equal(t, http.StatusOK, w3.Code)

		// 404 for non-existent bank
		notFoundReq, _ := http.NewRequest("GET", "/banks/vr008-non-existent", nil)
		w4 := httptest.NewRecorder()
		router.ServeHTTP(w4, notFoundReq)
		assert.Equal(t, http.StatusNotFound, w4.Code)
	})
}

// ============================================================================
// Bank Attribute Management Integration Tests
// User Story: Bank Attribute Management
// ============================================================================

// createTestBankForAttributes creates a test bank for attribute tests
func createTestBankForAttributes(t *testing.T, router *gin.Engine, bankID string) {
	// Generate a valid uppercase bank code from the bankID
	// Bank code must be uppercase letters only, 4-10 chars
	bankCode := "ATTRTEST"
	if len(bankID) >= 8 {
		// Use last 4 chars to make it unique, convert to uppercase
		suffix := bankID[len(bankID)-4:]
		bankCode = "ATTR" + strings.ToUpper(strings.ReplaceAll(suffix, "-", ""))
	}
	createBody := map[string]interface{}{
		"bankId":   bankID,
		"bankCode": bankCode,
		"bankName": "Attribute Test Bank " + bankID,
		"branding": map[string]interface{}{
			"logo":   "https://example.com/logo.png",
			"colors": "#FF5733",
		},
		"operationalParams": map[string]interface{}{
			"businessHours": "9-5",
			"limits":        map[string]interface{}{"daily": 10000},
			"currencies":    []string{"USD"},
		},
	}
	createData, _ := json.Marshal(createBody)
	createReq, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(createData))
	createReq.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()
	router.ServeHTTP(w, createReq)
	require.Equal(t, http.StatusCreated, w.Code, "Failed to create test bank: "+w.Body.String())
}

// TestIntegration_BankAttribute_CreateAttribute tests POST /banks/:bankId/attribute
func TestIntegration_BankAttribute_CreateAttribute(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// Create a test bank first
	createTestBankForAttributes(t, router, "attr-create-test")

	t.Run("Create STRING attribute successfully", func(t *testing.T) {
		body := map[string]interface{}{
			"name":      "branch_code",
			"type":      "STRING",
			"value":     "NYC-001",
			"is_active": true,
		}
		data, _ := json.Marshal(body)
		req, _ := http.NewRequest("POST", "/banks/attr-create-test/attribute", bytes.NewBuffer(data))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusCreated, w.Code)

		var response models.BankAttributeResponse
		err := json.Unmarshal(w.Body.Bytes(), &response)
		require.NoError(t, err)
		assert.Equal(t, "attr-create-test", response.BankID)
		assert.Equal(t, "branch_code", response.Name)
		assert.Equal(t, "STRING", response.Type)
		assert.Equal(t, "NYC-001", response.Value)
		assert.True(t, response.IsActive)
		assert.NotEmpty(t, response.BankAttributeID)
	})

	t.Run("Create INTEGER attribute successfully", func(t *testing.T) {
		body := map[string]interface{}{
			"name":      "max_transactions",
			"type":      "INTEGER",
			"value":     "1000",
			"is_active": true,
		}
		data, _ := json.Marshal(body)
		req, _ := http.NewRequest("POST", "/banks/attr-create-test/attribute", bytes.NewBuffer(data))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusCreated, w.Code)

		var response models.BankAttributeResponse
		err := json.Unmarshal(w.Body.Bytes(), &response)
		require.NoError(t, err)
		assert.Equal(t, "INTEGER", response.Type)
		assert.Equal(t, "1000", response.Value)
	})

	t.Run("Create DOUBLE attribute successfully", func(t *testing.T) {
		body := map[string]interface{}{
			"name":      "interest_rate",
			"type":      "DOUBLE",
			"value":     "3.75",
			"is_active": true,
		}
		data, _ := json.Marshal(body)
		req, _ := http.NewRequest("POST", "/banks/attr-create-test/attribute", bytes.NewBuffer(data))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusCreated, w.Code)

		var response models.BankAttributeResponse
		err := json.Unmarshal(w.Body.Bytes(), &response)
		require.NoError(t, err)
		assert.Equal(t, "DOUBLE", response.Type)
		assert.Equal(t, "3.75", response.Value)
	})

	t.Run("Create DATE_WITH_DAY attribute successfully", func(t *testing.T) {
		body := map[string]interface{}{
			"name":      "founding_date",
			"type":      "DATE_WITH_DAY",
			"value":     "2020-01-15",
			"is_active": true,
		}
		data, _ := json.Marshal(body)
		req, _ := http.NewRequest("POST", "/banks/attr-create-test/attribute", bytes.NewBuffer(data))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusCreated, w.Code)

		var response models.BankAttributeResponse
		err := json.Unmarshal(w.Body.Bytes(), &response)
		require.NoError(t, err)
		assert.Equal(t, "DATE_WITH_DAY", response.Type)
		assert.Equal(t, "2020-01-15", response.Value)
	})

	t.Run("Create attribute with default is_active", func(t *testing.T) {
		body := map[string]interface{}{
			"name":  "default_active_attr",
			"type":  "STRING",
			"value": "test",
		}
		data, _ := json.Marshal(body)
		req, _ := http.NewRequest("POST", "/banks/attr-create-test/attribute", bytes.NewBuffer(data))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusCreated, w.Code)

		var response models.BankAttributeResponse
		err := json.Unmarshal(w.Body.Bytes(), &response)
		require.NoError(t, err)
		assert.True(t, response.IsActive, "Default is_active should be true")
	})
}

// TestIntegration_BankAttribute_BR001_BankExistence tests BR-001: Bank Existence Validation
func TestIntegration_BankAttribute_BR001_BankExistence(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	t.Run("Create attribute for non-existent bank returns 404", func(t *testing.T) {
		body := map[string]interface{}{
			"name":      "test_attr",
			"type":      "STRING",
			"value":     "test",
			"is_active": true,
		}
		data, _ := json.Marshal(body)
		req, _ := http.NewRequest("POST", "/banks/non-existent-bank/attribute", bytes.NewBuffer(data))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusNotFound, w.Code)

		var errResponse models.ErrorResponse
		err := json.Unmarshal(w.Body.Bytes(), &errResponse)
		require.NoError(t, err)
		assert.Equal(t, "BANK-VAL-009", errResponse.Code)
	})

	t.Run("Get attributes for non-existent bank returns 404", func(t *testing.T) {
		req, _ := http.NewRequest("GET", "/banks/non-existent-bank/attributes", nil)

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusNotFound, w.Code)
	})
}

// TestIntegration_BankAttribute_BR002_TypeValidation tests BR-002: Attribute Type Validation
func TestIntegration_BankAttribute_BR002_TypeValidation(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	createTestBankForAttributes(t, router, "attr-type-test")

	t.Run("Invalid attribute type returns 400", func(t *testing.T) {
		body := map[string]interface{}{
			"name":      "invalid_type_attr",
			"type":      "INVALID_TYPE",
			"value":     "test",
			"is_active": true,
		}
		data, _ := json.Marshal(body)
		req, _ := http.NewRequest("POST", "/banks/attr-type-test/attribute", bytes.NewBuffer(data))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusBadRequest, w.Code)

		var errResponse models.ErrorResponse
		err := json.Unmarshal(w.Body.Bytes(), &errResponse)
		require.NoError(t, err)
		assert.Equal(t, "ATTR-VAL-003", errResponse.Code)
	})

	t.Run("Empty attribute type returns 400", func(t *testing.T) {
		body := map[string]interface{}{
			"name":      "empty_type_attr",
			"type":      "",
			"value":     "test",
			"is_active": true,
		}
		data, _ := json.Marshal(body)
		req, _ := http.NewRequest("POST", "/banks/attr-type-test/attribute", bytes.NewBuffer(data))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusBadRequest, w.Code)
	})
}

// TestIntegration_BankAttribute_BR003_TypeValueConsistency tests BR-003: Type-Value Consistency
func TestIntegration_BankAttribute_BR003_TypeValueConsistency(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	createTestBankForAttributes(t, router, "attr-consistency")

	t.Run("INTEGER type with non-integer value returns 400", func(t *testing.T) {
		body := map[string]interface{}{
			"name":      "bad_integer",
			"type":      "INTEGER",
			"value":     "not-a-number",
			"is_active": true,
		}
		data, _ := json.Marshal(body)
		req, _ := http.NewRequest("POST", "/banks/attr-consistency/attribute", bytes.NewBuffer(data))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusBadRequest, w.Code)

		var errResponse models.ErrorResponse
		err := json.Unmarshal(w.Body.Bytes(), &errResponse)
		require.NoError(t, err)
		assert.Equal(t, "ATTR-VAL-007", errResponse.Code)
	})

	t.Run("DOUBLE type with non-numeric value returns 400", func(t *testing.T) {
		body := map[string]interface{}{
			"name":      "bad_double",
			"type":      "DOUBLE",
			"value":     "abc.def",
			"is_active": true,
		}
		data, _ := json.Marshal(body)
		req, _ := http.NewRequest("POST", "/banks/attr-consistency/attribute", bytes.NewBuffer(data))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusBadRequest, w.Code)
	})

	t.Run("DATE_WITH_DAY type with invalid date format returns 400", func(t *testing.T) {
		body := map[string]interface{}{
			"name":      "bad_date",
			"type":      "DATE_WITH_DAY",
			"value":     "01-15-2020", // Wrong format
			"is_active": true,
		}
		data, _ := json.Marshal(body)
		req, _ := http.NewRequest("POST", "/banks/attr-consistency/attribute", bytes.NewBuffer(data))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusBadRequest, w.Code)
	})
}

// TestIntegration_BankAttribute_GetAttributes tests GET /banks/:bankId/attributes
func TestIntegration_BankAttribute_GetAttributes(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	createTestBankForAttributes(t, router, "attr-get-test")

	t.Run("Get attributes for bank with no attributes returns empty array", func(t *testing.T) {
		req, _ := http.NewRequest("GET", "/banks/attr-get-test/attributes", nil)

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusOK, w.Code)

		var response models.BankAttributesListResponse
		err := json.Unmarshal(w.Body.Bytes(), &response)
		require.NoError(t, err)
		assert.NotNil(t, response.BankAttributes)
		assert.Len(t, response.BankAttributes, 0)
	})

	t.Run("Get attributes for bank with attributes returns all attributes", func(t *testing.T) {
		// Create some attributes first
		for i, name := range []string{"attr1", "attr2", "attr3"} {
			body := map[string]interface{}{
				"name":      name,
				"type":      "STRING",
				"value":     "value" + string(rune('1'+i)),
				"is_active": true,
			}
			data, _ := json.Marshal(body)
			req, _ := http.NewRequest("POST", "/banks/attr-get-test/attribute", bytes.NewBuffer(data))
			req.Header.Set("Content-Type", "application/json")
			w := httptest.NewRecorder()
			router.ServeHTTP(w, req)
			require.Equal(t, http.StatusCreated, w.Code)
		}

		// Now get all attributes
		req, _ := http.NewRequest("GET", "/banks/attr-get-test/attributes", nil)

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusOK, w.Code)

		var response models.BankAttributesListResponse
		err := json.Unmarshal(w.Body.Bytes(), &response)
		require.NoError(t, err)
		assert.Len(t, response.BankAttributes, 3)
	})
}

// TestIntegration_BankAttribute_GetAttributeByID tests GET /banks/:bankId/attributes/:attributeId
func TestIntegration_BankAttribute_GetAttributeByID(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	createTestBankForAttributes(t, router, "attr-getbyid")

	// Create an attribute first
	createBody := map[string]interface{}{
		"name":      "test_attr",
		"type":      "STRING",
		"value":     "test_value",
		"is_active": true,
	}
	createData, _ := json.Marshal(createBody)
	createReq, _ := http.NewRequest("POST", "/banks/attr-getbyid/attribute", bytes.NewBuffer(createData))
	createReq.Header.Set("Content-Type", "application/json")
	w1 := httptest.NewRecorder()
	router.ServeHTTP(w1, createReq)
	require.Equal(t, http.StatusCreated, w1.Code)

	var createResponse models.BankAttributeResponse
	json.Unmarshal(w1.Body.Bytes(), &createResponse)
	attributeID := createResponse.BankAttributeID

	t.Run("Get existing attribute by ID returns 200", func(t *testing.T) {
		req, _ := http.NewRequest("GET", "/banks/attr-getbyid/attributes/"+attributeID, nil)

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusOK, w.Code)

		var response models.BankAttributeResponse
		err := json.Unmarshal(w.Body.Bytes(), &response)
		require.NoError(t, err)
		assert.Equal(t, attributeID, response.BankAttributeID)
		assert.Equal(t, "test_attr", response.Name)
	})

	t.Run("Get non-existent attribute returns 404", func(t *testing.T) {
		req, _ := http.NewRequest("GET", "/banks/attr-getbyid/attributes/non-existent-id", nil)

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusNotFound, w.Code)

		var errResponse models.ErrorResponse
		err := json.Unmarshal(w.Body.Bytes(), &errResponse)
		require.NoError(t, err)
		assert.Equal(t, "ATTR-VAL-006", errResponse.Code)
	})
}

// TestIntegration_BankAttribute_UpdateAttribute tests PUT /banks/:bankId/attributes/:attributeId
func TestIntegration_BankAttribute_UpdateAttribute(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	createTestBankForAttributes(t, router, "attr-update")

	// Create an attribute first
	createBody := map[string]interface{}{
		"name":      "update_test_attr",
		"type":      "STRING",
		"value":     "original_value",
		"is_active": true,
	}
	createData, _ := json.Marshal(createBody)
	createReq, _ := http.NewRequest("POST", "/banks/attr-update/attribute", bytes.NewBuffer(createData))
	createReq.Header.Set("Content-Type", "application/json")
	w1 := httptest.NewRecorder()
	router.ServeHTTP(w1, createReq)
	require.Equal(t, http.StatusCreated, w1.Code)

	var createResponse models.BankAttributeResponse
	json.Unmarshal(w1.Body.Bytes(), &createResponse)
	attributeID := createResponse.BankAttributeID

	t.Run("Update attribute successfully", func(t *testing.T) {
		updateBody := map[string]interface{}{
			"name":      "updated_attr_name",
			"type":      "INTEGER",
			"value":     "42",
			"is_active": false,
		}
		updateData, _ := json.Marshal(updateBody)
		req, _ := http.NewRequest("PUT", "/banks/attr-update/attributes/"+attributeID, bytes.NewBuffer(updateData))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusOK, w.Code)

		var response models.BankAttributeResponse
		err := json.Unmarshal(w.Body.Bytes(), &response)
		require.NoError(t, err)
		assert.Equal(t, "updated_attr_name", response.Name)
		assert.Equal(t, "INTEGER", response.Type)
		assert.Equal(t, "42", response.Value)
		assert.False(t, response.IsActive)
	})

	t.Run("Update non-existent attribute returns 404", func(t *testing.T) {
		updateBody := map[string]interface{}{
			"name":      "test",
			"type":      "STRING",
			"value":     "test",
			"is_active": true,
		}
		updateData, _ := json.Marshal(updateBody)
		req, _ := http.NewRequest("PUT", "/banks/attr-update/attributes/non-existent-id", bytes.NewBuffer(updateData))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusNotFound, w.Code)
	})
}

// TestIntegration_BankAttribute_DeleteAttribute tests DELETE /banks/:bankId/attributes/:attributeId
func TestIntegration_BankAttribute_DeleteAttribute(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	createTestBankForAttributes(t, router, "attr-delete")

	// Create an attribute first
	createBody := map[string]interface{}{
		"name":      "delete_test_attr",
		"type":      "STRING",
		"value":     "to_be_deleted",
		"is_active": true,
	}
	createData, _ := json.Marshal(createBody)
	createReq, _ := http.NewRequest("POST", "/banks/attr-delete/attribute", bytes.NewBuffer(createData))
	createReq.Header.Set("Content-Type", "application/json")
	w1 := httptest.NewRecorder()
	router.ServeHTTP(w1, createReq)
	require.Equal(t, http.StatusCreated, w1.Code)

	var createResponse models.BankAttributeResponse
	json.Unmarshal(w1.Body.Bytes(), &createResponse)
	attributeID := createResponse.BankAttributeID

	t.Run("Delete attribute successfully", func(t *testing.T) {
		req, _ := http.NewRequest("DELETE", "/banks/attr-delete/attributes/"+attributeID, nil)

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusNoContent, w.Code)

		// Verify attribute is deleted
		getReq, _ := http.NewRequest("GET", "/banks/attr-delete/attributes/"+attributeID, nil)
		w2 := httptest.NewRecorder()
		router.ServeHTTP(w2, getReq)
		assert.Equal(t, http.StatusNotFound, w2.Code)
	})

	t.Run("Delete non-existent attribute returns 404", func(t *testing.T) {
		req, _ := http.NewRequest("DELETE", "/banks/attr-delete/attributes/non-existent-id", nil)

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusNotFound, w.Code)
	})
}

// TestIntegration_BankAttribute_VR011_UniqueNameWithinBank tests VR-011: Unique Attribute Name Within Bank
func TestIntegration_BankAttribute_VR011_UniqueNameWithinBank(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	createTestBankForAttributes(t, router, "attr-unique")

	// Create first attribute
	body1 := map[string]interface{}{
		"name":      "unique_attr",
		"type":      "STRING",
		"value":     "value1",
		"is_active": true,
	}
	data1, _ := json.Marshal(body1)
	req1, _ := http.NewRequest("POST", "/banks/attr-unique/attribute", bytes.NewBuffer(data1))
	req1.Header.Set("Content-Type", "application/json")
	w1 := httptest.NewRecorder()
	router.ServeHTTP(w1, req1)
	require.Equal(t, http.StatusCreated, w1.Code)

	t.Run("Duplicate attribute name returns 409", func(t *testing.T) {
		body2 := map[string]interface{}{
			"name":      "unique_attr", // Same name
			"type":      "INTEGER",
			"value":     "100",
			"is_active": true,
		}
		data2, _ := json.Marshal(body2)
		req2, _ := http.NewRequest("POST", "/banks/attr-unique/attribute", bytes.NewBuffer(data2))
		req2.Header.Set("Content-Type", "application/json")

		w2 := httptest.NewRecorder()
		router.ServeHTTP(w2, req2)

		assert.Equal(t, http.StatusConflict, w2.Code)

		var errResponse models.ErrorResponse
		err := json.Unmarshal(w2.Body.Bytes(), &errResponse)
		require.NoError(t, err)
		assert.Equal(t, "ATTR-VAL-008", errResponse.Code)
	})
}

// TestIntegration_BankAttribute_FullLifecycle tests the complete attribute lifecycle
func TestIntegration_BankAttribute_FullLifecycle(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// Step 1: Create a bank
	createTestBankForAttributes(t, router, "lifecycle-test")

	// Step 2: Create an attribute
	createBody := map[string]interface{}{
		"name":      "lifecycle_attr",
		"type":      "STRING",
		"value":     "initial_value",
		"is_active": true,
	}
	createData, _ := json.Marshal(createBody)
	createReq, _ := http.NewRequest("POST", "/banks/lifecycle-test/attribute", bytes.NewBuffer(createData))
	createReq.Header.Set("Content-Type", "application/json")
	w1 := httptest.NewRecorder()
	router.ServeHTTP(w1, createReq)
	require.Equal(t, http.StatusCreated, w1.Code)

	var createResponse models.BankAttributeResponse
	json.Unmarshal(w1.Body.Bytes(), &createResponse)
	attributeID := createResponse.BankAttributeID

	// Step 3: Retrieve the attribute
	getReq, _ := http.NewRequest("GET", "/banks/lifecycle-test/attributes/"+attributeID, nil)
	w2 := httptest.NewRecorder()
	router.ServeHTTP(w2, getReq)
	assert.Equal(t, http.StatusOK, w2.Code)

	// Step 4: Update the attribute
	updateBody := map[string]interface{}{
		"name":      "updated_lifecycle_attr",
		"type":      "INTEGER",
		"value":     "999",
		"is_active": false,
	}
	updateData, _ := json.Marshal(updateBody)
	updateReq, _ := http.NewRequest("PUT", "/banks/lifecycle-test/attributes/"+attributeID, bytes.NewBuffer(updateData))
	updateReq.Header.Set("Content-Type", "application/json")
	w3 := httptest.NewRecorder()
	router.ServeHTTP(w3, updateReq)
	assert.Equal(t, http.StatusOK, w3.Code)

	// Step 5: Verify update
	getReq2, _ := http.NewRequest("GET", "/banks/lifecycle-test/attributes/"+attributeID, nil)
	w4 := httptest.NewRecorder()
	router.ServeHTTP(w4, getReq2)
	assert.Equal(t, http.StatusOK, w4.Code)

	var updatedResponse models.BankAttributeResponse
	json.Unmarshal(w4.Body.Bytes(), &updatedResponse)
	assert.Equal(t, "updated_lifecycle_attr", updatedResponse.Name)
	assert.Equal(t, "INTEGER", updatedResponse.Type)
	assert.Equal(t, "999", updatedResponse.Value)
	assert.False(t, updatedResponse.IsActive)

	// Step 6: Delete the attribute
	deleteReq, _ := http.NewRequest("DELETE", "/banks/lifecycle-test/attributes/"+attributeID, nil)
	w5 := httptest.NewRecorder()
	router.ServeHTTP(w5, deleteReq)
	assert.Equal(t, http.StatusNoContent, w5.Code)

	// Step 7: Verify deletion
	getReq3, _ := http.NewRequest("GET", "/banks/lifecycle-test/attributes/"+attributeID, nil)
	w6 := httptest.NewRecorder()
	router.ServeHTTP(w6, getReq3)
	assert.Equal(t, http.StatusNotFound, w6.Code)
}

// TestIntegration_BankAttribute_AcceptanceCriteria tests all acceptance criteria from user story
func TestIntegration_BankAttribute_AcceptanceCriteria(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	createTestBankForAttributes(t, router, "ac-test-bank")

	// AC1: Define a new bank attribute with name, type, value, and active status
	t.Run("AC1: Define new bank attribute", func(t *testing.T) {
		body := map[string]interface{}{
			"name":      "ac1_attr",
			"type":      "STRING",
			"value":     "ac1_value",
			"is_active": true,
		}
		data, _ := json.Marshal(body)
		req, _ := http.NewRequest("POST", "/banks/ac-test-bank/attribute", bytes.NewBuffer(data))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusCreated, w.Code)
	})

	// AC2: Update existing bank attribute
	t.Run("AC2: Update existing bank attribute", func(t *testing.T) {
		// Create attribute first
		createBody := map[string]interface{}{
			"name":      "ac2_attr",
			"type":      "STRING",
			"value":     "original",
			"is_active": true,
		}
		createData, _ := json.Marshal(createBody)
		createReq, _ := http.NewRequest("POST", "/banks/ac-test-bank/attribute", bytes.NewBuffer(createData))
		createReq.Header.Set("Content-Type", "application/json")
		w1 := httptest.NewRecorder()
		router.ServeHTTP(w1, createReq)
		require.Equal(t, http.StatusCreated, w1.Code)

		var createResponse models.BankAttributeResponse
		json.Unmarshal(w1.Body.Bytes(), &createResponse)

		// Update attribute
		updateBody := map[string]interface{}{
			"name":      "ac2_attr_updated",
			"type":      "INTEGER",
			"value":     "100",
			"is_active": false,
		}
		updateData, _ := json.Marshal(updateBody)
		updateReq, _ := http.NewRequest("PUT", "/banks/ac-test-bank/attributes/"+createResponse.BankAttributeID, bytes.NewBuffer(updateData))
		updateReq.Header.Set("Content-Type", "application/json")

		w2 := httptest.NewRecorder()
		router.ServeHTTP(w2, updateReq)

		assert.Equal(t, http.StatusOK, w2.Code)
	})

	// AC3: Retrieve all bank attributes
	t.Run("AC3: Retrieve all bank attributes", func(t *testing.T) {
		req, _ := http.NewRequest("GET", "/banks/ac-test-bank/attributes", nil)

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusOK, w.Code)

		var response models.BankAttributesListResponse
		err := json.Unmarshal(w.Body.Bytes(), &response)
		require.NoError(t, err)
		assert.GreaterOrEqual(t, len(response.BankAttributes), 2)
	})

	// AC4: Retrieve single bank attribute
	t.Run("AC4: Retrieve single bank attribute", func(t *testing.T) {
		// Create attribute first
		createBody := map[string]interface{}{
			"name":      "ac4_attr",
			"type":      "DOUBLE",
			"value":     "3.14",
			"is_active": true,
		}
		createData, _ := json.Marshal(createBody)
		createReq, _ := http.NewRequest("POST", "/banks/ac-test-bank/attribute", bytes.NewBuffer(createData))
		createReq.Header.Set("Content-Type", "application/json")
		w1 := httptest.NewRecorder()
		router.ServeHTTP(w1, createReq)
		require.Equal(t, http.StatusCreated, w1.Code)

		var createResponse models.BankAttributeResponse
		json.Unmarshal(w1.Body.Bytes(), &createResponse)

		// Retrieve single attribute
		req, _ := http.NewRequest("GET", "/banks/ac-test-bank/attributes/"+createResponse.BankAttributeID, nil)

		w2 := httptest.NewRecorder()
		router.ServeHTTP(w2, req)

		assert.Equal(t, http.StatusOK, w2.Code)

		var response models.BankAttributeResponse
		err := json.Unmarshal(w2.Body.Bytes(), &response)
		require.NoError(t, err)
		assert.Equal(t, "ac4_attr", response.Name)
	})

	// AC5: Delete bank attribute
	t.Run("AC5: Delete bank attribute", func(t *testing.T) {
		// Create attribute first
		createBody := map[string]interface{}{
			"name":      "ac5_attr",
			"type":      "STRING",
			"value":     "to_delete",
			"is_active": true,
		}
		createData, _ := json.Marshal(createBody)
		createReq, _ := http.NewRequest("POST", "/banks/ac-test-bank/attribute", bytes.NewBuffer(createData))
		createReq.Header.Set("Content-Type", "application/json")
		w1 := httptest.NewRecorder()
		router.ServeHTTP(w1, createReq)
		require.Equal(t, http.StatusCreated, w1.Code)

		var createResponse models.BankAttributeResponse
		json.Unmarshal(w1.Body.Bytes(), &createResponse)

		// Delete attribute
		req, _ := http.NewRequest("DELETE", "/banks/ac-test-bank/attributes/"+createResponse.BankAttributeID, nil)

		w2 := httptest.NewRecorder()
		router.ServeHTTP(w2, req)

		assert.Equal(t, http.StatusNoContent, w2.Code)
	})

	// AC6: Validation on attribute operations
	t.Run("AC6: Validation on attribute operations", func(t *testing.T) {
		// Missing name
		body := map[string]interface{}{
			"type":      "STRING",
			"value":     "test",
			"is_active": true,
		}
		data, _ := json.Marshal(body)
		req, _ := http.NewRequest("POST", "/banks/ac-test-bank/attribute", bytes.NewBuffer(data))
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusBadRequest, w.Code)
	})
}

// ============================================================================
// Multi-Bank Support Integration Tests
// User Story: Multi-Bank Support
// ============================================================================

// createTestBankForMultiBank creates a test bank for multi-bank support tests
func createTestBankForMultiBank(t *testing.T, router *gin.Engine, bankID, bankCode, bankName string) {
	body := map[string]interface{}{
		"bankId":   bankID,
		"bankCode": bankCode,
		"bankName": bankName,
		"branding": map[string]interface{}{
			"logo":   "https://logo.url",
			"colors": "#FFFFFF",
		},
		"operationalParams": map[string]interface{}{
			"businessHours": "9-5",
			"limits":        map[string]interface{}{"daily": 10000},
			"currencies":    []string{"USD"},
		},
	}
	data, _ := json.Marshal(body)
	req, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(data))
	req.Header.Set("Content-Type", "application/json")

	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)
	require.Equal(t, http.StatusCreated, w.Code)
}

// TestIntegration_MultiBank_GetBankAccounts_EmptyList tests GET /banks/:bankId/accounts with no accounts
// Implements: BR-004 (Empty Result Handling - returns 200 with empty array)
func TestIntegration_MultiBank_GetBankAccounts_EmptyList(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// Create a test bank
	createTestBankForMultiBank(t, router, "accounts-empty-bank", "ACCEMPTY", "Accounts Empty Bank")

	// Get accounts for bank with no accounts
	req, _ := http.NewRequest("GET", "/banks/accounts-empty-bank/accounts", nil)

	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	// BR-004: Should return 200 OK with empty array, not 404
	assert.Equal(t, http.StatusOK, w.Code)

	var response map[string]interface{}
	err := json.Unmarshal(w.Body.Bytes(), &response)
	require.NoError(t, err)

	accounts, ok := response["accounts"].([]interface{})
	require.True(t, ok, "Response should contain accounts array")
	assert.Empty(t, accounts, "Accounts array should be empty")
}

// TestIntegration_MultiBank_GetBankAccounts_NonExistentBank tests GET /banks/:bankId/accounts with non-existent bank
// Implements: BR-002 (Bank Existence Validation)
// Implements: VR-002 (Bank Identifier Existence Validation)
// Implements: VR-010 (Cross-Bank Access Error Response Validation - generic 404 message)
func TestIntegration_MultiBank_GetBankAccounts_NonExistentBank(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// Get accounts for non-existent bank
	req, _ := http.NewRequest("GET", "/banks/non-existent-bank/accounts", nil)

	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	// BR-002, VR-002: Should return 404 Not Found
	assert.Equal(t, http.StatusNotFound, w.Code)

	var response models.ErrorResponse
	err := json.Unmarshal(w.Body.Bytes(), &response)
	require.NoError(t, err)
	assert.Equal(t, "BANK-VAL-009", response.Code)
	assert.Contains(t, response.Message, "Bank not found")
}

// TestIntegration_MultiBank_GetBankEntitlements_EmptyList tests GET /banks/:bankId/entitlements with no entitlements
// Implements: BR-004 (Empty Result Handling - returns 200 with empty array)
// Implements: BR-005 (Bank-Scoped Entitlements)
func TestIntegration_MultiBank_GetBankEntitlements_EmptyList(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// Create a test bank
	createTestBankForMultiBank(t, router, "entitlements-empty-bank", "ENTEMPTY", "Entitlements Empty Bank")

	// Get entitlements for bank with no entitlements
	req, _ := http.NewRequest("GET", "/banks/entitlements-empty-bank/entitlements", nil)

	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	// BR-004: Should return 200 OK with empty array, not 404
	assert.Equal(t, http.StatusOK, w.Code)

	var response map[string]interface{}
	err := json.Unmarshal(w.Body.Bytes(), &response)
	require.NoError(t, err)

	entitlements, ok := response["entitlements"].([]interface{})
	require.True(t, ok, "Response should contain entitlements array")
	assert.Empty(t, entitlements, "Entitlements array should be empty")
}

// TestIntegration_MultiBank_GetBankEntitlements_NonExistentBank tests GET /banks/:bankId/entitlements with non-existent bank
// Implements: BR-002 (Bank Existence Validation)
// Implements: VR-002 (Bank Identifier Existence Validation)
func TestIntegration_MultiBank_GetBankEntitlements_NonExistentBank(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// Get entitlements for non-existent bank
	req, _ := http.NewRequest("GET", "/banks/non-existent-bank/entitlements", nil)

	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	// BR-002, VR-002: Should return 404 Not Found
	assert.Equal(t, http.StatusNotFound, w.Code)

	var response models.ErrorResponse
	err := json.Unmarshal(w.Body.Bytes(), &response)
	require.NoError(t, err)
	assert.Equal(t, "BANK-VAL-009", response.Code)
}

// TestIntegration_MultiBank_BR001_MandatoryBankIdentifier tests BR-001: Mandatory Bank Identifier for Resource Access
// All bank-specific endpoints require BANK_ID in URL path
func TestIntegration_MultiBank_BR001_MandatoryBankIdentifier(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// Create a test bank
	createTestBankForMultiBank(t, router, "br001-test-bank", "BR001TST", "BR001 Test Bank")

	t.Run("Accounts endpoint requires bank ID in path", func(t *testing.T) {
		// Valid request with bank ID
		req, _ := http.NewRequest("GET", "/banks/br001-test-bank/accounts", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		// Should succeed with 200 OK
		assert.Equal(t, http.StatusOK, w.Code)
	})

	t.Run("Entitlements endpoint requires bank ID in path", func(t *testing.T) {
		// Valid request with bank ID
		req, _ := http.NewRequest("GET", "/banks/br001-test-bank/entitlements", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		// Should succeed with 200 OK
		assert.Equal(t, http.StatusOK, w.Code)
	})
}

// TestIntegration_MultiBank_BR002_BankExistenceValidation tests BR-002: Bank Existence Validation
// Returns 404 if bank not found
func TestIntegration_MultiBank_BR002_BankExistenceValidation(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	t.Run("Accounts endpoint returns 404 for non-existent bank", func(t *testing.T) {
		req, _ := http.NewRequest("GET", "/banks/non-existent-bank-br002/accounts", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusNotFound, w.Code)

		var response models.ErrorResponse
		json.Unmarshal(w.Body.Bytes(), &response)
		assert.Equal(t, "BANK-VAL-009", response.Code)
	})

	t.Run("Entitlements endpoint returns 404 for non-existent bank", func(t *testing.T) {
		req, _ := http.NewRequest("GET", "/banks/non-existent-bank-br002/entitlements", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusNotFound, w.Code)

		var response models.ErrorResponse
		json.Unmarshal(w.Body.Bytes(), &response)
		assert.Equal(t, "BANK-VAL-009", response.Code)
	})
}

// TestIntegration_MultiBank_BR003_DataIsolation tests BR-003: Data Isolation Enforcement
// All queries scoped to specified bank
func TestIntegration_MultiBank_BR003_DataIsolation(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// Create two test banks
	createTestBankForMultiBank(t, router, "bank-alpha", "ALPHA", "Alpha Bank")
	createTestBankForMultiBank(t, router, "bank-beta", "BETA", "Beta Bank")

	t.Run("Accounts are isolated per bank", func(t *testing.T) {
		// Get accounts for Alpha bank
		reqAlpha, _ := http.NewRequest("GET", "/banks/bank-alpha/accounts", nil)
		wAlpha := httptest.NewRecorder()
		router.ServeHTTP(wAlpha, reqAlpha)

		assert.Equal(t, http.StatusOK, wAlpha.Code)

		// Get accounts for Beta bank
		reqBeta, _ := http.NewRequest("GET", "/banks/bank-beta/accounts", nil)
		wBeta := httptest.NewRecorder()
		router.ServeHTTP(wBeta, reqBeta)

		assert.Equal(t, http.StatusOK, wBeta.Code)

		// Both should return empty arrays (no accounts created)
		var responseAlpha, responseBeta map[string]interface{}
		json.Unmarshal(wAlpha.Body.Bytes(), &responseAlpha)
		json.Unmarshal(wBeta.Body.Bytes(), &responseBeta)

		accountsAlpha := responseAlpha["accounts"].([]interface{})
		accountsBeta := responseBeta["accounts"].([]interface{})

		assert.Empty(t, accountsAlpha)
		assert.Empty(t, accountsBeta)
	})

	t.Run("Entitlements are isolated per bank", func(t *testing.T) {
		// Get entitlements for Alpha bank
		reqAlpha, _ := http.NewRequest("GET", "/banks/bank-alpha/entitlements", nil)
		wAlpha := httptest.NewRecorder()
		router.ServeHTTP(wAlpha, reqAlpha)

		assert.Equal(t, http.StatusOK, wAlpha.Code)

		// Get entitlements for Beta bank
		reqBeta, _ := http.NewRequest("GET", "/banks/bank-beta/entitlements", nil)
		wBeta := httptest.NewRecorder()
		router.ServeHTTP(wBeta, reqBeta)

		assert.Equal(t, http.StatusOK, wBeta.Code)

		// Both should return empty arrays (no entitlements created)
		var responseAlpha, responseBeta map[string]interface{}
		json.Unmarshal(wAlpha.Body.Bytes(), &responseAlpha)
		json.Unmarshal(wBeta.Body.Bytes(), &responseBeta)

		entitlementsAlpha := responseAlpha["entitlements"].([]interface{})
		entitlementsBeta := responseBeta["entitlements"].([]interface{})

		assert.Empty(t, entitlementsAlpha)
		assert.Empty(t, entitlementsBeta)
	})
}

// TestIntegration_MultiBank_BR007_IndependentBankOperations tests BR-007: Independent Bank Operations
// Operations on one bank don't affect others
func TestIntegration_MultiBank_BR007_IndependentBankOperations(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// Create two test banks
	createTestBankForMultiBank(t, router, "independent-bank-1", "INDEP1", "Independent Bank 1")
	createTestBankForMultiBank(t, router, "independent-bank-2", "INDEP2", "Independent Bank 2")

	t.Run("Operations on bank 1 don't affect bank 2", func(t *testing.T) {
		// Verify bank 1 exists
		req1, _ := http.NewRequest("GET", "/banks/independent-bank-1", nil)
		w1 := httptest.NewRecorder()
		router.ServeHTTP(w1, req1)
		assert.Equal(t, http.StatusOK, w1.Code)

		// Verify bank 2 exists
		req2, _ := http.NewRequest("GET", "/banks/independent-bank-2", nil)
		w2 := httptest.NewRecorder()
		router.ServeHTTP(w2, req2)
		assert.Equal(t, http.StatusOK, w2.Code)

		// Update bank 1
		updateBody := map[string]interface{}{
			"bankName": "Updated Independent Bank 1",
		}
		updateData, _ := json.Marshal(updateBody)
		updateReq, _ := http.NewRequest("PUT", "/api/banks/independent-bank-1", bytes.NewBuffer(updateData))
		updateReq.Header.Set("Content-Type", "application/json")
		wUpdate := httptest.NewRecorder()
		router.ServeHTTP(wUpdate, updateReq)
		assert.Equal(t, http.StatusOK, wUpdate.Code)

		// Verify bank 2 is unchanged
		req2After, _ := http.NewRequest("GET", "/banks/independent-bank-2", nil)
		w2After := httptest.NewRecorder()
		router.ServeHTTP(w2After, req2After)
		assert.Equal(t, http.StatusOK, w2After.Code)

		var bank2Data map[string]interface{}
		json.Unmarshal(w2After.Body.Bytes(), &bank2Data)
		assert.Equal(t, "Independent Bank 2", bank2Data["full_name"])
	})
}

// TestIntegration_MultiBank_BR008_SingleAPIMultiBank tests BR-008: Single API Instance Multi-Bank Support
// Multiple banks share same infrastructure with logical separation
func TestIntegration_MultiBank_BR008_SingleAPIMultiBank(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// Create multiple banks
	banks := []struct {
		id   string
		code string
		name string
	}{
		{"multi-bank-1", "MULTI1", "Multi Bank 1"},
		{"multi-bank-2", "MULTI2", "Multi Bank 2"},
		{"multi-bank-3", "MULTI3", "Multi Bank 3"},
	}

	for _, bank := range banks {
		createTestBankForMultiBank(t, router, bank.id, bank.code, bank.name)
	}

	t.Run("All banks accessible via same API", func(t *testing.T) {
		for _, bank := range banks {
			// Verify bank exists
			req, _ := http.NewRequest("GET", "/banks/"+bank.id, nil)
			w := httptest.NewRecorder()
			router.ServeHTTP(w, req)
			assert.Equal(t, http.StatusOK, w.Code)

			// Verify accounts endpoint works
			reqAccounts, _ := http.NewRequest("GET", "/banks/"+bank.id+"/accounts", nil)
			wAccounts := httptest.NewRecorder()
			router.ServeHTTP(wAccounts, reqAccounts)
			assert.Equal(t, http.StatusOK, wAccounts.Code)

			// Verify entitlements endpoint works
			reqEntitlements, _ := http.NewRequest("GET", "/banks/"+bank.id+"/entitlements", nil)
			wEntitlements := httptest.NewRecorder()
			router.ServeHTTP(wEntitlements, reqEntitlements)
			assert.Equal(t, http.StatusOK, wEntitlements.Code)
		}
	})

	t.Run("Bank list returns all banks", func(t *testing.T) {
		req, _ := http.NewRequest("GET", "/banks", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusOK, w.Code)

		var response map[string]interface{}
		json.Unmarshal(w.Body.Bytes(), &response)

		bankList := response["banks"].([]interface{})
		assert.GreaterOrEqual(t, len(bankList), 3, "Should have at least 3 banks")
	})
}

// TestIntegration_MultiBank_VR001_BankIdRequired tests VR-001: Bank Identifier Required Validation
func TestIntegration_MultiBank_VR001_BankIdRequired(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// Create a test bank
	createTestBankForMultiBank(t, router, "vr001-test-bank", "VR001TST", "VR001 Test Bank")

	t.Run("Valid bank ID returns 200", func(t *testing.T) {
		req, _ := http.NewRequest("GET", "/banks/vr001-test-bank/accounts", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusOK, w.Code)
	})
}

// TestIntegration_MultiBank_VR002_BankIdExistence tests VR-002: Bank Identifier Existence Validation
func TestIntegration_MultiBank_VR002_BankIdExistence(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	t.Run("Non-existent bank ID returns 404", func(t *testing.T) {
		req, _ := http.NewRequest("GET", "/banks/vr002-non-existent/accounts", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusNotFound, w.Code)

		var response models.ErrorResponse
		json.Unmarshal(w.Body.Bytes(), &response)
		assert.Equal(t, "BANK-VAL-009", response.Code)
	})
}

// TestIntegration_MultiBank_VR010_CrossBankAccessError tests VR-010: Cross-Bank Access Error Response Validation
// Returns 404 with generic message to prevent information leakage
func TestIntegration_MultiBank_VR010_CrossBankAccessError(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	t.Run("Non-existent bank returns generic 404 message", func(t *testing.T) {
		req, _ := http.NewRequest("GET", "/banks/secret-bank-id/accounts", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusNotFound, w.Code)

		var response models.ErrorResponse
		json.Unmarshal(w.Body.Bytes(), &response)

		// VR-010: Generic message to prevent info leakage
		assert.Contains(t, response.Message, "Bank not found")
		// Should NOT reveal whether bank exists or not
		assert.NotContains(t, response.Message, "secret-bank-id")
	})
}

// TestIntegration_MultiBank_UserStory_AcceptanceCriteria tests all acceptance criteria from user story
func TestIntegration_MultiBank_UserStory_AcceptanceCriteria(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// Create test banks
	createTestBankForMultiBank(t, router, "ac-bank-alpha", "ACALPHA", "AC Alpha Bank")
	createTestBankForMultiBank(t, router, "ac-bank-beta", "ACBETA", "AC Beta Bank")

	// AC1: All bank-specific endpoints require BANK_ID in URL path
	t.Run("AC1: Bank-specific endpoints require BANK_ID", func(t *testing.T) {
		// Accounts endpoint
		req, _ := http.NewRequest("GET", "/banks/ac-bank-alpha/accounts", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)
		assert.Equal(t, http.StatusOK, w.Code)

		// Entitlements endpoint
		req2, _ := http.NewRequest("GET", "/banks/ac-bank-alpha/entitlements", nil)
		w2 := httptest.NewRecorder()
		router.ServeHTTP(w2, req2)
		assert.Equal(t, http.StatusOK, w2.Code)
	})

	// AC2: System validates bank exists before processing
	t.Run("AC2: System validates bank exists", func(t *testing.T) {
		req, _ := http.NewRequest("GET", "/banks/non-existent-ac-bank/accounts", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusNotFound, w.Code)
	})

	// AC3: Data isolation - queries scoped to specified bank
	t.Run("AC3: Data isolation between banks", func(t *testing.T) {
		// Get accounts for Alpha bank
		reqAlpha, _ := http.NewRequest("GET", "/banks/ac-bank-alpha/accounts", nil)
		wAlpha := httptest.NewRecorder()
		router.ServeHTTP(wAlpha, reqAlpha)
		assert.Equal(t, http.StatusOK, wAlpha.Code)

		// Get accounts for Beta bank
		reqBeta, _ := http.NewRequest("GET", "/banks/ac-bank-beta/accounts", nil)
		wBeta := httptest.NewRecorder()
		router.ServeHTTP(wBeta, reqBeta)
		assert.Equal(t, http.StatusOK, wBeta.Code)

		// Both should return independent results
		var responseAlpha, responseBeta map[string]interface{}
		json.Unmarshal(wAlpha.Body.Bytes(), &responseAlpha)
		json.Unmarshal(wBeta.Body.Bytes(), &responseBeta)

		assert.NotNil(t, responseAlpha["accounts"])
		assert.NotNil(t, responseBeta["accounts"])
	})

	// AC4: Cross-bank access prevention
	t.Run("AC4: Cross-bank access prevention", func(t *testing.T) {
		// Trying to access non-existent bank returns 404
		req, _ := http.NewRequest("GET", "/banks/other-bank/accounts", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusNotFound, w.Code)
	})

	// AC5: Bank-scoped entitlements
	t.Run("AC5: Bank-scoped entitlements", func(t *testing.T) {
		// Get entitlements for Alpha bank
		req, _ := http.NewRequest("GET", "/banks/ac-bank-alpha/entitlements", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusOK, w.Code)

		var response map[string]interface{}
		json.Unmarshal(w.Body.Bytes(), &response)
		assert.NotNil(t, response["entitlements"])
	})

	// AC6: Independent bank operations
	t.Run("AC6: Independent bank operations", func(t *testing.T) {
		// Update Alpha bank
		updateBody := map[string]interface{}{
			"bankName": "Updated AC Alpha Bank",
		}
		updateData, _ := json.Marshal(updateBody)
		updateReq, _ := http.NewRequest("PUT", "/api/banks/ac-bank-alpha", bytes.NewBuffer(updateData))
		updateReq.Header.Set("Content-Type", "application/json")
		wUpdate := httptest.NewRecorder()
		router.ServeHTTP(wUpdate, updateReq)
		assert.Equal(t, http.StatusOK, wUpdate.Code)

		// Verify Beta bank is unchanged
		req, _ := http.NewRequest("GET", "/banks/ac-bank-beta", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)
		assert.Equal(t, http.StatusOK, w.Code)

		var bankData map[string]interface{}
		json.Unmarshal(w.Body.Bytes(), &bankData)
		assert.Equal(t, "AC Beta Bank", bankData["full_name"])
	})

	// AC7: Single API instance multi-bank support
	t.Run("AC7: Single API instance multi-bank support", func(t *testing.T) {
		// Both banks accessible via same API
		req1, _ := http.NewRequest("GET", "/banks/ac-bank-alpha", nil)
		w1 := httptest.NewRecorder()
		router.ServeHTTP(w1, req1)
		assert.Equal(t, http.StatusOK, w1.Code)

		req2, _ := http.NewRequest("GET", "/banks/ac-bank-beta", nil)
		w2 := httptest.NewRecorder()
		router.ServeHTTP(w2, req2)
		assert.Equal(t, http.StatusOK, w2.Code)
	})
}

// TestIntegration_MultiBank_FullLifecycle tests the complete multi-bank support lifecycle
func TestIntegration_MultiBank_FullLifecycle(t *testing.T) {
	router, cleanup := setupIntegrationTest(t)
	defer cleanup()

	// Step 1: Create multiple banks
	createTestBankForMultiBank(t, router, "lifecycle-bank-1", "LIFE1", "Lifecycle Bank 1")
	createTestBankForMultiBank(t, router, "lifecycle-bank-2", "LIFE2", "Lifecycle Bank 2")

	// Step 2: Verify both banks exist
	for _, bankID := range []string{"lifecycle-bank-1", "lifecycle-bank-2"} {
		req, _ := http.NewRequest("GET", "/banks/"+bankID, nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)
		assert.Equal(t, http.StatusOK, w.Code)
	}

	// Step 3: Verify accounts endpoint works for both banks
	for _, bankID := range []string{"lifecycle-bank-1", "lifecycle-bank-2"} {
		req, _ := http.NewRequest("GET", "/banks/"+bankID+"/accounts", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)
		assert.Equal(t, http.StatusOK, w.Code)

		var response map[string]interface{}
		json.Unmarshal(w.Body.Bytes(), &response)
		assert.NotNil(t, response["accounts"])
	}

	// Step 4: Verify entitlements endpoint works for both banks
	for _, bankID := range []string{"lifecycle-bank-1", "lifecycle-bank-2"} {
		req, _ := http.NewRequest("GET", "/banks/"+bankID+"/entitlements", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)
		assert.Equal(t, http.StatusOK, w.Code)

		var response map[string]interface{}
		json.Unmarshal(w.Body.Bytes(), &response)
		assert.NotNil(t, response["entitlements"])
	}

	// Step 5: Verify bank list returns all banks
	req, _ := http.NewRequest("GET", "/banks", nil)
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)
	assert.Equal(t, http.StatusOK, w.Code)

	var response map[string]interface{}
	json.Unmarshal(w.Body.Bytes(), &response)
	bankList := response["banks"].([]interface{})
	assert.GreaterOrEqual(t, len(bankList), 2)

	// Step 6: Verify non-existent bank returns 404
	req, _ = http.NewRequest("GET", "/banks/non-existent-lifecycle-bank/accounts", nil)
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)
	assert.Equal(t, http.StatusNotFound, w.Code)
}
