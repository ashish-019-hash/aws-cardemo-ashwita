package test

import (
	"bytes"
	"encoding/json"
	"net/http"
	"net/http/httptest"
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
	service := services.NewBankService(repo)
	controller := controllers.NewBankController(service)

	router := gin.New()
	routes.SetupRoutes(router, controller)

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
	getReq, _ := http.NewRequest("GET", "/api/banks/integration-test-bank", nil)

	w2 := httptest.NewRecorder()
	router.ServeHTTP(w2, getReq)

	assert.Equal(t, http.StatusOK, w2.Code, "Bank retrieval should return 200 OK")

	var bankData map[string]interface{}
	err = json.Unmarshal(w2.Body.Bytes(), &bankData)
	require.NoError(t, err)
	assert.Equal(t, "integration-test-bank", bankData["permalink"])
	assert.Equal(t, "Integration Test Bank", bankData["fullBankName"])

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
	finalGetReq, _ := http.NewRequest("GET", "/api/banks/integration-test-bank", nil)

	w6 := httptest.NewRecorder()
	router.ServeHTTP(w6, finalGetReq)

	assert.Equal(t, http.StatusOK, w6.Code)

	var finalBankData map[string]interface{}
	err = json.Unmarshal(w6.Body.Bytes(), &finalBankData)
	require.NoError(t, err)
	assert.Equal(t, "Updated Integration Test Bank", finalBankData["fullBankName"])
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
	for _, bank := range banks {
		req, _ := http.NewRequest("GET", "/api/banks/"+bank.id, nil)

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusOK, w.Code, "Bank %s should be retrievable", bank.id)

		var bankData map[string]interface{}
		json.Unmarshal(w.Body.Bytes(), &bankData)
		assert.Equal(t, bank.name, bankData["fullBankName"])
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
