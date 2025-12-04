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

// setupTestRouter creates a test router with all dependencies
func setupTestRouter(t *testing.T) (*gin.Engine, func()) {
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

// createTestCreateBankJSON creates JSON for CreateBankRequest
func createTestCreateBankJSON(bankID, bankCode, bankName string) []byte {
	req := map[string]interface{}{
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
	data, _ := json.Marshal(req)
	return data
}

// TestHealthEndpoint tests GET /health
func TestHealthEndpoint(t *testing.T) {
	router, cleanup := setupTestRouter(t)
	defer cleanup()

	req, _ := http.NewRequest("GET", "/health", nil)
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	assert.Equal(t, http.StatusOK, w.Code)

	var response map[string]string
	err := json.Unmarshal(w.Body.Bytes(), &response)
	assert.NoError(t, err)
	assert.Equal(t, "healthy", response["status"])
}

// TestCreateBank_Success tests POST /api/banks with valid request
func TestCreateBank_Success(t *testing.T) {
	router, cleanup := setupTestRouter(t)
	defer cleanup()

	body := createTestCreateBankJSON("test-bank-001", "TESTBANK", "Test Bank")
	req, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(body))
	req.Header.Set("Content-Type", "application/json")

	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	assert.Equal(t, http.StatusCreated, w.Code)

	var response models.CreateBankResponse
	err := json.Unmarshal(w.Body.Bytes(), &response)
	assert.NoError(t, err)
	assert.Equal(t, "test-bank-001", response.BankID)
	assert.Equal(t, "created", response.Status)
}

// TestCreateBank_MissingBankID tests POST /api/banks with missing bank ID
func TestCreateBank_MissingBankID(t *testing.T) {
	router, cleanup := setupTestRouter(t)
	defer cleanup()

	body := map[string]interface{}{
		"bankCode": "TESTBANK",
		"bankName": "Test Bank",
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

	assert.Equal(t, http.StatusBadRequest, w.Code)

	var response models.ErrorResponse
	err := json.Unmarshal(w.Body.Bytes(), &response)
	assert.NoError(t, err)
	assert.Equal(t, "BANK-VAL-001", response.Code)
}

// TestCreateBank_MissingBankCode tests POST /api/banks with missing bank code
func TestCreateBank_MissingBankCode(t *testing.T) {
	router, cleanup := setupTestRouter(t)
	defer cleanup()

	body := map[string]interface{}{
		"bankId":   "test-bank-001",
		"bankName": "Test Bank",
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

	assert.Equal(t, http.StatusBadRequest, w.Code)

	var response models.ErrorResponse
	err := json.Unmarshal(w.Body.Bytes(), &response)
	assert.NoError(t, err)
	assert.Equal(t, "BANK-VAL-002", response.Code)
}

// TestCreateBank_MissingBankName tests POST /api/banks with missing bank name
func TestCreateBank_MissingBankName(t *testing.T) {
	router, cleanup := setupTestRouter(t)
	defer cleanup()

	body := map[string]interface{}{
		"bankId":   "test-bank-001",
		"bankCode": "TESTBANK",
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

	assert.Equal(t, http.StatusBadRequest, w.Code)

	var response models.ErrorResponse
	err := json.Unmarshal(w.Body.Bytes(), &response)
	assert.NoError(t, err)
	assert.Equal(t, "BANK-VAL-003", response.Code)
}

// TestCreateBank_DuplicateBankID tests POST /api/banks with duplicate bank ID (409 Conflict)
func TestCreateBank_DuplicateBankID(t *testing.T) {
	router, cleanup := setupTestRouter(t)
	defer cleanup()

	// Create first bank
	body1 := createTestCreateBankJSON("duplicate-bank", "BANK001", "Bank One")
	req1, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(body1))
	req1.Header.Set("Content-Type", "application/json")

	w1 := httptest.NewRecorder()
	router.ServeHTTP(w1, req1)
	assert.Equal(t, http.StatusCreated, w1.Code)

	// Try to create second bank with same ID
	body2 := createTestCreateBankJSON("duplicate-bank", "BANK002", "Bank Two")
	req2, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(body2))
	req2.Header.Set("Content-Type", "application/json")

	w2 := httptest.NewRecorder()
	router.ServeHTTP(w2, req2)

	assert.Equal(t, http.StatusConflict, w2.Code)

	var response models.ErrorResponse
	err := json.Unmarshal(w2.Body.Bytes(), &response)
	assert.NoError(t, err)
	assert.Equal(t, "BANK-VAL-007", response.Code)
}

// TestCreateBank_DuplicateBankCode tests POST /api/banks with duplicate bank code (409 Conflict)
func TestCreateBank_DuplicateBankCode(t *testing.T) {
	router, cleanup := setupTestRouter(t)
	defer cleanup()

	// Create first bank
	body1 := createTestCreateBankJSON("bank-001", "DUPCODE", "Bank One")
	req1, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(body1))
	req1.Header.Set("Content-Type", "application/json")

	w1 := httptest.NewRecorder()
	router.ServeHTTP(w1, req1)
	assert.Equal(t, http.StatusCreated, w1.Code)

	// Try to create second bank with same code
	body2 := createTestCreateBankJSON("bank-002", "DUPCODE", "Bank Two")
	req2, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(body2))
	req2.Header.Set("Content-Type", "application/json")

	w2 := httptest.NewRecorder()
	router.ServeHTTP(w2, req2)

	assert.Equal(t, http.StatusConflict, w2.Code)

	var response models.ErrorResponse
	err := json.Unmarshal(w2.Body.Bytes(), &response)
	assert.NoError(t, err)
	assert.Equal(t, "BANK-VAL-008", response.Code)
}

// TestCreateBank_InvalidBankIDFormat tests POST /api/banks with invalid bank ID format
func TestCreateBank_InvalidBankIDFormat(t *testing.T) {
	router, cleanup := setupTestRouter(t)
	defer cleanup()

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
			body := createTestCreateBankJSON(tt.bankID, "TESTBANK", "Test Bank")
			req, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(body))
			req.Header.Set("Content-Type", "application/json")

			w := httptest.NewRecorder()
			router.ServeHTTP(w, req)

			assert.Equal(t, http.StatusBadRequest, w.Code)

			var response models.ErrorResponse
			err := json.Unmarshal(w.Body.Bytes(), &response)
			assert.NoError(t, err)
			assert.Equal(t, "BANK-VAL-010", response.Code)
		})
	}
}

// TestCreateBank_InvalidBankCodeFormat tests POST /api/banks with invalid bank code format
func TestCreateBank_InvalidBankCodeFormat(t *testing.T) {
	router, cleanup := setupTestRouter(t)
	defer cleanup()

	tests := []struct {
		name     string
		bankCode string
	}{
		{"lowercase", "lowercase"},
		{"mixed case", "MixedCase"},
		{"contains special chars", "BANK-CODE"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			body := createTestCreateBankJSON("valid-bank-id", tt.bankCode, "Test Bank")
			req, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(body))
			req.Header.Set("Content-Type", "application/json")

			w := httptest.NewRecorder()
			router.ServeHTTP(w, req)

			assert.Equal(t, http.StatusBadRequest, w.Code)

			var response models.ErrorResponse
			err := json.Unmarshal(w.Body.Bytes(), &response)
			assert.NoError(t, err)
			assert.Equal(t, "BANK-VAL-011", response.Code)
		})
	}
}

// TestCreateBank_InvalidJSON tests POST /api/banks with invalid JSON
func TestCreateBank_InvalidJSON(t *testing.T) {
	router, cleanup := setupTestRouter(t)
	defer cleanup()

	req, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer([]byte("invalid json")))
	req.Header.Set("Content-Type", "application/json")

	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	assert.Equal(t, http.StatusBadRequest, w.Code)
}

// TestUpdateBank_Success tests PUT /api/banks/:bankId with valid request
func TestUpdateBank_Success(t *testing.T) {
	router, cleanup := setupTestRouter(t)
	defer cleanup()

	// Create bank first
	createBody := createTestCreateBankJSON("update-test-bank", "UPDATETEST", "Update Test Bank")
	createReq, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(createBody))
	createReq.Header.Set("Content-Type", "application/json")

	w1 := httptest.NewRecorder()
	router.ServeHTTP(w1, createReq)
	assert.Equal(t, http.StatusCreated, w1.Code)

	// Update bank
	updateBody := map[string]interface{}{
		"bankName": "Updated Bank Name",
	}
	data, _ := json.Marshal(updateBody)

	updateReq, _ := http.NewRequest("PUT", "/api/banks/update-test-bank", bytes.NewBuffer(data))
	updateReq.Header.Set("Content-Type", "application/json")

	w2 := httptest.NewRecorder()
	router.ServeHTTP(w2, updateReq)

	assert.Equal(t, http.StatusOK, w2.Code)

	var response models.UpdateBankResponse
	err := json.Unmarshal(w2.Body.Bytes(), &response)
	assert.NoError(t, err)
	assert.Equal(t, "update-test-bank", response.BankID)
	assert.Equal(t, "updated", response.Status)
}

// TestUpdateBank_NotFound tests PUT /api/banks/:bankId with non-existent bank (404)
func TestUpdateBank_NotFound(t *testing.T) {
	router, cleanup := setupTestRouter(t)
	defer cleanup()

	updateBody := map[string]interface{}{
		"bankName": "Updated Bank Name",
	}
	data, _ := json.Marshal(updateBody)

	req, _ := http.NewRequest("PUT", "/api/banks/non-existent-bank", bytes.NewBuffer(data))
	req.Header.Set("Content-Type", "application/json")

	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	assert.Equal(t, http.StatusNotFound, w.Code)

	var response models.ErrorResponse
	err := json.Unmarshal(w.Body.Bytes(), &response)
	assert.NoError(t, err)
	assert.Equal(t, "BANK-VAL-009", response.Code)
}

// TestUpdateBank_NoFieldsProvided tests PUT /api/banks/:bankId with no fields (VR-015)
func TestUpdateBank_NoFieldsProvided(t *testing.T) {
	router, cleanup := setupTestRouter(t)
	defer cleanup()

	// Create bank first
	createBody := createTestCreateBankJSON("no-fields-bank", "NOFIELDS", "No Fields Bank")
	createReq, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(createBody))
	createReq.Header.Set("Content-Type", "application/json")

	w1 := httptest.NewRecorder()
	router.ServeHTTP(w1, createReq)
	assert.Equal(t, http.StatusCreated, w1.Code)

	// Update with no fields
	updateBody := map[string]interface{}{}
	data, _ := json.Marshal(updateBody)

	updateReq, _ := http.NewRequest("PUT", "/api/banks/no-fields-bank", bytes.NewBuffer(data))
	updateReq.Header.Set("Content-Type", "application/json")

	w2 := httptest.NewRecorder()
	router.ServeHTTP(w2, updateReq)

	assert.Equal(t, http.StatusBadRequest, w2.Code)

	var response models.ErrorResponse
	err := json.Unmarshal(w2.Body.Bytes(), &response)
	assert.NoError(t, err)
	assert.Equal(t, "BANK-VAL-015", response.Code)
}

// TestGetBank_Success tests GET /banks/:bankId with existing bank
// Note: Using /banks/:bankId from Bank Information Retrieval user story
func TestGetBank_Success(t *testing.T) {
	router, cleanup := setupTestRouter(t)
	defer cleanup()

	// Create bank first
	createBody := createTestCreateBankJSON("get-test-bank", "GETTEST", "Get Test Bank")
	createReq, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(createBody))
	createReq.Header.Set("Content-Type", "application/json")

	w1 := httptest.NewRecorder()
	router.ServeHTTP(w1, createReq)
	assert.Equal(t, http.StatusCreated, w1.Code)

	// Get bank using /banks/:bankId from Bank Information Retrieval user story
	getReq, _ := http.NewRequest("GET", "/banks/get-test-bank", nil)

	w2 := httptest.NewRecorder()
	router.ServeHTTP(w2, getReq)

	assert.Equal(t, http.StatusOK, w2.Code)

	var response map[string]interface{}
	err := json.Unmarshal(w2.Body.Bytes(), &response)
	assert.NoError(t, err)
	assert.Equal(t, "get-test-bank", response["id"])
	assert.Equal(t, "Get Test Bank", response["full_name"])
}

// TestGetBank_NotFound tests GET /banks/:bankId with non-existent bank
// Note: Using /banks/:bankId from Bank Information Retrieval user story
func TestGetBank_NotFound(t *testing.T) {
	router, cleanup := setupTestRouter(t)
	defer cleanup()

	req, _ := http.NewRequest("GET", "/banks/non-existent-bank", nil)

	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	assert.Equal(t, http.StatusNotFound, w.Code)
}

// TestCreateBank_WithBranding tests POST /api/banks with branding information
func TestCreateBank_WithBranding(t *testing.T) {
	router, cleanup := setupTestRouter(t)
	defer cleanup()

	body := map[string]interface{}{
		"bankId":   "branding-test-bank",
		"bankCode": "BRANDTEST",
		"bankName": "Branding Test Bank",
		"branding": map[string]interface{}{
			"logo":   "https://example.com/logo.png",
			"colors": "#FF5733",
		},
		"operationalParams": map[string]interface{}{
			"businessHours": "9-5",
			"limits":        map[string]interface{}{"daily": 10000},
			"currencies":    []string{"USD", "EUR"},
		},
	}
	data, _ := json.Marshal(body)

	req, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(data))
	req.Header.Set("Content-Type", "application/json")

	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	assert.Equal(t, http.StatusCreated, w.Code)
}

// TestUpdateBank_WithBranding tests PUT /api/banks/:bankId with branding update
func TestUpdateBank_WithBranding(t *testing.T) {
	router, cleanup := setupTestRouter(t)
	defer cleanup()

	// Create bank first
	createBody := createTestCreateBankJSON("branding-update-bank", "BRANDUPD", "Branding Update Bank")
	createReq, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(createBody))
	createReq.Header.Set("Content-Type", "application/json")

	w1 := httptest.NewRecorder()
	router.ServeHTTP(w1, createReq)
	assert.Equal(t, http.StatusCreated, w1.Code)

	// Update branding
	updateBody := map[string]interface{}{
		"branding": map[string]interface{}{
			"logo":   "https://new-logo.url",
			"colors": "#000000",
		},
	}
	data, _ := json.Marshal(updateBody)

	updateReq, _ := http.NewRequest("PUT", "/api/banks/branding-update-bank", bytes.NewBuffer(data))
	updateReq.Header.Set("Content-Type", "application/json")

	w2 := httptest.NewRecorder()
	router.ServeHTTP(w2, updateReq)

	assert.Equal(t, http.StatusOK, w2.Code)
}

// TestUpdateBank_WithOperationalParams tests PUT /api/banks/:bankId with operational params update
func TestUpdateBank_WithOperationalParams(t *testing.T) {
	router, cleanup := setupTestRouter(t)
	defer cleanup()

	// Create bank first
	createBody := createTestCreateBankJSON("ops-update-bank", "OPSUPD", "Ops Update Bank")
	createReq, _ := http.NewRequest("POST", "/api/banks", bytes.NewBuffer(createBody))
	createReq.Header.Set("Content-Type", "application/json")

	w1 := httptest.NewRecorder()
	router.ServeHTTP(w1, createReq)
	assert.Equal(t, http.StatusCreated, w1.Code)

	// Update operational params
	updateBody := map[string]interface{}{
		"operationalParams": map[string]interface{}{
			"businessHours": "8-6",
			"limits":        map[string]interface{}{"daily": 20000},
			"currencies":    []string{"USD", "EUR", "GBP"},
		},
	}
	data, _ := json.Marshal(updateBody)

	updateReq, _ := http.NewRequest("PUT", "/api/banks/ops-update-bank", bytes.NewBuffer(data))
	updateReq.Header.Set("Content-Type", "application/json")

	w2 := httptest.NewRecorder()
	router.ServeHTTP(w2, updateReq)

	assert.Equal(t, http.StatusOK, w2.Code)
}
