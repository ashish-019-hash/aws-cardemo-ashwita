package test

import (
	"context"
	"database/sql"
	"testing"
	"time"

	"github.com/obp-api/bank-registration/internal/models"
	"github.com/obp-api/bank-registration/internal/repositories"
	"github.com/obp-api/bank-registration/pkg/db"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// setupTestDB creates a new test database for each test
func setupTestDB(t *testing.T) *sql.DB {
	testDB, err := db.NewTestDB()
	require.NoError(t, err)
	return testDB
}

// createTestBank creates a test bank for use in tests
func createTestBank(permalink, shortName, fullName string) *models.MappedBank {
	return &models.MappedBank{
		Permalink:          permalink,
		FullBankName:       fullName,
		ShortBankName:      shortName,
		LogoURL:            "https://example.com/logo.png",
		WebsiteURL:         "https://example.com",
		SwiftBIC:           "TESTBIC1",
		NationalIdentifier: "NAT001",
		BankRoutingScheme:  "BIC",
		BankRoutingAddress: "TESTADDR",
	}
}

// TestBankRepository_CreateBank tests bank creation
func TestBankRepository_CreateBank(t *testing.T) {
	testDB := setupTestDB(t)
	defer testDB.Close()

	repo := repositories.NewBankRepository(testDB)
	ctx := context.Background()

	tests := []struct {
		name    string
		bank    *models.MappedBank
		wantErr bool
	}{
		{
			name:    "create valid bank",
			bank:    createTestBank("test-bank-001", "TESTBANK1", "Test Bank One"),
			wantErr: false,
		},
		{
			name:    "create bank with minimal fields",
			bank:    &models.MappedBank{Permalink: "minimal-bank", FullBankName: "Minimal", ShortBankName: "MIN"},
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := repo.CreateBank(ctx, tt.bank)
			if tt.wantErr {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				assert.NotZero(t, tt.bank.ID)
				assert.False(t, tt.bank.CreatedAt.IsZero())
				assert.False(t, tt.bank.UpdatedAt.IsZero())
			}
		})
	}
}

// TestBankRepository_CreateBank_DuplicatePermalink tests duplicate permalink rejection
func TestBankRepository_CreateBank_DuplicatePermalink(t *testing.T) {
	testDB := setupTestDB(t)
	defer testDB.Close()

	repo := repositories.NewBankRepository(testDB)
	ctx := context.Background()

	// Create first bank
	bank1 := createTestBank("duplicate-test", "BANK1", "Bank One")
	err := repo.CreateBank(ctx, bank1)
	require.NoError(t, err)

	// Try to create second bank with same permalink
	bank2 := createTestBank("duplicate-test", "BANK2", "Bank Two")
	err = repo.CreateBank(ctx, bank2)
	assert.Error(t, err) // Should fail due to unique constraint
}

// TestBankRepository_CreateBank_DuplicateCode tests duplicate code rejection
func TestBankRepository_CreateBank_DuplicateCode(t *testing.T) {
	testDB := setupTestDB(t)
	defer testDB.Close()

	repo := repositories.NewBankRepository(testDB)
	ctx := context.Background()

	// Create first bank
	bank1 := createTestBank("bank-001", "DUPCODE", "Bank One")
	err := repo.CreateBank(ctx, bank1)
	require.NoError(t, err)

	// Try to create second bank with same code
	bank2 := createTestBank("bank-002", "DUPCODE", "Bank Two")
	err = repo.CreateBank(ctx, bank2)
	assert.Error(t, err) // Should fail due to unique constraint
}

// TestBankRepository_GetBankByPermalink tests bank retrieval by permalink
func TestBankRepository_GetBankByPermalink(t *testing.T) {
	testDB := setupTestDB(t)
	defer testDB.Close()

	repo := repositories.NewBankRepository(testDB)
	ctx := context.Background()

	// Create a bank first
	originalBank := createTestBank("get-test-bank", "GETTEST", "Get Test Bank")
	err := repo.CreateBank(ctx, originalBank)
	require.NoError(t, err)

	tests := []struct {
		name      string
		permalink string
		wantErr   bool
		errType   error
	}{
		{
			name:      "get existing bank",
			permalink: "get-test-bank",
			wantErr:   false,
		},
		{
			name:      "get non-existent bank",
			permalink: "non-existent-bank",
			wantErr:   true,
			errType:   sql.ErrNoRows,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			bank, err := repo.GetBankByPermalink(ctx, tt.permalink)
			if tt.wantErr {
				assert.Error(t, err)
				assert.Nil(t, bank)
			} else {
				assert.NoError(t, err)
				assert.NotNil(t, bank)
				assert.Equal(t, tt.permalink, bank.Permalink)
				assert.Equal(t, originalBank.FullBankName, bank.FullBankName)
				assert.Equal(t, originalBank.ShortBankName, bank.ShortBankName)
			}
		})
	}
}

// TestBankRepository_GetBankByCode tests bank retrieval by code
func TestBankRepository_GetBankByCode(t *testing.T) {
	testDB := setupTestDB(t)
	defer testDB.Close()

	repo := repositories.NewBankRepository(testDB)
	ctx := context.Background()

	// Create a bank first
	originalBank := createTestBank("code-test-bank", "CODETEST", "Code Test Bank")
	err := repo.CreateBank(ctx, originalBank)
	require.NoError(t, err)

	tests := []struct {
		name    string
		code    string
		wantErr bool
	}{
		{
			name:    "get existing bank by code",
			code:    "CODETEST",
			wantErr: false,
		},
		{
			name:    "get non-existent bank by code",
			code:    "NONEXIST",
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			bank, err := repo.GetBankByCode(ctx, tt.code)
			if tt.wantErr {
				assert.Error(t, err)
				assert.Nil(t, bank)
			} else {
				assert.NoError(t, err)
				assert.NotNil(t, bank)
				assert.Equal(t, tt.code, bank.ShortBankName)
			}
		})
	}
}

// TestBankRepository_UpdateBank tests bank update
func TestBankRepository_UpdateBank(t *testing.T) {
	testDB := setupTestDB(t)
	defer testDB.Close()

	repo := repositories.NewBankRepository(testDB)
	ctx := context.Background()

	// Create a bank first
	bank := createTestBank("update-test-bank", "UPDATETEST", "Update Test Bank")
	err := repo.CreateBank(ctx, bank)
	require.NoError(t, err)

	originalUpdatedAt := bank.UpdatedAt

	// Wait a bit to ensure timestamp difference
	time.Sleep(10 * time.Millisecond)

	// Update the bank
	bank.FullBankName = "Updated Bank Name"
	bank.LogoURL = "https://new-logo.url"

	err = repo.UpdateBank(ctx, bank)
	assert.NoError(t, err)

	// Verify update
	updatedBank, err := repo.GetBankByPermalink(ctx, "update-test-bank")
	require.NoError(t, err)

	assert.Equal(t, "Updated Bank Name", updatedBank.FullBankName)
	assert.Equal(t, "https://new-logo.url", updatedBank.LogoURL)
	assert.True(t, updatedBank.UpdatedAt.After(originalUpdatedAt) || updatedBank.UpdatedAt.Equal(originalUpdatedAt))
}

// TestBankRepository_BankExistsByPermalink tests existence check by permalink
func TestBankRepository_BankExistsByPermalink(t *testing.T) {
	testDB := setupTestDB(t)
	defer testDB.Close()

	repo := repositories.NewBankRepository(testDB)
	ctx := context.Background()

	// Create a bank first
	bank := createTestBank("exists-test-bank", "EXISTSTEST", "Exists Test Bank")
	err := repo.CreateBank(ctx, bank)
	require.NoError(t, err)

	tests := []struct {
		name      string
		permalink string
		exists    bool
	}{
		{
			name:      "existing bank",
			permalink: "exists-test-bank",
			exists:    true,
		},
		{
			name:      "non-existent bank",
			permalink: "non-existent-bank",
			exists:    false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			exists, err := repo.BankExistsByPermalink(ctx, tt.permalink)
			assert.NoError(t, err)
			assert.Equal(t, tt.exists, exists)
		})
	}
}

// TestBankRepository_BankExistsByCode tests existence check by code
func TestBankRepository_BankExistsByCode(t *testing.T) {
	testDB := setupTestDB(t)
	defer testDB.Close()

	repo := repositories.NewBankRepository(testDB)
	ctx := context.Background()

	// Create a bank first
	bank := createTestBank("code-exists-bank", "CODEEXISTS", "Code Exists Bank")
	err := repo.CreateBank(ctx, bank)
	require.NoError(t, err)

	tests := []struct {
		name   string
		code   string
		exists bool
	}{
		{
			name:   "existing code",
			code:   "CODEEXISTS",
			exists: true,
		},
		{
			name:   "non-existent code",
			code:   "NONEXIST",
			exists: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			exists, err := repo.BankExistsByCode(ctx, tt.code)
			assert.NoError(t, err)
			assert.Equal(t, tt.exists, exists)
		})
	}
}

// TestBankRepository_AllFieldsPersisted tests that all fields are correctly persisted
func TestBankRepository_AllFieldsPersisted(t *testing.T) {
	testDB := setupTestDB(t)
	defer testDB.Close()

	repo := repositories.NewBankRepository(testDB)
	ctx := context.Background()

	// Create bank with all fields
	bank := &models.MappedBank{
		Permalink:          "all-fields-bank",
		FullBankName:       "All Fields Bank",
		ShortBankName:      "ALLFIELDS",
		LogoURL:            "https://logo.url",
		WebsiteURL:         "https://website.url",
		SwiftBIC:           "SWIFTBIC1",
		NationalIdentifier: "NATID001",
		BankRoutingScheme:  "SCHEME1",
		BankRoutingAddress: "ADDRESS1",
	}

	err := repo.CreateBank(ctx, bank)
	require.NoError(t, err)

	// Retrieve and verify all fields
	retrieved, err := repo.GetBankByPermalink(ctx, "all-fields-bank")
	require.NoError(t, err)

	assert.Equal(t, bank.Permalink, retrieved.Permalink)
	assert.Equal(t, bank.FullBankName, retrieved.FullBankName)
	assert.Equal(t, bank.ShortBankName, retrieved.ShortBankName)
	assert.Equal(t, bank.LogoURL, retrieved.LogoURL)
	assert.Equal(t, bank.WebsiteURL, retrieved.WebsiteURL)
	assert.Equal(t, bank.SwiftBIC, retrieved.SwiftBIC)
	assert.Equal(t, bank.NationalIdentifier, retrieved.NationalIdentifier)
	assert.Equal(t, bank.BankRoutingScheme, retrieved.BankRoutingScheme)
	assert.Equal(t, bank.BankRoutingAddress, retrieved.BankRoutingAddress)
}

// TestBankRepository_ConcurrentCreation tests concurrent bank creation
func TestBankRepository_ConcurrentCreation(t *testing.T) {
	testDB := setupTestDB(t)
	defer testDB.Close()

	repo := repositories.NewBankRepository(testDB)
	ctx := context.Background()

	// Create multiple banks concurrently
	numBanks := 10
	errChan := make(chan error, numBanks)

	for i := 0; i < numBanks; i++ {
		go func(idx int) {
			bank := createTestBank(
				"concurrent-bank-"+string(rune('A'+idx)),
				"CONC"+string(rune('A'+idx)),
				"Concurrent Bank "+string(rune('A'+idx)),
			)
			errChan <- repo.CreateBank(ctx, bank)
		}(i)
	}

	// Collect results
	successCount := 0
	for i := 0; i < numBanks; i++ {
		err := <-errChan
		if err == nil {
			successCount++
		}
	}

	// All should succeed since they have unique identifiers
	assert.Equal(t, numBanks, successCount)
}
