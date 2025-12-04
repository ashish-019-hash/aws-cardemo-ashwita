package services

import (
	"context"
	"database/sql"
	"errors"

	"github.com/obp-api/bank-registration/internal/models"
	"github.com/obp-api/bank-registration/internal/repositories"
	"github.com/obp-api/bank-registration/internal/validators"
)

// BankService handles creation and management of Bank entities
// Maps to: User Story "BankService: Handles creation and management of Bank entities"
type BankService struct {
	repo      repositories.BankRepository
	validator *validators.BankValidator
}

// NewBankService creates a new BankService instance
func NewBankService(repo repositories.BankRepository) *BankService {
	return &BankService{
		repo:      repo,
		validator: validators.NewBankValidator(),
	}
}

// ServiceError represents a service-level error with code and HTTP status
type ServiceError struct {
	Code       string
	Message    string
	HTTPStatus int
	Field      string
}

func (e *ServiceError) Error() string {
	return e.Message
}

// CreateBank creates a new Bank entity
// Maps to: POST /api/banks
// Implements: BR-001 (Unique Identification), BR-002 (Required Fields)
// Source: NewStyle.function.createOrUpdateBank (code/api/util/NewStyle.scala:310)
func (s *BankService) CreateBank(ctx context.Context, req *models.CreateBankRequest) (*models.CreateBankResponse, error) {
	// BR-002: Validate required fields
	validationResults := s.validator.ValidateCreateBankRequest(req)
	if len(validationResults) > 0 {
		result := validationResults[0]
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 400,
			Field:      result.Field,
		}
	}

	// BR-001: Check unique identification - Bank ID
	exists, err := s.repo.BankExistsByPermalink(ctx, req.BankID)
	if err != nil {
		return nil, &ServiceError{
			Code:       "BANK-ERR-001",
			Message:    "Failed to check bank existence",
			HTTPStatus: 500,
		}
	}
	if exists {
		result := validators.NewBankIDUniqueError(req.BankID)
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 409,
			Field:      result.Field,
		}
	}

	// BR-001: Check unique identification - Bank Code
	exists, err = s.repo.BankExistsByCode(ctx, req.BankCode)
	if err != nil {
		return nil, &ServiceError{
			Code:       "BANK-ERR-001",
			Message:    "Failed to check bank code existence",
			HTTPStatus: 500,
		}
	}
	if exists {
		result := validators.NewBankCodeUniqueError(req.BankCode)
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 409,
			Field:      result.Field,
		}
	}

	// Create bank entity from request
	bank := &models.MappedBank{
		Permalink:     req.BankID,
		FullBankName:  req.BankName,
		ShortBankName: req.BankCode,
		LogoURL:       req.Branding.Logo,
		WebsiteURL:    req.Branding.Colors, // Using colors field for website as per user story mapping
	}

	// Persist via repository
	if err := s.repo.CreateBank(ctx, bank); err != nil {
		return nil, &ServiceError{
			Code:       "BANK-ERR-002",
			Message:    "Failed to create bank: " + err.Error(),
			HTTPStatus: 500,
		}
	}

	return &models.CreateBankResponse{
		BankID: bank.Permalink,
		Status: "created",
	}, nil
}

// UpdateBank updates an existing Bank entity
// Maps to: PUT /api/banks/{bankId}
// Implements: BR-003 (Valid Updates - only existing banks can be updated)
// Source: APIMethods500.updateBank (code/api/v5_0_0/APIMethods500.scala:262)
func (s *BankService) UpdateBank(ctx context.Context, bankID string, req *models.UpdateBankRequest) (*models.UpdateBankResponse, error) {
	// VR-006: Validate bank ID path parameter
	if result := s.validator.ValidateBankIDPathRequired(bankID); !result.Valid {
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 400,
			Field:      result.Field,
		}
	}

	// VR-010: Validate bank ID format
	if result := s.validator.ValidateBankIDFormat(bankID); !result.Valid {
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 400,
			Field:      result.Field,
		}
	}

	// Validate update request
	validationResults := s.validator.ValidateUpdateBankRequest(req)
	if len(validationResults) > 0 {
		result := validationResults[0]
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 400,
			Field:      result.Field,
		}
	}

	// BR-003: Verify bank exists before update
	existing, err := s.repo.GetBankByPermalink(ctx, bankID)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			result := validators.NewBankNotFoundError(bankID)
			return nil, &ServiceError{
				Code:       result.Code,
				Message:    result.Message,
				HTTPStatus: 404,
				Field:      result.Field,
			}
		}
		return nil, &ServiceError{
			Code:       "BANK-ERR-001",
			Message:    "Failed to retrieve bank",
			HTTPStatus: 500,
		}
	}

	// Update fields if provided
	if req.BankName != nil {
		existing.FullBankName = *req.BankName
	}
	if req.Branding != nil {
		if req.Branding.Logo != "" {
			existing.LogoURL = req.Branding.Logo
		}
		if req.Branding.Colors != "" {
			existing.WebsiteURL = req.Branding.Colors
		}
	}

	// Persist via repository
	if err := s.repo.UpdateBank(ctx, existing); err != nil {
		return nil, &ServiceError{
			Code:       "BANK-ERR-003",
			Message:    "Failed to update bank: " + err.Error(),
			HTTPStatus: 500,
		}
	}

	return &models.UpdateBankResponse{
		BankID: existing.Permalink,
		Status: "updated",
	}, nil
}

// GetBank retrieves a bank by ID (internal helper)
func (s *BankService) GetBank(ctx context.Context, bankID string) (*models.MappedBank, error) {
	bank, err := s.repo.GetBankByPermalink(ctx, bankID)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			result := validators.NewBankNotFoundError(bankID)
			return nil, &ServiceError{
				Code:       result.Code,
				Message:    result.Message,
				HTTPStatus: 404,
				Field:      result.Field,
			}
		}
		return nil, &ServiceError{
			Code:       "BANK-ERR-001",
			Message:    "Failed to retrieve bank",
			HTTPStatus: 500,
		}
	}
	return bank, nil
}
