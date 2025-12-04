package repositories

import (
	"context"
	"database/sql"
	"time"

	"github.com/obp-api/bank-registration/internal/models"
)

// BankRepository handles data persistence for Bank entities
// Maps to: User Story "BankRepository: Data persistence for Bank entities"
type BankRepository interface {
	// CreateBank creates a new bank in the database
	// Returns error if bank with same permalink or shortbankname already exists
	CreateBank(ctx context.Context, bank *models.MappedBank) error

	// UpdateBank updates an existing bank in the database
	UpdateBank(ctx context.Context, bank *models.MappedBank) error

	// GetBankByPermalink retrieves a bank by its permalink (bank ID)
	// Source: MappedBank.findByBankId (code/model/dataAccess/MappedBank.scala:37)
	GetBankByPermalink(ctx context.Context, permalink string) (*models.MappedBank, error)

	// GetBankByCode retrieves a bank by its short name/code
	GetBankByCode(ctx context.Context, code string) (*models.MappedBank, error)

	// BankExistsByPermalink checks if a bank exists by permalink
	BankExistsByPermalink(ctx context.Context, permalink string) (bool, error)

	// BankExistsByCode checks if a bank exists by code
	BankExistsByCode(ctx context.Context, code string) (bool, error)

	// ============================================================================
	// Bank Information Retrieval Methods
	// User Story: Bank Information Retrieval
	// ============================================================================

	// GetAllBanks retrieves all banks from the database
	// Source: LocalMappedConnector.getBanks (code/bankconnectors/LocalMappedConnector.scala)
	// BR-003: Returns basic bank info (excludes attributes for performance)
	// BR-004: Returns empty slice if no banks exist (not error)
	GetAllBanks(ctx context.Context) ([]*models.MappedBank, error)

	// GetBankAttributes retrieves all attributes for a specific bank
	// Source: MappedBankAttributeProvider.getBankAttributesByBank
	// VR-007: Returns empty slice if no attributes exist (not nil)
	GetBankAttributes(ctx context.Context, bankID string) ([]*models.BankAttribute, error)

	// CreateBankAttribute creates a new bank attribute
	// Used for testing and seeding data
	CreateBankAttribute(ctx context.Context, attr *models.BankAttribute) error
}

// bankRepository implements BankRepository
// Source: code/bankconnectors/LocalMappedConnector.scala:3169
type bankRepository struct {
	db *sql.DB
}

// NewBankRepository creates a new BankRepository instance
func NewBankRepository(db *sql.DB) BankRepository {
	return &bankRepository{db: db}
}

// CreateBank creates a new bank in the database
// Source: LocalMappedConnector.createOrUpdateBank (code/bankconnectors/LocalMappedConnector.scala:3169)
func (r *bankRepository) CreateBank(ctx context.Context, bank *models.MappedBank) error {
	now := time.Now()
	bank.CreatedAt = now
	bank.UpdatedAt = now

	query := `
		INSERT INTO mappedbank (
			permalink, fullbankname, shortbankname, logourl, websiteurl,
			swiftbic, national_identifier, mbankroutingscheme, mbankroutingaddress,
			createdat, updatedat
		) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
	`
	result, err := r.db.ExecContext(ctx, query,
		bank.Permalink,
		bank.FullBankName,
		bank.ShortBankName,
		bank.LogoURL,
		bank.WebsiteURL,
		bank.SwiftBIC,
		bank.NationalIdentifier,
		bank.BankRoutingScheme,
		bank.BankRoutingAddress,
		bank.CreatedAt,
		bank.UpdatedAt,
	)
	if err != nil {
		return err
	}

	id, err := result.LastInsertId()
	if err != nil {
		return err
	}
	bank.ID = id
	return nil
}

// UpdateBank updates an existing bank in the database
func (r *bankRepository) UpdateBank(ctx context.Context, bank *models.MappedBank) error {
	bank.UpdatedAt = time.Now()

	query := `
		UPDATE mappedbank SET
			fullbankname = ?,
			shortbankname = ?,
			logourl = ?,
			websiteurl = ?,
			swiftbic = ?,
			national_identifier = ?,
			mbankroutingscheme = ?,
			mbankroutingaddress = ?,
			updatedat = ?
		WHERE permalink = ?
	`
	_, err := r.db.ExecContext(ctx, query,
		bank.FullBankName,
		bank.ShortBankName,
		bank.LogoURL,
		bank.WebsiteURL,
		bank.SwiftBIC,
		bank.NationalIdentifier,
		bank.BankRoutingScheme,
		bank.BankRoutingAddress,
		bank.UpdatedAt,
		bank.Permalink,
	)
	return err
}

// GetBankByPermalink retrieves a bank by its permalink (bank ID)
// Source: MappedBank.findByBankId (code/model/dataAccess/MappedBank.scala:37)
func (r *bankRepository) GetBankByPermalink(ctx context.Context, permalink string) (*models.MappedBank, error) {
	query := `
		SELECT id, permalink, fullbankname, shortbankname, logourl, websiteurl,
			   swiftbic, national_identifier, mbankroutingscheme, mbankroutingaddress,
			   createdat, updatedat
		FROM mappedbank
		WHERE permalink = ?
	`

	bank := &models.MappedBank{}
	err := r.db.QueryRowContext(ctx, query, permalink).Scan(
		&bank.ID,
		&bank.Permalink,
		&bank.FullBankName,
		&bank.ShortBankName,
		&bank.LogoURL,
		&bank.WebsiteURL,
		&bank.SwiftBIC,
		&bank.NationalIdentifier,
		&bank.BankRoutingScheme,
		&bank.BankRoutingAddress,
		&bank.CreatedAt,
		&bank.UpdatedAt,
	)
	if err != nil {
		return nil, err
	}
	return bank, nil
}

// GetBankByCode retrieves a bank by its short name/code
func (r *bankRepository) GetBankByCode(ctx context.Context, code string) (*models.MappedBank, error) {
	query := `
		SELECT id, permalink, fullbankname, shortbankname, logourl, websiteurl,
			   swiftbic, national_identifier, mbankroutingscheme, mbankroutingaddress,
			   createdat, updatedat
		FROM mappedbank
		WHERE shortbankname = ?
	`

	bank := &models.MappedBank{}
	err := r.db.QueryRowContext(ctx, query, code).Scan(
		&bank.ID,
		&bank.Permalink,
		&bank.FullBankName,
		&bank.ShortBankName,
		&bank.LogoURL,
		&bank.WebsiteURL,
		&bank.SwiftBIC,
		&bank.NationalIdentifier,
		&bank.BankRoutingScheme,
		&bank.BankRoutingAddress,
		&bank.CreatedAt,
		&bank.UpdatedAt,
	)
	if err != nil {
		return nil, err
	}
	return bank, nil
}

// BankExistsByPermalink checks if a bank exists by permalink
func (r *bankRepository) BankExistsByPermalink(ctx context.Context, permalink string) (bool, error) {
	query := `SELECT COUNT(*) FROM mappedbank WHERE permalink = ?`
	var count int
	err := r.db.QueryRowContext(ctx, query, permalink).Scan(&count)
	if err != nil {
		return false, err
	}
	return count > 0, nil
}

// BankExistsByCode checks if a bank exists by code
func (r *bankRepository) BankExistsByCode(ctx context.Context, code string) (bool, error) {
	query := `SELECT COUNT(*) FROM mappedbank WHERE shortbankname = ?`
	var count int
	err := r.db.QueryRowContext(ctx, query, code).Scan(&count)
	if err != nil {
		return false, err
	}
	return count > 0, nil
}

// ============================================================================
// Bank Information Retrieval Methods Implementation
// User Story: Bank Information Retrieval
// ============================================================================

// GetAllBanks retrieves all banks from the database
// Source: LocalMappedConnector.getBanks (code/bankconnectors/LocalMappedConnector.scala)
// BR-003: Returns basic bank info (excludes attributes for performance)
// BR-004: Returns empty slice if no banks exist (not error)
func (r *bankRepository) GetAllBanks(ctx context.Context) ([]*models.MappedBank, error) {
	query := `
		SELECT id, permalink, fullbankname, shortbankname, logourl, websiteurl,
			   swiftbic, national_identifier, mbankroutingscheme, mbankroutingaddress,
			   createdat, updatedat
		FROM mappedbank
		ORDER BY fullbankname ASC
	`

	rows, err := r.db.QueryContext(ctx, query)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	// BR-004: Initialize as empty slice, not nil
	banks := make([]*models.MappedBank, 0)

	for rows.Next() {
		bank := &models.MappedBank{}
		err := rows.Scan(
			&bank.ID,
			&bank.Permalink,
			&bank.FullBankName,
			&bank.ShortBankName,
			&bank.LogoURL,
			&bank.WebsiteURL,
			&bank.SwiftBIC,
			&bank.NationalIdentifier,
			&bank.BankRoutingScheme,
			&bank.BankRoutingAddress,
			&bank.CreatedAt,
			&bank.UpdatedAt,
		)
		if err != nil {
			return nil, err
		}
		banks = append(banks, bank)
	}

	if err := rows.Err(); err != nil {
		return nil, err
	}

	return banks, nil
}

// GetBankAttributes retrieves all attributes for a specific bank
// Source: MappedBankAttributeProvider.getBankAttributesByBank
// VR-007: Returns empty slice if no attributes exist (not nil)
func (r *bankRepository) GetBankAttributes(ctx context.Context, bankID string) ([]*models.BankAttribute, error) {
	query := `
		SELECT id, bankid, bankattributeid, name, type, value, isactive
		FROM bankattribute
		WHERE bankid = ?
		ORDER BY name ASC
	`

	rows, err := r.db.QueryContext(ctx, query, bankID)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	// VR-007: Initialize as empty slice, not nil
	attributes := make([]*models.BankAttribute, 0)

	for rows.Next() {
		attr := &models.BankAttribute{}
		err := rows.Scan(
			&attr.ID,
			&attr.BankID,
			&attr.BankAttributeID,
			&attr.Name,
			&attr.Type,
			&attr.Value,
			&attr.IsActive,
		)
		if err != nil {
			return nil, err
		}
		attributes = append(attributes, attr)
	}

	if err := rows.Err(); err != nil {
		return nil, err
	}

	return attributes, nil
}

// CreateBankAttribute creates a new bank attribute
// Used for testing and seeding data
func (r *bankRepository) CreateBankAttribute(ctx context.Context, attr *models.BankAttribute) error {
	query := `
		INSERT INTO bankattribute (bankid, bankattributeid, name, type, value, isactive)
		VALUES (?, ?, ?, ?, ?, ?)
	`
	result, err := r.db.ExecContext(ctx, query,
		attr.BankID,
		attr.BankAttributeID,
		attr.Name,
		attr.Type,
		attr.Value,
		attr.IsActive,
	)
	if err != nil {
		return err
	}

	id, err := result.LastInsertId()
	if err != nil {
		return err
	}
	attr.ID = id
	return nil
}
