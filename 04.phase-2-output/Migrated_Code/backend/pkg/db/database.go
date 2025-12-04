package db

import (
	"database/sql"
	"sync"

	_ "github.com/mattn/go-sqlite3"
)

var (
	instance *sql.DB
	once     sync.Once
)

// GetDB returns the singleton database connection
// Uses SQLite in-memory mode as specified in the playbook
func GetDB() (*sql.DB, error) {
	var err error
	once.Do(func() {
		instance, err = sql.Open("sqlite3", ":memory:")
		if err != nil {
			return
		}
		err = initSchema(instance)
	})
	return instance, err
}

// initSchema creates the database schema
func initSchema(db *sql.DB) error {
	schema := `
	CREATE TABLE IF NOT EXISTS mappedbank (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		permalink TEXT NOT NULL UNIQUE,
		fullbankname TEXT NOT NULL,
		shortbankname TEXT NOT NULL UNIQUE,
		logourl TEXT,
		websiteurl TEXT,
		swiftbic TEXT,
		national_identifier TEXT,
		mbankroutingscheme TEXT,
		mbankroutingaddress TEXT,
		createdat DATETIME NOT NULL,
		updatedat DATETIME NOT NULL
	);

	CREATE INDEX IF NOT EXISTS idx_mappedbank_permalink ON mappedbank(permalink);
	CREATE INDEX IF NOT EXISTS idx_mappedbank_shortbankname ON mappedbank(shortbankname);
	`
	_, err := db.Exec(schema)
	return err
}

// NewTestDB creates a new in-memory database for testing
// Each test gets its own isolated database instance
func NewTestDB() (*sql.DB, error) {
	db, err := sql.Open("sqlite3", ":memory:")
	if err != nil {
		return nil, err
	}
	err = initSchema(db)
	if err != nil {
		db.Close()
		return nil, err
	}
	return db, nil
}

// CloseDB closes the database connection
func CloseDB() error {
	if instance != nil {
		return instance.Close()
	}
	return nil
}
