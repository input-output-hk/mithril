package pg

import (
	"context"
	"database/sql"
	"github.com/jackc/pgconn"
	"time"

	"github.com/input-output-hk/mithril/go-node/pkg/config"
	"github.com/jackc/pgx/v4"
	_ "github.com/jackc/pgx/v4/stdlib"
	migrate "github.com/rubenv/sql-migrate"
)

type Querier interface {
	Query(ctx context.Context, sql string, args ...interface{}) (pgx.Rows, error)
	QueryRow(ctx context.Context, sql string, args ...interface{}) pgx.Row
}

type Runner interface {
	Querier
	Exec(ctx context.Context, sql string, arguments ...interface{}) (pgconn.CommandTag, error)
}

// Transacter is the interface to wrap base sql transaction
type Transacter interface {
	Transact(txFunc func(tx *pgx.Tx) error) (err error)
}

func NewConn(ctx context.Context, connString string) (*pgx.Conn, error) {
	return pgx.Connect(ctx, connString)
}

func WithTX(ctx context.Context, conn *pgx.Conn, txFunc func(context.Context, pgx.Tx) error) (err error) {
	tCtx, _ := context.WithTimeout(ctx, time.Second*30)
	tx, err := conn.Begin(tCtx)
	if err != nil {
		return err
	}

	// Rollback is safe to call even if the tx is already closed, so if
	// the tx commits successfully, this is a no-op
	defer tx.Rollback(ctx)

	err = txFunc(tCtx, tx)
	if err != nil {
		return err
	}

	err = tx.Commit(ctx)
	return err
}

func ApplyMigrations(config *config.Config) error {
	db, err := sql.Open("pgx", config.PostgresDSN)
	if err != nil {
		return err
	}
	defer db.Close()

	migrations := &migrate.FileMigrationSource{Dir: "migrations"}
	if _, err := migrate.Exec(db, "postgres", migrations, migrate.Up); err != nil {
		return err
	}
	return nil
}
