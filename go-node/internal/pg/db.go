package pg

import (
	"context"
	"github.com/jackc/pgx/v4"
)

type Querier interface {
	Query(ctx context.Context, sql string, args ...interface{}) (pgx.Rows, error)
}

// Transacter is the interface to wrap base sql transaction
type Transacter interface {
	Transact(txFunc func(tx *pgx.Tx) error) (err error)
}

func NewConn(ctx context.Context, connString string) (*pgx.Conn, error) {
	return pgx.Connect(ctx, connString)
}