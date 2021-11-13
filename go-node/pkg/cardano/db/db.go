package db

import (
	"context"
	"github.com/jackc/pgx/v4"
	"time"
)

// Transacter is the interface to wrap base sql transaction
type Transacter interface {
	Transact(txFunc func(tx *pgx.Tx) error) (err error)
}

// Storage model
type Storage struct {
	Conn           *pgx.Conn
	UTXORepository *UTXORepository
}

// NewStorage new db storage
func NewStorage(ctx context.Context, connString string) (*Storage, error) {
	conn, err := pgx.Connect(ctx, connString)
	if err != nil {
		return nil, err
	}
	utxoRepo := &UTXORepository{}

	return &Storage{
		Conn:           conn,
		UTXORepository: utxoRepo,
	}, nil
}

// Transact generate and handle tx and recover in case of panic
func (ps *Storage) Transact(ctx context.Context, txFunc func(pgx.Tx) error) (err error) {
	tCtx, _ := context.WithTimeout(ctx, time.Second*30)
	tx, err := ps.Conn.Begin(tCtx)
	if err != nil {
		return err
	}
	// Rollback is safe to call even if the tx is already closed, so if
	// the tx commits successfully, this is a no-op
	defer tx.Rollback(ctx)

	err = txFunc(tx)
	if err != nil {
		return err
	}

	err = tx.Commit(ctx)
	return err
}
