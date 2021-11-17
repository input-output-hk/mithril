package cert

import (
	"context"
	"github.com/jackc/pgx/v4"
)


func NewStorage(conn *pgx.Conn) Storage {
	return Storage{conn: conn}
}

type Storage struct {
	conn *pgx.Conn
}

func (s Storage) Save(ctx context.Context, tx *pgx.Tx, cert *Certificate) error {
	return nil
}

func (s Storage) Recent(ctx context.Context, tx *pgx.Tx) ([]Certificate, error) {
	return nil, nil
}