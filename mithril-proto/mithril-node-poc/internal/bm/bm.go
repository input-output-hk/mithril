package bm

import (
	"context"
	"github.com/input-output-hk/mithril/mithril-proto/mithril-node-poc/internal/pg"
	"github.com/input-output-hk/mithril/mithril-proto/mithril-node-poc/pkg/cert"
	"github.com/jackc/pgx/v4"
	"time"
)

type Event struct {
	NodeId    uint64
	CertHash  cert.Hex
	Title     string
	StartTime time.Time
	EndTime   time.Time
}

func Save(ctx context.Context, dbx pg.Runner, e *Event) error {
	stmt := `insert into mithril_benchmarks (
				node_id, cert_hash, title, start_time, end_time
			 ) values ($1, $2, $3, $4, $5)`

	_, err := dbx.Exec(ctx, stmt,
		e.NodeId,
		e.CertHash,
		e.Title,
		e.StartTime,
		e.EndTime)
	return err
}

func SaveAll(ctx context.Context, conn *pgx.Conn, events []*Event) error {
	return pg.WithTX(ctx, conn, func(ctx context.Context, tx pgx.Tx) error {
		for _, e := range events {
			err := Save(ctx, tx, e)
			if err != nil {
				return err
			}
		}
		return nil
	})
}

func New(title string) *Event {
	return &Event{Title: title, StartTime: time.Now()}
}

func (e *Event) Stop() {
	e.EndTime = time.Now()
}
