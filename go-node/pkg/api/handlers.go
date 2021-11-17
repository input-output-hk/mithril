package api

import (
	"context"
	"fmt"
	"github.com/input-output-hk/mithril/go-node/internal/pg"
	"github.com/input-output-hk/mithril/go-node/pkg/cert"
	"github.com/jackc/pgx/v4"
	"net/http"
)

func listCertificates(w http.ResponseWriter, r *http.Request) {
	var certs []cert.Certificate

	err := pg.WithTX(r.Context(), GetDbConn(r), func(ctx context.Context, tx pgx.Tx) error {
		var err error
		certs, err = cert.Recent(ctx, tx)
		return err
	})
	if err != nil {
		ErrResponse(w, err)
		return
	}

	fmt.Println(certs)
	JsonResponse(w, 200, certs)
}

func proof(w http.ResponseWriter, r *http.Request) {
	// GET address=X

	// 1. Build MT
	// 2.

}

func utxo(w http.ResponseWriter, r *http.Request) {
}
