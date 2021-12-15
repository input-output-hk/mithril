package api

import (
	"context"
	"github.com/input-output-hk/mithril/go-node/pkg/node"
	"github.com/jackc/pgx/v4"
	"net/http"
)

const (
	DbConnCtxKey = "db"
	NodeCtxKey   = "node"
)

func DatabaseMiddleware(conn *pgx.Conn) func(http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			ctx := context.WithValue(r.Context(), DbConnCtxKey, conn)
			next.ServeHTTP(w, r.WithContext(ctx))
		})
	}
}

func NodeIdMiddleware(node *node.Node) func(http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			ctx := context.WithValue(r.Context(), NodeCtxKey, node)
			next.ServeHTTP(w, r.WithContext(ctx))
		})
	}
}
