package api

import (
	"encoding/json"
	"github.com/input-output-hk/mithril/go-node/pkg/node"
	"github.com/jackc/pgx/v4"
	"net/http"
)

func GetDbConn(r *http.Request) *pgx.Conn {
	conn, ok := r.Context().Value(DbConnCtxKey).(*pgx.Conn)
	if !ok {
		panic("GetDbConn panic")
	}
	return conn
}

func JsonResponse(w http.ResponseWriter, code int, payload interface{}) {
	w.WriteHeader(code)
	w.Header().Set("Content-Type", "application/json")

	data, _ := json.Marshal(payload)
	_, _ = w.Write(data)
}

func ErrResponse(w http.ResponseWriter, err error) {
	w.WriteHeader(http.StatusInternalServerError)
	w.Write([]byte(err.Error()))
}

func GetNode(r *http.Request) *node.Node {
	return r.Context().Value(NodeCtxKey).(*node.Node)
}
