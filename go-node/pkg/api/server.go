package api

import (
	"context"
	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/input-output-hk/mithril/go-node/internal/log"
	"github.com/input-output-hk/mithril/go-node/pkg/config"
	"github.com/input-output-hk/mithril/go-node/pkg/node"
	"github.com/jackc/pgx/v4"
	"net/http"
)

func NewServer(config *config.Config, conn *pgx.Conn, node *node.Node) (*Server, error) {
	router := chi.NewRouter()

	router.Use(middleware.RequestID)
	router.Use(middleware.RealIP)
	router.Use(middleware.Logger)
	router.Use(middleware.Recoverer)
	router.Use(DatabaseMiddleware(conn))
	router.Use(NodeIdMiddleware(node))

	router.Get("/certs", listCertificates)
	router.Get("/certs/{hash}", getCertByHash)
	router.Get("/utxo/{hash}", utxo)
	router.Get("/utxo/{hash}/{addr}", utxoByAddr)
	router.Get("/config", getConfig(config))
	router.Get("/list-certs", getAllCerts)

	return &Server{
		config: config,
		router: router,
		httpServer: http.Server{
			Addr:    config.Http.ServerAddr,
			Handler: router,
		},
	}, nil
}

type Server struct {
	config     *config.Config
	router     chi.Router
	httpServer http.Server
}

func (s *Server) ListenAndServe() error {
	log.Infow("Starting API Server",
		"listen_addr", s.config.Http.ServerAddr,
	)
	return s.httpServer.ListenAndServe()
}

func (s *Server) Shutdown(ctx context.Context) error {
	return s.httpServer.Shutdown(ctx)
}
