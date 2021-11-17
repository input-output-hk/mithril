package api

import (
	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/input-output-hk/mithril/go-node/internal/log"
	"github.com/input-output-hk/mithril/go-node/pkg/config"
	"github.com/jackc/pgx/v4"
	"net/http"
)

func NewServer(config *config.Config, conn *pgx.Conn) *Server {
	router := chi.NewRouter()

	router.Use(middleware.RequestID)
	router.Use(middleware.RealIP)
	router.Use(middleware.Logger)
	router.Use(middleware.Recoverer)
	router.Use(DatabaseMiddleware(conn))

	router.Get("/certs", listCertificates)

	return &Server{
		config: config,
		router: router,
		httpServer: http.Server{
			Addr:    config.Http.ServerAddr,
			Handler: router,
		},
	}
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

func (s *Server) Shutdown() {
}
