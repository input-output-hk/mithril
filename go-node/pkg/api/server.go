package api

import (
	"context"
	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/input-output-hk/mithril/go-node/internal/log"
	"github.com/input-output-hk/mithril/go-node/pkg/config"
	"github.com/jackc/pgx/v4"
	"github.com/pkg/errors"
	"net/http"
	"strconv"
	"strings"
)

func NewServer(config *config.Config, conn *pgx.Conn) (*Server, error) {
	router := chi.NewRouter()

	router.Use(middleware.RequestID)
	router.Use(middleware.RealIP)
	router.Use(middleware.Logger)
	router.Use(middleware.Recoverer)
	router.Use(DatabaseMiddleware(conn))

	router.Get("/certs", listCertificates)
	router.Get("/utxo/{merkle_root}", utxo)
	router.Get("/utxo/{merkle_root}/{addr}", utxoByAddr)

	// Temporary solution. Make all node api servers
	// run on different ports.
	args := strings.Split(config.Http.ServerAddr, ":")
	if len(args) != 2 {
		return nil, errors.New("invalid serverAddr string")
	}

	listenPort, err := strconv.ParseInt(args[1], 10, 64)
	if err != nil {
		return nil, errors.New("port is not a number")
	}
	listenPort += config.Mithril.PartyId

	return &Server{
		config: config,
		router: router,
		httpServer: http.Server{
			Addr:    strings.Join([]string{args[0], strconv.FormatInt(listenPort, 10)}, ":"),
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
