package client

import (
	"github.com/Pantani/request"
	"github.com/input-output-hk/mithril/go-node/pkg/cert"
	"strings"
)

func NewClient(url string) MithrilClient {
	url = strings.TrimRight(url, "/")
	return MithrilClient{httpClient: request.InitClient(url)}
}

type MithrilClient struct {
	url        string
	httpClient request.Request
}

func (mc MithrilClient) RecentCerts() ([]cert.Certificate, error) {
	var certs []cert.Certificate
	err := mc.httpClient.Get(&certs, "certs", nil)
	if err != nil {
		return nil, err
	}
	return certs, nil
}
