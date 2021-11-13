package db

import (
	"context"
	"github.com/input-output-hk/mithril/go-node/pkg/cardano/types"
	"github.com/jackc/pgx/v4"
	"github.com/stretchr/testify/assert"
	"testing"
	"time"
)

const connStr = "host=127.0.0.1 port=54321 user=alex password=123456 dbname=testnet sslmode=disable"

func TestNewStorage(t *testing.T) {
	s, err := NewStorage(context.TODO(), connStr)
	assert.Nil(t, err)
	timeCtx, _ := context.WithTimeout(context.Background(), time.Second*5)
	err = s.Conn.Ping(timeCtx)
	assert.Nil(t, err)
}

func TestUTXORepository_GetTxOutputs(t *testing.T) {
	s, err := NewStorage(context.TODO(), connStr)
	defer s.Conn.Close(context.TODO())
	assert.Nil(t, err)

	var txOuts map[types.Address][]*types.UTXO

	err = s.Transact(context.TODO(), func(tx pgx.Tx) error {
		txOuts, err = s.UTXORepository.GetTxOutputs(tx, 250000)
		return err
	})
	assert.Nil(t, err)
	assert.NotEmpty(t, txOuts)
	t.Log(txOuts)
}
