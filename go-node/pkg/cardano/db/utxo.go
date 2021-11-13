package db

import (
	"context"
	"github.com/input-output-hk/mithril/go-node/pkg/cardano/types"
	"github.com/jackc/pgx/v4"
	"log"
)

type Querier interface {
	Query(ctx context.Context, sql string, args ...interface{}) (pgx.Rows, error)
}

// UTXORepository type for utxo repository
type UTXORepository struct {
}

// GetTxOutputs get sets from Conn
func (r *UTXORepository) GetTxOutputs(tx Querier, blockNumber int) (txOuts map[types.Address][]*types.TxOut, err error) {

	rows, err := tx.Query(context.Background(), `select tx_outer.address,tx_outer.index,tx_outer.value,tx_outer.tx_id   from tx_out as tx_outer where
    												 not exists
							( select tx_out.id from tx_out inner join tx_in
																	  on tx_out.tx_id = tx_in.tx_out_id and tx_out.index = tx_in.tx_out_index
							  where tx_outer.id = tx_out.id
							)
ORDER BY tx_outer.address LIMIT 100`)
	if err != nil {
		log.Fatal(err)
	}
	defer rows.Close()

	txOuts = make(map[types.Address][]*types.TxOut)

	for rows.Next() {
		/*
		   1. group tx output
		*/
		txOut := &types.TxOut{}
		err := rows.Scan(&txOut.Address, &txOut.Index, &txOut.Value, &txOut.TxID)
		if err != nil {
			log.Fatal(err)
		}
		if txOuts[txOut.Address] == nil {
			txOuts[txOut.Address] = []*types.TxOut{}
		}
		txOuts[txOut.Address] = append(txOuts[txOut.Address], txOut)

	}

	if err := rows.Err(); err != nil {
		log.Fatal(err)
	}
	return txOuts, nil
}
