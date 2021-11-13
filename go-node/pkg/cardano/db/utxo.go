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

const getTxOutputsSQL = `-- select all spent out till block
with
     -- select all outputs till block_no
     tx_o as (select tx_out.id, tx_id, index,address,address_raw, payment_cred, stake_address_id, value
                from tx_out
                         inner join tx t on t.id = tx_out.tx_id
                         inner join block on t.block_id=block.id
                where block_no<$1),

     -- In this select i'm not sure which id sould be used
     -- tx_in_id or tx_out_id?????

     -- select all inputs till block_no
     tx_i as (select *
               from tx_in
                        inner join tx t on t.id = tx_in.tx_in_id
                        inner join block on t.block_id=block.id
               where block_no<$1
     )
select tx_outer.address,tx_outer.index, tx_outer.value, tx_outer.tx_id
from tx_o as tx_outer
where
    not exists
        ( select *
          from
              tx_o
                  inner join
              tx_i
              on tx_o.tx_id = tx_i.tx_out_id and
                 tx_o.index = tx_i.tx_out_index
          where tx_outer.id = tx_o.id
        )
ORDER BY tx_outer.address;`

// UTXORepository type for utxo repository
type UTXORepository struct {
}

// GetTxOutputs get sets from Conn
func (r *UTXORepository) GetTxOutputs(tx Querier, blockNumber int64) (txOuts map[types.Address][]*types.UTXO, err error) {

	rows, err := tx.Query(context.Background(), getTxOutputsSQL, blockNumber)
	if err != nil {
		log.Fatal(err)
	}
	defer rows.Close()

	txOuts = make(map[types.Address][]*types.UTXO)

	for rows.Next() {
		/*
		   1. group tx output
		*/
		txOut := &types.UTXO{}
		err := rows.Scan(&txOut.Address, &txOut.Index, &txOut.Value, &txOut.TxID)
		if err != nil {
			log.Fatal(err)
		}
		if txOuts[txOut.Address] == nil {
			txOuts[txOut.Address] = []*types.UTXO{}
		}
		txOuts[txOut.Address] = append(txOuts[txOut.Address], txOut)

	}

	if err := rows.Err(); err != nil {
		log.Fatal(err)
	}
	return txOuts, nil
}
