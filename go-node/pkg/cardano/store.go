package cardano

import (
	"context"
	"github.com/input-output-hk/mithril/go-node/internal/pg"
	"github.com/input-output-hk/mithril/go-node/pkg/cardano/mt"
	"github.com/input-output-hk/mithril/go-node/pkg/cardano/types"
	"github.com/jackc/pgx/v4"
	"time"
)

// Storage model
type Storage struct {
	Conn           *pgx.Conn
	UTXORepository *UTXORepository
}

// NewStorage new pg storage
func NewStorage(ctx context.Context, conn *pgx.Conn) Storage {

	utxoRepo := &UTXORepository{}

	return Storage{
		Conn:           conn,
		UTXORepository: utxoRepo,
	}
}

// Transact generate and handle tx and recover in case of panic
func (ps *Storage) Transact(ctx context.Context, txFunc func(pgx.Tx) error) (err error) {
	tCtx, _ := context.WithTimeout(ctx, time.Second*30)
	tx, err := ps.Conn.Begin(tCtx)
	if err != nil {
		return err
	}
	// Rollback is safe to call even if the tx is already closed, so if
	// the tx commits successfully, this is a no-op
	defer tx.Rollback(ctx)

	err = txFunc(tx)
	if err != nil {
		return err
	}

	err = tx.Commit(ctx)
	return err
}

const getTxOutputsSQL = `-- select all spent out till block
with
     -- select all outputs till block_no
     tx_o as (select tx_out.id, tx_id, index, address, address_raw, payment_cred, stake_address_id, value
                from tx_out
                         inner join tx t on t.id = tx_out.tx_id
                         inner join block on t.block_id=block.id
                where block_no < $1),

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
ORDER BY tx_outer.address, tx_id, index;`

// UTXORepository type for utxo repository
type UTXORepository struct {
}

// GetTxOutputs get sets from Conn
func (r *UTXORepository) GetTxOutputs(tx pg.Querier, blockNumber uint64, tree mt.MerkleTree) (int, error) {

	rows, err := tx.Query(context.Background(), getTxOutputsSQL, blockNumber)
	if err != nil {
		return 0, err
	}
	defer rows.Close()
	txOutLen := 0

	for rows.Next() {
		txOutLen++

		/*
		   1. group tx output
		*/
		txOut := &types.UTXO{}
		err := rows.Scan(&txOut.Address, &txOut.Index, &txOut.Value, &txOut.TxID)
		if err != nil {
			return 0, err
		}
		hash, err := mt.CalculateHash(txOut)
		if err != nil {
			return 0, err
		}

		err = tree.Add(hash)
		if err != nil {
			return 0, err
		}
	}

	err = rows.Err()
	if err != nil {
		return 0, err
	}

	return txOutLen, nil
}
