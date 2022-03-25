package cardano

import (
	"context"
	"fmt"
	"github.com/input-output-hk/mithril/mithril-proto/mithril-node-poc/internal/pg"
	"github.com/input-output-hk/mithril/mithril-proto/mithril-node-poc/pkg/cardano/types"
	"github.com/jackc/pgx/v4"
)

// const getTxOutputsSQL = `-- select all spent out till block
//with
//     -- select all outputs till block_no
//     tx_o as (select tx_out.id, tx_id, index, address, address_raw, payment_cred, stake_address_id, value
//                from tx_out
//                         inner join tx t on t.id = tx_out.tx_id
//                         inner join block on t.block_id=block.id
//                where block_no < $1),
//
//     -- In this select i'm not sure which id sould be used
//     -- tx_in_id or tx_out_id?????
//
//     -- select all inputs till block_no
//     tx_i as (select *
//               from tx_in
//                        inner join tx t on t.id = tx_in.tx_in_id
//                        inner join block on t.block_id=block.id
//               where block_no<$1
//     )
//select tx_outer.address,tx_outer.index, tx_outer.value, tx_outer.tx_id
//from tx_o as tx_outer
//where
//    not exists
//        ( select *
//          from
//              tx_o
//                  inner join
//              tx_i
//              on tx_o.tx_id = tx_i.tx_out_id and
//                 tx_o.index = tx_i.tx_out_index
//          where tx_outer.id = tx_o.id
//        )
//ORDER BY tx_outer.address, tx_id, index;`

const getTxOutputsSQL = `SELECT distinct address,
       t.hash as hash,
       index,
       value
FROM tx_out
         inner join tx t on t.id = tx_out.tx_id
         inner join block as block_out on t.block_id = block_out.id
         left join tx_in on tx_out.id = tx_in.tx_out_id
         left join tx t2 on t2.id = tx_in.tx_out_id
         left join block block_in on block_in.id = t2.block_id
WHERE block_out.block_no <= $1
  AND (tx_in_id IS NULL OR block_in.block_no > $1)
ORDER BY address, t.hash, index;`

func GetBlockHash(tx pg.Querier, blockNumber uint64) ([]byte, error) {
	stmt := `select hash from block where block_no = $1`

	var hash []byte
	err := tx.QueryRow(context.TODO(), stmt, blockNumber).Scan(&hash)
	if err != nil {
		return nil, err
	}

	return hash, nil
}

// GetAllTxOutputs get sets from Conn
func GetAllTxOutputs(ctx context.Context, tx pg.Querier, blockNumber uint64) ([]*types.UTXO, error) {
	rows, err := tx.Query(ctx, getTxOutputsSQL, blockNumber)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var outputs []*types.UTXO

	for rows.Next() {
		txOut := &types.UTXO{}
		err := rows.Scan(&txOut.Address, &txOut.Hash, &txOut.Index, &txOut.Value)
		if err != nil {
			return nil, err
		}
		outputs = append(outputs, txOut)
	}

	err = rows.Err()
	if err != nil {
		return nil, err
	}

	return outputs, nil
}

func GetTxOutputsByAddr(ctx context.Context, tx pgx.Tx, blockNumber uint64, addr string) ([]types.UTXO, error) {
	const sql = `SELECT distinct address,
       t.hash as hash,
       index,
       value
			FROM tx_out
         inner join tx t on t.id = tx_out.tx_id
         inner join block as block_out on t.block_id = block_out.id
         left join tx_in on tx_out.id = tx_in.tx_out_id
         left join tx t2 on t2.id = tx_in.tx_out_id
         left join block block_in on block_in.id = t2.block_id
		WHERE block_out.block_no <= $1
  		AND (tx_in_id IS NULL OR block_in.block_no > $1)
		AND address = $2
		ORDER BY address, t.hash, index;`

	rows, err := tx.Query(ctx, sql, blockNumber, addr)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var list []types.UTXO
	for rows.Next() {
		txOut := types.UTXO{}
		err := rows.Scan(&txOut.Address, &txOut.Hash, &txOut.Index, &txOut.Value)
		if err != nil {
			return nil, err
		}
		list = append(list, txOut)
	}
	return list, nil
}

func GetTxTestList(ctx context.Context, tx pgx.Tx, size uint64) ([]*types.UTXO, error) {
	sql := fmt.Sprintf(`SELECT address,
       t.hash as hash,
       index,
       value
			FROM tx_out
         inner join tx t on t.id = tx_out.tx_id
		ORDER BY address, t.hash, index
		LIMIT %d;`, size)

	rows, err := tx.Query(context.Background(), sql)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var list []*types.UTXO
	for rows.Next() {
		txOut := types.UTXO{}
		err := rows.Scan(&txOut.Address, &txOut.Hash, &txOut.Index, &txOut.Value)
		if err != nil {
			return nil, err
		}
		list = append(list, &txOut)
	}
	return list, nil
}
