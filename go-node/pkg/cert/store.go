package cert

import (
	"context"
	"github.com/jackc/pgx/v4"
)

func Save(ctx context.Context, tx pgx.Tx, cert *Certificate) error {
	stmt := `insert into mithril_certificates (
				id, node_id, participants, cert_hash, prev_hash,
				block_number, block_hash, merkle_root,
				multi_sig, sig_started_at, sig_finished_at
			 ) values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)`

	_, err := tx.Exec(ctx, stmt,
		cert.Id,
		cert.NodeId,
		cert.Participants,
		cert.CertHash,
		cert.PrevHash,
		cert.BlockNumber,
		cert.BlockHash,
		cert.MerkleRoot,
		cert.MultiSig,
		cert.SigStartedAt,
		cert.SigFinishedAt,
	)
	return err
}

func Recent(ctx context.Context, tx pgx.Tx, nodeId uint64) ([]Certificate, error) {
	stmt := `select id, block_number, block_hash, merkle_root, multi_sig, sig_started_at, sig_finished_at,
				cert_hash, participants
				from mithril_certificates
				where node_id = $1
				order by id desc limit 20`
	rows, err := tx.Query(ctx, stmt, nodeId)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var certs []Certificate
	for rows.Next() {
		var c Certificate
		err := rows.Scan(&c.Id, &c.BlockNumber, &c.BlockHash, &c.MerkleRoot,
			&c.MultiSig, &c.SigStartedAt, &c.SigFinishedAt,
			&c.CertHash, &c.Participants,
		)
		if err != nil {
			return nil, err
		}
		certs = append(certs, c)
	}

	return certs, nil
}

func GetByMerkleTreeHash(ctx context.Context, tx pgx.Tx, hash []byte) (*Certificate, error) {
	stmt := `select id, block_number, block_hash, merkle_root, multi_sig, sig_started_at, sig_finished_at
				from mithril_certificates where merkle_root = $1
				order by id limit 1`

	var c Certificate
	err := tx.QueryRow(ctx, stmt, hash).Scan(
		&c.Id, &c.BlockNumber, &c.BlockHash, &c.MerkleRoot,
		&c.MultiSig, &c.SigStartedAt, &c.SigFinishedAt,
	)
	if err != nil {
		return nil, err
	}

	return &c, nil
}
