package mithril

import "encoding/base64"

func DecodeParticipant(p *Participant) error {
	keyBytes, err := base64.StdEncoding.DecodeString(p.PublicKey)
	if err != nil {
		return err
	}
	p.pk = decodePublicKey(keyBytes)
	return nil
}
