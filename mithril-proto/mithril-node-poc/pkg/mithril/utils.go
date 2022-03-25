package mithril

import "encoding/base64"

func DecodeParticipant(p *Participant) error {
	keyBytes, err := base64.StdEncoding.DecodeString(p.PublicKey)
	if err != nil {
		return err
	}
	p.pk, err = decodePublicKey(keyBytes)
	if err != nil {
		return err
	}
	return nil
}
