package mithril

/*
#include "mithril.h"
*/
import "C"
import "encoding/base64"

func NewInitializer(params Parameters, partyId, stake uint64) Initializer {
	initializer := Initializer{params: params}
	stmParams := NewStmtParams(params.K, params.M, params.PhiF)

	initializer.ptr = C.stm_intializer_setup(stmParams, C.PartyId(partyId), C.uint64_t(stake))
	initializer.sk = C.stm_initializer_secret_key(initializer.ptr)
	initializer.pk = C.stm_initializer_verification_key(initializer.ptr)

	return initializer
}

type Initializer struct {
	params  Parameters
	ptr     C.StmInitializerPtr
	krPtr   C.KeyRegPtr
	ckrPtr  C.ClosedKeyRegPtr
	sk      C.MspSkPtr
	pk      C.MspPkPtr
	isFreed bool
}

func (si Initializer) PartyId() uint64 {
	return uint64(C.stm_initializer_party_id(si.ptr))
}

func (si Initializer) Stake() uint64 {
	return uint64(C.stm_initializer_stake(si.ptr))
}

func (si Initializer) Participant() Participant {
	return Participant{
		PartyId:   si.PartyId(),
		Stake:     si.Stake(),
		PublicKey: base64.StdEncoding.EncodeToString(encodePublicKey(si.pk)),
		pk:        si.pk,
	}
}

func (si *Initializer) RefreshKeys() {
	C.stm_initailizer_generate_new_key(si.ptr)

	si.sk = C.stm_initializer_secret_key(si.ptr)
	si.pk = C.stm_initializer_verification_key(si.ptr)
}

func (si Initializer) SecretKey() []byte {
	return encodeSecretKey(si.sk)
}

func (si Initializer) PublicKey() []byte {
	return encodePublicKey(si.pk)
}

func (si Initializer) Encode() []byte {
	return encodeInitializer(si)
}

func (si *Initializer) Free() {
	if si.isFreed {
		return
	}
	C.free_stm_initializer(si.ptr)
	si.isFreed = true
}
