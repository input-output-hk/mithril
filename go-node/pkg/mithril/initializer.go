package mithril

/*
#include "mithril.h"
*/
import "C"

func NewInitializer(params StmParameters, partyId, stake int64) Initializer {
	initializer := Initializer{params: params}

	initializer.ptr = C.stm_intializer_setup(params, C.PartyId(partyId), C.ulonglong(stake))
	initializer.sk = C.stm_initializer_secret_key(initializer.ptr)
	initializer.pk = C.stm_initializer_verification_key(initializer.ptr)

	return initializer
}

type Initializer struct {
	params  StmParameters
	ptr     C.StmInitializerPtr
	sk      C.MspSkPtr
	pk      C.MspPkPtr
	isFreed bool
}

func (si Initializer) PartyId() int64 {
	return int64(C.stm_initializer_party_id(si.ptr))
}

func (si Initializer) Stake() int64 {
	return int64(C.stm_initializer_stake(si.ptr))
}

func (si Initializer) Participant() Participant {
	return Participant{
		PartyId: si.PartyId(),
		Stake:   si.Stake(),
		pk:      si.pk,
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

type Participant struct {
	PartyId int64
	Stake   int64
	pk      C.MspPkPtr
}