package mithril

/*
#include "mithril.h"
*/
import "C"
import (
	"encoding/base64"
	"errors"
)

var (
	ErrInitFailed = errors.New("init failed")
)

func NewInitializer(params Parameters, partyId, stake uint64) (*Initializer, error) {
	initializer := &Initializer{params: params}
	stmParams := NewStmtParams(params.K, params.M, params.PhiF)

	ret := C.stm_intializer_setup(stmParams, C.PartyId(partyId), C.uint64_t(stake), &initializer.ptr)
	if ret != 0 {
		return nil, ErrInitFailed
	}

	ret = C.stm_initializer_secret_key(initializer.ptr, &initializer.sk)
	if ret != 0 {
		return nil, ErrInitFailed
	}
	ret = C.stm_initializer_verification_key(initializer.ptr, &initializer.pk)
	if ret != 0 {
		return nil, ErrInitFailed
	}

	return initializer, nil
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
	var id C.uint64_t
	C.stm_initializer_party_id(si.ptr, &id)
	return uint64(id)
}

func (si Initializer) Stake() uint64 {
	var st C.uint64_t
	C.stm_initializer_stake(si.ptr, &st)
	return uint64(st)
}

func (si Initializer) Participant() *Participant {
	return &Participant{
		PartyId:   si.PartyId(),
		Stake:     si.Stake(),
		PublicKey: base64.StdEncoding.EncodeToString(encodePublicKey(si.pk)),
		pk:        si.pk,
	}
}

func (si *Initializer) RefreshKeys() error {
	C.stm_initailizer_generate_new_key(si.ptr)

	ret := C.stm_initializer_secret_key(si.ptr, &si.sk)
	if ret != 0 {
		return ErrInitFailed
	}
	ret = C.stm_initializer_verification_key(si.ptr, &si.pk)
	if ret != 0 {
		return ErrInitFailed
	}
	return nil
}

func (si Initializer) SecretKey() []byte {
	return encodeSecretKey(si.sk)
}

func (si Initializer) PublicKey() []byte {
	return encodePublicKey(si.pk)
}

func (si *Initializer) Encode() []byte {
	return encodeInitializer(si)
}

func (si *Initializer) Free() {
	if si.isFreed {
		return
	}
	C.free_stm_initializer(si.ptr)
	si.isFreed = true
}
