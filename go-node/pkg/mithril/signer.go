package mithril

/*
#include "mithril.h"
*/
import "C"

import (
	"encoding/base64"
	"encoding/hex"
)

func NewSigner(initializer Initializer, participants []Participant) *Signer {
	partyIds := make([]C.PartyId, len(participants))
	partyStakes := make([]C.Stake, len(participants))
	partyKeys := make([]C.MspPkPtr, len(participants))

	for i, p := range participants {
		partyIds[i] = C.PartyId(p.PartyId)
		partyStakes[i] = C.Stake(p.Stake)
		partyKeys[i] = p.pk

	}

	keyReg := C.key_registration(C.ulong(len(partyIds)), &partyIds[0], &partyStakes[0])
	for _, p := range participants {
		C.register_party(keyReg, C.PartyId(p.PartyId), p.pk)
	}
	closedKeyReg := C.close_registration(keyReg)

	return &Signer{
		ptr: C.stm_initializer_new_signer(initializer.ptr, closedKeyReg),
	}
}

type Signer struct {
	ptr C.StmSignerPtr
}

func (s Signer) EligibilityCheck(index uint64, msg string) bool {
	rv := C.stm_signer_eligibility_check(s.ptr, C.CString(msg), C.uint64_t(index))
	return bool(rv)
}

func (s Signer) Sign(index uint64, msg string) (*Signature, error) {
	var sig Signature

	if C.stm_signer_sign(s.ptr, C.CString(msg), C.uint64_t(index), &sig.ptr); sig.ptr == nil {
		return nil, ErrSignFailed
	}

	sig.index = index
	return &sig, nil
}

func (s Signer) Clerk() Clerk {
	return Clerk{ptr: C.stm_clerk_from_signer(s.ptr)}
}

type Signature struct {
	ptr     C.SigPtr
	index   uint64
	isFreed bool
}

func (s Signature) Encode() []byte {
	return encodeSignature(s.ptr)
}

func (s Signature) HexString() string {
	return hex.EncodeToString(s.Encode())
}

func (s Signature) Base64() string {
	return base64.StdEncoding.EncodeToString(s.Encode())
}

func (s Signature) Index() uint64 {
	return s.index
}

func (s *Signature) Free() {
	if s.isFreed {
		return
	}
	s.isFreed = true
	C.free_sig(s.ptr)
}

type MultiSign struct {
	ptr     C.MultiSigPtr
	isFreed bool
}

func (ms MultiSign) Encode() []byte {
	return encodeMultiSign(ms.ptr)
}

func (ms *MultiSign) Free() {
	if ms.isFreed {
		return
	}
	ms.isFreed = true
	C.free_multi_sig(ms.ptr)
}
