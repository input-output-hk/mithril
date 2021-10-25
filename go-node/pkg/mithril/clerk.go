package mithril

/*
#include "mithril.h"
*/
import "C"

func NewClerk(params StmParameters, avk MerkelTree, stake int64) StmClerk {
	return StmClerk{ptr: C.stm_clerk_new(params, avk, C.ulonglong(stake))}
}

type StmClerk struct {
	ptr     C.StmClerkPtr
	isFreed bool
}

func (c StmClerk) Aggregate(index int64, sign Signature, msg string) (MultiSignConst, error) {
	var msc MultiSignConst

	idx := C.Index(index)
	rv := C.stm_clerk_aggregate(c.ptr, 1, sign, &idx, C.CString(msg), &msc)

	switch int(rv) {
	case 0: // If verification is successful
		return msc, nil
	case -1: // If verification is failed
		return nil, ErrVerifyFailed
	default: // If not enough signature
		return nil, ErrNotEnoughSignatures
	}
}

func (c StmClerk) VerifySign(msg string, index int64, sig Signature) bool {
	rv := C.stm_clerk_verify_sig(c.ptr, sig, C.ulonglong(index), C.CString(msg))
	return bool(rv)
}

func (c StmClerk) VerifyMultiSign(msc MultiSignConst, msg string) bool {
	rv := C.stm_clerk_verify_msig(c.ptr, msc, C.CString(msg))
	return bool(rv)
}

func (c *StmClerk) Free() {
	if c.isFreed {
		return
	}
	c.isFreed = true
	C.free_stm_clerk(c.ptr)
}
