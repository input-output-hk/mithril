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

	i := C.ulonglong(index)
	if rv := C.stm_clerk_aggregate(c.ptr, 1, sign, &i, C.CString(msg), &msc); int(rv) == -1 {
		return nil, ErrVerificationFailed
	}

	return msc, nil
}

func (c StmClerk) VerifySign(msg string, index int64, sig Signature) bool {
	return bool(C.stm_clerk_verify_sig(c.ptr, sig, C.ulonglong(index), C.CString(msg)))
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
