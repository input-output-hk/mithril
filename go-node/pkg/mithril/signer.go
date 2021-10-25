package mithril

/*
#include "mithril.h"
*/
import "C"

type (
	Signature      = C.SigPtr
	MultiSign      = C.MultiSigPtr
	MultiSignConst = C.MultiSigConstPtr
)

// what is this?
type StmSigner struct {
	ptr C.StmSignerPtr
}

func (s StmSigner) EligibilityCheck(msg string) bool {
	return bool(C.stm_signer_eligibility_check(s.ptr, C.CString(msg), 1))
}

func (s StmSigner) Sign(msg string) (Signature, error) {
	var sig Signature

	if C.stm_signer_sign(s.ptr, C.CString(msg), 1, &sig); sig == nil {
		return nil, ErrSignFailed
	}
	return sig, nil
}

func (s StmSigner) GetClerk() StmClerk {
	return StmClerk{ptr: C.stm_clerk_from_signer(s.ptr)}
}
