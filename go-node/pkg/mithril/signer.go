package mithril

/*
#include "mithril.h"
*/
import "C"

//type (
//	MultiSign      = C.MultiSigPtr
//	MultiSignConst = C.MultiSigConstPtr
//)
//
//// what is this?
//type StmSigner struct {
//	ptr C.StmSignerPtr
//}
//
//func (s StmSigner) EligibilityCheck(index int64, msg string) bool {
//	rv := C.stm_signer_eligibility_check(s.ptr, C.CString(msg), C.ulonglong(index))
//	return bool(rv)
//}
//
//func (s StmSigner) Sign(index int64, msg string) (*Signature, error) {
//	var sig Signature
//
//	if C.stm_signer_sign(s.ptr, C.CString(msg), C.ulonglong(index), &sig.ptr); sig.ptr == nil {
//		return nil, ErrSignFailed
//	}
//
//	return &sig, nil
//}
//
//func (s StmSigner) GetClerk() StmClerk {
//	return StmClerk{ptr: C.stm_clerk_from_signer(s.ptr)}
//}
//
//type Signature struct {
//	ptr     C.SigPtr
//	isFreed bool
//}
//
//func (s *Signature) Free() {
//	if s.isFreed {
//		return
//	}
//	s.isFreed = true
//	C.free_sig(s.ptr)
//}
