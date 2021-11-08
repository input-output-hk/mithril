package mithril

/*
#include "mithril.h"
*/
import "C"

//func NewKeyReg(participants []*Participant) KeyReg {
//	return KeyReg{ptr: C.key_reg_new(C.ulong(len(participants)), participants[0])}
//}
//
//type KeyReg struct {
//	ptr     C.KeyRegPtr
//	isFreed bool
//}
//
//func (key *KeyReg) Free() {
//	if key.isFreed {
//		return
//	}
//	C.free_keyreg(key.ptr)
//	key.isFreed = true
//}
