package mithril

/*
#include "mithril.h"
*/
import "C"
import (
	"fmt"
	"unsafe"
)

func NewStmInitializer(params StmParameters, partyId, stake int64) StmInitializer {
	return StmInitializer{ptr: C.stm_intializer_setup(params, C.PartyId(partyId), C.ulonglong(stake))}
}

type StmInitializer struct {
	ptr     C.StmInitializerPtr
	isFreed bool
}

func (si StmInitializer) PartyId() int64 {
	return int64(C.stm_initializer_party_id(si.ptr))
}

func (si StmInitializer) Stake() int64 {
	return int64(C.stm_initializer_stake(si.ptr))
}

func (si StmInitializer) SecretKey() []byte {
	key := C.stm_initializer_secret_key(si.ptr)

	var cSize C.ulong
	var cBytes *C.uchar

	C.msp_serialize_secret_key(key, &cSize, &cBytes)
	fmt.Println(cBytes, unsafe.Slice(cBytes, int(cSize)))




	return nil
	//return []byte(C.GoBytes(unsafe.Pointer(cBytes), C.int(cSize)))
}

func (si StmInitializer) PublicKey() []byte {
	return nil
}

func (si *StmInitializer) Free() {
	if si.isFreed {
		return
	}
	C.free_stm_initializer(si.ptr)
	si.isFreed = true
}
