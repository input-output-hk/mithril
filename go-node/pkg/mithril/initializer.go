package mithril

/*
#include "mithril.h"
*/
import "C"

func NewStmInitializer(params StmParameters) StmInitializer {
	return StmInitializer{ptr: C.stm_intializer_setup(params, 1, 1)}
}

type StmInitializer struct {
	ptr     C.StmInitializerPtr
	isFreed bool
}

func (f StmInitializer) Register(keyReg KeyReg) {
	if keyReg.isFreed {
		panic("no!")
	}
	C.stm_initializer_register(f.ptr, keyReg.ptr)
}

func (f StmInitializer) BuildAVK(keyReg KeyReg) {
	if keyReg.isFreed {
		panic("no!")
	}
	C.stm_initializer_build_avk(f.ptr, keyReg.ptr)
}

func (f StmInitializer) Finish() StmSigner {
	return StmSigner{ptr: C.stm_initializer_finish(f.ptr)}
}

func (f *StmInitializer) Free() {
	if f.isFreed {
		return
	}
	C.free_stm_initializer(f.ptr)
	f.isFreed = true
}
