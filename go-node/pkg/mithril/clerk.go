package mithril

/*
#include "mithril.h"
*/
import "C"
import (
	"errors"
	"fmt"
)

type Clerk struct {
	ptr     C.StmClerkPtr
	isFreed bool
}

func (c Clerk) Aggregate(signatures []*Signature, msg string) (*MultiSign, error) {
	var msc MultiSign

	signs := make([]C.SigPtr, len(signatures))
	indices := make([]C.Index, len(signatures))
	for i, s := range signatures {
		signs[i] = s.ptr
		indices[i] = C.Index(s.index)
	}

	rv := C.stm_clerk_aggregate(c.ptr, C.ulong(len(signs)), &signs[0], &indices[0], C.CString(msg), &msc.ptr)
	fmt.Println("Aggregate returns:", rv, msc.ptr)

	switch int(rv) {
	case 0: // If verification is successful
		return &msc, nil
	case -1: // If verification is failed
		return nil, ErrVerifyFailed
	default: // If not enough signature
		return nil, ErrNotEnoughSignatures
	}
}

func (c Clerk) VerifySign(msg string, index uint64, sig *Signature) error {
	rv := int(C.stm_clerk_verify_sig(c.ptr, sig.ptr, C.ulonglong(index), C.CString(msg)))
	switch rv {
	case 0:
		return nil
	case -1:
		return ErrLotteryLost
	case -2:
		return ErrInvalidMerkleTree
	case -3:
		return ErrInvalidSignature
	default:
		return errors.New("unknown error")
	}
}

func (c Clerk) VerifyMultiSign(ms *MultiSign, msg string) error {
	rv := int64(C.stm_clerk_verify_msig(c.ptr, ms.ptr, C.CString(msg)))
	switch rv {
	case 0:
		return nil
	case -1:
		return ErrInvalidAggregate
	default:
		return ErrProofError
	}
}

func (c *Clerk) Free() {
	if c.isFreed {
		return
	}
	c.isFreed = true
	C.free_stm_clerk(c.ptr)
}
