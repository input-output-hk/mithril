package mithril

import "C"

/*
#include "mithril.h"
*/
import "C"
import "encoding/base64"

func decodeSecretKey(src []byte) C.MspSkPtr {
	size, cBuf := toCBytes(src)
	return C.msp_deserialize_secret_key(size, cBuf)
}

func decodePublicKey(src []byte) C.MspPkPtr {
	size, cBuf := toCBytes(src)
	return C.msp_deserialize_verification_key(size, cBuf)
}

func DecodeInitializer(src string) Initializer {
	buf, _ := base64.StdEncoding.DecodeString(src)
	return decodeInitializer(buf)
}

func decodeInitializer(src []byte) Initializer {
	size, cBuf := toCBytes(src)

	si := Initializer{ptr: C.stm_deserialize_initializer(size, cBuf)}
	si.sk = C.stm_initializer_secret_key(si.ptr)
	si.pk = C.stm_initializer_verification_key(si.ptr)

	return si
}

func DecodeSignature(src string, index uint64) *Signature {
	buf, _ := base64.StdEncoding.DecodeString(src)
	sig := decodeSignature(buf)
	sig.index = index
	return sig
}

func decodeSignature(src []byte) *Signature {
	size, cBuf := toCBytes(src)
	return &Signature{ptr: C.stm_deserialize_sig(size, cBuf)}
}

func decodeMultiSign(src []byte) *MultiSign {
	size, cBuf := toCBytes(src)
	return &MultiSign{ptr: C.stm_deserialize_multi_sig(size, cBuf)}
}

func toCBytes(buf []byte) (C.ulong, *C.uchar) {
	cBuf := make([]C.uchar, len(buf))

	for i := 0; i < len(buf); i++ {
		cBuf[i] = C.uchar(buf[i])
	}
	return C.ulong(len(cBuf)), &cBuf[0]
}
