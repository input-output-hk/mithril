package mithril

import "unsafe"

/*
#include "mithril.h"
*/
import "C"

func encodeSecretKey(key C.MspSkPtr) []byte {
	return encodeToBytes(func(size *C.ulong, buf **C.uchar) {
		C.msp_serialize_secret_key(key, size, buf)
	})
}

func encodePublicKey(key C.MspPkPtr) []byte {
	return encodeToBytes(func(size *C.ulong, buf **C.uchar) {
		C.msp_serialize_verification_key(key, size, buf)
	})
}

func encodeInitializer(si Initializer) []byte {
	return encodeToBytes(func(size *C.ulong, buf **C.uchar) {
		C.stm_serialize_initializer(si.ptr, size, buf)
	})
}


func encodeSignature(sig C.SigPtr) []byte {
	return encodeToBytes(func(size *C.ulong, buf **C.uchar) {
		C.stm_serialize_sig(sig, size, buf)
	})
}

func EncodeMultiSig(multiSig *MultiSign) []byte {
	return encodeMultiSign(multiSig.ptr)
}

func encodeMultiSign(ms C.MultiSigPtr) []byte {
	return encodeToBytes(func(size *C.ulong, buf **C.uchar) {
		C.stm_serialize_multi_sig(ms, size, buf)
	})
}

func encodeToBytes(serializeCB func(size *C.ulong, buf **C.uchar)) []byte {
	var cSize C.ulong
	var cBytes *C.uchar

	serializeCB(&cSize, &cBytes)
	defer C.free(unsafe.Pointer(cBytes))

	bytes := make([]byte, cSize)
	copy(bytes[:], []byte(C.GoBytes(unsafe.Pointer(cBytes), C.int(cSize))))

	return bytes
}
