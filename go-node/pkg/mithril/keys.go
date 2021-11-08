package mithril

/*
#include "mithril.h"
*/
import "C"
import "unsafe"

func DecodeSk() {
}

func EncodeSk() {
}

func decodeSk(key []byte) C.MspSkPtr {
	k := make([]C.uchar, len(key))

	for i := 0; i < len(key); i++ {
		k[i] = C.uchar(key[i])
	}

	return C.msp_deserialize_secret_key(C.ulong(len(k)), &k[0])
}


func encodeSk(key C.MspSkPtr) []byte {
	var cSize C.ulong
	var cBytes *C.uchar

	C.msp_serialize_secret_key(key, &cSize, &cBytes)
	return []byte(C.GoBytes(unsafe.Pointer(cBytes), C.int(cSize)))
}