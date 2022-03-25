package mithril

/*
#include "mithril.h"
*/
import "C"
import (
	"encoding/base64"
	"errors"
)

var (
	ErrDecodeFailed = errors.New("decode failed")
	ErrEncodeFailed = errors.New("encode failed")
)

func decodeSecretKey(src []byte) (C.MspSkPtr, error) {
	size, cBuf := toCBytes(src)
	var ptr C.MspSkPtr
	ret := C.msp_deserialize_secret_key(size, cBuf, &ptr)
	if ret != 0 {
		return nil, ErrDecodeFailed
	}
	return ptr, nil
}

func decodePublicKey(src []byte) (C.MspPkPtr, error) {
	size, cBuf := toCBytes(src)
	var ptr C.MspPkPtr
	ret := C.msp_deserialize_verification_key(size, cBuf, &ptr)
	if ret != 0 {
		return nil, ErrDecodeFailed
	}
	return ptr, nil
}

func DecodeInitializer(src string) (*Initializer, error) {
	buf, _ := base64.StdEncoding.DecodeString(src)
	return decodeInitializer(buf)
}

func MultiSigFromBytes(src []byte) (*MultiSign, error) {
	return decodeMultiSign(src)
}

func decodeInitializer(src []byte) (*Initializer, error) {
	size, cBuf := toCBytes(src)

	var ptr C.StmInitializerPtr
	ret := C.stm_deserialize_initializer(size, cBuf, &ptr)
	if ret != 0 {
		return nil, ErrDecodeFailed
	}

	si := &Initializer{ptr: ptr}
	ret = C.stm_initializer_secret_key(si.ptr, &si.sk)
	if ret != 0 {
		return nil, ErrDecodeFailed
	}
	ret = C.stm_initializer_verification_key(si.ptr, &si.pk)
	if ret != 0 {
		return nil, ErrDecodeFailed
	}

	return si, nil
}

func DecodeSignature(src string, index uint64) (*Signature, error) {
	buf, _ := base64.StdEncoding.DecodeString(src)
	sig, err := decodeSignature(buf)
	if err != nil {
		return nil, err
	}
	sig.index = index
	return sig, nil
}

func decodeSignature(src []byte) (*Signature, error) {
	size, cBuf := toCBytes(src)
	var ptr C.SigPtr
	ret := C.stm_deserialize_sig(size, cBuf, &ptr)
	if ret != 0 {
		return nil, ErrDecodeFailed
	}
	return &Signature{ptr: ptr}, nil
}

func decodeMultiSign(src []byte) (*MultiSign, error) {
	size, cBuf := toCBytes(src)
	var ptr C.MultiSigPtr
	ret := C.stm_deserialize_multi_sig(size, cBuf, &ptr)
	if ret != 0 {
		return nil, ErrDecodeFailed
	}
	return &MultiSign{ptr: ptr}, nil
}

func toCBytes(buf []byte) (C.ulong, *C.uchar) {
	cBuf := make([]C.uchar, len(buf))

	for i := 0; i < len(buf); i++ {
		cBuf[i] = C.uchar(buf[i])
	}
	return C.ulong(len(cBuf)), &cBuf[0]
}
