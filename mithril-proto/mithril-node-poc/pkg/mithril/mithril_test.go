package mithril

import (
	"bytes"
	"encoding/base64"
	"fmt"
	"testing"
)

func TestSecretKeyDecodeEncode(t *testing.T) {
	sk, _ := base64.StdEncoding.DecodeString("j0ORXBuFv3QK6ZH0jkkTRZZmaxHT7C5PNwEuuZKwQAo=")
	decodedSk, err := decodeSecretKey(sk)
	if err != nil {
		t.Error("Failed to decode")
	}
	encodedKey := encodeSecretKey(decodedSk)

	if bytes.Compare(sk, encodedKey) != 0 {
		t.Error("Secret Key encode/decode failed")
	}
}

func TestPublicKeyDecodeEncode(t *testing.T) {
	pk, _ := base64.StdEncoding.DecodeString("Balv0XL1rl4IfgdB2CPQppjUhKdosF/3eFZzTaEUBpDsXTiUp1YK2lIUkDjBYoABasPZp9czphimFwdppPOvOLJO1/o+1DI5Wzb0+lDxcRkCakM22vDqIRhiu0Vc7skAOzyGTgUZFNW82KAp90tTm8WoGLJvY/ABnM5TIx5grIALsGGtHlj1FIghsw/UE4kAZgAPtxosLCizbmRj/xEpblUdaJHyertCSay144sBW9QZQ8G0wmnEWyRyWrxP+ygBvBzhP83qoL3psorInUJ8tW9Qsq5/9EEyTN/y3/PNU4oNCA0O8qGeCAULOWaoN6EA49azCoGV46UrFGfij56tKtqlBjqE3H1/OqUrPvfCqfraJUsd2kbVATSSu1FKr60BlAqhAHb0Hzq+KcblzgPaa2X5xanruNrO3xAkVyA5+gGzER3Le4eenJFGGwiI/CABU63OS5are+zZRZVJJC5SJcd0WbNW7rsmd1Izz0A6pJhwaiFwT2etyXIPPA0MChMA03AaI+eW8yspyVaUoK2El4LrXvsZcQmrueTdk7dqI6mvQhnHotTcG4e3SwBk2DYAyCOfZH1kXh2XgZ6hmUZvhaq7Z/+B87EpYdiyzGYZ4g0C+XPurMNKH8loa3qLan0BHrlWvIapCNK3fbK1YIrLcNYCciHsDlDTJUFIWTa+niOyMV2cJdEGsDbvfb+6/TcB/4oGXE9Rf+zH5pkt79VrS6fuIzhr6ihSlCsRRdjCsLIqLljkm5eRYb/y0GguVHUA")
	decodedPk, err := decodePublicKey(pk)
	if err != nil {
		t.Error("Failed to decode")
	}
	fmt.Println(decodedPk)
	encodedKey := encodePublicKey(decodedPk)

	if bytes.Compare(pk, encodedKey) != 0 {
		t.Error("Secret Key encode/decode failed")
	}
}

func TestSignatureDecodeEncode(t *testing.T) {
	s, _ := base64.StdEncoding.DecodeString("p/KXh7G5yQM7ToXZIZdlXEgv1GpllFtqfANrUG06pYLCSTtb3MPfi3hjBVhaCdIA4H2MQpalRDIqh9XtbTjUJ5n6YpEs0X8yzSrouArkVogzjm8FWmLfWAwt6J8bGJ8BuXiQrB8/i1NOHTC3N2MiywogKxh5h1p00eDDmmX6loQHZFyNkIG4oYCw1rFqtqAB1ZG0wkO5kehrLwQVxq3vRZLjddNOvcJeEqLl1bKXujsqKqGxvrYh+SUgaiZAfAoAVX1T40ezVnY/IEEXCfJNFAUfpK+CbS6VAkYXBbeE+pwwR5A2tBmrujcm7D2SNF4ABuLeCw6HhK94C7ClMaVIRnWqrUkn5bfwCkxIM52S023Ec9YK6W9zUX5fZxa6y6AAy9K1jolFeo89IvFkEiU6HP97kHclyFBFkz/lMqZg/f7bKhpa0RCNDmOarILUp3AB6TloFUSnSEJC2PtgKI41wTmS74XFdwb1pdqdlJ7y3u+YKUVQxvi/QwPHyPOkZf4AQj7VCwsfZJ2VxG034xLyTVq+ef9j9ZBDxLEjGFbUYigC90oM4g08cpyNgdAWRdoAD93HdxwWMDtdIWooQXpI19uhlBn9OpMxXeKiVKJv9fLKXNOor0HgK0IQ9+ZhbAMAA+KXE6AOK0C1zuxfXYvtXMmh0kawNDWa5ZQVSSdAT6h6+FHOiPvcdF/DL+FkV6EBM6IAVdvrQEhrEjzPqoaarDMCI5HCHIx93SxcDGL399vnjGm0tyNNy5ioUZplH5MAna1LzhveTCGTPVgPEQnEiKprmXjPAyDO9LDQWejc3ZWRMsX6tMM8dTwt14GQZuMA8g5UKS5XT4NuDZ/x8K8sSnkRbM4ukoyHfY+4ZCkwOq3RFBWiTZQwaEuiiKoLQiIBRyYjwNRYcmspJWXsIRiS4CnykUVARNH7Wy4sXa700i6rInf247TGrfjrkJ6K0vEAAAAAAAAAAAABAAAAAAAAAAEAAAAAAAAAKAEAAAAAAAApIvM5TNE+WsJq5+abyziUcjunzM2VmPFeLhWrFe2gZowawWjX6M4d0eeDi3FtjgFqjhsOZuxy813b/g+2Q/7cvBsDUX8UjFZZgFobjC8DWt5H6nguoPJf2L/KYCz1RQHu0P9DrXFK2GU/ogWg+8Zi/HOIuPlc3YOY+hetYzP0tgu377vg4nWqf4+iLE9G1gBTCmD8HCstD3LJGdmHdA0ZCuO+1Cs81Ku5jfz+lQuDkRRhc3XNo6BkLxfAcrk7fgButx5Qnl4cj+V49lD2pLDD6O/pB7JqVmN/XWeflIuDjw2Z9dmOjPcbFYv3vCGmmAE4yhcvbU/9Ra0FdzE2ps5A+mMiKEmfuqTnhOj2I3sHQVLRM6VbLGdHy3rmsykglwEAAAAAAAAAAA==")
	sig, err := decodeSignature(s)
	if err != nil {
		t.Error("Failed to decode")
	}
	encodedS := encodeSignature(sig.ptr)

	if bytes.Compare(s, encodedS) != 0 {
		t.Error("Failed to decode/encode signature")
	}
}

func TestMultiSignDecodeEncode(t *testing.T) {
	ms, _ := base64.StdEncoding.DecodeString("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")
	sig, err := decodeMultiSign(ms)
	if err != nil {
		t.Error("Failed to decode")
	}
	encodedMs := encodeMultiSign(sig.ptr)

	if bytes.Compare(ms, encodedMs) != 0 {
		t.Error("Failed to decode/encode multi sign")
	}
}

func TestInitializerDecodeEncode(t *testing.T) {
	initializerBytes, _ := base64.StdEncoding.DecodeString("AwAAAAAAAAAEAAAAAAAAAGQAAAAAAAAAAQAAAAAAAAAAAAAAAADwP49DkVwbhb90CumR9I5JE0WWZmsR0+wuTzcBLrmSsEAKBalv0XL1rl4IfgdB2CPQppjUhKdosF/3eFZzTaEUBpDsXTiUp1YK2lIUkDjBYoABasPZp9czphimFwdppPOvOLJO1/o+1DI5Wzb0+lDxcRkCakM22vDqIRhiu0Vc7skAOzyGTgUZFNW82KAp90tTm8WoGLJvY/ABnM5TIx5grIALsGGtHlj1FIghsw/UE4kAZgAPtxosLCizbmRj/xEpblUdaJHyertCSay144sBW9QZQ8G0wmnEWyRyWrxP+ygBvBzhP83qoL3psorInUJ8tW9Qsq5/9EEyTN/y3/PNU4oNCA0O8qGeCAULOWaoN6EA49azCoGV46UrFGfij56tKtqlBjqE3H1/OqUrPvfCqfraJUsd2kbVATSSu1FKr60BlAqhAHb0Hzq+KcblzgPaa2X5xanruNrO3xAkVyA5+gGzER3Le4eenJFGGwiI/CABU63OS5are+zZRZVJJC5SJcd0WbNW7rsmd1Izz0A6pJhwaiFwT2etyXIPPA0MChMA03AaI+eW8yspyVaUoK2El4LrXvsZcQmrueTdk7dqI6mvQhnHotTcG4e3SwBk2DYAyCOfZH1kXh2XgZ6hmUZvhaq7Z/+B87EpYdiyzGYZ4g0C+XPurMNKH8loa3qLan0BHrlWvIapCNK3fbK1YIrLcNYCciHsDlDTJUFIWTa+niOyMV2cJdEGsDbvfb+6/TcB/4oGXE9Rf+zH5pkt79VrS6fuIzhr6ihSlCsRRdjCsLIqLljkm5eRYb/y0GguVHUA")
	secretKeyRaw, _ := base64.StdEncoding.DecodeString("j0ORXBuFv3QK6ZH0jkkTRZZmaxHT7C5PNwEuuZKwQAo=")
	publicKeyRaw, _ := base64.StdEncoding.DecodeString("Balv0XL1rl4IfgdB2CPQppjUhKdosF/3eFZzTaEUBpDsXTiUp1YK2lIUkDjBYoABasPZp9czphimFwdppPOvOLJO1/o+1DI5Wzb0+lDxcRkCakM22vDqIRhiu0Vc7skAOzyGTgUZFNW82KAp90tTm8WoGLJvY/ABnM5TIx5grIALsGGtHlj1FIghsw/UE4kAZgAPtxosLCizbmRj/xEpblUdaJHyertCSay144sBW9QZQ8G0wmnEWyRyWrxP+ygBvBzhP83qoL3psorInUJ8tW9Qsq5/9EEyTN/y3/PNU4oNCA0O8qGeCAULOWaoN6EA49azCoGV46UrFGfij56tKtqlBjqE3H1/OqUrPvfCqfraJUsd2kbVATSSu1FKr60BlAqhAHb0Hzq+KcblzgPaa2X5xanruNrO3xAkVyA5+gGzER3Le4eenJFGGwiI/CABU63OS5are+zZRZVJJC5SJcd0WbNW7rsmd1Izz0A6pJhwaiFwT2etyXIPPA0MChMA03AaI+eW8yspyVaUoK2El4LrXvsZcQmrueTdk7dqI6mvQhnHotTcG4e3SwBk2DYAyCOfZH1kXh2XgZ6hmUZvhaq7Z/+B87EpYdiyzGYZ4g0C+XPurMNKH8loa3qLan0BHrlWvIapCNK3fbK1YIrLcNYCciHsDlDTJUFIWTa+niOyMV2cJdEGsDbvfb+6/TcB/4oGXE9Rf+zH5pkt79VrS6fuIzhr6ihSlCsRRdjCsLIqLljkm5eRYb/y0GguVHUA")

	initializer, err := decodeInitializer(initializerBytes)
	if err != nil {
		t.Error("Failed to decode")
	}

	if initializer.PartyId() != 3 {
		t.Errorf("PartyID(): expected 3 but got %d\n", initializer.PartyId())
	}

	if initializer.Stake() != 4 {
		t.Errorf("Stake(): expected 4 but got %d\n", initializer.Stake())
	}

	if bytes.Compare(initializer.SecretKey(), secretKeyRaw) != 0 {
		t.Error("Decoded secret key is invalid")
	}

	if bytes.Compare(initializer.PublicKey(), publicKeyRaw) != 0 {
		t.Error("Decoded public key is invalid")
	}

	if bytes.Compare(encodeInitializer(initializer), initializerBytes) != 0 {
		t.Error("Failed to encode initializer")
	}
}

func TestStmInitializerRefreshKeys(t *testing.T) {
	params := Parameters{K: 1, M: 100, PhiF: 1.0}
	i, err := NewInitializer(params, 3, 4)
	if err != nil {
		t.Error("Failed to decode")
	}

	oldKey := i.SecretKey()
	if err := i.RefreshKeys(); err != nil {
		t.Error("Failed to refresh")
	}

	if bytes.Compare(oldKey, i.SecretKey()) == 0 {
		t.Error("Initializer keys refresh failed.")
	}
}

func TestMultiSignWithStaticKeys(t *testing.T) {
	const (
		neededSigns = 5
		totalSigns  = 100
		signMsg     = "hi"
	)

	// params := NewStmtParams(neededSigns, totalSigns, 0.2)
	rawInitializers := []string{
		"AQAAAAAAAAABAAAAAAAAAGQAAAAAAAAABQAAAAAAAACamZmZmZnJP6kPGsFQOUFljlOuWOn4U8p9w+HyhLiNSCSq3ARfZEUN1ZG0wkO5kehrLwQVxq3vRZLjddNOvcJeEqLl1bKXujsqKqGxvrYh+SUgaiZAfAoAVX1T40ezVnY/IEEXCfJNFAUfpK+CbS6VAkYXBbeE+pwwR5A2tBmrujcm7D2SNF4ABuLeCw6HhK94C7ClMaVIRnWqrUkn5bfwCkxIM52S023Ec9YK6W9zUX5fZxa6y6AAy9K1jolFeo89IvFkEiU6HP97kHclyFBFkz/lMqZg/f7bKhpa0RCNDmOarILUp3AB6TloFUSnSEJC2PtgKI41wTmS74XFdwb1pdqdlJ7y3u+YKUVQxvi/QwPHyPOkZf4AQj7VCwsfZJ2VxG034xLyTVq+ef9j9ZBDxLEjGFbUYigC90oM4g08cpyNgdAWRdoAD93HdxwWMDtdIWooQXpI19uhlBn9OpMxXeKiVKJv9fLKXNOor0HgK0IQ9+ZhbAMAA+KXE6AOK0C1zuxfXYvtXMmh0kawNDWa5ZQVSSdAT6h6+FHOiPvcdF/DL+FkV6EBM6IAVdvrQEhrEjzPqoaarDMCI5HCHIx93SxcDGL399vnjGm0tyNNy5ioUZplH5MAna1LzhveTCGTPVgPEQnEiKprmXjPAyDO9LDQWejc3ZWRMsX6tMM8dTwt14GQZuMA8g5UKS5XT4NuDZ/x8K8sSnkRbM4ukoyHfY+4ZCkwOq3RFBWiTZQwaEuiiKoLQiIBRyYjwNRYcmspJWXsIRiS4CnykUVARNH7Wy4sXa700i6rInf247TGrfjrkJ6K0vEA",
		"AgAAAAAAAAAAAAAAAAAAAGQAAAAAAAAABQAAAAAAAACamZmZmZnJP+ZEhqwjcwbKuczhR2VoNVv3Q0So43f3dbe1j+3sMHgPKSLzOUzRPlrCaufmm8s4lHI7p8zNlZjxXi4VqxXtoGaMGsFo1+jOHdHng4txbY4Bao4bDmbscvNd2/4PtkP+3LwbA1F/FIxWWYBaG4wvA1reR+p4LqDyX9i/ymAs9UUB7tD/Q61xSthlP6IFoPvGYvxziLj5XN2DmPoXrWMz9LYLt++74OJ1qn+PoixPRtYAUwpg/BwrLQ9yyRnZh3QNGQrjvtQrPNSruY38/pULg5EUYXN1zaOgZC8XwHK5O34AbrceUJ5eHI/lePZQ9qSww+jv6QeyalZjf11nn5SLg48NmfXZjoz3GxWL97whppgBOMoXL21P/UWtBXcxNqbOQPpjIihJn7qk54To9iN7B0FS0TOlWyxnR8t65rMpIJcBqjpLRLgmHKHRv+hAe1IhnWt18p0pVitlo8ScmJXW5xMkds6BsCP3E9+ytzsKEhEAi3b4oQGT/oyK9ck8pIdo9PDwsvJG+aDzM7JVv+SP2O85vQazwvDgTq+RLWER4GkB6BUMBVBNWMU2qedy2tglpJzqvJsVe0PAgBXFaX3bBi7V7GPd6ZHMFYz+/MBqTXUB3H79C0m6cq7r7R7eEx6W3dwXV74BCpd8ETQkL5OP48P9QyazlQC5279IUdxd7ioA0GJVpImtAB0R+pVqO+f2kOWz+MlOynzrFUGDFYArDQE2C4FtDpi82DRxuaPMtpMAATA6VqH3w6+TZMUy4qBHi3d4XBxIqX/AD+h899untxOyj9lbbKICfwtWqTQTYdIA",
	}

	var me *Initializer
	var participants []*Participant
	var err error

	// Step #1. Done by initializer
	data, _ := base64.StdEncoding.DecodeString(rawInitializers[0])
	me, err = decodeInitializer(data)
	if err != nil {
		t.Error("Failed to decode")
	}

	participants = append(participants, me.Participant())

	for i := 1; i < len(rawInitializers); i++ {
		data, _ := base64.StdEncoding.DecodeString(rawInitializers[i])
		initializer, err := decodeInitializer(data)
		if err != nil {
			t.Error("Failed to decode")
		}

		participants = append(participants, initializer.Participant())
		initializer.Free()
	}

	// Step #2. Done by particular node.
	signer, err := NewSigner(me, participants)
	if err != nil {
		t.Error("Failed to create signer")
	}

	success := 0
	indices := make([]uint64, neededSigns)

	var i uint64
	for i = 0; i < totalSigns && success < neededSigns; i++ {
		if signer.EligibilityCheck(i, signMsg) {
			indices[success] = i
			success++
		}
	}

	if success < neededSigns {
		t.Error("Not eligible to sign enough indices")
	}

	fmt.Println("indices", indices)

	var signatures []*Signature
	for i = 0; i < neededSigns; i++ {
		sign, err := signer.Sign(indices[i], signMsg)
		if err != nil {
			t.Error("Failed to sign message")
		}
		signatures = append(signatures, sign)
	}

	// Step #3. Aggregate all signatures.
	clerk, err := signer.Clerk()
	if err != nil {
		t.Error("Failed to get signer")
	}

	defer clerk.Free()

	for i := 0; i < neededSigns; i++ {
		if err := clerk.VerifySign(signMsg, indices[i], signatures[i]); err != nil {
			t.Errorf("Signature %d invalid\n", i)
		}
	}

	multiSign, err := clerk.Aggregate(signatures, signMsg)
	if err != nil {
		t.Error("Failed to aggregate")
	}

	if err := clerk.VerifyMultiSign(multiSign, signMsg); err != nil {
		t.Error("Failed to verify multiSign")
	}
}

func TestMultiSignWithStaticKeys2(t *testing.T) {
	const (
		neededSigns = 5
		totalSigns  = 100
		signMsg     = "hi"
	)

	// params := NewStmtParams(neededSigns, totalSigns, 0.2)
	rawInitializers := []string{
		"AQAAAAAAAAABAAAAAAAAAGQAAAAAAAAABQAAAAAAAACamZmZmZnJP6kPGsFQOUFljlOuWOn4U8p9w+HyhLiNSCSq3ARfZEUN1ZG0wkO5kehrLwQVxq3vRZLjddNOvcJeEqLl1bKXujsqKqGxvrYh+SUgaiZAfAoAVX1T40ezVnY/IEEXCfJNFAUfpK+CbS6VAkYXBbeE+pwwR5A2tBmrujcm7D2SNF4ABuLeCw6HhK94C7ClMaVIRnWqrUkn5bfwCkxIM52S023Ec9YK6W9zUX5fZxa6y6AAy9K1jolFeo89IvFkEiU6HP97kHclyFBFkz/lMqZg/f7bKhpa0RCNDmOarILUp3AB6TloFUSnSEJC2PtgKI41wTmS74XFdwb1pdqdlJ7y3u+YKUVQxvi/QwPHyPOkZf4AQj7VCwsfZJ2VxG034xLyTVq+ef9j9ZBDxLEjGFbUYigC90oM4g08cpyNgdAWRdoAD93HdxwWMDtdIWooQXpI19uhlBn9OpMxXeKiVKJv9fLKXNOor0HgK0IQ9+ZhbAMAA+KXE6AOK0C1zuxfXYvtXMmh0kawNDWa5ZQVSSdAT6h6+FHOiPvcdF/DL+FkV6EBM6IAVdvrQEhrEjzPqoaarDMCI5HCHIx93SxcDGL399vnjGm0tyNNy5ioUZplH5MAna1LzhveTCGTPVgPEQnEiKprmXjPAyDO9LDQWejc3ZWRMsX6tMM8dTwt14GQZuMA8g5UKS5XT4NuDZ/x8K8sSnkRbM4ukoyHfY+4ZCkwOq3RFBWiTZQwaEuiiKoLQiIBRyYjwNRYcmspJWXsIRiS4CnykUVARNH7Wy4sXa700i6rInf247TGrfjrkJ6K0vEA",
		"AgAAAAAAAAAAAAAAAAAAAGQAAAAAAAAABQAAAAAAAACamZmZmZnJP+ZEhqwjcwbKuczhR2VoNVv3Q0So43f3dbe1j+3sMHgPKSLzOUzRPlrCaufmm8s4lHI7p8zNlZjxXi4VqxXtoGaMGsFo1+jOHdHng4txbY4Bao4bDmbscvNd2/4PtkP+3LwbA1F/FIxWWYBaG4wvA1reR+p4LqDyX9i/ymAs9UUB7tD/Q61xSthlP6IFoPvGYvxziLj5XN2DmPoXrWMz9LYLt++74OJ1qn+PoixPRtYAUwpg/BwrLQ9yyRnZh3QNGQrjvtQrPNSruY38/pULg5EUYXN1zaOgZC8XwHK5O34AbrceUJ5eHI/lePZQ9qSww+jv6QeyalZjf11nn5SLg48NmfXZjoz3GxWL97whppgBOMoXL21P/UWtBXcxNqbOQPpjIihJn7qk54To9iN7B0FS0TOlWyxnR8t65rMpIJcBqjpLRLgmHKHRv+hAe1IhnWt18p0pVitlo8ScmJXW5xMkds6BsCP3E9+ytzsKEhEAi3b4oQGT/oyK9ck8pIdo9PDwsvJG+aDzM7JVv+SP2O85vQazwvDgTq+RLWER4GkB6BUMBVBNWMU2qedy2tglpJzqvJsVe0PAgBXFaX3bBi7V7GPd6ZHMFYz+/MBqTXUB3H79C0m6cq7r7R7eEx6W3dwXV74BCpd8ETQkL5OP48P9QyazlQC5279IUdxd7ioA0GJVpImtAB0R+pVqO+f2kOWz+MlOynzrFUGDFYArDQE2C4FtDpi82DRxuaPMtpMAATA6VqH3w6+TZMUy4qBHi3d4XBxIqX/AD+h899untxOyj9lbbKICfwtWqTQTYdIA",
	}

	var me *Initializer
	var participants []*Participant
	var err error

	// Step #1. Done by initializer
	data, _ := base64.StdEncoding.DecodeString(rawInitializers[0])
	me, err = decodeInitializer(data)
	if err != nil {
		t.Error("Failed to decode")
	}

	participants = append(participants, me.Participant())

	for i := 1; i < len(rawInitializers); i++ {
		data, _ := base64.StdEncoding.DecodeString(rawInitializers[i])
		initializer, err := decodeInitializer(data)
		if err != nil {
			t.Error("Failed to decode")
		}

		participants = append(participants, initializer.Participant())
		initializer.Free()
	}

	// Step #2. Done by particular node.
	signer, err := NewSigner(me, participants)
	if err != nil {
		t.Error("Failed to create signer")
	}

	success := 0
	indices := make([]uint64, neededSigns)

	var i uint64
	for i = 0; i < totalSigns && success < neededSigns; i++ {
		if signer.EligibilityCheck(i, signMsg) {
			indices[success] = i
			success++
		}
	}

	if success < neededSigns {
		t.Error("Not eligible to sign enough indices")
	}

	var signatures []*Signature
	for i = 0; i < neededSigns; i++ {
		sign, err := signer.Sign(indices[i], signMsg)
		if err != nil {
			t.Error("Failed to sign message")
		}
		signatures = append(signatures, sign)
	}

	// Step #3. Aggregate all signatures.
	clerk, err := signer.Clerk()
	if err != nil {
		t.Error("Failed to get clerk")
	}

	defer clerk.Free()

	for i := 0; i < neededSigns; i++ {
		if err := clerk.VerifySign(signMsg, indices[i], signatures[i]); err != nil {
			t.Errorf("Signature %d invalid\n", i)
		}
	}

	multiSign, err := clerk.Aggregate(signatures, signMsg)
	if err != nil {
		t.Error("Failed to aggregate")
	}

	if err := clerk.VerifyMultiSign(multiSign, signMsg); err != nil {
		t.Error("Failed to verify multiSign")
	}

	vClerk, err := NewClerk(Parameters{K: neededSigns, M: totalSigns, PhiF: 0.2}, participants)
	if err != nil {
		t.Error("Failed to get clerk")
	}

	if err := vClerk.VerifyMultiSign(multiSign, signMsg); err != nil {
		t.Error("Clerk Failed to verify multiSig")
	}
}
