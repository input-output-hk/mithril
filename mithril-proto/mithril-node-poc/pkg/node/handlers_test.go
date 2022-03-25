package node

import (
	"bytes"
	"encoding/json"
	"fmt"
	"testing"
)

func TestHello(t *testing.T) {
	var stream []byte
	type Foo struct {
		Name string `json:"name"`
	}


	for i := 0; i < 2; i++ {
		f := Foo{Name: fmt.Sprintf("Name %d\n", i)}
		data, _ := json.Marshal(f)
		stream = append(stream, data...)
	}

	decoder := json.NewDecoder(bytes.NewReader(stream))
	for {
		var f Foo
		if err := decoder.Decode(&f); err != nil {
			break
		}
		fmt.Println(f)
	}
}

