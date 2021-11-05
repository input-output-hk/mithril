package node

const (
	helloMessage = "hello"
)

type Message struct {
	Type    string      `json:"type"`
	Payload interface{} `json:"payload"`
}

type Hello struct {
	Text string `mapstructure:"msg" json:"msg"`
}