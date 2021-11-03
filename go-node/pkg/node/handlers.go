package node

type Message struct {
	Type    string                 `json:"type"`
	Payload map[string]interface{} `json:"payload"`
}

type Hello struct {
	Msg string `mapstructure:"msg"`
}

type SignRequest struct {
}

type SignResponse struct {
	RequestId int64
	BlockId   int
}

type HandshakeParams struct{}

