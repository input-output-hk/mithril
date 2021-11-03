package config

type Config struct {
	Server struct {
		Port int    `mapstructure:"port"`
		Host string `mapstructure:"host"`
	} `mapstructure:"server"`

	Mithril struct {
		PartyId int64 `mapstructure:"party_id"`
		Params  struct {
			K    int     `mapstructure:"k"`
			M    int     `mapstructure:"m"`
			PhiF float64 `mapstructure:"phi_f"`
		} `mapstructure:"params"`
		Participants []struct {
			PartyId int64  `mapstructure:"party_id"`
			Stake   int64  `mapstructure:"stake"`
			Key     string `mapstructure:"key"`
		} `mapstructure:"participants"`
	} `mapstructure:"mithril"`
}
