package config

type Config struct {
	PostgresDSN string `mapstructure:"postgre_dsn"`
	Server      struct {
		Port int    `mapstructure:"port"`
		Host string `mapstructure:"host"`
	} `mapstructure:"server"`
	Mithril struct {
		PartyId int64 `mapstructure:"party_id"`
		Params  struct {
			K    uint64  `mapstructure:"k"`
			M    uint64  `mapstructure:"m"`
			PhiF float64 `mapstructure:"phi_f"`
		} `mapstructure:"params"`
		Participants []struct {
			PartyId uint64 `mapstructure:"party_id"`
			Stake   uint64 `mapstructure:"stake"`
			Key     string `mapstructure:"key"`
		} `mapstructure:"participants"`
	} `mapstructure:"mithril"`
}