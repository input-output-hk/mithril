package config

type Config struct {
	PostgresDSN string `mapstructure:"postgres_dsn"`
	Leader      bool   `mapstructure:"leader"`
	Http        struct {
		ServerAddr string `mapstructure:"listen_addr"`
	} `mapstructure:"http"`
	Mithril struct {
		PartyId uint64 `mapstructure:"party_id"`
		Params  struct {
			K    uint64  `mapstructure:"k"`
			M    uint64  `mapstructure:"m"`
			PhiF float64 `mapstructure:"phi_f"`
		} `mapstructure:"params"`
		Participants []struct {
			PartyId     uint64 `mapstructure:"party_id"`
			Stake       uint64 `mapstructure:"stake"`
			Initializer string `mapstructure:"initializer"`
		} `mapstructure:"participants"`
	} `mapstructure:"mithril"`
}
