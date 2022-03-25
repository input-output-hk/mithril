package config

import (
	"os"
	"strings"

	"github.com/pkg/errors"
	"github.com/spf13/viper"
)

// ReadConfigFromFile read node configuration
func ReadConfigFromFile(path string) (*Config, error) {
	viper.AddConfigPath("./configs")
	viper.SetConfigName(path)
	viper.AutomaticEnv()

	replacer := strings.NewReplacer(".", "_")
	viper.SetEnvKeyReplacer(replacer)

	err := viper.ReadInConfig()
	if err != nil {
		return nil, errors.Wrap(err, "Error reading config file")
	}

	config := &Config{}

	err = viper.Unmarshal(config)
	if err != nil {
		return nil, errors.Wrap(err, "Error parsing config file")
	}

	for _, e := range os.Environ() {
		if strings.Compare("TEST_RUN=true", e) == 0 {
			config.TestRun = true
		}
	}
	
	return config, nil
}

func Source() string {
	return "dev-config"
}
