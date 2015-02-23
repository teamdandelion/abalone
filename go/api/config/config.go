package config

import (
	"io/ioutil"

	"github.com/BurntSushi/toml"
)

type Config struct {
	Database map[Environment]DB
}

type DB struct {
	Driver string `toml:"driver"`
	Open   string `toml:"open"`
}

type Environment string

const (
	EnvProduction  Environment = "production"
	EnvDevelopment Environment = "development"
	EnvTest        Environment = "test"
)

func DBConfig(env Environment, path string) (*DB, error) {

	bytes, err := ioutil.ReadFile(path)
	if err != nil {
		return nil, err
	}
	var conf Config
	if _, err := toml.Decode(string(bytes), &conf); err != nil {
	}
	db := conf.Database[env]
	return &db, nil
}
