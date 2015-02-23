package config

import "testing"

func TestConfigDB(t *testing.T) {
	c, err := DBConfig(EnvProduction, "config.toml")
	if err != nil {
		t.Fatal(err)
	}

	const expectedProdDialect = "postgres"
	if c.Driver != expectedProdDialect {
		t.Fatalf("expected %s, but got %s", expectedProdDialect, c.Driver)
	}
}
