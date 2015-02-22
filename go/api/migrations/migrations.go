package migrations

import "github.com/BurntSushi/migration"

var Migrations = []migration.Migrator{
	func(txn migration.LimitedTx) error {
		q := `
		CREATE TABLE users (
			id bigserial NOT NULL,

			name character varying(255),
			email character varying(255),

			created_at timestamp with time zone,
			updated_at timestamp with time zone,

			CONSTRAINT users_pkey PRIMARY KEY (id)
		);

		CREATE TABLE players
		(
			id bigserial NOT NULL,

			name character varying(255),
			version bigint,
			host character varying(255),

			created_at timestamp with time zone,
			updated_at timestamp with time zone,

			author_id bigint,

			FOREIGN KEY (author_id) REFERENCES users (id) ON DELETE RESTRICT,
			CONSTRAINT players_pkey PRIMARY KEY (id)
		);
		`
		if _, err := txn.Exec(q); err != nil {
			return err
		}
		return nil
	},
}
