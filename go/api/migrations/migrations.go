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
	func(txn migration.LimitedTx) error {
		q := `
		CREATE TABLE matches
		(
			id bigserial NOT NULL,

			created_at timestamp with time zone,
			updated_at timestamp with time zone,

			player_1_id bigint,
			player_2_id bigint,

			FOREIGN KEY (player_1_id) REFERENCES players (id) ON DELETE RESTRICT,
			FOREIGN KEY (player_2_id) REFERENCES players (id) ON DELETE RESTRICT,
			CONSTRAINT matches_pkey PRIMARY KEY (id)
		);
		`
		if _, err := txn.Exec(q); err != nil {
			return err
		}
		return nil
	},
}
