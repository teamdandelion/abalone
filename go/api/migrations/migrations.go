package migrations

import "github.com/BurntSushi/migration"

// TODO(btc) find out about dope advanced constraints

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
			path character varying(255),

			created_at timestamp with time zone,
			updated_at timestamp with time zone,

			user_id bigint,

			FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE RESTRICT,
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
	func(txn migration.LimitedTx) error {

		// TODO how to represent initial state/game configuration

		q := `
		CREATE TABLE games
		(
			id bigserial NOT NULL,

			created_at timestamp with time zone,
			updated_at timestamp with time zone,

			match_id bigint NOT NULL,
			white_player_id bigint NOT NULL,
			black_player_id bigint NOT NULL,
			status character varying(140), 		-- 140 characters... leaving room for a tweet in the event of strong artificial embryogeny

			FOREIGN KEY (match_id) REFERENCES matches (id) ON DELETE RESTRICT,
			FOREIGN KEY (white_player_id) REFERENCES players (id) ON DELETE RESTRICT,
			FOREIGN KEY (black_player_id) REFERENCES players (id) ON DELETE RESTRICT,
			CONSTRAINT games_pkey PRIMARY KEY (id)
		);
		`
		if _, err := txn.Exec(q); err != nil {
			return err
		}
		return nil
	},
	func(txn migration.LimitedTx) error {

		q := `
		CREATE TABLE records
		(
			game_id bigint NOT NULL,
			turn_num bigint NOT NULL,

			created_at timestamp with time zone,
			updated_at timestamp with time zone,

			state bytea NOT NULL,

			FOREIGN KEY (game_id) REFERENCES games (id) ON DELETE RESTRICT,
			CONSTRAINT records_pkey PRIMARY KEY (game_id, turn_num)
		);
		`
		if _, err := txn.Exec(q); err != nil {
			return err
		}
		return nil
	},
	func(txn migration.LimitedTx) error {

		q := `
		ALTER TABLE games
		ADD COLUMN reason character varying(140);
		`
		if _, err := txn.Exec(q); err != nil {
			return err
		}
		return nil
	},
}
