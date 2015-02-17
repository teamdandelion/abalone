package abalone

import "encoding/json"

type Player int

const (
	NullPlayer Player = iota
	White
	Black
)

func (p Player) Next() Player {
	switch p {
	case White:
		return Black
	case Black:
		return White
	default:
		return NullPlayer
	}
}

func (p Player) String() string {
	switch p {
	case White:
		return "white"
	case Black:
		return "black"
	default:
		return "null"
	}
}

func (p Player) MarshalJSON() ([]byte, error) {
	return json.Marshal(p.String())
}

func (p *Player) UnmarshalJSON(data []byte) error {
	var s string
	if err := json.Unmarshal(data, &s); err != nil {
		return err
	}
	switch s {
	case "white":
		*p = White
	case "black":
		*p = Black
	default:
		*p = NullPlayer
	}
	return nil
}
