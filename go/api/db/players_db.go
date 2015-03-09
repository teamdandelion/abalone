package db

import (
	"bytes"
	"io"
	"io/ioutil"
	"log"
	"os"
	"path"

	"github.com/briantigerchow/go-multihash/multihash"
	"github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/operator"
)

type playersDB struct {
	*resources
}

func (s *playersDB) Create(userID int64, p api.Player) (*api.Player, error) {
	p.AuthorId = userID
	if err := s.DB.Create(&p).Error; err != nil {
		return nil, err
	}
	return &p, nil
}

func (s *playersDB) Upload(userID int64, p api.Player, executable io.Reader) (*api.Player, error) {

	var buf bytes.Buffer
	io.Copy(&buf, executable)
	exeBytes := buf.Bytes()

	mh, err := multihash.Sum(exeBytes, multihash.SHA2_256)
	if err != nil {
		return nil, err
	}

	hashOfExe := mh.HexString()
	exePath := path.Join(s.FilestoragePath, hashOfExe)

	if err := ioutil.WriteFile(exePath, exeBytes, os.ModePerm); err != nil {
		return nil, err
	}

	if err := operator.Validate(exePath, s.Ports); err != nil {
		return nil, err
	}
	log.Printf("successfully validated %s", p)

	p.AuthorId = userID
	p.Path = hashOfExe
	if err := s.DB.Create(&p).Error; err != nil {
		return nil, err
	}
	return &p, nil
}

func (s *playersDB) List() ([]api.Player, error) {
	var players []api.Player
	if err := s.DB.Find(&players).Error; err != nil {
		return nil, err
	}
	return players, nil
}

func (s *playersDB) Delete(id int64) error {
	return s.DB.Delete(api.Player{ID: id}).Error
}

var _ api.PlayersService = &playersDB{}
