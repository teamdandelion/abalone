package db

import (
	"bytes"
	"io"
	"io/ioutil"
	"os"
	"path"

	"github.com/briantigerchow/go-multihash/multihash"
	api "github.com/danmane/abalone/go/api"
	"github.com/jinzhu/gorm"
)

type playersDB struct {
	DB              *gorm.DB
	filestoragePath string // TODO extract blobstore
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

	if err := ioutil.WriteFile(path.Join(s.filestoragePath, hashOfExe), exeBytes, os.ModePerm); err != nil {
		return nil, err
	}

	// TODO(btc): test the player before saving

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
