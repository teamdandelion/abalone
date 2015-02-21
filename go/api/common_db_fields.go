package api

import "time"

type CommonDBFields struct {
	ID        int64 `gorm:"column:id"`
	CreatedAt time.Time
	UpdatedAt time.Time
	DeletedAt time.Time
}
