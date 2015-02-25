package httputil2

import (
	"bytes"
	"io"
	"mime/multipart"
	"net/http"
)

func NewFileUploadRequest(uri string, params map[string]string, paramName, filename string, executable io.Reader) (*http.Request, error) {

	var body bytes.Buffer
	writer := multipart.NewWriter(&body)
	part, err := writer.CreateFormFile(paramName, filename)
	if err != nil {
		return nil, err
	}
	_, err = io.Copy(part, executable)
	if err != nil {
		return nil, err
	}

	for key, val := range params {
		if err := writer.WriteField(key, val); err != nil {
			return nil, err

		}
	}
	if err := writer.Close(); err != nil {
		return nil, err
	}

	req, err := http.NewRequest("POST", uri, &body)
	if err != nil {
		return nil, err
	}
	req.Header.Add("Content-Type", writer.FormDataContentType())
	return req, nil
}
