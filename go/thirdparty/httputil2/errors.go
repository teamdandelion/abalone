package httputil2

import (
	"bytes"
	"fmt"
	"io"
	"net/http"
)

func NewHTTPError(resp *http.Response) error {
	return fmt.Errorf("error: %s; %s", resp.Status, rtos(resp.Body))
}

func rtos(r io.Reader) string {
	var buf bytes.Buffer
	io.Copy(&buf, r)
	return buf.String()
}
