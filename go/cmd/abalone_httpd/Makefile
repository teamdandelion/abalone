HTTPD_DIR = build/var/abalone
STATIC_BUILD = ../../../fe2/build
DBCTL = dbctl.sh
PKG_PATH = github.com/danmane/abalone/go/cmd/abalone_httpd

watch: $(HTTPD_DIR) rerun
	rerun $(PKG_PATH) -debug -static $(STATIC_BUILD) -dir $(HTTPD_DIR) -postgres `bash $(DBCTL) dsn`

run:
	go run main.go -debug -static $(STATIC_BUILD) -dir $(HTTPD_DIR) -postgres `bash $(DBCTL) dsn`

rerun:
	go get github.com/skelterjohn/rerun

$(HTTPD_DIR):
	mkdir -p build/var/abalone/players

dbreset:
	bash $(DBCTL) dbreset
