HTTPD_DIR = build/var/abalone
STATIC_BUILD = ../../../fe2/build
DBCTL = dbctl.sh

watch: $(HTTPD_DIR) rerun
	rerun github.com/danmane/abalone/go/cmd/abalone_httpd -debug -static $(STATIC_BUILD) -dir $(HTTPD_DIR) -postgres `bash $(DBCTL) dsn`

rerun:
	go get github.com/skelterjohn/rerun

$(HTTPD_DIR):
	mkdir -p build/var/abalone/players

dbreset:
	bash $(DBCTL) dbreset
