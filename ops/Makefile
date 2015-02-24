PLAY = ansible-playbook

bootstrap:
	$(PLAY) ./playbooks/localhost.yml

HTTPD_DIR = ../go/cmd/abalone_httpd
HTTPD_BINARY = ./build/abalone_httpd
.PHONY: ./build/abalone_httpd
build/abalone_httpd: build $(HTTPD_DIR)
	GOOS=linux go build -o $(HTTPD_BINARY) $(HTTPD_DIR)

build:
	mkdir build

httpd_auto_deploy: build/abalone_httpd http_deploy

httpd_deploy:
	$(PLAY) playbooks/httpd_deploy.yml
