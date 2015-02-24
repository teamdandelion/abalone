PLAY = ansible-playbook

bootstrap:
	$(PLAY) ./playbooks/localhost.yml

HTTPD_DIR = ../go/cmd/abalone_httpd
HTTPD_BINARY = ./build/abalone_httpd
.PHONY: ./build/abalone_httpd
build/abalone_httpd: build $(HTTPD_DIR)
	GOOS=linux go build -o $(HTTPD_BINARY) $(HTTPD_DIR)

	# ensure that the binary is compiled for linux/amd64
	file $(HTTPD_BINARY) | grep "ELF 64"

build/public:
	../fe2/node_modules/.bin/gulp

build:
	mkdir build

httpd_auto_deploy: build/abalone_httpd http_deploy

httpd_deploy:

	# the vault password is used to decrypt the file that contains the database
	# password. The database password is required to execute this task.
	# 
	$(PLAY) playbooks/httpd_deploy.yml --ask-vault-pass

http_check_status:
	ansible http -m shell -a "initctl status abalone_httpd"
