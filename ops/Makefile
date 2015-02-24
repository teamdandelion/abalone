http_check_status:
	ansible http -m shell -a "initctl status abalone_httpd"
	ansible http -m shell -a "tail /var/abalone/log"

localhost_bootstrap:
	ansible-playbook ./playbooks/localhost_bootstrap.yml

HTTPD_SRC_DIR = ../go/cmd/abalone_httpd
HTTPD_BINARY = ./build/abalone_httpd

.PHONY: build/abalone_httpd
build/abalone_httpd: build $(HTTPD_SRC_DIR)
	GOOS=linux go build -o $(HTTPD_BINARY) $(HTTPD_SRC_DIR)

	# ensure that the binary is compiled for linux/amd64
	file $(HTTPD_BINARY) | grep "ELF 64"

build:
	mkdir build

httpd_auto_deploy: build/abalone_httpd httpd_deploy

httpd_deploy: $(HTTPD_BINARY)

	# the vault password is used to decrypt the file that contains the database
	# password. The database password is required to execute this task.
	#
	ansible-playbook playbooks/httpd_deploy.yml --ask-vault-pass

configure_servers:
	ansible-playbook app.yml --ask-vault-pass

clean:
	rm -rf build
