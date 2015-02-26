http_check_status: ansible
	ansible http -m shell -a "initctl status abalone_httpd"
	ansible http -m shell -a "tail /var/abalone/log"

localhost_bootstrap: ansible
	ansible-playbook ./playbooks/localhost_bootstrap.yml

HTTPD_SRC_DIR = ../go/cmd/abalone_httpd
HTTPD_BINARY = ./build/abalone_httpd

.PHONY: build/abalone_httpd
build/abalone_httpd: build $(HTTPD_SRC_DIR)
	GOOS=linux go build -o $(HTTPD_BINARY) $(HTTPD_SRC_DIR)

	# ensure that the binary is compiled for linux/amd64
	file $(HTTPD_BINARY) | grep "ELF 64"

.PHONY: build/public
build/public:
	cd ../fe2 && ./node_modules/.bin/gulp compile
	mkdir -p build/public
	cp -r ../fe2/build/ build/public

build:
	mkdir build

frontend_deploy: build/public ansible
	ansible-playbook ./playbooks/frontend_deploy.yml

httpd_auto_deploy: build/abalone_httpd httpd_deploy

httpd_deploy: $(HTTPD_BINARY) ansible

	# The vault password decrypts the file that contains the database password.
	# This task requires the database password.
	#
	ansible-playbook playbooks/httpd_deploy.yml --ask-vault-pass

configure_servers: ansible
	ansible-playbook app.yml --ask-vault-pass

ansible:
	type ansible

clean:
	rm -rf build
