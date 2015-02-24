#! /bin/bash

name=pg
img=postgres:9.4.1
port=5432
dbname=abalone
dbuser=postgres
dbpass=password

if [ "$1" == "dsn" ]
then
    echo "postgres://$dbuser:$dbpass@$(boot2docker ip)/$dbname?sslmode=disable"
    exit 0
fi

if [ "$1" == "dbreset" ]
then

    # stop the server if one already exists
    docker stop $name > /dev/null || true
    docker rm   $name > /dev/null || true

    # run the postgreql server in a daemonized docker container
    docker run -d -p $port:5432 -e "POSTGRES_PASSWORD=$dbpass" --name $name $img

    with_connection_to_postgresql_server () {
        docker run --rm --link $name:$name -e "PGPASSWORD=$dbpass" $img $@
    }

    # we expect the server to take a few seconds to boot up. the following
    # while loop does the right thing by waiting until the server is reachable,
    # so this sleep statement is not strictly necessary. however, the errors
    # that are printed to stdout during the first couple seconds of waiting are
    # distracting to the user. the sole purpose of this sleep statement is to
    # avoid spewing unactionable noise to the console in the interim.
    sleep 3

    while ! with_connection_to_postgresql_server psql -c \\conninfo -h $name -U $dbuser | grep "You are connected"
    do
        echo "waiting for server to start"
        sleep 1
    done

    # connect to the running server and create the database
    with_connection_to_postgresql_server createdb $dbname -h $name -U $dbuser

    echo using password: $dbpass

    echo ip of database server is $(boot2docker ip)
fi
