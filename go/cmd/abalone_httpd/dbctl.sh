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
    docker run -v -d -p $port:5432 -e "POSTGRES_PASSWORD=$dbpass" --name $name $img

    with_connection_to_postgresql_server () {
        docker run --rm --link $name:$name -e "PGPASSWORD=$dbpass" $img $@
    }

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
