
To run postgres using Docker, install Docker, then execute:
```
docker run -p 5432:5432 -e POSTGRES_PASSWORD=password postgres
```

Copy the example config file. 
```
cp example.toml config.toml
```

Then update the development database entry to match the address of your
Postgres server. If you're using boot2docker on OS X, check the IP of the
Postgres instance by executing:
```
boot2docker ip
> 192.168.59.103
```

Set the `open` entry to look something like this...

```
    open = "postgres://postgres:password@192.168.59.103/abdb_dev?sslmode=disable"
```

Ensure the username, password, and database name match what you have in your
local environment. In the example above, u, p, and d are `postgres`,
`password`, and `abalone` respectively.

Finally, create the database. In this case, we'll create `abdb_dev`. In can be
anything you'd like. Just make sure it matches the "open" string shown above:
```
createdb abdb_dev -h $(boot2docker ip) -p 5432 -U postgres
```
