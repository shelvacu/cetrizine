# Cetrizine

Cetrizine is a discord bot that hopes to archive <strike>*everything*</strike> all messages, images, attachments, channel/server structure, and all data sent and/or stored by discord.

## Setup database

Cetrizine requires a Postgresql 11 database. See for example https://tecadmin.net/install-postgresql-server-on-ubuntu/

1.  Create a database:

        createdb cetrizine

2.  Figure out what the path is for your database, which starts with `postgres://`

    *   If you're running for development and want to connect to a local database via unix socket, the path will look like (although     see note about special characters):
    
            postgres://$USER@%2Fvar%2Frun%2Fpostgresql:$PORT/$DB
        
        So, if your username is `shelvacu`, postgres is running on port `5432` (the default if there is one version of postgres running on the system), and the database name is `cetrizine` as above, the path will be:

            postgres://shelvacu@%2Fvar%2Frun%2Fpostgresql:5432/cetrizine
    
    *   If you're connecting via TCP with a username and password, it's pretty straightforward (but see note about special characters):

            postgres://$USER:$PASS@$IP_OR_DOMAIN:$PORT/$DB

        Example:

            postgres://shelvacu:letmein@awesomedb.example.com:5432/cetrizine
    
    *   A note about special characters: You must url-encode any special characters (such as `%`,`/`,`@`,`"`,`,`, etc) in the user, pass, or *shudders* database name. You can do this with Ruby, Python, or whatever you're comfortable with.

        Examples, assuming the password is `"abc/def@ghi`

            ruby -ruri -e 'puts URI.encode_www_form_component(ARGV[0])' '"abc/def@ghi'

        <!-- -->

            python3 -c 'import urllib.parse;import sys;print(urllib.parse.quote(sys.argv[1], safe=""))' '"abc/def@ghi'

        These should both give you `%22abc%2Fdef%40ghi`

### Compile and run

```bash
# grab the repo:
git clone --recurse-submodules https://github.com/shelvacu/cetrizine
cd cetrizine
# build (development mode):
cargo build --help
# build (development mode) if needed and run:
cargo run -- --help
# build (release mode)
cargo build --release
# build (release mode) if needed and run:
cargo run --release -- --help
```

Cetrizine requires the postgres path from above and a discord token which you can get by making an application at https://discordapp.com/developers/applications/. **The first time cetrizine runs** (when the database is empty) **you must also supply `--init-db`**.

The postgres path and discord token can both be specified as an argument or pulled from the environment:

```
./target/release/cetrizine --token "discord.token.here" --postgres-path "postgres://path/db --init-db"
```

```
DISCORD_TOKEN="discord.token.here" POSTGRES_PATH="postgres://path/db" ./target/release/cetrizine --init-db
```

## License

Cetrizine is wholly distributed under the terms of both the MIT license
and the Apache License (Version 2.0), at your option.

See [LICENSE-APACHE](LICENSE-APACHE) and [LICENSE-MIT](LICENSE-MIT) for details.
