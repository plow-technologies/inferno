-- -*- mode: sql; sql-product: postgres; -*-

create extension lo;

-- Status tag for various DB entities, for soft deletion
create type status as enum ('active', 'terminated');

create table if not exists users
  ( -- Note: this is the bson object ID represented as an integer
    id integer primary key
    -- Also a list of bson object IDs. This determines model access (see below)
  , groups integer[]
  );

create table if not exists models
  ( id serial primary key
  , name text not null
    -- Represented as a map from group IDs to model permissions (read or write),
    -- serialized to JSON. This is a bit more flexible than using an `hstore` and
    -- might allow us to include a more complex structure in the future more
    -- easily
  , permissions jsonb not null
  , "user" integer references users (id)
  , status status not null
  , unique (name, "user")
  );

create table if not exists mversions
  ( id serial primary key
  , model integer references models (id)
    -- Model card (description and metadata) serialized as JSON
  , card jsonb not null
  , contents oid not null
  , version text not null
  , status status not null
  , unique (version)
  );

create table if not exists scripts
  ( -- Since the hash uniquely identifies a script, it's probably fine
    -- to keep this as a `bytea`
    id bytea primary key
    -- Script closure
  , obj jsonb not null
  );

create table if not exists params
  ( id serial primary key
    -- Script hash from `inferno-vc`
  , script bytea not null references scripts (id)
  , model integer references models (id)
  , inputs jsonb
  , outputs jsonb
  , status status not null
  , "user" integer references users (id)
  );

create table if not exists instances
  ( -- Note: the AWS ID
    id text primary key
    -- Private IP
  , ip inet not null
  , status status not null
  , param integer references params (id)
  );

create table if not exists running
  ( id text primary key references instances (id)
  , since timestamptz not null
  );

create trigger "manage-mversion-lo" before update or delete on mversions
  for each row execute function lo_manage(contents);
