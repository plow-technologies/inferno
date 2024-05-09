-- -*- mode: sql; sql-product: postgres; -*-

create extension lo;

-- NOTE: Most of the table below have a `terminated timestamptz` field, which
-- are by default `null`. If the timestamp exists, that means the row has been
-- soft-deleted. For record-keeping, we probably don't want to drop DB entities
-- unless they have been deactivated for a certain amount of time
--
-- Tracking the distinction between active/terminated rows could be accomplished
-- using an enum; however, we would still need another field to record the time.
-- On the Haskell side, a `Just x` value for this field means terminated at
-- time `x`; a `Nothing` means that the entity is still active
--
-- `inferno-ml-server` will check for null timestamps when running params,
-- caching models, etc... If the field is not null, then the entity has been
-- "deleted" and cannot be used any longer

create table if not exists users
  ( -- Note: this is the bson object ID represented as an integer
    id integer primary key
    -- Also a list of bson object IDs. This determines model access (see below)
  , groups integer[] not null
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
    -- See note above
  , terminated timestamptz
  , unique (name, "user")
  );

create table if not exists mversions
  ( id serial primary key
  , model integer references models (id)
    -- Model card (description and metadata) serialized as JSON
  , card jsonb not null
    -- The model contents are not stored directly because it might exceed
    -- the 1GB column-size limit. Instead the model version contains a
    -- pointer to a Postgres large object (the `oid` below). This means
    -- that saving a model version requires as its first step `lo_import`ing
    -- the contents
  , contents oid not null
  , version text not null
    -- See note above
  , terminated timestamptz
  , unique (version, model)
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
  , model integer references mversions (id)
  , inputs jsonb not null
  , outputs jsonb not null
    -- See note above
  , terminated timestamptz
  , "user" integer references users (id)
  );

create trigger "manage-mversion-lo" before update or delete on mversions
  for each row execute function lo_manage(contents);
