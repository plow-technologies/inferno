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
    -- Array of model version IDs; unfortunately we lose referential
    -- integrity because Postgres does not allow arrays of foreign keys.
    -- However, we don't allow model version deletion anyway, which
    -- mitigates this to some degree
  , models integer[] not null
    -- Strictly speaking, this includes both inputs and outputs. The
    -- corresponding Haskell type contains `(p, ScriptInputType)`, with
    -- the second element determining readability and writability
  , inputs jsonb not null
    -- Resolution passed to script evaluator
  , resolution integer not null
    -- See note above
  , terminated timestamptz
  , "user" integer references users (id)
  );

-- Execution info for inference evaluation
create table if not exists evalinfo
  ( id uuid primary key
  , param integer not null references params (id)
    -- When inference evaluation began
  , started timestamptz not null
    -- When inference evaluation ended
  , ended timestamptz not null
    -- Number of bytes allocated in the evaluation thread
  , allocated bigint not null
    -- CPU time between `start` and `end`, in milliseconds
  , cpu bigint not null
  );

create or replace function verifymvs()
returns trigger as $$
declare
  mv int;
begin
  foreach mv in array new.models loop
    if not exists(select 1 from mversions where id = mv) then
      raise exception 'Model version ID % is not a valid primary key', mv;
    end if;
  end loop;
  return new;
end;
$$ language plpgsql;

create trigger "verify-param-models" before insert or update on params
  for each row execute function verifymvs();

create trigger "manage-mversion-lo" before update or delete on mversions
  for each row execute function lo_manage(contents);
