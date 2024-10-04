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

create table if not exists models
  ( id uuid primary key default gen_random_uuid()
  , name text not null
  , gid numeric not null
  , visibility jsonb
    -- May be missing, if there is no model version yet
  , updated timestamptz
    -- See note above
  , terminated timestamptz
  , unique (name, gid)
  );

create table if not exists mversions
  ( id uuid primary key default gen_random_uuid()
  , model integer references models (id)
    -- Short, high-level model description
  , description text not null
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

-- Model versions linked to specific Inferno scripts (i.e. junction table
-- between `scripts` and `mversions`)
create table if not exists mselections
  ( script bytea not null references scripts (id)
  , model integer not null references mversions (id)
    -- Inferno identifier linked to this specific model version
  , ident text not null
  , unique (script, model)
  );

create table if not exists params
  ( id uuid primary key default gen_random_uuid()
    -- Script hash from `inferno-vc`
  , script bytea not null references scripts (id)
    -- Strictly speaking, this includes both inputs and outputs. The
    -- corresponding Haskell type contains `(p, ScriptInputType)`, with
    -- the second element determining readability and writability
  , inputs jsonb not null
    -- Resolution passed to script evaluator
  , resolution integer not null
    -- See note above
  , terminated timestamptz
  , uid numeric not null
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

-- Stores information required to call the data bridge
create table if not exists bridges
  ( -- Same ID as the referenced param
    id uuid not null references params (id)
    -- Host of the bridge server
  , ip inet not null
  , port integer check (port > 0)
  );

create trigger "manage-mversion-lo" before update or delete on mversions
  for each row execute function lo_manage(contents);
