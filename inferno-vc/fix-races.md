# Inferno Version Control Store

A simple version control for scripts.

```
store(object, predecessor) -> hash
fetch(hash) -> object
fetchHist(hash) -> [object]
del(hash)
```

## Current Implementation

Data structure: uses the file system. Objects are stored in the root directory with their hash as filenames, the `heads/` directory contains one file for each script history named by the hash of the head of that history, and the `to_head/` directory maps every script to the head of the history it belongs to:
```
/vc_store/
├── 2ZDR-_h3LhqkDsk3qIOx7gobNrAvwbSA67aakIQFabI=
├── Bu1zONJHnlYCqqACpMeJ4pac_V2jmMOYISqyAvaSoBs=
├── FqBr0k0PQ33qskv9c6z9IsKE8jbTkgMyHx_iqyJFXLc=
├── IlRYIOwovxwbHWor54PHrYWQL_pZVWCyFGw_lmvAGjI=
├── heads
│   ├── Bu1zONJHnlYCqqACpMeJ4pac_V2jmMOYISqyAvaSoBs
│   ├── IlRYIOwovxwbHWor54PHrYWQL_pZVWCyFGw_lmvAGjI=
└── to_head
    ├── 2ZDR-_h3LhqkDsk3qIOx7gobNrAvwbSA67aakIQFabI=
    ├── Bu1zONJHnlYCqqACpMeJ4pac_V2jmMOYISqyAvaSoBs=
    ├── FqBr0k0PQ33qskv9c6z9IsKE8jbTkgMyHx_iqyJFXLc=
    ├── IlRYIOwovxwbHWor54PHrYWQL_pZVWCyFGw_lmvAGjI=
```

Store method:
```python
def store(o, h_p):
    h_o = writeToStore(o)
    f_p = 'vc_store/heads/{h_p}'
    if fileExists(f_p):
        f_o = 'vc_store/heads/{h_o}'
        renameFile(f_p, f_o)
        appendFile(f_o, h_p)
        # Update to_head pointers:
        for h in readFile(f_o):
            writeFile('vc_store/to_head/{h}', h_o)
```

The `to_head` mappings are used by the `fetchHist(h)` operation, which uses the mapping to find the head of the given object and then reads the object's history from the file in the `heads/` directory.

## Issues with current implementation:

1. The `renameFile` and `appendFile` are not in an atomic block. This means by the time an operation tries to append to the file, the next store operation could have already renamed the file to something else, meaning the first operation's predecessor would be lost from the history.

2. The update of `to_head` pointers of one operation `store(o2, o1)` can race with the successive operation `store(o3, o2)`. If the latter overtakes the former, this will result in some objects in the history incorrectly pointing to `o2` as their head instead of `o3`.

3. Crash safety: crashes, for example between `renameFile` and `appendFile`, will leave the store in an inconsistent state.

## Option 0: slap a lock around all operations

- Pros: Easy fix, no need to migrate current store
- Cons: inefficient

## Option 1: fix the current implementation

- Idea: all versions of a script share a unique `histID`. Instead of `to_head`, you have a meta field `to_hist` in the object file as this is stable.
- Maintain a `hist_to_head` map in memory that maps each histID to the hash of its head, and use Control.Concurrent.Lock or an MVar to update this map atomically.
- `store(o, p)` writes a new head file for `o`, but retries until it can successfully update `hist_to_head`.
- `fetchHist(h)` looks up the `histID` from the meta file, finds the head from `hist_to_head`, and returns the history saved in the appropriate head file.
- `hist_to_head` can be periodically saved to file and on startup the last snapshot can be loaded and updated if necessary (as it is easy to reconstruct it from all object metas).
- Pros: small migration from current store, most operations don't need lock so is relatively efficient.
- Cons: need to carefully check/prove concurrent correctness, needs migration

```python
def store(o, h_p):
    hist_id = readStore(h_p)['hist_id']
    h_o = writeToStore(o['hist_id' := hist_id])
    if hist_to_head['hist_id'] == h_p:
        f_p = 'vc_store/heads/{h_p}'
        f_o = writeFile('vc_store/heads/{h_o}', readFile(f_p) ++ h_p)
        if CAS(hist_to_head['hist_id'], h_p, h_o):
            return Success
    return TriedToAppendToNonHeadError
```

<!-- NOTE: old heads need to be cleaned up. getAllHeads should use `hist_to_head` not files in `heads/`. -->

## Option 2: use existing file-backed DB

- For example: have a DB table with columns: `hash, pred, isHead, object, histID`
- Store adds row to table and unsets `isHead` of pred (atomically)
- Index can be `hash`, so fetch is fast
- All versions of a script share a unique `histID`, so `fetchHist(h)` looks up `histID` of `h` and fetches all rows with that `histID`.
- DB is periodically backed up to file
- Pro: concurrent correctness and efficiency guaranteed by DB
- Con: making all operations atomic might be slower than a correct implementation of Option 1
- Con: needs migration of store

## Option 3: STM

- Can use an in-memory STM data structure and make each operation atomic
- Like VPDB, have a thread save a snapshot of the structure to file periodically
- Pro: STM ensures no concurrency bugs
- Con: entire store needs to fit in memory
- Con: needs migration of store

## Testing: are there libraries to test for such concurrency bugs?

For example, something that runs a random sequence of operations in a single-thread and in a concurrent setting and compares outputs.
