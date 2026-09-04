# `category_vocabulary.json`

The growing record of every food and store category the pipeline knows about,
plus the cached decision for every raw phrase it has ever classified. It is the
most consequential file in the repo: `baseline_commonness` sits in the
denominator of every signal ratio on the published page, and the item maps
decide what a respondent's answer counts as.

It is created automatically on first run and committed by the hourly Action, so
it accumulates across runs rather than being rebuilt.

---

## The one rule that matters most

**Only ever write this file with R.**

The pipeline writes it through `jsonlite::write_json(auto_unbox = TRUE, pretty = TRUE)`,
which keeps JSON arrays inline. Python's `json.dump(indent=2)` explodes every
array to one element per line. Editing a single key with Python therefore
produces a ~200-line diff that is almost entirely reformatting, burying the
actual change.

If you need to make a bulk edit, mirror how the pipeline reads and writes it:

```r
v <- jsonlite::fromJSON("category_vocabulary.json", simplifyVector = TRUE)
# ...edit v...
jsonlite::write_json(v, "category_vocabulary.json", auto_unbox = TRUE, pretty = TRUE)
```

A no-op round-trip through those two calls is byte-identical to the committed
file. Verify that first if you are unsure — if a round-trip with no edits
produces a diff, something about your R or jsonlite version differs and a real
edit will produce noise.

---

## Two kinds of key, and never confuse them

This is the distinction that governs every field below. Getting it wrong
deletes real data.

| | Keyed by | Example key |
|---|---|---|
| **Category-keyed** | a category slug | `romaine_head` |
| **Phrase-keyed** | the raw text a respondent typed | `"trader joe's wichita kansas"` |

**Category-keyed maps**: every key must name a category that exists in
`produce_categories` or `store_categories`. A key that doesn't is residue and
gets pruned.

**Phrase-keyed maps** (`produce_item_map`, `store_item_map`): keys are
respondent text. They are *supposed* to contain apostrophes, parentheses,
accents and punctuation — that text is the record of what someone actually
reported. Never "clean" them. Applying the category-keyed orphan rule to these
would delete nearly every entry, and stripping punctuation would break
`split_delims()`, which depends on commas and parentheses to separate multiple
businesses in one answer.

---

## Fields

| Field | Keyed by | What it holds |
|---|---|---|
| `produce_categories` | — (array) | Every produce category slug |
| `store_categories` | — (array) | Every store/restaurant category slug |
| `baseline_commonness` | category | 0–100: roughly what share of people eat this in a two-week window. **The signal-ratio denominator.** A category without one is excluded from the signal table |
| `baseline_source` | category | `"USDA ERS Food Availability"` or `"author_estimate"` |
| `baseline_confidence` | category | `"exact_match"` or `"unsourced"` |
| `category_source_type` | category | `"checklist_direct"` or `"freetext_classified"` — how the category first entered |
| `active_since_form_version` | category | Form version the category dates from, e.g. `"v1"` |
| `checklist_label` | category | The checklist option text routing here; may be an array when several options map to one category |
| `category_notes` | category | Free prose explaining a non-obvious decision. Rare and deliberately so |
| `produce_item_map` | **phrase** | Cached classification: raw phrase → category |
| `store_item_map` | **phrase** | Same, for stores |

Approximate current sizes: 63 produce and 130 store categories, 29 baselines,
117 and 178 cached phrases.

Note that `baseline_source` and `baseline_confidence` are broader than
`baseline_commonness` — a category can carry a source label while having no
baseline. Those are categories nobody has assigned a number to yet, and they are
listed on the page under "Reported, but not yet rankable".

---

## Invariants

**Category names must match `^[a-z0-9_]+$`.** A name with capitals, spaces or
punctuation means raw respondent text reached the category list instead of being
classified — the signature of the classifier poisoning that produced categories
like `"No. I Made Sure To Not :("`. `check_vocabulary_integrity()` stops the run
on this rather than publishing over it.

**The vocabulary only grows.** Categories are added, never removed by the
pipeline. `scripts/check-render.R` refuses to publish a run where the known
category count fell, because a shrink means the file was overwritten rather than
updated.

**Writes merge, they do not replace.** `save_vocabulary()` re-reads the file it
is about to replace and merges into it, with the on-disk copy winning. This is
what lets a local run and the hourly Action both add categories without either
clobbering the other.

**Merge-on-write cannot express a deletion.** It is the direct consequence of
the rule above and the sharpest edge in this file. If you delete a key and
another writer still holds it in memory, it comes back. Deletions are safe only
because every writer re-reads from git at the start of a run — so delete, commit,
and then confirm the deletion survived the next Action run rather than assuming
it did.

---

## Editing it by hand

Adding a `baseline_commonness` number is the common case and is safe:

```jsonc
"baseline_commonness": {
  "cilantro": 20,
  "onions": 25        // <- added: moves onions into the signal table
}
```

Anything structural — renaming a category, removing a key — should go through R
and should be checked afterwards. `check_vocabulary_integrity()` runs
automatically before every save and will catch orphaned metadata, but running the
test suite is the faster feedback loop:

```sh
Rscript tests/run-tests.R
```

**Pull before you edit, and push straight after.** See the warning about stale
clones in the README — the hourly Action commits to this file, so a clone that
has fallen behind will clobber whatever the Action added.
