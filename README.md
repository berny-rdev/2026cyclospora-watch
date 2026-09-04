# Crowdsourced Cyclospora Watch

A crowdsourced, self-reported tracker for the current multistate cyclospora
outbreak, built by a grad student with an interest in public health. Not affiliated with CDC
or any state health department — this is a hypothesis-generating tool, not
an official investigation.
Here is the link to view: https://berny-rdev.github.io/2026cyclospora-watch/index.html

## What's in here

**Entry points** — the two things you actually run. They share their whole
pipeline core, so a fix lands once:

- `index.Rmd` — the report itself. Pulls responses from a Google Form's
  linked Google Sheet, normalizes messy free-text food/store answers into
  clean categories, and ranks foods by how often sick people report them
  relative to how often people eat them normally (a "signal ratio"). This is
  what the live page is built from.
- `cyclospora_outbreak_analysis.R` — the same analysis as a plain script you
  can run locally in RStudio, writing CSVs instead of a page.

**Shared core** (`R/`) — sourced by both entry points. These used to be
maintained by hand in both files, which is exactly how they drifted apart:

| File | What's in it |
|---|---|
| `columns.R` | Locating form columns in the sheet header |
| `text-normalize.R` | Punctuation canonicalisation, splitting answers into items, negation filtering |
| `checklist.R` | Routing preset checklist options to categories |
| `classify.R` | Regex + LLM classification, and the classification cache |
| `vocabulary-io.R` | Reading, merging and writing `category_vocabulary.json` |
| `vocabulary-integrity.R` | Locale guard and the pre-save integrity check |
| `stats.R` | Wilson confidence intervals |
| `run-manifest.R` | Recording what a run produced, for the publish guard |

**Data**

- `category_vocabulary.json` — the growing category list and classification
  cache. **[Documented in detail here](docs/category-vocabulary.md)** — read
  that before editing it by hand.
- `checklist-mapping.json` — maps checklist option text to categories.
- `baselines-external.json` — USDA ERS reference figures, shown on the page
  but deliberately not used in the ratio (see the page for why).

**Automation** (`.github/workflows/`)

- `update.yml` — renders `index.Rmd` to `index.html` and publishes via GitHub
  Pages. Scheduled hourly, though GitHub throttles scheduled workflows on
  low-activity repos, so in practice it runs every 3–5 hours.
- `tests.yml` — runs the unit suite. Never renders, never commits.
- `refresh-baselines.yml` — monthly USDA ERS refresh; opens a PR rather than
  committing, because a baseline feeds the signal-ratio denominator.

**Tests** — `Rscript tests/run-tests.R` from the repo root. No API key, no
network. The fixtures are the actual bugs this pipeline has had, so a failure
usually means one has come back.

## One-time setup

1. **Make your Google Sheet viewable.** Open the Sheet linked to your
   Google Form → Share → "Anyone with the link" → Viewer. (No login
   required for the script to read it. Don't do this if your form collects
   anything identifying — this one only asks about symptoms/foods/location,
   so it's fine.)

2. **Edit the config block** in both `index.Rmd` and
   `cyclospora_outbreak_analysis.R`:
   - `SHEET_URL` → your Sheet's URL
   - `col_signatures` → short keyword patterns matched against your form's
     actual question text (Google Forms stuffs the whole question into the
     Sheet header, so this matches on a distinctive phrase rather than the
     exact string - more robust to wrapping/formatting differences)
   - `REQUIRED_COLUMNS` / `REQUIRE_ANY_COLUMNS` → which of those must match
     or the run stops. `consent` is required for a reason: every downstream
     use of a column is guarded with `if ("x" %in% names(df))`, so if the
     consent column stops matching, the filter that drops non-consenting
     responses is silently skipped and they end up in the analysis.

3. **Set your Anthropic API key** (used to classify messy free-text food/
   store answers - people write things like "idk maybe romaine?" and the
   LLM handles that far better than a keyword list):
   - Get a key at [console.anthropic.com](https://console.anthropic.com)
     (separate account/billing from claude.ai)
   - Locally: put `ANTHROPIC_API_KEY=sk-ant-...` in `~/.Renviron`. **Do not**
     use `Sys.setenv()` at the R console — `.Rhistory` records every console
     line in plaintext, which is how a key leaked once already.
   - For the live page: repo → Settings → Secrets and variables → Actions →
     New repository secret → name it `ANTHROPIC_API_KEY`
   - No key set? Both entry points fall back to the regex seed dictionaries
     (`produce_dict_seed` / `store_dict_seed`) instead of crashing - just
     much less accurate. Set `CLASSIFICATION_METHOD <- "regex"` to skip the
     API entirely on purpose. Note the fallback is a real quality drop, not a
     minor one: the seeds match ~16 store categories where the LLM-built
     vocabulary has ~127, so the publish guard refuses to publish a run that
     silently degraded this way.
   - Cost note: only *distinct* raw phrases are sent per run, and phrases
     already classified in a previous run are answered from the cache instead
     of being re-sent. A single batched call on Haiku - trivially cheap.

4. **Push this repo to GitHub.**

5. **Turn on GitHub Pages:** repo Settings → Pages → Source: "Deploy from a
   branch" → Branch: `main`, folder: `/ (root)`. Your page will be live at
   `https://<your-username>.github.io/<repo-name>/index.html` within a few
   minutes of the first successful Action run.

6. **Run the Action once manually** to check it works: repo → Actions tab →
   "Update Cyclospora Watch Page" → Run workflow. Check the logs if it
   fails — most common issue is a `col_signatures` pattern that doesn't
   match your form's question text. The error prints the sheet's actual
   column headers, which is what you edit the pattern against.

## Running it locally

```sh
Rscript cyclospora_outbreak_analysis.R    # writes CSVs
Rscript tests/run-tests.R                 # unit suite
```

⚠️ **Pull before you run, push straight after.** The hourly Action commits
`category_vocabulary.json` to `main`. If your clone has fallen behind and you
run the pipeline, your run writes a vocabulary built from the stale file and
committing it **clobbers every category the Action added in the meantime** —
silently, with no merge conflict, because a full-file overwrite always wins a
rebase. This has already happened once: a clone 33 commits behind was missing
1 produce category, 15 store categories and 29 cached classifications.

`save_vocabulary()` merges on write and protects you *at runtime* — two runs
happening at once will not lose each other's categories. Git does not offer the
same protection. Treat "pull first" as mandatory, not tidy.

**Use a UTF-8 locale.** `Rscript` on macOS defaults to `LC_CTYPE=C`, where R's
regex engine will not match a multibyte character class — which silently turns
punctuation normalisation into a no-op and forks a duplicate category off every
curly apostrophe. The scripts detect this and set a UTF-8 locale themselves, but
if that fails they stop rather than continue, and `LANG=en_US.UTF-8` in your
environment avoids the whole problem.

## How category classification works

Answers get folded into a standard set of categories (`cilantro`,
`romaine_head`, `kroger`) so the frequency and signal-ratio math can run. That
list isn't fixed in the code — it lives in
[`category_vocabulary.json`](docs/category-vocabulary.md) and grows over time.

Answers arrive by two routes, treated differently on purpose:

- **Checklist options** are preset strings, already category-shaped, so they
  resolve by direct lookup through `checklist-mapping.json`. They never reach
  the LLM. That's around 90% of the volume, and removes any chance of drift on
  it. A checklist option missing from the mapping is a hard error rather than a
  guess.
- **Free text** goes to the classifier, which reuses an existing category where
  one fits or mints a new `lowercase_snake_case` one for a genuinely new food,
  rather than dumping it in "other". Each decision is cached by raw phrase, so
  the same answer always resolves the same way and is never re-sent.

The page shows which route each count came from, because they are different
kinds of evidence: a food only ever reported because it was on the checklist
reads differently from one people wrote in unprompted.

New categories have no `baseline_commonness` until someone adds one, so they
don't get a signal ratio. They still count in the frequency tables, they're
listed on the page under "Reported, but not yet rankable", and
`cyclospora_outbreak_analysis.R` prints them each run. Some of them never should
get a baseline — there is no meaningful "percent of people who ate a taco in two
weeks" to divide by — so that list is partly a to-do and partly permanent.

## Limits, said plainly

- No control group → no odds ratios, only proportional reporting among
  cases. A high "signal ratio" is a lead, not proof.
- Self-selected sample → prone to recall bias and selection bias.
- If a real pattern emerges, the right next step is reporting it to a
  local/state health department, who can run an actual case-control study
  and trace product lots. This page can flag smoke; it can't find the fire.
