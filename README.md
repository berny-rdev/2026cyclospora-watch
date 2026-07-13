# Crowdsourced Cyclospora Watch

A crowdsourced, self-reported tracker for the current multistate cyclospora
outbreak, built by a grad student with an interest in public health. Not affiliated with CDC
or any state health department — this is a hypothesis-generating tool, not
an official investigation.

## What's in here

- `index.Rmd` — the report itself. Pulls responses from a Google Form's
  linked Google Sheet, normalizes messy free-text food/store answers into
  clean categories, and ranks foods by how often sick people report them
  relative to how often people eat them normally (a "signal ratio").
- `cyclospora_outbreak_analysis.R` — same analysis, as a plain script you
  can run locally in RStudio instead of/in addition to the live page.
- `.github/workflows/update.yml` — GitHub Action that re-renders
  `index.Rmd` into `index.html` every 6 hours and publishes it via GitHub
  Pages.

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

3. **Set your Anthropic API key** (used to classify messy free-text food/
   store answers - people write things like "idk maybe romaine?" and the
   LLM handles that far better than a keyword list):
   - Get a key at [console.anthropic.com](https://console.anthropic.com)
     (separate account/billing from claude.ai)
   - Locally: `Sys.setenv(ANTHROPIC_API_KEY = "sk-ant-...")` before running
   - For the live page: repo → Settings → Secrets and variables → Actions →
     New repository secret → name it `ANTHROPIC_API_KEY`
   - No key set? Both scripts automatically fall back to the regex
     dictionaries (`produce_dict`/`store_dict`) instead of crashing - just
     less accurate on hedged/typo'd answers. Set `CLASSIFICATION_METHOD <-
     "regex"` to skip the API entirely and go regex-only on purpose.
   - Cost note: only *distinct* raw phrases are sent per run (not one call
     per response), and it's a single batched call on Haiku - trivially
     cheap at Reddit-thread data volumes.

3. **Push this repo to GitHub.**

4. **Turn on GitHub Pages:** repo Settings → Pages → Source: "Deploy from a
   branch" → Branch: `main`, folder: `/ (root)`. Your page will be live at
   `https://<your-username>.github.io/<repo-name>/index.html` within a few
   minutes of the first successful Action run.

5. **Run the Action once manually** to check it works: repo → Actions tab →
   "Update Cyclospora Watch Page" → Run workflow. Check the logs if it
   fails — most common issue is a `col_map` question text that doesn't
   exactly match your form.

## How category classification works

Free-text answers get folded into a small set of standard categories
(`lettuce`, `cilantro`, `kroger`, etc.) so the frequency/signal-ratio math
can run. That category list isn't fixed in the code - it lives in
**`category_vocabulary.json`**, a file created automatically on the first
run that grows over time:

- Claude classifies each distinct raw answer, reusing an existing category
  if one fits, or minting a new one if it's a genuinely new food/store
- The updated vocabulary is saved back to `category_vocabulary.json`
- The GitHub Action commits that file alongside `index.html`, so the next
  scheduled run starts from where the last one left off instead of
  reinventing categories from scratch

If you also run the script **locally**, pull the latest
`category_vocabulary.json` before running and push it after, so the local
and live vocabularies don't diverge.

New categories won't have a `baseline_commonness` number (how often people
normally eat that food) until you add one - the script prints a reminder
list each run. Open `category_vocabulary.json`, find the new category name
under `baseline_commonness`, add a rough 0-100 guess, and commit. Until
then, that food still shows up in the raw frequency tables, it just won't
get a signal ratio.

## Limits, said plainly

- No control group → no odds ratios, only proportional reporting among
  cases. A high "signal ratio" is a lead, not proof.
- Self-selected sample → prone to recall bias and selection bias.
- If a real pattern emerges, the right next step is reporting it to a
  local/state health department, who can run an actual case-control study
  and trace product lots. This page can flag smoke; it can't find the fire.
