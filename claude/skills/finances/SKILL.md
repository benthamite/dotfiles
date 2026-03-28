---
name: finances
description: Import bank statements, categorize transactions, and reconcile against the ledger. Use when the user says "finances", "import statements", "reconcile", "bank statements", "ledger import", or "accounting".
argument-hint: "[import | reconcile | update-guide]"
user-invocable: true
---

# Finances

Import CSV bank statements from Wise and Mercado Pago, categorize transactions, generate ledger entries, and reconcile against the journal.

## Key paths

- **Journal**: `~/My Drive/ledger/journal.ledger`
- **Staging file**: `~/My Drive/ledger/journal-staging.ledger`
- **Statements**: `~/My Drive/ledger/statements/{wise,mercadopago}/`
- **Categorization guide**: `~/.claude/skills/finances/categorization-guide.md`

## What the user must do manually

- **Export CSVs** from Wise and Mercado Pago and place them in the `statements/` subdirectories
- **Cash transactions**: these leave no bank record, so the user must enter them manually in `journal.ledger`
- **Categorize ambiguous items** (Mercado Libre, Amazon, Mom, one-off vendors) when prompted
- **Resolve reconciliation discrepancies** when flagged

## Modes

### `import` (default)

Full pipeline: parse CSVs → categorize → staging file → user review → merge → reconcile → commit.

### `reconcile`

Compare existing journal entries against bank statements. Flag missing entries and balance discrepancies. Mark matched entries as cleared.

### `update-guide`

Analyze recent journal entries and add any new payee→category mappings to the categorization guide.

---

## Import procedure

### Step 1: Check for statements

```bash
ls "~/My Drive/ledger/statements/wise/" "~/My Drive/ledger/statements/mercadopago/" 2>/dev/null
```

If no CSVs found, tell the user:
- **Wise**: Go to wise.com → Account → Statements → Download CSV
- **Mercado Pago**: Go to mercadopago.com.ar → Actividad → Descargar informe → CSV

Ask the user to place the CSVs in the appropriate subdirectory and try again.

### Step 2: Check for existing staging file

If `journal-staging.ledger` already exists, ask the user whether to:
- Resume reviewing the existing staging file
- Discard it and start fresh

### Step 3: Read inputs

Read these files:
1. The CSV file(s) from `statements/`
2. The categorization guide (`categorization-guide.md`)
3. The last ~200 lines of `journal.ledger` (for context on recent entries and to detect duplicates)

### Step 4: Parse CSVs

**Adaptive parsing**: Read the header row of each CSV to determine columns. Do NOT hardcode column positions. Map columns by name.

**Wise expected columns** (adapt if different):
- `Date` (DD-MM-YYYY or YYYY-MM-DD)
- `Amount` (negative = outflow)
- `Currency`
- `Description` or `Merchant`
- `Running Balance`
- `Payment Reference`
- `Exchange From` / `Exchange To` / `Exchange Rate` (for conversions)

**Mercado Pago expected columns** (adapt if different):
- `Fecha` or `Date`
- `Descripción` or `Description`
- `Monto` or `Amount` (may use comma as decimal separator)
- `Detalle` or similar (product details)
- `Medio de pago` (payment method)

**Number parsing**: Check if amounts use comma as decimal separator (Argentine format: `1.234,56`) vs dot (standard: `1,234.56`). If the CSV has both dots and commas in amounts, use the last separator as the decimal mark.

**Date parsing**: Detect format from the first data row. Convert to YYYY-MM-DD for ledger entries.

For each row, extract:
- `date`: transaction date
- `payee`: merchant/counterparty name
- `description`: product/service details (if available)
- `amount`: transaction amount (positive = credit/inflow, negative = debit/outflow)
- `currency`: ISO code
- `balance`: running balance (if available)

### Step 5: Deduplicate

For each parsed transaction, check if it already exists in `journal.ledger` by matching:
- Same source account (Assets:Mercado Pago or Assets:Wise)
- Date within +/- 1 day
- Amount within 0.01

If a match is found, skip the transaction (it's already in the journal). Report the skip count.

### Step 6: Categorize

Read `categorization-guide.md` and apply its rules to each new transaction.

For each transaction, in order:

1. **Transfer detection**: If the payee is "Pablo Stafforini", "Transfer", or "Conversion", or if the description indicates a transfer between own accounts → generate a transfer entry between the appropriate asset accounts. Include Expenses:Fees if a fee is present.

2. **Refund detection**: If the amount is a credit (positive/inflow) from a payee that is normally an expense source (Mercado Libre, Amazon, etc.), or the description contains "devolución"/"refund"/"reembolso" → generate a refund entry using `Assets:Refunds`.

3. **Income detection**: If the payee matches an income source (Airbnb payout, Tlon salary, etc.) → generate an income entry.

4. **Deterministic rules** (Section 1 of guide): Match payee against the table. If matched → auto-categorize. Use the default comment if one is specified.

5. **High-confidence defaults** (Section 2 of guide): Match payee. Check if the description contains an alternate-category trigger. If not → use default category. If yes → use the alternate. Mark with `!` in a comment if confidence is uncertain.

6. **Ambiguous payees** (Section 3 of guide): For Mercado Libre, Amazon, etc., attempt keyword-based inference from the description field. If the description matches a known keyword pattern → use that category with `!` marker. If no match → mark as `?` (needs user input).

7. **Unknown payees**: If the payee doesn't match any rule → mark as `?`.

### Step 7: Write staging file

Write `~/My Drive/ledger/journal-staging.ledger` with all new entries, sorted chronologically.

Format:
```ledger
; === FINANCES STAGING FILE ===
; Source: mercadopago (filename.csv), wise (filename.csv)
; Generated: YYYY-MM-DD
; Status: PENDING REVIEW
;
; Legend:
;   ?  = needs user input (unknown category)
;   !  = auto-categorized with low confidence
;   OK = auto-categorized with high confidence

; --- Auto-categorized (OK) ---

2026-03-01 Edesur
    Expenses:Housing:Utility               97,594.53 ARS
    Assets:Mercado Pago

2026-03-01 Cabify
    Expenses:Transportation:Taxi            5,540.63 ARS ; taxi
    Assets:Wise                                -3.97 USD

; --- Low confidence (!) ---

2026-03-02 Milu
    Expenses:Food:Groceries               25,455.54 ARS ; ! yogurt (default: Housing:Maid)
    Assets:Mercado Pago

; --- Needs user input (?) ---

2026-03-02 Mercado Libre
    Expenses:???                          39,198.60 ARS ; ? "GALAXY S24 FE FUNDA"
    Assets:Mercado Pago
```

### Step 8: Present for review

Show the user a summary:
- Total transactions parsed: N
- Already in journal (skipped): N
- Auto-categorized (OK): N
- Low confidence (!): N
- Needs user input (?): N

Then present the `?` and `!` entries as a numbered list:

```
## Entries needing your input

1. [2026-03-02] Mercado Libre — ARS 39,198.60 — "GALAXY S24 FE FUNDA"
   Suggested: Electronics:Mobile (phone case?)

2. [2026-03-05] Unknown Vendor — ARS 15,000.00 — "SERVICIO TÉCNICO"
   No suggestion

For each, tell me the category (e.g., "1: Electronics:Mobile, 2: Housing:Repairs")
or type "skip" to leave it for manual entry.
```

Apply the user's answers to the staging file. Repeat until all `?` entries are resolved or skipped.

### Step 9: Merge to journal

After all entries are resolved:

1. Read `journal.ledger` to find the correct insertion point (maintain rough chronological order — insert after the last entry with a date <= the earliest staging entry date).

2. Append the staging entries to `journal.ledger`.

3. Validate:
   ```bash
   cd "~/My Drive/ledger" && ledger -f journal.ledger bal > /dev/null 2>&1
   ```
   If validation fails, show the error and ask the user for help. Do NOT proceed with a broken journal.

4. Delete the staging file.

### Step 10: Reconcile

For each CSV source processed:

**Completeness check**: For every row in the CSV, verify there is a matching entry in the journal (date +/- 1 day, same amount, same source account). Report any unmatched rows.

**Balance check**: Compare the CSV's ending balance with ledger's balance for that account:
```bash
cd "~/My Drive/ledger" && ledger -f journal.ledger bal "Assets:Mercado Pago" --end YYYY-MM-DD
cd "~/My Drive/ledger" && ledger -f journal.ledger bal "Assets:Wise" --end YYYY-MM-DD
```
Report any discrepancy.

**Mark cleared**: For all matched entries, add `*` to the posting line of the source account. Edit the journal file to add the cleared marker.

### Step 11: Commit

```bash
cd "~/My Drive/ledger" && git add journal.ledger && git commit -m "finances: import $(date +%Y-%m-%d) statements"
```

If the categorization guide was updated, also commit that:
```bash
cd ~/.claude/skills/finances && git add categorization-guide.md && git commit -m "finances: update categorization guide"
```

---

## Reconcile procedure (standalone)

When run with `reconcile` argument:

1. Read CSV files from `statements/`
2. Read `journal.ledger`
3. For each CSV row, find the matching journal entry (date +/- 1 day, amount, source account)
4. Report:
   - Matched entries (add `*` cleared marker)
   - CSV rows with no journal match (missing from journal)
   - Journal entries with no CSV match (potentially incorrect or manual entries)
   - Balance discrepancy (if any)
5. Ask user how to handle discrepancies
6. Commit changes

---

## Update-guide procedure

When run with `update-guide` argument:

1. Run:
   ```bash
   cd "~/My Drive/ledger" && ledger -f journal.ledger reg Expenses --format '%(payee)\t%(account)\n' 2>/dev/null | sort | uniq -c | sort -rn
   ```
2. Compare against the existing categorization guide
3. Identify new payees or changed patterns
4. Present proposed additions/changes to the user
5. After approval, update `categorization-guide.md`
6. Commit

---

## Ledger entry format reference

**Standard expense:**
```ledger
YYYY-MM-DD Payee
    Expenses:Category:Sub              amount currency ; comment
    Assets:Source
```

**Cross-currency (Wise paying ARS expense):**
```ledger
YYYY-MM-DD Payee
    Expenses:Category:Sub            amount ARS ; comment
    Assets:Wise                     -amount USD
```

**Transfer between own accounts:**
```ledger
YYYY-MM-DD Pablo Stafforini
    Assets:Destination              amount currency
    Assets:Source
```

**Refund:**
```ledger
YYYY-MM-DD Payee
    Assets:Refunds                  amount currency ; item description
    Assets:Source
```

**Income:**
```ledger
YYYY-MM-DD Payee
    Income:Category                -amount currency ; note
    Assets:Destination
```

**Formatting rules:**
- 4-space indentation for postings
- ARS uses thousands separator: `1,400,000.00 ARS`
- USD/GBP/EUR: plain decimals: `200.00 USD`
- Currency code follows the amount (not before)
- Comments separated by ` ; ` (space-semicolon-space)
- Dates: YYYY-MM-DD
- No trailing whitespace
