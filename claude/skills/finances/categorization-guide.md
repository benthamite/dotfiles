# Categorization guide

This guide maps bank statement payees to ledger accounts. Generated from ~7,500 transactions in `journal.ledger`.

**Matching rules**: Use case-insensitive substring matching. Match the most specific pattern first.

## 1. Deterministic rules (auto-categorize without asking)

These payees map to a single category >95% of the time.

### Utilities

| CSV payee pattern | Ledger payee | Account | Source |
|---|---|---|---|
| Edesur | Edesur | Expenses:Housing:Utility | MP |
| Metrogas | Metrogas | Expenses:Housing:Utility | MP |
| Aysa | Aysa | Expenses:Housing:Utility | MP |
| Flow (not Personal Flow) | Flow | Expenses:Housing:Utility | MP |
| Personal Flow | Personal Flow | Expenses:Housing:Utility | MP |
| Vodafone | Vodafone | Expenses:Housing:Utility:Mobile | Revolut |

### Transportation

| CSV payee pattern | Ledger payee | Account | Default comment | Source |
|---|---|---|---|---|
| Cabify | Cabify | Expenses:Transportation:Taxi | ; taxi | MP, Wise |
| Uber (not Uber Eats) | Uber | Expenses:Transportation:Taxi | ; taxi | MP, Wise |
| Uber Eats | Uber Eats | Expenses:Food:Takeaway | | Revolut |
| Free Now | Free Now | Expenses:Transportation:Taxi | | |
| MyTaxi | MyTaxi | Expenses:Transportation:Taxi | | |
| Didi | Didi | Expenses:Transportation:Taxi | | MP |
| Tembici | Tembici | Expenses:Transportation:Misc | | |
| Ecobici | Ecobici | Expenses:Transportation:Misc | | |
| London Underground | London Underground | Expenses:Transportation:Metro | | Revolut |
| Emova | Emova | Expenses:Transportation:Metro | | MP |
| S-Bahn Berlin | S-Bahn Berlin GmbH | Expenses:Transportation:Metro | | |
| RATP | RATP | Expenses:Transportation:Metro | | |
| SNCF | SNCF | Expenses:Transportation:Metro | | |
| Great Western Railway | Great Western Railway | Expenses:Transportation:Train | | Revolut |
| Renfe | Renfe | Expenses:Transportation:Train | | |
| Trenitalia | Trenitalia | Expenses:Transportation:Train | | |
| Oxford Bus | Oxford Bus Company | Expenses:Transportation:Bus | | |
| BVG Berlin | BVG Berlin | Expenses:Transportation:Bus | | |
| Trip.com | Trip.com | Expenses:Transportation:Flight | | Revolut |
| British Airways | British Airways | Expenses:Transportation:Flight | | |
| Norwegian | Norwegian | Expenses:Transportation:Flight | | |
| Air Serbia | Air Serbia | Expenses:Transportation:Flight | | |
| Eurowings | Eurowings | Expenses:Transportation:Flight | | |
| Latam | Latam | Expenses:Transportation:Flight | | Wise |
| KLM | KLM | Expenses:Transportation:Flight | | |
| Kiwi | Kiwi | Expenses:Transportation:Flight | | Wise |
| Taurusmania | Taurusmania | Expenses:Transportation:Parking | | MP |
| Park Work | Park Work | Expenses:Transportation:Parking | | MP |
| Garage | Garage | Expenses:Transportation:Parking | | MP, Cash |

### Food: takeaway

| CSV payee pattern | Ledger payee | Account | Default comment | Source |
|---|---|---|---|---|
| X Hora | X Hora | Expenses:Food:Takeaway | ; salads | MP |
| Food Time | Food Time | Expenses:Food:Takeaway | ; salads | MP |
| Rappi | Rappi | Expenses:Food:Takeaway | | MP |
| Pedidos Ya | Pedidos Ya | Expenses:Food:Takeaway | | MP |
| Gatorta | Gatorta | Expenses:Food:Takeaway | | Revolut |
| Del Toro | Del Toro | Expenses:Food:Takeaway | | |
| Mundo Salad | Mundo Salad | Expenses:Food:Takeaway | | |
| Mad Mad Vegan | Mad Mad Vegan | Expenses:Food:Takeaway | | Revolut |
| Sandoval | Sandoval | Expenses:Food:Takeaway | | Revolut |
| Sana vegetariana | Sana vegetariana | Expenses:Food:Takeaway | | |
| Honest Greens | Honest Greens | Expenses:Food:Takeaway | | |
| Tostado | Tostado Café Club | Expenses:Food:Takeaway | | |
| McDonald's | McDonald's | Expenses:Food:Takeaway | | MP |

### Food: groceries

| CSV payee pattern | Ledger payee | Account | Source |
|---|---|---|---|
| Veritas | Veritas | Expenses:Food:Groceries | Revolut |
| Coto | Coto | Expenses:Food:Groceries | MP, Cash |
| Dia / DIA | Dia | Expenses:Food:Groceries | Revolut |
| Sainsbury's | Sainsbury's | Expenses:Food:Groceries | Revolut |
| Carrefour Express | Carrefour Express | Expenses:Food:Groceries | |
| Disco | Disco | Expenses:Food:Groceries | |
| Tesco | Tesco | Expenses:Food:Groceries | |
| Co-operative Food | Co-operative Food | Expenses:Food:Groceries | |
| Intermarche | Intermarché | Expenses:Food:Groceries | |
| Open 25 | Open 25 | Expenses:Food:Groceries | MP |
| New Garden | New Garden | Expenses:Food:Groceries | MP |
| Solomon Fresh | Solomon Fresh Market | Expenses:Food:Groceries | |
| Berries | Berries | Expenses:Food:Groceries | |
| Norma Rodríguez | Norma Rodríguez | Expenses:Food:Groceries | Cash |
| Diego Javier Marcon | Diego Javier Marcon | Expenses:Food:Groceries | MP |
| Marcela Beatriz García | Marcela Beatriz García | Expenses:Food:Groceries | |
| Todo fruta | Todo fruta | Expenses:Food:Groceries | MP |
| Carmin Alimentos | Carmin Alimentos | Expenses:Food:Groceries | |

### Food: coffee

| CSV payee pattern | Ledger payee | Account | Source |
|---|---|---|---|
| Starbucks | Starbucks | Expenses:Food:Coffee | MP |
| 7-Eleven | 7-Eleven | Expenses:Food:Coffee | |
| Cotterie | Cotterie | Expenses:Food:Coffee | |
| Brewklyn | Brewklyn Cafe | Expenses:Food:Coffee | |
| Society Cafe | Society Cafe | Expenses:Food:Coffee | |
| Paul | Paul | Expenses:Food:Coffee | |
| Café Tortoni | Café Tortoni | Expenses:Food:Coffee | |
| Flo's Refill | Flo's Refill Shop | Expenses:Food:Coffee | Wise |
| Vive Café | Vive Café | Expenses:Food:Coffee | |

### Food: restaurant

| CSV payee pattern | Ledger payee | Account | Source |
|---|---|---|---|
| Pertutti | Pertutti | Expenses:Food:Restaurant | MP |
| Hierbabuena | Hierbabuena | Expenses:Food:Restaurant | |
| La Hummusería | La Hummusería | Expenses:Food:Restaurant | |
| Angrid Thai | Angrid Thai | Expenses:Food:Restaurant | |
| Al-Shami | Al-Shami | Expenses:Food:Restaurant | |
| Pizza Cero | Pizza Cero | Expenses:Food:Restaurant | |
| Mark's | Mark's | Expenses:Food:Restaurant | |
| Ninina | Ninina | Expenses:Food:Restaurant | |
| Lo de Julio | Lo de Julio | Expenses:Food:Restaurant | |
| Plantasia | Plantasia | Expenses:Food:Restaurant | |
| Gracias Madre | Gracias Madre | Expenses:Food:Restaurant | |
| Casa Munay | Casa Munay | Expenses:Food:Restaurant | |
| Casa Saenz | Casa Saenz | Expenses:Food:Restaurant | |
| Bio | Bio | Expenses:Food:Restaurant | |
| Ashurbanipal | Ashurbanipal | Expenses:Food:Restaurant | |
| Rodi Bar | Rodi Bar | Expenses:Food:Restaurant | Wise |
| Invernadero | Invernadero | Expenses:Food:Restaurant | |

### Food: drinks

| CSV payee pattern | Ledger payee | Account |
|---|---|---|
| Tres Monos | Tres Monos | Expenses:Food:Drinks |
| 878 | 878 | Expenses:Food:Drinks |

### Housing

| CSV payee pattern | Ledger payee | Account | Default comment | Source |
|---|---|---|---|---|
| Milu | Milu | Expenses:Housing:Maid | ; maid | MP |
| Diego Acuña | Diego Acuña | Expenses:Housing:Household | ; misc repairs | MP |
| La nueva raza / La Nueva Raza | La nueva raza | Expenses:Housing:Household | | MP |
| Cristalcenter | Cristalcenter | Expenses:Housing:Household | | MP |
| Vivero Mario | Vivero Mario | Expenses:Housing:Household | | MP, Cash |
| TBS Climatización | TBS Climatización | Expenses:Housing:Household | | MP, Cash |
| Andrea Deco | Andrea Deco | Expenses:Housing:Household | | Cash |
| Eduardo Burletes | Eduardo Burletes | Expenses:Housing:Household | | Cash |
| Pablo Simón | Pablo Simón | Expenses:Housing:Household | | MP, Cash |
| Pulidor Martínez | Pulidor Martínez | Expenses:Housing:Household | | MP |
| Maralé Antigüedades | Maralé Antigüedades | Expenses:Housing:Household | | MP |
| Materia noble | Materia noble | Expenses:Housing:Household | | |
| Alfredo | Alfredo | Expenses:Housing:Household | | MP, Cash |

### Health

| CSV payee pattern | Ledger payee | Account | Source |
|---|---|---|---|
| Point HI | Point HI Srl | Expenses:Health:Medical | |
| Smart Fit | Smart Fit | Expenses:Health:Gym | |

### Beauty

| CSV payee pattern | Ledger payee | Account | Source |
|---|---|---|---|
| De La Cabeza | De La Cabeza | Expenses:Beauty:Hairdresser | |
| Popham Hairdressing | Popham Hairdressing | Expenses:Beauty:Hairdresser | |
| Dr Jasmin | Dr Jasmin | Expenses:Beauty:Skin | |
| SkinCeuticals / Skinceuticals | SkinCeuticals | Expenses:Beauty:Skin | Revolut |
| Clínica Barber | Clínica Barber | Expenses:Beauty:Skin | |

### Leisure

| CSV payee pattern | Ledger payee | Account | Source |
|---|---|---|---|
| Spotify | Spotify | Expenses:Leisure:Music | |
| Tango-DJ.at | Tango-DJ.at | Expenses:Leisure:Music | Wise |
| Audible | Audible | Expenses:Leisure:Audiobooks | |
| Speechify | Speechify | Expenses:Leisure:Audiobooks | |
| Cinemark | Cinemark | Expenses:Leisure:Movies | |
| Cinépolis | Cinépolis Recoleta | Expenses:Leisure:Movies | |
| Hoyts | Hoyts | Expenses:Leisure:Movies | |
| Alejandro Leonian | Alejandro Leonian | Expenses:Leisure:Books | |

### Donations

| CSV payee pattern | Ledger payee | Account | Source |
|---|---|---|---|
| GitHub | GitHub | Expenses:Donation | Wise, Revolut |
| Seúl / SEUL / Seoul | Seúl | Expenses:Donation | MP |
| Patreon | Patreon | Expenses:Donation | Revolut |
| David Federico Rivadeneira | David Federico Rivadeneira | Expenses:Donation | |
| Carlos Giudice | Carlos Giudice | Expenses:Donation | Wise |
| Substack | Substack | Expenses:Donation | Revolut |

### Online services

| CSV payee pattern | Ledger payee | Account | Source |
|---|---|---|---|
| Google Domains | Google Domains | Expenses:Online Services:Domains | Revolut, Wise |
| Squarespace | Squarespace | Expenses:Online Services:Domains | Wise |
| Google Drive | Google Drive | Expenses:Online services:Cloud storage | |
| Google One | Google One | Expenses:Online services:Cloud storage | Wise |
| Dropbox | Dropbox | Expenses:Online services:Cloud storage | |
| Digital Ocean | Digital Ocean | Expenses:Polymarket Bot | Wise |
| Meta Horizon | Meta Horizon store | Expenses:Online Services:Apps | Wise |
| Fitbit | Fitbit | Expenses:Online Services:Apps | |
| Google Play | Google Play | Expenses:Online Services:Apps | |
| La Nación | La Nación | Expenses:Online Services:Misc | |
| Anthropic | Anthropic | Expenses:Online Services:Misc | Wise |

### Clothing

| CSV payee pattern | Ledger payee | Account | Source |
|---|---|---|---|
| Wool & Prince | Wool & Prince | Expenses:Clothing:Clothes | Wise, Revolut |
| Levi's | Levi's | Expenses:Clothing:Clothes | |
| Minaal | Minaal | Expenses:Clothing:Clothes | |

### Fees (auto-generated from transfers)

| CSV payee pattern | Ledger payee | Account |
|---|---|---|
| Trade | Trade | Expenses:Fees |
| Interactive Brokers | Interactive Brokers | Expenses:Fees |
| Kraken | Kraken | Expenses:Fees |

### Taxes

| CSV payee pattern | Ledger payee | Account | Source |
|---|---|---|---|
| ARCA | ARCA | Expenses:Taxes | MP |

### Misc

| CSV payee pattern | Ledger payee | Account | Source |
|---|---|---|---|
| Unfolding Adventure | Unfolding Adventure | Expenses:Misc:Bike | Wise |

## 2. High-confidence defaults (use default unless description suggests otherwise)

These payees usually map to one category but sometimes vary. Use the default. If the CSV description or amount suggests an alternate, use that instead. Mark with `!` in staging if uncertain.

| CSV payee pattern | Ledger payee | Default account | Alt categories (triggers) |
|---|---|---|---|
| Milu | Milu | Expenses:Housing:Maid (; maid) | Food:Groceries (yogurt, groceries), Housing:Household (cleaning) |
| Starbucks | Starbucks | Expenses:Food:Coffee | Food:Groceries (beans, ground coffee) |
| Rappi | Rappi | Expenses:Food:Takeaway | Food:Misc, Food:Groceries |
| Fa Song Song | Fa Song Song | Expenses:Food:Takeaway | Food:Restaurant (dine-in) |
| Hierbabuena | Hierbabuena | Expenses:Food:Restaurant | Food:Coffee (coffee only) |
| Petit Colón | Petit Colón | Expenses:Food:Coffee | Food:Restaurant (meal) |
| Bulevar | Bulevar | Expenses:Food:Coffee | Food:Restaurant (meal) |
| Federico Stafforini | Federico Stafforini | Expenses:Food:Groceries | Food:Restaurant |
| Carrefour | Carrefour | Expenses:Food:Groceries | Food:Takeaway |
| Facebook Marketplace | Facebook Marketplace | Expenses:Housing:Household | Leisure:Books |
| Farmacity | Farmacity | Expenses:Health:Pharmacy | Beauty:Hair (shampoo, hair products), Health:Supplements |
| Cúspide | Cúspide | Expenses:Gift | Leisure:Books (for self) |
| Audible | Audible | Expenses:Leisure:Audiobooks | Leisure:Books, Online Services:Apps |
| AGIP | AGIP | Expenses:Housing:Utility | Tucumán 11, Tucumán 10 (rental property) |
| Subway | Subway | Expenses:Food:Takeaway | Transportation:Metro (if small amount, transit context) |

## 3. Ambiguous payees (always ask user or infer from description)

These payees map to many categories. The CSV description field is essential.

### Mercado Libre (~555 expense transactions)

Top categories:
- Housing:Household 41% (household items, tools, repairs supplies)
- Food:Groceries 15% (chocolate, whey, collagen, snacks)
- Electronics:Misc 6% (chargers, cables, adapters)
- Housing:Gym equipment 4%
- Health:Pharmacy 3% (floss, medical supplies)
- Electronics:Accessories 3%
- Health:Supplements 3% (collagen, protein)
- Electronics:Mobile 3% (phone cases, chargers)
- Clothing:Clothes 3%
- Leisure:Books 3%
- Food:Takeaway 3% (food delivery)
- Beauty:Skin 2%

**Strategy**: Use the product description from Mercado Libre (which should appear in the CSV) to infer category. Common keyword→category mappings:
- whey, protein, collagen, creatine → Health:Supplements
- floss, band-aid, medical → Health:Pharmacy
- charger, cable, usb, adapter, hub → Electronics:Misc
- phone, funda, case (phone) → Electronics:Mobile
- keyboard, monitor, mouse → Electronics:Computing
- headphone, earphone, speaker → Electronics:Audio
- shirt, pants, sock, underwear → Clothing:Clothes
- book, libro → Leisure:Books
- chocolate, yogurt, snack, food → Food:Groceries
- shampoo, serum, cream, skin → Beauty:Skin
- drill, screw, tool, wrench → Housing:Household
- mattress, pillow, mirror, curtain, shelf → Housing:Household
- dumbbell, weight, band, gym → Housing:Gym equipment

### Amazon (~199 expense transactions)

Top categories:
- Health:Supplements 17%
- Leisure:Books 10%
- Electronics:Misc 10%
- Food:Groceries 9%
- Misc 8%
- Health:Pharmacy 8%
- Housing:Household 7%
- Clothing:Clothes 6%
- Beauty:Hair 5%

**Strategy**: Same keyword-based inference as Mercado Libre.

### Mom (~133 expense transactions)

Split across: Housing:Household 35%, Tucumán 10 20%, Tucumán 11 19%, Food:Groceries 17%

**Strategy**: Always ask. These are reimbursements/payments to mom for various things.

### Property-split payees (Tucumán 10 vs Tucumán 11)

These payees are split roughly 50/50 between two rental properties. The skill cannot determine which property from the payee name alone — always ask.

| Payee | Categories |
|---|---|
| Misc | Tucumán 11 (57%) / Tucumán 10 (43%) |
| Utility | Tucumán 11 (54%) / Tucumán 10 (46%) |
| Cleaning lady | Tucumán 11 (48%) / Tucumán 10 (37%) / Housing:Household (8%) / Housing:Maid (7%) |
| Cleaning products | Tucumán 11 (51%) / Tucumán 10 (40%) / Housing:Household (7%) |
| Laundry | Tucumán 11 (53%) / Tucumán 10 (47%) |
| Expensas | Tucumán 11 (55%) / Tucumán 10 (45%) |
| Reception | Tucumán 11 (50%) / Tucumán 10 (50%) |
| Internet | Tucumán 11 (92%) / Tucumán 10 (8%) |
| Electricity | Tucumán 11 (50%) / Tucumán 10 (50%) |
| ABL | Tucumán 11 / Tucumán 10 |
| Beyond Pricing | Tucumán 11 (51%) / Tucumán 10 (49%) |

**Note**: These payees are unlikely to appear in Mercado Pago or Wise CSVs (they are typically paid through other channels or entered manually). If they do appear, always ask which property.

## 4. Transfer patterns (do not categorize as expenses)

### Self-transfers

Payee "Pablo Stafforini" moving money between own accounts:
- Mercado Pago ↔ Comafi
- Mercado Pago ↔ Ualá
- Wise ↔ Revolut
- Wise ↔ Kraken
- Cash deposits/withdrawals
- Any combination of: Mercado Pago, Comafi, Wise, Revolut, Ualá, Kraken, Interactive Brokers

**Detection**: Payee is "Pablo Stafforini" AND both postings are asset accounts.

**Note**: Self-transfers often have an Expenses:Fees posting for the transfer fee. Include the fee posting.

### Payee "Transfer"

Used for moving money to/from third-party liabilities (Mom, Dad, Tlon, etc.) and between own accounts.

**Detection**: Payee is "Transfer" AND postings only touch Assets and/or Liabilities accounts.

### Payee "Conversion"

Currency conversions within an account (e.g., Revolut multi-currency). Always has Expenses:Fees.

### Dad / Mom as transfer payees

When "Dad" or "Mom" appear as payees with only asset/liability accounts (no expense accounts), these are transfers, not expenses.

## 5. Income sources

| CSV payee pattern | Ledger payee | Income account | Notes |
|---|---|---|---|
| Airbnb (payout/credit) | Airbnb | Income:Rent | Primary income. Appears in Wise and Revolut as credits. |
| Tlon / Tlön | Tlon | Income:Salary | Employer |
| Epoch | Epoch | Income:Honorarium | |
| Centre for Effective Altruism | Centre for Effective Altruism | Income:Salary | |
| HEAR | HEAR | Income:Salary | |
| Ledn | Ledn | Income:Interest | |
| Metaculus | Metaculus | Income:Prize | |
| Future Fund | Future Fund | Income:Honorarium | |

**Detection**: Inbound credits to Wise/Revolut from these payees → generate income entry.

## 6. Refund detection

**Indicators that a transaction is a refund:**
- Positive/credit amount from a payee that is normally an expense source (Mercado Libre, Amazon, etc.)
- CSV description contains: "devolución", "refund", "reembolso", "cancelación"
- Amount matches a recent debit from the same payee (within 30 days)

**Ledger format for refunds:**
```ledger
YYYY-MM-DD Mercado Libre
    Assets:Refunds                         75,998.00 ARS ; magsafe charger (2x)
    Assets:Mercado Pago
```

Use `Assets:Refunds` as the debit account, with the original item description in the comment.

## 7. Currency conventions

- ARS amounts: use thousands separator (e.g., `1,400,000.00 ARS`)
- USD/GBP/EUR: standard decimal (e.g., `200.00 USD`)
- When Wise pays for an ARS expense, use dual-amount form:
  ```ledger
  2026-03-06 Cabify
      Expenses:Transportation:Taxi            5,540.63 ARS ; taxi
      Assets:Wise                                -3.97 USD
  ```
