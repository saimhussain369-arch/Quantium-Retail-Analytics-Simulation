# Quantium Retail Analytics: Chip Category Strategy

**Customer segmentation and trial-store uplift testing in R, written up as a client-ready strategy report.**

![R](https://img.shields.io/badge/R-data.table%20%7C%20ggplot2-276DC3)
![Method](https://img.shields.io/badge/Method-Uplift%20testing-1F6F8B)

> Completed as part of the **Quantium Data Analytics virtual job simulation (Forage)**. The data is Quantium's
> practice dataset for a fictional supermarket chain, July 2018 to June 2019.

---

## The business question

The Category Manager for chips needs evidence for next half-year's category plan:

1. **Who** buys chips, and which customer segments drive sales?
2. **How** do those segments shop: price per unit, pack sizes, brands?
3. Did a **new chip aisle layout** in three trial stores actually increase sales?

## Headline findings

| | |
|---|---|
| **246,740** transactions and **72,637** loyalty customers analysed across **272 stores** | **Budget Older Families** are the top segment by sales (**$156.9k**), ahead of Mainstream Young Singles/Couples (**$147.6k**) and Mainstream Retirees (**$145.2k**) |
| Mainstream Young/Midage Singles/Couples pay **more per unit** than Budget and Premium peers (**t = 37.83, p < 0.00001**) | That segment over-indexes on **Tyrrells** (affinity **1.24**) and the **270g** pack (affinity **1.27**) |
| All three trial stores (**77, 86, 88**) beat their scaled control stores during the trial (Feb to Apr 2019) | Recommendation: **roll out the new layout** and target premium, flavour-led SKUs at young singles/couples |

Full report: **[Quantium_Task3_Report.pdf](Quantium_Task3_Report.pdf)** (14 slides, structured with the Pyramid Principle)

![Total sales by lifestage and segment](images/01_sales_by_segment.png)

![Average price per unit and t-test](images/02_price_per_unit.png)

## Approach

### Task 1: Customer analysis ([`Task_01_Customer_Analysis.R`](Task_01_Customer_Analysis.R))
- **Data cleaning:** converted Excel serial dates, removed salsa products that were not chips, and excluded a
  commercial bulk-buyer outlier so it did not distort segment results.
- **Feature engineering:** extracted **pack size** and **brand** from product names, and standardised brand names
  (24 brands).
- **Segmentation:** joined transactions to loyalty data and analysed sales, customer counts, units per customer and
  price per unit across **7 lifestages × 3 premium tiers**.
- **Statistics:** one-sided independent t-test on price per unit (Mainstream Young/Midage Singles/Couples vs everyone else).
- **Affinity analysis** (in the report): brand and pack-size preferences of Mainstream Young Singles/Couples against the
  rest of the population.

### Task 2: Trial store uplift testing ([`Task_02_Uplift_Testing.R`](Task_02_Uplift_Testing.R))
- Built monthly store metrics (total sales, customers, transactions per customer).
- **Control store selection:** for each trial store, scored every other store on pre-trial similarity using
  **Pearson correlation** and a **magnitude distance** measure, then picked the best match
  (77 → 233, 86 → 155, 88 → 237).
- **Scaling:** scaled control sales to the trial store's pre-trial level so the two can be compared fairly.
- **Testing:** compared trial and scaled control sales month by month during Feb to Apr 2019, using t-values
  against the pre-trial variation at 95% confidence.

### Task 3: Client report ([`Quantium_Task3_Report.pdf`](Quantium_Task3_Report.pdf))
- Situation → complication → findings → recommendations → next steps, written for a non-technical category manager.

## Recommendations

1. **Protect shelf space for Budget Older Families:** the #1 revenue segment; keep multi-pack and value formats
   well stocked.
2. **Activate Mainstream Young Singles/Couples:** target Tyrrells, Doritos and the 270g sharing format.
3. **Roll out the trial layout:** all three trial stores showed uplift against matched controls.
4. **Plan stock for the pre-Christmas spike** in transactions.

## Repository contents

| File | What it is |
|---|---|
| `Task_01_Customer_Analysis.R` | Cleaning, feature engineering, segmentation and t-test |
| `Task_02_Uplift_Testing.R` | Control store selection, scaling and uplift testing |
| `Quantium_Task3_Report.pdf` | Final strategy report for the Category Manager |
| `images/` | Charts used in this README |

## Tools

**R** (`data.table`, `ggplot2`, `readr`) · statistical testing · customer segmentation · experiment design

---

*Saim Hussain · Data & BI Analyst · [LinkedIn](https://www.linkedin.com/in/saim-hussain-data) · [Main project: UK Campaign Analytics Platform](https://github.com/saimhussain369-arch/uk-campaign-analytics-platform)*
