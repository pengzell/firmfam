# Data Preparation Guide
**Firms and Intergenerational Mobility: Cross-Country Comparison**

# Overview

This document describes the data preparation and instructions in order to run the analysis script `Firms Main Script.R`.

## Folder structure

Before preparing the data, create the following folder structure on your system (any location you choose):

```
<your project folder>/
    Extract/          <- place all data preparation files here
```

No other folders need to be created manually. At the top of the script that has been sent named `Firms Main Script.R`, set `main_path` to the path of `<your project folder>` and set `country` to your country name. The script will automatically look for the data files inside `Extract/` and will create an output folder `Firms_intergen_<country>/` alongside it.

## Required datasets

Three datasets must be prepared and placed in the `Extract/` folder. We first list them below and then describe in detail what each should contain.

- **Child_Panel** — a worker $\times$ year panel for the child generation (sons and daughters).
- **Parents_Panel** — a worker $\times$ year panel for the parent generation.
- **AKM_Sample** — a full working-age population panel used to estimate two-way fixed effects. This is typically the same underlying employer–employee register data as the child panel, but covering all workers aged 25–65 rather than a specific birth cohort.

**Why three separate files?**  
The child panel and the AKM estimation panel serve different purposes and cover different populations. The child panel is restricted to a narrow birth cohort observed at prime working age; the AKM panel spans the entire working-age population to maximise the number of firm-to-firm transitions and improve the precision of firm pay premia. The parent parent panel is kept separate because parental income is measured in an earlier calendar period and used only as a socioeconomic status (SES) proxy, not for firm-effect estimation.

## File formats

The files can be produced in any statistical software (Stata and R are recommended). The analysis script accepts the following file formats:

- **`.dta`** (Stata) — recommended for cross-software compatibility.
- **`.rds`** (native R) — convenient for teams working entirely in R.
- `.sav` (SPSS) or `.sas7bdat` (SAS) — also accepted.

The script auto-detects the format from the file extension. Simply save the files with the correct stem name (e.g., `Child_Panel.rds` or `Child_Panel.dta`) inside the `Extract/` folder.

# Target Sample

Every country should aim for the following cohort and observation window:

| **Parameter** | **Target** |
| --- | --- |
| Child birth cohorts | 1980–1984 |
| Child observation ages | 35–40 |
| Child observation years | 2015 onwards |
| Parent income window | When child was aged 13–17 (approx. 1993–2001) |

**If your data cannot support this target.** If data limitations make the 1980–1984 / ages 35–40 target infeasible, the overriding priority is *the age at which child income is measured*: shift cohorts so the child is observed as late as possible, then choose the parent income window and the AKM sample years around that.

# Variable Names

The variable names below are *required* by the analysis script. Any deviation will cause the script to fail or produce incorrect results.

## Child Panel (name of file: `Child_Panel`)

| Variable | Type | Description |
| --- | --- | --- |
| `workerID` | String | Unique person identifier for the child |
| `yob` | Integer | Child's birth year |
| `age` | Integer | Child's age in the income year |
| `female` | Integer | Gender: $1 =$ woman, $0 =$ man |
| `year` | Integer | Calendar year of the income observation |
| `earning` | Numeric | Annual pre-tax labour earnings at the main employer. Inflation adjusted. |
| `firmID` | String | Employer identifier |
| `f_workerID` | String | Person identifier of the father (or household head if father unavailable) |
| `m_workerID` | String | Person identifier of the mother |
| `FTE` | Integer | Full-time Employed Dummy (see Full-Time Employment Filter section): $1 =$ full-time, $0 =$ otherwise |
| **Variables used for covariate decomposition if available (see Covariate Variables section):** | | |
| `ed_ID` | Integer | Education level category |
| `czID` | String/Int | Commuting zone or municipality identifier at child age 13–17 |
| `immigrant_bg` | Integer | Child is classified as immigrant if at least two among {child, father, mother} were born abroad. |
| `sectorID` | String | Industry identifier (1-digit sector code) |
| `munID` | String | Municipality identifier |

*Required and optional variables in `Child_Panel`.*

The child panel should contain *one row per child per income year* within the observation window. The child cohort restriction (1980–1984) and age window (35–40) are applied by the script; you may include a slightly wider range and let the script filter.

## Parents Panel (name of file: `Parents_Panel`)

The parents' panel contains *one row per parent per year with earnings* within the parental income window (child age 13–17).

| Variable | Type | Description |
| --- | --- | --- |
| `p_workerID` | String | Unique person identifier for the parent |
| `p_yob` | Integer | Parent's birth year |
| `p_age` | Integer | Parent's age in the income year |
| `p_female` | Integer | $1 =$ mother, $0 =$ father |
| `p_year` | Integer | Calendar year of the income observation |
| `p_earning` | Numeric | Annual total pre-tax labour earnings of the parent. Inflation adjusted. |

*Required variables in `Parents_Panel`.*

## AKM Panel (name of file: `AKM_Sample`)

The AKM panel should cover the *full working-age population* (all workers aged 25–65, not only the birth cohort of interest) for the years 2015 onward. A larger and longer panel improves the precision of the estimated firm pay premia. The script applies year, age, and firm-size filters internally.

| Variable | Type | Description |
| --- | --- | --- |
| `workerID` | String | Unique person identifier |
| `year` | Integer | Calendar year |
| `age` | Integer | Age in calendar year |
| `earning` | Numeric | Annual pre-tax labour earnings at the main employer. Inflation adjusted. |
| `firmID` | String | Employer identifier |
| `female` | Integer | Gender: $1 =$ woman, $0 =$ man |
| `FTE` | Integer | Full-Time Employed Dummy (see Full-Time Employment Filter section): $1 =$ full-time |
| **If available:** | | |
| `sectorID` | String | Industry identifier (1-digit sector code) |

*Required and optional variables in `AKM_Sample`.*

# Details on variable construction

## Annual Earnings (`earning`) in the Child Panel and AKM Panel

### Defining the main employer

If a worker had more than one employer in a given calendar year, then to define the main employer:
1. Select the employer at which the worker spent the most time.
2. If there is a tie, select the employer from whom the worker earned the most in that year.
3. If duration of the employment spell is not available, then directly select the employment spells with highest annual earnings.

### Defining annual earnings at the main employer

Construct `earning` using the following priority order; use the first option your data can support:

- **Employer-specific earnings, scaled to a full-year equivalent**: take the earnings recorded at the main employer and scale them to a full year based on the number of months worked there that year, e.g., $\text{(earnings at main employer / months worked at main employer)} \times 12$.
- **Total annual earnings, not attributed to a specific employer**: if months worked at the main employer are unavailable (so the employer-specific figure cannot be annualised), use the worker's total annual earnings summed across all employers in that year instead, while still reporting `firmID` as the main employer identified above.

## Annual Earnings (`earning`) in the Parents Panel

In contrast to the Child panel, in the Parents panel the earnings variable is total earnings in the year, i.e., the sum of earnings with all employers in that year.

## Full-Time Employment (`FTE`) Filter

Focus in the AKM-analysis is on full-time employed workers. To ensure comparability across countries, each team must create a binary `FTE` dummy variable ($1 =$ full-time equivalent worker, $0 =$ otherwise) in both the child panel and the AKM panel. To construct the FTE dummy, use the following priority order. If the first option is unavailable or cannot be implemented with your data, proceed to the next option in the list, and so on.

- **Survey/administrative FTE indicator**: Your data already contains a variable/indicator for full-time employment status. Rename or recode it to `FTE` ($1 =$ full-time, $0 =$ otherwise).
- **Minimum wage threshold**: You know the country's statutory or administrative annual minimum wage for each income year. Set $\text{FTE} = 1$ if annual earnings exceed this threshold.
- **32.5% of national average**: Set $\text{FTE} = 1$ if annual earnings exceed $32.5\%$ of the mean annual earnings of the full working-age population in your AKM sample. This threshold should be computed from `AKM_Sample` and applied consistently to both the child panel and the AKM panel.

## Covariate Variables

Covariate variables enable the decomposition of the intergenerational elasticity by observable characteristics. All covariates are optional; if a variable is absent from the child panel, the corresponding decomposition is simply omitted without breaking the script.

### Education (`ed_ID`)

Education level of the child, coded as an integer with the following construction:

1. Less than high school
2. High school diploma
3. Some college / vocational post-secondary
4. Bachelor's degree or above

*Which year to use:* if possible, set `ed_ID` to the child's **latest observed education value within the adult observation window** (ages 35–40, from 2015 onward). If that is not feasible with your data, use the latest education value observed for the child overall.

### Commuting zone (`czID`)

Commuting zone where the child lived at age 13–17, i.e., during the parental income window. Use the finest geographic unit available that remains consistent across years. Store as a string or integer identifier. If the commuting zone is unavailable, you can set it equal to the municipality ID (`munID`).

**Construction rule:** set `czID` (and `munID`) to the *modal* geography observed across ages 13–17 (based on year-specific residence information). If there is a tie between two or more geographies, select the **earliest** year's geography among the tied candidates — earlier childhood years are considered more formative, and the child is more likely to have already lived there before age 13. Report `munID` alongside `czID` even when `czID` is available, since both may be used in the analysis.

### Immigrant background (`immigrant_bg`)

Binary indicator for immigrant background. Child is classified as immigrant if at least two among {child, father, mother} were born abroad.

# Running the Script

Once the datasets are in the `Extract/` subfolder, open `Firms Main Script.R` and set the following two lines at the top:

```R
main_path <- "/your/path/to/the/project/folder"
country   <- "YourCountry"
```

**Software requirements.** The script runs in R ($\ge 4.1$). The following packages are installed automatically on first run if not already present: `data.table`, `haven`, `lfe`, `ggplot2`, `systemfit`, `car`, `openxlsx`, `LeaveOutKSS`.

# Optional: Prepare Now If Convenient

The items in this section are not required for the first version of the analysis but may be straightforward to prepare at the same time as the main datasets. Preparing them now avoids having to return to the data later.

## Additional covariates

The following variables will be used in extended analyses. Add them to `Child_Panel` now if they are easy to construct:

- Child occupation and industry: 2-digit and 4-digit codes.
- Field of study (for countries with education register data).
- Municipality, commuting zone, and neighbourhood identifiers at both childhood (age 13–17) and adulthood (analytical window).

## Robustness sample specifications

The first version uses only the main cohort specification (children born 1980–1984). If your data support the following extensions, prepare the corresponding panels now; they will be used in later robustness checks.

| **Extension** | **Purpose** | **Target** |
| --- | --- | --- |
| Age extension | Lifecycle effects | Born 1965–1969, ages 25–40, years 2005 onwards |
| Cohort extension | Change over time | Born 1965–1969, ages 35–40, years 2005 onwards |

If prepared, save these as `Child_Panel_robustness`, `Parents_Panel_robustness`, etc. The analysis script accepts a `spec = "robustness"` switch that automatically looks for the `_robustness` suffix.