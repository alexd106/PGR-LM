# Modernisation Plan: PGR-LM Course Exercises

## Guiding Principles

This plan reviews each exercise and solution file in the PGR-LM course and recommends changes to bring them into line with current statistical best practice and pedagogical thinking. All suggestions respect the following constraints agreed at the outset:

- **Audience**: Mostly complete R beginners (they have a attended a previous 3-day introductory R course) from diverse non-statistics backgrounds.
- **R style**: Base R throughout, unless good reason not to. Suggestions flag when a non-base package is required.
- **Dataset**: Loyn bird abundance data retained throughout for continuity.
- **Structure**: 4-day course structure unchanged.
- **Model selection**: F-test and AIC-based approaches presented with equal weight.

The overarching shift is from a **hypothesis-testing-first** framing to an **estimation-first** framing. This means leading with "what is our estimate and how uncertain are we?" before "is this significant?". This is consistent with current guidance from the American Statistical Association and reporting standards in most biological and medical journals.

---

## Cross-Cutting Changes

These apply to all exercises and solutions.

### 1. Remove non-base package dependencies where base R suffices

The following packages are currently used but are not base R and can be replaced without loss:

| Current | Replacement | Where |
|---|---|---|
| `lattice::dotplot()` | `dotchart()` (already used) | GDE |
| `lattice::xyplot()` | `coplot()` (already used) | GDE |
| `gplots::plotmeans()` | `tapply()` + `points()` on boxplot | LM2 solutions |
| `effects::allEffects()` | `predict()` approach (already shown) | LM2 solutions |

The `car` package (`car::vif()`) is recommended as a new addition in LM4 for VIF-based collinearity checking. It is not base R but ships with most RStudio installations and is widely used.

### 2. Add `confint()` as a standard step after every model fit

`confint()` is base R and produces 95% confidence intervals on model parameters. It should appear alongside `summary()` in every exercise from LM1 onwards. Students should be encouraged to report these intervals in their written results, not just p-values. This is a single-line addition but has high pedagogical value.

```r
summary(birds.lm)
confint(birds.lm)
```

### 3. Add explicit learning objectives to each exercise

Each exercise currently begins with contextual background but does not state clearly what students will be able to do by the end. A short bulleted list of learning objectives at the top of each file would help students orient themselves and help instructors assess whether the exercise has achieved its aims.

### 4. Estimation-first language in question prompts

Questions that currently ask "is there a significant relationship?" should be reframed to ask "what is the estimated relationship, and how certain are we?". Significance can follow as a secondary question. This is a language change that does not require new R code — it is a change to the prose of the exercise questions themselves.

### 5. Results paragraphs: report CIs alongside p-values

The academic writing questions in each exercise (Q12/Q13) should explicitly instruct students to include confidence intervals in their results paragraphs. For example: "The slope of bird abundance on log-transformed area was 9.78 (95% CI: 7.16 to 12.40; F₁,₆₅ = 90.7, p < 0.001)."

### 6. Clarify Type I vs Type II sums of squares

The `anova()` function in R computes Type I (sequential) sums of squares: each term is tested after all preceding terms. This is unambiguous for simple models but becomes misleading in models with multiple predictors or interactions, where the order of terms affects results. `drop1(test = "F")` computes Type II (marginal) sums of squares and is the appropriate tool for testing main effects when an interaction is present. This distinction is currently unexplained in the course and should be addressed in LM3 and LM4. A brief explanatory note (not a lengthy technical digression) is sufficient.

---

## Graphical Data Exploration Exercise (Day 1)

### What works well

The exercise does an excellent job of introducing a systematic exploration protocol: import → structure → missing values → outliers → distributions → collinearity → relationships → interactions. The use of `dotchart()` for outlier detection and `coplot()` for interaction visualisation are both base R functions that remain best-in-class for their purpose. Keep these.

### Suggested additions

**1. Add a brief note on why we explore data before modelling**

The exercise does not explain *why* graphical data exploration is necessary. A short introductory paragraph explaining that exploratory analysis should be conducted without reference to the research question — to avoid unconsciously shaping the analysis around the desired result — would be valuable. This concept (sometimes called HARKing: Hypothesising After Results are Known) is directly relevant to reproducible research and is appropriate to introduce at this stage even at a conceptual level.

**2. Add interpretation guidance for the correlation matrix (Q9)**

The collinearity section currently asks students to compute a correlation matrix and identify pairs of correlated variables. It would benefit from a rule-of-thumb threshold: pairs with |r| > 0.7 are typically considered problematic and worth investigating further. Students should also be told that VIF (variance inflation factor) is a more formal measure of collinearity that they will encounter in LM4.

### Suggested removals / simplifications

**Remove `lattice::dotplot()` and `lattice::xyplot()`**

The `lattice` package adds a dependency and introduces a plotting syntax that students will not use again. Base R equivalents (`dotchart()` and `coplot()`) are already present in the exercise and do the same job. Removing the lattice questions reduces cognitive load without any loss of content.

**Simplify the frequency table question**

`xtabs()` is a more advanced function than `table()` and is not used elsewhere in the course. Replacing it with `table()` reduces the number of functions students need to learn and is equally effective for showing the distribution of a categorical variable.

---

## Linear Model 1 Exercise (Day 2)

### What works well

The exercise builds logically from data exploration to model fitting, interpretation, diagnostics, prediction, and write-up. The inclusion of the back-transformation question (Q14) is important and unusual — most introductory courses omit this entirely. The 4-panel diagnostic plot (`plot(model)`) is standard and well-documented. Keep all of this.

### Suggested additions

**1. Add `confint()` after `summary()` (Q5)**

As noted in the cross-cutting section, confidence intervals on the slope and intercept should be a required step, not optional. The question prompt should explicitly ask students to extract and interpret these intervals alongside the coefficient estimates from `summary()`.

**2. Distinguish prediction intervals from confidence intervals (Q10)**

The exercise currently uses `predict(..., interval = "confidence")` to produce a fitted line with confidence bands. This is correct but an opportunity is missed: the distinction between a *confidence interval on the mean response* and a *prediction interval for a new individual observation* is a fundamental concept that is rarely taught well. Adding a follow-up question that asks students to compute both and plot them together on the same graph is a concrete, visual way to make this distinction clear. Both are available via `predict()` in base R.

```r
# Confidence interval on mean response
conf.int <- predict(birds.lm, newdata = new.data, interval = "confidence")

# Prediction interval for a new observation
pred.int <- predict(birds.lm, newdata = new.data, interval = "prediction")
```

**3. Make Q14 (back-transformation) compulsory**

Back-transformation is currently marked as optional. Given that LOGAREA is a log10-transformed variable, the relationship between the fitted values on the log scale and the actual biological interpretation is non-trivial. Every student who builds this model needs to understand that the fitted line on the original scale is a curve, not a straight line, and that the slope parameter does not have a simple additive interpretation once back-transformed. Making this question compulsory and placing it immediately after the prediction question (Q10/Q11) rather than at the end as an afterthought would improve this substantially.

**4. Add a Cook's distance interpretation note (Q8)**

The 4-panel diagnostic plot already includes a Cook's distance plot. The exercise should note that a Cook's D value greater than 1 is a commonly used threshold for identifying influential observations, and that values approaching 0.5 are worth investigating. This gives students a concrete criterion rather than relying purely on visual inspection.

### Suggested changes to question framing

Questions 4, 5, and 6 currently lead with significance testing: "Is the relationship significant?", "What are the estimates?". Reframing these to lead with estimation — "What is the estimated slope and its 95% confidence interval? What does this mean biologically?" — and treating the p-value as supplementary information would better reflect modern reporting standards without changing what students compute.

---

## Linear Model 2 Exercise (Day 3, morning)

### What works well

The exercise does an excellent job of introducing categorical predictors, treatment contrasts, and the releveling approach for exploring pairwise comparisons. The Tukey HSD plot (visual display of CI on pairwise differences) is pedagogically excellent and should be retained. The emphasis on biological interpretation throughout is a strength.

### Suggested additions

**1. Add a brief discussion of planned versus unplanned comparisons**

The exercise currently introduces Tukey HSD without explaining *when* it is appropriate. Students should understand that if they had a specific a priori hypothesis about a particular pairwise comparison (e.g., "we predicted that grazing level 1 would differ from level 5"), a Tukey adjustment would be overly conservative. Tukey HSD is appropriate for unplanned (post-hoc) comparisons across all pairs simultaneously. This is a conceptual point that can be made in two or three sentences.

**2. Add `interaction.plot()` as a preview of LM3**

After fitting the categorical model, adding a question that uses base R `interaction.plot()` to visualise mean ABUND across FGRAZE levels provides a clean bridge to LM3. If students then add LOGAREA to the plot, they can see visually whether the relationship between area and abundance looks similar across grazing levels (parallel lines → no interaction) or different (non-parallel → interaction). This motivates the next exercise without requiring any new theory.

```r
interaction.plot(x.factor = loyn$FGRAZE,
                 trace.factor = loyn$FGRAZE,
                 response = loyn$ABUND,
                 xlab = "Grazing level", ylab = "Mean bird abundance")
```

### Suggested removals

**Remove `gplots::plotmeans()` and `effects::allEffects()`**

Both functions are used in the solutions to visualise group means and fitted values respectively. The base R approach — plotting group means via `tapply()` and overlaying them on a boxplot, or using `predict()` with a sequence of FGRAZE values — is already shown in the solutions and is sufficient. Removing these package dependencies simplifies the environment setup and reduces the number of plotting paradigms students encounter.

---

## Linear Model 3 Exercise (Day 3, afternoon)

### What works well

The conceptual explanation of interactions is clear and well-suited to beginners. The `coplot()` approach to visualising conditional relationships is base R, effective, and consistent with the GDE exercise. The progression from LM2 (single categorical predictor) to LM3 (categorical + continuous + interaction) is well-paced. Keep the structure.

### Suggested additions

**1. Add `interaction.plot()` before model fitting**

If the suggestion above (LM2) is adopted, students will already have seen `interaction.plot()`. In LM3, use it to show ABUND against FGRAZE, with separate lines for different ranges of LOGAREA (e.g., split into low/medium/high thirds). Non-parallel lines provide visual motivation for fitting an interaction model before any statistics are computed.

**2. Explain Type I vs Type II sums of squares (critical)**

This is the most important statistical accuracy issue in the current exercise. When `anova()` is called on a model with multiple predictors, the p-value for FGRAZE depends on whether FGRAZE appears first or second in the model formula — because `anova()` uses sequential (Type I) SS. In LM3, the model is `ABUND ~ FGRAZE + LOGAREA + FGRAZE:LOGAREA`. If this were rewritten as `ABUND ~ LOGAREA + FGRAZE + FGRAZE:LOGAREA`, the p-value for FGRAZE would change. The exercise should acknowledge this and introduce `drop1(test = "F")` as the function that removes this ambiguity by testing each term last:

```r
# anova() gives sequential (Type I) SS — order matters
anova(birds.inter)

# drop1() gives marginal (Type II) SS — order does not matter
# Preferred for testing main effects when an interaction is present
drop1(birds.inter, test = "F")
```

A brief explanatory note (two or three sentences) is enough. Students do not need to understand the algebra of Type I vs Type II SS; they need to know that `drop1()` is the safer choice for testing in models with interactions.

**3. Add a note on centring continuous predictors in interaction models**

When a continuous × categorical interaction is present, the intercept and main effect coefficients for each group are interpreted at LOGAREA = 0 (i.e., at a forest patch of 1 hectare, since LOGAREA = log10(AREA)). This happens to be at the lower boundary of the observed data in this example, so the interpretation is not unreasonable. However, centring LOGAREA (subtracting its mean before fitting) would make the main effects interpretable at the mean patch size rather than at zero. This is worth a short paragraph so that students are aware of the issue — it does not require them to refit the model. Check if this is required as the log10 transformation already enhances the interpretation of the intercept and may add unnecessary complexity for the students.

### Suggested changes

None

## Linear Model 4 Exercise (Day 4)

### What works well

The iterative use of `drop1(test = "F")` and `update()` for variable elimination is excellent — it is far cleaner than `step()` and teaches students to think about each decision rather than automating the process. The inclusion of both F-test and AIC approaches (even if AIC is currently optional) is the right instinct. Keep all of this.

### Most important change: elevate AIC to equal status

The current exercise presents p-value backward elimination as the primary workflow and AIC as an optional extension (questions A1–A5). This should be reversed or, at minimum, presented as two equally valid approaches. There are two reasons.

First, p-value-based backward elimination inflates the Type I error rate because each test uses data that have already been used to decide which terms to remove — this is sometimes called "double-dipping". The p-values at each step are not valid in the usual sense. This does not mean the approach is without value, but students should be told about this limitation explicitly.

Second, AIC-based selection does not rely on p-values at all and is therefore not subject to the same concern. It selects the model that best balances goodness of fit against complexity, and it is the dominant approach in ecology and related fields. Presenting it as secondary risks students never engaging with it in practice.

The simplest structural change is to present the two approaches in two clearly labelled parallel sections, with a brief discussion of when each might be preferred:

- **F-test backward elimination**: preferable when you have strong a priori hypotheses and want to test them in sequence; when sample sizes are small (AIC can favour overly complex models); when you need to justify each removal decision explicitly.
- **AIC-based selection**: preferable when the goal is prediction or model comparison rather than hypothesis testing; when you have a larger set of candidate models; when you want to avoid the arbitrary p < 0.05 threshold.

### Add theory-first framing before any model selection

The exercise currently asks students to fit a maximal model and then remove terms mechanically. Before doing so, students should be asked to think about which variables they expect to matter and why — based on biological knowledge, not the data. This separates confirmatory inference (testing the LOGAREA × FGRAZE interaction, which is the central hypothesis) from exploratory variable selection (deciding whether LOGDIST, LOGLDIST, YR.ISOL, and ALT are worth retaining). Making this distinction explicit guards against the impression that model selection is a purely mechanical procedure.

### Add VIF for collinearity checking

The pairs plot with correlation matrix (Q1) is a good starting point for collinearity assessment. However, pairwise correlations do not capture multicollinearity (where one variable is a linear combination of two or more others). Variance inflation factors (VIF) address this. `car::vif()` is the standard function:

```r
library(car)
vif(M1)
```

VIF values above 5 are commonly used as a flag for moderate collinearity; values above 10 are considered severe. Adding this as a question following the pairs plot (and noting that `car` is required) would give students a more complete collinearity toolkit. The connection back to the GDE exercise (where collinearity was first identified graphically) is worth making explicit.

### Replace the `noquote(paste(...))` formula construction

The current LM4 exercise uses `noquote(paste(...))` to dynamically construct model formula strings for an AIC comparison table. This is an advanced technique that is unlikely to be understood by beginners and is not best practice (it produces strings, not formula objects). Replace with explicit model-fitting code: type out each formula in full. The exercise is already long; trading conciseness for clarity is worthwhile here.

### Add a note on model validation

The final model is selected from 67 observations using a procedure that considered 6 predictors plus an interaction. Students should be aware that the model has been fitted and selected on the same data, which means its apparent fit is optimistic — if applied to a new set of forest patches, it would likely perform less well. A short paragraph noting this and pointing towards cross-validation as a way of estimating predictive performance on new data would be appropriate. Full implementation of cross-validation is beyond the scope of this course, but awareness of the issue is not.

---

## Priority Summary

| Change | Exercise(s) | Priority | Effort |
|---|---|---|---|
| Add `confint()` as standard step after model fit | LM1, LM2, LM3, LM4 | High | Low |
| Make back-transformation (Q14) compulsory | LM1 | High | Low |
| Remove `lattice`, `gplots`, `effects` dependencies | GDE, LM2 | High | Low |
| Estimation-first language in question prompts | All | High | Medium |
| Results paragraphs: require CIs alongside p-values | All | High | Low |
| Explain Type I vs Type II SS; use `drop1()` in LM3 | LM3, LM4 | High | Medium |
| Elevate AIC to equal status with F-test | LM4 | High | Medium |
| Add prediction intervals vs confidence intervals | LM1 | High | Medium |
| Add note on HARKing / why explore before modelling | GDE | Medium | Low |
| Add |r| > 0.7 collinearity threshold guidance | GDE | Medium | Low |
| Add planned vs unplanned comparisons note | LM2 | Medium | Low |
| Add `interaction.plot()` to bridge LM2 and LM3 | LM2, LM3 | Medium | Low |
| Theory-first framing before model selection | LM4 | Medium | Medium |
| Add VIF (`car::vif()`) for collinearity checking | LM4 | Medium | Low |
| Explicit learning objectives on each exercise | All | Medium | Low |
| Replace `noquote(paste(...))` formula construction | LM4 | Medium | Low |
| Add note on centring continuous predictors | LM3 | Low | Low |
| Add Cook's D threshold note (D > 1) | LM1 | Low | Low |
| Add model validation / cross-validation note | LM4 | Low | Low |

---

## What Not to Change

- The Loyn dataset — continuity across all exercises is pedagogically valuable and well-motivated.
- The progression from GDE → LM1 → LM2 → LM3 → LM4 — well-paced and logical.
- Base R `lm()`, `anova()`, `summary()`, and `plot(model)` as the core modelling workflow.
- The 4-panel residual diagnostic plot — standard, well-documented, and appropriate for beginners.
- `coplot()` for conditional plots and interaction visualisation.
- `predict()` for fitted values and intervals.
- `relevel()` for changing the reference category — an important and underappreciated technique.
- `drop1(test = "F")` and `update()` for model selection — already a strength of the course.
- The academic writing questions (Q12/Q13 in each exercise) — these are unusual and valuable.
- The biological interpretation throughout — the course does an excellent job of connecting statistics to science.
