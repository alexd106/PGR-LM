## ----Q1, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------------------------------------------------------------------
loyn <- read.table("data/loyn.txt", header = TRUE)
str(loyn)

loyn$LOGAREA <- log10(loyn$AREA)
# create factor GRAZE as it was originally coded as an integer
loyn$FGRAZE <- factor(loyn$GRAZE)
loyn$LOGDIST <- log10(loyn$DIST)
loyn$LOGLDIST <- log10(loyn$LDIST)


## ----Q2, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------------------------------------------------------------------
## # define the panel.cor function
## panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
## {
##     usr <- par("usr")
##     par(usr = c(0, 1, 0, 1))
##     r <- abs(cor(x, y))
##     txt <- format(c(r, 0.123456789), digits = digits)[1]
##     txt <- paste0(prefix, txt)
##     if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
##     text(0.5, 0.5, txt, cex = cex.cor * r)
## }
## 
## # subset the variables of interest
## VOI<- c("ABUND", "LOGAREA", "LOGDIST", "LOGLDIST", "YR.ISOL", "ALT", "FGRAZE")
## pairs(loyn[, VOI], lower.panel = panel.cor)
## 
## # There are variable degrees of correlation between explanatory variables which
## # might indicate some collinearity, i.e. LOGAREA and FGRAZE (0.48), LOGDIST and
## # LOGLDIST (0.59) and YR.ISOL and FGRAZE (0.56). However, the relationships
## # between these explanatory variables are quite weak so we can probably continue
## # and include these variables (but keep an eye on things). There also seems to be
## # a decent spread of observations across these pairs of predictors.
## 
## # The relationship between the response variable ABUND and all the explanatory
## # variables is visible in the top row:
## #  Some potential relationships present like with LOGAREA (positive),
## #  maybe ALT (positive) and FGRAZE (negative).


## ----Q3, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------------------------------------------------------------------
M1 <- lm(ABUND ~ LOGDIST + LOGLDIST + YR.ISOL + ALT + LOGAREA + FGRAZE + 
           FGRAZE:LOGAREA, data = loyn)


## ----Q4, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------------------------------------------------------------------
summary(M1)


## ----Q5, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------------------------------------------------------------------
# Wait: why did we not use information from the 'summary()' or 'anova()' functions
# to do this?

# the 'summary' table tests if the coefficient for each explanatory variable 
# is significantly different from zero.

# the 'anova' tests for the significance of the proportion of variation explained
# by a particular term in the model. 

# The ANOVA allows testing the overall significance of categorical explanatory
# variables which involves several coefficients together, which is quite is handy. 
# But the results of this ANOVA are based on sequential sums of squares and therefore
# the order of the variables (which is arbitrary here) in the model matters.

# We could change the order but there are too many possible permutations
# Summary p-values don't suffer that problem but test different hypotheses
# It would be useful to use an ANOVA that doesn't depend on the order
# of inclusion of the variables, like 'drop1'

drop1(M1, test = "F")

# LOGLDIST is the least significant (p = 0.88), and therefore makes the least 
# contribution to the variability explained by the model, with respect to 
# the number of degrees of freedom it uses (1)


## ----Q7, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------------------------------------------------------------------
M2 <- lm(ABUND ~ LOGDIST + YR.ISOL + ALT + LOGAREA + FGRAZE +
           LOGAREA:FGRAZE, data = loyn) # removing LOGLDIST here

# or use a shortcut with the update function:

M2<- update(M1, formula = . ~ . - LOGLDIST) # "." means all previous variables
drop1(M2, test = "F")

# YR.ISOL is now the least significant (p = 0.859), hence makes the least 
# contribution to the variability explained by the model, 
# with respect to the number of degrees of freedom it uses (1)


## ----Q8, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------------------------------------------------------------------
M3 <- update(M2, formula = . ~ . - YR.ISOL)

drop1(M3, test = "F")

# LOGDIST now the least significant (p = 0.714) and should be removed from 
# the next model.


## ----Q9, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------------------------------------------------------------------
M4 <- update(M3, formula = . ~ . - LOGDIST)
drop1(M4, test = "F")

# ALT is not significant (p = 0.331)


## ----Q10, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-----------------------------------------------------------------------------------------------
# and finally drop ALT from the model
M5 <- update(M4, formula = . ~ . - ALT)
drop1(M5, test = "F")

# the LOGAREA:FGRAZE term represents the interaction between LOGAREA and
# FGRAZE. This is significant (p = 0.005) and so our model selection
# process comes to an end.


## ----Q11, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-----------------------------------------------------------------------------------------------
# As the interaction between LOGAREA and FGRAZE was significant at each step of
# model selection the main effects should be left in our model,
# irrespective of significance. This is because it is quite difficult to 
# interpret an interaction without the main effects. The drop1 
# function is clever enough that it doesn't let you see the p-values for the 
# main effects, in the presence of their significant interaction.

# Also note, because R always includes interactions *after* their main effects
# the P value of the interaction term (p = 0.005) from the model selection 
# is the same as P value if we use the anova() fucntion on our final model

# Demo:
anova(M5)
drop1(M5, test= "F") 


## ----Q12, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-----------------------------------------------------------------------------------------------
# Biologically: confirming what we already found out in the previous LM exercises:
# There is a significant interaction between the area of the patch and the level 
# of grazing 

# Some observations are poorly predicted (fitted) using the set of
# available predictors.

# Interpretation: 
# Bird abundance may increase with patch area due to populations being more
# viable in large patches (e.g. less prone to extinction), 
# or perhaps because there is proportionally less edge effect in larger
# patches, and this in turn provides more high quality habitat for species 
# associated with these habitat patches

# The negative effect of grazing may be due to grazing decreasing resource
# availability for birds, for example plants or seeds directly, or insects
# associated with the grazed plants.

# Methodologically:
# Doing model selection is difficult without intrinsic / expert knowledge
# of the system, to guide what variables to include.
# Even with this data set, many more models could have been formulated.
# For example, for me, theory would have suggested to test an interaction 
# between YR.ISOL and LOGDIST (or LOGLDIST?), 
# because LOGDIST will affect the dispersal of birds between patches 
# (hence the colonisation rate), and the time since isolation of the patch may 
# affect how important dispersal has been to maintain or rescue populations 
# (for recently isolated patches, dispersal, and hence distance to nearest
# patches may have a less important effect)


## ----QA1a, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------
# This time, we are not doing any specific hypothesis testing, rather we are 
# attempting to select a model with the 'best' goodness of fit with the minimal
# number of estimated parameters. 

# We will start with a reasonably complex but PLAUSABLE model (this is the same 
# model we started with using drop1() above.

M.start<- lm(ABUND ~ LOGLDIST + LOGDIST + YR.ISOL + ALT + LOGAREA + FGRAZE +
               LOGAREA:FGRAZE, data= loyn)

step(M.start)


## ----QA1b, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------
# The output above is rather complicated and voluminous! But let's work through 
# it step by step 

# The first table starts with the AIC of our full model (228.2) and writes out 
# the full model:
# ABUND ~ LOGLDIST + LOGDIST + YR.ISOL + ALT + LOGAREA + FGRAZE + LOGAREA:FGRAZE

# The table below this then presents the AIC from models that have had
# a single term removed from the model. 

# So the first line represents a model with LOGLDIST removed resulting in an 
# AIC of 226.4. 

# The step() function then puts LOGLDIST back into the model and then removes 
# YR.ISOL resulting in an AIC of 226.24

# This is repeated for each term in the model including the interaction term.
# Note, just like the drop1() function the main effects of LOGAREA and FGRAZE
# are not removed (or presented) as they are included in the interaction term.

# The <none> row just denotes the model with no terms removed (same AIC as the 
# Start:)

# The step() function then automatically drops the term with the lowest AIC 
# (LOGLDIST in our case) and presents models with the remaining terms 
# sequentially removed as in the first table. 

# So the model with the lowest AIC is the model with YR.ISOL removed (224.27) 
# in this round of deletions.

# The step() function then removes YR.ISOL and performs another round of deletions 
# from the model and removes LOGDIST (AIC = 222.43)

# The step() function then removes LOGDIST and performs another round of deletions 
# from the model and removes ALT (AIC = 221.57)

# Finally, the step() function displays the AIC when we remove the interaction 
# term LOGAREA:FGRAZE but this results in an increase in AIC (230.47) compared 
# to the model with the interaction term (AIC = 221.57). This suggests that the 
# fit of the model is worse when we remove the interaction term and therefore it 
# should be retained in the model (along with the main effects of LOGAREA and 
# FGRAZE). This is now our final model and the step() function stops at this
# point.


## ----QA2a, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------
# one way of constructing a summary table, for reporting the results:

# specify all the models compared during out model selection
model.formulas<- c(
  "LOGLDIST + LOGDIST + YR.ISOL + ALT + LOGAREA + FGRAZE + LOGAREA:FGRAZE",
	"LOGDIST + YR.ISOL + ALT + LOGAREA + FGRAZE + LOGAREA:FGRAZE",
	"ALT + LOGDIST + LOGAREA + FGRAZE + LOGAREA:FGRAZE",
	"ALT + LOGAREA + FGRAZE + LOGAREA:FGRAZE", 
	"LOGAREA + FGRAZE + LOGAREA:FGRAZE")

# fit each model. Need to use the noquote() function to remove the 
# quotations around our model formula (will cause an error with the lm()
# function otherwise). Also need to paste The response variable 'ABUND ~ ' 
# together with out explanatory variables
M.start <- lm(noquote(paste('ABUND ~', model.formulas[1])), data = loyn)
M.step2 <- lm(noquote(paste('ABUND ~', model.formulas[2])), data = loyn)
M.step3 <- lm(noquote(paste('ABUND ~', model.formulas[3])), data = loyn)
M.step4 <- lm(noquote(paste('ABUND ~', model.formulas[4])), data = loyn)
M.step5 <- lm(noquote(paste('ABUND ~', model.formulas[5])), data = loyn)

# obtain the AIC values for each model. Note: these will be slightly different 
# than those obtained with the drop1() function due to a small difference in
# how AIC is calculated.

model.AIC<- c(AIC(M.start),
	AIC(M.step2),
	AIC(M.step3),
	AIC(M.step4),
	AIC(M.step5))

# cerate a summary table of models and AIC values
summary.table<- data.frame(Model = model.formulas,
	AIC= round(model.AIC, 2))

# Sorting models from lowest AIC (preferred) to highest (least preferred):
summary.table<- summary.table[order(summary.table$AIC), ]

# Adding AIC differences with respect to best model:
summary.table$deltaAIC<- summary.table$AIC - summary.table$AIC[1]

# print the table on screen
summary.table


## ----Q12c, eval=TRUE, echo=FALSE------------------------------------------------------------------------------------------------------------------------------------
knitr::kable(summary.table, "html", align = "lcr", row.names = FALSE)

