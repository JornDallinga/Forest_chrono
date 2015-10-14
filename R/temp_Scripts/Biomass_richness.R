## reading data

#Biomass_data = read.table("Dataset recovery biomass Jorn formatted.txt")

#head(Biomass_data)
#Biomass_data[1]

#write.xlsx(Biomass_data, file = "output/Excel/Biomass_data.xlsx", sheetName = "Biomass_data", append = T)
#write.xlsx(Species_data, file = "output/Excel/Species_data.xlsx", sheetName = "Species_data", append = T)

########################  Preparing data
Biomass_data <- read.xlsx("Biomass_data.xlsx", 1)
Landscape <- read.xlsx("output_2/Sexton_single_years/Buffer500_Threshold30.xlsx", 1)
Climatic_data <- read.xlsx("Climatic_matrix.xlsx", 1)

## merging data frames
merge_data <- merge(Landscape, Biomass_data, by = "Chronosequence", all.x = F, all.y = F, sort = T)
#head(merge_data)

## cleaning data frame
drops <- c("NA.", "Country.y", "X1", "X1.x", "X1.y", "Lat", "Long", "class", "Cloud_cover", "percMFOR", "percSFOR", "percFOR", "land.use", "precip", "T_CEC_SOIL5", "min.age", "max.age")
merge_data <- merge_data[,!(names(merge_data) %in% drops)]

## merge with climatic data
merge_data <- merge(merge_data, Climatic_data, by = "Chronosequence", all.x = F, all.y = F, sort = T)

## Cleaning second data frame
drops <- c("X1", "Country")
merge_data <- merge_data[,!(names(merge_data) %in% drops)]

## writing to xl
#write.xlsx(merge_test, file = "output/Excel/merge.xlsx", sheetName = "merge_hansen", append = T)

## test linearity

## subset on biomass
subset_biomass <- subset(merge_data, variable == "biomass")
## converting to character
subset_biomass[,] <- sapply(subset_biomass[,], as.character, na.rm = T)
## Converting to numeric except for the new_drop variables
new_drop <- c("Chronosequence", "Country.x", "variable", "land.use.new", "chave.region", "general.region")
subset_biomass[,!(names(subset_biomass) %in% new_drop)] <- sapply(subset_biomass[,!(names(subset_biomass) %in% new_drop)], as.numeric, na.rm = T)
## Converting landuse to factor
landuse.factor <- c("land.use.new", "chave.region", "general.region")
subset_biomass[,(names(subset_biomass) %in% landuse.factor)] <- sapply(subset_biomass[,(names(subset_biomass) %in% landuse.factor)], as.factor)

## add column with rainfall ^ 2
#subset_biomass$precip_2 <- subset_biomass$precip ^ 2 # make new column 


# Selecting variables for regression
subset_biomass <- subset(subset_biomass, select = c("n.patches","Forest_cover", "landscape.shape.index", "mean.frac.dim.index", "prop.landscape.core","aggregation.index", "patch.cohesion.index","effective.mesh.size", "land.use.new", "precip","general.region","chave.region", "T_CEC_SOIL5","CWD2","BIO15", "pred10", "pred20", "pred30", "pred40", "pred50"))

## Cleaning biomass data set for the value pred20. Removing NA's
subset_biomass <- subset_biomass[!is.na(subset_biomass$pred20),]
subset_biomass <- subset_biomass[!is.na(subset_biomass$Forest_cover),]
#subset_biomass <- subset_biomass[!is.na(subset_biomass$CWD2),]

## Removing other pred values
subset_biomass <- subset_biomass[ , -which(names(subset_biomass) %in% c("pred10", "pred30", "pred40", "pred50"))]

## replace remaining NA's with 0
#subset_biomass[is.na(subset_biomass)] <- 0

## ScatterplotMatrix
subset_nolanduse <- subset_biomass[-9]
subset_nolanduse <- subset_nolanduse[-10]
subset_nolanduse <- subset_nolanduse[-10]
cor(subset_nolanduse)
scatterplotMatrix(subset_nolanduse)
#write.xlsx(x = subset_nolanduse, file = "output/Excel/Correlation/Dataprep_Buffer5000_Threshold30_Year2000.xlsx", "Sexton")

# automate VIF selection
VIF_Selection <- vif_func(in_frame= subset_nolanduse, thresh=5,trace=T)

## Correlation check
cor_plot <- cor(subset_nolanduse, use="complete.obs", method="pearson")
cov_plot <- cov(subset_nolanduse, use="complete.obs", method="pearson")
scatterplotMatrix(cor_plot)
cov2cor(cov_plot)

dir.create(file.path('output/Excel/Correlation'), showWarnings = FALSE)
write.xlsx(x = cor_plot, file = "output/Excel/Correlation/Buffer1000_Threshold30_Year2000.xlsx", "Sexton")


# -------- Buffer1000.Threshold30.Year200 -------------------

# Take log10 of number of patches
# Take log of patch density
# Take log10 of landscape.shape.index
# Take log 10 of total.core.area
# Take log of T_CEC_SOIL5
detach(Tr.biomass)
Tr.biomass <- subset_biomass
attach(Tr.biomass)

# Replacing values of original columns with transformed data


#Tr.biomass$shape
Tr.biomass$landscape.shape.index <- log(Tr.biomass$landscape.shape.index)
Tr.biomass$LSI2 <- Tr.biomass$landscape.shape.index ^ 2

#Tr.biomass$patch_shape <- Tr.biomass$n.patches * Tr.biomass$landscape.shape.index
Tr.biomass$n.patches <- sqrt(Tr.biomass$n.patches)

#Tr.biomass$aggregation.index
Tr.biomass$aggregation.index <- Tr.biomass$aggregation.index ^ 2
Tr.biomass$aggr2 <- Tr.biomass$aggregation.index ^ 2

#Tr.biomass$Forest_cover
Tr.biomass$Forest_cover <- Tr.biomass$Forest_cover ^ 2


#Climatic
Tr.biomass$T_CEC_SOIL5 <- log(Tr.biomass$T_CEC_SOIL5)
Tr.biomass$T_CEC_SOIL5_2 <- Tr.biomass$T_CEC_SOIL5 ^ 2

Tr.biomass$precip <- log(Tr.biomass$precip)
Tr.biomass$precip2 <- Tr.biomass$precip ^ 2

Tr.biomass$CWD2_2 <- Tr.biomass$CWD2 ^ 2

## Linear Model

fit <- lm(Tr.biomass$pred20 ~ Tr.biomass$landscape.shape.index + Tr.biomass$n.patches + Tr.biomass$patch.cohesion.index, data = Tr.biomass)

fit <- lm(Tr.biomass$pred20 ~ Tr.biomass$BIO15, data = Tr.biomass)
summary(fit)
plot(Tr.biomass$pred20 ~ Tr.biomass$landscape.shape.index + LSI2, data = Tr.biomass)

fit <- lm(Tr.biomass$pred20 ~ landscape.shape.index + LSI2, data = Tr.biomass)
summary(fit)


## assumptions checking
Boxplot(Tr.biomass$patch.cohesion.index ~ Tr.biomass$general.region, data = Tr.biomass)
residual = resid(fit)
plot(residual)
abline(0, 0)
hist(residual, freq = F)
curve(dnorm, add = TRUE)
plot(fit)



## draw lines

#Create a custom color scale
myColors <- brewer.pal(3,"Set1")
names(myColors) <- unique(Tr.biomass$general.region)
colScale <- scale_colour_manual(name = "Region",values = myColors)


p <- ggplot(Tr.biomass, aes(x = Tr.biomass$BIO15, y = pred20, colour = general.region)) + geom_point(size = 3) + 
  xlab("precip") + 
  ylab("Biomass pred 20 years (t/ha)") +
  ggtitle("Scatterplot of precip and predicted biomass at 20 years (t/ha)") +
  colScale +
  annotate("text", x = 8.2, y = 200, label = "R-squared: .46") + 
  theme(text = element_text(size=16))
p + stat_smooth(aes(group = 1), method = "lm", formula = y ~ x + I(x^2), size = 1, fill="darkgrey", colour="black")

## add formula to plot (does not work)
lm_eqn = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}

  
p1 = p + geom_text(aes(x = 25, y = 300, label = lm_eqn(lm(y ~ x, df))), parse = TRUE)












## select model for AIC
AIC_sel <- subset(Tr.biomass, select = c("landscape.shape.index","LSI2","aggregation.index","aggr2","land.use.new", "precip", "general.region", "T_CEC_SOIL5","precip2","pred20"))

AIC_mod <- lm(AIC_sel$pred20 ~ ., data = AIC_sel)

##
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid
qqnorm(sqrt(Tr.biomass$landscape.shape.index))
shapiro.test(Tr.biomass$landscape.shape.index)

options(na.action = "na.fail") 
AIC_run <- dredge(AIC_mod, rank = "AIC")
head(AIC_run)

AIC_run
## Model result testing
fin_mod <- lm(pred20 ~ general.region + landscape.shape.index + LSI2 + precip + precip2, data = AIC_sel)

summary(fin_mod)
plot(fin_mod)



AIC_sel$precip


residual = resid(fin_mod)
hist(residual, freq = F)


# plot studentized residuals vs. fitted values, test for homostedacity
ncvTest(fin_mod)
spreadLevelPlot(fin_mod)


# No landscape matrix is significant at 5000m buffer R.

## Use AIC on previous regression

step_reg <- step(fit, direction = 'forward', trace = T)
AIC_reg$anova


## start backwards linear regression

fit <- lm(Tr.biomass$pred20 ~ ., data = Tr.biomass)
fit1 <- update(fit, .~. -land.use)
fit2 <- update(fit1, .~. -edge.density)
fit3 <- update(fit2, .~. -landscape.shape.index)
fit4 <- update(fit3, .~. -percSFOR)
fit5 <- update(fit4, .~. -T_CEC_SOIL5)
fit6 <- update(fit5, .~. -Forest_cover)
fit7 <- update(fit6, .~. -patch.cohesion.index)
fit8 <- update(fit7, .~. -n.patches)
fit9 <- update(fit8, .~. -total.core.area)
fit10 <- update(fit9, .~. -percMFOR)







## Stepwise Regression
fit <- lm(subset_biomass$pred20 ~ subset_biomass$land.use + subset_biomass$precip + subset_biomass$T_CEC_SOIL5 + subset_biomass$patch.density + subset_biomass$edge.density + subset_biomass$landscape.shape.index + subset_biomass$patch.cohesion.index + subset_biomass$effective.mesh.size,data=mydata)
fit <- lm(subset_biomass$pred20 ~ subset_biomass$precip + subset_biomass$T_CEC_SOIL5 + subset_biomass$edge.density + subset_biomass$patch.cohesion.index + subset_biomass$Forest_cover)

rainfall <- subset_biomass$precip ^ 2
step <- stepAIC(fit, direction="backward")
step$anova # display results

## check for collinearity
vif(fit)
vcov(fit)













  
###############################################PCA##############################################################
# PCA

## Standardise values
standardised_biomass <- as.data.frame(scale(subset_biomass[1:8]), center = T, scale = T)
standardised_biomass[9:10] <- as.data.frame(scale(subset_biomass[10:11]), center = T, scale = T)
standardised_biomass <- as.data.frame(scale(subset_biomass[-which(names(subset_biomass) %in% c("land.use","pred20"))]))

# applying PCA analysis
Biomass.pca <- prcomp(na.omit(standardised_biomass))
summary(Biomass.pca)
Biomass.pca$x[,1]

(Biomass.pca$sdev)^2

fit <- lm(subset_biomass$pred20 ~ Biomass.pca$x[,1] + Biomass.pca$x[,2])

# How many PCA components to retain
screeplot(Biomass.pca, type="lines")


biplot(Biomass.pca, xlab = "PC1 = 49%", ylab = "PC2 = 28%")







## regression testing
## predictor = Dependant variable
fit <- lm(subset_biomass$pred20 ~ subset_biomass$precip + subset_biomass$Forest_cover + subset_biomass$T_CEC_SOIL5 + subset_biomass$patch.cohesion.index, na.action = na.exclude)
fit <- lm(subset_biomass$pred20 ~ subset_biomass$land.use + subset_biomass$precip + subset_biomass$T_CEC_SOIL5 + subset_biomass$patch.cohesion.index, na.action = na.exclude)
fit <- lm(subset_biomass$pred20 ~ subset_biomass$Forest_cover, na.action = na.exclude)
fit <- lm(subset_biomass$pred20 ~ subset_biomass$Patch_cohesion, na.action = na.exclude)

plot(subset_biomass$pred20 ~ subset_biomass$)
plot(fit)
summary(fit)

## ScatterplotMatrix

scatterplotMatrix(subset_biomass[6:8])

## Standardise values
standardised_biomass <- as.data.frame(scale(subset_biomass[3:40]))
standardised_biomass[7:8] <- as.data.frame(scale(subset_biomass[16:17]))


## linear plotting of standardised biomass values
fit <- lm(subset_biomass_pred20$pred20 ~ subset_biomass_pred20$land.use + standardised_biomass$precip + standardised_biomass$T_CEC_SOIL5 + standardised_biomass$Patch_cohesion, na.action = na.exclude)
plot(fit)
summary(fit)

# prepare for PCA analysis
Biomass.Cleaned <- standardised_biomass[complete.cases(standardised_biomass),]
Biomass.Cleaned <- Biomass.Cleaned[ , -which(names(Biomass.Cleaned) %in% c("Cloud_cover","Water_cover"))]

# applying PCA analysis
Biomass.pca <- prcomp(na.omit(Biomass.Cleaned))
summary(Biomass.pca)
Biomass.pca$x[,1]

## check PCA variances == above 1 should be retained
(Biomass.pca$sdev)^2

### Values of first PCA component
PCA_fit <- lm(subset_biomass$pred20 ~ Biomass.pca$x[,1] + Biomass.pca$x[,2])


subset_biomass_pred20)
Biomass.pca$x[,1]

nrow(subset_biomass_pred20)
length(Biomass.pca$x[,1])

# VIF testing
VIF_frame <- subset.biomass[ , -which(names(subset_biomass) %in% c("Cloud_cover","Water_cover", "Chronosequence", "Country.x", "Class", ""))]
VIF_frame <- subset(subset_biomass, select = c("Forest_cover","patch.density", "edge.density", "landscape.shape.index", "aggregation.index", "effective.mesh.size", "patch.cohesion.index"))
testing <- vif_func(in_frame= VIF_frame,thresh=5,trace=T)

# How many PCA components to retain
screeplot(Biomass.pca, type="lines")

# 

# non-linearity
crPlots(fit)
ceresPlots(fit)

## save sexton data
Sexton_fit <- fit
Hansen_fit <- fit
Kim_fit <- fit

summary(Sexton_fit)
summary(Hansen_fit)
summary(Kim_fit)

## test for significant different between models
Anova(Sexton_fit, Hansen_fit)


