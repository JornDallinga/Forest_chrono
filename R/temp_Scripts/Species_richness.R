## reading data

#Species_data = read.table("Dataset recovery species richness Jorn formatted.txt")

head(Species_data)

#write.xlsx(Biomass_data, file = "output/Excel/Biomass_data.xlsx", sheetName = "Biomass_data", append = T)
#write.xlsx(Species_data, file = "output/Excel/Species_data.xlsx", sheetName = "Species_data", append = T)

Species_data <- read.xlsx("Species_data.xlsx", 1)
Landscape <- read.xlsx("output_2/Sexton_single_years/Buffer1000_Threshold30.xlsx", 1)
Climatic_data <- read.xlsx("Climatic_matrix.xlsx", 1)

## merging data frames
merge_Species <- merge(Landscape, Species_data, by = "Chronosequence", all.x = F, all.y = F, sort = T)
head(merge_Species)

## cleaning data fram
drops <- c("NA.", "Country.y", "X1", "X1.x", "X1.y", "Lat", "Long", "class", "Water_cover", "Cloud_cover","precip","land.use","T_CEC_SOIL5" )
merge_Species <- merge_Species[,!(names(merge_Species) %in% drops)]

## merge with climatic data
merge_Species <- merge(merge_Species, Climatic_data, by = "Chronosequence", all.x = F, all.y = F, sort = T)

## Cleaning second data frame
drops <- c("X1", "Country")
merge_Species <- merge_Species[,!(names(merge_Species) %in% drops)]

## writing to xl
#write.xlsx(merge_test, file = "output/Excel/merge.xlsx", sheetName = "merge_hansen", append = T)

## test linearity

## subset on biomass
subset_species<- subset(merge_Species, variable == "Srar10")
## converting to character
subset_species[,] <- sapply(subset_species[,], as.character, na.rm = T)
## Converting to numeric except for the new_drop variables
new_drop <- c("Chronosequence", "Country.x", "variable", "land.use.new", "chave.region", "general.region")
subset_species[,!(names(subset_species) %in% new_drop)] <- sapply(subset_species[,!(names(subset_species) %in% new_drop)], as.numeric, na.rm = T)
## Converting landuse to factor
landuse.factor <- c("land.use.new", "chave.region", "general.region")
subset_species[,(names(subset_species) %in% landuse.factor)] <- sapply(subset_species[,(names(subset_species) %in% landuse.factor)], as.factor)



# Selecting variables for regression
subset_species <- subset(subset_species, select = c("Chronosequence","n.patches","Forest_cover", "landscape.shape.index", "mean.frac.dim.index", "aggregation.index", "patch.cohesion.index", "land.use.new", "precip","general.region","chave.region", "T_CEC_SOIL5","CWD2","BIO15", "pred10", "pred20", "pred30", "pred40", "pred50"))

## Cleaning species data set for the value pred20. Removing NA's
subset_species <- subset_species[!is.na(subset_species$pred20),]
subset_species <- subset_species[!is.na(subset_species$Forest_cover),]
#subset_species <- subset_species[!is.na(subset_species$CWD2),]

## Removing other pred values
subset_species <- subset_species[ , -which(names(subset_species) %in% c("pred10", "pred30", "pred40", "pred50"))]

## ScatterplotMatrix

drops <- c("land.use.new", "Chronosequence", "chave.region","general.region")
subset_nolanduse <- subset_species[,!(names(subset_species) %in% drops)]
cor(subset_nolanduse)

#scatterplotMatrix(sub_nolanduse)
#write.xlsx(x = subset_nolanduse, file = "output/Excel/Correlation/Dataprep_Buffer1000_Threshold30_Year2000.xlsx", "Sexton")

## explanatory data testing


ss <- subset_species


## drop outliers
#remove <- "93"
#ss <- ss[!rownames(ss) %in% remove, ]



#ss$patch.cohesion.index
ss$aggregation.index <- ss$aggregation.index ^ 2
ss$AI2 <- ss$aggregation.index ^ 2

#ss$landscape.shape.index
ss$LSI2 <- ss$landscape.shape.index ^ 2

#ss$landscape.shape.index
ss$n.patches <- sqrt(ss$n.patches)
ss$n.patch2 <- ss$n.patches ^ 2

#ss$Forest_cover 
ss$FC2 <- ss$Forest_cover  ^ 2

#ss$patch.cohesion.index
ss$patch.trans <- ss$patch.cohesion.index ^ 5
ss$patch2 <- ss$patch.trans ^ 2

#ss$mean.frac.dim.index
ss$frac2 <- ss$mean.frac.dim.index ^ 2
#ss$prop.landscape.core


#climate
BIO15_2 <- ss$BIO15 ^ 2
#ss$CWD2_2 <- ss$CWD2 
ss$CWD_LSI <- ss$CWD2 * ss$landscape.shape.index
ss$CWD_LSI2 <- ss$CWD_LSI ^ 2

ss$Soil2 <- ss$T_CEC_SOIL5 ^ 2

## interaction testing

ss$interact <- (ss$Forest_cover) + ss$landscape.shape.index
ss$interact <-  ss$n.patches * ss$landscape.shape.index
ss$LSI_FC <- ss$landscape.shape.index + ss$Forest_cover
ss$LSI_FC2 <- ss$LSI_FC ^ 2

## regression testing
## predictor = Dependant variable

plot(ss$pred20 ~ ss$CWD_LSI, data = ss)
text(ss$T_CEC_SOIL5, ss$pred20, labels= ss$Chronosequence, cex= 0.7, pos = 3)




fit <- lm(ss$pred20 ~ aggregation.index + AI2 + CWD2, data= ss)
#plot(fit)
summary(fit)
ncvTest(fit)

## assumptions checking
Boxplot(ss$pred20 ~ ss$general.region, data = ss)
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

summary(fit)$r.squared

p <- ggplot(ss, aes(x = ss$aggregation.index, y = pred20, colour = general.region)) + geom_point(size = 3) + 
  xlab(expression("AI")) + 
  ylab("RSI at 20 years") +
  ggtitle("Scatterplot of AI and RSI at 20 years") +
  colScale +
  annotate("text", x = 2000, y = 7, label = "R-squared: .40") + 
  theme(text = element_text(size=16))
p + stat_smooth(aes(group = 1), method = "lm", formula = y ~ x + I(x^2), size = 1, fill="darkgrey", colour="black")




## select model for AIC
AIC_sel <- subset(ss, select = c("aggregation.index","AI2","T_CEC_SOIL5","CWD2","pred20", "general.region","land.use.new"))

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
fin_mod <- lm(pred20 ~ CWD2 + aggregation.index + AI2 + land.use.new, data = AIC_sel)

summary(fin_mod)
plot(fin_mod)

residual = resid(fin_mod)
hist(residual, freq = F)


# plot studentized residuals vs. fitted values, test for homostedacity
ncvTest(fin_mod)
spreadLevelPlot(fin_mod)

















# plot studentized residuals vs. fitted values, test for homostedacity
ncvTest(fit)

## Scatterplot prep
subset_species_matrix <- subset_species[ , -which(names(subset_species) %in% c("land.use", "Country.x", "variable", "Chronosequence"))]
subset_species_matrix <- subset(subset_species_matrix, select = c("Forest_cover","n.patches","patch.density", "edge.density", "landscape.shape.index", "aggregation.index", "effective.mesh.size", "patch.cohesion.index", "precip", "T_CEC_SOIL5", "pred40"))

scatterplotMatrix(subset_species_matrix)

## check for collinearity
vif(fit)

# automate VIF selection

VIF_frame <- subset(subset_species, select = c("precip","Forest_cover", "T_CEC_SOIL5", "Patch_cohesion"))

testing <- vif_func(in_frame= VIF_frame,thresh=3,trace=T)

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


## standardize data?

scaled.dat <- scale(subset_species[,c("precip","Forest_cover", "T_CEC_SOIL5", "Patch_cohesion")])

# check that we get mean of 0 and sd of 1
colMeans(scaled.dat)  # faster version of apply(scaled.dat, 2, mean)
apply(scaled.dat, 2, sd)

fit <- lm(subset_species$pred20 ~ scaled.dat, na.action = na.exclude)
summary(fit)
