## standardise data
# select names NOT to standardise
not_names <- c("land.use.new", "general.region", "chave.region", "pred20", "pred10", "pred30", "pred40", "pred50")


# create regression for standarizing
structure_biomass <- subset(Tr.biomass, select = c("landscape.shape.index","mean.frac.dim.index","aggregation.index", "precip","CWD2","T_CEC_SOIL5","pred50"))

cor(structure_biomass )
# Peform standarise on remaining selection
structure_biomass[,!(names(structure_biomass) %in% not_names)] <- sapply(structure_biomass[,!(names(structure_biomass) %in% not_names)], scale, center = T, scale = T)

structure_biomass[,"pred20"] <- scale(structure_biomass[,"pred20"], center = F, scale = T)
structure_biomass[,"pred30"] <- scale(structure_biomass[,"pred30"], center = F, scale = T)
structure_biomass[,"pred40"] <- scale(structure_biomass[,"pred40"], center = F, scale = T)
structure_biomass[,"pred50"] <- scale(structure_biomass[,"pred50"], center = F, scale = T)
structure_biomass[,"pred10"] <- scale(structure_biomass[,"pred10"], center = F, scale = T)



structure_biomass$LSI2 <- structure_biomass$landscape.shape.index ^ 2
structure_biomass$AI2 <- structure_biomass$aggregation.index ^ 2
structure_biomass$precip2 <- structure_biomass$precip ^ 2

#structure_biomass$PLC2 <- structure_biomass$prop.landscape.core ^ 2
#structure_biomass$NP_LSI <- structure_biomass$landscape.shape.index * structure_biomass$n.patches 

# fit standardized model
fit <- lm(structure_biomass$pred50 ~ ., data = structure_biomass)
summary(fit)

# multivariate
#fit <- lm(cbind(pred10, pred20, pred30, pred40, pred50) ~ landscape.shape.index + LSI2, data = structure_biomass)


slopes <- coef(summary(fit))[c("T_CEC_SOIL5","precip2","precip","CWD2","AI2","aggregation.index","mean.frac.dim.index","LSI2","landscape.shape.index"), 1]  #' slopes
ses <- coef(summary(fit))[c("T_CEC_SOIL5","precip2","precip","CWD2","AI2","aggregation.index","mean.frac.dim.index","LSI2","landscape.shape.index"), 2]  #' SEs

par(mfrow=c(1,2), cex = 1.2)
plot(NA, xlim = c(-.2, .2), ylim = c(0, 10), xlab = "Coefficients estimates", ylab = "", yaxt = "n")
# We can add a title:
title("50 years biomass", cex.main=.8)

# We'll add a y-axis labelling our variables:
axis(2, 1:9, c("Soil CEC",expression(log("prec")^2),expression(log("prec")),"CWD", expression("AI"^2),expression("AI"), "FDI", expression(log("LSI") ^2), expression(log("LSI"))), las = 2, cex.axis=.8)
# We'll add a vertical line for zero:
abline(v = 0, col = "gray")
# Then we'll draw our slopes as points (`pch` tells us what type of point):
points(slopes[1],1, pch = 21, col = "black", bg = "white")
points(slopes[2],2, pch = 21, col = "black", bg = "white")
points(slopes[3],3, pch = 21, col = "black", bg = "white")
points(slopes[4],4, pch = 21, col = "black", bg = "white")
points(slopes[5],5, pch = 21, col = "black", bg = "white")
points(slopes[6],6, pch = 21, col = "black", bg = "white")
points(slopes[7],7, pch = 21, col = "black", bg = "white")
points(slopes[8],8, pch = 21, col = "black", bg = "white")
points(slopes[9],9, pch = 21, col = "black", bg = "white")
# Then we'll add thick line segments for each 1 SE:
segments((slopes - ses)[1], 1, (slopes + ses)[1], 1, col = "black", lwd = 2)
segments((slopes - ses)[2], 2, (slopes + ses)[2], 2, col = "black", lwd = 2)
segments((slopes - ses)[3], 3, (slopes + ses)[3], 3, col = "black", lwd = 2)
segments((slopes - ses)[4], 4, (slopes + ses)[4], 4, col = "black", lwd = 2)
segments((slopes - ses)[5], 5, (slopes + ses)[5], 5, col = "black", lwd = 2)
segments((slopes - ses)[6], 6, (slopes + ses)[6], 6, col = "black", lwd = 2)
segments((slopes - ses)[7], 7, (slopes + ses)[7], 7, col = "black", lwd = 2)
segments((slopes - ses)[8], 8, (slopes + ses)[8], 8, col = "black", lwd = 2)
segments((slopes - ses)[9], 9, (slopes + ses)[9], 9, col = "black", lwd = 2)

# Then we'll add thin line segments for the 2 SEs:
segments((slopes - (2 * ses))[1], 1, (slopes + (2 * ses))[1], 1, col = "black", 
         lwd = 1)
segments((slopes - (2 * ses))[2], 2, (slopes + (2 * ses))[2], 2, col = "black", 
         lwd = 1)
segments((slopes - (2 * ses))[3], 3, (slopes + (2 * ses))[3], 3, col = "black", 
         lwd = 1)
segments((slopes - (2 * ses))[4], 4, (slopes + (2 * ses))[4], 4, col = "black", 
         lwd = 1)
segments((slopes - (2 * ses))[5], 5, (slopes + (2 * ses))[5], 5, col = "black", 
         lwd = 1)
segments((slopes - (2 * ses))[6], 6, (slopes + (2 * ses))[6], 6, col = "black", 
         lwd = 1)
segments((slopes - (2 * ses))[7], 7, (slopes + (2 * ses))[7], 7, col = "black", 
         lwd = 1)
segments((slopes - (2 * ses))[8], 8, (slopes + (2 * ses))[8], 8, col = "black", 
         lwd = 1)
segments((slopes - (2 * ses))[9], 9, (slopes + (2 * ses))[9], 9, col = "black", 
         lwd = 1)

