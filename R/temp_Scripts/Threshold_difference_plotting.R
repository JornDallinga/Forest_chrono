Sexton <- Masked_Raster
Hansen

legend = c("Forest","Non-Forest"), fill = c('darkgreen','beige')
#par(mfrow=c(2,2))

plot(Sexton)
title(main="Forest cover map: Sexton
      Location: Barro Colorado Island", cex.main = 1)
title(xlab="Longitude (WGS84)")
title(ylab="Latitude (WGS84)")
grid.text("Forest cover 
          (%)", x=unit(0.9, "npc"), y=unit(.75, "npc"))


plot(Hansen)
title(main="Forest cover map: Hansen
      Location: Barro Colorado Island", cex.main = 1)
title(xlab="Longitude (WGS84)")
title(ylab="Latitude (WGS84)")
grid.text("Forest cover 
          (%)", x=unit(0.9, "npc"), y=unit(.75, "npc"))

#dev.off()



# resample
Hansen <- resample(Hansen,Sexton, method = "ngb")

# ------------------------------------------------------------------
# Plotting threshold

plot(Sexton, legend = F, col = c('beige','green3',"blue"))
legend("topright", c("Forest","Non-Forest","Water"), fill = c('green3','beige',"blue"))
title(main="Forest cover map: Sexton
      Location: Barro Colorado Island, Panama", cex.main = 1)
title(xlab="Longitude (WGS84)")
title(ylab="Latitude (WGS84)")

plot(Hansen, legend = F, col = c('beige','green3',"blue"))
legend("topright", c("Forest","Non-Forest","Water"), fill = c('green3','beige',"blue"))
title(main="Forest cover map: Hansen
      Location: Barro Colorado Island, Panama", cex.main = 1)
title(xlab="Longitude (WGS84)")
title(ylab="Latitude (WGS84)")

dev.off()


#--------------------------------------------------------------------

stack.r <- stack(Sexton_1, Hansen_1)
hist(stack.r, main = (c("Sexton", "Hansen")), freq = T, xlim=c(0,100), ylim = c(0,140000),xlab = c("Forest cover (%)"), ylab = c("Frequency (n cells)"))

