x = c(23, 25, 27, 29, 31, 32, 33)
y = c(0.1, 0.2, 0.3, 0.7, 0.8, 0.6, 0.1)

library(thermPerf)
library(RColorBrewer)

m = getModelLibrary()
f = fitModels(m, x, y)

par(mfrow = c(2, 1))
weights = calculateAIC(f)
plot(weights, col = col)
plot(x, y, xlim = c(20, 35), ylim = c(-0.1, 1.1), pch = 21, bg = "red")
xp = seq(23, 33, length.out = 128)
col = RColorBrewer::brewer.pal(length(m), "Set2")
for (i in 1:length(f)) {
    lines(xp, predict(f[[i]], newdata = data.frame(x = xp)), col = col[i],
          lwd = weights$modelWeight[i] * 6 + 1)
}
