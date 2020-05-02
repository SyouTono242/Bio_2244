dataset <- read.csv("/Users/shaoyiran/OneDrive\ -\ The\ University\ of\ Western\ Ontario/Academics/Biology\ 2244/Final/dataset.csv")
attach(dataset)
str(dataset)


weight_medal <- subset(dataset, select=c(weight, medal, sport))
Gdensity <- density(weight_medal$weight[weight_medal$medal=="Gold"&weight_medal$sport=="Athletics"])
Sdensity <- density(weight_medal$weight[weight_medal$medal=="Silver"&weight_medal$sport=="Athletics"])
Bdensity <- density(weight_medal$weight[weight_medal$medal=="Bronze"&weight_medal$sport=="Athletics"])
plot(Gdensity, main = "Density Plot of Weight of Three Medal Groups", ylim = c(0,0.035), xlab = "Weight", col = "darkgoldenrod1")
lines(Sdensity, col = "darkgray")
lines(Bdensity, col = "darkorange3")
legend("topright", inset=0.05, title = "Types of Medal", c("Golden", "Silver", "Bronze"), fill=c("darkgoldenrod1","darkgray","darkorange3"))


Gweight <- weight_medal$weight[weight_medal$medal=="Gold"&weight_medal$sport=="Athletics"]
Sweight <- weight_medal$weight[weight_medal$medal=="Silver"&weight_medal$sport=="Athletics"]
Bweight <- weight_medal$weight[weight_medal$medal=="Bronze"&weight_medal$sport=="Athletics"]
weight.residual <- c((Gweight-mean(Gweight)), (Sweight-mean(Sweight)), (Bweight-mean(Bweight)))
qqnorm(weight.residual, main="Normal QQ Plot of Residuals")
qqline(weight.residual)

weight_medal.wide <- list("Gold"=Gweight-mean(Gweight), "Silver"=Sweight-mean(Sweight), "Bronze"=Bweight-mean(Bweight))
stripchart(weight_medal.wide, main="Stripchart of Residuals by Medal Groups", xlab="Medal Type", ylab="Weight", vertical=TRUE, pch=1)

Athletics_data <- subset(dataset, sport=="Athletics")
weight_medal.aov <- aov(Athletics_data$weight~Athletics_data$medal, data=Athletics_data)
summary(weight_medal.aov)


Athletics_data <- subset(dataset, sport=="Athletics")
weight_medal.aov <- aov(Athletics_data$weight~Athletics_data$medal, data=Athletics_data)
TukeyHSD(x=weight_medal.aov, conf.level = 0.95)
Athletics_data$medal <- ordered(Athletics_data$medal, levels=c("Gold", "Silver", "Bronze"))
boxplot(Athletics_data$weight~Athletics_data$medal, data=Athletics_data, xlab="Medal Type", ylab="Weight", ylim=c(30,160), col=c("darkgoldenrod1","darkgray","darkorange3"))
text(x=1:3, y=153, labels=c("a \n n = 439","a \n n = 424","a \n n = 423"))


