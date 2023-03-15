library(Epi)
library(pROC)
data(aSAH)
data <- aSAH
res <- plot.roc(data$outcome, data$s100b)
polygon(with(res, cbind(specificities, sensitivities)), 
        col = rgb(.35,0.31,0.61, alpha = 0.4), 
        border = rgb(.35,0.31,0.61, 0.4),
        lwd=2)
polygon(x = c(1,0,0),
        y = c(0,0,1),
        col = "light blue")
auc1 <- auc(data$outcome, data$s100b)
text(0.5, 0.2, 
     paste0("AUC = ", round(auc1, 3)))