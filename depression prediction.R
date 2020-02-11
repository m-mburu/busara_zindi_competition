
setwd("E:/R/busara")

dir()
library(data.table)
train <- setDT(read.csv("train.csv"))

test <- setDT(read.csv("test.csv"))

train[, sample := "train"]

test[, sample := "test"]

depression_dat <- rbindlist(list(train,test))
library(mice)

depression_dat <- depression_dat[!is.na(depressed)]
naVals <- colSums(is.na(depression_dat))
withNa <- naVals[naVals>0]
pacman::p_load(mice)

nms <- names(withNa)
nms <- nms[nms != "depressed"]
i = 1
depression_dat[, (nms) := lapply(.SD, function(x) as.numeric(x)), .SDcols = nms]

setDF(depression_dat)
for (i in  1:length(nms) ) {
    cat(i)
    md <- median(as.numeric(depression_dat[[nms[1]]]),  na.rm= T)
    depression_dat[[nms[i]]][is.na(depression_dat[[nms[i]]])] <- md
}

df <- mice(depression_dat)

depression_dat <- complete(df)

save(depression_dat, file = "depression_dat2.rda")
load( "depression_dat2.rda")
setDT(depression_dat)
depression_dat[, depressed := as.factor(depressed)]
depression_dat[, age := as.numeric(age)]

depression_dat2 <- depression_dat[, -c(1:3, 76), with = F]
fit_glm <- glm(depressed~., data = depression_dat2, family = binomial)
summary(fit_glm)
table(depression_dat$depressed)
MASS::stepAIC(fit_glm)

fit2_glm <- glm(depressed ~ married + children + hhsize + edu + 
                    hh_children + hh_totalmembers + asset_livestock + asset_savings + 
                    cons_ownfood + cons_alcohol + cons_ed + ent_wagelabor + ent_nonagbusiness + 
                    ent_nonag_flowcost + ent_farmexpenses + ent_animalstockrev + 
                    ent_total_cost + fs_adwholed_often + med_expenses_sp_ep + 
                    med_healthconsult + ed_expenses_perkid + nondurable_investment, 
                family = binomial, data = depression_dat2)


summary(fit2_glm)

pred <- ifelse(predict(fit2_glm, type = "response") > 0.5, 1, 0)

mean(pred==depression_dat$depressed)  
library(e1071)

fit2_svm <- svm(depressed ~married + children + hhsize + edu + 
                    hh_children + hh_totalmembers + asset_livestock + asset_savings + 
                    cons_ownfood + cons_alcohol + cons_ed + ent_wagelabor + ent_nonagbusiness + 
                    ent_nonag_flowcost + ent_farmexpenses + ent_animalstockrev + 
                    ent_total_cost + fs_adwholed_often + med_expenses_sp_ep + 
                    med_healthconsult + ed_expenses_perkid + nondurable_investment, 
                cost = 10000, gamma = 0.01,
                    data = depression_dat2)
#kernel = "linear"
predsvm <- predict(fit2_svm )

mean(predsvm ==  depression_dat2$depressed)


depression_dat <- depression_dat2[, 
                                  .(depressed, married , children , hhsize , edu , 
                                       hh_children , hh_totalmembers + asset_livestock , asset_savings, 
                                       cons_ownfood, cons_alcohol, cons_ed, ent_wagelabor, ent_nonagbusiness,
                                       ent_nonag_flowcost, ent_farmexpenses, ent_animalstockrev, 
                                       ent_total_cost, fs_adwholed_often, med_expenses_sp_ep, 
                                       med_healthconsult, ed_expenses_perkid, nondurable_investment)]


train.err_svm <- c()
test.err_svm <- c()
predictedtest_svm <- c()
predictedtrain_svm <- c()
listfits_svm <- c()
sets <- seq(100, 1147, by= 100)
sets[length(sets)] = 1147
mysample <- sample(1:1147, round(0.7*1147))
#train <- depression_dat[1:1143,]
#test <- depression_dat[1144:1429,]
train <- depression_dat2[mysample,]
test <- depression_dat2[-mysample,]
library(e1071)
#linear kernel did better than non linear kernel
sets <- seq(100, 803, by= 100)
tune.out = tune(svm ,depression_dat~., data=dat , kernel ="linear",
                ranges =list (cost=c(0.001 , 0.01 , 0.1, 1 ,5 ,10 ,100) ))

for (i in 1:length(sets)) {
    
    traini = train[1:sets[i],]
    svm_fit <- svm(depressed ~.,  cost = 2,
                    data = traini)
    pred_train_svm = predict(svm_fit, newdata = traini)
    train.err_svm[i] = 1 - mean(traini$depressed== pred_train_svm)
    pred_test_svm = predict(svm_fit, newdata = test)
    test.err_svm[i] = 1 - mean(test$depressed == pred_test_svm)
    cat(i," ")
    
}


library(caret)
library(dplyr)
table(pred_test_svm, test$depressed) %>% confusionMatrix(positive = "1") %>%
    broom::tidy()
matplot(sets, cbind(test.err_svm, train.err_svm), pch = 19, col = c("red", "blue"),
        type = "b", ylab = "Error", xlab = "Sample size", main = "Support Vector Machines Radial Kernel")
legend("topright", legend = c("C V", "Train"), pch = 19, col = c("red", "blue"))


library(FNN)

test[, depressed := as.numeric(as.character(depressed))]
train[, depressed := as.numeric(as.character(depressed))]
err <- c()
for (i in 1:20) {
    knn_fit <- knn(train, test, train$depressed, k = 4)
    err[i] <- 1- mean(knn_fit == test$depressed)
}


plot(err)
library(caret)
table(test$depressed, pred_test_svm) %>%
    confusionMatrix(positive = "1")  %>% broom::tidy()
