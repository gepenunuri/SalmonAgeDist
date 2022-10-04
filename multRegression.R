library(nnls)
library(graphics)
library(quadprog)

##################CHUM REGRESSION###########################
#YEAR 3,4 and 5,6 COMBINED
a <- as.matrix(Chum_Adults_1987_2017)
a <- t(a)

#ALL FRY RELEASED FROM 1988-2018
b <- as.matrix(Chum_Fry_1988_2018)
b <- t(b)

#NON-NEGATIVE LEAST SQUARES 3&4 and 5&6
nnls(a,b)

#SUMMARY STATISTICS
X1 <- lm(b ~  0+a)
anova(X1)
summary(X1)
predict(X1)

##################CHINOOK REGRESSION ####################
#YEAR 2,3,4,5 and 6,7 COMBINED
c <- as.matrix(Chinook_Adults_1994_2016)
c <- t(c)

#ALL FRY RELEASED FROM 1996-2018
d <- as.matrix(Chinook_Fry_1996_2018)
d <- t(d)

#NON-NEGATIVE LEAST SQUARES 3&4 and 5&6
nnls(c,d)

#SUMMARY STATISTICS
X2 <- lm(c ~  0+d)
anova(X2)
summary(X2)
predict(X2)




