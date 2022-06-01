library(nnls)
library(graphics)
library(quadprog)

#YEAR 2 &3 COMBINED
b <- as.matrix(Year_2_3_Combined)
b <- t(b)

#YEAR 2&3 6&7 COMBINED
c <- as.matrix(Year_23_67_COmbined)
c <- t(c)

#YEAR 234 5 67 COMBINED
d <- as.matrix(Year234_5_67)
d <- t(d)

#YEAR 2/3 4/5 6/7 COMBINED
a <- as.matrix(year_23_45_67_Combined)
a <- t(a)

#EGGS RELEASED EACH YEAR 
y <- as.matrix(Eggs_Released_Each_Year)
y <- t(y)

#NON-NEGATIVE LEAST SQUARES 2:3
nnls(b,y)

#SUMMARY STATISTICS
X1 <- lm(y ~  0+b)
anova(X1)
summary(X1)
predict(X1)

#NON-NEGATIVE LEAST SQUARES 2:3 6:7
nnls(c,y)

#SUMMARY STATISTICS
X2 <- lm(y ~  0+c)
anova(X2)
summary(X2)
predict(X2)

#NON-NEGATIVE LEAST SQUARES 2-4 5 6-7
nnls(d,y)

#SUMMARY STATISTICS
X3 <- lm(y ~  0+d)
anova(X3)
summary(X3)
predict(X3)

#CONFIDENCE INTERVAL FOR ESTIMATED d1
confint(X3,'d1', level=.95)

#NON-NEGATIVE LEAST SQUARES 2:3 4:5 6:7
nnls(a,y)

#SUMMARY STATISTICS
X <- lm(y ~  0+a)
anova(X)
summary(X)
predict(X)

#CHUM DATA
#EGGS RELEASED EACH YEAR 
y1 <- as.matrix(CHUM_EggsReleased)
y1 <- t(y1)

#Return Ages 3 - 6
c1 <- as.matrix(CHUM_3_6)
c1 <- t(c1)

# Return Combined 34 & 56
c2 <- as.matrix(CHUM_34_56)
c2 <- t(c2)

#Return Combined 345 & 6 
c3 <- as.matrix(CHUM_345_6)
c3 <- t(c3)

#NNLS
nnls(c1, y1)

#Summary Stats CHUM 3/4/5/6
C1 <- lm(y1 ~ 0+c1)
anova(C1)
summary(C1)
predict(C1)

#NNLS 34/56
nnls(c2,y1)

#Summary Stats CHUM 34/56
C2 <- lm(y1 ~ 0 +c2)
anova(C2)
summary(C2)
predict(C2)

#NNLS 345 & 6
nnls(c3,y1)

#Summary Stats CHUM 345 & 6 - not as significant as C2
C3 <- lm(y1 ~ 0+c3)
anova(C3)
summary(C3)
predict(C3)



