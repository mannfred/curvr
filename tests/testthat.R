library(testthat)
library(curvy)

test_check("curvy")


# test that subdiv doesn't change answer test K for unit circle test that K increases for
# steeper curves test that changes in direction aren't negative and instead are converted
# to absolute values


# sanity test: using the gradient and hessian to compute k on the unit circle

# Using the canonical definition of curvature (the inverse radius), the curvature of a unit
# circle is 1 at all points (1/r = 1/1 = 1). We created a function for a partial (half??)
# cirlce for x={0,0.9999}: circlefun<-function(x) (1-(x^2))^0.5. Because the radius is 1,
# the segment

# circle
x<-seq(0, 0.9999, by=0.0001)

circlefun<-function(x) (1-(x^2))^0.5 #unit circle: 1= x^2 + y^2

dfun2<- deriv3(fun2form(function(x) (1-(x^2))^0.5), "x", func=TRUE)
param_fun<-function(t) c((1-(t^2))^0.5, t)
fun<-function(x) (1-(x^2))^0.5

gr2 <- attr(dfun2(x), "gradient") #computes 1st derivative bw x=0 to x=1

he2 <- attr(dfun2(x), "hessian")[ , , "x"] #computes 2nd derivative bw x=0 to x=1


k2 <- abs(he2)/(1 + gr2^2)^(3/2)
(sum(k2) * 0.0001) #*(180/pi)
