require(changepoint)

cusum <- function(data) {

res = multiple.mean.cusum(data,
         mul.method="SegNeigh",penalty="Manual",pen.value=0.8,Q=10,
         class=TRUE,
         param.estimates=FALSE)


    return (cpts(res))


}