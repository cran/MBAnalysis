
## Total inertia

## @description  Compute a total inertia of a dataset

## @usage inertia(tab)

## @param tab A dataset for which the inertia should be computed.

## @return returns the total inertia of a dataset.


inertie <-function(tab) {
  tab<- scale(tab, scale=FALSE)
  tab<-as.matrix(tab)
  V<-t(tab)%*%tab
  sum(diag(V))
}
