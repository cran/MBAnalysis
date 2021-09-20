## Deflate dataset

## @description  Deflation of a dataset with respect to a given component

## @usage deflation(X, d)

## @param X A dataset to deflate.
## @param d A component with respect to which dataset should be deflated

## @return returns a deflated dataset.

deflation=function (X, d)
{
  Y = X - d %*% t(d) %*% X
  return(Y)
}
