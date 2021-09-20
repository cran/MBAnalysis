
## Dataset normalization

## @description  Gives a vector such that its norm equals one

## @usage normv(x)

## @param x A dataset or a vector to normalize.

## @return returns a normalized dataset or vector.


normv=function (x)
{
  normx = sqrt(t(x)%*% x)
  if (normx==0) normx=1 # ESSO from Evelyne
  y = x/as.numeric(normx)
  return(y)
}
