
import numpy
from numpy import *
from scipy import *
#Mutual information
def mutual_info(x,y):
  N=double(x.size)
  I=0.0
  eps = numpy.finfo(float).eps
  for l1 in unique(x):
    for l2 in unique(y):
  #Find the intersections
  l1_ids=nonzero(x==l1)[0]
  l2_ids=nonzero(y==l2)[0]
  pxy=(double(intersect1d(l1_ids,l2_ids).size)/N)+eps
  I+=pxy*log2(pxy/((l1_ids.size/N)*(l2_ids.size/N)))
  return I
#Normalized mutual information
def nmi(x,y):
  N=x.size
  I=mutual_info(x,y)
  Hx=0
  for l1 in unique(x):
    l1_count=nonzero(x==l1)[0].size
  Hx+=-(double(l1_count)/N)*log2(double(l1_count)/N)
  Hy=0
  for l2 in unique(y):
    l2_count=nonzero(y==l2)[0].size
  Hy+=-(double(l2_count)/N)*log2(double(l2_count)/N)
  return I/((Hx+Hy)/2)


if __name__=="__main__":

print nmi(array([1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3])
          ,array([1,1,1,1,2,1,2,2,2,2,3,1,3,3,3,2,2]))