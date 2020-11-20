import numpy as np
import math
from sklearn.metrics import pairwise_kernels

def MMD2u(K, m, n):
  """The MMD^2_u unbiased statistic.
    """
  Kx = K[:m, :m]
  Ky = K[m:, m:]
  Kxy = K[:m, m:]
  term1 = 1.0 / (m * (m - 1.0)) * (Kx.sum() - Kx.diagonal().sum())
  term2 = 1.0 / (n * (n - 1.0)) * (Ky.sum() - Ky.diagonal().sum()) 
  term3 = 2.0 / (m * n) * Kxy.sum()
  return term1 + term2 - term3

def compute_null_distribution(K, m, n, iterations = 10000, random_state = None):
  """Compute the bootstrap null-distribution of MMD2u.
    """
  if type(random_state) == type(np.random.RandomState()):
    rng = random_state
  else:
    rng = np.random.RandomState(random_state)
  
  mmd2u_null = np.zeros(iterations)
  for i in range(iterations):
    idx = rng.permutation(m + n)
    K_i = K[idx, idx[:, None]]
    mmd2u_null[i] = MMD2u(K_i, m, n)
  return mmd2u_null

def kernel_two_sample_test(X, Y, kernel_function = 'rbf', iterations = 10000,
                           verbose = False, random_state = None, **kwargs):
  """Compute MMD^2_u, its null distribution and the p-value of the
    kernel two-sample test.
    Note that extra parameters captured by **kwargs will be passed to
    pairwise_kernels() as kernel parameters. E.g. if
    kernel_two_sample_test(..., kernel_function='rbf', gamma=0.1),
    then this will result in getting the kernel through
    kernel_function(metric='rbf', gamma=0.1).
    """
  m = len(X)
  n = len(Y)
  XY = np.vstack([X, Y])
  K = pairwise_kernels(XY, metric = kernel_function, **kwargs)
  mmd2u = MMD2u(K, m, n)
  if verbose:
    print("MMD^2_u = %s" % mmd2u)
  print("Computing the null distribution.")
  
  mmd2u_null = compute_null_distribution(K, m, n, iterations,
                                         random_state = random_state)
  p_value = max(1.0 / iterations, 
    (mmd2u_null > mmd2u).sum() / float(iterations))
  return mmd2u, mmd2u_null, p_value
