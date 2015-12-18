"""
  COMS4115: A compiled Odds program.

  Authors:
    - Alex Kalicki
    - Alexandra Medway
    - Daniel Echikson
    - Lilly Wang
"""
from __future__ import print_function
import math
import random
import sys
import matplotlib.pyplot as plt

# Odds constants
EUL = math.e
PI = math.pi

INDEX_STEP = 1000
DIST_LENGTH = 10000
SAMPLE_STEP = 100

PLOT = False

def exception(s):
    """Write exception s to stderr and exit program"""
    sys.stderr.write("%s\n" % s)
    exit(1)

def print(*args, **kwargs):
    """Plot distributions for long lists and call normal print() function,
    but return argument that was passed"""
    if type(args[0]) is list and len(args[0]) >= DIST_LENGTH:
        print_dist(args[0])
        return str(args[0])
    __builtins__.print(*args, **kwargs)
    return str(args[0])

def print_dist(dist):
    """Opens a new figure (window) for each distribution it prints, removes
    the y-axis labels, and does not show them all until the end"""
    global PLOT
    PLOT = True
    plt.figure()
    plt.hist(dist, bins=20, normed=True)
    ax = plt.gca()
    ax.axes.get_yaxis().set_visible(False)

def make_dist(start, end, f):
    """Return a list generated from dist<min, max> | f"""
    if end <= start: 
        exception("dist_make: start cannot be greater than end")
    step = (end - start) * 1.0 / INDEX_STEP
    indices = [ start + step * x for x in range(INDEX_STEP) ]

    cum_sum = 0.0
    cum_weights = []
    for x in indices:
        cum_sum += abs(f(x))
        cum_weights.append(cum_sum)
    rands = sorted([ random.uniform(0, cum_sum) for x in range(DIST_LENGTH) ])

    cum_i = 0
    rand_i = 0
    dist_list = []
    while rand_i < len(rands):
        if rands[rand_i] < cum_weights[cum_i]:
            dist_list.append(indices[cum_i])
            rand_i = rand_i + 1
        else:
            cum_i = cum_i + 1
    return dist_list


def dist_add(d1, d2):
    """Return the sum of two distributions, adding each combination"""
    s1 = d1[random.randint(0, SAMPLE_STEP - 1)::SAMPLE_STEP]
    s2 = d2[random.randint(0, SAMPLE_STEP - 1)::SAMPLE_STEP]
    return sorted([ x + y for x in s1 for y in s2 ])

def dist_mult(d1, d2):
    """Return the product of two distributions, multiplying each combination"""
    s1 = d1[random.randint(0, SAMPLE_STEP - 1)::SAMPLE_STEP]
    s2 = d2[random.randint(0, SAMPLE_STEP - 1)::SAMPLE_STEP]
    return sorted([ x * y for x in s1 for y in s2 ])

def make_discr_dist(vals, weights):
    """Return a list generated from dist<vals, weights>"""
    if len(vals) != len(weights): 
        exception("dist_make: discrete dist with different sized lists")
    
    cum_weights = [sum(weights[:i+1]) for i in xrange(len(weights))]
    rands = sorted([ random.uniform(0, max(cum_weights)) for x in range(DIST_LENGTH) ])

    cum_i = 0
    rand_i = 0
    dist_list = []
    while rand_i < len(rands):
        if rands[rand_i] < cum_weights[cum_i]:
            dist_list.append(vals[cum_i])
            rand_i = rand_i + 1
        else:
            cum_i = cum_i + 1
    return dist_list

def shift_dist(d, n):
    """Shift each element in distribution d by n"""
    return [ x + n for x in d ]

def dist_stretch(n, d):
    """Stretch distribution d, multiplying each element by n"""
    return [ x * n for x in d ]

def dist_exp(n, d):
    """Exponentiate distribution d, raising each element to power n"""
    return [ x ** n for x in d ]

def dist_sample(n, d):
    """Return a random sample of n elements in distribution d"""
    return sorted([ random.randint(0, DIST_LENGTH - 1) for x in range(n) ])

def P(n, d):
    """Return the probability that the distribution is less than 
    the inputted value
    """
    return len([i for i in d if i < n]) / DIST_LENGTH

def E(d):
    """ Expected value of the distribution """
    return sum(d) / DIST_LENGTH

def concat_str(s1, s2):
    """ Return s1 + s2 """
    return s1 + s2

"""
END ODDS CORE LIBRARY
BEGIN USER CODE
"""
