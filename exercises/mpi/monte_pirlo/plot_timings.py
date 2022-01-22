#!/usr/bin/env python

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns

def amdahl(N, f):
    amdahl = []
    for e in N:
        temp = float(1 + (e - 1)*f)
        amdahl.append(e/temp)
    return amdahl

df = pd.read_csv('timings.dat')

dnp = df.to_numpy()

ss = 100     # step size
sns.set_theme()
sns.set_style("white")
sns.set_palette("Set1")

N = np.arange(1,25,1)

# time and bandwidth subplots
fig, ax = plt.subplots()
ax.plot(dnp[:, 0], dnp[0,1]/dnp[:, 1], 'o')  
ax.plot(N,N, 'k-') 
ax.plot(N, amdahl(N, 0.02), '--')  
ax.plot(N, amdahl(N, 0.05), '--') 
ax.set_xlabel('processors [-]')
ax.set_ylabel('speed-up [-]')
ax.set_title("Timings for $\pi$-estimation using Monte Carlo")
ax.legend(["HPC timings", "Linear Scaling", "Amdahl's Law (f = 0.02)", "Amdahl's Law (f = 0.05)"])

plt.savefig("monte_pirlo.pdf")
plt.show()