#!/usr/bin/env python

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns

df = pd.read_csv('timings.dat')
dnp = df.to_numpy()
slope, intercept = np.polyfit(dnp[:,0], dnp[:,1], 1)

ss = 20     # step size
sns.set_theme()
sns.set_style("darkgrid")
sns.set_palette("Set2")

# time and bandwidth subplots
fig, (ax1, ax2) = plt.subplots(2, 1, sharex=True)
ax1.plot(dnp[::ss, 0], dnp[::ss, 2]*(10**3),'o')   #max
ax1.plot(dnp[::ss, 0], dnp[::ss, 3]*(10**3),'o')   #min
ax1.plot(dnp[::ss, 0], dnp[::ss, 1]*(10**3),'o')   #mean
ax1.plot(dnp[::ss, 0], (slope*dnp[::ss,0] + intercept)*(10**3), 'k-', linewidth=2)
#ax1.set_xlabel('message elements')
ax1.set_ylabel('time [ms]')
ax1.set_ylim(0, 0.5)
ax1.legend(['max', 'min', 'mean'])

ax2.plot(dnp[:, 0], dnp[:, 5]/(10**9),'o')   #max
ax2.plot(dnp[:, 0], dnp[:, 6]/(10**9),'o')   #min
ax2.plot(dnp[:, 0], dnp[:, 4]/(10**9),'o')   #mean
ax2.set_xlabel('message elements')
ax2.set_ylabel('bandwidth [GB/s]')
ax2.legend(['max', 'min', 'mean'])

plt.ticklabel_format(axis="x", style="sci", scilimits=(0,0))
plt.show()