#!/usr/bin/env python

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns

df = pd.read_csv('timings.dat')

dnp = df.to_numpy()
#f1, f2 = (0, 2000)
f1, f2 = (0, int(len(dnp)/1.0))
slope, intercept = np.polyfit(dnp[f1:f2,0], dnp[f1:f2,3], 1)

ss = 100     # step size
sns.set_theme()
sns.set_style("darkgrid")
sns.set_palette("Set2")

# time and bandwidth subplots
fig, (ax1, ax2) = plt.subplots(2, 1, sharex=True)
ax1.plot(dnp[::ss, 0], dnp[::ss, 2]*(10**3),'o')   #max
ax1.plot(dnp[::ss, 0], dnp[::ss, 3]*(10**3),'o')   #min
ax1.plot(dnp[::ss, 0], dnp[::ss, 1]*(10**3),'o')   #mean
ax1.plot(dnp[f1:f2:ss, 0], (slope*dnp[f1:f2:ss,0] + intercept)*(10**3), 'k-', linewidth=2)
#ax1.plot(dnp[::ss, 0], intercept*(10**3) + 8.0 * dnp[::ss, 0] / (dnp[::ss, 4]) * 10**3, 'k:', linewidth=2)
#ax1.set_xlabel('message elements')
ax1.set_ylim(-0.05*dnp[0, 1]*(10**3), 1.05*dnp[-1, 1]*(10**3))
ax1.set_ylabel('time [ms]')
ax1.legend(['max', 'min', 'mean'])
ax1.text(130000, 0.025, f"t(n) = {round(intercept*10**3,2)} ms + n/({round((8.0/(slope))/(10**9),2)} GB/s)")

# bandwidth in timings.dat is given as [8.0 bytes * array size] / [total time],
# as each element in the array is a double precision (8.0 bytes).
ax2.plot(dnp[::ss, 0], dnp[::ss, 6]/(10**9),'o')   #min
ax2.plot(dnp[::ss, 0], dnp[::ss, 5]/(10**9),'o')   #max
ax2.plot(dnp[::ss, 0], dnp[::ss, 4]/(10**9),'o')   #mean
ax2.plot(np.arange(0, dnp[-1, 0], 10), len(np.arange(0, dnp[-1, 0], 10))*[8.0/slope/(10**9)], 'k--')
ax2.set_xlabel('message elements')
ax2.set_ylim(0, 1.2*dnp[-1, 4]/(10**9))
ax2.set_ylabel('bandwidth [GB/s]')
ax2.set_xticks(np.arange(0, max(dnp[:, 0]), 20000))
ax2.legend(['min', 'max', 'mean'])

plt.ticklabel_format(axis="x", style="sci", scilimits=(0,0))
plt.show()
