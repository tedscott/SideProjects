import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
#from pypalettes import load_palette

sns.set_theme(style="whitegrid")

fs=24

# load kepler data
df_kepler = pd.read_csv("kepler_deltas.csv")

# add frequency column
df_kepler['polling_freq'] = 1000 / df_kepler.active_interval_ms

plt.figure(figsize=(9,7))
#kepler
sns.lineplot(data=df_kepler.query("cpu_ms == 100"), y="energy_joules", x="polling_freq", label="Kepler 100ms", errorbar=None, marker='o',linestyle="-", markersize=10)
sns.lineplot(data=df_kepler.query("cpu_ms == 250"), y="energy_joules", x="polling_freq", label="Kepler 250ms", errorbar=None, marker='o',linestyle="-", markersize=10)
sns.lineplot(data=df_kepler.query("cpu_ms == 500"), y="energy_joules", x="polling_freq", label="Kepler 500ms", errorbar=None, marker='o',linestyle="-", markersize=10)
sns.lineplot(data=df_kepler.query("cpu_ms == 1000"), y="energy_joules",x="polling_freq", label="Kepler 1000ms" , errorbar=None, marker='o',linestyle="-", markersize=10)

# execution
# sns.lineplot(data=df_summarised.query("cpu_ms.isin([50,100,250,500,1000])"),
#              y="mean_energy_joules", x="active_interval_ms", hue="cpu_ms", marker='o', legend='full', palette='tab10',linestyle=":", markersize=10)

plt.xlabel("Polling Frequency (per second)", fontsize=fs)
plt.xscale('log')
plt.ylabel("Energy (J)", fontsize=fs)
plt.ylim(0,5)
#plt.xlim(0,500)
plt.xticks([1,10,100,1000,10000],['1','10','100','1000','10000'], fontsize = fs-8)
plt.yticks(fontsize = fs-8)
plt.legend(title="Span Exec (ms)").remove()
#plt.title("Energy Cost of Measurement",fontsize=fs+4)
plt.savefig("./plots/energy_cost_kepler_polling_freq.png", bbox_inches='tight')
plt.show()