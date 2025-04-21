import pandas as pd

# Load CSV
df = pd.read_csv("data/FilteredJobOpportunities2019-2023.csv")

# Convert date column (e.g., 'Date Posted') to year only
df['Date Posted'] = pd.to_datetime(df['Date Posted'], format='%m-%d-%y').dt.year

# Save updated CSV
df.to_csv("updated.csv", index=False)