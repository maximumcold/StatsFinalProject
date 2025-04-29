import json
import pandas as pd
import csv
from pandas import json_normalize

# 1. Load the raw JSON
#    If your file is “one JSON object per line” use lines=True:
#    data = pd.read_json("pizza_data/pizza_request_dataset.json", lines=True).to_dict(orient="records")
#    Otherwise:
with open("pizza_data/pizza_request_dataset.json") as f:
    data = json.load(f)   # data is a list of dicts

# 2. “Flatten” nested objects into columns
df = json_normalize(data)

# 3. Convert any list-columns into delimited strings
for col in df.columns:
    if df[col].dtype == object and df[col].apply(lambda x: isinstance(x, list)).any():
        df[col] = df[col].apply(
            lambda lst: "|".join(map(str, lst)) if isinstance(lst, list) else lst
        )

# 4. (Optional) Drop any columns you don’t want
# if "comments" in df: df = df.drop(columns=["comments"])

# 5. Write out with full quoting so embedded commas stay inside quotes
df.to_csv(
    "output.csv",
    index=False,
    quoting=csv.QUOTE_ALL,  # wrap every field in quotes
    quotechar='"',
    escapechar='\\'
)