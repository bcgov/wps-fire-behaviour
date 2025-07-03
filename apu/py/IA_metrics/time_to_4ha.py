#####################################################################
#####################################################################
## author: Lochlan Noble
## initialization date: 2025.05.09
## purpose: Create time to 4ha IA metrics from the SFMS Daily GDB
## notes: 
## outputs: see folder ""
## updated: 2025.06.18
## To Do:
#####################################################################
#####################################################################



# Import necessary libraries (& functions in the future)
import fiona
import pandas as pd
from datetime import datetime

# current date and time in YYYYMMDDHH format
# Only use the 1300 hour for time - should this be 1500 or param?
current_time = datetime.now().strftime("%Y%m%d") + "13"

# Path to the .gdb file
gdb_path = "//WildfireGeo/Geomatics$/GIS_Data/SFMS_Daily_Shapefiles/SFMSDaily_" + current_time + ".gdb"

# List all layers in the .gdb file
layers = fiona.listlayers(gdb_path)
print("Layers:", layers)

# Example: Read the first layer into a pandas DataFrame
with fiona.open(gdb_path, layer=layers[0]) as src:
    records = [feature['properties'] for feature in src]
    df = pd.DataFrame(records)

print(df.head())