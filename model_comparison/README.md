# Fire model comparison
Comparison of extent and accuracy of fire growth models from two vendors. 
Authors: Sterling Von Dehn, Adithi Balaji

For most comprehensive and recently updated outputs, see the notes on **compare.py**. All code currently written works on 2023 data, which may be downloaded here:

https://bcgov.sharepoint.com/:u:/t/01324/EQUpIj8_xY9BnI1qiENUJZIB6tqgMLFSkeMth67GctzVwg?e=SW2S3L


This project was first started in summer 2024, comparing data from one vendor to BCWS perimeters or FIRMS/Sentinel-2 satellite data. **model.py** does this by plotting outputs, callin gon projection and conversion tools from **shape_compare.py**
Basic function: compare models predicted fire perimeter to the actual fire perimeter measured using FIRMS, Sentinel, and gps data.
Steps:
- Search for fire perimeters cut at a similar time to the simulation run
- Fire perimeters can come from BC data catalog or can be cut using FIRMS/Sentinel 2 data
- Comparison of simulated fire perimeter is done by converting shape files to rasters then implementing a confusion matrix
Sample call:
>>python3 model.py

**compare.py** expands on this by updating the perimter selection to pick the perimeter most aligned with the model end times (as opposed to end-of-season), producing skill score metrics and implementing comparison for the second source of models.

Dependencies: Python, Matplotlib, Geopandas, Pandas, Shapely
Use by calling file name with the alphanumeric fire number you are interested in seing evaluated
>>python3 compare.py K52125
>>python3 compare V82990 

Please note that not all fire in BCWS registry have data from all three sources.
