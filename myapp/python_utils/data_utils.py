#Data utility functions for Massasoit Model Forge.

#This module contains Python functions that can be called from R using the reticulate package.

import pandas as pd
import numpy as np

def clean_column_names(df, replace = True, spaces = False, firstupper = True):
    if df is None or df.empty:
        return df
    
    replacement_dictionary = {
        'pu' : 'Pick Up',
        'pd' : 'Put Down',
        'lat' : 'Latitude',
        'lon' : 'Longitude'
    }
    
    if replace:
        for key in replacement_dictionary.keys():   
            df.columns = df.columns.str.replace(key, replacement_dictionary[key])

    if spaces:
        df.columns = df.columns.str.replace('_', ' ')
    else:
        df.columns = df.columns.str.replace(' ', '_')

    if firstupper:
        df.columns = df.columns.str.title()
    else:
        df.columns = df.columns.str.lower()
    return df

def calculate_correlation(df, columns=None):
    
#Calculate correlation between numeric columns.    
#    Args:
#        df (pandas.DataFrame): Input data
#        columns (list, optional): List of columns to include. If None, all numeric columns are used.        
#    Returns:
#        pandas.DataFrame: Correlation matrix

    if df is None or df.empty:
        return None
        
    numeric_cols = df.select_dtypes(include=[np.number]).columns
    if columns:
        numeric_cols = [col for col in columns if col in numeric_cols]
        
    if len(numeric_cols) < 2:
        return None
        
    return df[numeric_cols].corr()

def append_coord(df):
    site_column = next((col for col in df.columns if 'site' in col.lower()), None)
    if site_column is None:
        raise ValueError("No column containing 'site' found in DataFrame.")

    else:
        coordinates = {
            'Leland Farm' : [42.063835,-71.249616],
            'Sachem Rock' : [42.018333,-70.951667],
            'Dunrovin Farm' : [42.003699,-70.840169],
            'Christos' : [42.06734,-71.00287],
            'Native Meadow' : [42.09107339,-71.04386531],
            'SoutheasternVocTech' : [42.183781,-71.101059],
            'Easton Powerline' : [42.183781,-71.101059],
            'Stonehill Farm' : [42.183781,-71.101059],
            'VA Hospital' : [42.183781,-71.101059],
            'Beaver Brook' : [42.183781,-71.101059],
            #Temporary fix for different site names with and without spaces
            'LelandFarm' : [42.063835,-71.249616],
            'SachemRock' : [42.018333,-70.951667],
            'DunrovinFarm' : [42.003699,-70.840169],
            'Christos' : [42.06734,-71.00287],
            'NativeMeadow' : [42.09107339,-71.04386531],
            'SoutheasternVocTech' : [42.183781,-71.101059],
            'EastonPowerline' : [42.183781,-71.101059],
            'StonehillFarm' : [42.183781,-71.101059],
            'VAHospital' : [42.183781,-71.101059],
            'BeaverBrook' : [42.183781,-71.101059],

            }
        df['coordinates'] = df[site_column].apply(lambda x: [coordinates[x][1],coordinates[x][0]] if x in coordinates else None)
    return df

