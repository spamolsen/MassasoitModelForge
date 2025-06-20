"""
Data utility functions for Massasoit Model Forge.

This module contains Python functions that can be called from R using the reticulate package.
"""
import pandas as pd
import numpy as np

def get_data_summary(df):
    """
    Generate a summary of the input DataFrame.
    
    Args:
        df (pandas.DataFrame): Input data
        
    Returns:
        dict: Dictionary containing various summary statistics
    """
    if df is None or df.empty:
        return {"error": "No data provided"}
        
    return {
        "num_rows": len(df),
        "num_columns": len(df.columns),
        "column_names": list(df.columns),
        "data_types": {col: str(dtype) for col, dtype in df.dtypes.items()},
        "missing_values": df.isnull().sum().to_dict(),
        "numeric_summary": df.describe(include=[np.number]).to_dict() if df.select_dtypes(include=[np.number]).shape[1] > 0 else {}
    }

def clean_column_names(df):
    """
    Clean column names by converting to lowercase and replacing spaces with underscores.
    
    Args:
        df (pandas.DataFrame): Input data
        
    Returns:
        pandas.DataFrame: DataFrame with cleaned column names
    """
    if df is None or df.empty:
        return df
        
    df.columns = df.columns.str.lower().str.replace(' ', '_')
    return df

# Example function that could be added later
def calculate_correlation(df, columns=None):
    """
    Calculate correlation between numeric columns.
    
    Args:
        df (pandas.DataFrame): Input data
        columns (list, optional): List of columns to include. If None, all numeric columns are used.
        
    Returns:
        pandas.DataFrame: Correlation matrix
    """
    if df is None or df.empty:
        return None
        
    numeric_cols = df.select_dtypes(include=[np.number]).columns
    if columns:
        numeric_cols = [col for col in columns if col in numeric_cols]
        
    if len(numeric_cols) < 2:
        return None
        
    return df[numeric_cols].corr()

# Add more utility functions here as needed
