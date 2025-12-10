#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import pandas as pd
import re

# ==========================================
# CONFIGURATION
# ==========================================

# Source Roots
DROPBOX_ROOT = "/Users/zungrulin/Library/CloudStorage/Dropbox/ML for Peace"
CIVIC_SRC_ROOT = os.path.join(DROPBOX_ROOT, "Counts_Civic_New", "Final_Aggregated")
RAI_SRC_ROOT   = os.path.join(DROPBOX_ROOT, "Counts_RAI_New",   "Final_Aggregated")
MLEED_SRC_ROOT = os.path.join(DROPBOX_ROOT, "Counts_Env",       "Final_Aggregated")  # NEW

# Destination Roots (repo)
REPO_ROOT     = "/Users/zungrulin/Documents/GitHub/mlp-data-intro/data"
CIVIC_DEST_DIR = os.path.join(REPO_ROOT, "1-civic-aggregate")
RAI_DEST_DIR   = os.path.join(REPO_ROOT, "1-rai-aggregate")
MLEED_DEST_DIR = os.path.join(REPO_ROOT, "1-mleed-aggregate")  # NEW (rename to 1-env-aggregate if you prefer)

# Ensure destination directories exist
os.makedirs(CIVIC_DEST_DIR, exist_ok=True)
os.makedirs(RAI_DEST_DIR,   exist_ok=True)
os.makedirs(MLEED_DEST_DIR, exist_ok=True)  # NEW

# ==========================================
# HELPER FUNCTIONS
# ==========================================

def get_latest_date_folder(country_path):
    """Find latest YYYY_M_D subfolder."""
    if not os.path.exists(country_path):
        return None
    subdirs = [d for d in os.listdir(country_path) if os.path.isdir(os.path.join(country_path, d))]
    date_dirs = [d for d in subdirs if re.fullmatch(r'\d{4}_\d{1,2}_\d{1,2}', d)]
    if not date_dirs:
        return None
    return sorted(date_dirs)[-1]

def normalize_date(df):
    """Converts 'date' to first of month (YYYY-MM-01) if present."""
    if 'date' in df.columns:
        df = df.copy()
        df['date'] = pd.to_datetime(df['date'], errors='coerce')
        df['date'] = df['date'].dt.to_period('M').dt.to_timestamp()  # YYYY-MM-01
    return df

def rename_norm_columns(df, prefix=""):
    """
    Renames xxx_norm -> xxxNorm; if prefix provided (e.g., 'ncr_'), adds it to non-key columns.
    Keys (date/year/month/country/influencer) are kept unchanged.
    """
    df = df.copy()
    keys = {'date', 'year', 'month', 'country', 'influencer'}
    new_cols = {}
    for col in df.columns:
        if col in keys:
            new_cols[col] = col
            continue
        new_name = col
        if new_name.endswith('_norm'):
            new_name = new_name[:-5] + 'Norm'
        if prefix:
            new_name = prefix + new_name
        new_cols[col] = new_name
    return df.rename(columns=new_cols)

# ==========================================
# CIVIC DATA PROCESSING (unchanged)
# ==========================================
print(f"Processing Civic Data from: {CIVIC_SRC_ROOT}")
if os.path.exists(CIVIC_SRC_ROOT):
    countries = [d for d in os.listdir(CIVIC_SRC_ROOT) if os.path.isdir(os.path.join(CIVIC_SRC_ROOT, d))]
else:
    countries = []

for country in countries:
    country_path = os.path.join(CIVIC_SRC_ROOT, country)
    date_folder = get_latest_date_folder(country_path)
    if not date_folder:
        print(f"[CIVIC] Skipping {country}: No date folder found.")
        continue

    full_path = os.path.join(country_path, date_folder)
    file_cr  = os.path.join(full_path, f"{country}_Civic_Related.csv")
    file_ncr = os.path.join(full_path, f"{country}_Non_Civic_Related.csv")

    if os.path.exists(file_cr) and os.path.exists(file_ncr):
        try:
            df_cr  = pd.read_csv(file_cr)
            df_ncr = pd.read_csv(file_ncr)

            df_cr  = normalize_date(df_cr)
            df_ncr = normalize_date(df_ncr)

            # CR: rename xxx_norm -> xxxNorm
            df_cr = rename_norm_columns(df_cr, prefix="")

            # NCR: rename and prefix ncr_
            keys = ['date', 'year', 'month']
            df_ncr_renamed = rename_norm_columns(df_ncr, prefix="ncr_")
            # undo prefix on the merge keys
            df_ncr_renamed = df_ncr_renamed.rename(columns={f"ncr_{k}": k for k in keys})

            # Merge on keys
            df_merged = pd.merge(df_cr, df_ncr_renamed, on=keys, how='outer')

            out_file = os.path.join(CIVIC_DEST_DIR, f"{country}.csv")
            df_merged.to_csv(out_file, index=False)
            print(f"[CIVIC] Saved: {out_file}")
        except Exception as e:
            print(f"[CIVIC] Error processing {country}: {e}")
    else:
        print(f"[CIVIC] Missing files for {country} in {full_path}")

# ==========================================
# RAI DATA PROCESSING (unchanged)
# ==========================================
print(f"\nProcessing RAI Data from: {RAI_SRC_ROOT}")
if os.path.exists(RAI_SRC_ROOT):
    countries_rai = [d for d in os.listdir(RAI_SRC_ROOT) if os.path.isdir(os.path.join(RAI_SRC_ROOT, d))]
else:
    countries_rai = []

for country in countries_rai:
    country_path = os.path.join(RAI_SRC_ROOT, country)
    date_folder = get_latest_date_folder(country_path)

    if not date_folder:
        print(f"[RAI] Skipping {country}: No date folder found.")
        continue

    full_path = os.path.join(country_path, date_folder)

    influencers = {
        'China':    f"{country}_China.csv",
        'Russia':   f"{country}_Russia.csv",
        'Combined': f"{country}_Combined.csv"
    }

    for inf_name, filename in influencers.items():
        file_path = os.path.join(full_path, filename)
        if not os.path.exists(file_path):
            print(f"[RAI] Missing file for {country} {inf_name}: {file_path}")
            continue

        try:
            df = pd.read_csv(file_path)
            df = normalize_date(df)

            # Add metadata
            df['country'] = country
            df['influencer'] = inf_name

            # Reorder columns
            cols = df.columns.tolist()
            front = ['country', 'influencer', 'date']
            other = [c for c in cols if c not in front]
            df = df[front + other]

            # Rename xxx_norm -> xxxNorm
            df = rename_norm_columns(df)

            out_filename = f"{country}_{inf_name.lower()}.csv"
            out_file = os.path.join(RAI_DEST_DIR, out_filename)
            df.to_csv(out_file, index=False)
            print(f"[RAI] Saved: {out_file}")
        except Exception as e:
            print(f"[RAI] Error processing {country} {inf_name}: {e}")

# ==========================================
# MLEED (ENV) DATA PROCESSING  <-- NEW
# ==========================================
print(f"\nProcessing MLEED (Env) Data from: {MLEED_SRC_ROOT}")
if os.path.exists(MLEED_SRC_ROOT):
    countries_env = [d for d in os.listdir(MLEED_SRC_ROOT) if os.path.isdir(os.path.join(MLEED_SRC_ROOT, d))]
else:
    countries_env = []

for country in countries_env:
    country_path = os.path.join(MLEED_SRC_ROOT, country)
    date_folder = get_latest_date_folder(country_path)

    if not date_folder:
        print(f"[MLEED] Skipping {country}: No date folder found.")
        continue

    full_path = os.path.join(country_path, date_folder)
    file_path = os.path.join(full_path, f"{country}.csv")  # single flat file per country

    if not os.path.exists(file_path):
        print(f"[MLEED] Missing file for {country}: {file_path}")
        continue

    try:
        df = pd.read_csv(file_path)
        df = normalize_date(df)
        # Standardize xxx_norm -> xxxNorm (consistent with civic/rai)
        df = rename_norm_columns(df)

        out_file = os.path.join(MLEED_DEST_DIR, f"{country}.csv")
        df.to_csv(out_file, index=False)
        print(f"[MLEED] Saved: {out_file}")
    except Exception as e:
        print(f"[MLEED] Error processing {country}: {e}")
