#!/usr/bin/env python3

"""
Path adaptation solution for Surge.py to work with the current repository structure.

This script provides two approaches:
1. Create a symbolic link structure that mimics what Surge.py expects
2. Modify Surge.py to work directly with the current data structure

Current repo structure: data/final-counts/, data/1-civic-aggregate/, data/1-rai-aggregate/
Expected by Surge.py: ../data/YYYY-MM-DD/[country].csv files
"""

import os
import pandas as pd
from datetime import datetime, date
import shutil

def create_symlink_structure():
    """
    Create symbolic links that mimic the expected folder structure.
    Creates: ../data/YYYY-MM-DD/[country].csv -> actual data files
    """
    today = date.today()
    date_folder = f"../data/{today.strftime('%Y-%m-%d')}"
    
    # Create the date folder structure
    os.makedirs(date_folder, exist_ok=True)
    
    # Method 1: Use individual country files from 1-civic-aggregate + 1-rai-aggregate
    civic_folder = "../data/1-civic-aggregate"
    rai_folder = "../data/1-rai-aggregate"
    
    if os.path.exists(civic_folder):
        print("Creating symlinks from 1-civic-aggregate files...")
        for file in os.listdir(civic_folder):
            if file.endswith('.csv'):
                source = os.path.abspath(os.path.join(civic_folder, file))
                target = os.path.join(date_folder, file)
                
                # Remove existing symlink if it exists
                if os.path.exists(target):
                    os.remove(target)
                
                # Create symlink
                os.symlink(source, target)
                print(f"  Linked: {file}")
    
    print(f"Symlink structure created at: {date_folder}")
    return date_folder

def create_combined_files():
    """
    Alternative approach: Create combined files that merge civic + RAI data for each country.
    This creates files that match what Surge.py expects to find.
    """
    today = date.today()
    date_folder = f"../data/{today.strftime('%Y-%m-%d')}"
    
    # Create the date folder structure
    os.makedirs(date_folder, exist_ok=True)
    
    civic_folder = "../data/1-civic-aggregate"
    rai_folder = "../data/1-rai-aggregate"
    
    if not os.path.exists(civic_folder):
        print(f"Civic folder not found: {civic_folder}")
        return None
    
    # Get list of countries that have civic data
    civic_files = [f for f in os.listdir(civic_folder) if f.endswith('.csv')]
    
    print("Creating combined civic+RAI files...")
    for civic_file in civic_files:
        country = civic_file.replace('.csv', '')
        print(f"Processing {country}...")
        
        # Load civic data
        civic_path = os.path.join(civic_folder, civic_file)
        civic_data = pd.read_csv(civic_path)
        
        # Check if RAI data exists for this country
        rai_path = os.path.join(rai_folder, civic_file)
        if os.path.exists(rai_path):
            print(f"  Found RAI data for {country}")
            rai_data = pd.read_csv(rai_path)
            
            # Merge civic and RAI data
            # RAI data has 'influencer' column, so we need to handle that
            if 'influencer' in rai_data.columns:
                # Get combined influencer data (as used in update_source_entries)
                rai_combined = rai_data[rai_data['influencer'] == 'Combined'].copy()
                rai_combined = rai_combined.drop('influencer', axis=1)
                
                # Merge on country, date
                combined_data = pd.merge(civic_data, rai_combined, 
                                       on=['country', 'date'], 
                                       how='left')  # left join to keep all civic data
            else:
                # RAI data doesn't have influencer column, merge directly
                combined_data = pd.merge(civic_data, rai_data, 
                                       on=['country', 'date'], 
                                       how='left')
        else:
            print(f"  No RAI data found for {country}, using civic only")
            combined_data = civic_data.copy()
        
        # Save combined file
        output_path = os.path.join(date_folder, civic_file)
        combined_data.to_csv(output_path, index=False)
        print(f"  Saved: {output_path}")
    
    print(f"Combined files created at: {date_folder}")
    return date_folder

def update_surge_script():
    """
    Create a modified version of Surge.py with updated paths.
    """
    print("Creating modified Surge.py with updated paths...")
    
    # Read the original Surge.py
    with open('Surge.py', 'r') as f:
        content = f.read()
    
    # Replace path variables
    modifications = [
        ("data_folder = '../data'", "data_folder = './data'"),
        ("result_folder = '../result'", "result_folder = './result'"),
        ("plot_dir = f'../plots2/{date}'", "plot_dir = f'./plots2/{date}'"),
        ("f'../result/{year}-{month}-{day}/peaks'", "f'./result/{year}-{month}-{day}/peaks'"),
        ("f'../result/{year}-{month}-{day}/peaks/{country}.csv'", "f'./result/{year}-{month}-{day}/peaks/{country}.csv'")
    ]
    
    for old, new in modifications:
        content = content.replace(old, new)
    
    # Save modified version
    with open('Surge_modified.py', 'w') as f:
        f.write(content)
    
    print("Modified script saved as: Surge_modified.py")
    print("Key changes:")
    for old, new in modifications:
        print(f"  {old} -> {new}")

def main():
    """
    Main function to set up the data structure for Surge.py
    """
    print("=== Surge.py Path Adaptation ===")
    print()
    
    # Check current directory
    if not os.path.exists('Surge.py'):
        print("ERROR: Surge.py not found in current directory")
        print("Please run this script from the build_data/ directory")
        return
    
    print("Choose adaptation method:")
    print("1. Create symbolic links (recommended)")
    print("2. Create combined files (creates copies)")
    print("3. Create modified Surge.py script")
    print("4. All of the above")
    
    choice = input("Enter choice (1-4): ").strip()
    
    if choice in ['1', '4']:
        try:
            folder = create_symlink_structure()
            print(f"✓ Symbolic links created successfully")
        except Exception as e:
            print(f"✗ Failed to create symbolic links: {e}")
    
    if choice in ['2', '4']:
        try:
            folder = create_combined_files()
            print(f"✓ Combined files created successfully")
        except Exception as e:
            print(f"✗ Failed to create combined files: {e}")
    
    if choice in ['3', '4']:
        try:
            update_surge_script()
            print(f"✓ Modified script created successfully")
        except Exception as e:
            print(f"✗ Failed to create modified script: {e}")
    
    print()
    print("=== Next Steps ===")
    print("1. Verify the data files contain the expected columns")
    print("2. Test run Surge.py (or Surge_modified.py)")
    print("3. Check that the 'data', 'visualization', 'training' modules work correctly")

if __name__ == "__main__":
    main()