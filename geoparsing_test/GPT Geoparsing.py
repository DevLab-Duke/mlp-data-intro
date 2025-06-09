import pandas as pd
import openai
import datetime

# Path to the Excel file
file_path = r'COL_2012-1_Admin_geoparsing.xlsx'

# Load the Excel file into a DataFrame
df = pd.read_excel(file_path)

print("Excel file loaded successfully.")

# Initialize OpenAI API 
openai.api_key = 'insert_API_KEY'

def extract_locations_from_gpt(text):
    """
    This function takes a text input and prompts GPT to extract the most specific location,
    and return the completed higher-level administrative divisions in a tabular format.
    """
    prompt = f"""
    You are a geographic data extraction expert with advanced knowledge of global administrative divisions, including each country's unique hierarchy (e.g., states, provinces, regions, districts, municipalities). Your task is to extract and categorize locations mentioned in the provided text by identifying the **most specific location** mentioned (e.g., neighborhood, city, or town) and completing the higher-level administrative divisions (admin1, admin2, and country) based on the specific location.
    
    For each event location, return the following information in a tabular format:
    
    1. location: The most specific location mentioned in the text 
    2. location_level: Indicate the highest level of location mentioned (e.g., 'Country', 'ADMIN1', 'ADMIN2', 'Other').
    3. admin_hierarchy: Provide an array detailing the administrative hierarchy. This should include:
        - admin1 (first-level administrative division, if applicable),
        - admin2 (second-level administrative division, if applicable),
        - country (the country where the location is situated).
        
    Ensure that the admin1 and admin2 levels align with the specific administrative structure of the country where the location is situated. Use your knowledge of global administrative divisions to ensure that:
    - If the country has specific admin1 (e.g., states, provinces, regions) and admin2 (e.g., districts, counties) levels, ensure that the extracted admin1 and admin2 levels are valid for the given country.
    - If the text mentions a location (e.g., a city or town) without specifying its admin1 or admin2 level, use the country's administrative hierarchy to infer the correct admin1 and admin2 levels for that location. 
    
    Structure your response as a table with the following columns:

    | Location        | Location Level | Admin1         | Admin2         | Country       |
    |-----------------|----------------|----------------|----------------|---------------|
    | Location Name   | Country/ADMIN1/ADMIN2/Other | Admin1 Name     | Admin2 Name    | Country Name |


    Here is the text for analysis:
    {text}
    """

    print(f"Sending request to GPT for text: {text[:50]}...")

    try:
        
        response = openai.ChatCompletion.create(
            model="gpt-4o-mini",
            messages=[
                {"role": "system", "content": "You are an assistant that extracts geographic locations and fills in missing details in the administrative hierarchy."},
                {"role": "user", "content": prompt}
            ],
            max_tokens=1000,
            temperature=0
        )

        # Get the content of the response
        locations = response['choices'][0]['message']['content'].strip()

        print(f"GPT response received: {locations[:100]}...")

        return locations  # Return the plain-text table response

    except Exception as e:
        print(f"Error processing GPT request: {e}")
        return None

# Initialize empty columns in the DataFrame
df['GPT_Location'] = None
df['GPT_Location_Level'] = None
df['GPT_Admin1'] = None
df['GPT_Admin2'] = None
df['GPT_Country'] = None

# Process only the first 10 rows for testing
#df_test = df.head(10)

# Apply the GPT location extraction function to the first 10 rows in 'maintext_translated'
for index, row in df.iterrows():
    # Check if 'maintext_translated' has valid text before sending to GPT
    if pd.isna(row['maintext_translated']) or not row['maintext_translated'].strip():
        print(f"Skipping empty or invalid text for row {index}.")
        continue
    
    gpt_response = extract_locations_from_gpt(row['maintext_translated'])
    
    # If the response is valid, parse it
    if gpt_response:
        # Split the response into rows (assuming response is a table format)
        lines = gpt_response.split("\n")
        # Skip header row and process remaining lines
        for line in lines[1:]:
            columns = line.split("|")
            if len(columns) >= 5:
                df.loc[index, 'GPT_Location'] = columns[1].strip()
                df.loc[index, 'GPT_Location_Level'] = columns[2].strip()
                df.loc[index, 'GPT_Admin1'] = columns[3].strip()
                df.loc[index, 'GPT_Admin2'] = columns[4].strip()
                df.loc[index, 'GPT_Country'] = columns[5].strip()

# Create a timestamped file name for the output file
timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
output_path = rf'COL_2012-1_Admin_geoparsing_with_GPT_locations_{timestamp}.xlsx'

# Save the updated DataFrame to the new Excel file
df.to_excel(output_path, index=False)

print(f"Excel file saved to {output_path}")
