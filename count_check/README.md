
# Count Validation Instructions:

1. **Familiarize Yourself**
   - Check the Notion page to familiarize yourself with the country pages.

2. **Initial Setup**
   - Connect to the VPN.
   - All scripts are located here on Dropbox: `"\\ML for Peace\\Count review"`
   - Read Hanling's latest scraping report before starting the count check for a source.

3. **Run Initial Count Check**
   - Open and run `Counts_Per_Source_new.ipynb`.
   - Specify the ISO3 country code.
   - This will generate count plots for all sources in that country.
   - Visually inspect the counts in the most recent months (since the last check).
     - The date of the last check is listed on the Notion page under the toggle titled **"Count Review Report"**.
   - **Check for Anomalies**
     - If you notice no counts or any anomalies (e.g., a sudden dip or spike), inspect the news website:
       - Confirm whether this pattern is genuine.
       - Check if Hanling has already flagged the anomaly in their report.

4. **Run Date Coverage Check**
   - Run `source_validation2.0`.
   - Specify the ISO3 country code, month(s), and year.
   - This will output the number of dates captured for each recent month for that source.
   - **Ensure:**
     - All recent months have full or comparable date coverage relative to previous months.
     - Report the number of dates captured for each recent month on Notion.

5. **Run Detailed Validation**
   - Run `source_validation`.
   - Specify the source_domain, month, and year.
   - This script checks multiple aspects; go through each section carefully:
     - **A. Duplicates**
       - Check for duplicate dates, titles, or main_text.
     - **B. Daily Counts**
       - Review article counts per date (sorted from highest to lowest).
       - Look for anomalies:
         - Articles should not be overly clustered on just a few dates.
         - Total number of unique dates should not exceed the number of days in the month.
     - **C. Environmental Counts**
       - There should not be an abnormally high number of counts for any single event.
       - Ideally, there should be very few or no `'nan'` entries.
       - If any category shows a spike, query it in the next section and check the article titles to confirm if the jump is genuine.
     - **D. Civic Space Counts**
       - Same checks as Environmental Counts: avoid anomalies, check for `'nan'`, and confirm spikes by reviewing article titles.
       - Confirm that all the generated titles are in English.
     - *Note: You can skip the RAI and Section Check sections for now.*

6. **Final Steps**
   - Repeat this process for all months of interest for a source.
   - Report your findings on Notion under **"Count Review Reports"**.
   - Mention if the issue has already been flagged in Hanling’s scraping report.
   - Repeat this process for each source.
   - Then repeat for other countries.
   - Post an update in the `#scraping` channel once finished.
