import time
import pandas as pd
from pytrends.request import TrendReq
from pytrends.exceptions import TooManyRequestsError

pytrend = TrendReq()
print(1)

# Define Keywords (must be in chunks of 5 or fewer per Google API, had issues with larger)
keywords_chunk1 = ['bean-to-cup machine', 'coffee brewer', 'coffee extract', 'cold brew', 'espresso']
keywords_chunk2 = ['french press', 'instant coffee', 'keurig', 'pour over']
print(2)

# Function to fetch interest by region with retry and delay
def get_interest_by_region(keywords):
    while True:
        try:
            pytrend.build_payload(
                kw_list=keywords,
                timeframe='today 12-m',  # Last 12 months
                geo='US',               
                gprop=''                
            )
            time.sleep(10)  # Add a delay of 10 seconds between requests to deal with 429 errors from Google
            return pytrend.interest_by_region(resolution='REGION')
        except TooManyRequestsError:
            print("Too many requests. Retrying in 30 seconds...")
            time.sleep(30)  # Wait 30 seconds before retrying to further deal with 429s
        except Exception as e:
            print(f"An unexpected error occurred: {e}")
            break

print(3)

# Fetch data for each chunk
interest_by_region1 = get_interest_by_region(keywords_chunk1)
print(4)
interest_by_region2 = get_interest_by_region(keywords_chunk2)

print(5)
# Combine results
interest_by_region_combined = pd.concat([interest_by_region1, interest_by_region2], axis=1)

print("\nInterest By Region (Combined):")
print(interest_by_region_combined.head(10))
interest_by_region_combined.to_excel('coffee_output_2.xlsx')