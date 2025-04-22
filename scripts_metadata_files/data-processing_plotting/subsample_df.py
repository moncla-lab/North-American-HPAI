import pandas as pd
import argparse
import os


# subsample df for discrete values of a given column for a specfic number of each discrete value
# usage: python subsample_df.py data.csv 10 category_column output.csv
def subsample_csv(input_csv, n, group_col, output_csv):
    # Read input file
    df = pd.read_csv(input_csv)

    if group_col not in df.columns:
        raise ValueError(f"Column '{group_col}' not found in the input CSV.")

    # Perform subsampling
    sampled_dfs = []
    for value, group_df in df.groupby(group_col):
        if len(group_df) < n:
            print(f"Warning: Only {len(group_df)} rows available for '{value}' (requested {n}). Sampling all.")
            sampled = group_df
        else:
            sampled = group_df.sample(n=n, random_state=42)
        sampled_dfs.append(sampled)

    # Concatenate all samples and write to output
    result_df = pd.concat(sampled_dfs)
    result_df.to_csv(output_csv, index=False)
    print(f"Subsampled data written to '{output_csv}'.")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Subsample rows from a CSV for each group in a specified column.")
    parser.add_argument("input_csv", help="Path to the input CSV file")
    parser.add_argument("n", type=int, help="Number of rows to sample per group")
    parser.add_argument("group_col", help="Name of the column to group by")
    parser.add_argument("output_csv", help="Path to the output CSV file")

    args = parser.parse_args()
    subsample_csv(args.input_csv, args.n, args.group_col, args.output_csv)
