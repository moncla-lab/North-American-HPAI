import re
import sys
import pandas as pd

# LD 2025-03-26
# this script takes in a BEAST nexus file for a posterior set of trees and formats it for use with the BaTs program written by Joe Parker
# usage: BaTs-nexusformat.py input.nex output.nex mapping.csv


def process_nexus_file(nexus_file, output_file, sub_df):
    with open(nexus_file, "r") as f:
        lines = f.readlines()

    in_taxa_block = False
    in_states_block = False
    in_trees_block = False
    found_tree_state = False
    states_section = []
    trees_section = []
    final_output = []

    # Phase 1: Replace taxa block with "begin states;"
    for i, line in enumerate(lines):
        lower_line = line.lower()

        if "begin taxa;" in lower_line:
            in_taxa_block = True
            final_output.append("begin states;\n")
            continue

        if in_taxa_block and "translate" in lower_line:
            in_taxa_block = False
            in_states_block = True
            continue

        if in_taxa_block:
            continue

        if in_states_block:
            if "tree state" in lower_line and not found_tree_state:
                in_states_block = False
                in_trees_block = True
                found_tree_state = True
            else:
                # Clean commas, tabs, semicolons
                line = line.replace(",", "").replace("	", "")
                line = line.replace(";", "End;\n\nbegin trees;")
                states_section.append(line)
                continue

        if in_trees_block:
            # Keep raw for now — we'll clean annotations after all replacements
            trees_section.append(line)
        else:
            final_output.append(line)

    # Phase 2: Combine all sections
    final_output += states_section + trees_section

    # Phase 3: Apply replacements from sub_df
    if sub_df is not None:
        replacement_dict = dict(zip(sub_df.iloc[:, 0], sub_df.iloc[:, 1]))
        final_output = [multi_replace(line, replacement_dict) for line in final_output]

    # Phase 4: Remove annotations inside square brackets from tree lines
    cleaned_lines = []
    for line in final_output:
        if "tree " in line:
            # Remove square bracket annotations
            line = re.sub(r'\[.*?\]', '', line)
            # Replace '=  ' with '= [&R] '
            line = re.sub(r"=  ", "= [&R] ", line)
        cleaned_lines.append(line)

    # Write the final output
    with open(output_file, "w") as f:
        f.writelines(cleaned_lines)

    print(f"Processed and cleaned file saved as {output_file}")

def multi_replace(text, replace_dict):
    for key, val in replace_dict.items():
        text = text.replace(key, val)
    return text

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Usage: python process_nexus.py <input.nexus> <output.nexus> <substitutions.csv>")
        sys.exit(1)

    input_nexus = sys.argv[1]
    output_nexus = sys.argv[2]
    substitutions_csv = sys.argv[3]

    sub_df = pd.read_csv(substitutions_csv, header=None)
    process_nexus_file(input_nexus, output_nexus, sub_df)
