import xml.etree.ElementTree as ET
import random
import argparse
import os

# LD 2026-06-07
# this script takes an input XML and creates 100 XMLS with the discrete traits shuffled across tips to assess the affect of bias in the sampling of discrete traits.
def shuffle_attr_elements(input_file, output_file):
    # Parse XML
    tree = ET.parse(input_file)
    root = tree.getroot()

    # Find the <taxa> section
    taxa_elem = root.find(".//taxa[@id='taxa']")
    if taxa_elem is None:
        raise ValueError("No <taxa id='taxa'> section found in the XML.")

    # Get all <taxon> elements
    taxon_elements = list(taxa_elem.findall("taxon"))

    # Extract all <attr> elements
    attr_elements = []
    for taxon in taxon_elements:
        attr = taxon.find("attr")
        if attr is not None:
            attr_elements.append(ET.fromstring(ET.tostring(attr)))

    # Shuffle the <attr> elements
    random.shuffle(attr_elements)

    # Reassign shuffled <attr> elements to the original <taxon> elements
    for taxon, new_attr in zip(taxon_elements, attr_elements):
        old_attr = taxon.find("attr")
        if old_attr is not None:
            taxon.remove(old_attr)
        taxon.append(new_attr)

    # Write modified XML to output file
    tree.write(output_file, encoding="utf-8", xml_declaration=True)

def generate_sub_script(xml_filename, output_path):
    script_contents = f"""#!/bin/bash
#BSUB -W 96:00 # walltime of 72 hours
#BSUB -J H5_shufflerandtiptiest # job name
#BSUB -o H5_shufflerandtiptiest.out
#BSUB -e H5_shufflerandtiptiest.err
#BSUB -M 20240
#BSUB -N

module load gcc/8.5.0 
module load openjdk-1.8.0
module load beast/1.10.4
module load beagle/4.0.0

beast -beagle -beagle_SSE {xml_filename}
"""
    with open(os.path.join(output_path, "sub.sh"), "w") as f:
        f.write(script_contents)

def main(input_xml, num_replicates):
    base_name = os.path.splitext(os.path.basename(input_xml))[0]

    for i in range(1, num_replicates + 1):
        replicate_dir = f"{base_name}_replicate_{i:03}"
        os.makedirs(replicate_dir, exist_ok=True)

        output_xml_name = f"{base_name}-{i:03}.xml"
        output_xml_path = os.path.join(replicate_dir, output_xml_name)

        shuffle_attr_elements(input_xml, output_xml_path)
        generate_sub_script(output_xml_name, replicate_dir)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Shuffle <attr> values in an XML <taxa> section multiple times.")
    parser.add_argument("input_xml", help="Path to the input XML file")
    parser.add_argument("--replicates", type=int, default=100, help="Number of replicates (default: 100)")

    args = parser.parse_args()
    main(args.input_xml, args.replicates)
