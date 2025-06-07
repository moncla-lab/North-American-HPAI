# Set the root directory to start the search
root_dir = "/Users/lamda/Documents/HPAI/HPAI_2344b/additional-analysis-2025/TTAT-TipRandTest/results/subfiles-shuffles-allanalysis-2025-05-29/orders/prop1"  # <-- Replace this

# Open output file for writing
with open("order_condensed_output.txt", "w") as out_file:
    # Recursively walk through all subdirectories
    for dirpath, dirnames, filenames in os.walk(root_dir):
        if "outfilemcc.tree" in filenames:
            filepath = os.path.join(dirpath, "outfilemcc.tree")
            out_file.write(f"\nProcessing file: {filepath}\n")

            # Load the tree
            tree = dendropy.Tree.get(
                path=filepath,
                schema="nexus",
                preserve_underscores=True
            )

            # Get root node
            root_node = tree.seed_node

            # Extract annotations of interest
            order_condensed_set = None
            order_condensed_prob = None
            for ann in root_node.annotations:
                if ann.name == "order_condensed.set":
                    order_condensed_set = ann.value
                elif ann.name == "order_condensed.set.prob":
                    order_condensed_prob = ann.value

            # Write results to file
            out_file.write(f"  order_condensed.set: {order_condensed_set}\n")
            out_file.write(f"  order_condensed.set.prob: {order_condensed_prob}\n")


with open("order_condensed_output.txt") as f:
    lines = f.readlines()

records = []
current_replicate = None
current_order_condensed_order = None

# Regex for floating numbers including scientific notation
float_re = re.compile(r"[-+]?\d*\.\d+(?:[Ee][-+]?\d+)?|[-+]?\d+(?:[Ee][-+]?\d+)?")

for line in lines:
    line = line.strip()

    if line.startswith("Processing file:"):
        match = re.search(r"replicate_(\d+)", line)
        if match:
            current_replicate = match.group(1)

    elif line.startswith("order_condensed.set:"):
        current_order_condensed_order = re.findall(r'"(.*?)"', line)

    elif line.startswith("order_condensed.set.prob:"):
        probs = float_re.findall(line)
        if current_order_condensed_order and probs:
            record = {"replicate": int(current_replicate)}
            for order_condensed, prob in zip(current_order_condensed_order, probs):
                record[order_condensed] = float(prob)
            records.append(record)

df = pd.DataFrame(records)
df = df.sort_values("replicate").reset_index(drop=True)
df.to_csv("order_condensed_probabilities.csv", index=False)

print(df.head())

# Melt to long format
df_long = df.melt(id_vars="replicate", var_name="order_condensed", value_name="probability")

# Set font globally to Arial and increase size
plt.rcParams.update({
    "font.size": 18,
    "font.family": "Arial"
})

plt.figure(figsize=(10, 6))

# Remove grid lines by setting style to 'white' or disable grid
sns.set_style("white")  # white background with no grid

# Create the stripplot with green dots and jitter
sns.stripplot(data=df_long, x="order_condensed", y="probability", jitter=True, alpha=0.7, color="green")

plt.title("order_condensed Probability for Shuffled DTA states by Replicate", fontsize=16)
plt.ylabel("Probability", fontsize=14)
plt.xlabel("order_condensed", fontsize=14)
plt.xticks(rotation=45)

plt.tight_layout()

# Save plot to PDF
plt.savefig("order_condensed_probabilities_by_replicate.pdf", format='pdf')

plt.show()


(df.drop(columns="replicate") > 0.5).sum() / len(df)
(df.drop(columns="replicate")).sum() / len(df)