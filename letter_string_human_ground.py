import json
import time
import random
import numpy as np
import os
from datetime import datetime

# Transformation type
transform_type = 'counting'
# Options: 'succ', 'pred', 'add_letter', 'remove_redundant', 'counting'

# Load the dataset
with open(r'C:\Users\Nonac\Documents\UvA\Internship\nona code\combined_dataset.json', 'r') as f:
    full_dataset = json.load(f)

print(f"Loaded dataset with {len(full_dataset)} items")

# Select items
num_trials = 50
selected_items = random.sample(full_dataset, num_trials)

# Rule explanations
transformation_explanations = {
    'succ': "Successorship: The last letter changes to the next letter in the alphabet (e.g., abb → abc; moose → moosf; vwxyz → vwxya)",
    'pred': "Predecessorship: The first letter changes to the previous letter in the alphabet (e.g., abb → zbb; moose → loose; vwxyz → uwxyz)",
    'add_letter': "Add a letter: The next letter in the alphabet after the last letter is added (e.g., abb → abbc; moose → moosef; vwxyz → vwxyza)",
    'remove_redundant': "Remove redundant letter: Remove the first duplicated letter (e.g., abb → ab; moose → mose; vwxyz → vwxyz)",
    'counting': "Count letters: Return the number of characters in the sequence (e.g., abb → 3; moose → 5; vwxyz → 5)"
}

# Format sequences for display
def format_sequence(seq):
    return "[" + " ".join(seq) + "]" if isinstance(seq, list) else str(seq)

def check_correctness(response, expected):
    response = response.strip().lower().replace('[', '').replace(']', '').replace(' ', '')

    if isinstance(expected, int):
        # For counting tasks
        try:
            return int(response) == expected
        except ValueError:
            return False
    elif isinstance(expected, list):
        expected_str = ''.join(expected).lower()
        return response == expected_str
    else:
        expected_str = str(expected).strip().lower().replace('[', '').replace(']', '').replace(' ', '')
        return response == expected_str

# Prepare to collect results
all_results = []
correct_answers = 0
incorrect_answers = 0

# Participant ID
participant_id = input("Enter participant ID: ").strip()

# Print task instructions
print(f"\nWelcome! You will perform '{transform_type}' transformations.")
print("Explanation:")
print(transformation_explanations[transform_type])
print("\nType your answer and press Enter.\n")

for i, item in enumerate(selected_items):
    input_data = item['input']
    expected_output = item['transformations'][transform_type]

    input_str = format_sequence(input_data)
    expected_str = format_sequence(expected_output)

    print(f"\n----- Trial {i+1}/{num_trials} -----")
    print(f"Input: {input_str}")

    user_answer = input("Your answer: ").strip()
    is_correct = check_correctness(user_answer, expected_str)

    if is_correct:
        correct_answers += 1
    else:
        incorrect_answers += 1

    result = {
        "trial_num": i+1,
        "participant_id": participant_id,
        "input": input_data,
        "input_str": input_str,
        "expected_output": expected_output,
        "expected_str": expected_str,
        "user_answer": user_answer,
        "is_correct": is_correct,
        "transform_type": transform_type,
        "is_duplicated": item.get('has_duplicates', False),
        "data_type": "word" if isinstance(input_data, str) else "sequence",
        "timestamp": datetime.now().isoformat()
    }

    all_results.append(result)

# Save and report
timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
results_dir = "results"
os.makedirs(results_dir, exist_ok=True)

json_filename = f"{results_dir}/human_transform_{transform_type}_{timestamp}.json"
with open(json_filename, 'w') as f:
    json.dump(all_results, f, indent=2)
print(f"\nResults saved to {json_filename}")

# Save NPZ
npz_filename = f"{results_dir}/human_transform_{transform_type}_{timestamp}.npz"
np.savez(
    npz_filename,
    trial_nums=np.array([r["trial_num"] for r in all_results]),
    correctness=np.array([1 if r["is_correct"] else 0 for r in all_results]),
    data_types=np.array([1 if r["data_type"] == "word" else 0 for r in all_results]),
    has_duplicates=np.array([1 if r["is_duplicated"] else 0 for r in all_results]),
    transform_type=transform_type,
    accuracy=correct_answers / num_trials,
    timestamp=timestamp
)
print(f"Results also saved to {npz_filename}")

print(f"\nTest complete! You got {correct_answers} out of {num_trials} correct.")
