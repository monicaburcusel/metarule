import json
import time
import random
import numpy as np
import os
import csv
from datetime import datetime

# Question/rule type to be tested
question_type = 'comp'
# Options: 'cat', 'func', 'ant', 'syn', 'comp'

QUESTION_TYPES = {
    'cat': {
        'name': 'Category',
        'question': "What category does this belong to?",
        'column': 'cat_answer',
        'explanation': "Category questions ask about the broader category the term belongs to. (e.g., Moose -> Animal; Persimmon -> Fruit; Stethoscope -> Medical Device)"
    },
    'func': {
        'name': 'Function',
        'question': "What function does this serve or what does it do?",
        'column': 'Func_answer',
        'explanation': "Function questions ask about the action or purpose associated with the term. (e.g., Moose -> Graze; Persimmon -> Eat; Stethoscope -> Listen)."
    },
    'ant': {
        'name': 'Antonym',
        'question': "What is the opposite of this?",
        'column': 'ant_answer',
        'explanation': "Antonym questions ask about the opposite or contrasting concept. (e.g., Moose -> Mouse; Persimmon -> Vinegar; Stethoscope -> Earplug)"
    },
    'syn': {
        'name': 'Synonym',
        'question': "What is similar to this?",
        'column': 'syn_answer',
        'explanation': "Synonym questions ask about what is similar to or can substitute for the term. (e.g., Moose -> Elk; Persimmon -> Kaki; Stethoscope -> Phonendoscope)"
    },
    'comp': {
        'name': 'Compositional',
        'question': "What larger structure contains this item, what is it composed of, or what is it a key ingredient of?",
        'column': 'comp_answer',
        'explanation': "Compositional questions ask about what larger structure contains this item, what is it composed of, or what is it a key ingredient of. (e.g., Moose -> Herd; Persimmon -> Tart; Stethoscope -> Hospital)"
    }
}

def load_data(file_path):
    data = []
    encodings_to_try = ['latin1', 'utf-8', 'cp1252', 'iso-8859-1']
    
    for encoding in encodings_to_try:
        try:
            with open(file_path, 'r', encoding=encoding) as f:
                reader = csv.DictReader(f, delimiter=';')
                for row in reader:
                    clean_row = {key.replace('\ufeff', ''): value for key, value in row.items()}
                    if 'term' in clean_row and clean_row['term'].strip():
                        data.append(clean_row)
            print(f"Successfully loaded {len(data)} items from CSV using {encoding} encoding")
            break
        except UnicodeDecodeError:
            print(f"Failed to read with {encoding} encoding, trying next...")
            continue
        except Exception as e:
            print(f"Error reading CSV with {encoding}: {e}")
            if encoding == encodings_to_try[-1]: 
                raise
    
    if not data:
        raise Exception("Could not read CSV file with any supported encoding")
    
    return data

# Update this to your file location
csv_file_path = r'C:\Users\Nonac\Documents\UvA\Internship\nona code\analogy_items.csv'

analogy_items = load_data(csv_file_path)
print(f"Loaded dataset with {len(analogy_items)} items")

# Setting number of trials
num_trials = min(50, len(analogy_items))
selected_indices = random.sample(range(len(analogy_items)), num_trials)
selected_items = [analogy_items[i] for i in selected_indices]

def prepare_question(item, q_type):
    column_name = QUESTION_TYPES[q_type]['column']
    correct_answer = item[column_name].strip()
    all_answers = [
        item['cat_answer'].strip(),
        item['Func_answer'].strip(),
        item['ant_answer'].strip(),
        item['syn_answer'].strip(),
        item['comp_answer'].strip()
    ]
    random.shuffle(all_answers)
    correct_index = all_answers.index(correct_answer)
    return {
        'term': item['term'].strip(),
        'question_type': q_type,
        'question_text': QUESTION_TYPES[q_type]['question'],
        'question_name': QUESTION_TYPES[q_type]['name'],
        'answers': all_answers,
        'correct_answer': correct_answer,
        'correct_index': correct_index
    }

def get_few_shot_examples():
    moose_options = ["Mouse", "Animal", "Herd", "Elk", "Graze"]
    persimmon_options = ["Fruit", "Tart", "Vinegar", "Kaki", "Eat"]

    def format_block(term, question, options, correct):
        letter = chr(65 + options.index(correct))
        opts_str = "\n".join([f"{chr(65+i)}. {opt}" for i, opt in enumerate(options)])
        return f"Term: {term}\nQuestion: {question}\n{opts_str}\nAnswer: {letter}"

    examples = [
        format_block("Moose", "What category does this belong to?", moose_options, "Animal"),
        format_block("Moose", "What function does this serve or what does it do?", moose_options, "Graze"),
        format_block("Moose", "What is the opposite of this?", moose_options, "Mouse"),
        format_block("Moose", "What is similar to this?", moose_options, "Elk"),
        format_block("Moose", "What larger structure contains this item, what is it composed of, or what is it a key ingredient of?", moose_options, "Herd"),
        format_block("Persimmon", "What category does this belong to?", persimmon_options, "Fruit"),
        format_block("Persimmon", "What function does this serve or what does it do?", persimmon_options, "Eat"),
        format_block("Persimmon", "What is the opposite of this?", persimmon_options, "Vinegar"),
        format_block("Persimmon", "What is similar to this?", persimmon_options, "Kaki"),
        format_block("Persimmon", "What larger structure contains this item, what is it composed of, or what is it a key ingredient of?", persimmon_options, "Tart")
    ]
    return "\n\n".join(examples)

def check_correctness(response, correct_letter):
    return response.strip().upper() == correct_letter

all_results = []
correct_answers = 0
incorrect_answers = 0

# Creating results directory
results_dir = os.path.join(os.getcwd(), "results")
if not os.path.exists(results_dir):
    os.makedirs(results_dir)

print(f"\nTesting '{QUESTION_TYPES[question_type]['name']}' questions on {num_trials} randomly selected items...\n")
print(f"EXPLANATION: {QUESTION_TYPES[question_type]['explanation']}\n")

few_shot_text = get_few_shot_examples()
print("=== FEW-SHOT EXAMPLES ===")
print(few_shot_text)
print("=========================\n")

for i, item in enumerate(selected_items):
    question_data = prepare_question(item, question_type)
    term = question_data['term']
    question_text = question_data['question_text']
    answers = question_data['answers']
    correct_answer = question_data['correct_answer']
    correct_index = question_data['correct_index']
    correct_letter = chr(65 + correct_index)
    formatted_answers = "\n".join([f"{chr(65+j)}. {answer}" for j, answer in enumerate(answers)])

    print(f"\n----- Trial {i+1}/{num_trials} -----")
    print(f"Term: {term}")
    print(f"Question: {question_text}")
    print(f"Options:\n{formatted_answers}")

    user_response = input("Your answer (A/B/C/D/E): ").strip().upper()
    is_correct = check_correctness(user_response, correct_letter)

    result = {
        "trial_num": i+1,
        "term": term,
        "question_type": question_type,
        "question_name": QUESTION_TYPES[question_type]['name'],
        "options": answers,
        "correct_answer": correct_answer,
        "correct_letter": correct_letter,
        "user_response": user_response,
        "is_correct": is_correct,
        "timestamp": datetime.now().isoformat()
    }

    all_results.append(result)
    if is_correct:
        correct_answers += 1
    else:
        incorrect_answers += 1

    time.sleep(0.5)

print("\n===== Results =====")
print(f"Total trials: {num_trials}")
print(f"Correct answers: {correct_answers} ({correct_answers/num_trials*100:.1f}%)")
print(f"Incorrect answers: {incorrect_answers} ({incorrect_answers/num_trials*100:.1f}%)")

timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
json_filename = os.path.join(results_dir, f"human_analogy_responses_{question_type}_{timestamp}.json")
with open(json_filename, 'w', encoding='utf-8') as f:
    json.dump(all_results, f, indent=2, ensure_ascii=False)
print(f"Results saved to {json_filename}")

npz_filename = os.path.join(results_dir, f"human_analogy_responses_{question_type}_{timestamp}.npz")
trial_nums = np.array([r["trial_num"] for r in all_results])
correctness = np.array([1 if r["is_correct"] else 0 for r in all_results])
np.savez(
    npz_filename,
    trial_nums=trial_nums,
    correctness=correctness,
    question_type=question_type,
    accuracy=correct_answers/num_trials,
    timestamp=timestamp
)
print(f"Results also saved to {npz_filename}")

print("\nDone testing!")