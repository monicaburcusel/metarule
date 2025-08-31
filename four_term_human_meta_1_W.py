import json
import time
import random
import numpy as np
import os
import csv
from datetime import datetime

# Trigger words
ORDINAL_WORDS = ["first", "second", "third", "fourth", "fifth"]

# Question/transformation/rule types
QUESTION_TYPES = {
    'cat': {
        'name': 'Category',
        'question': "What category does this belong to?",
        'column': 'cat_answer',
        'rule_number': 1
    },
    'func': {
        'name': 'Function', 
        'question': "What function does this serve or what does it do?",
        'column': 'Func_answer',
        'rule_number': 2
    },
    'ant': {
        'name': 'Antonym',
        'question': "What is the opposite of this?", 
        'column': 'ant_answer',
        'rule_number': 3
    },
    'syn': {
        'name': 'Synonym',
        'question': "What is similar to this?",
        'column': 'syn_answer',
        'rule_number': 4
    },
    'comp': {
        'name': 'Compositional',
        'question': "What larger structure contains this item, what is it composed of, or what is it a key ingredient of?",
        'column': 'comp_answer',
        'rule_number': 5
    }
}

# Answers file
META_ANSWERS_FILE = 'meta_rule_answers.json'

def create_instructions():
    """Create the game instructions with all rule explanations"""
    instructions = """**Rule explanations:**
**Rule 1 (Category):** What broader category does the term belong to? (e.g., Moose -> Animal; Persimmon -> Fruit; Stethoscope -> Medical Device)
**Rule 2 (Function):** What action or purpose is associated with the term? (e.g., Moose -> Graze; Persimmon -> Eat; Stethoscope -> Listen)
**Rule 3 (Antonym):** What is the opposite or contrasting concept? (e.g., Moose -> Mouse; Persimmon -> Vinegar; Stethoscope -> Earplug)
**Rule 4 (Synonym):** What is similar to or can substitute for the term? (e.g., Moose -> Elk; Persimmon -> Kaki; Stethoscope -> Phonendoscope)
**Rule 5 (Compositional):** What larger structure contains this item, what is it composed of, or what is it a key ingredient of? (e.g., Moose -> Herd; Persimmon -> Tart; Stethoscope -> Hospital)

The base rule of the game is rule 1. Unless otherwise specified, this is the rule you apply.
**Base Rule:** Use **Rule 1** as the default unless instructed otherwise by the meta-rules.
**Meta rule 1**: When you encounter an **ordinal number**, **switch the base rule** to the corresponding numbered rule ("first" apply rule 1, "second" apply rule 2, "third" apply rule 3, "fourth" apply rule 4, "fifth" apply rule 5). Apply this new rule from that point onward until another ordinal number appears, at which point you update the base rule again.

Answer with the letter of the correct option (A, B, C, D, or E)."""
    
    return instructions

def load_meta_answers():
    """Load predefined answers for ordinal words under different rules"""
    try:
        with open(META_ANSWERS_FILE, 'r', encoding='utf-8') as f:
            return json.load(f)
    except FileNotFoundError:
        # Return your specific answers if file doesn't exist
        return {
            "first": {"cat": "Order", "func": "Cheer", "ant": "Last", "syn": "One", "comp": "Competition"},
            "second": {"cat": "Order", "func": "Settle", "ant": "Second last", "syn": "Two", "comp": "Competition"}, 
            "third": {"cat": "Order", "func": "Finish", "ant": "Third last", "syn": "Three", "comp": "Competition"},
            "fourth": {"cat": "Order", "func": "Place", "ant": "Fourth last", "syn": "Four", "comp": "Competition"},
            "fifth": {"cat": "Order", "func": "Obtain", "ant": "Fifth last", "syn": "Five", "comp": "Competition"}
        }

def load_data(file_path):
    """Load the word items from CSV with fallback encoding options"""
    data = []
    encodings_to_try = ['latin1', 'utf-8', 'cp1252', 'iso-8859-1']
    
    for encoding in encodings_to_try:
        try:
            with open(file_path, 'r', encoding=encoding) as f:
                reader = csv.DictReader(f, delimiter=';')
                for row in reader:
                    clean_row = {}
                    for key, value in row.items():
                        clean_key = key.replace('\ufeff', '')
                        clean_row[clean_key] = value
                    
                    if 'term' in clean_row and clean_row['term'].strip():
                        data.append(clean_row)
            print(f"Successfully loaded {len(data)} items from CSV using {encoding} encoding")
            break
        except UnicodeDecodeError:
            print(f"Failed to read with {encoding} encoding, trying next...")
            continue
        except Exception as e:
            print(f"Error reading CSV with {encoding}: {e}")
            if encoding == encodings_to_try[-1]:  # Last encoding to try
                raise
    
    if not data:
        raise Exception("Could not read CSV file with any supported encoding")
    
    return data

def insert_ordinal_word(term, probability=0.1):
    """Randomly replace the term with 1 ordinal word with given probability"""
    if random.random() > probability:
        return term, []
    
    # Selecting 1 ordinal word to replace the entire term
    selected_word = random.choice(ORDINAL_WORDS)
    
    return selected_word, [selected_word]

def determine_active_rule(inserted_words, current_rule):
    """Determine which rule should be active based on inserted ordinal words"""
    if not inserted_words:
        return current_rule
    
    # Using the last ordinal word to determine the rule
    rule_mapping = {
        "first": "cat",    # Rule 1
        "second": "func",  # Rule 2
        "third": "ant",    # Rule 3
        "fourth": "syn",   # Rule 4
        "fifth": "comp"    # Rule 5
    }
    
    return rule_mapping.get(inserted_words[-1], current_rule)

def get_correct_answer_for_ordinal(word, rule_type, meta_answers):
    """Get the correct answer for an ordinal word under a specific rule"""
    if word in meta_answers and rule_type in meta_answers[word]:
        return meta_answers[word][rule_type]
    
    
    fallback = {
        "first": {"cat": "Order", "func": "Cheer", "ant": "Last", "syn": "One", "comp": "Competition"},
        "second": {"cat": "Order", "func": "Settle", "ant": "Second last", "syn": "Two", "comp": "Competition"},
        "third": {"cat": "Order", "func": "Finish", "ant": "Third last", "syn": "Three", "comp": "Competition"},
        "fourth": {"cat": "Order", "func": "Place", "ant": "Fourth last", "syn": "Four", "comp": "Competition"},
        "fifth": {"cat": "Order", "func": "Obtain", "ant": "Fifth last", "syn": "Five", "comp": "Competition"}
    }
    
    if word in fallback and rule_type in fallback[word]:
        return fallback[word][rule_type]
    
    return word.capitalize()

def prepare_question(item, q_type, meta_answers, inserted_words=None):
    """Prepare a question, handling both regular terms and ordinal words"""
    if inserted_words:
        # For ordinal words, we're asking about the ordinal word itself
        term = inserted_words[0]  # Use the ordinal word as the term
        
        # Get the correct answer for the ordinal word under the active rule
        correct_answer = get_correct_answer_for_ordinal(inserted_words[0], q_type, meta_answers)
        
        # Create answer options using meta-answers for ordinal words
        all_ordinal_answers = []
        for rule in ['cat', 'func', 'ant', 'syn', 'comp']:
            answer = get_correct_answer_for_ordinal(inserted_words[0], rule, meta_answers)
            if answer not in all_ordinal_answers:
                all_ordinal_answers.append(answer)
        
        # Mix in some regular answers to create distractors
        regular_answers = [
            item['cat_answer'].strip(),
            item['Func_answer'].strip(),
            item['ant_answer'].strip(), 
            item['syn_answer'].strip(),
            item['comp_answer'].strip()
        ]
        
        # Combine ordinal answers with some regular answers as distractors
        all_answers = all_ordinal_answers.copy()
        for ans in regular_answers:
            if ans not in all_answers and len(all_answers) < 5:
                all_answers.append(ans)
        
        # Ensure we have exactly 5 options
        while len(all_answers) < 5:
            all_answers.append(f"Option{len(all_answers)+1}")
        
        # Ensure correct answer is in the list
        if correct_answer not in all_answers:
            all_answers[0] = correct_answer
        
    else:
        # Regular processing for normal terms
        term = item['term'].strip()
        column_name = QUESTION_TYPES[q_type]['column']
        correct_answer = item[column_name].strip()
        
        all_answers = [
            item['cat_answer'].strip(),
            item['Func_answer'].strip(),
            item['ant_answer'].strip(), 
            item['syn_answer'].strip(),
            item['comp_answer'].strip()
        ]
    
    # Remove duplicates while preserving order
    unique_answers = []
    for ans in all_answers:
        if ans not in unique_answers:
            unique_answers.append(ans)
    
    # Take only the first 5 if we have more
    unique_answers = unique_answers[:5]
    
    # Shuffle the answers
    random.shuffle(unique_answers)
    correct_index = unique_answers.index(correct_answer)
    
    return {
        'term': term,
        'question_type': q_type,
        'question_text': QUESTION_TYPES[q_type]['question'],
        'question_name': QUESTION_TYPES[q_type]['name'],
        'rule_number': QUESTION_TYPES[q_type]['rule_number'],
        'answers': unique_answers,
        'correct_answer': correct_answer,
        'correct_index': correct_index,
        'has_ordinal': bool(inserted_words),
        'inserted_words': inserted_words or []
    }

def check_correctness(response, correct_letter):
    """Check if the response contains the correct letter"""
    response = response.strip().upper()
    return response == correct_letter or response.startswith(correct_letter + '.')

def main():
    # Windows-compatible file path - updated to your actual file location
    csv_path = r'C:\Users\Nonac\Documents\UvA\Internship\nona code\analogy_items.csv'
    
    # Load data and meta answers
    try:
        word_items = load_data(csv_path)
    except FileNotFoundError:
        print(f"Error: Could not find analogy_items.csv at {csv_path}")
        print("Please make sure the CSV file exists and update the path if needed.")
        return
    
    meta_answers = load_meta_answers()
    
    print("="*60)
    print("WORD ASSOCIATION CHALLENGE")
    print("="*60)
    
    # Show game instructions
    print(create_instructions())
    print("\n" + "="*60)
    
    # Get participant info
    participant_name = input("Enter your name or ID: ").strip()
    if not participant_name:
        participant_name = f"participant_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
    
    # Test configuration
    base_rule = 'cat'  # Default rule (Rule 1)
    current_rule = base_rule
    num_trials = min(250, len(word_items))  # Increased for better testing
    
    print(f"\nWelcome, {participant_name}!")
    print(f"Starting with Rule 1 (Category) as the default.")
    print("Press Enter when ready to begin...")
    input()
    
    # Results tracking
    all_results = []
    
    for i in range(num_trials):
        # Select random item
        item = random.choice(word_items)
        
        # Maybe insert ordinal words
        original_term = item['term'].strip()
        modified_term, inserted_words = insert_ordinal_word(original_term)
        
        # Update current rule based on ordinal words
        if inserted_words:
            current_rule = determine_active_rule(inserted_words, current_rule)
            rule_num = QUESTION_TYPES[current_rule]['rule_number']
        
        # Prepare question
        question_data = prepare_question(item, current_rule, meta_answers, inserted_words)
        
        term = question_data['term']
        question_text = question_data['question_text']
        answers = question_data['answers']
        correct_answer = question_data['correct_answer']
        correct_index = question_data['correct_index']
        correct_letter = chr(65 + correct_index)
        rule_number = question_data['rule_number']
        
        # Format answers
        formatted_answers = "\n".join([f"{chr(65+j)}. {answer}" for j, answer in enumerate(answers)])
        
        print(f"\n" + "-"*50)
        print(f"QUESTION {i+1}/{num_trials}")
        print(f"Word: {term}")
        print(f"Options:")
        print(formatted_answers)
        
        # Get response
        start_time = time.time()
        human_answer = input("Your answer (A, B, C, D, or E): ").strip().upper()
        end_time = time.time()
        response_time = end_time - start_time
        
        # Checking correctness but without feedback
        is_correct = check_correctness(human_answer, correct_letter)
        
        print("Answer recorded.")
        
        # Results
        result = {
            "participant_name": participant_name,
            "trial_num": i+1,
            "original_term": original_term,
            "modified_term": term,
            "inserted_words": inserted_words,
            "active_rule": current_rule,
            "rule_number": rule_number,
            "question_type": current_rule,
            "question_name": question_data['question_name'],
            "question_text": question_text,
            "options": answers,
            "correct_answer": correct_answer,
            "correct_letter": correct_letter,
            "user_response": human_answer,
            "response_time": response_time,
            "is_correct": is_correct,
            "timestamp": datetime.now().isoformat()
        }
        
        all_results.append(result)
        
        # Show progress every 10 trials (without accuracy info)
        if (i + 1) % 10 == 0:
            print(f"\nProgress: {i+1}/{num_trials} completed")
    
    # Calculate final results
    correct_answers = sum(1 for r in all_results if r["is_correct"])
    incorrect_answers = num_trials - correct_answers
    
    # Print final results
    print("\n" + "="*60)
    print("GAME COMPLETE - FINAL RESULTS")
    print("="*60)
    print(f"Participant: {participant_name}")
    print(f"Total questions: {num_trials}")
    print(f"Correct answers: {correct_answers} ({correct_answers/num_trials*100:.1f}%)")
    print(f"Incorrect answers: {incorrect_answers} ({incorrect_answers/num_trials*100:.1f}%)")
    
    # Optionally show detailed results
    show_details = input("\nWould you like to see the correct answers for questions you got wrong? (y/n): ").strip().lower()
    if show_details == 'y':
        print("\nQuestions you got wrong:")
        for result in all_results:
            if not result["is_correct"]:
                print(f"Q{result['trial_num']}: {result['modified_term']} - Correct answer was {result['correct_letter']}. {result['correct_answer']}")
    
    # Save results
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    results_dir = os.path.join(os.getcwd(), "results")
    os.makedirs(results_dir, exist_ok=True)
    
    # Save JSON
    json_filename = os.path.join(results_dir, f"human_word_game_{participant_name}_{timestamp}.json")
    with open(json_filename, 'w', encoding='utf-8') as f:
        json.dump(all_results, f, indent=2, ensure_ascii=False)
    print(f"\nResults saved to {json_filename}")
    
    # Save NPZ
    npz_filename = os.path.join(results_dir, f"human_word_game_{participant_name}_{timestamp}.npz")
    np.savez(
        npz_filename,
        participant_name=participant_name,
        trial_nums=np.array([r["trial_num"] for r in all_results]),
        correctness=np.array([1 if r["is_correct"] else 0 for r in all_results]),
        response_times=np.array([r["response_time"] for r in all_results]),
        has_ordinals=np.array([1 if r["inserted_words"] else 0 for r in all_results]),
        accuracy=correct_answers/num_trials,
        timestamp=timestamp
    )
    print(f"Summary saved to {npz_filename}")
    print(f"\nThanks for playing, {participant_name}!")

if __name__ == "__main__":
    main()