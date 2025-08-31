import json
import time
import random
import numpy as np
import os
from datetime import datetime

# Ordinal/trigger words
ORDINAL_WORDS = ["first", "second", "third", "fourth", "fifth"]

# Transformation types
TRANSFORM_TYPES = {
    'succ': {
        'name': 'Successorship',
        'description': 'The last letter changes to the next letter in the alphabet',
        'rule_number': 1
    },
    'pred': {
        'name': 'Predecessorship', 
        'description': 'The first letter changes to the previous letter in the alphabet',
        'rule_number': 2
    },
    'add_letter': {
        'name': 'Adding a letter',
        'description': 'The next letter in the alphabet after the last letter is added to the end',
        'rule_number': 3
    },
    'remove_redundant': {
        'name': 'Removing redundant character',
        'description': 'If there are duplicate letters, the first duplicate found is removed',
        'rule_number': 4
    },
    'counting': {
        'name': 'Counting',
        'description': 'Count the number of letters in the sequence',
        'rule_number': 5
    }
}

 #Meta-rule answer file for letter-string analogies (alphabet/counting game)
ALPHABET_META_ANSWERS_FILE = 'alphabet_meta_answers.json'

def create_system_prompt():
    """Create the system prompt with all rule explanations and meta-rules"""
    prompt = """**Rule explanations:**
**Rule Explanations:
Rule 1 (Successorship): The last letter changes to the next letter in the alphabet (e.g., abb -> abc; moose -> moosf; vwxyz -> vwxya)
Rule 2 (Predecessorship): The first letter changes to the previous letter in the alphabet (e.g., abb -> zbb; moose -> loose; vwxyz -> uwxyz)  
Rule 3 (Adding a letter): The next letter in the alphabet after the last letter is added to the end of the sequence (e.g., abb -> abbc; moose -> moosef; vwxyz -> vwxyza)
Rule 4 (Removing redundant character): If there are duplicate letters, the first duplicate found is removed (e.g., abb -> ab; moose -> mose; vwxyz -> vwxyz)
Rule 5 (Counting): Count the number of letters in the sequence (e.g., abb -> 3; moose -> 5; vwxyz -> 5)
Base Rule: Use Rule 1 as the default unless instructed otherwise by the meta-rules.
Meta rule 1: When you encounter an ordinal number, switch the base rule to the corresponding numbered rule ("first" apply rule 1). Apply this new rule from that point onward until another ordinal number appears, at which point you update the base rule again. (e.g., "second" switch to rule 2, "third" switch to rule 3)."""
    
    return prompt

def load_alphabet_meta_answers():
    """Load predefined transformations for ordinal words under different rules"""
    try:
        with open(ALPHABET_META_ANSWERS_FILE, 'r') as f:
            return json.load(f)
    except FileNotFoundError:
        # Return default transformations if file doesn't exist
        return {
            "first": {
                "succ": "firsu",
                "pred": "eirst",
                "add_letter": "firstu",
                "remove_redundant": "first",
                "counting": "5"
            },
            "second": {
                "succ": "secone",
                "pred": "recond",
                "add_letter": "seconde",
                "remove_redundant": "second",
                "counting": "6"
            },
            "third": {
                "succ": "thire",
                "pred": "shird",
                "add_letter": "thirde",
                "remove_redundant": "third",
                "counting": "5"
            },
            "fourth": {
                "succ": "fourti",
                "pred": "eourth",
                "add_letter": "fourthi",
                "remove_redundant": "fourth",
                "counting": "6"
            },
            "fifth": {
                "succ": "fifti",
                "pred": "eifth",
                "add_letter": "fifthi",
                "remove_redundant": "fith",
                "counting": "5"
            }
        }

def insert_ordinal_word(sequence, probability=0.1):
    """Randomly replace the sequence with 1 ordinal word with given probability"""
    if random.random() > probability:
        return sequence, []
    
    # Selecting excatly 1 ordinal word to replace the entire sequence
    selected_word = random.choice(ORDINAL_WORDS)
    
    return selected_word, [selected_word]

def determine_active_transform(inserted_words, current_transform):
    """Determine which transformation should be active based on inserted ordinal words"""
    if not inserted_words:
        return current_transform
    
    # Use the last ordinal word to determine the transformation
    transform_mapping = {
        "first": "succ",           # Rule 1
        "second": "pred",          # Rule 2
        "third": "add_letter",     # Rule 3
        "fourth": "remove_redundant", # Rule 4
        "fifth": "counting"        # Rule 5
    }
    
    return transform_mapping.get(inserted_words[-1], current_transform)

def get_transform_result_for_ordinal(word, transform_type, meta_answers):
    """Get the transformation result for an ordinal word under a specific transformation"""
    if word in meta_answers and transform_type in meta_answers[word]:
        return meta_answers[word][transform_type]
    
    fallback_results = {
        "succ": word + "t",  
        "pred": "a" + word,  
        "add_letter": word + word[-1],  
        "remove_redundant": word,  
        "counting": str(len(word))  
    
    return fallback_results.get(transform_type, word)

def format_sequence(seq):
    """Format a sequence for display"""
    if isinstance(seq, list):
        return "[" + " ".join(seq) + "]"
    elif isinstance(seq, str):
        return seq
    else:
        return str(seq)

def check_correctness(response, expected_str, transform_type, input_str=None):
    """
    Check if the human's response matches the expected answer.
    
    Args:
        response (str): The human's response
        expected_str (str): The expected output string
        transform_type (str): The type of transformation applied
        input_str (str, optional): The original input string
        
    Returns:
        bool: True if the expected output matches the answer, False otherwise
    """
    # Handle empty responses
    if not response or not expected_str:
        return False
    
    # Clean both strings for comparison
    clean_answer = response.replace('[', '').replace(']', '').replace('"', '').replace("'", '').strip()
    clean_expected = expected_str.replace('[', '').replace(']', '').replace('"', '').replace("'", '').strip()
    
    # For "remove_redundant" transformation - must be exact match
    if transform_type == 'remove_redundant':
        # Remove spaces for comparison
        no_space_answer = clean_answer.replace(' ', '')
        no_space_expected = clean_expected.replace(' ', '')
        
        # Check if the answer is exactly the expected result
        return no_space_answer.lower() == no_space_expected.lower()
    
    # For counting transformation, extract digits
    if transform_type == 'counting':
        # If expected is a number, extract numbers from the answer
        if clean_expected.isdigit():
            import re
            numbers = re.findall(r'\d+', clean_answer)
            return clean_expected in numbers
    
    #For other transformations
    #Remove all spaces for comparison
    no_space_answer = clean_answer.replace(' ', '')
    no_space_expected = clean_expected.replace(' ', '')
    
    #Exact match after normalizing spaces
    return no_space_answer.lower() == no_space_expected.lower()

def main():
    #Load dataset and meta answers
    dataset_path = 'c:/Users/Nonac/Documents/UvA/Internship/nona code/combined_dataset.json'
    try:
        with open(dataset_path, 'r') as f:
            full_dataset = json.load(f)
    except FileNotFoundError:
        print(f"Error: Could not find dataset file at {dataset_path}")
        print("Please make sure the dataset file exists and update the path if needed.")
        return
    
    meta_answers = load_alphabet_meta_answers()
    
    print("="*60)
    print("HUMAN META-RULE ALPHABET GAME TEST")
    print("="*60)
    print(f"Loaded dataset with {len(full_dataset)} items\n")
    
    # Display the rules to the participant
    print(create_system_prompt())
    print("\n" + "="*60)
    
    # Test info
    base_transform = 'succ'  # Default transformation (Rule 1)
    current_transform = base_transform
    num_trials = min(250, len(full_dataset))
    
    # Participant info (not necessarily necessary)
    participant_id = input("Enter participant ID: ").strip()
    if not participant_id:
        participant_id = f"participant_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
    
    print(f"\nStarting test with {num_trials} trials...")
    print(f"Base transformation: Rule {TRANSFORM_TYPES[base_transform]['rule_number']} ({TRANSFORM_TYPES[base_transform]['name']})")
    print("\nPress Enter after reading the rules to continue...")
    input()
    
    # Results
    all_results = []
    correct_answers = 0
    incorrect_answers = 0
    
    for i in range(num_trials):
        # Select random item
        item = random.choice(full_dataset)
        
        # Check for existing ordinal words or insert new ones
        original_input = item['input']
        if any(word in str(original_input).lower() for word in ORDINAL_WORDS):
            found_word = next(word for word in ORDINAL_WORDS if word in str(original_input).lower())
            modified_input = original_input  # Keep the original input
            inserted_words = [found_word]    # But mark it as having an ordinal word
        else:
            # Only do random insertion if no natural ordinal found
            modified_input, inserted_words = insert_ordinal_word(original_input)
        
        # Update current transformation based on ordinal words
        if inserted_words:
            current_transform = determine_active_transform(inserted_words, current_transform)
            rule_num = TRANSFORM_TYPES[current_transform]['rule_number']
        
        # Get expected output
        if inserted_words:
            # For ordinal words, use meta-rule transformations
            expected_output = get_transform_result_for_ordinal(inserted_words[-1], current_transform, meta_answers)
        else:
            # Use regular transformation from dataset
            expected_output = item['transformations'][current_transform]
        
        # Format for display
        input_str = format_sequence(modified_input)
        expected_str = format_sequence(expected_output)
        rule_number = TRANSFORM_TYPES[current_transform]['rule_number']
        
        print(f"\n" + "-"*50)
        print(f"TRIAL {i+1}/{num_trials}")
        print(f"Input: {input_str}")
        
        # Get response
        start_time = time.time()
        human_answer = input("Your answer: ").strip()
        end_time = time.time()
        response_time = end_time - start_time
        
        # Check correctness
        is_correct = check_correctness(human_answer, expected_str, current_transform, input_str)
        
        # Create result object
        result = {
            "participant_id": participant_id,
            "trial_num": i+1,
            "original_input": original_input,
            "modified_input": modified_input,
            "inserted_words": inserted_words,
            "active_transform": current_transform,
            "rule_number": rule_number,
            "expected_output": expected_output,
            "input_str": input_str,
            "expected_str": expected_str,
            "transform_type": current_transform,
            "is_duplicated": item.get('has_duplicates', False),
            "data_type": "word" if isinstance(original_input, str) else "sequence",
            "timestamp": datetime.now().isoformat(),
            "response": human_answer,
            "response_time": response_time,
            "is_correct": is_correct
        }
        
        if is_correct:
            print("✓ Correct!")
            correct_answers += 1
        else:
            print(f"✗ Incorrect. The correct answer was: {expected_str}")
            incorrect_answers += 1
        
        all_results.append(result)
        
        # Show progress
        if (i + 1) % 50 == 0:
            current_accuracy = correct_answers / (i + 1) * 100
            print(f"\nProgress: {i+1}/{num_trials} trials completed")
            print(f"Current accuracy: {current_accuracy:.1f}%")
    
    # Print final results
    print("\n" + "="*60)
    print("FINAL RESULTS")
    print("="*60)
    print(f"Participant: {participant_id}")
    print(f"Total trials: {num_trials}")
    print(f"Correct answers: {correct_answers} ({correct_answers/num_trials*100:.1f}%)")
    print(f"Incorrect answers: {incorrect_answers} ({incorrect_answers/num_trials*100:.1f}%)")
    
    # Save results
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    results_dir = "results"
    os.makedirs(results_dir, exist_ok=True)
    
    # Save JSON
    json_filename = f"{results_dir}/human_meta_test_{participant_id}_{timestamp}.json"
    with open(json_filename, 'w') as f:
        json.dump(all_results, f, indent=2)
    print(f"Results saved to {json_filename}")
    
    # Save NPZ
    npz_filename = f"{results_dir}/human_meta_test_{participant_id}_{timestamp}.npz"
    np.savez(
        npz_filename,
        participant_id=participant_id,
        trial_nums=np.array([r["trial_num"] for r in all_results]),
        correctness=np.array([1 if r["is_correct"] else 0 for r in all_results]),
        response_times=np.array([r["response_time"] for r in all_results]),
        data_types=np.array([1 if r["data_type"] == "word" else 0 for r in all_results]),
        has_duplicates=np.array([1 if r["is_duplicated"] else 0 for r in all_results]),
        has_ordinals=np.array([1 if r["inserted_words"] else 0 for r in all_results]),
        transform_type=current_transform,
        accuracy=correct_answers/num_trials,
        timestamp=timestamp
    )
    print(f"Summary saved to {npz_filename}")
    print(f"\nThank you for participating, {participant_id}!")

if __name__ == "__main__":
    main()