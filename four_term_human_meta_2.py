import json
import numpy as np
import os
import time
from datetime import datetime

def create_instructions():
    """Create the game instructions with all rule explanations"""
    instructions = """**Game rules:**
Rule Explanations:
"1. Category: What broader category does the term belong to? (e.g., Moose -> Animal; Persimmon -> Fruit; Stethoscope -> Object)\n"
"2. Function: What action or purpose is associated with the term? (e.g., Moose -> Graze; Persimmon -> Eat; Stethoscope -> Listen)\n"
"3. Antonym: What is the opposite or contrasting concept? (e.g., Moose -> Mouse; Persimmon -> Vinegar; Stethoscope -> Earplug)\n"
"4. Synonym: What is similar to or can substitute for the term? (e.g., Moose -> Elk; Persimmon -> Kaki; Stethoscope -> Phonendoscope)\n"
"5. Compositional: What larger structure contains this item, what is it composed of, or what is it a key ingredient of? (e.g., Moose -> Herd; Persimmon -> Tart; Stethoscope -> Hospital)\n\n"
Base Rule: Use Rule 1 as the default unless instructed otherwise by the meta-rules.
Meta rule 1: When you encounter an ordinal number, switch the base rule to the corresponding numbered rule ("first" apply rule 1). Apply this new rule from that point onward until another ordinal number appears, at which point you update the base rule again. (e.g., "second" switch to rule 2, "third" switch to rule 3).
Meta rule 2: Figure out which rule out of the 5 rules you used for each ordinal number (first, second, etc.) and then apply the following rule (i.e., if you used rule 1, now apply rule 2/if you applied the nth rule, apply n+1) to the 5 novel statements. If you need to apply rule 5, then apply rule 1 as the following rule. Apply them in order (e.g., apply the (n+1)th to the nth novel statement).
Answer with the letter of the correct option (A, B, C, D, or E)
"""
    return instructions

def main():
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
    
    print(f"\nWelcome, {participant_name}!")
    print("\nFirst you'll see 5 examples, then solve 5 new problems.")
    print("Press Enter when ready to continue...")
    input()
    
    # Phase 1: Show examples
    print("\n" + "="*60)
    print("LEARNING PHASE: EXAMPLE PROBLEMS")
    print("="*60)
    print("Study these examples to understand the pattern:")
    
    examples = [
        {
            "term": "second",
            "options": ["A. Competition", "B. Order", "C. Two", "D. Second last", "E. Settle"],
            "answer": "E"
        },
        {
            "term": "fourth", 
            "options": ["A. Four", "B. Order", "C. Fourth last", "D. Competition", "E. Place"],
            "answer": "A"
        },
        {
            "term": "first",
            "options": ["A. Competition", "B. Last", "C. Cheer", "D. One", "E. Order"],
            "answer": "E"
        },
        {
            "term": "fifth",
            "options": ["A. Five", "B. Order", "C. Fifth last", "D. Competition", "E. Obtain"],
            "answer": "D"
        },
        {
            "term": "third",
            "options": ["A. Three", "B. Competition", "C. Third last", "D. Order", "E. Finish"],
            "answer": "C"
        }
    ]
    
    for i, example in enumerate(examples, 1):
        print(f"\n{i}. Word: {example['term']}")
        for option in example['options']:
            print(f"   {option}")
        print(f"   Correct answer: {example['answer']}")
    
    print(f"\nLook for the pattern in these examples...")
    print("When ready for the challenge, press Enter...")
    input()
    
    # Phase 2: Challenge problems
    print("\n" + "="*60)
    print("CHALLENGE PHASE: SOLVE THESE NEW PROBLEMS")
    print("="*60)
    
    challenge_problems = [
        {
            "term": "Shirt",
            "options": ["A. Outfit", "B. Clothing", "C. Top", "D. Pants", "E. Wear"],
            "correct": "D"
        },
        {
            "term": "Second", 
            "options": ["A. Two", "B. Order", "C. Competition", "D. Second last", "E. Settle"],
            "correct": "C"
        },
        {
            "term": "Fourth",
            "options": ["A. Place", "B. Four", "C. Order", "D. Fourth last", "E. Competition"],
            "correct": "A"
        },
        {
            "term": "Book",
            "options": ["A. Library", "B. Object", "C. Novel", "D. Phone", "E. Read"],
            "correct": "B"
        },
        {
            "term": "Venus",
            "options": ["A. Solar system", "B. Planet", "C. Asteroid", "D. Morning star", "E. Orbit"],
            "correct": "D"
        }
    ]
    
    print("Solve these 5 new problems:")
    
    # Collect answers
    user_answers = []
    start_time = time.time()
    
    for i, problem in enumerate(challenge_problems, 1):
        print(f"\n{i}. Word: {problem['term']}")
        for option in problem['options']:
            print(f"   {option}")
        
        answer = input(f"Your answer (A, B, C, D, or E): ").strip().upper()
        user_answers.append(answer)
    
    end_time = time.time()
    total_time = end_time - start_time
    
    # Show results
    print("\n" + "="*60)
    print("YOUR RESULTS")
    print("="*60)
    
    correct_count = 0
    problem_results = []
    
    for i, (problem, user_answer) in enumerate(zip(challenge_problems, user_answers), 1):
        is_correct = user_answer == problem['correct']
        if is_correct:
            correct_count += 1
            status = "✓ Correct"
        else:
            status = "✗ Incorrect"
        
        print(f"{i}. {problem['term']}")
        print(f"   Your answer: {user_answer}")
        print(f"   Correct answer: {problem['correct']}")
        print(f"   {status}")
        print()
        
        problem_results.append({
            "problem_number": i,
            "term": problem['term'],
            "your_answer": user_answer,
            "correct_answer": problem['correct'],
            "correct": is_correct
        })
    
    score_percent = correct_count / len(challenge_problems) * 100
    print(f"Final Score: {correct_count}/{len(challenge_problems)} ({score_percent:.1f}%)")
    
    # Save data
    session_data = {
        "participant": participant_name,
        "date": datetime.now().isoformat(),
        "examples_shown": examples,
        "challenge_problems": challenge_problems,
        "user_answers": user_answers,
        "problem_results": problem_results,
        "completion_time_seconds": total_time,
        "score": correct_count,
        "total_problems": len(challenge_problems),
        "score_percentage": score_percent,
        "game_rules": create_instructions()
    }
    
    # Create results folder and save files
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    if not os.path.exists("results"):
        os.makedirs("results")
    
    # Save detailed results
    json_file = f"results/word_challenge_{participant_name}_{timestamp}.json"
    with open(json_file, 'w') as f:
        json.dump(session_data, f, indent=2)
    print(f"\nSession saved to {json_file}")
    
    # Save summary data
    summary_file = f"results/word_summary_{participant_name}_{timestamp}.npz"
    np.savez(
        summary_file,
        participant=participant_name,
        answers=np.array(user_answers, dtype=object),
        correct_answers=np.array([p['correct'] for p in challenge_problems], dtype=object),
        scores=np.array([1 if r["correct"] else 0 for r in problem_results]),
        final_score=score_percent,
        completion_time=total_time,
        timestamp=timestamp
    )
    print(f"Summary saved to {summary_file}")
    print(f"\nThanks for playing, {participant_name}!")

if __name__ == "__main__":
    main()