import json
import pandas as pd

def main():
    file_name = 'example_Data.json'
    with open (file_name) as file:
        data = json.load(file)

    index = 0

    df = pd.DataFrame()

    for participant in data:
        for trial in participant:
            # Assign textual answer to participant
            try:
                if trial['scenario'] != '':
                    if str(trial['response']) == '0':
                        textual_answer = 'true'
                        indeterminate_status = 0
                    elif str(trial['response']) == '1':
                        textual_answer = 'indeterminate'
                        indeterminate_status = trial['indeterminate_status']
                    elif str(trial['response']) == '2':
                        textual_answer = 'false'
                        indeterminate_status = 0
                    else:
                        textual_answer = ''
                    
                    row = pd.DataFrame({
                        'participant_id': trial['run_id'],
                        'type': trial['question_type'],
                        'ordering':  trial['ordering'],
                        'scenario':  trial['scenario'],
                        'rt': trial['rt'],
                        'textual_answer': textual_answer,
                        'numerical_answer': trial['response'],
                        'type': trial['question_type'],
                        'pred_ans': trial['predicted_ans'],
                        'condition': trial['condition'],
                        'indeterminate_status': indeterminate_status,
                        }, index= [index])
                
                    df = pd.concat([df,row])
                    index +=1
            except KeyError as e:
                print(e)
                pass


        df.to_csv(f'{file_name.split(".")[0]}.csv', index=False)

if __name__ == "__main__":
    main()