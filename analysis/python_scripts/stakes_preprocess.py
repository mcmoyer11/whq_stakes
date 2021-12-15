import csv

results = './data/stakes_present.txt'
processed_results = './data/processed_results.csv'

with open(results,'rb+') as results, open(processed_results,'ab+') as processed_results:

    results = list(csv.reader(results))
    results = [x for x in results if not x[0].startswith('#')]

    processed_results = csv.writer(processed_results)

    by_trial_info = []
    subj_info = []
    subj_id = ''
    native_lang = ''
    age = ''
	
    for i,row in enumerate(results):

        probe = row[-2]
        target = row[-1]
        trial_type = row[-4]
        subj_id = ''
        native_lang = ''
        age = ''

        if trial_type == 'consent':
            if probe == 'subject_id':
                subj_info.append(target)
               
                # if not subj_id:
	               #  subj_id = target

                if target != subj_id:
                	subj_info = []
                	by_trial_info = []
                	subj_info.append(target)
                
            if probe == 'native_lang':
                subj_info.append(target)
               
                # if not native_lang:
                #     native_lang = target

                # elif target != native_lang:
                #     subj_info = []
                #     by_trial_info = []
                #     subj_info.append(target)

            if probe == 'age':
                subj_info.append(target)
                
                # if not age:
                #     age = target

                # elif target != age:
                #     subj_info = []
                #     by_trial_info = []
                #     subj_info.append(target)

            if probe == 'sex':
                subj_info.append(target)

        else:
            if probe == '_REACTION_TIME_':
            	by_trial_info.append((probe, target))
            	processed_results.writerow(subj_info + [trial_type] + by_trial_info)
            	by_trial_info = []
            else:by_trial_info.append((probe,target))
