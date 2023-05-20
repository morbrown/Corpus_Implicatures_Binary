import json

def replace():
    key1 = "EntireSentence"
    key2 = "BestResponse"
    with open("/Users/morbrown/code/corpus_implicatures_binary/experiment_01/public/stims/all_stims copy.json", "r") as f:  
        data = json.load(f)
        print(data[0])
        # for v in data.values(str()):
            # print(v["EntireSentence"])
                
replace()
