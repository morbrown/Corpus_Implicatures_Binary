import os
import csv
import json

def consolidate():
    root_dir = "/Users/morbrown/Desktop/corpus_implicatures/some/experiments/main_experiments/2_speaker_knowledge"
    experiment_dirs = os.listdir(root_dir)
    all_stims = []
    for experiment_dir in experiment_dirs:
        if not experiment_dir.startswith("experiment_"):
            continue
        experiment_number = experiment_dir.split("_")[1]
        corpus_path = root_dir + "/" + experiment_dir + "/public/js/corpus" + experiment_number + ".txt"
        file = open(corpus_path, "r")
        reader = csv.DictReader(file, delimiter="\t")
        all_stims.extend(reader)
        file.close()

    with open("../experiment_01/public/stims/all_stims.json", "w") as f:
        all_stims_string = "items = " + json.dumps(all_stims, indent=4)
        f.write(all_stims_string)


consolidate()
