import argparse
import csv

parser = argparse.ArgumentParser()
parser.add_argument("spec_file")
parser.add_argument("trial_template")
parser.add_argument("control_template")
args = parser.parse_args()

spec_file = args.spec_file
trial_template = args.trial_template
control_template = args.control_template

for a in [spec_file,trial_template,control_template]:
	print a

# with open(spec_file,'rb+') as spec_file:
	
