#########
# Loops #
#########


#
# Initial setup
# 
# - We will set up some string variables that 
#   will make it easier to program the loop 
#     - path$: the relative path to the dir 
#       where we keep the files. 
#     - outputDir$: the relative path to the 
#       dir where we want to keep the output 
#       .csv file.
# - We will also create the .csv file where 
#   the output will go. We have to include 
#   the variables we want (the column names).


# Set path to folder where files are
path$ = "../../production_files/sound_files_eng_mono/"

# Where should the output be saved?
outputDir$ = "../../data/tidy/"

# Create output file and set header
fileappend 'outputDir$'/bil_data.csv prefix,f1e,f2e,f1_20e,f1_35e,f1_50e,f1_65e,f1_80e,f2_20e,f2_35e,f2_50e,f2_65e,f2_80e,tle,f1a,f2a,f1_20a,f1_35a,f1_50a,f1_65a,f1_80a,f2_20a,f2_35a,f2_50a,f2_65a,f2_80a,tla,f1i,f2i,f1_20i,f1_35i,f1_50i,f1_65i,f1_80i,f2_20i,f2_35i,f2_50i,f2_65i,f2_80i,tli,cdP,cdT,votP,votT,labID'newline$'



#
# Prepare loop
#

# Go to folder where files are located, create list
Create Strings as file list: "fileList", path$ + "*.wav"

# Select the object fileList
selectObject: "Strings fileList"

# Count # of files and assign total to 'numFiles'
numFiles = Get number of strings

#
# Start loop
#

for i from 1 to numFiles

	# Select string, read in files
	select Strings fileList
	fileName$ = Get string... i
	prefix$ = fileName$ - ".wav"
	Read from file... 'path$'/'prefix$'.wav
	Read from file... 'path$'/'prefix$'.TextGrid


	# Calculate mid-point of vowel E 
	vowelEstart = Get start point: 3, 3
	vowelEend  = Get end point: 3, 3
	durationVe =  vowelEend - vowelEstart
	per20e = vowelEstart + (durationVe * 0.20)
	per35e = vowelEstart + (durationVe * 0.35)
	per50e = vowelEstart + (durationVe * 0.50)
	per65e = vowelEstart + (durationVe * 0.65)
	per80e = vowelEstart + (durationVe * 0.80)

	# Get formants
	select Sound 'prefix$'
	do ("To Formant (burg)...", 0, 5, 5500, 0.025, 50)
	f1_20e = do ("Get value at time...", 1, per20e, "Hertz", "Linear")
	f1_35e = do ("Get value at time...", 1, per35e, "Hertz", "Linear")
	f1_50e = do ("Get value at time...", 1, per50e, "Hertz", "Linear")	
	f1_65e = do ("Get value at time...", 1, per65e, "Hertz", "Linear")
	f1_80e = do ("Get value at time...", 1, per80e, "Hertz", "Linear")
	f2_20e = do ("Get value at time...", 2, per20e, "Hertz", "Linear")
	f2_35e = do ("Get value at time...", 2, per35e, "Hertz", "Linear")
	f2_50e = do ("Get value at time...", 2, per50e, "Hertz", "Linear")	
	f2_65e = do ("Get value at time...", 2, per65e, "Hertz", "Linear")
	f2_80e = do ("Get value at time...", 2, per80e, "Hertz", "Linear")


	f1e = (f1_20e + f1_35e + f1_50e + f1_65e + f1_80e)/5
	f2e = (f2_20e + f2_35e + f2_50e + f2_65e + f2_80e)/5

	 
	# Append data to .csv file
	fileappend 'outputDir$'/bil_data.csv 'prefix$','f1e:2','f2e:2','f1_20e:2','f1_35e:2','f1_50e:2','f1_65e:2','f1_80e:2','f2_20e:2','f2_35e:2','f2_50e:2','f2_65e:2','f2_80e:2','tle:2', 'f1a:2','f2a:2','f1_20a:2','f1_35a:2','f1_50a:2','f1_65a:2','f1_80a:2','f2_20a:2','f2_35a:2','f2_50a:2','f2_65a:2','f2_80a:2','tla:2', 'f1i:2','f2i:2','f1_20i:2','f1_35i:2','f1_50i:2','f1_65i:2','f1_80i:2','f2_20i:2','f2_35i:2','f2_50i:2','f2_65i:2','f2_80i:2','tli:2', 'cdP:2','cdT:2','votP:2','votT:2','labID$''newline$'

	# Printline for bug fixes (comment out for speed)
	printline 'prefix$','f1e:2','f2e:2','f1_20e:2','f1_35e:2','f1_50e:2','f1_65e:2','f1_80e:2','f2_20e:2','f2_35e:2','f2_50e:2','f2_65e:2','f2_80e:2','tle:2','f1a:2','f2a:2','f1_20a:2','f1_35a:2','f1_50a:2','f1_65a:2','f1_80a:2','f2_20a:2','f2_35a:2','f2_50a:2','f2_65a:2','f2_80a:2','tla:2','f1i:2','f2i:2','f1_20i:2','f1_35i:2','f1_50i:2','f1_65i:2','f1_80i:2','f2_20i:2','f2_35i:2','f2_50i:2','f2_65i:2','f2_80i:2','tli:2','cdP:2','cdT:2','votP:2','votT:2','labID$'

	# Clean up
	select all
	minus Strings fileList
	Remove
endfor

# Clean objects