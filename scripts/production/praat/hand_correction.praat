#########################################################
# Praat script to hand correct auto-segmented textgrids #
# Created by                                            #
# Kyle Parrish 9/10/21.  			                     # 	
# Adapted from a script by Joseph Casillas              #
#########################################################
 

#
# Enter the path to where the files are kept -------------------
#

form Enter information
	comment Participant ID (folder with .wav files):
	sentence fileID 492813
endform


dirFiles$ = "../../../production_files/participant_uploads_bilingual/"+fileID$+"/"
newDir$ = "../../../production_files/participant_uploads_bilingual_correct/"+fileID$+"/"
number = 1

# --------------------------------------------------------------



#
# Prepare the loop ---------------------------------------------
#

# Find the .wav files
Create Strings as file list: "allFiles", dirFiles$ + "/*.wav"

Create Strings as file list: "textgrids", dirFiles$ + "/*.TextGrid"

# Select allFiles
select Strings allFiles

# Count number of stings 
numberOfFiles = Get number of strings

# Clear info window just in case
clearinfo

# --------------------------------------------------------------




for i from number to numberOfFiles
select Strings allFiles
fileName$ = Get string... i
Read from file... 'dirFiles$'/'fileName$'
select Strings textgrids
fileName$ = Get string... i
Read from file... 'dirFiles$'/'fileName$'
nameSound$ = selected$("TextGrid")
select TextGrid 'nameSound$'
plus Sound 'nameSound$'
Edit
pause Continue?
	select Sound 'nameSound$'
	Write to WAV file... 'newDir$'/'nameSound$'.wav
	select TextGrid 'nameSound$'
	Write to text file... 'newDir$'/'nameSound$'.TextGrid
	select all
	minus Strings allFiles
	minus Strings textgrids
	Remove
	printline 'nameSound$'	'i'
endfor


