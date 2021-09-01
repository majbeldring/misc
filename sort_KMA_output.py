'''
Maj Beldring Henningsen, 
Sort KMA output for nnaopore sequenced data

To:
Sort KMA output, only showing Template name and template_ID

NOTE:
In order to receive Template_Identity in the second array:
Template name should only be one string (no gap in the name)
'''

final_name = [] #2 arrays, one for name and one for number
final_number = [] #number

with open("test.res", 'r') as data: #open test.res, "r" mean read only, 'data' is the complete file


    for line in data: # 'line' is each seperate line in the 'data' file

        # [data] = {
        #   [line]
        #   [line]
        #   [line]
        #   [line]
        #   [line]
        #   [line]
        # }

        print("--- Newline ---") # DEDBUGGING: because there was a split in data

        line_split = line.split() # splits the line with " " (whitespace)
        # [line] = [string][string][string][string][string]

        final_name.append(line_split[0]) # received the string from the first index
        final_number.append(line_split[4]) # received the string from the fifth index


for index, element in enumerate(final_name): # Run over the array, using the enumerate to get the index: 0, 1, 2, 3, 4 instead of the element.

    print(final_name[index]+"   "+final_number[index]) #

