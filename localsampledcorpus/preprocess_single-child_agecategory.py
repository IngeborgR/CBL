# -*- coding: utf-8 -*-
import glob, os, re

# IMPORTANT: for this script to run correctly,
# place it in the same folder as the *.cha-files from
# the corpus

if __name__ == "__main__":
    child_name = "William" #either Alex, Ethan, Lily, Naima, Violet or William
    age = "2_6"
    #  make a new file in which the cleaned corpus will be stored
    output_caregivers_filename = "corpusProvidence_caregivers_"  + child_name + "_age" + age + ".txt"
    soutput_caregivers_filename = "corpusProvidence_caregivers_size" + child_name +"_age" + age + ".txt"
    outputfile = open(output_caregivers_filename, "w")

    output_child_filename = "corpusProvidence_child" + child_name +"_age" + age + ".txt"
    soutput_child_filename = "corpusProvidence_child_size" + child_name +"_age" + age + ".txt"
    outputchildfile = open(output_child_filename, "w")

    # initialization:
    num_utterances = 0
    num_words = 0
    num_child_utterances = 0
    num_child_words = 0

    # for all *.cha-files in the current folder
    for file in glob.glob("*.cha"):
        # open the current *.cha-file
        with open(file, "r") as f:
            # read the file line by line
            for line in f:
                # if utterance spoken by child
                if "*CHI:" in line:
                    nline = line.split('.', 1)
                    nline = nline[0]
                    nline = nline.split('*CHI:\t', 1)
                    nline = nline[1]
                    nline = nline.replace('[?]', '')
                    nline = nline.split('?', 1)
                    nline = nline[0]
                    nline = nline.split('!', 1)
                    nline = nline[0]
                    nline = re.sub(r"(\(.*?\))", "", nline)
                    nline = re.sub(r"(0.*?\s)", "", nline)
                    nline = re.sub(r"(\[.*?\])", "", nline)
                    nline = re.sub(r"(\+.*\s*)", " ", nline)
                    nline = re.sub(r"(\@.*?\s)", " ", nline)
                    nline = nline.replace('‹', '')
                    nline = nline.replace('›', '')
                    nline = nline.replace('yyy', '')
                    nline = nline.replace('<', '')
                    nline = nline.replace('>', '')
                    nline = nline.replace('xxx', '')
                    nline = nline.replace(",", "")
                    nline = nline.replace("(", "")
                    nline = nline.replace(":", "")
                    nline = nline.replace("  ", " ")
                    nline = nline.strip(" ")

                    if not len(nline) == 0:
                        # if the line still contains text
                        nline = '# ' + nline
                        # print nline  # optional to check script correctness
                        # write line to outputfile
                        num_child_utterances += 1
                        outputchildfile.write(nline + "\n")

                # if the line contains the following string of tokens (same code repeated for different speakers)
                # cleans up the line by removing the speaker tier, punctuation marks (question marks, exclamation marks & comma's), interruption
                # markers (+/ and +//), unintelligentable speech ('xxx'), assimilation remarks ('(...)') and explanations ('[...]')

                # if spoken by mother
                if "*MOT:" in line:
                    nline = line.split('.', 1)
                    nline = nline[0]
                    nline = nline.split('*MOT:\t', 1)
                    nline = nline[1]
                    nline = nline.split('?', 1)
                    nline = nline[0]
                    nline = nline.split('!', 1)
                    nline = nline[0]
                    nline = re.sub(r"(\(.*?\))", "", nline)
                    nline = re.sub(r"(\[.*?\])", "", nline)
                    nline = re.sub(r"(\+.*\s*)", " ", nline)
                    nline = re.sub(r"(\@.*?\s)", " ", nline)
                    nline = nline.replace('xxx', '')
                    nline = nline.replace(", ", "")
                    nline = nline.replace("(", "")
                    nline = nline.replace(":", "")
                    nline = nline.replace("  ", " ")
                    nline = nline.strip(" ")

                    if not len(nline) == 0:
                        # if the line still contains text
                        nline = '# ' + nline
                        # print nline #optional to check script correctness
                        # write line to outputfile
                        num_utterances += 1
                        outputfile.write(nline + "\n")

                # if spoken by father
                if "*FAT:" in line:  # adjust if searching for different speaker(s) /*or "*FAT:" or "*GRA" or "*ADU" or "*GRN"*/
                    nline = line.split('.', 1)
                    nline = nline[0]
                    nline = nline.split('*FAT:\t', 1)
                    nline = nline[1]
                    nline = nline.split('?', 1)
                    nline = nline[0]
                    nline = nline.split('!', 1)
                    nline = nline[0]
                    nline = re.sub(r"(\(.*?\))", "", nline)
                    nline = re.sub(r"(\[.*?\])", "", nline)
                    nline = re.sub(r"(\+.*\s*)", " ", nline)
                    nline = re.sub(r"(\@.*?\s)", " ", nline)
                    nline = nline.replace('xxx', '')
                    nline = nline.replace(", ", "")
                    nline = nline.replace("(", "")
                    nline = nline.replace(":", "")
                    nline = nline.replace("  ", " ")
                    nline = nline.strip(" ")

                    if not len(nline) == 0:
                        # if the line still contains text
                        nline = '# ' + nline
                        # print nline #optional to check script correctness
                        # write line to outputfile
                        num_utterances += 1
                        outputfile.write(nline + "\n")

                # if spoken by grandmother
                if "*GRA:" in line:  # adjust if searching for different speaker(s) /*or "*FAT:" or "*GRA" or "*ADU" or "*GRN"*/
                    nline = line.split('.', 1)
                    nline = nline[0]
                    nline = nline.split('*GRA:\t', 1)
                    nline = nline[1]
                    nline = nline.split('?', 1)
                    nline = nline[0]
                    nline = nline.split('!', 1)
                    nline = nline[0]
                    nline = re.sub(r"(\(.*?\))", "", nline)
                    nline = re.sub(r"(\[.*?\])", "", nline)
                    nline = re.sub(r"(\+.*\s*)", " ", nline)
                    nline = re.sub(r"(\@.*?\s)", " ", nline)
                    nline = nline.replace('xxx', '')
                    nline = nline.replace(", ", "")
                    nline = nline.replace("(", "")
                    nline = nline.replace(":", "")
                    nline = nline.replace("  ", " ")
                    nline = nline.strip(" ")

                    if not len(nline) == 0:
                        # if the line still contains text
                        nline = '# ' + nline
                        # print nline #optional to check script correctness
                        # write line to outputfile
                        num_utterances += 1
                        outputfile.write(nline + "\n")

                if "*GRN:" in line:  # adjust if searching for different speaker(s) /*or "*FAT:" or "*GRA" or "*ADU" or "*GRN"*/
                    nline = line.split('.', 1)
                    nline = nline[0]
                    nline = nline.split('*GRN:\t', 1)
                    nline = nline[1]
                    nline = nline.split('?', 1)
                    nline = nline[0]
                    nline = nline.split('!', 1)
                    nline = nline[0]
                    nline = re.sub(r"(\(.*?\))", "", nline)
                    nline = re.sub(r"(\[.*?\])", "", nline)
                    nline = re.sub(r"(\+.*\s*)", " ", nline)
                    nline = re.sub(r"(\@.*?\s)", " ", nline)
                    nline = nline.replace('xxx', '')
                    nline = nline.replace(", ", "")
                    nline = nline.replace("(", "")
                    nline = nline.replace(":", "")
                    nline = nline.replace("  ", " ")
                    nline = nline.strip(" ")

                    if not len(nline) == 0:
                        # if the line still contains text
                        nline = '# ' + nline
                        # print nline #optional to check script correctness
                        # write line to outputfile
                        num_utterances += 1
                        outputfile.write(nline + "\n")

                if "*ADU:" in line:  # adjust if searching for different speaker(s) /*or "*FAT:" or "*GRA" or "*ADU" or "*GRN"*/
                    nline = line.split('.', 1)
                    nline = nline[0]
                    nline = nline.split('*ADU:\t', 1)
                    nline = nline[1]
                    nline = nline.split('?', 1)
                    nline = nline[0]
                    nline = nline.split('!', 1)
                    nline = nline[0]
                    nline = re.sub(r"(\(.*?\))", "", nline)
                    nline = re.sub(r"(\[.*?\])", "", nline)
                    nline = re.sub(r"(\+.*\s*)", " ", nline)
                    nline = re.sub(r"(\@.*?\s)", " ", nline)
                    nline = nline.replace('xxx', '')
                    nline = nline.replace(", ", "")
                    nline = nline.replace("(", "")
                    nline = nline.replace(":", "")
                    nline = nline.replace("  ", " ")
                    nline = nline.strip(" ")

                    if not len(nline) == 0:
                        # if the line still contains text
                        # print nline #optional to check script correctness
                        # write line to outputfile
                        nline = '# ' + nline
                        num_utterances += 1
                        outputfile.write(nline + "\n")

    # close the outputfile
    outputfile.close()
    soutputfile = open(soutput_caregivers_filename, "w")
    soutputfile.write(str(num_utterances))
    soutputfile.close()
    outputchildfile.close()
    soutput_child_file = open(soutput_child_filename, "w")
    soutput_child_file.write(str(num_child_utterances))
    soutput_child_file.close()
    print("Program done")
