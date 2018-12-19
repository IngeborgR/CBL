from __future__ import division
import numpy as np
import cPickle as pickle
from random import shuffle
from copy import deepcopy
import csv
import os

"class Word: "
class Word:
    def __init__(self, ortho, count):
        self.ortho = ortho  # A string, orthographic representation of the word
        self.count = count # An integer-value, how often the word has been counted so far
        #self.word_class = None  # An integer-value, refering to a (part-)word class
        #self.frame = None

    # returns a printable string version of the word
    def show(self):
        return self.ortho

"class Wordlist: "
class WordList:
    def __init__(self):
        self.size = 0 #A n integer-value, how many words are stored in the list
        self.all = [] # List of Word-objects

    # adds a Word to WordList, and updates WordList values
    def add_word(self, word):
        #go through all words already stored in the WordList
        for k in range(0, self.size):
            # if the word is already in the list, adjust the associated wordcount
            if self.all[k].ortho == word:
                self.all[k].count += 1
                # list of ortho, number of ortho in list, count of updated word
                return self.all[k].count
        # if the word is new, add it to the list
        ww = Word(word, 1)
        self.all.append(ww)
        self.size += 1
        return ww.count #returns the count of the added Word

    # returns the wordcount for the word
    def getWordcount(self, word):
        for k in range(0, self, size):
            if self.all[k].ortho == word:
                return self.all[k].count
        return 0

    # returns the index of the word in the WordList
    def findWord(self, word):
        for k in range(0, self.size):
            if self.all[k].ortho == word.ortho:
                return k
        return None

"class ChunkList: "
class ChunkList:
    def __init__(self):
        self.size = 0 # An integer-value, how many chunks are stored in the list
        self.all = [] # List of Chunk-objects

    # counts how often pair is (part of) an already stored chunk
    def count_chunk(self, pair):
        count = 0
        match = None
        for k in range(0, self.size - 1):
            # check for perfect match of pair with chunk
            if self.all[k].ortho == pair:
                match = k
                count = count + self.all[k].count
            # check if pair is subset of stored chunk
            elif findsubset(self.all[k].ortho, pair):
                count = count + self.all[k].count
                # count: how often the pair is (part of) a chunk, match: the chunk that exactly matches pair
        return count, match

"class Chunk: "
class Chunk:
    def __init__(self):
        self.ortho = [] # String, orthographic representating of the chunk
        self.count = 0 # An integer-value, how often the chunk has been seen

# returns a printable string version of the chunk
def show(chunk):
    return (str(chunk.ortho) + "\t counted: " + str(chunk.count) + " times")

"class FrameList: "
class FrameList:
    def __init__(self):
        self.numframes = 0 # An integer-value, how many frames are stored in the list
        self.all = []  # List of Frame-objects

    # adds a new frame to the FrameList
    def add_frame(self, chunk, utterance, previous_frame):
        # initialization of variables
        before = False
        after = False
        found = False
        start = None
        end = None

        # make temporary copy of utterance and frame as lists for practical purposes
        temp_u = []
        temp_chunk = []
        for k in range(0, len(utterance)):
            temp_u.append(utterance[k].ortho)
        for k in range(0, len(chunk.ortho)):
            temp_chunk.append(chunk.ortho[k])

        # search for frame within utterance and determine if it is preceded and/or
        # followed by a word(s)
        for subtuple_length in reversed(xrange(1, len(temp_u))):
            for start_index in xrange(0, (len(temp_u) + 1 - subtuple_length)):
                temp = temp_u[start_index:start_index + subtuple_length]
                if temp == temp_chunk:
                    found = True
                    start = start_index
                    end = start_index + subtuple_length
                    break
        if found:
            if start > 1:
                before = True
            if end < len(utterance) - 1:
                after = True

        # make a new frame object and add it to the list of frames
        new_frame = Frame(chunk)
        ortho = (" ".join(chunk.ortho))
        new_frame.ortho = ortho

        if previous_frame:
            p = self.all[previous_frame]
            new_frame.before_slot.append(p)
            new_frame.before_probs.append(1)
        elif before:
            new_frame.before_slot.append(utterance[start - 1])
            new_frame.before_probs.append(1)
        if after:
            new_frame.after_slot.append(utterance[end])
            new_frame.after_probs.append(1)

        self.all.append(new_frame)
        self.numframes += 1
        stop = False

        if previous_frame:
            for k in range(0, len(self.all[previous_frame].after_slot)):
                if self.all[previous_frame].after_slot[k].ortho == new_frame.ortho:
                    self.all[previous_frame].after_probs[k] += 1
                    stop = True
                    break
            if not stop:
                self.all[previous_frame].after_slot.append(new_frame)
                self.all[previous_frame].after_probs.append(1)

    # updates existing frames by adding orthographic information to the before and after slots when encountered
    def update_frame(self, frame, utterance, previous_frame):
        # initialization
        before = False
        after = False
        found = False
        start = None
        end = None
        temp_u = []
        temp_chunk = []

        # make temporary copy of utterance and frame as lists for practical purposes
        for k in range(0, len(utterance)):
            temp_u.append(utterance[k].ortho)
        for k in range(0, len(self.all[frame].chunk.ortho)):
            temp_chunk.append(self.all[frame].chunk.ortho[k])

        # search for frame within utterance and determine if it is preceded and/or
        # followed by a word(s)
        for subtuple_length in reversed(xrange(1, len(temp_u))):
            for start_index in xrange(0, (len(temp_u) + 1 - subtuple_length)):
                temp = temp_u[start_index:start_index + subtuple_length]
                if temp == temp_chunk:
                    found = True
                    start = start_index
                    end = start_index + subtuple_length
                    break
        if found:
            if start > 1:
                before = True
            if end < len(utterance) - 1:
                after = True

        # add ortho to frame in the correct slot
        stop = False

        if previous_frame:
            for k in range(0, len(self.all[frame].before_slot)):
                # print self.all[frame].before_slot[k].ortho
                if self.all[frame].before_slot[k].ortho == self.all[previous_frame].ortho:
                    self.all[frame].before_probs[k] += 1
                    stop = True
                    break
            if not stop:
                self.all[frame].before_slot.append(self.all[previous_frame])
                self.all[frame].before_probs.append(1)
            stop = False
            for k in range(0, len(self.all[previous_frame].after_slot)):
                if self.all[previous_frame].after_slot[k].ortho == self.all[frame].ortho:
                    self.all[previous_frame].after_probs[k] += 1
                    stop = True
                    break
            if not stop:
                self.all[previous_frame].after_slot.append(self.all[frame])
                self.all[previous_frame].after_probs.append(1)
        elif before:
            for k in range(0, len(self.all[frame].before_slot)):
                # if the ortho is already in this slot, adjust the word count
                if self.all[frame].before_slot[k].ortho == utterance[start - 1].ortho:
                    self.all[frame].before_probs[k] += 1
                    stop = True
                    break
            # if the word is new to this slot, add the word
            if not stop:
                self.all[frame].before_slot.append(utterance[start - 1])
                self.all[frame].before_probs.append(1)

        stop = False
        if after:
            for k in range(0, len(self.all[frame].after_slot)):
                # if the ortho is already in this slot, adjust the word count
                if self.all[frame].after_slot[k].ortho == utterance[end].ortho:
                    self.all[frame].after_probs[k] += 1
                    stop = True
                    break
            if not stop:  # if the word is new to this slot, add the word
                self.all[frame].after_slot.append(utterance[end])
                self.all[frame].after_probs.append(1)

    # searches whether a chunk is already a frame stored in self.all
    def find_frame(self, chunk):
        for l in range(self.numframes):
            if self.all[l].chunk.ortho == chunk.ortho:
                # l: index of matching frame in frames
                return l
        # no match is found, None: no index of matching frame in f
        return None

    # TODO: UNFINISHED CODE, NOT SURE WHAT THIS WAS SUPPOSED TO DO
    def findWordClass(self, all):
        for i in range(0, self.numframes):
            # for each frame, mark the ortho that occur in the same frame (or something like that)
            for j in range(0, len(self.all[i].before_slot)):
                word = self.all[i].before_slot[j]
                w = all.findWord(word)
            # update word.framecount oid

            for j in range(0, len(self.all[i].after_slot)):
                word = self.all[i].after_slot[j]
                w = all.findWord(word)
        return all


"class Frame: "
class Frame:
    def __init__(self, chunk):
        self.chunk = chunk  # A list of word objects, empty slots represented by 'X# '
        self.ortho = None #String-value, orthographic representation of chunk
        self.before_slot = [] #A list, containing the Words / Chunks seen before the chunk
        self.before_probs = [] #A list, containing the counts of Words / Chunks seen before the chunk
        self.after_slot = [] #A list, containing the Words / Chunks seen after the chunk
        self.after_probs = [] #A list, containing the counts of Words / Chunks seen after the chunk


# returns printable string to check frame contents
def show(frame):
    string = ""
    if len(frame.before_slot) > 0:
        string += "X "
    for k in range(0, len(frame.before_slot)):
        string += str(frame.before_slot[k].ortho) + str(frame.before_probs[k]) + "\t"
    string += str(frame.ortho)
    if len(frame.after_slot) > 0:
        string += " X"
    string += "\t counted: " + str(frame.chunk.count) + " times" + "\t"
    for k in range(0, len(frame.after_slot)):
        string += str(frame.after_slot[k].ortho) + str(frame.after_probs[k]) + "\t"
    return string

"class Wordpair: "
class Wordpair:
    def __init__(self, pair, count):
        self.ortho = pair #String, orthographic representation of Word pair
        self.count = count #An integer-value, how often the pair has been seen


"class PairList: "
class PairList:
    def __init__(self):
        self.all = [] # List, containing Pair-object
        self.size = 0 # An integer-value, how many pairs are stored in the PairList

    # updates word-pair list, by adding pair if it is new, or increasing the count of an already stored Pair
    def update_pairlist(self, pair):
        for k in range(0, self.size):
            # if the word-pair is already in the list, adjust the associated word-pair count
            if self.all[k].ortho == pair:
                self.all[k].count += 1
                # list of pairs, number of pairs, count of added pair
                return self.all[k].count
        # if the word-pair is new, add it to the list
        wordpair = Wordpair(pair, 1)
        self.all.append(wordpair)
        self.size += 1
        # list of pairs, number of pairs, count of new pair
        return wordpair.count

"class PairList: "
class ChunkPairList:
    def __init__(self):
        self.all = [] # List, containing ChunkPair-objects
        self.size = 0 # An integer-value, how many ChunkPairs are stored in the ChunkList

    # updates chunkpair list,  by adding pair if it is new, or increasing the count of an already stored ChunkPair
    def update_pairlist(self, pair):
        temp_pair = ""
        if isinstance(pair[0], Chunk):
            temp_pair += str(pair[0].ortho)
        else:
            temp_pair += str(pair[0])
        temp_pair += str(pair[1].ortho)

        for k in range(0, self.size):
            # if the word-pair is already in the list, adjust the associated word-pair count
            temp = ''
            if isinstance(self.all[k].ortho[0], Chunk):
                temp += str(self.all[k].ortho[0].ortho)
            else:
                temp += self.all[k].ortho[0]
            temp += str(self.all[k].ortho[1].ortho)
            if temp == temp_pair:
                # print 'match found'
                self.all[k].count += 1
                # list of pairs, number of pairs, count of added pair
                return self.all[k].count
        # if the word-pair is new, add it to the list
        wordpair = Wordpair(pair, 1)
        self.all.append(wordpair)
        self.size += 1
        # list of pairs, number of pairs, count of new pair
        return wordpair.count

# For production task
class Utterance:
    def __init__(self):
        self.ortho = None # String, orthographic representation of Utterance
        self.num = None # Integer-value, utterance number
        self.skipped = False # Boolean, False if Utterance is not skipped during reconstruction
        self.reconstructed = None # Boolean, True if Utterance is reconstructed correctly
        self.bag_of_ortho = None # List, shuffled Chunks build from utterance
        self.prediction = None # String, reconstructed version of the utterance


# For production task
class allUtterances:
    def __init__(self):
        self.all = [] # List, containing Utterance-objects
        self.size = 0 # Integer-value, number of Utterances stored in allUtterances


"----------------------------"
"functions outside of classes"
"----------------------------"

# Computes the backward transitional probability between a pair of Words with w_count: word count and p_count: pair count
def compute_btp(w_count, p_count):
    # BTP = P(Y|x) = frequency XY / frequency Y
    assert (p_count >= 0)
    assert (w_count >= 0)
    if w_count == 0:
        return 0
    btp = p_count / w_count
    #
    return btp

# Reads in corpus files, filename: corpus file, sfilename: size of corpus file and returns the corpus, number of utterances,
# and words in it, and average, minimum and maximum utterance lengths
def fileread(filename, sfilename):
    MIN = float('Inf')
    MAX = 0
    # reads in the size of the corpusfile from a -.txt file
    sfile = open(sfilename, "r")
    NUM_UTTERANCES = int(sfile.readline())
    sfile.close()
    # reads in '\n'-separated utterances from -.txt file
    file = open(filename, "r")
    corpus = np.ndarray((NUM_UTTERANCES,), dtype=np.object)
    NUM_WORDS = 0

    # reads in the utterances one by one and save each utterance as a list of Word-objects
    for i in range(0, NUM_UTTERANCES):
        temp = file.readline()
        temp = temp.split()
        u = np.ndarray((len(temp),), dtype=np.object)
        NUM_WORDS += len(temp)

        if MIN > len(temp):
            MIN = len(temp)
        if MAX < len(temp):
            MAX = len(temp)

        for j in range(0, len(temp)):
            w = Word(temp[j], None)
            u[j] = w
        corpus[i] = u
    file.close()

    # computes the average utterance length
    AVERAGE_UT_LENGTH = NUM_WORDS / NUM_UTTERANCES
    UT_MEASURES = [AVERAGE_UT_LENGTH, MIN, MAX]
    #print UT_MEASURES # uncomment if you want to print the utterance measures
    return corpus, NUM_UTTERANCES, NUM_WORDS, UT_MEASURES


# returns whether Pair-object pair is a subset of Chunk-object chunk
def findsubset(chunk, pair):
    for subtuple_length in reversed(xrange(1, len(chunk))):
        for start_index in xrange(0, (len(chunk) + 1 - subtuple_length)):
            temp = chunk[start_index:start_index + subtuple_length]
            if temp == pair:
                return True
    return False


# returns the running average backward transitional probability from a list of backward transitional probabilities
def averageBTP(btps):
    # average is set to zero when there are no btp's calculated yet
    if len(btps) == 0:
        average = 0
    else:
        average = np.mean(btps)
    return average

# determines the chunks in the corpus
def chunk_corpus(corpus):
    # initializations
    chunks = ChunkList()
    ortho = WordList()
    pairs = PairList()
    btps = []
    chunkpairs = ChunkPairList()

    utteranceThreshold = NUM_UTTERANCES / 5
    #print utteranceThreshold

    # loop through all utterances in the corpus one-by-one
    for i in range(0, NUM_UTTERANCES):

        if i%1000 == 0:
            print("currently processing utterance: "  + str(i))

        utterance = corpus[i]
        # reset chunk for each utterance
        chunk = Chunk()
        previous_chunk = ''

        # update word count for first word in utterance, compensation for for-loop
        word = utterance[0].ortho
        w_count = ortho.add_word(word)

        # for the second-until-the-last word in the utterance
        for j in range(1, len(utterance)):
            #update word and wordpair count for current word
            word = utterance[j].ortho
            w_count = ortho.add_word(word)
            pair = [utterance[j - 1].ortho, utterance[j].ortho]
            p_count = pairs.update_pairlist(pair)

            # compute running average backward transitional probability
            average = averageBTP(btps)

            # compute backward transitional probability of current word pair
            btp = compute_btp(w_count, p_count)
            btps.append(btp)

            # search for chunks in utterances
            # when chunkatory is empty
            if chunks.size == 0:
                # add first word of pair to chunk, but do not insert boundary yet

                chunk.ortho = chunk.ortho + [pair[0]]
                # if btp indicates boundary
                if btp <= average:
                    # add chunk to chunk-array
                    chunk.count = 1
                    chunks.all.append(chunk)
                    chunks.size += 1
                    chunkpair = [previous_chunk, chunk]
                    c_count = chunkpairs.update_pairlist(chunkpair)
                    previous_chunk = chunk
                    # reset chunk
                    chunk = Chunk()
                    continue

            # when chunkatory is not empty
            else:
                # count how often pair (is part of) an already stored chunk
                count, match = chunks.count_chunk(pair)
                # if pair has been seen at least twice before as (part of) a chunk
                if count > 2:
                    # no perfect match in the dictionary, but count is high enough
                    if match is None:
                        # add chunk to chunkatory, and frame to frame dictionary
                        chunk.ortho = pair
                        chunk.count = 1
                        chunks.all.append(chunk)
                        chunks.size += 1
                        chunkpair = [previous_chunk, chunk]
                        c_count = chunkpairs.update_pairlist(chunkpair)
                        previous_chunk = chunk
                    # if perfect match was found
                    else:
                        # up count of existing matching chunk
                        chunks.all[match].count += 1
                        chunkpair = [previous_chunk, chunk]
                        c_count = chunkpairs.update_pairlist(chunkpair)
                        previous_chunk = chunks.all[match]
                    # reset chunk
                    chunk = Chunk()

                # if the pair is not (subset) of stored chunk (new chunk)
                else:
                    # add first word of pair to chunk, but do not store in chunkatory yet
                    chunk.ortho = chunk.ortho + [pair[0]]
                    stop = False
                    # if btp indicates boundary
                    if btp <= average:
                        # check whether chunk is already in chunkatory
                        for k in range(0, chunks.size):
                            if chunks.all[k].ortho == chunk.ortho:
                                chunks.all[k].count += 1
                                chunkpair = [previous_chunk, chunk]
                                c_count = chunkpairs.update_pairlist(chunkpair)
                                previous_chunk = chunks.all[k]
                                # reset chunk
                                chunk = Chunk()
                                stop = True
                                break
                        # if chunk is new to the chunkatory
                        if not stop:
                            # add chunk
                            chunk.count = 1
                            chunks.all.append(chunk)

                            # for production task
                            chunkpair = [previous_chunk, chunk]
                            c_count = chunkpairs.update_pairlist(chunkpair)
                            previous_chunk = chunk
                            chunks.size += 1
                            chunk = Chunk()
    return chunks, chunkpairs, ortho

## utterance production task from McCauley (2011) paper ##
# reconstructing child utterances from the corpus using the chunks and TPs discovered in the caregivers' utterances
def production_task(child_corpus, chunks, chunkpairs):

    # initialization of variable to store all (reconstructed) utterances
    utterances = allUtterances()

    # determine size of largest chunk in the corpus
    maxlen_chunk = 0
    for i in range(0, chunks.size):
        if maxlen_chunk < len(chunks.all[i].ortho):
            maxlen_chunk = len(chunks.all[i].ortho)

    # loop through all child utterances one-by-one
    for i in range(0, NUM_UTTERANCES):
        # save child utterance in new variable

        if i%1000 == 0:
            print("Currently processing utterance: "  + str(i))

        utterance = child_corpus[i]
        ut = Utterance()
        ut.num = i
        bag_of_chunks = []

        # adjust child utterance formatting
        u = []

        for j in range(0, len(utterance)):
            u.append(utterance[j].ortho)
        copy_u = deepcopy(u)

        # making sure n (size of utterance matching chunk the algorithm searches for) isn't larger than the size of the utterance
        n = min(maxlen_chunk, len(utterance))

        # until utterance is found completely, or the size of the matching chunk that is sought for is negative, search for chunks that match
        # (part of) the child utterance
        while not len(u) == 0: #and not n <= 0:  #Comment AND-out for handling new words
            # reset stop-criterion and make copy of (part of) utterance
            stop = False
            temp_u = u[0:n]

            #when no chunk match is found, make new chunk with btp of 0.0 to everything
            """
            Comment this n==0 part out for running without handling new words"
            """

            if n == 0:
                new_chunk = Chunk()
                new_chunk.ortho = [u[0]]
                new_chunk.count = 0
                bag_of_chunks.append(new_chunk)
                u = u[1:]
                n = min(maxlen_chunk, len(u))


            # loop through all chunks to find a match with the utterance copy
            for j in range(0, chunks.size):
                # check for a (partial) match
                if chunks.all[j].ortho == temp_u:
                    # add chunk to bag of words and remove matching part from utterance
                    chunk = chunks.all[j]
                    bag_of_chunks.append(chunk)
                    u = u[n:]
                    stop = True
                    # reset n
                    n = min(maxlen_chunk, len(u))
                    break
            # if no match of length n could be found, decrease n to search for a smaller matching chunk
            if not stop:
                if n != 0:
                    n = n - 1

        # store original utterance
        ut.ortho = utterance

        ###Use this code if running without handling new words
        """
        if len(u) > 0:  # continue to next utterance if not all parts of utterances existed as chunks
            ut.skipped = True
            ut.bag_of_chunks = []
            utterances.all.append(ut)
            utterances.size += 1
            continue
        """
        # randomize order of chunks in bag_of_chunks
        shuffle(bag_of_chunks)
        ut.bag_of_chunks = deepcopy(bag_of_chunks)
        previous = ['#'] # beginning of utterance marker
        reconstructed_utterance = []
        first = True # True when searching for which chunk should be the first one in the utterance

        # as long as there still are unordered chunks in the bag of chunks, find the next chunk to rebuild the utterance
        # using the stored btps between chunks
        while not len(bag_of_chunks) == 0:
            max_btp = 0
            selected_chunk = bag_of_chunks[0]

            # determine chunk with highest btp compared to previously selected chunk (or # if first)
            for current in bag_of_chunks:
                # determine pair of previous selected chunk + current chunk c
                pair = []
                if first:
                    if not '#' in current.ortho:
                        pair = ['#'] + current.ortho  # pair = [previously chosen chunk / '#', c]\
                    else:
                        pair = current.ortho
                else:
                    pair = previous + current.ortho

                # determine how often the pair of chunks has been seen in corpus
                current_count = current.count
                if current_count == 0:
                    pair_count = 0
                else:
                    pair_count = determine_pc(pair, chunkpairs)

                # check whether the btp of the current chunk is highest
                temp_btp = compute_btp(current_count, pair_count)
                if max_btp < temp_btp:
                    max_btp = temp_btp
                    selected_chunk = current
                # if equal btps, randomly select old or new chunk
                elif max_btp == temp_btp:
                    rand = np.random.choice(2,1)
                    if rand == 0:
                        selected_chunk = current
            # save the selected chunk in the reconstruction, remove it from bag of chunks and update previous chunk
            reconstructed_utterance.append(selected_chunk)
            bag_of_chunks.remove(selected_chunk)
            previous = [selected_chunk.ortho]
            first = False

        # store reconstructed utterance
        ut.prediction = reconstructed_utterance
        utterances.all.append(ut)
        utterances.size += 1
    return utterances

# returns how often the chunkpair has been seen
def determine_pc(chunkpair, chunkpairs):
    for i in range(0, chunkpairs.size):
        temp = []
        # convert representation
        if isinstance(chunkpairs.all[i].ortho[0], Chunk):
            temp += chunkpairs.all[i].ortho[0].ortho
        else:
            temp += chunkpairs.all[i].ortho[0]
        if isinstance(chunkpairs.all[i].ortho[1], Chunk):
            temp += chunkpairs.all[i].ortho[1].ortho
        else:
            temp += chunkpairs.all[i].ortho[1]
        if chunkpair == temp:
            return chunkpairs.all[i].count
    return 0

# evaluates the reconstructed utterances and saves results in .csv file
def evaluate_and_save(utterances):
    # create outputfile with file header
    outputfilename = child_file.replace('.txt', '_productiontask.csv')
    outputfilename = outputfilename.replace('Data', 'results/noskipping')

    with open(outputfilename, "w") as output:
        writer = csv.writer(output, delimiter=";")
        header = ["num", "utterance", "child", "age", "skipped", "reconstructed", "bow", "gold", "prediction","chance","controlledscore"]
        writer.writerow(header)

        # evaluate reconstructed utterances one-by-by one
        for i in range(0, utterances.size):
            utterance = utterances.all[i].ortho
            num = utterances.all[i].num
            skip = utterances.all[i].skipped
            bow = utterances.all[i].bag_of_chunks
            # if utterance was not skipped because of lack of matching chunks, check if it was reconstructed correctly
            if not skip:
                true_temp = ""
                # determine true utterance
                for j in range(0, len(utterance)):
                    true_temp += str(utterance[j].ortho)
                # determine reconstruction
                reconstructed_temp = ""
                for j in range(0, len(utterances.all[i].prediction)):
                    reconstructed_temp += ''.join(utterances.all[i].prediction[j].ortho)
                #compare true utterance and reconstructed utterance
                if true_temp == reconstructed_temp:
                    utterances.all[i].reconstructed = True
                else:
                    utterances.all[i].reconstructed = False
                gold = true_temp
                prediction = reconstructed_temp
               # bow = utterances.all[i].bag_of_chunks
                reconstructed = utterances.all[i].reconstructed
                n_chunks = len(utterances.all[i].bag_of_chunks)
                chance = 1/(np.math.factorial(n_chunks))
                if utterances.all[i].reconstructed:
                    controlledscore = np.math.log(np.math.factorial(n_chunks))
                else:
                    controlledscore = np.math.log(1 - (1/np.math.factorial(n_chunks)))
            # if utterance was skipped, set all irrelevant variables to NaN
            else:
                gold = 'NaN'
                prediction = 'NaN'
                bow = 'NaN'
                reconstructed = 'NaN'
                chance = 'NaN'
                controlledscore = 'NaN'

            # write utterance data to output file
            row = [str(num), str(utterance), str(CHILD), str(AGE), str(skip), str(reconstructed), str(bow), str(gold),
                   str(prediction),str(chance),str(controlledscore)]
            writer.writerow(row)
    print("Output written to file")
    return

if __name__ == "__main__":
    #change path of location of corpusfiles
    path = os.path.abspath('')
    path = path.replace('/Model/Scripts','')
    path = os.path.join(path, 'Data')

    ##A DJUST THESE TO SELECT CORPUS DATA ###
    TYPE = "a" # or "l"
    CHILD = "William" # Choose between Alex, Ethan, Lily, Naima, Violet and William for Providence corpus, or ArtLg for Artificial Grammar
    AGE = "3_0" #  Choose between 1_6, 2_0, 2_6, 3_0, 3_ or 4_0 for Providence corpus or NVT for Artificial Grammar
    if TYPE == "a":
        LOC = os.path.join(path, 'accumulativesampledcorpus')
    elif TYPE == "l":
        LOC = os.path.join(path, 'localsampledcorpus')
    else:
        LOC = os.path.join(path, 'ArtCorpus')

    caregiver_filename =  LOC + "/" + TYPE + "_corpusProvidence_caregivers_" + CHILD + "_age" + AGE + ".txt" #"ArtLgCorpus.txt"
    caregiver_size_filename =  LOC + "/" + TYPE + "_corpusProvidence_caregivers_size" + CHILD + "_age" + AGE + ".txt"  #"ArtLgCorpus_size.txt"

    child_file = LOC +  "/" + TYPE + "_corpusProvidence_child" + CHILD + "_age" + AGE + ".txt"  # or: "ArtLgcorpus_child.txt"
    child_size_file = LOC + "/" + TYPE + "_corpusProvidence_child_size" + CHILD + "_age" + AGE + ".txt"  # or: "ArtLgcorpus_child_size.txt"

    print ('Start' + CHILD + AGE + TYPE)

    corpus, NUM_UTTERANCES, NUM_WORDS, PHRASE_MEASURES = fileread(caregiver_filename, caregiver_size_filename)
    NUM_PAIRS = 2 * NUM_WORDS
    NUM_CHUNKS = NUM_PAIRS
    NUM_FRAMES = NUM_PAIRS

    #NUM_UTTERANCES = min(200, NUM_UTTERANCES)  # for testing code with subset of data
    print NUM_UTTERANCES

    print("\nFile read")
    chunks, chunkpairs, all = chunk_corpus(corpus)
    print("\nChunking complete")

    child_corpus, NUM_UTTERANCES, NUM_WORDS, PHRASE_MEASURES = fileread(child_file, child_size_file)
    #NUM_UTTERANCES = min(200, NUM_UTTERANCES)  # for testing code with subset of data

    print("\nChild file read")
    print NUM_UTTERANCES
    utterances = production_task(child_corpus, chunks, chunkpairs)
    print("\nProduction task completed")
    evaluate_and_save(utterances)
    print("Program done")
