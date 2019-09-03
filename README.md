## Text Module

Hi! Thanks for taking some time to actually read this. This project, "Text-Module", for the lack of a better name is something I did during the summer break after my first year and took me about 10 days to complete it. Here is some information about the project, which will help you to get started.

### What does it do?

I tried to make a text-typing system (similar to the one in our smartphones) which could auto-complete pending words, suggest the next word as well as suggest corrections to incorrectly spelt words.

### Some technical details.

Alright, so initially the "dictionary" of the program is empty and it gets updated everytime you hit ENTER. The system has been designed in such a way that it does not differentiate between CAPS and small alphabets. However during sugestion, it does suggest to start off the sentence with a CAPITAL letter. Also to end the sentence I've only kept full-stop(.) as the indicator which can easily be extended to other symbols such as ?!, etc. Also, there is no support for commas, hiphens and numerals. My main aim was to build the logic of it without worrying about minutes. Therefore, the front end is also, so as to speak, just okay...-ish. I didn't want to use a library to design textboxes and did it using a simple drawing library using just lines and colors. Nothing much fancy there.

To register incorrect words, I've considered a few basic, logical and common errors :-
1. Missed a single alphabet.
2. Added some extra alphabets here and there (max 2 errors detected)
3. Got some alphabets wrong (depending upon the total length of word, so it's pretty cool ;))
4. Shuffled the words (till a distance of 2 words)

I wanted to use the Functional Programming Paradigm to make the whole thing and so used Scheme (or Racket) to do all the coding.

The data is updated dynamically and uses only the frequentist approach to guess. No Machine Learning was used in this project. The whole dictionary words, along with their frequencies have been stored using the TRIE data structure (I modified it a little to make things easier). It would have been more efficient to not make the modification if you were using C/C++/Java, but I don't think it matters much in Racket. I used a standard table to record the frequencies of what words follow each word. Better alternatives can be used here, but were hard (seemingly infeasible) in Racket.

### Files in the Folder

The only file you actually need is "Final.rkt" (and of course the readme file). The "data.rkt" just contains a few formatted sentences which can be easily added to the dictionary to start off. Or you can otherwise start off with empty dictionary and make your own by typing in sentences. Multiple sentences can be typed in at once before hitting enter. The files in the development_files are the temporary files that I used to make the individual segments of the code.

### Running the program

You will need to download a free Scheme/Racket interpreter "Dr. Racket". Run the "Final.rkt" file in that and type (run-module) (with brackets) in the console and you will be all set to go. You should see a GUI screen after hitting enter.

