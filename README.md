##It's a command line interface to manage notes, flashcards, and todos.
### Written in Haskell.

I used to awkwardly enter these things in GUIs (Evernote, Trello, Things, StudyBlue, etc.) and then had to (or never did) triage. This program makes it easy to enter information, and it triages it for me. It's pretty much a DSL that I choose and change as I see fit. For example,
<pre>
todo school 144 Write router
</pre>
Creates a todo item tagged with `school` and `144` with the title "Write router".

Usually my todo creation is much more simple. Like `todo buy Another laptop charger`

I use `g todo tags` to show me how many items are assigned to each tag, as in:
<pre>
23 school
19 read
16 friends
11 apartment
8 intern
6 math
4 buy
</pre>

My favorite thing about this program is making and testing flashcards really quickly. For example,
`fc haskell Define a monad? A structure that represents computations defined as sequences of steps`
creates a flashcard tagged with `haskell` with the question "Define a monad" and the answer as "A structure...". To test all the flashcards tagged with `haskell`, I say `test 144`
and that prints the Question. I hit enter. It prints the answer. I type "y" or "n" depending on whether I got it right or wrong, and it records the score.

#### Anyway,
what follows is a bucket list of things I can create, and actions I can perform, including:

- Search (by tags)
- Edit
- Study flashcards
- Delete

### Todos
<pre>
todo someday Learn to play guitar
todo school Read the research papers
</pre>

### search
<pre>
g todo (returns a list of 1 - mytodo here)
g todo done between one week ago and now
g todo read
g todo stay-current-on
g todo travel
g todo at-whole-foods
g todo created
   1 - 4 minutes ago - Alrighty now
   2 - 5 seconds ago - Hey
g todo school
g todo 144
g todo p1 router
g todo daily
g todo 228 Pset
</pre>

### edit
<pre>
e 2 (The 2 is the second result of the last search)
This lands you in vi, and you can edit any fields of the item, for any item type
</pre>

### Notes
If no word "note" is supplied, and only tags are supplied, phil creates a note
with those tags
<pre>
note Omgomgomgo I'm soooo excited for Winter quarter!
quote Great men make history? Nah, history makes great men.
</pre>

### search

<pre>
tags are lowercase (or numbers)
when the note you want to get just has one tag, you don't
need to use "g note tag". Just use 'g tag'.

g 228 Bayes l2
g note haha (searches by tag)
g day
</pre>

### Flashcards
<pre>
Format:
fc tags ai AnswerImageFilename qi QuestionImageFilename Question ending in a question mark? answer

fc haskell Define a monad? A structure that represents computations defined as sequences of steps
fc 144 What are the 3 main types of error correction?
fc 229 ai matrix-derivatives What are the 3 main types of error correction?
Checksum: adds up values in packet (IP, TCP)
Cyclic Redundancy Check (CRC) (Ethernet): Protects against any 2 bit error, any burst less than c bits long, and any odd number of errors. Can't detect all errors: 2^c chance another packet's CRC matches.
MAC (Message Authentication Code): Not as good for error detection as CRC

In that last one, I used vim to edit the flashcard.
I store the images for my flashcards in a single folder in Dropbox.
Whenevery I create flashcards, I just take a screenshot of the slide or equation, rename the image, and drag it to my flashcards folder. Simple and easy.
</pre>

### Review
<pre>
review 144

1 - What are common L3 attacks?
    BGP Hijacking
    ICMP redirects
    ...
</pre>

### delete
<pre>
d 2 (after searching for fc)
</pre>

### Test
<pre>
test 144
test 144 reverse
test 144 hardest first (not implemented yet)
> What should the buffer size be?
Hit return to see the answer
> B * RTT
Hit return if you got it right, "n" if you
got it wrong.

You can edit a card that you are currently
testing by entering "e" when you see the
answer and usually hit return or "n".
</pre>

### View the log
<pre>
To see the last n lines appended to the log, run
log 15
</pre>

### Tags

`g type tags` shows all the tags and the frequency with which they are assigned to items
of that type. For example:
g todo tags
shows:

<pre>
12 math
9 waiting
6 friends
5 read
4 song
4 school
4 learn-random
4 coding
4 buy
3 apartment
3 book
2 someday
2 research
2 learn
2 daily
2 buy-online
2 fam
2 whenever
2 229
2 221
2 144
1 safeway
1 pleasure
1 228
</pre>

See the tags that are assigned to items with
g type with tags

where tags is literally "tags"
and type can be any item type.

<pre>
1 - [learn-random] - What is SEO?
2 - [this-project] - Enable text search
3 - [daily] - Write down 5 things I'm thankful for
4 - [hungry] - Clean out email. Killme.
5 - [math] - Do Project Euler problems
6 - [228] - Read a probabilistic graphical modeling textbook
7 - [math, book] - Read The Art and Craft of Problem Solving
8 - [math, learn] - Deeply understand how matrix inverses work
9 - [friends] - Plan to go to Burning Man
10 - [apartment] - Clean room
11 - [apartment] - Buy a tall office chair
12 - [whenever] - Cut hair
13 - [math, book, learn] - Read a book on convex optimization
14 - [229] - Finish watching 229_23.mp4 on Markov Models
15 - [whenever] - Make sure Mail On My Mac is backed up on Dropbox.
16 - [someday] - Learn to draw a Rose
17 - [friends] - Plan Spring Break
</pre>

# How to set up

Change this line in `phil/src/Main.hs`:
	<pre>
	main = do
       config <- readConfig "/Users/rose/phil/src/phil.cfg"
	</pre>
to
	<pre>
	main = do
       config <- readConfig "/path-to-your-copy-of-the-project/phil/src/phil.cfg"
	</pre>

I use this bash alias to build and test: `alias cab='cabal configure --enable-tests; cabal build; cabal test`

Start the database with this:
<pre>
cd ~/phil; mongod --dbpath mongoFiles
(My alias for that is phildb)
Make sure the mongoFiles folder is backed up
A log of all text input to the program is
located at ~/Desktop/Dropbox/log.txt.
</pre>

### Backup the db

I use the following bash command to back up the db. It has a tiny footprint compared to all the content it seems to contain, because I keep flashcard images in a separate Dropbox folder.

<pre>
function md() {
    cd ~/Desktop/Dropbox/PhilBackups
    _now=$(date +%F--%T)
    mkdir "$_now"
    mongodump --out="$_now"
    echo "Backed up to ~/Desktop/Dropbox/PhilBackups/dump-$_now"
}
</pre>

To restore, run `mongorestore`
