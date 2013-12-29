## Written in Haskell.
###The extension of my brain.

# How to use

### Set up
<pre>
I use this bash alias:
alias cab='cabal configure --enable-tests; cabal build; cabal test'

mongod --dbpath mongoFiles --setParameter textSearchEnabled=true
Make sure the mongoFiles folder is backed up
A log of all text input to the program is
located at ~/Desktop/Dropbox/log.txt.
</pre>

### Backup the db

I use the following bash command to back up the db.

<pre>
function md() {
    cd ~/Desktop/Dropbox/PhilBackups
    _now=$(date +%F--%T)
    mkdir "$_now"
    mongodump --out="$_now"
    echo "Backed up to ~/Desktop/Dropbox/PhilBackups/dump-$_now"
}

To restore, run mongorestore
</pre>

### Abberviations used in this guide
* g means get
* everything until the first capital letter is metadata
all arguments until next metadata tag are arguments
to that metadata tag
* fc stands for flashcard

## Types

### Todos
<pre>
todo someday Learn to play guitar
</pre>

### Complete
<pre>
done 11 (after searching for todo's. Delete the 11th item)
</pre>

### Create
<pre>
todo p3 school Read the research papers
</pre>

### Search
<pre>
g todo (returns a list of 1 - mytodo here); d 3
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
g todo created before 2 days ago
g todo daily
g todo by 1/4
g todo by tomorrow
g todo 228 Pset
</pre>

### Edit
<pre>
e 2 (The 2 is the second result of the last search)
This lands you in vi, and you can edit any fields of the item, for any item type
</pre>

## note
<pre>
note Omgomgomgo I'm soooo excited for Winter quarter!
</pre>

### Search

<pre>
tags are lowercase (or numbers)
keywords to search on begin at the first capitalized word
searches are all not case-sensitive

g note 228 Bayes l2
g todo Haha (searches by content)
g note haha (searches by tag)
</pre>

## reminder
<pre>
remind 3d todo 4
remind 4d Thank stephen
remind daily Take vitamins
remind daily quote 8881
</pre>

## fc
<pre>
fc <tags> <Question ending in a question mark>? <answer>

fc haskell Define a monad? A structure that represents computations defined as sequences of steps
fc 144 What are the 3 main types of error correction?
Checksum: adds up values in packet (IP, TCP)
Cyclic Redundancy Check (CRC) (Ethernet): Protects against any 2 bit error, any burst <= c bits long, and any odd number of errors. Can't detect all errors: 2^c chance another packet's CRC matches.
MAC (Message Authentication Code): Not as good for error detection as CRC

In that last one, I used vim to edit the flashcard.
</pre>

### Review
<pre>
review 144

1 - What are common L3 attacks?
    BGP Hijacking
    ICMP redirects
    ...
</pre>

### Delete
<pre>
d 2 (after searching for fc)
</pre>

### Test
<pre>
test 144
test 144 created yesterday
test 144 hardest first
> What should the buffer size be?
Hit return to see the answer
> B * RTT
Hit return if you got it right, "n" if you
got it wrong.
</pre>

## Event

<pre>
1/1 2pm - 2:15pm home Call Marie
</pre>

### Show options for metadata
<pre>
g types
    returns:
        todo
        event
        note
        goal
        fc
</pre>

## Goal
<pre>
goal remind daily Take vitamins
goal Brush three times a day
</pre>

### Test
<pre>
test goals
When a goal is shown, type "y" if you made it, "n" otherwise.
</pre>

### Search
<pre>
a capitalized word means all of this word and the next words are in the
content -- they're not tags.
</pre>

### View the log
<pre>
To see the last n lines appended to the log, run
log 15
</pre>

### Tags

<pre>
g <type> <tags>
shows all the tags and the frequency with which they are assigned to items
of that type. For example:
g todo tags
shows:

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

See the tags that are assigned to items with
g <type> with tags

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
