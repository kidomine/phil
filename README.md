##The extension of my brain.
### Written in Haskell.

It's a command line interface to manage

- Todo's
- Notes
- Flashcards
- Goals
- Calendar events

I used to awkwardly enter these things in GUIs (Evernote, Trello, Things, StudyBlue, Reminders, FollowUpThen, iCal, etc.) and then had to (or never did) triage. This program makes it easy to enter information, and it triages it for me. It's pretty much a DSL that I choose and change as I see fit. For example,
<pre>
todo school 144 p2 by 1/4 Write router
</pre>
Creates a todo item tagged with `school` and `144`, priority 2, and sets a due
due date of January 4, 2014, with the title "Write router".

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

I use `g todo < p2` to show me all todos that are of priority 1 or 2.

I use `g todo by 1/5` to show me everything due before January 5.

I gotta blurt out that my favorite thing about this program is making and testing flashcards really quickly. For example,
`fc haskell Define a monad? A structure that represents computations defined as sequences of steps`
creates a flashcard tagged with `haskell` with the question "Define a monad" and the answer as "A structure...". To test all the flashcards tagged with `haskell`, I say `test 144`
and that prints the Question. I hit enter. It prints the answer. I type "y" or "n" depending on whether I got it right or wrong, and it records the score.

#### Anyway,
what follows is a bucket list of things I can create, and actions I can perform, including:

- Search (by tags, priorities, content)
- Edit
- Complete (as in todo's)
- Study flashcards
- Record (yes/no) progress on daily goals
- Remind to do stuff
- Delete

### Todos
<pre>
todo someday Learn to play guitar
todo p3 school Read the research papers
</pre>

### complete
<pre>
done 11 (after searching for todo's. Delete the 11th item)
</pre>

### search
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

### edit
<pre>
e 2 (The 2 is the second result of the last search)
This lands you in vi, and you can edit any fields of the item, for any item type
</pre>

### Notes
<pre>
note Omgomgomgo I'm soooo excited for Winter quarter!
</pre>

### search

<pre>
tags are lowercase (or numbers)
keywords to search on begin at the first capitalized word
searches are all not case-sensitive

g note 228 Bayes l2
g todo Haha (searches by content)
g note haha (searches by tag)
</pre>

### Reminders
<pre>
remind 3d todo 4
remind 4d Thank stephen
remind daily Take vitamins
</pre>

### Flashcards
<pre>
Format:
fc tags Question ending in a question mark>? answer

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

### delete
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

### Event
<pre>
1/1 2pm - 2:15pm home Call Marie
</pre>

### Show available types
<pre>
g types
        todo
        event
        note
        goal
        fc
</pre>

### Goal
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


# How set up

I use this bash alias to build and test: `alias cab='cabal configure --enable-tests; cabal build; cabal test`


Start the database with this:
<pre>
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
</pre>

To restore, run `mongorestore`


