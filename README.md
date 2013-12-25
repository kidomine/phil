## Written in Haskell.
###The extension of my brain.

# How to use

### Set up
<pre>
mongod --dbpath mongoFiles
Make sure the mongoFiles folder is backed up
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
done 823
</pre>

### Complete
<pre>
g todo created today
done 823
</pre>


### Create
<pre>
todo p3 prog 0 Read the research papers
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

### Update
alarm todo every day at 10pm

## note
### Create
<pre>
note Omgomgomgo I'm soooo excited for Winter quarter!
</pre>

### Search

<pre>
tags are lowercase (or numbers)
keywords to search on begin at the first capitalized word
searches are all not case-sensitive

g note 228 Bayes l2
g note Haha (searches by content)
g note haha (searches by tag)
</pre>

## list

### Search
<pre>
g list fall asleep
g list makes me happy
g list things i love
</pre>

## reminder
<pre>
remind 3d todo 4
remind 4d Thank stephen
remind daily Take vitamins
remind daily quote 8881
</pre>

## Search

<pre>
g reminders due tomorrow
</pre>

## fc
### Create
<pre>
fc haskell Define a monad
fc 144 sleep What are examples of L3 attacks?
</pre>

(This reminds me of notes I should study in an
exponential-backoff fashion)
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
d fc 2
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

### Create
<pre>
1/1 2pm - 2:15pm home Call Marie
</pre>

### Delete
<pre>
d event 2
</pre>

# Other operations

### Modes
<pre>
mode (quiet|loud|silent)
Quiet - don't show any secret notes
Loud - show secret notes
Silent - don't send reminders
</pre>

### Top secret
<pre>
This means a password will be
requested before the information
is released.
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

### Saved searches
create saved search school = tags school 221 229 144

### Stats
<pre>
stats outstanding todo p1
stats percent flashcards correct today
stats days kept goal 8
stats vector for sleep
stats
</pre>

### vi
<pre>
the vi command will let me enter the text in
vim rather than the command line. like when github
goes into the commit message mode.
</pre>


### Creating dates
<pre>
date break ends = Jan 4
</pre>

### Updating dates
<pre>
date break ends = Jan 5
> Are you sure you want to change the date break ends from Jan 4 to Jan 5?
> yes
</pre>


### Deleting dates
<pre>
d date break ends
</pre>

### Undoing deletion
<pre>
undo
undo in last 100
> [shows a list of last 100 operations, so I'll keep a log of everything that's
   done, and when something is selected, I'll simply do the inverse of what
   was done and it will show up in the log]
</pre>

### Quantified Self
<pre>
start survey
> How many hours of sleep did you get last night?
</pre>

## Goal
### Create
<pre>
goal remind daily Take vitamins
</pre>

## Question
<pre>
This asks me questions every once in a while, like...
question What are 5 things you're grateful for?
question What do you want to do in three years?
</pre>


### Tags
<pre>
reading through tags looks at the next tag to see if it's a part of this tag
before declaring it a new tag. E.g. south african columbae math
breaks that up as 'south african' 'columbae' and 'math'
</pre>

### Search
<pre>
a capitalized word means all of this word and the next words are in the
content -- they're not tags.
</pre>
