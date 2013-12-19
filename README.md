How to use
================

Set up
================
start mongod
mongod --dbpath mongoFiles
Make sure the mongoFiles folder is backed up

Abberviations used in this guide
================
g means get
everything until the first capital letter is metadata
all arguments until next metadata tag are arguments
to that metadata tag
fc stands for flashcard

Types
===========
===========
===========

Todos
================
todo someday Learn to play guitar
done 823

Complete
--------
g todo created today
done 823


Create
--------
todo p3 prog 0 Read the research papers

View
--------
g todo (returns a list of 1 - mytodo here); d 3
g todo done between one week ago and now
g todo read
g todo stay current on
g todo travel to
g todo at whole foods


Update
--------
alarm todo every day at 10pm

Search
--------
g todo school
g todo 144
g todo p1 router
g todo created before 2 days ago
g todo daily
g todo due before break ends

note
=============
Create
--------
note Omgomgomgo I'm soooo excited for Winter quarter!

Search
---------


list
===============

Search
g list fall asleep
g list makes me happy
g list things i love

reminder
===========
remind 3d todo 4
remind 4d Thank stephen
remind daily Take vitamins
remind daily quote 8881

Search
-------
g reminders due tomorrow

fc
=============
Create
-----------
fc haskell Define a monad
fc 144 sleep What are examples of L3 attacks?

(This reminds me of notes I should study in an
exponential-backoff fashion)
Review
---------
review 144

1 - What are common L3 attacks?
    BGP Hijacking
    ICMP redirects
    ...

Delete
---------
d fc 2

Test
---------
test 144
test 144 created yesterday
test 144 hardest first
> What should the buffer size be?
  (I hit the space bar to see the answer)
(y|n) depending on whether I got it right or wrong

Quote
=========
Create
--------
quote said by some dude Generalizations are generally wrong

(said by is where I heard it (e.g. Cameron Green). "by" alone means the acutal
author, e.g. Benjamin Franklin

Search
--------
quote text generalization

Delete
----------
d quote 90


Other operations
============
============
============

Phil modes
================
mode (quiet|loud|silent)
Quiet - don't show any secret notes
Loud - show secret notes
Silent - don't send reminders

Top secret
================
This means a password will be
requested before the information
is released.

Show options for metadata
================
g types
    returns:
        todo
        cal
        note
        haha
        quote
        people
        goal
        survey
        question
        fc
        reminder


Saved searches
================
create saved search school = tags school 221 229 144

Stats
=====
stats outstanding todo p1
stats percent flashcards correct today
stats days kept goal 8
stats vector for sleep
stats

vi
================
the vi command will let me enter the text in
vim rather than the command line. like when github
goes into the commit message mode.


Creating dates
-----------------
date break ends = Jan 4

Updating dates
-----------------
date break ends = Jan 5
> Are you sure you want to change the date break ends from Jan 4 to Jan 5?
> yes

Deleting dates
-----------------
d date break ends

Undoing deletion
------------------
undo
undo in last 100
> [shows a list of last 100 operations, so I'll keep a log of everything that's
   done, and when something is selected, I'll simply do the inverse of what
   was done and it will show up in the log]

Quantified Self
================
start survey
> How many hours of sleep did you get last night?

Goal
==============
Create
-------
goal remind daily Take vitamins

Question
==============
This asks me questions every once in a while, like...
question What are 5 things you're grateful for?
question What do you want to do in three years?


Tags
==============
reading through tags looks at the next tag to see if it's a part of this tag
before declaring it a new tag. E.g. south african columbae math
breaks that up as 'south african' 'columbae' and 'math'

Search
==============
a capitalized word means all of this word and the next words are in the
content -- they're not tags.
