# manifest-search

A Common Lisp library to full text index common lisp documentation.  
The goal is to be able to search all of the online documentation of 
a running lisp system effectively.

Currently searches are run interactively with results being printed
to the REPL (stdout).  Eventually I would like to integrate this 
more tightly to manifest.

## Rationale

I (like I am guessing many) have at least one giant-ball-of-mud
utility library with lots of interesting but not well organized and
perhaps poorly named functionality.  I hope to make this code easier 
to organize, use and find even in the face of poor naming (assuming
documentation was better than the name).

This library is a direct answer to a co-workers complaints of being
unable to find (and thus use) functionality from libraries he is
unfamiliar with.  Hopefully it will evolve into a tool that (with
manifest) allows one to search for topics and receive documentation
about relavant functions.


## Dependancies

 * Montezuma provides full text indexing
 * Manifest provides the documentation gathering facilities
 * Other utility libs

## Example

The following will index all loaded packages and then search for 
things relevant to plist

```
;; adds all documentation for the running lisp to the search index
MANIFEST-SEARCH> (index-packages) 

;; searches all docs for ones that MUST contain plist
MANIFEST-SEARCH> (search-manifest "+plist")

NET.ACCELERATION.UTILS:OBJECT->PLIST <FUNCTION : 1.785362> 
    Converts an objects direct slots to a plist of (reader-name value ...) pairs 

ALEXANDRIA.0.DEV:PLIST-ALIST <FUNCTION : 1.7305975> 
    Returns an association list containing the same keys and values as the
property list PLIST in the same order.

ALEXANDRIA.0.DEV:DELETE-FROM-PLIST <FUNCTION : 1.6973867> 
    Just like REMOVE-FROM-PLIST, but this version may destructively modify the
provided plist.

NET.ACCELERATION.UTILS:INTERN-PLIST-KEYS <FUNCTION : 1.6199379> 
    interns plist keys to the specified package and returns a
   new plist of those keys and the original values

ALEXANDRIA.0.DEV:REMOVE-FROM-PLIST <FUNCTION : 1.4572165> 
    Returns a propery-list with same keys and values as PLIST, except that keys
in the list designated by KEYS and values corresponding to them are removed.
The returned property-list may share structure with the PLIST, but PLIST is
not destructively modified. Keys are compared using EQ.

ALEXANDRIA.0.DEV:PLIST-HASH-TABLE <FUNCTION : 1.4063838> 
    Returns a hash table containing the keys and values of the property list
PLIST. Hash table is initialized using the HASH-TABLE-INITARGS.

CLOSER-COMMON-LISP:SYMBOL-PLIST <FUNCTION : 1.3472456> 
    Return SYMBOL's property list.

  ... <More Results Elided> ...

```

## TODO:
 * Saving/Loading the index
 * Refreshing data in the index when it changes or periodically
 * Produce manifest links
 * Add search screen to manifest that ties into this system

## License

```
;; Copyright (c) 2011 Russ Tyndall , Acceleration.net http://www.acceleration.net
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```
