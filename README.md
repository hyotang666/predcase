# PREDCASE 0.0.0 - New CASE like conditional operator.

## Current lisp world
Special operator CL:COND provides strong primitive conditional diverge.

## Issues
CL:COND is too much concrete.
In some cases, more abstruct and specific one is better (e.g. CL:TYPECASE CL:CASE etc...).
And still there is a typical case which should be abstructed.
e.g.
```lisp
(cond
  ((pred1 arg)(do-for-1 ...))
  ((pred2 arg)(do-for-2 ...))
  ...
  ((predN arg)(do-for-n ...)))
```

## Proposal
PREDCASE covor it.

## Usage
```lisp
(predcase arg
  (pred1 (do-for-1 ...))
  (pred2 (do-for-2 ...))
  ...
  (predN (do-for-n ...)))
```

## From developer

### Product's goal
Be merged by other famous utility libraries.
### License
public domain
### Developped with
CLISP
### Tested with
* SBCL/1.5.6
* CCL/1.11.5
* ECL/16.1.3
