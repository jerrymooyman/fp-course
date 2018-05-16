{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: FilePath -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Tuple Functions that could help --

  fst :: (a, b) -> a
  snd :: (a, b) -> b
  (,) :: a -> b -> (a, b)

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Consideration --
  Try to avoid repetition. Factor out any common expressions.
  
Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ... 
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- Given the file name, and file contents, print them.
-- Use @putStrLn@.
printFile :: FilePath -> Chars -> IO ()
printFile p c = do
  putStrLn p
  putStrLn c
  return ()
  -- error "todo: Course.FileIO#printFile"

-- Given a list of (file name and file contents), print each.
-- Use @printFile@.
printFiles :: List (FilePath, Chars) -> IO ()
printFiles Nil = return ()
printFiles (x:.xs) = 
  let (p, c) = x in
    do
        printFile p c 
        printFiles xs
        return ()
  -- error "todo: Course.FileIO#printFiles"

-- Given a file name, return (file name and file contents).
-- Use @readFile@.
getFile :: FilePath -> IO (FilePath, Chars)
getFile p = do
  c <- readFile p
  return (p, c)
  -- error "todo: Course.FileIO#getFile"

-- Given a list of file names, return list of (file name and file contents).
-- Use @getFile@.
getFiles :: List FilePath -> IO (List (FilePath, Chars))
getFiles Nil = return (Nil)
getFiles (x:.xs) = do
  getFiles xs >>= (\a -> (getFile x) >>= (\b -> return (b:.a)))

  -- let y = (foldLeft(\a c -> ((getFile c)) ++ a) Nil l)
  -- return (y)
  -- error "todo: Course.FileIO#getFiles"

-- Given a file name, read it and for each line in that file, read and print contents of each.
-- Use @getFiles@ and @printFiles@.a
run :: FilePath -> IO ()
run p = do
  (_, c) <- getFile p -- get initial file contents
  a <- getFiles $ lines c  -- get contents from files listed in initial file
  printFiles a
  -- error "todo: Course.FileIO#run"

-- /Tip:/ use @getArgs@ and @run@
main :: IO ()
main = do
  a <- getArgs
  case a of
    f:.Nil -> run f
    _ -> putStrLn "usage: runhaskell io.hs filename"

  -- error "todo: Course.FileIO#main"

----

-- Was there was some repetition in our solution?
-- ? `sequence . (<$>)`
-- ? `void . sequence . (<$>)`
-- Factor it out.
