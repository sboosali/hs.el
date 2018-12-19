# Notes

## 




## `filepath` (Haskell)

See <http://hackage.haskell.org/package/filepath-1.4.2.1/src/System/FilePath/Internal.hs>


```haskell
---------------------------------------------------------------------

(</>) :: FilePath -> FilePath -> FilePath 
infixr 5

Combine two paths with a path separator. If the second path starts with a path separator or a drive letter, then it returns the second. The intention is that readFile (dir </> file) will access the same file as setCurrentDirectory dir; readFile file.
Posix:   "/directory" </> "file.ext" == "/directory/file.ext"
Windows: "/directory" </> "file.ext" == "/directory\\file.ext"
         "directory" </> "/file.ext" == "/file.ext"
Valid x => (takeDirectory x </> takeFileName x) `equalFilePath` x
Combined:
Posix:   "/" </> "test" == "/test"
Posix:   "home" </> "bob" == "home/bob"
Posix:   "x:" </> "foo" == "x:/foo"
Windows: "C:\\foo" </> "bar" == "C:\\foo\\bar"
Windows: "home" </> "bob" == "home\\bob"
Not combined:
Posix:   "home" </> "/bob" == "/bob"
Windows: "home" </> "C:\\bob" == "C:\\bob"
Not combined (tricky):
On Windows, if a filepath starts with a single slash, it is relative to the root of the current drive. In [1], this is (confusingly) referred to as an absolute path. The current behavior of </> is to never combine these forms.
Windows: "home" </> "/bob" == "/bob"
Windows: "home" </> "\\bob" == "\\bob"
Windows: "C:\\home" </> "\\bob" == "\\bob"
On Windows, from [1]: "If a file name begins with only a disk designator but not the backslash after the colon, it is interpreted as a relative path to the current directory on the drive with the specified letter." The current behavior of </> is to never combine these forms.
Windows: "D:\\foo" </> "C:bar" == "C:bar"
Windows: "C:\\foo" </> "C:bar" == "C:bar"

---------------------------------------------------------------------

-- | Is an element a drive
--
-- > Posix:   isDrive "/" == True
-- > Posix:   isDrive "/foo" == False
-- > Windows: isDrive "C:\\" == True
-- > Windows: isDrive "C:\\foo" == False
-- >          isDrive "" == False
isDrive :: FilePath -> Bool
isDrive x = not (null x) && null (dropDrive x)


-- | Delete the drive, if it exists.
--
-- > dropDrive x == snd (splitDrive x)
dropDrive :: FilePath -> FilePath
dropDrive = snd . splitDrive


-- | Split a path into a drive and a path.
--   On Posix, \/ is a Drive.
--
-- > uncurry (++) (splitDrive x) == x
-- > Windows: splitDrive "file" == ("","file")
-- > Windows: splitDrive "c:/file" == ("c:/","file")
-- > Windows: splitDrive "c:\\file" == ("c:\\","file")
-- > Windows: splitDrive "\\\\shared\\test" == ("\\\\shared\\","test")
-- > Windows: splitDrive "\\\\shared" == ("\\\\shared","")
-- > Windows: splitDrive "\\\\?\\UNC\\shared\\file" == ("\\\\?\\UNC\\shared\\","file")
-- > Windows: splitDrive "\\\\?\\UNCshared\\file" == ("\\\\?\\","UNCshared\\file")
-- > Windows: splitDrive "\\\\?\\d:\\file" == ("\\\\?\\d:\\","file")
-- > Windows: splitDrive "/d" == ("","/d")
-- > Posix:   splitDrive "/test" == ("/","test")
-- > Posix:   splitDrive "//test" == ("//","test")
-- > Posix:   splitDrive "test/file" == ("","test/file")
-- > Posix:   splitDrive "file" == ("","file")
splitDrive :: FilePath -> (FilePath, FilePath)
splitDrive x | isPosix = span (== '/') x
splitDrive x | Just y <- readDriveLetter x = y
splitDrive x | Just y <- readDriveUNC x = y
splitDrive x | Just y <- readDriveShare x = y
splitDrive x = ("",x)


-- See [1].
-- "\\?\D:\<path>" or "\\?\UNC\<server>\<share>"
readDriveUNC :: FilePath -> Maybe (FilePath, FilePath)
readDriveUNC (s1:s2:'?':s3:xs) | all isPathSeparator [s1,s2,s3] =
    case map toUpper xs of
        ('U':'N':'C':s4:_) | isPathSeparator s4 ->
            let (a,b) = readDriveShareName (drop 4 xs)
            in Just (s1:s2:'?':s3:take 4 xs ++ a, b)
        _ -> case readDriveLetter xs of
                 -- Extended-length path.
                 Just (a,b) -> Just (s1:s2:'?':s3:a,b)
                 Nothing -> Nothing
readDriveUNC _ = Nothing



{- c:\ -}
readDriveLetter :: String -> Maybe (FilePath, FilePath)
readDriveLetter (x:':':y:xs) | isLetter x && isPathSeparator y = Just $ addSlash [x,':'] (y:xs)
readDriveLetter (x:':':xs) | isLetter x = Just ([x,':'], xs)
readDriveLetter _ = Nothing



{- \\sharename\ -}
readDriveShare :: String -> Maybe (FilePath, FilePath)
readDriveShare (s1:s2:xs) | isPathSeparator s1 && isPathSeparator s2 =
        Just (s1:s2:a,b)
    where (a,b) = readDriveShareName xs
readDriveShare _ = Nothing


{- assume you have already seen \\ -}
{- share\bob -> "share\", "bob" -}
readDriveShareName :: String -> (FilePath, FilePath)
readDriveShareName name = addSlash a b
    where (a,b) = break isPathSeparator name



-- Information for validity functions on Windows. See [1].
isBadCharacter :: Char -> Bool
isBadCharacter x = x >= '\0' && x <= '\31' || x `elem` ":*?><|\""

badElements :: [FilePath]
badElements =
    ["CON","PRN","AUX","NUL","CLOCK$"
    ,"COM1","COM2","COM3","COM4","COM5","COM6","COM7","COM8","COM9"
    ,"LPT1","LPT2","LPT3","LPT4","LPT5","LPT6","LPT7","LPT8","LPT9"]

---------------------------------------------------------------------

-- | Is the given character a valid drive letter?
-- only a-z and A-Z are letters, not isAlpha which is more unicodey
isLetter :: Char -> Bool
isLetter x = isAsciiLower x || isAsciiUpper x



addSlash :: FilePath -> FilePath -> (FilePath, FilePath)
addSlash a xs = (a++c,d)
    where (c,d) = span isPathSeparator xs


-- | The character that is used to separate the entries in the $PATH environment variable.
--
-- > Windows: searchPathSeparator == ';'
-- > Posix:   searchPathSeparator == ':'
searchPathSeparator :: Char
searchPathSeparator = if isWindows then ';' else ':'



-- | The character that separates directories. In the case where more than
--   one character is possible, 'pathSeparator' is the \'ideal\' one.
--
-- > Windows: pathSeparator == '\\'
-- > Posix:   pathSeparator ==  '/'
-- > isPathSeparator pathSeparator
pathSeparator :: Char
pathSeparator = if isWindows then '\\' else '/'

-- | The list of all possible separators.
--
-- > Windows: pathSeparators == ['\\', '/']
-- > Posix:   pathSeparators == ['/']
-- > pathSeparator `elem` pathSeparators
pathSeparators :: [Char]
pathSeparators = if isWindows then "\\/" else "/"

-- | Rather than using @(== 'pathSeparator')@, use this. Test if something
--   is a path separator.
--
-- > isPathSeparator a == (a `elem` pathSeparators)
isPathSeparator :: Char -> Bool
isPathSeparator '/' = True
isPathSeparator '\\' = isWindows
isPathSeparator _ = False


-- | The character that is used to separate the entries in the $PATH environment variable.
--
-- > Windows: searchPathSeparator == ';'
-- > Posix:   searchPathSeparator == ':'
searchPathSeparator :: Char
searchPathSeparator = if isWindows then ';' else ':'

-- | Is the character a file separator?
--
-- > isSearchPathSeparator a == (a == searchPathSeparator)
isSearchPathSeparator :: Char -> Bool
isSearchPathSeparator = (== searchPathSeparator)


-- | File extension character
--
-- > extSeparator == '.'
extSeparator :: Char
extSeparator = '.'

-- | Is the character an extension character?
--
-- > isExtSeparator a == (a == extSeparator)
isExtSeparator :: Char -> Bool
isExtSeparator = (== extSeparator)



-- | Is the operating system Unix or Linux like
isPosix :: Bool
isPosix = not isWindows

-- | Is the operating system Windows like
isWindows :: Bool
isWindows = IS_WINDOWS



-- | Get a list of 'FilePath's in the $PATH variable.
getSearchPath :: IO [FilePath]
getSearchPath = fmap splitSearchPath (getEnv "PATH")



---------------------------------------------------------------------


span :: (a -> Bool) -> [a] -> ([a], [a])

span, applied to a predicate p and a list xs, returns a tuple where first element is longest prefix (possibly empty) of xs of elements that satisfy p and second element is the remainder of the list:

span (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
span (< 9) [1,2,3] == ([1,2,3],[])
span (< 0) [1,2,3] == ([],[1,2,3])


span p xs is equivalent to (takeWhile p xs, dropWhile p xs)


---------------------------------------------------------------------
```


## GHC 8.6.3 Release Notes

See <https://downloads.haskell.org/~ghc/master/users-guide/8.6.1-notes.html>

See <https://msdn.microsoft.com/en-us/library/windows/desktop/aa365247.aspx>

### Maximum Path Length Limitation

> To specify an extended-length path, use the "\\?\" prefix. For example, "\\?\D:\very long path".
>  
>  
> Fully Qualified vs. Relative Paths
> For Windows API functions that manipulate files, file names can often be relative to the current directory, while some APIs require a fully qualified path. A file name is relative to the current directory if it does not begin with one of the following:
> A UNC name of any format, which always start with two backslash characters ("\\"). For more information, see the next section.
> A disk designator with a backslash, for example "C:\" or "d:\".
> A single backslash, for example, "\directory" or "\file.txt". This is also referred to as an absolute path.
> If a file name begins with only a disk designator but not the backslash after the colon, it is interpreted as a relative path to the current directory on the drive with the specified letter. Note that the current directory may or may not be the root directory depending on what it was set to during the most recent "change directory" operation on that disk. Examples of this format are as follows:
> "C:tmp.txt" refers to a file named "tmp.txt" in the current directory on drive C.
> "C:tempdir\tmp.txt" refers to a file in a subdirectory to the current directory on drive C.
> A path is also said to be relative if it contains "double-dots"; that is, two periods together in one component of the path. This special specifier is used to denote the directory above the current directory, otherwise known as the "parent directory". Examples of this format are as follows:
> "..\tmp.txt" specifies a file named tmp.txt located in the parent of the current directory.
> "..\..\tmp.txt" specifies a file that is two directories above the current directory.
> "..\tempdir\tmp.txt" specifies a file named tmp.txt located in a directory named tempdir that is a peer directory to the current directory.
> Relative paths can combine both example types, for example "C:..\tmp.txt". This is useful because, although the system keeps track of the current drive along with the current directory of that drive, it also keeps track of the current directories in each of the different drive letters (if your system has more than one), regardless of which drive designator is set as the current drive.
>  
>  
>  
>  
> Naming Conventions
> The following fundamental rules enable applications to create and process valid names for files and directories, regardless of the file system:
> Use a period to separate the base file name from the extension in the name of a directory or file.
> Use a backslash () to separate the components of a path. The backslash divides the file name from the path to it, and one directory name from another directory name in a path. You cannot use a backslash in the name for the actual file or directory because it is a reserved character that separates the names into components.
> Use a backslash as required as part of volume names, for example, the "C:\" in "C:\path\file" or the "\\server\share" in "\\server\share\path\file" for Universal Naming Convention (UNC) names. For more information about UNC names, see the Maximum Path Length Limitation section.
> Do not assume case sensitivity. For example, consider the names OSCAR, Oscar, and oscar to be the same, even though some file systems (such as a POSIX-compliant file system) may consider them as different. Note that NTFS supports POSIX semantics for case sensitivity but this is not the default behavior. For more information, see CreateFile.
> Volume designators (drive letters) are similarly case-insensitive. For example, "D:\" and "d:\" refer to the same volume.
> Use any character in the current code page for a name, including Unicode characters and characters in the extended character set (128–255), except for the following:
> The following reserved characters:
> < (less than)
> (greater than)
> : (colon)
> " (double quote)
> / (forward slash)
> \ (backslash)
> | (vertical bar or pipe)
> ? (question mark)
> * (asterisk)
> Integer value zero, sometimes referred to as the ASCII NUL character.
> Characters whose integer representations are in the range from 1 through 31, except for alternate data streams where these characters are allowed. For more information about file streams, see File Streams.
> Any other character that the target file system does not allow.
> Use a period as a directory component in a path to represent the current directory, for example ".\temp.txt". For more information, see Paths.
> Use two consecutive periods (..) as a directory component in a path to represent the parent of the current directory, for example "..\temp.txt". For more information, see Paths.
> Do not use the following reserved names for the name of a file:
> CON, PRN, AUX, NUL, COM1, COM2, COM3, COM4, COM5, COM6, COM7, COM8, COM9, LPT1, LPT2, LPT3, LPT4, LPT5, LPT6, LPT7, LPT8, and LPT9. Also avoid these names followed immediately by an extension; for example, NUL.txt is not recommended. For more information, see Namespaces.
> Do not end a file or directory name with a space or a period. Although the underlying file system may support such names, the Windows shell and user interface does not. However, it is acceptable to specify a period as the first character of a name. For example, ".temp".
> Namespaces
> There are two main categories of namespace conventions used in the Windows APIs, commonly referred to as NT namespaces and the Win32 namespaces. The NT namespace was designed to be the lowest level namespace on which other subsystems and namespaces could exist, including the Win32 subsystem and, by extension, the Win32 namespaces. POSIX is another example of a subsystem in Windows that is built on top of the NT namespace. 
> 







## `f.el`

### `f.el` issues

> relevant:
> 
> * Lisp Changes in Emacs 26.1
> [...]
> ** The new function 'file-name-case-insensitive-p' tests whether a
> given file is on a case-insensitive filesystem.


### `f.el` code

```elisp
(defun f-ext (path)
  "Return the file extension of PATH.
The extension, in a file name, is the part that follows the last
'.', excluding version numbers and backup suffixes."
  (file-name-extension path))



(defun f-no-ext (path)
  "Return everything but the file extension of PATH."
  (file-name-sans-extension path))



(defun f-swap-ext (path ext)
  "Return PATH but with EXT as the new extension.
EXT must not be nil or empty."
  (if (s-blank? ext)
      (error "Extension cannot be empty or nil")
    (concat (f-no-ext path) "." ext)))



(defun f-base (path)
  "Return the name of PATH, excluding the extension of file."
  (f-no-ext (f-filename path)))



(defun f-relative (path &optional dir)
  "Return PATH relative to DIR."
  (file-relative-name path dir))




(defun f-join (&rest args)
  "Join ARGS to a single path."
  (let (path (relative (f-relative? (car args))))
    (-map
     (lambda (arg)
       (setq path (f-expand arg path)))
     args)
    (if relative (f-relative path) path)))



(defun f-expand (path &optional dir)
  "Expand PATH relative to DIR (or `default-directory').
PATH and DIR can be either a directory names or directory file
names.  Return a directory name if PATH is a directory name, and
a directory file name otherwise.  File name handlers are
ignored."
  (let (file-name-handler-alist)
    (expand-file-name path dir)))



(defun f-filename (path)
  "Return the name of PATH."
  (file-name-nondirectory (directory-file-name path)))





(defun f-exists? (path)
  "Return t if PATH exists, false otherwise."
  (file-exists-p path))

(defalias 'f-exists-p 'f-exists?)

(defalias 'f-dir? 'f-directory?)
(defalias 'f-dir-p 'f-dir?)

(defun f-directory? (path)
  "Return t if PATH is directory, false otherwise."
  (file-directory-p path))




(defun f-file? (path)
  "Return t if PATH is file, false otherwise."
  (file-regular-p path))

(defalias 'f-file-p 'f-file?)

(defun f-symlink? (path)
  "Return t if PATH is symlink, false otherwise."
  (not (not (file-symlink-p path))))

(defalias 'f-symlink-p 'f-symlink?)

(defun f-readable? (path)
  "Return t if PATH is readable, false otherwise."
  (file-readable-p path))

(defalias 'f-readable-p 'f-readable?)

(defun f-writable? (path)
  "Return t if PATH is writable, false otherwise."
  (file-writable-p path))

(defalias 'f-writable-p 'f-writable?)



(defun f-executable? (path)
  "Return t if PATH is executable, false otherwise."
  (file-executable-p path))

(defalias 'f-executable-p 'f-executable?)

(defun f-absolute? (path)
  "Return t if PATH is absolute, false otherwise."
  (file-name-absolute-p path))

(defalias 'f-absolute-p 'f-absolute?)

(defun f-relative? (path)
  "Return t if PATH is relative, false otherwise."
  (not (f-absolute? path)))

(defalias 'f-relative-p 'f-relative?)

(defun f-root? (path)
  "Return t if PATH is root directory, false otherwise."
  (not (f-parent path)))




(defun f-this-file ()
  "Return path to this file."
  (cond
   (load-in-progress load-file-name)
   ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
    byte-compile-current-file)
   (:else (buffer-file-name))))



(defun f-this-file ()
  "Return path to this file."
  (cond
   (load-in-progress load-file-name)
   ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
    byte-compile-current-file)
   (:else (buffer-file-name))))
```




## 