# unicode-show : readable unicode characters in print and show. [![Build Status](https://travis-ci.org/nushio3/unicode-show.svg?branch=master)](https://travis-ci.org/nushio3/unicode-show)


Provides a interactive printer for printing Unicode characters in ghci REPL. Our design goal is that `uprint` produces String representations that are valid Haskell `String` literals, and uses as many Unicode printable characters as possible. Hence
`read . ushow == id`


See the [tests](https://github.com/nushio3/unicode-show/blob/master/test/Spec.hs) of this package for detailed specifications.

## Example

With `print` :

```
$ ghci
...
Ok, modules loaded: Text.Show.Unicode.
> ["哈斯克尔7.6.1"]
["\21704\26031\20811\23572\&7.6.1"]
>
```

With `uprint` :

```
$ ghci -interactive-print=Text.Show.Unicode.uprint Text.Show.Unicode
...
Ok, modules loaded: Text.Show.Unicode.
> ("Хорошо!",["哈斯克尔7.6.1的力量","感じる"])
("Хорошо!",["哈斯克尔7.6.1的力量","感じる"])
> "改\n行"
"改\n行"
```

You can make `uprint` the default interactive printer in several ways. One is to
`cabal install unicode-show`, and add the following lines to your `~/.ghci` config file.

```
import qualified Text.Show.Unicode
:set -interactive-print=Text.Show.Unicode.uprint
```


## References
* https://ghc.haskell.org/trac/ghc/ticket/11529
* http://stackoverflow.com/questions/5535512/how-to-hack-ghci-or-hugs-so-that-it-prints-unicode-chars-unescaped?lq=1
