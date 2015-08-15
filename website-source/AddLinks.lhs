Little hack to add links to the navigation bars

> main :: IO ()
> main = interact addLinks


> addLinks :: String -> String
> addLinks [] = error "not found"
> addLinks ('<':'/':'u':'l':'>':'\n':'<':'/':'d':'i':'v':'>':xs) =
>     "</ul>" ++ linkSection ++ "\n</div>" ++ xs
> addLinks (x:xs) = x : addLinks xs

> linkSection :: String
> linkSection =
>   "<hr />\n\
>   \<ul class='sectlevel1'>\n\
>   \<div id='toctitle'>Links</div>\n\
>   \<li><a href='index.html'>Index</a></li>\n\
>   \<li><a href='haddock/index.html'>Haddock</li>\n\
>   \<li><a href='examples.html'>Usage examples</a></li>\n\
>   \<li><a href='ParserTests.html'>Parsing test cases</a></li>\n\
>   \<li><a href='TypeCheckTests.html'>Type-checking test cases</a></li>\n\
>   \<li><a href='QuasiQuoteTests.html'>Quasiquotation test cases</a></li>\n\
>   \</ul>\n\
>   \<br />\n\
>   \<ul class='sectlevel1'>\n\
>   \<li><a href='http://jakewheat.github.io/hssqlppp/latest' class='bare'>Homepage</a></li>\n\
>   \<li><a href='http://hackage.haskell.org/package/hssqlppp' class='bare'>Hackage</a></li>\n\
>   \<li><a href='https://github.com/JakeWheat/hssqlppp' class='bare'>Repository</a></li>\n\
>   \<li><a href='https://github.com/JakeWheat/hssqlppp/issues' class='bare'>Bug tracker</a></li>\n\
>   \<li><a href='https://github.com/JakeWheat/hssqlppp/blob/master/CHANGES' class='bare'>Changes</a></li>\n\
>   \<li><a href='http://jakewheat.github.io/hssqlppp/' class='bare'>Other versions</a></li>\n\
>   \<li><a href='http://jakewheat.github.io/' class='bare'>Parent project</a>\n\
>   \</li><li>jakewheatmail@gmail.com</li>\n\
>   \</ul>\n"
