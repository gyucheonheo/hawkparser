![logo](./etc/hawkparser.png)
# hawkparser
Implementation of Parser using Monad

## Language Syntax
* Expression -> Variable | Identifier | FunctionCall
* Identifier -> [a-z][a-zA-Z0-9|_]*
* Variable -> [A-Z][a-zA-Z0-9|_]*
* FunctionCall -> Identifier(Arguments)
* Arguments -> Expression ArgTail | empty
* ArgTail -> , Expression ArgTail | empty

## How to run

### Go to ghci
``` bash
$> ghci
```

### Load the file
``` bash
(ghci)$> :l hawkparser.hs
```

### Enjoy parsing
``` bash
(ghci)$>run expression "f(a,b)"
==> Just (FCN "f" [ID "a", ID "b"], "")
```

``` bash
(ghci)$>run expression "f(a,b,g(a,b))"
==> Just (FCN "f" [ID "a",ID "b",FCN "g" [ID "a",ID "b"]],"") 
```

