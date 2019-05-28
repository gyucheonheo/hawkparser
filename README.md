![logo](./etc/hawkparser.png)
# hawkparser
Implementation of Parser using Monad

## Language Syntax
* Expression -> Variable | Identifier | FCN (Note that FCN in Expression is still in progress 🐌)
* Identifier -> [a-z][a-zA-Z0-9|_]*
* Variable -> [A-Z][a-zA-Z0-9|_]*
* FCN -> Identifier(Arguments)
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
```

``` bash
(ghci)$>run identifier "itistrue"
```

