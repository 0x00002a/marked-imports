# marked-imports

## Overview

Tool to add the source of packages above imports. Uses `ghc-pkg dump` and your local stack env


## Usage

```bash
mimps <file> [-i]
```

Note you'll need a <packagename>.cabal file in your source root and you'll need to run the above in either
your source root or a subdirectory of it. If your using stack you should run it with `stack exec -- mimps ...`
instead so it can access your stack environment.


## Examples

```
>>> echo "
module M
import Data.Text
import Data.Maybe
import Data.Either
import MyLocalModule
" | mimps
module M
-- text
import Data.Text
-- base
import Data.Maybe
import Data.Either
-- local
import MyLocalModule
```

## Installation

Clone this repo, then run `stack install`

## Troubleshooting

If you get a bunch of errors about being unable to find packages, make sure:

- If your using stack your running with `stack exec` **and** that your stack env is up to date (run `stack build` if unsure)
- The package shows up if you run `ghc-pkg find-module <module in error>` if it doesn't then ghc isn't aware of the package
  either and your build shouldn't be working

