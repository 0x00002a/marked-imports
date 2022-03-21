# marked-imports

## Overview

Tool to add the source of packages above imports. Uses `ghc-pkg dump` and your local stack env


## Usage

```bash
mimps <file> [-i]
```

Note you'll need a <packagename>.cabal file in your source root and you'll need to run the above in either
your source root or a subdirectory of it, you'll also need to be using stack


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

