## pathlib

```python
from pathlib import Path
```

## Regex

```python
import re

re.findall(pattern, string, flags=0) # -> list[str] | list[tuple]

re.compile(r'...') # -> re.Pattern
# Match from start (match) or anywhere (search)
Pattern.match(string[, pos[, endpos]]) # -> re.Match
Pattern.search(string[, pos[, endpos]]) # -> re.Match
```

## datetime

```python
import datetime

return datetime.datetime.now().strftime("%Y-%m-%d %I:%M%p")
```

## Collections

## Flow

- itertools
- more_itertools

```python

def chunk(iterable, maxsize):
    """A generator that yields lists of size `maxsize` containing the results of iterable `it`.
    """
    from itertools import islice
    iter_it = iter(iterable)
    yield from iter(lambda: tuple(islice(iter_it, maxsize)), ())
```

## Logging

```python
import logging

logger = logging.getLogger(__name__)

logger.setLevel(logging.INFO)

ch = logging.StreamHandler()
ch.setLevel(logging.INFO)
# ch.setFormatter(ColorFormatter()) ...

logger.addHandler(ch)
```

## Types

```python
from typing import Iterator, ClassVar, TypeAlias

MyDict: TypeAlias = dict[str, list]
```

## Caching

```python
from joblib import Memory
memory = Memory("cachedir")
@memory.cache
def f(x):
    print('Running f(%s)' % x)
    return x
```
