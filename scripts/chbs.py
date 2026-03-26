#!/bin/python3
# A wordlist-based password generator.

import random
import string
import collections
import itertools
import signal
import time
import sys
import functools
import traceback
import argparse

from contextlib import contextmanager

# A password and associated plaintext
CorrectHorse = collections.namedtuple(
    'CorrectHorse',
    ['password', 'original', 'edits', 'scrubpasses']
)

def printchbs(chbs):
    print(f"{chbs.password} (Remember '{chbs.original}', complexity {chbs.edits + chbs.scrubpasses})")

urandom = random.SystemRandom()

SPECIALS = "!@.=-_^+;:#,%*$&()\\"
SPECIAL_JOINERS = set(SPECIALS) | set("!?@.=-_+:#&")

CHAR_CLASSES = [
    string.ascii_uppercase,
    string.ascii_lowercase,
    string.digits,
    SPECIALS
]

nop = lambda *a, **k: None
vprint = nop

# print(SPECIALS)

# Map of lowercase letters to valid characters they can transform to
# Sorted by priority. The first valid character will be used when replacing.
# Since special character sets may be required having multiple symbol
# options is a good idea. Multiple numbers won't do anything usually.
TRANSFORMS = {
    'a': '@4^',
    'b': '8%',
    'c': '([',
    'd': '&0',
    'e': '3@*',  # '-*+'
    'f': '4',
    'g': '9&',
    'h': '4#',
    'i': '1!*',
    'j': '!$',
    'k': '%',
    'l': '1|!',
    'm': '',  # '/',
    'n': '~',
    'o': '0+*',
    'p': '9)',
    'q': '9(',
    'r': '^',
    's': '$5',
    't': '7+|',
    'u': '',  # '_-',
    'v': '^>',
    'w': '=#~',
    'x': '*%<+',
    'y': '7',  # '+',
    'z': '/%-~',
}

for l in list(TRANSFORMS.keys()):
    TRANSFORMS[l.upper()] = TRANSFORMS[l] + l  # Upper can transform to lower and its transforms
    TRANSFORMS[l] += l.upper()  # Lower can transform to upper

words_by_len = {}

def makeDictionary(wordlistFilepath, minlength=4):
    global words_by_len
    with open(wordlistFilepath, "r") as fp:
        word_file = fp.read()

    word_dictionary = list(filter(bool, word_file.split('\n')))

    words_by_len = collections.defaultdict(list)
    for word in word_dictionary:
        if len(word) > minlength:
            words_by_len[len(word)].append(word)
    words_by_len[1] = list(SPECIAL_JOINERS)
    # words_by_len['PUNC'] = list('!.;')

# print(word_dictionary[:3], '...', word_dictionary[-3:])
# print({k: len(v) for k, v in words_by_len.items()})

@functools.lru_cache(maxsize=4)
def coin_changes(numbers, target, permute=True, onejoin=True):
    vprint("Changing coins...", end=' ')
    # Coin changing algorithm. Generate all combinations of numbers that sum to target.

    @functools.lru_cache(maxsize=2048)
    def _coin_changes(numbers, partial=tuple(), partial_sum=0):
        # vprint(numbers, partial, partial_sum)
        if partial_sum == target:
            yield partial
        if partial_sum >= target:
            return
        for i, n in enumerate(numbers):
            yield from _coin_changes(
                numbers[i + 1:],  # Slice list after number
                tuple(list(partial) + [n]),  # Add number to partials
                partial_sum + n  # Compute partial sum
            )
    combinations = list(_coin_changes(tuple(numbers)))

    if permute and target < 30:
        combinations = sum([list(itertools.permutations(s)) for s in combinations], [])
    if onejoin and target < 30:
        for wordlengths in list(filter(lambda c: all(i > 2 for i in c), combinations)):
            regular_list = [*[[n-1, 1] for n in wordlengths[:-1]], [wordlengths[-1]]]
            wordlengths2 = [item for sublist in regular_list for item in sublist]
            if all(i in words_by_len.keys() for i in wordlengths2):
                # print(wordlengths, wordlengths2)
                combinations.append(wordlengths2)
            regular_list = [[n-1, 1] for n in wordlengths]
            wordlengths3 = [item for sublist in regular_list for item in sublist]
            if all(i in words_by_len.keys() for i in wordlengths3):
                combinations.append(wordlengths3)

    vprint("Done,", len(combinations), 'patterns')
    return combinations

@contextmanager
def timeout(duration):
    def timeout_handler(signum, frame):
        raise Exception(f'block timedout after {duration} seconds')
    signal.signal(signal.SIGALRM, timeout_handler)
    signal.alarm(duration)
    yield
    signal.alarm(0)

def intersection(l1, l2):
    """Intersection (all items from l1 also in l2) of two lists. Includes duplicates, unlike set."""
    return list(filter(lambda i: i in l2, l1))

def difference(l1, l2):
    """Difference (all items from l1 not in l2) of two lists. Includes duplicates, unlike set."""
    return list(filter(lambda i: i not in l2, l1))

def printPointer(word, last, pointer):
    # prbuffer = ''.join(c or ' ' for c in buffer)
    vprint(''.join(word), last, TRANSFORMS.get(word[pointer]))
    vprint(' ' * (pointer - last), 'x' * last, '^', sep='')

def generate(length=12, classes=CHAR_CLASSES, maxseq=3, mineach=2, verify=None):
    if verify:
        pw = verify
    else:
        lengths = coin_changes(tuple([*[i for i in words_by_len.keys() if i > 1]] * 2), length)
        wordlengths = urandom.choice(lengths)
        # wordlengths = [length]
        vprint('Using word pattern', wordlengths)

        pw = ''.join(urandom.choice(words_by_len[i]).capitalize() for i in wordlengths)
    # print(wordlengths)
    # print(pw)
    edits = 0
    scrubpasses = 0

    def clamp(pw, silent=False):
        # Take a password and "clamp" it to the ruleset.
        nonlocal edits
        nonlocal scrubpasses
        pw = list(pw)  # Use a list so indexes can be modified

        def getClass(c):
            # Get the character class that matches a character c
            for cls in classes:
                if c in cls:
                    return cls

        # Scrub over the password until it meets minimum requirements
        scrubpasses = 0
        clean = False
        while not clean:
            clean = True
            for cc in classes:
                if len(intersection(pw, cc)) < mineach:
                    clean = False
                    vprint("Too few", cc, 'in', ''.join(pw), 'only', intersection(pw, cc))
                    indices = list(range(len(pw)))
                    urandom.shuffle(indices)
                    c = None
                    subset = []
                    for i in indices:
                        c = pw[i]
                        subset = intersection(TRANSFORMS.get(c, ''), cc)
                        if len(subset) > 0:
                            vprint("Picked random char", c, 'to replace with best from subset', subset)
                            pw[i] = subset[0]  # urandom.choice(subset)
                            scrubpasses += 1
                            break
                    else:
                        raise AssertionError(f"Failed to populate '{''.join(pw)}' with {cc}")
                    # print(''.join(pw))

        # Scrub over the password to check for consecutive characters

        # Keep a pointer to a position in the string
        pointer = 0  # urandom.randrange(0, maxseq)

        # Keep track of the last class and how long the substring was
        lastclass = None
        lastclasslen = 0

        # When we modify the string, jump back and recheck the section.
        def back():
            nonlocal pointer
            nonlocal lastclasslen
            lastclasslen = 0
            pointer = max(0, pointer - maxseq)

        edits = 0
        back()
        while pointer < len(pw):
            thisclass = getClass(pw[pointer])
            # vprint(thisclass, lastclass, lastclasslen)
            if thisclass == lastclass:
                if lastclasslen + 1 > maxseq:
                    if not silent:
                        printPointer(pw, lastclasslen, pointer)
                    newchars = difference(TRANSFORMS.get(pw[pointer], pw[pointer]), lastclass)
                    newchars = list(filter(lambda char: any(char in cls for cls in classes), newchars))
                    pw[pointer] = newchars[0]  # Try to pick the best one
                    edits += 1
                    back()
                    continue
                lastclasslen += 1
            else:
                lastclasslen = 1
                lastclass = thisclass

            pointer += 1

        # print("Done:", ''.join(pw))
        return ''.join(pw)
        # raise NotImplementedError()

    clamped = clamp(pw)
    ret = CorrectHorse(clamped, pw, edits=edits, scrubpasses=scrubpasses)

    vprint("Verifying")
    reclamped = clamp(clamped, silent=True)
    # If clamping a clamped password changes anything, then clamping missed something.
    assert(clamped == reclamped), f'{clamped} != {reclamped}'
    return ret


def inftest(generatePartial=generate):
    # If this is called directly, just stream out passwords.
    success = 0
    fail = 0
    while True:
        try:
            result = generatePartial()
            # print(result)
            success += 1
            sys.stdout.write(result.password)
            sys.stdout.flush()
            sys.stderr.write(
                f" (\"{result.original}\") ({result.edits + result.scrubpasses} edits) (failed %{100*fail/(fail+success)})")
            sys.stderr.flush()
            sys.stdout.write('\n')
            sys.stdout.flush()
            # if not success % 100:
            #     print(f"Generated {success} passwords, failed %{100*fail/(fail+success)}")
        except KeyboardInterrupt:
            return
        except (AssertionError, Exception) as e:
            print(e, flush=True)
            # traceback.print_exc()
            fail += 1
            continue
# break


if __name__ == "__main__":
    # global SPECIALS  # not in function

    import argparse
    import os
    wordlist = 'wordlist.txt'
    wordlist = (os.path.isfile(wordlist) and wordlist) or os.path.join(os.path.dirname(__file__), wordlist)
    parser = argparse.ArgumentParser(
        description="Generate lexographic passwords",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
    parser.add_argument('-l', '--length', type=int, default=12,
        help="Password length, in characters")
    parser.add_argument('--maxseq', type=int, default=3,
        help="Max length of any group of characters in the same class (upper, lower, numeric, symbol)")
    parser.add_argument('--mineach', type=int, default=2,
        help="Minimum total count of characters from any class (upper, lower, numeric, symbol)")
    parser.add_argument('--max-complexity', type=int, default=10,
        help="Maximum \"password complexity")
    parser.add_argument('-s', '--specials', default=SPECIALS,
        help="List of allowed special characters")
    parser.add_argument('-w', '--wordlist', default=wordlist,
        help="Text file with wordlist. Should be lowercase with one word per line. ")
    parser.add_argument('--test', action='store_true', help="Test generation")
    parser.add_argument('--verify', help="Verify/clean the given password instead of generating one")
    parser.add_argument('-v', '--verbose', action='store_true', help="Trace password generation")
    parser.add_argument('-r', '--raw', action='store_true', help="Print only raw password, for use in automation")
    args = parser.parse_args()

    if args.verbose:
        vprint = print

    SPECIALS = args.specials
    SPECIAL_JOINERS = set(SPECIALS) & set("!@.=-_+:#%&")

    CHAR_CLASSES = [
        string.ascii_uppercase,
        string.ascii_lowercase,
        string.digits,
        SPECIALS
    ]

    # print(CHAR_CLASSES)

    makeDictionary(args.wordlist)

    # print(words_by_len[1])

    def _generate():
        for i in range(100):
            try:
                with timeout(1):
                    res = generate(length=args.length, classes=CHAR_CLASSES, maxseq=args.maxseq, mineach=args.mineach)
                    complexity = res.edits + res.scrubpasses
                    assert complexity <= args.max_complexity, f"{res.password}/{res.original} complexity {complexity} too high (max {args.max_complexity})"
                    return res
                break
            except:
                if args.verbose:
                    traceback.print_exc()
                continue

    if args.test:
        inftest(_generate)
    elif args.verify:
        res = generate(
            length=args.length,
            classes=CHAR_CLASSES,
            maxseq=args.maxseq,
            mineach=args.mineach,
            verify=args.verify)
        if args.raw:
            print(res.password)
        else:
            print("OK" if res.password == res.original else "FIX", repr(res.password))
    else:
        if args.raw:
            print(_generate().password)
        else:
            printchbs(_generate())

    # inftest()
