#!/usr/bin/env python
from itertools import chain
from functools import total_ordering
from string import ascii_lowercase
from dataclasses import dataclass
from typing import Self


def starts_alpha(s):
    return s[0] in ascii_lowercase


raw = open("example").read()
lines = [line for line in raw.split("\n") if line]
seeds, lines = tuple(int(seed) for seed in lines[0].split(" ")[1:]), lines[1:]
raw_map_lines = []
for line in lines:
    if starts_alpha(line):
        raw_map_lines.append([])
    else:
        raw_map_lines[-1].append(line)
maps = []
for raw_map in raw_map_lines:
    maps.append([])
    for raw_map_line in raw_map:
        maps[-1].append(tuple(int(s) for s in raw_map_line.split(" ")))


@total_ordering
@dataclass
class Range:
    # Inclusive.
    lower_bound: int
    # Exclusive.
    upper_bound: int
    # By how much does the input change?
    delta: int

    def __lt__(self, other):
        return self.lower_bound < self.upper_bound

    @classmethod
    def from_tuple(cls, t):
        if len(t) != 3:
            raise ValueError(f"invalid tuple {t}")
        (dest_range_start, source_range_start, length) = t
        return Range(
            lower_bound=source_range_start,
            upper_bound=source_range_start + length,
            delta=dest_range_start - source_range_start,
        )

    def map(self, i):
        if self.lower_bound <= i <= self.upper_bound:
            return i
        return i + self.delta


class Ranges:
    _ranges: tuple[Range, ...]

    def __init__(self, ranges):
        self._ranges = tuple(ranges)

    def compose(self, other) -> Self:
        acc = []
        all_ranges = iter(sorted(chain(self._ranges, other._ranges)))


print(seeds, [list(map(Range.from_tuple, m)) for m in maps], sep="\n")
