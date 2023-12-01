#/usr/bin/env sh
for f in ./example-part2.txt ./input.txt ; do
  echo $f
  sed '
    # Translate from text to digits.
    # We need to transform `one` into `one1one` so that overlapping words are correctly handled.
    # e.g. eightwo needs to eventually be 82, not 8wo or eigh2.
    # So using &{digit}& gets us, e.g.
    #   eightwo
    #   eight8eightwo
    #   eight8eightwo2wo
    # Which, after removing letters, becomes 82.
    s/one/&1&/g
    s/two/&2&/g
    s/three/&3&/g
    s/four/&4&/g
    s/five/&5&/g
    s/six/&6&/g
    s/seven/&7&/g
    s/eight/&8&/g
    s/nine/&9&/g
    # Having made word substitutions, remove letters.
    s/[a-z]//g
    # First and last characters (in this case, digits) only.
    s/\(.\).*\(.\)/\1\2/g
    # Double single characters.
    s/^.$/&&/g
  ' $f | tr -s '\n' + | sed 's/+$//' | bc # Newlines to plus signs, then trusty ol' bc.
done
