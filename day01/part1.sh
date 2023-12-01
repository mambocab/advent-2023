for f in ./example.txt ./input.txt ; do
  echo $f
    sed '
      s/[a-z]//g
      s/\(.\).*\(.\)/\1\2/g
      s/^.$/&&/g
    ' $f | tr -s '\n' + | sed 's/+$//' | bc
done
