#!/bin/sh -e

FILE="$1"
[ -f "$FILE" ]
cd "$(dirname "$(realpath "$FILE")")"

head -1 "$FILE" > t
IFS="$(printf '\t')" read -r SCORE REF CONTENT < t
rm -f t

echo "$REF" > good
echo "" >> good
cp -f good bad
echo "$CONTENT" | gsed -e 's//\n\n/g' >> good

TOTAL=$(echo "$CONTENT" | wc -w)
RAND=$((TOTAL - SCORE))

echo "$CONTENT" | sed 's/[^A-Za-z]/ /g' | awk -v ORS='\n' '{ for (i = 1; i <= NF; i++) { print $i } }' | sort -R | head -"$RAND" > rands

RCONTENT="$CONTENT"
while read -r WORD ; do
  REPL=$(echo "$WORD" | sed 's/./-/g')
  RCONTENT=$(echo "$RCONTENT" | gsed -e "s/\\b$WORD\\b/$REPL/")
done < rands
rm -f rands

echo "$RCONTENT" | gsed -e 's//\n\n/g' >> bad

"$EDITOR" bad

if cmp good bad >/dev/null 2>&1; then
  SCORE=$((SCORE - 1))
else
  SCORE=$((SCORE + 1))
  clear
  cwdiff bad good
  read -r
fi
rm -f good bad

printf "%s\t%s\t%s\n" "$SCORE" "$REF" "$CONTENT" >> "$FILE"
tail -n +2 "$FILE" > t
sort -nrs t > "$FILE"
rm -f t

git commit -m "m" "$FILE"
