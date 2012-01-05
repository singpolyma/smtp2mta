#!/bin/sh

# This example script adds the email addresses of those to whom you send mail
# to your ~/.mutt_aliases file

MESSAGE="`cat`"

# Extract contents of header lines
EMAILS="`echo "$MESSAGE" | grep -i '^\(To:\|Cc:\|Bcc:\)' | cut -d: -f2-`"
# XXX: This will fail on quoted ,
IFS=",
"
for ITEM in $EMAILS; do
	FN=""
	NICKNAME=""
	EMAIL="`echo "$ITEM" | grep -o '<[^>]*' | cut -d'<' -f2-`"
	# If there was an ADR in <>, look for a name
	if [ -n "$EMAIL" ]; then
		# XXX: This will fail on quoted <
		FN="`echo "$ITEM" | cut -d'<' -f1 | tr -d '"'`"
		NICKNAME="`echo "$FN" | tr -d ' '`"
	else # Just an email address
		EMAIL="$ITEM"
		FN="`echo "$EMAIL" | cut -d@ -f1`"
	fi
	EMAIL="`echo "$EMAIL" | tr -d ' '`"
	FN="`echo "$FN" | sed -e 's/^ *//;s/ *$//'`"
	echo "alias \"$EMAIL\" \"$FN\" <$EMAIL>" >> ~/.mutt_aliases
	if [ -n "$NICKNAME" ]; then
		echo "alias \"$NICKNAME\" \"$FN\" <$EMAIL>" >> ~/.mutt_aliases
	fi
done

sorted="`sort < ~/.mutt_aliases | uniq`"
echo "$sorted" > ~/.mutt_aliases

echo "$MESSAGE" | ssh mailserver "sendmail $@"
