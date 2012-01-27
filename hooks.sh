#!/bin/bash

function pre_clean {
	rm -rf rpg_battlemap
	rm -f include/commit_ver.hrl
}

function pre_compile {
	if [ ! -d ebin ]; then
		mkdir ebin
	fi

	if [ ! -d priv ]; then
		mkdir priv
	fi

	if [ ! -d include ]; then
		mkdir include
	fi

#	for file in proto_src/*.proto
#	do
#		nameBase=`echo "$file" | sed -e "s/^proto_src\///"`
#		nameBase="src/${nameBase}"
#		if [ ! -e $nameBase -o $file -nt $nameBase ]
#		then
#			cp $file src/
#		fi
#	done
	# hack for reltool
	if [ ! -d rpg_battlemap ]; then
		mkdir rpg_battlemap
		ln -sf ../ebin rpg_battlemap/ebin
		ln -sf ../src rpg_battlemap/src
		ln -sf ../include rpg_battlemap/include
		ln -sf ../priv rpg_battlemap/priv
		ln -sf ../deps rpg_battlemap/deps
	fi

	# record what commit/version the rep is at
	COMMIT=""
	if [ -d ".git" ]
	then
		COMMIT=`git log -1 --pretty=format:%H`
	fi
	if [ -e "include/commit_ver.hrl" ] && [ ! $COMMIT ]
	then
		exit 0
	else
		if [ ! COMMIT ]
		then
			COMMIT="undefined"
		else
			COMMIT="\"$COMMIT\""
		fi
	fi
	echo "%% automatically generated by precompile script.  Editing means this
%% will just be overwritten.

-define(COMMIT, $COMMIT)." > include/commit_ver.hrl
}

function post_compile {
	cat success_message
}

case $1 in
	"pre_compile")
		pre_compile;;
	"post_compile")
		post_compile;;
	"pre_clean")
		pre_clean;;
esac
