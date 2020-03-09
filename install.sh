#!/bin/bash

dir=~/dotfiles
olddir=~/dotfiles_old
confdir="config"
files="$(ls $dir)"

echo "Creating $olddir and $olddir/$confdir for backup purposes"
mkdir -p "$olddir"
mkdir -p "$olddir/$confdir"
echo "...done"

echo "Changing to $dir"
cd "$dir"
echo "...done"

for file in $files; do
	if [[ "$file" == "config" ]]; then
		for file2 in $(ls $dir/$confdir); do
			if [["$file2" != "README.md"]]; then
				echo "Found: $file2"
				mv ~/.config/$file2 "$olddir/config/"
				ln -s "$dir/$confdir/$file2" ~/.config/$file2
			fi
		done
	elif [[ "$file" != "install.sh" ] && [ "$file" != "README.md" ]]; then
		echo "Found: $file"
		mv ~/.$file "$olddir"
		ln -s "$dir/$file" ~/.$file
	fi
done
