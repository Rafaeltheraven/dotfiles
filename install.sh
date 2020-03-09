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
			echo "Found: $file2"
			mv ~/.config/$file2 "$olddir/config/"
			ln -s "$dir/$confdir" ~/.config/$file2
		done
	elif [[ "$file" != "install.sh" ]]; then
		echo "Found: $file"
		mv ~/.$file "$olddir"
		ln -s "$dir/$file" ~/.$file
	fi
done
