#!/usr/bin/env bash

src_dir="$(dirname "$0")/../static/photos"
dst_dir="$(dirname "$0")/../static/photos/thumb"

mkdir -p "$dst_dir"

for img in "$src_dir"/*; do
    if [[ -f "$img" ]]; then
        filename=$(basename "$img")
        case "$img" in
            *.jpg|*.jpeg|*.png) ;;
            *) continue ;;
        esac
        if [[ ! -f "$dst_dir/$filename" ]]; then
            printf "\r%*s\r" 80 ""  # Clear the line
            magick "$img" \
                -resize "640x640" \
                -strip \
                "$dst_dir/$filename"
            echo "Generated thumbnail: $dst_dir/$filename"
        else
            printf "\rSkipping existing: %s" "$filename"
        fi
    fi
done
printf "\r%*s\r" 80 ""  # Clear the final cached line
