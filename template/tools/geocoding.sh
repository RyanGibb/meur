#!/usr/bin/env bash
set -euo pipefail

PHOTOS_DIR="$(dirname "$0")/../static/photos"
CACHE_DIR="$(dirname "$0")/../reverse-geocoding"
USER_AGENT="curl/8.12.1"
RATE_LIMIT_DELAY=1

mkdir -p "$CACHE_DIR"

process_photo() {
    local photo=$1
    local base=$(basename "$photo")
    local cache_file="${CACHE_DIR}/${base//\//_}.json"

    if [[ -f "$cache_file" ]]; then
        printf "\rUsing cached data for %s" "$base"
        return
    fi

    printf "\r%*s\r" 80 ""  # Clear the line
    echo "Processing $photo"
    
    read lat lon <<< "$(
       exiftool -n -GPSLatitude# -GPSLongitude# -T "$photo"
     )"

	if [[ -z "$lat" || -z "$lon" || "$lat" == "-" || "$lon" == "-" ]]; then
		echo "No lat/lon, caching empty result"
		echo '{"address": null}' > "$cache_file"
		return
	fi
    
    echo "Querying $lat $lon"
    curl -sS -A "$USER_AGENT" \
        "https://nominatim.openstreetmap.org/reverse?format=json&lat=$lat&lon=$lon&zoom=18&accept-language=en" \
        -o "$cache_file"
    
    # Add delay to respect rate limits
    sleep "$RATE_LIMIT_DELAY"
}

export -f process_photo
export CACHE_DIR USER_AGENT RATE_LIMIT_DELAY

for photo in "$PHOTOS_DIR"/*; do
    if [[ -f "$photo" ]]; then
        process_photo "$photo"
    fi
done
printf "\r%*s\r" 80 ""  # Clear the final cached line
