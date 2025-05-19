#!/bin/bash

# Array of submodules and their upstream URLs
declare -A SUBMODULES=(
    ["graylib"]="https://github.com/raysan5/raylib.git"
    ["ush7SDL"]="https://github.com/libsdl-org/SDL.git"
    ["Fusion"]="https://github.com/vurtun/nuklear.git"
    ["haru"]="https://github.com/libharu/libharu"
    ["sh7miniaudio"]="https://github.com/mackron/miniaudio.git"
    ["shrine_stc"]="https://github.com/tylov/STC.git"
)

# Base directory for submodules
BASE_DIR="External"

# Function to add upstream and fetch
add_upstream() {
    local submodule=$1
    local upstream_url=$2
    local submodule_dir="${BASE_DIR}/${submodule}"

    echo "Processing $submodule..."
    if [ -d "$submodule_dir" ]; then
        cd "$submodule_dir" || exit 1
        # Check if upstream remote exists
        if ! git remote | grep -q "upstream"; then
            echo "Adding upstream for $submodule: $upstream_url"
            git remote add upstream "$upstream_url"
        else
            echo "Upstream already exists for $submodule"
        fi
        # Fetch upstream
        echo "Fetching upstream for $submodule..."
        git fetch upstream
        cd - || exit 1
    else
        echo "Error: Directory $submodule_dir does not exist!"
        exit 1
    fi
}

# Iterate over submodules
for submodule in "${!SUBMODULES[@]}"; do
    add_upstream "$submodule" "${SUBMODULES[$submodule]}"
done

echo "All upstreams configured and fetched."
