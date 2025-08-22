#!/bin/bash

# List of required tools
#   * https://github.com/orhun/git-cliff
#   * https://github.com/killercup/cargo-edit

set -euo pipefail

VERSION=$(git cliff --bumped-version)

# Set the new version in all crates
cargo set-version --workspace "${VERSION:1}"

# Sanity check that everything compiles
cargo check

# Generate the changelog entry
git cliff --unreleased --tag "$VERSION" --prepend CHANGELOG.md

# Create the release commit and tag
git add CHANGELOG.md ./**/Cargo.toml
git commit -m "chore: release $VERSION"
git tag -a "$VERSION" -m "Release $VERSION"

# Last check
git diff HEAD^..HEAD

# Instructions
echo "If you are satisfied, run git push && git push --tags"
echo "If there is something wrong, run git reset --hard HEAD^ && git tag -d $VERSION"
