#!/usr/bin/env bash
#
# Build the Haddock documentation of every package in the project and
# assemble it into a self-contained directory, ready for hosting:
#
#     scripts/haddock-site.sh OUTDIR [STACK-OPTIONS...]
#
# Runs `stack haddock --no-haddock-deps` (STACK-OPTIONS go to Stack, e.g.
# --work-dir) and fills OUTDIR -- new, empty, or a previous run's output,
# which is emptied first -- with the project packages' documentation and
# Stack's index of it (OUTDIR/index.html). Dependencies are not
# documented: Haddock links to a dependency only when that dependency's
# documentation was built, and building it for the whole snapshot (the
# Cryptol tree included) takes hours. The libraries GHC ships with (base,
# containers, text, template-haskell, ghc, ...) are the exception: Stack
# copies their documentation from the GHC installation to directories beside
# the project packages' -- several hundred megabytes -- and links into the
# copies. The copies stay out of OUTDIR and the links into them are
# redirected to Hackage. Everything else is unlinked (Haddock's "could not
# find link destinations" warnings), as are the references Haddock turned
# into links to pages that do not exist. Pages of modules that no longer
# exist, which Stack's incremental copying leaves behind, are dropped.
#
# CI (.github/workflows/ci.yml) runs this on every run -- pushes to master,
# pull requests, manual runs -- and publishes master's OUTDIR at
# https://rewire-hardware.github.io/ReWire/haddock/.

set -euo pipefail
unset CDPATH # cd must not print

if [ $# -lt 1 ]; then
      echo "usage: $0 OUTDIR [STACK-OPTIONS...]" >&2
      exit 2
fi
out=$1
shift

# OUTDIR is emptied; refuse to empty anything but a previous run's output,
# recognized by the generator tag this script puts in its index page.
mine='<meta name="generator" content="scripts/haddock-site.sh" />'
if [ -e "$out" ] && [ -n "$(ls -A "$out")" ] && ! grep -qsF "$mine" "$out/index.html"; then
      echo "$0: $out exists and is not this script's output; remove it first" >&2
      exit 1
fi
mkdir -p "$out"

stack "$@" haddock --no-haddock-deps

# Where Stack put the documentation. (Not `stack path --local-doc-root`,
# which with Stack 3.x names a directory that does not exist.)
root=$(stack "$@" path --local-install-root)/doc
if [ ! -f "$root/index.html" ]; then
      echo "$0: no Haddock index at $root" >&2
      exit 1
fi

# The project packages as name-version, the names of their directories
# under $root: the contents of Stack's local package database.
pkgs=$(stack "$@" exec -- ghc-pkg --no-user-package-db \
      --package-db="$(stack "$@" path --local-pkg-db)" list --simple-output)

case "$(cd "$out" && pwd -P)/" in
      "$(cd "$root" && pwd -P)/"*)
            echo "$0: $out is inside Stack's documentation directory $root" >&2
            exit 1 ;;
esac

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

escape_ere() { # a string as a sed -E pattern matching it literally
      # shellcheck disable=SC2016
      printf '%s\n' "$1" | sed -E 's/[][+.*?^$(){}|\\]/\\&/g'
}
rewrite() { # FILE SED-OPTIONS...
      local f=$1
      shift
      sed -E "$@" "$f" > "$f.tmp" && mv "$f.tmp" "$f"
}

find "$out/" -mindepth 1 -delete
# Stamped again below, into the real index page; this marks OUTDIR as this
# script's should the run die before that.
printf '<html><head>%s</head></html>\n' "$mine" > "$out/index.html"
n=0
for pkg in $pkgs; do
      if [ -d "$root/$pkg" ]; then
            cp -R "$root/$pkg" "$out/"
            n=$((n + 1))
      else
            echo "$0: warning: no documentation for $pkg" >&2
      fi
done
# Stack's index of the project packages (index.html and doc-index*.html)
# and the stylesheet, script, and image the pages share.
find "$root" -maxdepth 1 -type f -exec cp {} "$out/" \;
printf 's|<head>|<head>%s|\n' "$mine" > "$tmp/generator.sed"
rewrite "$out/index.html" -f "$tmp/generator.sed"

# Stack copies Haddock's output over a package's directory without emptying
# it, so the page of a module renamed or removed since an earlier build (in
# CI: the restored cache) survives there. Keep only the module pages the
# package's fresh contents page lists, and their hyperlinked sources.
for dir in "$out"/*/; do
      dir=${dir%/}
      grep -oE 'href="[^"/:#]+\.html"' "$dir/index.html" | sed -E 's/^href="//; s/"$//' > "$tmp/listed" || true
      stale=
      for f in "$dir"/*.html; do
            page=$(basename "$f")
            case $page in index.html | doc-index*.html) continue ;; esac
            grep -qxF -- "$page" "$tmp/listed" && continue
            rm -f "$f" "$dir/src/$(printf '%s\n' "${page%.html}" | tr '-' '.').html"
            stale="$stale $page"
      done
      if [ -n "$stale" ]; then
            echo "$0: $(basename "$dir"): dropped the pages of modules no longer in the package:$stale" >&2
      fi
done

# Every other directory under $root (but all/, Stack's index of everything)
# holds a GHC boot library's documentation, named <name>-<version>; a link
# into it becomes a link to the same page on Hackage. (The GHC
# installation's own copies carry a unit-id hash suffix, which Stack strips;
# it is stripped again here in case one survives.) The links occur in the
# pages and, with the quote escaped, in the Quick Jump data (doc-index.json).
: > "$tmp/hackage.sed"
for dir in "$root"/*/; do
      dir=$(basename "$dir")
      case " $pkgs " in
            *" $dir "*) continue ;;
      esac
      [ "$dir" = all ] && continue
      hackage=$(printf '%s\n' "$dir" | sed -E 's/^(.*-[0-9]+(\.[0-9]+)*)-[0-9a-f]{4}$/\1/')
      printf 's|(href=\\\\?")(\\.\\./)+%s/|\\1https://hackage.haskell.org/package/%s/docs/|g\n' \
            "$(escape_ere "$dir")" "$hackage" >> "$tmp/hackage.sed"
done
# The index pages Stack generates have no title, and they load Haddock's
# script, whose Quick Jump search needs a doc-index.json that exists only per
# package (the search works on the package pages).
cat > "$tmp/index.sed" <<'SED'
s|<title>( \(Index[^<]*\))?</title>|<title>ReWire\1</title>|
s|<span class="caption empty">&nbsp;</span>|<span class="caption">ReWire</span>|
s|<script src="haddock-bundle.min.js"[^>]*></script>||
SED

# (With the trailing slash, find, grep -r, and du below look through an
# OUTDIR that is a symbolic link.)
find "$out/" -mindepth 2 \( -name '*.html' -o -name doc-index.json \) -print0 \
      | while IFS= read -r -d '' f; do
      rewrite "$f" -f "$tmp/hackage.sed"
done
for f in "$out"/*.html; do
      rewrite "$f" -f "$tmp/hackage.sed" -f "$tmp/index.sed"
done
rm -f "$out/haddock-bundle.min.js"

# A reference Haddock could not resolve -- an instance from an undocumented
# dependency ("Defined in Prettyprinter.Internal"), a doc comment naming a
# module or identifier that does not exist -- comes out as a link to a page
# in the same directory that was never generated. Unlink those, and say so.
dead_links() { # DIR: the same-directory link targets of its pages that do not exist
      grep -hoE 'href="[^"/:#]+["#]' "$1"/*.html | sed -E 's/^href="//; s/["#]$//' | sort -u \
            | while IFS= read -r target; do
                  [ -e "$1/$target" ] || printf '%s\n' "$target"
            done || true
}
for dir in "$out"/*/; do
      dir=${dir%/}
      dead=$(dead_links "$dir")
      [ -n "$dead" ] || continue
      # A target has no / or : (so : can delimit) and no #; the link text may
      # carry inline markup.
      : > "$tmp/dead.sed"
      printf '%s\n' "$dead" | while IFS= read -r target; do
            printf 's:<a href="%s(#[^"]*)?"[^>]*>(([^<]|</?(code|em|strong|i|b)>)*)</a>:\\2:g\n' \
                  "$(escape_ere "$target")" >> "$tmp/dead.sed"
      done
      for f in "$dir"/*.html; do
            rewrite "$f" -f "$tmp/dead.sed"
      done
      still=$(dead_links "$dir")
      if [ -n "$still" ]; then
            echo "$0: warning: $(basename "$dir"): could not unlink references to: $(echo "$still" | tr '\n' ' ')" >&2
            printf '%s\n' "$still" > "$tmp/still"
            dead=$(printf '%s\n' "$dead" | grep -vxF -f "$tmp/still" || true)
      fi
      if [ -n "$dead" ]; then
            echo "$0: $(basename "$dir"): unlinked references to pages that do not exist: $(echo "$dead" | tr '\n' ' ')" >&2
      fi
done

# Nothing may link out of the site by a relative path.
echo "$pkgs" | tr ' ' '\n' > "$tmp/pkgs"
dangling=$(grep -rhoE 'href=\\?"(\.\./)+[^"/#?]+/' "$out/" | sed -E 's|^href=\\?"(\.\./)+||; s|/$||' \
      | sort -u | grep -vxF -f "$tmp/pkgs" | tr '\n' ' ' || true)
if [ -n "$dangling" ]; then
      echo "$0: links out of $out to: $dangling" >&2
      exit 1
fi

echo "Documentation of $n packages assembled in $out ($(du -sh "$out/" | awk '{ print $1 }'))."
