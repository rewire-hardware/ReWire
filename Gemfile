source "https://rubygems.org"

# Local development / preview. Run:
#   bundle install
#   bundle exec jekyll serve
#
# Note: GitHub Pages (classic, building from the gh-pages branch) uses its own
# pinned Jekyll; this site relies only on core Jekyll + kramdown + rouge, so it
# renders the same under either. If you'd rather build with a current Jekyll on
# GitHub Actions, this Gemfile is what that workflow would use.
gem "jekyll", "~> 4.3"

# Windows and JRuby do not include zoneinfo files; bundle the data.
platforms :mingw, :x64_mingw, :mswin, :jruby do
  gem "tzinfo", ">= 1", "< 3"
  gem "tzinfo-data"
end

# Faster file watching on Windows.
gem "wdm", "~> 0.1", platforms: [:mingw, :x64_mingw, :mswin]
