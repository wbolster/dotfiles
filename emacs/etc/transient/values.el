((magit-commit-absorb "--force")
 (magit-fetch "--prune" "--tags")
 (magit-log "-n256" "--follow" "--topo-order" "--graph" "--color" "--decorate")
 (magit-merge "--no-ff")
 (magit-rebase "--autosquash" "--autostash")
 (magit-show-refs "--sort=-committerdate")
 (magit-tag "--annotate" "--edit")
 (python-pytest-dispatch "--color" "--capture=no" "--verbose --verbose" "--new-first" "--failed-first" "--maxfail=10" "--tb=short"))
