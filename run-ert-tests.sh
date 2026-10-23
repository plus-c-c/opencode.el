#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

mapfile -d '' test_files < <(
  find "$repo_root" -type f \
    \( -name '*-test.el' -o -name '*-tests.el' \) \
    -not -path "$repo_root/.git/*" \
    -print0
)

if ((${#test_files[@]} == 0)); then
  printf 'No ERT test files found.\n' >&2
  exit 1
fi

escaped_root="${repo_root//\\/\\\\}"
escaped_root="${escaped_root//\"/\\\"}"

elisp="(progn
  (package-initialize)
  (add-to-list 'load-path \"$escaped_root\")"

for test_file in "${test_files[@]}"; do
  escaped="${test_file//\\/\\\\}"
  escaped="${escaped//\"/\\\"}"
  elisp+="
  (load \"$escaped\" nil nil t)"
done

elisp+="
  (let ((stats (ert-run-tests-batch t)))
    (if (zerop (ert-stats-completed-unexpected stats))
        (progn (message \"PASS\") (kill-emacs 0))
      (progn (message \"FAIL\") (kill-emacs 1)))))"

timeout 300 emacs -Q --batch --eval "$elisp"
