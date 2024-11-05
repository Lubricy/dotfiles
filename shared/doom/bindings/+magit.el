(after! magit
  (transient-replace-suffix 'magit-push "t" '("t" "a tag" magit-push-tag))
  (transient-replace-suffix 'magit-push "T" '("T" "all tags" magit-push-tags)))
