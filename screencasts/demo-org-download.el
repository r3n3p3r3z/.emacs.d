(require 'screencast)

(defun elmacro-org-download ()
  (insert screencast-outline-mode-example-text)
  (goto-char (point-min))
  (outline-next-heading)
  (forward-line)
  (setq org-startup-with-inline-images t)
  (setq random-image "https://loremflickr.com/600/400/star-wars")
  (org-download-image random-image))


(defconst demo-org-download
  '("* Org-Download" n
    "Allows you to drag and drop images in org-mode."

    l
    n

"1. An image inside your browser that you can drag to Emacs." n
"2. An image on your file system that you can drag to Emacs." n
"3. An image taking using a screenshot tool." n

n

l

"For a local or remote image use: [[elisp:org-download-yank][org-download-yank]]" n

n

"For an image taken using a screenshot tool use: [[elisp:org-download-screenshot][org-download-screenshot]]" n

(progn
      (org-mode)
    (elmacro-org-download)
    )

n

"[[https://github.com/DynamicMetaFlow/.emacs.d/issues/new][Questions?]]" n

n

"[[https://github.com/abo-abo/org-download][Visit the project]]" n

))
(defconst screencast-outline-mode-example-text
  "* Random image\n")

(defun demo-org-download (&optional arg)
  (interactive "P")
  (apply (if arg
             'screencast-record
           'screencast)
         demo-org-download
         "org-download"
         1
         ()
         )
  )
