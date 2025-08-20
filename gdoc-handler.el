;;; gdoc-handler.el --- Efficient .gdoc handler with caching -*- lexical-binding: t; -*-

;; Author: Martin Bari Garnier <martbari.g@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience, files
;; URL: https://github.com/MartinBaGar

;;; Commentary:

;; This package allows opening .gdoc files in Emacs using rclone and pandoc,
;; caching them locally until reboot.

;;; Code:

(defvar gdoc-cache-dir "/tmp/gdoc-cache"
  "Directory for caching downloaded .gdoc files.")

(defvar gdoc-cache-ttl (* 60 60)  ; 1 hour in seconds
  "Time-to-live for cached .gdoc files in seconds.")

(defun gdoc-ensure-cache-dir ()
  "Ensure the cache directory exists."
  (unless (file-exists-p gdoc-cache-dir)
    (make-directory gdoc-cache-dir t)))

(defun gdoc-cache-key (gdoc-path)
  "Generate a unique cache key for a .gdoc file."
  (secure-hash 'md5 gdoc-path))

(defun gdoc-cache-file-path (cache-key)
  "Get the full path for a cached file."
  (expand-file-name (concat cache-key ".txt") gdoc-cache-dir))

(defun gdoc-cache-valid-p (cache-file)
  "Check if cached file is still valid (exists and not expired)."
  (and (file-exists-p cache-file)
       (let* ((file-time (nth 5 (file-attributes cache-file)))
              (current-time (current-time))
              (age (float-time (time-subtract current-time file-time))))
         (< age gdoc-cache-ttl))))

(defun gdoc-transform-path (gdoc-path)
  "Transform filesystem path to rclone remote path."
  (let ((relative-path (cond
                        ;; Handle shared drives (.shortcut-targets-by-id)
                        ((string-match "/mnt/g/\\.shortcut-targets-by-id/[^/]+/\\(.*\\)" gdoc-path)
                         (concat "remote:" (match-string 1 gdoc-path)))
                        ;; Handle regular Google Drive
                        ((string-match "/mnt/g/\\(.*\\)" gdoc-path)
                         (concat "remote:" (match-string 1 gdoc-path)))
                        ;; Fallback
                        (t gdoc-path))))
    ;; Convert .gdoc to .docx and fix directory names with quotes
    (replace-regexp-in-string
     "\\([0-9]+\\)\\.[ ]+\\([^/]+\\)/" "\\1. \"\\2\"/"
     (replace-regexp-in-string "\\.gdoc$" ".docx" relative-path))))

(defun gdoc-latest-docx ()
  "Return the most recent .docx file in /tmp/, or nil if none."
  (car (sort (directory-files "/tmp/" t "\\.docx$")
             (lambda (a b) (time-less-p (nth 5 (file-attributes b))
                                        (nth 5 (file-attributes a)))))))

(defun gdoc-download-and-convert (rclone-path cache-file)
  "Download .gdoc as docx and convert to text, saving to CACHE-FILE."
  (let* ((user (or (getenv "SUDO_USER") (getenv "USER") "mabagar"))
         ;; Use the working rclone command structure
         (rclone-cmd (format "sudo -u %s rclone copy '%s' /tmp/ --drive-export-formats docx"
                             user rclone-path)))

    (message "Running: %s" rclone-cmd)

    (unwind-protect
        (progn
          ;; Download with rclone
          (let ((download-result (shell-command rclone-cmd "*rclone-output*"))
                (temp-docx nil))
            (setq temp-docx (gdoc-latest-docx))
            (if (and (= download-result 0) temp-docx (file-exists-p temp-docx))
                ;; Convert with pandoc
                (progn
                  (message "Converting %s to text..." temp-docx)
                  (let ((convert-cmd (format "pandoc '%s' -t plain -o '%s'" temp-docx cache-file))
                        (convert-result nil))
                    (setq convert-result (shell-command convert-cmd))
                    (if (and (= convert-result 0) (file-exists-p cache-file))
                        cache-file
                      (error "Failed to convert docx to text. Pandoc result: %d" convert-result))))
              (error "Failed to download file. Rclone result: %d. Check *rclone-output* buffer"
                     download-result))))
      ;; Cleanup temporary docx files
      (dolist (f (directory-files "/tmp/" t "\\.docx$"))
        (ignore-errors (delete-file f))))))

(defun gdoc-handler ()
  "Handle .gdoc files efficiently with caching."
  (interactive)
  (let* ((gdoc-path (buffer-file-name))
         (cache-key (gdoc-cache-key gdoc-path))
         (cache-file (gdoc-cache-file-path cache-key))
         (rclone-path (gdoc-transform-path gdoc-path)))

    (message "Original: %s" gdoc-path)
    (message "Transformed: %s" rclone-path)

    ;; Kill the .gdoc buffer immediately
    (kill-buffer (current-buffer))

    ;; Setup cache directory
    (gdoc-ensure-cache-dir)

    ;; Check cache first
    (if (gdoc-cache-valid-p cache-file)
        (progn
          (message "Opening cached version...")
          (switch-to-buffer (find-file-noselect cache-file))
          (read-only-mode 1)
          (message "Opened cached file (read-only)"))

      ;; Not cached or expired - download
      (message "Downloading and converting .gdoc file...")
      (condition-case err
          (progn
            (gdoc-download-and-convert rclone-path cache-file)
            (switch-to-buffer (find-file-noselect cache-file))
            (read-only-mode 1)
            (message "Downloaded and opened %s (read-only)" (file-name-nondirectory cache-file)))
        (error
         (message "Error: %s" (error-message-string err))
         (display-buffer "*rclone-output*"))))))

;; Auto-trigger for .gdoc files
(add-to-list 'auto-mode-alist '("\\.gdoc\\'" . gdoc-handler))

;; Debug function to test path transformation
(defun gdoc-debug-path ()
  "Debug function to test path transformation without downloading."
  (interactive)
  (let* ((gdoc-path (buffer-file-name))
         (rclone-path (gdoc-transform-path gdoc-path))
         (cache-key (gdoc-cache-key gdoc-path))
         (cache-file (gdoc-cache-file-path cache-key)))
    (message "Original: %s" gdoc-path)
    (message "Transformed: %s" rclone-path)
    (message "Cache key: %s" cache-key)
    (message "Cache file: %s" cache-file)
    (message "Test rclone ls: rclone ls '%s'" (file-name-directory rclone-path))))

;; Optional: Clear cache command
(defun gdoc-clear-cache ()
  "Clear all cached .gdoc files."
  (interactive)
  (when (file-exists-p gdoc-cache-dir)
    (let ((files (directory-files gdoc-cache-dir t "^[^.]")))
      (dolist (file files)
        (delete-file file))
      (message "Cleared %d cached files" (length files)))))

(provide 'gdoc-handler)
;;; gdoc-handler.el ends here
