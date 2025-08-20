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

(defgroup gdoc-handler nil
  "Handle .gdoc files via rclone and pandoc."
  :group 'files
  :prefix "gdoc-handler-")

(defcustom gdoc-handler-cache-dir "/tmp/gdoc-cache"
  "Directory for caching downloaded .gdoc files."
  :type 'directory
  :group 'gdoc-handler)

(defun gdoc-handler-ensure-cache-dir ()
  "Ensure the cache directory exists."
  (unless (file-exists-p gdoc-handler-cache-dir)
    (make-directory gdoc-handler-cache-dir t)))

(defun gdoc-handler-cache-key (gdoc-path)
  "Generate a unique cache key for a .gdoc file."
  (secure-hash 'md5 gdoc-path))

(defun gdoc-handler-cache-file-path (cache-key)
  "Get the full path for a cached file."
  (expand-file-name (concat cache-key ".org") gdoc-handler-cache-dir))

(defun gdoc-handler-transform-path (gdoc-path)
  "Transform filesystem path to rclone remote path."
  (let ((relative-path
         (cond
          ((string-match "/mnt/g/\\.shortcut-targets-by-id/[^/]+/\\(.*\\)" gdoc-path)
           (concat "remote:" (match-string 1 gdoc-path)))
          ((string-match "/mnt/g/\\(.*\\)" gdoc-path)
           (concat "remote:" (match-string 1 gdoc-path)))
          (t gdoc-path))))
    (replace-regexp-in-string
     "\\([0-9]+\\)\\.[ ]+\\([^/]+\\)/" "\\1. \"\\2\"/"
     (replace-regexp-in-string "\\.gdoc$" ".docx" relative-path))))

(defun gdoc-handler-latest-docx ()
  "Return the most recent .docx file in /tmp/, or nil if none."
  (car (sort (directory-files "/tmp/" t "\\.docx$")
             (lambda (a b)
               (time-less-p (nth 5 (file-attributes b))
                            (nth 5 (file-attributes a)))))))

(defun gdoc-handler-download-and-convert (rclone-path cache-file)
  "Download .gdoc as docx and convert to text, saving to CACHE-FILE."
  (let* ((user (or (getenv "SUDO_USER") (getenv "USER") "mabagar"))
         (rclone-cmd (format "sudo -u %s rclone copy %s /tmp/ --drive-export-formats docx"
                             user (shell-quote-argument rclone-path))))
    (message "[gdoc] Running: %s" rclone-cmd)
    (unwind-protect
        (let ((download-result (shell-command rclone-cmd))
              (temp-docx nil))
          (setq temp-docx (gdoc-handler-latest-docx))
          (if (and (= download-result 0) temp-docx (file-exists-p temp-docx))
              (let ((convert-cmd (format "pandoc %s -t plain -o %s"
                                         (shell-quote-argument temp-docx)
                                         (shell-quote-argument cache-file)))
                    (convert-result nil))
                (setq convert-result (shell-command convert-cmd))
                (if (and (= convert-result 0) (file-exists-p cache-file))
                    cache-file
                  (error "[gdoc] Failed to convert docx to text. Pandoc result: %d" convert-result)))
            (error "[gdoc] Failed to download file. Rclone result: %d" download-result)))
      ;; Cleanup temporary docx files
      (dolist (f (directory-files "/tmp/" t "\\.docx$"))
        (ignore-errors (delete-file f))))))

;;;###autoload
(defun gdoc-handler ()
  "Handle .gdoc files efficiently with caching."
  (interactive)
  (let* ((gdoc-path (buffer-file-name))
         (cache-key (gdoc-handler-cache-key gdoc-path))
         (cache-file (gdoc-handler-cache-file-path cache-key))
         (rclone-path (gdoc-handler-transform-path gdoc-path)))
    (message "[gdoc] Original: %s" gdoc-path)
    (message "[gdoc] Transformed: %s" rclone-path)

    ;; Kill the .gdoc buffer immediately
    (kill-buffer (current-buffer))

    ;; Setup cache directory
    (gdoc-handler-ensure-cache-dir)

    ;; If cached, just open it
    (if (file-exists-p cache-file)
        (progn
          (message "[gdoc] Opening cached version...")
          (switch-to-buffer (find-file-noselect cache-file))
          (read-only-mode 1)
          (message "[gdoc] Opened cached file (read-only)"))
      ;; Not cached - download
      (message "[gdoc] Downloading and converting .gdoc file...")
      (condition-case err
          (progn
            (gdoc-handler-download-and-convert rclone-path cache-file)
            (switch-to-buffer (find-file-noselect cache-file))
            (read-only-mode 1)
            (message "[gdoc] Downloaded and opened %s (read-only)" (file-name-nondirectory cache-file)))
        (error
         (message "[gdoc] Error: %s" (error-message-string err)))))))

;;;###autoload
(defun gdoc-handler-clear-cache ()
  "Clear all cached .gdoc files."
  (interactive)
  (when (file-exists-p gdoc-handler-cache-dir)
    (let ((files (directory-files gdoc-handler-cache-dir t "^[^.]")))
      (dolist (file files)
        (delete-file file))
      (message "[gdoc] Cleared %d cached files" (length files)))))

;; Automatically handle .gdoc files
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gdoc\\'" . gdoc-handler))

(provide 'gdoc-handler)
;;; gdoc-handler.el ends here
